// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  Implementation of WorksheetLayout, the worksheet's layout engine.
*/

#include "WorksheetLayout.h"
#include "Configuration.h"
#include "cells/GroupCell.h"
#include <wx/log.h>
#include <wx/stopwatch.h>
#include <algorithm>
#include <utility>
#include <vector>

WorksheetLayout::WorksheetLayout(Configuration *config, WorksheetView &view,
                                 std::function<GroupCell *()> getTree,
                                 std::function<GroupCell *()> getLastCell)
  : m_configuration(config), m_view(view), m_getTree(std::move(getTree)),
    m_getLastCell(std::move(getLastCell)) {}

bool WorksheetLayout::RecalculateIfNeeded(bool timeout, long timeSliceMs) {
  if (m_configuration->GetCanvasSize().x < 1)
    return (false);
  if (m_configuration->GetCanvasSize().y < 1)
    return (false);

  GroupCell *tree = m_getTree();
  if (!m_recalculateStart || !tree) {
    m_recalculateStart = {};
    return false;
  }

  if (!tree->Contains(m_recalculateStart))
    m_recalculateStart = tree;

  int viewX, viewY;
  m_view.GetViewPosition(&viewX, &viewY);
  m_configuration->SetWorksheetPosition(wxPoint(viewX, viewY));

  if (m_recalculateStart == tree)
    m_maxWidth_Cached = -1;

  // One walk drives both modes. When timeout is set the pass is time-sliced:
  // it advances the resume point (m_recalculateStart) cell by cell and yields
  // (returns) once timeSliceMs is up, resuming here next time. Otherwise it lays
  // out the whole scheduled range in one go and the shared tail below does the
  // final AdjustSize(). The stopwatch also feeds the end-of-worksheet log.
  {
    wxStopWatch stopwatch;
    bool propagationNeeded = true;
    int cellsVisited = 0;
    int cellsRecalculated = 0;
    for (auto &cell : OnList(m_recalculateStart.get())) {
      cellsVisited++;
      GroupCell *group = static_cast<GroupCell *>(&cell);
      bool neededRecalc = group->NeedsRecalculation();
      if (neededRecalc) cellsRecalculated++;
      bool movedThisTime = false;
      bool sizeChanged = false;

      if (neededRecalc) {
        int oldHeight = group->GetHeight();
        sizeChanged = group->Recalculate();
        if (group->GetHeight() != oldHeight)
          sizeChanged = true;
        movedThisTime = true;
        // The time-sliced path calls AdjustSize() itself when it reaches the end
        // of the worksheet; the one-shot path defers it to the shared tail and so
        // has to record here that a size change makes it necessary.
        if (sizeChanged && !timeout)
          m_adjustWorksheetSizeNeeded = true;

        int currentWidth = GroupCellWidthWithMargins(cell.GetWidth());
        m_maxWidth_Cached = std::max(m_maxWidth_Cached, currentWidth);
      } else if (propagationNeeded) {
        movedThisTime = group->Reposition();
      } else {
        // Don't stop here: a cell further down the worksheet may still be dirty.
        // Cells can be marked for recalculation non-contiguously (e.g. by
        // folding/hiding or by asynchronous Maxima output landing in specific
        // cells), so a clean, non-propagating cell does not imply everything
        // below it is clean. Keep scanning.
      }
      propagationNeeded = movedThisTime || sizeChanged;

      const bool atEnd = (cell.GetNext() == NULL);
      if (timeout) {
        if (!atEnd)
          m_recalculateStart = cell.GetNext();
        else {
          wxLogMessage(_("Recalculation hit the end of the worksheet => Updating its size (Visited %d cells, recalculated %d, in %ld ms)"), cellsVisited, cellsRecalculated, stopwatch.Time());
          m_recalculateStart = {};
          AdjustSize();
        }
        if (stopwatch.Time() > timeSliceMs) {
          m_lastCellsVisited = cellsVisited;
          m_lastCellsRecalculated = cellsRecalculated;
          // Yield to the event loop, but keep m_recalculateStart pointing at the
          // cell we stopped before so the next call resumes there. Returning here
          // (instead of breaking out to the shared tail below) is essential: that
          // tail clears m_recalculateStart on the assumption the whole range was
          // walked, which would drop the resume point and leave every cell past
          // this slice permanently un-recalculated.
          return true;
        }
      } else if (atEnd) {
        wxLogMessage(_("Recalculated the whole worksheet at once => Updating its size (Visited %d cells, recalculated %d, in %ld ms)"), cellsVisited, cellsRecalculated, stopwatch.Time());
      }
    }
    m_lastCellsVisited = cellsVisited;
    m_lastCellsRecalculated = cellsRecalculated;
  }
  // Clear the pending-recalculation marker before AdjustSize(): the whole
  // scheduled range has been walked, so cell positions are valid now and
  // AdjustSize()'s stale-position guard must let this legitimate call through.
  m_recalculateStart = {};
  if (m_adjustWorksheetSizeNeeded)
    AdjustSize();

  return true;
}

void WorksheetLayout::RequestRecalculation(Cell *start) {
  if (!m_getTree())
    return;
  wxASSERT(start);
  if (!start)
    return;

  GroupCell *group = start->GetGroup();
  if (!group)
    return;

  if (m_recalculateStart == group)
    return;

  group->MarkNeedsRecalculate();

  // Walk from group towards the start of its list: if we meet
  // m_recalculateStart the pending layout pass already covers group, and if
  // we end at the tree's first cell, group lies above m_recalculateStart and
  // becomes the new resume point. If we end anywhere else, group is not part
  // of the worksheet at all (a freshly constructed cell that is not spliced
  // in yet, or a cell in a hidden/folded subtree). Adopting such a cell as
  // the resume point would make the layout pass walk the wrong list and
  // silently drop the range that was already scheduled - so ignore the
  // request; detached cells get scheduled when they are spliced into the
  // tree, hidden ones when their fold parent is unfolded.
  Cell *chainHead = group;
  for (Cell *walk = group; walk; walk = walk->GetPrevious()) {
    if (walk == m_recalculateStart)
      return;
    chainHead = walk;
  }
  if (chainHead == m_getTree())
    m_recalculateStart = group;
}

void WorksheetLayout::UpdateConfigurationClientSize() {
  int clientWidth, clientHeight;
  m_view.GetViewClientSize(&clientWidth, &clientHeight);
  m_configuration->SetCanvasSize(wxSize(clientWidth, clientHeight));
}

int WorksheetLayout::GroupCellWidthWithMargins(int cellWidth) const {
  const int margin =
    m_configuration->Scale_Px(m_configuration->GetIndent() +
                              m_configuration->GetDefaultFontSize());
  return margin + cellWidth + margin;
}

void WorksheetLayout::GetMaxPoint(int *width, int *height) {
  // The height we return is read off the cells' positions, which are only
  // meaningful once a scheduled recalculation has finished. Reading them while
  // one is pending yields a silently-wrong (too small) height. AdjustSize() -
  // our only caller - guards against that; this assertion surfaces any future
  // caller that forgets to, instead of shipping a subtly wrong scroll range.
  wxASSERT_MSG(!m_recalculateStart,
               wxS("GetMaxPoint() called with a recalculation still pending: "
                   "cell positions are stale. Run RecalculateIfNeeded() first."));

  int currentHeight = m_configuration->GetIndent();
  *width = m_configuration->GetBaseIndent();

  if (m_maxWidth_Cached >= 0) {
    *width = std::max(*width, m_maxWidth_Cached);
  } else {
    // Collect each cell's width (including margins) and let the GUI-free width
    // math take the max - see ComputeWorksheetContentWidth() / test_WorksheetSizeMath.
    std::vector<int> cellWidths;
    for (Cell const &tmp : OnList(m_getTree()))
      cellWidths.push_back(GroupCellWidthWithMargins(tmp.GetWidth()));
    *width = ComputeWorksheetContentWidth(cellWidths,
                                          m_configuration->GetBaseIndent());
    m_maxWidth_Cached = *width;
  }
  GroupCell *lastCell = m_getLastCell();
  if (lastCell) {
    // Collect the trailing cells' geometry (last cell backward, up to and
    // including the anchor) and let the GUI-free height math replay the walk -
    // see ComputeWorksheetContentHeight() / test_WorksheetSizeMath.
    std::vector<TrailingGroupGeometry> trailing;
    for (Cell *walk = lastCell; walk; walk = walk->GetPrevious()) {
      const bool stale = walk->HasStaleSize();
      const int y = walk->GetCurrentPoint().y;
      trailing.push_back({stale, y, walk->GetMaxDrop()});
      if (stale && y >= 0)
        break; // reached the anchor
    }
    *height = ComputeWorksheetContentHeight(
        trailing, m_configuration->GetGroupSkip(),
        m_configuration->GetBaseIndent());
  } else {
    *height = currentHeight;
  }
}

void WorksheetLayout::AdjustSize() {
  // The virtual size is derived from the cell positions (GetMaxPoint reads the
  // last cell's y). Those positions are only valid once the scheduled layout
  // pass has actually run. While a recalculation is still pending -
  // RequestRecalculation() recorded a start point but RecalculateIfNeeded() has
  // not finished walking the list yet - reading them here would underestimate
  // the height and, at scroll position 0, leave the worksheet with no scroll
  // range at all (the bug that made a freshly opened diff-viewer pane
  // unscrollable). Defer instead: flag the size as needing adjustment and let
  // RecalculateIfNeeded() call us back once the positions are correct. This
  // makes "AdjustSize() with stale positions" harmless no matter who triggers
  // it, rather than relying on every caller to recalculate first.
  if (m_recalculateStart) {
    m_adjustWorksheetSizeNeeded = true;
    return;
  }

  // Measure the document here (cell tree), then let the GUI-free
  // ApplyWorksheetVirtualSize() read the window through the WorksheetView
  // interface, run the arithmetic (ComputeWorksheetVirtualSize) and push the
  // result back to the scrollbars - see WorksheetSizeMath.h, unit-tested there.
  const bool hasTree = (m_getTree() != NULL);
  int maxWidth = m_configuration->GetBaseIndent();
  int maxHeight = maxWidth;
  if (hasTree)
    GetMaxPoint(&maxWidth, &maxHeight);

  ApplyWorksheetVirtualSize(m_view, hasTree, maxWidth, maxHeight,
                            m_virtualSizeCache, m_scrollUnit);
  m_adjustWorksheetSizeNeeded = false;
}
