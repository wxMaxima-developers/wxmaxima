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
  Implementation of WorksheetDocument's structural edits - see
  WorksheetDocument.h. Kept in a .cpp (rather than inline in the header) so the
  cell-list surgery does not have to be recompiled by every Worksheet.h user.
*/

#include "WorksheetDocument.h"
#include "WorksheetDocumentView.h"
#include "cells/CellList.h"
#include "cells/CellPtr.h"
#include "cells/GroupCell.h"

void WorksheetDocument::SetSaved(bool saved) {
  if (m_saved == saved)
    return;
  m_saved = saved;
  if (m_view)
    m_view->NotifySavedStateChanged();
}

GroupCell *WorksheetDocument::GetLastCell() const {
  if (m_last)
    return m_last;
  GroupCell *last = m_tree.get();
  if (last)
    last = last->last();
  m_last = last;
  return m_last;
}

void WorksheetDocument::NumberSections() const {
  int s, sub, subsub, h5, h6, i;
  s = sub = subsub = i = h5 = h6 = 0;
  if (m_tree)
    m_tree->Number(s, sub, subsub, h5, h6, i);
}

GroupCell *WorksheetDocument::InsertCells(std::unique_ptr<GroupCell> &&cells,
                                          GroupCell *where) {
  if (!cells)
    return nullptr; // nothing to insert

  if (m_view)
    m_view->NotifyAdjustSizeNeeded();
  bool renumbersections = false; // only renumber when true

  // Find the last cell in the tree that is to be inserted, and note whether any
  // of the inserted cells force the section numbering to be redone.
  GroupCell *lastOfCellsToInsert = cells.get();
  if (lastOfCellsToInsert->IsFoldable() ||
      (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (lastOfCellsToInsert->GetNext()) {
    if (lastOfCellsToInsert->IsFoldable() ||
        (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
    lastOfCellsToInsert = lastOfCellsToInsert->GetNext();
  }

  if (!m_tree) {
    m_tree = std::move(cells);
    m_last = lastOfCellsToInsert;
  } else if (!where) {
    CellList::SpliceInAfter(lastOfCellsToInsert, std::move(m_tree));
    if (m_view)
      m_view->NotifyRedraw(cells.get());
    m_tree = std::move(cells);
    m_last = nullptr; // finding new last is easier via GetLastCell()
  } else {
    if (where == m_last)
      m_last = lastOfCellsToInsert;
    else
      m_last = nullptr;
    CellList::SpliceInAfter(where, std::move(cells), lastOfCellsToInsert);
    // make sure m_last still points to the last cell of the worksheet!!
  }

  if (renumbersections)
    NumberSections();

  GroupCell *recalcStart = nullptr;
  if (where)
    recalcStart = where->GetNext();
  if (!recalcStart)
    recalcStart = m_tree.get();

  if (recalcStart && m_view) {
    m_view->NotifyRecalculation(recalcStart);
    // Also report the last inserted cell, so the layout engine's dirty range
    // covers the whole insertion, not just its first cell.
    if (lastOfCellsToInsert && lastOfCellsToInsert != recalcStart)
      m_view->NotifyRecalculation(lastOfCellsToInsert);
  }
  SetSaved(false); // document has been modified

  // AdjustSize() is intentionally NOT triggered directly here. Cell positions
  // are stale at this point (recalculation has only been scheduled, not
  // executed). Calling AdjustSize() with stale positions would compute a
  // virtualHeight that is too small, causing wxWidgets to clamp the scroll
  // position and jump the view to the top. NotifyAdjustSizeNeeded() (above)
  // ensures that RecalculateIfNeeded() will call AdjustSize() after positions
  // are correct.
  return lastOfCellsToInsert;
}

bool WorksheetDocument::CanRemoveCells(GroupCell *start,
                                      const GroupCell *end) const {
  if (!start || !end)
    return false;

  // We refuse deletion of a cell we are planning to evaluate
  for (const GroupCell &tmp : OnList(start)) {
    // We refuse deletion of a cell maxima is currently evaluating
    if (&tmp == m_cellPointers.GetWorkingGroup())
      return false;

    if (&tmp == end)
      return true;
  }

  return true;
}

std::unique_ptr<GroupCell> WorksheetDocument::RemoveCells(GroupCell *start,
                                                          GroupCell *end) {
  if (!CanRemoveCells(start, end))
    return nullptr;

  // check if chapters or sections need to be renumbered
  bool renumber = false;
  for (auto &tmp : OnList(start)) {
    m_evaluationQueue.Remove(&tmp);

    if (tmp.IsFoldable() || (tmp.GetGroupType() == GC_TYPE_IMAGE))
      renumber = true;

    // Don't keep cached versions of scaled images around in the undo buffer.
    if (tmp.GetOutput())
      tmp.GetOutput()->ClearCacheList();

    if (&tmp == end)
      break;
  }

  GroupCell *cellBeforeStart = start->GetPrevious();

  auto tornOut = CellList::TearOut(start, end);
  m_last = nullptr;
  if (!tornOut.cellOwner) {
    wxASSERT(m_tree.get() == tornOut.cell);
    tornOut.cellOwner = std::move(m_tree);
    m_tree = dynamic_unique_ptr_cast<GroupCell>(std::move(tornOut.tailOwner));
  }
  auto removed = static_unique_ptr_cast<GroupCell>(std::move(tornOut.cellOwner));

  if (renumber)
    NumberSections();

  if (m_view) {
    // Geometry above the deleted region is unaffected: recalculate starting
    // at the cell before it (or from the new tree start if we deleted at the
    // top). If both are null the whole document was just emptied - nothing
    // left to recalculate (RequestRecalculation() requires a non-null start).
    GroupCell *recalcStart = cellBeforeStart ? cellBeforeStart : m_tree.get();
    if (recalcStart)
      m_view->NotifyRecalculation(recalcStart);
    m_view->NotifyRedraw(nullptr);
  }
  SetSaved(false);

  return removed;
}
