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
  The pure arithmetic that turns a laid-out worksheet's extent into the
  scrollable (virtual) size and scroll granularity.

  Kept deliberately free of any GUI/wxWidgets dependency so the scroll-range
  math - historically a source of "the pane won't scroll" bugs - can be
  unit-tested in isolation. Worksheet::AdjustSize() measures the document
  (GetMaxPoint) and the window (GetClientSize) and feeds the numbers here, then
  applies the result with SetVirtualSize()/SetScrollRate().
*/

#ifndef WORKSHEETSIZEMATH_H
#define WORKSHEETSIZEMATH_H

#include <algorithm>
#include <vector>

//! One trailing group cell's geometry, as GetMaxPoint's height walk needs it.
struct TrailingGroupGeometry {
  //! Whether the cell's size still needs recomputing (GroupCell::HasStaleSize).
  bool sizeIsStale = false;
  //! The cell's laid-out top position (GetCurrentPoint().y); < 0 = not yet set.
  int currentY = 0;
  //! The cell's extent below its baseline (GetMaxDrop()).
  int maxDrop = 0;
};

/*! Compute the worksheet's content height from its trailing cells' geometry.

  This is the backward walk Worksheet::GetMaxPoint() runs from the last cell:
  it accumulates the heights of the cells at the bottom whose position is not yet
  trustworthy (stale-and-unpositioned, or still-being-laid-out) until it reaches
  an "anchor" - a cell whose size is stale but whose top position (y) is already
  valid - and pins the total to that anchor's top plus everything accumulated
  below it. If the walk runs off the top of the document (no such anchor), the
  height falls back to the document's base indent plus the accumulated heights.

  \param trailing   The cells starting at the LAST cell and running backward. The
                    walk stops at (and includes) the first entry that is the
                    anchor, i.e. sizeIsStale && currentY >= 0.
  \param groupSkip  The vertical gap between group cells (Configuration::GetGroupSkip).
  \param baseIndent The document's top margin (Configuration::GetBaseIndent), used
                    only when there is no anchor.

  Kept GUI-free (no Cell/Configuration dependency) so this fiddly stale-cell
  accounting can be unit-tested in isolation - see test_WorksheetSizeMath.
*/
inline int ComputeWorksheetContentHeight(
    const std::vector<TrailingGroupGeometry> &trailing, int groupSkip,
    int baseIndent) {
  int extraHeight = 0;
  for (const TrailingGroupGeometry &cell : trailing) {
    if (cell.sizeIsStale && cell.currentY >= 0)
      // The anchor: its top position is trusted; add its drop and everything
      // accumulated below it.
      return cell.currentY + cell.maxDrop + extraHeight;
    if (cell.sizeIsStale)
      // Stale and not yet positioned (currentY < 0): count its own extent.
      extraHeight += cell.maxDrop + groupSkip;
    else
      // Not stale but still unpositioned mid-layout: reserve a nominal height.
      extraHeight += groupSkip + 20;
  }
  return baseIndent + extraHeight;
}


//! The scrollable size + scroll granularity computed for a worksheet.
struct WorksheetVirtualSize {
  //! The virtual width (device px).
  int width = 40;
  //! The virtual (scrollable) height (device px).
  int height = 40;
  //! The scroll rate (device px per scroll unit).
  int scrollUnit = 10;
};

/*! Compute a worksheet's virtual size and scroll unit.

  \param hasTree            Whether the worksheet has any content. Without it the
                            result is a small fixed size with a sane scroll unit.
  \param maxPointWidth      The document's right extent (Worksheet::GetMaxPoint x).
  \param maxPointHeight     The document's bottom extent (Worksheet::GetMaxPoint y).
  \param clientHeight       The visible height of the worksheet window.
  \param currentScrollPixelY The current vertical scroll offset in pixels.

  Invariants (that the callers rely on):
  - a vertical scrollbar always has room to move: height >= clientHeight + 10;
  - the virtual size never shrinks below what the current scroll position needs
    (height >= currentScrollPixelY + clientHeight + 10), so wxWidgets does not
    clamp the scroll position and jump the view;
  - a little over-scroll past the end is allowed (with the view scrolled fully
    down the document occupies the top 1/8 of the client area);
  - the scroll unit is at least 10 px, so scrolling never feels sluggish on
    hi-res screens nor degenerates on tiny ones.
*/
inline WorksheetVirtualSize ComputeWorksheetVirtualSize(bool hasTree,
                                                        int maxPointWidth,
                                                        int maxPointHeight,
                                                        int clientHeight,
                                                        int currentScrollPixelY) {
  WorksheetVirtualSize result;
  int scrollUnitBasisHeight = 40;
  if (hasTree) {
    result.width = maxPointWidth;
    // Allow scrolling a little past the end of the document.
    int height = maxPointHeight + clientHeight - clientHeight / 8;
    // Keep a vertical scrollbar active.
    result.height = std::max(clientHeight + 10, height);
    // Never shrink below what the current scroll position requires.
    result.height =
      std::max(result.height, currentScrollPixelY + clientHeight + 10);
    scrollUnitBasisHeight = clientHeight;
  }
  result.scrollUnit = std::max(scrollUnitBasisHeight / 30, 10);
  return result;
}

/*! The narrow view surface the worksheet's size pipeline needs.

  Worksheet::AdjustSize() historically read the window (client size, scroll
  position) and wrote the scrollbars (virtual size, scroll rate) inline, which
  meant the deduplication and scroll-position-preserving logic could only ever
  run inside a live GUI. This abstract interface pulls those four operations out
  so the apply step can be driven headlessly by a mock view.

  The method names deliberately differ from the wxScrolled ones they forward to,
  so a class can inherit both wxScrolled and this without name-collision. Kept
  GUI-free (plain ints, no wx types) so a mock has nothing to pull in.
*/
class WorksheetView {
public:
  virtual ~WorksheetView() = default;
  //! The visible client area, in device pixels.
  virtual void GetViewClientSize(int *width, int *height) const = 0;
  //! The current vertical scroll position, in scroll units (not pixels).
  virtual int GetViewScrollUnitY() const = 0;
  //! Set the scrollable (virtual) area, in device pixels.
  virtual void SetViewVirtualSize(int width, int height) = 0;
  //! Set the scroll granularity (device pixels per scroll unit) on both axes.
  virtual void SetViewScrollRate(int rate) = 0;
};

//! Remembers the last virtual size applied, so an unchanged size is a no-op.
struct WorksheetVirtualSizeCache {
  int lastWidth = -1;
  int lastHeight = -1;
};

/*! Measure the view, compute the virtual size and apply it, skipping no-ops.

  This is the view-facing half of Worksheet::AdjustSize(): it reads the client
  size and scroll position from \p view, feeds them (with the already-measured
  document extent \p maxWidth / \p maxHeight) to ComputeWorksheetVirtualSize(),
  and - only if the result actually changed and is positive - pushes it back to
  the view and updates the scroll rate.

  \param view       The window abstraction (real worksheet or a test mock).
  \param hasTree     Whether the worksheet has content (ignores the extent if not).
  \param maxWidth    The document's right extent (Worksheet::GetMaxPoint x).
  \param maxHeight   The document's bottom extent (Worksheet::GetMaxPoint y).
  \param cache       The last-applied size, updated in place to dedupe.
  \param scrollUnit  Read to turn the scroll position into pixels, and updated to
                     the new granularity. Worksheet keeps it as a member because
                     its scroll handlers read it too.
*/
inline void ApplyWorksheetVirtualSize(WorksheetView &view, bool hasTree,
                                      int maxWidth, int maxHeight,
                                      WorksheetVirtualSizeCache &cache,
                                      int &scrollUnit) {
  int clientWidth, clientHeight;
  view.GetViewClientSize(&clientWidth, &clientHeight);

  int currentScrollPixelY = 0;
  if (hasTree)
    currentScrollPixelY = view.GetViewScrollUnitY() * scrollUnit;

  const WorksheetVirtualSize vs = ComputeWorksheetVirtualSize(
    hasTree, maxWidth, maxHeight, clientHeight, currentScrollPixelY);

  if ((cache.lastWidth != vs.width || cache.lastHeight != vs.height) &&
      vs.height > 0) {
    cache.lastWidth = vs.width;
    cache.lastHeight = vs.height;
    view.SetViewVirtualSize(vs.width, vs.height);
    scrollUnit = vs.scrollUnit;
    view.SetViewScrollRate(vs.scrollUnit);
  }
}

#endif
