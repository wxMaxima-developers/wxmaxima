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

#endif
