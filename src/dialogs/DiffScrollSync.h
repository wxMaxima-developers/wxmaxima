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
  The pure scroll-synchronization geometry for the diff viewer.

  Kept deliberately free of any GUI/wxWidgets dependency so the alignment math
  (which has historically been a source of bugs) can be unit-tested in isolation.
  DiffFrame::SyncScrollFrom() feeds it the cell positions read from the live
  worksheets and applies the result with Worksheet::Scroll().
*/

#ifndef DIFFSCROLLSYNC_H
#define DIFFSCROLLSYNC_H

#include <vector>

//! A diff-entry cell that has no counterpart in a given worksheet is marked with
//! this top coordinate.
constexpr int DIFFSYNC_NO_CELL = -1;

/*! Compute where the "other" worksheet must scroll to stay aligned with the
  scrolled ("source") worksheet.

  A single top-anchored rule is used for both scroll directions: find the first
  diff entry whose source cell starts at or below the source viewport top, and
  scroll the other worksheet so its matching cell sits at the same offset below
  the viewport top.

  \param srcCellTops    For each diff entry, the top y-coordinate of the source
                        worksheet's cell, or DIFFSYNC_NO_CELL if that entry has
                        no cell in the source worksheet.
  \param otherCellTops  The same, for the other worksheet. Must be the same
                        length as @p srcCellTops (indexed by diff entry).
  \param srcViewportTop The source worksheet's new viewport-top, in pixels.
  \return The other worksheet's target viewport-top (>= 0), or DIFFSYNC_NO_CELL
          (-1) if there is no usable anchor (no source cell at/below the viewport
          top, or the anchor entry has no cell in the other worksheet) -- in
          which case the caller should leave the other worksheet where it is.
*/
int ComputeSyncedScrollY(const std::vector<int> &srcCellTops,
                         const std::vector<int> &otherCellTops,
                         int srcViewportTop);

#endif // DIFFSCROLLSYNC_H
