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

#include "DiffScrollSync.h"

#include <algorithm>
#include <cstddef>

int ComputeSyncedScrollY(const std::vector<int> &srcCellTops,
                         const std::vector<int> &otherCellTops,
                         int srcViewportTop) {
  const std::size_t n =
    std::min(srcCellTops.size(), otherCellTops.size());

  // The anchor is the first diff entry whose source cell starts at or below the
  // source viewport top.
  int anchor = DIFFSYNC_NO_CELL;
  for (std::size_t k = 0; k < n; ++k) {
    if (srcCellTops[k] != DIFFSYNC_NO_CELL &&
        srcCellTops[k] >= srcViewportTop) {
      anchor = static_cast<int>(k);
      break;
    }
  }
  if (anchor == DIFFSYNC_NO_CELL)
    return DIFFSYNC_NO_CELL;
  if (otherCellTops[anchor] == DIFFSYNC_NO_CELL)
    return DIFFSYNC_NO_CELL;

  // Offset of the anchor cell below the source viewport top; reproduce it in the
  // other worksheet.
  const int offsetBelowTop = srcCellTops[anchor] - srcViewportTop;
  return std::max(0, otherCellTops[anchor] - offsetBelowTop);
}
