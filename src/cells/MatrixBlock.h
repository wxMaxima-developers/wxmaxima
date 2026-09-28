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

#ifndef MATRIXBLOCK_H
#define MATRIXBLOCK_H

#include <cstddef>

/*! A rectangular block of a matrix's entries: a sub-matrix (GH #2345)

  Row and column indices are the matrix's own, zero-based and inclusive, so a
  block always holds at least one entry. Rows or columns the worksheet elides
  are still part of a block that spans them: a block is a piece of the
  matrix, not of what happens to be on screen.

  Kept in a header of its own so that DocumentCellPointers, which stores the
  selected block, needn't include MatrCell.h.
*/
struct MatrixBlock
{
  std::size_t firstRow = 0;
  std::size_t lastRow = 0;
  std::size_t firstCol = 0;
  std::size_t lastCol = 0;

  std::size_t Rows() const { return lastRow - firstRow + 1; }
  std::size_t Columns() const { return lastCol - firstCol + 1; }
  bool Contains(std::size_t row, std::size_t col) const {
    return (row >= firstRow) && (row <= lastRow) &&
      (col >= firstCol) && (col <= lastCol);
  }
  bool operator==(const MatrixBlock &) const = default;
};

#endif // MATRIXBLOCK_H
