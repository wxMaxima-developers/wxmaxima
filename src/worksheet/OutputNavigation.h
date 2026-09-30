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
  The output of a cell as a tree the keyboard can walk through (GH #2382).

  The worksheet's output cursor is its ordinary selection. This file only
  answers the structural questions the arrow keys need: what the parts of a
  selected expression are (Enter goes into them), which part it is itself a
  part of (Escape goes back out) and which parts sit next to it (Left and
  Right). It knows nothing about key events or the worksheet, so it can be
  tested on parsed output alone.

  The tree follows the expression's structure, Cell::GetInnerCell(), not the
  draw list: whether a wide expression happens to be broken into lines must
  not change what a key press does. Its levels are:

  - the results of a cell (GroupCell::GetOutputResults()), each a label and
    its expression;
  - below a result, the cells of its expression, without the label;
  - below a single cell, its parts: a fraction's numerator and denominator,
    a function's name and arguments, the contents of parentheses, ... Glyphs
    a cell only shows while broken into lines ("sqrt(", ",", ")") are not
    parts;
  - below a part that is a run of several cells (the numerator of (a+b)/c),
    those cells;
  - below a matrix, its entries. An entry is selected as a one-entry block
    (DocumentCellPointers::SetSelectedMatrixBlock()), so the entry is
    highlighted in place and Shift+arrow keys grow it into a block exactly
    as they grow a block selected with the mouse (GH #2370).

  Anything that isn't drawn -- hidden cells, empty ones, elided matrix
  entries -- is left out.
*/

#ifndef OUTPUTNAVIGATION_H
#define OUTPUTNAVIGATION_H

#include "cells/MatrixBlock.h"
#include <vector>

class Cell;
class GroupCell;
class MatrCell;

namespace OutputNavigation {

/*! One node of the tree: a run of sibling cells, or one entry of a matrix

  For a run, first and last are the first and the last of the cells, which
  are equal for a single cell. For a matrix entry, matrix is set, entry says
  which one and first and last are the matrix.
*/
struct Item
{
  Cell *first = nullptr;
  Cell *last = nullptr;
  MatrCell *matrix = nullptr;
  MatrixEntry entry;

  bool IsMatrixEntry() const { return matrix != nullptr; }
  bool operator==(const Item &other) const;
};

//! The item a run of cells forms
Item Run(Cell *first, Cell *last);
//! The item one entry of a matrix forms
Item Entry(MatrCell *matrix, MatrixEntry entry);

//! The top level: the results of this cell's output, as items
std::vector<Item> Results(const GroupCell *group);

/*! The parts of an item, in reading order; empty if it has none

  What Enter goes into.
*/
std::vector<Item> Children(const Item &item);

/*! Where an item sits in the tree

  path runs from the top-level result down to the item itself, and siblings
  is the level the item is one of (the results, for a top-level item), with
  index its position there. Found is false if the item isn't a node of the
  tree, e.g. a part of an expression selected with the mouse that doesn't
  match any node.
*/
struct Location
{
  bool found = false;
  std::vector<Item> path;
  std::vector<Item> siblings;
  std::size_t index = 0;
};
Location Locate(const GroupCell *group, const Item &item);

/*! Where a run of several siblings sits: the level holding both of its ends

  For a selection that Shift+Left/Right grew over several siblings, which
  is not a node of the tree itself. firstIndex and lastIndex are the
  positions of the siblings the run starts and ends with. Found is false if
  no level has a sibling starting where the run starts and a later one
  ending where it ends.
*/
struct RunLocation
{
  bool found = false;
  std::vector<Item> path; //!< The path to the parent; empty at the top level
  std::vector<Item> siblings;
  std::size_t firstIndex = 0;
  std::size_t lastIndex = 0;
};
RunLocation LocateRun(const GroupCell *group, const Cell *first, const Cell *last);

} // namespace OutputNavigation

#endif // OUTPUTNAVIGATION_H
