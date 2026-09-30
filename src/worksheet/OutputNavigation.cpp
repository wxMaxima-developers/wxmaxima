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
  Walking a cell's output as a tree, see OutputNavigation.h.
*/

#include "OutputNavigation.h"
#include "cells/Cell.h"
#include "cells/GroupCell.h"
#include "cells/LimitCell.h"
#include "cells/MatrCell.h"

namespace OutputNavigation {

namespace {

//! Does this cell have inner cells, i.e. is it built from other cells?
bool HasInnerCells(const Cell *cell) {
  for ([[maybe_unused]] const Cell &inner : OnInner(cell))
    return true;
  return false;
}

//! Is this cell drawn, and does it show anything?
bool IsShown(const Cell *cell) {
  if (cell->IsHidden())
    return false;
  wxString text = cell->ToString();
  text.Trim();
  text.Trim(false);
  return !text.IsEmpty() || HasInnerCells(cell) ||
    (cell->GetType() == MC_TYPE_IMAGE) || (cell->GetType() == MC_TYPE_SLIDE);
}

/*! Is this part of a cell only a glyph of its linear form?

  Composite cells keep the "sqrt(", "," and ")" they show while broken into
  lines among their inner cells (see Cell::GetBrokenCell()). They are
  punctuation, not parts of the expression, so the keyboard skips them. They
  are always a single text cell, and their text is one of a few kinds: an
  opening bracket, possibly after a function name, or a lone bracket,
  separator or operator glyph.
*/
bool IsLinearFormGlyph(const Cell *cell) {
  if (cell->GetNext() || HasInnerCells(cell))
    return false;
  wxString text = cell->ToString();
  text.Trim();
  text.Trim(false);
  if (text.IsEmpty())
    return true;
  if (text.EndsWith(wxS("("))) {
    for (auto ch : text.Left(text.Length() - 1))
      if (!wxIsalpha(ch) && (ch != wxS('_')))
        return false;
    return true;
  }
  static const wxString glyphs[] = {
    wxS(")"), wxS("["), wxS("]"), wxS("{"), wxS("}"), wxS(","), wxS("/"),
    wxS("^"), wxS("|"), wxS("…"), wxS("...")};
  for (const auto &glyph : glyphs)
    if (text == glyph)
      return true;
  return false;
}

//! The shown cells of a run, output labels left out
std::vector<Cell *> ShownCells(Cell *first, const Cell *last) {
  std::vector<Cell *> cells;
  for (Cell *cell = first; cell; cell = cell->GetNext()) {
    if ((cell->GetType() != MC_TYPE_LABEL) && IsShown(cell))
      cells.push_back(cell);
    if (cell == last)
      break;
  }
  return cells;
}

//! The run a whole list of cells forms, from its head to its last shown cell
std::vector<Item> ListAsChild(Cell *head) {
  const auto cells = ShownCells(head, nullptr);
  if (cells.empty())
    return {};
  return {Run(cells.front(), cells.back())};
}

//! The parts of a single cell that isn't a matrix
std::vector<Item> PartsOf(const Cell *cell) {
  std::vector<Item> parts;
  // A limit's first inner cell is its "lim", a label rather than something
  // the limit is made of
  bool skipNext = dynamic_cast<const LimitCell *>(cell) != nullptr;
  for (Cell &head : OnInner(cell)) {
    if (skipNext) {
      skipNext = false;
      continue;
    }
    if (IsLinearFormGlyph(&head))
      continue;
    for (const auto &part : ListAsChild(&head))
      parts.push_back(part);
  }
  return parts;
}

//! The entries of a matrix that are drawn, row by row
std::vector<Item> EntriesOf(MatrCell *matrix) {
  std::vector<Item> entries;
  for (std::size_t row = 0; row < matrix->GetMatrixRows(); row++)
    for (std::size_t col = 0; col < matrix->GetMatrixColumns(); col++)
      if (!matrix->IsElided(row, col))
        entries.push_back(Entry(matrix, MatrixEntry{row, col}));
  return entries;
}

//! The cells an item shows, to tell whether two items look the same
std::vector<Cell *> ShownContent(const Item &item) {
  if (item.IsMatrixEntry()) {
    Cell *head = item.matrix->GetInnerCell(static_cast<int>(item.entry.row),
                                           static_cast<int>(item.entry.col));
    return head ? ShownCells(head, nullptr) : std::vector<Cell *>{};
  }
  std::vector<Cell *> cells;
  for (Cell *cell = item.first; cell; cell = cell->GetNext()) {
    if (IsShown(cell))
      cells.push_back(cell);
    if (cell == item.last)
      break;
  }
  return cells;
}

//! The parts of an item, before looking through items that only wrap one cell
std::vector<Item> DirectChildren(const Item &item) {
  if (item.IsMatrixEntry()) {
    Cell *head = item.matrix->GetInnerCell(static_cast<int>(item.entry.row),
                                           static_cast<int>(item.entry.col));
    std::vector<Item> cells;
    if (head)
      for (Cell *cell : ShownCells(head, nullptr))
        cells.push_back(Run(cell, cell));
    return cells;
  }
  if (!item.first)
    return {};
  if (item.first != item.last) {
    std::vector<Item> cells;
    for (Cell *cell : ShownCells(item.first, item.last))
      cells.push_back(Run(cell, cell));
    return cells;
  }
  // An image or an animation is a picture, not an expression with parts
  if ((item.first->GetType() == MC_TYPE_IMAGE) ||
      (item.first->GetType() == MC_TYPE_SLIDE))
    return {};
  if (auto *matrix = dynamic_cast<MatrCell *>(item.first))
    return EntriesOf(matrix);
  return PartsOf(item.first);
}

bool Search(const std::vector<Item> &level, const Item &target,
            std::vector<Item> &path, Location &location) {
  for (std::size_t i = 0; i < level.size(); i++) {
    path.push_back(level[i]);
    if (level[i] == target) {
      location.found = true;
      location.path = path;
      location.siblings = level;
      location.index = i;
      return true;
    }
    if (Search(Children(level[i]), target, path, location))
      return true;
    path.pop_back();
  }
  return false;
}

bool SearchRun(const std::vector<Item> &level, const Cell *first,
               const Cell *last, std::vector<Item> &path,
               RunLocation &location) {
  for (std::size_t i = 0; i < level.size(); i++)
    if (!level[i].IsMatrixEntry() && (level[i].first == first))
      for (std::size_t j = i; j < level.size(); j++)
        if (!level[j].IsMatrixEntry() && (level[j].last == last)) {
          location.found = true;
          location.path = path;
          location.siblings = level;
          location.firstIndex = i;
          location.lastIndex = j;
          return true;
        }
  for (const auto &item : level) {
    path.push_back(item);
    if (SearchRun(Children(item), first, last, path, location))
      return true;
    path.pop_back();
  }
  return false;
}

} // namespace

bool Item::operator==(const Item &other) const {
  if (matrix || other.matrix)
    return (matrix == other.matrix) && (entry.row == other.entry.row) &&
      (entry.col == other.entry.col);
  return (first == other.first) && (last == other.last);
}

Item Run(Cell *first, Cell *last) {
  Item item;
  item.first = first;
  item.last = last;
  return item;
}

Item Entry(MatrCell *matrix, MatrixEntry entry) {
  Item item;
  item.first = matrix;
  item.last = matrix;
  item.matrix = matrix;
  item.entry = entry;
  return item;
}

std::vector<Item> Results(const GroupCell *group) {
  std::vector<Item> results;
  if (!group)
    return results;
  for (const auto &result : group->GetOutputResults())
    results.push_back(Run(result.first, result.last));
  return results;
}

std::vector<Item> Children(const Item &item) {
  auto children = DirectChildren(item);
  // An item that only wraps a single part -- a matrix entry holding one
  // fraction, a run of which only one cell is shown -- would make Enter
  // select what looks exactly like what was selected before. Go on into that
  // part's own parts instead.
  if ((children.size() == 1) && !children.front().IsMatrixEntry() &&
      (ShownContent(children.front()) == ShownContent(item)))
    return Children(children.front());
  return children;
}

Location Locate(const GroupCell *group, const Item &item) {
  Location location;
  std::vector<Item> path;
  Search(Results(group), item, path, location);
  return location;
}

RunLocation LocateRun(const GroupCell *group, const Cell *first,
                      const Cell *last) {
  RunLocation location;
  std::vector<Item> path;
  SearchRun(Results(group), first, last, path, location);
  return location;
}

} // namespace OutputNavigation
