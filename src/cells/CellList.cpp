// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include "CellList.h"
#include "GroupCell.h"
#include <utility>
void CellListBuilderBase::base_Append(std::unique_ptr<Cell> &&cells) {
  m_lastAppended = cells.get();
  if (!cells)
    return;

  if (!m_head) {
    m_head = std::move(cells);
    m_tail = m_head.get();
  } else
    CellList::AppendCell(m_tail, std::move(cells));

  m_tail = m_tail->last();
}

Cell *CellListBuilderBase::base_DynamicAppend(std::unique_ptr<Cell> &&cells,
                                              Cell *(*caster)(Cell *)) {
  for (Cell &cell : OnList(cells.get()))
    if (!caster(&cell))
      return {};

  Cell *const theCells = cells.get();
  base_Append(std::move(cells));
  return theCells;
}

//

void CellList::Check(const Cell *cell) {
  if (!cell)
    return;
  wxASSERT_MSG(!cell->m_next || cell->m_next->m_previous == cell,
               "Bug: The successor cell's predecessor is invalid.");
  wxASSERT_MSG(!cell->m_previous || cell->m_previous->m_next.get() == cell,
               "Bug: The predecessor cell's successor is invalid.");
}

std::unique_ptr<Cell> CellList::SetNext(Cell *cell, std::unique_ptr<Cell> &&next) {
  using std::swap;
  if (next)
    Check(next.get());

  // Reset the previous pointers so they have no chance of dangling
  if (cell->m_next)
    cell->m_next->m_previous = nullptr;
  if (next)
    next->m_previous = nullptr;
  // Set the next pointers
  swap(cell->m_next, next);
  // Set the previous pointer for our successor
  if (cell->m_next)
    cell->m_next->m_previous = cell;
  cell->SetNextToDraw(cell->m_next);

  Check(cell);
  Check(next.get());

  return std::move(next);
}

void CellList::DeleteList(Cell *afterMe) {
  for (auto next = std::move(afterMe->m_next); next;
       next = std::move(next->m_next)) {
    next->m_previous = nullptr;
    // next's destructor will run next, and it will see correct, null m_previous
  }
}

void CellList::AppendCell(Cell *cell, std::unique_ptr<Cell> &&tail) {
  Check(cell);
  if (!tail)
    return;
  if (cell->m_group)
    // Note: The above cannot be m_group or an assert will trigger!
    // We do not *expect* all cells here to have groups, so GetGroup()
    // above would be inappropriate.
    cell->GetGroup()->ResetSize_Recursively();

  auto *const next = tail.get();
  auto *const last = cell->last();

  // We want to append to the draw list as well
  // Get the end of the draw list
  auto *lastToDraw = last->GetNextToDraw();
  if (lastToDraw)
    while (lastToDraw->GetNextToDraw())
      lastToDraw = lastToDraw->GetNextToDraw();

  // Append the cell
  SetNext(last, std::move(tail));

  // Restore the draw list, and append the cell to it
  if (lastToDraw)
    lastToDraw->SetNextToDraw(next);
}

CellList::SplicedIn
CellList::SpliceInAfter(Cell *where, std::unique_ptr<Cell> &&head, Cell *last) {
  if (!where)
    return {};
  Check(where);
  if (!head)
    return {};

  if (!last)
    last = head->last();
  wxASSERT(last);
  if(last)
    {
      wxASSERT_MSG(!last->m_next,
                   "Bug: SpliceIn::last has a successor, it will be deleted.");

      // We're explicitly splicing into the draw list as well
      // - preserve the draw list.
      auto *const nextToDraw = where->GetNextToDraw();

      // Insert the cells into the cell list
      SetNext(last, std::move(where->m_next));
      last->SetNextToDraw(nextToDraw);
    }
  SetNext(where, std::move(head));

  Check(where);
  if(last)
    Check(last);

  //! \todo There should be more diagnostic checks for list integrity.
  return {last};
}

CellList::TornOut CellList::TearOut(Cell *first, Cell *last) {
  if (!first || !last)
    return {};

  Check(first);
  Check(last);

  TornOut retval;
  retval.cell = first;

  auto *const previous = first->m_previous;
  if (previous) {
    retval.cellOwner = SetNext(previous, SetNext(last, nullptr));
    Check(previous);
  } else {
    retval.tailOwner = SetNext(last, nullptr);
    Check(retval.tailOwner.get());
  }

  wxASSERT(!last->GetNextToDraw());
  wxASSERT(!first->m_previous);
  Check(first);
  Check(last);

  return retval;
}
