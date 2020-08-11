// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

void CellListBuilderBase::base_Append(std::unique_ptr<Cell> &&cells)
{
  m_lastAppended = cells;
  if (!cells)
    return;

  if (!m_head)
  {
    m_head = std::move(cells);
    m_tail = m_head;
  }
  else
    CellList::AppendCell(m_tail, std::move(cells));

  m_tail = m_tail->last();
}

Cell *CellListBuilderBase::base_DynamicAppend(std::unique_ptr<Cell> &&cells, Cell *(*caster)(Cell *))
{
  for (Cell *cell = cells.get(); cell; cell = cell->GetNext())
    if (!caster(cell))
      return {};

  Cell *const theCells = cells.get();
  base_Append(std::move(cells));
  return theCells;
}

//

void CellList::Check(const Cell *c)
{
  if (!c) return;
  wxASSERT_MSG(!c->m_next || c->m_next->m_previous == c, "Bug: The successor cell's predecessor is invalid.");
  wxASSERT_MSG(!c->m_previous || c->m_previous->m_next == c, "Bug: The predecessor cell's successor is invalid.");
}

std::unique_ptr<Cell> CellList::SetNext(Cell *c, std::unique_ptr<Cell> &&next)
{
  if (next)
    Check(next.get());

  // Detach old successor from this list
#if 0
  auto oldNext = std::move(c->m_next);
#else
  std::unique_ptr<Cell> oldNext{c->m_next};
#endif
  if (oldNext)
    oldNext->m_previous = nullptr;

  // Add next as a successor
#if 0
  c->m_next = std::move(next);
#else
  c->m_next = next.release();
#endif
  if (c->m_next)
    c->m_next->m_previous = c;
  c->SetNextToDraw(c->m_next);

  Check(c);
  return oldNext;
}

void CellList::AppendCell(Cell *c, std::unique_ptr<Cell> &&head)
{
  Check(c);
  if (!head)
    return;
  if (c->m_group)
    // Note: The above cannot be m_group or an assert will trigger!
    // We do not *expect* all cells here to have groups, so GetGroup()
    // above would be inappropriate.
    c->GetGroup()->ResetData();

  auto *const next = head.get();
  auto *const last = c->last();

  // We want to append to the draw list as well
  // Get the end of the draw list
  auto *lastToDraw = last->GetNextToDraw();
  if (lastToDraw)
    while (lastToDraw->GetNextToDraw())
      lastToDraw = lastToDraw->GetNextToDraw();

  // Append the cell
  SetNext(last, std::move(head));

  // Restore the draw list, and append the cell to it
  if (lastToDraw)
    lastToDraw->SetNextToDraw(next);
}

CellList::SplicedIn CellList::SpliceIn(Cell *where, std::unique_ptr<Cell> &&head, Cell *last)
{
  if (!where)
    return {};
  Check(where);
  if (!head)
    return {};

  where->m_maxDrop = -1;
  where->m_maxCenter = -1;

  if (!last)
    last = head->last();
  wxASSERT(last);
  wxASSERT_MSG(!last->m_next, "Bug: SpliceIn::last has a successor, it will be deleted.");

  // We're explicitly splicing into the draw list as well
  // - preserve the draw list.
  auto *const nextToDraw = where->GetNextToDraw();

  // Insert the cells into the cell list
#if 0
  SetNext(last, std::move(where->m_next));
#else
  std::unique_ptr<Cell> wherem_next{where->m_next};
  where->m_next = nullptr;
  SetNext(last, std::move(wherem_next));
#endif
  last->SetNextToDraw(nextToDraw);
  SetNext(where, std::move(head));

  Check(where);
  Check(last);

  //! \todo There should be more diagnostic checks for list integrity.
  return {last};
}

CellList::TornOut CellList::TearOut(Cell *first, Cell *last)
{
  if (!first || !last) return {};

  Check(first);
  Check(last);

  TornOut retval;
  retval.cell = first;

  auto *const previous = first->m_previous.get();
  if (previous)
  {
#if 0
    retval.cellOwner = std::move(previous->m_next);
#else
    auto *previousNext = previous->m_next;
    previous->m_next = nullptr;
    retval.cellOwner = std::unique_ptr<Cell>(previousNext);
#endif
    if ((previous->m_next = std::move(last->m_next)))
      previous->m_next->m_previous = previous;
    previous->SetNextToDraw(previous->m_next);
    Check(previous);
  }
  else
  {
#if 0
    if ((retval.tailOwner = std::move(last->m_next)))
#else
    auto *lastNext = last->m_next;
    last->m_next = nullptr;
    retval.tailOwner = std::unique_ptr<Cell>(lastNext);
#endif
    {
      retval.tailOwner->m_previous = nullptr;
      Check(retval.tailOwner.get());
    }
  }

  last->SetNextToDraw(nullptr);
  first->m_previous = nullptr;
  Check(first);
  Check(last);

  return retval;
}
