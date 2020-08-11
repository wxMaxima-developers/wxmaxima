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
#include "Cell.h"

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
    m_tail->AppendCell(cells.release());

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
