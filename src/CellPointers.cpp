// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "CellPointers.h"
#include "GroupCell.h"
#include <algorithm>
#include <iterator>

CellPointers::CellPointers(wxScrolledCanvas *worksheet) :
  m_worksheet(worksheet)
{}

wxString CellPointers::WXMXGetNewFileName()
{
  wxString file(wxT("image"));
  file << (++m_wxmxImgCounter) << wxT(".");
  return file;
}

void CellPointers::SetTimerIdForCell(Cell *const cell, int const timerId)
{
  auto match = std::find_if(m_timerIds.begin(), m_timerIds.end(),
                            [cell](auto const &ctid) { return ctid.cell == cell; });
  if (match != m_timerIds.end())
  {
    match->timerId = timerId;
    return;
  }

  m_timerIds.emplace_back(cell, timerId);
}

int CellPointers::GetTimerIdForCell(Cell *const cell) const
{
  auto match = std::find_if(m_timerIds.begin(), m_timerIds.end(),
                            [cell](auto const &ctid) { return ctid.cell == cell; });
  if (match != m_timerIds.end())
    return match->timerId;
  return -1;
}

Cell *CellPointers::GetCellForTimerId(int const timerId) const
{
  auto match = std::find_if(m_timerIds.begin(), m_timerIds.end(),
                            [timerId](auto const &ctid) { return ctid.timerId == timerId; });
  if (match != m_timerIds.end())
    return match->cell;
  return nullptr;
}

void CellPointers::RemoveTimerIdForCell(Cell *const cell)
{
  auto it = m_timerIds.begin();
  while (it != m_timerIds.end() && it->cell != cell)
    std::advance(it, 1);
  if (it != m_timerIds.end())
    m_timerIds.erase(it);
}

void CellPointers::ErrorList::Remove(GroupCell * cell)
{
  m_errors.erase(std::remove(m_errors.begin(), m_errors.end(), cell), m_errors.end());
}

bool CellPointers::ErrorList::Contains(GroupCell * cell) const
{
  return std::find(m_errors.begin(), m_errors.end(), cell) != m_errors.end();
}

void CellPointers::ErrorList::Add(GroupCell * cell)
{ m_errors.emplace_back(cell); }

void CellPointers::SetWorkingGroup(GroupCell *group)
{
  if (group)
    m_lastWorkingGroup = group;
  m_workingGroup = group;
}

GroupCell *CellPointers::GetWorkingGroup(bool resortToLast) const
{ return (m_workingGroup || !resortToLast) ? m_workingGroup : m_lastWorkingGroup; }

GroupCell *CellPointers::ErrorList::FirstError() const
{ return m_errors.empty() ? nullptr : m_errors.front().get(); }

GroupCell *CellPointers::ErrorList::LastError() const
{ return m_errors.empty() ? nullptr : m_errors.back().get(); }
