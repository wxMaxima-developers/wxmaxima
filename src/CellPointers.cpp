// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
#include "cells/GroupCell.h"
#include "cells/TextCell.h"
#include <algorithm>
#include <iterator>

// ======================================================================
//  ViewCellPointers (transient window state)
// ======================================================================

wxString ViewCellPointers::WXMXGetNewFileName() {
  wxString file(wxS("image"));
  file << (++m_wxmxImgCounter) << wxS(".");
  return file;
}

void ViewCellPointers::SetTimerIdForCell(Cell *const cell, int const timerId) {
  auto match =
    std::find_if(m_timerIds.begin(), m_timerIds.end(),
                 [cell](auto const &ctid) { return ctid.cell == cell; });
  if (match != m_timerIds.end()) {
    match->timerId = timerId;
    return;
  }

  m_timerIds.emplace_back(cell, timerId);
}

int ViewCellPointers::GetTimerIdForCell(Cell *const cell) const {
  auto match =
    std::find_if(m_timerIds.begin(), m_timerIds.end(),
                 [cell](auto const &ctid) { return ctid.cell == cell; });
  if (match != m_timerIds.end())
    return match->timerId;
  return -1;
}

Cell *ViewCellPointers::GetCellForTimerId(int const timerId) const {
  auto match = std::find_if(
                            m_timerIds.begin(), m_timerIds.end(),
                            [timerId](auto const &ctid) { return ctid.timerId == timerId; });
  if (match != m_timerIds.end())
    return match->cell;
  return nullptr;
}

void ViewCellPointers::RemoveTimerIdForCell(const Cell *const cell) {
  auto it = m_timerIds.begin();
  while (it != m_timerIds.end() && it->cell != cell)
    std::advance(it, 1);
  if (it != m_timerIds.end())
    m_timerIds.erase(it);
}

void ViewCellPointers::SetSearchStart(EditorCell *cell, int index) {
  m_cellSearchStartedIn = cell;
  m_indexSearchStartedAt = index;
}

void ViewCellPointers::SetMouseSelectionStart(EditorCell *cell) {
  m_cellMouseSelectionStartedIn = cell;
}

void ViewCellPointers::SetKeyboardSelectionStart(EditorCell *cell) {
  m_cellKeyboardSelectionStartedIn = cell;
}

GroupCell *ViewCellPointers::GetGroupCellUnderPointer() const {
  return m_groupCellUnderPointer;
}

void ViewCellPointers::SetGroupCellUnderPointer(GroupCell *cell) {
  m_groupCellUnderPointer = cell;
}

// ======================================================================
//  DocumentCellPointers (the document model)
// ======================================================================

void DocumentCellPointers::ErrorList::Remove(GroupCell *cell) {
  m_errors.erase(std::remove(m_errors.begin(), m_errors.end(), cell),
                 m_errors.end());
}

bool DocumentCellPointers::ErrorList::Contains(GroupCell *cell) const {
  return std::find(m_errors.begin(), m_errors.end(), cell) != m_errors.end();
}

void DocumentCellPointers::ErrorList::Add(GroupCell *cell) {
  m_errors.emplace_back(cell);
}

GroupCell *DocumentCellPointers::ErrorList::FirstError() const {
  return m_errors.empty() ? nullptr : m_errors.front().get();
}

GroupCell *DocumentCellPointers::ErrorList::LastError() const {
  return m_errors.empty() ? nullptr : m_errors.back().get();
}

EditorCell *DocumentCellPointers::GetAnswerCell() const { return m_answerCell; }

void DocumentCellPointers::SetAnswerCell(EditorCell *cell) { m_answerCell = cell; }

void DocumentCellPointers::ClearAnswerCellIfInGroup(GroupCell *group) {
  if (m_answerCell && m_answerCell->GetGroup() == group)
    m_answerCell = {};
}

TextCell *DocumentCellPointers::GetCurrentTextCell() const {
  return m_currentTextCell;
}

void DocumentCellPointers::SetCurrentTextCell(TextCell *cell) {
  m_currentTextCell = cell;
}

EditorCell *DocumentCellPointers::GetActiveCell() const { return m_activeCell; }

void DocumentCellPointers::SetActiveCell(EditorCell *cell) { m_activeCell = cell; }

void DocumentCellPointers::SetWorkingGroup(GroupCell *group) {
  if (group)
    m_lastWorkingGroup = group;
  m_workingGroup = group;
}

GroupCell *DocumentCellPointers::GetWorkingGroup(bool resortToLast) const {
  return (m_workingGroup || !resortToLast) ? m_workingGroup
    : m_lastWorkingGroup;
}
