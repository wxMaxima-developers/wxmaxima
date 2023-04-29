// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009      Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012      Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class EvaluationQueue

  This queue contains all code cells that still have to be passed to maxima.
*/

#include "EvaluationQueue.h"
#include "MaximaTokenizer.h"

bool EvaluationQueue::Empty() const {
  return (m_queue.empty()) && (m_commands.empty());
}

EvaluationQueue::EvaluationQueue() {
  m_size = 0;
  m_workingGroupChanged = false;
}

void EvaluationQueue::Clear() {
  m_queue.clear();
  m_size = 0;
  m_commands.clear();
  m_workingGroupChanged = false;
}

bool EvaluationQueue::IsInQueue(GroupCell *gr) const {
  return std::find(m_queue.begin(), m_queue.end(), gr) != m_queue.end();
}

void EvaluationQueue::Remove(GroupCell *gr) {
  bool removeFirst = IsLastInQueue(gr);
  auto pos = std::find(m_queue.begin(), m_queue.end(), gr);
  if (pos != m_queue.end())
    m_queue.erase(pos);
  m_size = m_queue.size();
  if (removeFirst) {
    m_commands.clear();
    if (!m_queue.empty())
      AddTokens(gr);
  }
}

void EvaluationQueue::AddToQueue(GroupCell *gr) {
  if (gr == NULL)
    return;

  if (gr->GetGroupType() != GC_TYPE_CODE ||
      gr->GetEditable() == NULL) // don't add cells which can't be evaluated
    return;

  if (m_queue.empty()) {
    AddTokens(gr);
    m_workingGroupChanged = true;
  }
  m_size++;
  m_queue.push_back(gr);
}

/**
 * Add the tree of hidden cells to the EQ by recursively adding cells'
 * hidden branches to the EQ.
 */
void EvaluationQueue::AddHiddenTreeToQueue(GroupCell *gr) {
  if (!gr)
    return; // caller should check, but just in case

  for (auto &cell : OnList(gr->GetHiddenTree())) {
    AddToQueue(&cell);
    AddHiddenTreeToQueue(&cell);
  }
}

void EvaluationQueue::RemoveFirst() {
  if (!m_commands.empty()) {
    m_workingGroupChanged = false;
    m_commands.erase(m_commands.begin());
  } else {
    do {
      if (m_queue.empty())
        return;

      m_queue.erase(m_queue.begin());
      m_size--;
      AddTokens(GetCell());
    } while (m_commands.empty() && (!m_queue.empty()));
    m_workingGroupChanged = true;
  }
}

void EvaluationQueue::AddTokens(GroupCell *cell) {
  if (cell == NULL)
    return;
  wxString token;
  int index = 0;
  for (auto const &tok : cell->GetEditable()->GetAllTokens()) {
    const TextStyle itemStyle = tok.GetTextStyle();
    wxString itemText = tok.GetText();
    itemText.Replace(wxT("\u00a0"), " ");
    index += itemText.Length();
    if (itemStyle != TS_CODE_COMMENT)
      token += itemText;

    if (itemStyle == TS_CODE_LISP) {
      token.Trim(true);
      token.Trim(false);
      if (!token.IsEmpty())
        m_commands.emplace_back(token, index);
      token.Clear();
      continue;
    }

    if (itemStyle == TS_CODE_ENDOFLINE) {
      token.Trim(true);
      token.Trim(false);
      if (!token.IsEmpty())
        m_commands.emplace_back(token, index);
      token.Clear();
      continue;
    }
  }
  token.Trim(true);
  token.Trim(false);
  if (!token.IsEmpty())
    m_commands.emplace_back(token, index);
}

GroupCell *EvaluationQueue::GetCell() {
  if (m_queue.empty())
    return NULL;
  else
    return m_queue.front();
}

wxString EvaluationQueue::GetCommand() {
  if (m_commands.empty())
    return {};

  auto retval = m_commands.front().GetString();

  m_userLabel.Clear();
  wxString userLabel;

  int colonPos = retval.find(wxT(":"));
  if (colonPos != wxNOT_FOUND && !retval.StartsWith(wxT(":lisp"))) {
    userLabel = retval.Left(colonPos);
    userLabel.Trim(true);
    userLabel.Trim(false);
    if (!userLabel.empty() &&
        (wxIsalpha(userLabel[0]) || (userLabel[0] == wxT('\\')) ||
         (userLabel[0] > 127) || (userLabel[0] == wxT('_')))) {
      for (size_t i = 0; i < userLabel.Length(); i++) {
        if (userLabel[i] == wxT('\\'))
          i++;
        else {
          if ((!wxIsalnum(userLabel[i])) && (userLabel[i] != '_') &&
              (userLabel[i] < 128) && (userLabel[i] != '[') &&
              (userLabel[i] != ']')) {
            userLabel.Clear();
            break;
          }
        }
      }
      m_userLabel = userLabel;
    }
  }
  return retval;
}
