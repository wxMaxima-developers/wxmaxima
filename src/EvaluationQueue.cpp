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
  m_pendingText.Clear();
  m_pendingConfig = nullptr;
  m_cellConsumedChars = 0;
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
    m_pendingText.Clear();
    m_pendingConfig = nullptr;
    m_cellConsumedChars = 0;
    if (!m_queue.empty())
      AddTokens(GetCell());
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
  m_queue.push_back(CellPtr<GroupCell>(gr));
}

/**
 * Add the tree of hidden cells to the EQ by recursively adding cells'
 * hidden branches to the EQ.
 */
void EvaluationQueue::AddHiddenTreeToQueue(const GroupCell *gr) {
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
    // The prompt for the command we just finished may have switched lisp mode on
    // or off, so tokenize the next command of THIS cell in the mode current now.
    ProduceNextCommand();
    if (!m_commands.empty())
      return;
  }
  // The current cell is drained: advance to the next cell (skipping any that
  // turn out to contain no runnable command).
  do {
    if (m_queue.empty())
      return;

    m_queue.erase(m_queue.begin());
    m_size--;
    AddTokens(GetCell());
  } while (m_commands.empty() && (!m_queue.empty()));
  m_workingGroupChanged = true;
}

void EvaluationQueue::AddTokens(const GroupCell *cell) {
  m_commands.clear();
  m_pendingText.Clear();
  m_pendingConfig = nullptr;
  m_cellConsumedChars = 0;
  if (cell == NULL)
    return;
  m_pendingConfig = cell->GetEditable()->GetConfiguration();
  m_pendingText = cell->GetEditable()->ToString(true);
  ProduceNextCommand();
}

void EvaluationQueue::ProduceNextCommand() {
  if (!m_commands.empty() || (m_pendingConfig == nullptr))
    return;

  // Peel one command off the front of m_pendingText, re-tokenizing in the mode
  // that is current NOW. A command ends at the first ";"/"$" (TS_CODE_ENDOFLINE)
  // or at a whole lisp form (TS_CODE_LISP). If the text ends without such a
  // boundary, whatever is left is the cell's LAST command and, when we are in
  // maxima (not lisp) mode, needs a ";" appended so maxima evaluates it instead
  // of waiting for a line ending.
  while (!m_pendingText.IsEmpty()) {
    // In lisp mode each top-level form is its OWN command: maxima's lisp REPL
    // prints a prompt after EVERY form, so bundling several forms into one send
    // would make the queue over-advance (one RemoveFirst per prompt) and desync
    // the lisp/maxima mode for the cells that follow. Peel off one balanced
    // parenthesized form (honoring strings, "#\c" char literals and ";" line
    // comments, inside which parentheses do not count).
    if (m_pendingConfig->InLispMode()) {
      const wxString &text = m_pendingText;
      const std::size_t len = text.Length();
      std::size_t i = 0;
      while ((i < len) && wxIsspace(text[i]))
        ++i;
      if (i >= len) { // only whitespace left
        m_cellConsumedChars += static_cast<long>(len);
        m_pendingText.Clear();
        return;
      }
      std::size_t j = i;
      if (text[j] == wxS('(')) {
        int depth = 0;
        bool inString = false;
        for (; j < len; ++j) {
          const wxChar c = text[j];
          if (inString) {
            if (c == wxS('\\')) { ++j; continue; }
            if (c == wxS('"')) inString = false;
            continue;
          }
          if (c == wxS('"')) { inString = true; continue; }
          if (c == wxS(';')) { // lisp line comment
            while ((j < len) && (text[j] != wxS('\n')))
              ++j;
            if (j >= len) break;
            continue;
          }
          if ((c == wxS('#')) && (j + 1 < len) && (text[j + 1] == wxS('\\'))) {
            j += 2; // skip a "#\c" character literal
            continue;
          }
          if (c == wxS('('))
            ++depth;
          else if (c == wxS(')')) {
            --depth;
            if (depth <= 0) { ++j; break; }
          }
        }
      } else {
        // A bare atom/symbol (rare): read up to the next whitespace.
        while ((j < len) && !wxIsspace(text[j]))
          ++j;
      }
      wxString form = m_pendingText.SubString(i, j - 1);
      form.Trim(true).Trim(false);
      const long index = m_cellConsumedChars + static_cast<long>(j);
      m_cellConsumedChars += static_cast<long>(j);
      m_pendingText = m_pendingText.Mid(j);
      if (!form.IsEmpty()) {
        m_commands.emplace_back(form, index);
        return;
      }
      continue;
    }

    auto tokens = MaximaTokenizer(m_pendingText, m_pendingConfig).PopTokens();
    wxString command;
    int scanned = 0;   // raw chars walked so far (incl. skipped comments)
    int consumed = 0;  // raw chars this command takes out of m_pendingText
    bool haveBoundary = false;

    for (auto const &tok : tokens) {
      const TextStyle itemStyle = tok.GetTextStyle();
      wxString itemText = tok.GetText();
      itemText.Replace(wxS("\u00a0"), " ");
      scanned += itemText.Length();
      if (itemStyle != TS_CODE_COMMENT)
        command += itemText;

      if (itemStyle == TS_CODE_LISP) {
        wxString trimmed = command;
        trimmed.Trim(true).Trim(false);
        if (!trimmed.IsEmpty()) {
          consumed = scanned;
          haveBoundary = true;
          break;
        }
        command.Clear();
        continue;
      }
      if (itemStyle == TS_CODE_ENDOFLINE) {
        wxString stripped = command;
        stripped.Replace(wxS(";"), wxEmptyString);
        stripped.Replace(wxS("$"), wxEmptyString);
        stripped.Trim(true).Trim(false);
        if (!stripped.IsEmpty()) {
          consumed = scanned;
          haveBoundary = true;
          break;
        }
        command.Clear();
        continue;
      }
    }

    if (!haveBoundary) {
      // Reached the end of the cell without a terminator.
      consumed = scanned;
      wxString stripped = command;
      stripped.Replace(wxS(";"), wxEmptyString);
      stripped.Replace(wxS("$"), wxEmptyString);
      stripped.Trim(true).Trim(false);
      if (stripped.IsEmpty()) {
        // Only whitespace/comments were left: no more commands in this cell.
        m_cellConsumedChars += consumed;
        m_pendingText.Clear();
        return;
      }
      if (!m_pendingConfig->InLispMode()) {
        wxString trimmed = command;
        trimmed.Trim(true);
        if (!trimmed.EndsWith(wxS(";")) && !trimmed.EndsWith(wxS("$")))
          command += wxS(";");
      }
    }

    const long index = m_cellConsumedChars + consumed;
    m_cellConsumedChars += consumed;
    m_pendingText = m_pendingText.Mid(consumed);

    command.Trim(true);
    command.Trim(false);
    if (!command.IsEmpty()) {
      m_commands.emplace_back(command, index);
      return;
    }
    // command was empty (e.g. a stray ";"): loop and try the next one.
  }
}

int EvaluationQueue::CommandsLeftInCell() const {
  // Best-effort progress hint (see the header): the current command plus a rough
  // count of what is still pending. The exact split depends on the future
  // lisp/maxima mode, so we count statement terminators plus any unterminated
  // tail rather than re-running the mode-sensitive tokenizer.
  int left = static_cast<int>(m_commands.size());
  bool tailHasContent = false;
  for (wxString::const_iterator it = m_pendingText.begin();
       it != m_pendingText.end(); ++it) {
    const wxUniChar c = *it;
    if ((c == wxS(';')) || (c == wxS('$'))) {
      left++;
      tailHasContent = false;
    } else if (!wxIsspace(c)) {
      tailHasContent = true;
    }
  }
  if (tailHasContent)
    left++;
  return left;
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

  const auto &retval = m_commands.front().GetString();

  m_userLabel.Clear();
  wxString userLabel;

  int colonPos = retval.find(wxS(":"));
  if (colonPos != wxNOT_FOUND && !retval.StartsWith(wxS(":lisp"))) {
    userLabel = retval.Left(colonPos);
    userLabel.Trim(true);
    userLabel.Trim(false);
    if (!userLabel.empty() &&
        (wxIsalpha(userLabel[0]) || (userLabel[0] == wxS('\\')) ||
         (userLabel[0] > 127) || (userLabel[0] == wxS('_')))) {
      for (std::size_t i = 0; i < userLabel.Length(); i++) {
        if (userLabel[i] == wxS('\\'))
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
