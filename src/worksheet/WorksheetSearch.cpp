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
  Implementation of the worksheet's search engine, extracted from
  Worksheet::FindNext / FindNext_Regex.
*/

#include "WorksheetSearch.h"

#include "cells/Cell.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#include <vector>

namespace WorksheetSearch {

bool StringMatcher::Matches(wxString text) const {
  wxString s = m_str;
  if (m_ignoreCase) {
    text.MakeLower();
    s.MakeLower();
  }
  return text.Contains(s);
}

bool StringMatcher::FindInEditor(EditorCell *editor, bool down) const {
  // The editor gets the original string: its incremental search handles
  // the case folding itself and compares the previous match against the
  // un-folded search expression.
  return editor->FindNext(m_str, down, m_ignoreCase);
}

bool RegexMatcher::Matches(wxString text) const {
  return m_regex.Matches(text);
}

bool RegexMatcher::FindInEditor(EditorCell *editor, bool down) const {
  return editor->FindNext_RegEx(m_pattern, down);
}

namespace {

/*! Search one group.

  The start-group skip rules (applied only when isStartGroup) make the
  search continue behind the cursor or the previous match:
  - a previous match in the editor continues inside the editor (the
    editor's own search state knows where it was);
  - a previously matched output cell restricts the output scan to the
    cells behind it (in the search direction);
  - anything already matched or holding the cursor makes the parts before
    it (in the search direction) off-limits.

  On a match fills in the target's part and cell and returns true.
*/
bool FindInGroup(GroupCell *pos, bool isStartGroup, const SearchStart &start,
                 bool down, const Matcher &matcher, bool searchInInput,
                 bool searchInOutput, SearchTarget *target) {
  if (down) {
    // Search order for 'down': Prompt -> Editor -> Output

    // 1. Prompt
    bool skipPrompt = !searchInInput;
    if (isStartGroup && (start.m_inEditor || start.m_atCell)) {
      // Any cursor or previous match in the start group lies at or after
      // its prompt, so going down the prompt is already behind us.
      skipPrompt = true;
    }
    if (!skipPrompt && pos->GetPrompt() &&
        matcher.Matches(pos->GetPrompt()->ToString())) {
      target->m_part = MatchPart::Prompt;
      target->m_cell = pos->GetPrompt();
      return true;
    }

    // 2. Editor
    bool skipEditor = !searchInInput;
    if (isStartGroup && start.m_atCell && start.m_atCell != pos->GetPrompt())
      skipEditor = true;
    if (!skipEditor && pos->GetEditable() &&
        matcher.FindInEditor(pos->GetEditable(), down)) {
      target->m_part = MatchPart::Editor;
      target->m_cell = nullptr;
      return true;
    }

    // 3. Output
    if (searchInOutput && pos->GetLabel()) {
      bool outputStarted = true;
      if (isStartGroup && start.m_atCell && start.m_atCell != pos->GetPrompt())
        outputStarted = false;
      for (const Cell &cell : OnDrawList(pos->GetLabel())) {
        if (!outputStarted) {
          if (&cell == start.m_atCell)
            outputStarted = true;
          continue;
        }
        if (matcher.Matches(cell.ToString())) {
          target->m_part = MatchPart::Output;
          target->m_cell = const_cast<Cell *>(&cell);
          return true;
        }
      }
    }
  } else {
    // Search order for 'up': Output -> Editor -> Prompt

    // 1. Output
    if (searchInOutput && pos->GetLabel()) {
      bool skipOutput =
        isStartGroup && (start.m_inEditor ||
                         (start.m_atCell && start.m_atCell == pos->GetPrompt()));
      if (!skipOutput) {
        std::vector<const Cell *> outputCells;
        for (const Cell &cell : OnDrawList(pos->GetLabel()))
          outputCells.push_back(&cell);

        int startIndex = static_cast<int>(outputCells.size()) - 1;
        if (isStartGroup && start.m_atCell && !start.m_inEditor) {
          for (int i = 0; i < static_cast<int>(outputCells.size()); ++i) {
            if (outputCells[i] == start.m_atCell) {
              startIndex = i - 1;
              break;
            }
          }
        }

        for (int i = startIndex; i >= 0; --i) {
          if (matcher.Matches(outputCells[i]->ToString())) {
            target->m_part = MatchPart::Output;
            target->m_cell = const_cast<Cell *>(outputCells[i]);
            return true;
          }
        }
      }
    }

    // 2. Editor
    bool skipEditor = !searchInInput;
    if (isStartGroup && start.m_atCell && start.m_atCell == pos->GetPrompt())
      skipEditor = true;
    if (!skipEditor && pos->GetEditable() &&
        matcher.FindInEditor(pos->GetEditable(), down)) {
      target->m_part = MatchPart::Editor;
      target->m_cell = nullptr;
      return true;
    }

    // 3. Prompt
    bool skipPrompt = !searchInInput;
    if (isStartGroup && start.m_atCell && start.m_atCell == pos->GetPrompt())
      skipPrompt = true;
    if (!skipPrompt && pos->GetPrompt() &&
        matcher.Matches(pos->GetPrompt()->ToString())) {
      target->m_part = MatchPart::Prompt;
      target->m_cell = pos->GetPrompt();
      return true;
    }
  }
  return false;
}

} // anonymous namespace

SearchTarget FindNextTarget(GroupCell *tree, GroupCell *last,
                            const SearchStart &start, bool down,
                            const Matcher &matcher, bool searchInInput,
                            bool searchInOutput) {
  if (!start.m_group)
    return {};

  // Remember where to go if we need to wrap the search.
  const GroupCell *startGroup = start.m_group;

  GroupCell *pos = start.m_group;
  bool wrappedSearch = false;
  bool isStartGroup = true;
  bool done = false;
  while (!done) {
    SearchTarget target;
    if (FindInGroup(pos, isStartGroup, start, down, matcher, searchInInput,
                    searchInOutput, &target)) {
      target.m_group = pos;
      target.m_wrapped = wrappedSearch;
      return target;
    }

    GroupCell *nextPos = down ? pos->GetNext() : pos->GetPrevious();
    if (!nextPos && !wrappedSearch) {
      wrappedSearch = true;
      nextPos = down ? tree : last;
    }

    if (!nextPos || (nextPos == startGroup && wrappedSearch))
      done = true;

    pos = nextPos;
    isStartGroup = false;
  }
  return {};
}

} // namespace WorksheetSearch
