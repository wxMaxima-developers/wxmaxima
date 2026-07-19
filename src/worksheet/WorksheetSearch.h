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
  The worksheet's search engine.

  Finds the next match of a search expression in the tree of group cells,
  walking the groups in a wrap-around loop and checking each group's parts
  in document order: prompt, editor, output when searching down; output,
  editor, prompt when searching up. In the group the search starts in,
  skip rules make the search continue behind the cursor or the previous
  match instead of re-finding it.

  This module only decides WHERE the next match is; it knows nothing about
  the view. Applying the match (activating the editor or selecting the
  matched cell, scrolling to it) stays in Worksheet.
*/

#ifndef WORKSHEETSEARCH_H
#define WORKSHEETSEARCH_H

#include <wx/regex.h>
#include <wx/string.h>

class Cell;
class EditorCell;
class GroupCell;

namespace WorksheetSearch {

/*! Matches a search expression against worksheet text.

  One implementation per search flavor. Prompt and output cells are matched
  against their rendered text; editors are searched through their own
  incremental search, which continues behind the previous match when the
  editor already holds it as its selection.
*/
class Matcher {
public:
  virtual ~Matcher() = default;
  //! Does this text contain a match?
  virtual bool Matches(wxString text) const = 0;
  //! Search inside an editor, continuing that editor's previous search.
  virtual bool FindInEditor(EditorCell *editor, bool down) const = 0;
};

//! Substring search, optionally case-insensitive.
class StringMatcher final : public Matcher {
public:
  StringMatcher(const wxString &str, bool ignoreCase)
    : m_str(str), m_ignoreCase(ignoreCase) {}
  bool Matches(wxString text) const override;
  bool FindInEditor(EditorCell *editor, bool down) const override;

private:
  wxString m_str;
  bool m_ignoreCase;
};

//! Regular-expression search.
class RegexMatcher final : public Matcher {
public:
  explicit RegexMatcher(const wxString &pattern)
    : m_pattern(pattern), m_regex(pattern) {}
  //! False if the pattern did not compile; searching would find nothing.
  bool IsValid() const { return m_regex.IsValid(); }
  bool Matches(wxString text) const override;
  bool FindInEditor(EditorCell *editor, bool down) const override;

private:
  wxString m_pattern;
  wxRegEx m_regex;
};

//! Which part of a group cell a search match was found in.
enum class MatchPart { Prompt, Editor, Output };

//! A search match: the group it is in and the matching part.
struct SearchTarget {
  //! The group holding the match; null if nothing was found
  GroupCell *m_group = nullptr;
  MatchPart m_part = MatchPart::Prompt;
  //! The matching prompt or output cell; null for an editor match
  Cell *m_cell = nullptr;
  //! True if the search passed the document end and started over
  bool m_wrapped = false;
  bool Found() const { return m_group != nullptr; }
};

/*! Where a search starts: a group plus what the cursor is doing there.

  At most one of m_inEditor/m_atCell is set - the editor cursor and a cell
  selection exclude each other.
*/
struct SearchStart {
  //! The group the search starts in; null = nowhere to search
  GroupCell *m_group = nullptr;
  //! True if this group's editor holds the cursor or the previous match
  bool m_inEditor = false;
  //! The previously matched prompt or output cell in this group, if any
  Cell *m_atCell = nullptr;
};

/*! Find the next match, walking the group cells with wrap-around.

  \param tree the first group cell of the worksheet
  \param last the last group cell of the worksheet (wrap target going up)
  \param start where to start and what the cursor is doing there
  \param down search direction
  \param matcher the search expression
  \param searchInInput whether prompts and editors are searched
  \param searchInOutput whether output cells are searched

  The start group is searched only once, from the cursor onwards: after a
  full wrap-around the loop stops when it reaches the start group again.
*/
SearchTarget FindNextTarget(GroupCell *tree, GroupCell *last,
                            const SearchStart &start, bool down,
                            const Matcher &matcher, bool searchInInput,
                            bool searchInOutput);

} // namespace WorksheetSearch

#endif // WORKSHEETSEARCH_H
