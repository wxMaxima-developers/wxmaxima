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

#ifndef MCPTOOLS_H
#define MCPTOOLS_H

#include <nlohmann/json.hpp>
#include <wx/string.h>
#include <stdexcept>
#include <string>

class Worksheet;
class Variablespane;
class GroupCell;

/*! Thrown by McpTools::CallTool() for an unknown tool name or invalid/missing
  arguments. The transport layer (McpServer) catches this and turns it into a
  JSON-RPC error response -- McpTools itself knows nothing about JSON-RPC.
*/
class McpToolError : public std::runtime_error {
public:
  explicit McpToolError(const std::string &msg) : std::runtime_error(msg) {}
};

/*! Implements the read-only "worksheet context" tools the MCP server exposes
  to an external AI tool (GH request: "a de facto standard [MCP] AI sidebar
  that ... gives it access to a worksheet").

  Every tool here only *reads* the worksheet, with two narrow exceptions that
  the maintainer explicitly scoped in as safe: watch_variable/unwatch_variable
  only change what the Variables sidebar happens to be tracking for display,
  the same as a user typing a name into that sidebar by hand -- they never
  touch worksheet content, insert/edit/evaluate anything, or have any
  side effect Maxima itself would notice beyond the pre-existing "query this
  variable's value" flow the sidebar already triggers on its own schedule.
  Every other tool cannot mutate anything even in principle: turning a
  worksheet or a cell into text is all they do. This split is deliberate
  product scope (confirmed with the maintainer), not an oversight -- a
  write-capable "insert/edit/evaluate" tool surface is a different, far
  higher-stakes feature this first pass does not implement.

  Deliberately free of any networking/JSON-RPC framing (that's McpServer's
  job) so this can be unit-tested directly against a real, headless
  Worksheet/Variablespane -- see test/unit_tests/test_McpTools.cpp.

  Thread safety: none of this is thread-safe, same as every other class that
  touches a Worksheet/GroupCell/Variablespane. McpServer only ever calls into
  this from the GUI thread's own event loop (its wxSocketServer runs
  event-driven on that thread, not on a worker thread), so no locking is
  needed here -- keep it that way if McpServer's transport ever changes.
*/
class McpTools {
public:
  McpTools(Worksheet *worksheet, Variablespane *variablesPane);

  //! The tools/list result: name/description/inputSchema for every tool below.
  nlohmann::json ListTools() const;

  //! Dispatches one tools/call request by tool name. Throws McpToolError.
  nlohmann::json CallTool(const wxString &name,
                          const nlohmann::json &arguments) const;

  nlohmann::json ListCells() const;
  nlohmann::json ReadCell(const nlohmann::json &arguments) const;
  nlohmann::json ReadWorksheet() const;
  nlohmann::json ReadToc() const;
  nlohmann::json ReadSection(const nlohmann::json &arguments) const;
  nlohmann::json ReadVariables() const;
  //! Is Maxima actively evaluating a command right now, and if so: which
  //! cell, which exact statement within that (possibly multi-statement)
  //! cell, and how long has that specific statement been running? Raised by
  //! the maintainer directly: "A way to query if Maxima is evaluating, what
  //! cell it works on, what command within that cell and for how long this
  //! command already is being evaluated."
  nlohmann::json EvaluationStatus() const;
  nlohmann::json WatchVariable(const nlohmann::json &arguments) const;
  nlohmann::json UnwatchVariable(const nlohmann::json &arguments) const;
  //! Finds every cell whose input and/or output contains `pattern` (a plain
  //! substring by default, or a regular expression with "regex":true), so an
  //! AI can jump straight to the relevant cell(s) of a large worksheet
  //! instead of reading everything via read_worksheet/list_cells. Read-only,
  //! same as every other tool here -- it never touches worksheet content.
  nlohmann::json SearchCells(const nlohmann::json &arguments) const;

  //! A cap on how much text a single response ever carries (read_worksheet/
  //! read_section), so a huge worksheet can't produce an unbounded reply.
  static constexpr std::size_t MAX_TEXT_LENGTH = 200000;
  //! search_cells stops collecting further matches once it hits this many,
  //! reporting "truncated" instead -- a pathological pattern matching most
  //! of a huge worksheet must not turn into an unbounded response either.
  static constexpr std::size_t MAX_SEARCH_MATCHES = 50;
  //! The cap applied to each individual cell's own output when it's one of
  //! many being concatenated (ReadWorksheet/ReadSection) -- keeps one cell
  //! with a huge output (a large matrix, a long list, ...) from crowding
  //! out every other cell's info in the same response. read_cell, which
  //! targets exactly one cell the caller already chose, is not limited to
  //! this -- see its output_length/output_from_end arguments.
  static constexpr std::size_t OUTPUT_PREVIEW_LENGTH = 2000;

private:
  Worksheet *m_worksheet;
  Variablespane *m_variablesPane;

  //! Finds the (still tree-attached) GroupCell with this UUID, or nullptr.
  GroupCell *FindGroupByUUID(const wxString &uuid) const;
  //! Human-readable name for a GroupType ("code", "section", ...).
  static wxString GroupTypeName(int groupType);
  //! This cell's input, as plain text (empty if it has none, e.g. an image).
  static wxString InputText(GroupCell &cell);
  //! This cell's output, as plain text (empty if it has none/isn't evaluated).
  static wxString OutputText(GroupCell &cell);
  //! {uuid, group_type, index} plus a short input preview -- used by ListCells().
  nlohmann::json CellSummary(GroupCell &cell, int index) const;
  //! One cell's "--- type (markers) ---\ninput\nOutput: ...\n\n" block, with
  //! the is_current/has_error markers and OUTPUT_PREVIEW_LENGTH capping on
  //! its own output -- shared by ReadWorksheet() and ReadSection() so a
  //! marker or the capping can't accidentally end up in only one of them.
  wxString CellText(GroupCell &cell) const;
  //! The cell the user's cursor/h-caret is currently at or after (the same
  //! cell Worksheet itself treats as the insertion point) -- or nullptr if
  //! there is no worksheet at all. Falls back to the last cell in the
  //! worksheet if nothing more specific is active, same as
  //! Worksheet::GetHCaret() itself does for its own callers.
  GroupCell *CurrentCell() const;
  //! Is this cell the one Maxima has reported an error in? (DocumentCellPointers::
  //! ErrorList, populated live by MaximaEvaluator/MaximaResponseReader --
  //! the same mechanism Worksheet::ScrollToError() already relies on.)
  bool HasError(GroupCell &cell) const;
  //! Is Maxima currently evaluating something, or does it still have queued
  //! work? A newly-watched (or already-watched) variable's value in
  //! ReadVariables() only updates once Maxima actually answers the query
  //! read_variables/watch_variable causes -- while this is true, an empty
  //! or unchanged value there may just mean "not answered yet," not "this
  //! variable is undefined."
  bool MaximaIsBusy() const;
  //! Truncates to MAX_TEXT_LENGTH, appending a note if it had to.
  static wxString CapLength(wxString text);
  //! Truncates text to at most maxLen characters -- the end if fromEnd,
  //! otherwise the start -- and reports via wasTruncated whether it had to.
  //! Shared by read_cell's output_length/output_from_end arguments and the
  //! per-cell OUTPUT_PREVIEW_LENGTH capping in ReadWorksheet()/ReadSection().
  static wxString TruncateText(const wxString &text, std::size_t maxLen,
                               bool fromEnd, bool &wasTruncated);
  //! Extracts a required string argument, or throws McpToolError.
  static wxString RequireString(const nlohmann::json &arguments,
                                const char *name);
};

#endif // MCPTOOLS_H
