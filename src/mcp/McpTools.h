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
  nlohmann::json WatchVariable(const nlohmann::json &arguments) const;
  nlohmann::json UnwatchVariable(const nlohmann::json &arguments) const;

  //! A cap on how much text a single response ever carries (read_worksheet/
  //! read_section), so a huge worksheet can't produce an unbounded reply.
  static constexpr std::size_t MAX_TEXT_LENGTH = 200000;

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
  static nlohmann::json CellSummary(GroupCell &cell, int index);
  //! Truncates to MAX_TEXT_LENGTH, appending a note if it had to.
  static wxString CapLength(wxString text);
  //! Extracts a required string argument, or throws McpToolError.
  static wxString RequireString(const nlohmann::json &arguments,
                                const char *name);
};

#endif // MCPTOOLS_H
