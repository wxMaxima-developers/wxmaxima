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

#include "McpTools.h"
#include "cells/GroupCell.h"
#include "cells/EditorCell.h"
#include "sidebars/VariablesPane.h"
#include "worksheet/Worksheet.h"
#include <wx/log.h>
#include <wx/regex.h>
#include <algorithm>
#include <memory>

using json = nlohmann::json;

namespace {
//! wxString -> UTF-8 std::string, the only encoding nlohmann::json accepts.
std::string U8(const wxString &s) { return s.ToUTF8().data(); }
wxString FromU8(const std::string &s) {
  return wxString::FromUTF8(s.c_str());
}

//! A short excerpt of `text` around [matchStart, matchStart+matchLen), with
//! "..." markers where the excerpt was cut -- lets search_cells's result show
//! *where* a match sits without dumping the cell's entire (possibly huge)
//! input/output for every hit.
wxString SearchSnippet(const wxString &text, std::size_t matchStart,
                       std::size_t matchLen) {
  const std::size_t context = 40;
  std::size_t start = (matchStart > context) ? matchStart - context : 0;
  std::size_t afterMatch = matchStart + matchLen;
  std::size_t end = std::min(text.Length(), afterMatch + context);
  wxString snippet = text.Mid(start, end - start);
  if (start > 0)
    snippet = wxS("...") + snippet;
  if (end < text.Length())
    snippet += wxS("...");
  return snippet;
}
} // namespace

McpTools::McpTools(Worksheet *worksheet, Variablespane *variablesPane)
  : m_worksheet(worksheet), m_variablesPane(variablesPane) {}

wxString McpTools::GroupTypeName(int groupType) {
  switch (groupType) {
  case GC_TYPE_CODE: return wxS("code");
  case GC_TYPE_TITLE: return wxS("title");
  case GC_TYPE_SECTION: return wxS("section");
  case GC_TYPE_SUBSECTION: return wxS("subsection");
  case GC_TYPE_SUBSUBSECTION: return wxS("subsubsection");
  case GC_TYPE_HEADING5: return wxS("heading5");
  case GC_TYPE_HEADING6: return wxS("heading6");
  case GC_TYPE_TEXT: return wxS("text");
  case GC_TYPE_IMAGE: return wxS("image");
  case GC_TYPE_PAGEBREAK: return wxS("pagebreak");
  default: return wxS("unknown");
  }
}

wxString McpTools::InputText(GroupCell &cell) {
  EditorCell *editable = cell.GetEditable();
  if (!editable)
    return wxEmptyString;
  return editable->ToString(true);
}

wxString McpTools::OutputText(GroupCell &cell) {
  Cell *output = cell.GetOutput();
  if (!output)
    return wxEmptyString;
  return output->ListToString();
}

wxString McpTools::CapLength(wxString text) {
  if (text.Length() > MAX_TEXT_LENGTH) {
    text = text.Left(MAX_TEXT_LENGTH);
    text += wxS("\n... [truncated, worksheet content exceeds the ")
      + wxString::Format(wxS("%zu"), MAX_TEXT_LENGTH)
      + wxS("-character limit of a single MCP response]");
  }
  return text;
}

wxString McpTools::TruncateText(const wxString &text, std::size_t maxLen,
                                bool fromEnd, bool &wasTruncated) {
  wasTruncated = text.Length() > maxLen;
  if (!wasTruncated)
    return text;
  return fromEnd ? text.Right(maxLen) : text.Left(maxLen);
}

wxString McpTools::RequireString(const json &arguments, const char *name) {
  auto it = arguments.find(name);
  if (it == arguments.end() || !it->is_string())
    throw McpToolError(std::string("Missing or non-string argument: ") + name);
  wxString value = FromU8(it->get<std::string>());
  if (value.IsEmpty())
    throw McpToolError(std::string("Argument must not be empty: ") + name);
  return value;
}

GroupCell *McpTools::FindGroupByUUID(const wxString &uuid) const {
  if (!m_worksheet || !m_worksheet->GetTree())
    return nullptr;
  for (GroupCell &cell : OnList(m_worksheet->GetTree())) {
    if (cell.GetUUID().IsEmpty())
      cell.GenerateUUID();
    if (cell.GetUUID() == uuid)
      return &cell;
  }
  return nullptr;
}

json McpTools::CellSummary(GroupCell &cell, int index) const {
  if (cell.GetUUID().IsEmpty())
    cell.GenerateUUID();
  wxString input = InputText(cell);
  const std::size_t previewLen = 200;
  wxString preview = input.Left(previewLen);
  if (input.Length() > previewLen)
    preview += wxS("...");
  json entry;
  entry["uuid"] = U8(cell.GetUUID());
  entry["index"] = index;
  entry["group_type"] = U8(GroupTypeName(cell.GetGroupType()));
  entry["input_preview"] = U8(preview);
  entry["has_output"] = cell.GetOutput() != nullptr;
  entry["is_current"] = (&cell == CurrentCell());
  entry["has_error"] = HasError(cell);
  return entry;
}

GroupCell *McpTools::CurrentCell() const {
  return m_worksheet ? m_worksheet->GetHCaret() : nullptr;
}

bool McpTools::HasError(GroupCell &cell) const {
  return m_worksheet && m_worksheet->GetErrorList().Contains(&cell);
}

bool McpTools::MaximaIsBusy() const {
  if (!m_worksheet)
    return false;
  // GetWorkingGroup(false) (no fallback) is null exactly when nothing is
  // currently being evaluated; a non-empty queue means there is more work
  // waiting behind whatever (if anything) is running right now.
  return (m_worksheet->GetWorkingGroup(false) != nullptr) ||
    !m_worksheet->GetEvaluationQueue().Empty();
}

json McpTools::ListTools() const {
  auto stringArg = [](const char *desc) {
    json schema;
    schema["type"] = "object";
    schema["properties"]["uuid"] = {{"type", "string"},
                                    {"description", desc}};
    schema["required"] = json::array({"uuid"});
    return schema;
  };
  auto nameArg = [](const char *desc) {
    json schema;
    schema["type"] = "object";
    schema["properties"]["name"] = {{"type", "string"},
                                    {"description", desc}};
    schema["required"] = json::array({"name"});
    return schema;
  };
  json noArgs;
  noArgs["type"] = "object";
  noArgs["properties"] = json::object();

  json tools = json::array();
  tools.push_back({{"name", "list_cells"},
                   {"description",
                    "List every cell in the worksheet, in document order: "
                    "its UUID (use with read_cell/read_section), its type "
                    "(code/text/title/section/.../image), a short preview of "
                    "its input, whether it has output, is_current (the "
                    "user's cursor is at or just after this cell -- what "
                    "\"the current cell\" or \"the cell above the cursor\" "
                    "means), and has_error (Maxima reported an error here)."},
                   {"inputSchema", noArgs}});
  json readCellSchema;
  readCellSchema["type"] = "object";
  readCellSchema["properties"]["uuid"] = {{"type", "string"},
                                          {"description", "The cell's UUID"}};
  readCellSchema["properties"]["output_length"] = {
    {"type", "integer"},
    {"description",
     "Max characters of this cell's output to return (default: no real "
     "limit, up to a 200000-character hard cap). Ask for a small value "
     "for a quick preview of a cell whose output might be huge (a large "
     "matrix, a long list, ...) instead of the whole thing."}};
  readCellSchema["properties"]["output_from_end"] = {
    {"type", "boolean"},
    {"description",
     "If true, return the LAST output_length characters instead of the "
     "first -- e.g. to check whether a long computation converged, or "
     "what its final answer was, without the potentially huge middle."}};
  readCellSchema["required"] = json::array({"uuid"});
  tools.push_back({{"name", "read_cell"},
                   {"description",
                    "Read one cell's full input and its output (optionally "
                    "capped/from-the-end via output_length/output_from_end), "
                    "plus is_current/has_error (see list_cells) and "
                    "output_truncated, by UUID (from list_cells/read_toc)."},
                   {"inputSchema", readCellSchema}});
  tools.push_back(
    {{"name", "read_worksheet"},
     {"description",
      "Read the whole worksheet's input and output as plain text. The cell "
      "the user's cursor is at or just after is marked \"(CURRENT CELL)\", "
      "and any cell Maxima reported an error in is marked \"(THIS CELL HAS "
      "AN ERROR)\", directly in the text. Each cell's own output is capped "
      "to a short preview here (marked \"[truncated -- use read_cell ...]\" "
      "when it was) so one cell with a huge output can't crowd out every "
      "other cell -- use read_cell for that one cell's full output. For a "
      "large worksheet, prefer read_toc + read_section to avoid a huge "
      "response in the first place."},
     {"inputSchema", noArgs}});
  tools.push_back(
    {{"name", "read_toc"},
     {"description",
      "Read the worksheet's table of contents: every title/section/"
      "subsection/.../heading6 cell, with its heading level (1=title, "
      "6=heading6) and its UUID (use with read_section to read that whole "
      "section)."},
     {"inputSchema", noArgs}});
  tools.push_back(
    {{"name", "read_section"},
     {"description",
      "Read one whole section's text (a heading cell and every cell under "
      "it, up to but excluding the next heading of the same or higher "
      "level), by the heading cell's UUID (from read_toc). Same per-cell "
      "output preview capping as read_worksheet -- use read_cell for one "
      "cell's full output."},
     {"inputSchema", stringArg("The heading cell's UUID, from read_toc")}});
  tools.push_back(
    {{"name", "read_variables"},
     {"description",
      "Read the name/value pairs currently shown in the Variables sidebar's "
      "watchlist. A variable not on the watchlist isn't returned -- add it "
      "with watch_variable first. Also reports maxima_busy: while true, "
      "Maxima is still evaluating something (or has queued work), so a "
      "variable's value may not have updated to reflect a query yet -- an "
      "empty value in that case likely means \"not answered yet,\" not "
      "\"undefined.\" If maxima_busy is true right after watch_variable, "
      "wait and call read_variables again rather than concluding the "
      "variable has no value; call evaluation_status for the specific "
      "reason (which cell/command, and for how long)."},
     {"inputSchema", noArgs}});
  tools.push_back(
    {{"name", "watch_variable"},
     {"description",
      "Add a Maxima variable name to the Variables sidebar's watchlist, the "
      "same as typing it into that sidebar by hand. Its value becomes "
      "available via read_variables only once Maxima actually answers the "
      "query this triggers -- not immediately, and not at all while Maxima "
      "is busy. This call's own maxima_busy field already reports whether "
      "that's the case right now, without a separate read_variables round "
      "trip; call evaluation_status for the specific reason (which cell/ "
      "command, and for how long) if it is. Does not touch worksheet "
      "content or evaluate anything."},
     {"inputSchema", nameArg("The Maxima variable name, e.g. \"x\" or \"%o3\"")}});
  tools.push_back(
    {{"name", "unwatch_variable"},
     {"description",
      "Remove a Maxima variable name from the Variables sidebar's "
      "watchlist, the same as removing it from that sidebar by hand."},
     {"inputSchema", nameArg("The Maxima variable name to stop watching")}});

  tools.push_back(
    {{"name", "evaluation_status"},
     {"description",
      "Report whether Maxima is actively evaluating a command right now: "
      "\"evaluating\" (false if it's idle -- everything else below is only "
      "present when true), the cell's uuid/is_current/has_error (see "
      "list_cells), the exact text of the specific statement currently in "
      "flight (a code cell can hold several $/;-separated statements; only "
      "one is ever sent to Maxima at a time) plus its character offset "
      "within the cell's input, how many milliseconds that one statement "
      "has been running (elapsed_ms), roughly how many more statements are "
      "left in this same cell (commands_left_in_cell), and how many cells "
      "in total still have work queued up (queue_length, which counts the "
      "cell currently evaluating too)."},
     {"inputSchema", noArgs}});

  json searchSchema;
  searchSchema["type"] = "object";
  searchSchema["properties"]["pattern"] = {
    {"type", "string"},
    {"description", "The text to search for (a plain substring by default; "
     "an extended-regular-expression pattern if regex is true)."}};
  searchSchema["properties"]["regex"] = {
    {"type", "boolean"},
    {"description", "Treat pattern as a POSIX extended regular expression "
     "instead of a plain substring. Default: false."}};
  searchSchema["properties"]["case_sensitive"] = {
    {"type", "boolean"},
    {"description", "Default: false (case-insensitive)."}};
  searchSchema["properties"]["scope"] = {
    {"type", "string"},
    {"description", "\"input\", \"output\", or \"both\" (default) -- which "
     "part of each cell to search."}};
  searchSchema["required"] = json::array({"pattern"});
  tools.push_back(
    {{"name", "search_cells"},
     {"description",
      "Find every cell whose input and/or output contains a given text or "
      "regular expression, so you can jump straight to the relevant cell(s) "
      "of a large worksheet instead of reading it all via read_worksheet/ "
      "list_cells. Returns each matching cell's UUID (use with read_cell/"
      "read_section), is_current/has_error (see list_cells), which part it "
      "matched in, and a short snippet of context around the match. Capped "
      "to 50 matches; \"truncated\" reports if there would have been more."},
     {"inputSchema", searchSchema}});

  json result;
  result["tools"] = tools;
  return result;
}

namespace {
//! Wraps a JSON value as an MCP tool result's single text content block.
json TextResult(const json &value) {
  json result;
  result["content"] = json::array({{{"type", "text"}, {"text", value.dump(2)}}});
  return result;
}
} // namespace

json McpTools::CallTool(const wxString &name, const json &arguments) const {
  if (name == wxS("list_cells"))
    return TextResult(ListCells());
  if (name == wxS("read_cell"))
    return TextResult(ReadCell(arguments));
  if (name == wxS("read_worksheet"))
    return TextResult(ReadWorksheet());
  if (name == wxS("read_toc"))
    return TextResult(ReadToc());
  if (name == wxS("read_section"))
    return TextResult(ReadSection(arguments));
  if (name == wxS("read_variables"))
    return TextResult(ReadVariables());
  if (name == wxS("evaluation_status"))
    return TextResult(EvaluationStatus());
  if (name == wxS("watch_variable"))
    return TextResult(WatchVariable(arguments));
  if (name == wxS("unwatch_variable"))
    return TextResult(UnwatchVariable(arguments));
  if (name == wxS("search_cells"))
    return TextResult(SearchCells(arguments));
  throw McpToolError("Unknown tool: " + std::string(name.ToUTF8()));
}

json McpTools::ListCells() const {
  json cells = json::array();
  if (m_worksheet && m_worksheet->GetTree()) {
    int index = 0;
    for (GroupCell &cell : OnList(m_worksheet->GetTree()))
      cells.push_back(CellSummary(cell, index++));
  }
  json result;
  result["cells"] = cells;
  return result;
}

json McpTools::ReadCell(const json &arguments) const {
  wxString uuid = RequireString(arguments, "uuid");
  GroupCell *cell = FindGroupByUUID(uuid);
  if (!cell)
    throw McpToolError("No cell with that UUID");

  // Defaults to the full MAX_TEXT_LENGTH (in practice, no real limit for
  // any normal cell) since the caller already named this one specific
  // cell -- but a pathologically huge single output (a large matrix, a
  // long list, ...) still can't exceed the same hard cap every other tool
  // respects. output_length/output_from_end let an AI that only needs a
  // preview -- or specifically the tail, e.g. "did this converge" -- ask
  // for just that instead of the whole thing.
  std::size_t maxLen = MAX_TEXT_LENGTH;
  // is_number_integer() (not is_number_unsigned()) on purpose: nlohmann
  // only classifies a value as "unsigned" if it came from something
  // already typed unsigned (e.g. text-parsed JSON with no minus sign);
  // the identical value built from a plain C++ int literal -- as any
  // hand-constructed json object, tests included, would -- is classified
  // "integer" (signed) instead despite being positive. is_number_integer()
  // covers both representations; a negative value is simply ignored below.
  if (arguments.contains("output_length") &&
      arguments["output_length"].is_number_integer()) {
    long long requested = arguments["output_length"].get<long long>();
    if (requested > 0)
      maxLen = std::min<std::size_t>(static_cast<std::size_t>(requested),
                                     MAX_TEXT_LENGTH);
  }
  bool fromEnd = arguments.contains("output_from_end") &&
    arguments["output_from_end"].is_boolean() &&
    arguments["output_from_end"].get<bool>();
  bool truncated = false;
  wxString output = TruncateText(OutputText(*cell), maxLen, fromEnd, truncated);

  json result;
  result["uuid"] = U8(cell->GetUUID());
  result["group_type"] = U8(GroupTypeName(cell->GetGroupType()));
  result["input"] = U8(InputText(*cell));
  result["output"] = U8(output);
  result["output_truncated"] = truncated;
  result["is_current"] = (cell == CurrentCell());
  result["has_error"] = HasError(*cell);
  return result;
}

wxString McpTools::CellText(GroupCell &cell) const {
  wxString text = wxS("--- ") + GroupTypeName(cell.GetGroupType());
  // Flagged inline, not just in the JSON-returning tools, so the same
  // information reaches an AI reading only this plain-text dump -- e.g.
  // the in-app AI chat sidebar, which sends this text as its entire
  // worksheet context and has no separate way to ask "where is the
  // user's cursor" or "which cell errored."
  if (&cell == CurrentCell())
    text += wxS(" (CURRENT CELL -- the user's cursor is here)");
  if (HasError(cell))
    text += wxS(" (THIS CELL HAS AN ERROR)");
  text += wxS(" ---\n");
  wxString input = InputText(cell);
  if (!input.IsEmpty())
    text += input + wxS("\n");
  // Capped per cell (unlike read_cell, which the caller uses precisely
  // because they want one specific cell's full output): a single cell
  // with a huge output must not crowd out every other cell sharing this
  // same response. read_cell(uuid, output_length, output_from_end) is the
  // way to actually read a capped-here cell's output in full, or its tail.
  bool truncated = false;
  wxString output =
    TruncateText(OutputText(cell), OUTPUT_PREVIEW_LENGTH, false, truncated);
  if (!output.IsEmpty()) {
    text += wxS("Output: ") + output;
    if (truncated)
      text += wxS(" ... [truncated -- use read_cell with this cell's UUID "
                  "for the full output]");
    text += wxS("\n");
  }
  text += wxS("\n");
  return text;
}

json McpTools::ReadWorksheet() const {
  wxString text;
  if (m_worksheet && m_worksheet->GetTree()) {
    for (GroupCell &cell : OnList(m_worksheet->GetTree()))
      text += CellText(cell);
  }
  json result;
  result["text"] = U8(CapLength(text));
  return result;
}

json McpTools::ReadToc() const {
  json entries = json::array();
  if (m_worksheet && m_worksheet->GetTree()) {
    for (GroupCell &cell : OnList(m_worksheet->GetTree())) {
      if (!cell.IsHeading())
        continue;
      if (cell.GetUUID().IsEmpty())
        cell.GenerateUUID();
      json entry;
      entry["uuid"] = U8(cell.GetUUID());
      entry["level"] = static_cast<int>(cell.GetGroupType());
      entry["text"] = U8(InputText(cell));
      entries.push_back(entry);
    }
  }
  json result;
  result["entries"] = entries;
  return result;
}

json McpTools::ReadSection(const json &arguments) const {
  wxString uuid = RequireString(arguments, "uuid");
  GroupCell *heading = FindGroupByUUID(uuid);
  if (!heading)
    throw McpToolError("No cell with that UUID");
  if (!heading->IsHeading())
    throw McpToolError("That cell is not a heading (title/section/.../heading6)");

  wxString text = CellText(*heading);
  // Same "how far does this section extend" walk GroupCell::Fold() uses:
  // everything up to (excluding) the next cell whose type is the same as, or
  // a higher heading level than, the section's own heading.
  for (GroupCell *cell = heading->GetNext(); cell; cell = cell->GetNext()) {
    if ((cell->GetGroupType() == heading->GetGroupType()) ||
        heading->IsLesserGCType(cell->GetGroupType()))
      break;
    text += CellText(*cell);
  }

  json result;
  result["uuid"] = U8(heading->GetUUID());
  result["text"] = U8(CapLength(text));
  return result;
}

json McpTools::ReadVariables() const {
  json variables = json::array();
  if (m_variablesPane) {
    for (const auto &nameValue : m_variablesPane->GetWatchedValues()) {
      json entry;
      entry["name"] = U8(nameValue.first);
      entry["value"] = U8(nameValue.second);
      variables.push_back(entry);
    }
  }
  json result;
  result["variables"] = variables;
  // A variable's value only updates once Maxima actually answers the query
  // watching it triggers -- while Maxima is busy, an empty or stale-looking
  // value here can just mean "no answer yet," not "undefined." Surfaced
  // explicitly so a tool-calling AI doesn't misread the difference.
  result["maxima_busy"] = MaximaIsBusy();
  return result;
}

json McpTools::EvaluationStatus() const {
  json result;
  if (!m_worksheet) {
    result["evaluating"] = false;
    result["queue_length"] = 0;
    return result;
  }
  EvaluationQueue &queue = m_worksheet->GetEvaluationQueue();
  // GetWorkingGroup(false) (no fallback) is the cell Maxima is actually
  // waiting on an answer for right now -- null the instant nothing is
  // in flight, even if the queue still has more cells waiting behind it
  // (same distinction MaximaIsBusy() already relies on).
  GroupCell *working = m_worksheet->GetWorkingGroup(false);
  result["evaluating"] = (working != nullptr);
  result["queue_length"] = queue.Size();
  if (working) {
    if (working->GetUUID().IsEmpty())
      working->GenerateUUID();
    result["cell_uuid"] = U8(working->GetUUID());
    result["is_current"] = (working == CurrentCell());
    result["has_error"] = HasError(*working);
    result["command"] = U8(queue.GetCommand());
    result["command_index_in_cell"] = queue.GetIndex();
    // -1 means "no command has actually been sent since the queue last
    // advanced" -- shouldn't normally happen while working != nullptr, but
    // omitting the field entirely rather than reporting a bogus 0 keeps
    // that (hopefully impossible) case honest.
    long elapsedMs = queue.GetCommandElapsedMilliseconds();
    if (elapsedMs >= 0)
      result["elapsed_ms"] = elapsedMs;
    result["commands_left_in_cell"] = queue.CommandsLeftInCell();
  }
  return result;
}

json McpTools::WatchVariable(const json &arguments) const {
  wxString name = RequireString(arguments, "name");
  if (!m_variablesPane)
    throw McpToolError("No Variables sidebar available");
  if (!Variablespane::IsValidVariable(name))
    throw McpToolError("Not a valid Maxima variable name");
  m_variablesPane->AddWatch(name);
  json result;
  result["ok"] = true;
  result["name"] = U8(name);
  // Reported here too, not just from read_variables: without it, an AI
  // that calls watch_variable then immediately reads back an empty value
  // has no way to tell "not answered yet" apart from "genuinely
  // undefined" without a second round trip it might not think to make.
  result["maxima_busy"] = MaximaIsBusy();
  return result;
}

json McpTools::UnwatchVariable(const json &arguments) const {
  wxString name = RequireString(arguments, "name");
  if (!m_variablesPane)
    throw McpToolError("No Variables sidebar available");
  m_variablesPane->RemoveWatch(name);
  json result;
  result["ok"] = true;
  result["name"] = U8(name);
  return result;
}

json McpTools::SearchCells(const json &arguments) const {
  wxString pattern = RequireString(arguments, "pattern");
  bool useRegex = arguments.contains("regex") && arguments["regex"].is_boolean() &&
    arguments["regex"].get<bool>();
  bool caseSensitive = arguments.contains("case_sensitive") &&
    arguments["case_sensitive"].is_boolean() && arguments["case_sensitive"].get<bool>();
  wxString scope = wxS("both");
  if (arguments.contains("scope") && arguments["scope"].is_string())
    scope = FromU8(arguments["scope"].get<std::string>());
  bool searchInput = scope != wxS("output");
  bool searchOutput = scope != wxS("input");

  // Suppresses any wxLogXXX a bad pattern or a match attempt might trigger --
  // an unhandled one could otherwise surface as a modal wxLogGui popup (see
  // AGENTS.md's "wxLogMessage/wxLogWarning/wxLogError are NOT reliably
  // visible" entry) from what is, from the caller's perspective, an ordinary
  // JSON-RPC error response, not something that should ever touch the GUI.
  wxLogNull suppressLogging;
  std::unique_ptr<wxRegEx> regex;
  if (useRegex) {
    regex = std::make_unique<wxRegEx>(pattern, caseSensitive ? wxRE_DEFAULT : wxRE_ICASE);
    if (!regex->IsValid())
      throw McpToolError("Invalid regular expression");
  }
  wxString patternLower = pattern.Lower();

  // Returns true and fills `snippet` if `text` contains a match; false
  // (leaving `snippet` untouched) otherwise.
  auto matchesText = [&](const wxString &text, wxString &snippet) -> bool {
    if (text.IsEmpty())
      return false;
    if (useRegex) {
      if (!regex->Matches(text))
        return false;
      std::size_t start = 0;
      std::size_t len = 0;
      regex->GetMatch(&start, &len, 0);
      snippet = SearchSnippet(text, start, len);
      return true;
    }
    const wxString &haystack = caseSensitive ? text : text.Lower();
    const wxString &needle = caseSensitive ? pattern : patternLower;
    int pos = haystack.Find(needle);
    if (pos == wxNOT_FOUND)
      return false;
    snippet = SearchSnippet(text, static_cast<std::size_t>(pos), needle.Length());
    return true;
  };

  json matches = json::array();
  bool truncated = false;
  if (m_worksheet && m_worksheet->GetTree()) {
    int index = 0;
    for (GroupCell &cell : OnList(m_worksheet->GetTree())) {
      int idx = index++;
      wxString snippet;
      wxString matchedIn;
      if (searchInput && matchesText(InputText(cell), snippet))
        matchedIn = wxS("input");
      else if (searchOutput && matchesText(OutputText(cell), snippet))
        matchedIn = wxS("output");
      else
        continue;
      if (matches.size() >= MAX_SEARCH_MATCHES) {
        truncated = true;
        break;
      }
      json entry = CellSummary(cell, idx);
      entry["matched_in"] = U8(matchedIn);
      entry["match_snippet"] = U8(snippet);
      matches.push_back(entry);
    }
  }

  json result;
  result["matches"] = matches;
  result["truncated"] = truncated;
  return result;
}
