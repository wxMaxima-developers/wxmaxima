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

using json = nlohmann::json;

namespace {
//! wxString -> UTF-8 std::string, the only encoding nlohmann::json accepts.
std::string U8(const wxString &s) { return s.ToUTF8().data(); }
wxString FromU8(const std::string &s) {
  return wxString::FromUTF8(s.c_str());
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

json McpTools::CellSummary(GroupCell &cell, int index) {
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
  return entry;
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
                    "its input, and whether it has output."},
                   {"inputSchema", noArgs}});
  tools.push_back({{"name", "read_cell"},
                   {"description",
                    "Read one cell's full input and output text by UUID "
                    "(from list_cells/read_toc)."},
                   {"inputSchema", stringArg("The cell's UUID")}});
  tools.push_back(
    {{"name", "read_worksheet"},
     {"description",
      "Read the whole worksheet's input and output as plain text. For a "
      "large worksheet, prefer read_toc + read_section to avoid a huge "
      "response."},
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
      "level), by the heading cell's UUID (from read_toc)."},
     {"inputSchema", stringArg("The heading cell's UUID, from read_toc")}});
  tools.push_back(
    {{"name", "read_variables"},
     {"description",
      "Read the name/value pairs currently shown in the Variables sidebar's "
      "watchlist. A variable not on the watchlist isn't returned -- add it "
      "with watch_variable first."},
     {"inputSchema", noArgs}});
  tools.push_back(
    {{"name", "watch_variable"},
     {"description",
      "Add a Maxima variable name to the Variables sidebar's watchlist, the "
      "same as typing it into that sidebar by hand. Its value becomes "
      "available (once Maxima has answered) via read_variables. Does not "
      "touch worksheet content or evaluate anything."},
     {"inputSchema", nameArg("The Maxima variable name, e.g. \"x\" or \"%o3\"")}});
  tools.push_back(
    {{"name", "unwatch_variable"},
     {"description",
      "Remove a Maxima variable name from the Variables sidebar's "
      "watchlist, the same as removing it from that sidebar by hand."},
     {"inputSchema", nameArg("The Maxima variable name to stop watching")}});

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
  if (name == wxS("watch_variable"))
    return TextResult(WatchVariable(arguments));
  if (name == wxS("unwatch_variable"))
    return TextResult(UnwatchVariable(arguments));
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
  json result;
  result["uuid"] = U8(cell->GetUUID());
  result["group_type"] = U8(GroupTypeName(cell->GetGroupType()));
  result["input"] = U8(InputText(*cell));
  result["output"] = U8(OutputText(*cell));
  return result;
}

json McpTools::ReadWorksheet() const {
  wxString text;
  if (m_worksheet && m_worksheet->GetTree()) {
    for (GroupCell &cell : OnList(m_worksheet->GetTree())) {
      text += wxS("--- ") + GroupTypeName(cell.GetGroupType()) + wxS(" ---\n");
      wxString input = InputText(cell);
      if (!input.IsEmpty())
        text += input + wxS("\n");
      wxString output = OutputText(cell);
      if (!output.IsEmpty())
        text += wxS("Output: ") + output + wxS("\n");
      text += wxS("\n");
    }
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

  wxString text;
  auto appendCell = [&text](GroupCell &cell) {
    text += wxS("--- ") + GroupTypeName(cell.GetGroupType()) + wxS(" ---\n");
    wxString input = InputText(cell);
    if (!input.IsEmpty())
      text += input + wxS("\n");
    wxString output = OutputText(cell);
    if (!output.IsEmpty())
      text += wxS("Output: ") + output + wxS("\n");
    text += wxS("\n");
  };

  appendCell(*heading);
  // Same "how far does this section extend" walk GroupCell::Fold() uses:
  // everything up to (excluding) the next cell whose type is the same as, or
  // a higher heading level than, the section's own heading.
  for (GroupCell *cell = heading->GetNext(); cell; cell = cell->GetNext()) {
    if ((cell->GetGroupType() == heading->GetGroupType()) ||
        heading->IsLesserGCType(cell->GetGroupType()))
      break;
    appendCell(*cell);
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
