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
  Pins McpTools (the MCP server's read-only "worksheet context" tools,
  src/mcp/McpTools.h) against a real, headless Worksheet/Variablespane -- no
  live Maxima, no sockets, no JSON-RPC framing (that's McpServer's job, and
  it was instead verified live in a real Xvfb session with curl, see
  AGENTS.md's MCP server entry).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <algorithm>
#include <vector>

#include "Configuration.h"
#include "mcp/McpTools.h"
#include "sidebars/VariablesPane.h"
#include "worksheet/Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
Variablespane *g_vars = nullptr;

//! A code group with the given input and, optionally, an output -- a real
//! Maxima response's output always starts with a label cell ("(%o1)")
//! GroupCell::GetOutput() deliberately skips (see its own doc comment); to
//! exercise that same "skip the label" path, chain a label cell ahead of the
//! actual output text the same way, via SetOutput() then AppendOutput().
GroupCell *AppendCodeGroup(const wxString &code, GroupCell *after,
                          const wxString &outputText = wxEmptyString) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, code);
  if (!outputText.IsEmpty()) {
    group->SetOutput(std::make_unique<TextCell>(group.get(), g_cfg, wxS("(%o1)")));
    group->AppendOutput(std::make_unique<TextCell>(group.get(), g_cfg, outputText));
  }
  return g_ws->InsertGroupCells(std::move(group), after);
}

//! A heading group of the given type (title/section/.../heading6).
GroupCell *AppendHeadingGroup(GroupType type, const wxString &text,
                             GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, text);
  return g_ws->InsertGroupCells(std::move(group), after);
}

//! A JSON object argument, e.g. Args({"uuid", uuid.ToStdString()}).
nlohmann::json Args(std::string key, const std::string &value) {
  nlohmann::json j;
  j[key] = value;
  return j;
}
} // namespace

SCENARIO("McpTools::ListCells() and ReadCell() reflect the real worksheet") {
  g_ws->ClearDocument();
  g_vars->Clear();

  GIVEN("A worksheet with a title, a section and two code cells") {
    GroupCell *title = AppendHeadingGroup(GC_TYPE_TITLE, wxS("My Title"), nullptr);
    GroupCell *section =
      AppendHeadingGroup(GC_TYPE_SECTION, wxS("My Section"), title);
    GroupCell *codeNoOutput = AppendCodeGroup(wxS("1+1;"), section);
    GroupCell *codeWithOutput =
      AppendCodeGroup(wxS("2+2;"), codeNoOutput, wxS("4"));

    McpTools tools(g_ws, g_vars);

    WHEN("ListCells() is called") {
      nlohmann::json result = tools.ListCells();
      THEN("it lists every cell, in document order, with a real UUID") {
        REQUIRE(result["cells"].size() == 4);
        CHECK(result["cells"][0]["group_type"] == "title");
        CHECK(result["cells"][0]["input_preview"] == "My Title");
        CHECK(result["cells"][1]["group_type"] == "section");
        CHECK(result["cells"][2]["group_type"] == "code");
        CHECK(result["cells"][2]["has_output"] == false);
        CHECK(result["cells"][3]["has_output"] == true);
        // Every listed cell must have gotten a real, non-empty UUID.
        for (const auto &cell : result["cells"])
          CHECK(!cell["uuid"].get<std::string>().empty());
      }
    }

    WHEN("ReadCell() is called with a real cell's UUID") {
      wxString uuid = codeWithOutput->GetUUID();
      if (uuid.IsEmpty()) {
        codeWithOutput->GenerateUUID();
        uuid = codeWithOutput->GetUUID();
      }
      nlohmann::json result =
        tools.ReadCell(Args("uuid", uuid.ToStdString()));
      THEN("it returns that cell's full input and output") {
        CHECK(result["group_type"] == "code");
        CHECK(result["input"] == "2+2;");
        CHECK(result["output"] == "4");
      }
    }

    WHEN("ReadCell() is called with an unknown UUID") {
      THEN("it throws McpToolError, not a crash or a silent wrong answer") {
        CHECK_THROWS_AS(tools.ReadCell(Args("uuid", "not-a-real-uuid")),
                        McpToolError);
      }
    }

    WHEN("ReadCell() is called with a missing uuid argument") {
      THEN("it throws McpToolError") {
        CHECK_THROWS_AS(tools.ReadCell(nlohmann::json::object()), McpToolError);
      }
    }
  }
}

SCENARIO("McpTools::ReadToc() and ReadSection() follow the same section "
        "boundaries GroupCell::Fold() does") {
  g_ws->ClearDocument();
  g_vars->Clear();

  GIVEN("Two sections, the first with a subsection nested inside it") {
    GroupCell *section1 =
      AppendHeadingGroup(GC_TYPE_SECTION, wxS("Section 1"), nullptr);
    GroupCell *sub1 =
      AppendHeadingGroup(GC_TYPE_SUBSECTION, wxS("Sub 1.1"), section1);
    GroupCell *codeInSub = AppendCodeGroup(wxS("a: 1;"), sub1);
    GroupCell *section2 =
      AppendHeadingGroup(GC_TYPE_SECTION, wxS("Section 2"), codeInSub);
    AppendCodeGroup(wxS("b: 2;"), section2);

    McpTools tools(g_ws, g_vars);

    WHEN("ReadToc() is called") {
      nlohmann::json result = tools.ReadToc();
      THEN("it lists only the heading cells, with their nesting level") {
        REQUIRE(result["entries"].size() == 3);
        CHECK(result["entries"][0]["text"] == "Section 1");
        CHECK(result["entries"][0]["level"] == static_cast<int>(GC_TYPE_SECTION));
        CHECK(result["entries"][1]["text"] == "Sub 1.1");
        CHECK(result["entries"][1]["level"] ==
             static_cast<int>(GC_TYPE_SUBSECTION));
        CHECK(result["entries"][2]["text"] == "Section 2");
      }
    }

    WHEN("ReadSection() is called on the outer section (Section 1)") {
      if (section1->GetUUID().IsEmpty())
        section1->GenerateUUID();
      nlohmann::json result =
        tools.ReadSection(Args("uuid", section1->GetUUID().ToStdString()));
      THEN("it includes the nested subsection and its code, but stops "
          "before Section 2") {
        std::string text = result["text"].get<std::string>();
        CHECK(text.find("Section 1") != std::string::npos);
        CHECK(text.find("Sub 1.1") != std::string::npos);
        CHECK(text.find("a: 1;") != std::string::npos);
        CHECK(text.find("Section 2") == std::string::npos);
        CHECK(text.find("b: 2;") == std::string::npos);
      }
    }

    WHEN("ReadSection() is called on a non-heading cell") {
      if (codeInSub->GetUUID().IsEmpty())
        codeInSub->GenerateUUID();
      THEN("it throws McpToolError") {
        CHECK_THROWS_AS(
          tools.ReadSection(Args("uuid", codeInSub->GetUUID().ToStdString())),
          McpToolError);
      }
    }
  }
}

SCENARIO("McpTools' variable watchlist tools only ever touch the sidebar, "
        "never worksheet content") {
  g_ws->ClearDocument();
  g_vars->Clear();
  McpTools tools(g_ws, g_vars);

  GIVEN("An empty watchlist") {
    WHEN("ReadVariables() is called") {
      THEN("it reports no variables") {
        CHECK(tools.ReadVariables()["variables"].empty());
      }
    }

    WHEN("WatchVariable() adds a valid variable name") {
      nlohmann::json result = tools.WatchVariable(Args("name", "myvar"));
      THEN("it reports success and the sidebar now tracks it") {
        CHECK(result["ok"] == true);
        std::vector<wxString> names = g_vars->GetVarnames();
        CHECK(std::find(names.begin(), names.end(), wxS("myvar")) !=
             names.end());
      }
      AND_THEN("ReadVariables() reports it (Maxima isn't running, so its "
              "value is whatever the sidebar currently shows)") {
        nlohmann::json vars = tools.ReadVariables();
        REQUIRE(vars["variables"].size() == 1);
        CHECK(vars["variables"][0]["name"] == "myvar");
      }
      AND_THEN("UnwatchVariable() removes it again") {
        tools.UnwatchVariable(Args("name", "myvar"));
        CHECK(tools.ReadVariables()["variables"].empty());
      }
    }

    WHEN("WatchVariable() is given an invalid variable name") {
      THEN("it throws McpToolError rather than silently adding garbage") {
        CHECK_THROWS_AS(tools.WatchVariable(Args("name", "1bad:name")),
                        McpToolError);
      }
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  g_vars = new Variablespane(g_frame, wxID_ANY);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
