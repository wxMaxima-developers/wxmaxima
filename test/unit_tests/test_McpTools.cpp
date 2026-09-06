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

SCENARIO("McpTools caps a cell's output so one huge cell can't crowd out "
        "everything else, while still letting an AI read it in full or "
        "just its tail via read_cell") {
  g_ws->ClearDocument();
  g_vars->Clear();

  GIVEN("A cell whose output is longer than the per-cell preview cap") {
    wxString hugeOutput;
    for (int i = 0; i < 500; ++i)
      hugeOutput += wxString::Format(wxS("line %d "), i); // well over 2000 chars
    REQUIRE(hugeOutput.Length() > McpTools::OUTPUT_PREVIEW_LENGTH);
    GroupCell *small = AppendCodeGroup(wxS("1+1;"), nullptr, wxS("2"));
    GroupCell *huge = AppendCodeGroup(wxS("big();"), small, hugeOutput);

    McpTools tools(g_ws, g_vars);

    WHEN("ReadCell() is called on it with no output_length") {
      huge->GenerateUUID();
      nlohmann::json result = tools.ReadCell(Args("uuid", huge->GetUUID().ToStdString()));
      THEN("it returns the full output, uncapped, since the caller named "
          "this one cell specifically") {
        CHECK(result["output"] == hugeOutput.ToStdString());
        CHECK(result["output_truncated"] == false);
      }
    }

    WHEN("ReadCell() is called with a small output_length") {
      huge->GenerateUUID();
      nlohmann::json args = {{"uuid", huge->GetUUID().ToStdString()},
                             {"output_length", 50}};
      nlohmann::json result = tools.ReadCell(args);
      THEN("it returns only the first 50 characters and reports truncation") {
        std::string output = result["output"].get<std::string>();
        CHECK(output.size() == 50);
        CHECK(output == hugeOutput.Left(50).ToStdString());
        CHECK(result["output_truncated"] == true);
      }
    }

    WHEN("ReadCell() is called with a small output_length and output_from_end") {
      huge->GenerateUUID();
      nlohmann::json args = {{"uuid", huge->GetUUID().ToStdString()},
                             {"output_length", 50},
                             {"output_from_end", true}};
      nlohmann::json result = tools.ReadCell(args);
      THEN("it returns the LAST 50 characters instead of the first") {
        std::string output = result["output"].get<std::string>();
        CHECK(output.size() == 50);
        CHECK(output == hugeOutput.Right(50).ToStdString());
        CHECK(result["output_truncated"] == true);
      }
    }

    WHEN("ReadWorksheet() is called") {
      wxString text = wxString::FromUTF8(
        tools.ReadWorksheet()["text"].get<std::string>().c_str());
      THEN("the huge cell's own output is capped to the preview length, but "
          "the small cell right after it is still fully present -- proving "
          "the huge cell didn't crowd it out") {
        CHECK(text.Contains(wxS("[truncated -- use read_cell")));
        CHECK(text.Contains(wxS("1+1;")));
        CHECK(text.Contains(wxS("Output: 2")));
      }
    }
  }
}

SCENARIO("McpTools flags the current cell and any cell with an error, so an "
        "AI reading the worksheet can answer \"what's wrong with the "
        "current cell\" / \"the cell above the cursor\"") {
  g_ws->ClearDocument();
  g_vars->Clear();

  GIVEN("Three code cells, the cursor at the second, and an error flagged "
       "on the third") {
    GroupCell *first = AppendCodeGroup(wxS("1+1;"), nullptr, wxS("2"));
    GroupCell *second = AppendCodeGroup(wxS("2+2;"), first, wxS("4"));
    GroupCell *third = AppendCodeGroup(wxS("1/0;"), second, wxS("Error"));
    g_ws->SetHCaret(second);
    g_ws->GetErrorList().Add(third);

    McpTools tools(g_ws, g_vars);

    WHEN("ListCells() is called") {
      nlohmann::json result = tools.ListCells();
      THEN("only the cursor's cell is_current, and only the erroring cell "
          "has_error") {
        REQUIRE(result["cells"].size() == 3);
        CHECK(result["cells"][0]["is_current"] == false);
        CHECK(result["cells"][0]["has_error"] == false);
        CHECK(result["cells"][1]["is_current"] == true);
        CHECK(result["cells"][1]["has_error"] == false);
        CHECK(result["cells"][2]["is_current"] == false);
        CHECK(result["cells"][2]["has_error"] == true);
      }
    }

    WHEN("ReadCell() is called on the current cell and on the erroring cell") {
      // GetUUID() is lazy -- empty until something first asks for it (see
      // McpTools::FindGroupByUUID()'s own on-demand generation) -- so
      // generate it explicitly here rather than capturing an empty string.
      second->GenerateUUID();
      third->GenerateUUID();
      nlohmann::json current = tools.ReadCell(Args("uuid", second->GetUUID().ToStdString()));
      nlohmann::json errored = tools.ReadCell(Args("uuid", third->GetUUID().ToStdString()));
      THEN("each reports its own flags correctly") {
        CHECK(current["is_current"] == true);
        CHECK(current["has_error"] == false);
        CHECK(errored["is_current"] == false);
        CHECK(errored["has_error"] == true);
      }
    }

    WHEN("ReadWorksheet() is called") {
      wxString text = wxString::FromUTF8(
        tools.ReadWorksheet()["text"].get<std::string>().c_str());
      THEN("the plain-text dump marks both inline, since that is the only "
          "context the AI chat sidebar actually sends") {
        CHECK(text.Contains(wxS("(CURRENT CELL -- the user's cursor is here)")));
        CHECK(text.Contains(wxS("(THIS CELL HAS AN ERROR)")));
        // The marked-current cell's own input is "2+2;" -- confirm the
        // marker landed on the right cell's line, not just anywhere.
        int currentMarkerPos = text.Find(wxS("(CURRENT CELL"));
        int secondInputPos = text.Find(wxS("2+2;"));
        REQUIRE(currentMarkerPos != wxNOT_FOUND);
        REQUIRE(secondInputPos != wxNOT_FOUND);
        CHECK(currentMarkerPos < secondInputPos);
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
      THEN("it reports no variables, and maxima_busy is false (nothing is "
          "queued or being evaluated)") {
        nlohmann::json result = tools.ReadVariables();
        CHECK(result["variables"].empty());
        CHECK(result["maxima_busy"] == false);
      }
    }

    WHEN("Maxima is set as currently working on a cell") {
      GroupCell *working = AppendCodeGroup(wxS("1+1;"), nullptr);
      g_ws->SetWorkingGroup(working);
      THEN("ReadVariables() reports maxima_busy true, so a tool-calling AI "
          "knows an empty/stale value might just not have arrived yet") {
        CHECK(tools.ReadVariables()["maxima_busy"] == true);
      }
      g_ws->SetWorkingGroup(nullptr); // don't leak state into later SCENARIOs
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

SCENARIO("McpTools::SearchCells() finds cells by plain substring or regex, "
        "in input and/or output, so an AI can jump straight to a match "
        "instead of reading every cell") {
  g_ws->ClearDocument();
  g_vars->Clear();

  GIVEN("Three code cells, only one of which mentions \"integrate\"") {
    GroupCell *first = AppendCodeGroup(wxS("x: 1+1;"), nullptr, wxS("2"));
    GroupCell *second =
      AppendCodeGroup(wxS("integrate(sin(x), x);"), first, wxS("-cos(x)"));
    AppendCodeGroup(wxS("y: 3;"), second, wxS("3"));

    McpTools tools(g_ws, g_vars);

    WHEN("SearchCells() is called with a plain substring in the input") {
      nlohmann::json result = tools.SearchCells(Args("pattern", "integrate"));
      THEN("it finds only that cell, and reports where it matched") {
        REQUIRE(result["matches"].size() == 1);
        CHECK(result["matches"][0]["matched_in"] == "input");
        CHECK(result["matches"][0]["group_type"] == "code");
        CHECK(result["truncated"] == false);
        std::string snippet = result["matches"][0]["match_snippet"].get<std::string>();
        CHECK(snippet.find("integrate") != std::string::npos);
      }
    }

    WHEN("SearchCells() is called with a substring only present in output") {
      nlohmann::json result = tools.SearchCells(Args("pattern", "-cos"));
      THEN("it still finds the cell, flagged as matched in output") {
        REQUIRE(result["matches"].size() == 1);
        CHECK(result["matches"][0]["matched_in"] == "output");
      }
    }

    WHEN("SearchCells() is called with different case than the text") {
      nlohmann::json result = tools.SearchCells(Args("pattern", "INTEGRATE"));
      THEN("it still matches, since case_sensitive defaults to false") {
        REQUIRE(result["matches"].size() == 1);
      }
    }

    WHEN("SearchCells() is called with case_sensitive true and wrong case") {
      nlohmann::json args = {{"pattern", "INTEGRATE"}, {"case_sensitive", true}};
      nlohmann::json result = tools.SearchCells(args);
      THEN("it finds nothing") {
        CHECK(result["matches"].empty());
      }
    }

    WHEN("SearchCells() is called with scope=\"output\" for a pattern only "
        "in that cell's input") {
      nlohmann::json args = {{"pattern", "integrate"}, {"scope", "output"}};
      nlohmann::json result = tools.SearchCells(args);
      THEN("it finds nothing, since the match was excluded by scope") {
        CHECK(result["matches"].empty());
      }
    }

    WHEN("SearchCells() is called with a regular expression") {
      nlohmann::json args = {{"pattern", "y: [0-9]+"}, {"regex", true}};
      nlohmann::json result = tools.SearchCells(args);
      THEN("it matches using regex semantics, not literal substring") {
        REQUIRE(result["matches"].size() == 1);
        CHECK(result["matches"][0]["matched_in"] == "input");
      }
    }

    WHEN("SearchCells() is given an invalid regular expression") {
      nlohmann::json args = {{"pattern", "("}, {"regex", true}};
      THEN("it throws McpToolError instead of crashing") {
        CHECK_THROWS_AS(tools.SearchCells(args), McpToolError);
      }
    }

    WHEN("SearchCells() finds no matches at all") {
      nlohmann::json result = tools.SearchCells(Args("pattern", "nonexistent_xyz"));
      THEN("it returns an empty, non-truncated match list") {
        CHECK(result["matches"].empty());
        CHECK(result["truncated"] == false);
      }
    }

    WHEN("SearchCells() is called with a missing pattern argument") {
      THEN("it throws McpToolError") {
        CHECK_THROWS_AS(tools.SearchCells(nlohmann::json::object()), McpToolError);
      }
    }
  }

  GIVEN("More cells than MAX_SEARCH_MATCHES that all match") {
    GroupCell *last = nullptr;
    for (std::size_t i = 0; i < McpTools::MAX_SEARCH_MATCHES + 5; ++i)
      last = AppendCodeGroup(wxS("needle: 1;"), last);

    McpTools tools(g_ws, g_vars);

    WHEN("SearchCells() is called") {
      nlohmann::json result = tools.SearchCells(Args("pattern", "needle"));
      THEN("it caps the result and reports truncation") {
        CHECK(result["matches"].size() == McpTools::MAX_SEARCH_MATCHES);
        CHECK(result["truncated"] == true);
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
