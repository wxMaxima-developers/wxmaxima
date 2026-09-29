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
  Reaching a cell's output with the keyboard (GH #2382).

  Down at the end of a cell's input used to leave the cell, so its output
  could only be selected with the mouse. Now Down steps into the output, one
  result at a time, and Up walks back. These tests send the key events to the
  worksheet's own key handler and check what ends up selected. The output is
  parsed from the XML Maxima sends, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/GroupCell.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
} // namespace

// An output with a label and an expression, as Maxima sends it
static wxString Result(int n, const wxString &math) {
  return wxString::Format(wxS("<mth><lbl altCopy=\"%%o%d\">(%%o%d) </lbl>"), n, n) +
    math + wxS("</mth>");
}

// Puts a code cell with this input and these outputs into the (otherwise
// empty) worksheet, followed by an empty code cell, and lays them out.
static GroupCell *ShowCell(const wxString &input,
                           const std::vector<wxString> &outputs) {
  g_ws->DestroyTree();
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, input);
  MathParser parser(g_cfg);
  parser.SetGroup(group.get());
  for (const auto &output : outputs)
    group->AppendOutput(parser.ParseLine(output));
  g_ws->InsertGroupCells(std::move(group), nullptr, nullptr);
  g_ws->InsertGroupCells(std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("b;")),
                         g_ws->GetTree(), nullptr);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetTree() != nullptr);
  return g_ws->GetTree();
}

// Sends a key to the worksheet as if it had been typed
static void Press(int keyCode) {
  wxKeyEvent event(wxEVT_CHAR);
  event.m_keyCode = keyCode;
  g_ws->OnChar(event);
}

// Puts the text cursor at the end of the cell's input
static void CursorToEndOfInput(GroupCell *group) {
  g_ws->SetHCaret(nullptr);
  g_ws->SetActiveCell(group->GetEditable());
  group->GetEditable()->CaretToEnd();
}

static const DocumentCellPointers &Pointers() {
  return g_ws->GetDocumentCellPointers();
}

SCENARIO("A cell's output is split into results") {
  GIVEN("a cell with two results") {
    GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                              Result(2, wxS("<mi>b</mi>"))});
    const auto results = group->GetOutputResults();
    THEN("each result is a label and its expression") {
      REQUIRE(results.size() == 2);
      CHECK(results[0].first->ToString().Contains(wxS("%o1")));
      CHECK(results[0].last->ToString().Trim() == wxS("a"));
      CHECK(results[1].first->ToString().Contains(wxS("%o2")));
      CHECK(results[1].last->ToString().Trim() == wxS("b"));
    }
    AND_WHEN("the output is hidden") {
      group->Hide(true);
      THEN("there is nothing to step into") {
        CHECK(group->GetOutputResults().empty());
      }
      group->Hide(false);
    }
  }
  GIVEN("a cell without output") {
    GroupCell *group = ShowCell(wxS("a$"), {});
    THEN("it has no results") {
      CHECK(group->GetOutputResults().empty());
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Up and Down walk through a cell's output (GH #2382)") {
  GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                            Result(2, wxS("<mi>b</mi>"))});
  const auto results = group->GetOutputResults();
  REQUIRE(results.size() == 2);

  WHEN("Down is pressed at the end of the cell's input") {
    CursorToEndOfInput(group);
    Press(WXK_DOWN);
    THEN("the first result is selected and the input is left") {
      CHECK(g_ws->GetActiveCell() == nullptr);
      CHECK(Pointers().GetSelectionStart() == results[0].first);
      CHECK(Pointers().GetSelectionEnd() == results[0].last);
      CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
      CHECK(g_ws->GetString().Contains(wxS("a")));
    }
    AND_WHEN("Down is pressed again") {
      Press(WXK_DOWN);
      THEN("the second result is selected") {
        CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(1));
        CHECK(Pointers().GetSelectionStart() == results[1].first);
      }
      AND_WHEN("Down is pressed past the last result") {
        Press(WXK_DOWN);
        THEN("the horizontal cursor sits below the cell, as before") {
          CHECK_FALSE(Pointers().GetSelectionStart());
          CHECK(g_ws->HCaretActive());
          CHECK(g_ws->GetHCaret() == group);
        }
        AND_WHEN("Up is pressed there") {
          Press(WXK_UP);
          THEN("the last result is selected again") {
            CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(1));
          }
        }
      }
      AND_WHEN("Up is pressed twice") {
        Press(WXK_UP);
        THEN("the first press goes back to the first result") {
          CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
        }
        Press(WXK_UP);
        THEN("the second one goes back into the input, at its end") {
          CHECK_FALSE(Pointers().GetSelectionStart());
          CHECK(g_ws->GetActiveCell() == group->GetEditable());
          CHECK(group->GetEditable()->CaretAtEnd());
        }
      }
    }
  }

  WHEN("Down is pressed in the input, but not at its end") {
    CursorToEndOfInput(group);
    group->GetEditable()->CaretToStart();
    Press(WXK_DOWN);
    THEN("the cursor stays in the input") {
      CHECK(g_ws->GetActiveCell() == group->GetEditable());
      CHECK_FALSE(Pointers().GetSelectionStart());
    }
  }

  WHEN("a part of a result was selected with the mouse") {
    // A mouse selection leaves neither cursor active: selecting a whole
    // result gets there, then narrowing it makes it only part of one.
    g_ws->SelectOutputResult(group, 0);
    g_ws->SetSelection(results[0].last);
    REQUIRE_FALSE(g_ws->SelectedOutputResult());
    Press(WXK_DOWN);
    THEN("Down leaves the cell, as it always did") {
      CHECK(g_ws->HCaretActive());
      CHECK(g_ws->GetHCaret() == group);
    }
  }

  g_ws->DestroyTree();
}

SCENARIO("Down leaves a cell that has no output, as before") {
  GroupCell *group = ShowCell(wxS("a$"), {});
  CursorToEndOfInput(group);
  Press(WXK_DOWN);
  THEN("the horizontal cursor sits below the cell") {
    CHECK_FALSE(Pointers().GetSelectionStart());
    CHECK(g_ws->HCaretActive());
    CHECK(g_ws->GetHCaret() == group);
  }
  AND_WHEN("Up is pressed there") {
    Press(WXK_UP);
    THEN("the cursor goes back into the input") {
      CHECK(g_ws->GetActiveCell() == group->GetEditable());
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Up and Down can be told to skip the output, as they used to") {
  GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                            Result(2, wxS("<mi>b</mi>"))});
  g_cfg->ArrowKeysSkipOutput(true);

  WHEN("Down is pressed at the end of the cell's input") {
    CursorToEndOfInput(group);
    Press(WXK_DOWN);
    THEN("the output is skipped and the horizontal cursor sits below the cell") {
      CHECK_FALSE(Pointers().GetSelectionStart());
      CHECK(g_ws->HCaretActive());
      CHECK(g_ws->GetHCaret() == group);
    }
    AND_WHEN("Up is pressed there") {
      Press(WXK_UP);
      THEN("the cursor goes back into the input, not into the output") {
        CHECK_FALSE(Pointers().GetSelectionStart());
        CHECK(g_ws->GetActiveCell() == group->GetEditable());
      }
    }
  }

  g_cfg->ArrowKeysSkipOutput(false);
  g_ws->DestroyTree();
}

SCENARIO("By default Up and Down don't skip the output") {
  // Temporary, so it doesn't write its settings back to the config file
  Configuration cfg(g_dc, Configuration::temporary);
  cfg.ArrowKeysSkipOutput(true);
  cfg.ResetAllToDefaults();
  CHECK_FALSE(cfg.ArrowKeysSkipOutput());
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

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  // Whatever the config file says, show code cells, so the input is reachable
  g_cfg->ShowCodeCells(true);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
