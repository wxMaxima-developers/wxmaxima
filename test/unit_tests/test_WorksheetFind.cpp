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
  Pins the worksheet's find/replace behavior before the search engine is
  extracted into WorksheetSearch.

  Worksheet::FindNext walks the group cells in a wrap-around loop, checking
  each group's prompt, editor and output (in reverse order when searching
  upwards), with skip rules in the start group so a search continues from
  the cursor or the previous match. These scenarios pin that behavior -
  including its quirks - so the extraction cannot change it.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
} // namespace

static void EnsureDisplay() {
#ifndef _WIN32
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
}

//! A code group with the given input and a one-cell text output.
static GroupCell *AppendCodeGroup(const wxString &code,
                                  const wxString &outputText,
                                  GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, code);
  // The group must be set at construction: m_group is only ever assigned in
  // the Cell constructor; SetOutput does not re-parent cells.
  group->SetOutput(
    std::make_unique<TextCell>(group.get(), g_cfg, outputText));
  return g_ws->InsertGroupCells(std::move(group), after);
}

/*! A fresh three-cell document; returns the first cell.

  The first editor holds two occurrences of "alpha_" for the
  repeated-search-within-one-editor scenarios.
*/
static GroupCell *BuildDocument() {
  g_ws->ClearDocument();
  GroupCell *first = AppendCodeGroup(wxS("alpha_variable : alpha_value;"),
                                     wxS("output_alpha"), nullptr);
  GroupCell *second = AppendCodeGroup(wxS("beta_variable : 2;"),
                                      wxS("output_beta"), first);
  AppendCodeGroup(wxS("gamma_variable : 3;"), wxS("output_gamma"), second);
  g_ws->SetActiveCell(nullptr);
  g_ws->ClearSelection();
  g_ws->RecalculateIfNeeded();
  return first;
}

SCENARIO("Searching down finds a match in an editor cell and selects it") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();

  REQUIRE(g_ws->FindNext(wxS("beta_variable"), true, true, true, true, false));

  THEN("the match's editor is active and holds the match as its selection") {
    REQUIRE(g_ws->GetActiveCell() == second->GetEditable());
    REQUIRE(g_ws->GetActiveCell()->GetSelectionString() == wxS("beta_variable"));
  }
}

SCENARIO("Searching finds a match in an output cell and selects the cell") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();

  REQUIRE(g_ws->FindNext(wxS("output_beta"), true, true, true, true, false));

  THEN("the output cell is selected; no editor is active") {
    REQUIRE(g_ws->GetSelectionStart() == second->GetLabel());
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("Searching finds a match in a prompt") {
  GroupCell *first = BuildDocument();
  REQUIRE(first->GetPrompt() != nullptr);
  REQUIRE(first->GetPrompt()->ToString().Contains(wxS("-->")));

  REQUIRE(g_ws->FindNext(wxS("-->"), true, true, true, true, false));

  THEN("the first group's prompt is selected") {
    REQUIRE(g_ws->GetSelectionStart() == first->GetPrompt());
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("A repeated search advances past a prompt match") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  REQUIRE(g_ws->FindNext(wxS("-->"), true, true, true, true, false));
  REQUIRE(g_ws->GetSelectionStart() == first->GetPrompt());

  REQUIRE(g_ws->FindNext(wxS("-->"), true, true, true, true, false));

  THEN("the second search moves on to the second group's prompt") {
    REQUIRE(g_ws->GetSelectionStart() == second->GetPrompt());
  }
}

SCENARIO("A search continues in the current group after the active cursor") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  GroupCell *third = second->GetNext();
  g_ws->SetActiveCell(second->GetEditable());

  // "output_" exists in every group's output; the cursor sits in the second
  // group's editor, and going down the output comes after the editor.
  REQUIRE(g_ws->FindNext(wxS("output_"), true, true, true, true, false));

  THEN("the match is the second group's own output, not the first group's") {
    REQUIRE(g_ws->GetSelectionStart() == second->GetLabel());
  }

  THEN("searching again moves on to the third group's output") {
    REQUIRE(g_ws->FindNext(wxS("output_"), true, true, true, true, false));
    REQUIRE(g_ws->GetSelectionStart() == third->GetLabel());
  }
}

SCENARIO("A search down from the h-caret starts at the group below it") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  g_ws->SetHCaret(first);

  // "variable" exists in every editor; the caret below the first group makes
  // the second group's match the first one found.
  REQUIRE(g_ws->FindNext(wxS("variable"), true, true, true, true, false));

  THEN("the match is in the second group's editor") {
    REQUIRE(g_ws->GetActiveCell() == second->GetEditable());
  }
}

SCENARIO("A selected cell is the search start and is itself skipped") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  GroupCell *third = second->GetNext();
  g_ws->SetSelection(second->GetLabel(), second->GetLabel());

  REQUIRE(g_ws->FindNext(wxS("output_"), true, true, true, true, false));

  THEN("the match is in the group after the selected output cell") {
    REQUIRE(g_ws->GetSelectionStart() == third->GetLabel());
  }
}

SCENARIO("A search wraps around the end of the document") {
  GroupCell *first = BuildDocument();
  GroupCell *last = g_ws->GetLastCellInWorksheet();
  g_ws->SetHCaret(last);

  REQUIRE(g_ws->FindNext(wxS("alpha_variable"), true, true, true, true, false));

  THEN("the match in the first group is found") {
    REQUIRE(g_ws->GetActiveCell() == first->GetEditable());
  }
}

SCENARIO("Searching up checks the output before the editor") {
  GroupCell *first = BuildDocument();
  GroupCell *third = first->GetNext()->GetNext();
  g_ws->SetHCaret(third);

  // "gamma" occurs in the third group's editor AND its output; going up the
  // output is checked first.
  REQUIRE(g_ws->FindNext(wxS("gamma"), false, true, true, true, false));

  THEN("the output match wins") {
    REQUIRE(g_ws->GetSelectionStart() == third->GetLabel());
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("Searching up finds editor matches in reverse document order") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  GroupCell *third = second->GetNext();
  g_ws->SetHCaret(third);

  REQUIRE(g_ws->FindNext(wxS("variable"), false, true, true, true, false));

  THEN("the third group's editor matches first") {
    REQUIRE(g_ws->GetActiveCell() == third->GetEditable());
  }
}

SCENARIO("Case sensitivity is honored") {
  BuildDocument();

  THEN("a case-sensitive search does not cross case") {
    REQUIRE_FALSE(
      g_ws->FindNext(wxS("BETA_VARIABLE"), true, false, true, true, false));
  }
  THEN("a case-insensitive search does") {
    REQUIRE(
      g_ws->FindNext(wxS("BETA_VARIABLE"), true, true, true, true, false));
  }
}

SCENARIO("The input/output filters restrict where matches are found") {
  BuildDocument();

  THEN("with searchInInput=false an editor match is not found") {
    REQUIRE_FALSE(
      g_ws->FindNext(wxS("beta_variable"), true, true, false, true, false));
  }
  THEN("with searchInOutput=false an output match is not found") {
    REQUIRE_FALSE(
      g_ws->FindNext(wxS("output_beta"), true, true, true, false, false));
  }
}

SCENARIO("Repeated searches cycle through the matches inside one editor") {
  GroupCell *first = BuildDocument();

  // Two occurrences of "alpha_" in the first editor.
  REQUIRE(g_ws->FindNext(wxS("alpha_"), true, true, true, true, false));
  REQUIRE(g_ws->GetActiveCell() == first->GetEditable());
  const size_t firstMatchPos = g_ws->GetActiveCell()->GetCaretPosition();

  THEN("the second search selects the later occurrence in the same editor") {
    REQUIRE(g_ws->FindNext(wxS("alpha_"), true, true, true, true, false));
    REQUIRE(g_ws->GetActiveCell() == first->GetEditable());
    REQUIRE(g_ws->GetActiveCell()->GetCaretPosition() > firstMatchPos);

    // Quirk, pinned as-is: after the last occurrence the search does NOT
    // wrap back to the part of the start group that lies before the cursor -
    // the wrap-around loop stops when it reaches the start group again.
    THEN("past the last occurrence the search reports no further match") {
      REQUIRE_FALSE(g_ws->FindNext(wxS("alpha_"), true, true, true, true, false));
    }
  }
}

SCENARIO("A regex search finds matches in editors and outputs") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  GroupCell *third = second->GetNext();

  THEN("a regex matches in an editor") {
    REQUIRE(g_ws->FindNext_Regex(wxS("gamma_[a-z]+"), true, true, true, false));
    REQUIRE(g_ws->GetActiveCell() == third->GetEditable());
  }
  THEN("a regex matches in an output cell") {
    REQUIRE(g_ws->FindNext_Regex(wxS("output_b.ta"), true, true, true, false));
    REQUIRE(g_ws->GetSelectionStart() == second->GetLabel());
  }
  THEN("an invalid regex finds nothing") {
    REQUIRE_FALSE(g_ws->FindNext_Regex(wxS("output_["), true, true, true, false));
  }
}

SCENARIO("Replace replaces the current match in the active editor") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  REQUIRE(g_ws->FindNext(wxS("beta_variable"), true, true, true, true, false));

  g_ws->Replace(wxS("beta_variable"), wxS("delta_thing"), true);

  THEN("the editor's text is changed") {
    REQUIRE(second->GetEditable()->GetValue().Contains(wxS("delta_thing")));
    REQUIRE_FALSE(second->GetEditable()->GetValue().Contains(wxS("beta")));
  }
}

SCENARIO("ReplaceAll replaces every occurrence in all inputs") {
  GroupCell *first = BuildDocument();
  GroupCell *second = first->GetNext();
  GroupCell *third = second->GetNext();

  const int count = g_ws->ReplaceAll(wxS("variable"), wxS("thing"), true, true, true);

  THEN("one replacement per editor is reported and applied") {
    REQUIRE(count == 3);
    REQUIRE(first->GetEditable()->GetValue().Contains(wxS("alpha_thing")));
    REQUIRE(second->GetEditable()->GetValue().Contains(wxS("beta_thing")));
    REQUIRE(third->GetEditable()->GetValue().Contains(wxS("gamma_thing")));
  }
}

SCENARIO("Searching an empty worksheet finds nothing") {
  g_ws->ClearDocument();
  REQUIRE(g_ws->GetTree() == nullptr);

  REQUIRE_FALSE(g_ws->FindNext(wxS("anything"), true, true, true, true, false));
  REQUIRE_FALSE(g_ws->FindNext_Regex(wxS("anything"), true, true, true, false));
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
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

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
