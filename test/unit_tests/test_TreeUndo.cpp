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
  Regression tests for the worksheet's cell-tree undo/redo (the TreeUndo_*
  subsystem of Worksheet).

  The tree-level undo (adding, deleting and changing whole GroupCells, as opposed
  to editing text inside one cell) had no automated coverage. These tests drive
  the real undoable edit paths (InsertGroupCells / DeleteRegion / an active-cell
  text change) on a real Worksheet and check that TreeUndo() restores the tree and
  TreeRedo() re-applies the edit -- a safety net for refactoring TreeUndo into its
  own class.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "Worksheet.h"
#include "cells/GroupCell.h"
#include "cells/EditorCell.h"

#include <cstdlib>
#include <vector>
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

int CellCount() {
  int n = 0;
  for (GroupCell *g = g_ws->GetTree(); g; g = g->GetNext())
    ++n;
  return n;
}

std::vector<wxString> CellTexts() {
  std::vector<wxString> v;
  for (GroupCell *g = g_ws->GetTree(); g; g = g->GetNext())
    v.push_back(g->GetEditable() ? g->GetEditable()->GetValue() : wxString());
  return v;
}

GroupCell *NthCell(int idx) {
  GroupCell *g = g_ws->GetTree();
  for (int i = 0; i < idx && g; ++i)
    g = g->GetNext();
  return g;
}

//! Reset the worksheet to a tree of code cells holding the given texts, with an
//! empty undo/redo history.
void BuildTree(const std::vector<wxString> &texts) {
  g_ws->ClearDocument();
  GroupCell *where = NthCell(CellCount() - 1); // append after whatever is left
  for (const auto &t : texts)
    where = g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, t), where);
  g_ws->TreeUndo_ClearBuffers();
}
} // namespace

SCENARIO("Adding a cell is undoable and redoable") {
  GIVEN("a worksheet and its captured state") {
    BuildTree({wxS("a:1$"), wxS("b:2$")});
    const auto before = CellTexts();
    const int countBefore = CellCount();

    WHEN("a cell is appended") {
      g_ws->InsertGroupCells(
        std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("c:3$")),
        NthCell(countBefore - 1));
      THEN("undo removes it and restores the tree; redo brings it back") {
        REQUIRE(CellCount() == countBefore + 1);
        REQUIRE(g_ws->CanUndo());

        REQUIRE(g_ws->TreeUndo());
        CHECK(CellCount() == countBefore);
        CHECK(CellTexts() == before);

        REQUIRE(g_ws->TreeRedo());
        CHECK(CellCount() == countBefore + 1);
      }
    }
  }
}

SCENARIO("Deleting a cell is undoable and redoable") {
  GIVEN("a three-cell worksheet") {
    BuildTree({wxS("a:1$"), wxS("b:2$"), wxS("c:3$")});
    const auto before = CellTexts();
    const int countBefore = CellCount();

    WHEN("the middle cell is deleted") {
      GroupCell *mid = NthCell(countBefore - 2);
      g_ws->DeleteRegion(mid, mid);
      THEN("undo restores the deleted cell with its text; redo deletes it again") {
        REQUIRE(CellCount() == countBefore - 1);

        REQUIRE(g_ws->TreeUndo());
        CHECK(CellCount() == countBefore);
        CHECK(CellTexts() == before); // text content restored too

        REQUIRE(g_ws->TreeRedo());
        CHECK(CellCount() == countBefore - 1);
      }
    }
  }
}

SCENARIO("Changing a cell's text is undoable") {
  GIVEN("a worksheet with one cell") {
    BuildTree({wxS("orig$")});
    GroupCell *cell = NthCell(CellCount() - 1);
    REQUIRE(cell);
    REQUIRE(cell->GetEditable());

    WHEN("the active cell's text is changed") {
      g_ws->SetActiveCell(cell->GetEditable());
      g_ws->TreeUndo_CellEntered();
      cell->GetEditable()->SetValue(wxS("changed$"));
      g_ws->TreeUndo_CellLeft();
      THEN("undo reverts the text") {
        CHECK(cell->GetEditable()->GetValue() == wxS("changed$"));
        REQUIRE(g_ws->TreeUndo());
        CHECK(NthCell(CellCount() - 1)->GetEditable()->GetValue() == wxS("orig$"));
      }
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

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

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
