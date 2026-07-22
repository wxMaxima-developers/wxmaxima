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
#include "worksheet/Worksheet.h"
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

//! Append a fresh GroupCell of the given type/text after the last cell, without
//! recording an undo action (buffers are cleared afterwards by the caller).
GroupCell *AppendCell(GroupType type, const wxString &text) {
  return g_ws->InsertGroupCells(std::make_unique<GroupCell>(g_cfg, type, text),
                                NthCell(CellCount() - 1));
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

SCENARIO("Converting a cell's type via SetCellStyle rebuilds it and is undoable") {
  GIVEN("a one-cell worksheet holding a code cell") {
    BuildTree({wxS("f(x):=x^2$")});
    GroupCell *cell = NthCell(CellCount() - 1);
    REQUIRE(cell);
    REQUIRE(cell->GetGroupType() == GC_TYPE_CODE);
    REQUIRE(cell->GetEditable());
    REQUIRE(cell->GetEditable()->IsCodeEditor());
    const int countBefore = CellCount();

    WHEN("it is converted to a text cell") {
      g_ws->SetCellStyle(cell, GC_TYPE_TEXT);
      GroupCell *converted = NthCell(CellCount() - 1);

      THEN("it becomes a prose cell of the right subclass, keeps its text, and "
           "undo restores the code cell") {
        REQUIRE(CellCount() == countBefore); // rebuilt in place, not appended
        REQUIRE(converted->GetGroupType() == GC_TYPE_TEXT);
        REQUIRE(converted->GetEditable() != nullptr);
        REQUIRE_FALSE(converted->GetEditable()->IsCodeEditor()); // TextEditorCell
        REQUIRE(converted->GetEditable()->GetValue() == wxS("f(x):=x^2$"));

        REQUIRE(g_ws->CanUndo());
        REQUIRE(g_ws->TreeUndo());
        GroupCell *restored = NthCell(CellCount() - 1);
        CHECK(restored->GetGroupType() == GC_TYPE_CODE);
        CHECK(restored->GetEditable()->IsCodeEditor()); // code editor again
        CHECK(restored->GetEditable()->GetValue() == wxS("f(x):=x^2$"));
      }
    }
  }
}

SCENARIO("SetCellStyle refuses to convert an image cell") {
  GIVEN("a worksheet whose last cell is an image cell") {
    BuildTree({wxS("a:1$")});
    g_ws->InsertGroupCells(std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE),
                           NthCell(CellCount() - 1));
    GroupCell *img = NthCell(CellCount() - 1);
    REQUIRE(img->GetGroupType() == GC_TYPE_IMAGE);
    const int countBefore = CellCount();

    WHEN("we try to convert it to text (which would discard the image)") {
      g_ws->SetCellStyle(img, GC_TYPE_TEXT);

      THEN("the cell is left untouched as an image cell") {
        REQUIRE(CellCount() == countBefore);
        REQUIRE(NthCell(CellCount() - 1)->GetGroupType() == GC_TYPE_IMAGE);
      }
    }
  }
}

SCENARIO("Folding survives an unrelated deletion and its undo") {
  GIVEN("a worksheet [code, section, code] with the section folded") {
    g_ws->ClearDocument();
    GroupCell *top = AppendCell(GC_TYPE_CODE, wxS("top:1$"));
    GroupCell *section = AppendCell(GC_TYPE_SECTION, wxS("A section"));
    AppendCell(GC_TYPE_CODE, wxS("child:2$")); // becomes the section's child
    g_ws->TreeUndo_ClearBuffers();

    // Fold the section: its following code child moves into the hidden tree.
    REQUIRE(g_ws->ToggleFold(section) == section);
    REQUIRE(section->GetHiddenTree() != nullptr);
    REQUIRE(section->GetHiddenTree()->GetEditable()->GetValue() == wxS("child:2$"));
    REQUIRE(CellCount() == 2); // only [code, section] are visible now
    g_ws->TreeUndo_ClearBuffers(); // folding itself is not an undoable action

    WHEN("an unrelated cell above the fold is deleted and the delete is undone") {
      g_ws->DeleteRegion(top, top);
      REQUIRE(CellCount() == 1); // just the section is left visible

      REQUIRE(g_ws->TreeUndo());

      THEN("the deleted cell returns and the section is still folded") {
        REQUIRE(CellCount() == 2);
        GroupCell *restoredSection = NthCell(1);
        REQUIRE(restoredSection->GetGroupType() == GC_TYPE_SECTION);
        REQUIRE(restoredSection->GetHiddenTree() != nullptr);
        CHECK(restoredSection->GetHiddenTree()->GetEditable()->GetValue() ==
              wxS("child:2$"));
      }
    }
  }
}

SCENARIO("Deleting a folded section and undoing it restores the fold") {
  GIVEN("a worksheet [code, section, code] with the section folded") {
    g_ws->ClearDocument();
    AppendCell(GC_TYPE_CODE, wxS("top:1$"));
    GroupCell *section = AppendCell(GC_TYPE_SECTION, wxS("A section"));
    AppendCell(GC_TYPE_CODE, wxS("child:2$"));
    g_ws->TreeUndo_ClearBuffers();

    REQUIRE(g_ws->ToggleFold(section) == section);
    REQUIRE(section->GetHiddenTree() != nullptr);
    REQUIRE(CellCount() == 2);
    g_ws->TreeUndo_ClearBuffers();

    WHEN("the folded section itself is deleted") {
      g_ws->DeleteRegion(section, section);
      REQUIRE(CellCount() == 1); // the hidden child goes with the section

      THEN("undo brings the section back still folded; redo removes it again") {
        REQUIRE(g_ws->TreeUndo());
        REQUIRE(CellCount() == 2);
        GroupCell *restored = NthCell(1);
        REQUIRE(restored->GetGroupType() == GC_TYPE_SECTION);
        REQUIRE(restored->GetHiddenTree() != nullptr);
        CHECK(restored->GetHiddenTree()->GetGroupType() == GC_TYPE_CODE);
        CHECK(restored->GetHiddenTree()->GetEditable()->GetValue() ==
              wxS("child:2$"));
        // The hidden child must not have leaked into the visible list.
        CHECK(restored->GetNext() == nullptr);

        REQUIRE(g_ws->TreeRedo());
        CHECK(CellCount() == 1);
      }
    }
  }
}

SCENARIO("Undoing an insertion whose cell was folded away reveals and removes it") {
  // This drives TreeUndoCellAddition's RevealHidden() branch: the undo range
  // (the inserted cell) has since been folded into a section's hidden tree, so
  // the undo must first unfold it back into the visible list before deleting it.
  GIVEN("a section, a code cell inserted after it, then folded into the section") {
    g_ws->ClearDocument();
    GroupCell *section = AppendCell(GC_TYPE_SECTION, wxS("A section"));
    g_ws->TreeUndo_ClearBuffers();

    // The insertion we will undo: a code cell right after the section.
    GroupCell *child = g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("child:2$")), section);
    REQUIRE(CellCount() == 2);
    REQUIRE(g_ws->CanUndo());

    // Fold the section: the freshly-inserted child moves into the hidden tree,
    // so the pending undo action now points at a hidden cell.
    REQUIRE(g_ws->ToggleFold(section) == section);
    REQUIRE(section->GetHiddenTree() == child);
    REQUIRE(CellCount() == 1);

    WHEN("the insertion is undone") {
      REQUIRE(g_ws->TreeUndo());

      THEN("the child is gone and the section is left unfolded and empty") {
        REQUIRE(CellCount() == 1);
        GroupCell *only = NthCell(0);
        REQUIRE(only->GetGroupType() == GC_TYPE_SECTION);
        CHECK(only->GetHiddenTree() == nullptr); // unfolded during the undo
        CHECK(only->GetNext() == nullptr);        // the inserted cell was removed
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
