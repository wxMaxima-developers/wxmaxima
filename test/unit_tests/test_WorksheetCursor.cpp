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
  Pins the worksheet's cursor model before it is gathered into a cursor class.

  The worksheet has two cursors - the horizontal caret between group cells and
  the text cursor inside an active EditorCell - with the invariant that at
  most one is active at a time; while cells are selected, neither is. Today
  that invariant is re-established by hand at every mutation site, and the
  state is split between Worksheet (h-caret) and CellPointers (active cell,
  written by EditorCell itself). These scenarios pin the exact transition
  behavior so the planned WorksheetCursor extraction cannot change it.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

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

//! A fresh three-cell document; returns the first cell.
static GroupCell *BuildDocument() {
  g_ws->ClearDocument();
  GroupCell *first = g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("1+1;")), nullptr);
  GroupCell *second = g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, wxS("middle")), first);
  g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("2+2;")), second);
  g_ws->SetActiveCell(nullptr);
  g_ws->ClearSelection();
  g_ws->Recalculate();
  g_ws->RecalculateIfNeeded();
  return first;
}

SCENARIO("Activating an editor cell deactivates the h-caret and the selection") {
  GroupCell *first = BuildDocument();
  g_ws->SetHCaret(first);
  REQUIRE(g_ws->HCaretActive());

  g_ws->SetActiveCell(first->GetEditable());

  THEN("the editor cursor is the active one") {
    REQUIRE(g_ws->GetActiveCell() == first->GetEditable());
    REQUIRE_FALSE(g_ws->HCaretActive());
    // GetHCaret() is a derived "where would the caret go" anchor, not raw
    // state: with an active cell it answers with that cell's group.
    REQUIRE(g_ws->GetHCaret() == first);
    REQUIRE(g_ws->GetSelectionStart() == nullptr);
  }
}

SCENARIO("Setting the h-caret deactivates the editor cursor") {
  GroupCell *first = BuildDocument();
  g_ws->SetActiveCell(first->GetEditable());
  REQUIRE(g_ws->GetActiveCell() != nullptr);

  g_ws->SetHCaret(first);

  THEN("the h-caret is the active one, below the given cell") {
    REQUIRE(g_ws->HCaretActive());
    REQUIRE(g_ws->GetHCaret() == first);
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("The h-caret at the document start is active with a null position") {
  BuildDocument();
  g_ws->SetHCaret(nullptr);

  THEN("null position means: before the first cell") {
    REQUIRE(g_ws->HCaretActive());
    REQUIRE(g_ws->GetHCaret() == nullptr);
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("Deactivating the editor cursor does not re-activate the h-caret") {
  GroupCell *first = BuildDocument();
  g_ws->SetActiveCell(first->GetEditable());
  REQUIRE_FALSE(g_ws->HCaretActive());

  g_ws->SetActiveCell(nullptr);

  THEN("neither cursor is active") {
    REQUIRE(g_ws->GetActiveCell() == nullptr);
    REQUIRE_FALSE(g_ws->HCaretActive());
  }
}

SCENARIO("Selecting a cell leaves the selection as the focus owner") {
  GroupCell *first = BuildDocument();
  g_ws->SelectGroupCell(first);

  THEN("the group cell is selected") {
    REQUIRE(g_ws->GetSelectionStart() == first);
  }
  THEN("the editor cursor is not active") {
    REQUIRE(g_ws->GetActiveCell() == nullptr);
  }
}

SCENARIO("SetHCaret clears an existing cell selection") {
  GroupCell *first = BuildDocument();
  g_ws->SetSelection(first, first);
  REQUIRE(g_ws->GetSelectionStart() == first);

  g_ws->SetHCaret(first);

  THEN("placing the caret dissolves the selection") {
    REQUIRE(g_ws->GetSelectionStart() == nullptr);
    REQUIRE(g_ws->HCaretActive());
  }
}

SCENARIO("ShowHCaret without a position places the caret at the document end") {
  BuildDocument();
  g_ws->SetHCaret(nullptr);
  REQUIRE(g_ws->GetHCaret() == nullptr);

  g_ws->ShowHCaret();

  THEN("a null h-caret position stays the document start") {
    // ShowHCaret only jumps to the end if no position was ever set; a null
    // position is a valid position (document start), so it is kept.
    REQUIRE(g_ws->HCaretActive());
  }
}

SCENARIO("OpenHCaret opens a new cell at the h-caret and activates its editor") {
  GroupCell *first = BuildDocument();
  g_ws->SetHCaret(first);

  g_ws->OpenHCaret(wxS("foo"), GC_TYPE_CODE);

  THEN("a new code cell exists after the first cell and has the cursor") {
    GroupCell *fresh = first->GetNext();
    REQUIRE(fresh != nullptr);
    REQUIRE(fresh->GetGroupType() == GC_TYPE_CODE);
    REQUIRE(g_ws->GetActiveCell() == fresh->GetEditable());
    REQUIRE(g_ws->GetActiveCell()->GetValue() == wxS("foo"));
    REQUIRE_FALSE(g_ws->HCaretActive());
  }
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
