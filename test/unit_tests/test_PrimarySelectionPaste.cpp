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
  GH #794: a middle-click pastes the primary selection, not the Ctrl+C clipboard.

  X11 has two clipboards: the ordinary one (Ctrl+C / Ctrl+V) and the primary
  selection, which holds whatever was last selected and is pasted by a
  middle-click. Worksheet::OnMouseMiddleUp() switched wxTheClipboard to the
  primary selection, but Worksheet::PasteFromClipboard() then handed the paste
  to EditorCell::PasteFromClipboard() without saying which clipboard it was
  for - and that function's default switched back to the ordinary clipboard.
  So a middle-click into a cell pasted the Ctrl+C contents, while a
  middle-click between cells (which never reaches the cell) worked. Anyone
  testing with both clipboards holding the same text could not see it.

  This test uses the real clipboard, so it needs a display (CI runs it under
  xvfb). Only platforms that have a primary selection can show the bug; on
  the others wxWidgets ignores UsePrimarySelection(true), and the scenarios
  check only that pasting still reads the one clipboard there is.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;

const wxString primaryText(wxS("FROM_PRIMARY"));
const wxString clipboardText(wxS("FROM_CLIPBOARD"));

//! Puts text on the primary selection (primary = true) or the ordinary clipboard
void SetClipboardText(bool primary, const wxString &text) {
  wxTheClipboard->UsePrimarySelection(primary);
  REQUIRE(wxTheClipboard->Open());
  wxTheClipboard->SetData(new wxTextDataObject(text));
  wxTheClipboard->Close();
  wxTheClipboard->UsePrimarySelection(false);
}

//! Does this platform have a primary selection distinct from the clipboard?
bool HasPrimarySelection() {
  wxTheClipboard->UsePrimarySelection(true);
  const bool hasIt = wxTheClipboard->IsUsingPrimarySelection();
  wxTheClipboard->UsePrimarySelection(false);
  return hasIt;
}

//! What the paste is expected to insert when asked for the primary selection
wxString ExpectedPrimaryPaste() {
  // Without a primary selection both writes went to the one clipboard, and
  // the later one (the clipboard text) is what is there.
  return HasPrimarySelection() ? primaryText : clipboardText;
}

//! A one-cell document whose editor is active, with the cursor after "abc"
EditorCell *ActiveCodeCell() {
  g_ws->ClearDocument();
  GroupCell *group = g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("abc")), nullptr);
  g_ws->RecalculateIfNeeded();
  EditorCell *editor = group->GetEditable();
  g_ws->SetActiveCell(editor);
  editor->CaretToEnd();
  return editor;
}

void FillBothClipboards() {
  SetClipboardText(true, primaryText);
  SetClipboardText(false, clipboardText);
}
} // namespace

SCENARIO("Pasting the primary selection into a cell inserts the primary selection") {
  GIVEN("an active cell and different text on the two clipboards") {
    EditorCell *editor = ActiveCodeCell();
    FillBothClipboards();
    WHEN("the primary selection is pasted, as on a middle-click") {
      g_ws->PasteFromClipboard(true);
      THEN("the cell got the primary selection, not the Ctrl+C clipboard") {
        REQUIRE(editor->GetValue() == wxS("abc") + ExpectedPrimaryPaste());
      }
      THEN("the clipboard is switched back to the ordinary one") {
        REQUIRE_FALSE(wxTheClipboard->IsUsingPrimarySelection());
      }
    }
  }
}

SCENARIO("An ordinary paste into a cell still inserts the Ctrl+C clipboard") {
  GIVEN("an active cell and different text on the two clipboards") {
    EditorCell *editor = ActiveCodeCell();
    FillBothClipboards();
    WHEN("the ordinary clipboard is pasted, as on Ctrl+V") {
      g_ws->PasteFromClipboard();
      THEN("the cell got the Ctrl+C clipboard") {
        REQUIRE(editor->GetValue() == wxS("abc") + clipboardText);
      }
    }
  }
}

SCENARIO("Pasting the primary selection at the h-caret opens a cell with it") {
  GIVEN("a document with the h-caret below its only cell") {
    g_ws->ClearDocument();
    GroupCell *group = g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("abc")), nullptr);
    g_ws->RecalculateIfNeeded();
    g_ws->SetHCaret(group);
    FillBothClipboards();
    WHEN("the primary selection is pasted") {
      g_ws->PasteFromClipboard(true);
      THEN("a new cell holds the primary selection") {
        REQUIRE(group->GetNext() != nullptr);
        REQUIRE(group->GetNext()->GetEditable()->GetValue() ==
                ExpectedPrimaryPaste());
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

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
