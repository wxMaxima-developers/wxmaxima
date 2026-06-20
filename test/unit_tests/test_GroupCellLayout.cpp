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
  Regression tests for the GroupCell <-> EditorCell layout coupling.

  Historically the GroupCell repeatedly stopped growing when its input
  EditorCell gained a line (by pressing Enter or by word-wrapping) once the cell
  had been evaluated and thus carried a "(%i1)" prompt label. Two bugs caused
  this:

   1. GroupCell::RecalculateInput() only counted the height of the *first* cell
      on each line, so the taller EditorCell that follows the prompt label was
      ignored (the group kept the label's one-line height).
   2. EditorCell::Recalculate() updated its height without invalidating the
      cached list geometry (GetHeightList()), so an *incremental* recalculation
      (the keypress path, which does not go through ResetSize()) read a stale,
      too-small editor height.

  These tests exercise the real GroupCell/EditorCell/Worksheet/Configuration
  code (linked in via the wxmTestApp object library) so a future regression in
  either path fails the build.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "Worksheet.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/EditorCell.h"

#include <cstdlib>
#include <unistd.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// The real recalculation pipeline reaches Cell::GetWorksheet(), which asserts a
// worksheet is registered, so we set up a genuine (off-screen, event-less) one
// once and let every scenario share it. Owned for the process lifetime.
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
} // namespace

// wxGTK routes font/DC work through GTK, which needs an X display. When run via
// ctest the headless wrapper provides one; when run directly we start our own.
static void EnsureDisplay() {
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
}

// Builds a code GroupCell (prompt label + input EditorCell) holding "text" and
// lays it out, returning its total height.
static int CodeGroupHeight(const wxString &text) {
  GroupCell group(g_cfg, GC_TYPE_CODE, text);
  group.Recalculate();
  return group.GetHeight();
}

SCENARIO("A GroupCell's height tracks the number of lines in its input cell") {
  GIVEN("code GroupCells with one and with three input lines") {
    const int h1 = CodeGroupHeight(wxS("a:1$"));
    const int h3 = CodeGroupHeight(wxS("a:1$\nb:2$\nc:3$"));

    THEN("the three-line cell is clearly taller than the one-line cell") {
      // If RecalculateInput() ignored the editor (counting only the prompt
      // label) both would be one line tall and this would fail.
      REQUIRE(h3 > h1);
      REQUIRE(h3 >= 2 * h1);
    }
  }
}

SCENARIO("Adding a line to an already-laid-out input cell grows its GroupCell") {
  GIVEN("a laid-out one-line code GroupCell") {
    GroupCell group(g_cfg, GC_TYPE_CODE, wxS("a:1$"));
    group.Recalculate();
    const int hBefore = group.GetHeight();
    EditorCell *editor = group.GetEditable();
    REQUIRE(editor != nullptr);

    WHEN("the user presses Enter to add a line to the input") {
      // Reproduce the worksheet keypress path: a real Return key event makes
      // the editor dirty and inserts a newline...
      wxKeyEvent ev(wxEVT_CHAR);
      ev.m_keyCode = WXK_RETURN;
      ev.m_uniChar = WXK_RETURN;
      editor->ProcessEvent(ev);
      // ...then Worksheet::OnCharInActive() recalculates the editor and, on a
      // height change, notifies the group incrementally (no full ResetSize()).
      editor->Recalculate(g_cfg->GetDefaultFontSize());
      group.InputHeightChanged();

      THEN("the GroupCell has grown to contain the new line") {
        REQUIRE(group.GetHeight() > hBefore);
      }
    }
  }
}

SCENARIO("Incremental search extends the current match in place") {
  // When a forward search already highlights a match and the user types another
  // character that the SAME match still satisfies, the highlight must just grow
  // by that character -- it must not skip ahead to the next match. The text has
  // two "foob" occurrences so a regression (advancing past the current match)
  // is observable as a jump to the second one.
  GIVEN("an active editor whose text contains two 'foob' matches") {
    // The first match starts at index 3 (not 0) so the regression -- advancing
    // one char past the current match -- is observable: the buggy code skipped
    // to the second "foob" at index 10.
    GroupCell group(g_cfg, GC_TYPE_CODE, wxS("ab foobar foobat"));
    EditorCell *editor = group.GetEditable();
    REQUIRE(editor != nullptr);
    g_ws->SetActiveCell(editor);
    // Anchor the search at the top of the cell, as Worksheet::FindIncremental
    // does (it moves the caret back to where the search started before each
    // incremental lookup).
    editor->SetSelection(0, 0);

    WHEN("'foo' is searched and then extended to 'foob'") {
      REQUIRE(editor->FindNext(wxS("foo"), /*down=*/true, /*ignoreCase=*/false));
      REQUIRE(editor->SelectionLeft() == 3);
      REQUIRE(editor->GetSelectionString() == wxS("foo"));

      const bool found =
        editor->FindNext(wxS("foob"), /*down=*/true, /*ignoreCase=*/false);

      THEN("the first match is extended, not skipped to the second one") {
        REQUIRE(found);
        REQUIRE(editor->SelectionLeft() == 3);
        REQUIRE(editor->GetSelectionString() == wxS("foob"));
      }
    }
  }
}

SCENARIO("Growing a cell pushes the cell below it further down") {
  // The visible symptom of the GroupCell-grow bug was that, when a cell gained a
  // line, the cells below it did not move down and ended up overlapping. Build
  // two stacked code cells and check that growing the first one repositions the
  // second.
  auto cell1 = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("a:1$"));
  GroupCell *c1 = cell1.get();
  GroupCell *c2 = nullptr;
  {
    auto cell2 = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("b:2$"));
    c2 = cell2.get();
    CellList::AppendCell(c1, std::move(cell2)); // link c1 -> c2 (c1 owns c2)
  }

  GIVEN("two stacked, laid-out code cells") {
    c1->RecalculateList(g_cfg->GetDefaultFontSize());
    c1->UpdateYPositionList();
    const int c2yBefore = c2->GetCurrentPoint().y;
    REQUIRE(c2yBefore > c1->GetCurrentPoint().y); // c2 sits below c1

    WHEN("the first cell's input gains a line") {
      EditorCell *editor = c1->GetEditable();
      REQUIRE(editor != nullptr);
      editor->SetSelection(0, 0);
      wxKeyEvent ev(wxEVT_CHAR);
      ev.m_keyCode = WXK_RETURN;
      ev.m_uniChar = WXK_RETURN;
      editor->ProcessEvent(ev);
      editor->Recalculate(g_cfg->GetDefaultFontSize());
      c1->InputHeightChanged();

      THEN("the second cell has moved further down (no overlap)") {
        REQUIRE(c2->GetCurrentPoint().y > c2yBefore);
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
  g_ws = new Worksheet(frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
