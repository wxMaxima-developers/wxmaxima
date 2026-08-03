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
  Tests for real tab-character ('\t') support inside EditorCell.

  A tab is a single real character in m_text (see NormalizeLineEndings(),
  which replaced the old TabExpand() that irreversibly rewrote every tab into
  1-4 spaces), expanded to the next 4-column tab stop only where text turns
  into pixels (NextTabStop(), used by Draw()/Recalculate()/GetLineWidth()) or
  where a click turns into a text offset (SelectPointText()).

  Windowless: real GroupCell/EditorCell against a memory-DC Configuration, no
  Worksheet, no wxFrame -- the test_EditorCellWrapping/test_EditorCellBidi
  pattern.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#include <memory>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
} // namespace

namespace {

//! Builds a code cell holding \p text and lays it out.
std::unique_ptr<GroupCell> MakeCodeCell(const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, text);
  group->Recalculate();
  return group;
}

//! Sends one key to the cell (Tab, Backspace, ...) with the given modifier
//! state and returns where the caret ended up.
size_t PressKey(EditorCell *editor, int keyCode, bool shift = false) {
  wxKeyEvent event(wxEVT_KEY_DOWN);
  event.m_keyCode = keyCode;
  event.m_shiftDown = shift;
  editor->ProcessEvent(event);
  return editor->CursorPosition();
}

} // namespace

SCENARIO("A tab survives construction/SetValue untouched") {
  GIVEN("text containing tabs used for alignment, as a file load would produce") {
    const wxString original = wxS("a:1\t+\t2$\nb:3\t*\t4$");

    WHEN("a code cell is built directly from it") {
      auto group = MakeCodeCell(original);
      EditorCell *editor = group->GetEditable();
      REQUIRE(editor != nullptr);

      THEN("GetValue() returns it byte-identical -- no tab became spaces") {
        REQUIRE(editor->GetValue() == original);
      }
    }

    WHEN("it is fed through SetValue() directly") {
      auto group = MakeCodeCell(wxS("placeholder$"));
      EditorCell *editor = group->GetEditable();
      editor->SetValue(original);

      THEN("the tabs are still there") {
        REQUIRE(editor->GetValue() == original);
      }
    }
  }
}

SCENARIO("GetLineWidth() expands a tab to the next 4-column stop") {
  GIVEN("code lines with a tab after varying amounts of leading text") {
    // Each prefix ends right before the tab; the character after the tab
    // must land at NextTabStop(width of the prefix), not at
    // width(prefix) + width("\t") the way an ordinary character would.
    const wxString prefixes[] = {wxS(""), wxS("a"), wxS("ab"), wxS("abcd"),
                                 wxS("abcde")};

    for (const wxString &prefix : prefixes) {
      DYNAMIC_SECTION("prefix = \"" << prefix.ToStdString() << "\"") {
        const wxString text = prefix + wxS("\tx");
        auto group = MakeCodeCell(text);
        EditorCell *editor = group->GetEditable();
        REQUIRE(editor != nullptr);

        const size_t tabPos = prefix.Length();
        const wxCoord beforeTab = editor->GetLineWidth(0, tabPos);
        const wxCoord afterTab = editor->GetLineWidth(0, tabPos + 1);

        REQUIRE(afterTab == editor->NextTabStop(beforeTab));
        // A tab always advances by at least one column: it can never be a
        // zero-width no-op, even right at a stop already (prefix = "abcd").
        REQUIRE(afterTab > beforeTab);
      }
    }
  }

  GIVEN("a run of several consecutive tabs") {
    auto group = MakeCodeCell(wxS("x\t\t\ty"));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("each one independently advances to its own next stop") {
      const wxCoord afterX = editor->GetLineWidth(0, 1);
      const wxCoord afterTab1 = editor->GetLineWidth(0, 2);
      const wxCoord afterTab2 = editor->GetLineWidth(0, 3);
      const wxCoord afterTab3 = editor->GetLineWidth(0, 4);

      REQUIRE(afterTab1 == editor->NextTabStop(afterX));
      REQUIRE(afterTab2 == editor->NextTabStop(afterTab1));
      REQUIRE(afterTab3 == editor->NextTabStop(afterTab2));
      // Three real tabs in a row must not collapse to one stop.
      REQUIRE(afterTab3 > afterTab1);
    }
  }
}

SCENARIO("The Tab key inserts a real tab character") {
  GIVEN("an empty code cell, no selection") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->CursorPosition(0);

    WHEN("Tab is pressed") {
      const size_t pos = PressKey(editor, WXK_TAB);

      THEN("a literal tab was inserted and the caret moved past it") {
        REQUIRE(editor->GetValue() == wxS("\t"));
        REQUIRE(pos == 1);
      }
    }
  }
}

SCENARIO("Backspace removes a whole tab in one press") {
  GIVEN("a code cell ending in a tab, caret at the end") {
    auto group = MakeCodeCell(wxS("a\t"));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->CursorPosition(2);

    WHEN("Backspace is pressed once") {
      const size_t pos = PressKey(editor, WXK_BACK);

      THEN("only the tab is gone, not the character before it") {
        REQUIRE(editor->GetValue() == wxS("a"));
        REQUIRE(pos == 1);
      }
    }
  }

  GIVEN("a code cell with 4 literal spaces (no tab) before the caret") {
    // The old space-expanded-tab world had Backspace gobble up to 4 trailing
    // spaces in one press as a stand-in for deleting a tab. That heuristic is
    // gone now that a real tab is one character; plain spaces the user
    // actually typed must go back to deleting one at a time.
    auto group = MakeCodeCell(wxS("a    "));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->CursorPosition(5);

    WHEN("Backspace is pressed once") {
      const size_t pos = PressKey(editor, WXK_BACK);

      THEN("exactly one space is removed") {
        REQUIRE(editor->GetValue() == wxS("a   "));
        REQUIRE(pos == 4);
      }
    }
  }
}

SCENARIO("Selection + Tab/Shift+Tab indent and dedent with a real tab") {
  GIVEN("a two-line selection spanning both lines, no existing indentation") {
    auto group = MakeCodeCell(wxS("a:1$\nb:2$"));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->SetSelection(0, editor->GetValue().Length());

    WHEN("Tab is pressed") {
      PressKey(editor, WXK_TAB);

      THEN("each line gets a leading tab, not 4 spaces") {
        REQUIRE(editor->GetValue() == wxS("\ta:1$\n\tb:2$"));
      }

      AND_WHEN("the same lines are selected again and Shift+Tab is pressed") {
        editor->SetSelection(0, editor->GetValue().Length());
        PressKey(editor, WXK_TAB, /*shift=*/true);

        THEN("the leading tabs are removed again") {
          REQUIRE(editor->GetValue() == wxS("a:1$\nb:2$"));
        }
      }
    }
  }

  GIVEN("a selection over lines that are already space-indented") {
    // No tab to remove: dedent must fall back to eating up to 4 leading
    // spaces, exactly as it always did, so pre-existing space-indented
    // documents keep working.
    auto group = MakeCodeCell(wxS("    a:1$\n    b:2$"));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->SetSelection(0, editor->GetValue().Length());

    WHEN("Shift+Tab is pressed") {
      PressKey(editor, WXK_TAB, /*shift=*/true);

      THEN("the leading spaces are removed") {
        REQUIRE(editor->GetValue() == wxS("a:1$\nb:2$"));
      }
    }
  }
}

SCENARIO("Selection stays active across repeated Tab/Shift+Tab presses") {
  // Regression guard: Tab/Shift+Tab on a selection used to call
  // CursorPosition(start) right after SetSelection(start, end), which
  // collapses the selection to a caret (CursorPosition(pos) sets both
  // selection ends to pos) -- so a second Tab press saw no selection at all
  // and did nothing, instead of indenting the same lines again.
  GIVEN("a two-line selection, no existing indentation") {
    auto group = MakeCodeCell(wxS("a:1$\nb:2$"));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    editor->SetSelection(0, editor->GetValue().Length());

    WHEN("Tab is pressed once") {
      PressKey(editor, WXK_TAB);

      THEN("the selection is still active, spanning the (now longer) lines") {
        REQUIRE(editor->SelectionActive());
        REQUIRE(editor->SelectionLeft() == 0);
        REQUIRE(editor->SelectionRight() == editor->GetValue().Length());
      }

      AND_WHEN("Tab is pressed again without re-selecting") {
        PressKey(editor, WXK_TAB);

        THEN("both lines are indented a second time") {
          REQUIRE(editor->GetValue() == wxS("\t\ta:1$\n\t\tb:2$"));
        }

        AND_WHEN("Shift+Tab is pressed twice more, still without re-selecting") {
          PressKey(editor, WXK_TAB, /*shift=*/true);
          PressKey(editor, WXK_TAB, /*shift=*/true);

          THEN("both dedents apply, back to the original text") {
            REQUIRE(editor->GetValue() == wxS("a:1$\nb:2$"));
          }
        }
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

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
