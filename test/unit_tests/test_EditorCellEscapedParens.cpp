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
  Tests that the code editor treats a backslash-escaped bracket or quote as an
  ordinary character (GH #528).

  In Maxima "a\(3\]" is a single name; the "(" and "]" in it open and close
  nothing. The editor's bracket helpers -- auto-closing a typed opener,
  jumping over a typed closer, deleting an empty pair with one Backspace and
  highlighting the partner of the bracket under the cursor -- used to treat
  them as real brackets, which made such a name hard to type.

  Windowless: real GroupCell/EditorCell against a memory-DC Configuration, the
  test_EditorCellTabs pattern.
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

//! Builds a code cell holding \p text with the caret at its end.
std::unique_ptr<GroupCell> MakeCodeCell(const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, text);
  group->Recalculate();
  group->GetEditable()->CursorPosition(text.Length());
  return group;
}

//! Types one printable character, the way a key press reaches the cell.
void Type(EditorCell *editor, wxChar ch) {
  wxKeyEvent event(wxEVT_CHAR);
  event.m_keyCode = ch;
  event.m_uniChar = ch;
  editor->ProcessEvent(event);
}

//! Types every character of \p text in turn.
void TypeAll(EditorCell *editor, const wxString &text) {
  for (auto ch : text)
    Type(editor, ch);
}

void PressBackspace(EditorCell *editor) {
  wxKeyEvent event(wxEVT_KEY_DOWN);
  event.m_keyCode = WXK_BACK;
  editor->ProcessEvent(event);
}
} // namespace

SCENARIO("Typing the name from GH #528 gives exactly that name") {
  g_cfg->SetMatchParens(true);
  GIVEN("an empty code cell with bracket matching on") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    WHEN("a\\(3\\]:5; is typed character by character") {
      TypeAll(editor, wxS("a\\(3\\]:5;"));
      THEN("no closing bracket was added for the escaped ones") {
        REQUIRE(editor->GetValue() == wxS("a\\(3\\]:5;"));
        REQUIRE(editor->CursorPosition() == editor->GetValue().Length());
      }
    }
  }
}

SCENARIO("Unescaped brackets are still auto-closed") {
  g_cfg->SetMatchParens(true);
  GIVEN("an empty code cell") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();

    WHEN("f( is typed") {
      TypeAll(editor, wxS("f("));
      THEN("the closing bracket is inserted after the caret") {
        REQUIRE(editor->GetValue() == wxS("f()"));
        REQUIRE(editor->CursorPosition() == 2);
      }
      AND_WHEN(") is typed") {
        Type(editor, wxS(')'));
        THEN("the caret jumps over the existing closing bracket") {
          REQUIRE(editor->GetValue() == wxS("f()"));
          REQUIRE(editor->CursorPosition() == 3);
        }
      }
    }

    WHEN("an escaped backslash is followed by a bracket") {
      // "\\" is a backslash that is itself escaped, so the "(" is real.
      TypeAll(editor, wxS("a\\\\("));
      THEN("that bracket is auto-closed") {
        REQUIRE(editor->GetValue() == wxS("a\\\\()"));
      }
    }
  }
}

SCENARIO("An escaped closer does not jump over a real one") {
  g_cfg->SetMatchParens(true);
  GIVEN("f(a with the caret in front of the auto-inserted closer") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    TypeAll(editor, wxS("f(a"));
    REQUIRE(editor->GetValue() == wxS("f(a)"));

    WHEN("\\) is typed") {
      TypeAll(editor, wxS("\\)"));
      THEN("the escaped closer is inserted and the real one kept") {
        REQUIRE(editor->GetValue() == wxS("f(a\\))"));
        REQUIRE(editor->CursorPosition() == 5);
      }
    }
  }
}

SCENARIO("An escaped quote inside a string does not end the string") {
  g_cfg->SetMatchParens(true);
  GIVEN("an empty code cell") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();

    WHEN("a string containing an escaped quote is typed") {
      TypeAll(editor, wxS("\"a\\\"b\""));
      THEN("the text is exactly what was typed") {
        REQUIRE(editor->GetValue() == wxS("\"a\\\"b\""));
        REQUIRE(editor->CursorPosition() == editor->GetValue().Length());
      }
    }
  }
}

SCENARIO("Backspace after an escaped opener deletes only the opener") {
  g_cfg->SetMatchParens(true);
  GIVEN("a\\() with the caret after the escaped bracket") {
    auto group = MakeCodeCell(wxS("a\\()"));
    EditorCell *editor = group->GetEditable();
    editor->CursorPosition(3);

    WHEN("Backspace is pressed") {
      PressBackspace(editor);
      THEN("the \")\" that follows stays, as it is no partner of it") {
        REQUIRE(editor->GetValue() == wxS("a\\)"));
      }
    }
  }
  GIVEN("a() with the caret inside the pair") {
    auto group = MakeCodeCell(wxS("a()"));
    EditorCell *editor = group->GetEditable();
    editor->CursorPosition(2);

    WHEN("Backspace is pressed") {
      PressBackspace(editor);
      THEN("both halves of the empty pair go, as before") {
        REQUIRE(editor->GetValue() == wxS("a"));
      }
    }
  }
}

SCENARIO("An escaped bracket under the cursor has no partner to highlight") {
  GIVEN("a name with an escaped bracket followed by a real pair") {
    auto group = MakeCodeCell(wxS("a\\((x)"));
    EditorCell *editor = group->GetEditable();

    WHEN("the cursor is on the escaped bracket") {
      editor->CursorPosition(2);
      editor->FindMatchingParens();
      THEN("nothing is highlighted") {
        REQUIRE(editor->GetMatchingParens() == std::pair<long, long>(-1, -1));
      }
    }
    WHEN("the cursor is on the real opening bracket") {
      editor->CursorPosition(3);
      editor->FindMatchingParens();
      THEN("its real partner is highlighted") {
        REQUIRE(editor->GetMatchingParens() == std::pair<long, long>(3, 5));
      }
    }
    WHEN("the cursor is on the real closing bracket") {
      editor->CursorPosition(5);
      editor->FindMatchingParens();
      THEN("the real opening bracket is highlighted, not the escaped one") {
        REQUIRE(editor->GetMatchingParens() == std::pair<long, long>(3, 5));
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
