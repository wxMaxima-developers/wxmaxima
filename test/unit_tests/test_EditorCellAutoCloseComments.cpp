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
  Tests that a comment typed into a code cell gets its closer auto-inserted,
  like a bracket does (GH #1641).

  Typing slash and star adds star and slash after the caret, typing the
  closer in front of the auto-inserted one jumps over it, and Backspace on an
  empty comment removes both halves. Inside a string and in text cells
  nothing happens.

  Windowless: real GroupCell/EditorCell against a memory-DC Configuration, the
  test_EditorCellEscapedParens pattern.
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

namespace {
std::unique_ptr<GroupCell> MakeTextCell(const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, text);
  group->Recalculate();
  group->GetEditable()->CursorPosition(text.Length());
  return group;
}
} // namespace

SCENARIO("A typed comment opener gets its closer") {
  g_cfg->SetMatchParens(true);
  g_cfg->SetInsertAns(false);
  GIVEN("a code cell holding a:1$ ") {
    auto group = MakeCodeCell(wxS("a:1$ "));
    EditorCell *editor = group->GetEditable();

    WHEN("/* is typed") {
      TypeAll(editor, wxS("/*"));
      THEN("the closer follows the caret") {
        REQUIRE(editor->GetValue() == wxS("a:1$ /**/"));
        REQUIRE(editor->CursorPosition() == 7);
      }
      AND_WHEN("a comment and its closer are typed") {
        TypeAll(editor, wxS(" note */"));
        THEN("the caret jumped over the existing closer, none was added") {
          REQUIRE(editor->GetValue() == wxS("a:1$ /* note */"));
          REQUIRE(editor->CursorPosition() == editor->GetValue().Length());
        }
      }
      AND_WHEN("Backspace is pressed") {
        PressBackspace(editor);
        THEN("the empty comment's closer goes with its star") {
          REQUIRE(editor->GetValue() == wxS("a:1$ /"));
          REQUIRE(editor->CursorPosition() == 6);
        }
      }
    }
  }
}

SCENARIO("A comment opener at the start of a cell with insert-% on") {
  g_cfg->SetMatchParens(true);
  g_cfg->SetInsertAns(true);
  GIVEN("an empty code cell") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    WHEN("/* is typed") {
      TypeAll(editor, wxS("/*"));
      THEN("the % is dropped again and the closer added") {
        REQUIRE(editor->GetValue() == wxS("/**/"));
        REQUIRE(editor->CursorPosition() == 2);
      }
    }
  }
  g_cfg->SetInsertAns(false);
}

SCENARIO("No closer is added where /* opens no comment") {
  g_cfg->SetMatchParens(true);
  g_cfg->SetInsertAns(false);
  GIVEN("an empty code cell") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    WHEN("/* is typed inside a string") {
      TypeAll(editor, wxS("s:\"a/*"));
      THEN("only the string's quote was auto-closed") {
        REQUIRE(editor->GetValue() == wxS("s:\"a/*\""));
      }
    }
    WHEN("/* is typed after a string that contains an escaped quote") {
      TypeAll(editor, wxS("s:\"a\\\"\"$ /*"));
      THEN("the comment is closed") {
        REQUIRE(editor->GetValue() == wxS("s:\"a\\\"\"$ /**/"));
      }
    }
    WHEN("/* is typed inside a comment") {
      TypeAll(editor, wxS("/* x /*"));
      THEN("the nested comment is closed, too, as Maxima nests them") {
        REQUIRE(editor->GetValue() == wxS("/* x /**/*/"));
      }
    }
  }
  GIVEN("a text cell") {
    auto group = MakeTextCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    WHEN("/* is typed") {
      TypeAll(editor, wxS("a/*"));
      THEN("it stays as typed") {
        REQUIRE(editor->GetValue() == wxS("a/*"));
      }
    }
  }
  GIVEN("bracket matching switched off") {
    g_cfg->SetMatchParens(false);
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    WHEN("/* is typed") {
      TypeAll(editor, wxS("/*"));
      THEN("no closer is added") {
        REQUIRE(editor->GetValue() == wxS("/*"));
      }
    }
    g_cfg->SetMatchParens(true);
  }
}

SCENARIO("Ordinary division and multiplication are left alone") {
  g_cfg->SetMatchParens(true);
  g_cfg->SetInsertAns(false);
  GIVEN("an empty code cell") {
    auto group = MakeCodeCell(wxS(""));
    EditorCell *editor = group->GetEditable();
    WHEN("a*b/c is typed") {
      TypeAll(editor, wxS("a*b/c"));
      THEN("the text is exactly that") {
        REQUIRE(editor->GetValue() == wxS("a*b/c"));
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
