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
  What the toolbar's bold/italic/... buttons do to a text cell (GH #492):
  formatting a selection, switching a format on for what is typed next,
  undoing it, and that the formatted text is laid out in the formatted font.

  test_TextFormat covers how formats follow edits and how they are saved;
  this covers the EditorCell that holds them.

  Windowless: a real GroupCell/EditorCell against a memory-DC Configuration, in
  the manner of test_EditorCellBidi.
*/

#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

//! Builds a cell of the given type holding \p text and lays it out.
std::unique_ptr<GroupCell> MakeCell(const wxString &text,
                                    GroupType type = GC_TYPE_TEXT) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, text);
  group->Recalculate();
  return group;
}

//! Lays the cell out again after its contents changed.
void Relayout(GroupCell *group) {
  group->ResetSize_Recursively();
  group->Recalculate();
}
} // namespace

SCENARIO("Formatting a selection") {
  GIVEN("a text cell with a word selected") {
    auto group = MakeCell(wxS("plainboldplain"));
    EditorCell *editor = group->GetEditable();
    editor->SetSelection(5, 9);
    REQUIRE_FALSE(editor->HasFormat(TextFormat::Bold));
    WHEN("bold is toggled") {
      REQUIRE(editor->ToggleFormat(TextFormat::Bold));
      THEN("exactly the selected characters are bold") {
        REQUIRE(editor->HasFormat(TextFormat::Bold));
        const TextFormat::Formats &formats = editor->GetFormats();
        for (size_t i = 0; i < editor->GetValue().Length(); ++i)
          REQUIRE((TextFormat::At(formats, i) == TextFormat::Bold) ==
                  (i >= 5 && i < 9));
      }
      THEN("toggling it again makes them plain again") {
        REQUIRE(editor->ToggleFormat(TextFormat::Bold));
        REQUIRE_FALSE(editor->HasFormat(TextFormat::Bold));
        REQUIRE(TextFormat::IsPlain(editor->GetFormats()));
      }
      THEN("undo makes them plain again, keeping the text") {
        REQUIRE(editor->CanUndo());
        editor->Undo();
        REQUIRE(editor->GetValue() == wxS("plainboldplain"));
        REQUIRE(TextFormat::IsPlain(editor->GetFormats()));
      }
      THEN("the cell gets wider, as bold text is in a proportional font") {
        auto plain = MakeCell(wxS("plainboldplain"));
        Relayout(group.get());
        REQUIRE(editor->GetWidth() > plain->GetEditable()->GetWidth());
      }
    }
    WHEN("underline is toggled") {
      auto plain = MakeCell(wxS("plainboldplain"));
      REQUIRE(editor->ToggleFormat(TextFormat::Underline));
      Relayout(group.get());
      THEN("the cell keeps its width: the line is drawn on top of the text") {
        REQUIRE(editor->GetWidth() == plain->GetEditable()->GetWidth());
      }
    }
  }
  GIVEN("a selection that is only partly bold") {
    auto group = MakeCell(wxS("abcd"));
    EditorCell *editor = group->GetEditable();
    editor->SetSelection(0, 2);
    editor->ToggleFormat(TextFormat::Bold);
    editor->SetSelection(0, 4);
    THEN("it doesn't count as bold") {
      REQUIRE_FALSE(editor->HasFormat(TextFormat::Bold));
    }
    THEN("toggling makes all of it bold, as in a word processor") {
      editor->ToggleFormat(TextFormat::Bold);
      REQUIRE(editor->GetFormats() == TextFormat::Formats(4, TextFormat::Bold));
    }
  }
}

SCENARIO("Switching a format on with nothing selected") {
  GIVEN("a caret at the end of a plain text cell") {
    auto group = MakeCell(wxS("ab"));
    EditorCell *editor = group->GetEditable();
    editor->CursorPosition(2);
    WHEN("italic is switched on") {
      THEN("the text itself doesn't change") {
        REQUIRE_FALSE(editor->ToggleFormat(TextFormat::Italic));
        REQUIRE(TextFormat::IsPlain(editor->GetFormats()));
      }
      THEN("the button shows it is on") {
        editor->ToggleFormat(TextFormat::Italic);
        REQUIRE(editor->HasFormat(TextFormat::Italic));
      }
      THEN("what is typed next is italic") {
        editor->ToggleFormat(TextFormat::Italic);
        editor->InsertText(wxS("cd"));
        REQUIRE(editor->GetValue() == wxS("abcd"));
        const TextFormat::Formats expected = {TextFormat::None, TextFormat::None,
                                              TextFormat::Italic, TextFormat::Italic};
        REQUIRE(editor->GetFormats() == expected);
      }
    }
  }
}

SCENARIO("Code cells aren't formatted") {
  GIVEN("a code cell with a selection") {
    auto group = MakeCell(wxS("a*b=c*d;"), GC_TYPE_CODE);
    EditorCell *editor = group->GetEditable();
    editor->SetSelection(0, 3);
    THEN("formatting is refused: code cells have syntax highlighting instead") {
      REQUIRE_FALSE(editor->CanFormat());
      REQUIRE_FALSE(editor->ToggleFormat(TextFormat::Bold));
      REQUIRE(TextFormat::IsPlain(editor->GetFormats()));
    }
  }
}

SCENARIO("Formats move to the cell that replaces a cell") {
  GIVEN("a text cell with a bold word") {
    auto source = MakeCell(wxS("ab"));
    source->GetEditable()->SetSelection(0, 1);
    source->GetEditable()->ToggleFormat(TextFormat::Bold);
    WHEN("its contents are moved to a new cell, as changing a cell's style does") {
      auto target = MakeCell(wxS("ab"), GC_TYPE_SECTION);
      target->GetEditable()->CopyFormatsFrom(*source->GetEditable());
      THEN("the new cell has the same formatting") {
        REQUIRE(target->GetEditable()->GetFormats() ==
                source->GetEditable()->GetFormats());
      }
    }
    WHEN("the new cell is a code cell") {
      auto target = MakeCell(wxS("ab"), GC_TYPE_CODE);
      target->GetEditable()->CopyFormatsFrom(*source->GetEditable());
      THEN("the formatting is dropped") {
        REQUIRE(TextFormat::IsPlain(target->GetEditable()->GetFormats()));
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
  // Text cells default to a monospace font, whose bold glyphs are exactly as
  // wide as its regular ones - which would hide whether bold text is laid out
  // in the bold font at all.
  g_cfg->GetWritableStyle(TS_TEXT)->SetFontName(wxS("DejaVu Sans"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
