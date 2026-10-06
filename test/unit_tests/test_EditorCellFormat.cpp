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

SCENARIO("Superscript and subscript") {
  GIVEN("a text cell with one character selected") {
    auto group = MakeCell(wxS("x2"));
    EditorCell *editor = group->GetEditable();
    editor->SetSelection(1, 2);
    WHEN("it is made a superscript") {
      REQUIRE(editor->ToggleFormat(TextFormat::Superscript));
      THEN("it is one") {
        REQUIRE(editor->GetFormats() ==
                TextFormat::Formats({TextFormat::None, TextFormat::Superscript}));
        REQUIRE(editor->HasFormat(TextFormat::Superscript));
      }
      THEN("making it a subscript makes it no longer a superscript") {
        REQUIRE(editor->ToggleFormat(TextFormat::Subscript));
        REQUIRE(editor->GetFormats() ==
                TextFormat::Formats({TextFormat::None, TextFormat::Subscript}));
        REQUIRE_FALSE(editor->HasFormat(TextFormat::Superscript));
      }
      THEN("it can still be made bold, too") {
        REQUIRE(editor->ToggleFormat(TextFormat::Bold));
        REQUIRE(editor->GetFormats()[1] == (TextFormat::Bold | TextFormat::Superscript));
      }
      THEN("the cell gets narrower: superscripts are smaller") {
        auto plain = MakeCell(wxS("x2"));
        Relayout(group.get());
        REQUIRE(editor->GetWidth() < plain->GetEditable()->GetWidth());
      }
    }
  }
  GIVEN("nothing selected") {
    auto group = MakeCell(wxS("x"));
    EditorCell *editor = group->GetEditable();
    editor->SetCaretPosition(1);
    WHEN("subscript is switched on while superscript is pending") {
      editor->ToggleFormat(TextFormat::Superscript);
      editor->ToggleFormat(TextFormat::Subscript);
      THEN("only the subscript is pending") {
        REQUIRE(editor->HasFormat(TextFormat::Subscript));
        REQUIRE_FALSE(editor->HasFormat(TextFormat::Superscript));
      }
    }
  }
}

SCENARIO("Each format gets a font made from the cell's own") {
  // With wxWidgets' Qt port a default-constructed wxFont counts as "ok", so
  // a cache that told "not made yet" by IsOk() handed every format the same
  // unrelated default font: formatted text came out narrower than plain
  // text, and bold, subscript and italic all looked alike.
  auto group = MakeCell(wxS("abc"));
  EditorCell *editor = group->GetEditable();
  editor->SetSelection(0, 1);
  editor->ToggleFormat(TextFormat::Bold);
  const wxFont plain = editor->GetFont();
  const double size = plain.GetFractionalPointSize();
  REQUIRE(size > 0);
  THEN("bold is the cell's font, in bold") {
    const wxFont &bold = editor->GetFont(TextFormat::Bold);
    REQUIRE(bold.GetFaceName() == plain.GetFaceName());
    REQUIRE(bold.GetFractionalPointSize() == Approx(size));
    REQUIRE(bold.GetWeight() == wxFONTWEIGHT_BOLD);
    REQUIRE(bold.GetStyle() == plain.GetStyle());
  }
  THEN("italic is the cell's font, in italic") {
    const wxFont &italic = editor->GetFont(TextFormat::Italic);
    REQUIRE(italic.GetFaceName() == plain.GetFaceName());
    REQUIRE(italic.GetFractionalPointSize() == Approx(size));
    REQUIRE(italic.GetStyle() == wxFONTSTYLE_ITALIC);
    REQUIRE(italic.GetWeight() == plain.GetWeight());
  }
  THEN("a bold subscript is bold and smaller, a plain one only smaller") {
    const wxFont &boldSub =
      editor->GetFont(TextFormat::Format(TextFormat::Bold | TextFormat::Subscript));
    const wxFont &sub = editor->GetFont(TextFormat::Subscript);
    REQUIRE(boldSub.GetFaceName() == plain.GetFaceName());
    REQUIRE(boldSub.GetWeight() == wxFONTWEIGHT_BOLD);
    REQUIRE(boldSub.GetFractionalPointSize() < size);
    REQUIRE(sub.GetWeight() == plain.GetWeight());
    REQUIRE(sub.GetFractionalPointSize() == Approx(boldSub.GetFractionalPointSize()));
  }
  THEN("text in a bold font is wider than plain text") {
    Relayout(group.get());
    editor->SetCurrentPoint(wxPoint(10, 100));
    auto plainCell = MakeCell(wxS("abc"));
    plainCell->GetEditable()->SetCurrentPoint(wxPoint(10, 100));
    REQUIRE(editor->PositionToPoint(1).x > plainCell->GetEditable()->PositionToPoint(1).x);
  }
}

SCENARIO("Overlapping formats keep every character its own width") {
  // The order Gunter formatted things in: a subscript, then bold across it
  // and the text around it, then italic from inside the bold to past it.
  auto group = MakeCell(wxS("aaxbbcc"));
  EditorCell *editor = group->GetEditable();
  editor->SetSelection(2, 3);
  editor->ToggleFormat(TextFormat::Subscript);
  editor->SetSelection(1, 5);
  editor->ToggleFormat(TextFormat::Bold);
  editor->SetSelection(4, 6);
  editor->ToggleFormat(TextFormat::Italic);
  Relayout(group.get());
  editor->SetCurrentPoint(wxPoint(10, 100));
  THEN("each character has the formats it was given") {
    using namespace TextFormat;
    REQUIRE(editor->GetFormats() ==
            Formats({None, Bold, Format(Bold | Subscript), Bold, Format(Bold | Italic),
                     Italic, None}));
  }
  THEN("no character is laid out with zero width") {
    for (size_t i = 0; i < editor->GetValue().Length(); ++i)
      REQUIRE(editor->PositionToPoint(i + 1).x > editor->PositionToPoint(i).x);
  }
  THEN("the bold text after the subscript is as wide as bold text elsewhere") {
    const wxCoord boldA = editor->PositionToPoint(2).x - editor->PositionToPoint(1).x;
    const wxCoord boldB = editor->PositionToPoint(4).x - editor->PositionToPoint(3).x;
    const wxCoord subX = editor->PositionToPoint(3).x - editor->PositionToPoint(2).x;
    // "a" and "b" differ a little; a subscript is far narrower than either.
    REQUIRE(subX < boldA);
    REQUIRE(subX < boldB);
  }
}

SCENARIO("A line grows to make room for what is raised or lowered") {
  // The cell's geometry: its height, and where the middle of its first
  // line's plain text is.
  auto layout = [](const wxString &text, size_t start, size_t end,
                   TextFormat::Format format) {
    auto group = MakeCell(text);
    EditorCell *editor = group->GetEditable();
    if (format != TextFormat::None) {
      editor->SetSelection(start, end);
      editor->ToggleFormat(format);
    }
    Relayout(group.get());
    editor->SetCurrentPoint(wxPoint(10, 100));
    return group;
  };
  GIVEN("a one-line cell") {
    auto plain = layout(wxS("x2"), 0, 0, TextFormat::None);
    const EditorCell *plainEditor = plain->GetEditable();
    WHEN("a character in it is a superscript") {
      auto sup = layout(wxS("x2"), 1, 2, TextFormat::Superscript);
      THEN("the room is added above the text") {
        REQUIRE(sup->GetEditable()->GetHeight() > plainEditor->GetHeight());
        REQUIRE(sup->GetEditable()->GetCenter() > plainEditor->GetCenter());
        REQUIRE(sup->GetEditable()->GetHeight() - sup->GetEditable()->GetCenter() ==
                plainEditor->GetHeight() - plainEditor->GetCenter());
      }
    }
    WHEN("a character in it is a subscript") {
      auto sub = layout(wxS("x2"), 1, 2, TextFormat::Subscript);
      THEN("the room is added below the text") {
        REQUIRE(sub->GetEditable()->GetHeight() > plainEditor->GetHeight());
        REQUIRE(sub->GetEditable()->GetCenter() == plainEditor->GetCenter());
      }
    }
    WHEN("a character in it is bold") {
      auto bold = layout(wxS("x2"), 1, 2, TextFormat::Bold);
      THEN("the line keeps its height") {
        REQUIRE(bold->GetEditable()->GetHeight() == plainEditor->GetHeight());
        REQUIRE(bold->GetEditable()->GetCenter() == plainEditor->GetCenter());
      }
    }
  }
  GIVEN("three lines, the middle one with a superscript") {
    const wxString text = wxS("a\nx2\nb");
    auto plain = layout(text, 0, 0, TextFormat::None);
    auto sup = layout(text, 3, 4, TextFormat::Superscript);
    EditorCell *plainEditor = plain->GetEditable();
    EditorCell *supEditor = sup->GetEditable();
    THEN("the first line stays where it was") {
      REQUIRE(supEditor->PositionToPoint(0).y == plainEditor->PositionToPoint(0).y);
    }
    THEN("the middle line moves down, to make room above it") {
      REQUIRE(supEditor->PositionToPoint(2).y > plainEditor->PositionToPoint(2).y);
    }
    THEN("the last line moves down by just as much") {
      REQUIRE(supEditor->PositionToPoint(5).y - plainEditor->PositionToPoint(5).y ==
              supEditor->PositionToPoint(2).y - plainEditor->PositionToPoint(2).y);
    }
    THEN("a click on each line puts the cursor into that line") {
      for (const size_t pos : {size_t(0), size_t(2), size_t(5)}) {
        supEditor->SelectPointText(supEditor->PositionToPoint(pos) + wxPoint(1, 0));
        REQUIRE(supEditor->CursorPosition() == pos);
      }
    }
    THEN("a click on the superscript's room above the line still hits that line") {
      // The middle line moved down by exactly the room above it; aim at the
      // topmost pixel of that room. PositionToPoint() is the middle of the
      // plain text, half a line (the first line's GetCenter()) below its top.
      const wxCoord room = supEditor->PositionToPoint(2).y - plainEditor->PositionToPoint(2).y;
      REQUIRE(room > 0);
      supEditor->SelectPointText(supEditor->PositionToPoint(2) +
                                 wxPoint(1, -plainEditor->GetCenter() - room));
      REQUIRE(supEditor->CursorPosition() >= 2);
      REQUIRE(supEditor->CursorPosition() <= 4);
    }
    THEN("a click on the bottom pixel of the middle line's text hits that line") {
      // With every line equally high this pixel would belong to the last
      // line: the middle line has moved down into its place.
      const wxCoord lineHeight =
        plainEditor->PositionToPoint(2).y - plainEditor->PositionToPoint(0).y;
      supEditor->SelectPointText(supEditor->PositionToPoint(2) +
                                 wxPoint(1, lineHeight - plainEditor->GetCenter() - 1));
      REQUIRE(supEditor->CursorPosition() >= 2);
      REQUIRE(supEditor->CursorPosition() <= 4);
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
