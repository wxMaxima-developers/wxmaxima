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
  Selecting some of the characters of a text output with the mouse (GH #825).

  The drags go through Worksheet::SelectOutputRect(), which is what a mouse
  move during a drag in an output calls, so what is tested is what the mouse
  does. Besides selecting characters, the tests pin how this gets along with
  the other kinds of selection: a drag that leaves the text selects whole
  cells, coming back selects characters again, and any other change of the
  selection forgets the characters. The output is parsed from the XML Maxima
  sends, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"
#include "worksheet/OutputTextSelection.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
} // namespace

// Puts a code cell with this output into the (otherwise empty) worksheet and
// lays it out.
static GroupCell *ShowOutput(const wxString &output) {
  g_ws->DestroyTree();
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("a;"));
  MathParser parser(g_cfg);
  parser.SetGroup(group.get());
  group->AppendOutput(parser.ParseLine(wxS("<mth>") + output + wxS("</mth>")));
  g_ws->InsertGroupCells(std::move(group), nullptr, nullptr);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetTree() != nullptr);
  REQUIRE(g_ws->GetTree()->GetLabel() != nullptr);
  return g_ws->GetTree();
}

// The n-th cell of the output, in the order the cells are drawn
static Cell *OutputCell(GroupCell *group, int n) {
  for (Cell &cell : OnDrawList(group->GetLabel()))
    if (n-- == 0)
      return &cell;
  return nullptr;
}

static TextCell *TextOf(Cell *cell) {
  auto *text = dynamic_cast<TextCell *>(cell);
  REQUIRE(text != nullptr);
  return text;
}

// A point just right of the start of character index of the text cell,
// vertically in its middle
static wxPoint At(TextCell *cell, std::size_t index) {
  return {cell->CharacterX(index) + 1, cell->GetRect().GetTop() +
          cell->GetRect().GetHeight() / 2};
}

static const DocumentCellPointers &Pointers() {
  return g_ws->GetDocumentCellPointers();
}

// An error message, split into pieces the worksheet may break it into lines
// between, as wxMaxima does with every line of text Maxima prints
static const wxString splitError =
  wxS("<t type=\"error\">division </t>"
      "<t type=\"error\" continuation=\"true\">by </t>"
      "<t type=\"error\" continuation=\"true\">zero</t>");

SCENARIO("Positions between characters of a text cell") {
  GroupCell *group = ShowOutput(wxS("<t type=\"error\">expression</t>"));
  TextCell *text = TextOf(OutputCell(group, 0));
  REQUIRE(text->AllowsCharacterSelection());
  REQUIRE(text->DisplayedLength() == 10);
  THEN("they grow from the start to the end of the cell's text") {
    for (std::size_t i = 1; i <= text->DisplayedLength(); ++i)
      CHECK(text->CharacterX(i) > text->CharacterX(i - 1));
    CHECK(text->CharacterX(0) >= text->GetRect().GetLeft());
    CHECK(text->CharacterX(10) <= text->GetRect().GetRight() + 1);
  }
  THEN("a point finds the nearest one, and outside the text its end") {
    for (std::size_t i = 0; i <= text->DisplayedLength(); ++i)
      CHECK(text->CharacterIndexAt(text->CharacterX(i)) == i);
    CHECK(text->CharacterIndexAt(text->GetRect().GetLeft() - 100) == 0);
    CHECK(text->CharacterIndexAt(text->GetRect().GetRight() + 100) == 10);
  }
  g_ws->DestroyTree();
}

SCENARIO("Maths doesn't take part in selecting characters") {
  GroupCell *group = ShowOutput(wxS("<mi>abc</mi>"));
  THEN("a variable's characters aren't selectable") {
    CHECK_FALSE(TextOf(OutputCell(group, 0))->AllowsCharacterSelection());
  }
  g_ws->DestroyTree();
}

SCENARIO("Dragging inside a text cell selects characters (GH #825)") {
  GroupCell *group = ShowOutput(wxS("<t type=\"error\">expression</t>"));
  TextCell *text = TextOf(OutputCell(group, 0));

  WHEN("the drag goes from the 3rd to the 7th position") {
    g_ws->SelectOutputRect(group, At(text, 2), At(text, 6));
    THEN("those characters are selected, and the cell") {
      const auto range = Pointers().GetSelectedText();
      REQUIRE(range.has_value());
      CHECK(range->first == text);
      CHECK(range->last == text);
      CHECK(range->from == 2);
      CHECK(range->to == 6);
      CHECK(Pointers().GetSelectionStart() == text);
      CHECK(Pointers().GetSelectionEnd() == text);
      CHECK(g_ws->GetString() == wxS("pres"));
      CHECK(Pointers().GetSelectionString() == wxS("pres"));
    }
  }
  WHEN("the drag goes backwards") {
    g_ws->SelectOutputRect(group, At(text, 6), At(text, 2));
    THEN("the same characters are selected") {
      CHECK(g_ws->GetString() == wxS("pres"));
    }
  }
  WHEN("the drag goes on past the end of the text") {
    wxPoint beyond = At(text, 6);
    beyond.x = text->GetRect().GetRight() + 50;
    g_ws->SelectOutputRect(group, At(text, 6), beyond);
    THEN("everything up to the text's end is selected") {
      CHECK(g_ws->GetString() == wxS("sion"));
    }
  }
  WHEN("the drag hasn't covered a character yet") {
    g_ws->SelectOutputRect(group, At(text, 3), At(text, 3));
    THEN("the whole cell is selected, as a click selects it") {
      CHECK_FALSE(Pointers().GetSelectedText().has_value());
      CHECK(Pointers().GetSelectionStart() == text);
      CHECK(g_ws->GetString() == wxS("expression"));
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Selected characters may span several cells and lines") {
  GIVEN("a line of text split into pieces") {
    GroupCell *group = ShowOutput(splitError);
    TextCell *division = TextOf(OutputCell(group, 0));
    TextCell *zero = TextOf(OutputCell(group, 2));
    WHEN("the drag goes from inside the first piece into the last") {
      g_ws->SelectOutputRect(group, At(division, 4), At(zero, 2));
      THEN("the characters in between are selected, and no line break") {
        const auto range = Pointers().GetSelectedText();
        REQUIRE(range.has_value());
        CHECK(range->first == division);
        CHECK(range->last == zero);
        CHECK(g_ws->GetString() == wxS("sion by ze"));
      }
    }
  }
  GIVEN("two lines of text") {
    GroupCell *group = ShowOutput(wxS("<t>first line</t><t breakline=\"true\">second line</t>"));
    TextCell *first = TextOf(OutputCell(group, 0));
    TextCell *second = TextOf(OutputCell(group, 1));
    REQUIRE(second->GetRect().GetTop() > first->GetRect().GetBottom() - 1);
    WHEN("the drag goes from the first into the second") {
      g_ws->SelectOutputRect(group, At(first, 6), At(second, 6));
      THEN("the copied text has a line break between them") {
        CHECK(g_ws->GetString() == wxS("line\nsecond"));
      }
    }
    WHEN("the drag goes from the second up into the first") {
      g_ws->SelectOutputRect(group, At(second, 6), At(first, 6));
      THEN("the same text is selected") {
        CHECK(g_ws->GetString() == wxS("line\nsecond"));
      }
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Selected characters are copied as they read") {
  GroupCell *group = ShowOutput(wxS("<t>a-b -> c</t>"));
  TextCell *text = TextOf(OutputCell(group, 0));
  WHEN("all of the text is dragged over") {
    g_ws->SelectOutputRect(group, At(text, 0),
                           {text->GetRect().GetRight() + 10, At(text, 0).y});
    THEN("the minus sign and arrow are copied as Maxima printed them") {
      CHECK(g_ws->GetString() == wxS("a-b -> c"));
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Selecting characters and selecting cells don't get in each other's way") {
  // Text, then maths, then text again, all in one output
  GroupCell *group = ShowOutput(wxS("<t>before </t><mi>x</mi><t> after</t>"));
  TextCell *before = TextOf(OutputCell(group, 0));
  Cell *maths = OutputCell(group, 1);
  TextCell *after = TextOf(OutputCell(group, 2));
  REQUIRE_FALSE(TextOf(maths)->AllowsCharacterSelection());
  const wxPoint onMaths(maths->GetRect().GetLeft() + maths->GetRect().GetWidth() / 2,
                        maths->GetRect().GetTop() + maths->GetRect().GetHeight() / 2);

  WHEN("a drag starts on the maths") {
    g_ws->SelectOutputRect(group, onMaths, At(after, 3));
    THEN("it selects whole cells") {
      CHECK_FALSE(Pointers().GetSelectedText().has_value());
      CHECK(Pointers().GetSelectionStart() != nullptr);
    }
  }
  WHEN("a drag from the text goes on over the maths") {
    g_ws->SelectOutputRect(group, At(before, 2), onMaths);
    THEN("it selects whole cells") {
      CHECK_FALSE(Pointers().GetSelectedText().has_value());
      CHECK(Pointers().GetSelectionStart() == before);
      CHECK(Pointers().GetSelectionEnd() == maths);
    }
    AND_WHEN("it comes back into the text") {
      g_ws->SelectOutputRect(group, At(before, 2), At(before, 5));
      THEN("it selects characters again") {
        CHECK(Pointers().GetSelectedText().has_value());
        CHECK(g_ws->GetString() == wxS("for"));
      }
    }
  }
  WHEN("characters are selected") {
    g_ws->SelectOutputRect(group, At(before, 2), At(before, 5));
    REQUIRE(Pointers().GetSelectedText().has_value());
    AND_WHEN("the same cell is selected as a whole") {
      g_ws->SetSelection(before);
      THEN("the characters are forgotten") {
        CHECK_FALSE(Pointers().GetSelectedText().has_value());
        CHECK(g_ws->GetString() == before->ToString());
      }
    }
    AND_WHEN("the group cell is selected") {
      g_ws->SetSelection(group);
      THEN("the characters are forgotten") {
        CHECK_FALSE(Pointers().GetSelectedText().has_value());
      }
    }
    AND_WHEN("the selection is cleared") {
      g_ws->ClearSelection();
      THEN("nothing is selected") {
        CHECK_FALSE(Pointers().GetSelectedText().has_value());
        CHECK(g_ws->GetString().IsEmpty());
      }
    }
    AND_WHEN("the output the characters were in is deleted") {
      group->RemoveOutput();
      THEN("the characters are forgotten, too") {
        CHECK_FALSE(Pointers().GetSelectedText().has_value());
      }
    }
  }
  g_ws->DestroyTree();
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
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  g_cfg->ShowCodeCells(true);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
