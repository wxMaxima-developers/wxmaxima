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
  Tests for links in text cells and in Maxima's output (GH #2396).

  A link in a text cell becomes a text snippet of its own, which Draw() paints
  in the link color and GetLinkAt() hit-tests -- both through the same walk
  over the snippets, WalkDrawnSnippets(). These tests pin GetLinkAt() against
  PositionToPoint(), an independent source for where each character is drawn:
  if the two ever disagree, a Ctrl+click would open a link the pointer isn't
  on, or miss one it is.

  Links in the output are TextCells parsed from the XML wxMathML.lisp sends,
  through the real MathParser; they are checked against the cell's own
  rectangle, whose parts left and right of the link are known to be plain
  text.

  Windowless: real GroupCell/EditorCell against a memory-DC Configuration, no
  Worksheet, no wxFrame -- the test_EditorCellTabs pattern.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"
#include "MathParser.h"

#include <memory>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

//! Builds a cell of the given type holding text, laid out at a known place.
std::unique_ptr<GroupCell> MakeCell(GroupType type, const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, text);
  group->SetCurrentPoint(wxPoint(50, 50));
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  return group;
}

/*! A point just inside the character at position pos.

  Deliberately not "halfway to the next position": at the end of a line the
  next position is at the start of the following one.
*/
wxPoint Inside(EditorCell *editor, size_t pos) {
  const wxPoint left = editor->PositionToPoint(pos);
  return wxPoint(left.x + 2, left.y);
}
} // namespace

SCENARIO("A link in a text cell is found under the pointer") {
  GIVEN("a text cell with a link in the middle of a sentence") {
    const wxString text = wxS("Read https://example.org/docs first.");
    auto group = MakeCell(GC_TYPE_TEXT, text);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    REQUIRE(editor->GetCurrentPoint().x >= 0);
    const size_t linkStart = text.Find(wxS("https"));
    const size_t linkEnd = linkStart + wxString(wxS("https://example.org/docs")).Length();

    THEN("every character of the link finds it") {
      for (size_t pos = linkStart; pos < linkEnd; ++pos) {
        INFO("position " << pos);
        REQUIRE(editor->GetLinkAt(Inside(editor, pos)) ==
                wxS("https://example.org/docs"));
      }
    }
    THEN("the words around it don't") {
      REQUIRE(editor->GetLinkAt(Inside(editor, 1)).empty());
      REQUIRE(editor->GetLinkAt(Inside(editor, linkEnd + 1)).empty());
      REQUIRE(editor->GetLinkAt(Inside(editor, text.Length() - 2)).empty());
    }
    THEN("the pixel right after the link's end isn't on it") {
      const wxPoint end = editor->PositionToPoint(linkEnd - 1);
      const wxPoint last = editor->PositionToPoint(linkEnd);
      REQUIRE(last.y == end.y);
      REQUIRE(editor->GetLinkAt(wxPoint(last.x - 2, last.y)) ==
              wxS("https://example.org/docs"));
      REQUIRE(editor->GetLinkAt(wxPoint(last.x + 1, last.y)).empty());
    }
    THEN("a point outside the cell doesn't") {
      REQUIRE(editor->GetLinkAt(wxPoint(0, 0)).empty());
    }
    THEN("the cell's text is unchanged") {
      REQUIRE(editor->GetValue() == text);
    }
  }

  GIVEN("a link on the second line, after a tab") {
    const wxString text = wxS("first line\n\thttp://b.org");
    auto group = MakeCell(GC_TYPE_TEXT, text);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    const size_t linkStart = text.Find(wxS("http"));

    THEN("it is found where it is drawn") {
      REQUIRE(editor->GetLinkAt(Inside(editor, linkStart + 2)) ==
              wxS("http://b.org"));
      REQUIRE(editor->GetLinkAt(Inside(editor, 2)).empty());
    }
  }

  GIVEN("a bullet list item that is a link") {
    const wxString text = wxS("* https://a.org\n* plain");
    auto group = MakeCell(GC_TYPE_TEXT, text);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("the link is found and the plain item isn't one") {
      REQUIRE(editor->GetLinkAt(Inside(editor, 5)) == wxS("https://a.org"));
      REQUIRE(editor->GetLinkAt(Inside(editor, text.Length() - 2)).empty());
    }
  }

  GIVEN("a link that automatic line wrapping moves to a line of its own") {
    wxString text;
    for (int i = 0; i < 150; ++i)
      text += wxS("word ");
    const size_t linkStart = text.Length();
    text += wxS("https://example.org/wrapped");
    auto group = MakeCell(GC_TYPE_TEXT, text);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("it really was wrapped, and is still found under the pointer") {
      REQUIRE(editor->PositionToPoint(linkStart).y >
              editor->PositionToPoint(0).y);
      REQUIRE(editor->GetLinkAt(Inside(editor, linkStart + 3)) ==
              wxS("https://example.org/wrapped"));
    }
  }

  GIVEN("a code cell containing an address") {
    const wxString text = wxS("s:\"https://example.org\";");
    auto group = MakeCell(GC_TYPE_CODE, text);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("code never has links") {
      REQUIRE(editor->GetLinkAt(Inside(editor, 8)).empty());
    }
  }
}

SCENARIO("The LaTeX export writes a text cell's links as \\url{}") {
  GIVEN("a link with characters LaTeX would otherwise escape") {
    auto group = MakeCell(GC_TYPE_TEXT,
                          wxS("See https://a.org/x_y%20z#top?a=1&b=2 for 100% more."));
    const wxString tex = group->GetEditable()->ToTeX();

    THEN("the address is written verbatim inside \\url{}") {
      INFO(tex.ToStdString());
      REQUIRE(tex.Contains(wxS("\\url{https://a.org/x_y%20z#top?a=1&b=2}")));
    }
    THEN("the text around it is still escaped") {
      INFO(tex.ToStdString());
      REQUIRE(tex.Contains(wxS("100\\%")));
    }
  }
  GIVEN("a section heading containing a link") {
    auto group = MakeCell(GC_TYPE_SECTION, wxS("About https://a.org/x_y"));
    const wxString tex = group->GetEditable()->ToTeX();

    THEN("it stays escaped text, since \\url{} can't go into \\section{}") {
      INFO(tex.ToStdString());
      REQUIRE_FALSE(tex.Contains(wxS("\\url")));
      REQUIRE(tex.Contains(wxS("x\\_y")));
    }
  }
}

namespace {
//! A code cell whose output is what Maxima sends as xml, laid out at a known place.
std::unique_ptr<GroupCell> MakeOutput(const wxString &xml) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("x;"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(xml);
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  group->SetCurrentPoint(wxPoint(50, 50));
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  return group;
}

/*! The first TextCell in list, or inside it, whose text is text.

  MathParser stores a "-" as a unicode minus sign, so that is what is compared.
*/
TextCell *FindText(Cell *list, wxString text) {
  text.Replace(wxS("-"), wxS("\u2212"));
  for (Cell &cell : OnList(list)) {
    if (auto *textCell = dynamic_cast<TextCell *>(&cell))
      if (textCell->GetValue() == text)
        return textCell;
    for (Cell &inner : OnInner(&cell))
      if (TextCell *found = FindText(&inner, text))
        return found;
  }
  return nullptr;
}
TextCell *FindText(GroupCell *group, const wxString &text) {
  return FindText(group->GetOutput(), text);
}

//! The vertical middle of a cell, at a horizontal fraction of its width.
wxPoint At(const Cell *cell, double fraction) {
  const wxRect rect = cell->GetRect();
  return wxPoint(rect.GetLeft() + static_cast<int>(rect.GetWidth() * fraction),
                 rect.GetTop() + rect.GetHeight() / 2);
}
} // namespace

SCENARIO("A link Maxima prints is found under the pointer") {
  const wxString link = wxS("https://example.org/a/rather/long/path/to/a/page");
  GIVEN("a string with a link between two words") {
    const wxString text = wxS("Read ") + link + wxS(" first");
    auto group = MakeOutput(wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl><st>") +
                            text + wxS("</st></mth>"));
    TextCell *cell = FindText(group.get(), text);
    REQUIRE(cell != nullptr);
    REQUIRE(cell->GetWidth() > 0);

    THEN("the middle of the cell is the link") {
      REQUIRE(cell->GetLinkAt(At(cell, 0.5)) == link);
      REQUIRE(group->GetLinkAt(At(cell, 0.5)) == link);
    }
    THEN("the words before and after it aren't") {
      REQUIRE(cell->GetLinkAt(At(cell, 0.02)).empty());
      REQUIRE(cell->GetLinkAt(At(cell, 0.98)).empty());
    }
    THEN("a point outside the cell isn't either") {
      REQUIRE(group->GetLinkAt(wxPoint(0, 0)).empty());
    }
  }

  GIVEN("a link inside a list") {
    auto group = MakeOutput(wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>"
                                "<mrow list=\"true\"><t listdelim=\"true\">[</t>"
                                "<mrow><n>1</n></mrow><mo>,</mo><mrow><st>") +
                            link + wxS("</st></mrow><t listdelim=\"true\">]</t></mrow></mth>"));
    TextCell *cell = FindText(group.get(), link);
    REQUIRE(cell != nullptr);

    THEN("the group finds it among the cells inside the list") {
      REQUIRE(group->GetLinkAt(At(cell, 0.5)) == link);
    }
  }

  GIVEN("an address with hyphens") {
    const wxString hyphenated = wxS("https://en.wikipedia.org/wiki/Rolle-type_theorem");
    auto group = MakeOutput(wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl><st>") +
                            hyphenated + wxS("</st></mth>"));
    TextCell *cell = FindText(group.get(), hyphenated);
    REQUIRE(cell != nullptr);

    THEN("the link has plain hyphens, not the minus signs maths is drawn with") {
      REQUIRE(cell->GetLinkAt(At(cell, 0.5)) == hyphenated);
    }
  }

  GIVEN("an address that is displayed differently from what Maxima printed") {
    // "->" is displayed as an arrow; the link on screen would lead elsewhere
    // than what Maxima printed, so it isn't offered at all.
    const wxString text = wxS("https://a.org/x->y");
    auto group = MakeOutput(wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl><st>") +
                            text + wxS("</st></mth>"));
    TextCell *cell = FindText(group.get(), text);
    REQUIRE(cell != nullptr);

    THEN("it is no link") {
      REQUIRE(cell->GetLinkAt(At(cell, 0.3)).empty());
    }
  }

  GIVEN("output without any link") {
    auto group = MakeOutput(wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>"
                                "<st>time: 12:30</st></mth>"));
    TextCell *cell = FindText(group.get(), wxS("time: 12:30"));
    REQUIRE(cell != nullptr);

    THEN("nothing is found anywhere in it") {
      for (double f = 0.05; f < 1.0; f += 0.1)
        REQUIRE(group->GetLinkAt(At(cell, f)).empty());
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
  // Wide enough that the short sentences below fit on one line; the wrapping
  // scenario makes its text long enough to wrap anyway.
  g_cfg->SetCanvasSize(wxSize(1200, 800));
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
