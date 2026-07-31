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
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  What the caret does inside bidirectional text.

  EditorCell maps a caret position (an index into the content) to a point by
  measuring how wide the text before it is - PositionToPoint(). That is only
  correct while the text is drawn in the same order it is stored. In a
  right-to-left script it is not: the first character of a Hebrew word is drawn
  at its *right* end, so a caret one character in belongs near the right of the
  word and not, as the measurement says, one character's width from its left.

  These scenarios measure the mapping rather than assume it, for a left-to-right
  string as the control and a right-to-left one as the subject. They are written
  to describe what the caret *should* do, so they document the gap for as long as
  it exists.

  Windowless: a real GroupCell/EditorCell against a memory-DC Configuration, in
  the manner of test_EditorCellWrapping. The display comes from the run_headless
  wrapper ctest starts this through, so there is no Xvfb wrangling here.
*/

#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>

#include <vector>

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
} // namespace

namespace {

//! Hebrew, so every letter is a strong right-to-left character.
const wxString kHebrew = wxS("שלוםעולם");
//! The left-to-right control, same number of characters.
const wxString kLatin = wxS("abcdefgh");

//! The caret's x for every position in the text, cell-relative.
std::vector<wxCoord> CaretXs(EditorCell *editor, const wxString &text) {
  std::vector<wxCoord> xs;
  for (size_t i = 0; i <= text.Length(); i++)
    xs.push_back(editor->PositionToPoint(i).x - editor->GetCurrentPoint().x);
  return xs;
}

//! Builds a text cell holding \p text and lays it out.
std::unique_ptr<GroupCell> MakeCell(const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, text);
  group->Recalculate();
  return group;
}

} // namespace

SCENARIO("The caret follows the text it is placed in") {
  GIVEN("a left-to-right word") {
    auto group = MakeCell(kLatin);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    const std::vector<wxCoord> xs = CaretXs(editor, kLatin);

    THEN("it advances rightwards, one character at a time") {
      // The control: for Latin, stored order is drawn order, so each further
      // position is further right. If this fails the harness is wrong, not the
      // bidi handling.
      for (size_t i = 1; i < xs.size(); i++)
        REQUIRE(xs.at(i) >= xs.at(i - 1));
      REQUIRE(xs.back() > xs.front());
    }
  }

  GIVEN("a right-to-left word") {
    auto group = MakeCell(kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    const std::vector<wxCoord> xs = CaretXs(editor, kHebrew);

    THEN("the caret before the first letter is at the word's right end") {
      // The first stored character is drawn rightmost, so a caret in front of
      // it belongs at the right edge of the word - not at its left edge, which
      // is where measuring the (empty) text before it puts it.
      INFO("caret x per position: " << [&xs] {
          std::string s;
          for (const wxCoord x : xs)
            s += std::to_string(x) + " ";
          return s;
        }());
      REQUIRE(xs.front() > xs.back());
    }

    THEN("it advances leftwards as the position grows") {
      for (size_t i = 1; i < xs.size(); i++)
        REQUIRE(xs.at(i) <= xs.at(i - 1));
    }
  }
}

namespace {

//! Sends one arrow key to the cell and returns where the caret ended up.
size_t PressArrow(EditorCell *editor, int keyCode, size_t from) {
  editor->CursorPosition(from);
  wxKeyEvent event(wxEVT_KEY_DOWN);
  event.m_keyCode = keyCode;
  editor->ProcessEvent(event);
  return editor->CursorPosition();
}

} // namespace

SCENARIO("The arrow keys move the caret the way it is drawn") {
  GIVEN("a left-to-right word") {
    auto group = MakeCell(kLatin);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("Right advances through the text and Left goes back") {
      REQUIRE(PressArrow(editor, WXK_RIGHT, 3) == 4);
      REQUIRE(PressArrow(editor, WXK_LEFT, 3) == 2);
    }
  }

  GIVEN("a right-to-left word") {
    auto group = MakeCell(kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("Left advances through the text, because that is rightwards on screen") {
      // The text is drawn in the reverse of its stored order, so moving the
      // caret leftwards on screen means stepping forward through the content.
      REQUIRE(PressArrow(editor, WXK_LEFT, 3) == 4);
      REQUIRE(PressArrow(editor, WXK_RIGHT, 3) == 2);
    }

    THEN("the caret keeps moving the same way on screen as the key repeats") {
      // The real complaint a user would make: pressing one arrow twice must not
      // move the caret back where it started.
      const size_t once = PressArrow(editor, WXK_LEFT, 2);
      const size_t twice = PressArrow(editor, WXK_LEFT, once);
      REQUIRE(twice != 2);
      REQUIRE(editor->PositionToPoint(twice).x < editor->PositionToPoint(2).x);
    }
  }
}

SCENARIO("A selection covers the characters it selects") {
  // The rectangle MarkSelection() draws for a run: its left edge, and its width.
  // Selecting positions [2,5) must cover exactly the glyphs of those three
  // characters - the same span of screen in either script, just reached from
  // the other end.
  const size_t from = 2, to = 5;

  GIVEN("a left-to-right word") {
    auto group = MakeCell(kLatin);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("the rectangle starts where the first selected character is") {
      const wxCoord startX = editor->PositionToPoint(from).x;
      const wxCoord endX = editor->PositionToPoint(to).x;
      const wxCoord width = endX - startX;
      REQUIRE(width > 0);
      REQUIRE(editor->SelectionRunLeft(from, to, startX, width) == startX);
    }
  }

  GIVEN("a right-to-left word") {
    auto group = MakeCell(kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("the rectangle starts at the *last* selected character instead") {
      const wxCoord startX = editor->PositionToPoint(from).x;
      const wxCoord endX = editor->PositionToPoint(to).x;
      // Reading right to left, a later position is further left.
      REQUIRE(endX < startX);
      const wxCoord width = startX - endX;

      const wxCoord left = editor->SelectionRunLeft(from, to, startX, width);
      // Taking the run's start as the left edge would have highlighted the text
      // *after* the selection; the rectangle has to sit over the glyphs.
      REQUIRE(left == endX);
      REQUIRE(left + width == startX);
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
