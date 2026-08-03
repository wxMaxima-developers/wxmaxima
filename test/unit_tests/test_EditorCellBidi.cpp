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
  What the caret, click-to-position and arrow keys do inside bidirectional text.

  EditorCell maps a caret position (an index into the content) to a point by
  measuring how wide the text before it is - PositionToPoint(). That is only
  correct while the text is drawn in the same order it is stored. In a
  right-to-left script it is not: the first character of a Hebrew word is drawn
  at its *right* end, so a caret one character in belongs near the right of the
  word and not, as the measurement says, one character's width from its left.
  SelectPointText() (turning a click into a position) and HandleSpecialKey()'s
  arrow-key handling face the same problem from the other direction.

  These scenarios measure the mapping rather than assume it, for a left-to-right
  string as the control, a right-to-left one, and a line mixing the two.

  Windowless: a real GroupCell/EditorCell against a memory-DC Configuration, in
  the manner of test_EditorCellWrapping. The display comes from the run_headless
  wrapper ctest starts this through, so there is no Xvfb wrangling here.
*/

#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>

#include <vector>

#include "Bidi.h"
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

SCENARIO("The caret is placed correctly on a mixed-direction line") {
  // "abcdefgh" (Latin) followed by "שלוםעולם" (Hebrew) on one line: the line
  // as a whole mixes directions, but a range entirely inside the Hebrew tail
  // is a single right-to-left run - the case LineIsRightToLeft() can't
  // characterise by itself (see its own comment), because it only knows
  // whether the *line* is wholly one direction. This is what a selection or a
  // "text that coincides with the search term" marker inside a mixed
  // paragraph actually needs, and it is MixedDirectionOffset() (used by
  // PositionToPoint() and MarkSelection() alike) that supplies it.
  const wxString mixed = kLatin + kHebrew;

  GIVEN("a position inside the trailing right-to-left run") {
    auto group = MakeCell(mixed);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    REQUIRE(editor->LineIsMixedDirection(0));

    // One character into the Hebrew run, and the position right after it.
    const size_t from = kLatin.Length() + 1, to = kLatin.Length() + 2;
    wxCoord offsetFrom = 0, offsetTo = 0;
    const bool ok = editor->MixedDirectionOffset(0, from, &offsetFrom) &&
                    editor->MixedDirectionOffset(0, to, &offsetTo);

    if (Bidi::IsAvailable()) {
      THEN("it succeeds and mirrors the run, the same way a wholly "
           "right-to-left line does in SelectionRunLeft() above") {
        REQUIRE(ok);
        // Later position -> smaller offset: reading right to left, "from" is
        // closer to the run's right (later-drawn) end than "to" is.
        REQUIRE(offsetTo < offsetFrom);
      }

      THEN("PositionToPoint() agrees with it") {
        // PositionToPoint() adds the cell's own origin and any right-to-left
        // flush-right shift on top; for a cell with neither (this one), the
        // two must land on the very same pixel.
        REQUIRE(editor->PositionToPoint(from).x - editor->GetCurrentPoint().x == offsetFrom);
        REQUIRE(editor->PositionToPoint(to).x - editor->GetCurrentPoint().x == offsetTo);
      }
    } else {
      THEN("without libfribidi it declines rather than guess") {
        REQUIRE_FALSE(ok);
      }
    }
  }
}

namespace {

//! Clicks exactly where each position's caret is drawn, and checks that the
//! resolved position's caret lands on the very same pixel. Not necessarily
//! the *same* position: at a direction boundary, the position ending a
//! left-to-right run and the position ending the right-to-left run right
//! after it are drawn at the very same edge (one run's end is the next
//! run's start), so either is a correct answer to "which position is this
//! click on" - the pixel is what a click resolves by, so the pixel is what
//! has to round-trip.
void CheckClickRoundTrips(EditorCell *editor, const wxString &text) {
  for (size_t target = 0; target <= text.Length(); target++) {
    const wxCoord targetX = editor->PositionToPoint(target).x;
    editor->SelectPointText(editor->PositionToPoint(target));
    const wxCoord resolvedX = editor->PositionToPoint(editor->CursorPosition()).x;
    INFO("target=" << target << " resolved=" << editor->CursorPosition());
    REQUIRE(resolvedX == targetX);
  }
}

} // namespace

SCENARIO("A click resolves to the position whose caret it lands on") {
  GIVEN("a left-to-right word") {
    auto group = MakeCell(kLatin);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("every position round-trips") {
      CheckClickRoundTrips(editor, kLatin);
    }
  }

  GIVEN("a right-to-left word") {
    auto group = MakeCell(kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);

    THEN("every position round-trips too, not its mirror image") {
      // This is the bug a plain left-to-right forward scan has: it resolves
      // a click on the *first* letter's caret to the *last* position, and
      // the other way round, because it measures growing substring width
      // without ever asking which direction the text reads.
      CheckClickRoundTrips(editor, kHebrew);
    }
  }

  GIVEN("a mixed-direction line") {
    auto group = MakeCell(kLatin + kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    REQUIRE(editor->LineIsMixedDirection(0));

    if (Bidi::IsAvailable()) {
      THEN("every position round-trips, across the direction boundary too") {
        CheckClickRoundTrips(editor, kLatin + kHebrew);
      }
    }
  }
}

SCENARIO("The arrow keys move the caret the way it is drawn on a mixed-direction line") {
  GIVEN("a caret in the right-to-left run of a mixed-direction line") {
    auto group = MakeCell(kLatin + kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    REQUIRE(editor->LineIsMixedDirection(0));
    // One character into the Hebrew run.
    const size_t pos = kLatin.Length() + 1;

    if (Bidi::IsAvailable()) {
      THEN("Left steps forward through the run, because that is rightwards "
           "on screen - LineIsRightToLeft() alone would say this whole line "
           "isn't right-to-left and leave the caret going the wrong way") {
        REQUIRE(PressArrow(editor, WXK_LEFT, pos) == pos + 1);
        REQUIRE(PressArrow(editor, WXK_RIGHT, pos) == pos - 1);
      }
    }
  }

  GIVEN("a caret in the left-to-right run of a mixed-direction line") {
    auto group = MakeCell(kLatin + kHebrew);
    EditorCell *editor = group->GetEditable();
    REQUIRE(editor != nullptr);
    REQUIRE(editor->LineIsMixedDirection(0));
    const size_t pos = 3; // inside the Latin run

    THEN("Right still steps forward, same as a wholly left-to-right line") {
      REQUIRE(PressArrow(editor, WXK_RIGHT, pos) == pos + 1);
      REQUIRE(PressArrow(editor, WXK_LEFT, pos) == pos - 1);
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
