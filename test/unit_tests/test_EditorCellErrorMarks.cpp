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
  Tests the marks a code cell shows where Maxima reported an error (GH #289):
  the statement that failed, and in it the place a syntax error was found at.

  The marks belong to the text they were set for. Pinned here: they are
  painted, only while the text is unchanged, and only where they were set.

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

#include <memory>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

//! Builds a laid-out code cell holding text
std::unique_ptr<GroupCell> MakeCodeCell(const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, text);
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  return group;
}

//! How many pixels of each line of the cell have exactly the given colour
struct PixelCount {
  int firstLine = 0;
  int secondLine = 0;
};

/*! Draws the editor onto a white bitmap and counts the pixels of a colour

  The cell's text has two lines; pixels above the middle between their caret
  positions count for the first one, the others for the second one.
*/
PixelCount CountColour(EditorCell *editor, size_t secondLineStart,
                       const wxColour &colour) {
  wxBitmap bitmap(600, 300);
  wxMemoryDC dc(bitmap);
  dc.SetBackground(*wxWHITE_BRUSH);
  dc.Clear();
  {
    NoClipToDrawRegion noClip(g_cfg);
    editor->Draw(&dc, &dc);
  }
  dc.SelectObject(wxNullBitmap);
  const wxImage image = bitmap.ConvertToImage();
  const int divider =
    (editor->PositionToPoint(0).y + editor->PositionToPoint(secondLineStart).y) / 2;

  PixelCount count;
  for (int y = 0; y < image.GetHeight(); y++)
    for (int x = 0; x < image.GetWidth(); x++)
      if ((image.GetRed(x, y) == colour.Red()) &&
          (image.GetGreen(x, y) == colour.Green()) &&
          (image.GetBlue(x, y) == colour.Blue())) {
        if (y < divider)
          count.firstLine++;
        else
          count.secondLine++;
      }
  return count;
}
} // namespace

SCENARIO("A failing statement and its syntax error are marked") {
  const wxString code = wxS("a: 1$\nb: 2 3;");
  const size_t secondLine = code.Find(wxS("b"));
  auto group = MakeCodeCell(code);
  EditorCell *editor = group->GetEditable();
  REQUIRE(editor != nullptr);
  const wxColour statementColour = g_cfg->GetColor(TS_ERROR_STATEMENT);
  const wxColour spotColour = g_cfg->GetColor(TS_ERROR_SPOT);
  REQUIRE(statementColour != spotColour);

  GIVEN("no marks") {
    THEN("nothing is painted in either colour") {
      REQUIRE_FALSE(editor->ErrorMarksValid());
      const auto statement = CountColour(editor, secondLine, statementColour);
      const auto spot = CountColour(editor, secondLine, spotColour);
      REQUIRE(statement.firstLine + statement.secondLine == 0);
      REQUIRE(spot.firstLine + spot.secondLine == 0);
    }
  }

  GIVEN("the second statement marked, and the 3 in it") {
    editor->AddErrorStatement(secondLine, code.Length());
    const size_t three = code.Find(wxS("3"));
    editor->AddErrorSpot(three, three + 1);
    REQUIRE(editor->ErrorMarksValid());

    THEN("the second line shows both colours, the first one neither") {
      const auto statement = CountColour(editor, secondLine, statementColour);
      const auto spot = CountColour(editor, secondLine, spotColour);
      REQUIRE(statement.firstLine == 0);
      REQUIRE(spot.firstLine == 0);
      REQUIRE(statement.secondLine > 0);
      REQUIRE(spot.secondLine > 0);
    }

    WHEN("the text is edited without changing its length") {
      // Replacing one character by another keeps the length, but may well
      // have fixed the error: the marks must not stay.
      editor->SetSelection(three, three + 1);
      editor->InsertText(wxS("4"));
      REQUIRE(editor->GetValue().Length() == code.Length());
      THEN("the marks are no longer valid, and not painted") {
        REQUIRE_FALSE(editor->ErrorMarksValid());
        const auto spot = CountColour(editor, secondLine, spotColour);
        REQUIRE(spot.firstLine + spot.secondLine == 0);
      }
    }

    WHEN("the cell's value is replaced") {
      editor->SetValue(code);
      THEN("the marks are gone") {
        REQUIRE(editor->GetErrorStatements().empty());
        REQUIRE(editor->GetErrorSpots().empty());
      }
    }

    WHEN("the marks are cleared, as a new evaluation does") {
      editor->ClearErrorMarks();
      THEN("nothing is marked any more") {
        REQUIRE_FALSE(editor->ErrorMarksValid());
        REQUIRE(editor->GetErrorStatements().empty());
      }
    }

    WHEN("another statement of the unchanged text fails, too") {
      editor->AddErrorStatement(0, secondLine);
      THEN("both statements are marked") {
        REQUIRE(editor->GetErrorStatements().size() == 2);
        const auto statement = CountColour(editor, secondLine, statementColour);
        REQUIRE(statement.firstLine > 0);
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
  // Wide enough that no line in here gets a soft break
  g_cfg->SetCanvasSize(wxSize(4000, 800));
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
