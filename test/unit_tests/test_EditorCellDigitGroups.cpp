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
  Tests the gaps between digit groups of numbers in code cells (GH #192).

  The gaps are drawn, never stored: m_text keeps the number exactly as typed,
  and every place that turns text into pixels -- Recalculate(), Draw(), the
  caret (GetLineWidth()/PositionToPoint()), a click (SelectPointText()) and
  the selection rectangle -- has to agree on where they are. Most checks here
  compare a cell with grouping on against the same cell with it off, which
  doesn't depend on which font the test machine has.

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

//! Builds a code cell holding text, laid out with digit grouping on or off
std::unique_ptr<GroupCell> MakeCodeCell(const wxString &text, bool grouping) {
  g_cfg->DigitGrouping(grouping);
  g_cfg->DigitGroupingMinDigits(5);
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, text);
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  return group;
}

//! How far the caret moves going from position pos to pos + 1
wxCoord Step(EditorCell *editor, size_t pos) {
  return editor->GetLineWidth(0, pos + 1) - editor->GetLineWidth(0, pos);
}
} // namespace

SCENARIO("A long number in a code cell gets gaps, its text doesn't") {
  const wxString code = wxS("x:2111230496;");
  auto grouped = MakeCodeCell(code, true);
  auto plain = MakeCodeCell(code, false);
  EditorCell *groupedEditor = grouped->GetEditable();
  EditorCell *plainEditor = plain->GetEditable();
  REQUIRE(groupedEditor != nullptr);
  REQUIRE(plainEditor != nullptr);

  THEN("the text is what was typed") {
    REQUIRE(groupedEditor->GetValue() == code);
  }
  THEN("the line is wider by three gaps") {
    const wxCoord extra = groupedEditor->GetLineWidth(0, code.Length()) -
      plainEditor->GetLineWidth(0, code.Length());
    REQUIRE(extra > 0);
    REQUIRE(extra % 3 == 0);
  }
  THEN("the caret steps over a gap only where 2 111 230 496 has one") {
    // The digits start at position 2; the gaps go before the digits at
    // positions 3, 6 and 9.
    for (size_t pos = 0; pos < code.Length(); pos++) {
      INFO("pos=" << pos);
      const bool gapAfter = (pos == 3) || (pos == 6) || (pos == 9);
      const wxCoord difference = Step(groupedEditor, pos) - Step(plainEditor, pos);
      if (gapAfter)
        REQUIRE(difference > 0);
      else
        REQUIRE(difference == 0);
    }
  }
  THEN("a click resolves to the position whose caret it is drawn at") {
    // Up to and including the end of the line, which used to resolve to the
    // position before the last character, with or without grouping.
    for (EditorCell *editor : {groupedEditor, plainEditor})
      for (size_t target = 0; target <= code.Length(); target++) {
        INFO("grouped=" << (editor == groupedEditor) << " target=" << target);
        editor->SelectPointText(editor->PositionToPoint(target));
        REQUIRE(editor->CursorPosition() == target);
      }
  }
  THEN("a click on a character's right half puts the caret after it") {
    // 1 px left of a caret position is still right of the midpoint of the
    // character in front of it.
    for (EditorCell *editor : {groupedEditor, plainEditor})
      for (size_t target = 1; target <= code.Length(); target++) {
        INFO("grouped=" << (editor == groupedEditor) << " target=" << target);
        editor->SelectPointText(editor->PositionToPoint(target) - wxPoint(1, 0));
        REQUIRE(editor->CursorPosition() == target);
      }
  }
  THEN("a click into a gap lands right in front of the group after it") {
    // The caret of position 3 is drawn at the left edge of the gap in front
    // of the digit at position 3: the gap belongs to the digits after it.
    const wxPoint gapStart = groupedEditor->PositionToPoint(3);
    groupedEditor->SelectPointText(gapStart + wxPoint(1, 0));
    REQUIRE(groupedEditor->CursorPosition() == 3);
  }
  THEN("a selection starts and ends where the caret does") {
    for (size_t from = 0; from < code.Length(); from++)
      for (size_t to = from + 1; to <= code.Length(); to++) {
        INFO("selection [" << from << ", " << to << ")");
        wxCoord width = 0;
        const wxCoord left = groupedEditor->SelectionLineSpan(from, to, &width).x;
        REQUIRE(left == groupedEditor->PositionToPoint(from).x);
        REQUIRE(left + width == groupedEditor->PositionToPoint(to).x);
      }
  }
}

SCENARIO("The digits after a decimal point are grouped from the point on") {
  // The tokenizer splits "3.14159265" into "3", "." and "14159265"; grouped
  // on its own, the last of them would be grouped from the right.
  const wxString code = wxS("x:3.14159265;");
  auto grouped = MakeCodeCell(code, true);
  auto plain = MakeCodeCell(code, false);
  for (size_t pos = 0; pos < code.Length(); pos++) {
    INFO("pos=" << pos);
    // 3.141 592 65: gaps in front of the digits at positions 7 and 10
    const bool gapAfter = (pos == 7) || (pos == 10);
    const wxCoord difference =
      Step(grouped->GetEditable(), pos) - Step(plain->GetEditable(), pos);
    if (gapAfter)
      REQUIRE(difference > 0);
    else
      REQUIRE(difference == 0);
  }
}

SCENARIO("Short numbers and names containing digits stay as they are") {
  for (const auto &code : {wxString(wxS("x:2026;")), wxString(wxS("x12345678:1;")),
                           wxString(wxS("\"1234567\";"))}) {
    INFO(code.utf8_str().data());
    auto grouped = MakeCodeCell(code, true);
    auto plain = MakeCodeCell(code, false);
    REQUIRE(grouped->GetEditable()->GetLineWidth(0, code.Length()) ==
            plain->GetEditable()->GetLineWidth(0, code.Length()));
  }
}

SCENARIO("Switching grouping off removes the gaps") {
  const wxString code = wxS("x:2111230496;");
  auto plain = MakeCodeCell(code, false);
  auto cell = MakeCodeCell(code, true);
  g_cfg->DigitGrouping(false);
  cell->ResetSize_Recursively();
  cell->Recalculate();
  REQUIRE(cell->GetEditable()->GetLineWidth(0, code.Length()) ==
          plain->GetEditable()->GetLineWidth(0, code.Length()));
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
