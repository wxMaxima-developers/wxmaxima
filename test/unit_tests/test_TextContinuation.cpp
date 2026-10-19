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
  Tests for TextCell::SetTextContinuation(): a long line of plain text Maxima
  sends (a warning, an error, a status message) is split into one cell per
  piece wxm::SplitAtTextBreaks() returns, so the worksheet can break it into
  lines between them. As long as it fits, it has to look, copy, save and export
  exactly as the single cell it used to be.

  The cells are built the way MaximaOutputAppender::DoRawConsoleAppend() builds
  them, and put into a real GroupCell -- no live Maxima needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "MathParser.h"
#include "TextBreaks.h"
#include "cells/CellIterators.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/LabelCell.h"
#include "cells/TextCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

const wxString message =
  wxS("Warning: this is a long message, whose words wxMaxima may break into "
      "lines after every space, comma, +, -, *, / and dot.");

//! A code cell whose output is lines, each split as DoRawConsoleAppend() does.
std::unique_ptr<GroupCell> MakeTextOutput(const std::vector<wxString> &lines,
                                          CellType type, bool split = true) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("x;"));
  // A group's first output cell is its label slot, which GetOutput() skips.
  group->AppendOutput(std::make_unique<LabelCell>(group.get(), g_cfg, wxEmptyString));
  CellListBuilder<Cell> tree;
  for (const auto &line : lines) {
    const auto pieces =
      split ? wxm::SplitAtTextBreaks(line) : std::vector<wxString>{line};
    for (size_t i = 0; i < pieces.size(); i++) {
      auto cell = std::make_unique<TextCell>(group.get(), g_cfg, pieces[i]);
      cell->SetType(type);
      if (i > 0)
        cell->SetTextContinuation(true);
      const bool newLine = (i == 0);
      tree.Append(std::move(cell));
      if (newLine)
        tree.GetLastAppended()->ForceBreakLine(true);
    }
  }
  group->AppendOutput(std::move(tree));
  group->Recalculate();
  return group;
}

//! The summed width of the cells of the output's first line.
wxCoord FirstLineWidth(const GroupCell &group) {
  wxCoord width = 0;
  bool first = true;
  for (const Cell &cell : OnList(group.GetOutput())) {
    if (!first && cell.BreakLineHere())
      break;
    first = false;
    width += cell.GetWidth();
  }
  return width;
}

//! How many lines the output is drawn in.
int LineCount(const GroupCell &group) {
  int lines = 0;
  for (const Cell &cell : OnList(group.GetOutput()))
    if (cell.BreakLineHere())
      lines++;
  return lines;
}
} // namespace

SCENARIO("A split line of text is as wide as the unsplit one") {
  const CellType type = GENERATE(MC_TYPE_WARNING, MC_TYPE_ERROR, MC_TYPE_TEXT,
                                 MC_TYPE_ASCIIMATHS);
  g_cfg->SetCanvasSize(wxSize(100000, 800));
  auto split = MakeTextOutput({message}, type);
  auto whole = MakeTextOutput({message}, type, false);
  INFO("type " << static_cast<int>(type));
  REQUIRE(split->GetOutput()->GetNext() != nullptr);
  THEN("it is drawn in one line") { REQUIRE(LineCount(*split) == 1); }
  THEN("its pieces have no gaps between them") {
    // Text measured in pieces may kern a little differently than in one go.
    REQUIRE(std::abs(FirstLineWidth(*split) - FirstLineWidth(*whole)) <= 3);
  }
}

SCENARIO("A split line of text is broken into lines in a narrow window") {
  const CellType type = GENERATE(MC_TYPE_WARNING, MC_TYPE_ASCIIMATHS);
  g_cfg->SetCanvasSize(wxSize(250, 800));
  auto split = MakeTextOutput({message, wxS("Second line.")}, type);
  auto whole = MakeTextOutput({message, wxS("Second line.")}, type, false);
  INFO("type " << static_cast<int>(type));
  THEN("the unsplit line can't be broken") { REQUIRE(LineCount(*whole) == 2); }
  THEN("the split one is") { REQUIRE(LineCount(*split) > 3); }
  THEN("no piece is wider than the window") {
    for (const Cell &cell : OnList(split->GetOutput()))
      REQUIRE(cell.GetWidth() < 250);
  }
  THEN("only the lines Maxima sent start with a hard line break") {
    int hardBreaks = 0;
    for (const Cell &cell : OnList(split->GetOutput()))
      if (cell.HasHardLineBreak())
        hardBreaks++;
    REQUIRE(hardBreaks == 2);
  }
  THEN("copying the text gives Maxima's lines, not the wrapped ones") {
    REQUIRE(split->GetOutput()->ListToString() ==
            message + wxS("\nSecond line."));
  }
  THEN("the TeX export doesn't break the line either") {
    wxString tex = split->ToTeX();
    INFO(tex);
    if (type == MC_TYPE_ASCIIMATHS)
      REQUIRE(tex.Contains(message + wxS("\nSecond line.")));
    else {
      // Each piece is a \texttt{} of its own, whose "%" comment swallows the
      // line break that starts its text: together they are one line.
      tex.Replace(wxS("}\\texttt{%error\n"), wxEmptyString);
      REQUIRE(tex.Contains(wxS("Warning: this is a long message, whose words")));
    }
  }
  g_cfg->SetCanvasSize(wxSize(100000, 800));
}

SCENARIO("A split line of text survives saving and loading") {
  const CellType type = GENERATE(MC_TYPE_WARNING, MC_TYPE_ASCIIMATHS);
  auto split = MakeTextOutput({message}, type);
  wxString xml;
  for (const Cell &cell : OnList(split->GetOutput()))
    xml += cell.ToXML();
  INFO(xml);
  REQUIRE(xml.Contains(wxS("continuation=\"true\"")));

  MathParser parser(g_cfg);
  auto loaded = parser.ParseLine(wxS("<mth>") + xml + wxS("</mth>"));
  REQUIRE(loaded != nullptr);
  int continuations = 0;
  for (const Cell &cell : OnList(loaded.get())) {
    if (cell.IsTextContinuation())
      continuations++;
    // ParseCommonAttrs() would keep an unknown attribute and write it twice.
    REQUIRE(!cell.ToXML().Contains(wxS("continuation=\"true\" continuation")));
  }
  REQUIRE(continuations == static_cast<int>(wxm::SplitAtTextBreaks(message).size()) - 1);
  REQUIRE(loaded->ListToString() == message);
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
  // A GroupCell's editor needs somewhere to keep the cursor and selection.
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
