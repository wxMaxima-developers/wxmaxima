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
  Keeps the per-cell memory footprint from creeping back up (GH #804).

  A large result (a polynomial with a million terms) becomes several million
  cells, so every byte in Cell counts several megabytes. A cell's UUID and
  its unknown XML attributes used to be a wxString and a std::map in every
  cell (96 bytes on 64-bit Linux), although only GroupCells normally get a
  UUID; they now live in side data that is only allocated when used. This
  test checks both halves: that an ordinary maths cell allocates none of
  it, and that the data still survives the trip through MathParser and
  ToXML() for the cells that do carry it.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/Cell.h"
#include "cells/TextCell.h"

#include <cstdio>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
} // namespace

SCENARIO("Cells stay small (GH #804)") {
  // Printed so that a CI log shows how the sizes develop over time.
  std::printf("sizeof(Cell)=%zu sizeof(TextCell)=%zu sizeof(wxString)=%zu\n",
              sizeof(Cell), sizeof(TextCell), sizeof(wxString));
  THEN("Cell doesn't hold rarely used strings or maps inline") {
    // 136 bytes on 64-bit Linux with wxWidgets 3.2 when this was written,
    // down from 224. The bound leaves room for platform differences (e.g.
    // the accessibility pointer on MSW) but not for another wxString plus a
    // container. Only checked on 64-bit platforms, where it was measured.
    if constexpr (sizeof(void *) == 8)
      CHECK(sizeof(Cell) <= 160);
  }
}

SCENARIO("Ordinary maths cells allocate no side data") {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(
      wxS("<mth><mrow><n>2</n><mo>*</mo><v>a</v></mrow></mth>"));
  REQUIRE(output != nullptr);
  for (const Cell &cell : OnList(output.get()))
    CHECK_FALSE(cell.HasRareCellData());
}

SCENARIO("A UUID and unknown attributes survive parsing and ToXML()") {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(
      wxS("<mth><v uuid=\"0123-abcd\" futureAttribute=\"x&lt;y\">a</v></mth>"));
  REQUIRE(output != nullptr);
  Cell *cell = output.get();
  REQUIRE(cell->HasRareCellData());
  CHECK(cell->GetUUID() == wxS("0123-abcd"));
  const wxString xml = cell->ToXML();
  CHECK(xml.Contains(wxS("uuid=\"0123-abcd\"")));
  CHECK(xml.Contains(wxS("futureAttribute=\"x&lt;y\"")));
}

SCENARIO("Clearing the UUID of a cell that has none doesn't allocate") {
  TextCell cell(nullptr, g_cfg, wxS("x"));
  cell.SetUUID(wxString());
  CHECK_FALSE(cell.HasRareCellData());
  CHECK(cell.GetUUID().IsEmpty());
  cell.GenerateUUID();
  CHECK(cell.HasRareCellData());
  CHECK_FALSE(cell.GetUUID().IsEmpty());
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

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
