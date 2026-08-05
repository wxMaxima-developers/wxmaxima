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
  Tests for Configuration::GetAsciiArtColumns(), which tells Maxima's own
  ASCII-art 2D/1D printer ($linel, see MaximaEvaluator::LinelConfigCommand())
  how many columns of the worksheet's monospace ASCII-math font (TS_ASCIIMATHS)
  currently fit into the window (#1608).

  Windowless: a memory-DC Configuration, no Worksheet, no wxFrame - the
  test_EditorCellWrapping pattern. Assertions stay relative (narrower canvas
  -> fewer or equal columns) and bounds-based rather than pinning an exact
  column count, since the monospace font's rendered glyph width can differ
  a little across platforms/CI.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "Configuration.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
} // namespace

SCENARIO("GetAsciiArtColumns() reflects the worksheet's width") {
  const wxSize savedCanvas = g_cfg->GetCanvasSize();

  GIVEN("a wide canvas") {
    g_cfg->SetCanvasSize(wxSize(4000, 800));
    const long wideColumns = g_cfg->GetAsciiArtColumns();

    THEN("the column count is well above the clamp floor") {
      REQUIRE(wideColumns > 20);
    }

    WHEN("the canvas is narrowed a lot") {
      g_cfg->SetCanvasSize(wxSize(200, 800));
      const long narrowColumns = g_cfg->GetAsciiArtColumns();

      THEN("fewer columns fit than on the wide canvas") {
        REQUIRE(narrowColumns <= wideColumns);
        REQUIRE(narrowColumns >= 20); // the clamp floor: never near-zero.
      }
    }
  }

  GIVEN("an absurdly narrow canvas") {
    g_cfg->SetCanvasSize(wxSize(1, 800));

    THEN("the result is still clamped to a sane, positive minimum") {
      const long columns = g_cfg->GetAsciiArtColumns();
      REQUIRE(columns >= 20);
      REQUIRE(columns <= 2000);
    }
  }

  g_cfg->SetCanvasSize(savedCanvas);
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

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
