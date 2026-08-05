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
  Regression test for a plain .mac file surviving wxMaxima's open/save
  round-trip byte-for-byte, exercising the same code Worksheet::ExportToMAC()
  and MaximaFileIO::OpenMACFile() use (Format::TreeToWXM() with wxm=false and
  Format::ParseMACContents()).

  Before EditorCell kept '\t' as a real character, every tab in a loaded .mac
  file was silently rewritten to 1-4 spaces the moment it reached an
  EditorCell (EditorCell::TabExpand(), now gone) - so opening a hand-written
  or externally generated .mac file with tabs and immediately re-saving it
  produced a byte-different file. This pins that a tab used for interior
  alignment in a code cell survives unchanged.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "WXMformat.h"
#include "cells/CellList.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

// Serializes a one-cell tree to plain .mac text (wxm=false, the format
// ExportToMAC() writes) and parses it straight back, the way OpenMACFile()
// does. Returns the reloaded cell's own text.
wxString RoundTripThroughMAC(GroupType type, const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, text);
  const wxString macContents = Format::TreeToWXM(group.get(), /*wxm=*/false);

  auto reloaded = Format::ParseMACContents(macContents, g_cfg);
  REQUIRE(reloaded != nullptr);
  REQUIRE(reloaded->GetGroupType() == type);
  const EditorCell *editor = reloaded->GetEditable();
  REQUIRE(editor != nullptr);
  return editor->GetValue();
}
} // namespace

SCENARIO("A tab inside a code cell survives the .mac round-trip byte-for-byte") {
  GIVEN("a code cell whose statement uses a tab for interior alignment") {
    // The tab sits strictly between non-whitespace characters: ParseMACContents
    // trims leading/trailing whitespace off each reconstructed statement (as it
    // always has, independent of tab handling), so an interior tab is the case
    // that actually exercises tab preservation.
    const wxString original = wxS("a:1\t+\t2$");

    THEN("the reloaded cell's text is byte-identical") {
      REQUIRE(RoundTripThroughMAC(GC_TYPE_CODE, original) == original);
    }
  }

  GIVEN("a code cell with several consecutive tabs") {
    const wxString original = wxS("matrix([1,2],\t\t[3,4])$");

    THEN("all of them survive, not just the first") {
      REQUIRE(RoundTripThroughMAC(GC_TYPE_CODE, original) == original);
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
