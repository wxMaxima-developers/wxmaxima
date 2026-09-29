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

// What Worksheet::ExportToMAC() writes for a one-cell tree.
wxString ExportedMAC(GroupType type, const wxString &text) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, text);
  return Format::TreeToWXM(group.get(), /*wxm=*/false);
}

// Parses .mac text and returns its only cell's text, requiring its type.
wxString OnlyCellText(const wxString &macContents, GroupType type) {
  auto reloaded = Format::ParseMACContents(macContents, g_cfg);
  REQUIRE(reloaded != nullptr);
  CHECK(reloaded->GetNext() == nullptr);
  REQUIRE(reloaded->GetGroupType() == type);
  REQUIRE(reloaded->GetEditable() != nullptr);
  return reloaded->GetEditable()->GetValue();
}

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

SCENARIO("A hand-written .mac comes back as it was read (GH #2353)") {
  GIVEN("comments that nest, or contain \"&\" and entities") {
    // Maxima reads each of these lines as one comment.
    const wxString comments[] = {
      wxS("a /* nested */ comment"),
      wxS("/*/ opens and */ closes"),
      wxS("Q&A, &amp; and &#47; stay as they are"),
      wxS("1/2 * 3/4"),
    };
    for (const auto &comment : comments) {
      const wxString mac = wxS("/* ") + comment + wxS(" */\n");
      THEN(("\"" + comment + "\" is one text cell").ToStdString()) {
        CHECK(OnlyCellText(mac, GC_TYPE_TEXT) == comment);
      }
      THEN(("\"" + comment + "\" is written back unchanged").ToStdString()) {
        CHECK(ExportedMAC(GC_TYPE_TEXT, comment) == mac);
      }
    }
  }
  GIVEN("a nested comment followed by code") {
    auto tree = Format::ParseMACContents(
      wxS("/* a /* b */ c */\nx:1$\n"), g_cfg);
    THEN("the code after it is still code") {
      REQUIRE(tree != nullptr);
      CHECK(tree->GetGroupType() == GC_TYPE_TEXT);
      REQUIRE(tree->GetNext() != nullptr);
      CHECK(tree->GetNext()->GetGroupType() == GC_TYPE_CODE);
      CHECK(tree->GetNext()->GetEditable()->GetValue() == wxS("x:1$"));
    }
  }
}

SCENARIO("A text cell that isn't one Maxima comment stays inert in a .mac (GH #1907)") {
  // Each would end its comment early, or leave it open and swallow the
  // code after it, if it were written as it is.
  const wxString texts[] = {
    wxS("see a*/b x:1$"),
    wxS("files in src/*/lib"),
    wxS("*/ x:2$ /* and more"),
  };
  for (const auto &text : texts) {
    const wxString mac = ExportedMAC(GC_TYPE_TEXT, text) + wxS("y:3$\n");
    THEN(("\"" + text + "\" is one text cell followed by the code").ToStdString()) {
      auto tree = Format::ParseMACContents(mac, g_cfg);
      REQUIRE(tree != nullptr);
      CHECK(tree->GetGroupType() == GC_TYPE_TEXT);
      REQUIRE(tree->GetNext() != nullptr);
      CHECK(tree->GetNext()->GetEditable()->GetValue() == wxS("y:3$"));
      // Only the slashes next to a star changed.
      wxString expected = text;
      expected.Replace(wxS("*/"), wxS("*&#47;"));
      expected.Replace(wxS("/*"), wxS("&#47;*"));
      CHECK(tree->GetEditable()->GetValue() == expected);
    }
  }
}

SCENARIO("A heading can't end its .mac comment early (GH #2353)") {
  // A heading's start marker opens a comment that only its end marker
  // closes. Unescaped, the "*/" below would end it and make "x:2$" code.
  const wxString title = wxS("abc */ x:2$ /* def & more");
  const wxString mac = ExportedMAC(GC_TYPE_TITLE, title);
  THEN("the title's own slashes next to a star are escaped") {
    CHECK_FALSE(mac.Contains(wxS("abc */")));
    CHECK_FALSE(mac.Contains(wxS("/* def")));
  }
  THEN("and the title comes back as it was") {
    CHECK(OnlyCellText(mac, GC_TYPE_TITLE) == title);
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
