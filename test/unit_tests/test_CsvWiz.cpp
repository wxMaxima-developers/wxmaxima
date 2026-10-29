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
  Regression test: the CSV import and export wizards remember the field
  separator the user chose last time.

  They called wxConfig::Read(key, choice) with a plain long, which is the
  overload that returns the value it read instead of storing it -- so the
  result was thrown away and every wizard came up with "Semicolon", whatever
  had been saved. Coverity found it as dead code (CID 507037): the range
  checks after the read could never fire.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/config.h>
#include <wx/dcmemory.h>
#include <wx/fileconf.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "wizards/CsvWiz.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
wxFrame *g_frame = nullptr;
} // namespace

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

SCENARIO("The CSV wizards start with the separator that was saved") {
  GIVEN("a saved separator of \"Tab\"") {
    wxConfig::Get()->Write(wxS("csvSeparator"), 2L);

    THEN("the import wizard offers a tab") {
      auto *wiz = new CsvImportWiz(g_frame, g_cfg);
      REQUIRE(wiz->GetSeparator() == wxS("'tab"));
      wiz->Destroy();
    }
    THEN("the export wizard offers a tab") {
      auto *wiz = new CsvExportWiz(g_frame, g_cfg, wxS("Matrix"));
      REQUIRE(wiz->GetSeparator() == wxS("'tab"));
      wiz->Destroy();
    }
  }
  GIVEN("a saved separator of \"Comma\"") {
    wxConfig::Get()->Write(wxS("csvSeparator"), 1L);

    THEN("the import wizard offers a comma") {
      auto *wiz = new CsvImportWiz(g_frame, g_cfg);
      REQUIRE(wiz->GetSeparator() == wxS("'comma"));
      wiz->Destroy();
    }
  }
  GIVEN("a saved value that is out of range") {
    wxConfig::Get()->Write(wxS("csvSeparator"), 17L);

    THEN("the last entry is chosen rather than none") {
      auto *wiz = new CsvImportWiz(g_frame, g_cfg);
      REQUIRE(wiz->GetSeparator() == wxS("'tab"));
      wiz->Destroy();
    }
  }
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  // In memory only (no file names, style 0): the test must neither read nor
  // change the user's settings.
  delete wxConfig::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                        wxEmptyString, wxEmptyString, 0));

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
