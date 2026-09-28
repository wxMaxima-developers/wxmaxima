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
  Closing the diff viewer must leave the config file alone (GH #2356).

  Each pane of the diff viewer has its own copy of the main window's
  Configuration. A Configuration that isn't temporary writes all of its
  settings to the config file when it is destroyed, so closing the viewer
  used to overwrite anything changed in the main window while it was open
  with what the config file said when the viewer was opened.

  The config file is an in-memory wxFileConfig, so the test neither reads
  nor changes anything on disk.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/ffile.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/log.h>

#include "Configuration.h"
#include "dialogs/DiffFrame.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;

//! Writes a small .wxm file with one code cell and returns its name
wxString WriteWxm(const wxString &code) {
  const wxString name = wxFileName::CreateTempFileName(wxS("diffcfg")) + wxS(".wxm");
  wxFFile file(name, wxS("w"));
  file.Write(wxS("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/\n"
                 "/* [wxMaxima: input   start ] */\n") +
             code +
             wxS("\n/* [wxMaxima: input   end   ] */\n"
                 "/* Old versions of Maxima abort on loading files that end in a comment. */\n"
                 "\"Created with wxMaxima\"$\n"));
  return name;
}
} // namespace

SCENARIO("Closing the diff viewer leaves settings changed meanwhile alone") {
  wxArrayString files;
  files.Add(WriteWxm(wxS("a: 1$")));
  files.Add(WriteWxm(wxS("a: 2$")));

  GIVEN("a diff viewer opened while the config file says one thing") {
    wxConfig::Get()->Write(wxS("showLabelChoice"),
                           static_cast<long>(Configuration::labels_automatic));
    auto *diff = new DiffFrame(nullptr, files, g_cfg);

    WHEN("the main window saves a different setting, then the viewer is closed") {
      // What the main window's Configuration writes when a setting is
      // changed in the options dialog.
      wxConfig::Get()->Write(wxS("showLabelChoice"),
                             static_cast<long>(Configuration::labels_useronly));
      delete diff;

      THEN("the config file still holds the main window's setting") {
        long labels = -1;
        wxConfig::Get()->Read(wxS("showLabelChoice"), &labels);
        CHECK(labels == static_cast<long>(Configuration::labels_useronly));
      }
    }
  }

  for (const auto &file : files)
    wxRemoveFile(file);
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

  // No local or global file: nothing this test writes reaches the disk, and
  // no config file left over from an earlier run can change what it sees.
  delete wxConfig::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                        wxEmptyString, wxEmptyString, 0));

  wxBitmap bmp(400, 400);
  wxMemoryDC dc;
  dc.SelectObject(bmp);
  // Not temporary, like the main window's: the diff viewer's copies inherit
  // that, which is the bug this tests. It writes only to the in-memory
  // config set above.
  g_cfg = new Configuration(&dc);

  const int result = Catch::Session().run(argc, argv);

  delete g_cfg;
  wxEntryCleanup();
  return result;
}
