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
  The directory-scanning half of the bash-like file-name completion: file
  arguments of openr()/read_matrix()/load()/demo()/... complete against the
  directory the partial file name currently points into, descending level by
  level as the partial acquires directory components (the popup calls
  AutoComplete::UpdateFiles() on every boundary change).

  Runs against a temporary directory tree, no Maxima and no GUI needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/dir.h>
#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/log.h>

#include <algorithm>
#include <vector>

#include "Autocomplete.h"
#include "Configuration.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
wxString g_baseDir;
} // namespace

static void EnsureDisplay() {
#ifndef _WIN32
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
}

// Runs before Catch2's session, so failures abort instead of REQUIREing.
static void Touch(const wxString &path) {
  wxFFile file(path, wxS("w"));
  if (!file.IsOpened()) {
    fprintf(stderr, "cannot create test file %s\n", (const char *)path.utf8_str());
    exit(1);
  }
  file.Write(wxS("x"));
}

/*! Builds the directory tree the scenarios complete against:

  base/alpha.csv
  base/foo.mac
  base/data/inner.csv
  base/data/deep/leaf.txt
  base/empty/
*/
static wxString MakeTestTree() {
  wxString base = wxFileName::CreateTempFileName(wxS("wxm_completion_test"));
  if (base.IsEmpty() || !wxRemoveFile(base) || !wxFileName::Mkdir(base) ||
      !wxFileName::Mkdir(base + wxS("/data")) ||
      !wxFileName::Mkdir(base + wxS("/data/deep")) ||
      !wxFileName::Mkdir(base + wxS("/empty"))) {
    fprintf(stderr, "cannot create the test directory tree\n");
    exit(1);
  }
  Touch(base + wxS("/alpha.csv"));
  Touch(base + wxS("/foo.mac"));
  Touch(base + wxS("/data/inner.csv"));
  Touch(base + wxS("/data/deep/leaf.txt"));
  return base;
}

static bool Contains(const std::vector<wxString> &list, const wxString &item) {
  return std::find(list.begin(), list.end(), item) != list.end();
}

SCENARIO("General file completion lists the directory the partial points into") {
  AutoComplete completer(g_cfg);

  GIVEN("a partial that is just the opening quote") {
    completer.UpdateFiles(AutoComplete::generalfile, wxS("\""), g_baseDir);
    auto completions =
        completer.CompleteSymbol(wxS("\""), AutoComplete::generalfile);

    THEN("the base directory's files and subdirectories are offered "
         "without a spurious path prefix") {
      CAPTURE(g_baseDir);
      for (const auto &c : completions)
        UNSCOPED_INFO(c.utf8_str());
      CHECK(Contains(completions, wxS("\"alpha.csv\"")));
      CHECK(Contains(completions, wxS("\"foo.mac\"")));
      CHECK(Contains(completions, wxS("\"data/\"")));
      CHECK(Contains(completions, wxS("\"empty/\"")));
      CHECK(!Contains(completions, wxS("\"/alpha.csv\"")));
    }
  }

  GIVEN("a partial that descends into a subdirectory") {
    completer.UpdateFiles(AutoComplete::generalfile, wxS("\"data/"), g_baseDir);
    auto completions =
        completer.CompleteSymbol(wxS("\"data/"), AutoComplete::generalfile);

    THEN("that directory's contents are offered, with the directory prefix") {
      CHECK(Contains(completions, wxS("\"data/inner.csv\"")));
      CHECK(Contains(completions, wxS("\"data/deep/\"")));
      AND_THEN("the parent directory's files are not") {
        CHECK(!Contains(completions, wxS("\"alpha.csv\"")));
      }
    }

    AND_WHEN("the partial descends one level deeper") {
      completer.UpdateFiles(AutoComplete::generalfile, wxS("\"data/deep/"),
                            g_baseDir);
      auto deeper = completer.CompleteSymbol(wxS("\"data/deep/"),
                                             AutoComplete::generalfile);
      THEN("the deep directory's file is offered") {
        CHECK(Contains(deeper, wxS("\"data/deep/leaf.txt\"")));
      }
    }

    AND_WHEN("the partial returns to the top level") {
      completer.UpdateFiles(AutoComplete::generalfile, wxS("\""), g_baseDir);
      auto top = completer.CompleteSymbol(wxS("\""), AutoComplete::generalfile);
      THEN("the subdirectory's contents no longer clutter the listing") {
        CHECK(Contains(top, wxS("\"alpha.csv\"")));
        CHECK(!Contains(top, wxS("\"data/inner.csv\"")));
      }
    }
  }

  GIVEN("a partial naming part of a file name") {
    completer.UpdateFiles(AutoComplete::generalfile, wxS("\"alp"), g_baseDir);
    auto completions =
        completer.CompleteSymbol(wxS("\"alp"), AutoComplete::generalfile);
    THEN("only matching entries survive the filter") {
      CHECK(Contains(completions, wxS("\"alpha.csv\"")));
      CHECK(!Contains(completions, wxS("\"foo.mac\"")));
    }
  }
}

SCENARIO("Load-file completion only offers loadable files") {
  AutoComplete completer(g_cfg);
  completer.UpdateFiles(AutoComplete::loadfile, wxS("\""), g_baseDir);
  auto completions =
      completer.CompleteSymbol(wxS("\""), AutoComplete::loadfile);

  THEN("packages are offered by name, other files not at all") {
    // Loadable files are offered without their extension.
    CHECK(Contains(completions, wxS("\"foo\"")));
    CHECK(!Contains(completions, wxS("\"alpha.csv\"")));
    AND_THEN("subdirectories can be descended into") {
      CHECK(Contains(completions, wxS("\"data/\"")));
    }
  }
}

SCENARIO("Only the file-name completion types complete files") {
  CHECK(AutoComplete::CompletesFiles(AutoComplete::generalfile));
  CHECK(AutoComplete::CompletesFiles(AutoComplete::loadfile));
  CHECK(AutoComplete::CompletesFiles(AutoComplete::demofile));
  CHECK(!AutoComplete::CompletesFiles(AutoComplete::command));
  CHECK(!AutoComplete::CompletesFiles(AutoComplete::unit));
  CHECK(!AutoComplete::CompletesFiles(AutoComplete::esccommand));
  CHECK(!AutoComplete::CompletesFiles(AutoComplete::tmplte));
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_baseDir = MakeTestTree();

  const int result = Catch::Session().run(argc, argv);

  wxFileName::Rmdir(g_baseDir, wxPATH_RMDIR_RECURSIVE);
  wxEntryCleanup();
  return result;
}
