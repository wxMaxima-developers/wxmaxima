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
  Unit test for MaximaManual's help-anchor parsing.

  The manual anchors used to be exercised only indirectly, by the batch-mode
  integration tests. That coverage was removed once batch mode stopped compiling
  anchors (they are an interactive-only convenience and compiling them in a
  --batch/--exit-on-error run only wasted time -- and, on a read-only config dir,
  wedged shutdown). This test exercises the parser directly against a small
  fixture manual instead, so MaximaManual::CompileHelpFileAnchors() keeps a
  regression net without needing Maxima, the real HTML manual, or a writable
  config dir.
*/

#include <wx/app.h>
#include <wx/config.h>
#include <wx/file.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/log.h>

#include "Configuration.h"
#include "Dirstructure.h"
#include "MaximaManual.h"

#include <chrono>
#include <cstdlib>
#include <future>
#include <iostream>
#include <thread>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// CompileHelpFileAnchors() keeps a parse only if it yields >= 100 anchors
// (otherwise it falls back to the built-in list), so the fixture needs plenty.
constexpr int kAnchorCount = 150;
} // namespace

SCENARIO("CompileHelpFileAnchors extracts every anchor from the HTML manual") {
  GIVEN("a maxima_singlepage.html holding many <dt id=\"index-...\"> anchors") {
    // Turn a temp file name into a fresh directory tree.
    const wxString docDir = wxFileName::CreateTempFileName(wxS("wxm_manualtest"));
    wxRemoveFile(docDir);
    REQUIRE(wxFileName::Mkdir(docDir, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL));
    const wxString shareDir = docDir + wxS("/share");
    REQUIRE(wxFileName::Mkdir(shareDir, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL));

    wxString html = wxS("<html><body>\n");
    for (int i = 0; i < kAnchorCount; i++)
      html += wxString::Format(wxS("<dt id=\"index-cmd%03d\"></dt>\n"), i);
    html += wxS("</body></html>\n");
    {
      wxFile f(docDir + wxS("/maxima_singlepage.html"), wxFile::write);
      REQUIRE(f.IsOpened());
      REQUIRE(f.Write(html));
    }

    // FindMaximaHtmlDir() short-circuits to a user-configured "helpFile" if one
    // is set; make sure a developer's real config can't hijack the test.
    wxConfig::Get()->DeleteEntry(wxS("helpFile"));

    Configuration cfg;
    cfg.MaximaShareDir(shareDir);
    MaximaManual manual(&cfg);
    manual.FindMaximaHtmlDir(docDir);

    WHEN("the anchors are compiled") {
      const wxString cacheFile =
        wxFileName::CreateTempFileName(wxS("wxm_anchorcache"));
      manual.CompileHelpFileAnchors({}, docDir, shareDir, wxS("test-version"), cacheFile);

      THEN("every anchor in the file is found, keyed without its index- prefix") {
        const MaximaManual::HelpFileAnchors anchors = manual.GetHelpfileAnchors();
        CHECK(anchors.size() >= static_cast<std::size_t>(kAnchorCount));
        CHECK(anchors.count(wxS("cmd000")) == 1);
        CHECK(anchors.count(wxS("cmd149")) == 1);
        // The stored anchor keeps the raw id; the key drops the "index-" prefix.
        auto it = anchors.find(wxS("cmd042"));
        REQUIRE(it != anchors.end());
        CHECK(it->second == wxS("index-cmd042"));
      }
      wxRemoveFile(cacheFile);
    }
    wxFileName::Rmdir(docDir, wxPATH_RMDIR_RECURSIVE);
  }
}

SCENARIO("Asking for the anchors again while they are compiled in the background") {
  GIVEN("a manual whose anchors are compiled in a thread of their own") {
    const wxString docDir = wxFileName::CreateTempFileName(wxS("wxm_manualrace"));
    wxRemoveFile(docDir);
    REQUIRE(wxFileName::Mkdir(docDir, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL));
    const wxString shareDir = docDir + wxS("/share");
    REQUIRE(wxFileName::Mkdir(shareDir, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL));
    // A directory without a manual: asking for that one would clear the
    // manual's HTML directory.
    const wxString emptyDir = docDir + wxS("/empty");
    REQUIRE(wxFileName::Mkdir(emptyDir, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL));
    {
      wxString html = wxS("<html><body>\n");
      for (int i = 0; i < kAnchorCount; i++)
        html += wxString::Format(wxS("<dt id=\"index-cmd%03d\"></dt>\n"), i);
      html += wxS("</body></html>\n");
      wxFile f(docDir + wxS("/maxima_singlepage.html"), wxFile::write);
      REQUIRE(f.IsOpened());
      REQUIRE(f.Write(html));
    }
    wxConfig::Get()->DeleteEntry(wxS("helpFile"));

    // The compile caches its result here; don't leave a file behind that wasn't
    // there before.
    const wxString cacheFile = Dirstructure::AnchorsCacheFile();
    const bool hadCacheFile = wxFileExists(cacheFile);
    const bool usedThreads = Configuration::UseThreads();
    Configuration::UseThreads(true);
    Configuration cfg;
    cfg.MaximaShareDir(shareDir);
    {
      MaximaManual manual(&cfg);
      WHEN("the anchors are asked for again, for another directory, while the compile runs") {
        // A version no anchors cache on disk can match, so the manual is
        // really compiled.
        manual.LoadHelpFileAnchors(docDir, wxS("race-test-version"));
        // Each of these used to assign the manual's HTML directory and Maxima
        // version while the compile thread was reading them: a data race on
        // two wxStrings (ThreadSanitizer reports it), which could also make
        // the thread search the wrong directory.
        for (int i = 0; i < 100; i++)
          manual.LoadHelpFileAnchors(emptyDir, wxS("other-version"));

        THEN("the compile finishes with the anchors of the manual it was started for") {
          MaximaManual::HelpFileAnchors anchors;
          for (int i = 0; i < 600; i++) {
            anchors = manual.GetHelpfileAnchors();
            if (anchors.count(wxS("cmd149")) == 1)
              break;
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
          }
          CHECK(anchors.count(wxS("cmd000")) == 1);
          CHECK(anchors.count(wxS("cmd149")) == 1);
        }
      }
    }
    Configuration::UseThreads(usedThreads);
    if (!hadCacheFile)
      wxRemoveFile(cacheFile);
    wxFileName::Rmdir(docDir, wxPATH_RMDIR_RECURSIVE);
  }
}

SCENARIO("Looking up a keyword before the manual was read uses the built-in anchors") {
  GIVEN("a MaximaManual that hasn't read any manual yet") {
    Configuration cfg;
    // Leaked on purpose: if the lookup deadlocks, the thread doing it never
    // lets go of the object.
    auto *manual = new MaximaManual(&cfg);
    WHEN("help for a keyword is asked for") {
      // GetHelpfileAnchorName() used to hold its lock while loading the
      // built-in anchors, which take the same lock: Maxima's ? then froze
      // wxMaxima for good. Look the keyword up on a thread of its own, so
      // that a deadlock fails this test instead of hanging it.
      std::promise<wxString> anchor;
      std::future<wxString> result = anchor.get_future();
      std::thread([manual, &anchor] {
        anchor.set_value(manual->GetHelpfileAnchorName(wxS("integrate")));
      }).detach();
      if (result.wait_for(std::chrono::seconds(60)) != std::future_status::ready) {
        std::cerr << "GetHelpfileAnchorName() deadlocked\n";
        // The blocked thread still references the promise on this stack.
        std::_Exit(EXIT_FAILURE);
      }
      THEN("the keyword is found in the built-in list") {
        CHECK(!result.get().IsEmpty());
      }
    }
    delete manual;
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

// wxGTK routes regex/file work that needs no display, but Configuration still
// pulls in GTK; mirror the other unit tests and make sure a display exists.
int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
