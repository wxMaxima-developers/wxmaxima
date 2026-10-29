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
#include "UnicodeNames.h"

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

SCENARIO("A hex number typed after ESC names a code point (GH #2561)") {
  CHECK(wxm::HexCodePoint(wxS("61")) == 0x61);
  CHECK(wxm::HexCodePoint(wxS("00be")) == 0xBE);
  CHECK(wxm::HexCodePoint(wxS("U+1D49C")) == 0x1D49C);
  CHECK(wxm::HexCodePoint(wxS("u+3b1")) == 0x3B1);
  THEN("anything else doesn't") {
    CHECK(!wxm::HexCodePoint(wxS("")));
    CHECK(!wxm::HexCodePoint(wxS("u+")));
    CHECK(!wxm::HexCodePoint(wxS("alpha")));
    CHECK(!wxm::HexCodePoint(wxS("0x61")));
    CHECK(!wxm::HexCodePoint(wxS("-61")));
    CHECK(!wxm::HexCodePoint(wxS(" 61")));
    CHECK(!wxm::HexCodePoint(wxS("110000")));
    CHECK(!wxm::HexCodePoint(wxS("d800")));
  }
}

SCENARIO("Code points get their names from UnicodeData.txt") {
  CHECK(wxm::UnicodeCharName(0x61) == wxS("LATIN SMALL LETTER A"));
  CHECK(wxm::UnicodeCharName(0x3B1) == wxS("GREEK SMALL LETTER ALPHA"));
  CHECK(wxm::UnicodeCharName(0x1D49C) == wxS("MATHEMATICAL SCRIPT CAPITAL A"));
  THEN("the cached last answer is the right one") {
    CHECK(wxm::UnicodeCharName(0x1D49C) == wxS("MATHEMATICAL SCRIPT CAPITAL A"));
    CHECK(wxm::UnicodeCharName(0x61) == wxS("LATIN SMALL LETTER A"));
  }
  THEN("control characters, ranges and unassigned code points have none") {
    CHECK(wxm::UnicodeCharName(0x07).IsEmpty());
    // The first CJK Unified Ideograph Extension A, only a range in the table
    CHECK(wxm::UnicodeCharName(0x3400).IsEmpty());
    CHECK(wxm::UnicodeCharName(0x10FFFF).IsEmpty());
  }
}

SCENARIO("ESC completion offers the code point typed, not all of Unicode") {
  AutoComplete completer(g_cfg);
  for (const auto &name : {wxS("alpha"), wxS("beta"), wxS("ee"), wxS("pm")})
    completer.AddSymbol(name, AutoComplete::esccommand);

  WHEN("the typed text is a hex number and no name") {
    auto completions = completer.CompleteSymbol(wxS("6"), AutoComplete::esccommand);
    THEN("the code point is the one completion, so typing \"61\" can go on") {
      REQUIRE(completions.size() == 1);
      CHECK(completions.at(0) == wxS("6"));
      CHECK(AutoComplete::IsEscHexCompletion(completions.at(0)));
    }
  }
  WHEN("the typed text is the \"u+\" a code point may begin with") {
    auto completions = completer.CompleteSymbol(wxS("u+"), AutoComplete::esccommand);
    THEN("it is offered, so the popup stays open for the digits") {
      REQUIRE(completions.size() == 1);
      CHECK(AutoComplete::IsEscHexCompletion(completions.at(0)));
      CHECK(AutoComplete::EscCompletionLabel(completions.at(0)) == wxS("u+  U+..."));
    }
  }
  WHEN("a name begins with the typed text, too") {
    auto completions = completer.CompleteSymbol(wxS("be"), AutoComplete::esccommand);
    THEN("the name comes first, so ESC be ESC still enters beta") {
      REQUIRE(completions.size() == 2);
      CHECK(completions.at(0) == wxS("beta"));
      CHECK(completions.at(1) == wxS("be"));
    }
  }
  WHEN("the typed text is a name made of hex digits") {
    auto completions = completer.CompleteSymbol(wxS("ee"), AutoComplete::esccommand);
    THEN("it is a name") {
      REQUIRE(completions.size() == 1);
      CHECK(completions.at(0) == wxS("ee"));
      CHECK(!AutoComplete::IsEscHexCompletion(wxS("ee")));
    }
  }
  WHEN("the typed text is neither") {
    CHECK(completer.CompleteSymbol(wxS("xyz"), AutoComplete::esccommand).empty());
    CHECK(completer.CompleteSymbol(wxS("alp"), AutoComplete::esccommand) ==
          std::vector<wxString>{wxS("alpha")});
  }
  THEN("only ESC completion offers code points") {
    CHECK(!Contains(completer.CompleteSymbol(wxS("61"), AutoComplete::command),
                    wxS("61")));
  }
}

SCENARIO("Maxima's symbol list doesn't cut the startup symbol list short") {
  // At startup LoadSymbols() fills the built-in commands and every ESC name
  // in a background task, and Maxima's own list of symbols arrives right
  // after it connects. That used to stop the task still running, which left
  // ESC with nothing to complete for the rest of the session.
  const bool useThreads = Configuration::UseThreads();
  Configuration::UseThreads(true);
  AutoComplete completer(g_cfg);
  completer.LoadSymbols();
  // Short enough to be read right away, after the previous task is done.
  completer.AddSymbols(
    wxS("<wxxml-symbols><function>fromMaxima</function></wxxml-symbols>"));
  Configuration::UseThreads(useThreads);

  THEN("the ESC names are all there") {
    CHECK(completer.CompleteSymbol(wxS("alp"), AutoComplete::esccommand) ==
          std::vector<wxString>{wxS("alpha")});
  }
  THEN("so are the built-in commands and the symbol Maxima sent") {
    CHECK(Contains(completer.CompleteSymbol(wxS("integr"), AutoComplete::command),
                   wxS("integrate")));
    CHECK(Contains(completer.CompleteSymbol(wxS("fromM"), AutoComplete::command),
                   wxS("fromMaxima")));
  }
}

SCENARIO("The ESC popup shows each symbol next to its name") {
  CHECK(AutoComplete::EscCompletionLabel(wxS("alpha")) ==
        wxString(wxS("alpha  ")) + wxString(wxUniChar(0x3B1)));
  CHECK(AutoComplete::EscCompletionLabel(wxS("61")) ==
        wxS("61  a  U+0061 LATIN SMALL LETTER A"));
  CHECK(AutoComplete::EscCompletionLabel(wxS("u+1d49c")) ==
        wxString(wxS("u+1d49c  ")) + wxString(wxUniChar(0x1D49C)) +
        wxS("  U+1D49C MATHEMATICAL SCRIPT CAPITAL A"));
  THEN("a control character has no glyph to show") {
    CHECK(AutoComplete::EscCompletionLabel(wxS("6")) == wxS("6  U+0006"));
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
