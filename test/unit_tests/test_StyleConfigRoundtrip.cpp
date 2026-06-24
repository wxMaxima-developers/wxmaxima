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
  Round-trip regression test for the worksheet text styles.

  ReadStyles() and WriteStyles() used to carry two independent, hand-synced lists
  of TextStyle -> config-key-prefix mappings. If the two lists ever drifted (a
  typo, or a key renamed on only one side) the affected style silently stopped
  round-tripping through the configuration storage -- a recurring source of
  "my style settings don't stick" bugs.

  Both functions now iterate a single source of truth,
  Configuration::StyleConfigKeys(). This test guards that: it gives every
  persisted style a distinct, prefix-derived color, writes the styles to a
  temporary config file, reads them back into a fresh Configuration, and checks
  that each style comes back with the color stored under its own key. A future
  read/write key mismatch makes a style read back the wrong color (or a default)
  and fails the test instead of shipping as a silent settings bug.
*/

#include <wx/app.h>
#include <wx/colour.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/log.h>

#include "Configuration.h"
#include "cells/TextStyle.h"

#include <cstdint>
#include <cstdlib>
#include <unistd.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// A deterministic, distinct color per config-key prefix (an FNV-1a hash of the
// prefix). Keying off the prefix (not the style) keeps the two styles that
// deliberately share a key -- TS_CURSOR and TS_ACTIVE_CELL_BRACKET both use
// "Style/ActiveCellBracket/" -- consistent instead of clashing.
wxColour ColorForPrefix(const wxString &prefix) {
  std::uint32_t h = 2166136261u;
  for (const auto ch : prefix)
    h = (h ^ static_cast<std::uint32_t>(static_cast<wxUChar>(ch))) * 16777619u;
  return wxColour(static_cast<unsigned char>(h & 0xFF),
                  static_cast<unsigned char>((h >> 8) & 0xFF),
                  static_cast<unsigned char>((h >> 16) & 0xFF));
}
} // namespace

SCENARIO("Every persisted style round-trips through WriteStyles/ReadStyles") {
  GIVEN("a configuration whose every style carries a distinct, key-derived color") {
    const wxString file = wxFileName::CreateTempFileName(wxS("wxm_styletest"));
    REQUIRE(!file.IsEmpty());

    Configuration cfgWrite;
    for (const auto &keyEntry : Configuration::StyleConfigKeys())
      cfgWrite.GetWritableStyle(keyEntry.first)
        ->SetColor(ColorForPrefix(keyEntry.second));

    WHEN("the styles are written to a file and read into a fresh configuration") {
      cfgWrite.WriteStyles(file);
      Configuration cfgRead;
      // Pre-seed every style with a *different* color than what was written, so
      // the only way a style can end up with its written color is for ReadStyles()
      // to actually read it back under the matching key. Without this the shared
      // global wxConfig (one Configuration writing it for the next) could mask a
      // key mismatch and make the test vacuous.
      for (const auto &keyEntry : Configuration::StyleConfigKeys())
        cfgRead.GetWritableStyle(keyEntry.first)
          ->SetColor(ColorForPrefix(keyEntry.second + wxS("#sentinel")));
      cfgRead.ReadStyles(file);

      THEN("each style reads back the color stored under its own key") {
        for (const auto &keyEntry : Configuration::StyleConfigKeys()) {
          INFO("style index " << static_cast<int>(keyEntry.first) << ", key "
                              << keyEntry.second.ToStdString());
          CHECK(cfgRead.GetStyle(keyEntry.first)->GetColor().GetRGB() ==
                ColorForPrefix(keyEntry.second).GetRGB());
        }
      }
    }
    wxRemoveFile(file);
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

// wxGTK routes color/font work through GTK, which needs an X display. When run
// via ctest the headless wrapper provides one; when run directly we start ours.
static void EnsureDisplay() {
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
