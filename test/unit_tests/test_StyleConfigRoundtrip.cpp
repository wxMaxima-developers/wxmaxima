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
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

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

namespace {
// The light and dark sets get distinct colors so a key/namespace mix-up between
// them is caught. "salt" picks the set's color family; selecting the appearance
// makes GetWritableStyle()/GetStyle() target that set.
void SeedSet(Configuration &cfg, Configuration::Appearance which, const wxString &salt) {
  cfg.SetAppearance(which);
  for (const auto &keyEntry : Configuration::StyleConfigKeys())
    cfg.GetWritableStyle(keyEntry.first)
      ->SetColor(ColorForPrefix(keyEntry.second + salt));
}
} // namespace

SCENARIO("Both the light and dark style sets round-trip through Write/ReadStyles") {
  GIVEN("a configuration with distinct, key-derived colors in each style set") {
    const wxString file = wxFileName::CreateTempFileName(wxS("wxm_styletest"));
    REQUIRE(!file.IsEmpty());

    Configuration cfgWrite;
    SeedSet(cfgWrite, Configuration::Appearance::light, wxEmptyString);
    SeedSet(cfgWrite, Configuration::Appearance::dark, wxS("#dark"));

    WHEN("the styles are written to a file and read into a fresh configuration") {
      cfgWrite.WriteStyles(file);
      Configuration cfgRead;
      // Pre-seed BOTH sets with different colors than were written, so the only
      // way a style can end up with its written color is for ReadStyles() to read
      // it back under the matching key. Otherwise the shared global wxConfig (one
      // Configuration writing it for the next) could mask a key mismatch.
      SeedSet(cfgRead, Configuration::Appearance::light, wxS("#sentinelL"));
      SeedSet(cfgRead, Configuration::Appearance::dark, wxS("#sentinelD"));
      cfgRead.ReadStyles(file);

      THEN("the light set reads back its colors, and switching to dark shows the "
           "dark set's colors") {
        cfgRead.SetAppearance(Configuration::Appearance::light);
        for (const auto &keyEntry : Configuration::StyleConfigKeys()) {
          INFO("LIGHT style " << static_cast<int>(keyEntry.first) << ", key "
                              << keyEntry.second.ToStdString());
          CHECK(cfgRead.GetStyle(keyEntry.first)->GetColor().GetRGB() ==
                ColorForPrefix(keyEntry.second).GetRGB());
        }
        cfgRead.SetAppearance(Configuration::Appearance::dark);
        for (const auto &keyEntry : Configuration::StyleConfigKeys()) {
          INFO("DARK style " << static_cast<int>(keyEntry.first) << ", key "
                             << keyEntry.second.ToStdString());
          CHECK(cfgRead.GetStyle(keyEntry.first)->GetColor().GetRGB() ==
                ColorForPrefix(keyEntry.second + wxS("#dark")).GetRGB());
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
#ifndef _WIN32
  // Windows runners have a real desktop session, so this is a no-op there
  // (and Xvfb/setenv/sleep are POSIX-only anyway).
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
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
