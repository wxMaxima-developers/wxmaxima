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
  Round-trip regression test for the mechanical scalar configuration settings.

  ReadConfig() and the settings-writing side used to carry independent,
  hand-synced config->Read()/config->Write() call pairs; if the two ever
  drifted (a typo, or a key renamed on only one side) the affected setting
  silently stopped round-tripping - the same recurring bug class the styles
  had before Configuration::StyleConfigKeys().

  Both sides now iterate Configuration::ScalarConfigSettings(). This test
  guards that table: it perturbs every listed member away from its default,
  writes the settings to a temporary config file, reads them back into a
  fresh Configuration, and checks every member arrives unchanged. A future
  key or type mismatch leaves the fresh instance at the default value for
  that key and fails the test naming it.
*/

#include <wx/app.h>
#include <wx/fileconf.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/log.h>

#include "Configuration.h"

#include <cstdlib>
#include <type_traits>
#include <variant>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
/*! Moves every setting listed in ScalarConfigSettings() away from its default.

  Bools are flipped, strings gain a suffix and numbers are incremented by one.
  The +1 stays within the legal range of every currently clamped setting
  (showLength <= 3 has default 2, tocDepth >= 1, displayedDigits >= 20 has a
  three-digit default); if a future table entry clamps tighter, pick its
  perturbation here accordingly.
*/
void Perturb(Configuration &cfg) {
  for (const auto &setting : Configuration::ScalarConfigSettings())
    std::visit(
      [&](auto member) {
        using Member = std::remove_reference_t<decltype(cfg.*member)>;
        if constexpr (std::is_same_v<Member, bool>)
          cfg.*member = !(cfg.*member);
        else if constexpr (std::is_same_v<Member, wxString>)
          (cfg.*member) += wxS("X");
        else
          cfg.*member = (cfg.*member) + 1;
      },
      setting.member);
}
} // namespace

SCENARIO("Every scalar setting in the table round-trips through write and read") {
  GIVEN("a configuration whose scalar settings all differ from the defaults") {
    const wxString file = wxFileName::CreateTempFileName(wxS("wxm_configtest"));
    REQUIRE(!file.IsEmpty());

    Configuration cfgWrite(nullptr, Configuration::temporary);
    Perturb(cfgWrite);

    WHEN("the settings are written to a file and read into a fresh configuration") {
      cfgWrite.WriteStyles(file);

      // ReadConfig() reads from the global wxConfig, so point it at our file
      // for the duration of the read (restoring the old one afterwards).
      wxConfigBase *oldConfig = wxConfig::Get(false);
      wxConfigBase *fileConfig =
        new wxFileConfig(wxS("wxMaxima"), wxEmptyString, file);
      wxConfig::Set(fileConfig);
      Configuration cfgRead(nullptr, Configuration::temporary);
      cfgRead.ReadConfig();
      wxConfig::Set(oldConfig);
      delete fileConfig;

      THEN("every member of the settings table arrives unchanged") {
        for (const auto &setting : Configuration::ScalarConfigSettings()) {
          INFO("config key: " << wxString(setting.key).ToStdString());
          std::visit(
            [&](auto member) {
              CHECK(cfgRead.*member == cfgWrite.*member);
            },
            setting.member);
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
