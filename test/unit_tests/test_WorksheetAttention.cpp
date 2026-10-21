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
  Tests that the worksheet tells its owner when something needs the user's
  attention, and when that is over.

  The tray icon is shown only while this is the case (see TrayIcon), so a
  notification that is raised without the callback firing would leave the
  icon hidden when it is needed, and a missing "cleared" call would leave it
  in the tray forever.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"

#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Worksheet *g_ws = nullptr;
} // namespace

SCENARIO("A notification asks for attention until it is cleared") {
  std::vector<bool> calls;
  g_ws->SetAttentionCallback([&calls](bool needed) { calls.push_back(needed); });

  GIVEN("an active window") {
    g_ws->WindowActive(true);
    WHEN("Maxima finishes calculating") {
      g_ws->SetNotification(wxS("Maxima has finished calculating."));
      THEN("nobody is told: the user is looking at the window anyway") {
        CHECK(calls.empty());
        CHECK_FALSE(g_ws->GetNotification().has_value());
      }
    }
  }

  GIVEN("a window that isn't active") {
    g_ws->WindowActive(false);
    WHEN("Maxima finishes calculating") {
      g_ws->SetNotification(wxS("Maxima has finished calculating."));
      THEN("attention is asked for") {
        REQUIRE(calls == std::vector<bool>{true});
        CHECK(g_ws->GetNotification().has_value());
      }
      AND_WHEN("the notification is cleared") {
        g_ws->ClearNotification();
        THEN("the attention request ends") {
          CHECK(calls == std::vector<bool>{true, false});
        }
        AND_WHEN("it is cleared again, as every key press does") {
          g_ws->ClearNotification();
          THEN("nothing more is reported") {
            CHECK(calls == std::vector<bool>{true, false});
          }
        }
      }
    }
  }

  g_ws->ClearNotification();
  g_ws->SetAttentionCallback({});
  g_ws->WindowActive(true);
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

  auto *bmp = new wxBitmap(400, 400);
  auto *dc = new wxMemoryDC();
  dc->SelectObject(*bmp);
  auto *cfg = new Configuration(dc);
  cfg->SetZoomFactor(1.0);
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(frame, wxID_ANY, cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
