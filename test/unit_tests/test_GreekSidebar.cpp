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
  Regression test for the Greek sidebar's line wrapping.

  The sidebar is a wxScrolled that wraps its letter buttons into rows and scrolls
  vertically when they don't all fit. On wxWidgets 3.3 its OnSize() clamped the
  virtual (scrollable) height to the client height, so every wrapped row past the
  first was clipped with no scrollbar to reach it -- the "sidebars no longer break
  into lines" bug. This drives a real GreekSidebar and checks that, when it is too
  small to show every letter, its virtual height grows past the client height so
  the lower rows are reachable.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/panel.h>

#include "Configuration.h"
#include "sidebars/GreekSidebar.h"
#include "sidebars/StatSidebar.h"

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
wxFrame *g_frame = nullptr;
wxWindow *g_worksheet = nullptr;
} // namespace

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

SCENARIO("A too-small statistics sidebar keeps its buttons reachable by scrolling") {
  // Same bug family as the Greek sidebar: a Buttonwrapsizer's own min height
  // is a deliberately-small rearrangeable minimum, so without pinning it the
  // virtual height underestimates the wrapped rows and the buttons below the
  // fold can be neither shown nor scrolled to.
  StatSidebar *sidebar = new StatSidebar(g_frame, wxID_ANY);

  GIVEN("a size far too small to show every button") {
    sidebar->SetSize(wxSize(70, 30));
    sidebar->UpdateVirtualSize();

    THEN("its virtual height exceeds its client height") {
      REQUIRE(sidebar->GetClientSize().x > 0);
      REQUIRE(sidebar->GetVirtualSize().y > sidebar->GetClientSize().y);
    }
  }
  sidebar->Destroy();
}

SCENARIO("A too-small Greek sidebar keeps its wrapped rows reachable by scrolling") {
  GreekSidebar *sidebar =
    new GreekSidebar(g_frame, g_cfg, g_worksheet, wxID_ANY);

  GIVEN("a height far too small to show every wrapped row") {
    // Short -> the wrapped rows are taller than the visible area, so the sidebar
    // must grow its virtual height and let the vertical scrollbar reach them.
    sidebar->SetSize(wxSize(70, 30));
    sidebar->UpdateVirtualSize();

    THEN("its virtual height exceeds its client height (rows below the fold "
         "are scrollable, not clipped away)") {
      REQUIRE(sidebar->GetClientSize().x > 0);
      REQUIRE(sidebar->GetVirtualSize().y > sidebar->GetClientSize().y);
    }
  }
  sidebar->Destroy();
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_worksheet = new wxPanel(g_frame);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
