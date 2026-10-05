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
#include "sidebars/CharButton.h"
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

SCENARIO("A symbol button knows its size before its first size event") {
  // The sidebars compute how many rows their wrapped buttons need from the
  // buttons' sizes as soon as they get their own first size event. wxQt sends a
  // hidden window's size events only once it is shown, the parent's first --
  // so a button that learned its size only in OnSize() was still at its
  // smaller, provisional size then, and the sidebars came up laid out wrongly
  // until the user resized them.
  GIVEN("a freshly constructed button that has never been sized") {
    CharButton *button = new CharButton(g_frame, g_worksheet, g_cfg,
                                        {L'\u03B1', wxS("alpha")}, true);
    const wxSize atConstruction = button->GetMinSize();

    THEN("it already has the min size it needs") {
      REQUIRE(atConstruction.x > 0);
      REQUIRE(atConstruction.y > 0);
    }
    AND_WHEN("it receives its first size event") {
      wxSizeEvent event(wxSize(atConstruction.x * 2, atConstruction.y * 2),
                        button->GetId());
      event.SetEventObject(button);
      button->GetEventHandler()->ProcessEvent(event);

      THEN("that does not change its min size") {
        REQUIRE(button->GetMinSize() == atConstruction);
      }
    }
    button->Destroy();
  }
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
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
