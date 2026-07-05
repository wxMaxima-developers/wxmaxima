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
  Regression test for BTextCtrl::LastActive(), the "which text control gets
  the sidebar symbols?" tracking.

  It used to be a raw wxTextCtrl* inside Configuration and BTextCtrl's
  destructor could not unregister itself (the Configuration object dies first
  on some platforms, issue #2027) -- so closing a wizard whose field had the
  focus left a dangling pointer that Worksheet::OnSidebarKey() would write
  into and SetFocus() on: a use-after-free. The tracking is now a
  wxWeakRef-backed static on BTextCtrl itself; this drives a real BTextCtrl
  and checks the reference nulls itself when the control is destroyed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "BTextCtrl.h"
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
wxFrame *g_frame = nullptr;
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

//! Deliver the focus event BTextCtrl::OnFocus is bound to.
static void FocusByEvent(BTextCtrl *ctrl) {
  wxFocusEvent focus(wxEVT_SET_FOCUS, ctrl->GetId());
  focus.SetEventObject(ctrl);
  ctrl->ProcessWindowEvent(focus);
}

SCENARIO("A focused text control registers itself as the symbol target") {
  BTextCtrl *ctrl = new BTextCtrl(g_frame, wxID_ANY, g_cfg);

  GIVEN("a text control that received the keyboard focus") {
    FocusByEvent(ctrl);

    THEN("it is the last active text control")
      REQUIRE(BTextCtrl::LastActive() == ctrl);

    WHEN("the worksheet takes the input focus back") {
      BTextCtrl::ForgetLastActive();
      THEN("no text control is the symbol target anymore")
        REQUIRE(BTextCtrl::LastActive() == nullptr);
    }
  }
  ctrl->Destroy();
  BTextCtrl::ForgetLastActive();
}

SCENARIO("A destroyed text control cannot stay the symbol target") {
  GIVEN("a focused text control, e.g. in a wizard") {
    BTextCtrl *ctrl = new BTextCtrl(g_frame, wxID_ANY, g_cfg);
    FocusByEvent(ctrl);
    REQUIRE(BTextCtrl::LastActive() == ctrl);

    WHEN("the control is destroyed, e.g. because its wizard was closed") {
      ctrl->Destroy();
      wxTheApp->ProcessIdle(); // flush any delayed window deletion

      THEN("the last-active reference has nulled itself, it doesn't dangle")
        REQUIRE(BTextCtrl::LastActive() == nullptr);
    }
  }
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
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
