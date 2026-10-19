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
  Tests for LimitFloatingPaneHeight().

  The find/replace sidebar and the wizard have a natural height; a floating
  one used to be resizable to any height, which only added empty space. These
  tests float a pane in a real wxAuiManager and check that its frame can be
  widened but not made higher than its contents, and that the function leaves
  docked and hidden panes alone.
*/

#include <wx/app.h>
#include <wx/aui/aui.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/panel.h>

#include "FloatingPaneHeight.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

namespace {
//! A main frame with one pane, \p floating or docked, managed by wxAUI
struct AuiSetup {
  explicit AuiSetup(bool floating) {
    frame = new wxFrame(nullptr, wxID_ANY, wxS("test"), wxDefaultPosition,
                        wxSize(800, 600));
    manager.SetManagedWindow(frame);
    manager.AddPane(new wxPanel(frame), wxAuiPaneInfo().Name(wxS("center")).Center());
    pane = new wxPanel(frame, wxID_ANY, wxDefaultPosition, wxSize(200, 100));
    wxAuiPaneInfo info;
    info.Name(wxS("pane")).Caption(wxS("pane")).FloatingSize(wxSize(300, 400));
    if (floating)
      info.Float();
    else
      info.Right();
    manager.AddPane(pane, info);
    frame->Show();
    manager.Update();
    wxYield();
  }
  ~AuiSetup() {
    manager.UnInit();
    frame->Destroy();
  }
  wxAuiFloatingFrame *FloatingFrame() {
    return wxDynamicCast(manager.GetPane(pane).frame, wxAuiFloatingFrame);
  }
  wxFrame *frame;
  wxAuiManager manager;
  wxPanel *pane;
};
} // namespace

SCENARIO("A floating pane can be widened but not made higher than its contents") {
  AuiSetup setup(true);
  wxAuiFloatingFrame *floating = setup.FloatingFrame();
  REQUIRE(floating != nullptr);
  REQUIRE(floating->IsShown());
  const int decoration = floating->GetSize().y - setup.pane->GetSize().y;
  // Smaller than the 400 pixels the frame floats with: it has to shrink.
  const int contentHeight = 150;

  WHEN("its height is limited") {
    LimitFloatingPaneHeight(setup.pane, contentHeight);
    const int maxHeight = contentHeight + decoration;

    THEN("the frame shrinks to the contents and can't grow higher") {
      REQUIRE(floating->GetMaxSize().y == maxHeight);
      REQUIRE(floating->GetSize().y == maxHeight);
      floating->SetSize(wxSize(floating->GetSize().x, 3 * maxHeight));
      REQUIRE(floating->GetSize().y == maxHeight);
    }
    THEN("its width stays free") {
      REQUIRE(floating->GetMaxSize().x == wxDefaultCoord);
      floating->SetSize(wxSize(500, floating->GetSize().y));
      REQUIRE(floating->GetSize().x == 500);
    }
    THEN("its minimum height doesn't exceed the maximum") {
      REQUIRE(floating->GetMinSize().y <= maxHeight);
    }
    AND_WHEN("the contents get higher") {
      LimitFloatingPaneHeight(setup.pane, contentHeight + 50);
      THEN("the frame may grow that much more") {
        REQUIRE(floating->GetMaxSize().y == maxHeight + 50);
      }
    }
  }
}

SCENARIO("A docked pane is left alone") {
  AuiSetup setup(false);
  REQUIRE(setup.FloatingFrame() == nullptr);
  const wxSize maxBefore = setup.frame->GetMaxSize();
  LimitFloatingPaneHeight(setup.pane, 50);
  REQUIRE(setup.frame->GetMaxSize() == maxBefore);
}

SCENARIO("A hidden floating pane is left alone") {
  // wxGTK sets a floating frame's minimum size when it is first shown; a
  // maximum set before, for contents that may grow until then, would make
  // that assert.
  AuiSetup setup(true);
  setup.manager.GetPane(setup.pane).Hide();
  setup.manager.Update();
  wxAuiFloatingFrame *floating = setup.FloatingFrame();
  if (floating != nullptr) {
    REQUIRE(!floating->IsShown());
    const wxSize maxBefore = floating->GetMaxSize();
    LimitFloatingPaneHeight(setup.pane, 50);
    REQUIRE(floating->GetMaxSize() == maxBefore);
  }
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
