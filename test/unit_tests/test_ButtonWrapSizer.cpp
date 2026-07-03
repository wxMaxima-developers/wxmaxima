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
  Regression tests for Buttonwrapsizer (the Greek/symbol sidebars' wrap sizer).

  The sizer overrides CalcMin() to give every button a uniform, row-filling
  width, which depends on wxWrapSizer's protected m_availSize. That member is -1
  until the first RepositionChildren(), and using it while invalid produced a
  negative button width that stopped the sidebars wrapping into lines (broke on
  wxWidgets 3.3). These tests lay out real buttons and check they wrap onto more
  than one row and never get a non-positive width.
*/

#include <wx/app.h>
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/panel.h>

#include "sidebars/ButtonWrapSizer.h"

#include <cstdlib>
#include <vector>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

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

// Fills a panel with n buttons inside a Buttonwrapsizer, sizes it to the given
// width and lays it out. The buttons are the caller's to inspect afterwards.
static std::vector<wxWindow *> LayOutButtons(wxFrame *frame, int n, int width) {
  wxPanel *panel = new wxPanel(frame);
  auto *sizer = new Buttonwrapsizer(wxHORIZONTAL);
  std::vector<wxWindow *> buttons;
  for (int i = 0; i < n; ++i) {
    auto *b = new wxButton(panel, wxID_ANY, wxString::Format(wxS("%d"), i),
                           wxDefaultPosition, wxSize(40, 20));
    sizer->Add(b, wxSizerFlags().Expand());
    buttons.push_back(b);
  }
  panel->SetSizer(sizer);
  panel->SetSize(wxSize(width, 600));
  // Two layout passes: the first runs CalcMin() while m_availSize is still -1,
  // the second with it known -- both must produce a sane, wrapped layout.
  panel->Layout();
  panel->Layout();
  return buttons;
}

SCENARIO("Buttonwrapsizer wraps its buttons onto multiple rows") {
  wxFrame *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  GIVEN("many buttons in a panel far narrower than their total width") {
    const std::vector<wxWindow *> buttons = LayOutButtons(frame, 16, 150);

    THEN("the buttons occupy more than one row, each with a positive width") {
      const int firstRowY = buttons.front()->GetPosition().y;
      bool wrapped = false;
      for (wxWindow *b : buttons) {
        REQUIRE(b->GetSize().x > 0); // the m_availSize == -1 bug gave width <= 0
        if (b->GetPosition().y > firstRowY)
          wrapped = true;
      }
      REQUIRE(wrapped);
    }
  }
  frame->Destroy();
}

SCENARIO("Buttonwrapsizer keeps everything on one row when the panel is wide enough") {
  wxFrame *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  GIVEN("a few buttons in a panel much wider than their total width") {
    const std::vector<wxWindow *> buttons = LayOutButtons(frame, 4, 4000);

    THEN("they all share the first row") {
      const int firstRowY = buttons.front()->GetPosition().y;
      for (wxWindow *b : buttons) {
        REQUIRE(b->GetSize().x > 0);
        REQUIRE(b->GetPosition().y == firstRowY);
      }
    }
  }
  frame->Destroy();
}

// Mimics the Greek sidebar's nesting: an outer panel with a vertical box sizer
// holds an inner panel (Expand) that carries the Buttonwrapsizer. The width has
// to propagate outer -> vbox -> inner -> wrap sizer for the buttons to wrap.
SCENARIO("Buttonwrapsizer wraps when nested like the Greek sidebar") {
  wxFrame *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  wxPanel *outer = new wxPanel(frame);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxPanel *inner = new wxPanel(outer);
  auto *wrap = new Buttonwrapsizer(wxHORIZONTAL);

  std::vector<wxWindow *> buttons;
  for (int i = 0; i < 24; ++i) { // ~ the Greek alphabet
    auto *b = new wxButton(inner, wxID_ANY, wxString::Format(wxS("%d"), i),
                           wxDefaultPosition, wxSize(40, 20));
    wrap->Add(b, wxSizerFlags().Expand());
    buttons.push_back(b);
  }
  inner->SetSizer(wrap);
  vbox->Add(inner, wxSizerFlags().Expand());
  outer->SetSizer(vbox);

  GIVEN("the outer panel constrained to a narrow width") {
    outer->SetSize(wxSize(180, 600));
    outer->Layout();
    outer->Layout();

    THEN("the letters still wrap onto more than one row") {
      const int firstRowY = buttons.front()->GetScreenPosition().y;
      bool wrapped = false;
      for (wxWindow *b : buttons)
        if (b->GetScreenPosition().y > firstRowY)
          wrapped = true;
      REQUIRE(wrapped);
    }
  }
  frame->Destroy();
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
