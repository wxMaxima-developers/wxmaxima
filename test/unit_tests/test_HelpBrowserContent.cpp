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
  Tests for HelpBrowser::HasContent() and ShowStartPageIfEmpty().

  The help sidebar creates its browser only when something asks for help, so
  until then it is an empty panel. wxMaximaFrame therefore hides it at
  startup, and if the user opens it by hand before asking for help it shows
  the manual (ShowStartPageIfEmpty()) instead of staying empty.
*/

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "sidebars/HelpBrowser.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#ifdef USE_WEBVIEW
namespace {
//! A help sidebar in a frame of its own, with nothing loaded yet
struct HelpSetup {
  HelpSetup() {
    frame = new wxFrame(nullptr, wxID_ANY, wxS("help browser test"));
    help = new HelpBrowser(frame, nullptr, nullptr,
                           wxS("data:text/html,<p>start page</p>"));
  }
  ~HelpSetup() { frame->Destroy(); }
  wxFrame *frame;
  HelpBrowser *help;
};
} // namespace

SCENARIO("The help sidebar is empty until help is put into it") {
  GIVEN("a freshly created help sidebar") {
    HelpSetup s;
    THEN("it has no content") { CHECK_FALSE(s.help->HasContent()); }
    WHEN("Maxima's ? offers several topics") {
      s.help->SelectKeywords({wxS("integrate"), wxS("integrate_use_rootsof")});
      THEN("the topic list counts as content") { CHECK(s.help->HasContent()); }
    }
    WHEN("it is opened by hand before any help was asked for") {
      s.help->ShowStartPageIfEmpty();
      THEN("it shows the start page") { CHECK(s.help->HasContent()); }
    }
  }
}
#endif // USE_WEBVIEW

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

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
