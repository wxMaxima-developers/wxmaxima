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
  Tests for AdaptiveSplitter, the splitter between the configuration
  dialog's style settings and its sample worksheet.

  The splitter puts the preview below the settings or beside them, whichever
  leaves it more room, keeps a sash the user moved, and remembers it across
  dialog sessions. These tests check the direction choice as plain geometry
  and then drive a real splitter through resizes and sash moves.
*/

#include <wx/app.h>
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/panel.h>

#include "dialogs/AdaptiveSplitter.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

SCENARIO("ChooseSplitMode gives the preview the larger area") {
  const wxSize settings(300, 200);
  THEN("a wide, short space puts the preview beside the settings") {
    // below: 1000 x 200; beside: 700 x 400
    REQUIRE(AdaptiveSplitter::ChooseSplitMode(wxSize(1000, 400), settings) ==
            wxSPLIT_VERTICAL);
  }
  THEN("a tall space puts the preview below the settings") {
    // below: 600 x 600; beside: 300 x 800
    REQUIRE(AdaptiveSplitter::ChooseSplitMode(wxSize(600, 800), settings) ==
            wxSPLIT_HORIZONTAL);
  }
  THEN("a tie keeps the preview below the settings") {
    // below: 600 x 200; beside: 300 x 400
    REQUIRE(AdaptiveSplitter::ChooseSplitMode(wxSize(600, 400), settings) ==
            wxSPLIT_HORIZONTAL);
  }
  THEN("settings larger than the space in both directions don't go negative") {
    REQUIRE(AdaptiveSplitter::ChooseSplitMode(wxSize(100, 100), settings) ==
            wxSPLIT_HORIZONTAL);
  }
}

namespace {
//! A frame holding an AdaptiveSplitter between two plain panels
struct SplitterFixture {
  SplitterFixture() {
    frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
    splitter = new AdaptiveSplitter(frame);
    settings = new wxPanel(splitter);
    preview = new wxPanel(splitter);
    settings->SetMinSize(wxSize(300, 80));
    splitter->SplitAdaptively(settings, preview, wxSize(300, 200));
  }
  ~SplitterFixture() { frame->Destroy(); }

  void Resize(wxSize size) {
    splitter->SetSize(size);
    // Not every port sends a size event for a window that isn't shown yet.
    splitter->SendSizeEvent();
  }

  //! What a user dragging the sash to \p pos causes
  void DragSashTo(int pos) {
    splitter->SetSashPosition(pos);
    wxSplitterEvent event(wxEVT_SPLITTER_SASH_POS_CHANGED, splitter);
    event.SetSashPosition(pos);
    splitter->GetEventHandler()->ProcessEvent(event);
  }

  wxFrame *frame;
  AdaptiveSplitter *splitter;
  wxPanel *settings;
  wxPanel *preview;
};
} // namespace

SCENARIO("AdaptiveSplitter picks its direction from its size") {
  SplitterFixture f;
  WHEN("it is wide and short") {
    f.Resize(wxSize(1000, 400));
    THEN("the preview is beside the settings, which get their preferred width") {
      REQUIRE(f.splitter->IsSplit());
      REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_VERTICAL);
      REQUIRE(f.splitter->GetSashPosition() == 300);
    }
  }
  WHEN("it is tall") {
    f.Resize(wxSize(600, 800));
    THEN("the preview is below the settings, which get their preferred height") {
      REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_HORIZONTAL);
      REQUIRE(f.splitter->GetSashPosition() == 200);
    }
  }
  THEN("its minimum size only asks for the settings' minimum plus some preview") {
    // Not the sum of both panes' best sizes: inside the dialog's scrolled
    // tab that would make the tab scroll instead of the splitter fitting.
    REQUIRE(f.splitter->GetMinSize().x == 300);
    REQUIRE(f.splitter->GetMinSize().y > 80);
    REQUIRE(f.splitter->GetMinSize().y < 200);
  }
}

SCENARIO("AdaptiveSplitter keeps a sash the user moved") {
  SplitterFixture f;
  f.Resize(wxSize(600, 800));
  REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_HORIZONTAL);

  WHEN("the user shrinks the settings and the splitter then grows") {
    f.DragSashTo(120);
    f.Resize(wxSize(600, 900));
    THEN("the settings keep the height the user gave them") {
      REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_HORIZONTAL);
      REQUIRE(f.splitter->GetSashPosition() == 120);
      REQUIRE(f.splitter->GetPreferredSettingsSize().y == 120);
    }
  }
  WHEN("the user shrinks the settings and the direction changes and back") {
    f.DragSashTo(120);
    f.Resize(wxSize(1200, 300));
    REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_VERTICAL);
    REQUIRE(f.splitter->GetSashPosition() == 300);
    f.Resize(wxSize(600, 800));
    THEN("the settings get the height the user chose again") {
      REQUIRE(f.splitter->GetSplitMode() == wxSPLIT_HORIZONTAL);
      REQUIRE(f.splitter->GetSashPosition() == 120);
    }
  }
}

SCENARIO("AdaptiveSplitter remembers the sash across sessions") {
  const wxString configFile = wxFileName::CreateTempFileName(wxS("splittertest"));
  wxConfigBase *oldConfig = wxConfig::Set(
    new wxFileConfig(wxS("test"), wxS("test"), configFile, wxEmptyString,
                     wxCONFIG_USE_LOCAL_FILE));
  {
    SplitterFixture f;
    f.splitter->RememberSashIn(wxS("Test/SettingsSize"));
    f.Resize(wxSize(600, 800));
    f.DragSashTo(150);
  }
  GIVEN("a new splitter that remembers in the same place") {
    SplitterFixture f;
    f.splitter->RememberSashIn(wxS("Test/SettingsSize"));
    f.Resize(wxSize(600, 800));
    THEN("the settings get the height the user chose last time") {
      REQUIRE(f.splitter->GetPreferredSettingsSize() == wxSize(300, 150));
      REQUIRE(f.splitter->GetSashPosition() == 150);
    }
  }
  delete wxConfig::Set(oldConfig);
  wxRemoveFile(configFile);
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
