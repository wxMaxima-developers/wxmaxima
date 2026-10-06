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
  Regression tests for which toolbar buttons are active.

  ToolBar::CanRedo() used to enable or disable the *Undo* button, so Redo
  never changed state and Undo followed whichever of the two was set last.
  And ToolBar remembers the last state it set each button to, to skip
  redundant updates: re-creating the tools (which the toolbar does when the
  user shows or hides a group of buttons via its context menu) enabled them
  all again without resetting that memory, so a button could stay active
  with nothing to act on.
*/

#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/fileconf.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "ToolBar.h"
#include "wxMaximaArtProvider.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxFrame *g_frame = nullptr;

//! Gives the test access to the handler of the toolbar's context menu
class TestToolBar : public ToolBar {
public:
  explicit TestToolBar(wxWindow *parent) : ToolBar(parent) {}
  //! Show or hide a group of tools, as the toolbar's context menu does
  void ToggleToolGroup(int id) {
    wxCommandEvent ev(wxEVT_MENU, id);
    OnMenu(ev);
  }
};
} // namespace

SCENARIO("Undo and Redo are activated independently") {
  auto *toolbar = new TestToolBar(g_frame);
  REQUIRE(toolbar->FindTool(wxID_UNDO) != nullptr);
  REQUIRE(toolbar->FindTool(wxID_REDO) != nullptr);

  toolbar->CanUndo(false);
  toolbar->CanRedo(false);
  REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_UNDO));
  REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_REDO));

  WHEN("there is something to redo, but nothing to undo") {
    toolbar->CanRedo(true);
    THEN("only Redo is active") {
      REQUIRE(toolbar->GetToolEnabled(wxID_REDO));
      REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_UNDO));
    }
  }
  WHEN("there is something to undo, but nothing to redo") {
    toolbar->CanUndo(true);
    THEN("only Undo is active") {
      REQUIRE(toolbar->GetToolEnabled(wxID_UNDO));
      REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_REDO));
    }
  }
  toolbar->Destroy();
}

SCENARIO("Re-creating the tools doesn't leave them wrongly active") {
  auto *toolbar = new TestToolBar(g_frame);
  toolbar->CanUndo(false);
  toolbar->CanRedo(false);
  toolbar->CanCopy(false);

  WHEN("the user hides and shows a group of tools again") {
    toolbar->ToggleToolGroup(
        ToolBar::SectionMenuId(ToolBarSections::Section::CopyPaste));
    toolbar->ToggleToolGroup(
        ToolBar::SectionMenuId(ToolBarSections::Section::CopyPaste));
    // The re-created tools are all active; the next update has to correct
    // that.
    toolbar->CanUndo(false);
    toolbar->CanRedo(false);
    toolbar->CanCopy(false);
    THEN("the buttons with nothing to act on are inactive again") {
      REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_UNDO));
      REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_REDO));
      REQUIRE_FALSE(toolbar->GetToolEnabled(wxID_COPY));
    }
  }
  toolbar->Destroy();
}

SCENARIO("The Select All button selects all instead of hiding the code") {
  // The button used to be created with the id of the "Hide code" button, so
  // clicking it toggled the visibility of all code cells.
  auto *toolbar = new TestToolBar(g_frame);
  THEN("Select All has its own id") {
    REQUIRE(toolbar->FindTool(wxID_SELECTALL) != nullptr);
  }
  THEN("the only button with the id of Hide code is Hide code") {
    int hideCodeButtons = 0;
    for (size_t i = 0; i < toolbar->GetToolCount(); i++)
      if (toolbar->FindToolByIndex(i)->GetId() == ToolBar::tb_hideCode)
        hideCodeButtons++;
    REQUIRE(hideCodeButtons == 1);
    REQUIRE(toolbar->FindTool(ToolBar::tb_hideCode)->GetLabel() ==
            _("Hide Code"));
  }
  toolbar->Destroy();
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
  // An in-memory configuration (style 0 never touches the disk), so that
  // which tool groups are shown doesn't depend on, or leak into, a config
  // file a previous run left behind. Undo/Redo is hidden by default.
  delete wxConfigBase::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                            wxEmptyString, wxEmptyString, 0));
  wxConfig::Get()->Write(wxS("Toolbar/showUndoRedo"), true);
  wxConfig::Get()->Write(wxS("Toolbar/showCopyPaste"), true);
  wxArtProvider::Push(new wxMaximaArtProvider);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
