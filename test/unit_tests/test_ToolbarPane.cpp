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
  Tests SetToolbarPaneProperties(): the main toolbar's height must follow from
  its contents, not from the layout a previous session stored.
*/

#include "ToolbarPane.h"

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/panel.h>
#include <wx/artprov.h>
#include <wx/aui/framemanager.h>
#include <wx/aui/auibar.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {

//! A frame laid out the way wxMaximaFrame does it: a worksheet stand-in in the
//! centre and a horizontal wxAuiToolBar docked at the top.
struct ToolbarFrame {
  ToolbarFrame() {
    frame = new wxFrame(nullptr, wxID_ANY, wxS("test"), wxDefaultPosition,
                        wxSize(800, 600));
    manager.SetManagedWindow(frame);
    toolbar = new wxAuiToolBar(frame, wxID_ANY, wxDefaultPosition,
                               wxDefaultSize, wxAUI_TB_HORIZONTAL);
    toolbar->AddTool(wxID_OPEN, wxS("Open"),
                     wxArtProvider::GetBitmap(wxART_FILE_OPEN, wxART_TOOLBAR));
    toolbar->Realize();
    manager.AddPane(new wxPanel(frame), wxAuiPaneInfo().Name(wxS("console"))
                    .Center().CaptionVisible(false).PaneBorder(false));
    manager.AddPane(toolbar,
                    SetToolbarPaneProperties(
                      wxAuiPaneInfo().Name(wxS("toolbar")).Top().Row(0),
                      toolbar));
    frame->Show();
    manager.Update();
  }
  ~ToolbarFrame() {
    manager.UnInit();
    frame->Destroy();
  }
  //! What the toolbar's contents need.
  int NaturalHeight() const { return toolbar->GetHintSize(wxAUI_DOCK_TOP).y; }
  //! What wxAUI actually gives it.
  int ActualHeight() const { return toolbar->GetSize().y; }
  wxAuiPaneInfo &Pane() { return manager.GetPane(wxS("toolbar")); }
  //! Loads a stored layout the way wxMaximaFrame's constructor does, then
  //! lays out twice, as wxMaximaFrame's and wxMaxima's constructors each do:
  //! the dock a perspective creates keeps its stored size for the first
  //! layout, and only a fixed dock is sized from its contents after that.
  void Load(const wxString &perspective, bool repair) {
    manager.LoadPerspective(perspective, false);
    toolbar->Realize();
    if (repair)
      SetToolbarPaneProperties(Pane(), toolbar);
    manager.Update();
    manager.Update();
  }

  wxFrame *frame;
  wxAuiManager manager;
  wxAuiToolBar *toolbar;
};

//! Replaces the value of one `key=value` field of the toolbar's pane entry.
wxString SetToolbarField(const wxString &perspective, const wxString &key,
                         long value) {
  wxString result;
  for (const wxString &entry : wxSplit(perspective, '|', '\0')) {
    wxString out = entry;
    if (entry.StartsWith(wxS("name=toolbar;"))) {
      wxArrayString fields = wxSplit(entry, ';', '\0');
      for (auto &field : fields)
        if (field.StartsWith(key + wxS("=")))
          field = key + wxS("=") + wxString::Format(wxS("%ld"), value);
      out = wxJoin(fields, ';', '\0');
    }
    if (!result.IsEmpty())
      result += wxS("|");
    result += out;
  }
  return result;
}

//! Shrinks every stored dock size by one pixel, like a drifted layout would.
wxString ShrinkDockSizes(const wxString &perspective) {
  wxString result;
  for (const wxString &entry : wxSplit(perspective, '|', '\0')) {
    wxString out = entry;
    if (entry.StartsWith(wxS("dock_size(1,"))) {
      long size;
      if (entry.AfterFirst('=').ToLong(&size))
        out = entry.BeforeFirst('=') + wxString::Format(wxS("=%ld"), size - 1);
    }
    if (!result.IsEmpty())
      result += wxS("|");
    result += out;
  }
  return result;
}

} // namespace

SCENARIO("The toolbar's height comes from its contents, not the stored layout") {
  ToolbarFrame f;
  const int natural = f.NaturalHeight();
  REQUIRE(natural > 1);
  REQUIRE(f.ActualHeight() == natural);

  GIVEN("a stored layout that remembers the toolbar one pixel too short") {
    wxString stored = f.manager.SavePerspective();
    stored = SetToolbarField(stored, wxS("besth"), natural - 1);
    stored = ShrinkDockSizes(stored);

    WHEN("it is loaded without repairing the toolbar pane") {
      f.Load(stored, false);
      THEN("wxAUI keeps the stored, wrong height") {
        // Not what we want -- but it shows the scenario really reproduces
        // the bug, so the check below means something.
        CHECK(f.ActualHeight() == natural - 1);
      }
    }
    WHEN("it is loaded the way wxMaxima does") {
      f.Load(stored, true);
      THEN("the toolbar gets the height its contents need") {
        CHECK(f.ActualHeight() == natural);
      }
      THEN("the next stored layout is correct again") {
        CHECK(f.manager.SavePerspective().Contains(
                wxString::Format(wxS("besth=%d;"), natural)));
      }
    }
  }

  GIVEN("a stored layout whose toolbar pane lost its DockFixed flag") {
    wxString stored = f.manager.SavePerspective();
    stored = SetToolbarField(stored, wxS("state"),
                             f.Pane().state &
                             ~static_cast<unsigned int>(
                               wxAuiPaneInfo::optionDockFixed));
    stored = SetToolbarField(stored, wxS("besth"), natural - 1);
    stored = ShrinkDockSizes(stored);

    WHEN("it is loaded the way wxMaxima does") {
      f.Load(stored, true);
      THEN("the dock is fixed again and sized from the toolbar") {
        CHECK(f.Pane().HasFlag(wxAuiPaneInfo::optionDockFixed));
        CHECK(f.ActualHeight() == natural);
      }
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

// wxGTK needs a display for the frame, the toolbar and wxAUI's layout.
int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
