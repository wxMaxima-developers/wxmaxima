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
  The configuration dialog's style editor edits the style that was clicked
  (GH #2527).

  wxQt sends the style list's wxEVT_LISTBOX from QListWidget::currentItemChanged,
  which Qt emits on a mouse click before it moves the selection: inside the
  handler the list's GetSelection() still names the style selected before. The
  dialog used to ask the list, so on wxQt every click showed the previously
  selected style, and every edit after it was written to the wrong one.

  The tests below click the way wxQt does on every port: they send the event
  for the new item first and only then move the list's selection.
*/

#include <wx/app.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/listbox.h>
#include <wx/log.h>

#include "cells/TextStyle.h"
#include "dialogs/ConfigDialogue.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
ConfigDialogue *g_dialog = nullptr;

template <class T> T *Control(int id) {
  T *control = dynamic_cast<T *>(g_dialog->FindWindow(id));
  REQUIRE(control != nullptr);
  return control;
}

//! Clicks style `style` in the style list the way wxQt reports the click.
void ClickStyleLikeWxQt(TextStyle style) {
  auto *list = Control<wxListBox>(listbox_styleFor);
  wxCommandEvent event(wxEVT_LISTBOX, list->GetId());
  event.SetEventObject(list);
  event.SetInt(style);
  event.SetString(list->GetString(style));
  list->GetEventHandler()->ProcessEvent(event);
  list->SetSelection(style);
}

//! Ticks or unticks the "bold" checkbox the way a click does.
void ClickBold(bool bold) {
  auto *checkbox = Control<wxCheckBox>(checkbox_bold);
  checkbox->SetValue(bold);
  wxCommandEvent event(wxEVT_CHECKBOX, checkbox->GetId());
  event.SetEventObject(checkbox);
  event.SetInt(bold);
  checkbox->GetEventHandler()->ProcessEvent(event);
}

bool BoldShown() { return Control<wxCheckBox>(checkbox_bold)->GetValue(); }
} // namespace

SCENARIO("The style editor shows the style that was clicked") {
  GIVEN("Code default is selected") {
    ClickStyleLikeWxQt(TS_CODE_DEFAULT);
    REQUIRE(Control<wxButton>(style_font_family)->IsEnabled());

    WHEN("a code highlighting style, whose font can't be chosen, is clicked") {
      ClickStyleLikeWxQt(TS_CODE_VARIABLE);
      THEN("the font button is disabled") {
        CHECK(!Control<wxButton>(style_font_family)->IsEnabled());
      }
      AND_WHEN("Code default is clicked again") {
        ClickStyleLikeWxQt(TS_CODE_DEFAULT);
        THEN("the font button is enabled again") {
          CHECK(Control<wxButton>(style_font_family)->IsEnabled());
        }
      }
    }
  }
}

SCENARIO("Edits in the style editor change the style that was clicked") {
  GIVEN("the section style and the title style are equally bold") {
    ClickStyleLikeWxQt(TS_SECTION);
    ClickBold(false);
    ClickStyleLikeWxQt(TS_TITLE);
    ClickBold(false);

    WHEN("the section style is clicked and made bold") {
      ClickStyleLikeWxQt(TS_SECTION);
      ClickBold(true);
      THEN("the title style stays non-bold") {
        ClickStyleLikeWxQt(TS_TITLE);
        CHECK(!BoldShown());
        AND_THEN("the section style shows the bold it was given") {
          ClickStyleLikeWxQt(TS_SECTION);
          CHECK(BoldShown());
        }
      }
    }
  }
}

SCENARIO("The style editor remembers the style that was clicked last") {
  WHEN("a style is clicked") {
    ClickStyleLikeWxQt(TS_HEADING5);
    THEN("it is stored as the style to edit next time") {
      int stored = -1;
      wxConfig::Get()->Read(wxS("StyleToEdit"), &stored);
      CHECK(stored == TS_HEADING5);
    }
  }
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

  // Keep the dialog away from the user's real configuration.
  const wxString configFile =
    wxFileName::CreateTempFileName(wxS("wxm_configdialogtest"));
  wxConfig::Set(new wxFileConfig(wxS("wxMaxima"), wxEmptyString, configFile));

  g_dialog = new ConfigDialogue(nullptr);

  const int result = Catch::Session().run(argc, argv);

  g_dialog->Destroy();
  delete wxConfig::Set(nullptr);
  wxRemoveFile(configFile);
  wxEntryCleanup();
  return result;
}
