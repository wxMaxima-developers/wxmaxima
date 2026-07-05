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
  Regression test for MaximaMenuSync, the declarative variable-to-menu-items
  table that replaced a dozen hand-written VariableAction* handlers in the
  wxMaxima class.

  It builds a menu bar containing real check/radio items for every table row
  (with the row's genuine menu ids) and then feeds variable values through
  SyncMenusToMaximaVariable(), asserting on the resulting check marks - one
  scenario per match kind, plus the enable-on-any-value side effect and the
  table's internal consistency.
*/

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>

#include "MaximaMenuSync.h"

#include <cstdlib>
#include <set>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxFrame *g_frame = nullptr;
wxMenuBar *g_menubar = nullptr;
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

//! Build one menu per table row, holding real items with the row's menu ids.
static wxMenuBar *MenuBarFromTable() {
  wxMenuBar *menubar = new wxMenuBar;
  int n = 0;
  for (const MaximaMenuSyncRow &row : MaximaMenuSyncRows()) {
    wxMenu *menu = new wxMenu;
    if (row.m_enableId != wxID_NONE) {
      menu->Append(row.m_enableId, wxS("feature trigger"));
      menu->Enable(row.m_enableId, false);
    }
    const bool radio =
      (row.m_kind == MaximaMenuSyncRow::Kind::RadioFromValue) ||
      (row.m_kind == MaximaMenuSyncRow::Kind::RadioFromSuffix);
    for (const MaximaMenuSyncEntry &entry : row.m_entries) {
      const wxString label = wxString::Format(wxS("item %d"), entry.m_menuId);
      if (radio)
        menu->AppendRadioItem(entry.m_menuId, label);
      else
        menu->AppendCheckItem(entry.m_menuId, label);
    }
    menubar->Append(menu, wxString::Format(wxS("menu %d"), n++));
  }
  return menubar;
}

//! The menu id the given table row maps this value to (REQUIREs it exists).
static int IdForValue(const wxString &variable, const wxString &value) {
  for (const MaximaMenuSyncRow &row : MaximaMenuSyncRows())
    if (row.m_variable == variable)
      for (const MaximaMenuSyncEntry &entry : row.m_entries)
        if (entry.m_value == value)
          return entry.m_menuId;
  REQUIRE(false);
  return wxID_NONE;
}

SCENARIO("The table is internally consistent") {
  std::set<wxString> variables;
  std::set<int> ids;
  for (const MaximaMenuSyncRow &row : MaximaMenuSyncRows()) {
    THEN("every row watches a distinct variable and has entries") {
      REQUIRE(variables.insert(row.m_variable).second);
      REQUIRE(!row.m_entries.empty());
    }
    std::set<wxString> values;
    for (const MaximaMenuSyncEntry &entry : row.m_entries) {
      THEN("within a row, the values and menu ids are unique") {
        REQUIRE(values.insert(entry.m_value).second);
        REQUIRE(ids.insert(entry.m_menuId).second);
      }
    }
  }
}

SCENARIO("A boolean variable toggles its check item") {
  const int id = IdForValue(wxS("numer"), wxS("true"));
  WHEN("numer becomes true") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("numer"), wxS("true")));
    THEN("the numeric-output item is checked")
      REQUIRE(g_menubar->IsChecked(id));
  }
  WHEN("numer becomes false again") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("numer"), wxS("false")));
    THEN("the numeric-output item is unchecked")
      REQUIRE(!g_menubar->IsChecked(id));
  }
}

SCENARIO("An inverted boolean is only unchecked on its 'off' value") {
  const int id = IdForValue(wxS("showtime"), wxS("false"));
  // showtime knows more values than true/false; "real" etc. mean "on".
  WHEN("showtime becomes anything but false") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("showtime"), wxS("real")));
    THEN("the time-display item is checked")
      REQUIRE(g_menubar->IsChecked(id));
  }
  WHEN("showtime becomes false") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("showtime"), wxS("false")));
    THEN("the time-display item is unchecked")
      REQUIRE(!g_menubar->IsChecked(id));
  }
}

SCENARIO("A multiple-choice variable checks the matching radio item") {
  WHEN("gentranlang becomes fortran") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("gentranlang"),
                                      wxS("fortran")));
    THEN("the fortran item is checked, displacing its siblings") {
      REQUIRE(g_menubar->IsChecked(IdForValue(wxS("gentranlang"),
                                              wxS("fortran"))));
      REQUIRE(!g_menubar->IsChecked(IdForValue(wxS("gentranlang"), wxS("c"))));
    }
  }
}

SCENARIO("A suffix-matched variable ignores a prefix on the value") {
  WHEN("lmxchar's value ends in the matrix bracket character") {
    REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("lmxchar"),
                                      wxS("\x1B[")));
    THEN("the square-parenthesis item is checked")
      REQUIRE(g_menubar->IsChecked(IdForValue(wxS("lmxchar"), wxS("["))));
  }
}

SCENARIO("Receiving a value at all can unlock a feature's menu") {
  // Receiving debugmode proves this Maxima can drive the debugger, whatever
  // the value - the initially-disabled trigger submenu item becomes enabled.
  const MaximaMenuSyncRow *debugmode = nullptr;
  for (const MaximaMenuSyncRow &row : MaximaMenuSyncRows())
    if (row.m_variable == wxS("debugmode"))
      debugmode = &row;
  REQUIRE(debugmode != nullptr);
  REQUIRE(debugmode->m_enableId != wxID_NONE);

  GIVEN("the debugger-trigger item starts out disabled") {
    REQUIRE(!g_menubar->IsEnabled(debugmode->m_enableId));
    WHEN("any debugmode value arrives") {
      REQUIRE(SyncMenusToMaximaVariable(g_menubar, wxS("debugmode"),
                                        wxS("lisp")));
      THEN("the trigger is enabled and the value's item checked") {
        REQUIRE(g_menubar->IsEnabled(debugmode->m_enableId));
        REQUIRE(g_menubar->IsChecked(IdForValue(wxS("debugmode"),
                                                wxS("lisp"))));
      }
    }
  }
}

SCENARIO("Variables without a table row are reported as unhandled") {
  REQUIRE(!SyncMenusToMaximaVariable(g_menubar, wxS("no_such_variable"),
                                     wxS("true")));
  THEN("a menu-synced variable is handled even without a menu bar")
    REQUIRE(SyncMenusToMaximaVariable(nullptr, wxS("numer"), wxS("true")));
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_menubar = MenuBarFromTable();
  g_frame->SetMenuBar(g_menubar);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
