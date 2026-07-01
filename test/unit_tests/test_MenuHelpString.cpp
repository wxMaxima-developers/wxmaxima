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
  Regression test for MenuHelpString() (the status-bar menu-help lookup).

  wxMaxima shows a menu item's help string in the status bar while the item is
  highlighted. The old code called wxMenu::GetHelpString(), which in wxWidgets
  3.3 asserts ("item" failed) when the highlighted id is not an item of that
  menu -- a case the native menu code produces mostly on Windows (menu/submenu
  titles, ids belonging to another menu). That platform never reaches this test
  machine, so we reproduce the exact bad condition here deterministically: a
  throwing wxWidgets assert handler turns the assert into a catchable failure,
  and we look up an id that is not in the menu.

  With the buggy GetHelpString() lookup this test fails on ANY platform whose
  wxWidgets has assertions enabled (both the Windows and the Linux CI); with the
  MenuHelpString() fix it passes.
*/

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>

#include "MenuHelpString.h"

#include <cstdlib>
#include <stdexcept>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

// Builds a menu with a normal item and a submenu item; both carry help text.
// Returned by value; the submenu is owned by the menu.
static wxMenu *MakeMenu() {
  auto *menu = new wxMenu();
  menu->Append(1001, wxS("Copy"), wxS("Copies the selection to the clipboard"));
  auto *sub = new wxMenu();
  sub->Append(1002, wxS("Deeper"), wxS("A help string from inside a submenu"));
  menu->AppendSubMenu(sub, wxS("Submenu"));
  return menu;
}

SCENARIO("MenuHelpString returns the help of a highlighted item") {
  wxMenu *menu = MakeMenu();

  THEN("a top-level item's help string is returned") {
    REQUIRE(MenuHelpString(menu, 1001) ==
            wxS("Copies the selection to the clipboard"));
  }
  THEN("a submenu item's help string is found by recursion") {
    REQUIRE(MenuHelpString(menu, 1002) ==
            wxS("A help string from inside a submenu"));
  }
  delete menu;
}

SCENARIO("MenuHelpString never asserts on an id that is not in the menu") {
  // This is the actual regression: wxMenu::GetHelpString() asserts here, while
  // MenuHelpString() must quietly return an empty string.
  wxMenu *menu = MakeMenu();

  WHEN("an id that no item carries is looked up") {
    THEN("it does not assert/throw and yields an empty string") {
      wxString help;
      REQUIRE_NOTHROW(help = MenuHelpString(menu, 987654));
      REQUIRE(help.IsEmpty());
    }
  }
  WHEN("the menu pointer is null or the id is not a real item id") {
    THEN("an empty string comes back without asserting") {
      wxString h1, h2, h3;
      REQUIRE_NOTHROW(h1 = MenuHelpString(nullptr, 1001));
      REQUIRE_NOTHROW(h2 = MenuHelpString(menu, 0));
      REQUIRE_NOTHROW(h3 = MenuHelpString(menu, -1));
      REQUIRE(h1.IsEmpty());
      REQUIRE(h2.IsEmpty());
      REQUIRE(h3.IsEmpty());
    }
  }
  delete menu;
}

// Turns any wxWidgets assertion into a C++ exception so REQUIRE_NOTHROW above
// can observe a GetHelpString()-style assert as a test failure instead of a
// (headless) modal dialog / abort.
static void ThrowingAssertHandler(const wxString &file, int line,
                                  const wxString &func, const wxString &cond,
                                  const wxString &msg) {
  throw std::runtime_error(
    (file + wxS(":") + wxString::Format(wxS("%d"), line) + wxS(" in ") + func +
     wxS(": ") + cond + wxS(" ") + msg)
      .ToStdString());
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

// wxGTK routes menu/GUI object creation through GTK, which needs an X display.
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

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  wxSetAssertHandler(ThrowingAssertHandler);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
