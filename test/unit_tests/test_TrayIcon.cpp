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
  Regression test: the tray icon's menu items must work with an AppIndicator.

  With a StatusNotifier/AppIndicator tray, wxGTK (3.3) builds the indicator's
  menu once from CreatePopupMenu() and then calls
  menu->SetEventHandler(taskBarIcon), which replaces the menu as the first
  handler its events go to. Handlers bound to the menu itself were therefore
  never called, and "Show wxMaxima" (as well as Interrupt and Exit) silently
  did nothing. The classic GtkStatusIcon/XEmbed tray, and the other ports,
  pop the menu up from a hidden window the icon is pushed onto instead, which
  is why the bug didn't show there.

  No tray needs to exist for this: wxMenu::SendEvent() is what every port
  calls for a clicked menu item, so it is called directly, with exactly the
  handler arrangement each kind of tray sets up.
*/

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>

#include "EventIDs.h"
#include "StatusBar.h"
#include "TrayIcon.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#if wxUSE_TASKBARICON
namespace {
//! Stands in for the main window and counts the menu commands it receives
class CountingFrame : public wxFrame {
public:
  CountingFrame() : wxFrame(nullptr, wxID_ANY, wxS("tray icon test")) {
    Bind(wxEVT_MENU, [this](wxCommandEvent &) { ++m_interrupts; },
         EventIDs::menu_interrupt_id);
    Bind(wxEVT_MENU, [this](wxCommandEvent &) { ++m_exits; }, wxID_EXIT);
  }
  int m_interrupts = 0;
  int m_exits = 0;
};

//! Delivers a click on the menu item \p id the way an AppIndicator tray does
bool ClickLikeAppIndicator(TrayIcon &icon, wxMenu &menu, int id) {
  menu.SetEventHandler(&icon);
  return menu.SendEvent(id);
}

//! Delivers a click on the menu item \p id the way a classic tray does:
//! the menu is popped up from a hidden window the icon is pushed onto.
bool ClickLikeStatusIcon(TrayIcon &icon, wxMenu &menu, int id) {
  wxFrame popupWindow(nullptr, wxID_ANY, wxEmptyString);
  popupWindow.PushEventHandler(&icon);
  menu.SetInvokingWindow(&popupWindow);
  const bool handled = menu.SendEvent(id);
  menu.SetInvokingWindow(nullptr);
  popupWindow.PopEventHandler();
  return handled;
}
} // namespace

SCENARIO("The tray icon's menu items reach their handlers") {
  auto *frame = new CountingFrame;
  auto *statusBar = new StatusBar(frame, wxID_ANY);
  frame->SetStatusBar(statusBar);
  {
    TrayIcon icon(frame, statusBar);
    for (const bool appIndicator : {true, false}) {
      const auto click = appIndicator ? ClickLikeAppIndicator
        : ClickLikeStatusIcon;
      const std::string tray =
        appIndicator ? "an AppIndicator tray" : "a classic tray";
      GIVEN(tray) {
        std::unique_ptr<wxMenu> menu(icon.CreatePopupMenu());
        REQUIRE(menu->FindItem(TrayIcon::ShowMenuId()) != nullptr);

        WHEN("Show wxMaxima is clicked while the window is hidden") {
          frame->Hide();
          REQUIRE_FALSE(frame->IsShown());
          CHECK(click(icon, *menu, TrayIcon::ShowMenuId()));
          THEN("the window is shown again") { CHECK(frame->IsShown()); }
        }
        WHEN("Interrupt and Exit are clicked") {
          CHECK(click(icon, *menu, EventIDs::menu_interrupt_id));
          CHECK(click(icon, *menu, wxID_EXIT));
          wxTheApp->ProcessPendingEvents();
          THEN("both are forwarded to the main window") {
            CHECK(frame->m_interrupts == 1);
            CHECK(frame->m_exits == 1);
          }
        }
      }
    }
  }
  frame->Destroy();
  wxTheApp->ProcessPendingEvents();
}
#endif // wxUSE_TASKBARICON

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
