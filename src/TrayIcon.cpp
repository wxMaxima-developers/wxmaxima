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
  This file defines the class TrayIcon.
*/

#include "TrayIcon.h"

#if wxUSE_TASKBARICON

#include "EventIDs.h"

namespace {
//! Only ever appended to and read from this file's own popup menu -- a
//! plain wxWindow::NewControlId() (rather than a standard wxID_* or an
//! EventIDs.h entry) is enough since nothing outside TrayIcon needs to
//! recognize it.
const wxWindowIDRef menu_show_id(wxWindow::NewControlId());
} // namespace

TrayIcon::TrayIcon(wxFrame *frame, StatusBar *statusBar)
  : m_frame(frame), m_statusBar(statusBar) {
  Bind(wxEVT_TASKBAR_LEFT_DCLICK, &TrayIcon::OnLeftDClick, this);
  // The menu items' handlers live here rather than on the wxMenu
  // CreatePopupMenu() returns: with an AppIndicator (StatusNotifier) tray,
  // wxGTK hands the menu to the indicator once and redirects it to this
  // object via wxMenu::SetEventHandler(), which takes the menu's own bindings
  // out of the event's path. The other backends pop the menu up from a
  // hidden window this object is pushed onto, so they reach it here, too.
  Bind(wxEVT_MENU, &TrayIcon::OnShow, this, menu_show_id);
  Bind(wxEVT_MENU, &TrayIcon::OnInterrupt, this, EventIDs::menu_interrupt_id);
  Bind(wxEVT_MENU, &TrayIcon::OnExit, this, wxID_EXIT);
  // No icon yet: it only appears once something needs attention.
}

TrayIcon::~TrayIcon() { RemoveIcon(); }

wxString TrayIcon::TooltipFor(StatusBar::MaximaStatus status) {
  // Same wording StatusBar::UpdateStatusMaximaBusy() uses for its own
  // tooltip, kept in sync by hand since the two live in different classes
  // with no shared string table to draw from.
  switch (status) {
  case StatusBar::MaximaStatus::wait_for_start:
    return _("Maxima started. Waiting for connection...");
  case StatusBar::MaximaStatus::process_wont_start:
    return _("Cannot start the maxima binary");
  case StatusBar::MaximaStatus::sending:
    return _("Sending a command to Maxima");
  case StatusBar::MaximaStatus::waiting:
    return _("Ready for user input");
  case StatusBar::MaximaStatus::waitingForPrompt:
    return _("Maxima started. Waiting for initial prompt...");
  case StatusBar::MaximaStatus::waitingForAuth:
    return _("Maxima started. Waiting for authentication...");
  case StatusBar::MaximaStatus::calculating:
    return _("Maxima is calculating");
  case StatusBar::MaximaStatus::parsing:
    return _("Parsing output");
  case StatusBar::MaximaStatus::transferring:
    return _("Reading Maxima output");
  case StatusBar::MaximaStatus::userinput:
    return _("Maxima asks a question");
  case StatusBar::MaximaStatus::maximaerror:
    return _("Maxima returned an error message");
  case StatusBar::MaximaStatus::debugging:
    // Reuses StatusBar's own (longer, multi-line) wording rather than a
    // shorter tray-only paraphrase, so this doesn't add a second,
    // near-duplicate string for translators on top of StatusBar's.
    return _("Maxima has stopped in a debugger and is waiting for a command.\n"
             "Common commands (type ':h' for the full list):\n"
             "  :bt        show a backtrace of the call stack\n"
             "  :continue  continue the computation\n"
             "  :top       return to the top level (abandon the computation)\n"
             "  :next / :step   step to the next line\n"
             "  :frame     show the current stack frame");
  case StatusBar::MaximaStatus::lispmode:
    return _("Maxima's reader is in Lisp mode (after to_lisp()).\n"
             "Type Lisp forms; enter (to-maxima) to return to Maxima mode.");
  case StatusBar::MaximaStatus::disconnected:
    return _("Not connected to Maxima");
  default:
    return _("wxMaxima");
  }
}

void TrayIcon::UpdateStatus(StatusBar::MaximaStatus status) {
  m_status = status;
  if (m_attentionNeeded)
    ShowIcon();
}

void TrayIcon::SetAttentionNeeded(bool needed) {
  if (needed == m_attentionNeeded)
    return;
  m_attentionNeeded = needed;
  if (needed)
    ShowIcon();
  else
    RemoveIcon();
}

void TrayIcon::ShowIcon() {
  wxIcon icon;
  icon.CopyFromBitmap(m_statusBar->GetTrayIconBitmap(m_status));
  SetIcon(icon, wxS("wxMaxima: ") + TooltipFor(m_status));
}

wxMenu *TrayIcon::CreatePopupMenu() {
  // wxTaskBarIcon takes ownership of and destroys the menu this returns
  // once the popup closes, so a fresh one is built on every right-click
  // rather than caching one as a member.
  wxMenu *menu = new wxMenu;
  // Reuses the exact label text the Maxima/File menus already use for these
  // two items (see wxMaximaFrame.cpp) rather than a tray-only rewording, so
  // no new strings are added for translators beyond "&Show wxMaxima".
  menu->Append(EventIDs::menu_interrupt_id, _("&Interrupt\tCtrl+G"));
  menu->AppendSeparator();
  menu->Append(menu_show_id, _("&Show wxMaxima"));
  menu->Append(wxID_EXIT, _("E&xit\tCtrl+Q"));
  return menu;
}

wxWindowID TrayIcon::ShowMenuId() { return menu_show_id; }

void TrayIcon::ShowFrame() {
  m_frame->Show();
  m_frame->Iconize(false);
  m_frame->Raise();
  m_frame->SetFocus();
}

void TrayIcon::OnShow(wxCommandEvent &WXUNUSED(event)) { ShowFrame(); }

void TrayIcon::OnInterrupt(wxCommandEvent &WXUNUSED(event)) {
  // wxTaskBarIcon's own popup menu delivers wxEVT_MENU to this class, not to
  // the main frame -- re-post it there so the exact same handler the
  // Interrupt menu item/toolbar button use (MaximaProcessManager::Interrupt)
  // runs, instead of duplicating what it does.
  wxCommandEvent interrupt(wxEVT_MENU, EventIDs::menu_interrupt_id);
  m_frame->GetEventHandler()->AddPendingEvent(interrupt);
}

void TrayIcon::OnExit(wxCommandEvent &WXUNUSED(event)) {
  // Same reasoning as OnInterrupt(): reuse MaximaCommandMenus::FileMenu's
  // wxID_EXIT handling (which itself defers to wxMaxima::Close(), the path
  // that already knows how to prompt for unsaved changes) instead of
  // reimplementing "quit" here.
  wxCommandEvent exitEvent(wxEVT_MENU, wxID_EXIT);
  m_frame->GetEventHandler()->AddPendingEvent(exitEvent);
}

void TrayIcon::OnLeftDClick(wxTaskBarIconEvent &WXUNUSED(event)) {
  ShowFrame();
}

#endif // wxUSE_TASKBARICON
