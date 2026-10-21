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
  This file declares the class TrayIcon, a system tray/notification-area icon
  mirroring wxMaxima's busy status and offering a small quick-access menu.
*/

#include "precomp.h"
#include <wx/wx.h>
#include <wx/taskbar.h>
#include "StatusBar.h"

#ifndef TRAYICON_H
#define TRAYICON_H

#if wxUSE_TASKBARICON

/*! A system tray icon that mirrors wxMaxima's busy status (GH #2286).

  The icon is only shown while something needs the user's attention that
  they haven't seen yet: a long calculation has finished, Maxima asks a
  question, Maxima has issued an error or entered the debugger, or an
  autosave failed -- each while the window wasn't active. That is exactly
  when Worksheet::SetNotification() raises a desktop notification, which is
  why the worksheet's notification state drives SetAttentionNeeded(). Once
  the window is activated again (or the user types or clicks into the
  worksheet) the icon goes away. A tray icon that is always there tells the
  user nothing they can't see in the window itself.

  Windows already gets an equivalent of this for free via the taskbar
  button's progress/overlay state (see StatusBar::UpdateStatusMaximaBusy()'s
  \#ifdef __WXMSW__ block) -- wxTaskBarIcon is the portable way to offer the
  same "can I tell at a glance whether Maxima is busy without the window
  being in focus" affordance on GTK/Linux (and, for free, on any other
  platform this runs on). On GTK specifically this only renders as a genuinely
  visible icon on desktops where the linked wxWidgets was itself built with
  AppIndicator/Ayatana support (wxUSE_APPINDICATOR) -- a build-time property
  of the wxWidgets package wxMaxima links against, not something this class
  can detect or needs to special-case: wxTaskBarIcon's own GTK backend
  transparently falls back to the older GtkStatusIcon/XEmbed mechanism
  wherever AppIndicator support isn't compiled in.

  Reuses StatusBar's own already-loaded icons (StatusBar::GetTrayIconBitmap())
  rather than embedding the same art a second time -- the generated headers
  under art/statusbar (one per icon) define their byte arrays without
  static/extern, so #include-ing one from a second .cpp file is a
  duplicate-symbol link error (confirmed by trying exactly that first).

  The popup menu's handlers are bound to the TrayIcon itself, never to the
  wxMenu: wxGTK's AppIndicator backend replaces the menu's event handler
  with the TrayIcon (wxMenu::SetEventHandler()), so anything bound to the
  menu is never reached there. Binding to the TrayIcon works on every
  backend, since the classic ones push it onto the hidden window they pop
  the menu up from.
*/
class TrayIcon : public wxTaskBarIcon {
public:
  /*! \param frame The window "Show wxMaxima" brings back, and the one the
      Interrupt and Exit items are forwarded to.
      \param statusBar Provides the icons, see StatusBar::GetTrayIconBitmap().
  */
  TrayIcon(wxFrame *frame, StatusBar *statusBar);
  ~TrayIcon() override;

  //! Refreshes the icon/tooltip for the current Maxima status. The caller
  //! (wxMaximaFrame::UpdateStatusMaximaBusy()) already only invokes this when
  //! the status genuinely changed, so no de-duplication is needed here.
  //! While no attention is needed the status is only remembered, for the
  //! moment the icon is shown.
  void UpdateStatus(StatusBar::MaximaStatus status);

  /*! Shows the icon while something needs the user's attention, removes it
      otherwise.

      Driven by the worksheet's notification state, see
      Worksheet::SetAttentionCallback().
  */
  void SetAttentionNeeded(bool needed);
  //! Is the icon meant to be shown right now?
  bool AttentionNeeded() const { return m_attentionNeeded; }

  wxMenu *CreatePopupMenu() override;

  //! The id of the popup menu's "Show wxMaxima" item
  static wxWindowID ShowMenuId();

private:
  static wxString TooltipFor(StatusBar::MaximaStatus status);

  void OnShow(wxCommandEvent &event);
  void OnInterrupt(wxCommandEvent &event);
  void OnExit(wxCommandEvent &event);
  void OnLeftDClick(wxTaskBarIconEvent &event);
  //! Shows, de-iconizes and raises the main window
  void ShowFrame();
  //! Puts the icon for m_status into the tray
  void ShowIcon();

  wxFrame *m_frame;
  StatusBar *m_statusBar;
  //! The status the icon shows once it is shown
  StatusBar::MaximaStatus m_status = StatusBar::MaximaStatus::wait_for_start;
  bool m_attentionNeeded = false;
};

#endif // wxUSE_TASKBARICON
#endif // TRAYICON_H
