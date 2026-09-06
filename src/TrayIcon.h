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

class wxMaximaFrame;

/*! A system tray icon that mirrors wxMaxima's busy status (GH #2286).

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
  rather than embedding the same art a second time -- the generated
  art/statusbar/*.h headers define their byte arrays without static/extern,
  so #include-ing one from a second .cpp file is a duplicate-symbol link
  error (confirmed by trying exactly that first).
*/
class TrayIcon : public wxTaskBarIcon {
public:
  TrayIcon(wxMaximaFrame *frame, StatusBar *statusBar);
  ~TrayIcon() override;

  //! Refreshes the icon/tooltip for the current Maxima status. The caller
  //! (wxMaximaFrame::UpdateStatusMaximaBusy()) already only invokes this when
  //! the status genuinely changed, so no de-duplication is needed here.
  void UpdateStatus(StatusBar::MaximaStatus status);

  wxMenu *CreatePopupMenu() override;

private:
  static wxString TooltipFor(StatusBar::MaximaStatus status);

  void OnShow(wxCommandEvent &event);
  void OnInterrupt(wxCommandEvent &event);
  void OnExit(wxCommandEvent &event);
  void OnLeftDClick(wxTaskBarIconEvent &event);

  wxMaximaFrame *m_frame;
  StatusBar *m_statusBar;
};

#endif // wxUSE_TASKBARICON
#endif // TRAYICON_H
