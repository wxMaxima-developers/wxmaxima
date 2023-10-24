// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class Notification

  Notification is a notification message which the operating system
  claims to be non-disturbing.
*/

#ifndef NOTIFICATION_H
#define NOTIFICATION_H

#include "GroupCell.h"
#include "precomp.h"
#include <wx/notifmsg.h>
#include <wx/wx.h>

/*! A user notification the operating system claims to be non-disturbing

  We use this notification sparingly, which means: Only if the wxMaxima window has lost
  focus between the user action that caused maxima to run a command and the moment the
  command has finished or has caused an error.
*/
class Notification final: public wxNotificationMessage
{
public:
    //! A constructor that doesn't take any arguments
    Notification();
    //! A constructor that completely prepares the notification, but doesn't call Show() yet.
    explicit Notification(const wxString &title,
                          const wxString &message = {},
                          wxWindow *parent = NULL,
                          int flags = wxICON_INFORMATION);
    //! A destructor that also closes the notification
    ~Notification() { Notification::Close(); }
    //! Informs the notification which the main window is it notified for.
    void SetGroup(wxWindow *parent);
    //! Returns a pointer to the main window or NULL, if no main window is set.
    wxWindow *GetGroup() { return m_parent; }
    //! The cell we signal an error for
    GroupCell *m_errorNotificationCell;
private:
    //! The main window we notify for.
    wxWindow *m_parent;
protected:
    //! Called on clicking at the notification, if the OS supports that.
    void OnClick(wxCommandEvent &event);
};

#endif
