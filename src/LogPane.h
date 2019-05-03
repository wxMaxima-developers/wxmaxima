// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef LOGPANE_H
#define LOGPANE_H

#include <wx/wx.h>
#include <wx/panel.h>
#include <wx/textctrl.h>
#include "ErrorRedirector.h"

/*! A "debug messages" sidepane

*/
class LogPane : public wxPanel
{
public:
  LogPane(wxWindow *parent, wxWindowID id = wxID_ANY, bool becomeLogTarget = true);
  void BecomeLogTarget();
  void DropLogTarget();
  ~LogPane();

private:
  //! The textctrl all log messages appear on
  wxTextCtrl *m_textCtrl;
  //! Redirects all error messages to gui dialogues
  ErrorRedirector *m_errorRedirector;
  wxLog *m_logPanelTarget;
  bool m_isLogTarget;
};

#endif // LOGPANE_H
