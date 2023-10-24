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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef BTEXTCTRL_H
#define BTEXTCTRL_H

#include "precomp.h"
#include <wx/wx.h>
#include "Configuration.h"

/*! A wxTextCtrl with parenthesis matching

 */
class BTextCtrl : public wxTextCtrl
{
public:
    BTextCtrl(wxWindow *parent,
              wxWindowID id,
              Configuration *cfg,
              const wxString &value = wxEmptyString,
              const wxPoint &pos = wxDefaultPosition,
              const wxSize &size = wxDefaultSize,
              long style = 0);

    ~BTextCtrl();

    void SetSkipTab(bool skip)
        {
            m_skipTab = skip;
        }

private:
    bool m_skipTab;

    bool MatchParenthesis(int code);

    void CloseParenthesis(wxString open, wxString close, bool fromOpen);

    void OnChar(wxKeyEvent &event);
    void OnFocus(wxFocusEvent &event);

    Configuration *m_config;
};

#endif // BTEXTCTRL_H
