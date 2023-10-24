// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef GEN2WIZ_H
#define GEN2WIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

// cppcheck-suppress ctuOneDefinitionRuleViolation
class Gen2Wiz : public wxDialog
{
public:
    Gen2Wiz(wxString lab1, wxString lab2,
            wxString val1, wxString val2,
            Configuration *cfg,
            wxWindow *parent, int id, const wxString &title,
            bool eq = false,
            const wxString &warning = wxEmptyString,
            const wxString &warningToolTip = wxEmptyString,
            const wxPoint &pos = wxDefaultPosition,
            const wxSize &size = wxDefaultSize,
            long style = wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN);

    wxString GetValue1()
        {
            return text_ctrl_1->GetValue();
        }

    wxString GetValue2()
        {
            return text_ctrl_2->GetValue();
        }

    void SetLabel1ToolTip(wxString toolTip){label_2->SetToolTip(toolTip);}
    void SetLabel2ToolTip(wxString toolTip){label_3->SetToolTip(toolTip);}

private:
    bool equal;

    void set_properties();

    void do_layout();

protected:
    wxStaticText *label_2;
    BTextCtrl *text_ctrl_1;
    wxStaticText *label_3;
    BTextCtrl *text_ctrl_2;
    wxStaticLine *static_line_1;
    wxButton *button_1;
    wxButton *button_2;
    wxStaticText *m_warning;
    wxString m_warningText;
};


#endif // GEN2WIZ_H
