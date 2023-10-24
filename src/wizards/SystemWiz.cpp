// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include "SystemWiz.h"

SysWiz::SysWiz(wxWindow *parent, int id, Configuration *cfg,
               const wxString &title, int numEq, const wxPoint &pos,
               const wxSize &sz, long style)
    : wxDialog(parent, id, title, pos, sz, style) {
    m_size = numEq;
    for (int i = 0; i < m_size; i++) {
        m_inputs.push_back(new BTextCtrl(this, -1, cfg, wxS("0"), wxDefaultPosition,
                                         wxSize(230, -1)));
    }
    variables = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
    static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
    button_1 = new wxButton(this, wxID_OK, _("OK"));
    button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
    button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
    button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

    set_properties();
    do_layout();
}

void SysWiz::set_properties() {
    variables->SetToolTip(_("Enter comma separated list of variables."));
#if defined __WXMSW__
    button_1->SetDefault();
#else
    button_2->SetDefault();
#endif

    m_inputs[0]->SetFocus();
    m_inputs[0]->SetSelection(-1, -1);
}

void SysWiz::do_layout() {
    wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
    wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(m_size + 1, 2, 0, 0);
    wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
    wxStaticText *text;
    for (long i = 1; i <= m_size; i++) {
        text = new wxStaticText(this, -1, wxString::Format(_("Equation %d:"), i));
        grid_sizer_2->Add(text, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                          5);
        grid_sizer_2->Add(m_inputs[static_cast<size_t>(i) - 1], 0, wxALL, 5);
    }
    text = new wxStaticText(this, -1, _("Variables:"));
    grid_sizer_2->Add(text, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                      5);
    grid_sizer_2->Add(variables, 0, wxALL, 5);
    grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
    grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
    sizer_1->Add(button_1, 0, wxALL, 5);
    sizer_1->Add(button_2, 0, wxALL, 5);
    grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
    SetAutoLayout(true);
    SetSizer(grid_sizer_1);
    grid_sizer_1->Fit(this);
    grid_sizer_1->SetSizeHints(this);
    Layout();
}

wxString SysWiz::GetValue() {
    wxString cmd = wxS("([");
    for (int i = 0; i < m_size; i++) {
        cmd += m_inputs[i]->GetValue();
        if (i < m_size - 1)
            cmd += wxS(", ");
    }
    cmd += wxS("], [") + variables->GetValue() + wxS("]);");
    return cmd;
}
