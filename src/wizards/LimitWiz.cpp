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

#include "LimitWiz.h"

LimitWiz::LimitWiz(wxWindow *parent, int id, Configuration *cfg,
                   const wxString &title, const wxPoint &pos,
                   const wxSize &size, long style)
    : wxDialog(parent, id, title, pos, size, style) {
    label_2 = new wxStaticText(this, -1, _("Expression:"));
    text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                                wxSize(230, -1));
    label_3 = new wxStaticText(this, -1, _("Variable:"));
    text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxS("x"), wxDefaultPosition,
                                wxSize(110, -1));
    label_4 = new wxStaticText(this, -1, _("Point:"));
    text_ctrl_3 = new BTextCtrl(this, -1, cfg, wxS("0"), wxDefaultPosition,
                                wxSize(110, -1));
    button_1 = new wxButton(this, wxID_ANY, _("Special"));
    button_1->Connect(wxEVT_BUTTON, wxCommandEventHandler(LimitWiz::OnButton), NULL, this);

    label_5 = new wxStaticText(this, -1, _("Direction:"));
    const wxString choice_1_choices[] = {_("both sides"), _("left"), _("right")};
    choice_1 = new wxChoice(this, -1, wxDefaultPosition, wxSize(130, -1), 3,
                            choice_1_choices);
    checkbox_1 = new wxCheckBox(this, -1, _("&Taylor series"));
    static_line_1 = new wxStaticLine(this, -1);

#if defined __WXMSW__
    button_2 = new wxButton(this, wxID_OK, _("OK"));
    button_3 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
    button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
    button_3 = new wxButton(this, wxID_OK, _("OK"));
#endif

    button_2->SetDefault();
    Connect(wxEVT_IDLE, wxIdleEventHandler(LimitWiz::OnIdle), NULL, this);

    set_properties();
    do_layout();
}

void LimitWiz::set_properties() {
    SetTitle(_("Limit"));
    choice_1->SetSelection(0);
#if defined __WXMSW__
    button_2->SetDefault();
#else
    button_3->SetDefault();
#endif

    text_ctrl_1->SetFocus();
}

void LimitWiz::do_layout() {
    wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(3, 1, 0, 0);
    wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
    wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(5, 2, 0, 0);
    wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
    grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                      5);
    grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
    grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                      5);
    grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
    grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                      5);
    sizer_1->Add(text_ctrl_3, 0, wxALL | wxEXPAND, 5);
    sizer_1->Add(button_1, 0, wxALL, 5);
    grid_sizer_2->Add(sizer_1, 1, 0, 0);
    grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                      5);
    grid_sizer_2->Add(choice_1, 0, wxALL, 5);
    grid_sizer_2->Add(20, 20, 0, wxALL, 5);
    grid_sizer_2->Add(checkbox_1, 9, wxALL, 5);
    grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
    grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
    sizer_2->Add(button_2, 0, wxALL, 5);
    sizer_2->Add(button_3, 0, wxALL, 5);
    grid_sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT, 0);
    SetAutoLayout(true);
    SetSizer(grid_sizer_1);
    grid_sizer_1->Fit(this);
    grid_sizer_1->SetSizeHints(this);
    Layout();
}

wxString LimitWiz::GetValue() {
    wxString s;
    if (checkbox_1->GetValue())
        s = wxS("tlimit(");
    else
        s = wxS("limit(");
    s += text_ctrl_1->GetValue();
    s += wxS(", ");
    s += text_ctrl_2->GetValue();
    s += wxS(", ");
    s += text_ctrl_3->GetValue();
    if (choice_1->IsEnabled()) {
        int f = choice_1->GetSelection();
        if (f == 1)
            s += wxS(", minus");
        else if (f == 2)
            s += wxS(", plus");
    }
    s += wxS(");");

    return s;
}

void LimitWiz::OnButton(wxCommandEvent &WXUNUSED(event)) {
    wxString choices[] = {wxS("Pi"), wxS("E"), _("Infinity"), _("- Infinity")};
    wxString choice = wxGetSingleChoice(_("Select a constant"), _("Constant"), 4,
                                        choices, this);
    if (choice.Length()) {
        if (choice == wxS("Pi"))
            text_ctrl_3->SetValue(wxS("%pi"));
        else if (choice == wxS("E"))
            text_ctrl_3->SetValue(wxS("%e"));
        else if (choice == _("Infinity"))
            text_ctrl_3->SetValue(wxS("inf"));
        else if (choice == _("- Infinity"))
            text_ctrl_3->SetValue(wxS("minf"));
    }
}

void LimitWiz::OnIdle(wxIdleEvent &WXUNUSED(ev)) {
    wxString point = text_ctrl_3->GetValue();

    if (point == wxS("inf") || point == wxS("-inf") || point == wxS("+inf") ||
        point == wxS("minf") || point == wxS("-minf") || point == wxS("+minf")) {
        choice_1->Enable(false);
    } else if (choice_1->IsEnabled() == false) {
        choice_1->Enable(true);
    }
}
