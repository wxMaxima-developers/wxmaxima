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

#include "SeriesWiz.h"

SeriesWiz::SeriesWiz(wxWindow *parent, int id, Configuration *cfg,
                     const wxString &title, const wxPoint &pos,
                     const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  
  label_2 = new wxStaticText(this, wxID_ANY, _("Expression:"));
  text_ctrl_1 = new BTextCtrl(this, wxID_ANY, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, wxID_ANY));
  label_3 = new wxStaticText(this, wxID_ANY, _("Variable:"));
  text_ctrl_2 = new BTextCtrl(this, wxID_ANY, cfg, wxT("x"), wxDefaultPosition,
                              wxSize(110, -1));
  label_4 = new wxStaticText(this, wxID_ANY, _("Point:"));
  text_ctrl_3 = new BTextCtrl(this, wxID_ANY, cfg, wxT("0"), wxDefaultPosition,
                              wxSize(110, -1));
  button_3 = new wxButton(this, wxID_ANY, _("Special"));
  button_3->Connect(wxEVT_BUTTON, wxCommandEventHandler(SeriesWiz::OnButton), NULL, this);

  label_5 = new wxStaticText(this, wxID_ANY, _("Depth:"));
  spin_ctrl_1 = new wxSpinCtrl(this, wxID_ANY, wxT("8"), wxDefaultPosition,
                               wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 8);
  checkbox_1 = new wxCheckBox(this, wxID_ANY, _("&Power series"));
  checkbox_1->Connect(wxEVT_CHECKBOX, wxCommandEventHandler(SeriesWiz::OnCheckbox), NULL, this);
  static_line_1 = new wxStaticLine(this, wxID_ANY);
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

void SeriesWiz::set_properties() {
  SetTitle(_("Series"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}

void SeriesWiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(5, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_2->Add(text_ctrl_3, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_2->Add(button_3, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(spin_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(20, 20, 0, 0, 0);
  grid_sizer_2->Add(checkbox_1, 0, 0, 0);
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

void SeriesWiz::OnButton(wxCommandEvent &WXUNUSED(event)) {
  wxString choices[] = {wxT("Pi"), wxT("E")};
  wxString choice = wxGetSingleChoice(_("Select a constant"), _("Constant"), 2,
                                      choices, this);
  if (choice.Length()) {
    if (choice == wxT("Pi"))
      text_ctrl_3->SetValue(wxT("%pi"));
    else if (choice == wxT("E"))
      text_ctrl_3->SetValue(wxT("%e"));
  }
}

wxString SeriesWiz::GetValue() {
  wxString s;
  if (checkbox_1->IsChecked())
    s = wxT("niceindices(powerseries(");
  else
    s = wxT("taylor(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  if (!checkbox_1->IsChecked()) {
    s += wxT(", ");
    s += wxString::Format(wxT("%d"), spin_ctrl_1->GetValue());
    s += wxT(");");
  } else
    s += wxT("));");

  return s;
}

void SeriesWiz::OnCheckbox(wxCommandEvent &WXUNUSED(event)) {
  spin_ctrl_1->Enable(!checkbox_1->GetValue());
}

