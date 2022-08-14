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

#include "SubstituteWiz.h"

SubstituteWiz::SubstituteWiz(wxWindow *parent, int id, Configuration *cfg,
                             const wxString &title, const wxPoint &pos,
                             const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  label_2 = new wxStaticText(this, -1, _("Expression:"));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxT("%"), wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, _("Old value:"));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxT("x"), wxDefaultPosition,
                              wxSize(230, -1));
  label_4 = new wxStaticText(this, -1, _("New value:"));
  text_ctrl_3 = new BTextCtrl(this, -1, cfg, wxT("y"), wxDefaultPosition,
                              wxSize(230, -1));
  checkbox_1 = new wxCheckBox(this, -1, _("&Rational"));
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

void SubstituteWiz::set_properties() {
  SetTitle(_("Substitute"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}

void SubstituteWiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(3, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(4, 2, 0, 0);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_3, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(20, 20, 0, 0);
  grid_sizer_2->Add(checkbox_1, 0, wxALL, 5);
  grid_sizer_2->AddGrowableCol(1);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  grid_sizer_1->AddGrowableCol(0);
  Layout();
}

wxString SubstituteWiz::GetValue() {
  wxString val;
  if (checkbox_1->IsChecked())
    val = wxT("ratsubst(");
  else
    val = wxT("subst(");
  val.Append(text_ctrl_3->GetValue());
  val.Append(wxT(", "));
  val.Append(text_ctrl_2->GetValue());
  val.Append(wxT(", "));
  val.Append(text_ctrl_1->GetValue());
  val.Append(wxT(");"));
  return val;
}
