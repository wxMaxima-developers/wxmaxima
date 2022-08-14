// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include "BC2Wiz.h"

BC2Wiz::BC2Wiz(wxWindow *parent, int id, Configuration *cfg,
               const wxString &title, const wxPoint &pos, const wxSize &size,
               long style)
    : wxDialog(parent, id, title, pos, size, style) {
  label_2 = new wxStaticText(this, -1, _("Solution:"));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, _("Point:"));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxT("x="), wxDefaultPosition,
                              wxSize(70, -1));
  label_4 = new wxStaticText(this, -1, _("Value:"));
  text_ctrl_3 = new BTextCtrl(this, -1, cfg, wxT("y="), wxDefaultPosition,
                              wxSize(70, -1));
  label_5 = new wxStaticText(this, -1, _("Point:"));
  text_ctrl_4 = new BTextCtrl(this, -1, cfg, wxT("x="), wxDefaultPosition,
                              wxSize(70, -1));
  label_6 = new wxStaticText(this, -1, _("Value:"));
  text_ctrl_5 = new BTextCtrl(this, -1, cfg, wxT("y="), wxDefaultPosition,
                              wxSize(70, -1));
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

void BC2Wiz::set_properties() {
  SetTitle(_("BC2"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}

void BC2Wiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(3, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_1->Add(text_ctrl_2, 0, wxALL, 5);
  sizer_1->Add(label_4, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_1->Add(text_ctrl_3, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_1, 0, wxALL, 0);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_2->Add(text_ctrl_4, 0, wxALL, 5);
  sizer_2->Add(label_6, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_2->Add(text_ctrl_5, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_2, 0, wxALL, 0);
  grid_sizer_1->Add(grid_sizer_2, 1, 0, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_3->Add(button_1, 0, wxALL, 5);
  sizer_3->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_3, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString BC2Wiz::GetValue() {
  wxString s;
  s += wxT("bc2(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  s += wxT(", ");
  s += text_ctrl_4->GetValue();
  s += wxT(", ");
  s += text_ctrl_5->GetValue();
  s += wxT(");");

  return s;
}
