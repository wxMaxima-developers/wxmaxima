// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//                2011-2011 cw.ahbong <cw.ahbong@gmail.com>
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

#include "PlotFormatWiz.h"

PlotFormatWiz::PlotFormatWiz(wxWindow *parent, int id,
                             Configuration *WXUNUSED(cfg),
                             const wxString &title, const wxPoint &pos,
                             const wxSize &size, long style)
    : wxDialog(parent, id, title, pos, size, style) {
  label_1 = new wxStaticText(this, -1, _("Choose new plot format:"));
  const wxString combo_box_1_choices[] = {
    wxT("gnuplot"),
#if !defined(__WXMSW__)
    /* gnuplot_pipes is not available on Windows, geomview requires Motif, which
       is not available on Windows */
    wxT("gnuplot_pipes"),
    wxT("geomview"),
#endif
    wxT("xmaxima"),
    wxT("mgnuplot")
  };
  combo_box_1 = new wxComboBox(
      this, -1, combo_box_1_choices[0], wxDefaultPosition, wxSize(140, -1),
      sizeof(combo_box_1_choices) / sizeof(combo_box_1_choices[0]),
      combo_box_1_choices, wxCB_DROPDOWN);
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

void PlotFormatWiz::set_properties() {
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif
}

void PlotFormatWiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(label_1, 0, wxALIGN_CENTER | wxALL, 5);
  grid_sizer_1->Add(combo_box_1, 0, wxALIGN_CENTER | wxALL, 5);
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

wxString PlotFormatWiz::GetValue() {
  wxString s;
  s = wxT("set_plot_option(['plot_format, '");
  s += combo_box_1->GetValue();
  s += wxT("])$");
  return s;
}
