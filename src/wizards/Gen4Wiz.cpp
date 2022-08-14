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

#include "Gen4Wiz.h"
#include <wx/persist/toplevel.h>

Gen4Wiz::Gen4Wiz(wxString lab1, wxString lab2, wxString lab3, wxString lab4,
                 wxString val1, wxString val2, wxString val3, wxString val4,
                 Configuration *cfg, wxWindow *parent, int id,
                 const wxString &title, bool eq, const wxString &warning,
                 const wxString &warningToolTip, const wxPoint &pos,
                 const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  SetName(title);
  label_2 = new wxStaticText(this, -1, lab1);
  text_ctrl_1 =
    new BTextCtrl(this, -1, cfg, val1, wxDefaultPosition, wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, lab2);
  if (eq)
    text_ctrl_2 =
      new BTextCtrl(this, -1, cfg, val2, wxDefaultPosition, wxSize(230, -1));
  else
    text_ctrl_2 =
      new BTextCtrl(this, -1, cfg, val2, wxDefaultPosition, wxSize(110, -1));
  label_4 = new wxStaticText(this, -1, lab3);
  if (eq)
    text_ctrl_3 =
      new BTextCtrl(this, -1, cfg, val3, wxDefaultPosition, wxSize(230, -1));
  else
    text_ctrl_3 =
      new BTextCtrl(this, -1, cfg, val3, wxDefaultPosition, wxSize(110, -1));
  label_5 = new wxStaticText(this, -1, lab4);
  if (eq)
    text_ctrl_4 =
      new BTextCtrl(this, -1, cfg, val4, wxDefaultPosition, wxSize(230, -1));
  else
    text_ctrl_4 =
      new BTextCtrl(this, -1, cfg, val4, wxDefaultPosition, wxSize(110, -1));
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  if (warning != wxEmptyString) {
    m_warningText = warning;
    m_warning = new wxStaticText(this, -1, wxEmptyString);
    m_warning->SetToolTip(warningToolTip);
  } else
    m_warning = NULL;

  set_properties();
  SetName(title);
  wxPersistenceManager::Get().RegisterAndRestore(this);
  do_layout();
}

void Gen4Wiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(4, 2, 0, 0);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_3, 0, wxALL, 5);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_4, 0, wxALL, 5);
  if (m_warning != NULL)
    grid_sizer_1->Add(m_warning, 0, wxALL, 5);
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
  if (m_warning != NULL) {
    m_warning->SetLabel(m_warningText);
    m_warning->Wrap(GetClientSize().GetWidth());
    Fit();
    Layout();
    SetMinSize(GetSize());
  }
}

void Gen4Wiz::set_properties() {
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}
