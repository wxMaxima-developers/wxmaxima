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

#include "MaximaNotStartingDialog.h"

MaximaNotStartingDialog::MaximaNotStartingDialog(wxWindow *parent, int id, Configuration *config,
                                                 wxString text,
                                                 const wxPoint &pos,
                                                 const wxSize &size, long style)
  : wxDialog(parent, id, text, pos, size,
             style),
    m_configuration(config) {
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer *nameSizer = new wxFlexGridSizer(9, 2, 0, 0);
  m_autodetectMaxima =
    new wxRadioButton(this, wxID_ANY, _("Autodetect"),
                      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_autodetectMaxima,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(new wxTextCtrl(
                                this, wxID_ANY,
                                m_configuration->MaximaDefaultLocation(), wxDefaultPosition,
                                wxSize(250 * GetContentScaleFactor(), -1), wxTE_RICH | wxTE_READONLY));

  m_noAutodetectMaxima =
    new wxRadioButton(this, wxID_ANY,
                      _("User specified"));
  nameSizer->Add(m_noAutodetectMaxima,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  m_maximaUserLocation = new BinaryNameCtrl(this, wxID_ANY);

  nameSizer->Add(m_maximaUserLocation,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  vsizer->Add(nameSizer, 1, wxEXPAND | wxLEFT, 0);

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_1->SetDefault();
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
  button_2->SetDefault();
#endif
  buttonSizer->Add(button_1, 0, wxALL, 5);
  buttonSizer->Add(button_2, 0, wxALL, 5);
  vsizer->Add(buttonSizer, 1, wxEXPAND | wxLEFT, 0);
  SetSizerAndFit(vsizer);
}
