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

#include "ResolutionChooser.h"

ResolutionChooser::ResolutionChooser(wxWindow *parent, int id,
          const int &resolution,
          const wxPoint &pos,
          const wxSize &size, long style) :
  wxDialog(parent, id, _("Set maximum image size [in mm]"), pos, size, style)
{
  wxStaticText *resolutionText = new wxStaticText(this, -1, wxT("Image resolution [in ppi]:"));

  m_resolution = new wxSpinCtrl(this, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1),wxSP_VERTICAL,
                           72, 9600);
  m_resolution->SetValue(resolution);

#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_1 -> SetDefault();
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
  button_2 -> SetDefault();
#endif

  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 2, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  vsizer->Add(new wxStaticText(this, -1, _("The maximum size for this image. Values <= 0 mean: Unspecified.")), 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 5);
  grid_sizer_1->Add(resolutionText, 0, wxALL | wxEXPAND, 5);
  grid_sizer_1->Add(m_resolution, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  vsizer->Add(grid_sizer_1, 1, wxEXPAND | wxLEFT, 0);
  SetSizerAndFit(vsizer);
}

