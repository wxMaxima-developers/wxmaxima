﻿// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#include "Gen1Wiz.h"
#include <wx/persist/toplevel.h>

Gen1Wiz::Gen1Wiz(wxWindow *parent, int id, Configuration *cfg, const wxString &title,
                 const wxString &label1,
                 const wxString &val1,
                 const wxString &warning,
                 const wxString &warningToolTip,
                 const wxPoint &pos, const wxSize &size, long style) :
        wxDialog(parent, id, title, pos, size, style)
{
  SetName(title);
  label_2 = new wxStaticText(this, -1, label1);
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(300, -1));
  text_ctrl_1 -> SetValue(val1);
  static_line_1 = new wxStaticLine(this, -1);

#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  if(warning != wxEmptyString)    
  {
    m_warningText = warning;
    m_warning = new wxStaticText(this, -1, wxEmptyString);
    m_warning->SetToolTip(warningToolTip);
  }
  else
    m_warning = NULL;

  set_properties();
  do_layout();
  SetName(title);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}


void Gen1Wiz::set_properties()
{
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}


void Gen1Wiz::do_layout()
{
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(label_2, 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 5);
  grid_sizer_1->Add(text_ctrl_1, 0, wxALL | wxEXPAND, 5);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);

  if(m_warning != NULL)
    grid_sizer_1->Add(m_warning, 0, wxALL, 5);

  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString GetTextFromUser(wxString label, wxString title, Configuration *cfg, wxString value,
                         wxWindow *parent)
{
  Gen1Wiz *wiz = new Gen1Wiz(parent, -1, cfg, title, label);
  wiz->SetValue(value);
  wxString val;
  wiz->Centre(wxBOTH);
  if (wiz->ShowModal() == wxID_OK)
  {
    val = wiz->GetValue();
    val.Trim();
    val.Trim(false);
  }
  wiz->Destroy();
  return val;
}
