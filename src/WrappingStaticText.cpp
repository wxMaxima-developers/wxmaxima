// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file contains code to create a wxPanel containing image data.
 */

#include "WrappingStaticText.h"
#include <wx/wupdlock.h>

WrappingStaticText::WrappingStaticText(wxWindow* parent, int id, wxString text):
  wxPanel(parent, -1),
  m_text(text)
{
  wxWindowUpdateLocker SpeedUp(this);
  m_textCtrl = new wxStaticText(this, id, text);
  m_textCtrl->Wrap(GetContentScaleFactor()*600);
  wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
  sizer->Add(m_textCtrl,
    wxSizerFlags().
    Expand()
    );
  Connect(wxEVT_SIZE, wxSizeEventHandler(WrappingStaticText::OnSize), NULL, this);
  SetSizerAndFit(sizer);
}

void WrappingStaticText::OnSize(wxSizeEvent &event)
{
  wxWindowUpdateLocker SpeedUp(this);
  m_textCtrl->SetLabel(m_text);
  m_textCtrl->Wrap(event.GetSize().GetWidth());
  Layout();
//Fit();
//GetParent()->Fit();
  event.Skip();
}

