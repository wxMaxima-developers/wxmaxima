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
#include <wx/sizer.h>

WrappingStaticText::WrappingStaticText(wxWindow* parent, int id, wxString text):
  wxPanel(parent, -1),
  m_label(text)
{
  wxWindowUpdateLocker SpeedUp(this);
  m_textCtrl = new wxStaticText(this, id, text);
  m_textCtrl->Wrap(GetContentScaleFactor()*50);
//  m_textCtrl->SetMinSize(wxSize(50,10));
  wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
  sizer->Add(m_textCtrl,
             wxSizerFlags().
             Expand()
    );
  Connect(wxEVT_SIZE, wxSizeEventHandler(WrappingStaticText::OnSize), NULL, this);
  SetSizer(sizer);
  FitInside();
}

void WrappingStaticText::SetLabel(wxString const &value)
{
  m_textCtrl->SetLabel(m_label = value);
//  m_textCtrl->Wrap(GetSize().GetWidth());
}

void WrappingStaticText::OnSize(wxSizeEvent &event)
{
  m_textCtrl->SetLabel(m_label);
  m_textCtrl->Wrap(wxMax(event.GetSize().GetWidth(), 50*GetContentScaleFactor()));
  event.Skip();
}

