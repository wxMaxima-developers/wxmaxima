// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class CharacterButton

  This file contains the definition of the class CharacterButton that allows to 
  select arbitrary unicode symbols.
 */


#include "CharButton.h"
#include <wx/button.h>

void CharButton::ForwardToParent(wxMouseEvent &event)
{
  event.Skip();
  if(GetParent())
    GetParent()->GetEventHandler()->QueueEvent(new wxMouseEvent(event));
}

void CharButton::CharButtonPressed(wxMouseEvent &WXUNUSED(event))
{
  wxCommandEvent *ev = new wxCommandEvent(SIDEBARKEYEVENT, (long)(m_char));
  m_worksheet->GetEventHandler()->QueueEvent(ev);
}

CharButton::CharButton (wxPanel *parent, wxWindow *worksheet, wxChar ch, wxString description, bool WXUNUSED(matchesMaximaCommand)) : wxPanel(parent, wxID_ANY),
                                                                                                                 m_char(ch),
                                                                                                                 m_worksheet(worksheet)
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxStaticText *text = new wxStaticText(this, wxID_ANY, wxString(ch));
  vbox->Add(text, 1, wxALL | wxCENTER, 0);

  if (description.Length() > 0)
    text->SetToolTip(description);
  Connect(wxEVT_LEFT_UP, wxMouseEventHandler(CharButton::CharButtonPressed), NULL, this);
  text->Connect(wxEVT_LEFT_UP, wxMouseEventHandler(CharButton::CharButtonPressed), NULL, this);
  text->Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(CharButton::ForwardToParent), NULL, this);
  SetSizerAndFit(vbox);
  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(CharButton::ForwardToParent), NULL, this);
}
