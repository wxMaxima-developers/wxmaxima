// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C)      2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class FindReplaceDialog

  FindReplaceDialog is the find/replace dialog
*/

#include "FindReplaceDialog.h"
#include "EditorCell.h"

FindReplaceDialog::FindReplaceDialog(wxWindow *parent,
				     FindReplacePane::FindReplaceData *data,
                                     const wxString &title,
				     FindReplaceDialog **pointerToDialogue, int style)
  : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
	     style) {
  m_pointerToDialogue = pointerToDialogue;
  if(m_pointerToDialogue != NULL)
    *m_pointerToDialogue = this;
  m_contents = new FindReplacePane(this, data);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(m_contents, wxSizerFlags().Expand());
  SetSizerAndFit(vbox);
  m_activateDuringConstruction = true;
  // Remember how wide the user wanted the dialogue to be the last time it was
  // used.
  if (m_windowPos != wxPoint(-1, -1))
    SetPosition(m_windowPos);
  if (m_windowSize.x > 0)
    SetSize(wxSize(m_windowSize.x, GetSize().y));
  Connect(wxEVT_ACTIVATE, wxActivateEventHandler(FindReplaceDialog::OnActivate),
          NULL, this);
  Connect(wxEVT_CHAR_HOOK, wxKeyEventHandler(FindReplaceDialog::OnKeyDown),
          NULL, this);
}

FindReplaceDialog::~FindReplaceDialog()
{
  if(m_pointerToDialogue)
    {
      *m_pointerToDialogue = NULL;
    }
}

void FindReplaceDialog::OnKeyDown(wxKeyEvent &event) {
  if (event.GetKeyCode() == WXK_ESCAPE)
    Close();
  else
    event.Skip();
}

void FindReplaceDialog::OnActivate(wxActivateEvent &event) {
  event.Skip();
  if(m_activateDuringConstruction)
    {
      m_activateDuringConstruction = false;
      return;
    }
  if (event.GetActive())
    SetTransparent(255);
  else
    SetTransparent(180);
}

wxSize FindReplaceDialog::m_windowSize = wxSize(-1, -1);
wxPoint FindReplaceDialog::m_windowPos = wxPoint(-1, -1);
