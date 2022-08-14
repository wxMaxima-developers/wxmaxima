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

FindReplaceDialog::FindReplaceDialog(wxWindow *parent, wxFindReplaceData *data,
                                     const wxString &title, int style)
    : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
               style) {
  m_contents = new FindReplacePane(this, data);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(m_contents, wxSizerFlags().Expand());
  SetSizerAndFit(vbox);

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
  Connect(wxEVT_CLOSE_WINDOW, wxCloseEventHandler(FindReplaceDialog::OnClose),
          NULL, this);
}

void FindReplaceDialog::OnKeyDown(wxKeyEvent &event) {
  if (event.GetKeyCode() == WXK_ESCAPE)
    Close();
  else
    event.Skip();
}

void FindReplaceDialog::OnClose(wxCloseEvent &WXUNUSED(event)) {
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_CLOSE);
  GetParent()->GetEventHandler()->QueueEvent(findEvent);
  m_windowSize = GetSize();
  m_windowPos = GetPosition();
}

void FindReplaceDialog::OnActivate(wxActivateEvent &event) {
  if (event.GetActive())
    SetTransparent(255);
  else
    SetTransparent(180);
}

wxSize FindReplaceDialog::m_windowSize = wxSize(-1, -1);
wxPoint FindReplaceDialog::m_windowPos = wxPoint(-1, -1);
