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
  This file defines the class FindReplacePane

  FindReplacePane is the contents of the find/replace dialog/sidebar
*/

#include "FindReplacePane.h"
#include "EditorCell.h"
#include <wx/button.h>
#include <wx/stattext.h>

FindReplacePane::FindReplacePane(wxWindow *parent, wxFindReplaceData *data)
  : wxPanel(parent, -1) {
  m_active = true;
  m_findReplaceData = data;
  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(3, 1, 1);
  grid_sizer->SetFlexibleDirection(wxHORIZONTAL);
  grid_sizer->AddGrowableCol(0, 0);
  grid_sizer->AddGrowableCol(1, 1);
  grid_sizer->AddGrowableCol(2, 0);

  grid_sizer->Add(new wxStaticText(this, -1, _("Find:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));

  m_searchText = new wxTextCtrl(this, -1, data->GetFindString());
  m_searchText->Connect(
			wxEVT_TEXT, wxCommandEventHandler(FindReplacePane::OnFindStringChange),
			NULL, this);
  grid_sizer->Add(m_searchText, wxSizerFlags().Expand().Border(wxALL, 5));

  m_searchButton = new wxButton(this, wxID_FIND);
  grid_sizer->Add(m_searchButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_searchButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(FindReplacePane::OnSearch),
                          NULL, this);

  grid_sizer->Add(new wxStaticText(this, -1, _("Replacement:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));

  m_replaceText = new wxTextCtrl(this, -1, data->GetReplaceString());
  m_replaceText->Connect(
			 wxEVT_TEXT, wxCommandEventHandler(FindReplacePane::OnReplaceStringChange),
			 NULL, this);
  grid_sizer->Add(m_replaceText, wxSizerFlags().Expand().Border(wxALL, 5));

  m_replaceButton = new wxButton(this, wxID_REPLACE);
  m_replaceButton->Connect(wxEVT_BUTTON,
                           wxCommandEventHandler(FindReplacePane::OnReplace),
                           NULL, this);
  grid_sizer->Add(m_replaceButton, wxSizerFlags().Expand().Border(wxALL, 5));

  grid_sizer->Add(new wxStaticText(this, -1, _("Direction:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));

  wxBoxSizer *fbbox = new wxBoxSizer(wxHORIZONTAL);
  m_forward = new wxRadioButton(this, -1, _("Up"), wxDefaultPosition,
                                wxDefaultSize, wxRB_GROUP);
  fbbox->Add(m_forward, wxSizerFlags().Expand().Border(wxALL, 5));
  m_backwards = new wxRadioButton(this, -1, _("Down"));
  fbbox->Add(m_backwards, wxSizerFlags().Expand().Border(wxALL, 5));

  m_forward->SetValue(!(data->GetFlags() & wxFR_DOWN));
  m_backwards->SetValue(!!(data->GetFlags() & wxFR_DOWN));
  m_forward->Connect(wxEVT_RADIOBUTTON,
                     wxCommandEventHandler(FindReplacePane::OnDirectionChange),
                     NULL, this);
  m_backwards->Connect(
		       wxEVT_RADIOBUTTON,
		       wxCommandEventHandler(FindReplacePane::OnDirectionChange), NULL, this);

  grid_sizer->Add(fbbox, wxSizerFlags().Expand());

  m_replaceAllButton = new wxButton(this, 1, _("Replace All"));
  grid_sizer->Add(m_replaceAllButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_replaceAllButton->Connect(
			      wxEVT_BUTTON, wxCommandEventHandler(FindReplacePane::OnReplaceAll), NULL,
			      this);

  grid_sizer->AddSpacer(0);

  m_matchCase = new wxCheckBox(this, -1, _("Match Case"));
  m_matchCase->SetValue(!!(data->GetFlags() & wxFR_MATCHCASE));
  grid_sizer->Add(m_matchCase, wxSizerFlags().Expand().Border(wxALL, 5));
  m_matchCase->Connect(wxEVT_CHECKBOX,
                       wxCommandEventHandler(FindReplacePane::OnMatchCase),
                       NULL, this);

  // If I press <tab> in the search text box I want to arrive in the
  // replacement text box immediately.
  m_replaceText->MoveAfterInTabOrder(m_searchText);
  this->SetSizerAndFit(grid_sizer);
  Connect(wxEVT_ACTIVATE, wxActivateEventHandler(FindReplacePane::OnActivate),
          NULL, this);
  Connect(wxEVT_CHAR_HOOK, wxKeyEventHandler(FindReplacePane::OnKeyDown), NULL,
          this);
}

void FindReplacePane::SetFindString(wxString strng) {
  m_findReplaceData->SetFindString(strng);
  m_searchText->SetValue(strng);
}

void FindReplacePane::OnSearch(wxCommandEvent &WXUNUSED(event)) {
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_NEXT);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  GetParent()->GetParent()->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplace(wxCommandEvent &WXUNUSED(event)) {
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  GetParent()->GetParent()->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplaceAll(wxCommandEvent &WXUNUSED(event)) {
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE_ALL);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  GetParent()->GetParent()->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnDirectionChange(wxCommandEvent &WXUNUSED(event)) {
  m_findReplaceData->SetFlags(!((m_findReplaceData->GetFlags() & (~wxFR_DOWN)) |
                                (m_forward->GetValue() * wxFR_DOWN)));
  wxConfig::Get()->Write(wxT("findFlags"), m_findReplaceData->GetFlags());
}

void FindReplacePane::OnMatchCase(wxCommandEvent &event) {
  m_findReplaceData->SetFlags(
			      (m_findReplaceData->GetFlags() & (~wxFR_MATCHCASE)) |
			      (event.IsChecked() * wxFR_MATCHCASE));
  wxConfig::Get()->Write(wxT("findFlags"), m_findReplaceData->GetFlags());
}

void FindReplacePane::OnActivate(wxActivateEvent &event) {
  if (event.GetActive())
    SetTransparent(255);
  else
    SetTransparent(180);
  m_active = true;
}

void FindReplacePane::OnFindStringChange(wxCommandEvent &WXUNUSED(event)) {
  m_findReplaceData->SetFindString(m_searchText->GetValue());
}

void FindReplacePane::OnReplaceStringChange(wxCommandEvent &WXUNUSED(event)) {
  m_findReplaceData->SetReplaceString(m_replaceText->GetValue());
}

void FindReplacePane::OnKeyDown(wxKeyEvent &event) {
  if (event.ControlDown() && (event.GetUnicodeKey() == wxT('F'))) {
    wxCommandEvent dummyEvent;
    OnSearch(dummyEvent);
  } else if (event.GetKeyCode() == WXK_RETURN) {
    if (m_searchText->HasFocus()) {
      wxCommandEvent dummyEvent;
      OnSearch(dummyEvent);
    } else if (m_replaceText->HasFocus()) {
      wxCommandEvent dummyEvent;
      OnReplace(dummyEvent);
    } else
      event.Skip();
  } else
    event.Skip();
}
