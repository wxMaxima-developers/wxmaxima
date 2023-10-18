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
#include <wx/regex.h>

FindReplacePane::FindReplacePane(wxWindow *parent, FindReplaceData *data)
  : wxPanel(parent, -1) {
  m_active = true;
  m_findReplaceData = data;
  wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *top_sizer = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *button_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *lefttop_sizer = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(2);
  grid_sizer->SetFlexibleDirection(wxHORIZONTAL);
  grid_sizer->AddGrowableCol(1, 100);

  grid_sizer->Add(new wxStaticText(this, -1, _("Find:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));
  
  m_searchText = new wxTextCtrl(this, -1, data->GetFindString());
  m_searchText->Connect(
			wxEVT_TEXT, wxCommandEventHandler(FindReplacePane::OnFindStringChange),
			NULL, this);
  grid_sizer->Add(m_searchText, wxSizerFlags(1).Expand().Border(wxALL, 5));

  m_searchButton = new wxButton(this, wxID_FIND);
  button_sizer->Add(m_searchButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_searchButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(FindReplacePane::OnSearch),
                          NULL, this);

  grid_sizer->Add(new wxStaticText(this, -1, _("Replacement:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));

  m_replaceText = new wxTextCtrl(this, -1, data->GetReplaceString());
  m_replaceText->Connect(
			 wxEVT_TEXT, wxCommandEventHandler(FindReplacePane::OnReplaceStringChange),
			 NULL, this);
  grid_sizer->Add(m_replaceText, wxSizerFlags(1).Expand().Border(wxALL, 5));
  m_matchCase = new wxCheckBox(this, -1, _("Match Case"));
  lefttop_sizer->Add(grid_sizer, wxSizerFlags(1).Expand());
  lefttop_sizer->Add(m_matchCase, wxSizerFlags().Expand().Border(wxLEFT|wxRIGHT, 5));

  m_replaceButton = new wxButton(this, wxID_REPLACE);
  m_replaceButton->Connect(wxEVT_BUTTON,
                           wxCommandEventHandler(FindReplacePane::OnReplace),
                           NULL, this);
  button_sizer->Add(m_replaceButton, wxSizerFlags().Expand().Border(wxALL, 5));

  wxSizer *fbbox = new wxBoxSizer(wxHORIZONTAL);
  m_forward = new wxRadioButton(this, -1, _("Up"), wxDefaultPosition,
                                wxDefaultSize, wxRB_GROUP);
  fbbox->Add(m_forward, wxSizerFlags().Expand().Border(wxALL, 5));
  m_backwards = new wxRadioButton(this, -1, _("Down"));
  fbbox->Add(m_backwards, wxSizerFlags().Expand().Border(wxALL, 5));
  m_regexSearch = new wxRadioButton(this, -1, _("Regex"), wxDefaultPosition,
                                wxDefaultSize, wxRB_GROUP);
  fbbox->Add(m_regexSearch, wxSizerFlags().Expand().Border(wxALL, 5));
  m_simpleSearch = new wxRadioButton(this, -1, _("Simple"));
  fbbox->Add(m_simpleSearch, wxSizerFlags().Expand().Border(wxALL, 5));

  m_regexSearch->SetValue((data->GetRegexSearch()));
  m_regexSearch->Connect(wxEVT_RADIOBUTTON,
			 wxCommandEventHandler(FindReplacePane::OnRegexSimpleChange),
			 NULL, this);
  m_simpleSearch->SetValue(!(data->GetRegexSearch()));
  m_simpleSearch->Connect(wxEVT_RADIOBUTTON,
			  wxCommandEventHandler(FindReplacePane::OnRegexSimpleChange),
			  NULL, this);
  m_matchCase->Enable(!m_regexSearch->GetValue());
  if(m_regexSearch->GetValue())
    m_matchCase->SetValue(true);
  else
    m_matchCase->SetValue(!!(data->GetFlags() & wxFR_MATCHCASE));

  m_forward->SetValue(!(data->GetFlags() & wxFR_DOWN));
  m_backwards->SetValue(!!(data->GetFlags() & wxFR_DOWN));
  m_forward->Connect(wxEVT_RADIOBUTTON,
                     wxCommandEventHandler(FindReplacePane::OnDirectionChange),
                     NULL, this);
  m_backwards->Connect(
		       wxEVT_RADIOBUTTON,
		       wxCommandEventHandler(FindReplacePane::OnDirectionChange), NULL, this);

  m_replaceAllButton = new wxButton(this, 1, _("Replace All"));
  button_sizer->Add(m_replaceAllButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_replaceAllButton->Connect(
			      wxEVT_BUTTON, wxCommandEventHandler(FindReplacePane::OnReplaceAll), NULL,
			      this);
  top_sizer->Add(lefttop_sizer, wxSizerFlags(1).Expand());
  top_sizer->Add(button_sizer, wxSizerFlags());

  mainSizer->Add(top_sizer, wxSizerFlags(1).Expand());
  mainSizer->Add(fbbox, wxSizerFlags().Expand());
  m_matchCase->Connect(wxEVT_CHECKBOX,
                       wxCommandEventHandler(FindReplacePane::OnMatchCase),
                       NULL, this);

  // If I press <tab> in the search text box I want to arrive in the
  // replacement text box immediately.
  m_replaceText->MoveAfterInTabOrder(m_searchText);
  Connect(wxEVT_ACTIVATE, wxActivateEventHandler(FindReplacePane::OnActivate),
	  NULL, this);
  m_activateDuringConstruction = true;
  Connect(wxEVT_CHAR_HOOK, wxKeyEventHandler(FindReplacePane::OnKeyDown), NULL,
          this);
  this->SetSizerAndFit(mainSizer);
}

FindReplacePane::FindReplaceData::FindReplaceData() :
  wxFindReplaceData(),
  m_regexSearch(false)
{
}

void FindReplacePane::SetFindString(wxString strng) {
  m_findReplaceData->SetFindString(strng);
  m_searchText->SetValue(strng);
}

void FindReplacePane::OnSearch(wxCommandEvent &event) {
  event.Skip();
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_NEXT);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplace(wxCommandEvent &event) {
  event.Skip();
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplaceAll(wxCommandEvent &event) {
  event.Skip();
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE_ALL);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnDirectionChange(wxCommandEvent &event) {
  event.Skip();
  m_findReplaceData->SetFlags(!((m_findReplaceData->GetFlags() & (~wxFR_DOWN)) |
                                (m_forward->GetValue() * wxFR_DOWN)));
  wxConfig::Get()->Write(wxS("findFlags"), m_findReplaceData->GetFlags());
}

void FindReplacePane::OnRegexSimpleChange(wxCommandEvent &event){
  event.Skip();
  m_findReplaceData->SetRegexSearch(m_regexSearch->GetValue());
  m_matchCase->Enable(!m_regexSearch->GetValue());
  if(m_regexSearch->GetValue())
    m_matchCase->SetValue(true);
  else
    m_matchCase->SetValue(!!(m_findReplaceData->GetFlags() & wxFR_MATCHCASE));
}


void FindReplacePane::OnMatchCase(wxCommandEvent &event) {
  m_findReplaceData->SetFlags(
			      (m_findReplaceData->GetFlags() & (~wxFR_MATCHCASE)) |
			      (event.IsChecked() * wxFR_MATCHCASE));
  wxConfig::Get()->Write(wxS("findFlags"), m_findReplaceData->GetFlags());
}

void FindReplacePane::OnActivate(wxActivateEvent &event) {
  event.Skip();
  if(m_activateDuringConstruction)
    {
      m_activateDuringConstruction = false;
      return;
    }
  if (event.GetActive())
    {
      SetTransparent(255);
      m_searchText->SetFocus();
      m_active = true;
    }
  else
    {
      SetTransparent(180);
      m_active = false;
    }
}

void FindReplacePane::OnFindStringChange(wxCommandEvent &event) {
  event.Skip();
  m_findReplaceData->SetFindString(m_searchText->GetValue());
  if(m_findReplaceData->GetRegexSearch())
    {
      wxLogNull suppressor;
      wxRegEx test(m_searchText->GetValue());
      if(test.IsValid())
	m_searchText->SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
      else
	m_searchText->SetForegroundColour(wxColor(255, 165, 0));
    }
  else
    m_searchText->SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
  
}

void FindReplacePane::OnReplaceStringChange(wxCommandEvent &WXUNUSED(event)) {
  m_findReplaceData->SetReplaceString(m_replaceText->GetValue());
}

void FindReplacePane::OnKeyDown(wxKeyEvent &event) {
  if (event.ControlDown() && (event.GetUnicodeKey() == wxS('F'))) {
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
