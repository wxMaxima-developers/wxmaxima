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
#include "cells/EditorCell.h"
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/regex.h>

FindReplacePane::FindReplacePane(wxWindow *parent, FindReplaceData *data)
  : wxPanel(parent, -1) {
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

  m_searchText = new wxComboBox(this, -1, data->GetFindString(), wxDefaultPosition,
                                wxDefaultSize, 0, NULL, wxTE_PROCESS_ENTER);
  LoadHistory(m_searchText, wxS("SearchHistory"));
  m_searchText->SetValue(data->GetFindString());
  m_searchText->Bind(wxEVT_TEXT, &FindReplacePane::OnFindStringChange, this);
  grid_sizer->Add(m_searchText, wxSizerFlags(1).Expand().Border(wxALL, 5));
  m_searchText->SetFocus();

  m_searchButton = new wxButton(this, wxID_FIND);
  button_sizer->Add(m_searchButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_searchButton->Bind(wxEVT_BUTTON, &FindReplacePane::OnSearch, this);
  m_searchButton->SetDefault();

  grid_sizer->Add(new wxStaticText(this, -1, _("Replacement:")),
                  wxSizerFlags().Right().Center().Border(wxALL, 5));

  m_replaceText = new wxComboBox(this, -1, data->GetReplaceString(), wxDefaultPosition,
                                 wxDefaultSize, 0, NULL, wxTE_PROCESS_ENTER);
  LoadHistory(m_replaceText, wxS("ReplaceHistory"));
  m_replaceText->SetValue(data->GetReplaceString());
  m_replaceText->Bind(wxEVT_TEXT, &FindReplacePane::OnReplaceStringChange, this);
  grid_sizer->Add(m_replaceText, wxSizerFlags(1).Expand().Border(wxALL, 5));
  m_matchCase = new wxCheckBox(this, -1, _("Match Case"));
  lefttop_sizer->Add(grid_sizer, wxSizerFlags(1).Expand());
  lefttop_sizer->Add(m_matchCase, wxSizerFlags().Expand().Border(wxLEFT|wxRIGHT, 5));

  m_replaceButton = new wxButton(this, wxID_REPLACE);
  m_replaceButton->Bind(wxEVT_BUTTON, &FindReplacePane::OnReplace, this);
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
  m_regexSearch->Bind(wxEVT_RADIOBUTTON, &FindReplacePane::OnRegexSimpleChange, this);
  m_simpleSearch->SetValue(!(data->GetRegexSearch()));
  m_simpleSearch->Bind(wxEVT_RADIOBUTTON, &FindReplacePane::OnRegexSimpleChange, this);
  m_matchCase->Enable(!m_regexSearch->GetValue());
  if(m_regexSearch->GetValue())
    m_matchCase->SetValue(true);
  else
    m_matchCase->SetValue(!!(data->GetFlags() & wxFR_MATCHCASE));

  m_forward->SetValue(!(data->GetFlags() & wxFR_DOWN));
  m_backwards->SetValue(!!(data->GetFlags() & wxFR_DOWN));
  m_forward->Bind(wxEVT_RADIOBUTTON, &FindReplacePane::OnDirectionChange, this);
  m_backwards->Bind(wxEVT_RADIOBUTTON, &FindReplacePane::OnDirectionChange, this);

  wxSizer *inoutbox = new wxBoxSizer(wxHORIZONTAL);
  m_searchInInput = new wxCheckBox(this, -1, _("Input"));
  inoutbox->Add(m_searchInInput, wxSizerFlags().Expand().Border(wxALL, 5));
  m_searchInOutput = new wxCheckBox(this, -1, _("Output"));
  inoutbox->Add(m_searchInOutput, wxSizerFlags().Expand().Border(wxALL, 5));

  m_searchInInput->SetValue(!!(data->GetFlags() & wxFR_SEARCH_IN_INPUT));
  m_searchInOutput->SetValue(!!(data->GetFlags() & wxFR_SEARCH_IN_OUTPUT));
  m_searchInInput->Bind(wxEVT_CHECKBOX, &FindReplacePane::OnSearchIn, this);
  m_searchInOutput->Bind(wxEVT_CHECKBOX, &FindReplacePane::OnSearchIn, this);

  m_replaceAllButton = new wxButton(this, 1, _("Replace All"));
  button_sizer->Add(m_replaceAllButton, wxSizerFlags().Expand().Border(wxALL, 5));
  m_replaceAllButton->Bind(wxEVT_BUTTON, &FindReplacePane::OnReplaceAll, this);
  top_sizer->Add(lefttop_sizer, wxSizerFlags(1).Expand());
  top_sizer->Add(button_sizer, wxSizerFlags());

  mainSizer->Add(top_sizer, wxSizerFlags(1).Expand());
  mainSizer->Add(fbbox, wxSizerFlags().Expand());
  mainSizer->Add(inoutbox, wxSizerFlags().Expand());
  m_matchCase->Bind(wxEVT_CHECKBOX, &FindReplacePane::OnMatchCase, this);

  // If I press <tab> in the search text box I want to arrive in the
  // replacement text box immediately.
  m_replaceText->MoveAfterInTabOrder(m_searchText);
  Bind(wxEVT_CHAR_HOOK, &FindReplacePane::OnKeyDown, this);
  this->SetSizerAndFit(mainSizer);
}

FindReplacePane::~FindReplacePane()
{
  wxConfig::Get()->Write(wxS("Find/Flags"), m_findReplaceData->GetFlags());
  wxConfig::Get()->Write(wxS("Find/RegexSearch"), m_findReplaceData->GetRegexSearch());

}

FindReplacePane::FindReplaceData::FindReplaceData() :
  wxFindReplaceData(),
  m_regexSearch(false)
{
}

void FindReplacePane::LoadHistory(wxComboBox *combo, const wxString &key) {
  wxConfigBase *config = wxConfig::Get();
  long count = 0;
  config->Read(wxS("Find/") + key + wxS("/count"), &count, 0);
  if (count > m_historyLength)
    count = m_historyLength;
  for (long i = 0; i < count; i++) {
    wxString item;
    if (config->Read(wxString::Format(wxS("Find/%s/%ld"), key, i), &item) &&
        !item.IsEmpty())
      combo->Append(item);
  }
}

void FindReplacePane::AddToHistory(wxComboBox *combo, const wxString &key,
                                   const wxString &value) {
  if (value.IsEmpty())
    return;

  // Move the term to the front, without duplicates, capped at m_historyLength.
  int existing = combo->FindString(value);
  if (existing != wxNOT_FOUND)
    combo->Delete(existing);
  combo->Insert(value, 0);
  while (static_cast<int>(combo->GetCount()) > m_historyLength)
    combo->Delete(combo->GetCount() - 1);
  // Inserting may have cleared the text field; keep showing the current term.
  combo->SetValue(value);

  // Persist the whole list so it survives across sessions.
  wxConfigBase *config = wxConfig::Get();
  const long count = combo->GetCount();
  config->Write(wxS("Find/") + key + wxS("/count"), count);
  for (long i = 0; i < count; i++)
    config->Write(wxString::Format(wxS("Find/%s/%ld"), key, i),
                  combo->GetString(i));
}

void FindReplacePane::SetFocus() {
  m_searchText->SetFocus();
}

void FindReplacePane::SetFindString(wxString strng) {
  m_findReplaceData->SetFindString(strng);
  m_searchText->SetValue(strng);
  m_searchText->SetFocus();
}

void FindReplacePane::OnSearch(wxCommandEvent &event) {
  event.Skip();
  AddToHistory(m_searchText, wxS("SearchHistory"),
               m_findReplaceData->GetFindString());
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_NEXT);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  const wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplace(wxCommandEvent &event) {
  event.Skip();
  AddToHistory(m_searchText, wxS("SearchHistory"),
               m_findReplaceData->GetFindString());
  AddToHistory(m_replaceText, wxS("ReplaceHistory"),
               m_findReplaceData->GetReplaceString());
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  const wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnReplaceAll(wxCommandEvent &event) {
  event.Skip();
  AddToHistory(m_searchText, wxS("SearchHistory"),
               m_findReplaceData->GetFindString());
  AddToHistory(m_replaceText, wxS("ReplaceHistory"),
               m_findReplaceData->GetReplaceString());
  wxFindDialogEvent *findEvent = new wxFindDialogEvent(wxEVT_FIND_REPLACE_ALL);
  findEvent->SetFindString(m_findReplaceData->GetFindString());
  findEvent->SetReplaceString(m_findReplaceData->GetReplaceString());
  findEvent->SetFlags(m_findReplaceData->GetFlags());
  const wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  topLevelWindow->GetEventHandler()->QueueEvent(findEvent);
}

void FindReplacePane::OnDirectionChange(wxCommandEvent &event) {
  event.Skip();
  m_findReplaceData->SetFlags(
                              (m_findReplaceData->GetFlags() & (~wxFR_DOWN)) |
                              (m_backwards->GetValue() * wxFR_DOWN));
  wxConfig::Get()->Write(wxS("Find/Flags"), m_findReplaceData->GetFlags());
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
  wxConfig::Get()->Write(wxS("Find/Flags"), m_findReplaceData->GetFlags());
}

void FindReplacePane::OnSearchIn(wxCommandEvent &event) {
  if (!m_searchInInput->GetValue() && !m_searchInOutput->GetValue()) {
    if (event.GetEventObject() == m_searchInInput)
      m_searchInOutput->SetValue(true);
    else
      m_searchInInput->SetValue(true);
  }
  m_findReplaceData->SetFlags(
                              (m_findReplaceData->GetFlags() & (~(wxFR_SEARCH_IN_INPUT | wxFR_SEARCH_IN_OUTPUT))) |
                              (m_searchInInput->GetValue() * wxFR_SEARCH_IN_INPUT) |
                              (m_searchInOutput->GetValue() * wxFR_SEARCH_IN_OUTPUT));
  wxConfig::Get()->Write(wxS("Find/Flags"), m_findReplaceData->GetFlags());
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
