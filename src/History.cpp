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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class History

  History is a side bar that shows all recently issued maxima commands.
 */

#include "History.h"

#include <wx/sizer.h>
#include <wx/regex.h>

History::History(wxWindow *parent, int id) : wxPanel(parent, id)
{
  m_history = new wxListBox(this, history_ctrl_id);
  m_regex = new wxTextCtrl(this, history_regex_id);
  wxFlexGridSizer *box = new wxFlexGridSizer(1);
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);

  box->Add(m_history, wxSizerFlags().Expand());
  box->Add(m_regex, wxSizerFlags().Expand());

  SetSizer(box);
  box->Fit(this);
  box->SetSizeHints(this);
  m_current = 0;
  Connect(history_regex_id, wxEVT_TEXT, wxCommandEventHandler(History::OnRegExEvent), NULL, this);
}

History::~History()
{
}

void History::AddToHistory(const wxString &cmd)
{  
  if (cmd.IsEmpty())
    return;
    
  commands.Insert(cmd, 0);
  m_current = commands.GetCount();

  wxString regex = m_regex->GetValue();
  wxArrayString display;
  wxRegEx matcher;
  
  if (regex != wxEmptyString)
    matcher.Compile(regex);
  
  if (regex.Length() > 0 && matcher.IsValid())
  {
    if (matcher.Matches(cmd))
    {
      int pos = m_history->GetCount() - 1;
      m_history->Insert(cmd, pos);
      m_history->SetSelection(pos);
    }
  }
  else
  {
    int pos = m_history->GetCount() - 1;
    m_history->Insert(cmd, m_current - 1);
    m_history->SetSelection(pos);          
  }
 
  UpdateDisplay();
}

void History::UpdateDisplay()
{
  wxString regex = m_regex->GetValue();
  wxArrayString display;
  wxRegEx matcher;

  if (regex != wxEmptyString)
    matcher.Compile(regex);

  for (unsigned int i = 0; i < commands.Count(); i++)
  {
    wxString curr = commands.Item(i);

    if (regex.Length() > 0 && matcher.IsValid())
    {
      if (matcher.Matches(curr))
        display.Add(curr);
    }
    else
      display.Add(curr);
  }

  m_history->Set(display);
}

void History::OnRegExEvent(wxCommandEvent &WXUNUSED(ev))
{
  UpdateDisplay();
}

wxString History::GetCommand(bool next)
{
  if (commands.GetCount() == 0)
    return wxEmptyString;

  else if (next)
  {
    --m_current;
    if (m_current < 0)
      m_current = commands.GetCount() - 1;
    m_history->SetSelection(m_current);
    return commands[m_current];
  }
  else
  {
    ++m_current;
    if (m_current >= (long) commands.GetCount())
      m_current = 0;
    m_history->SetSelection(m_current);
    return commands[m_current];
  }
}
