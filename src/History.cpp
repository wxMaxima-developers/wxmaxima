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
#include <wx/menu.h>
#include <wx/filedlg.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <memory>
#include "ErrorRedirector.h"

History::History(wxWindow *parent, int id) : wxPanel(parent, id)
{
  if(m_regexTooltip_norm.IsEmpty())
    m_regexTooltip_norm = _("Input a RegEx here to filter the results");
  if(m_regexTooltip_error.IsEmpty())
    m_regexTooltip_error = _("Invalid RegEx!");
  
  m_history = new wxListBox(this, history_ctrl_id, wxDefaultPosition, wxDefaultSize, 0, NULL, wxLB_MULTIPLE);
  m_regex = new wxTextCtrl(this, history_regex_id);
  m_regex->SetToolTip(m_regexTooltip_norm);
  wxFlexGridSizer *box = new wxFlexGridSizer(1);
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);

  box->Add(m_history, wxSizerFlags().Expand());
  box->Add(m_regex, wxSizerFlags().Expand());

  SetSizer(box);
  box->Fit(this);
  box->SetSizeHints(this);
  m_current = 0;
  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(History::OnMouseRightDown), NULL, this);
  m_history->Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(History::OnMouseRightDown), NULL, this);
  Connect(wxEVT_MENU,
          wxCommandEventHandler(History::OnMenu), NULL, this);
  m_regex->Connect(wxEVT_TEXT,
          wxCommandEventHandler(History::OnRegExEvent), NULL, this);
}

void History::OnMouseRightDown(wxMouseEvent &event)
{
  if(m_commands.GetCount() == 0)
  {
    event.Skip();
    return;
  }
  wxString number;
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  popupMenu->Append(export_all, _("Export all history to a .mac file"));
   popupMenu->Append(export_session, _("Export commands from the current maxima session to a .mac file"));
  wxArrayInt selections;
  if(m_history->GetSelections(selections) > 0)
    popupMenu->Append(export_selected, _("Export selected commands to a .mac file"));
  if(m_history->GetCount() > 0)
    popupMenu->Append(export_visible, _("Export visible commands to a .mac file"));
  
  PopupMenu(&*popupMenu);
}

void History::MaximaSessionStart()
{
  if(m_commands.GetCount() != 0)
    AddToHistory(wxT("quit();"));
  m_sessionCommands = -1;
}

void History::OnMenu(wxCommandEvent &event)
{
  int start = m_commands.GetCount() - 1;
  switch (event.GetId())
  {
  case export_selected:
  {
    wxArrayInt selections;
    if(m_history->GetSelections(selections) > 0)
    {
      wxFileDialog fileDialog(this,
                              _("Export As"), wxEmptyString,
                              wxEmptyString,
                              _("Maxima session (*.mac)|*.mac"),
                              wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (fileDialog.ShowModal() == wxID_OK)
      {
        wxString file = fileDialog.GetPath();
        wxFileOutputStream output(file);
        if(output.IsOk())
        {
          wxTextOutputStream text(output);
          while(!selections.IsEmpty())
          {
            text << m_history->GetString(selections.Last()) << "\n";
            selections.RemoveAt(selections.GetCount() - 1);
          }
          text.Flush();
        }
        if(!output.IsOk() || !output.Close())
          LoggingMessageBox(_("Exporting to .mac file failed!"), _("Error!"),
                            wxOK);
        
      }
    }
    break;
  }
  case export_session:
    start = m_sessionCommands;
    //fallthrough
    
  case export_all:
  {
    wxFileDialog fileDialog(this,
                            _("Export As"), wxEmptyString,
                            wxEmptyString,
                            _("Maxima session (*.mac)|*.mac"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (fileDialog.ShowModal() == wxID_OK)
    {
      wxString file = fileDialog.GetPath();
      wxFileOutputStream output(file);
      if(output.IsOk())
      {
        wxTextOutputStream text(output);
        for(int i = start; i >= 0; --i)
          text << m_commands[i] << "\n";
        text.Flush();
      }
      if(!output.IsOk() || !output.Close())
        LoggingMessageBox(_("Exporting to .mac file failed!"), _("Error!"),
                          wxOK);
      
    }
  break;
  }
  case export_visible:
  {
    wxFileDialog fileDialog(this,
                            _("Export As"), wxEmptyString,
                            wxEmptyString,
                            _("Maxima session (*.mac)|*.mac"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (fileDialog.ShowModal() == wxID_OK)
    {
      start = m_history->GetCount()-1;
      wxString file = fileDialog.GetPath();
      wxFileOutputStream output(file);
      if(output.IsOk())
      {
        wxTextOutputStream text(output);
        for(int i = start; i >= 0; --i)
          text << m_history->GetString(i) << "\n";
        text.Flush();
      }
      if(!output.IsOk() || !output.Close())
        LoggingMessageBox(_("Exporting to .mac file failed!"), _("Error!"),
                          wxOK);
      
    }
  break;
  }
  }
}


History::~History()
{
}

void History::AddToHistory(const wxString &cmd)
{
  if (cmd.IsEmpty())
    return;
  m_sessionCommands ++;
    
  m_commands.Insert(cmd, 0);
  m_current = m_commands.GetCount();

  wxString regex = m_regex->GetValue();
  wxArrayString display;
    
  if (!regex.IsEmpty() && m_matcher.IsValid())
  {
    if (m_matcher.Matches(cmd))
    {
      int pos = m_history->GetCount() - 1;
      m_history->Insert(cmd, pos);
      m_history->SetSelection(pos);
    }
  }
  else
  {
    int pos = m_history->GetCount() - 1;
    if(pos < 0)
      pos = 0;
    m_history->Insert(cmd, pos);
    m_history->SetSelection(pos);          
  }
}

void History::UpdateDisplay()
{
  wxString regex = m_regex->GetValue();
  wxArrayString display;

  for (unsigned int i = 0; i < m_commands.Count(); i++)
  {
    wxString curr = m_commands.Item(i);

    if (!regex.IsEmpty() && m_matcher.IsValid())
    {
      if (m_matcher.Matches(curr))
        display.Add(curr);
    }
    else
      display.Add(curr);
  }

  m_history->Set(display);
}

void History::OnRegExEvent(wxCommandEvent &WXUNUSED(ev))
{
  wxString regex = m_regex->GetValue();
  if (regex != wxEmptyString)
  {
    SuppressErrorDialogs blocker;
    m_matcher.Compile(regex);
  }
  if(m_matcher.IsValid())
  {
    m_regex->SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHTTEXT));
    m_regex->SetToolTip(m_regexTooltip_norm);
  }
  else
  {
    m_regex->SetBackgroundColour(wxColor(255,192,192));
    m_regex->SetToolTip(m_regexTooltip_error);
  }
  UpdateDisplay();
}

wxString History::GetCommand(bool next)
{
  if (m_commands.GetCount() == 0)
    return wxEmptyString;

  else if (next)
  {
    --m_current;
    if (m_current < 0)
      m_current = m_history->GetCount() - 1;
    m_history->SetSelection(m_current);
    return m_history->GetString(m_current);
  }
  else
  {
    ++m_current;
    if (m_current >= (long) m_history->GetCount())
      m_current = 0;
    m_history->SetSelection(m_current);
    return m_history->GetString(m_current);
  }
}

wxString History::m_regexTooltip_error;
wxString History::m_regexTooltip_norm;
