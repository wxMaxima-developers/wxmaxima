// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include "Configuration.h"
#include <algorithm>
#include <memory>
#include <wx/config.h>
#include <wx/filedlg.h>
#include <wx/menu.h>
#include <wx/sizer.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/wupdlock.h>

History::History(wxWindow *parent, int id, Configuration *cfg)
  : wxPanel(parent, id) {
  wxConfig::Get()->Read(m_showCurrentSessionOnlyKey, &m_showCurrentSessionOnly);
  // wxLB_MULTIPLE and wxLB_EXTENDED are mutually exclusive and will assert on
  // Windows
  m_history =
    new wxListBox(this, history_ctrl_id, wxDefaultPosition, wxDefaultSize, 0,
		  NULL, wxLB_EXTENDED | wxLB_HSCROLL | wxLB_NEEDED_SB);
  m_regex = new RegexCtrl(this, wxID_ANY, cfg);
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);

  box->Add(m_history, wxSizerFlags(1).Expand());
  box->Add(m_regex, wxSizerFlags().Expand());

  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(History::OnMouseRightDown),
          NULL, this);
  m_history->Connect(wxEVT_RIGHT_DOWN,
                     wxMouseEventHandler(History::OnMouseRightDown), NULL,
                     this);
  Connect(wxEVT_MENU, wxCommandEventHandler(History::OnMenu), NULL, this);
  m_regex->Connect(REGEX_EVENT, wxCommandEventHandler(History::OnRegExEvent),
                   NULL, this);
  SetSizer(box);
  FitInside();
}

void History::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  wxArrayInt selections;
  bool const hasSelections = (m_history->GetSelections(selections) > 0);
  wxString number;

  wxMenu popupMenu;
  if (m_commands.size() > 0) {
    popupMenu.Append(export_all, _("Export all history to a .mac file"));
    popupMenu.Append(
		     export_session,
		     _("Export commands from the current maxima session to a .mac file"));
    if (hasSelections)
      popupMenu.Append(export_selected,
                       _("Export selected commands to a .mac file"));
    if (m_history->GetCount() > 0)
      popupMenu.Append(export_visible,
                       _("Export visible commands to a .mac file"));
    if (popupMenu.GetMenuItemCount() > 0)
      popupMenu.AppendSeparator();
    if (hasSelections)
      popupMenu.Append(clear_selection, _("Clear the selection"));
    popupMenu.Append(clear_history, _("Clear all history"));
  }
  if (popupMenu.GetMenuItemCount() > 0)
    popupMenu.AppendSeparator();
  popupMenu.AppendCheckItem(toggle_ShowCurrentSessionOnly,
                            _("Show commands from current session only"));
  popupMenu.Check(toggle_ShowCurrentSessionOnly, m_showCurrentSessionOnly);
  PopupMenu(&popupMenu);
}

void History::MaximaSessionStart() {
  if (m_commands.size() != 0)
    AddToHistory(wxS("quit();"));
  m_sessionCommands = 0;
  if (m_showCurrentSessionOnly)
    m_history->Clear();
}

bool History::UpdateDeferred() {
  if (!m_deferredCommands.empty()) {
    std::reverse(m_deferredCommands.begin(), m_deferredCommands.end());
    m_history->Insert(m_deferredCommands, 0);
    m_current += m_deferredCommands.size();
    m_deferredCommands.clear();
    SetCurrent(0);
    return true;
  } else
    return false;
}

void History::UnselectAll() const {
  // Unselect all, See: https://forums.wxwidgets.org/viewtopic.php?t=29463
  m_history->SetSelection(-1, false);
}

static wxString AskForFileName(wxPanel *parent) {
  wxFileDialog fileDialog(parent, _("Export As"), wxEmptyString, wxEmptyString,
                          _("Maxima session (*.mac)|*.mac"),
                          wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
  if (fileDialog.ShowModal() == wxID_OK)
    return fileDialog.GetPath();
  return {};
}

void History::OnMenu(wxCommandEvent &event) {
  bool indicateError = false;
  int start = 0;
  int const size = m_commands.size();

  switch (event.GetId()) {
  case toggle_ShowCurrentSessionOnly:
    m_showCurrentSessionOnly = !m_showCurrentSessionOnly;
    wxConfig::Get()->Write(m_showCurrentSessionOnlyKey,
                           m_showCurrentSessionOnly);
    RebuildDisplay();
    break;
  case export_selected: {
    wxArrayInt selections;
    if (m_history->GetSelections(selections) > 0) {
      auto file = AskForFileName(this);
      if (!file.empty()) {
        wxFileOutputStream output(file);
        if (output.IsOk()) {
          wxTextOutputStream text(output);
          for (auto sel = selections.rbegin(); sel != selections.rend(); ++sel)
            text << m_history->GetString(*sel) << "\n";
        }
        indicateError = !output.IsOk() || !output.Close();
      }
    }
    break;
  }
  case export_session:
    start = size - m_sessionCommands;
    // fallthrough

  case export_all: {
    auto file = AskForFileName(this);
    if (!file.empty()) {
      wxFileOutputStream output(file);
      if (output.IsOk()) {
        wxTextOutputStream text(output);
        for (int i = start; i < size; ++i)
          text << m_commands[i] << "\n";
      }
      indicateError = !output.IsOk() || !output.Close();
    }
    break;
  }
  case export_visible: {
    auto file = AskForFileName(this);
    if (!file.empty()) {
      wxFileOutputStream output(file);
      if (output.IsOk()) {
        wxTextOutputStream text(output);
        for (int i = m_history->GetCount() - 1; i >= 0; --i)
          text << m_history->GetString(i) << "\n";
      }
      indicateError = !output.IsOk() || !output.Close();
    }
    break;
  }
  case clear_history:
    m_history->Clear();
    m_commands.clear();
    m_sessionCommands = 0;
    break;
  case clear_selection:
    m_history->DeselectAll();
    break;
  }
  if (indicateError)
    LoggingMessageBox(_("Exporting to .mac file failed!"), _("Error!"), wxOK);
}

History::~History() {}

void History::AddToHistory(const wxString &cmd) {
  if (cmd.IsEmpty())
    return;

  m_sessionCommands++;
  m_commands.push_back(cmd);

  if (m_regex->Matches(cmd))
    m_deferredCommands.Add(cmd);
}

void History::RebuildDisplay() {
  //  wxWindowUpdateLocker speedUp(this);
  wxArrayString display;
  std::vector<wxString>::const_reverse_iterator sessionEnd;
  if (m_showCurrentSessionOnly)
    sessionEnd = m_commands.rbegin() + m_sessionCommands;
  else
    sessionEnd = m_commands.rend();

  display.reserve(m_commands.size());
  display.reserve(m_commands.size());
  for (auto cmd = m_commands.rbegin(); cmd != sessionEnd; ++cmd) {
    if (m_regex->Matches(*cmd))
      display.Add(*cmd);
  }
  m_history->Set(display);
  m_current = -1;
  if (m_history->GetCount() > 0)
    SetCurrent(0);
}

void History::OnRegExEvent(wxCommandEvent &WXUNUSED(ev)) {
  UnselectAll();
  RebuildDisplay();
}

wxString History::GetCommand(bool next) {
  if (m_commands.size() == 0)
    return {};

  auto current = m_current + (next ? +1 : -1);
  SetCurrent(current);
  return m_history->GetString(m_current);
}

void History::SetCurrent(long current) {
  auto const count = long(m_history->GetCount());
  if (current < 0)
    current = count - 1;
  else if (current >= count)
    current = 0;
  if (count < 1)
    current = -1;

  if (current == m_current)
    return;

  if (current >= 0) {
    m_current = current;
    m_history->EnsureVisible(m_current);
    UnselectAll();
    m_history->SetSelection(m_current);
  }
}

wxString
History::m_showCurrentSessionOnlyKey(wxS("history/ShowCurrentSessionOnly"));
