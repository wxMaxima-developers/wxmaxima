// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "RegexCtrl.h"
#include "../EventIDs.h"
#include "../Configuration.h"
#include "../ErrorRedirector.h"
#include <wx/config.h>
#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

RegexCtrl::RegexCtrl(wxWindow *parent, wxWindowID id, Configuration *cfg, wxString configName)
  : BTextCtrl(parent, id, cfg),
    m_configName(configName)
{
  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(RegexCtrl::OnMouseRightDown),
          NULL, this);
  Connect(wxEVT_MENU, wxCommandEventHandler(RegexCtrl::OnMenu), NULL, this);
  Connect(wxEVT_TEXT, wxCommandEventHandler(RegexCtrl::OnTextChange), NULL,
          this);
  if (RegexTooltip_norm.IsEmpty())
    RegexTooltip_norm = _("Input a RegEx here to filter the results.\nRight-click for options!");
  if (RegexTooltip_error.IsEmpty())
    RegexTooltip_error = _("Invalid RegEx!");
  if (RegexTooltip_textsearch.IsEmpty())
    RegexTooltip_textsearch = _("Input your filter text here.\nRight-click for options!");
  wxConfig::Get()->Read(wxS("SearchBox/") + m_configName + "/isRegex", &m_isRegex);
  if(m_isRegex)
    SetToolTip(RegexTooltip_norm);
  else
    SetToolTip(RegexTooltip_textsearch);
}

RegexCtrl::RegexInputState RegexCtrl::GetNewRegexInputState() const {
  if (GetValue().empty())
    return RegexInputState::empty;
  if (m_isRegex || m_regex.IsValid())
    return RegexInputState::valid;
  return RegexInputState::invalid;
}

bool RegexCtrl::Matches(wxString text) {
  if (GetValue().IsEmpty())
    return true;
  if(m_isRegex)
    {
      if (!m_regex.IsValid())
        return true;
      return m_regex.Matches(text);
    }
  else
    {
      return text.Find(GetValue()) != wxNOT_FOUND;
    }
}

void RegexCtrl::OnTextChange(wxCommandEvent &WXUNUSED(ev)) {
  OnChange();
}

void RegexCtrl::OnChange() {
  if (GetValue() != m_oldRegex) {
    if(m_isRegex)
      {
        if (!GetValue().empty()) {
          m_oldRegex = GetValue();
          wxRegEx regex;
          wxString errMsg;
          {
            wxLog *logOld = wxLog::GetActiveTarget();
            wxLogBuffer_noStdErrFlush errOut;
            wxLog::SetActiveTarget(&errOut);
            m_regex.Compile(GetValue());
            errMsg = errOut.GetBuffer();
            errMsg.Trim(true);
            int colonPos = errMsg.Find(": ");
            if (colonPos > 2)
              errMsg = errMsg.Right(errMsg.Length() - colonPos - 2);
            wxLog::SetActiveTarget(logOld);
          }
          // Update UI feedback if the state of the regex input control has changed
          auto const newInputState = GetNewRegexInputState();
          if (m_regexInputState != newInputState) {
            m_regexInputState = newInputState;
            const wxColor colors[3] = {
              wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT), /* empty regexx field  */
              {255, 165, 0}, /* invalid is orange. Red seems too 'dangerous'.
                              */
              {0, 255, 0}    /* valid is green: Input okay. */
            };
            if (errMsg.IsEmpty())
              errMsg = RegexTooltip_error;
            else
              wxLogMessage("%s", errMsg.mb_str());
            const wxString tooltips[3] = {/* empty */ RegexTooltip_norm,
                                          /* invalid */ errMsg,
                                          /* valid */ RegexTooltip_norm};
            // One could also set the background color, with
            // SetBackgroundColour(...); Be careful, not only set the foreground
            // color to black or white the background color may be the same (or have
            // not enough contrast) if dark mode is used. Choose always colors with
            // some contrast. Green and orange (as above for valid/invalid input)
            // works with normal and dark mode.
            SetForegroundColour(colors[static_cast<int>(m_regexInputState)]);
            SetToolTip(tooltips[static_cast<int>(m_regexInputState)]);
            Refresh();
          }
        }
      }
  }
  wxCommandEvent event(REGEX_EVENT);
  wxPostEvent(this, event);
}

void RegexCtrl::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  wxMenu popupMenu;
  popupMenu.AppendRadioItem(EventIDs::menu_regex_isRegex, _("Use RegEx filtering"));
  popupMenu.AppendRadioItem(EventIDs::menu_regex_isTextSearch, _("Use Text search filtering"));
  if(m_isRegex)
    popupMenu.Check(EventIDs::menu_regex_isRegex, true);
  else
    {
      popupMenu.Check(EventIDs::menu_regex_isTextSearch, true);
      SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
    }
  PopupMenu(&popupMenu);
}

void RegexCtrl::OnMenu(wxCommandEvent &event) {
  if (event.GetId() == EventIDs::menu_regex_isRegex)
    m_isRegex = true;

  if (event.GetId() == EventIDs::menu_regex_isTextSearch)
    m_isRegex = false;
}

RegexCtrl::~RegexCtrl() {
  wxConfig::Get()->Write(wxS("SearchBox/") + m_configName + "/isRegex", m_isRegex);
}

wxDEFINE_EVENT(REGEX_EVENT, wxCommandEvent);

wxString RegexCtrl::RegexTooltip_error;
wxString RegexCtrl::RegexTooltip_norm;
wxString RegexCtrl::RegexTooltip_textsearch;
