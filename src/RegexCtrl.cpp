// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
#include "ErrorRedirector.h"

RegexCtrl::RegexCtrl(wxWindow *parent,
                     wxWindowID id) :
  wxTextCtrl(parent, id)
{
  SetToolTip(RegexTooltip_norm);
  Connect(wxEVT_TEXT,
          wxCommandEventHandler(RegexCtrl::OnTextChange), NULL, this);
  if (RegexTooltip_norm.IsEmpty())
    RegexTooltip_norm = _("Input a RegEx here to filter the results");
  if (RegexTooltip_error.IsEmpty())
    RegexTooltip_error = _("Invalid RegEx!");
}

RegexCtrl::RegexInputState RegexCtrl::GetNewRegexInputState() const
{
  if (GetValue().empty()) return RegexInputState::empty;
  if (m_regex.IsValid())  return RegexInputState::valid;
  return RegexInputState::invalid;
}

bool RegexCtrl::Matches(wxString text)
{
  if(GetValue().IsEmpty())
    return true;
  if(!m_regex.IsValid())
    return true;
  return m_regex.Matches(text);
}

void RegexCtrl::OnTextChange(wxCommandEvent &WXUNUSED(ev))
{
  if (GetValue() != m_oldRegex)
  {
    if(!GetValue().empty())
    {
      m_oldRegex = GetValue();
      wxRegEx regex;
      SuppressErrorDialogs blocker;
      m_regex.Compile(GetValue());
      // Update UI feedback if the state of the regex input control has changed
      auto const newInputState = GetNewRegexInputState();
      if (m_regexInputState != newInputState)
      {
        m_regexInputState = newInputState;
        const wxColor colors[3] = {
          /* empty   */ wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOW),
          /* invalid */ {255,192,192},
          /* valid   */ wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHTTEXT)
        };
        const wxString tooltips[3] = {
          /* empty */ RegexTooltip_norm, /* invalid */ RegexTooltip_error, /* valid */ RegexTooltip_norm
        };
        SetBackgroundColour(colors[int(m_regexInputState)]);
        SetToolTip(tooltips[int(m_regexInputState)]);
        Refresh();
      }
    }
    wxCommandEvent event(REGEX_EVENT);
    wxPostEvent(this, event);
  }
}


RegexCtrl::~RegexCtrl()
{}

wxDEFINE_EVENT(REGEX_EVENT, wxCommandEvent);

//! The tooltip that is displayed if the regex cannot be interpreted
wxString RegexCtrl::RegexTooltip_error;
//! The tooltip that is displayed if the regex is empty or can be interpreted
wxString RegexCtrl::RegexTooltip_norm;
