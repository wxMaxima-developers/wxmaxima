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

#include "BTextCtrl.h"
#include <wx/config.h>

BTextCtrl::BTextCtrl(wxWindow *parent,
                     wxWindowID id,
                     Configuration *cfg,
                     const wxString &value,
                     const wxPoint &pos,
                     const wxSize &size,
                     long style)
        : wxTextCtrl(parent, id, value, pos, size, style)
{
#ifdef __WXOSX__
  #if wxCHECK_VERSION(3, 1, 1)
  OSXDisableAllSmartSubstitutions();
  #endif
#endif
  m_config = cfg;
  bool fixedFont = true;
  m_skipTab = true;
  wxConfigBase *config = wxConfig::Get();
  config->Read(wxT("fixedFontTC"), &fixedFont);
  if (fixedFont)
  {
    wxFont font;
#if defined (__WXOSX__)
    font = wxFont(12, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, 0, wxEmptyString);
#else
    font = wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, 0, wxEmptyString);
#endif
    wxASSERT_MSG(font.IsOk(),
                 _("Seems like something is broken with a font."));
    if(font.IsOk())
      SetFont(font);
  }
#if defined __WXGTK__
  Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(BTextCtrl::OnChar), NULL, this);
#else
  Connect(wxEVT_CHAR, wxKeyEventHandler(BTextCtrl::OnChar), NULL, this);
#endif
  #ifdef __WXOSX__
  #if wxCHECK_VERSION(3, 1, 1)
  OSXDisableAllSmartSubstitutions();
  #endif
  #endif
}


BTextCtrl::~BTextCtrl()
{}

void BTextCtrl::OnChar(wxKeyEvent &event)
{
  if (!m_config->GetMatchParens() || MatchParenthesis(event.GetUnicodeKey()))
    event.Skip();
}

bool BTextCtrl::MatchParenthesis(int code)
{
  bool skip = true;
  switch (code)
  {
    case '(':
      CloseParenthesis(wxT("("), wxT(")"), true);
      skip = false;
      break;
    case ')':
      CloseParenthesis(wxT("("), wxT(")"), false);
      skip = false;
      break;
    case '[':
      CloseParenthesis(wxT("["), wxT("]"), true);
      skip = false;
      break;
    case ']':
      CloseParenthesis(wxT("["), wxT("]"), false);
      skip = false;
      break;
    case '{':
      CloseParenthesis(wxT("{"), wxT("}"), true);
      skip = false;
      break;
    case '}':
      CloseParenthesis(wxT("{"), wxT("}"), false);
      skip = false;
      break;
    case '"':
      CloseParenthesis(wxT("\""), wxT("\""), true);
      skip = false;
      break;
    case WXK_UP:
    case WXK_DOWN:
    case WXK_TAB:
      skip = m_skipTab;
    default:
      break;
  }

  return skip;
}

void BTextCtrl::CloseParenthesis(wxString open, wxString close, bool fromOpen)
{
  long from, to;
  GetSelection(&from, &to);

  if (from == to)  // nothing selected
  {
    wxString text = GetValue();
    wxString charHere = wxT(" ");//text.GetChar((size_t)GetInsertionPoint());
    size_t insp = GetInsertionPoint();

    if (!fromOpen && charHere == close)
      SetInsertionPoint(insp + 1);
    else
    {
      wxString newtext =
              (insp > 0 ? text.SubString(0, insp - 1) : wxT("")) +
              (fromOpen ? open : wxT("")) + close +
              text.SubString(insp, text.length());

      ChangeValue(newtext);

      SetInsertionPoint(insp + 1);
    }
  }
  else
  {
    wxString text = GetValue();

    wxString newtext =
            (from > 0 ? text.SubString(0, from - 1) : wxT("")) +
            open + text.SubString(from, to - 1) + close +
            text.SubString(to, text.length());

    ChangeValue(newtext);

    if (fromOpen)
      SetInsertionPoint(from + 1);
    else
      SetInsertionPoint(to + 1);
  }
}
