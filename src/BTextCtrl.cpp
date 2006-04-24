///
///  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "BTextCtrl.h"
#include <wx/config.h>

BTextCtrl::BTextCtrl(wxWindow *parent,
                     wxWindowID id,
                     const wxString& value,
                     const wxPoint& pos,
                     const wxSize& size,
                     long style)
    : wxTextCtrl(parent, id, value, pos, size, style)
{
  bool fixedFont = true;
  m_matchParens = true;
  wxConfigBase *config = wxConfig::Get();
  config->Read(wxT("matchParens"), &m_matchParens);
  config->Read(wxT("fixedFontTC"), &fixedFont);
  if (fixedFont)
  {
#if defined (__WXGTK12__) && !defined (__WXGTK20__)
    SetFont(wxFont(12, wxMODERN, wxNORMAL, wxNORMAL, 0, wxEmptyString));
#elif defined (__WXMAC__)
    SetFont(wxFont(12, wxMODERN, wxNORMAL, wxNORMAL, 0, wxEmptyString));
#else
    SetFont(wxFont(10, wxMODERN, wxNORMAL, wxNORMAL, 0, wxEmptyString));
#endif

  }
}


BTextCtrl::~BTextCtrl()
{}

void BTextCtrl::OnChar(wxKeyEvent& event)
{
  if (!m_matchParens || MatchParenthesis(event.GetKeyCode()))
    event.Skip();
}

bool BTextCtrl::MatchParenthesis(int code)
{
  bool skip = true;
  switch (code)
  {
  case '(':
    CloseParenthesis(wxT("("), wxT(")"));
    skip = false;
    break;
  case '[':
    CloseParenthesis(wxT("["), wxT("]"));
    skip = false;
    break;
  case '{':
    CloseParenthesis(wxT("{"), wxT("}"));
    skip = false;
    break;
  case '"':
    CloseParenthesis(wxT("\""), wxT("\""));
    skip = false;
    break;
  case WXK_UP:
  case WXK_DOWN:
  case WXK_TAB:
    skip = false;
  default:
    break;
  }

  return skip;
}

void BTextCtrl::CloseParenthesis(wxString open, wxString close)
{
  long from, to;
  GetSelection(&from, &to);

  if (from == to)
  {
    WriteText(open + close);
    SetInsertionPoint(GetInsertionPoint() - 1);
  }
  else
  {
    SetInsertionPoint(to);
    WriteText(close);
    SetInsertionPoint(from);
    WriteText(open);
    SetInsertionPoint(to + 1);
  }
}

BEGIN_EVENT_TABLE(BTextCtrl, wxTextCtrl)
  EVT_CHAR(BTextCtrl::OnChar)
END_EVENT_TABLE()
