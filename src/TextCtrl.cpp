///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "TextCtrl.h"
#include <wx/config.h>

TextCtrl::TextCtrl(wxWindow *parent, wxWindowID id, const wxString& value,
                   const wxPoint& pos, const wxSize& size,
                   long style) : wxTextCtrl(parent, id, value, pos, size, style)
{
  m_matchParens = true;
  wxConfig::Get()->Read(wxT("matchParens"), &m_matchParens);
}


TextCtrl::~TextCtrl()
{}

void TextCtrl::OnChar(wxKeyEvent& event)
{
  long from, to;
  GetSelection(&from, &to);
  switch (event.GetKeyCode())
  {
  case WXK_RETURN:
    {
      long int c, l;
      PositionToXY(GetInsertionPoint(), &c, &l);
      wxString line = GetLineText(l);
      WriteText(wxT("\n"));
      for (int i = 0; i < (int)line.Length(); i++)
      {
        if (line.GetChar(i) != ' ')
          break;
        WriteText(wxT(" "));
      }
    }
    return ;
    break;
  case WXK_TAB:
    {
      long int c, l;
      PositionToXY(GetInsertionPoint(), &c, &l);
      do
      {
        WriteText(wxT(" "));
        c++;
      }
      while (c % 4 != 0);
      SetFocus();
    }
    return ;
    break;
  default:
    break;
  }
  if (m_matchParens)
  {
    switch (event.GetKeyCode())
    {
    case '(':
      if (from == to)
      {
        WriteText(wxT("()"));
        SetInsertionPoint(GetInsertionPoint() - 1);
      }
      else
      {
        SetInsertionPoint(to);
        WriteText(wxT(")"));
        SetInsertionPoint(from);
        WriteText(wxT("("));
        SetInsertionPoint(to + 1);
      }
      break;
    case '[':
      if (from == to)
      {
        WriteText(wxT("[]"));
        SetInsertionPoint(GetInsertionPoint() - 1);
      }
      else
      {
        SetInsertionPoint(to);
        WriteText(wxT("]"));
        SetInsertionPoint(from);
        WriteText(wxT("["));
        SetInsertionPoint(to + 1);
      }
      break;
    case '"':
      if (from == to)
      {
        WriteText(wxT("\"\""));
        SetInsertionPoint(GetInsertionPoint() - 1);
      }
      else
      {
        SetInsertionPoint(to);
        WriteText(wxT("\""));
        SetInsertionPoint(from);
        WriteText(wxT("\""));
        SetInsertionPoint(to + 1);
      }
      break;
    default:
      event.Skip();
    }
  }
  else
    event.Skip();
}

BEGIN_EVENT_TABLE(TextCtrl, wxTextCtrl)
  EVT_CHAR(TextCtrl::OnChar)
END_EVENT_TABLE()
