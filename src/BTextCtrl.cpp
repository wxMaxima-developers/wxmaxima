/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */



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
  matchParens = true;
  wxConfigBase *config = wxConfig::Get();
  config->Read(wxT("matchParens"), &matchParens);
  marked = -1;
#if defined(__WXGTK12__) && !defined(__WXGTK20__)
  SetFont(wxFont(12, wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("")));
#else
  SetFont(wxFont(10, wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("")));
#endif
}


BTextCtrl::~BTextCtrl()
{
}

void BTextCtrl::onChar(wxKeyEvent& event)
{
  long from, to;
  if (!matchParens) {
    event.Skip();
    return;
  }
  GetSelection(&from, &to);
  switch (event.GetKeyCode()) {
    case '(':
    if (from==to) {
      WriteText(wxT("()"));
      SetInsertionPoint(GetInsertionPoint()-1);
    }
    else {
      SetInsertionPoint(to);
      WriteText(wxT(")"));
      SetInsertionPoint(from);
      WriteText(wxT("("));
      SetInsertionPoint(to+1);
    }
    break;
  case '[':
    if (from==to) {
      WriteText(wxT("[]"));
      SetInsertionPoint(GetInsertionPoint()-1);
    }
    else {
      SetInsertionPoint(to);
      WriteText(wxT("]"));
      SetInsertionPoint(from);
      WriteText(wxT("["));
      SetInsertionPoint(to+1);
    }
    break;
  case '"':
    if (from==to) {
      WriteText(wxT("\"\""));
      SetInsertionPoint(GetInsertionPoint()-1);
    }
    else {
      SetInsertionPoint(to);
      WriteText(wxT("\""));
      SetInsertionPoint(from);
      WriteText(wxT("\""));
      SetInsertionPoint(to+1);
    }
    break;
  default:
    event.Skip();
  }
}

BEGIN_EVENT_TABLE(BTextCtrl, wxTextCtrl)
  EVT_CHAR(BTextCtrl::onChar)
END_EVENT_TABLE()
