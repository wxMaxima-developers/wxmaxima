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

#include "CommandLine.h"
#include <wx/config.h>

CommandLine::CommandLine(wxWindow *parent,
                         wxWindowID id,
                         const wxString& value,
                         const wxPoint& pos,
                         const wxSize& size,
                         long style) : wxTextCtrl(parent, id, value, pos, size, style)
{
  history_index = 1;
  m_matchParens = true;
  wxConfigBase* config = wxConfig::Get();
  config->Read(wxT("matchParens"), &m_matchParens);
}


CommandLine::~CommandLine()
{
}

int CommandLine::addToHistory(wxString s)
{
  if (s.Last() == ';')
    s.RemoveLast();
  if (s.Find(wxT("\n"))>-1) {
    s.Replace(wxT("\n"), wxT("<nl>"));
    s = wxT("<ml>") + s;
  }
  s.Trim();
  s.Trim(false);
  if (s.Length()!=0)
    history.push_back(s);
  history_index = history.size();
  return 0;
}

wxString CommandLine::next()
{
  if (history.size()==0)
    return wxT("");
  if (history_index+1 < (int)history.size()) {
    history_index++;
    return history[history_index];
  }
  else if (history_index+1 == (int)history.size()) {
    history_index++;
    return wxT("");
  }
  else {
    history_index = 0;
    return history[history_index];
  }
  return wxT("");
}

wxString CommandLine::previous()
{
  if (history.size()==0)
    return wxT("");
  if (history_index>0) {
    history_index--;
    return history[history_index];
  }
  else {
    history_index = history.size();
    return wxT("");
  }
  return wxT("");
}

wxString CommandLine::complete(wxString s)
{
  int i;
  if (history.size()==0)
    return s;
  if (history_index < 0)
    history_index = history.size();
  for (i = history_index-1; i>=0; i--)
    if (history[i].StartsWith(s) ||
        history[i].StartsWith(wxT("<ml>") + s))
      break;
  if (i>=0) {
    history_index = i;
    return history[i];
  }
  for (i = history.size()-1; i >= history_index; i--)
    if (history[i].StartsWith(s) ||
        history[i].StartsWith(wxT("<ml>") + s))
      break;
  if (i >= history_index) {
    history_index = i;
    return history[i];
  }
  return s;
}

void CommandLine::filterLine(wxKeyEvent& event)
{
  long from, to;
  GetSelection(&from, &to);
  switch(event.GetKeyCode()) {
  case WXK_UP:
    marked = -1;
    SetValue(previous());
#if defined (__WXMSW__)
    SetStyle(0, GetLastPosition(), wxTextAttr(*wxBLACK));
#endif
    SetInsertionPointEnd();
    return;
    break;
  case WXK_DOWN:
    marked = -1;
    SetValue(next());
#if defined (__WXMSW__)
    SetStyle(0, GetLastPosition(), wxTextAttr(*wxBLACK));
#endif
    SetInsertionPointEnd();
    return;
    break;
  case WXK_TAB:
    {
      marked = -1;
      wxString s = GetValue();
      long int l = GetInsertionPoint();
      long int l1,l2;
      GetSelection(&l1, &l2);
      if (l1!=l2)
        l = l1;
      if (s.StartsWith(wxT("<ml>"))) {
        s = s.SubString(4, l-1);
        l = l-4;
      }
      else
        s = s.SubString(0, l-1);
      wxString com = complete(s);
      if (com != s)
        SetValue(com);
      if (com.Find(wxT("<ml>"))>-1)
        l = l+4;
      SetInsertionPoint(l-1);
      SetSelection(l, GetLastPosition());
    }
    return;
    break;
  default:
    break;
  }
  if (m_matchParens) {
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
      break;
    }
  }
  else
    event.Skip();
}

void CommandLine::highligth(wxKeyEvent& event)
{
#if defined (__WXMSW__)
  long curr = GetInsertionPoint();
  wxString value = GetValue();
  char cl = value.GetChar(curr-1);
  char op;
  int j;

  if (marked >=0) {
    SetStyle(marked, marked+1, wxTextAttr(*wxBLACK));
    marked = -1;
  }
  if (cl == ')')
    op = '(';
  else if (cl == ']')
    op = '[';
  else if (cl == '"')
    op = '"';
  else
    return;
  int depth = 0;
  for (j = curr-2; j>=0; j--) {
    if (value.GetChar(j) == cl)
      depth++;
    else if (value.GetChar(j) == op) {
      if (depth == 0)
        break;
      else
        depth--;
    }
  }
  if (j>=0) {
    marked = j;
    SetStyle(marked, marked+1, wxTextAttr(*wxBLUE));
  }
#endif
}

BEGIN_EVENT_TABLE(CommandLine, wxTextCtrl)
  EVT_CHAR(CommandLine::filterLine)
  EVT_KEY_UP(CommandLine::highligth)
END_EVENT_TABLE()
