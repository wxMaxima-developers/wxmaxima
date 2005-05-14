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
  m_historyIndex = 1;
  m_matchParens = true;
  wxConfig::Get()->Read(wxT("matchParens"), &m_matchParens);
}


CommandLine::~CommandLine()
{
}

int CommandLine::AddToHistory(wxString s)
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
    m_history.push_back(s);
  m_historyIndex = m_history.size();
  return 0;
}

wxString CommandLine::Next()
{
  if (m_history.size()==0)
    return wxT("");
  if (m_historyIndex+1 < (int)m_history.size()) {
    m_historyIndex++;
    return m_history[m_historyIndex];
  }
  else if (m_historyIndex+1 == (int)m_history.size()) {
    m_historyIndex++;
    return wxT("");
  }
  else {
    m_historyIndex = 0;
    return m_history[m_historyIndex];
  }
  return wxT("");
}

wxString CommandLine::Previous()
{
  if (m_history.size()==0)
    return wxT("");
  if (m_historyIndex>0) {
    m_historyIndex--;
    return m_history[m_historyIndex];
  }
  else {
    m_historyIndex = m_history.size();
    return wxT("");
  }
  return wxT("");
}

wxString CommandLine::Complete(wxString s)
{
  int i;
  if (m_history.size()==0)
    return s;
  if (m_historyIndex < 0)
    m_historyIndex = m_history.size();
  for (i = m_historyIndex-1; i>=0; i--)
    if (m_history[i].StartsWith(s) ||
        m_history[i].StartsWith(wxT("<ml>") + s))
      break;
  if (i>=0) {
    m_historyIndex = i;
    return m_history[i];
  }
  for (i = m_history.size()-1; i >= m_historyIndex; i--)
    if (m_history[i].StartsWith(s) ||
        m_history[i].StartsWith(wxT("<ml>") + s))
      break;
  if (i >= m_historyIndex) {
    m_historyIndex = i;
    return m_history[i];
  }
  return s;
}

void CommandLine::FilterLine(wxKeyEvent& event)
{
  long from, to;
  GetSelection(&from, &to);
  switch(event.GetKeyCode()) {
  case WXK_UP:
    m_marked = -1;
    SetValue(Previous());
#if defined (__WXMSW__)
    SetStyle(0, GetLastPosition(),
             wxTextAttr(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)));
#endif
    SetInsertionPointEnd();
    return;
    break;
  case WXK_DOWN:
    m_marked = -1;
    SetValue(Next());
#if defined (__WXMSW__)
    SetStyle(0, GetLastPosition(),
             wxTextAttr(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)));
#endif
    SetInsertionPointEnd();
    return;
    break;
  case WXK_TAB:
    {
      m_marked = -1;
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
      wxString com = Complete(s);
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
    case '{':
      if (from==to) {
        WriteText(wxT("{}"));
        SetInsertionPoint(GetInsertionPoint()-1);
      }
      else {
        SetInsertionPoint(to);
        WriteText(wxT("}"));
        SetInsertionPoint(from);
        WriteText(wxT("{"));
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

void CommandLine::Highligth(wxKeyEvent& event)
{
#if defined(__WXMSW__)
  wxString value = GetValue();
  if (value == wxEmptyString)
    return;
  long curr = GetInsertionPoint();
  if (curr == 0)
    return;
  
  char cl = value.GetChar(curr-1);
  char op;
  int j;

  if (m_marked >=0) {
    SetStyle(m_marked, m_marked+1,
             wxTextAttr(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)));
    m_marked = -1;
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
    m_marked = j;
    SetStyle(m_marked, m_marked+1, wxTextAttr(*wxBLUE));
  }
#endif
}

BEGIN_EVENT_TABLE(CommandLine, wxTextCtrl)
  EVT_CHAR(CommandLine::FilterLine)
  EVT_KEY_UP(CommandLine::Highligth)
END_EVENT_TABLE()
