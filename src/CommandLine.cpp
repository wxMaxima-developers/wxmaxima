/*
 *  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#define MIN(a,b) (a)<(b)?(a):(b)
#define MAX(a,b) (a)<(b)?(b):(a)

#if defined __WXMSW__

#define NUM_OF_COLOURS 10
const wxString hl_colours[] = {
  wxT("RED"), wxT("BLUE"), wxT("PURPLE"), wxT("ORANGE"),
  wxT("ORANGE RED"), wxT("GOLD"), wxT("NAVY"),
  wxT("DARK GREEN"), wxT("SEA GREEN"), wxT("YELLOW GREEN"), wxT("BLACK")
};

#endif

CommandLine::CommandLine(wxWindow *parent,
                         wxWindowID id,
                         const wxString& value,
                         const wxPoint& pos,
                         const wxSize& size,
                         long style) : BTextCtrl(parent, id, value, pos, size, style)
{
  m_historyIndex = 1;
  m_historySize = 0;
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
  m_historySize = m_history.size();
  m_historyIndex = m_historySize;
  m_currentValue = wxEmptyString;
  m_currentDisplayedValue = wxEmptyString;
  return 0;
}

wxString CommandLine::Next()
{
  if (m_history.size()==0)
    return wxEmptyString;
  m_historyIndex = MIN(m_historyIndex+1, m_historySize);
  if (m_historyIndex == m_historySize)
    return m_currentValue;
  return m_history[m_historyIndex];
}

wxString CommandLine::Previous()
{
  if (m_history.size()==0)
    return wxEmptyString;

  m_historyIndex = MAX(m_historyIndex-1, 0);
  return m_history[m_historyIndex];
}

wxString CommandLine::Complete(wxString s)
{
  int i;
  if (m_historySize==0)
    return s;
  if (m_historyIndex < 0)
    m_historyIndex = m_historySize;
  for (i = m_historyIndex-1; i>=0; i--)
    if (m_history[i].StartsWith(s) ||
        m_history[i].StartsWith(wxT("<ml>") + s))
      break;
  if (i>=0) {
    m_historyIndex = i;
    return m_history[i];
  }
  for (i = m_historySize-1; i >= m_historyIndex; i--)
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
  switch(event.GetKeyCode()) {
  case WXK_UP:
    m_currentHistoryValue = Previous();
    m_currentDisplayedValue = m_currentHistoryValue;
    SetValue(m_currentHistoryValue);
    SetInsertionPointEnd();
    break;
  case WXK_DOWN:
    m_currentHistoryValue = Next();
    m_currentDisplayedValue = m_currentHistoryValue;
    SetValue(m_currentHistoryValue);
    SetInsertionPointEnd();
    break;
  case WXK_TAB:
    {
      if (event.AltDown())
        return;
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
      wxString comp = Complete(s);
      if (comp != s) {
        m_currentDisplayedValue = comp;
        m_currentHistoryValue = comp;
        SetValue(comp);
      }
      if (comp.Find(wxT("<ml>"))>-1)
        l = l+4;
      SetInsertionPoint(l-1);
      SetSelection(l, GetLastPosition());
    }
    break;
  default:
    {
      wxString value = GetValue();
      if (m_currentDisplayedValue != value) {
        m_currentValue = value;
        m_historyIndex = m_historySize;
      }
      break;
    }
  }
#if defined __WXMSW__
  DoHighlight();
#endif
}

#if defined __WXMSW__

void CommandLine::DoHighlight()
{
  wxString value = GetValue();
  if (value == wxEmptyString)
    return;

  Freeze();

  SetStyle(0, value.Length(),
           wxTextAttr(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)));

  int depth = 1;
  for (int i=0; i<value.Length(); i++) {
    if (value.GetChar(i) == '(' || value.GetChar(i) == '[') {
      if (depth < 1)
        depth = 1;
      SetStyle(i, i+1,
               wxTextAttr(wxTheColourDatabase->Find(hl_colours[MIN(NUM_OF_COLOURS, depth)])));
      depth++;
    }
    else if (value.GetChar(i) == ')' || value.GetChar(i) == ']') {
      if (depth > 0)
        depth--;
      SetStyle(i, i+1,
               wxTextAttr(wxTheColourDatabase->Find(hl_colours[MIN(NUM_OF_COLOURS, depth)])));
    }
  }

  Thaw();
}

void CommandLine::SetValue(wxString s)
{
  Freeze();
  wxTextCtrl::SetValue(s);
  DoHighlight();
  Thaw();
}

void CommandLine::WriteText(wxString s)
{
  Freeze();
  wxTextCtrl::WriteText(s);
  DoHighlight();
  Thaw();
}

#endif

BEGIN_EVENT_TABLE(CommandLine, BTextCtrl)
  EVT_KEY_UP(CommandLine::FilterLine)
END_EVENT_TABLE()
