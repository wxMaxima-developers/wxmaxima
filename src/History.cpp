///
///  Copyright (C) 2009-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "History.h"

#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <wx/regex.h>

History::History(wxWindow* parent, int id) : wxPanel(parent, id)
{
  m_history = new wxListBox(this, history_ctrl_id);
  m_regex = new wxTextCtrl(this, history_regex_id);
  wxFlexGridSizer * box = new wxFlexGridSizer(1);
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);

  box->Add(m_history, 0, wxEXPAND | wxALL, 0);
  box->Add(m_regex, 0, wxEXPAND | wxALL, 1);

  SetSizer(box);
  box->Fit(this);
  box->SetSizeHints(this);
  m_current = 0;
}

History::~History()
{
  //TODO: Load/save history?
}

void History::AddToHistory(wxString cmd)
{
  wxString lineends = wxT(";$");
  if (cmd.StartsWith(wxT(":lisp")))
    lineends = wxT(";");

  wxStringTokenizer cmds(cmd, lineends);

  while (cmds.HasMoreTokens())
  {
    wxString curr = cmds.GetNextToken().Trim(false).Trim(true);

    if (curr != wxEmptyString)
      commands.Insert(curr, 0);
  }

  m_current = commands.GetCount();

  UpdateDisplay();
}

void History::UpdateDisplay()
{
  wxLogNull disableWarnings;

  wxString regex = m_regex->GetValue();
  wxArrayString display;
  wxRegEx matcher;

  if (regex != wxEmptyString)
    matcher.Compile(regex);

  for (unsigned int i=0; i<commands.Count(); i++)
  {
    wxString curr = commands.Item(i);

    if (regex.Length()>0 && matcher.IsValid())
    {
      if (matcher.Matches(curr))
        display.Add(curr);
    }
    else
      display.Add(curr);
  }

  m_history->Set(display);
}

void History::OnRegExEvent(wxCommandEvent &ev)
{
  UpdateDisplay();
}

wxString History::GetCommand(bool next)
{
  if (commands.GetCount() == 0)
    return wxEmptyString;

  else if (next)
  {
    --m_current;
    if (m_current < 0)
      m_current = commands.GetCount()-1;
    return commands[m_current];
  }
  else
  {
    ++m_current;
    if (m_current >= commands.GetCount())
      m_current = 0;
    return commands[m_current];
  }
}

BEGIN_EVENT_TABLE(History, wxPanel)
  EVT_TEXT(history_regex_id, History::OnRegExEvent)
END_EVENT_TABLE()
