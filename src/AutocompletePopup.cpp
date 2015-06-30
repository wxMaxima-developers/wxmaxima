// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

#include "AutocompletePopup.h"
#include "Dirstructure.h"
#include "wxMaximaFrame.h"

#include <wx/textfile.h>

void AutocompletePopup::UpdateResults()
{
  wxString partial = m_editor->GetSelectionString();

  m_completions = m_autocomplete->CompleteSymbol(partial, m_type);
  m_completions.Sort();
  if(m_completions.GetCount()==1)
  {
    m_editor->ReplaceSelection(
      m_editor->GetSelectionString(),
      m_completions[0]
      );
    Dismiss();
  }
  m_autocompletions->Set(m_completions);
}

void AutocompletePopup::OnKeyPress(wxKeyEvent& event)
{
#if wxUSE_UNICODE
  wxChar key = event.GetUnicodeKey();
#else
  wxChar key = wxString::Format(wxT("%c"), ChangeNumpadToChar(event.GetKeyCode()));
#endif
  
  switch (event.GetKeyCode()) {
  case WXK_TAB:
    if(m_completions.GetCount()>0)
    {
      wxChar ch;
      bool addChar = true;
      wxString word=m_editor->GetSelectionString();
      int index=word.Length();
      do
      {
        if(m_completions[0].Length()<=index)
          addChar = false;
        else
        {
          ch = m_completions[0][index];
          for(size_t i=0;i<m_completions.GetCount();i++)
            if((m_completions[i].Length()<index + 1)||(m_completions[i][index]!=ch))
              addChar = false;
        }
        
        if(addChar)
        {
          index++;
          word += ch;
        }
      }
      while(addChar);
      m_editor->ReplaceSelection(m_editor->GetSelectionString(),word,true);
    }
    break;
  case WXK_RETURN:
  case WXK_RIGHT:
  case WXK_NUMPAD_ENTER:
  {
    int selection = m_autocompletions->GetSelection();
    if(selection<0)
      selection = 0;
    
    m_editor->ReplaceSelection(
      m_editor->GetSelectionString(),
      m_completions[selection]
      );
    Dismiss();
  }
  break;
  case WXK_LEFT:
  case WXK_ESCAPE:
    Dismiss();
    break;
  case WXK_UP:
  {
    int selection = m_autocompletions->GetSelection();
    if(selection > 0)
      m_autocompletions->SetSelection(selection-1);
    else
    {
      if(m_completions.GetCount()>0)
        m_autocompletions->SetSelection(0);
    }
    break;
  }
  case WXK_DOWN:
  {
    int selection = m_autocompletions->GetSelection();
    if(selection<0) selection = 0;
    selection++;
    if(selection >= m_completions.GetCount())
      selection--;
    if(m_completions.GetCount()>0)
      m_autocompletions->SetSelection(selection);
    break;
  }
  case WXK_BACK:
  {
    wxString oldString=m_editor->GetSelectionString();
    if(oldString!=wxEmptyString)
    {
      m_editor->ReplaceSelection(
        oldString,
        oldString.Left(oldString.Length()-1),
        true
        );
      UpdateResults();
    }
    else
      Dismiss();
    break;
  }
  default:
  {
    if((key==wxT(' '))||(key==wxT('(')))
    {
      int selection = m_autocompletions->GetSelection();
      if(selection<0)
        selection = 0;
      
      m_editor->ReplaceSelection(
        m_editor->GetSelectionString(),
        m_completions[selection]+key
        );
      Dismiss();
    }
    else if((wxIsalpha(key))||(key==wxT('_')))
    {
      wxString oldString=m_editor->GetSelectionString();
      m_editor->ReplaceSelection(
        oldString,
        oldString+wxString(key),
        true
        );
      UpdateResults();
    }
    else
      event.Skip();
  }
  }
  this->GetParent()->GetParent()->Refresh();
}

void AutocompletePopup::OnClick(wxCommandEvent& event)
{
  int selection = event.GetSelection();
  if(selection > 0)
  {
    m_editor->ReplaceSelection(
      m_editor->GetSelectionString(),
      m_completions[selection]
      );
    this->GetParent()->GetParent()->Refresh();
    Dismiss();
  }
}

AutocompletePopup::AutocompletePopup(
  wxWindow *parent,
  EditorCell* editor,
  AutoComplete * autocomplete,
  AutoComplete::autoCompletionType type
  ) : wxPopupTransientWindow(parent,-1)
{
  m_autocomplete = autocomplete;
  m_editor       = editor;
  m_type         = type;
  m_length       = 0;
  m_autocompletions = new wxListBox(this, -1);
  
  m_autocompletions->Connect(wxEVT_LISTBOX,
                             wxCommandEventHandler(AutocompletePopup::OnClick),
                             NULL, this);
  m_autocompletions->Connect(wxEVT_CHAR,
                             wxKeyEventHandler(AutocompletePopup::OnKeyPress),
                             NULL, this);
wxFlexGridSizer *box = new wxFlexGridSizer(1);
  UpdateResults();
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);
  box->Add(m_autocompletions, 0, wxEXPAND | wxALL, 0);
  SetSizerAndFit(box);
}
