// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2018 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class AutocompletePopup.

  The content assistant offers more functionality than AutocompletePopup but 
  only works on systems that allow popups to handle key presses.
*/

#include "AutocompletePopup.h"
#include "Dirstructure.h"
#include "wxMaximaFrame.h"

#include <wx/textfile.h>
#include <wx/combo.h>
#include <wx/listctrl.h>

void AutocompletePopup::UpdateResults()
{
  m_completions = m_autocomplete->CompleteSymbol(m_partial, m_type);
  m_completions.Sort();

  switch (m_completions.GetCount())
  {
  case 1:
    if(m_type != AutoComplete::esccommand)
    {
      m_editor->ReplaceSelection(
        m_editor->GetSelectionString(),
        m_completions[0]
        );
      m_editor->ClearSelection();
    }
    else
      m_editor->InsertEscCommand(m_completions[0]);

    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
    break;
  case 0:
    if(m_type != AutoComplete::esccommand)
    {
      m_editor->ClearSelection();
      m_parent->GetParent()->Refresh();
    }
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
    break;
  default:
    DeleteAllItems();
    for(unsigned int i=0; i < m_completions.GetCount(); i++)
      InsertItem(i, m_completions[i]);

    Select(0);Focus(0);
  }
}

void AutocompletePopup::OnKeyDown(wxKeyEvent &event)
{
  switch (event.GetKeyCode())
  {
  case WXK_TAB:
    if (m_completions.GetCount() > 0)
    {
      wxChar ch;
      bool addChar = true;
      wxString word = m_editor->GetSelectionString();
      size_t index = word.Length();
      do
      {
        if (m_completions[0].Length() <= index)
          addChar = false;
        else
        {
          ch = m_completions[0][index];
          for (size_t i = 0; i < m_completions.GetCount(); i++)
            if ((m_completions[i].Length() < index + 1) || (m_completions[i][index] != ch))
              addChar = false;
        }

        if (addChar)
        {
          index++;
          word += ch;
        }
      } while (addChar);
      m_partial = word;
      if(m_type != AutoComplete::esccommand)  
        m_editor->ReplaceSelection(m_editor->GetSelectionString(), m_partial, true);
    }
    break;
  case WXK_RETURN:
  case WXK_RIGHT:
  case WXK_NUMPAD_ENTER:
  {
    int selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                wxLIST_STATE_SELECTED);
    if (selection < 0)
      selection = 0;

    if (m_completions.GetCount() > 0)
    {
      if(m_type != AutoComplete::esccommand)
        m_editor->ReplaceSelection(
          m_editor->GetSelectionString(),
          m_completions[selection]
          );
      else
        m_editor->InsertEscCommand(m_completions[selection]);
    }
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
  }
  break;
  case WXK_LEFT:
  case WXK_ESCAPE:
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
    break;
  case WXK_UP:
  case WXK_NUMPAD_UP:
  {
    int selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                wxLIST_STATE_SELECTED);
    if (selection > 0)
    {
      Select(selection - 1);Focus(selection - 1);
    }
    else
    {
      if (m_completions.GetCount() > 0)
      {
        Select(0);Focus(0);
      }
    }
    break;
  }
  case WXK_PAGEUP:
  case WXK_NUMPAD_PAGEUP:
  {
    int selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                wxLIST_STATE_SELECTED);
    selection -= 8;
    if (selection < 0)
      selection = 0;
    
    if (m_completions.GetCount() > 0)
    {
      Select(selection);Focus(selection);
    }
    
    break;
  }
  case WXK_PAGEDOWN:
  case WXK_NUMPAD_PAGEDOWN:
  {
    int selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                wxLIST_STATE_SELECTED);
    selection += 8;
    if ((unsigned) selection >= m_completions.GetCount())
      selection = m_completions.GetCount() - 1;

    if (m_completions.GetCount() > 0)
    {
      Select(selection);
      Focus(selection);
    }
    break;
  }
  case WXK_DOWN:
  case WXK_NUMPAD_DOWN:
  {
    long selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                 wxLIST_STATE_SELECTED);
    if (selection < 0) selection = 0;
    selection++;
    if (selection >= (long) m_completions.GetCount())
      selection--;
    if (m_completions.GetCount() > 0)
    {
      Select(selection);Focus(selection);
    }
    break;
  }
  case WXK_BACK:
  case WXK_NUMPAD_DELETE:
  {
    wxString oldString = m_partial;
    if(m_partial != wxEmptyString)
      m_partial = m_partial.Left(m_partial.Length() - 1);
    if (oldString != wxEmptyString)
    {
      UpdateResults();
      
      if(m_type != AutoComplete::esccommand)
        m_editor->ReplaceSelection(oldString,m_partial,true);
    }
    else
      m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();

    *m_doneptr = NULL;
    Destroy();
    break;
  }
  default:
    event.Skip();
  }
  m_parent->GetParent()->Refresh();
}

bool AutocompletePopup::Create(wxWindow* parent)
{
  bool retval = wxListView::Create(parent,1,m_position,wxDefaultSize,
                                   wxLC_ALIGN_LEFT |
                                   wxLC_REPORT |
                                   wxLC_NO_HEADER |
                                   wxLC_SINGLE_SEL);
  InsertColumn(0,wxEmptyString);
  UpdateResults();
  SetColumnWidth(0, wxLIST_AUTOSIZE);

  wxSize minSize;
  wxSize optimumSize = wxSize(-1,0);
  for (size_t i = 0; i < m_completions.GetCount(); i++)
  {
    wxRect itemRect;
    if(GetItemRect(i, itemRect))
    {
      if(optimumSize.x < itemRect.GetWidth())
        optimumSize.x = itemRect.GetWidth();
      
      optimumSize.y += itemRect.GetHeight();
    }
  }
  minSize = optimumSize;
  if (minSize.y > 300) minSize.y = 300;
  SetMinSize(minSize);
  SetClientSize(minSize);
  Layout();
  //SetOptimumSize(optimumSize);
  return retval;
}

void AutocompletePopup::OnClick(wxMouseEvent& WXUNUSED(event))
{
  if (GetItemCount() <= 0)
    return;

  m_value = wxListView::GetFirstSelected();

  {
    if (m_value < 0)
      m_value = 0;
    m_partial = m_completions[m_value];
    if(m_type != AutoComplete::esccommand)
      m_editor->ReplaceSelection(
        m_editor->GetSelectionString(),
        m_partial
        );
    else
      m_editor->InsertEscCommand(m_partial);
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
  }
}

void AutocompletePopup::OnDismiss()
{
  *m_doneptr = NULL;
}

void AutocompletePopup::OnClose(wxCloseEvent& WXUNUSED(event))
{
  *m_doneptr = NULL;
}

AutocompletePopup::~AutocompletePopup()
{
  GetParent()->SetFocus();
  *m_doneptr = NULL;
}

AutocompletePopup::AutocompletePopup(
  wxWindow *parent, EditorCell *editor, AutoComplete *autocomplete,
  AutoComplete::autoCompletionType type,
  AutocompletePopup **doneptr): wxListView(), wxComboPopup()
{
  m_parent = parent;
  m_doneptr = doneptr;
  m_autocomplete = autocomplete;
  m_editor = editor;
  m_type = type;
  m_length = 0;

  if(m_type != AutoComplete::esccommand)
    m_partial = m_editor->GetSelectionString();
      
  Connect(wxEVT_CHAR,
          wxKeyEventHandler(AutocompletePopup::OnChar),
          NULL, this);
  Connect(wxEVT_KEY_DOWN,
          wxKeyEventHandler(AutocompletePopup::OnKeyDown),
          NULL, this);
}

void AutocompletePopup::OnChar(wxKeyEvent &event)
{
  wxChar key = event.GetUnicodeKey();
  if (
    ((m_type == AutoComplete::esccommand) && wxIsprint(key)) ||
    ((wxIsalnum(key)) || (key == wxT('_')) || (key == wxT('\"')) ||
     (
       (
          (m_type == AutoComplete::generalfile) ||
          (m_type == AutoComplete::loadfile) ||
          (m_type == AutoComplete::demofile)
          ) &&
        (key == wxT('/'))
               )
        )
    )
  {
    wxString oldString = m_editor->GetSelectionString();
    m_partial += wxString(key);
    if(m_type != AutoComplete::esccommand)
      m_editor->ReplaceSelection(
        oldString,
        m_partial,
        true
        );
    UpdateResults();
    return;
  }
  else if (wxIsprint(key))
  {
    int selection = GetNextItem(0,wxLIST_NEXT_ALL,
                                wxLIST_STATE_SELECTED);

    // The current key is no more part of the current command
    //
    // => Add the current selection to the worksheet and handle this keypress normally.
    if (selection < 0)
      selection = 0;
        
    m_editor->ReplaceSelection(
      m_editor->GetSelectionString(),
      m_completions[selection]
      );
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    *m_doneptr = NULL;
    Destroy();
        
    // Tell MathCtrl to handle this key event the normal way.
    wxKeyEvent *keyEvent = new wxKeyEvent(event);
    m_parent->GetEventHandler()->QueueEvent(keyEvent);
    return;
  }
}

wxBEGIN_EVENT_TABLE(AutocompletePopup, wxListView)
EVT_LEFT_UP(AutocompletePopup::OnClick)
EVT_CLOSE(AutocompletePopup::OnClose)
wxEND_EVENT_TABLE()


