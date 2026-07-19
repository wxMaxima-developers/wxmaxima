// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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

#include <wx/combo.h>
#include <wx/listctrl.h>
#include <wx/textfile.h>
#include <wx/wupdlock.h>
#include <algorithm>

void AutocompletePopup::UpdateResults(bool allowAutoFinish) {
  // The editor we are completing into may have been destroyed while this popup
  // was open (m_editor is a CellPtr and auto-nulls). If so there is nothing to
  // complete; our callers dismiss the popup.
  if (!m_editor)
    return;
  m_completions = m_autocomplete->CompleteSymbol(m_partial, m_type);
  std::sort(m_completions.begin(), m_completions.end());

  switch (m_completions.size()) {
  case 1:
    if (((m_type == AutoComplete::esccommand) && (m_partial.Length() < 2)) ||
        !allowAutoFinish) {
      DeleteAllItems();
      for (unsigned int i = 0; i < m_completions.size(); i++)
        InsertItem(i, m_completions.at(i));

      Select(0);
      Focus(0);
      break;
    }

    // A single directory match: descend into it and keep completing,
    // bash-style, instead of finishing the completion.
    if (CompletesFiles() && IsDirectoryCompletion(m_completions.at(0)) &&
        DescendIntoDirectory(m_completions.at(0)))
      return;

    if (m_type != AutoComplete::esccommand) {
      m_editor->ReplaceSelection(m_editor->GetSelectionString(),
                                 m_completions.at(0));
      m_editor->ClearSelection();
    } else
      m_editor->InsertEscCommand(m_completions.at(0));

    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    return void(Destroy());
  case 0:
    if (m_type != AutoComplete::esccommand) {
      m_editor->ClearSelection();
      m_parent->GetParent()->Refresh();
    }
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    return void(Destroy());
  default:
    DeleteAllItems();
    for (unsigned int i = 0; i < m_completions.size(); i++)
      InsertItem(i, m_completions.at(i));

    Select(0);
    Focus(0);
  }
}

void AutocompletePopup::OnKeyDown(wxKeyEvent &event) {
  // If the editor was destroyed while the popup was open, dismiss the popup
  // rather than dereference a freed cell.
  if (!m_editor)
    return void(Destroy());
  switch (event.GetKeyCode()) {
  case WXK_TAB:
    if (m_completions.size() > 0) {
      wxChar ch = 0;
      bool addChar = true;
      wxString word = m_editor->GetSelectionString();
      std::size_t index = word.Length();
      do {
        if (m_completions.at(0).Length() <= index)
          addChar = false;
        else {
          ch = m_completions.at(0).at(index);
          for (std::size_t i = 0; i < m_completions.size(); i++)
            if ((m_completions.at(i).Length() < index + 1) ||
                (m_completions.at(i).at(index) != ch))
              addChar = false;
        }

        if (addChar) {
          index++;
          word += ch;
        }
      } while (addChar);
      m_partial = word;
      if (m_type != AutoComplete::esccommand)
        m_editor->ReplaceSelection(m_editor->GetSelectionString(), m_partial,
                                   true);
      // The common prefix may end right at a directory boundary; make sure
      // that directory's contents are on offer.
      if (CompletesFiles() && m_partial.EndsWith(wxS("/"))) {
        m_autocomplete->UpdateFiles(m_type, m_partial, m_fileBaseDir);
        UpdateResults();
      }
    }
    break;
  case WXK_RETURN:
  case WXK_RIGHT:
  case WXK_NUMPAD_ENTER: {
    int selection = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
    if (selection < 0)
      selection = 0;

    if (m_completions.size() > 0) {
      // Accepting a directory descends into it and continues completing
      // there, bash-style.
      if (CompletesFiles() &&
          IsDirectoryCompletion(m_completions.at(selection)) &&
          DescendIntoDirectory(m_completions.at(selection))) {
        m_parent->GetParent()->Refresh();
        break;
      }
      if (m_type != AutoComplete::esccommand)
        m_editor->ReplaceSelection(m_editor->GetSelectionString(),
                                   m_completions.at(selection));
      else
        m_editor->InsertEscCommand(m_completions.at(selection));
    }
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    return void(Destroy());
  }
  case WXK_LEFT:
  case WXK_ESCAPE:
    if ((m_type == AutoComplete::esccommand) && (m_partial != wxEmptyString)) {
      int selection = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
      if (selection < 0)
        selection = 0;
      m_editor->InsertEscCommand(m_completions.at(selection));
    }
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();
    return void(Destroy());
  case WXK_UP:
  case WXK_NUMPAD_UP: {
    int selection = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
    if (selection > 0) {
      Select(selection - 1);
      Focus(selection - 1);
    } else {
      if (m_completions.size() > 0) {
        Select(0);
        Focus(0);
      }
    }
    break;
  }
  case WXK_PAGEUP:
  case WXK_NUMPAD_PAGEUP: {
    int selection = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
    selection -= 8;
    if (selection < 0)
      selection = 0;

    if (m_completions.size() > 0) {
      Select(selection);
      Focus(selection);
    }

    break;
  }
  case WXK_PAGEDOWN:
  case WXK_NUMPAD_PAGEDOWN: {
    unsigned int selection =
      GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
    selection += 8;
    if (selection >= m_completions.size())
      selection = m_completions.size() - 1;

    if (m_completions.size() > 0) {
      Select(selection);
      Focus(selection);
    }
    break;
  }
  case WXK_DOWN:
  case WXK_NUMPAD_DOWN: {
    auto sel = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
    if (sel < 0)
      sel = 0;
    std::size_t selection = sel;
    selection++;
    if (selection >= m_completions.size())
      selection--;
    if (m_completions.size() > 0) {
      Select(selection);
      Focus(selection);
    }
    break;
  }
  case WXK_BACK:
  case WXK_NUMPAD_DELETE: {
    wxString oldString = m_partial;
    if (m_partial != wxEmptyString)
      m_partial = m_partial.Left(m_partial.Length() - 1);
    if (m_type == AutoComplete::esccommand) {
      // Esc-command completion keeps its traditional behavior: deleting ends
      // the completion.
      if (oldString != wxEmptyString)
        UpdateResults();
      else
        m_parent->GetParent()->Refresh();
      if (!m_editor->IsActive())
        m_editor->ActivateCursor();
      return void(Destroy());
    }
    if (oldString == wxEmptyString) {
      // Nothing left to delete from the completion: end it.
      m_parent->GetParent()->Refresh();
      if (!m_editor->IsActive())
        m_editor->ActivateCursor();
      return void(Destroy());
    }
    m_editor->ReplaceSelection(oldString, m_partial, true);
    // Deleting back across a "/" moves the completion into the parent
    // directory, whose contents may not be scanned yet.
    if (CompletesFiles() && oldString.EndsWith(wxS("/")))
      m_autocomplete->UpdateFiles(m_type, m_partial, m_fileBaseDir);
    // Widen the filter, but keep the popup open: silently re-completing the
    // just-deleted text would fight the user.
    UpdateResults(false);
    break;
  }
  default:
    event.Skip();
  }
  m_parent->GetParent()->Refresh();
}

bool AutocompletePopup::Create(wxWindow *parent) {
  bool retval = wxListView::Create(parent, 1, m_position, wxDefaultSize,
                                   wxLC_ALIGN_LEFT | wxLC_REPORT |
                                   wxLC_NO_HEADER | wxLC_SINGLE_SEL);
  //  wxWindowUpdateLocker lock(this);
  InsertColumn(0, wxEmptyString);
  UpdateResults();
  SetColumnWidth(0, wxLIST_AUTOSIZE);

  wxSize minSize;
  wxSize optimumSize = wxSize(-1, 0);
  for (std::size_t i = 0; i < m_completions.size(); i++) {
    wxRect itemRect;
    if (GetItemRect(i, itemRect)) {
      if (optimumSize.x < itemRect.GetWidth())
        optimumSize.x = itemRect.GetWidth();

      optimumSize.y += itemRect.GetHeight();
    }
  }
  minSize = optimumSize;
  if (minSize.y > 300)
    minSize.y = 300;
  SetMinSize(minSize);
  SetClientSize(minSize);
  Layout();
  // SetOptimumSize(optimumSize);
  return retval;
}

void AutocompletePopup::OnClick(wxMouseEvent &WXUNUSED(event)) {
  // If the editor was destroyed while the popup was open, dismiss the popup
  // rather than dereference a freed cell.
  if (!m_editor)
    return void(Destroy());
  if (GetItemCount() <= 0)
    return;

  m_value = wxListView::GetFirstSelected();

  if (m_value < 0)
    m_value = 0;
  // Clicking a directory descends into it and continues completing there,
  // bash-style.
  if (CompletesFiles() && IsDirectoryCompletion(m_completions.at(m_value)) &&
      DescendIntoDirectory(m_completions.at(m_value))) {
    m_parent->GetParent()->Refresh();
    return;
  }
  m_partial = m_completions.at(m_value);
  if (m_type != AutoComplete::esccommand)
    m_editor->ReplaceSelection(m_editor->GetSelectionString(), m_partial);
  else
    m_editor->InsertEscCommand(m_partial);
  m_parent->GetParent()->Refresh();
  if (!m_editor->IsActive())
    m_editor->ActivateCursor();
  return void(Destroy());
}

AutocompletePopup::~AutocompletePopup() { GetParent()->SetFocus(); }

bool AutocompletePopup::DescendIntoDirectory(const wxString &completion) {
  // Strip the closing quote: the user continues typing inside the string.
  wxString newPartial = completion;
  if (newPartial.EndsWith(wxS("\"")))
    newPartial.Truncate(newPartial.Length() - 1);

  // An empty directory offers itself as its only completion; there is
  // nothing to descend into.
  if (newPartial == m_partial)
    return false;

  m_editor->ReplaceSelection(m_editor->GetSelectionString(), newPartial, true);
  m_partial = newPartial;
  m_autocomplete->UpdateFiles(m_type, m_partial, m_fileBaseDir);
  UpdateResults();
  return true;
}

AutocompletePopup::AutocompletePopup(wxWindow *parent, EditorCell *editor,
                                     AutoComplete *autocomplete,
                                     AutoComplete::autoCompletionType type,
                                     AutocompletePopup **doneptr,
                                     const wxString &fileBaseDir)
  : m_parent(parent), m_doneptr{*doneptr}, m_autocomplete(autocomplete),
    m_editor(editor), m_type(type), m_fileBaseDir(fileBaseDir) {
  wxASSERT_MSG(!*doneptr,
               "Attempted to create coexistent autocomplete popups.");

  if (m_type != AutoComplete::esccommand)
    m_partial = m_editor->GetSelectionString();

  Bind(wxEVT_CHAR, &AutocompletePopup::OnChar, this);
  Bind(wxEVT_KEY_DOWN, &AutocompletePopup::OnKeyDown, this);
  Bind(wxEVT_LEFT_UP, &AutocompletePopup::OnClick, this);
}

void AutocompletePopup::OnChar(wxKeyEvent &event) {
  // If the editor was destroyed while the popup was open, dismiss the popup
  // rather than dereference a freed cell.
  if (!m_editor)
    return void(Destroy());
  wxUniChar key = event.GetUnicodeKey();
  if (((m_type == AutoComplete::esccommand) && wxIsprint(key)) ||
      // File names may contain nearly any character (dots, dashes,
      // spaces...), so while completing file names every printable key
      // narrows the filter bash-style instead of ending the completion.
      (CompletesFiles() && wxIsprint(key)) ||
      (wxIsalnum(key)) || (key == wxS('_')) || (key == wxS('\"'))) {
    wxString oldString = m_editor->GetSelectionString();
    m_partial += key;
    if (m_type != AutoComplete::esccommand)
      m_editor->ReplaceSelection(oldString, m_partial, true);
    // Typing a "/" moves the completion into a subdirectory, whose contents
    // haven't been scanned yet.
    if (CompletesFiles() && (key == wxS('/')))
      m_autocomplete->UpdateFiles(m_type, m_partial, m_fileBaseDir);
    UpdateResults();
    return;
  } else if (wxIsprint(key)) {
    int selection = GetNextItem(0, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);

    // The current key is no more part of the current command
    //
    // => Add the current selection to the worksheet and handle this keypress
    // normally.
    if (selection < 0)
      selection = 0;

    m_editor->ReplaceSelection(m_editor->GetSelectionString(),
                               m_completions[selection]);
    m_parent->GetParent()->Refresh();
    if (!m_editor->IsActive())
      m_editor->ActivateCursor();

    // Tell MathCtrl to handle this key event the normal way.
    wxKeyEvent *keyEvent = new wxKeyEvent(event);
    m_parent->GetEventHandler()->QueueEvent(keyEvent);

    return void(Destroy());
  }
}
