// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2017 Gunter Königsmann     <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class AutocompletePopup.

  AutocompletePopup offers less functionality than AutoCompletePopup, but works
  on systems that don't allow popups to handle key presses.
*/

#include "AutocompletePopup.h"
#include "Dirstructure.h"

#include <wx/textfile.h>

void AutocompletePopup::UpdateResults()
{
  wxString partial = m_editor->GetSelectionString();

  m_completions = m_autocomplete->CompleteSymbol(partial, m_type);
  m_completions.Sort();

  for (unsigned int i = 0; i < m_length; i++)
    Destroy(popid_complete_00 + i);

  m_length = m_completions.GetCount();
  if (m_length > AC_MENU_LENGTH) m_length = AC_MENU_LENGTH;

  for (unsigned int i = 0; i < m_length; i++)
    Append(popid_complete_00 + i, m_completions[i]);
}

void AutocompletePopup::ProcessCharEvent(wxKeyEvent &event)
{
  event.Skip();
}

AutocompletePopup::AutocompletePopup(
        EditorCell *editor,
        AutoComplete *autocomplete,
        AutoComplete::autoCompletionType type
) : wxMenu()
{
  m_autocomplete = autocomplete;
  m_editor = editor;
  m_type = type;
  m_length = 0;
  UpdateResults();
}

BEGIN_EVENT_TABLE(AutocompletePopup, wxMenu)
//EVT_KEY_DOWN(AutocompletePopup::ProcessEvent)
                EVT_CHAR(AutocompletePopup::ProcessCharEvent)
END_EVENT_TABLE()

