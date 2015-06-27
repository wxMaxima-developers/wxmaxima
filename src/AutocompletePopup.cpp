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

#include <wx/textfile.h>

AutocompletePopup::AutocompletePopup(
  EditorCell* editor,
  AutoComplete * autocomplete,
  AutoComplete::autoCompletionType type
  ) : wxMenu()
{
  m_autocomplete = autocomplete;
  wxString partial = editor->GetSelectionString();
  
  m_completions = m_autocomplete->CompleteSymbol(partial, type);
  type == AutoComplete::tmplte;
  
  m_completions.Sort();

  for (unsigned int i=0; i<m_completions.GetCount() && i<AC_MENU_LENGTH; i++)
    Append(popid_complete_00 + i, m_completions[i]);
}
