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

#ifndef AUTOCOMPLETEPOPUP_H
#define AUTOCOMPLETEPOPUP_H

#include "Autocomplete.h"
#include "EditorCell.h"
#include <wx/menu.h>

//! The maximum number of popup menu entries we show at the same time
#define AC_MENU_LENGTH 25

    /*! The first number that is open for dynamic ID assignment

      If we want to add additional elements to a pop-up this is the 
      lowest ID that is guaranteed to be free for this purpose.
     */
#define popid_complete_00 (wxID_HIGHEST + 1000)

class AutocompletePopup : public wxMenu
{
private:
  wxArrayString m_completions;
  AutoComplete *m_autocomplete;
public:
  AutocompletePopup(EditorCell* editor, AutoComplete *autocomplete, AutoComplete::autoCompletionType type);
};

#endif // AUTOCOMPLETEPOPUP_H
