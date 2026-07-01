// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Implements MenuHelpString(); see MenuHelpString.h for the rationale.
*/

#include "MenuHelpString.h"
#include <wx/menu.h>
#include <wx/menuitem.h>

wxString MenuHelpString(wxMenu *menu, int id) {
  if ((menu == NULL) || (id <= 0))
    return {};
  // wxMenu::GetHelpString() asserts in wxWidgets 3.3 if id is not one of this
  // menu's items; FindItem() recurses into submenus and returns nullptr for a
  // non-match instead, so we look the item up ourselves and never assert.
  wxMenuItem *item = menu->FindItem(id);
  return (item != NULL) ? item->GetHelp() : wxString();
}
