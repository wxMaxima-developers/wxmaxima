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
  Declares MenuHelpString(), the safe menu-item help-text lookup used for the
  status bar.
*/

#ifndef MENUHELPSTRING_H
#define MENUHELPSTRING_H

#include <wx/string.h>

class wxMenu;

/*! Returns the help string of the item with the given id inside \p menu.

  This is what wxMaxima shows in the status bar while a menu item is
  highlighted. It exists as a separate, testable function because
  wxMenu::GetHelpString() asserts ("item" failed) in wxWidgets 3.3 when the
  highlighted id is not an item of that menu -- a situation the native menu
  code (notably on Windows) can produce for menu/submenu titles or ids that
  belong to another menu. Instead of relying on GetHelpString()'s not-found
  behaviour we look the item up with wxMenu::FindItem(), which recurses into
  submenus and simply returns nullptr when nothing matches.

  \param menu The menu whose item is being highlighted (may be nullptr).
  \param id   The highlighted item id (ids <= 0 are treated as "no item").
  \return The item's help text, or an empty string when there is no matching
          item. Never asserts.
*/
wxString MenuHelpString(wxMenu *menu, int id);

#endif // MENUHELPSTRING_H
