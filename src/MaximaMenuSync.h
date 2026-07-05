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
  Declares the table that keeps menu check marks in sync with Maxima's variables.

  Maxima informs wxMaxima whenever one of a watched set of its option variables
  changes (see MaximaVariableUpdates). For many of these variables the only
  thing wxMaxima does with the new value is update the check mark of the menu
  item that controls the same option - a dozen hand-written handler methods
  inside the wxMaxima god class that all looked alike. That mapping is data,
  not code, so it now lives in one declarative variable-to-menu-items table
  here, applied by a single function (the same move that replaced the
  hand-synced configuration Read/Write pairs with
  Configuration::ScalarConfigSettings). Handlers with real logic beyond the
  check mark stay hand-written in wxMaxima.
*/

#ifndef MAXIMAMENUSYNC_H
#define MAXIMAMENUSYNC_H

#include "precomp.h"
#include <wx/menu.h>
#include <wx/string.h>
#include <cstdint>
#include <vector>

/*! One menu item a Maxima variable value maps to.
 */
struct MaximaMenuSyncEntry
{
  //! The variable value (or value suffix) that selects this menu item
  wxString m_value;
  //! The id of the menu item to check when the value matches
  int m_menuId;
};

/*! How the check marks of one Maxima variable's menu items follow its value.
 */
struct MaximaMenuSyncRow
{
  enum class Kind : std::int8_t {
    //! A check item: checked exactly if the value equals the entry's value
    CheckedIfEqual,
    //! A check item: checked exactly if the value differs from the entry's value
    UncheckedIfEqual,
    //! Radio items: check the entry whose value equals the variable's value
    RadioFromValue,
    //! Radio items: check the entry whose value the variable's value ends in
    RadioFromSuffix
  };
  //! The name of the Maxima variable this row watches
  wxString m_variable;
  //! How to match the variable's value against the entries
  Kind m_kind;
  //! Which menu item(s) the value selects between
  std::vector<MaximaMenuSyncEntry> m_entries;
  /*! A menu item to enable whenever any value arrives, or wxID_NONE.

    Receiving the variable at all proves the Maxima side supports the
    feature (used to unlock the debugger-trigger submenu).
  */
  int m_enableId = wxID_NONE;
};

/*! The variable-to-menu-items table itself.

  Built on first use so the dynamically-allocated menu ids in EventIDs are
  guaranteed to exist, whatever the translation units' initialization order.
*/
const std::vector<MaximaMenuSyncRow> &MaximaMenuSyncRows();

/*! Update the menu check marks that mirror one Maxima variable.

  Looks the variable up in MaximaMenuSyncRows() and applies its row to the
  items in \p menubar (missing items are skipped, so a menu that only exists
  conditionally cannot make this fail).

  \returns true if the variable has a row in the table (whether or not any
  check mark changed), false if it is not menu-synced.
*/
bool SyncMenusToMaximaVariable(wxMenuBar *menubar, const wxString &variable,
                               const wxString &value);

#endif // MAXIMAMENUSYNC_H
