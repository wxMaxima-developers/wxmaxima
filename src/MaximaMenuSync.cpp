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
  Implements the table that keeps menu check marks in sync with Maxima's
  variables.
*/

#include "MaximaMenuSync.h"
#include "EventIDs.h"
#include <algorithm>

const std::vector<MaximaMenuSyncRow> &MaximaMenuSyncRows()
{
  using Kind = MaximaMenuSyncRow::Kind;
  static const std::vector<MaximaMenuSyncRow> rows = {
    {wxS("numer"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_num_out}}},
    {wxS("algebraic"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_talg}}},
    {wxS("stringdisp"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_stringdisp}}},
    {wxS("wxanimate_autoplay"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_animationautostart}}},
    {wxS("engineering_format_floats"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_engineeringFormat}}},
    {wxS("domain"), Kind::CheckedIfEqual,
     {{wxS("complex"), EventIDs::menu_num_domain}}},
    {wxS("opsubst"), Kind::CheckedIfEqual,
     {{wxS("true"), EventIDs::menu_opsubst}}},
    // showtime knows more values than true/false ("real") - anything but an
    // explicit "false" means time display is on.
    {wxS("showtime"), Kind::UncheckedIfEqual,
     {{wxS("false"), EventIDs::menu_time}}},
    {wxS("gentranlang"), Kind::RadioFromValue,
     {{wxS("c"), EventIDs::gentran_lang_c},
      {wxS("fortran"), EventIDs::gentran_lang_fortran},
      {wxS("ratfor"), EventIDs::gentran_lang_ratfor}}},
    {wxS("logexpand"), Kind::RadioFromValue,
     {{wxS("false"), EventIDs::menu_logexpand_false},
      {wxS("true"), EventIDs::menu_logexpand_true},
      {wxS("all"), EventIDs::menu_logexpand_all},
      {wxS("super"), EventIDs::menu_logexpand_super}}},
    {wxS("wxsubscripts"), Kind::RadioFromValue,
     {{wxS("false"), EventIDs::menu_noAutosubscript},
      {wxS("true"), EventIDs::menu_defaultAutosubscript},
      {wxS("all"), EventIDs::menu_alwaysAutosubscript}}},
    // Receiving debugmode at all means this Maxima can drive the debugger,
    // so the (initially disabled) trigger submenu is unlocked as a side effect.
    {wxS("debugmode"), Kind::RadioFromValue,
     {{wxS("false"), EventIDs::menu_debugmode_off},
      {wxS("lisp"), EventIDs::menu_debugmode_lisp},
      {wxS("true"), EventIDs::menu_debugmode_all}},
     EventIDs::menu_debugmode},
    // lmxchar is the matrix parenthesis character, e.g. "[" - but themes may
    // prepend escape sequences, so only the value's end is compared.
    {wxS("lmxchar"), Kind::RadioFromSuffix,
     {{wxS("("), EventIDs::menu_roundedMatrixParens},
      {wxS("<"), EventIDs::menu_angledMatrixParens},
      {wxS("|"), EventIDs::menu_straightMatrixParens},
      {wxS("["), EventIDs::menu_squareMatrixParens},
      {wxS(" "), EventIDs::menu_noMatrixParens}}},
  };
  return rows;
}

bool SyncMenusToMaximaVariable(wxMenuBar *menubar, const wxString &variable,
                               const wxString &value)
{
  const std::vector<MaximaMenuSyncRow> &rows = MaximaMenuSyncRows();
  const auto row =
    std::find_if(rows.begin(), rows.end(),
                 [&](const MaximaMenuSyncRow &r) { return r.m_variable == variable; });
  if (row == rows.end())
    return false;
  if (!menubar)
    return true;

  using Kind = MaximaMenuSyncRow::Kind;
  if ((row->m_enableId != wxID_NONE) && menubar->FindItem(row->m_enableId))
    menubar->Enable(row->m_enableId, true);
  for (const MaximaMenuSyncEntry &entry : row->m_entries) {
    if (!menubar->FindItem(entry.m_menuId))
      continue;
    switch (row->m_kind) {
    case Kind::CheckedIfEqual:
      menubar->Check(entry.m_menuId, value == entry.m_value);
      break;
    case Kind::UncheckedIfEqual:
      menubar->Check(entry.m_menuId, value != entry.m_value);
      break;
    // Radio items cannot be unchecked individually: checking the matching one
    // unchecks its group siblings, non-matching entries are left alone.
    case Kind::RadioFromValue:
      if (value == entry.m_value)
        menubar->Check(entry.m_menuId, true);
      break;
    case Kind::RadioFromSuffix:
      if (value.EndsWith(entry.m_value))
        menubar->Check(entry.m_menuId, true);
      break;
    }
  }
  return true;
}
