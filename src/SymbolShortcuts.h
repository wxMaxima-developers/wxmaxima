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
  Keyboard shortcuts that type a symbol into the worksheet (GH #2578).

  A "symbol" here is what one button of the Greek or the Symbols sidebar
  enters. It used to be exactly one character, which made a letter with a
  combining accent (t + U+0304 = t̄, GH #2296) two buttons, neither of which
  was what the user wanted. So a symbol is a short string: a base character
  together with the combining marks that follow it.

  Everything in this file is free of GUI state, so the tests can pin it down
  without a window.
*/
#ifndef SYMBOLSHORTCUTS_H
#define SYMBOLSHORTCUTS_H

#include <wx/string.h>
#include <vector>

class wxKeyEvent;

namespace wxm {

/*! Splits text into the symbols a sidebar shows as one button each.

  A combining mark, a variation selector or the low half of a UTF-16
  surrogate pair (wxString is UTF-16 on Windows) stays with the character
  before it, and a zero-width joiner joins its neighbours on both sides. So
  "Øt̄§" gives the three symbols "Ø", "t̄" and "§". A combining mark with
  nothing before it, or a space before it, is a symbol of its own. Spaces
  only separate symbols.
*/
std::vector<wxString> SplitIntoSymbols(const wxString &text);

/*! Joins symbols into a string SplitIntoSymbols() splits into the same symbols.

  Without separators, which is what older wxMaxima versions expect of the
  list of additional symbols, except for a space in front of a symbol that
  would otherwise merge with the one before it.
*/
wxString JoinSymbols(const std::vector<wxString> &symbols);

//! A symbol and the keyboard shortcut that types it.
struct SymbolShortcut
{
  wxString symbol;
  /*! The shortcut, as wxAcceleratorEntry::ToRawString() writes it ("Ctrl+Alt+T").

    The raw, untranslated form, so that the configuration stays valid when
    the user switches wxMaxima's language.
   */
  wxString shortcut;
  bool operator==(const SymbolShortcut &) const = default;
};

/*! Reads the shortcuts list the configuration stores.

  One "symbol<TAB>shortcut" per line. Lines that don't have this form, or
  whose shortcut wxWidgets can't parse, are skipped.
*/
std::vector<SymbolShortcut> ParseSymbolShortcuts(const wxString &stored);
//! The inverse of ParseSymbolShortcuts().
wxString SerializeSymbolShortcuts(const std::vector<SymbolShortcut> &shortcuts);

/*! Brings a shortcut a human typed into the raw form we store.

  Returns an empty string if wxWidgets can't parse it, or if it is a key a
  symbol must not take over (see IsUsableShortcut()).
*/
wxString NormalizeShortcut(const wxString &shortcut);

/*! Would this shortcut leave ordinary typing alone?

  It needs Ctrl, Alt (or, on the Mac, Cmd or Ctrl) or has to be a function key:
  anything else would be needed for typing text.
*/
bool IsUsableShortcut(int flags, int keyCode);

/*! The shortcut a key press is, in raw form, or an empty string.

  Empty for a bare modifier key (the user is still holding down the start of
  a combination) and for keys IsUsableShortcut() turns down.
*/
wxString ShortcutFromKeyEvent(const wxKeyEvent &event);

//! Do the two raw shortcuts name the same key combination?
bool SameShortcut(const wxString &a, const wxString &b);

//! The shortcut in the user's language, for display ("Strg+Alt+T").
wxString ShortcutForDisplay(const wxString &rawShortcut);

/*! The symbol whose shortcut the key event is, or an empty string.

  Linear in the number of shortcuts; a user has a handful of them, and this
  runs once per key press.
*/
wxString SymbolForKeyEvent(const std::vector<SymbolShortcut> &shortcuts,
                           const wxKeyEvent &event);

} // namespace wxm

#endif // SYMBOLSHORTCUTS_H
