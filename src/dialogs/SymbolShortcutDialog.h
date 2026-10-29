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
  The dialog that records the keyboard shortcut for a symbol (GH #2578).
*/

#ifndef SYMBOLSHORTCUTDIALOG_H
#define SYMBOLSHORTCUTDIALOG_H

#include <functional>
#include <vector>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include "SymbolShortcuts.h"

class wxMenuBar;

/*! Asks the user to press the keys that should type a symbol.

  The keys are caught before any control of the dialog sees them, so a
  combination like Ctrl+Alt+T is recorded instead of acting on the dialog.
  Enter, Escape and Tab without Ctrl or Alt keep working as usual: they aren't
  usable as a symbol's shortcut anyway (see wxm::IsUsableShortcut()).

  The dialog only records. What the shortcut is currently used for is asked
  of the function passed to the constructor, which is how the dialog can
  warn about a clash with a menu or another symbol without knowing where
  the shortcuts are kept.
*/
class SymbolShortcutDialog : public wxDialog
{
public:
  /*! Describes what a shortcut is already used for: a menu entry's or
    another symbol's name, or an empty string if it is free.
  */
  using UsageLookup = std::function<wxString(const wxString &rawShortcut)>;

  SymbolShortcutDialog(wxWindow *parent, const wxString &symbol,
                       const wxString &currentShortcut, UsageLookup usage);

  /*! The recorded shortcut, in the raw form wxm::SymbolShortcut stores.

    Empty if the user removed the shortcut.
  */
  const wxString &GetShortcut() const { return m_shortcut; }

  /*! Records a key press as if the user had pressed it.

    Returns whether it was a usable shortcut. The test drives the dialog
    through this, as synthesising real key presses is not portable.
  */
  bool RecordKey(const wxKeyEvent &event);

  /*! What the menu bar already uses this shortcut for

    The label of the menu entry it belongs to (without the "&" mnemonics),
    or an empty string.
  */
  static wxString MenuItemUsingShortcut(const wxMenuBar *menuBar,
                                        const wxString &rawShortcut);
  //! The menu bar of the main window \p window belongs to, if any
  static wxMenuBar *FindMenuBar(wxWindow *window);
  /*! The usual UsageLookup: the menu entries of \p window's main window
    and the symbols in \p shortcuts other than \p symbol.
  */
  static UsageLookup MakeUsageLookup(wxWindow *window,
                                     std::vector<wxm::SymbolShortcut> shortcuts,
                                     const wxString &symbol);

private:
  void OnCharHook(wxKeyEvent &event);
  void OnRemove(wxCommandEvent &event);
  //! Shows m_shortcut and what else uses it
  void ShowShortcut();

  wxString m_shortcut;
  UsageLookup m_usage;
  wxStaticText *m_shortcutText;
  wxStaticText *m_warning;
};

#endif // SYMBOLSHORTCUTDIALOG_H
