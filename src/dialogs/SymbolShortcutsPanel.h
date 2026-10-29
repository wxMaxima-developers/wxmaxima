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
  The configuration dialogue's list of extra symbols and symbol shortcuts.
*/

#ifndef SYMBOLSHORTCUTSPANEL_H
#define SYMBOLSHORTCUTSPANEL_H

#include <vector>
#include <wx/grid.h>
#include <wx/panel.h>
#include "SymbolShortcuts.h"

class Configuration;

/*! The extra symbols of the Symbols sidebar, and the keyboard shortcuts of
  every symbol (GH #2578).

  A table with one row per symbol: the symbol, its keyboard shortcut and
  which sidebar shows it. A symbol neither sidebar has built in becomes an
  extra symbol of the Symbols sidebar; one that is built in (α, ∞, ...) only
  gets its shortcut here. This replaces the text field the extra symbols
  used to be typed into, which they still are stored as, so older versions
  of wxMaxima keep reading them.

  Nothing is stored before Write() is called, so "Cancel" in the dialogue
  changes nothing.
*/
class SymbolShortcutsPanel : public wxPanel
{
public:
  //! Where a symbol is shown
  enum class Location { GreekLowercase, GreekUppercase, SymbolsBuiltIn, SymbolsExtra };
  /*! Which sidebar shows this symbol

    A Greek letter the Greek sidebar currently hides (a Latin lookalike, by
    default) is an extra symbol: it would be shown nowhere otherwise.
  */
  static Location Locate(const wxString &symbol, const Configuration *configuration);
  //! The text the "Where" column shows for a location
  static wxString LocationName(Location location);

  //! One row of the table
  struct Row
  {
    wxString symbol;
    wxString shortcut; //!< raw, see wxm::SymbolShortcut
    bool operator==(const Row &) const = default;
  };

  SymbolShortcutsPanel(wxWindow *parent, const Configuration *configuration);

  //! Stores the extra symbols and the shortcuts in the configuration
  void Write(Configuration *configuration) const;

  //! The rows, in the order the table shows them
  const std::vector<Row> &GetRows() const { return m_rows; }
  /*! Puts what is typed into a row's "Symbol" cell there.

    Several symbols typed or pasted at once become a row each. An empty text
    removes the row. The test types into the table this way.
  */
  void SetSymbolText(int row, const wxString &text);
  //! Adds an empty row and starts editing its symbol
  void AddRow();
  //! Removes a row
  void RemoveRow(int row);
  /*! Gives a row's symbol a shortcut, or none if \p shortcut is empty.

    The row that had this shortcut before loses it, as a shortcut types one
    symbol only.
  */
  void SetShortcut(int row, const wxString &shortcut);

  /*! The rows Write() would store for this text field content and shortcuts

    How the table is filled from the configuration.
  */
  static std::vector<Row> RowsFor(const wxString &extraSymbols,
                                  const std::vector<wxm::SymbolShortcut> &shortcuts);
  //! The extra symbols Write() stores for these rows
  wxString ExtraSymbols() const;

private:
  //! Shows m_rows in the grid
  void Fill();
  void OnCellChanged(wxGridEvent &event);
  void OnCellDClick(wxGridEvent &event);
  void OnGridKey(wxKeyEvent &event);
  //! Asks for the shortcut of the selected row
  void EditSelectedShortcut();
  //! The row the grid's cursor is on, or -1
  int SelectedRow() const;

  std::vector<Row> m_rows;
  //! Only used to tell where a symbol is shown
  const Configuration *m_configuration;
  wxGrid *m_grid;
};

#endif // SYMBOLSHORTCUTSPANEL_H
