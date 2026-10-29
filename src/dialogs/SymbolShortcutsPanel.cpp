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

#include "SymbolShortcutsPanel.h"
#include "Configuration.h"
#include "SymbolShortcutDialog.h"
#include "sidebars/GreekSidebar.h"
#include "sidebars/SymbolsSidebar.h"
#include <algorithm>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

namespace {
enum Column { symbolColumn = 0, shortcutColumn, whereColumn, columnCount };

bool GreekLetterShown(const GreekSidebar::Definition &def,
                      const Configuration *configuration) {
  switch (def.condition) {
  case GreekSidebar::Condition::Show_mu:
    return configuration->GreekSidebar_Show_mu();
  case GreekSidebar::Condition::ShowLatinLookalikes:
    return configuration->GreekSidebar_ShowLatinLookalikes();
  default:
    return true;
  }
}
} // namespace

SymbolShortcutsPanel::Location
SymbolShortcutsPanel::Locate(const wxString &symbol,
                             const Configuration *configuration) {
  for (const auto &def : GreekSidebar::LowercaseDefinitions())
    if ((def.symbol == symbol) && GreekLetterShown(def, configuration))
      return Location::GreekLowercase;
  for (const auto &def : GreekSidebar::UppercaseDefinitions())
    if ((def.symbol == symbol) && GreekLetterShown(def, configuration))
      return Location::GreekUppercase;
  for (const auto &def : SymbolsSidebar::BuiltInDefinitions())
    if (def.symbol == symbol)
      return Location::SymbolsBuiltIn;
  return Location::SymbolsExtra;
}

wxString SymbolShortcutsPanel::LocationName(Location location) {
  switch (location) {
  case Location::GreekLowercase:
    return _("Greek sidebar (lowercase)");
  case Location::GreekUppercase:
    return _("Greek sidebar (uppercase)");
  case Location::SymbolsBuiltIn:
    return _("Symbols sidebar (standard)");
  default:
    return _("Symbols sidebar (extra symbols)");
  }
}

std::vector<SymbolShortcutsPanel::Row>
SymbolShortcutsPanel::RowsFor(const wxString &extraSymbols,
                              const std::vector<wxm::SymbolShortcut> &shortcuts) {
  std::vector<Row> rows;
  auto has = [&rows](const wxString &symbol) {
    return std::ranges::any_of(rows, [&](const Row &row) {return row.symbol == symbol;});
  };
  for (const auto &symbol : wxm::SplitIntoSymbols(extraSymbols))
    if (!has(symbol))
      rows.push_back({symbol, {}});
  for (const auto &entry : shortcuts) {
    auto row = std::ranges::find_if(rows, [&](const Row &r) {return r.symbol == entry.symbol;});
    if (row != rows.end())
      row->shortcut = entry.shortcut;
    else
      rows.push_back({entry.symbol, entry.shortcut});
  }
  return rows;
}

SymbolShortcutsPanel::SymbolShortcutsPanel(wxWindow *parent,
                                           const Configuration *configuration)
  : wxPanel(parent, wxID_ANY),
    m_rows(RowsFor(configuration->SymbolPaneAdditionalChars(),
                   configuration->SymbolShortcuts())),
    m_configuration(configuration) {
  const int border = 5 * GetContentScaleFactor();
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  m_grid = new wxGrid(this, wxID_ANY);
  m_grid->CreateGrid(0, columnCount);
  m_grid->HideRowLabels();
  m_grid->SetColLabelValue(symbolColumn, _("Symbol"));
  m_grid->SetColLabelValue(shortcutColumn, _("Keyboard shortcut"));
  m_grid->SetColLabelValue(whereColumn, _("Shown in"));
  m_grid->SetSelectionMode(wxGrid::wxGridSelectRows);
  m_grid->SetMinSize(wxSize(-1, 150 * GetContentScaleFactor()));
  // Big enough to tell a symbol's accents apart
  wxGridCellAttr *symbolAttr = new wxGridCellAttr;
  symbolAttr->SetFont(GetFont().Scaled(1.5));
  symbolAttr->SetAlignment(wxALIGN_CENTER, wxALIGN_CENTER);
  m_grid->SetColAttr(symbolColumn, symbolAttr);
  wxGridCellAttr *shortcutAttr = new wxGridCellAttr;
  shortcutAttr->SetReadOnly();
  m_grid->SetColAttr(shortcutColumn, shortcutAttr);
  wxGridCellAttr *whereAttr = new wxGridCellAttr;
  whereAttr->SetReadOnly();
  m_grid->SetColAttr(whereColumn, whereAttr);
  vbox->Add(m_grid, wxSizerFlags(1).Expand().Border(wxALL, border));

  wxBoxSizer *buttons = new wxBoxSizer(wxHORIZONTAL);
  wxButton *add = new wxButton(this, wxID_ANY, _("Add symbol"));
  add->Bind(wxEVT_BUTTON, [this](wxCommandEvent &) {AddRow();});
  buttons->Add(add, wxSizerFlags().Border(wxRIGHT, border));
  wxButton *remove = new wxButton(this, wxID_ANY, _("Remove"));
  remove->Bind(wxEVT_BUTTON, [this](wxCommandEvent &) {RemoveRow(SelectedRow());});
  buttons->Add(remove, wxSizerFlags().Border(wxRIGHT, border));
  wxButton *shortcut = new wxButton(this, wxID_ANY, _("Keyboard shortcut..."));
  shortcut->Bind(wxEVT_BUTTON, [this](wxCommandEvent &) {EditSelectedShortcut();});
  buttons->Add(shortcut);
  vbox->Add(buttons, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, border));

  m_grid->SetToolTip(
    _("Symbols that are entered or copied here will appear in the symbols "
      "sidebar so they can be entered into the worksheet easily. A letter "
      "with accents counts as one symbol. A symbol one of the sidebars "
      "already shows only gets its keyboard shortcut here. A keyboard "
      "shortcut types its symbol while the worksheet has the keyboard "
      "focus; double-click the shortcut to change it."));

  m_grid->Bind(wxEVT_GRID_CELL_CHANGED, &SymbolShortcutsPanel::OnCellChanged, this);
  m_grid->Bind(wxEVT_GRID_CELL_LEFT_DCLICK, &SymbolShortcutsPanel::OnCellDClick, this);
  m_grid->GetGridWindow()->Bind(wxEVT_KEY_DOWN, &SymbolShortcutsPanel::OnGridKey, this);
  SetSizer(vbox);
  Fill();
}

void SymbolShortcutsPanel::Fill() {
  m_grid->BeginBatch();
  const int rows = static_cast<int>(m_rows.size());
  if (m_grid->GetNumberRows() > rows)
    m_grid->DeleteRows(rows, m_grid->GetNumberRows() - rows);
  else if (m_grid->GetNumberRows() < rows)
    m_grid->AppendRows(rows - m_grid->GetNumberRows());
  for (int i = 0; i < rows; ++i) {
    const Row &row = m_rows[static_cast<size_t>(i)];
    m_grid->SetCellValue(i, symbolColumn, row.symbol);
    m_grid->SetCellValue(i, shortcutColumn,
                         row.shortcut.IsEmpty() ? wxString() :
                         wxm::ShortcutForDisplay(row.shortcut));
    m_grid->SetCellValue(i, whereColumn,
                         row.symbol.IsEmpty() ? wxString() :
                         LocationName(Locate(row.symbol, m_configuration)));
  }
  m_grid->AutoSizeColumns(false);
  m_grid->EndBatch();
}

void SymbolShortcutsPanel::SetSymbolText(int row, const wxString &text) {
  if ((row < 0) || (row >= static_cast<int>(m_rows.size())))
    return;
  const auto symbols = wxm::SplitIntoSymbols(text);
  const auto at = m_rows.begin() + row;
  if (symbols.empty()) {
    m_rows.erase(at);
  } else {
    at->symbol = symbols.front();
    std::vector<Row> more;
    for (auto symbol = symbols.begin() + 1; symbol != symbols.end(); ++symbol)
      more.push_back({*symbol, {}});
    m_rows.insert(at + 1, more.begin(), more.end());
  }
  // A symbol listed twice: the first row with it wins, keeping its shortcut
  // if it has one.
  std::vector<Row> unique;
  for (const auto &r : m_rows) {
    auto same = std::ranges::find_if(unique, [&](const Row &u) {return u.symbol == r.symbol;});
    if (same == unique.end())
      unique.push_back(r);
    else if (same->shortcut.IsEmpty())
      same->shortcut = r.shortcut;
  }
  m_rows = std::move(unique);
  Fill();
}

void SymbolShortcutsPanel::AddRow() {
  m_rows.push_back({});
  Fill();
  const int row = static_cast<int>(m_rows.size()) - 1;
  m_grid->SetGridCursor(row, symbolColumn);
  m_grid->MakeCellVisible(row, symbolColumn);
  m_grid->SetFocus();
  m_grid->EnableCellEditControl();
}

void SymbolShortcutsPanel::RemoveRow(int row) {
  if ((row < 0) || (row >= static_cast<int>(m_rows.size())))
    return;
  m_rows.erase(m_rows.begin() + row);
  Fill();
}

void SymbolShortcutsPanel::SetShortcut(int row, const wxString &shortcut) {
  if ((row < 0) || (row >= static_cast<int>(m_rows.size())))
    return;
  if (!shortcut.IsEmpty())
    for (auto &r : m_rows)
      if (wxm::SameShortcut(r.shortcut, shortcut))
        r.shortcut.Clear();
  m_rows[static_cast<size_t>(row)].shortcut = shortcut;
  Fill();
}

int SymbolShortcutsPanel::SelectedRow() const {
  const int row = m_grid->GetGridCursorRow();
  if ((row < 0) || (row >= static_cast<int>(m_rows.size())))
    return -1;
  return row;
}

void SymbolShortcutsPanel::EditSelectedShortcut() {
  const int row = SelectedRow();
  if ((row < 0) || m_rows[static_cast<size_t>(row)].symbol.IsEmpty())
    return;
  const Row &current = m_rows[static_cast<size_t>(row)];
  std::vector<wxm::SymbolShortcut> shortcuts;
  for (const auto &r : m_rows)
    if (!r.shortcut.IsEmpty())
      shortcuts.push_back({r.symbol, r.shortcut});
  SymbolShortcutDialog dialog(
    this, current.symbol, current.shortcut,
    SymbolShortcutDialog::MakeUsageLookup(this, shortcuts, current.symbol));
  if (dialog.ShowModal() == wxID_OK)
    SetShortcut(row, dialog.GetShortcut());
}

void SymbolShortcutsPanel::OnCellChanged(wxGridEvent &event) {
  if (event.GetCol() == symbolColumn)
    // Not right away: the grid is still busy with the editor that sent this.
    CallAfter([this, row = event.GetRow()]() {
      if (row < m_grid->GetNumberRows())
        SetSymbolText(row, m_grid->GetCellValue(row, symbolColumn));
    });
  event.Skip();
}

void SymbolShortcutsPanel::OnCellDClick(wxGridEvent &event) {
  if (event.GetCol() == shortcutColumn) {
    m_grid->SetGridCursor(event.GetRow(), event.GetCol());
    EditSelectedShortcut();
  } else
    event.Skip();
}

void SymbolShortcutsPanel::OnGridKey(wxKeyEvent &event) {
  // Keyboard access to what double-clicking a shortcut does
  if ((m_grid->GetGridCursorCol() == shortcutColumn) &&
      ((event.GetKeyCode() == WXK_RETURN) || (event.GetKeyCode() == WXK_NUMPAD_ENTER) ||
       (event.GetKeyCode() == WXK_SPACE))) {
    EditSelectedShortcut();
    return;
  }
  if ((m_grid->GetGridCursorCol() == shortcutColumn) &&
      ((event.GetKeyCode() == WXK_DELETE) || (event.GetKeyCode() == WXK_BACK))) {
    SetShortcut(SelectedRow(), {});
    return;
  }
  event.Skip();
}

wxString SymbolShortcutsPanel::ExtraSymbols() const {
  std::vector<wxString> extras;
  for (const auto &row : m_rows)
    if (!row.symbol.IsEmpty() &&
        (Locate(row.symbol, m_configuration) == Location::SymbolsExtra))
      extras.push_back(row.symbol);
  return wxm::JoinSymbols(extras);
}

void SymbolShortcutsPanel::Write(Configuration *configuration) const {
  configuration->SymbolPaneAdditionalChars(ExtraSymbols());
  std::vector<wxm::SymbolShortcut> shortcuts;
  for (const auto &row : m_rows)
    if (!row.symbol.IsEmpty() && !row.shortcut.IsEmpty())
      shortcuts.push_back({row.symbol, row.shortcut});
  configuration->SymbolShortcuts(shortcuts);
}
