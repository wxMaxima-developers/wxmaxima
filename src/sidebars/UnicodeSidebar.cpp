// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class UnicodeSidebar

  This file contains the definition of the class Unicodesidebar that allows to
  select arbitrary unicode symbols.
*/

#include "../data/UnicodeData.h"
#include "EventIDs.h"
#include <memory>
#include <wx/mstream.h>
#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/wupdlock.h>
#include <wx/zstream.h>

#include "UnicodeSidebar.h"
wxDEFINE_EVENT(SIDEBARKEYEVENT, SidebarKeyEvent);
wxDEFINE_EVENT(SYMBOLADDEVENT, SymboladdEvent);

UnicodeSidebar::UnicodeSidebar(wxWindow *parent, wxWindow *worksheet,
                               Configuration *cfg)
  : wxPanel(parent), m_worksheet(worksheet) {
  //  wxWindowUpdateLocker speedUp(this);
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  m_initialized = false;
  m_regex = new RegexCtrl(this, wxID_ANY, cfg, "UnicodeSidebar");
  m_regex->Bind(REGEX_EVENT, &UnicodeSidebar::OnRegExEvent, this);
  m_grid = new wxGrid(this, wxID_ANY);
  m_grid->CreateGrid(0, 3);
  m_grid->BeginBatch();
  box->Add(m_regex, wxSizerFlags().Expand().Proportion(10));
  box->Add(m_grid, wxSizerFlags().Expand().Proportion(100));
  Bind(wxEVT_PAINT, &UnicodeSidebar::OnPaint, this);
  Bind(wxEVT_SIZE, &UnicodeSidebar::OnSize, this);
  m_grid->Bind(wxEVT_GRID_CELL_LEFT_DCLICK, &UnicodeSidebar::OnDClick, this);
  m_grid->Bind(wxEVT_GRID_CELL_RIGHT_CLICK, &UnicodeSidebar::OnRightClick, this);
  m_grid->Bind(wxEVT_GRID_CELL_CHANGING, &UnicodeSidebar::OnChangeAttempt, this);
  // Key events go to the grid's inner window; catch Enter there so it inserts
  // the focused character (keyboard/screen-reader operability).
  m_grid->GetGridWindow()->Bind(wxEVT_KEY_DOWN, &UnicodeSidebar::OnGridKey, this);
  m_grid->EndBatch();
  SetSizer(box);
  FitInside();
}

UnicodeSidebar::~UnicodeSidebar() {}

void UnicodeSidebar::InsertCharFromRow(int row) {
  if (row < 0)
    return;
  long numVal;
  if (m_grid->GetCellValue(row, 0).ToLong(&numVal, 16)) {
    wxCommandEvent *ev = new wxCommandEvent(SIDEBARKEYEVENT, numVal);
    m_worksheet->GetEventHandler()->QueueEvent(ev);
  }
  m_worksheet->SetFocus();
}

void UnicodeSidebar::OnDClick(wxGridEvent &event) {
  InsertCharFromRow(event.GetRow());
}

void UnicodeSidebar::OnGridKey(wxKeyEvent &event) {
  const int key = event.GetKeyCode();
  if ((key == WXK_RETURN) || (key == WXK_NUMPAD_ENTER)) {
    // Activate the focused character instead of moving the grid cursor, so a
    // keyboard-only / screen-reader user can insert a symbol.
    InsertCharFromRow(m_grid->GetGridCursorRow());
    return;
  }
  event.Skip(); // leave arrow-key navigation etc. to wxGrid
}

void UnicodeSidebar::OnRightClick(wxGridEvent &event) {
  wxString number;
  number = m_grid->GetCellValue(event.GetRow(), 0);
  if (number.ToLong(&m_charRightClickedOn, 16)) {
    std::unique_ptr<wxMenu> popupMenu(new wxMenu());
    popupMenu->Append(EventIDs::popid_addToSymbols, _("Add to symbols Sidebar"));
    Bind(wxEVT_MENU, &UnicodeSidebar::OnMenu, this, EventIDs::popid_addToSymbols);
    if(wxWindow::FindFocus())
      wxWindow::FindFocus()->PopupMenu(&*popupMenu);
  }
}

void UnicodeSidebar::OnMenu(wxCommandEvent &event) {
  if (event.GetId() == EventIDs::popid_addToSymbols) {
    const wxWindow *toplevel = this;
    while (toplevel->GetParent() != NULL)
      toplevel = toplevel->GetParent();
    wxCommandEvent *ev =
      new wxCommandEvent(SYMBOLADDEVENT, m_charRightClickedOn);
    toplevel->GetEventHandler()->QueueEvent(ev);
  }
  event.Skip();
}

void UnicodeSidebar::OnChangeAttempt(wxGridEvent &event) { event.Veto(); }

void UnicodeSidebar::UpdateDisplay() {
  wxGridUpdateLocker speedUp(m_grid);
  int rows = m_grid->GetNumberRows();
  for (int i = 0; i < rows; i++) {
    wxString name = m_grid->GetCellValue(i, 2).Lower();
    wxString unicodenumberLower = m_grid->GetCellValue(i, 0).Lower();
    wxString unicodenumberUpper = m_grid->GetCellValue(i, 0).Upper();
    // Match either the name of the unicode character or the hex code of the unicode char
    if (m_regex->Matches(name) || m_regex->Matches(unicodenumberLower) || m_regex->Matches(unicodenumberUpper))
      m_grid->ShowRow(i);
    else
      m_grid->HideRow(i);
  }
}

void UnicodeSidebar::OnSize(wxSizeEvent &event) {
  int width =
    GetClientSize().x - m_grid->GetColSize(0) - m_grid->GetColSize(1) - 10;
  if (width < 0)
    width = 0;
  m_grid->SetColSize(2, width);
  event.Skip();
}

void UnicodeSidebar::OnPaint(wxPaintEvent &event) {
  event.Skip();
  if (m_initialized)
    return;

  wxMemoryInputStream istream(UnicodeData_txt_gz, UnicodeData_txt_gz_len);
  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  wxString regex_string = m_regex->GetValue();
  wxRegEx regex(m_regex->GetValue());
  m_grid->HideRowLabels();
  m_grid->HideColLabels();
  while (!zstream.Eof()) {
    wxString line = textIn.ReadLine();
    wxStringTokenizer items(line, wxS(";"), wxTOKEN_RET_EMPTY_ALL);
    wxString number = items.GetNextToken();
    wxString name = items.GetNextToken();
    if (!name.IsEmpty() && (name != "<control>") &&
        (!name.StartsWith("<Plane")) && (regex_string.IsEmpty())) {
      long numVal;
      if (number.ToLong(&numVal, 16)) {
        m_grid->AppendRows();
        int row = m_grid->GetNumberRows() - 1;
        m_grid->SetCellValue(row, 0, number);
        m_grid->SetCellValue(row, 1, wxString(wxChar(numVal)));
        m_grid->SetCellValue(row, 2, name);
        m_grid->EnableEditing(false);
      }
    }
  }
  m_initialized = true;
}

void UnicodeSidebar::OnRegExEvent(wxCommandEvent &WXUNUSED(ev)) {
  UpdateDisplay();
}
