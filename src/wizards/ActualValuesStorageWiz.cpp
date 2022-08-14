// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "ActualValuesStorageWiz.h"

ActualValuesStorageWiz::ActualValuesStorageWiz(Configuration *WXUNUSED(cfg),
                                               wxWindow *parent, int id,
                                               const wxString &title,
                                               bool WXUNUSED(eq),
                                               const wxPoint &pos,
                                               const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  wxStaticText *txt1 = new wxStaticText(
					this, -1, _("Maxima's power lies in symbolic operations."));
  vsizer->Add(txt1, wxSizerFlags().Expand().Border(wxTOP, 10));
  wxStaticText *txt2 =
    new wxStaticText(this, -1,
		     _("It therefore makes sense to apply the known values "
		       "for variables as late as possible."));
  vsizer->Add(txt2, wxSizerFlags().Expand().Border(wxALL, 0));
  wxStaticText *txt3 =
    new wxStaticText(this, -1,
		     _("Until then the actual values for all variables can "
		       "be kept in a list."));
  vsizer->Add(txt3, wxSizerFlags().Expand().Border(wxBOTTOM, 10));
  m_grid = new wxGrid(this, -1);
  m_grid->CreateGrid(1, 2);
  m_grid->SetColLabelValue(0, _("Variable name"));
  m_grid->SetColLabelValue(1, _("Value"));
  m_grid->SetColSize(0, 250);
  m_grid->SetColSize(1, 450);
  wxSize gridSize = m_grid->GetMinSize();
  gridSize.y = 500;
  m_grid->SetMinSize(gridSize);
  vsizer->Add(m_grid, wxSizerFlags().Expand().Border(wxALL, 10));
  m_grid->Connect(wxEVT_GRID_CELL_CHANGED,
                  wxGridEventHandler(ActualValuesStorageWiz::OnValueChange),
                  NULL, this);

  wxPanel *buttonPanel = new wxPanel(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(buttonPanel, wxID_OK, _("OK"));
  button_2 = new wxButton(buttonPanel, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(buttonPanel, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(buttonPanel, wxID_OK, _("OK"));
#endif
  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  buttonSizer->Add(button_1, 0, wxALL, 5);
  buttonSizer->Add(button_2, 0, wxALL, 5);
  buttonPanel->SetSizer(buttonSizer);
  vsizer->Add(buttonPanel, wxSizerFlags().Right());
  SetSizerAndFit(vsizer);

  set_properties();
  SetAutoLayout(true);
  Layout();
}

void ActualValuesStorageWiz::OnValueChange(wxGridEvent &event) {
  if (event.GetRow() >= m_grid->GetNumberRows() - 1) {
    if ((m_grid->GetCellValue(event.GetRow(), 0) != wxEmptyString) ||
        (m_grid->GetCellValue(event.GetRow(), 1) != wxEmptyString))
      m_grid->AppendRows();
    else {
      if ((event.GetRow() > 0) &&
          (m_grid->GetCellValue(event.GetRow() - 1, 0) == wxEmptyString) &&
          (m_grid->GetCellValue(event.GetRow() - 1, 1) == wxEmptyString))
        m_grid->DeleteRows(event.GetRow() - 1);
    }
  } else {
    if ((m_grid->GetCellValue(event.GetRow(), 0) == wxEmptyString) &&
        (m_grid->GetCellValue(event.GetRow(), 1) == wxEmptyString))
      m_grid->DeleteRows(event.GetRow());
  }
}

void ActualValuesStorageWiz::set_properties() {
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  m_grid->SetFocus();
}

wxString ActualValuesStorageWiz::GetValue() {
  bool firstline = true;
  wxString retval = wxT("[");
  for (int i = 0; i < m_grid->GetNumberRows(); i++) {
    if ((m_grid->GetCellValue(i, 0) != wxEmptyString) ||
        (m_grid->GetCellValue(i, 1) != wxEmptyString)) {
      if (!firstline)
        retval += wxT(",\n");
      else
        retval += wxT("\n");
      retval += wxT("    ") + m_grid->GetCellValue(i, 0) + wxT("=") +
	m_grid->GetCellValue(i, 1);
      firstline = false;
    }
  }
  retval += wxT("\n]");
  return retval;
}
