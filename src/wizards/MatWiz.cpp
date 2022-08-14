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

#include "MatWiz.h"

MatWiz::MatWiz(wxWindow *parent, int id, Configuration *cfg,
               const wxString &title, int type, int h, int w,
               const wxPoint &pos, const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  m_height = h;
  m_width = w;
  m_matrixType = type;
  int width = 50 > 400 / m_width ? 50 : 400 / m_width;
  for (int i = 0; i < h * w; i++) {
    m_inputs.push_back(new BTextCtrl(this, -1, cfg, wxT("0"), wxDefaultPosition,
                                     wxSize(width, -1)));
  }
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
}

void MatWiz::set_properties() {
  button_1->SetDefault();

  if (m_matrixType == MATRIX_ANTISYMMETRIC) {
    for (int i = 0; i < m_height; i++)
      for (int j = 0; j <= i; j++) {
        m_inputs[i * m_width + j]->SetValue(wxEmptyString);
        m_inputs[i * m_width + j]->Enable(false);
      }
  } else if (m_matrixType == MATRIX_SYMMETRIC) {
    for (int i = 0; i < m_height; i++)
      for (int j = 0; j < i; j++) {
        m_inputs[i * m_width + j]->SetValue(wxEmptyString);
        m_inputs[i * m_width + j]->Enable(false);
      }
  } else if (m_matrixType == MATRIX_DIAGONAL) {
    for (int i = 0; i < m_height; i++)
      for (int j = 0; j < m_width; j++)
        if (i != j) {
          m_inputs[i * m_width + j]->SetValue(wxEmptyString);
          m_inputs[i * m_width + j]->Enable(false);
        }
  }
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  m_inputs[0]->SetFocus();
  m_inputs[0]->SetSelection(-1, -1);
}

void MatWiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxFlexGridSizer *grid_sizer_2 =
    new wxFlexGridSizer(m_height + 1, m_width + 1, 2, 2);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxStaticText *text;
  grid_sizer_2->Add(20, 20, 0, 0);
  for (int i = 1; i <= m_width; i++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), i),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL | wxALIGN_CENTER_VERTICAL, 0);
  }
  for (int j = 0; j < m_height; j++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), j + 1),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL | wxALIGN_CENTER_VERTICAL, 0);
    for (int i = 0; i < m_width; i++) {
      grid_sizer_2->Add(m_inputs[j * m_width + i], 0, wxALL, 1);
    }
  }
  grid_sizer_1->Add(grid_sizer_2, 1, wxALL | wxALIGN_CENTER_HORIZONTAL, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxALL, 2);
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString MatWiz::GetValue() {
  wxString cmd = wxT("matrix(");
  for (int i = 0; i < m_height; i++) {
    cmd += wxT("\n [");
    for (int j = 0; j < m_width; j++) {
      if (m_matrixType == MATRIX_SYMMETRIC && i > j)
        cmd += m_inputs[j * m_width + i]->GetValue();
      else if (m_matrixType == MATRIX_ANTISYMMETRIC && i > j)
        cmd += wxT("-(") + m_inputs[j * m_width + i]->GetValue() + wxT(")");
      else {
        wxString entry = m_inputs[i * m_width + j]->GetValue();
        if (entry == wxEmptyString)
          entry = wxT("0");
        cmd += entry;
      }

      if (j < m_width - 1)
        cmd += wxT(",");
      else
        cmd += wxT("]");
    }
    if (i < m_height - 1)
      cmd += wxT(", ");
  }
  if (m_width > 7 || m_height > 7)
    cmd += wxT("\n)$");
  else
    cmd += wxT("\n);");
  return cmd;
}

MatDim::MatDim(wxWindow *parent, int id, Configuration *cfg,
               const wxString &title, const wxPoint &pos, const wxSize &size,
               long style)
  : wxDialog(parent, id, title, pos, size, style) {
  label_2 = new wxStaticText(this, -1, _("Rows:"));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxT("3"), wxDefaultPosition,
                              wxSize(150, -1));
  label_3 = new wxStaticText(this, -1, _("Columns:"));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxT("3"), wxDefaultPosition,
                              wxSize(150, -1));
  label_4 = new wxStaticText(this, -1, _("Type:"));
  const wxString choice_1_choices[] = {_("general"), _("diagonal"),
    _("symmetric"), _("antisymmetric")};
  choice_1 = new wxChoice(this, -1, wxDefaultPosition, wxSize(150, -1), 4,
                          choice_1_choices);
  label_0 = new wxStaticText(this, -1, _("Name:"));
  text_ctrl_0 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
}

void MatDim::set_properties() {
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  choice_1->SetSelection(0);
  text_ctrl_1->SetFocus();
  text_ctrl_1->SetSelection(-1, -1);
}

void MatDim::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(4, 2, 0, 0);
  grid_sizer_2->Add(label_2, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  grid_sizer_2->Add(label_4, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                    5);
  grid_sizer_2->Add(choice_1, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_2->Add(label_0, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                    5);
  grid_sizer_2->Add(text_ctrl_0, 0, wxALL, 5);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxALL, 2);
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

int MatDim::GetMatrixType() {
  int type = choice_1->GetSelection();
  if (type == 0)
    return MatWiz::MATRIX_GENERAL;
  if (type == 1)
    return MatWiz::MATRIX_DIAGONAL;
  if (type == 2)
    return MatWiz::MATRIX_SYMMETRIC;
  return MatWiz::MATRIX_ANTISYMMETRIC;
}
