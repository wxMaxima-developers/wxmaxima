/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "MatWiz.h"

MatWiz::MatWiz(wxWindow* parent, int id, const wxString& title,
               int type, int w, int h,
               const wxPoint& pos, const wxSize& size, long style):
  wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  m_height = h;
  m_width = w;
  m_matrixType = type;
  label_1 = new wxStaticText(this, -1, title);
  for (int i=0; i<h*w; i++) {
    m_inputs.push_back(new BTextCtrl(this, -1, wxT("0"), wxDefaultPosition,
    wxSize(50,-1)));
  }
  static_line_1 = new wxStaticLine(this, -1);
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));

  set_properties();
  do_layout();
}

void MatWiz::set_properties()
{
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
  button_1->SetDefault();
  
  if (m_matrixType == MATRIX_ANTISYMETRIC) {
    for (int i=0; i<m_height; i++)
      for (int j=0; j<=i; j++)
        m_inputs[i*m_width+j]->Enable(false);
  }
  else if (m_matrixType == MATRIX_SYMETRIC) {
    for (int i=0; i<m_height; i++)
      for (int j=0; j<i; j++)
        m_inputs[i*m_width+j]->Enable(false);
  }
  else if (m_matrixType == MATRIX_DIAGONAL) {
    for (int i=0; i<m_height; i++)
      for (int j=0; j<m_width; j++)
        if (i!=j)
          m_inputs[i*m_width+j]->Enable(false);
  }
}

void MatWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(m_height+1, m_width+1, 1, 1);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxStaticText* text;
  grid_sizer_2->Add(20, 20, 0, 0);
  for (int i=1; i<=m_width; i++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), i),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
  }
  for (int j=0; j<m_height; j++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), j+1),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    for (int i=0; i<m_width; i++) {
      grid_sizer_2->Add(m_inputs[j*m_width+i], 0, wxALL, 1);
    }
  }
  grid_sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 1);
  grid_sizer_1->Add(grid_sizer_2, 1, wxALL|wxALIGN_CENTER_HORIZONTAL, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND|wxLEFT|wxRIGHT, 2);
  sizer_1->Add(button_2, 0, wxALL, 2);
  sizer_1->Add(button_1, 0, wxALL, 2);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT|wxBOTTOM, 2);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString MatWiz::GetValue()
{
  wxString cmd = wxT("matrix(");
  for (int i=0; i<m_height; i++) {
    cmd += wxT("[");
    for (int j=0; j<m_width; j++) {
      
      if (m_matrixType == MATRIX_SYMETRIC && i>j)
        cmd += m_inputs[j*m_width+i]->GetValue();
      else if (m_matrixType == MATRIX_ANTISYMETRIC && i>j)
        cmd += wxT("-(") + m_inputs[j*m_width+i]->GetValue() + wxT(")");
      else
        cmd += m_inputs[i*m_width+j]->GetValue();
      
      if (j<m_width-1)
        cmd += wxT(",");
      else
        cmd += wxT("]");
    }
    if (i<m_height-1)
      cmd += wxT(", ");
  }
  if (m_width>10 || m_height>10)
    cmd += wxT(")$");
  else
    cmd += wxT(");");
  return cmd;
}

MatDim::MatDim(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
  wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, title);
  label_2 = new wxStaticText(this, -1, _("Rows:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxT("3"), wxDefaultPosition,
                              wxSize(150,-1));
  label_3 = new wxStaticText(this, -1, _("Columns:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("3"), wxDefaultPosition,
                                wxSize(150,-1));
  label_4 = new wxStaticText(this, -1, _("Type:"));
  const wxString combo_box_1_choices[] = {
    _("general"),
    _("diagonal"),
    _("symmetric"),
    _("antisymmetric")
  };
  combo_box_1 = new wxComboBox(this, -1, wxT(""), wxDefaultPosition,
                               wxSize(150, -1), 4,
                               combo_box_1_choices,
                               wxCB_DROPDOWN|wxCB_READONLY);
  static_line_1 = new wxStaticLine(this, -1);
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  
  set_properties();
  do_layout();
}


void MatDim::set_properties()
{
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
  button_1->SetDefault();
  combo_box_1->SetSelection(0);
}


void MatDim::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(3, 2, 3, 3);
  grid_sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 1);
  grid_sizer_2->Add(label_2, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 2);
  grid_sizer_2->Add(label_3, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 2);
  grid_sizer_2->Add(label_4, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2);
  grid_sizer_2->Add(combo_box_1, 0, wxALL, 2);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND|wxLEFT|wxRIGHT, 2);
  sizer_1->Add(button_2, 0, wxALL, 2);
  sizer_1->Add(button_1, 0, wxALL, 2);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT|wxBOTTOM, 2);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

int MatDim::GetMatrixType()
{
  int type = combo_box_1->GetSelection();
  if (type == 0)
    return MATRIX_GENERAL;
  if (type == 1)
    return MATRIX_DIAGONAL;
  if (type == 2)
    return MATRIX_SYMETRIC;
  return MATRIX_ANTISYMETRIC;
}
