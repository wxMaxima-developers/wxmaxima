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
  height = h;
  width = w;
  matrix_type = type;
  label_1 = new wxStaticText(this, -1, title);
  for (int i=0; i<h*w; i++) {
    inputs.push_back(new BTextCtrl(this, -1, wxT("0"), wxDefaultPosition,
    wxSize(50,-1)));
  }
  static_line_1 = new wxStaticLine(this, -1);
  button_1 = new wxButton(this, wxOK, _("OK"));
  button_2 = new wxButton(this, wxCANCEL, _("Cancel"));
  button_1->SetDefault();

  set_properties();
  do_layout();

  ok = false;
}

void MatWiz::set_properties()
{
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
  
  if (matrix_type == MATRIX_ANTISYMETRIC) {
    for (int i=0; i<height; i++)
      for (int j=0; j<=i; j++)
        inputs[i*width+j]->Enable(false);
  }
  else if (matrix_type == MATRIX_SYMETRIC) {
    for (int i=0; i<height; i++)
      for (int j=0; j<i; j++)
        inputs[i*width+j]->Enable(false);
  }
  else if (matrix_type == MATRIX_DIAGONAL) {
    for (int i=0; i<height; i++)
      for (int j=0; j<width; j++)
        if (i!=j)
          inputs[i*width+j]->Enable(false);
  }
}

void MatWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(height+1, width+1, 1, 1);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxStaticText* text;
  grid_sizer_2->Add(20, 20, 0, 0);
  for (int i=1; i<=width; i++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), i),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
  }
  for (int j=0; j<height; j++) {
    text = new wxStaticText(this, -1, wxString::Format(wxT("%d"), j+1),
                            wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    grid_sizer_2->Add(text, 0,
                      wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    for (int i=0; i<width; i++) {
      grid_sizer_2->Add(inputs[j*width+i], 0, wxALL, 1);
    }
  }
  grid_sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND, 0);
  sizer_1->Add(button_1, 0, wxALL, 2);
  sizer_1->Add(button_2, 0, wxALL, 2);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString MatWiz::getValue()
{
  wxString cmd = wxT("matrix(");
  for (int i=0; i<height; i++) {
    cmd += wxT("[");
    for (int j=0; j<width; j++) {
      
      if (matrix_type == MATRIX_SYMETRIC && i>j)
        cmd += inputs[j*width+i]->GetValue();
      else if (matrix_type == MATRIX_ANTISYMETRIC && i>j)
        cmd += wxT("-(") + inputs[j*width+i]->GetValue() + wxT(")");
      else
        cmd += inputs[i*width+j]->GetValue();
      
      if (j<width-1)
        cmd += wxT(",");
      else
        cmd += wxT("]");
    }
    if (i<height-1)
      cmd += wxT(", ");
  }
  if (width>10 || height>10)
    cmd += wxT(")$");
  else
    cmd += wxT(");");
  return cmd;
}

void MatWiz::onButton(wxCommandEvent& event)
{
  if (event.GetId() == wxOK)
    ok = true;
  Close();
}

BEGIN_EVENT_TABLE(MatWiz, wxDialog)
  EVT_BUTTON(wxOK, MatWiz::onButton)
  EVT_BUTTON(wxCANCEL, MatWiz::onButton)
END_EVENT_TABLE()


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
  button_1 = new wxButton(this, wxOK, _("OK"));
  button_2 = new wxButton(this, wxCANCEL, _("Cancel"));
  
  set_properties();
  do_layout();
  ok = false;
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
  grid_sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
  grid_sizer_2->Add(label_2, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 2);
  grid_sizer_2->Add(label_3, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 2);
  grid_sizer_2->Add(label_4, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(combo_box_1, 0, wxALL, 2);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND, 0);
  sizer_1->Add(button_1, 0, wxALL, 2);
  sizer_1->Add(button_2, 0, wxALL, 2);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

void MatDim::onButton(wxCommandEvent& event)
{
  if (event.GetId() == wxOK)
    ok = true;
  Close();
}

int MatDim::getMatrixType()
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

BEGIN_EVENT_TABLE(MatDim, wxDialog)
  EVT_BUTTON(wxOK, MatDim::onButton)
  EVT_BUTTON(wxCANCEL, MatDim::onButton)
END_EVENT_TABLE()
