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



#include "BC2Wiz.h"


BC2Wiz::BC2Wiz(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, _("BC2"));
  label_2 = new wxStaticText(this, -1, _("Solution:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxT(""), wxDefaultPosition,
                                wxSize(200,-1));
  label_3 = new wxStaticText(this, -1, _("At point:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("x="), wxDefaultPosition,
                                wxSize(60,-1));
  label_4 = new wxStaticText(this, -1, _("the value is:"));
  text_ctrl_3 = new BTextCtrl(this, -1, wxT("y="), wxDefaultPosition,
                                wxSize(60,-1));
  label_5 = new wxStaticText(this, -1, _("At point:"));
  text_ctrl_4 = new BTextCtrl(this, -1, wxT("x="), wxDefaultPosition,
                                wxSize(60,-1));
  label_6 = new wxStaticText(this, -1, _("the value is:"));
  text_ctrl_5 = new BTextCtrl(this, -1, wxT("y="), wxDefaultPosition,
                                wxSize(60,-1));
  static_line_1 = new wxStaticLine(this, -1);
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_1 = new wxButton(this, wxID_OK, _("OK"));

  set_properties();
  do_layout();
  ok = false;
}


void BC2Wiz::set_properties()
{
  SetTitle(_("BC2"));
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
  button_1->SetDefault();
}


void BC2Wiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(2, 1, 3, 3);
  wxFlexGridSizer* grid_sizer_3 = new wxFlexGridSizer(2, 4, 3, 3);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(label_1, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 2);
  sizer_2->Add(label_2, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  sizer_2->Add(text_ctrl_1, 0, wxALL, 2);
  grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
  grid_sizer_3->Add(label_3, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_3->Add(text_ctrl_2, 0, 0, 0);
  grid_sizer_3->Add(label_4, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_3->Add(text_ctrl_3, 0, 0, 0);
  grid_sizer_3->Add(label_5, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_3->Add(text_ctrl_4, 0, 0, 0);
  grid_sizer_3->Add(label_6, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_3->Add(text_ctrl_5, 0, 0, 0);
  grid_sizer_2->Add(grid_sizer_3, 1, wxALIGN_CENTER_HORIZONTAL, 0);
  grid_sizer_1->Add(grid_sizer_2, 1, 0, 0);
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


void BC2Wiz::onButton(wxCommandEvent& event)
{
  if (event.GetId() == wxID_OK)
    ok = true;
  event.Skip();
}

wxString BC2Wiz::getValue() {
  wxString s;
  s += wxT("bc2(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  s += wxT(", ");
  s += text_ctrl_4->GetValue();
  s += wxT(", ");
  s += text_ctrl_5->GetValue();
  s += wxT(");");

  return s;
}
