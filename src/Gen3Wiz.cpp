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



#include "Gen3Wiz.h"

Gen3Wiz::Gen3Wiz(wxString lab1, wxString lab2, wxString lab3,
                 wxString val1, wxString val2, wxString val3,
                 wxWindow* parent, int id, const wxString& title,
                 const wxPoint& pos, const wxSize& size, long style):
  wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, title);
  label_2 = new wxStaticText(this, -1, lab1);
  text_ctrl_1 = new BTextCtrl(this, -1, val1, wxDefaultPosition,
                              wxSize(180,-1));
  label_3 = new wxStaticText(this, -1, lab2);
  text_ctrl_2 = new BTextCtrl(this, -1, val2);
  label_4 = new wxStaticText(this, -1, lab3);
  text_ctrl_3 = new BTextCtrl(this, -1, val3);
  static_line_1 = new wxStaticLine(this, -1);
  button_1 = new wxButton(this, wxOK, _("OK"));
  button_2 = new wxButton(this, wxCANCEL, _("Cancel"));
  button_1->SetDefault();

  set_properties();
  do_layout();
  ok = false;
}

void Gen3Wiz::do_layout()
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
  grid_sizer_2->Add(text_ctrl_3, 0, wxALL, 2);
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

void Gen3Wiz::set_properties()
{
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
}

void Gen3Wiz::onButton(wxCommandEvent& event)
{
  if (event.GetId()==wxOK)
    ok = true;
  Close();
}

BEGIN_EVENT_TABLE(Gen3Wiz, wxDialog)
  EVT_BUTTON(wxOK, Gen3Wiz::onButton)
  EVT_BUTTON(wxCANCEL, Gen3Wiz::onButton)
END_EVENT_TABLE()
