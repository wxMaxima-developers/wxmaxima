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



#include "IC1Wiz.h"


IC1Wiz::IC1Wiz(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
    label_1 = new wxStaticText(this, -1, _("IC1"));
    label_2 = new wxStaticText(this, -1, _("Solution:"));
    text_ctrl_1 = new BTextCtrl(this, -1, wxT(""), wxDefaultPosition,
                                wxSize(200,-1));
    label_3 = new wxStaticText(this, -1, _("At point:"));
    text_ctrl_2 = new BTextCtrl(this, -1, wxT("x="), wxDefaultPosition,
                                wxSize(60,-1));
    label_4 = new wxStaticText(this, -1, _("the value is:"));
    text_ctrl_3 = new BTextCtrl(this, -1, wxT("y="), wxDefaultPosition,
                                wxSize(60,-1));
    static_line_1 = new wxStaticLine(this, -1);
    button_1 = new wxButton(this, wxOK, _("OK"));
    button_2 = new wxButton(this, wxCANCEL, _("Cancel"));

    set_properties();
    do_layout();
    ok = false;
}


void IC1Wiz::set_properties()
{
    SetTitle(_("IC1"));
    label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
    button_1->SetDefault();
}


void IC1Wiz::do_layout()
{
    wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
    wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(2, 1, 3, 3);
    wxBoxSizer* sizer_3 = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
    grid_sizer_1->Add(label_1, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 2);
    sizer_2->Add(label_2, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
    sizer_2->Add(text_ctrl_1, 0, wxALL, 2);
    grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
    sizer_3->Add(label_3, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
    sizer_3->Add(text_ctrl_2, 0, wxALL, 2);
    sizer_3->Add(label_4, 0, wxALL|wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL, 2);
    sizer_3->Add(text_ctrl_3, 0, 0, 0);
    grid_sizer_2->Add(sizer_3, 1, wxEXPAND, 0);
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


void IC1Wiz::onButton(wxCommandEvent& event)
{
  if (event.GetId() == wxOK)
    ok = true;
  Close();
}

wxString IC1Wiz::getValue() {
  wxString s;
  s += wxT("ic1(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  s += wxT(");");

  return s;
}

BEGIN_EVENT_TABLE(IC1Wiz, wxDialog)
  EVT_BUTTON(wxOK, IC1Wiz::onButton)
  EVT_BUTTON(wxCANCEL, IC1Wiz::onButton)
END_EVENT_TABLE()
