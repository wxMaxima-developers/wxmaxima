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



#include "SeriesWiz.h"

SeriesWiz::SeriesWiz(wxWindow* parent, int id, const wxString& title,
                     const wxPoint& pos, const wxSize& size, long style):
  wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, _("Series"));
  label_2 = new wxStaticText(this, -1, _("Expression:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxT(""), wxDefaultPosition,
                              wxSize(180,-1));
  label_3 = new wxStaticText(this, -1, _("var:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("x"));
  label_4 = new wxStaticText(this, -1, _("around:"));
  text_ctrl_3 = new BTextCtrl(this, -1, wxT("0"));
  button_3 = new wxButton(this, special_sw, _("Special"));
  label_5 = new wxStaticText(this, -1, _("depth:"));
  text_ctrl_4 = new BTextCtrl(this, -1, wxT("8"));
  checkbox_1 = new wxCheckBox(this, -1, _("Power series"));
  static_line_1 = new wxStaticLine(this, -1);
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));

  set_properties();
  do_layout();
  ok = false;
}


void SeriesWiz::set_properties()
{
  SetTitle(_("Series"));
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
  button_1->SetDefault();
}


void SeriesWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(5, 2, 3, 3);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(label_1, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 2);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 2);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 2);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  sizer_2->Add(text_ctrl_3, 0, wxALL, 2);
  sizer_2->Add(button_3, 0, wxALL, 2);
  grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_2->Add(text_ctrl_4, 0, wxALL, 2);
  grid_sizer_2->Add(20, 20, 0, 0, 0);
  grid_sizer_2->Add(checkbox_1, 0, 0, 0);
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

void SeriesWiz::onButton(wxCommandEvent& event)
{
  switch (event.GetId()) {
  case wxID_OK:
    event.Skip();
    break;
  case special_sw:
    {
      wxString choices[] = {wxT("Pi"), wxT("E")};
      wxString choice = wxGetSingleChoice(_("Select a constant"),
                                          _("Constant"), 2, choices, this);
      if (choice.Length()) {
        if (choice == wxT("Pi"))
          text_ctrl_3->SetValue(wxT("%pi"));
        else if (choice == wxT("E"))
          text_ctrl_3->SetValue(wxT("%e"));
      }
    }
    break;
  }
}

wxString SeriesWiz::getValue()
{
  wxString s;
  if (checkbox_1->IsChecked())
    s = wxT("niceindices(powerseries(");
  else
    s = wxT("taylor(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  if (!checkbox_1->IsChecked()) {
    s += wxT(", ");
    s += text_ctrl_4->GetValue();
    s += wxT(");");
  }
  else
    s += wxT("));");

  return s;
}

BEGIN_EVENT_TABLE(SeriesWiz, wxDialog)
  EVT_BUTTON(wxID_OK, SeriesWiz::onButton)
  EVT_BUTTON(special_sw, SeriesWiz::onButton)
END_EVENT_TABLE()
