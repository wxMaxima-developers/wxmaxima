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

#include "IntegrateWiz.h"

IntegrateWiz::IntegrateWiz(wxWindow* parent, int id,
                           const wxString& title, const wxPoint& pos,
                           const wxSize& size, long style):
  wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, _("Integrate"));
  label_2 = new wxStaticText(this, -1, _("Integrate:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxT(""), wxDefaultPosition,
                              wxSize(180,-1));
  label_3 = new wxStaticText(this, -1, _("by variable:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("x"));
  label_4 = new wxStaticText(this, -1, _("from:"));
  text_ctrl_3 = new BTextCtrl(this, -1, wxT(""));
  button_3 = new wxButton(this, special_from, _("Special"));
  label_5 = new wxStaticText(this, -1, _("to:"));
  text_ctrl_4 = new BTextCtrl(this, -1, wxT(""));
  button_4 = new wxButton(this, special_to, _("Special"));
  checkbox_1 = new wxCheckBox(this, -1, _("Numerical"));
  static_line_1 = new wxStaticLine(this, -1);
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_1 = new wxButton(this, wxID_OK, _("OK"));

  set_properties();
  do_layout();
}


void IntegrateWiz::set_properties()
{
  SetTitle(_("Integrate"));
  label_1->SetFont(wxFont(20, wxROMAN, wxSLANT, wxNORMAL, 0, wxT("")));
  button_1->SetDefault();
}


void IntegrateWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_3 = new wxFlexGridSizer(4, 1, 3, 3);
  wxBoxSizer* sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_4 = new wxFlexGridSizer(5, 2, 3, 3);
  wxFlexGridSizer* grid_sizer_6 = new wxFlexGridSizer(1, 2, 0, 0);
  wxFlexGridSizer* grid_sizer_5 = new wxFlexGridSizer(1, 2, 0, 0);
  grid_sizer_3->Add(label_1, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 2);
  grid_sizer_4->Add(label_2, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_4->Add(text_ctrl_1, 0, wxALL, 2);
  grid_sizer_4->Add(label_3, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_4->Add(text_ctrl_2, 0, wxALL, 2);
  grid_sizer_4->Add(label_4, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_5->Add(text_ctrl_3, 0, wxALL|wxEXPAND, 2);
  grid_sizer_5->Add(button_3, 0, wxALL, 2);
  grid_sizer_5->AddGrowableCol(0);
  grid_sizer_4->Add(grid_sizer_5, 1, 0, 0);
  grid_sizer_4->Add(label_5, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 2);
  grid_sizer_6->Add(text_ctrl_4, 0, wxALL|wxEXPAND, 2);
  grid_sizer_6->Add(button_4, 0, wxALL, 2);
  grid_sizer_6->AddGrowableCol(0);
  grid_sizer_4->Add(grid_sizer_6, 1, 0, 0);
  grid_sizer_4->Add(20, 20, 0, 0);
  grid_sizer_4->Add(checkbox_1, 0, wxALL, 2);
  grid_sizer_3->Add(grid_sizer_4, 1, wxEXPAND, 0);
  grid_sizer_3->Add(static_line_1, 0, wxEXPAND|wxLEFT|wxRIGHT, 2);
  sizer_3->Add(button_2, 0, wxALL, 2);
  sizer_3->Add(button_1, 0, wxALL, 2);
  grid_sizer_3->Add(sizer_3, 1, wxALIGN_RIGHT|wxBOTTOM, 2);
  SetAutoLayout(true);
  SetSizer(grid_sizer_3);
  grid_sizer_3->Fit(this);
  grid_sizer_3->SetSizeHints(this);
  Layout();
}

wxString IntegrateWiz::getValue()
{
  wxString s;
  if (checkbox_1->IsChecked())
    s = wxT("romberg(");
  else
    s = wxT("integrate(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  wxString from = text_ctrl_3->GetValue();
  wxString to = text_ctrl_4->GetValue();
  if (from.Length()>0 && to.Length()>0) {
    s += wxT(", ");
    s += from;
    s += wxT(", ");
    s += to;
  }
  s += wxT(");");
  
  return s;
}

void IntegrateWiz::onButton(wxCommandEvent& event) {
  switch (event.GetId()) {
  case special_from:
    {
      wxString choices[] = {wxT("Pi"), wxT("E"), wxT("Infinity"),
                            wxT("- Infinity")};
      wxString choice = wxGetSingleChoice(_("Select a constant"),
                                          _("Constant"), 4, choices, this);
      if (choice.Length()) {
        if (choice == wxT("Pi"))
          text_ctrl_3->SetValue(wxT("%pi"));
        else if (choice == wxT("E"))
          text_ctrl_3->SetValue(wxT("%e"));
        else if (choice == wxT("Infinity"))
          text_ctrl_3->SetValue(wxT("inf"));
        else if (choice == wxT("- Infinity"))
          text_ctrl_3->SetValue(wxT("minf"));
      }
    }
    break;
  case special_to:
    {
      wxString choices[] = {wxT("Pi"), wxT("E"), wxT("Infinity"),
                            wxT("- Infinity")};
      wxString choice = wxGetSingleChoice(_("Select a constant"),
                                          _("Constant"), 4, choices, this);
      if (choice.Length()) {
        if (choice == wxT("Pi"))
          text_ctrl_4->SetValue(wxT("%pi"));
        else if (choice == wxT("E"))
          text_ctrl_4->SetValue(wxT("%e"));
        else if (choice == wxT("Infinity"))
          text_ctrl_4->SetValue(wxT("inf"));
        else if (choice == wxT("- Infinity"))
          text_ctrl_4->SetValue(wxT("minf"));
      }
    }
    break;
  }
}

BEGIN_EVENT_TABLE(IntegrateWiz, wxDialog)
  EVT_BUTTON(special_from, IntegrateWiz::onButton)
  EVT_BUTTON(special_to, IntegrateWiz::onButton)
END_EVENT_TABLE()
