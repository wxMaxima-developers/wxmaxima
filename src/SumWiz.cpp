///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

enum {
  use_nusum_id
};

#include "SumWiz.h"

SumWiz::SumWiz(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_2 = new wxStaticText(this, -1, _("Expression:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("k"), wxDefaultPosition,
                              wxSize(110, -1));
  label_4 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_3 = new BTextCtrl(this, -1, wxT("1"), wxDefaultPosition,
                              wxSize(110, -1));
  label_5 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_4 = new BTextCtrl(this, -1, wxT("inf"), wxDefaultPosition,
                              wxSize(110, -1));
  checkbox_1 = new wxCheckBox(this, -1, _("&Simplify"));
  checkbox_2 = new wxCheckBox(this, use_nusum_id, _("&Nusum"));
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  button_1->SetDefault();
  set_properties();
  do_layout();
}


void SumWiz::set_properties()
{
  SetTitle(_("Sum"));
  checkbox_1->SetValue(true);
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  checkbox_1->SetToolTip(_("Simplify the sum"));
  checkbox_2->SetToolTip(_("Use Gosper algorithm"));

  text_ctrl_1->SetFocus();
}


void SumWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(6, 2, 0, 0);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_3, 0, wxALL, 5);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_4, 0, wxALL, 5);
  sizer_2->Add(checkbox_1, 0, wxALL, 5);
  sizer_2->Add(checkbox_2, 0, wxALL, 5);
  grid_sizer_2->Add(20, 20, 0, 0);
  grid_sizer_2->Add(sizer_2, 1, wxALIGN_LEFT, 0);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString SumWiz::GetValue()
{
  wxString s;
  if (checkbox_2->IsChecked())
    s = wxT("nusum(");
  else
    s = wxT("sum(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  s += wxT(", ");
  s += text_ctrl_4->GetValue();
  s += wxT(")");
  if (checkbox_1->IsChecked() && !checkbox_2->IsChecked())
    s += wxT(", simpsum;");
  else
    s += wxT(";");

  return s;
}

void SumWiz::OnCheckbox(wxCommandEvent& event)
{
  checkbox_1->Enable(!checkbox_2->GetValue());
}

BEGIN_EVENT_TABLE(SumWiz, wxDialog)
  EVT_CHECKBOX(use_nusum_id, SumWiz::OnCheckbox)
END_EVENT_TABLE()
