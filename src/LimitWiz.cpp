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

#include "LimitWiz.h"

LimitWiz::LimitWiz(wxWindow* parent, int id, const wxString& title,
                   const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_2 = new wxStaticText(this, -1, _("Expression:"));
  text_ctrl_1 = new BTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_2 = new BTextCtrl(this, -1, wxT("x"), wxDefaultPosition,
                              wxSize(110, -1));
  label_4 = new wxStaticText(this, -1, _("Point:"));
  text_ctrl_3 = new BTextCtrl(this, -1, wxT("0"), wxDefaultPosition,
                              wxSize(110, -1));
  button_1 = new wxButton(this, special, _("Special"));
  label_5 = new wxStaticText(this, -1, _("Direction:"));
  const wxString combo_box_1_choices[] =
    {
      _("both sides"),
      _("left"),
      _("right")
    };
  combo_box_1 = new wxComboBox(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(130, -1), 3,
                               combo_box_1_choices, wxCB_DROPDOWN | wxCB_READONLY);
  checkbox_1 = new wxCheckBox(this, -1, _("&Taylor series"));
  static_line_1 = new wxStaticLine(this, -1);

#if defined __WXMSW__
  button_2 = new wxButton(this, wxID_OK, _("OK"));
  button_3 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_3 = new wxButton(this, wxID_OK, _("OK"));
#endif

  button_2->SetDefault();

  set_properties();
  do_layout();
}


void LimitWiz::set_properties()
{
  SetTitle(_("Limit"));
  combo_box_1->SetSelection(0);
#if defined __WXMSW__
  button_2->SetDefault();
#else
  button_3->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}


void LimitWiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(3, 1, 0, 0);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(5, 2, 0, 0);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  grid_sizer_2->Add(label_4, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_1->Add(text_ctrl_3, 0, wxALL | wxEXPAND, 5);
  sizer_1->Add(button_1, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_1, 1, 0, 0);
  grid_sizer_2->Add(label_5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(combo_box_1, 0, wxALL, 5);
  grid_sizer_2->Add(20, 20, 0, wxALL, 5);
  grid_sizer_2->Add(checkbox_1, 9, wxALL, 5);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_2->Add(button_2, 0, wxALL, 5);
  sizer_2->Add(button_3, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString LimitWiz::GetValue()
{
  wxString s;
  if (checkbox_1->GetValue())
    s = wxT("tlimit(");
  else
    s = wxT("limit(");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", ");
  s += text_ctrl_3->GetValue();
  wxString f = combo_box_1->GetValue();
  if (f == _("left"))
    s += wxT(", minus");
  else if (f == _("right"))
    s += wxT(", plus");
  s += wxT(");");

  return s;
}

void LimitWiz::OnButton(wxCommandEvent& event)
{
  wxString choices[] = {wxT("Pi"), wxT("E"), _("Infinity"),
                        _("- Infinity")};
  wxString choice = wxGetSingleChoice(_("Select a constant"),
                                      _("Constant"), 4, choices, this);
  if (choice.Length())
  {
    if (choice == wxT("Pi"))
      text_ctrl_3->SetValue(wxT("%pi"));
    else if (choice == wxT("E"))
      text_ctrl_3->SetValue(wxT("%e"));
    else if (choice == _("Infinity"))
      text_ctrl_3->SetValue(wxT("inf"));
    else if (choice == _("- Infinity"))
      text_ctrl_3->SetValue(wxT("minf"));
  }
}

void LimitWiz::OnIdle(wxIdleEvent& ev)
{
  wxString point = text_ctrl_3->GetValue();

  if (point == wxT("inf") || point == wxT("-inf") || point == wxT("+inf") ||
      point == wxT("minf") || point == wxT("-minf") || point == wxT("+minf"))
  {
    combo_box_1->SetValue(wxEmptyString);
    combo_box_1->Enable(false);
  }
  else if (combo_box_1->IsEnabled() == false)
  {
    combo_box_1->Enable(true);
    if (combo_box_1->GetValue() == wxEmptyString)
      combo_box_1->SetValue(_("both sides"));
  }
}

BEGIN_EVENT_TABLE(LimitWiz, wxDialog)
  EVT_BUTTON(special, LimitWiz::OnButton)
  EVT_IDLE(LimitWiz::OnIdle)
END_EVENT_TABLE()
