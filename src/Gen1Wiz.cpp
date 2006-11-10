///
///  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "Gen1Wiz.h"

Gen1Wiz::Gen1Wiz(wxWindow* parent, int id, const wxString& title,
                 const wxString& label1,
                 const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  label_1 = new wxStaticText(this, -1, title);
  label_2 = new wxStaticText(this, -1, label1);
  text_ctrl_1 = new BTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                              wxSize(300, -1));
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


void Gen1Wiz::set_properties()
{
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxEmptyString));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif
  
  text_ctrl_1->SetFocus();
}


void Gen1Wiz::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(4, 1, 3, 3);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(label_1, 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 3);
  grid_sizer_1->Add(label_2, 0, wxALL | wxALIGN_CENTER_HORIZONTAL, 0);
  grid_sizer_1->Add(text_ctrl_1, 0, wxALL | wxEXPAND, 3);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_1->Add(button_1, 0, wxLEFT | wxRIGHT, 5);
  sizer_1->Add(button_2, 0, wxLEFT | wxRIGHT, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT | wxTOP | wxBOTTOM, 3);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString GetTextFromUser(wxString label, wxString title, wxString value,
                         wxWindow* parent)
{
  Gen1Wiz *wiz = new Gen1Wiz(parent, -1, title, label);
  wiz->SetValue(value);
  wxString val;
  wiz->Centre(wxBOTH);
  if (wiz->ShowModal() == wxID_OK)
  {
    val = wiz->GetValue();
    val.Trim();
    val.Trim(false);
  }
  wiz->Destroy();
  return val;
}
