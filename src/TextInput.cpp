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


#include "TextInput.h"

TextInput::TextInput(wxWindow* parent, int id, const wxString& title,
                     bool setfont, const wxPoint& pos, const wxSize& size,
                     long style):
  wxDialog(parent, id, title, pos, size,
           wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMINIMIZE_BOX|wxMAXIMIZE_BOX),
  setFont(setfont)
{
  text_ctrl_1 = new TextCtrl(this, -1, wxT(""), wxDefaultPosition,
                             wxDefaultSize,
                             wxTE_PROCESS_TAB|wxTE_PROCESS_ENTER|wxTE_MULTILINE|wxTE_RICH);
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_1 = new wxButton(this, wxID_OK, _("OK"));

  set_properties();
  do_layout();
  text_ctrl_1->SetFocus();
}

wxString TextInput::GetValue()
{
  return (text_ctrl_1->GetValue()).Strip();
}

void TextInput::SetValue(wxString s)
{
  if (s!=wxT("%")) {
    text_ctrl_1->SetValue(s);
    text_ctrl_1->SetInsertionPoint(text_ctrl_1->GetLastPosition());
  }
}

void TextInput::set_properties()
{
  SetSize(wxSize(780, 550));
  SetTitle(_("wxMaxima input"));
  if (setFont)
#if defined(__WXGTK12__) && !defined(__WXGTK20__)
    text_ctrl_1->SetFont(wxFont(14, wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("")));
#else
    text_ctrl_1->SetFont(wxFont(12, wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("")));
#endif
  button_1->SetDefault();
}


void TextInput::do_layout()
{
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(2, 1, 0, 0);
  wxBoxSizer* sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_1->Add(text_ctrl_1, 0, wxALL|wxEXPAND, 2);
  sizer_1->Add(button_2, 0, wxLEFT|wxRIGHT, 5);
  sizer_1->Add(button_1, 0, wxLEFT|wxRIGHT, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_CENTER|wxTOP|wxBOTTOM, 3);

  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->AddGrowableRow(0);
  grid_sizer_1->AddGrowableCol(0);
  Layout();
}
