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

#ifndef PLOT3DWIZ_H
#define PLOT3DWIZ_H

#include <wx/wx.h>
#include <wx/spinctrl.h>
#include <wx/statline.h>
#include <wx/bmpbuttn.h>

#include "BTextCtrl.h"

class Plot3DWiz: public wxDialog {
public:
  Plot3DWiz(wxWindow* parent, int id, const wxString& title,
            const wxPoint& pos=wxDefaultPosition,
            const wxSize& size=wxDefaultSize,
            long style=wxDEFAULT_DIALOG_STYLE);
  void setValue(wxString s);
  void parse(wxString s);
  void onCombobox(wxCommandEvent& event);
  void onFileBrowse(wxCommandEvent& event);
  wxString getValue();
private:
  void set_properties();
  void do_layout();
protected:
  int type;
  wxStaticText* label_1;
  wxStaticText* label_2;
  BTextCtrl* text_ctrl_1;
  wxStaticText* label_3;
  BTextCtrl* text_ctrl_2;
  wxStaticText* label_4;
  wxTextCtrl* text_ctrl_3;
  wxStaticText* label_5;
  wxTextCtrl* text_ctrl_4;
  wxStaticText* label_6;
  BTextCtrl* text_ctrl_5;
  wxStaticText* label_7;
  wxTextCtrl* text_ctrl_6;
  wxStaticText* label_8;
  wxTextCtrl* text_ctrl_7;
  wxStaticText* label_9;
  wxSpinCtrl* text_ctrl_8;
  wxStaticText* label_10;
  wxSpinCtrl* text_ctrl_9;
  wxStaticText* label_11;
  wxComboBox* combo_box_1;
  wxStaticText* label_12;
  wxComboBox* combo_box_2;
  wxCheckBox* check_box_1;
  wxStaticText* label_13;
  wxTextCtrl* text_ctrl_10;
  wxBitmapButton* button_3;
  wxStaticLine* static_line_1;
  wxButton* button_1;
  wxButton* button_2;
  DECLARE_EVENT_TABLE()
};


#endif // PLOT3DWIZ_H
