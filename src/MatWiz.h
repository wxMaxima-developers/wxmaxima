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


#ifndef MATWIZ_H
#define MATWIZ_H

#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

enum {
  MATRIX_GENERAL,
  MATRIX_DIAGONAL,
  MATRIX_SYMETRIC,
  MATRIX_ANTISYMETRIC
};

#include <vector>
using namespace std;

class MatWiz: public wxDialog {
public:
  MatWiz(wxWindow* parent, int id, const wxString& title, int type,
         int heigth, int width, const wxPoint& pos=wxDefaultPosition,
         const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);
  bool isOk() { return ok; }
  void onButton(wxCommandEvent& event);
  wxString getValue();
private:
  bool ok;
  void set_properties();
  void do_layout();
  int width, height;
  int matrix_type;
  wxStaticText* label_1;
  vector<BTextCtrl*> inputs;
  wxStaticLine* static_line_1;
  wxButton* button_1;
  wxButton* button_2;
  DECLARE_EVENT_TABLE()
};

class MatDim: public wxDialog {
public:
  MatDim(wxWindow* parent, int id, const wxString& title,
         const wxPoint& pos=wxDefaultPosition,
         const wxSize& size=wxDefaultSize,
         long style=wxDEFAULT_DIALOG_STYLE);
  bool isOk() { return ok; }
  wxString getValue_1() { return text_ctrl_1->GetValue(); }
  wxString getValue_2() { return text_ctrl_2->GetValue(); }
  int getMatrixType();
  void onButton(wxCommandEvent& event);
private:
  bool ok;
  void set_properties();
  void do_layout();
protected:
  wxStaticText* label_1;
  wxStaticText* label_2;
  BTextCtrl* text_ctrl_1;
  wxStaticText* label_3;
  BTextCtrl* text_ctrl_2;
  wxStaticText* label_4;
  wxComboBox* combo_box_1;
  wxStaticLine* static_line_1;
  wxButton* button_1;
  wxButton* button_2;
  DECLARE_EVENT_TABLE()
};


#endif // LIMITWIZ_H
