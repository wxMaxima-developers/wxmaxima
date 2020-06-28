// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef PLOT3DWIZ_H
#define PLOT3DWIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/spinctrl.h>
#include <wx/statline.h>
#include <wx/bmpbuttn.h>

#include "BTextCtrl.h"

class Plot3DWiz : public wxDialog
{
public:
  Plot3DWiz(wxWindow *parent, int id,
            Configuration *cfg,
            const wxString &title,
            const wxPoint &pos = wxDefaultPosition,
            const wxSize &size = wxDefaultSize,
            long style = wxDEFAULT_DIALOG_STYLE);

  void SetValue(wxString s);

  void Parse(wxString s);

  wxString GetValue();

private:
  enum
  {
    combobox,
    file_browse_3d
  };

  enum
  {
    cartesian,
    cylindrical,
    spherical
  };

  void OnCombobox(wxCommandEvent &event);

  void OnFileBrowse(wxCommandEvent &event);

  void set_properties();

  void do_layout();

protected:
  int type;
  wxStaticText *label_2;
  BTextCtrl *text_ctrl_1;
  wxStaticText *label_3;
  BTextCtrl *text_ctrl_2;
  wxStaticText *label_4;
  BTextCtrl *text_ctrl_3;
  wxStaticText *label_5;
  BTextCtrl *text_ctrl_4;
  wxStaticText *label_6;
  BTextCtrl *text_ctrl_5;
  wxStaticText *label_7;
  BTextCtrl *text_ctrl_6;
  wxStaticText *label_8;
  BTextCtrl *text_ctrl_7;
  wxStaticText *label_9;
  wxSpinCtrl *text_ctrl_8;
  wxStaticText *label_10;
  wxSpinCtrl *text_ctrl_9;
  wxStaticText *label_11;
  wxComboBox *combo_box_1;
  wxStaticText *label_12;
  wxComboBox *combo_box_2;
  wxCheckBox *check_box_1;
  wxStaticText *label_13;
  BTextCtrl *text_ctrl_10;
  wxBitmapButton *button_3;
  wxStaticLine *static_line_1;
  wxButton *button_1;
  wxButton *button_2;
DECLARE_EVENT_TABLE()
};

#endif // PLOT3DWIZ_H
