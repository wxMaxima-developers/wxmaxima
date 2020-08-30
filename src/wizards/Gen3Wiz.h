// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef GEN3WIZ_H
#define GEN3WIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

class Gen3Wiz : public wxDialog
{
public:
  Gen3Wiz(wxString lab1, wxString lab2, wxString lab3,
          wxString val1, wxString val2, wxString val3,
          Configuration *cfg,
          wxWindow *parent, int id, const wxString &title,
          bool eq = false,
          const wxString &warning = wxEmptyString,
          const wxString &warningToolTip = wxEmptyString,
          const wxPoint &pos = wxDefaultPosition,
          const wxSize &size = wxDefaultSize,
          long style = wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN);

  void SetValue(const wxString &s)
  {
    text_ctrl_1->SetValue(s);
    text_ctrl_1->SetSelection(-1, -1);
  }

  wxString GetValue1()
  {
    return text_ctrl_1->GetValue();
  };

  wxString GetValue2()
  {
    return text_ctrl_2->GetValue();
  };

  wxString GetValue3()
  {
    return text_ctrl_3->GetValue();
  };
  void SetLabel1ToolTip(wxString toolTip){label_2->SetToolTip(toolTip);}
  void SetLabel2ToolTip(wxString toolTip){label_3->SetToolTip(toolTip);}
  void SetLabel3ToolTip(wxString toolTip){label_4->SetToolTip(toolTip);}

private:
  void set_properties();

  void do_layout();

protected:
  wxStaticText *label_2;
  BTextCtrl *text_ctrl_1;
  wxStaticText *label_3;
  BTextCtrl *text_ctrl_2;
  wxStaticText *label_4;
  BTextCtrl *text_ctrl_3;
  wxStaticLine *static_line_1;
  wxButton *button_1;
  wxButton *button_2;
  wxStaticText *m_warning;
  wxString m_warningText;
};

#endif // GEN3WIZ_H
