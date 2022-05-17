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

#ifndef GENWIZ_H
#define GENWIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/statline.h>
#include <vector>
#include "BTextCtrl.h"
#include "WrappingStaticText.h"

class GenWiz : public wxDialog
{
public:
  GenWiz(wxWindow *parent, Configuration *cfg,
         const wxString &title,
         const wxString &description, const wxString &description_tooltip,
         const wxString &commandRule,
         wxString label1, wxString defaultval1, wxString tooltip1 = {},
         wxString label2 = {}, wxString defaultval2 = {}, wxString tooltip2 = {},
         wxString label3 = {}, wxString defaultval3 = {}, wxString tooltip3 = {},
         wxString label4 = {}, wxString defaultval4 = {}, wxString tooltip4 = {},
         wxString label5 = {}, wxString defaultval5 = {}, wxString tooltip5 = {},
         wxString label6 = {}, wxString defaultval6 = {}, wxString tooltip6 = {},
         wxString label7 = {}, wxString defaultval7 = {}, wxString tooltip7 = {},
         wxString label8 = {}, wxString defaultval8 = {}, wxString tooltip8 = {},
         wxString label9 = {}, wxString defaultval9 = {}, wxString tooltip9 = {});

  wxString operator[](int i) const { return m_textctrl[i]->GetValue(); }

  wxString GetOutput() const {return m_output->GetValue();}
  
protected:
  void UpdateOutput();
  void OnParamChange(wxCommandEvent& event);

  std::vector<BTextCtrl*> m_textctrl;
  std::vector<wxStaticText *> m_label;
  wxButton *button_1;
  wxButton *button_2;
  WrappingStaticText *m_description;
  wxString m_commandRule;
  wxTextCtrl *m_output;
};

#endif // GENWIZ_H
