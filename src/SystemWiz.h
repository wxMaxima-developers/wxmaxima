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

#ifndef SYSTEMWIZ_H
#define SYSTEMWIZ_H

#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

#include <vector>

class SysWiz : public wxDialog
{
public:
  SysWiz(wxWindow *parent, int id,
         Configuration *cfg,
         const wxString &title,
         int numEq, const wxPoint &pos = wxDefaultPosition,
         const wxSize &sz = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);

  wxString GetValue();

private:
  void set_properties();

  void do_layout();

  int m_size;
  std::vector<BTextCtrl *> m_inputs;
  BTextCtrl *variables;
  wxStaticLine *static_line_1;
  wxButton *button_1;
  wxButton *button_2;
};

#endif // SYSTEMWIZ_H
