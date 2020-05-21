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

#ifndef MATWIZ_H
#define MATWIZ_H

#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

#include <vector>

class MatWiz : public wxDialog
{
public:
  enum MatrixType
  {
    MATRIX_GENERAL,
    MATRIX_DIAGONAL,
    MATRIX_SYMMETRIC,
    MATRIX_ANTISYMMETRIC
  };

  MatWiz(wxWindow *parent, int id,
         Configuration *cfg,
         const wxString &title,
         int type, int h, int w,
         const wxPoint &pos = wxDefaultPosition,
         const wxSize &size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);

  wxString GetValue();

private:
  void OnButton(wxCommandEvent &event);

  void set_properties();

  void do_layout();

  int m_width, m_height;
  int m_matrixType;
  std::vector<BTextCtrl *> m_inputs;
  wxStaticLine *static_line_1;
  wxButton *button_1;
  wxButton *button_2;
};

class MatDim : public wxDialog
{
public:
  MatDim(wxWindow *parent, int id,
         Configuration *cfg,
         const wxString &title,
         const wxPoint &pos = wxDefaultPosition,
         const wxSize &size = wxDefaultSize,
         long style = wxDEFAULT_DIALOG_STYLE);

  wxString GetValue1()
  {
    return text_ctrl_1->GetValue();
  }

  wxString GetValue2()
  {
    return text_ctrl_2->GetValue();
  }

  wxString GetValue0()
  {
    return text_ctrl_0->GetValue();
  }

  int GetMatrixType();

private:
  void set_properties();

  void do_layout();

protected:
  wxStaticText *label_0;
  BTextCtrl *text_ctrl_0;
  wxStaticText *label_2;
  BTextCtrl *text_ctrl_1;
  wxStaticText *label_3;
  BTextCtrl *text_ctrl_2;
  wxStaticText *label_4;
  wxChoice *choice_1;
  wxStaticLine *static_line_1;
  wxButton *button_1;
  wxButton *button_2;
};

#endif // MATWIZ_H
