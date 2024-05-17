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

/*! \file
  This file declares all the wizards the draw sidepane needs.
*/

#ifndef BINARYNAMECTRL_H
#define BINARYNAMECTRL_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/panel.h>
#include "../Configuration.h"


//! A dialog that shows the program's license.
class BinaryNameCtrl : public wxPanel
{
public:
  explicit BinaryNameCtrl(wxWindow *parent, int id = wxID_ANY);
  wxString GetValue() const {return m_binaryName->GetValue();}
  void SetValue(const wxString &val)  {m_binaryName->SetValue(val);}
  void SetToolTip(const wxString tip){m_binaryName->SetToolTip(tip);}
protected:
  void TextChanged(wxCommandEvent evt);
  void TextChanged();
  void OnBrowse(wxCommandEvent &event);
private:
  wxTextCtrl *m_binaryName;
  wxButton   *m_browseButton;
};

#endif // BINARYNAMECTRL_H
