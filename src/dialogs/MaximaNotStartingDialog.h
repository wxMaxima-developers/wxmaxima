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

#ifndef MAXIMANOTSTARTINGDIALOG_H
#define MAXIMANOTSTARTINGDIALOG_H

#include "../precomp.h"
#include "../Configuration.h"
#include <wx/wx.h>
#include <wx/dialog.h>
#include <wx/radiobut.h>
#include <wx/textctrl.h>
#include "BinaryNameCtrl.h"
class MaximaNotStartingDialog : public wxDialog
{
public:
  MaximaNotStartingDialog(wxWindow *parent, int id,
                          Configuration *config,
                          wxString text = wxEmptyString,
                          const wxPoint &pos = wxDefaultPosition,
                          const wxSize &size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);
  
protected:
  void OnOkPress(wxCommandEvent &event);
private:
  //! Autodetect the maxima location?
  wxRadioButton *m_autodetectMaxima;
  //! The radio button that is set if m_autodetectMaxima is unset
  wxRadioButton *m_noAutodetectMaxima;
  //! The place the user has configured maxima to reside in
  BinaryNameCtrl *m_maximaUserLocation;
  wxButton *button_1;
  wxButton *button_2;
  Configuration *m_configuration;

};

#endif // MAXIMANOTSTARTINGDIALOG_H
