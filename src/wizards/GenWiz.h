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
#include "GenWizPanel.h"

/*! A generic wizard dialogue

  The generic wizard dialogue we use almost everywhere, if the user has chosen
  not to use dockable dialogues. Else we use a GenWizPanel.
 */
class GenWiz : public wxDialog
{
public:
  GenWiz(wxWindow *parent, Configuration *cfg, MaximaManual *manual,
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

  //! Returns the contents of the nth field. 
  wxString operator[](int i) const { return m_panel->operator[](i); }
  //! Returns the command the wizard has generated, if commandRule wasn't empty
  wxString GetOutput() const {return m_panel->GetOutput();}
  //! If we generate a help keyword event this function tells what keyword it was for
  wxString GetHelpKeyword(int ID){return m_panel->GetHelpKeyword(ID);}
  
protected:
  //! The contents of the dialogue
  GenWizPanel *m_panel;
};

#endif // GENWIZ_H
