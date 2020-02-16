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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file contains all the wizards the draw sidepane needs.
 */

#include <wx/textctrl.h>
#include <wx/mstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/string.h>
#include "LicenseDialog.h"
#include "../data/License.h"

LicenseDialog::LicenseDialog(wxWindow *parent) :
  wxDialog(parent, -1, _("License"))
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxMemoryInputStream istream(License_gz, License_gz_len);
  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  wxString line;
  wxString licenseText;
  
  while(!istream.Eof())
  {
    line = textIn.ReadLine();
    licenseText += line + wxT("\n");
  }
  
  wxTextCtrl *license = new wxTextCtrl(this, -1,
                                     wxEmptyString, wxDefaultPosition,
                                     wxDefaultSize,
                                     wxTE_MULTILINE | wxHSCROLL | wxTE_READONLY);
  license->SetMinSize(wxSize(550*GetContentScaleFactor(),500*GetContentScaleFactor()));
  license->SetValue(licenseText);  
  vbox->Add(license, wxSizerFlags().Expand());
  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  
  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  buttonSizer->Add(okButton);
  okButton->SetDefault(); 
  vbox->Add(buttonSizer, wxSizerFlags().Right());

  SetName("License");
  wxPersistenceManager::Get().RegisterAndRestore(this);
  SetSizerAndFit(vbox);
}
