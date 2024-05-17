// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  A text ctrl that allows to browse for a binary
*/

#include "BinaryNameCtrl.h"
#include <wx/sizer.h>

BinaryNameCtrl::BinaryNameCtrl(wxWindow *parent, int id)
  : wxPanel(parent, id) {
  wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
  m_binaryName =
    new wxTextCtrl(invocationSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                   wxSize(250 * GetContentScaleFactor(), -1), wxTE_RICH);
  m_binaryName->AutoCompleteFileNames();
  m_binaryName->Connect(wxEVT_COMMAND_TEXT_UPDATED,
                        wxCommandEventHandler(BinaryNameCtrl::TextChanged), NULL, this);
  hbox->Add(m_maximaUserLocation,
            wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  m_browseButton =
    new wxButton(invocationSizer->GetStaticBox(), wxID_OPEN, _("Browse"));
  Open->Connect(wxEVT_BUTTON,
                wxCommandEventHandler(ConfigDialogue::OnBrowse), NULL,
                this);
  SetSizerAndFit(hbox);
}

void BinaryNameCtrl::TextChanged(wxCommandEvent &evt) {
  evt.Skip();
  TextChanged();
}

void BinaryNameCtrl::TextChanged() {
  if (Configuration::FindProgram(m_binaryName->GetValue()).IsEmpty())
    m_binaryName->SetForegroundColour(*wxRED);
  else
    m_binaryName->SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
}

void BinaryNameCtrl::OnBrowse(wxCommandEvent &WXUNUSED(event)) {
  wxConfigBase *config = wxConfig::Get();
  wxString dd;
  config->Read(wxS("maxima"), &dd);
  wxString file = wxFileSelector(_("Select binary location program"), wxPathOnly(dd),
                                 wxFileNameFromPath(dd), wxEmptyString,
#if defined __WXMSW__
                                 _("Bat files (*.bat)|*.bat|Exe files (*.exe)|*.exe|All|*"),
#else
                                 _("All|*"),
#endif
                                 wxFD_OPEN);

  if (file.Length()) {
    if (file.Right(8).Lower() == wxS("wxmaxima") ||
        file.Right(12).Lower() == wxS("wxmaxima.exe"))
      LoggingMessageBox(_("We weren't searching for the location of wxMaxima!\n\nPlease enter "
                          "the path to the program again."),
                        _("Error"), wxOK | wxICON_ERROR);
    else
      m_binaryName->SetValue(file);
  }
}
