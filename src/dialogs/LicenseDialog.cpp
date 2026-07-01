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
  A dialog that shows the program's license.
*/

#include "LicenseDialog.h"
#include "wxm_license.h"
#include "wxm_thirdparty_notices.h"
#include <wx/mstream.h>
#include <wx/notebook.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/txtstrm.h>

void LicenseDialog::FillTextCtrl(wxTextCtrl *ctrl, const unsigned char *data,
                                 size_t size, bool trackLongestLine) {
  wxMemoryInputStream istream(data, size);
  wxTextInputStream textIn(istream);
  wxClientDC dc(this);
  dc.SetFont(ctrl->GetFont());
  wxCoord textWidth = 0;
  wxString text;
  while (!istream.Eof()) {
    wxString line = textIn.ReadLine();
    text += line + wxS("\n");
    if (trackLongestLine) {
      wxSize linesize = dc.GetTextExtent(line);
      if (linesize.x > textWidth) {
        textWidth = linesize.x;
        m_longestLine = line;
      }
    }
  }
  if (trackLongestLine)
    ctrl->SetMinSize(wxSize(textWidth + 20 * GetContentScaleFactor(),
                            550 * GetContentScaleFactor()));
  ctrl->SetValue(text);
}

LicenseDialog::LicenseDialog(wxWindow *parent)
  : wxDialog(parent, -1, _("License"), wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxCLOSE_BOX | wxMAXIMIZE_BOX |
             wxMINIMIZE_BOX) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  m_movedToStart = false;
  Bind(wxEVT_TEXT_URL, &LicenseDialog::OnTextURLEvent, this);

  wxNotebook *notebook = new wxNotebook(this, wxID_ANY);

  // The wxMaxima license (GPL) itself.
  m_license = new wxTextCtrl(
                             notebook, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                             wxTE_MULTILINE | wxHSCROLL | wxTE_READONLY | wxTE_AUTO_URL);
  FillTextCtrl(m_license, WXM_LICENSE, WXM_LICENSE_SIZE, true);
  notebook->AddPage(m_license, _("License"));

  // Notices for third-party components we redistribute (e.g. the Microsoft Edge
  // WebView2 loader on Windows), whose licenses require reproducing them.
  m_notices = new wxTextCtrl(
                             notebook, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                             wxTE_MULTILINE | wxHSCROLL | wxTE_READONLY | wxTE_AUTO_URL);
  FillTextCtrl(m_notices, WXM_THIRDPARTY_NOTICES, WXM_THIRDPARTY_NOTICES_SIZE, false);
  notebook->AddPage(m_notices, _("Third-party notices"));

  vbox->Add(notebook, wxSizerFlags(10).Expand().Border(wxALL, 5));
  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  buttonSizer->Add(okButton, wxSizerFlags().Border(wxALL, 5));
  okButton->SetDefault();
  vbox->Add(buttonSizer, wxSizerFlags(0).Right());

  SetName("License");
  wxPersistenceManager::Get().RegisterAndRestore(this);
  Bind(wxEVT_SIZE, &LicenseDialog::OnSize, this);
  SetSizerAndFit(vbox);
}

void LicenseDialog::OnSize(wxSizeEvent &event) {
  wxFont fnt = m_license->GetFont();
  wxClientDC dc(this);
  double pointSize = 8;
  int width;
  do {
#if wxCHECK_VERSION(3, 1, 2)
    pointSize += .1;
    fnt.SetFractionalPointSize(pointSize);
#else
    pointSize += 1;
    fnt.SetPointSize(pointSize);
#endif
    dc.SetFont(fnt);
    width = dc.GetTextExtent(m_longestLine).x;
  } while ((pointSize < 128) && (width < event.GetSize().x));
#if wxCHECK_VERSION(3, 1, 2)
  pointSize -= .1;
  fnt.SetFractionalPointSize(pointSize);
#else
  pointSize -= 1;
  fnt.SetPointSize(pointSize);
#endif
  m_license->SetFont(fnt);
  m_notices->SetFont(fnt);
  event.Skip();
  if (!m_movedToStart) {
    m_license->SetInsertionPoint(0);
    m_license->ShowPosition(0);
  }
}

void LicenseDialog::OnTextURLEvent(wxTextUrlEvent &event) {
  if (event.GetMouseEvent().LeftUp()) {
    wxTextCtrl *pTextCtrl = static_cast<wxTextCtrl *>(event.GetEventObject());
    wxLaunchDefaultBrowser(pTextCtrl->GetRange(event.GetURLStart(), event.GetURLEnd()));
  }
}

