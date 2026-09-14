// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file defines the contents of the class AiConnectionMonitor, a sidebar
  showing the raw HTTP traffic between wxMaxima and the configured AI
  provider.
*/

#include "AiConnectionMonitor.h"

AiConnectionMonitor::AiConnectionMonitor(wxWindow *parent, int id)
  : wxRichTextCtrl(parent, id, wxEmptyString, wxDefaultPosition,
                   wxSize(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) / 10,
                          wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) / 10),
                   wxTE_READONLY | wxTE_RICH | wxHSCROLL | wxTE_MULTILINE) {
  BeginSuppressUndo();
  AiConnectionMonitor::Clear();
}

void AiConnectionMonitor::Clear() {
  wxRichTextCtrl::Clear();
  m_hasContent = false;
}

void AiConnectionMonitor::Add_Request(const wxString &providerName,
                                      const wxString &requestBody) {
  if (m_hasContent) {
    Newline();
    Newline();
  }
  SetInsertionPointEnd();
  BeginTextColour(wxColour(0, 0, 0));
  WriteText(wxString::Format(_("REQUEST TO %s:"), providerName));
  Newline();
  Newline();
  EndTextColour();

  BeginTextColour(wxColour(128, 0, 0));
  WriteText(requestBody);
  EndTextColour();
  m_hasContent = true;
}

void AiConnectionMonitor::Add_Response(bool ok, const wxString &responseBodyOrDetail) {
  if (m_hasContent) {
    Newline();
    Newline();
  }
  SetInsertionPointEnd();
  BeginTextColour(wxColour(0, 0, 0));
  WriteText(ok ? _("RESPONSE:") : _("ERROR:"));
  Newline();
  Newline();
  EndTextColour();

  BeginTextColour(ok ? wxColour(0, 128, 0) : wxColour(192, 0, 0));
  WriteText(responseBodyOrDetail);
  EndTextColour();
  m_hasContent = true;
}
