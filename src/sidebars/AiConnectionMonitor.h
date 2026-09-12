// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#ifndef AICONNECTIONMONITOR_H
#define AICONNECTIONMONITOR_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/richtext/richtextctrl.h>

/*! A sidebar that shows the raw HTTP traffic between wxMaxima and the
  configured AI provider -- the AI Chat sidebar's equivalent of XmlInspector
  (which shows the wxMaxima<->Maxima socket traffic). Opened via the AI
  status bar icon's double-click (see StatusBar::AiStatus/wxMaxima's click
  handlers).

  No redaction is needed here: an AiProvider's request/response bodies never
  contain the API key (grep confirms it's only ever placed in a request
  *header*, via each provider's own AuthHeaders()), so the plain JSON text
  is safe to display as-is.

  Unlike XmlInspector, this doesn't batch/defer via an idle-driven
  UpdateContents() -- one AI chat turn is a single user-paced Send click,
  not a flood of small socket reads, so there is no performance reason to
  defer the write.
*/
class AiConnectionMonitor : public wxRichTextCtrl {
public:
  AiConnectionMonitor(wxWindow *parent, int id);

  //! Remove all text from the display.
  void Clear() override;

  //! Record a request about to be sent to `providerName`.
  void Add_Request(const wxString &providerName, const wxString &requestBody);
  //! Record the reply (or error detail) for the most recent request.
  void Add_Response(bool ok, const wxString &responseBodyOrDetail);

private:
  //! Whether the display already has SOME content, so a new entry can
  //! separate itself from a previous one with a blank line.
  bool m_hasContent = false;
};

#endif // AICONNECTIONMONITOR_H
