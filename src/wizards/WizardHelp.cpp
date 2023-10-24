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

#include "WizardHelp.h"
#include <wx/persist/toplevel.h>

Wizardhelp::Wizardhelp(wxWindow *parent, int id, const wxString &title,
                       const wxString &message, const wxString &tooltip)
    : wxDialog(parent, id, title, wxDefaultPosition, wxDefaultSize,
               wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION |
               wxCLOSE_BOX | wxCLIP_CHILDREN) {
    SetName(title);
    m_message = new WrappingStaticText(this, -1, message);
    m_message->SetToolTip(tooltip);
    static_line_1 = new wxStaticLine(this, -1);

    button_1 = new wxButton(this, wxID_OK, _("OK"));
    button_1->SetDefault();
    wxSizer *vbox = new wxBoxSizer(wxVERTICAL);
    vbox->Add(m_message, wxSizerFlags(1).Expand().Border(
                  wxALL, 5 * GetContentScaleFactor()));
    vbox->Add(static_line_1, wxSizerFlags(1).Expand());
    vbox->Fit(this);
    vbox->SetSizeHints(this);
    Layout();
}
