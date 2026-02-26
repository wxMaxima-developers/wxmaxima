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

#include "ScrollingGenWizPanel.h"
#include <wx/persist/toplevel.h>

ScrollingGenWizPanel::ScrollingGenWizPanel(
                                           wxWindow *parent, Configuration *cfg, MaximaManual *manual,
                                           const wxString &description, const wxString &description_tooltip,
                                           const wxString &commandRule, const wxString &label1, const wxString &defaultval1,
                                           const wxString &tooltip1, const wxString &label2, const wxString &defaultval2, const wxString &tooltip2,
                                           const wxString &label3, const wxString &defaultval3, const wxString &tooltip3, const wxString &label4,
                                           const wxString &defaultval4, const wxString &tooltip4, const wxString &label5,
                                           const wxString &defaultval5, const wxString &tooltip5, const wxString &label6,
                                           const wxString &defaultval6, const wxString &tooltip6, const wxString &label7,
                                           const wxString &defaultval7, const wxString &tooltip7, const wxString &label8,
                                           const wxString &defaultval8, const wxString &tooltip8, const wxString &label9,
                                           const wxString &defaultval9, const wxString &tooltip9, const wxString &label10,
                                           const wxString &defaultval10, const wxString &tooltip10)
                                           : wxScrolled<wxPanel>(parent, wxID_ANY) {
  wxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  m_panel = new GenWizPanel(
                            this, cfg, manual, description, description_tooltip, commandRule, true,
                            label1, defaultval1, tooltip1, label2, defaultval2, tooltip2, label3,
                            defaultval3, tooltip3, label4, defaultval4, tooltip4, label5, defaultval5,
                            tooltip5, label6, defaultval6, tooltip6, label7, defaultval7, tooltip7,
                            label8, defaultval8, tooltip8, label9, defaultval9, tooltip9);
  vbox->Add(m_panel, wxSizerFlags(1).Expand());
  EnableScrolling(false, true);
  SetScrollRate(5, 5);
  Connect(wxEVT_SIZE, wxSizeEventHandler(ScrollingGenWizPanel::OnSize), NULL,
          this);
  SetSizerAndFit(vbox);
}

void ScrollingGenWizPanel::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize().x, wxDefaultSize.y);
  event.Skip();
}
