// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2017-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file contains code to create a wxPanel containing image data.
*/

#include "wxImagePanel.h"
#include <wx/display.h>
#include <wx/mstream.h>

wxImagePanel::wxImagePanel(wxWindow *parent, unsigned char *data, size_t len)
    : wxPanel(parent) {
    Load(data, len);
    int ppi;
#if wxCHECK_VERSION(3, 1, 1)
    wxDisplay display;

    int display_idx = wxDisplay::GetFromWindow(GetParent());
    if (display_idx < 0)
        ppi = 72;
    else
        ppi = wxDisplay(display_idx).GetPPI().x;
#else
    ppi = wxGetDisplayPPI().x;
#endif
    ppi = wxMax(ppi, 75);

    SetMinSize(
        wxSize(ppi * 4, m_image.GetHeight() * ppi * 4 / m_image.GetWidth()));

    Connect(wxEVT_PAINT, wxPaintEventHandler(wxImagePanel::paintEvent), NULL,
            this);
    Connect(wxEVT_SIZE, wxSizeEventHandler(wxImagePanel::OnSize), NULL, this);
}

void wxImagePanel::Load(unsigned char *data, size_t len) {
    wxMemoryInputStream istream(data, len);
    m_image.LoadFile(istream);
    m_w = m_h = -1;
    Refresh(true);
}

/*
 * Called by the system of by wxWidgets when the panel needs
 * to be redrawn. You can also trigger this call by
 * calling Refresh()/Update().
 */

void wxImagePanel::paintEvent(wxPaintEvent &WXUNUSED(evt)) {
    // depending on your system you may need to look at double-buffered dcs
    wxMemoryDC dcm;
    wxPaintDC dc(this);
    int neww, newh;
    dc.GetSize(&neww, &newh);

    if (neww != m_w || newh != m_h) {
        m_resized = wxBitmap(m_image.Scale(neww, newh, wxIMAGE_QUALITY_HIGH));
        m_w = neww;
        m_h = newh;
    }
    dc.DrawBitmap(m_resized, 0, 0, false);
}

/*
 * Here we call refresh to tell the panel to draw itself again.
 * So when the user resizes the image panel the image should be resized too.
 */
void wxImagePanel::OnSize(wxSizeEvent &event) {
    Refresh();
    event.Skip();
}
