// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class Emfout that renders math as scalable emf graphics.
*/

#include "EMFout.h"
#include "Cell.h"

#if wxUSE_ENH_METAFILE

Emfout::Emfout(Configuration **configuration, const wxString &filename)
    : m_cmn(configuration, filename, 500, 1.0),
      m_recalculationDc(m_cmn.GetTempFilename(), 3000, 50000) {
    m_cmn.SetRecalculationContext(m_recalculationDc);
    auto &config = m_cmn.GetConfiguration();
    config.SetRecalcContext(m_recalculationDc);
    config.SetCanvasSize(wxSize(3000,100000));
}

Emfout::Emfout(Configuration **configuration, std::unique_ptr<Cell> &&tree,
               const wxString &filename)
    : Emfout(configuration, filename) {
    Render(std::move(tree));
}

Emfout::~Emfout() {}

wxSize Emfout::Render(std::unique_ptr<Cell> &&tree) {
    m_tree = std::move(tree);
    m_isOk = m_tree && Layout();
    m_size = m_isOk ? m_cmn.GetSize() : wxDefaultSize;
    return m_size;
}

bool Emfout::Layout() {
    if (!m_cmn.PrepareLayout(m_tree.get()))
        return false;

    // Let's switch to a DC of the right size for our object.
    auto size = m_cmn.GetSize();
    auto &config = m_cmn.GetConfiguration();
    wxEnhMetaFileDC dc(m_cmn.GetFilename(), size.x, size.y);

    config.SetRecalcContext(dc);
    m_cmn.Draw(m_tree.get());
    m_metaFile.reset(
        dc.Close()); // Closing the DC triggers the output of the file.
    config.UnsetContext();

    return true;
}

std::unique_ptr<wxEnhMetaFileDataObject> Emfout::GetDataObject() const {
    return m_metaFile ? std::make_unique<wxEnhMetaFileDataObject>(*m_metaFile)
        : nullptr;
}

bool Emfout::ToClipboard() { return m_metaFile && m_metaFile->SetClipboard(); }

#endif // wxUSE_ENH_METAFILE
