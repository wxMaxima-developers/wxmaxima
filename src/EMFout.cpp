// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

Emfout::Emfout(Configuration **configuration, const wxString &filename) :
    m_cmn(configuration, filename, 500, 1.0),
    m_recalculationDc(m_cmn.GetTempFilename(), 3000, 50000)
{
  m_cmn.SetRecalculationContext(m_recalculationDc);
  auto &config = m_cmn.GetConfiguration();
  config.SetContext(m_recalculationDc);
  config.SetClientWidth(3000);
  config.RecalculationForce(true);
}

Emfout::~Emfout()
{}

wxSize Emfout::SetData(Cell *tree)
{
  m_tree.reset(tree);
  if (m_tree && Layout())
      return m_cmn.GetSize();

  return wxDefaultSize;
}

bool Emfout::Layout()
{
  if (!m_cmn.PrepareLayout(m_tree.get()))
    return false;

  // Let's switch to a DC of the right size for our object.
  auto size = m_cmn.GetSize();
  auto &config = m_cmn.GetConfiguration();
  wxEnhMetaFileDC dc(m_cmn.GetFilename(), size.x, size.y);

  config.SetContext(dc);
  m_cmn.Draw(m_tree.get());
  m_metaFile.reset(dc.Close()); // Closing the DC triggers the output of the file.
  config.UnsetContext();

  return true;
}

wxEnhMetaFileDataObject *Emfout::GetDataObject()
{
  return m_metaFile ? new wxEnhMetaFileDataObject(*m_metaFile) : nullptr;
}

bool Emfout::ToClipboard()
{
  return m_metaFile && m_metaFile->SetClipboard();
}

#endif // wxUSE_ENH_METAFILE
