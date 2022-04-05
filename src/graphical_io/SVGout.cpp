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
  This file defines the class Svgout that renders math as scalable vector graphics.
 */

#define wxNO_UNSAFE_WXSTRING_CONV 1
#include "SVGout.h"
#include "ErrorRedirector.h"
#include <wx/txtstrm.h> 
#include <wx/filename.h> 
#include <wx/wfstream.h>
#include <wx/stdpaths.h>
#include "Configuration.h"
#include "GroupCell.h"
#include <wx/config.h>
#include <wx/clipbrd.h>

Svgout::Svgout(Configuration **configuration, const wxString &filename, double scale) :
    m_cmn(configuration, filename, 500, scale), // Note: old SVGout code had this also at 500
    m_recalculationDc(m_cmn.GetTempFilename(), 700*scale, 50000*scale, 20*scale),
    m_CWD(wxGetCwd())
{

  wxString path = wxFileName(filename).GetPath();
  if (path.Length() > 1)
    wxSetWorkingDirectory(path);
  m_cmn.SetRecalculationContext(m_recalculationDc);
  
#if wxCHECK_VERSION(3, 1, 0)
  m_recalculationDc.SetBitmapHandler(new wxSVGBitmapEmbedHandler());
#endif
  auto &config = m_cmn.GetConfiguration();
  config.SetContext(m_recalculationDc);
  config.SetClientWidth(700*scale);
}

Svgout::Svgout(Configuration **configuration, std::unique_ptr<Cell> &&tree, const wxString &filename, double scale) :
    Svgout(configuration, filename, scale)
{
  Render(std::move(tree));
}

Svgout::~Svgout()
{
  wxSetWorkingDirectory(m_CWD);
}

wxSize Svgout::Render(std::unique_ptr<Cell> &&tree)
{
  m_tree = std::move(tree);
  m_isOk = m_tree && Layout();
  m_size = m_isOk ? m_cmn.GetScaledSize() : wxDefaultSize;
  return m_size;
}

bool Svgout::Layout()
{
  if (!m_cmn.PrepareLayout(m_tree.get()))
    return false;

  // Let's switch to a DC of the right size for our object.
  auto size = m_cmn.GetSize();
  auto &config = m_cmn.GetConfiguration();
  wxSVGFileDC dc(m_cmn.GetFilename(), size.x, size.y, 20*m_cmn.GetScale());
#if wxCHECK_VERSION(3, 1, 0)
  dc.SetBitmapHandler(new wxSVGBitmapEmbedHandler());
#endif

  config.SetContext(dc);
  m_cmn.Draw(m_tree.get());
  config.UnsetContext();

  return true;
}

static wxDataFormat &Format()
{
  static wxDataFormat format(wxT("image/svg+xml"));
  return format;
}

std::unique_ptr<wxCustomDataObject> Svgout::GetDataObject()
{
  return m_cmn.GetDataObject(Format());
}

bool Svgout::ToClipboard()
{
  return m_cmn.ToClipboard(Format());
}
