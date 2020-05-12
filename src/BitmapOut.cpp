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
  This file defines the class BitMap that renders math as bitmap.
 */

#include "BitmapOut.h"
#include "Cell.h"
#include <wx/clipbrd.h>

#define BM_FULL_WIDTH 1000

BitmapOut::BitmapOut(Configuration **configuration, int scale) :
    m_cmn(configuration, BM_FULL_WIDTH, scale)
{
  m_cmn.SetSize({10, 10});
  m_bmp.CreateScaled(10, 10, 24, scale);
  m_dc.SelectObject(m_bmp);
  m_dc.SetUserScale(scale, scale);
  m_dc.SetPen(wxNullPen);
  m_cmn.SetRecalculationContext(m_dc);

  auto &config = m_cmn.GetConfiguration();
  config.SetContext(m_dc);
  config.SetClientWidth(BM_FULL_WIDTH);
  config.SetClientHeight(BM_FULL_WIDTH);
  config.RecalculationForce(true);
}

BitmapOut::~BitmapOut()
{}

bool BitmapOut::SetData(Cell *tree, long int maxSize)
{
  m_tree.reset(tree);
  return m_tree && Layout(maxSize);
}

bool BitmapOut::Layout(long int maxSize)
{
  if (!m_cmn.PrepareLayout(m_tree.get()))
    return false;

  auto scale = m_cmn.GetScale();
  auto size = m_cmn.GetScaledSize();
  auto rawSize = m_cmn.GetSize();
  
  // Too big bitmaps or bitmaps that are too wide or high can crash windows
  // or the X server.
  if (maxSize >= 0 && (
        (size.x * size.y >= maxSize) ||
        (size.x >= 20000) ||
        (size.y >= 20000)
        ))
    goto failed;

  // The depth 24 hinders wxWidgets from creating rgb0 bitmaps that some
  // windows applications will interpret as rgba if they appear on
  // the clipboards and therefore render them all-transparent.
  m_bmp.CreateScaled(rawSize.x, rawSize.y, 24, scale);

  if (!m_bmp.IsOk())
    goto failed;

  m_dc.SelectObject(m_bmp);
  if (!m_dc.IsOk())
    goto failed;

  m_dc.SetUserScale(scale, scale);
  m_dc.SetPen(wxNullPen);
  Draw();
  return true;

failed:
  m_bmp = wxNullBitmap;
  return false;
}

void BitmapOut::Draw()
{
  auto &config = m_cmn.GetConfiguration();
  config.ClipToDrawRegion(false);

  auto bgColor = config.m_styles[TS_TEXT_BACKGROUND].Color();
  m_dc.SetBackground(*(wxTheBrushList->FindOrCreateBrush(bgColor, wxBRUSHSTYLE_SOLID)));
  m_dc.Clear();

  m_cmn.Draw(m_tree.get());
}

wxSize BitmapOut::ToFile(wxString file)
{
  // Assign a resolution to the bitmap.
  wxImage img = m_bmp.ConvertToImage();
  int resolution = img.GetOptionInt(wxIMAGE_OPTION_RESOLUTION);
  if (resolution <= 0)
    resolution = 75;
  img.SetOption(wxIMAGE_OPTION_RESOLUTION, resolution * m_cmn.GetScale());

  bool success = false;
  if (file.EndsWith(wxT(".bmp")))
    success = img.SaveFile(file, wxBITMAP_TYPE_BMP);
  else if (file.EndsWith(wxT(".xpm")))
    success = img.SaveFile(file, wxBITMAP_TYPE_XPM);
  else if (file.EndsWith(wxT(".jpg")))
    success = img.SaveFile(file, wxBITMAP_TYPE_JPEG);
  else
  {
    if (file.EndsWith(wxT(".png")))
      success = img.SaveFile(file, wxBITMAP_TYPE_PNG);
    else
      success = img.SaveFile(file + wxT(".png"), wxBITMAP_TYPE_PNG);
  }

  if (success)
    return m_cmn.GetScaledSize();
  else
    return wxDefaultSize;
}

bool BitmapOut::ToClipboard()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_bmp));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}
