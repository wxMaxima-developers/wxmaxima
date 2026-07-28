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
  This file defines the class BitMap that renders math as bitmap.
*/

#include "BitmapOut.h"
#include "cells/Cell.h"
#include <wx/clipbrd.h>

#define BM_FULL_WIDTH 1000

BitmapOut::BitmapOut(const Configuration * const *configuration, double scale)
  : m_cmn(configuration, BM_FULL_WIDTH, scale) {
  m_cmn.SetSize({10, 10});
  // A plain placeholder bitmap plus SetUserScale below: the same scale regime
  // Layout() uses for the real canvas, so cell measurement during PrepareLayout
  // and the final drawing agree. (Not CreateScaled, which would add a second
  // scale factor on top of the user scale -- see the comment in Layout().)
  m_bmp.Create(10, 10, 24);
  m_dc.SelectObject(m_bmp);
  m_dc.SetUserScale(scale, scale);
  m_dc.SetPen(wxNullPen);
  m_cmn.SetRecalculationContext(&m_dc);

  auto *config = m_cmn.GetConfiguration();
  config->SetRecalcContext(m_dc);
  config->SetCanvasSize(wxSize(BM_FULL_WIDTH, BM_FULL_WIDTH));
}

BitmapOut::BitmapOut(const Configuration * const *configuration,
                     std::unique_ptr<Cell> &&tree, double scale, long maxSize)
  : BitmapOut(configuration, scale) {
  Render(std::move(tree), maxSize);
}

BitmapOut::~BitmapOut() {}

bool BitmapOut::Render(std::unique_ptr<Cell> &&tree, long int maxSize) {
  m_tree = std::move(tree);
  m_isOk = Layout(maxSize);
  return m_isOk;
}

bool BitmapOut::Layout(long int maxSize) {
  if(m_tree == NULL)
    return false;

  if (!m_cmn.PrepareLayout(m_tree.get()))
    return false;

  auto scale = m_cmn.GetScale();
  auto size = m_cmn.GetScaledSize();

  // Bitmaps that are bigger than the available memory can lead to crashes within
  // MS Windows or the X server.
  if (maxSize >= 0 && (((long)size.x * size.y >= maxSize) ||
                       (size.x >= 20000) || (size.y >= 20000)))
    goto failed;

  // Allocate the canvas at the full *device* size (the already-scaled size) and
  // let SetUserScale(scale) below do the magnification. Using
  // CreateScaled(rawSize, scale) here gave the bitmap its own scale factor on
  // top of the user scale, so drawing was magnified by `scale` twice: at the
  // default BitmapScale of 3 the content was laid down three times too large for
  // the canvas and only its upper-left third survived (equations exported to
  // HTML as bitmaps came out mostly blank). A plain bitmap sized to the scaled
  // extent matches the single SetUserScale magnification. At scale == 1 this is
  // identical to the old CreateScaled(size, 1), so the clipboard "copy as
  // bitmap" path (which always renders at scale 1) is unchanged.
  //
  // The depth 24 hinders wxWidgets from creating rgb0 bitmaps that some
  // windows applications will interpret as rgba if they appear on
  // the clipboards and therefore render them all-transparent.
  m_bmp.Create(size.x, size.y, 24);

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

void BitmapOut::Draw() {
  auto config = m_cmn.GetConfiguration();
  config->ClipToDrawRegion(false);

  auto bgColor = config->GetStyle(TS_TEXT_BACKGROUND)->GetColor();
  m_dc.SetBackground(*(wxTheBrushList->FindOrCreateBrush(bgColor,
                                                         wxBRUSHSTYLE_SOLID)));
  m_dc.Clear();

  m_cmn.Draw(m_tree.get());
}

wxSize BitmapOut::ToFile(const wxString &file) {
  // Assign a resolution to the bitmap.
  wxImage img = m_bmp.ConvertToImage();
  int resolution = m_cmn.GetScreenConfig().GetRecalcDC()->GetPPI().x;
  img.SetOption(wxIMAGE_OPTION_RESOLUTION, resolution * m_cmn.GetScale());

  bool success = false;
  if (file.EndsWith(wxS(".bmp")))
    success = img.SaveFile(file, wxBITMAP_TYPE_BMP);
  else if (file.EndsWith(wxS(".xpm")))
    success = img.SaveFile(file, wxBITMAP_TYPE_XPM);
  else if (file.EndsWith(wxS(".jpg")))
    success = img.SaveFile(file, wxBITMAP_TYPE_JPEG);
  else {
    if (file.EndsWith(wxS(".png")))
      success = img.SaveFile(file, wxBITMAP_TYPE_PNG);
    else
      success = img.SaveFile(file + wxS(".png"), wxBITMAP_TYPE_PNG);
  }

  if (success)
    return m_cmn.GetScaledSize();
  else
    return wxDefaultSize;
}

std::unique_ptr<wxBitmapDataObject> BitmapOut::GetDataObject() const {
  return m_isOk ? std::make_unique<wxBitmapDataObject>(GetBitmap()) : nullptr;
}

bool BitmapOut::ToClipboard() const {
  if (!m_isOk)
    return false;
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    bool res = wxTheClipboard->SetData(GetDataObject().release());
    wxTheClipboard->Close();
    return res;
  }
  return false;
}
