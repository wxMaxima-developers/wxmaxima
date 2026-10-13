// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2019      Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*\file

  Generates bitmaps from our embedded svg data
*/

#include <vector>
#include <wx/mstream.h>
#include <wx/zstream.h>
#include "SvgBitmap.h"
#include "invalidImage.h"

SvgBitmap::SvgBitmap(wxWindow *WXUNUSED(window), const unsigned char *data,
                     const std::size_t len, int width, int height) {
  std::vector<char> svgContents;
  if (data && (len > 0) && (data[0] == '<'))
    svgContents.assign(data, data + len);
  else if (data && (len > 0)) {
    // Unzip the .svgz image
    wxMemoryInputStream istream(data, len);
    wxZlibInputStream zstream(istream);
    static constexpr auto chunkSize = 8192;
    while (!zstream.Eof()) {
      auto const baseSize = svgContents.size();
      svgContents.resize(baseSize + chunkSize);
      zstream.Read(svgContents.data() + baseSize, chunkSize);
      if (zstream.LastRead() < chunkSize)
        svgContents.resize(baseSize + zstream.LastRead());
    }
  }
  svgContents.push_back('\0');

  if (svgContents.size() > 1)
    m_svg = wxBitmapBundle::FromSVG(svgContents.data(), wxSize(width, height));
  SetSize(width, height);
}

const SvgBitmap &SvgBitmap::SetSize(int width, int height) {
  if (width < 1)
    width = 1;
  if (height < 1)
    height = 1;

  wxBitmap bitmap;
  if (m_svg.IsOk())
    bitmap = m_svg.GetBitmap(wxSize(width, height));
  // GetBitmap() gives a bitmap of any other than the bundle's default size a
  // scale factor, which would make a HiDPI screen draw it at the default
  // size. We want it drawn at the size we asked for.
  if (bitmap.IsOk())
    bitmap.SetScaleFactor(1);
  if (!bitmap.IsOk())
    bitmap = GetInvalidBitmap(width);
  wxBitmap::operator=(bitmap);
  return *this;
}

SvgBitmap::SvgBitmap(wxWindow *window, const unsigned char *data, const std::size_t len,
                     wxSize siz)
  : SvgBitmap(window, data, len, siz.x, siz.y) {}

SvgBitmap &SvgBitmap::operator=(SvgBitmap &&o) noexcept {
  wxBitmap::operator=(o);
  m_svg = std::move(o.m_svg);
  return *this;
}

wxBitmap SvgBitmap::GetInvalidBitmap(int targetSize) {
  wxImage img = wxImage(invalidImage_xpm);
  img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH);
  return wxBitmap(img, wxBITMAP_SCREEN_DEPTH);
}
