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

  The C code that generates bitmaps from compressed svg data
*/

#include <vector>
#include <utility>
#include <algorithm>
#include <wx/mstream.h>
#include <wx/rawbmp.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/window.h>
#include <wx/wx.h>
#include <wx/zstream.h>
#include "SvgBitmap.h"
#include "Image.h"
#include "invalidImage.h"

SvgBitmap::SvgBitmap(wxWindow *window, const unsigned char *data, const std::size_t len,
                     int width, int height)
  : m_window(window) {
  // Unzip the .svgz image
  wxMemoryInputStream istream(data, len);
  wxZlibInputStream zstream(istream);
  std::vector<char> svgContents;

  static constexpr auto chunkSize = 8192;
  while (!zstream.Eof()) {
    auto const baseSize = svgContents.size();
    svgContents.resize(baseSize + chunkSize);
    zstream.Read(svgContents.data() + baseSize, chunkSize);
    if (zstream.LastRead() < chunkSize)
      svgContents.resize(baseSize + zstream.LastRead());
  }
  svgContents.push_back('\0');

  // Render the .svgz image
  if (!m_svgRast)
    m_svgRast = wxm_nsvgCreateRasterizer();
  if (!m_svgRast)
    wxBitmap::operator=(GetInvalidBitmap(width));
  if (svgContents.size() < 2)
    wxBitmap::operator=(GetInvalidBitmap(width));

  m_svgImage.reset(wxm_nsvgParse(svgContents.data(), "px", 96));
  SetSize(width, height);
}

SvgBitmap::SvgBitmap(wxWindow *window, const wxString &data, wxSize siz):
  m_window(window)
{
  wxCharBuffer buffer = data.ToUTF8();
  m_svgImage.reset(wxm_nsvgParse(buffer.data(), "px", 96));
  SetSize(siz.x, siz.y);
}

SvgBitmap::~SvgBitmap() {}

const SvgBitmap &SvgBitmap::SetSize(int width, int height) {
  if (width < 1)
    width = 1;
  if (height < 1)
    height = 1;
  // Set the bitmap to the new size
  this->wxBitmap::operator=(wxBitmap(wxSize(width, height), 32));

  if (!m_svgImage) {
    this->wxBitmap::operator=(GetInvalidBitmap(width));
    return *this;
  }

  std::vector<unsigned char> imgdata(static_cast<std::size_t>(width) * height * 4);

  // Actually render the bitmap
  wxm_nsvgRasterize(m_svgRast, m_svgImage.get(), 0, 0,
                    std::min(static_cast<double>(width) / static_cast<double>(m_svgImage->width),
                             static_cast<double>(height) / static_cast<double>(m_svgImage->height)),
                    imgdata.data(), width, height, width * 4);

  // Copy the bitmap to this object's bitmap storage
  wxAlphaPixelData bmpdata(*this);
  wxAlphaPixelData::Iterator dst(bmpdata);
  const unsigned char *rgba = imgdata.data();
  for (int y = 0; y < height; y++) {
    dst.MoveTo(bmpdata, 0, y);
    for (int x = 0; x < width; x++) {
      unsigned char a = rgba[3];
      dst.Red() = rgba[0] * a / 255;
      dst.Green() = rgba[1] * a / 255;
      dst.Blue() = rgba[2] * a / 255;
      dst.Alpha() = a;
      dst++;
      rgba += 4;
    }
  }
  return *this;
}

SvgBitmap::SvgBitmap(wxWindow *window, const unsigned char *data, const std::size_t len,
                     wxSize siz)
  : SvgBitmap(window, data, len, siz.x, siz.y) {}

SvgBitmap &SvgBitmap::operator=(SvgBitmap &&o) noexcept {
  wxBitmap::operator=(o);
  m_svgImage = std::move(o.m_svgImage);
  return *this;
}

wxBitmap SvgBitmap::GetInvalidBitmap(int targetSize) {
  wxImage img = wxImage(invalidImage_xpm);
  img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH);
  return wxBitmap(img, wxBITMAP_SCREEN_DEPTH);
}

struct wxm_NSVGrasterizer *SvgBitmap::m_svgRast = NULL;
