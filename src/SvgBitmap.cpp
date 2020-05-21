// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/rawbmp.h>
#include "Image.h"
#include "invalidImage.h"

SvgBitmap::SvgBitmap(const unsigned char *data, size_t len, int width, int height)
{
  // Unzip the .svgz image
  wxMemoryInputStream istream(data, len);
  wxZlibInputStream zstream(istream);
  std::vector<char> svgContents;

  static constexpr auto chunkSize = 8192;
  while (!zstream.Eof())
  {
    auto const baseSize = svgContents.size();
    svgContents.resize(baseSize + chunkSize);
    zstream.Read(svgContents.data() + baseSize, chunkSize);
    if (zstream.LastRead() < chunkSize)
      svgContents.resize(baseSize + zstream.LastRead());
  }
  svgContents.push_back('\0');

  // Render the .svgz image
  if (!m_svgRast)
    m_svgRast = nsvgCreateRasterizer();
  if (!m_svgRast)
    wxBitmap::operator=(GetInvalidBitmap(width));
  if (svgContents.size() < 1)
    wxBitmap::operator=(GetInvalidBitmap(width));

  m_svgImage.reset(nsvgParse(svgContents.data(), "px", 96));
  SetSize(width, height);
}

SvgBitmap::~SvgBitmap()
{}

const SvgBitmap &SvgBitmap::SetSize(int width, int height)
{
  // Set the bitmap to the new size
  wxBitmap::operator=(wxBitmap(width, height, 32));

  if (!m_svgImage)
  {
    wxBitmap::operator=(GetInvalidBitmap(width));
    return *this;
  }
  std::vector<unsigned char> imgdata(width*height*4);

  // Actually render the bitmap
  nsvgRasterize(m_svgRast, m_svgImage.get(), 0,0,
                wxMin((double)width/(double)m_svgImage->width,
                      (double)height/(double)m_svgImage->height),
                imgdata.data(),
                width, height, width*4);

  // Copy the bitmap to this object's bitmap storage
  wxAlphaPixelData bmpdata(*this);
  wxAlphaPixelData::Iterator dst(bmpdata);
  const unsigned char* rgba = imgdata.data();
  for( int y = 0; y < height; y++)
  {
    dst.MoveTo(bmpdata, 0, y);
    for(int x = 0; x < width; x++)
    {
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

SvgBitmap::SvgBitmap(const unsigned char *data, size_t len, wxSize siz):
  SvgBitmap(data, len, siz.x, siz.y)
{}

SvgBitmap &SvgBitmap::operator=(SvgBitmap &&o)
{
  wxBitmap::operator=(o);
  m_svgImage = std::move(o.m_svgImage);
  return *this;
}

wxBitmap SvgBitmap::GetInvalidBitmap(int targetSize)
{
  wxImage img = wxImage(invalidImage_xpm);
  img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH);
  wxBitmap retval;
  retval = wxBitmap(img,wxBITMAP_SCREEN_DEPTH);
  return retval;
}

wxBitmap SvgBitmap::RGBA2wxBitmap(const unsigned char imgdata[],
                                  const int &width, const int &height)
{
  wxBitmap retval = wxBitmap(width, height, 32);
  const unsigned char* rgba = imgdata;
  if(!retval.Ok())
    return retval;
  
  wxAlphaPixelData bmpdata(retval);
  wxAlphaPixelData::Iterator dst(bmpdata);
  for( int y = 0; y < height; y++)
  {
    dst.MoveTo(bmpdata, 0, y);
    for(int x = 0; x < width; x++)
    {
      unsigned char a = rgba[3];
      dst.Red() = rgba[0] * a / 255;
      dst.Green() = rgba[1] * a / 255;
      dst.Blue() = rgba[2] * a / 255;
      dst.Alpha() = a;
      dst++;
          rgba += 4;
    }
  }
  return retval;
}

struct NSVGrasterizer* SvgBitmap::m_svgRast = NULL;
