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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

SvgBitmap::SvgBitmap(unsigned char *data, size_t len, int width, int height)
  : wxBitmap(width, height, 32)
{
  // Unzip the .svgz image
  wxMemoryInputStream istream(data, len);
  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  wxString svgContents_string;
  wxString line;
  while(!istream.Eof())
  {
    line = textIn.ReadLine();
    svgContents_string += line + wxT("\n");
  }
  
  // Render the .svgz image
  m_svgRast = nsvgCreateRasterizer();
  std::unique_ptr<char> svgContents((char *)strdup(svgContents_string.utf8_str()));
  std::unique_ptr<NSVGimage> svgImage(nsvgParse(svgContents.get(), "px", 96));
  std::unique_ptr<unsigned char> imgdata(new unsigned char[width*height*4]);        
  nsvgRasterize(m_svgRast, svgImage.get(), 0,0,
                wxMin((double)width/(double)svgImage->width,
                      (double)height/(double)svgImage->height),
                imgdata.get(),
                width, height, width*4);

  
  wxAlphaPixelData bmpdata(*this);
  wxAlphaPixelData::Iterator dst(bmpdata);
  const unsigned char* rgba = imgdata.get();
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
