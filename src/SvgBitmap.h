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

/*! \file
  This file declares the class Image

  Image stores compressed images and handles scaling and uncompressing them.
*/

#ifndef SVGBITMAP_H
#define SVGBITMAP_H

#include "Cell.h"

#include <wx/bitmap.h>
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"

/*! An wxBitmap with a constructor that generates the image from SVG.
 */
class SvgBitmap: public wxBitmap
{
public:
//! A constructor that loads the data into a wxBitmap
  SvgBitmap(unsigned char *data, size_t len, int width, int height);
  
  //! Converts rgba data to a wxBitmap
  static wxBitmap RGBA2wxBitmap(const unsigned char imgdata[],const int &width, const int &height);

  static wxBitmap GetInvalidBitmap(int targetSize);
private:
  //! No idea what nanoSVG stores here. But can be shared between images.
  static struct NSVGrasterizer* m_svgRast;
  /*! An "invalid bitmap" sign
  
    We generate this image on start-up so whenever we need it it already
    is available as a refcounted object and therefore is likely to
    be available even if the problem was a failing malloc()
   */
  static wxBitmap m_invalidBitmap_unscaled;
};

#endif // SVGBITMAP_H
