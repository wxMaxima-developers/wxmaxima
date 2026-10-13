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

/*! \file
  This file declares the class SvgBitmap

  SvgBitmap is a wxBitmap rendered from one of our embedded, usually
  gzip-compressed, SVG images.
*/

#ifndef SVGBITMAP_H
#define SVGBITMAP_H

#include "precomp.h"
#include <wx/bitmap.h>
#include <wx/bmpbndl.h>

/*! A wxBitmap with a constructor that generates the image from SVG.

  The rendering is done by wxWidgets (wxBitmapBundle::FromSVG()); this class
  only unpacks our embedded data and keeps the bundle around, so a new size
  is rendered from the vector data again rather than scaled from pixels.
 */
class SvgBitmap: public wxBitmap
{
public:
  /*! Renders an SVG image

    \param window Unused; kept so the call sites read as before
    \param data   The SVG, either as text or gzip-compressed
    \param len    The length of data in bytes
  */
  SvgBitmap(wxWindow *window, const unsigned char *data, const size_t len, int width = 640, int height = 480);
  SvgBitmap(wxWindow *window, const unsigned char *data, const std::size_t len, wxSize siz);
  SvgBitmap(SvgBitmap &&) = delete;
  SvgBitmap &operator=(SvgBitmap &&o) noexcept;

  //! Sets the bitmap to a new size and renders the svg image at this size.
  const SvgBitmap& SetSize(int width, int height);
  //! Sets the bitmap to a new size and renders the svg image at this size.
  const SvgBitmap& SetSize(wxSize siz){return SetSize(siz.x, siz.y);}
  /*! Does the SVG renderer wxWidgets was built with draw text?

    Only LunaSVG does (wxWidgets >= 3.3.2, built with LunaSVG on and NanoSVG
    off); NanoSVG ignores text altogether, which would leave a plot without
    its labels and tick marks' numbers. Found out by rendering a word set in
    the font gnuplot's svg terminal asks for, so this also says no if the
    renderer can find neither that font nor its fallback. Rendered once, on
    first use, which has to be on the GUI thread.
  */
  static bool RendersText();
  /*! An "invalid bitmap" sign */
  static wxBitmap GetInvalidBitmap(int targetSize);
private:
  //! The renderable svg image after we have read it in
  wxBitmapBundle m_svg;
};

#endif // SVGBITMAP_H
