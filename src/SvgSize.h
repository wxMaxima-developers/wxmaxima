// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Reads the size an SVG image says it has.

  wxBitmapBundle::FromSVG() renders an SVG at whatever size it is asked for,
  but wxWidgets has no public way to ask the SVG how big it wants to be (its
  implementations know, but keep it to themselves, in 3.2 and 3.3 alike). An
  image in the worksheet needs exactly that, as its 100% size, so we read it
  from the root element ourselves: its width and height attributes, or its
  viewBox if those are missing or relative.
*/

#ifndef SVGSIZE_H
#define SVGSIZE_H

#include <wx/gdicmn.h>
#include <wx/string.h>

namespace wxm {
/*! The size an SVG document declares, in pixels.

  \param svg The SVG document as text.
  \param ppi The resolution physical units (in, cm, mm, pt, pc) are converted
             at. A unitless length or one in px is a pixel at any resolution,
             as in every SVG renderer.
  \return The size, or wxDefaultSize if the text has no root svg element or
          that element gives no usable size.
*/
wxSize SvgIntrinsicSize(const wxString &svg, double ppi = 96);
} // namespace wxm

#endif // SVGSIZE_H
