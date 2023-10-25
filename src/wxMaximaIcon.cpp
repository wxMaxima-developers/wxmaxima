// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class Image that stores compressed images and handles
  scaling and uncompressing them.
*/

#include "wxMaximaIcon.h"
#include "../data/icon.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>

wxIcon wxMaximaIcon() {
  wxImage img;
  wxMemoryInputStream istream(io_github_wxmaxima_developers_wxMaxima_png,
                              io_github_wxmaxima_developers_wxMaxima_png_len);
  img.LoadFile(istream);
  wxIcon icon;
  icon.CopyFromBitmap(wxBitmap(img));
  return icon;
}
