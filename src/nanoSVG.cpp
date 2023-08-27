// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file instantiates nanoSVG, except the case wxWidgets does do so. If it
  fails to do this we get linker errors wrt nanoSVG after compilation.
*/
#include <wx/wx.h>

/* The nanosvg .h files contain both the header and the implementation.
   In exactly one file of the project need to be defined in order to
   make the implementation visible to the compiler.

   In wxWidgets >3.1.6 this is done in wxWidgets itself so we need to
   skip that step there.

   So that should work. But for Linux builds with 3.1.6 I get linking errors,
   e.g.: undefined reference to `nsvgRasterize' Therefore do not define
   NANOSVG_IMPLEMENTATION/NANOSVGRAST_IMPLEMENTATION only for Windows. That is
   probably not correct (a FIXME), but I have no idea, why Linux still requires
   that NANOSVG_IMPLEMENTATION/NANOSVGRAST_IMPLEMENTATION is defined.
*/
#if (wxCHECK_VERSION(3, 1, 6)) && (defined(__WINDOWS__))
#ifdef _MSC_VER
#if _MSC_VER < 1700
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"
#endif
#endif
#else
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"
#endif
