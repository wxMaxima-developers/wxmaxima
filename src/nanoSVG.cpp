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
  This file instantiates nanoSVG.
*/

/* The nanosvg .h files contain both the header and the implementation.
   In exactly one file of the project need to be defined in order to
   make the implementation visible to the compiler. That might be here -
   or in wxWidgets.
*/
#include <stdio.h>
#include <wx/wx.h>
#include "Version.h"

// Before wxWidgets 3.1.6 we need to instantiate nanoSVG.
// With newer wxWidgets versions than that we needto instantiate it
// if it doesn't cause a linker error, except on mingw32 where it
// mysteriously doesn'tl but only in the example cmake tries.
#if (wxCHECK_VERSION(3, 1, 6))
#ifdef __MINGW32__
#else
#ifdef NANOSVG_CAUSES_NO_LINK_ERROR
#define INSTANTIATE_NANOSVG 1
#endif
#endif
#else
#define INSTANTIATE_NANOSVG 1
#endif

#ifdef INSTANTIATE_NANOSVG
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"
#endif
