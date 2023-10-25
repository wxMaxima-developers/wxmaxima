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
  This C++ project instantiates nanoSVG. If it is linked with wxWidgets
  and wxWidgets provides nanoSVG it fails to build with a linker error.
  CMake tests for that before deciding if to include nanoSVG.cpp in the
  wxMaxima sources.
*/

/*

  27.8.2023:
  Local Ubuntu:
  Appveyor
*/
#include <stdio.h>
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"

int main(int argc, char* argv[]) {
  struct NSVGimage* image;
  image = nsvgParseFromFile("test.svg", "px", 96);
  return 0;
}

