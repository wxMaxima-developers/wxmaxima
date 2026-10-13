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
  Tests SvgBitmap::RendersText(), which decides whether plots may be SVG.
*/

#include <wx/app.h>
#include <wx/image.h>
#include <wx/sysopt.h>
#include "SvgBitmap.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

// NanoSVG ignores text; LunaSVG draws it. wxBitmapBundle::FromSVG() uses
// NanoSVG whenever it is enabled, so LunaSVG only renders if NanoSVG is off.
SCENARIO("Only a wxWidgets that renders SVG with LunaSVG is said to draw text") {
#if wxUSE_NANOSVG || !wxUSE_LUNASVG
  REQUIRE(!SvgBitmap::RendersText());
#else
  // Needs the font gnuplot asks for, or LunaSVG's fallback for it: Arial on
  // MSW and macOS, DejaVu Sans on Linux.
  REQUIRE(SvgBitmap::RendersText());
#endif
}

SCENARIO("Asking twice gives the same answer") {
  REQUIRE(SvgBitmap::RendersText() == SvgBitmap::RendersText());
}

class MyApp : public wxApp
{
public:
  MyApp() {
    // See test_ImgCell.cpp: suppresses wx 3.3's modal manifest warning
    wxSystemOptions::SetOption(wxS("msw.no-manifest-check"), 1);
  }
  bool OnInit() override {
    int rc = Catch::Session().run();
    std::exit(rc);
    return false;
  }
};

IMPLEMENT_APP(MyApp);
