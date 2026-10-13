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
  Tests reading an SVG's own size from its root element.
*/

#include "SvgSize.h"

// Not CATCH_CONFIG_MAIN: see test_SyntaxErrorLocation.cpp for why.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::SvgIntrinsicSize;

SCENARIO("gnuplot's SVG gives its size in width and height") {
  const wxString svg = wxS(
    "<?xml version=\"1.0\" encoding=\"utf-8\"  standalone=\"no\"?>\n"
    "<svg \n"
    " width=\"600\" height=\"400\"\n"
    " viewBox=\"0 0 600 400\"\n"
    " xmlns=\"http://www.w3.org/2000/svg\">\n"
    "<title>Gnuplot</title></svg>\n");
  REQUIRE(SvgIntrinsicSize(svg) == wxSize(600, 400));
}

SCENARIO("Physical units are converted at the given resolution") {
  const wxString svg = wxS("<svg width=\"2in\" height='72pt'></svg>");
  REQUIRE(SvgIntrinsicSize(svg, 96) == wxSize(192, 96));
  REQUIRE(SvgIntrinsicSize(svg, 192) == wxSize(384, 192));
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"2.54cm\" height=\"25.4mm\"/>"), 100) ==
          wxSize(100, 100));
}

SCENARIO("Pixels stay pixels at any resolution") {
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"30px\" height=\"20\"/>"), 300) ==
          wxSize(30, 20));
}

SCENARIO("A missing or relative size comes from the viewBox") {
  REQUIRE(SvgIntrinsicSize(wxS("<svg viewBox=\"0 0 120 60\"/>")) == wxSize(120, 60));
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"100%\" viewBox=\"0,0,120,60\"/>")) ==
          wxSize(120, 60));
  // Only one of them given: the other keeps the viewBox' aspect ratio
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"240\" viewBox=\"10 10 120 60\"/>")) ==
          wxSize(240, 120));
}

SCENARIO("Comments, processing instructions and a doctype come before the root") {
  const wxString svg = wxS(
    "<?xml version=\"1.0\"?>\n"
    "<!-- <svg width=\"1\" height=\"1\"> is not the root -->\n"
    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" "
    "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
    "<svg height=\"50\" width=\"80\">");
  REQUIRE(SvgIntrinsicSize(svg) == wxSize(80, 50));
}

SCENARIO("Something that isn't an SVG with a size has no size") {
  REQUIRE(SvgIntrinsicSize(wxEmptyString) == wxDefaultSize);
  REQUIRE(SvgIntrinsicSize(wxS("<html width=\"10\" height=\"10\"/>")) == wxDefaultSize);
  REQUIRE(SvgIntrinsicSize(wxS("<svgx width=\"10\" height=\"10\"/>")) == wxDefaultSize);
  REQUIRE(SvgIntrinsicSize(wxS("<svg/>")) == wxDefaultSize);
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"10\" height=\"tall\"/>")) == wxDefaultSize);
  REQUIRE(SvgIntrinsicSize(wxS("<svg width=\"-10\" height=\"10\"/>")) == wxDefaultSize);
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
