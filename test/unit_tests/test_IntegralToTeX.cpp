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
  Pins the LaTeX export of an integral's "d" (GH #972): the "d" of "dx"/
  "d\theta"/... must render upright (\mathrm{d}), not in math mode's default
  italic. wxMathML.lisp's wxxml-int emits this "d" as its own <s> ("special
  constant") tag -- the same style %e/%i/%pi/inf/minf use -- but unlike
  those, "d" fell all the way through TextCell::ToTeX()'s early-returning
  TS_SPECIAL_CONSTANT branch as a bare, unstyled character, since none of
  that branch's specific cases matched it.

  The math content is two hand-written <in> (IntCell) XML fragments, parsed
  through the real MathParser exactly like wxMathML.lisp would emit them for
  the definite and indefinite forms of integrate() -- no live Maxima needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/Cell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

// integrate(x, x, 0, 1): the definite-integral (5-argument) form wxxml-int
// emits as <in><mrow>low</mrow><mrow>hi</mrow><mrow>base</mrow>
// <mrow><s>d</s>var</mrow></in>.
const char *const definiteIntegralXml =
  R"(<mth><in><mrow><n>0</n></mrow><mrow><n>1</n></mrow><mrow><mi>x</mi></mrow><mrow><s>d</s><mi>x</mi></mrow></in></mth>)";

// integrate(sin(theta), theta): the indefinite-integral (3-argument) form,
// <in def="false"><mrow>base</mrow><mrow><s>d</s>var</mrow></in>.
const char *const indefiniteIntegralXml =
  R"(<mth><in def="false"><mrow><fnm>sin</fnm><mi>theta</mi></mrow><mrow><s>d</s><mi>theta</mi></mrow></in></mth>)";

// The GH #972 report's own example, byte-for-byte as captured from a live
// Maxima 5.46.0 + wxMathML.lisp for
//   'integrate('integrate(r^2*sin(theta)+8,r,0,theta),theta,0,%pi);
// (quoted so it stays a symbolic nested integral instead of evaluating).
const char *const reportedExampleXml =
  R"(<math><in><mrow><mn>0</mn></mrow><mrow><s>%pi</s></mrow><mrow><in><mrow><mn>0</mn></mrow><mrow><g>theta</g></mrow><mrow><msup><mrow><mi lisp="*var-tag*">r</mi></mrow><mn>2</mn></msup><h>*</h><fn lisp="wxxml-function"><fnm lisp="wxxml-function">sin</fnm><mrow><p><mrow><g>theta</g></mrow></p></mrow></fn><mo>+</mo><mn>8</mn></mrow><mrow><s>d</s><mi lisp="*var-tag*">r</mi></mrow></in></mrow><mrow><s>d</s><g>theta</g></mrow></in></math>)";
} // namespace

SCENARIO("A definite integral's LaTeX export renders \"d\" upright") {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(definiteIntegralXml));
  REQUIRE(output != nullptr);
  const wxString tex = output->ListToTeX();

  THEN("the differential is \\mathrm{d}, not a bare, italic \"d\"") {
    REQUIRE(tex.Contains(wxS("\\mathrm{d}")));
  }
  THEN("it directly precedes the integration variable") {
    REQUIRE(tex.Contains(wxS("\\mathrm{d}x")));
  }
}

SCENARIO("An indefinite integral's LaTeX export renders \"d\" upright") {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(indefiniteIntegralXml));
  REQUIRE(output != nullptr);
  const wxString tex = output->ListToTeX();

  THEN("the differential is \\mathrm{d}, not a bare, italic \"d\"") {
    REQUIRE(tex.Contains(wxS("\\mathrm{d}")));
  }
}

SCENARIO("GH #972's own nested double-integral example exports both differentials upright") {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(reportedExampleXml));
  REQUIRE(output != nullptr);
  const wxString tex = output->ListToTeX();

  THEN("both \"dr\" and \"dtheta\" render upright") {
    REQUIRE(tex.Contains(wxS("\\mathrm{d}r")));
    REQUIRE(tex.Contains(wxS("\\mathrm{d}theta")));
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
