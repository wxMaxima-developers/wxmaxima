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
  Pins the LaTeX export to the markup an expression actually needs.

  The cells' ToTeX() used to wrap everything in braces "just in case"
  ({{x}^{2}}, {x_1}, \sin{(x)}), to use \left( \right) around anything that
  contained a "+", to make every operator a \mathop{} (which gives "=" the
  spacing of a \sum), to put \ensuremath{} into what always is math mode and
  to leave empty equations between two labels that a string replacement then
  half cleaned up. Each scenario here is one of these.

  The maths is hand-written XML of the shape wxMathML.lisp emits, parsed by
  the real MathParser, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "MathParser.h"
#include "cells/Cell.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

//! The LaTeX of a piece of maths, as Copy as LaTeX would produce it
wxString TeX(const wxString &xml) {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxS("<mth>") + xml + wxS("</mth>"));
  REQUIRE(output != nullptr);
  return output->ListToTeX();
}

//! How often needle occurs in haystack
int Count(const wxString &haystack, const wxString &needle) {
  int count = 0;
  for (size_t pos = haystack.find(needle); pos != wxString::npos;
       pos = haystack.find(needle, pos + needle.Length()))
    count++;
  return count;
}
} // namespace

SCENARIO("Powers and subscripts get braces only where TeX needs them") {
  CHECK(TeX(wxS("<msup><mi>x</mi><mn>2</mn></msup>")) == wxS("x^2"));
  CHECK(TeX(wxS("<msup><mi>x</mi><mn>12</mn></msup>")) == wxS("x^{12}"));
  CHECK(TeX(wxS("<msup><mi>x</mi><mrow><mi>y</mi><mo>+</mo><mn>1</mn></mrow></msup>")) ==
        wxS("x^{y+1}"));
  CHECK(TeX(wxS("<munder><mi>x</mi><mn>1</mn></munder>")) == wxS("x_1"));
  CHECK(TeX(wxS("<munder><mi>x</mi><mn>12</mn></munder>")) == wxS("x_{12}"));
  CHECK(TeX(wxS("<msup><mrow><p><mi>x</mi><mo>+</mo><mn>1</mn></p></mrow><mn>2</mn></msup>")) ==
        wxS("(x+1)^2"));

  THEN("a base with a script of its own keeps its braces: x_1^2 would be a double script") {
    CHECK(TeX(wxS("<msup><munder><mi>x</mi><mn>1</mn></munder><mn>2</mn></msup>")) ==
          wxS("{x_1}^2"));
    CHECK(TeX(wxS("<msup><msup><mi>x</mi><mn>2</mn></msup><mn>3</mn></msup>")) ==
          wxS("{x^2}^3"));
  }
  THEN("a control word as exponent stays separated from what follows") {
    CHECK(TeX(wxS("<msup><mi>x</mi><s>%pi</s></msup><h>*</h><mi>y</mi>")) ==
          wxS("x^\\pi  y"));
  }
}

SCENARIO("Operators are typeset as operators, not as big operators") {
  const wxString tex = TeX(wxS("<mi>x</mi><mo>=</mo><mn>2</mn><mo>*</mo><mi>y</mi>"
                               "<mo>+</mo><mn>1</mn><mo>,</mo><mi>z</mi>"));
  CHECK(tex == wxS("x=2\\cdot y+1,z"));
  CHECK(!tex.Contains(wxS("\\mathop")));

  THEN("the arrow of a limit is \\to") {
    CHECK(TeX(wxS("<lm><fnm>lim</fnm><mrow><mi>x</mi><mo>&#x2192;</mo><mn>0</mn></mrow>"
                  "<mrow><mi>x</mi></mrow></lm>")) == wxS("\\lim_{x\\to 0} x"));
  }
}

SCENARIO("Function names and their arguments carry no extra markup") {
  CHECK(TeX(wxS("<fn><fnm>sin</fnm><mrow><p><mi>x</mi></p></mrow></fn>")) ==
        wxS("\\sin(x)"));
  CHECK(TeX(wxS("<fn><fnm>sin</fnm><mrow><p><mi>x</mi><mo>+</mo><mn>1</mn></p></mrow></fn>")) ==
        wxS("\\sin(x+1)"));
  CHECK(TeX(wxS("<fn><fnm>asinh</fnm><mrow><p><mi>x</mi></p></mrow></fn>")) ==
        wxS("\\operatorname{asinh}(x)"));

  THEN("a one-letter function isn't a \\mathop{}, which would center it vertically") {
    CHECK(TeX(wxS("<fn><fnm>f</fnm><mrow><p><mi>x</mi><mo>,</mo><mi>y</mi></p></mrow></fn>")) ==
          wxS("f(x,y)"));
  }
  THEN("a longer one still is") {
    CHECK(TeX(wxS("<fn><fnm>foo</fnm><mrow><p><mi>x</mi></p></mrow></fn>")) ==
          wxS("\\mathop{foo}(x)"));
  }
}

SCENARIO("Delimiters only grow when their contents are taller than a line") {
  CHECK(TeX(wxS("<mrow><p><mi>x</mi><mo>+</mo><mn>1</mn></p></mrow>")) ==
        wxS("(x+1)"));
  CHECK(TeX(wxS("<mrow><p><mfrac><mi>x</mi><mn>2</mn></mfrac></p></mrow>")) ==
        wxS("\\left( \\frac{x}{2}\\right) "));
  CHECK(TeX(wxS("<mrow><a><mi>x</mi><mo>-</mo><mn>1</mn></a></mrow>")) ==
        wxS("|x-1|"));
  CHECK(TeX(wxS("<mrow><a><mfrac><mi>x</mi><mn>2</mn></mfrac></a></mrow>")) ==
        wxS("\\left| \\frac{x}{2}\\right| "));
}

SCENARIO("A sum carries no \\left. \\right. and the parentheses the worksheet shows") {
  CHECK(TeX(wxS("<sm type=\"sum\" needsparen=\"false\"><mrow><mi>k</mi><mo>=</mo><mn>1</mn></mrow>"
                "<mrow><mi>n</mi></mrow><mrow><mi>k</mi></mrow></sm>")) ==
        wxS("\\sum_{k=1}^n k"));
  CHECK(TeX(wxS("<sm type=\"sum\" needsparen=\"true\"><mrow><mi>k</mi><mo>=</mo><mn>1</mn></mrow>"
                "<mrow><mi>n</mi></mrow><mrow><mi>k</mi><mo>+</mo><mn>1</mn></mrow></sm>")) ==
        wxS("\\sum_{k=1}^n (k+1)"));
}

SCENARIO("An integral's differential is no group of its own") {
  CHECK(TeX(wxS("<in def=\"false\"><mrow><mi>x</mi></mrow><mrow><s>d</s><mi>x</mi></mrow></in>")) ==
        wxS("\\int x\\,\\mathrm{d}x"));
  CHECK(TeX(wxS("<in><mrow><mn>0</mn></mrow><mrow><mn>1</mn></mrow><mrow><mi>x</mi></mrow>"
                "<mrow><s>d</s><mi>x</mi></mrow></in>")) ==
        wxS("\\int_0^1 x\\,\\mathrm{d}x"));
}

SCENARIO("Maths needs no \\ensuremath{}: it always is math mode") {
  CHECK(TeX(wxS("<mi>abc</mi>")) == wxS("\\mathrm{abc}"));
  CHECK(TeX(wxS("<s>%pi</s>")) == wxS("\\pi "));
}

SCENARIO("A set's braces are drawn, not used as a TeX group") {
  CHECK(TeX(wxS("<mrow set=\"true\"><t listdelim=\"true\">{</t><mn>1</mn>"
                "<t listdelim=\"true\">}</t></mrow>")) == wxS("\\{1\\}"));
}

SCENARIO("A code cell with two results exports two equations, and no empty one") {
  GroupCell group(g_cfg, GC_TYPE_CODE, wxS("x;y;"));
  MathParser parser(g_cfg);
  group.AppendOutput(parser.ParseLine(
                       wxS("<mth><lbl>(%o1) </lbl><mi>x</mi></mth>")));
  group.AppendOutput(parser.ParseLine(
                       wxS("<mth><lbl>(%o2) </lbl><mi>y</mi></mth>")));
  group.Recalculate();
  const wxString tex = group.ToTeX();
  INFO("TeX: " << tex);

  CHECK(Count(tex, wxS("\\[\\displaystyle ")) == 2);
  CHECK(Count(tex, wxS("\\]")) == 2);
  CHECK(Count(tex, wxS("\\tag{")) == 2);
  THEN("no equation is empty or padded with \\mbox{}") {
    CHECK(!tex.Contains(wxS("\\[\\]")));
    CHECK(!tex.Contains(wxS("\\[\\displaystyle \\]")));
    CHECK(!tex.Contains(wxS("\\mbox{}")));
  }
  THEN("the input prompt's \">\" is wrapped in \\ensuremath{} once, not twice") {
    CHECK(tex.Contains(wxS("\\ensuremath{>}")));
    CHECK(!tex.Contains(wxS("\\ensuremath{\\ensuremath")));
  }
  THEN("the tag shows %o1, not % o1") {
    CHECK(tex.Contains(wxS("\\tag{\\%o1}")));
  }
}

SCENARIO("An equation that holds only a label contains no empty line") {
  // A string result: the label is maths, the string is text. An empty line
  // ends a paragraph, which LaTeX refuses to do inside an equation.
  GroupCell group(g_cfg, GC_TYPE_CODE, wxS("\"s\";"));
  MathParser parser(g_cfg);
  group.AppendOutput(parser.ParseLine(
                       wxS("<mth><lbl>(%o1) </lbl><st>s</st></mth>")));
  group.Recalculate();
  const wxString tex = group.ToTeX();
  INFO("TeX: " << tex);
  CHECK(!tex.Contains(wxS("\n\n\\]")));
  CHECK(tex.Contains(wxS("\\tag{\\%o1} \n\\]")));
}

SCENARIO("Cell::TeXGroup() leaves single tokens and whole groups alone") {
  CHECK(Cell::TeXGroup(wxS("x")) == wxS("x"));
  CHECK(Cell::TeXGroup(wxS("\\alpha ")) == wxS("\\alpha "));
  CHECK(Cell::TeXGroup(wxS("\\%")) == wxS("\\%"));
  CHECK(Cell::TeXGroup(wxS("{x+1}")) == wxS("{x+1}"));
  CHECK(Cell::TeXGroup(wxS("12")) == wxS("{12}"));
  CHECK(Cell::TeXGroup(wxS("{a}{b}")) == wxS("{{a}{b}}"));
  CHECK(Cell::TeXGroup(wxS("\\{a\\}")) == wxS("{\\{a\\}}"));
  CHECK(Cell::TeXGroup(wxEmptyString) == wxS("{}"));
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
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
