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
  Tests for MathImport: LaTeX and MathML maths to Maxima commands
  (GH #932, GH #760). GUI-free and needs no Maxima, so tested directly.
*/

#include "MathImport.h"

#include <wx/init.h>

// Not CATCH_CONFIG_MAIN: see test_SyntaxErrorLocation.cpp
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::LaTeXToMaxima;
using wxm::MathMLToMaxima;
using wxm::MathToMaxima;

namespace {
//! The Maxima command, or the error message prefixed with "ERROR: "
std::string FromLaTeX(const wxString &latex) {
  const auto result = LaTeXToMaxima(latex);
  if (!result.Ok())
    return "ERROR: " + result.error.ToStdString(wxConvUTF8);
  return result.maxima.ToStdString(wxConvUTF8);
}

std::string FromMathML(const wxString &mathml) {
  const auto result = MathMLToMaxima(mathml);
  if (!result.Ok())
    return "ERROR: " + result.error.ToStdString(wxConvUTF8);
  return result.maxima.ToStdString(wxConvUTF8);
}

bool Fails(const std::string &converted) { return converted.rfind("ERROR: ", 0) == 0; }

wxString Math(const wxString &body) {
  return wxS("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">") + body +
    wxS("</math>");
}
} // namespace

SCENARIO("Plain arithmetic converts with the fewest parentheses") {
  CHECK(FromLaTeX(wxS("x^2+2x+1")) == "x^2+2*x+1");
  CHECK(FromLaTeX(wxS("a-b+c")) == "a-b+c");
  CHECK(FromLaTeX(wxS("a-(b+c)")) == "a-(b+c)");
  CHECK(FromLaTeX(wxS("(a+b)(a-b)")) == "(a+b)*(a-b)");
  CHECK(FromLaTeX(wxS("2 \\cdot 3 \\times 4")) == "2*3*4");
  CHECK(FromLaTeX(wxS("a \\div b")) == "a/b");
  CHECK(FromLaTeX(wxS("-x^2")) == "-x^2");
  CHECK(FromLaTeX(wxS("(-x)^2")) == "(-x)^2");
  CHECK(FromLaTeX(wxS("x^{-1}")) == "x^(-1)");
  CHECK(FromLaTeX(wxS("2^{3^4}")) == "2^3^4");
  CHECK(FromLaTeX(wxS("(2^3)^4")) == "(2^3)^4");
  CHECK(FromLaTeX(wxS("3.14 r^2")) == "3.14*r^2");
  CHECK(FromLaTeX(wxS("n!")) == "n!");
  CHECK(FromLaTeX(wxS("(n+1)!")) == "(n+1)!");
}

SCENARIO("A script without braces is a single token, as in TeX") {
  CHECK(FromLaTeX(wxS("x^23")) == "x^2*3");
  CHECK(FromLaTeX(wxS("x^{23}")) == "x^23");
  CHECK(FromLaTeX(wxS("\\frac12")) == "1/2");
}

SCENARIO("Math delimiters and trailing punctuation are removed") {
  CHECK(FromLaTeX(wxS("$x+1$")) == "x+1");
  CHECK(FromLaTeX(wxS("$$x+1$$")) == "x+1");
  CHECK(FromLaTeX(wxS("\\(x+1\\)")) == "x+1");
  CHECK(FromLaTeX(wxS("\\[ x+1, \\]")) == "x+1");
  CHECK(FromLaTeX(wxS("\\begin{equation} E = m c^2. \\end{equation}")) == "E=m*c^2");
  CHECK(FromLaTeX(wxS("x+1 % a comment\n")) == "x+1");
}

SCENARIO("Fractions and roots") {
  CHECK(FromLaTeX(wxS("\\frac{a+b}{c}")) == "(a+b)/c");
  CHECK(FromLaTeX(wxS("\\frac{a}{b c}")) == "a/(b*c)");
  CHECK(FromLaTeX(wxS("\\dfrac{1}{x}+1")) == "1/x+1");
  CHECK(FromLaTeX(wxS("\\frac{\\sqrt{(x-2)^2}}{4}")) == "sqrt((x-2)^2)/4");
  CHECK(FromLaTeX(wxS("\\sqrt[3]{x+1}")) == "(x+1)^(1/3)");
  CHECK(FromLaTeX(wxS("\\binom{n}{k}")) == "binomial(n,k)");
}

SCENARIO("Constants and Greek letters") {
  CHECK(FromLaTeX(wxS("2\\pi r")) == "2*%pi*r");
  CHECK(FromLaTeX(wxS("e^{x}")) == "%e^x");
  CHECK(FromLaTeX(wxS("\\mathrm{e}^{\\mathrm{i}\\varphi}")) == "%e^(%i*phi)");
  THEN("a plain i stays a variable, as it is usually an index") {
    CHECK(FromLaTeX(wxS("x_i + i")) == "x_i+i");
  }
  CHECK(FromLaTeX(wxS("\\alpha+\\Omega")) == "alpha+Omega");
  CHECK(FromLaTeX(wxS("\\infty")) == "inf");
  CHECK(FromLaTeX(wxS("α + π")) == "alpha+%pi");
}

SCENARIO("Functions, with and without parentheses") {
  CHECK(FromLaTeX(wxS("\\sin x")) == "sin(x)");
  CHECK(FromLaTeX(wxS("\\sin 2x")) == "sin(2*x)");
  CHECK(FromLaTeX(wxS("\\sin x \\cos x")) == "sin(x)*cos(x)");
  CHECK(FromLaTeX(wxS("\\sin(x)+1")) == "sin(x)+1");
  CHECK(FromLaTeX(wxS("\\sin^2 x + \\cos^2 x")) == "sin(x)^2+cos(x)^2");
  CHECK(FromLaTeX(wxS("\\sin(x)^2")) == "sin(x)^2");
  CHECK(FromLaTeX(wxS("\\sin^{-1} x")) == "asin(x)");
  CHECK(FromLaTeX(wxS("\\arctan(x)")) == "atan(x)");
  CHECK(FromLaTeX(wxS("\\ln x")) == "log(x)");
  CHECK(FromLaTeX(wxS("\\log_2 x")) == "log(x)/log(2)");
  CHECK(FromLaTeX(wxS("\\exp\\left(-\\frac{x^2}{2}\\right)")) == "exp(-x^2/2)");
  CHECK(FromLaTeX(wxS("\\operatorname{erfc}(x)")) == "erfc(x)");
  CHECK(FromLaTeX(wxS("\\max(a, b)")) == "max(a,b)");
  THEN("f(x) is a call, but x(y+1) is a product") {
    CHECK(FromLaTeX(wxS("f(x)+g(x,y)")) == "f(x)+g(x,y)");
    CHECK(FromLaTeX(wxS("x(y+1)")) == "x*(y+1)");
    CHECK(FromLaTeX(wxS("\\mathrm{speed}(t)")) == "speed(t)");
  }
  CHECK(FromLaTeX(wxS("f(x) := x^2")) == "f(x):=x^2");
}

SCENARIO("Subscripts become names or array indices") {
  CHECK(FromLaTeX(wxS("x_1+x_{12}")) == "x_1+x_12");
  CHECK(FromLaTeX(wxS("a_{ij}")) == "a_ij");
  CHECK(FromLaTeX(wxS("a_{n+1}")) == "a[n+1]");
  CHECK(FromLaTeX(wxS("x_1^2")) == "x_1^2");
}

SCENARIO("Brackets, sets and absolute values") {
  CHECK(FromLaTeX(wxS("\\left( \\frac{a}{b} \\right)^2")) == "(a/b)^2");
  CHECK(FromLaTeX(wxS("|x-1|")) == "abs(x-1)");
  CHECK(FromLaTeX(wxS("|x|^2+|y|")) == "abs(x)^2+abs(y)");
  CHECK(FromLaTeX(wxS("\\left|x\\right|")) == "abs(x)");
  CHECK(FromLaTeX(wxS("[1, 2, 3]")) == "[1,2,3]");
  CHECK(FromLaTeX(wxS("\\{1, 2\\}")) == "{1,2}");
  CHECK(Fails(FromLaTeX(wxS("(a+b"))));
  CHECK(Fails(FromLaTeX(wxS("a+b)"))));
  CHECK(Fails(FromLaTeX(wxS("[0, 1)"))));
}

SCENARIO("Derivatives") {
  CHECK(FromLaTeX(wxS("\\frac{d}{dx} x^2")) == "diff(x^2,x)");
  CHECK(FromLaTeX(wxS("\\frac{\\mathrm{d}y}{\\mathrm{d}x}")) == "diff(y,x)");
  CHECK(FromLaTeX(wxS("\\frac{d^2 y}{dx^2}")) == "diff(y,x,2)");
  CHECK(FromLaTeX(wxS("\\frac{\\partial^2 f}{\\partial x \\partial y}")) ==
        "diff(f,x,1,y,1)");
  CHECK(FromLaTeX(wxS("\\frac{d}{dt}\\left(t \\sin t\\right)")) == "diff(t*sin(t),t)");
  CHECK(FromLaTeX(wxS("f'(x)")) == "diff(f(x),x)");
  CHECK(FromLaTeX(wxS("f''(t)")) == "diff(f(t),t,2)");
  CHECK(FromLaTeX(wxS("f^{\\prime}(x)")) == "diff(f(x),x)");
  THEN("an ordinary fraction with a d in it stays a fraction") {
    CHECK(FromLaTeX(wxS("\\frac{d}{2}")) == "d/2");
  }
}

SCENARIO("Integrals, sums, products and limits") {
  CHECK(FromLaTeX(wxS("\\int x^2 \\, dx")) == "integrate(x^2,x)");
  CHECK(FromLaTeX(wxS("\\int_0^1 x^2 \\,\\mathrm{d}x")) == "integrate(x^2,x,0,1)");
  CHECK(FromLaTeX(wxS("\\int_{0}^{\\infty} e^{-x} dx")) == "integrate(%e^(-x),x,0,inf)");
  CHECK(FromLaTeX(wxS("\\int \\sin x \\, dx")) == "integrate(sin(x),x)");
  CHECK(FromLaTeX(wxS("\\int \\frac{1}{x} dx + C")) == "integrate(1/x,x)+C");
  CHECK(FromLaTeX(wxS("\\int_0^1\\int_0^1 x y \\,dx\\,dy")) ==
        "integrate(integrate(x*y,x,0,1),y,0,1)");
  CHECK(Fails(FromLaTeX(wxS("\\int x^2"))));

  CHECK(FromLaTeX(wxS("\\sum_{k=1}^{n} k^2")) == "sum(k^2,k,1,n)");
  CHECK(FromLaTeX(wxS("\\sum\\limits_{i=0}^{\\infty} \\frac{x^i}{i!}")) ==
        "sum(x^i/i!,i,0,inf)");
  CHECK(FromLaTeX(wxS("\\prod_{i=1}^{n} i")) == "product(i,i,1,n)");
  CHECK(FromLaTeX(wxS("\\sum_{k=1}^{n} k + 1")) == "sum(k,k,1,n)+1");
  CHECK(Fails(FromLaTeX(wxS("\\sum_i a_i"))));

  CHECK(FromLaTeX(wxS("\\lim_{x \\to 0} \\frac{\\sin x}{x}")) == "limit(sin(x)/x,x,0)");
  CHECK(FromLaTeX(wxS("\\lim_{x \\to 0^+} \\frac{1}{x}")) == "limit(1/x,x,0,plus)");
  CHECK(FromLaTeX(wxS("\\lim_{n\\rightarrow\\infty} (1+\\frac{1}{n})^n")) ==
        "limit((1+1/n)^n,n,inf)");
}

SCENARIO("Relations, lists and equation arrays") {
  CHECK(FromLaTeX(wxS("x \\le 1")) == "x<=1");
  CHECK(FromLaTeX(wxS("x \\neq 0")) == "x#0");
  CHECK(FromLaTeX(wxS("x=1, y=2")) == "[x=1,y=2]");
  CHECK(FromLaTeX(wxS("\\begin{align} x &= 1 \\\\ y &= 2 \\end{align}")) ==
        "x=1;\ny=2");
}

SCENARIO("Matrices") {
  CHECK(FromLaTeX(wxS("\\begin{pmatrix} 1 & 2 \\\\ 3 & 4 \\end{pmatrix}")) ==
        "matrix([1,2],[3,4])");
  CHECK(FromLaTeX(wxS("\\begin{bmatrix} a & b \\\\ c & d \\\\ \\end{bmatrix}")) ==
        "matrix([a,b],[c,d])");
  CHECK(FromLaTeX(wxS("\\begin{vmatrix} a & b \\\\ c & d \\end{vmatrix}")) ==
        "determinant(matrix([a,b],[c,d]))");
  CHECK(FromLaTeX(wxS("\\left(\\begin{array}{cc} 1 & 0 \\\\ 0 & 1 \\end{array}\\right)")) ==
        "matrix([1,0],[0,1])");
  CHECK(FromLaTeX(wxS("\\begin{pmatrix} x \\\\ y \\end{pmatrix}")) == "matrix([x],[y])");
}

SCENARIO("Anything without a single meaning is refused, not guessed") {
  CHECK(Fails(FromLaTeX(wxS("x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}"))));
  CHECK(Fails(FromLaTeX(wxS("a_1 + \\dots + a_n"))));
  CHECK(Fails(FromLaTeX(wxS("\\text{for all } x"))));
  CHECK(Fails(FromLaTeX(wxS("\\alpha \\in S"))));
  CHECK(Fails(FromLaTeX(wxS("x^"))));
  CHECK(Fails(FromLaTeX(wxS("\\frac{1}"))));
  CHECK(Fails(FromLaTeX(wxS(""))));
  THEN("the message names what couldn't be read") {
    CHECK(FromLaTeX(wxS("\\in")).find("\\in") != std::string::npos);
  }
}

SCENARIO("Presentation MathML") {
  CHECK(FromMathML(Math(wxS("<mrow><msup><mi>x</mi><mn>2</mn></msup><mo>+</mo>"
                            "<mn>1</mn></mrow>"))) == "x^2+1");
  CHECK(FromMathML(Math(wxS("<mfrac><msqrt><msup><mrow><mo>(</mo><mi>x</mi>"
                            "<mo>&#x2212;</mo><mn>2</mn><mo>)</mo></mrow><mn>2</mn>"
                            "</msup></msqrt><mn>4</mn></mfrac>"))) ==
        "sqrt((x-2)^2)/4");
  THEN("invisible times and function application are honoured") {
    CHECK(FromMathML(Math(wxS("<mn>2</mn><mo>&InvisibleTimes;</mo><mi>x</mi>"))) ==
          "2*x");
    CHECK(FromMathML(Math(wxS("<mi>g</mi><mo>&#x2061;</mo><mi>x</mi>"))) == "g(x)");
    CHECK(FromMathML(Math(wxS("<mi>sin</mi><mo>&#x2061;</mo><mrow><mo>(</mo>"
                              "<mi>x</mi><mo>)</mo></mrow>"))) == "sin(x)");
  }
  THEN("multi-letter identifiers are one name") {
    CHECK(FromMathML(Math(wxS("<mi>xy</mi><mo>+</mo><mi>z</mi>"))) == "xy+z");
  }
  CHECK(FromMathML(Math(wxS("<mfenced><mi>a</mi><mi>b</mi></mfenced>"))) ==
        "ERROR: A list in parentheses, like (a, b), has no single Maxima meaning.");
  CHECK(FromMathML(Math(wxS("<munderover><mo>&#x2211;</mo><mrow><mi>k</mi>"
                            "<mo>=</mo><mn>1</mn></mrow><mi>n</mi></munderover>"
                            "<mi>k</mi>"))) == "sum(k,k,1,n)");
  CHECK(FromMathML(Math(wxS("<msubsup><mo>&#x222B;</mo><mn>0</mn><mn>1</mn>"
                            "</msubsup><mi>x</mi><mo>&#x2146;</mo><mi>x</mi>"))) ==
        "integrate(x,x,0,1)");
  CHECK(FromMathML(Math(wxS("<mo>&#x222B;</mo><mi>x</mi><mi>dx</mi>"))) ==
        "integrate(x,x)");
  CHECK(FromMathML(Math(wxS("<mo>&#x222B;</mo><mi>x</mi>"))) ==
        "ERROR: Couldn't find the integration variable (the \"dx\") of an integral.");
  CHECK(FromMathML(Math(wxS("<msubsup><mo>&#x222B;</mo><mn>0</mn><mn>1</mn>"
                            "</msubsup><mi>x</mi><mi>&#x2146;</mi><mi>x</mi>"))) ==
        "integrate(x,x,0,1)");
  CHECK(FromMathML(Math(wxS("<mi>&#x3C0;</mi><msup><mi>r</mi><mn>2</mn></msup>"))) ==
        "%pi*r^2");
  CHECK(FromMathML(wxS("<m:math xmlns:m=\"http://www.w3.org/1998/Math/MathML\">"
                       "<m:mi>a</m:mi><m:mo>+</m:mo><m:mi>b</m:mi></m:math>")) ==
        "a+b");
  CHECK(Fails(FromMathML(wxS("<math><mi>a</mi>"))));
  CHECK(Fails(FromMathML(Math(wxS("<apply><plus/><ci>a</ci><ci>b</ci></apply>")))));
  CHECK(Fails(FromMathML(Math(wxS("<mover><mi>x</mi><mo>&#xAF;</mo></mover>")))));
}

SCENARIO("wxMaxima's own MathML comes back as what it was") {
  GIVEN("a copy with the Maxima command as an annotation") {
    const wxString mathml =
      Math(wxS("<semantics><mrow><msup><mi>x</mi><mn>2</mn></msup></mrow>"
               "<annotation encoding=\"application/x-maxima\">x^2+y_1</annotation>"
               "</semantics>"));
    THEN("the annotation is used as it is") { CHECK(FromMathML(mathml) == "x^2+y_1"); }
  }
  GIVEN("labelled output without an annotation") {
    const wxString mathml =
      Math(wxS("<semantics><mtable columnalign=\"left\"><mtr><mtd><mtext>(%o1) </mtext>"
               "</mtd><mtd><msup><mi>x</mi><mn>2</mn></msup></mtd></mtr>"
               "<mtr><mtd><mtext>(%o2) </mtext></mtd><mtd><mi>y</mi></mtd></mtr>"
               "</mtable></semantics>"));
    THEN("the labels are dropped, one statement per line") {
      CHECK(FromMathML(mathml) == "x^2;\ny");
    }
  }
  GIVEN("a matrix as MatrCell writes it") {
    const wxString mathml =
      Math(wxS("<mrow><mo>(</mo><mrow><mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn>"
               "</mtd></mtr></mtable></mrow><mo>)</mo></mrow>"));
    THEN("it is a matrix") { CHECK(FromMathML(mathml) == "matrix([1,2])"); }
  }
}

SCENARIO("MathToMaxima tells MathML from LaTeX") {
  CHECK(wxm::LooksLikeMathML(wxS("  <?xml version=\"1.0\"?>\n<math><mi>x</mi></math>")));
  CHECK(wxm::LooksLikeMathML(wxS("<math display=\"block\"><mi>x</mi></math>")));
  CHECK_FALSE(wxm::LooksLikeMathML(wxS("x < y")));
  CHECK(MathToMaxima(wxS("<math><mi>x</mi></math>")).maxima == wxS("x"));
  CHECK(MathToMaxima(wxS("\\frac{1}{2}")).maxima == wxS("1/2"));
}

int main(int argc, char *argv[]) {
  wxInitializer init;
  return Catch::Session().run(argc, argv);
}
