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
  Regression coverage for RTF/OMML export (Cell::ToRTF()/ToOMML(), Cell::
  ListToRTF(), Cell::OMML2RTF()) -- until now this path had no test coverage
  at all.

  GH #1456: TextCell::ToRTF() didn't check IsHidden()/GetHidableMultSign()/
  HidemultiplicationSign() the way ToTeX()/ToXML() already do. A hidden
  multiplication sign therefore always showed up as a literal "*" in RTF
  export regardless of the "Hide multiplication sign" setting, and -- had it
  ever been suppressed without a replacement -- would have concatenated its
  neighbours with no separator at all (e.g. the scientific-notation "2*10^7"
  becoming the unreadable "210^7").

  GH #1457: MatrCell::ToOMML() emitted "<m:grow>\"1\"</m:grow>" (a child
  element whose text content is the two literal characters '"' and '1' and
  '"') instead of the "m:grow=\"1\"" attribute form every other delimiter-
  emitting cell (ParenCell/ListCell/IntervalCell) already uses. Since
  Cell::OMML2RTF() turns element text content into raw RTF, that produced
  the malformed math control word "{\mgrow "1"}" (literal quote characters
  in what must be a bare flag) instead of the well-formed "{\mgrow 1}" --
  Word/LibreOffice silently ignored it and rendered the delimiter at a
  fixed, small size regardless of the matrix's actual height.

  All content below is hand-written XML matching exactly what wxMathML.lisp
  emits for the corresponding Maxima expressions (verified against
  wxMathML.lisp itself, not guessed) -- no live Maxima needed.
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

// wxxmlnumformat's (wxMathML.lisp) scientific-notation output for 2e7,
// i.e. what Maxima sends wxMaxima for a float displayed as "2*10^7": a
// hidden ("<h>") multiplication sign between the mantissa and "10^7".
const char *const sciNotationXml =
  R"(<mth><mrow><n>2</n><h>*</h><msup><n>10</n><n>7</n></msup></mrow></mth>)";

// A plain, non-hidden multiplication for contrast: 2*a.
const char *const plainMultXml =
  R"(<mth><mrow><n>2</n><mo>*</mo><v>a</v></mrow></mth>)";

// A 2x2 matrix for each of the paren styles wxxml-matrix (wxMathML.lisp)
// can tag a <tb> with.
wxString MatrixXml(const char *parenAttr) {
  return wxString::Format(
      wxS("<mth><tb %s><mtr><mtd><n>1</n></mtd><mtd><n>2</n></mtd></mtr>"
          "<mtr><mtd><n>3</n></mtd><mtd><n>4</n></mtd></mtr></tb></mth>"),
      parenAttr);
}
} // namespace

SCENARIO("A hidden multiplication sign in RTF export respects the "
         "\"Hide multiplication sign\" setting (GH #1456)") {
  GIVEN("HidemultiplicationSign is off") {
    g_cfg->HidemultiplicationSign(false);
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(sciNotationXml));
    REQUIRE(output != nullptr);
    const wxString rtf = output->ListToRTF(true);

    THEN("the multiplication sign is shown, same as on screen") {
      REQUIRE(rtf.Contains(wxS("{*}")));
    }
  }

  GIVEN("HidemultiplicationSign is on") {
    g_cfg->HidemultiplicationSign(true);
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(sciNotationXml));
    REQUIRE(output != nullptr);
    const wxString rtf = output->ListToRTF(true);

    THEN("no literal \"*\" reaches the RTF output") {
      REQUIRE_FALSE(rtf.Contains(wxS("{*}")));
    }
    THEN("a separator space is left behind instead of nothing, so the "
         "mantissa and the exponent's \"10\" don't run together") {
      // The hidden-multiplication TextCell must still contribute a space,
      // not an empty string -- otherwise "2" and the "10^7" OMML field
      // that follows it would render back-to-back as "210^7".
      REQUIRE(rtf.Contains(wxS("{ }")));
    }
  }

  GIVEN("HidemultiplicationSign is on but the multiplication is a plain, "
        "user-visible one (not marked hidden by Maxima)") {
    g_cfg->HidemultiplicationSign(true);
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(plainMultXml));
    REQUIRE(output != nullptr);
    const wxString rtf = output->ListToRTF(true);

    THEN("it is still shown -- only cells Maxima marked hidable are hidden") {
      REQUIRE(rtf.Contains(wxS("{*}")));
    }
  }
}

SCENARIO("A matrix's delimiters grow to match its height in RTF/OMML "
         "export, for every bracket style (GH #1457)") {
  struct Style {
    const char *xmlAttr;
    const char *begChr;
    const char *endChr;
  };
  // begChr/endChr as they appear in the OMML/RTF output: "<" and ">" are
  // XML-escaped by MatrCell::ToOMML() itself.
  const Style styles[] = {
      {"roundedParens=\"true\"", "(", ")"},
      {"bracketParens=\"true\"", "[", "]"},
      {"angledParens=\"true\"", "&lt;", "&gt;"},
      {"straightParens=\"true\"", "|", "|"},
  };

  for (const auto &style : styles) {
    GIVEN(wxString::Format("a matrix tagged %s", style.xmlAttr).ToStdString()) {
      MathParser parser(g_cfg);
      auto output = parser.ParseLine(MatrixXml(style.xmlAttr));
      REQUIRE(output != nullptr);
      const wxString omml = output->ListToOMML();
      const wxString rtf = output->ListToRTF(true);

      THEN("the OMML delimiter uses the same m:begChr=/m:endChr=/m:grow= "
           "attribute form as ParenCell/ListCell/IntervalCell") {
        REQUIRE(omml.Contains(
            wxString::Format(wxS("m:begChr=\"%s\""), style.begChr)));
        REQUIRE(omml.Contains(
            wxString::Format(wxS("m:endChr=\"%s\""), style.endChr)));
        REQUIRE(omml.Contains(wxS("m:grow=\"1\"")));
      }
      THEN("the RTF math control word for grow is the bare flag "
           "\"{\\mgrow 1}\", not the malformed \"{\\mgrow \\\"1\\\"}\"") {
        REQUIRE(rtf.Contains(wxS("{\\mgrow 1}")));
        REQUIRE_FALSE(rtf.Contains(wxS("\\mgrow \"1\"")));
      }
    }
  }
}

SCENARIO("abs()'s bars grow to match content height in RTF/OMML export, "
         "consistent with the other delimiter-emitting cells") {
  // abs(a/b): a FracCell inside an AbsCell, so a real "tall content" case.
  const char *const absOfFracXml =
      R"(<mth><a><f><v>a</v><v>b</v></f></a></mth>)";

  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(absOfFracXml));
  REQUIRE(output != nullptr);
  const wxString omml = output->ListToOMML();

  THEN("the delimiter is marked to grow, like Parens/List/Interval/Matrix") {
    REQUIRE(omml.Contains(wxS("m:begChr=\"|\"")));
    REQUIRE(omml.Contains(wxS("m:grow=\"1\"")));
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
