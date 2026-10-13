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
  Pins that LaTeX export ignores how the worksheet happened to break a wide
  expression into lines (GH #408).

  A cell that is too wide for the window is "broken into lines": it is drawn as
  a linear sequence of pieces instead of in 2D -- a power becomes base, "^",
  "(", exponent, ")". GroupCell::ToTeX() used to walk that draw list, so a
  broken power was exported piece by piece as e^({very long exponent}) with only
  the parenthesis in the superscript (and, today, as a literal \hat{} glyph with
  no superscript at all). LaTeX does its own typesetting, so the export has to
  be the one of the unbroken expression, whatever the window width.

  Each sample is the XML Maxima sends, parsed by the real MathParser and put
  into a real GroupCell -- no live Maxima needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "CellPointers.h"
#include "cells/Cell.h"
#include "cells/CellIterators.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

//! The label every sample starts with: a GroupCell's first output cell is its
//! label slot, which GetOutput() skips.
const wxString label = wxS("<lbl>(%o1) </lbl>");

//! A code cell whose output is xml.
std::unique_ptr<GroupCell> MakeOutput(const wxString &xml) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("x;"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxS("<mth>") + label + xml + wxS("</mth>"));
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  group->Recalculate();
  return group;
}

//! Breaks every top-level output cell of group into lines, as a narrow window
//! would. Returns how many cells actually broke up.
int BreakUpOutput(GroupCell &group) {
  int broken = 0;
  for (Cell &cell : OnList(group.GetOutput()))
    if (cell.BreakUp())
      ++broken;
  return broken;
}

//! The exponent as TeX reads it: whatever group directly follows the "^".
wxString TeXSuperscript(const wxString &tex) {
  const int hat = tex.Find(wxS('^'));
  if (hat == wxNOT_FOUND)
    return {};
  const wxString rest = tex.Mid(hat + 1);
  if (!rest.StartsWith(wxS("{")))
    return rest.Left(1);
  int depth = 0;
  for (size_t i = 0; i < rest.Length(); ++i) {
    if (rest[i] == wxS('{'))
      ++depth;
    else if (rest[i] == wxS('}') && --depth == 0)
      return rest.Mid(1, i - 1);
  }
  return {};
}
} // namespace

SCENARIO("A power that is broken into lines exports its whole exponent as superscript") {
  // %e^(x+y), the issue's own example.
  auto group = MakeOutput(
    wxS("<e><r><s>%e</s></r><r><v>x</v><mo>+</mo><v>y</v></r></e>"));
  const wxString unbroken = group->ToTeX();
  REQUIRE(BreakUpOutput(*group) == 1);
  const wxString broken = group->ToTeX();
  INFO("unbroken: " << unbroken);
  INFO("broken: " << broken);

  THEN("the superscript is the whole exponent, not just its parenthesis") {
    REQUIRE(TeXSuperscript(broken).Contains(wxS("x")));
    REQUIRE(TeXSuperscript(broken).Contains(wxS("y")));
  }
  THEN("no hat glyph from the linear form ends up in the export") {
    REQUIRE(!broken.Contains(wxS("\\hat")));
  }
  THEN("the export is the same as for the unbroken power") {
    REQUIRE(broken == unbroken);
  }
}

SCENARIO("Every kind of cell that can break into lines exports as if it were unbroken") {
  const wxString xml = GENERATE(
    // x^2: an exponent the linear form shows without parentheses
    wxS("<e><r><v>x</v></r><r><n>2</n></r></e>"),
    // (a+b)/c
    wxS("<f><r><v>a</v><mo>+</mo><v>b</v></r><r><v>c</v></r></f>"),
    // sqrt(a+b)
    wxS("<q><v>a</v><mo>+</mo><v>b</v></q>"),
    // abs(a+b)
    wxS("<a><v>a</v><mo>+</mo><v>b</v></a>"),
    // f(a+b)
    wxS("<fn><r><fnm>f</fnm></r><r><p><v>a</v><mo>+</mo><v>b</v></p></r></fn>"),
    // (a+b), as the arguments of a function
    wxS("<p><v>a</v><mo>+</mo><v>b</v></p>"),
    // sum(k,k,1,n)
    wxS("<sm><r><v>k</v><mo>=</mo><n>1</n></r><r><v>n</v></r><r><v>k</v></r></sm>"),
    // integrate(x,x,0,1)
    wxS("<in><r><n>0</n></r><r><n>1</n></r><r><v>x</v></r><r><s>d</s><v>x</v></r></in>"),
    // 'diff(f,x)
    wxS("<d><f diffstyle=\"yes\"><r><s>d</s></r><r><s>d</s><v>x</v></r></f><v>f</v></d>"));
  auto group = MakeOutput(xml);
  const wxString unbroken = group->ToTeX();
  INFO("xml: " << xml);
  REQUIRE(BreakUpOutput(*group) == 1);
  REQUIRE(group->ToTeX() == unbroken);
}

SCENARIO("A broken cell's own TeX is that of the unbroken cell") {
  // What "Copy as LaTeX" uses: it calls ToTeX() on the selected cells, which
  // for a broken power used to be empty.
  auto group = MakeOutput(
    wxS("<e><r><s>%e</s></r><r><v>x</v><mo>+</mo><v>y</v></r></e>"));
  Cell *const power = group->GetOutput();
  REQUIRE(power != nullptr);
  const wxString unbroken = power->ToTeX();
  REQUIRE(power->BreakUp());
  REQUIRE(power->ToTeX() == unbroken);
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
  // A GroupCell's editor needs somewhere to keep the cursor and selection.
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
