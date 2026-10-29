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
  Pins the LaTeX export of Greek constants: every one of them must become a
  command LaTeX actually knows, or a plain letter where LaTeX has no command
  for an uppercase Greek letter that looks like a Latin one. %xi and %Xi used
  to export as the nonexistent "\ui", and e.g. %Epsilon as "\Epsilon".

  The cells are parsed from the <g> tags wxMathML.lisp emits for these
  constants -- no live Maxima needed.
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

wxString GreekToTeX(const wxString &name) {
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxS("<mth><g>%") + name + wxS("</g></mth>"));
  REQUIRE(output != nullptr);
  return output->ListToTeX();
}
} // namespace

SCENARIO("%xi and %Xi export as \\xi and \\Xi") {
  CHECK(GreekToTeX(wxS("xi")).Contains(wxS("\\xi")));
  CHECK(GreekToTeX(wxS("Xi")).Contains(wxS("\\Xi")));
  CHECK_FALSE(GreekToTeX(wxS("xi")).Contains(wxS("\\ui")));
  CHECK_FALSE(GreekToTeX(wxS("Xi")).Contains(wxS("\\ui")));
}

SCENARIO("Greek letters LaTeX has no command for export as Latin letters") {
  const std::pair<const char *, const char *> letters[] = {
    {"omicron", "o"}, {"Alpha", "A"}, {"Beta", "B"}, {"Epsilon", "E"},
    {"Zeta", "Z"},    {"Eta", "H"},   {"Iota", "I"}, {"Kappa", "K"},
    {"Mu", "M"},      {"Nu", "N"},    {"Omicron", "O"}, {"Rho", "P"},
    {"Tau", "T"},     {"Chi", "X"}};
  for (const auto &[name, letter] : letters) {
    INFO("%" << name);
    const wxString tex = GreekToTeX(wxString::FromUTF8(name));
    CHECK_FALSE(tex.Contains(wxS("\\") + wxString::FromUTF8(name)));
    CHECK(tex.Contains(wxString::FromUTF8(letter)));
  }
}

SCENARIO("Greek letters LaTeX knows export as their command") {
  const char *const commands[] = {
    "alpha", "beta",  "gamma", "delta", "epsilon", "zeta",  "eta",
    "theta", "iota",  "kappa", "lambda", "mu",     "nu",    "pi",
    "rho",   "sigma", "tau",   "upsilon", "phi",   "chi",   "psi",
    "omega", "Gamma", "Delta", "Theta", "Lambda",  "Pi",    "Sigma",
    "Upsilon", "Phi", "Psi",   "Omega"};
  for (const char *name : commands) {
    INFO("%" << name);
    CHECK(GreekToTeX(wxString::FromUTF8(name))
            .Contains(wxS("\\") + wxString::FromUTF8(name)));
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
