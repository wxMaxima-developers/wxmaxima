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
  Regression tests for GH #898: an identifier split by an escaped newline
  ("\<newline>") used to be tokenized (and therefore syntax-highlighted) as
  two unrelated halves -- the first not even classified at all (it fell
  through the function/variable/operator/keyword lookups entirely), the
  second starting over as if it were a brand new, independent identifier.

  Confirmed against a real Maxima that an escaped newline contributes
  nothing to the resolved symbol name at all (fo\<newline>obar resolves to
  the plain identifier foobar), unlike an ordinary escaped character
  (a\,b resolves to the symbol a,b -- the backslash drops but the escaped
  character stays part of the name). The fix keeps collecting one logical
  name across any number of escaped newlines for classification, while
  still emitting a newline as its own isolated token when rendering (the
  editor's line splitting depends on that -- see "Tab Characters in
  EditorCell" in AGENTS.md).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MaximaTokenizer.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
} // namespace

static MaximaTokenizer::TokenList Tokenize(const wxString &commands) {
  return MaximaTokenizer(commands, g_cfg).PopTokens();
}

SCENARIO("A plain identifier (no escapes) tokenizes as a single styled token") {
  auto tokens = Tokenize(wxS("foobar:"));
  REQUIRE(tokens.size() >= 1);
  REQUIRE(tokens[0].GetText() == wxS("foobar"));
  REQUIRE(tokens[0].GetTextStyle() == TS_CODE_VARIABLE);
}

SCENARIO("An identifier split by an escaped newline stays one logical, consistently-styled name") {
  // "fo\<newline>obar: 42;" -- confirmed against a real Maxima to resolve to
  // the plain identifier foobar.
  auto tokens = Tokenize(wxS("fo\\\nobar:"));

  THEN("it is rendered as two segments joined by an isolated newline token") {
    REQUIRE(tokens.size() >= 3);
    REQUIRE(tokens[0].GetText() == wxS("fo\\"));
    REQUIRE(tokens[1].GetText() == wxS("\n"));
    REQUIRE(tokens[2].GetText() == wxS("obar"));
  }
  THEN("both halves share the same style, resolved from the whole name") {
    // Before the fix, the first half fell through every classification
    // lookup untouched (default style) while the second half was
    // classified independently -- here both must agree, and match what a
    // plain "foobar:" would get (TS_CODE_VARIABLE, see the scenario above).
    REQUIRE(tokens[0].GetTextStyle() == TS_CODE_VARIABLE);
    REQUIRE(tokens[2].GetTextStyle() == TS_CODE_VARIABLE);
  }
}

SCENARIO("A hardcoded keyword split by an escaped newline is still recognized as one") {
  // "th\<newline>en" resolves to the plain keyword "then".
  auto tokens = Tokenize(wxS("th\\\nen"));
  REQUIRE(tokens.size() >= 3);
  REQUIRE(tokens[0].GetText() == wxS("th\\"));
  REQUIRE(tokens[1].GetText() == wxS("\n"));
  REQUIRE(tokens[2].GetText() == wxS("en"));
  REQUIRE(tokens[0].GetTextStyle() == TS_CODE_FUNCTION);
  REQUIRE(tokens[2].GetTextStyle() == TS_CODE_FUNCTION);
}

SCENARIO("A function name split by an escaped newline is still recognized as a function call") {
  // Classification peeks at the character right after the whole name ends;
  // an escaped newline must not confuse that either.
  auto tokens = Tokenize(wxS("fo\\\nobar(x)"));
  REQUIRE(tokens.size() >= 3);
  REQUIRE(tokens[0].GetText() == wxS("fo\\"));
  REQUIRE(tokens[1].GetText() == wxS("\n"));
  REQUIRE(tokens[2].GetText() == wxS("obar"));
  REQUIRE(tokens[0].GetTextStyle() == TS_CODE_FUNCTION);
  REQUIRE(tokens[2].GetTextStyle() == TS_CODE_FUNCTION);
}

SCENARIO("An ordinary escaped character in an identifier is unaffected by the fix") {
  // a\,b resolves to the symbol a,b (confirmed against a real Maxima) and
  // has always rendered with the backslash still visible, as a single,
  // unsplit token -- this scenario pins that this still works exactly as
  // before the GH #898 fix.
  auto tokens = Tokenize(wxS("a\\,b:"));
  REQUIRE(tokens.size() >= 1);
  REQUIRE(tokens[0].GetText() == wxS("a\\,b"));
  REQUIRE(tokens[0].GetTextStyle() == TS_CODE_VARIABLE);
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
  wxInitAllImageHandlers();

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
