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
  Tests for wxm::SplitAtTextBreaks(): where a line of plain text Maxima sends
  may be broken into lines. GUI-free, so tested directly.
*/

#include "TextBreaks.h"

#include <vector>

// Not CATCH_CONFIG_MAIN: see test_SyntaxErrorLocation.cpp.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::SplitAtTextBreaks;

namespace {
std::vector<wxString> Pieces(std::initializer_list<const wchar_t *> list) {
  std::vector<wxString> result;
  for (const auto *s : list)
    result.emplace_back(s);
  return result;
}

wxString Joined(const std::vector<wxString> &pieces) {
  wxString result;
  for (const auto &piece : pieces)
    result += piece;
  return result;
}
} // namespace

SCENARIO("A line is split after every character a line may be broken after") {
  THEN("spaces, + - , ; . * / allow a break, and stay before it") {
    REQUIRE(SplitAtTextBreaks(wxS("a b+c-d,e;f.g*h/i")) ==
            Pieces({L"a ", L"b+", L"c-", L"d,", L"e;", L"f.", L"g*", L"h/", L"i"}));
  }
  THEN("a tab allows a break, too") {
    REQUIRE(SplitAtTextBreaks(wxS("a\tb")) == Pieces({L"a\t", L"b"}));
  }
  THEN("other characters don't") {
    REQUIRE(SplitAtTextBreaks(wxS("f(x)=[a:b]")) == Pieces({L"f(x)=[a:b]"}));
  }
}

SCENARIO("Some places a break character occurs don't allow a break") {
  THEN("a run of break characters stays together") {
    REQUIRE(SplitAtTextBreaks(wxS("a, b -- c")) ==
            Pieces({L"a, ", L"b -- ", L"c"}));
  }
  THEN("indentation stays with the word it indents") {
    REQUIRE(SplitAtTextBreaks(wxS("   indented text")) ==
            Pieces({L"   indented ", L"text"}));
  }
  THEN("a decimal point or a thousands separator doesn't tear a number apart") {
    REQUIRE(SplitAtTextBreaks(wxS("pi=3.14, n=1,000.")) ==
            Pieces({L"pi=3.14, ", L"n=1,000."}));
  }
  THEN("a dot that ends a sentence does allow a break") {
    REQUIRE(SplitAtTextBreaks(wxS("Done.Next")) == Pieces({L"Done.", L"Next"}));
  }
  THEN("a link is never broken") {
    REQUIRE(SplitAtTextBreaks(wxS("See https://wxmaxima-developers.github.io/a-b/c.html now.")) ==
            Pieces({L"See ", L"https://wxmaxima-developers.github.io/a-b/c.html ", L"now."}));
  }
}

SCENARIO("Splitting a line never changes its text") {
  const wxString line = GENERATE(
    wxString(wxS("")), wxString(wxS(" ")), wxString(wxS("   ")),
    wxString(wxS("word")), wxString(wxS("trailing space ")),
    wxString(wxS("-1")), wxString(wxS("..")),
    wxString(wxS("Warning: assigning 1 to a variable that is declared as integer.")),
    wxString(wxS("expt: undefined: 0 to a negative exponent.")));
  INFO(line);
  const auto pieces = SplitAtTextBreaks(line);
  REQUIRE(!pieces.empty());
  REQUIRE(Joined(pieces) == line);
  for (size_t i = 0; i + 1 < pieces.size(); i++)
    REQUIRE(!pieces[i].IsEmpty());
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
