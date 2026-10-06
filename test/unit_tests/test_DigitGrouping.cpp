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
  Tests wxm::DigitGroupGaps(), which decides where a number displayed in the
  worksheet gets the small gaps between its digit groups (GH #192).
*/

#include "DigitGrouping.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
//! number as it would be drawn, with a space where each gap goes
wxString Grouped(const wxString &number, size_t minDigits = 5,
                 size_t groupSize = 3) {
  wxString result;
  const auto gaps = wxm::DigitGroupGaps(number, minDigits, groupSize);
  size_t gap = 0;
  for (size_t i = 0; i < number.Length(); i++) {
    if ((gap < gaps.size()) && (gaps[gap] == i)) {
      result += wxS(' ');
      gap++;
    }
    result += number[i];
  }
  return result;
}
} // namespace

SCENARIO("Integers are grouped from the right") {
  CHECK(Grouped(wxS("2111230496")) == wxS("2 111 230 496"));
  CHECK(Grouped(wxS("123456")) == wxS("123 456"));
  CHECK(Grouped(wxS("12345")) == wxS("12 345"));
}

SCENARIO("Numbers shorter than the minimum length are left alone") {
  CHECK(Grouped(wxS("2026")) == wxS("2026"));
  CHECK(Grouped(wxS("1023"), 4) == wxS("1 023"));
  CHECK(Grouped(wxS("12"), 2) == wxS("12"));
  CHECK(Grouped(wxS("1234"), 2) == wxS("1 234"));
}

SCENARIO("Signs don't count as digits and are never separated") {
  CHECK(Grouped(wxS("-1234567")) == wxS("-1 234 567"));
  CHECK(Grouped(wxString::FromUTF8("\xe2\x88\x92" "1234567")) ==
        wxString::FromUTF8("\xe2\x88\x92" "1 234 567"));
  CHECK(Grouped(wxS("-1234")) == wxS("-1234"));
}

SCENARIO("Fractions are grouped from the decimal point on") {
  CHECK(Grouped(wxS("3.14159265")) == wxS("3.141 592 65"));
  CHECK(Grouped(wxS("12345.6789")) == wxS("12 345.678 9"));
  CHECK(Grouped(wxS(".1234567")) == wxS(".123 456 7"));
  // Both parts together decide about the minimum length.
  CHECK(Grouped(wxS("1234.5")) == wxS("1 234.5"));
  CHECK(Grouped(wxS("12.34")) == wxS("12.34"));
}

SCENARIO("Exponents are never grouped") {
  CHECK(Grouped(wxS("6.02214076e23")) == wxS("6.022 140 76e23"));
  CHECK(Grouped(wxS("1.234567890123b-12345")) ==
        wxS("1.234 567 890 123b-12345"));
  CHECK(Grouped(wxS("1.0e-10")) == wxS("1.0e-10"));
}

SCENARIO("Things that only start like a number are left alone") {
  // A hexadecimal number, as Maxima prints it with obase > 10
  CHECK(Grouped(wxS("12345ABC")) == wxS("12345ABC"));
  CHECK(Grouped(wxS("inf")) == wxS("inf"));
  CHECK(Grouped(wxS("")) == wxS(""));
  CHECK(Grouped(wxS("-")) == wxS("-"));
}

SCENARIO("The group size can be 4") {
  CHECK(Grouped(wxS("123456789"), 5, 4) == wxS("1 2345 6789"));
  CHECK(Grouped(wxS("0.123456789"), 5, 4) == wxS("0.1234 5678 9"));
}

SCENARIO("The locale's group size is 3 or 4") {
  const int size = wxm::LocaleDigitGroupSize();
  CHECK(((size == 3) || (size == 4)));
}

namespace {
//! DigitGroupGapsInTokens() for these tokens, numbers being the ones that
//! start with a digit; the gaps shown as spaces, tokens separated by "|"
wxString GroupedTokens(const std::vector<wxString> &texts, size_t minDigits = 5) {
  std::vector<std::pair<wxString, bool>> tokens;
  for (const auto &text : texts)
    tokens.emplace_back(text, !text.IsEmpty() && (text[0] >= wxS('0')) && (text[0] <= wxS('9')));
  const auto gaps = wxm::DigitGroupGapsInTokens(tokens, minDigits);
  REQUIRE(gaps.size() == texts.size());
  wxString result;
  for (size_t t = 0; t < texts.size(); t++) {
    if (t > 0)
      result += wxS("|");
    size_t gap = 0;
    for (size_t i = 0; i < texts[t].Length(); i++) {
      if ((gap < gaps[t].size()) && (gaps[t][gap] == i)) {
        result += wxS(' ');
        gap++;
      }
      result += texts[t][i];
    }
  }
  return result;
}
} // namespace

SCENARIO("A number the tokenizer split at its decimal point is grouped as one") {
  CHECK(GroupedTokens({wxS("x"), wxS(":"), wxS("3"), wxS("."), wxS("14159265")}) ==
        wxS("x|:|3|.|141 592 65"));
  CHECK(GroupedTokens({wxS("12345"), wxS("."), wxS("6789")}) ==
        wxS("12 345|.|678 9"));
  CHECK(GroupedTokens({wxS("."), wxS("1234567")}) == wxS(".|123 456 7"));
  // Both parts count towards the minimum length
  CHECK(GroupedTokens({wxS("1234"), wxS("."), wxS("5")}) == wxS("1 234|.|5"));
  CHECK(GroupedTokens({wxS("12"), wxS("."), wxS("34")}) == wxS("12|.|34"));
  CHECK(GroupedTokens({wxS("1"), wxS("."), wxS("23456789e-10")}) ==
        wxS("1|.|234 567 89e-10"));
}

SCENARIO("Number tokens that don't form one number are grouped separately") {
  // An exponent can't be followed by a fraction
  CHECK(GroupedTokens({wxS("5e10"), wxS("."), wxS("1234567")}) ==
        wxS("5e10|.|1 234 567"));
  CHECK(GroupedTokens({wxS("1234567"), wxS("+"), wxS("7654321")}) ==
        wxS("1 234 567|+|7 654 321"));
  CHECK(GroupedTokens({wxS("x"), wxS("1234567")}) == wxS("x|1 234 567"));
  CHECK(GroupedTokens({}) == wxS(""));
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
