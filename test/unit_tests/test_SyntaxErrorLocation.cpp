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
  Tests for SyntaxErrorLocation: finding the spot in a code cell Maxima's
  "incorrect syntax" message points at (GH #289). GUI-free, so tested
  directly.

  The Maxima outputs below are copied from real Maxima runs: the ones with
  "Space" in them from Maxima 5.46, which spells blanks out that way, the
  others in the form current Maxima (nparse.lisp's mread-synerr) prints.
*/

#include "SyntaxErrorLocation.h"

// Not CATCH_CONFIG_MAIN: with _UNICODE defined (as wxWidgets does on Windows)
// Catch then defines wmain(), which MinGW's startup code does not look for --
// the link fails with "undefined symbol: WinMain". Every test here therefore
// supplies its own main(), at the end of this file.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::LocateSyntaxError;
using wxm::SyntaxErrorEcho;
using wxm::TextRange;

namespace {
//! The marked characters, for readable test failures
wxString Marked(const wxString &text, std::optional<TextRange> range) {
  if (!range)
    return wxS("<nothing>");
  return text.SubString(range->start, range->end - 1);
}

//! The range a statement covers in a cell, found by its text
TextRange StatementOf(const wxString &cell, const wxString &statement) {
  const auto start = static_cast<size_t>(cell.Find(statement));
  return TextRange{start, start + statement.Length()};
}
} // namespace

SCENARIO("The echoed line is read from Maxima's syntax error message") {
  GIVEN("a complete message") {
    const wxString output =
      wxS("\nincorrect syntax: , is not a prefix operator\nf(a,,\n   ^\n");
    THEN("the line above the caret is the echo") {
      REQUIRE(SyntaxErrorEcho(output) == wxString(wxS("f(a,,")));
    }
  }
  GIVEN("a message whose caret line has not arrived yet") {
    const wxString output =
      wxS("\nincorrect syntax: , is not a prefix operator\nf(a,,\n");
    THEN("there is no echo yet") {
      REQUIRE_FALSE(SyntaxErrorEcho(output).has_value());
    }
  }
  GIVEN("Windows line endings") {
    const wxString output =
      wxS("incorrect syntax: Missing )\r\nsin(x)+(2;\r\n        ^\r\n");
    THEN("the carriage returns are no part of the echo") {
      REQUIRE(SyntaxErrorEcho(output) == wxString(wxS("sin(x)+(2;")));
    }
  }
  GIVEN("two syntax errors in one reply") {
    const wxString output =
      wxS("incorrect syntax: , is not a prefix operator\nSpaceSpaceb:x,,\n     ^\n"
          "incorrect syntax: Too many )'s\nSpaceSpaceb)\n  ^\n");
    THEN("the first one counts: everything after it is Maxima recovering") {
      REQUIRE(SyntaxErrorEcho(output) == wxString(wxS("SpaceSpaceb:x,,")));
    }
  }
  GIVEN("a runtime error") {
    const wxString output =
      wxS("length: argument cannot be a symbol; found y\n"
          " -- an error. To debug this try: debugmode(true);\n");
    THEN("there is no echo") {
      REQUIRE_FALSE(SyntaxErrorEcho(output).has_value());
    }
  }
}

SCENARIO("A syntax error is located in its statement") {
  GIVEN("a doubled comma, as current Maxima reports it") {
    const wxString cell = wxS("a: 1$\nf(a,,b);\n");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: , is not a prefix operator\nf(a,,\n   ^\n"), cell,
      StatementOf(cell, wxS("f(a,,b);")));
    THEN("both commas are marked") { REQUIRE(Marked(cell, range) == wxS(",,")); }
  }
  GIVEN("a number Maxima had to read past to see where it ends") {
    const wxString cell = wxS("x: 2 34;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: 34 is not an infix operator\nx:Space2Space34;\n      ^\n"),
      cell, StatementOf(cell, cell));
    THEN("the whole number is marked, with the character after it") {
      REQUIRE(Marked(cell, range) == wxS("34;"));
    }
  }
  GIVEN("a missing closing parenthesis") {
    const wxString cell = wxS("sin(x)+(2;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: Missing )\nsin(x)+(2;\n        ^\n"), cell,
      StatementOf(cell, cell));
    THEN("the place the parenthesis is missing is marked") {
      REQUIRE(Marked(cell, range) == wxS("2;"));
    }
  }
  GIVEN("an error in the third line of a function definition (Maxima 5.46)") {
    const wxString cell = wxS("a: 1$\nf(x):=block(\n  [b],\n  b:x,,\n  b)$\nc: 3;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: , is not a prefix operator\nSpaceSpaceb:x,,\n     ^\n"),
      cell, StatementOf(cell, wxS("f(x):=block(\n  [b],\n  b:x,,\n  b)$")));
    THEN("the error is found in the line it is in") {
      REQUIRE(range.has_value());
      REQUIRE(Marked(cell, range) == wxS(",,"));
      REQUIRE(range->start == static_cast<size_t>(cell.Find(wxS(",,"))));
    }
  }
  GIVEN("the same text in an earlier line, but not at a line start") {
    const wxString cell = wxS("f(x):=g(  a,,\n  a,,\n  b)$");
    // The echo "  a,," starts its line. The same five characters also occur
    // earlier, in the middle of the first line, which a plain search would
    // find first.
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: , is not a prefix operator\n  a,,\n   ^\n"), cell,
      StatementOf(cell, cell));
    THEN("the occurrence that starts a line is chosen") {
      REQUIRE(range.has_value());
      REQUIRE(range->start == static_cast<size_t>(cell.Find(wxS("\n  a,,"))) + 4);
    }
  }
  GIVEN("an echo cut to Maxima's 20 characters") {
    const wxString cell =
      wxS("verylongvariablename1 + verylongvariablename2 ) ;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: Too many )'s\nylongvariablename2Space)\n                  ^\n"),
      cell, StatementOf(cell, cell));
    THEN("it is found in the middle of the line") {
      REQUIRE(Marked(cell, range) == wxS(" )"));
    }
  }
  GIVEN("a name that really contains the word Space") {
    const wxString cell = wxS("mySpace: 1 2;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: 2 is not an infix operator\nmySpace: 1 2;\n           ^\n"),
      cell, StatementOf(cell, cell));
    THEN("the echo is taken literally first") {
      REQUIRE(Marked(cell, range) == wxS("2;"));
    }
  }
  GIVEN("an echo that doesn't occur in the statement") {
    const wxString cell = wxS("a: 1$ b: 2 /* note */ 3;");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: 3 is not an infix operator\nb:Space2SpaceSpace3;\n       ^\n"),
      cell, StatementOf(cell, wxS("b: 2 /* note */ 3;")));
    THEN("nothing is located: the statement as a whole stays marked") {
      REQUIRE_FALSE(range.has_value());
    }
  }
  GIVEN("an echo that occurs only in a different statement") {
    const wxString cell = wxS("f(a,,b);\ng(c,,d);");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: , is not a prefix operator\ng(c,,\n   ^\n"), cell,
      StatementOf(cell, wxS("f(a,,b);")));
    THEN("nothing outside the failing statement is marked") {
      REQUIRE_FALSE(range.has_value());
    }
  }
  GIVEN("a statement range reaching past the end of the cell") {
    const wxString cell = wxS("f(a,,b)");
    const auto range = LocateSyntaxError(
      wxS("incorrect syntax: , is not a prefix operator\nf(a,,\n   ^\n"), cell,
      TextRange{0, 100});
    THEN("it is clipped to the cell") { REQUIRE(Marked(cell, range) == wxS(",,")); }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
