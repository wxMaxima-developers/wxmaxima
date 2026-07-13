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
  Tests for the pure LDB-recognition helpers (LdbSupport). No GUI, no Maxima:
  the detectors are fed captured stdout/stderr text.
*/

#define CATCH_CONFIG_RUNNER
#include "LdbSupport.cpp"
#include <catch2/catch.hpp>

using namespace LdbSupport;

SCENARIO("The sbcl LDB banner is recognized") {
  GIVEN("the text sbcl prints when it enters its low-level debugger") {
    // The real banner, as emitted on stderr.
    const wxString banner =
      wxS("Welcome to LDB, a low-level debugger for the Lisp runtime environment.\n"
          "ldb> ");
    THEN("the banner is detected") { REQUIRE(ContainsLdbBanner(banner)); }
    THEN("the trailing prompt is detected") { REQUIRE(EndsWithLdbPrompt(banner)); }
    THEN("it is recognized as LDB") { REQUIRE(LooksLikeLdb(banner)); }
  }
}

SCENARIO("A bare ldb prompt with trailing whitespace is recognized") {
  GIVEN("just the prompt with a trailing newline") {
    THEN("the prompt is detected despite trailing whitespace") {
      REQUIRE(EndsWithLdbPrompt(wxS("ldb> \n")));
      REQUIRE(EndsWithLdbPrompt(wxS("ldb>")));
      REQUIRE(LooksLikeLdb(wxS("ldb> ")));
    }
  }
}

SCENARIO("Ordinary Maxima output is not mistaken for LDB") {
  GIVEN("normal Maxima chatter and its own Lisp-debugger prompts") {
    // Maxima's OWN debugger prompts (MAXIMA> / (dbm:1)) arrive over the socket,
    // not here, and must never be confused with sbcl's LDB.
    THEN("none of it looks like LDB") {
      REQUIRE_FALSE(LooksLikeLdb(wxS("(%i1) ")));
      REQUIRE_FALSE(LooksLikeLdb(wxS("MAXIMA> ")));
      REQUIRE_FALSE(LooksLikeLdb(wxS("(dbm:1) ")));
      REQUIRE_FALSE(LooksLikeLdb(wxS("End of animation sequence")));
      REQUIRE_FALSE(LooksLikeLdb(wxS("a variable named ldb_prompt := 5")));
      REQUIRE_FALSE(LooksLikeLdb(wxEmptyString));
    }
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
