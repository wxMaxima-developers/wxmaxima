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
  Tests wxm::PackagePrefixes(), where on macOS Maxima and gnuplot are looked for (GH #1347).
*/

#include "PackagePrefixes.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::PackagePrefixes;

SCENARIO("The prefix wxMaxima was installed to is searched first") {
  GIVEN("MacPorts building wxMaxima with a non-default prefix") {
    auto prefixes = PackagePrefixes(wxS("/opt/macports"));
    THEN("that prefix comes first, followed by the well-known ones") {
      REQUIRE(prefixes.size() == 4);
      CHECK(prefixes[0] == wxS("/opt/macports"));
      CHECK(prefixes[1] == wxS("/opt/homebrew"));
      CHECK(prefixes[2] == wxS("/usr/local"));
      CHECK(prefixes[3] == wxS("/opt/local"));
    }
  }
  GIVEN("MacPorts' default prefix") {
    auto prefixes = PackagePrefixes(wxS("/opt/local"));
    THEN("it is preferred over Homebrew, as OSX_MACPORTS_PREFER used to do, and not repeated") {
      REQUIRE(prefixes.size() == 3);
      CHECK(prefixes[0] == wxS("/opt/local"));
      CHECK(prefixes[1] == wxS("/opt/homebrew"));
      CHECK(prefixes[2] == wxS("/usr/local"));
    }
  }
  GIVEN("CMake's default prefix, with a trailing slash") {
    auto prefixes = PackagePrefixes(wxS("/usr/local/"));
    THEN("it is recognised as Homebrew's Intel prefix") {
      REQUIRE(prefixes.size() == 3);
      CHECK(prefixes[0] == wxS("/usr/local"));
      CHECK(prefixes[1] == wxS("/opt/homebrew"));
      CHECK(prefixes[2] == wxS("/opt/local"));
    }
  }
  GIVEN("No install prefix") {
    auto prefixes = PackagePrefixes(wxEmptyString);
    THEN("only the well-known prefixes are searched") {
      REQUIRE(prefixes.size() == 3);
      CHECK(prefixes[0] == wxS("/opt/homebrew"));
    }
  }
  GIVEN("A prefix containing characters that need quoting in C") {
    // BuildConfig.h hands CMAKE_INSTALL_PREFIX over as a raw string literal,
    // so a quote or backslash in it reaches this function as it is.
    auto prefixes = PackagePrefixes(wxS("/Users/a \"b\"\\c"));
    THEN("it is kept as it is") {
      CHECK(prefixes[0] == wxS("/Users/a \"b\"\\c"));
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
