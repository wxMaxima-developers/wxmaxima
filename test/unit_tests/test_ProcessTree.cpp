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
  Tests DescendantPids(), which finds the Lisp that maxima.bat started so an
  interrupt can reach it on MS Windows (GH #2289).
*/

#include "ProcessTree.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

SCENARIO("DescendantPids finds the Lisp below the process wxMaxima started") {
  GIVEN("wxMaxima -> maxima.bat (cmd) -> sbcl, next to unrelated processes") {
    const std::vector<ProcessEntry> processes = {
      {4, 0}, {100, 4}, {200, 100}, {300, 200}, {400, 200}, {500, 4}};
    WHEN("walking from the cmd.exe wxMaxima started") {
      const auto pids = DescendantPids(200, processes);
      THEN("the result is the root first, then its children") {
        REQUIRE(pids == std::vector<long>{200, 300, 400});
      }
    }
    WHEN("walking from a process without children") {
      THEN("only that process is returned") {
        REQUIRE(DescendantPids(300, processes) == std::vector<long>{300});
      }
    }
  }
  GIVEN("a deeper tree") {
    const std::vector<ProcessEntry> processes = {
      {12, 11}, {11, 10}, {13, 10}, {14, 12}};
    THEN("children come before grandchildren") {
      REQUIRE(DescendantPids(10, processes) == std::vector<long>{10, 11, 13, 12, 14});
    }
  }
  GIVEN("a snapshot in which reused process ids form a cycle") {
    // pid 10's parent died and its id was reused by 20's child: 10 -> 20 -> 10.
    const std::vector<ProcessEntry> processes = {{20, 10}, {10, 20}, {30, 20}};
    THEN("the walk terminates and reports every process once") {
      REQUIRE(DescendantPids(10, processes) == std::vector<long>{10, 20, 30});
    }
  }
  GIVEN("an invalid root") {
    const std::vector<ProcessEntry> processes = {{1, 0}, {2, 0}};
    THEN("nothing is returned, not the whole system") {
      REQUIRE(DescendantPids(0, processes).empty());
      REQUIRE(DescendantPids(-1, processes).empty());
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
