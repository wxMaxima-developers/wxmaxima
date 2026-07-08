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
  Unit tests for the worksheet's virtual-size arithmetic
  (ComputeWorksheetVirtualSize). The math is GUI-free so it can be exercised
  directly, unlike Worksheet::AdjustSize() which reads the live window size.
  These pin the scroll-range contract that "the pane won't scroll" bugs violate.
*/

#define CATCH_CONFIG_RUNNER
#include "WorksheetSizeMath.h"
#include <catch2/catch.hpp>

SCENARIO("A tall document gets a scroll range proportional to its height") {
  GIVEN("a document far taller than the window") {
    const int clientHeight = 800;
    const WorksheetVirtualSize vs =
      ComputeWorksheetVirtualSize(true, 500, 5000, clientHeight, 0);
    THEN("the virtual height exceeds the client height, so there is room to "
         "scroll") {
      REQUIRE(vs.height > clientHeight);
      // document height + the deliberate over-scroll (clientHeight - 1/8).
      REQUIRE(vs.height == 5000 + 800 - 100);
      REQUIRE(vs.width == 500);
    }
  }
}

SCENARIO("A short document still keeps a vertical scrollbar active") {
  GIVEN("a document shorter than the window") {
    const int clientHeight = 800;
    const WorksheetVirtualSize vs =
      ComputeWorksheetVirtualSize(true, 300, 50, clientHeight, 0);
    THEN("the virtual height is at least clientHeight + 10") {
      REQUIRE(vs.height >= clientHeight + 10);
    }
  }
}

SCENARIO("The virtual size never shrinks below the current scroll position") {
  GIVEN("the view scrolled far down while the measured document is tiny "
        "(as happens with stale positions mid-recalculation)") {
    const int clientHeight = 800;
    const int scrollY = 4000;
    const WorksheetVirtualSize vs =
      ComputeWorksheetVirtualSize(true, 300, 0, clientHeight, scrollY);
    THEN("the virtual height still covers the scrolled-to region, so the view "
         "does not get clamped and jump") {
      REQUIRE(vs.height >= scrollY + clientHeight + 10);
    }
  }
}

SCENARIO("An empty worksheet gets a small fixed size and a sane scroll unit") {
  GIVEN("no cell tree") {
    const WorksheetVirtualSize vs =
      ComputeWorksheetVirtualSize(false, 12345, 67890, 800, 0);
    THEN("the measured extents are ignored") {
      REQUIRE(vs.width == 40);
      REQUIRE(vs.height == 40);
      REQUIRE(vs.scrollUnit == 10);
    }
  }
}

SCENARIO("The scroll unit scales with the window but never drops below 10") {
  THEN("a normal window scrolls by clientHeight/30") {
    REQUIRE(ComputeWorksheetVirtualSize(true, 100, 100, 900, 0).scrollUnit == 30);
  }
  THEN("a tiny window is clamped to 10") {
    REQUIRE(ComputeWorksheetVirtualSize(true, 100, 100, 60, 0).scrollUnit == 10);
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
