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

// ComputeWorksheetContentHeight() replays GetMaxPoint's backward walk over the
// trailing cells. `trailing` runs from the last cell backward; the anchor is the
// first entry with sizeIsStale && currentY >= 0.

SCENARIO("The content height is pinned to the anchor cell's top") {
  GIVEN("a single anchor cell (stale size but a valid position)") {
    const std::vector<TrailingGroupGeometry> trailing = {{true, 1000, 30}};
    THEN("the height is its top plus its drop, no extra") {
      REQUIRE(ComputeWorksheetContentHeight(trailing, 10, 5) == 1030);
    }
  }
  GIVEN("cells below the anchor that are stale-and-unpositioned") {
    // last cell (top), ..., anchor (bottom of the vector).
    const std::vector<TrailingGroupGeometry> trailing = {
      {true, -1, 40}, // stale, y<0: adds maxDrop + groupSkip = 50
      {true, -1, 20}, // stale, y<0: adds maxDrop + groupSkip = 30
      {true, 500, 15} // anchor: 500 + 15
    };
    THEN("their extents accumulate onto the anchor's top+drop") {
      REQUIRE(ComputeWorksheetContentHeight(trailing, 10, 5) ==
              500 + 15 + 50 + 30);
    }
  }
}

SCENARIO("Non-stale but unpositioned trailing cells reserve a nominal height") {
  GIVEN("a not-yet-positioned non-stale cell above the anchor") {
    const std::vector<TrailingGroupGeometry> trailing = {
      {false, -1, 999}, // not stale: adds groupSkip + 20 = 30 (maxDrop ignored)
      {true, 200, 10}   // anchor
    };
    THEN("it reserves groupSkip + 20 regardless of its drop") {
      REQUIRE(ComputeWorksheetContentHeight(trailing, 10, 5) == 200 + 10 + 30);
    }
  }
}

SCENARIO("Without an anchor the height falls back to the base indent") {
  GIVEN("a walk that ran off the top with no anchor found") {
    const std::vector<TrailingGroupGeometry> trailing = {
      {true, -1, 40}, // adds 40 + groupSkip(10) = 50
      {false, -1, 0}  // adds groupSkip(10) + 20 = 30
    };
    THEN("the height is baseIndent plus the accumulated extents") {
      REQUIRE(ComputeWorksheetContentHeight(trailing, 10, 7) == 7 + 50 + 30);
    }
  }
  GIVEN("no trailing cells at all") {
    THEN("the height is just the base indent") {
      REQUIRE(ComputeWorksheetContentHeight({}, 10, 42) == 42);
    }
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
