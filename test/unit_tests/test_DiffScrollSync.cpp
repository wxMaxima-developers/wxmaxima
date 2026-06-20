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
  Unit tests for the diff viewer's scroll-synchronization geometry
  (ComputeSyncedScrollY). The math is GUI-free so it can be exercised directly,
  unlike the live scroll events it normally consumes.
*/

#define CATCH_CONFIG_RUNNER
#include "dialogs/DiffScrollSync.cpp"
#include <catch2/catch.hpp>

// Two files whose cells line up 1:1, every cell 100 px tall starting at y=0.
static const std::vector<int> kTops = {0, 100, 200, 300, 400};

SCENARIO("ComputeSyncedScrollY keeps identical panes locked together") {
  GIVEN("two identical panes") {
    THEN("the other pane mirrors the source position exactly, both ways") {
      // Scrolling down: viewport top at 150 -> anchor is the cell at 200, 50px
      // below the top; the matching cell is also at 200, so the other pane lands
      // at 150 too.
      REQUIRE(ComputeSyncedScrollY(kTops, kTops, 150) == 150);
      // Scrolling up to 50: anchor is the cell at 100, 50px below the top.
      REQUIRE(ComputeSyncedScrollY(kTops, kTops, 50) == 50);
      // Exactly on a cell top.
      REQUIRE(ComputeSyncedScrollY(kTops, kTops, 200) == 200);
    }
  }
}

SCENARIO("ComputeSyncedScrollY aligns panes whose cells differ in size") {
  GIVEN("an other pane whose cells are taller (so its tops drift down)") {
    // Same diff entries, but the other worksheet's cells are 130 px tall.
    const std::vector<int> other = {0, 130, 260, 390, 520};

    WHEN("the source scrolls down to 150") {
      // Anchor = source cell at 200 (offset 50 below top). Other anchor cell is
      // at 260, so the other pane must show it 50px below its top -> 210.
      THEN("the other pane lines that diff entry up at the same screen offset")
        REQUIRE(ComputeSyncedScrollY(kTops, other, 150) == 210);
    }
    WHEN("the source scrolls up to 50") {
      // Anchor = source cell at 100 (offset 50). Other anchor at 130 -> 80.
      THEN("the same top-anchored rule handles upward scrolling")
        REQUIRE(ComputeSyncedScrollY(kTops, other, 50) == 80);
    }
  }
}

SCENARIO("ComputeSyncedScrollY reports 'no anchor' when it cannot align") {
  GIVEN("a viewport scrolled past the last cell") {
    // No source cell starts at or below y=1000.
    THEN("there is no anchor")
      REQUIRE(ComputeSyncedScrollY(kTops, kTops, 1000) == DIFFSYNC_NO_CELL);
  }

  GIVEN("an anchor entry that only exists in the source pane") {
    // The other pane is missing the cell for the entry the anchor would pick
    // (the entry at index 2, source top 200).
    std::vector<int> other = {0, 100, DIFFSYNC_NO_CELL, 300, 400};
    THEN("no scroll target is produced")
      REQUIRE(ComputeSyncedScrollY(kTops, other, 150) == DIFFSYNC_NO_CELL);
  }
}

SCENARIO("ComputeSyncedScrollY never returns a negative scroll position") {
  GIVEN("an anchor far below the source viewport top but near the other's top") {
    // Source viewport at 300 -> anchor is the cell at 500 (200px below the top).
    // The matching cell in the other pane is only at 10, so reproducing the
    // 200px offset would put the other viewport at -190.
    const std::vector<int> src = {0, 500};
    const std::vector<int> other = {0, 10};
    THEN("the result is clamped to 0")
      REQUIRE(ComputeSyncedScrollY(src, other, 300) == 0);
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
