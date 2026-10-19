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
  Tests wxm::WheelSteps, which turns the small wheel events a touchpad or a
  high-resolution wheel sends (wxGTK on Wayland) into whole steps, so scrolling
  over a selected animation doesn't flick through a frame per event.
*/

#include "WheelSteps.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::WheelSteps;

static constexpr int delta = 120;

TEST_CASE("A classic wheel notch is one step") {
  WheelSteps steps;
  CHECK(steps.Add(delta, delta) == 1);
  CHECK(steps.Add(delta, delta) == 1);
  CHECK(steps.Add(-delta, delta) == -1);
}

TEST_CASE("Small high-resolution events add up to one step") {
  WheelSteps steps;
  int total = 0;
  for (int i = 0; i < 9; ++i) {
    const int step = steps.Add(15, delta);
    CHECK(step == ((i == 7) ? 1 : 0));
    total += step;
  }
  CHECK(total == 1);
  // 9 * 15 = 135: 15 are left over, 105 more complete the next step.
  CHECK(steps.Add(104, delta) == 0);
  CHECK(steps.Add(1, delta) == 1);
}

TEST_CASE("Small negative events add up to one step backwards") {
  WheelSteps steps;
  CHECK(steps.Add(-60, delta) == 0);
  CHECK(steps.Add(-59, delta) == 0);
  CHECK(steps.Add(-1, delta) == -1);
}

TEST_CASE("A fast flick can complete several steps at once") {
  WheelSteps steps;
  CHECK(steps.Add(3 * delta + 10, delta) == 3);
  CHECK(steps.Add(delta - 10, delta) == 1);
}

TEST_CASE("Reversing direction drops the leftover from the old direction") {
  WheelSteps steps;
  CHECK(steps.Add(100, delta) == 0);
  // Without the reset this would need 220 to get to -1.
  CHECK(steps.Add(-100, delta) == 0);
  CHECK(steps.Add(-20, delta) == -1);
}

TEST_CASE("Zero rotation never makes a step") {
  WheelSteps steps;
  CHECK(steps.Add(0, delta) == 0);
  CHECK(steps.Add(119, delta) == 0);
  CHECK(steps.Add(0, delta) == 0);
  CHECK(steps.Add(1, delta) == 1);
}

TEST_CASE("Reset forgets a partial step") {
  WheelSteps steps;
  CHECK(steps.Add(110, delta) == 0);
  steps.Reset();
  CHECK(steps.Add(10, delta) == 0);
}

TEST_CASE("A nonsensical delta counts every event as one step") {
  WheelSteps steps;
  CHECK(steps.Add(5, 0) == 1);
  CHECK(steps.Add(-5, 0) == -1);
  CHECK(steps.Add(0, 0) == 0);
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
