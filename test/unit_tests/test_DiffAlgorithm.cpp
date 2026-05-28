// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Gemini CLI
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

#define CATCH_CONFIG_RUNNER
#include "DiffAlgorithm.cpp"
#include "levenshtein/levenshtein.cpp"
#include <catch2/catch.hpp>

using namespace Diff;

SCENARIO("Diff alignment correctly identifies matches and gaps") {
  GIVEN("Two identical sequences") {
    std::vector<CellMatchData> s1 = {
      {"uuid1", "content1", GC_TYPE_CODE},
      {"", "content2", GC_TYPE_TEXT}
    };
    std::vector<CellMatchData> s2 = s1;

    WHEN("aligned") {
      auto alignment = Align2(s1, s2);
      THEN("all cells are matched 1:1") {
        REQUIRE(alignment.size() == 2);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
        REQUIRE(alignment[1] == std::make_pair(1, 1));
      }
    }
  }

  GIVEN("Sequences with an insertion") {
    std::vector<CellMatchData> s1 = {
      {"u1", "c1", GC_TYPE_CODE},
      {"u3", "c3", GC_TYPE_CODE}
    };
    std::vector<CellMatchData> s2 = {
      {"u1", "c1", GC_TYPE_CODE},
      {"u2", "c2", GC_TYPE_CODE}, // inserted
      {"u3", "c3", GC_TYPE_CODE}
    };

    WHEN("aligned") {
      auto alignment = Align2(s1, s2);
      THEN("the gap is correctly identified in s1") {
        REQUIRE(alignment.size() == 3);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
        REQUIRE(alignment[1] == std::make_pair(-1, 1));
        REQUIRE(alignment[2] == std::make_pair(1, 2));
      }
    }
  }

  GIVEN("Sequences with a deletion") {
    std::vector<CellMatchData> s1 = {
      {"u1", "c1", GC_TYPE_CODE},
      {"u2", "c2", GC_TYPE_CODE},
      {"u3", "c3", GC_TYPE_CODE}
    };
    std::vector<CellMatchData> s2 = {
      {"u1", "c1", GC_TYPE_CODE},
      {"u3", "c3", GC_TYPE_CODE}
    };

    WHEN("aligned") {
      auto alignment = Align2(s1, s2);
      THEN("the gap is correctly identified in s2") {
        REQUIRE(alignment.size() == 3);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
        REQUIRE(alignment[1] == std::make_pair(1, -1));
        REQUIRE(alignment[2] == std::make_pair(2, 1));
      }
    }
  }

  GIVEN("Fuzzy matches within threshold") {
    std::vector<CellMatchData> s1 = {{"", "The quick brown fox", GC_TYPE_TEXT}};
    std::vector<CellMatchData> s2 = {{"", "The quick brown fix", GC_TYPE_TEXT}}; // 1 char diff

    WHEN("aligned with 20% threshold") {
      auto alignment = Align2(s1, s2, 20);
      THEN("they are matched") {
        REQUIRE(alignment.size() == 1);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
      }
    }
  }

  GIVEN("Differences beyond fuzzy threshold") {
    std::vector<CellMatchData> s1 = {{"", "The quick brown fox", GC_TYPE_TEXT}};
    std::vector<CellMatchData> s2 = {{"", "Jumped over the lazy dog", GC_TYPE_TEXT}};

    WHEN("aligned with 20% threshold") {
      auto alignment = Align2(s1, s2, 20);
      THEN("they are not matched") {
        REQUIRE(alignment.size() == 2);
        REQUIRE(alignment[0] == std::make_pair(0, -1));
        REQUIRE(alignment[1] == std::make_pair(-1, 0));
      }
    }
  }

  GIVEN("UUID priority overrides content difference") {
    std::vector<CellMatchData> s1 = {{"uuid-a", "Very different content", GC_TYPE_CODE}};
    std::vector<CellMatchData> s2 = {{"uuid-a", "Something else entirely", GC_TYPE_CODE}};

    WHEN("aligned") {
      auto alignment = Align2(s1, s2, 0); // 0 threshold
      THEN("they are matched due to same UUID") {
        REQUIRE(alignment.size() == 1);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
      }
    }
  }

  GIVEN("UUID mismatch prevents accidental matching") {
    std::vector<CellMatchData> s1 = {{"u1", "content", GC_TYPE_CODE}};
    std::vector<CellMatchData> s2 = {{"u2", "content", GC_TYPE_CODE}};

    WHEN("aligned") {
      auto alignment = Align2(s1, s2, 100); // 100% threshold
      THEN("they are NOT matched due to different UUIDs") {
        REQUIRE(alignment.size() == 2);
        REQUIRE(alignment[0] == std::make_pair(0, -1));
        REQUIRE(alignment[1] == std::make_pair(-1, 0));
      }
    }
  }

  GIVEN("Asymmetric UUID availability") {
    std::vector<CellMatchData> s1 = {{"u1", "content", GC_TYPE_CODE}};
    std::vector<CellMatchData> s2 = {{"", "content", GC_TYPE_CODE}};

    WHEN("aligned") {
      auto alignment = Align2(s1, s2, 100);
      THEN("they should be matched via content") {
        REQUIRE(alignment.size() == 1);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
      }
    }
  }
}

int main(int argc, char* argv[]) {
  return Catch::Session().run(argc, argv);
}
