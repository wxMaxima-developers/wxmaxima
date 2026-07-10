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

  GIVEN("UUID mismatch prevents accidental matching when UUIDs are in use") {
    // The anchor cell shares a UUID, so the two files are recognised as using
    // a common UUID scheme. A second cell pair then has identical content but
    // different UUIDs and must therefore NOT be matched.
    std::vector<CellMatchData> s1 = {{"anchor", "anchor", GC_TYPE_CODE},
                                     {"u1", "content", GC_TYPE_CODE}};
    std::vector<CellMatchData> s2 = {{"anchor", "anchor", GC_TYPE_CODE},
                                     {"u2", "content", GC_TYPE_CODE}};

    WHEN("aligned") {
      auto alignment = Align2(s1, s2, 100); // 100% threshold
      THEN("the anchor matches but the differing-UUID cells do not") {
        REQUIRE(alignment.size() == 3);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
        REQUIRE(alignment[1] == std::make_pair(1, -1));
        REQUIRE(alignment[2] == std::make_pair(-1, 1));
      }
    }
  }

  GIVEN("No shared UUIDs falls back to content matching") {
    // Both files have UUIDs, but none in common - e.g. the UUIDs were stripped
    // by an old wxMaxima and later regenerated as unrelated ones. Matching must
    // then fall back to the cell contents.
    std::vector<CellMatchData> s1 = {{"old-1", "content A", GC_TYPE_CODE},
                                     {"old-2", "content B", GC_TYPE_CODE}};
    std::vector<CellMatchData> s2 = {{"new-1", "content A", GC_TYPE_CODE},
                                     {"new-2", "content B", GC_TYPE_CODE}};

    WHEN("aligned") {
      auto alignment = Align2(s1, s2, 0); // exact content match
      THEN("the cells are matched by their identical content") {
        REQUIRE(alignment.size() == 2);
        REQUIRE(alignment[0] == std::make_pair(0, 0));
        REQUIRE(alignment[1] == std::make_pair(1, 1));
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

//! The text of @p s the given ranges cover, ranges separated by '|'. Makes the
//! assertions below readable and independent of exact token tie-breaking.
static wxString CoveredText(const wxString &s, const std::vector<CharRange> &ranges) {
  wxString result;
  for (const auto &r : ranges) {
    if (!result.IsEmpty())
      result += wxS("|");
    result += s.SubString(r.first, r.second - 1);
  }
  return result;
}

SCENARIO("MergeRanges sorts, merges and drops empty ranges") {
  GIVEN("Overlapping, touching, empty and out-of-order ranges") {
    std::vector<CharRange> ranges = {{10, 12}, {3, 5}, {5, 7}, {8, 8}, {11, 15}};
    WHEN("merged") {
      auto merged = MergeRanges(ranges);
      THEN("touching/overlapping ranges are united and empty ones dropped") {
        REQUIRE(merged == std::vector<CharRange>{{3, 7}, {10, 15}});
      }
    }
  }
}

SCENARIO("InlineDiff finds the intra-cell ranges the other side lacks") {
  std::vector<CharRange> aOnly, bOnly;

  GIVEN("Identical texts") {
    InlineDiff(wxS("x : 1;\ny : 2;"), wxS("x : 1;\ny : 2;"), aOnly, bOnly);
    THEN("nothing is marked") {
      REQUIRE(aOnly.empty());
      REQUIRE(bOnly.empty());
    }
  }

  GIVEN("A single changed word") {
    wxString a = wxS("plot2d(sin(x), [x, 0, 10]);");
    wxString b = wxS("plot2d(cos(x), [x, 0, 10]);");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("exactly that word is marked on both sides") {
      REQUIRE(CoveredText(a, aOnly) == wxS("sin"));
      REQUIRE(CoveredText(b, bOnly) == wxS("cos"));
    }
  }

  GIVEN("A word inserted on one side") {
    wxString a = wxS("a + c");
    wxString b = wxS("a + b + c");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("only the inserted side gets a mark, one contiguous range") {
      REQUIRE(aOnly.empty());
      REQUIRE(bOnly.size() == 1);
      // Exactly which of the equal " + " runs the LCS keeps is a tie;
      // either way the marked text is the inserted word plus one " + ".
      wxString covered = CoveredText(b, bOnly);
      REQUIRE(covered.Length() == 4);
      REQUIRE(covered.Contains(wxS("b")));
      REQUIRE(covered.Contains(wxS("+")));
    }
  }

  GIVEN("A line deleted from a multi-line cell") {
    wxString a = wxS("a: 1;\nb: 2;\nc: 3;");
    wxString b = wxS("a: 1;\nc: 3;");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("the whole deleted line is marked, without its line break") {
      REQUIRE(CoveredText(a, aOnly) == wxS("b: 2;"));
      REQUIRE(bOnly.empty());
    }
  }

  GIVEN("One token changed inside one line of a multi-line cell") {
    wxString a = wxS("x: 1;\ny: 2;\nz: 3;");
    wxString b = wxS("x: 1;\ny: 42;\nz: 3;");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("only the changed number is marked") {
      REQUIRE(CoveredText(a, aOnly) == wxS("2"));
      REQUIRE(CoveredText(b, bOnly) == wxS("42"));
    }
  }

  GIVEN("Completely different texts") {
    wxString a = wxS("integrate(f(x), x);");
    wxString b = wxS("The quick brown fox");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("everything is marked on both sides") {
      REQUIRE(CoveredText(a, aOnly) == a);
      REQUIRE(CoveredText(b, bOnly) == b);
    }
  }

  GIVEN("A change in whitespace only") {
    wxString a = wxS("x:1;");
    wxString b = wxS("x : 1;");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("only the added spaces are marked") {
      REQUIRE(aOnly.empty());
      REQUIRE(CoveredText(b, bOnly) == wxS(" | "));
    }
  }

  GIVEN("Empty vs non-empty text") {
    wxString a = wxS("");
    wxString b = wxS("f(x) := x^2;");
    InlineDiff(a, b, aOnly, bOnly);
    THEN("the non-empty side is fully marked") {
      REQUIRE(aOnly.empty());
      REQUIRE(CoveredText(b, bOnly) == b);
    }
  }
}

int main(int argc, char* argv[]) {
  return Catch::Session().run(argc, argv);
}
