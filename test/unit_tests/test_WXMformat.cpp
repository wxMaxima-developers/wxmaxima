// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Gemini CLI
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the Licence, or
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
#define WXM_FORMAT_TEST 1
#include "WXMformat.cpp"
#include "TestStubs.cpp"
#include <catch2/catch.hpp>
#include <vector>

using namespace Format;

SCENARIO("WXM parsing correctly handles markers and filters noise") {
  Configuration config;

  GIVEN("A WXM file with leading spaces on markers") {
    std::vector<wxString> lines = {
      WXMFirstLine,
      " /* [wxMaxima: input   start ] */",
      "1+1;",
      "  /* [wxMaxima: input   end   ] */"
    };

    WHEN("parsed") {
      auto tree = TreeFromWXM(lines, &config);
      THEN("it correctly identifies the input cell") {
        auto cells = OnList(tree.get());
        auto it = cells.begin();
        REQUIRE(it != cells.end());
        REQUIRE(it->GetGroupType() == GC_TYPE_CODE);
        REQUIRE(it->GetEditable()->GetValue() == "1+1;");
      }
    }
  }

  GIVEN("A WXM file with end-of-file mitigation comments") {
    std::vector<wxString> lines = {
      WXMFirstLine,
      "/* [wxMaxima: input   start ] */",
      "1+1;",
      "/* [wxMaxima: input   end   ] */",
      "",
      "/* Old versions of Maxima abort on loading files that end in a comment. */",
      "\"Created with wxMaxima 23.02.0-DevelopmentSnapshot\"$"
    };

    WHEN("parsed") {
      auto tree = TreeFromWXM(lines, &config);
      THEN("the mitigation noise is ignored") {
        auto cells = OnList(tree.get());
        auto it = cells.begin();
        REQUIRE(it != cells.end());
        REQUIRE(it->GetGroupType() == GC_TYPE_CODE);
        REQUIRE(it->GetEditable()->GetValue() == "1+1;");
        
        // Ensure NO second cell was created for the noise
        ++it;
        REQUIRE(it == cells.end());
      }
    }
  }

  GIVEN("A WXM file with multiple cells and empty lines between them") {
    std::vector<wxString> lines = {
      WXMFirstLine,
      "/* [wxMaxima: input   start ] */",
      "1+1;",
      "/* [wxMaxima: input   end   ] */",
      "",
      "/* [wxMaxima: input   start ] */",
      "2+2;",
      "/* [wxMaxima: input   end   ] */"
    };

    WHEN("parsed") {
      auto tree = TreeFromWXM(lines, &config);
      THEN("empty lines are skipped and not added to the previous cell") {
        auto cells = OnList(tree.get());
        auto it = cells.begin();
        REQUIRE(it != cells.end());
        REQUIRE(it->GetEditable()->GetValue() == "1+1;");
        
        ++it;
        REQUIRE(it != cells.end());
        REQUIRE(it->GetEditable()->GetValue() == "2+2;");
      }
    }
  }
}

int main(int argc, char* argv[]) {
  return Catch::Session().run(argc, argv);
}
