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
  The Matrix menu's wizard rules put each field where its label says (GH #647).

  "Generate Matrix from Expression" once asked for a width and a height and
  used the width as the number of rows. The fields are filled in the way
  GenWizPanel::UpdateOutput() does it; no GUI and no Maxima needed.
*/

#define CATCH_CONFIG_RUNNER
#include "wizards/MatrixWizardRules.h"
#include <catch2/catch.hpp>
#include <vector>

//! Fills #1#, #2#, ... of a wizard rule, the way GenWizPanel does.
static wxString Fill(wxString rule, const std::vector<wxString> &values) {
  for (std::size_t i = 0; i < values.size(); i++)
    rule.Replace(wxString::Format("#%i#", static_cast<int>(i + 1)), values[i]);
  return rule;
}

SCENARIO("Generate Matrix from Expression: rows are rows") {
  GIVEN("2 rows, 3 columns and the fields in the order the wizard shows them") {
    // Rule, row variable, column variable, rows, columns
    const wxString cmd = Fill(wxm::GenMatrixFromExpressionRule(),
                              {wxS("10*i+j"), wxS("i"), wxS("j"), wxS("2"), wxS("3")});
    THEN("the outer list -- the list of rows -- runs the row variable up to "
         "the number of rows, each row the column variable up to the columns") {
      CHECK(cmd == wxS("apply('matrix,makelist(makelist(10*i+j,j,1,3),i,1,2));"));
    }
    THEN("no field is left unfilled") {
      CHECK_FALSE(cmd.Contains(wxS("#")));
    }
  }
}

SCENARIO("Classic matrix operations take the matrix as their only field") {
  THEN("the matrix may be a variable name") {
    CHECK(Fill(wxm::MatrixFunctionRule(wxS("invert")), {wxS("M")}) ==
          wxS("invert(M);"));
  }
  THEN("or what used to be sent at once: the selection or %") {
    CHECK(Fill(wxm::MatrixFunctionRule(wxS("determinant")), {wxS("%")}) ==
          wxS("determinant(%);"));
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
