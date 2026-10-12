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
  Which words of a wizard's command get a "Help on" button.

  The Solve ODE wizard used to offer help on ";" only: "ode2" contains a
  digit and wasn't recognized as a word, and the statement terminator every
  wizard's command ends with was offered instead. No GUI, no manual needed.
*/

#define CATCH_CONFIG_RUNNER
#include "wizards/WizardHelpKeywords.cpp"
#include <catch2/catch.hpp>

using wxm::WizardHelpKeywords;
using Keywords = std::vector<wxString>;

SCENARIO("Identifiers containing digits are recognized") {
  THEN("the Solve ODE wizard offers ode2 and nothing else") {
    CHECK(WizardHelpKeywords(wxS("ode2(#1#,#2#,#3#);")) == Keywords{wxS("ode2")});
  }
  THEN("an identifier may contain underscores and start with %") {
    CHECK(WizardHelpKeywords(wxS("find_root(%e^x,x,0,1)$")) ==
          (Keywords{wxS("find_root"), wxS("%e"), wxS("x")}));
  }
}

SCENARIO("The placeholders are not keywords") {
  THEN("neither their digits nor their # are offered") {
    CHECK(WizardHelpKeywords(wxS("#1##10#")).empty());
    CHECK(WizardHelpKeywords(wxS("a#2#;")) == Keywords{wxS("a")});
  }
  THEN("a # that is Maxima's operator still is") {
    CHECK(WizardHelpKeywords(wxS("#1# # #2#;")) == Keywords{wxS("#")});
  }
}

SCENARIO("The statement terminator at the end is not offered") {
  THEN("neither ; nor $ at the end of the command") {
    CHECK(WizardHelpKeywords(wxS("f(#1#);")) == Keywords{wxS("f")});
    CHECK(WizardHelpKeywords(wxS("f(#1#)$  ")) == Keywords{wxS("f")});
  }
  THEN("but one that separates two statements is") {
    CHECK(WizardHelpKeywords(wxS("a:#1#$ b(a);")) ==
          (Keywords{wxS("a"), wxS("b"), wxS("$")}));
  }
}

SCENARIO("Operators and repeated words") {
  THEN("each keyword is listed once, in the order it first appears") {
    CHECK(WizardHelpKeywords(wxS("''(f(x)) + 'g(x) + n!;")) ==
          (Keywords{wxS("f"), wxS("x"), wxS("g"), wxS("n"), wxS("''"),
                    wxS("'"), wxS("!")}));
  }
  THEN("an empty command has no keywords") {
    CHECK(WizardHelpKeywords(wxEmptyString).empty());
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
