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
  Checks every entry of the catalog of standard wizards.

  A wizard's rule refers to its fields by number (#1#, #2#, ...), so a field
  added or dropped without updating the rule silently leaves a "#3#" in the
  command, or a field the command ignores. Nothing else notices: the wizard
  opens and looks fine. No GUI and no Maxima needed.
*/

#define CATCH_CONFIG_RUNNER
#include "EventIDs.cpp"
#include "wizards/WizardCatalog.cpp"
#include "wizards/WizardHelpKeywords.cpp"
#include <catch2/catch.hpp>
#include <wx/init.h>
#include <wx/regex.h>
#include <set>

using wxm::WizardCatalog;
using wxm::WizardContext;
using wxm::WizardSpec;

namespace {
//! The numbers n of all #n# placeholders in \p rule
std::set<long> Placeholders(const wxString &rule) {
  std::set<long> numbers;
  wxRegEx placeholder(wxS("#([0-9]+)#"));
  wxString rest = rule;
  while (placeholder.Matches(rest)) {
    long n = 0;
    placeholder.GetMatch(rest, 1).ToLong(&n);
    numbers.insert(n);
    size_t start = 0, len = 0;
    placeholder.GetMatch(&start, &len, 0);
    rest = rest.Mid(start + len);
  }
  return numbers;
}
}

SCENARIO("Every wizard's rule uses exactly the fields it has") {
  for (const WizardSpec &spec : WizardCatalog()) {
    INFO("Wizard: " << spec.title.utf8_string() << ", rule: "
         << spec.rule.utf8_string());
    // GenWizPanel has 10 fields.
    REQUIRE(!spec.fields.empty());
    REQUIRE(spec.fields.size() <= 10);
    std::set<long> expected;
    for (long n = 1; n <= static_cast<long>(spec.fields.size()); n++)
      expected.insert(n);
    CHECK(Placeholders(spec.rule) == expected);
  }
}

SCENARIO("Every wizard can be found, and opens with a usable command") {
  for (const WizardSpec &spec : WizardCatalog()) {
    INFO("Wizard: " << spec.title.utf8_string());
    CHECK(!spec.title.IsEmpty());
    CHECK(!spec.ids.empty());
    // A command that doesn't end in a terminator would sit in the input
    // waiting for one.
    CHECK((spec.rule.EndsWith(wxS(";")) || spec.rule.EndsWith(wxS("$"))));
    // The "Help on" buttons come from the rule: each wizard gets at least one.
    CHECK(!wxm::WizardHelpKeywords(spec.rule).empty());
    for (const auto &field : spec.fields)
      CHECK(!field.label.IsEmpty());
    for (const wxWindowID id : spec.ids) {
      const auto found = wxm::FindWizard(id);
      REQUIRE(found.has_value());
      CHECK(found->rule == spec.rule);
    }
  }
}

SCENARIO("No menu entry opens two different wizards") {
  std::set<wxWindowID> seen;
  for (const WizardSpec &spec : WizardCatalog())
    for (const wxWindowID id : spec.ids) {
      INFO("Wizard: " << spec.title.utf8_string());
      CHECK(seen.insert(id).second);
    }
}

SCENARIO("IDs that have no standard wizard find none") {
  CHECK(!wxm::FindWizard(EventIDs::menu_integrate).has_value());
  CHECK(!wxm::FindWizard(wxID_ANY).has_value());
}

SCENARIO("A field that takes the selection starts with it") {
  const auto diff = wxm::FindWizard(EventIDs::menu_diff);
  REQUIRE(diff.has_value());
  const auto fields = wxm::FieldsWithSelection(*diff, wxS("sin(x)^2"));
  REQUIRE(fields.size() == 3);
  CHECK(fields[0].defaultValue == wxS("sin(x)^2"));
  // The others keep their own defaults.
  CHECK(fields[1].defaultValue == wxS("x"));
  CHECK(fields[2].defaultValue == wxS("1"));
}

SCENARIO("The sidebar button and the menu entry open the same wizard") {
  const auto menu = wxm::FindWizard(EventIDs::menu_diff);
  const auto button = wxm::FindWizard(EventIDs::button_diff);
  REQUIRE(menu.has_value());
  REQUIRE(button.has_value());
  CHECK(menu->rule == button->rule);
}

SCENARIO("\"Dito, and evaluate the result\" has a wizard") {
  // This menu entry was never bound to a handler and did nothing.
  const auto spec = wxm::FindWizard(EventIDs::menu_change_var_evaluate);
  REQUIRE(spec.has_value());
  CHECK(spec->rule.EndsWith(wxS(",nouns;")));
}

SCENARIO("The Fourier wizard loads its package only once") {
  WizardContext notLoaded;
  notLoaded.fourierLoaded = false;
  WizardContext loaded;
  loaded.fourierLoaded = true;
  const auto first = wxm::FindWizard(EventIDs::menu_fourier, notLoaded);
  const auto later = wxm::FindWizard(EventIDs::menu_fourier, loaded);
  REQUIRE(first.has_value());
  REQUIRE(later.has_value());
  CHECK(first->rule.StartsWith(wxS("load(\"fourie\")$")));
  CHECK(later->rule == wxS("fourier(#1#,#2#,#3#);"));
}

SCENARIO("The Cauchy principal value wizard passes c, a and b in order") {
  // Its rule used to stop at #7#: c, a and b landed in the wrong arguments.
  const auto qawc = wxm::FindWizard(EventIDs::menu_quad_qawc);
  REQUIRE(qawc.has_value());
  REQUIRE(qawc->fields.size() == 8);
  CHECK(qawc->fields[2].label == wxS("c"));
  CHECK(qawc->rule.StartsWith(wxS("quad_qawc(#1#,#2#,#3#,#4#,#5#,")));
}

SCENARIO("Both quad_qawo() wizards ask for the upper limit") {
  // The cos variant had no "b" field and left "#10#" in its command.
  for (const wxWindowID id :
         {EventIDs::menu_quad_qawo_sin, EventIDs::menu_quad_qawo_cos}) {
    const auto qawo = wxm::FindWizard(id);
    REQUIRE(qawo.has_value());
    REQUIRE(qawo->fields.size() == 10);
    CHECK(qawo->fields[3].label == wxS("b"));
  }
}

SCENARIO("The quad_qaws() wizards differ only in their weight function") {
  long weight = 1;
  for (const wxWindowID id :
         {EventIDs::menu_quad_qaws1, EventIDs::menu_quad_qaws2,
          EventIDs::menu_quad_qaws3, EventIDs::menu_quad_qaws4}) {
    const auto qaws = wxm::FindWizard(id);
    REQUIRE(qaws.has_value());
    CHECK(qaws->rule.Contains(wxString::Format(wxS("#6#,%ld,"), weight)));
    weight++;
  }
}

SCENARIO("The numerical integration wizards use the integral sign given") {
  WizardContext noGlyph;
  noGlyph.integralSign = wxS("integrate");
  const auto qags = wxm::FindWizard(EventIDs::menu_quad_qags, noGlyph);
  REQUIRE(qags.has_value());
  CHECK(qags->title.StartsWith(wxS("integrate(f(x)")));
  const auto withGlyph = wxm::FindWizard(EventIDs::menu_quad_qags);
  REQUIRE(withGlyph.has_value());
  CHECK(withGlyph->title.StartsWith(wxS("\u222B")));
}

int main(int argc, char *argv[]) {
  // The event IDs come from wxWindow::NewControlId(), which needs wx.
  wxInitializer initializer;
  return Catch::Session().run(argc, argv);
}

SCENARIO("The substitution wizards wrap their equations in a list") {
  // Without the brackets, several comma-separated equations are read as
  // the (new,old,expr) form of these commands.
  for (const wxWindowID id :
         {EventIDs::menu_subst, EventIDs::menu_ratsubst, EventIDs::menu_psubst,
          EventIDs::menu_fullratsubst, EventIDs::menu_at}) {
    const auto spec = wxm::FindWizard(id);
    REQUIRE(spec.has_value());
    INFO("Rule: " << spec->rule.utf8_string());
    CHECK(spec->rule.Contains(wxS("[#2#]")));
    REQUIRE(spec->fields.size() == 2);
    CHECK(spec->fields[0].takesSelection);
  }
}

SCENARIO("The radcan button and menu entry open the same wizard") {
  const auto menu = wxm::FindWizard(EventIDs::menu_radsimp);
  const auto button = wxm::FindWizard(EventIDs::button_radcan);
  REQUIRE(menu.has_value());
  REQUIRE(button.has_value());
  CHECK(menu->rule == wxS("radcan(#1#);"));
  CHECK(button->rule == menu->rule);
}

SCENARIO("substinpart()'s tooltip describes the expression field") {
  const auto spec = wxm::FindWizard(EventIDs::menu_substinpart);
  REQUIRE(spec.has_value());
  REQUIRE(spec->fields.size() == 3);
  CHECK(spec->fields[0].takesSelection);
  CHECK(spec->fields[0].tooltip == _("The expression to substitute in"));
  CHECK(spec->fields[1].tooltip.IsEmpty());
}

SCENARIO("The equation solvers wrap equations and variables in lists") {
  for (const wxWindowID id :
         {EventIDs::menu_solve, EventIDs::button_solve,
          EventIDs::menu_solve_to_poly}) {
    const auto spec = wxm::FindWizard(id);
    REQUIRE(spec.has_value());
    INFO("Rule: " << spec->rule.utf8_string());
    CHECK(spec->rule.Contains(wxS("([#1#],[#2#])")));
    REQUIRE(spec->fields.size() == 2);
    CHECK(spec->fields[0].takesSelection);
  }
}

SCENARIO("The ODE wizards open from both the menu and the sidebar") {
  const auto menu = wxm::FindWizard(EventIDs::menu_solve_ode);
  const auto button = wxm::FindWizard(EventIDs::button_solve_ode);
  REQUIRE(menu.has_value());
  REQUIRE(button.has_value());
  CHECK(menu->rule == wxS("ode2(#1#,#2#,#3#);"));
  CHECK(button->rule == menu->rule);
  // The context menu's "Value at a specific point" is the atvalue() wizard.
  const auto atvalue = wxm::FindWizard(EventIDs::popid_property_atvalue);
  REQUIRE(atvalue.has_value());
  CHECK(atvalue->rule.StartsWith(wxS("atvalue(")));
}

SCENARIO("find_root() and the fraction wizard keep their own handler") {
  // It rewrites a selection starting with % into ''(%), which no catalog
  // field can do.
  CHECK(!wxm::FindWizard(EventIDs::menu_solve_num).has_value());
  // "Construct a fraction" names no function a "Help on" button could show.
  CHECK(!wxm::FindWizard(EventIDs::menu_construct_fraction).has_value());
}
