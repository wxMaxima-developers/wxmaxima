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
#include <clocale>
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
  // _() converts its narrow string with the C library's character set, as
  // wxMaxima does once it has set its locale. In the "C" locale the label
  // "m×n Matrix A:" would come out empty.
  for (const char *locale : {"C.UTF-8", "en_US.UTF-8", ".UTF8"})
    if (setlocale(LC_ALL, locale))
      break;
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

SCENARIO("Removing matrix rows passes the rows to submatrix() first") {
  const auto rows = wxm::FindWizard(EventIDs::menu_submatrix_rows);
  const auto columns = wxm::FindWizard(EventIDs::menu_submatrix_columns);
  REQUIRE(rows.has_value());
  REQUIRE(columns.has_value());
  CHECK(rows->rule == wxS("submatrix(#2#,#1#);"));
  CHECK(columns->rule == wxS("submatrix(#1#,#2#);"));
}

SCENARIO("The complex LAPACK norms call zlange(), the real ones dlange()") {
  const auto real = wxm::FindWizard(EventIDs::menu_matrix_dlange_frobenius);
  const auto complex = wxm::FindWizard(EventIDs::menu_matrix_zlange_frobenius);
  REQUIRE(real.has_value());
  REQUIRE(complex.has_value());
  CHECK(real->rule == wxS("dlange('frobenius,#1#);"));
  CHECK(complex->rule == wxS("zlange('frobenius,#1#);"));
  CHECK(real->title == complex->title);
}

SCENARIO("dgesv() gets both A and b") {
  const auto spec = wxm::FindWizard(EventIDs::menu_matrix_dgesv);
  REQUIRE(spec.has_value());
  CHECK(spec->rule == wxS("dgesv(#1#,#2#);"));
}

SCENARIO("The classic matrix operations are one-field wizards") {
  for (const wxWindowID id :
         {EventIDs::menu_invert_mat, EventIDs::menu_determinant,
          EventIDs::menu_rank, EventIDs::menu_eigen, EventIDs::menu_eigvect,
          EventIDs::menu_adjoint_mat, EventIDs::menu_transpose}) {
    const auto spec = wxm::FindWizard(id);
    REQUIRE(spec.has_value());
    INFO("Title: " << spec->title.utf8_string());
    // The title is the menu label without its mnemonic.
    CHECK(!spec->title.Contains(wxS("&")));
    REQUIRE(spec->fields.size() == 1);
    CHECK(spec->fields[0].takesSelection);
    CHECK(spec->rule.EndsWith(wxS("(#1#);")));
  }
  CHECK(wxm::FindWizard(EventIDs::menu_invert_mat)->rule == wxS("invert(#1#);"));
}

SCENARIO("map() opens from both the menu and the sidebar") {
  const auto menu = wxm::FindWizard(EventIDs::menu_map);
  const auto button = wxm::FindWizard(EventIDs::button_map);
  REQUIRE(menu.has_value());
  REQUIRE(button.has_value());
  CHECK(menu->rule == wxS("map(#1#,#2#);"));
  CHECK(button->rule == menu->rule);
}

SCENARIO("The matrix wizards that need a dialog of their own keep their handler") {
  for (const wxWindowID id :
         {EventIDs::menu_csv2mat, EventIDs::menu_mat2csv, EventIDs::menu_enter_mat,
          EventIDs::menu_map_mat, EventIDs::menu_matrix_loadLapack})
    CHECK(!wxm::FindWizard(id).has_value());
}

SCENARIO("The list wizards that pass the list second keep that order") {
  // push(item, list) and delete(item, list) take the list second.
  const auto push = wxm::FindWizard(EventIDs::menu_list_push);
  REQUIRE(push.has_value());
  CHECK(push->rule == wxS("push(#2#,#1#);"));
  CHECK(push->fields[0].label == _("List:"));
  const auto remove = wxm::FindWizard(EventIDs::menu_list_remove_element);
  REQUIRE(remove.has_value());
  CHECK(remove->rule == wxS("delete(#1#,#2#);"));
  CHECK(remove->fields[0].label == _("Element"));
}

SCENARIO("Extracting the last n list elements counts from the end") {
  const auto spec = wxm::FindWizard(EventIDs::menu_list_lastn);
  REQUIRE(spec.has_value());
  CHECK(spec->rule == wxS("rest(#1#,length(#1#)-#2#);"));
}

SCENARIO("Values are substituted as a list") {
  for (const wxWindowID id :
         {EventIDs::menu_list_use_actual_values, EventIDs::menu_list_extract_value}) {
    const auto spec = wxm::FindWizard(id);
    REQUIRE(spec.has_value());
    CHECK(spec->rule == wxS("subst([#1#],#2#);"));
  }
}

SCENARIO("The list wizards that need a dialog of their own keep their handler") {
  for (const wxWindowID id :
         {EventIDs::menu_csv2list, EventIDs::menu_list2csv,
          EventIDs::menu_list_sort, EventIDs::menu_list_actual_values_storage})
    CHECK(!wxm::FindWizard(id).has_value());
}
