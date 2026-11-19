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
  The standard wizards of wxMaxima's menus: see WizardCatalog.h.

  Adding a wizard here is all it takes: wxMaxima binds every ID listed to
  MaximaCommandMenus::CatalogWizard(), and test_WizardCatalog checks that
  each rule uses exactly the fields its wizard has. Keep the text of an
  existing wizard byte-identical when moving it here, or its translations
  go fuzzy.
*/

#include "WizardCatalog.h"
#include "EventIDs.h"
#include <wx/intl.h>
#include <algorithm>

std::vector<wxm::WizardSpec> wxm::WizardCatalog(const WizardContext &context) {
  std::vector<WizardSpec> catalog;

  // ---------------------------------------------------------------- Calculus
  // changevar()'s description has never been translatable (wxS rather than
  // _()); it is kept as it was so that this move changes no msgid.
  const wxString changeVarDescription =
    wxS("Takes an integral or sum in respect to the Old Variable and "
        "replaces "
        "that variable by a new one both adjusting the integrand and the "
        "limits "
        "accordingly. The field \"Equation\" specifies how the Old "
        "Variable and "
        "the New Variable are related to each other; For sums changevar "
        "isn't "
        "intelligent enough to do more than a shift of the variable.\n\n"
        "changevar(integrate(f(x),x,1,10),u=sqrt(x),u,x);\n"
        "results in\n"
        "2*integrate(u*f(u^2),u,1,sqrt(10))\n\n");
  const std::vector<WizardField> changeVarFields = {
    SelectionField(_("Integral/Sum:")),
    Field(_("Equation:"), wxS("u=sqrt(x)")),
    Field(_("New variable:"), wxS("u")),
    Field(_("Old variable:"), wxS("x"))};
  catalog.push_back({
      .ids = {EventIDs::menu_change_var},
      .title = _("Change variable"),
      .description = changeVarDescription,
      .rule = wxS("changevar(#1#,#2#,#3#,#4#);"),
      .fields = changeVarFields});
  catalog.push_back({
      .ids = {EventIDs::menu_change_var_evaluate},
      .title = _("Change variable and evaluate"),
      .description =
        wxS("Takes an integral or sum in respect to the Old Variable and "
            "replaces "
            "that variable by a new one both adjusting the integrand and the "
            "limits "
            "accordingly. Then evaluates the resulting integral or sum. "
            "The field \"Equation\" specifies how the Old Variable and "
            "the New Variable are related to each other; For sums changevar "
            "isn't "
            "intelligent enough to do more than a shift of the variable.\n\n"
            "changevar(integrate(f(x),x,1,10),u=sqrt(x),u,x);\n"
            "results in\n"
            "2*integrate(u*f(u^2),u,1,sqrt(10))\n\n"),
      .rule = wxS("changevar(#1#,#2#,#3#,#4#),nouns;"),
      .fields = changeVarFields});
  catalog.push_back({
      .ids = {EventIDs::menu_pade},
      .title = _("Pade approximation"),
      .rule = wxS("pade(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Taylor series:")),
                 Field(_("Num. deg:"), wxS("4")),
                 Field(_("Denom. deg:"), wxS("4"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_taylor},
      .title = _("Taylor series"),
      .description =
        _("Approximates a expression around a point as a polynom\n"
          "The trailing \"...\" can be removed by using ratdisrep()"),
      .rule = wxS("taylor(#1#,#2#,#3#,#4#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("x")),
                 Field(_("Point:"), wxS("0")),
                 Field(_("Degree:"), wxS("3"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_powerseries},
      .title = _("Power series"),
      .description = _("Approximates a expression as a polynom"),
      // The niceindices( ... ) parenthesis has to be closed, too: the old
      // template emitted "niceindices(powerseries(e,x,0);" which Maxima
      // rejects with "incorrect syntax: Missing )".
      .rule = wxS("niceindices(powerseries(#1#,#2#,#3#));"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("x")),
                 Field(_("point:"), wxS("0"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_fourier},
      .title = _("Fourier coefficients"),
      .description = _("Calculates the fourier coefficients for the expression "
                       "from -p to p"),
      .rule = (context.fourierLoaded ? wxString()
                                       : wxString(wxS("load(\"fourie\")$\n"))) +
              wxS("fourier(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("x")),
                 Field(_("Range radius:"), wxS("2"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_lcm},
      .title = _("LCM"),
      .rule = wxS("lcm(#1#,#2#);"),
      .fields = {SelectionField(_("Polynomial 1:")),
                 Field(_("Polynomial 2:"), wxString())}});
  catalog.push_back({
      .ids = {EventIDs::menu_gcd},
      .title = _("GCD"),
      .rule = wxS("gcd(#1#,#2#);"),
      .fields = {SelectionField(_("Polynomial 1:")),
                 Field(_("Polynomial 2:"), wxString())}});
  catalog.push_back({
      .ids = {EventIDs::menu_divide},
      // Not a typo of this move: the divide() wizard has always been
      // titled "GCD". Fixing that adds a msgid, so it gets a commit of its
      // own.
      .title = _("GCD"),
      .rule = wxS("divide(#1#,#2#);"),
      .fields = {SelectionField(_("Polynomial 1:")),
                 Field(_("Polynomial 2:"), wxString())}});
  catalog.push_back({
      .ids = {EventIDs::menu_partfrac},
      .title = _("Partial Fractions"),
      .rule = wxS("partfrac(#1#,#2#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("n"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_risch},
      .title = _("Integrate (risch)"),
      .rule = wxS("risch(#1#,#2#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("x"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_laplace},
      .title = _("Laplace"),
      .rule = wxS("laplace(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Old variable:"), wxS("t")),
                 Field(_("New variable:"), wxS("s"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_ilt},
      .title = _("Inverse Laplace"),
      .rule = wxS("ilt(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Old variable:"), wxS("s")),
                 Field(_("New variable:"), wxS("t"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_diff, EventIDs::button_diff},
      .title = _("Differentiate"),
      .description = _("Differentiates an expression n times"),
      .rule = wxS("diff(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable(s):"), wxS("x")),
                 Field(_("Times:"), wxS("1"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_lbfgs},
      .title = _("Find minimum"),
      .description = _("Allows varying the parameters of a function until it "
                       "fits experimental data."),
      .rule = wxS("lbfgs(#1#,#2#,#3#,#4#,[1,1]);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variables:"), wxS("x")),
                 Field(_("Initial estimates:"), wxS("1.0")),
                 Field(_("Epsilon:"), wxS("1e-4"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_product, EventIDs::button_product},
      .title = _("Product"),
      .rule = wxS("product(#1#,#2#,#3#,#4#);"),
      .fields = {SelectionField(_("Expression:")),
                 Field(_("Variable:"), wxS("k")),
                 Field(_("From:"), wxS("1")),
                 Field(_("To:"), wxS("n"))}});

  return catalog;
}

std::optional<wxm::WizardSpec> wxm::FindWizard(wxWindowID id,
                                               const WizardContext &context) {
  for (auto &spec : WizardCatalog(context))
    if (std::find(spec.ids.begin(), spec.ids.end(), id) != spec.ids.end())
      return std::move(spec);
  return std::nullopt;
}

std::vector<wxm::WizardField>
wxm::FieldsWithSelection(const WizardSpec &spec, const wxString &selection) {
  std::vector<WizardField> fields = spec.fields;
  for (auto &field : fields)
    if (field.takesSelection)
      field.defaultValue = selection;
  return fields;
}
