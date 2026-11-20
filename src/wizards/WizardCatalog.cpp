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
#include <array>
#include <utility>

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

  // --------------------------------------------------------------- Numerical
  catalog.push_back({
      .ids = {EventIDs::menu_set_precision},
      .title = _("Enter new precision for bigfloats:"),
      .rule = wxS("fpprec : #1#$"),
      .fields = {Field(_("Precision"), wxS("%"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_set_displayprecision},
      .title = _("Displayed Precision"),
      .rule = wxS("fpprintprec : #1#$"),
      .fields = {Field(_("How many digits to show:"), wxS("%"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_engineeringFormatSetup},
      .title = _("Setup the engineering format"),
      .rule = wxS("engineering_format_floats: #1#$\n"
                  "engineering_format_min: #2#$\n"
                  "engineering_format_max: #3#$\n"
                  "fpprintprec: #4#$"),
      .fields = {
        Field(_("Enable:"), wxS("true")),
        Field(_("Minimum absolute value printed without exponent:"),
              wxS(".01")),
        Field(_("Maximum absolute value printed without exponent"),
              wxS("1000")),
        Field(_("Maximum number of digits to be displayed:"), wxS("6"))}});

  // QUADPACK. The integrand starts with the selection (or %) as the menu
  // handler's other wizards do; it used to be a literal "%" even when an
  // expression was selected.
  const WizardField quadIntegrand = SelectionField(wxS("f(x)"));
  const WizardField quadVariable = Field(wxS("x"), wxS("x"));
  const WizardField epsrel =
    Field(wxS("epsrel"), wxS("1d-8"),
          _("Desired relative error of approximation."));
  const WizardField epsabs =
    Field(wxS("epsabs"), wxS("0"),
          _("Desired absolute error of approximation."));
  const WizardField limit =
    Field(wxS("limit"), wxS("200"),
          _("Size of internal work array. limit is the maximum number of "
            "subintervals to use."));
  const WizardField maxp1 =
    Field(wxS("maxp1"), wxS("100"),
          _("Maximum number of Chebyshev moments. Must be greater than 0."));
  const WizardField limlst =
    Field(wxS("limlst"), wxS("10"),
          _("Upper bound on the number of cycles. Must be greater than or "
            "equal to 3."));
  const wxString &integral = context.integralSign;

  catalog.push_back({
      .ids = {EventIDs::menu_quad_qag},
      .title = integral + _("(f(x),x,a,b)), Strategy of Aind"),
      .rule = wxS("quad_qag(#1#,#2#,#3#,#4#,#5#,'epsrel=#6#,'epsabs=#7#,"
                  "'limit=#8#);"),
      .fields = {quadIntegrand, quadVariable,
                 Field(wxS("a"), wxS("0")), Field(wxS("b"), wxS("10")),
                 Field(wxS("key"), wxS("4"),
                       _("An integer between 1..6; Higher numbers work "
                         "better for oscillating integrands")),
                 epsrel, epsabs, limit}});
  catalog.push_back({
      .ids = {EventIDs::menu_quad_qags},
      .title = integral + _("(f(x),x,a,b)), Epsilon algorithm"),
      .rule = wxS("quad_qags(#1#,#2#,#3#,#4#,'epsrel=#5#,'epsabs=#6#,"
                  "'limit=#7#);"),
      .fields = {quadIntegrand, quadVariable,
                 Field(wxS("a"), wxS("0")), Field(wxS("b"), wxS("10")),
                 epsrel, epsabs, limit}});
  catalog.push_back({
      .ids = {EventIDs::menu_quad_qagi},
      .title = integral + _("(f(x),x,a,b), (semi-) infinite interval"),
      .rule = wxS("quad_qagi(#1#,#2#,#3#,#4#,'epsrel=#5#,'epsabs=#6#,"
                  "'limit=#7#);"),
      .fields = {quadIntegrand, quadVariable,
                 Field(wxS("a"), wxS("0")), Field(wxS("b"), wxS("10")),
                 epsrel, epsabs, limit}});
  catalog.push_back({
      .ids = {EventIDs::menu_quad_qawc},
      .title = _("Cauchy principal value of f(x)/(x-c), finite interval"),
      // The rule used to stop at #7#, so c, a and b were passed in the wrong
      // places and "limit" was never used.
      .rule = wxS("quad_qawc(#1#,#2#,#3#,#4#,#5#,'epsrel=#6#,'epsabs=#7#,"
                  "'limit=#8#);"),
      .fields = {quadIntegrand, quadVariable,
                 Field(wxS("c"), wxS("4")),
                 Field(wxS("a"), wxS("0")), Field(wxS("b"), wxS("10")),
                 epsrel, epsabs, limit}});

  const WizardField qawfLimit =
    Field(wxS("limit"), wxS("200"),
          _("Size of internal work array. (limit - limlst)/2 is the maximum "
            "number of subintervals to use."));
  for (const auto &[id, trig] :
         {std::pair<wxWindowID, wxString>{EventIDs::menu_quad_qawf_sin, wxS("sin")},
          std::pair<wxWindowID, wxString>{EventIDs::menu_quad_qawf_cos, wxS("cos")}}) {
    catalog.push_back({
        .ids = {id},
        .title = integral + wxS("(f(x)*") + trig + wxS("(ω·x),x,a,∞)"),
        .rule = wxS("quad_qawf(#1#,#2#,#3#,#4#,'") + trig +
                wxS(",'epsabs=#5#,'limit=#6#,'maxp1=#7#,'limlst=#8#);"),
        .fields = {quadIntegrand, quadVariable,
                   Field(wxS("a"), wxS("a")), Field(wxS("ω"), wxS("2")),
                   Field(wxS("epsabs"), wxS("1d-10"),
                         _("Desired absolute error of approximation.")),
                   qawfLimit, maxp1, limlst}});
  }

  const WizardField qawoLimit =
    Field(wxS("limit"), wxS("200"),
          _("Size of internal work array. limit/2 is the maximum number of "
            "subintervals to use."));
  for (const auto &[id, trig] :
         {std::pair<wxWindowID, wxString>{EventIDs::menu_quad_qawo_sin, wxS("sin")},
          std::pair<wxWindowID, wxString>{EventIDs::menu_quad_qawo_cos, wxS("cos")}}) {
    catalog.push_back({
        .ids = {id},
        .title = integral + wxS("(f(x)*") + trig + wxS("(ω·x),x,a,b)"),
        // The cos variant used to lack the "b" field, which shifted every
        // later field by one and left "#10#" in the command.
        .rule = wxS("quad_qawo(#1#,#2#,#3#,#4#,#5#,'") + trig +
                wxS(",'epsrel=#6#,'epsabs=#7#,'limit=#8#,'maxp1=#9#,"
                    "'limlst=#10#);"),
        .fields = {quadIntegrand, quadVariable,
                   Field(wxS("a"), wxS("a")), Field(wxS("b"), wxS("b")),
                   Field(wxS("ω"), wxS("2")),
                   epsrel, epsabs, qawoLimit, maxp1, limlst}});
  }

  // quad_qaws()'s weight functions 1 to 4. The values for alpha and beta are
  // those of the example in Maxima's documentation.
  const std::array<std::pair<wxWindowID, wxString>, 4> qawsWeights = {{
      {EventIDs::menu_quad_qaws1, wxS("(f(x)*(x-a)^α(b-x)^β,x,a,b)")},
      {EventIDs::menu_quad_qaws2,
       wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a),x,a,b)")},
      {EventIDs::menu_quad_qaws3,
       wxS("(f(x)*(x-a)^α(b-x)^β·log(b-x),x,a,b)")},
      {EventIDs::menu_quad_qaws4,
       wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a)·log(b-x),x,a,b)")}}};
  for (std::size_t i = 0; i < qawsWeights.size(); i++)
    catalog.push_back({
        .ids = {qawsWeights[i].first},
        .title = integral + qawsWeights[i].second,
        .rule = wxString::Format(
          wxS("quad_qaws(#1#,#2#,#3#,#4#,#5#,#6#,%u,'epsrel=#7#,'epsabs=#8#,"
              "'limit=#9#);"),
          static_cast<unsigned>(i + 1)),
        .fields = {quadIntegrand, quadVariable,
                   Field(wxS("a"), wxS("1")), Field(wxS("b"), wxS("2")),
                   Field(wxS("α"), wxS("-0.5")), Field(wxS("β"), wxS("-0.5")),
                   epsrel, epsabs, limit}});

  catalog.push_back({
      .ids = {EventIDs::menu_quad_qagp},
      .title = integral + _("(f(x),x,y) with singularities+discontinuities"),
      .rule = wxS("quad_qagp(#1#,#2#,#3#,#4#,[#5#],'epsrel=#6#,'epsabs=#7#,"
                  "'limit=#8#);"),
      .fields = {quadIntegrand, quadVariable,
                 Field(wxS("a"), wxS("1")), Field(wxS("b"), wxS("2")),
                 Field(wxS("points"), wxS(".5,.75")),
                 epsrel, epsabs, limit}});

  // ---------------------------------------------------------------- Simplify
  // Most of these wizards' field labels have never been translatable (wxS
  // rather than _()); they are kept as they were so that this move changes
  // no msgid. Their "Expression" used to be a literal "%" even when an
  // expression was selected; it now starts with the selection, as in
  // the other menus.
  catalog.push_back({
      .ids = {EventIDs::menu_nouns},
      .title = _("Evaluate Nouns"),
      .description =
        _("Maxima allows making functions \"nouns\", which means that they "
          "aren't automatically evaluated as soon as Maxima encounters them.\n"
          "Ways to make a function a noun include declaring it a noun, preceding "
          "it with a single quote or putting it between the parentheses of \'().\n\n"
          "This command tells Maxima that the nouns in this expression "
          "shall now be evaluated, too."),
      .rule = wxS("#1#,nouns;"),
      .fields = {SelectionField(_("Expression"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_simpsum},
      .title = _("Simplify sums"),
      .description = _("Try to simplify sums that result from sum() commands."),
      .rule = wxS("simpsum(#1#);"),
      .fields = {SelectionField(_("Expression"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_radsimp, EventIDs::button_radcan},
      .title = _("Simplify radicals"),
      .description =
        _("radcan() is a powerful tools for simplification trigonometric "
          "functions "
          "but needs to be taken with care: If a function has more than one "
          "branch "
          "radcan uses the one that looks like it would fit best, not "
          "necessarily "
          "the one that makes sense for the problem that resulted in the "
          "Expression that is to be simplified."),
      .rule = wxS("radcan(#1#);"),
      .fields = {SelectionField(_("Expression"))}});

  const WizardField simplifyExpression = SelectionField(wxS("Expression"));
  const WizardField expandVariables =
    Field(wxS("Variable(s)"), wxS("x"), _("Comma-separated variables"));
  catalog.push_back({
      .ids = {EventIDs::menu_expandwrt},
      .title = _("Expand for variable(s):"),
      .rule = wxS("expandwrt(#1#,#2#);"),
      .fields = {simplifyExpression, expandVariables}});
  catalog.push_back({
      .ids = {EventIDs::menu_expandwrt_denom},
      .title = _("Expand for variable(s) including denominator:"),
      .rule = wxS("expandwrt(#1#,#2#),expandwrt_denom=true;"),
      .fields = {simplifyExpression, expandVariables}});

  // The substitution commands. Each takes a list of equations: without the
  // brackets several comma-separated equations would be misread as the
  // (new,old,expr) form, and all but the first silently ignored.
  const auto substituents = [](const wxString &example) {
    return Field(wxS("Substituents"), example,
                 _("Comma-separated expressions"));
  };
  catalog.push_back({
      .ids = {EventIDs::menu_subst},
      .title = _("Substitute"),
      .description =
        _("Subst is a better string-search-and-replace for expressions."),
      .rule = wxS("subst([#2#],#1#);"),
      .fields = {simplifyExpression, substituents(wxS("x^2=u"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_ratsubst},
      .title = _("Smart substitution"),
      .description = _("ratsubst works like subst, but it knows some basic maths, if "
                       "needed."),
      // ratsubst() takes (new, old, expr) and rejects an equation, so use
      // lratsubst(), which accepts a (list of) equation(s).
      .rule = wxS("lratsubst([#2#],#1#);"),
      .fields = {simplifyExpression, substituents(wxS("x^2=u"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_psubst},
      .title = _("Parallel substitution"),
      .description = _("Substitutes, but makes sure that nothing is substituted into "
                       "the other substituents."),
      .rule = wxS("psubst([#2#],#1#);"),
      .fields = {simplifyExpression, substituents(wxS("x^2=u,u=x^2"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_fullratsubst},
      .title = _("Recursive substitution"),
      .description = _("Substitutes up to lrats_max_iter times, or until the "
                       "expression stops changing when substituting."),
      .rule = wxS("fullratsubst([#2#],#1#);"),
      .fields = {simplifyExpression, substituents(wxS("x^2=u"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_at},
      .title = _("Value at a given point"),
      .description =
        _("Substitutes, but makes sure that if substituting t=0 in diff(x,t) "
          "the result isn't 0 (as t no more changes), but %at(diff(x,t),t=0)."),
      .rule = wxS("at(#1#,[#2#]);"),
      .fields = {simplifyExpression, substituents(wxS("x=0"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_substinpart},
      .title = _("Substitute only in a specific part"),
      .description =
        _("Replaces the subexpression that part(expr, n_1, n_2, ...) "
          "would select by a new value. The part numbers form a path: "
          "n_1 selects a part of the expression, n_2 a part of that, and "
          "so on."),
      .rule = wxS("substinpart(#2#,#1#,#3#);"),
      // "The expression to substitute in" used to be the tooltip of
      // "New value".
      .fields = {SelectionField(wxS("Expression"),
                                _("The expression to substitute in")),
                 Field(wxS("New value"), wxS("z")),
                 Field(wxS("Part path"), wxS("1"),
                       _("Comma-separated part numbers selecting the "
                         "subexpression to replace (as in part())"))}});

  catalog.push_back({
      .ids = {EventIDs::menu_mainvar},
      .title = _("Declare main variable:"),
      .rule = wxS("declare(#1#,mainvar);"),
      .fields = {SelectionField(wxS("Variable"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_tellrat},
      .title = _("Enter an equation for rational simplification:"),
      .rule = wxS("tellrat(#1#);"),
      .fields = {SelectionField(wxS("Equation"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_modulus},
      .title = _("Calculate modulus:"),
      .rule = wxS("modulus : #1#$"),
      .fields = {Field(wxS("Modulus"), wxS("%"))}});

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
