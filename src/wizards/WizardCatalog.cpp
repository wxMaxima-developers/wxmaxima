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
#include "MatrixWizardRules.h"
#include <wx/utils.h>
#include <wx/intl.h>
#include <algorithm>
#include <array>
#include <tuple>
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

  // --------------------------------------------------------------- Equations
  // find_root() stays in MaximaCommandMenus::EquationsMenu(): it rewrites a
  // selection starting with "%" to ''(%), which a field can't express. So
  // does "Construct a fraction": its command names no function, so there is
  // nothing for a "Help on" button, and it has no terminator.
  const WizardField polynomial = SelectionField(_("Polynomial:"));
  catalog.push_back({
      .ids = {EventIDs::menu_allroots},
      .title = _("Solve polynomials numerically"),
      .description =
        _("Tries to find all solutions of a polynomial numerically.\n"
          "Might be able to detect solutions in non-polynomials, as well, if "
          "the expression is approximated by an polynomial, beforehand:\n\n"
          "    allroots(ratdisrep(taylor(expression,0,30)));"),
      .rule = wxS("allroots(#1#);"),
      .fields = {polynomial}});
  catalog.push_back({
      .ids = {EventIDs::menu_bfallroots},
      .title = _("Solve polynomials numerically (bfloats)"),
      .description =
        _("Tries to find all solutions of a polynomial numerically using "
          "bfloats.\n"
          "Might be able to detect solutions in non-polynomials, as well, if "
          "the expression is approximated by an polynomial, beforehand:\n\n"
          "    bfallroots(ratdisrep(taylor(expression,0,30)));"),
      .rule = wxS("bfallroots(#1#);"),
      .fields = {polynomial}});
  catalog.push_back({
      .ids = {EventIDs::menu_realroots},
      .title = _("Solve polynomials numerically (real roots)"),
      .description =
        // xgettext:no-c-format
        _("Tries to find exact fractions that match the numerical solutions of "
          "a polynomial.\n"
          "Is not able to deal with solutions with a imaginary part. "
          "Numerical constants like %pi% need to be eliminated using float() "
          "or similar\n"
          "Might be able to detect solutions in non-polynomials, as well, if "
          "the expression is approximated by an polynomial, beforehand:\n\n"
          "    realroots(ratdisrep(taylor(expression,0,30)));\n\n"
          "See also guess_exact_value()"),
      .rule = wxS("realroots(#1#,#2#);"),
      .fields = {polynomial, Field(_("precision:"), wxS("1e-12"))}});

  const WizardField equations =
    SelectionField(_("Equation(s)"), _("Comma-separated equations"));
  const WizardField solveVariables =
    Field(_("Variable(s)"), wxS("x"), _("Comma-separated variables"));
  catalog.push_back({
      .ids = {EventIDs::menu_solve, EventIDs::button_solve},
      .title = _("Solve equation(s)"),
      .description =
        _("solve() will solve a list of equations only if for n "
          "independent equations there are n variables to solve to.\n"
          "If only one result variable is of interest the other result "
          "variables solve needs to do its work can be used to tell "
          "solve() which variables to eliminate in the solution "
          "for the interesting variable."),
      .rule = wxS("solve([#1#],[#2#]);"),
      .fields = {equations, solveVariables}});
  catalog.push_back({
      .ids = {EventIDs::menu_solve_to_poly},
      .title = _("Solve equations to polynom"),
      .description =
        _(wxS("The function to_poly_solve tries to solve the equations "
              "e for the variables l. The equation(s) e can either be a "
              "single expression or a set or list of expressions; "
              "similarly, l can either be a single symbol or a list of "
              "set of symbols. When a member of e isn’t explicitly an "
              "equation, for example x^2 -1, the solver assumes that the "
              "expression vanishes.")),
      .rule = wxS("to_poly_solve([#1#],[#2#]);"),
      .fields = {equations, solveVariables}});

  catalog.push_back({
      .ids = {EventIDs::menu_solve_ode, EventIDs::button_solve_ode},
      .title = _("Solve ODE"),
      .description = _("solves an equation of the form\n    'diff(y,t) = -y;"),
      .descriptionTooltip =
        _("The solution of an ODE describes the general shape of the "
          "resulting curve. The actual height of that curve is defined "
          "by the initial condition or boundary values, later on."),
      .rule = wxS("ode2(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Equation:")), Field(_("y:"), wxS("y")),
                 Field(_("t:"), wxS("t"))}});
  const wxString initialConditionDescription =
    _("The solution of an ODE tells the shape, but not the height "
      "of the solution.\n"
      "If the ODE\'s state is known at a point this "
      "function fills in the correct values for the constants");
  const WizardField odeSolution = SelectionField(_("Solution of the ODE:"));
  catalog.push_back({
      .ids = {EventIDs::menu_ivp_1},
      .title = _("Initial Condition"),
      .description = initialConditionDescription,
      .rule = wxS("ic1(#1#,#2#,#3#);"),
      .fields = {odeSolution,
                 Field(_("Point the value is known at:"), wxS("t=0")),
                 Field(_("Value at that point:"), wxS("y=1"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_ivp_2},
      .title = _("Initial Condition"),
      .description = initialConditionDescription,
      .rule = wxS("ic2(#1#,#2#,#3#,#4#);"),
      .fields = {odeSolution,
                 Field(_("Point the value is known at:"), wxS("t=0")),
                 Field(_("Value y at that point:"), wxS("y=1")),
                 Field(_("Derivate of y at that point:"),
                       wxS("\'diff(y,t)=-1"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_bvp},
      .title = _("Boundary value problem"),
      .description =
        _("The solution of an ODE tells the shape, but not the height "
          "of the solution.\n"
          "If the ODE\'s result is known at two points this "
          "function fills in the correct values for the  constants"),
      .rule = wxS("bc2(#1#,#2#,#3#,#4#,#5#);"),
      .fields = {odeSolution,
                 Field(_("Point #1 with known value:"), wxS("t=0")),
                 Field(_("Value y at that point:"), wxS("y=0")),
                 Field(_("Point #2 with known value:"), wxS("t=1")),
                 Field(_("Value y at that point:"), wxS("y=1"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_rk},
      .title = _("Numerical solution for 1st degree ODE"),
      .description =
        _("Tries to find a numerical solution for a 1st order ODE (or in other "
          "words: a equation of the format depends(x,t);diff(x,t)=(something "
          "containing x and t)"),
      .rule = wxS("rk(#1#,#2#,#3#,[#4#,#5#,#6#,#7#]);"),
      .fields = {
        SelectionField(_("diff(x,t)="),
                       _("Accepts one expression or a list in the format "
                         "[ode1,ode2,...]")),
        Field(_("Name of x:"), wxS("x"),
              _("Accepts one variable or a list in the format "
                "[var1,var2,...]")),
        Field(_("Initial x:"), wxS("1"),
              _("Accepts one variable or a list in the format [1,4,...]")),
        Field(_("Name of t:"), wxS("t")), Field(_("Start of t:"), wxS("0")),
        Field(_("End of t:"), wxS("10")), Field(_("Step width:"), wxS(".1"))}});

  catalog.push_back({
      .ids = {EventIDs::menu_eliminate},
      .title = _("Eliminate a variable"),
      .rule = wxS("eliminate([#1#],[#2#]);"),
      .fields = {SelectionField(_("Equation(s):")),
                 Field(_("Variable(s):"), wxEmptyString)}});
  catalog.push_back({
      .ids = {EventIDs::menu_solve_de},
      .title = _("Solve differential equations using laplace()"),
      .description = _("The solution variable needs to be in the form\n"
                       "   U(t)=1/2*U(t)+3*diff(U(t),t)\n"
                       "for this to work; Initial conditions can be specified using "
                       "atvalue()."),
      .rule = wxS("desolve([#1#],[#2#]);"),
      .fields = {SelectionField(_("Equation(s):")),
                 Field(_("Variable(s):"), wxEmptyString)}});
  catalog.push_back({
      .ids = {EventIDs::menu_atvalue, EventIDs::popid_property_atvalue},
      .title = _("Make a function value at a specific point known"),
      .description = _("Tells maxima for an f(x), that f(x=t)=a"),
      .rule = wxS("atvalue(#1#,#2#,#3#);"),
      .fields = {SelectionField(_("Function f(x):")),
                 Field(_("Point:"), wxS("x=0")), Field(_("Value:"), wxS("0"))}});

  // ------------------------------------------------------------------ Matrix
  // Reading and writing CSV files, entering a matrix, matrixmap() and
  // "Load LAPACK" stay in MaximaCommandMenus::MatrixMenu(): they need
  // dialogs of their own, or no wizard at all.
  const auto matrixColon = SelectionField(_("Matrix:"));
  const auto matrix = SelectionField(_("Matrix"));
  const auto leftMatrix = SelectionField(_("Left Matrix:"));
  const auto rightMatrix = Field(_("Right Matrix:"), wxEmptyString);
  catalog.push_back({.ids = {EventIDs::menu_matrix_row},
                     .title = _("Extract a matrix row"),
                     .rule = wxS("row(#1#,#2#);"),
                     .fields = {matrixColon, Field(_("Row number:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_col},
                     .title = _("Extract a matrix column"),
                     .rule = wxS("col(#1#,#2#);"),
                     .fields = {matrixColon, Field(_("Column number:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_row_list},
                     .title = _("Extract a matrix row as a list"),
                     .rule = wxS("#1#[#2#];"),
                     .fields = {matrixColon, Field(_("Row number:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_col_list},
                     .title = _("Extract a matrix column as a list"),
                     .rule = wxS("transpose(#1#)[#2#];"),
                     .fields = {matrixColon, Field(_("Column number:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_submatrix_columns},
                     .title = _("Remove matrix columns"),
                     .rule = wxS("submatrix(#1#,#2#);"),
                     .fields = {matrixColon, Field(_("Column numbers:"), wxEmptyString)}});
  // submatrix() takes the rows to remove before the matrix.
  catalog.push_back({.ids = {EventIDs::menu_submatrix_rows},
                     .title = _("Remove matrix rows"),
                     .rule = wxS("submatrix(#2#,#1#);"),
                     .fields = {matrixColon, Field(_("Row numbers:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_multiply},
                     .title = _("Multiply two matrices"),
                     .rule = wxS("#1#.#2#;"),
                     .fields = {leftMatrix, rightMatrix}});
  // The labels say "Left/Right Matrix" although the right operand is an
  // exponent; fixing that changes msgids, which is a change of its own.
  catalog.push_back({.ids = {EventIDs::menu_matrix_exponent},
                     .title = _("Matrix Exponent"),
                     .rule = wxS("#1#^^#2#;"),
                     .fields = {leftMatrix, rightMatrix}});
  catalog.push_back({
      .ids = {EventIDs::menu_copymatrix},
      .title = _("Copy a matrix"),
      .description = _("In order to save memory the \":\" operator does clone the matrix, "
                       "not copy it:\n"
                       "If you change an element of one matrix the same element will change "
                       "in all of its clones. copymatrix() instead generates a copy of a "
                       "matrix: "
                       "A new matrix that, if changed in any way, won't change the "
                       "original."),
      .rule = wxS("copymatrix(#1#);"),
      .fields = {matrixColon}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_hadamard_product},
                     .title = _("Hadamard Product"),
                     .description = _("Element-by-element Product of matrices of the same size "
                                       "(Hadamard product)"),
                     .rule = wxS("#1#*#2#;"),
                     .fields = {leftMatrix, rightMatrix}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_hadamard_exponent},
                     .title = _("Hadamard exponent"),
                     .description = _("Element-by-element exponentiation of two matrices"),
                     .rule = wxS("#1#^#2#;"),
                     .fields = {leftMatrix, rightMatrix}});

  // LAPACK. These rules used to end without a terminator, which left the
  // command waiting in its cell for one to be typed.
  const wxString eigenvaluesOnly = _("Calculate the eigenvalues of a matrix numerically");
  const wxString eigenvectors = _("Calculate the eigenvalues and eigenvectors numerically");
  const std::array<std::tuple<wxWindowID, wxString, wxString>, 4> lapackEigen = {{
      {EventIDs::menu_matrix_dgeev_eigenvaluesOnly, eigenvaluesOnly,
       wxS("dgeev(#1#,false,false)[1];")},
      {EventIDs::menu_matrix_dgeev, eigenvectors, wxS("dgeev(#1#,true,true);")},
      {EventIDs::menu_matrix_zgeev_eigenvaluesOnly, eigenvaluesOnly,
       wxS("zgeev(#1#,false,false)[1];")},
      {EventIDs::menu_matrix_zgeev, eigenvectors, wxS("zgeev(#1#,true,true);")}}};
  for (const auto &[wizardId, title, rule] : lapackEigen)
    catalog.push_back({.ids = {wizardId}, .title = title, .rule = rule, .fields = {matrix}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_dgeqrf},
                     .title = _("Numerical QR decomposition of a matrix"),
                     .rule = wxS("dgeqrf(#1#);"),
                     .fields = {matrix}});
  // dgesv(A, b) solves A*x=b.
  catalog.push_back({.ids = {EventIDs::menu_matrix_dgesv},
                     .title = _("Solve A*x=b numerically"),
                     .rule = wxS("dgesv(#1#,#2#);"),
                     .fields = {SelectionField(_("m×n Matrix A:")),
                                Field(_("n×1 Matrix b:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_dgesvd},
                     .title = _("Calculate Singular Value Decomposition, left and right "
                                "singular vectors numerically"),
                     .rule = wxS("dgesvd(#1#,true,true);"),
                     .fields = {matrix}});
  catalog.push_back({.ids = {EventIDs::menu_matrix_dgesvd_valuesOnly},
                     .title = _("Calculate Singular Value Decomposition of a matrix numerically"),
                     .rule = wxS("dgesvd(#1#,false,false)[1];"),
                     .fields = {matrix}});
  // The norms. The z* routines are the complex-matrix versions of the d* ones.
  for (const wxString &prefix : {wxString(wxS("d")), wxString(wxS("z"))}) {
    const bool real = (prefix == wxS("d"));
    catalog.push_back({.ids = {real ? EventIDs::menu_matrix_dlange_max
                                    : EventIDs::menu_matrix_zlange_max},
                       .title = _("Find the maximum absolute value of a matrix entry"),
                       .rule = prefix + wxS("lange('max,#1#);"),
                       .fields = {matrix}});
    catalog.push_back({.ids = {real ? EventIDs::menu_matrix_dlange_one
                                    : EventIDs::menu_matrix_zlange_one},
                       .title = _("Find the maximum sum of the absolute values of a matrix column"),
                       .rule = prefix + wxS("lange('one_norm,#1#);"),
                       .fields = {matrix}});
    catalog.push_back({.ids = {real ? EventIDs::menu_matrix_dlange_inf
                                    : EventIDs::menu_matrix_zlange_inf},
                       .title = _("Find the maximum sum of the absolute values of a matrix row"),
                       .rule = prefix + wxS("lange('inf_norm,#1#);"),
                       .fields = {matrix}});
    catalog.push_back({.ids = {real ? EventIDs::menu_matrix_dlange_frobenius
                                    : EventIDs::menu_matrix_zlange_frobenius},
                       .title = _("Calculate the root of the sum of squares of matrix entries"),
                       .rule = prefix + wxS("lange('frobenius,#1#);"),
                       .fields = {matrix}});
  }

  // The classic matrix operations used to send their command at once, with
  // the selection or % as the matrix (GH #647). The titles are the menu
  // labels, which already are translated.
  const std::array<std::tuple<wxWindowID, wxString, wxString>, 7> classic = {{
      {EventIDs::menu_invert_mat, _("&Invert Matrix"), wxS("invert")},
      {EventIDs::menu_determinant, _("&Determinant"), wxS("determinant")},
      {EventIDs::menu_rank, _("Rank"), wxS("rank")},
      {EventIDs::menu_eigen, _("Eigen&values"), wxS("eigenvalues")},
      {EventIDs::menu_eigvect, _("Eige&nvectors"), wxS("eigenvectors")},
      {EventIDs::menu_adjoint_mat, _("Ad&joint Matrix"), wxS("adjoint")},
      {EventIDs::menu_transpose, _("&Transpose Matrix"), wxS("transpose")}}};
  for (const auto &[wizardId, title, function] : classic)
    catalog.push_back(
        {.ids = {wizardId},
         .title = wxStripMenuCodes(title),
         .rule = MatrixFunctionRule(function),
         .fields = {SelectionField(_("Matrix"),
                                   _("The matrix, or the name of a variable holding one"))}});

  catalog.push_back({.ids = {EventIDs::menu_cpoly},
                     .title = _("Characteristic polynom"),
                     .rule = wxS("expand(charpoly(#1#,#2#));"),
                     .fields = {matrix, Field(_("Variable"), wxEmptyString)}});
  // genmatrix(array, rowMax, colMax, rowMin, colMin): the bottom/top fields
  // are the row range, the right/left fields the column range.
  catalog.push_back({
      .ids = {EventIDs::menu_genmatrix},
      .title = _("Extract matrix from 2D array"),
      .description = _("Extracts a rectangle from a 2D array and converts it to a matrix"),
      .rule = wxS("genmatrix(#1#,#3#,#2#,#5#,#4#);"),
      .fields = {SelectionField(_("Array")), Field(_("Right end"), wxS("10")),
                 Field(_("Bottom end"), wxS("10")), Field(_("Left end"), wxS("0")),
                 Field(_("Top end"), wxS("0"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_gen_mat_lambda},
      .title = _("Generate matrix from a rule"),
      .description = _("Generates a matrix and fills each element with the "
                       "result of the expression \"Rule\"."),
      .rule = GenMatrixFromExpressionRule(),
      .fields = {SelectionField(_("Rule")),
                 Field(_("Row variable"), wxS("i"), _("Counts the rows, from 1")),
                 Field(_("Column variable"), wxS("j"), _("Counts the columns, from 1")),
                 Field(_("Rows"), wxS("6")), Field(_("Columns"), wxS("5"))}});
  // The map() wizards' fields have never started with the selection.
  catalog.push_back({
      .ids = {EventIDs::button_map, EventIDs::menu_map},
      .title = _("Map"),
      .description = _("Runs each element of an object (list, matrix, equation,...) "
                       "through a function individually"),
      .rule = wxS("map(#1#,#2#);"),
      .fields = {Field(_("function"), wxS("sin")),
                 Field(_("Object composed of elements"), wxS("expr"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_map_lambda},
      .title = _("Map an expression"),
      .description = _("Runs each element of an object (list, matrix, equation,...) "
                       "through an expression individually"),
      .rule = wxS("map(lambda([#2#],#1#),#3#);"),
      .fields = {Field(_("Expression"), wxS("sin(i)")),
                 Field(_("Loop variable"), wxS("i"),
                       _("The name of the variable that shall contain the current element")),
                 Field(_("Object composed of elements"), wxS("expr"))}});

  // -------------------------------------------------------------------- List
  // Reading and writing CSV files, sorting and the "storage for the values
  // of variables" wizard stay in MaximaCommandMenus::ListMenu(): they need
  // dialogs of their own.
  const auto listColon = SelectionField(_("List:"));
  catalog.push_back({.ids = {EventIDs::menu_list_create_from_args},
                     .title = _("Extract function arguments"),
                     .rule = wxS("args(#1#)$"),
                     .fields = {SelectionField(_("The function call whose arguments to extract"))}});
  // apply('matrix, list-of-rows) builds a matrix from a nested list.
  catalog.push_back({.ids = {EventIDs::menu_list_list2matrix},
                     .title = _("Nested list to matrix"),
                     .rule = wxS("apply('matrix, #1#);"),
                     .fields = {listColon}});
  // args(matrix) returns the matrix rows as a nested list.
  catalog.push_back({.ids = {EventIDs::menu_list_matrix2list},
                     .title = _("Matrix to nested list"),
                     .rule = wxS("args(#1#);"),
                     .fields = {SelectionField(_("Matrix:"))}});
  // This rule used to end without a terminator.
  catalog.push_back({.ids = {EventIDs::menu_list_create_from_elements},
                     .title = _("Create list from comma-separated elements"),
                     .rule = wxS("[#1#];"),
                     .fields = {SelectionField(_("Comma-separated elements"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_list_create_from_rule},
      .title = _("Create a list from a rule"),
      .rule = wxS("makelist(#1#,#2#,#3#,#4#,#5#);"),
      .fields = {SelectionField(
                     _("Rule:"),
                     _("The rule that explains how to generate the value of a list item.\n"
                       "Might be something like \"i\", \"i^2\" or \"sin(i)\"")),
                 Field(_("Index variable:"), wxS("i"),
                       _("The number of the item which is stepped from \"Index Start\" to "
                         "\"Index End\".")),
                 Field(_("Index Start:"), wxS("1")), Field(_("Index End:"), wxS("100")),
                 Field(_("Index Step:"), wxS("1"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_list_create_from_list},
      .title = _("Create a list from a list"),
      .rule = wxS("makelist(#1#,#2#,#3#);"),
      .fields = {SelectionField(
                     _("Expr:"),
                     _("The ‘j’th element is equal to ‘ev (<expr>, <x>=<list>[j])’ \n"
                       "j are the elements of the source list.\n"
                       "Might be something like \"x=i\"")),
                 Field(_("Index variable:"), wxS("i"),
                       _("The variable the value of the current source item is stored in.")),
                 Field(_("Source list:"), wxS("[1,8,32]"))}});
  catalog.push_back({.ids = {EventIDs::menu_list_length},
                     .title = _("Returns the number of elements of a list"),
                     .rule = wxS("length(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_push},
                     .title = _("Push an element to a list"),
                     .rule = wxS("push(#2#,#1#);"),
                     .fields = {listColon, Field(_("Element:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_list_pop},
                     .title = _("Remove and return the first element of a list"),
                     .rule = wxS("pop(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_reverse},
                     .title = _("Reverses the order of the members of a list"),
                     .rule = wxS("reverse(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_first},
                     .title = _("Returns the first element of a list"),
                     .rule = wxS("first(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_last},
                     .title = _("Returns the last element of a list"),
                     .rule = wxS("last(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_rest},
                     .title = _("Drop the first n list elements"),
                     .description = _("Return the list without its first n elements"),
                     .rule = wxS("rest(#1#,#2#);"),
                     .fields = {listColon, Field(_("n:"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_list_restN},
                     .title = _("Drop the last n list elements"),
                     .description = _("Return the list without its last n elements"),
                     .rule = wxS("rest(#1#,-#2#);"),
                     .fields = {listColon, Field(_("n:"), wxEmptyString)}});
  // rest(list, n) drops the FIRST n elements, so extracting the last n needs
  // rest(list, length(list) - n).
  catalog.push_back({.ids = {EventIDs::menu_list_lastn},
                     .title = _("Extract the last n list elements"),
                     .description = _("Extract the last n elements from a list"),
                     .rule = wxS("rest(#1#,length(#1#)-#2#);"),
                     .fields = {SelectionField(_("List")),
                                Field(_("Number of elements"), wxEmptyString)}});
  catalog.push_back({
      .ids = {EventIDs::menu_list_nth},
      .title = _("Extract the nth list elements"),
      .description = _("Attention: Extracting a random list element isn't efficient for "
                       "long lists."
                       "Iterating over lists using makelist() or for loops is way faster."),
      .rule = wxS("#1#[#2#];"),
      .fields = {SelectionField(_("List")), Field(_("Element number"), wxEmptyString)}});
  // map() takes several lists, too: map(f,[a,b],[c,d]) is [f(a,c),f(b,d)]
  // (GH #647).
  catalog.push_back({
      .ids = {EventIDs::menu_list_map},
      .title = _("Apply a function to each list element"),
      .rule = wxS("map(#1#,#2#);"),
      .fields = {SelectionField(_("function")),
                 Field(_("list(s)"), wxEmptyString,
                       _("One list, or several lists of the same length separated by "
                         "commas: then the function gets one element of each list as "
                         "its arguments."))}});
  // The values have to be wrapped in a list; otherwise several
  // comma-separated equations become the subst(new,old,expr) form and are
  // not substituted.
  catalog.push_back({
      .ids = {EventIDs::menu_list_use_actual_values},
      .title = _("Introduce a list of actual values into an equation"),
      .rule = wxS("subst([#1#],#2#);"),
      .fields = {Field(_("List with values"), wxEmptyString,
                       _("Comma-separated list entry in the format val1=1,val2=2")),
                 SelectionField(_("Equation"))}});
  catalog.push_back({
      .ids = {EventIDs::menu_list_extract_value},
      .title = _("Extract a variable's value from a list of variable values"),
      .rule = wxS("subst([#1#],#2#);"),
      .fields = {SelectionField(_("List"),
                                _("Comma-separated list entry in the format val1=1,val2=2")),
                 Field(_("Variable name"), wxEmptyString)}});
  // The "val1=1,val2=2" tooltip the next two wizards' first field used to
  // have described neither a function name nor the list to loop over.
  catalog.push_back({.ids = {EventIDs::menu_list_as_function_arguments},
                     .title = _("Use a list as parameter list for a function"),
                     .rule = wxS("apply(#1#,#2#);"),
                     .fields = {SelectionField(_("Function name")),
                                Field(_("List"), wxEmptyString)}});
  catalog.push_back({
      .ids = {EventIDs::menu_list_do_for_each_element},
      .title = _("Do for each list element"),
      .rule = wxS("for #2# in #1# do #3#;"),
      .fields = {listColon,
                 Field(_("Iterator name:"), wxS("i"),
                       _("The variable the value of the current source item is stored in.")),
                 Field(_("What to do:"), wxS("disp(i)"),
                       _("Either a single expression or a comma-separated list of expressions "
                         "between parenthesis. In the latter case the result of the last "
                         "expression in the parenthesis is used."))}});
  catalog.push_back({.ids = {EventIDs::menu_list_remove_duplicates},
                     .title = _(" Returns the unique elements of the list"),
                     .rule = wxS("unique(#1#);"),
                     .fields = {listColon}});
  catalog.push_back({.ids = {EventIDs::menu_list_remove_element},
                     .title = _("Remove an element from a list"),
                     .rule = wxS("delete(#1#,#2#);"),
                     .fields = {SelectionField(_("Element")), Field(_("List"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_list_append_item_start},
                     .title = _("Add an element to the start of a list"),
                     .rule = wxS("cons(#1#,#2#);"),
                     .fields = {SelectionField(_("Item")), Field(_("List"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_list_append_item_end},
                     .title = _("Add an element to the end of a list"),
                     .rule = wxS("append(#1#,[#2#]);"),
                     .fields = {SelectionField(_("List")), Field(_("Item"), wxEmptyString)}});
  catalog.push_back({.ids = {EventIDs::menu_list_append_list},
                     .title = _("Append a list to another list"),
                     .rule = wxS("append(#1#,#2#);"),
                     .fields = {SelectionField(_("List #1")), Field(_("List #2"), wxS("[1]"))}});
  catalog.push_back({.ids = {EventIDs::menu_list_interleave},
                     .title = _("Interleave two lists"),
                     .rule = wxS("join(#1#,#2#);"),
                     .fields = {SelectionField(_("List #1")),
                                Field(_("List #2"), wxEmptyString)}});

  // ------------------------------------------------------------- Statistics
  // Reading a matrix from a file and selecting a subsample stay in
  // MaximaCommandMenus::StatsMenu(): they need a file dialog and a command
  // that leaves out what wasn't entered.
  const auto data = SelectionField(_("Data:"));
  catalog.push_back({.ids = {EventIDs::menu_stats_histogram},
                     .title = _("Histogram"),
                     .rule = wxS("wxhistogram(#1#,nclasses=#2#);"),
                     .fields = {data, Field(_("Classes:"), wxS("10"))}});
  catalog.push_back({.ids = {EventIDs::menu_stats_scatterplot},
                     .title = _("Scatterplot"),
                     .rule = wxS("wxscatterplot(#1#,nclasses=#2#);"),
                     .fields = {data, Field(_("Classes:"), wxS("10"))}});
  // These used to start with "%" even when something was selected.
  const std::array<std::tuple<wxWindowID, wxString, wxString>, 7> oneDataField = {{
           {EventIDs::menu_stats_barsplot, _("Plot as bars"), wxS("wxbarsplot")},
           {EventIDs::menu_stats_boxplot, _("Boxplot"), wxS("wxboxplot")},
           {EventIDs::menu_stats_piechart, _("Plot as pie chart"), wxS("wxpiechart")},
           {EventIDs::menu_stats_mean, _("Calculate mean value"), wxS("mean")},
           {EventIDs::menu_stats_median, _("Calculate median value"), wxS("median")},
           {EventIDs::menu_stats_var, _("Calculate variance"), wxS("var")},
           {EventIDs::menu_stats_dev, _("Calculate standard deviation"), wxS("std")}}};
  for (const auto &[id, title, function] : oneDataField)
    catalog.push_back({.ids = {id},
                       .title = title,
                       .rule = function + wxS("(#1#);"),
                       .fields = {data}});
  catalog.push_back({.ids = {EventIDs::menu_stats_tt1},
                     .title = _("One sample t-test"),
                     .rule = wxS("test_mean(#1#,mean=#2#);"),
                     .fields = {SelectionField(_("Sample:")), Field(_("Mean:"), wxS("0"))}});
  catalog.push_back({.ids = {EventIDs::menu_stats_tt2},
                     .title = _("Two sample t-test"),
                     .rule = wxS("test_means_difference(#1#,#2#);"),
                     .fields = {SelectionField(_("Sample 1:")),
                                SelectionField(_("Sample 2:"))}});
  catalog.push_back({.ids = {EventIDs::menu_stats_tnorm},
                     .title = _("Shapiro-Wilk test for normality"),
                     .rule = wxS("test_normality(#1#);"),
                     .fields = {data}});
  // simple_linear_regression() fits a single predictor (a list of x/y
  // pairs), not a multivariate model.
  catalog.push_back({.ids = {EventIDs::menu_stats_linreg},
                     .title = _("Simple linear regression"),
                     .rule = wxS("simple_linear_regression(#1#);"),
                     .fields = {data}});
  catalog.push_back({
      .ids = {EventIDs::menu_stats_lsquares},
      .title = _("Least Squares Fit"),
      .rule = wxS("lsquares_estimates(#1#,[#2#],#3#,[#4#],iprint=[-1,0]);"),
      .fields = {SelectionField(_("Data Matrix:"),
                                _("A matrix in which each row is a set of variables")),
                 Field(_("Col. names:"), wxS("x,y"),
                       _("The list of variables contained in each matrix row")),
                 Field(_("Equation:"), wxS("y=A*x+B"), _("The equation to fit the data to")),
                 Field(_("Variables:"), wxS("A,B"),
                       _("The variables to search the optimum solution for"))}});

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
