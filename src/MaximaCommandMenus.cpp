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
  Implementation of the menu-command handlers extracted from wxMaxima - see
  MaximaCommandMenus.h.
*/

#include "MaximaCommandMenus.h"

#include "wxMaxima.h"
#include "EventIDs.h"
#include <wx/windowptr.h>
#include "wizards/IntegrateWiz.h"
#include "wizards/LimitWiz.h"
#include "wizards/Plot2dWiz.h"
#include "wizards/Plot3dWiz.h"
#include "wizards/PlotFormatWiz.h"
#include "wizards/SeriesWiz.h"
#include "wizards/SumWiz.h"

void MaximaCommandMenus::PlotMenu(wxCommandEvent &event) {
  if (!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if ((event.GetId() == EventIDs::button_plot3) ||
      (event.GetId() == EventIDs::gp_plot3)) {
    wxWindowPtr<Plot3DWiz> wiz(
      new Plot3DWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Plot 3D")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        m_wxMaxima.MenuCommand(val);
      }
    });
  } else if (event.GetId() == EventIDs::menu_animationautostart) {
    if (event.IsChecked())
      m_wxMaxima.MenuCommand(wxS("wxanimate_autoplay:true$"));
    else
      m_wxMaxima.MenuCommand(wxS("wxanimate_autoplay:false$"));
  } else if (event.GetId() == EventIDs::menu_animationframerate) {
    m_wxMaxima.CommandWiz(_("Enter new animation frame rate [Hz, integer]:"),
                          wxEmptyString, wxEmptyString,
                          wxS("wxanimate_framerate : #1#$"), _("Frame rate"),
                          wxS("%"), wxEmptyString);
  } else if ((event.GetId() == EventIDs::button_plot2) ||
             (event.GetId() == EventIDs::gp_plot2)) {
    wxWindowPtr<Plot2DWiz> wiz(
      new Plot2DWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Plot 2D")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        m_wxMaxima.MenuCommand(val);
      }
    });
  } else if (event.GetId() == EventIDs::menu_plot_format) {
    wxWindowPtr<PlotFormatWiz> wiz(new PlotFormatWiz(
      &m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Plot format")));
    wiz->Center(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        m_wxMaxima.MenuCommand(wiz->GetValue());
      }
    });
  }
}

void MaximaCommandMenus::CalculusMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_change_var){
    m_wxMaxima.CommandWiz(
               _("Change variable"),
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
                   "2*integrate(u*f(u^2),u,1,sqrt(10))\n\n"),
               wxEmptyString, wxS("changevar(#1#,#2#,#3#,#4#);"), _("Integral/Sum:"),
               expr, wxEmptyString, _("Equation:"), wxS("u=sqrt(x)"), wxEmptyString,
               _("New variable:"), wxS("u"), wxEmptyString, _("Old variable:"),
               wxS("x"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_change_var_evaluate){
    m_wxMaxima.CommandWiz(
               _("Change variable and evaluate"),
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
               wxEmptyString, wxS("changevar(#1#,#2#,#3#,#4#),nouns;"),
               _("Integral/Sum:"), expr, wxEmptyString, _("Equation:"),
               wxS("u=sqrt(x)"), wxEmptyString, _("New variable:"), wxS("u"),
               wxEmptyString, _("Old variable:"), wxS("x"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_pade){
    m_wxMaxima.CommandWiz(_("Pade approximation"), wxEmptyString, wxEmptyString,
               wxS("pade(#1#,#2#,#3#);"), _("Taylor series:"), expr,
               wxEmptyString, _("Num. deg:"), wxS("4"), wxEmptyString,
               _("Denom. deg:"), wxS("4"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_taylor){
    m_wxMaxima.CommandWiz(_("Taylor series"),
               _("Approximates a expression around a point as a polynom\n"
                 "The trailing \"...\" can be removed by using ratdisrep()"),
               wxEmptyString, wxS("taylor(#1#,#2#,#3#,#4#);"), _("Expression:"),
               expr, wxEmptyString, _("Variable:"), wxS("x"), wxEmptyString,
               _("Point:"), wxS("0"), wxEmptyString, _("Degree:"), wxS("3"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_powerseries){
    m_wxMaxima.CommandWiz(_("Power series"), _("Approximates a expression as a polynom"),
               // The niceindices( ... ) parenthesis has to be closed, too:
               // the old template emitted "niceindices(powerseries(e,x,0);"
               // which Maxima rejects with "incorrect syntax: Missing )".
               wxEmptyString, wxS("niceindices(powerseries(#1#,#2#,#3#));"),
               _("Expression:"), expr, wxEmptyString, _("Variable:"), wxS("x"),
               wxEmptyString, _("point:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_fourier){
      wxString loadCmd;
      if (!m_wxMaxima.m_fourierLoaded)
        loadCmd = wxS("load(\"fourie\")$\n");
      m_wxMaxima.CommandWiz(_("Fourier coefficients"),
                 _("Calculates the fourier coefficients for the expression from "
                   "-p to p"),
                 wxEmptyString, loadCmd + wxS("fourier(#1#,#2#,#3#);"),
                 _("Expression:"), expr, wxEmptyString, _("Variable:"), wxS("x"),
                 wxEmptyString, _("Range radius:"), wxS("2"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_continued_fraction){
      wxString cmd = wxS("cfdisrep(cf(") + expr + wxS("));");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_lcm){
    m_wxMaxima.CommandWiz(_("LCM"), wxEmptyString, wxEmptyString, wxS("lcm(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gcd){
    m_wxMaxima.CommandWiz(_("GCD"), wxEmptyString, wxEmptyString, wxS("gcd(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_divide){
    m_wxMaxima.CommandWiz(_("GCD"), wxEmptyString, wxEmptyString, wxS("divide(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_partfrac){
    m_wxMaxima.CommandWiz(_("Partial Fractions"), wxEmptyString, wxEmptyString,
               wxS("partfrac(#1#,#2#);"), _("Expression:"), expr, wxEmptyString,
               _("Variable:"), wxS("n"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_risch){
    m_wxMaxima.CommandWiz(_("Integrate (risch)"), wxEmptyString, wxEmptyString,
               wxS("risch(#1#,#2#);"), _("Expression:"), expr, wxEmptyString,
               _("Variable:"), wxS("x"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_integrate) ||
          (event.GetId() == EventIDs::menu_integrate)){
      wxWindowPtr<IntegrateWiz> wiz(new IntegrateWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Integrate")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_laplace){
    m_wxMaxima.CommandWiz(_("Laplace"), wxEmptyString, wxEmptyString,
               wxS("laplace(#1#,#2#,#3#);"), _("Expression:"), expr,
               wxEmptyString, _("Old variable:"), wxS("t"), wxEmptyString,
               _("New variable:"), wxS("s"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ilt){
    m_wxMaxima.CommandWiz(_("Inverse Laplace"), wxEmptyString, wxEmptyString,
               wxS("ilt(#1#,#2#,#3#);"), _("Expression:"), expr, wxEmptyString,
               _("Old variable:"), wxS("s"), wxEmptyString, _("New variable:"),
               wxS("t"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_diff) ||
          (event.GetId() == EventIDs::menu_diff)){
    m_wxMaxima.CommandWiz(_("Differentiate"), _("Differentiates an expression n times"),
               wxEmptyString, wxS("diff(#1#,#2#,#3#);"), _("Expression:"), expr,
               wxEmptyString, _("Variable(s):"), wxS("x"), wxEmptyString,
               _("Times:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::button_taylor){
      wxWindowPtr<SeriesWiz> wiz(new SeriesWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Series")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if((event.GetId() == EventIDs::button_limit) ||
          (event.GetId() == EventIDs::menu_limit)){
      wxWindowPtr<LimitWiz> wiz(new LimitWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Limit")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_lbfgs){
    m_wxMaxima.CommandWiz(_("Find minimum"),
               _("Allows varying the parameters of a function until it fits "
                 "experimental data."),
               wxEmptyString, wxS("lbfgs(#1#,#2#,#3#,#4#,[1,1]);"),
               _("Expression:"), expr, wxEmptyString, _("Variables:"), wxS("x"),
               wxEmptyString, _("Initial estimates:"), wxS("1.0"),
               wxEmptyString, _("Epsilon:"), wxS("1e-4"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_sum) ||
          (event.GetId() == EventIDs::menu_sum)){
      wxWindowPtr<SumWiz> wiz(new SumWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Sum")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if((event.GetId() == EventIDs::button_product) ||
          (event.GetId() == EventIDs::menu_product)){
    m_wxMaxima.CommandWiz(_("Product"), wxEmptyString, wxEmptyString,
               wxS("product(#1#,#2#,#3#,#4#);"), _("Expression:"), expr,
               wxEmptyString, _("Variable:"), wxS("k"), wxEmptyString,
               _("From:"), wxS("1"), wxEmptyString, _("To:"), wxS("n"),
               wxEmptyString);
  }
}

void MaximaCommandMenus::NumericalMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  wxString integralSign = wxS("∫");
  if (!m_wxMaxima.m_configuration.FontRendersChar(L'\u222B', *wxNORMAL_FONT)) //  \u222B = integral sign
    integralSign = wxS("integrate");

  if(event.GetId() == EventIDs::popid_special_constant_percent){
      m_wxMaxima.m_configuration.SetKeepPercent(event.IsChecked());
      m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_hideasterisk){
      m_wxMaxima.m_configuration.HidemultiplicationSign(event.IsChecked());
      m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_changeasterisk){
      m_wxMaxima.m_configuration.SetChangeAsterisk(event.IsChecked());
      m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::menu_num_domain){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("domain:'complex$");
      else
        cmd = wxS("domain:'real$");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_float){
      wxString cmd = wxS("float(") + expr + wxS("), numer;");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_bfloat){
      wxString cmd = wxS("bfloat(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_numer){
      wxString cmd = expr + wxS(",numer;");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rat){
      wxString cmd = wxS("rat(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rationalize){
      wxString cmd = wxS("rationalize(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_guess_exact_value){
      wxString cmd = wxS("guess_exact_value(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_num_out) {
    if (!event.IsChecked())
      m_wxMaxima.MenuCommand("numer:false$");
    else
      m_wxMaxima.MenuCommand("numer:true$");
  }
  else if(event.GetId() == EventIDs::menu_stringdisp){
      wxString cmd;
      if (!event.IsChecked())
        cmd = wxS("stringdisp:false$");
      else
        cmd = wxS("stringdisp:true$");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_set_precision){
    m_wxMaxima.CommandWiz(_("Enter new precision for bigfloats:"), wxEmptyString,
               wxEmptyString, wxS("fpprec : #1#$"), _("Precision"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_set_displayprecision){
    m_wxMaxima.CommandWiz(_("Displayed Precision"), wxEmptyString, wxEmptyString,
               wxS("fpprintprec : #1#$"), _("How many digits to show:"),
               wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_engineeringFormat){
    if ((m_wxMaxima.m_maximaVariable_engineeringFormat != wxS("true")) &&
        (m_wxMaxima.m_maximaVariable_engineeringFormat != wxS("false")))
      m_wxMaxima.MenuCommand(wxS("load(\"engineering-format\")$"));
    if (m_wxMaxima.m_maximaVariable_engineeringFormat == wxS("true"))
      m_wxMaxima.MenuCommand(wxS("engineering_format_floats:false$"));
    if (m_wxMaxima.m_maximaVariable_engineeringFormat == wxS("false"))
      m_wxMaxima.MenuCommand(wxS("engineering_format_floats:true$"));
  }
  else if(event.GetId() == EventIDs::menu_engineeringFormatSetup){
    m_wxMaxima.CommandWiz(_("Setup the engineering format"), wxEmptyString, wxEmptyString,
               wxS("engineering_format_floats: #1#$\n"
                   "engineering_format_min: #2#$\n"
                   "engineering_format_max: #3#$\n"
                   "fpprintprec: #4#$"),
               _("Enable:"), wxS("true"), wxEmptyString,
               _("Minimum absolute value printed without exponent:"),
               wxS(".01"), wxEmptyString,
               _("Maximum absolute value printed without exponent"),
               wxS("1000"), wxEmptyString,
               _("Maximum number of digits to be displayed:"), wxS("6"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qag){
      m_wxMaxima.CommandWiz(
                 integralSign + _("(f(x),x,a,b)), Strategy of Aind"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qag(#1#,#2#,#3#,#4#,#5#,'epsrel=#6#,'epsabs=#7#,'limit=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString,
                 wxS("b"), wxS("10"), wxEmptyString,
                 wxS("key"), wxS("4"), _("An integer between 1..6; Higher numbers work better for oscillating integrands"),
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qags){
      m_wxMaxima.CommandWiz(
                 integralSign + _("(f(x),x,a,b)), Epsilon algorithm"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qags(#1#,#2#,#3#,#4#,'epsrel=#5#,'epsabs=#6#,'limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString,
                 wxS("b"), wxS("10"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qagi){
      m_wxMaxima.CommandWiz(
                 integralSign + _("(f(x),x,a,b), (semi-) infinite interval"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qagi(#1#,#2#,#3#,#4#,'epsrel=#5#,'epsabs=#6#,'limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString,
                 wxS("b"), wxS("10"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qawc){
      m_wxMaxima.CommandWiz(
                 _("Cauchy principal value of f(x)/(x-c), finite interval"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qawc(#1#,#2#,#3#,#4#,'epsrel=#5#,'epsabs=#6#,'limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("c"), wxS("4"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString,
                 wxS("b"), wxS("10"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qawf_sin){
      m_wxMaxima.CommandWiz(integralSign + wxS("(f(x)*sin(ω·x),x,a,∞)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawf(#1#,#2#,#3#,#4#,'sin,'epsabs=#5#,'limit=#6#,'maxp1=#7#,'limlst=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("a"), wxEmptyString,
                 wxS("ω"), wxS("2"), wxEmptyString,
                 wxS("epsabs"), wxS("1d-10"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. (limit - limlst)/2 is the maximum number of subintervals to use."),
                 wxS("maxp1"), wxS("100"), _("Maximum number of Chebyshev moments. Must be greater than 0."),
                 wxS("limlst"), wxS("10"), _("Upper bound on the number of cycles. Must be greater than or equal to 3."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qawf_cos){
      m_wxMaxima.CommandWiz(integralSign + wxS("(f(x)*cos(ω·x),x,a,∞)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawf(#1#,#2#,#3#,#4#,'cos,'epsabs=#5#,'limit=#6#,'maxp1=#7#,'limlst=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("a"), wxEmptyString,
                 wxS("ω"), wxS("2"), wxEmptyString,
                 wxS("epsabs"), wxS("1d-10"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. (limit - limlst)/2 is the maximum number of subintervals to use."),
                 wxS("maxp1"), wxS("100"), _("Maximum number of Chebyshev moments. Must be greater than 0."),
                 wxS("limlst"), wxS("10"), _("Upper bound on the number of cycles. Must be greater than or equal to 3."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qawo_sin){
      m_wxMaxima.CommandWiz(integralSign + wxS("(f(x)*sin(ω·x),x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawo(#1#,#2#,#3#,#4#,#5#,'sin,'epsrel=#6#,'epsabs=#7#,'limit=#8#,'maxp1=#9#,'limlst=#10#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("a"), wxEmptyString,
                 wxS("b"), wxS("b"), wxEmptyString,
                 wxS("ω"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit/2 is the maximum number of subintervals to use."),
                 wxS("maxp1"), wxS("100"), _("Maximum number of Chebyshev moments. Must be greater than 0."),
                 wxS("limlst"), wxS("10"), _("Upper bound on the number of cycles. Must be greater than or equal to 3."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qawo_cos){
      m_wxMaxima.CommandWiz(integralSign + wxS("(f(x)*cos(ω·x),x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawo(#1#,#2#,#3#,#4#,#5#,'cos,'epsrel=#6#,'epsabs=#7#,'limit=#8#,'maxp1=#9#,'limlst=#10#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("a"), wxEmptyString,
                 wxS("ω"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit/2 is the maximum number of subintervals to use."),
                 wxS("maxp1"), wxS("100"), _("Maximum number of Chebyshev moments. Must be greater than 0."),
                 wxS("limlst"), wxS("10"), _("Upper bound on the number of cycles. Must be greater than or equal to 3."));
  }

  else if(event.GetId() == EventIDs::menu_quad_qaws1){
      m_wxMaxima.CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β,x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,#5#,#6#,1,'epsrel=#7#,'epsabs=#8#,'limit=#9#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString,
                 wxS("b"), wxS("2"), wxEmptyString,
                 wxS("α"), wxS("-0.5"), wxEmptyString,
                 wxS("β"), wxS("-0.5"), wxEmptyString, // Values for alpha and beta from the example in the documentation
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws2){
      m_wxMaxima.CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,#5#,#6#,2,'epsrel=#7#,'epsabs=#8#,'limit=#9#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString,
                 wxS("b"), wxS("2"), wxEmptyString,
                 wxS("α"), wxS("-0.5"), wxEmptyString,
                 wxS("β"), wxS("-0.5"), wxEmptyString, // Values for alpha and beta from the example in the documentation
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws3){
      m_wxMaxima.CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(b-x),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,#5#,#6#,3,'epsrel=#7#,'epsabs=#8#,'limit=#9#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString,
                 wxS("b"), wxS("2"), wxEmptyString,
                 wxS("α"), wxS("-0.5"), wxEmptyString,
                 wxS("β"), wxS("-0.5"), wxEmptyString, // Values for alpha and beta from the example in the documentation
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws4){
      m_wxMaxima.CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a)·log(b-x),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,#5#,#6#,4,'epsrel=#7#,'epsabs=#8#,'limit=#9#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString,
                 wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString,
                 wxS("b"), wxS("2"), wxEmptyString,
                 wxS("α"), wxS("-0.5"), wxEmptyString,
                 wxS("β"), wxS("-0.5"), wxEmptyString, // Values for alpha and beta from the example in the documentation
                 wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
                 wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
                 wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
  else if(event.GetId() == EventIDs::menu_quad_qagp){
    m_wxMaxima.CommandWiz(
               integralSign + _("(f(x),x,y) with singularities+discontinuities"),
               wxEmptyString, wxEmptyString,
               wxS("quad_qagp(#1#,#2#,#3#,#4#,[#5#],'epsrel=#6#,'epsabs=#7#,'limit=#8#)"),
               wxS("f(x)"), wxS("%"), wxEmptyString,
               wxS("x"), wxS("x"), wxEmptyString,
               wxS("a"), wxS("1"), wxEmptyString,
               wxS("b"), wxS("2"), wxEmptyString,
               wxS("points"), wxS(".5,.75"), wxEmptyString,
               wxS("epsrel"), wxS("1d-8"), _("Desired relative error of approximation."),
               wxS("epsabs"), wxS("0"), _("Desired absolute error of approximation."),
               wxS("limit"), wxS("200"), _("Size of internal work array. limit is the maximum number of subintervals to use."));
  }
}
