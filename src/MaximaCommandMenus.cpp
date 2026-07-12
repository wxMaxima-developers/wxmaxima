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
#include "wizards/ActualValuesStorageWiz.h"
#include "wizards/CsvWiz.h"
#include "wizards/ListSortWiz.h"
#include "wizards/Gen1Wiz.h"
#include "wizards/Gen3Wiz.h"
#include "wizards/Gen4Wiz.h"
#include "wizards/IntegrateWiz.h"
#include "wizards/MatWiz.h"
#include "wizards/LimitWiz.h"
#include "wizards/Plot2dWiz.h"
#include "wizards/Plot3dWiz.h"
#include "wizards/PlotFormatWiz.h"
#include "wizards/SeriesWiz.h"
#include "wizards/SumWiz.h"
#include "wizards/SystemWiz.h"

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

void MaximaCommandMenus::SimplifyMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_nouns){
    m_wxMaxima.CommandWiz(
               _("Evaluate Nouns"),
               _("Maxima allows making functions \"nouns\", which means that they "
                 "aren't automatically evaluated as soon as Maxima encounters them.\n"
                 "Ways to make a function a noun include declaring it a noun, preceding "
                 "it with a single quote or putting it between the parentheses of \'().\n\n"
                 "This command tells Maxima that the nouns in this expression "
                 "shall now be evaluated, too."),
               wxEmptyString, wxS("#1#,nouns;"), _("Expression"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_simpsum){
    m_wxMaxima.CommandWiz(_("Simplify sums"),
               _("Try to simplify sums that result from sum() commands."),
               wxEmptyString, wxS("simpsum(#1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_ratsimp) ||
          (event.GetId() == EventIDs::menu_ratsimp)){
      wxString cmd = wxS("ratsimp(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_radcan) ||
          (event.GetId() == EventIDs::menu_radsimp)) {
    m_wxMaxima.CommandWiz(
               _("Simplify radicals"),
               _("radcan() is a powerful tools for simplification trigonometric "
                 "functions "
                 "but needs to be taken with care: If a function has more than one "
                 "branch "
                 "radcan uses the one that looks like it would fit best, not "
                 "necessarily "
                 "the one that makes sense for the problem that resulted in the "
                 "Expression that is to be simplified."),
               wxEmptyString, wxS("radcan(#1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_to_fact){
      wxString cmd = wxS("makefact(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_gamma){
      wxString cmd = wxS("makegamma(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_factcomb){
      wxString cmd = wxS("factcomb(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_factsimp){
      wxString cmd = wxS("minfactorial(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logcontract){
      wxString cmd = wxS("logcontract(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logexpand){
      wxString cmd = expr + wxS(", logexpand=super;");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logexpand_false){
    m_wxMaxima.MenuCommand(wxS("logexpand:false$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_true){
    m_wxMaxima.MenuCommand(wxS("logexpand:true$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_all){
    m_wxMaxima.MenuCommand(wxS("logexpand:all$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_super){
    m_wxMaxima.MenuCommand(wxS("logexpand:super$"));
  }
  else if((event.GetId() == EventIDs::button_expand) ||
          (event.GetId() == EventIDs::menu_expand)){
      wxString cmd = wxS("expand(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_scsimp){
      wxString cmd = wxS("scsimp(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_xthru){
      wxString cmd = wxS("xthru(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_factor) ||
          (event.GetId() == EventIDs::menu_factor)){
      wxString cmd = wxS("factor(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_expandwrt){
    m_wxMaxima.CommandWiz(_("Expand for variable(s):"), wxEmptyString, wxEmptyString,
               wxS("expandwrt(#1#,#2#);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_subst){
    m_wxMaxima.CommandWiz(
               _("Substitute"),
               _("Subst is a better string-search-and-replace for expressions."),
               // subst() accepts a (list of) equation(s). Without the list
               // brackets several comma-separated equations would be misread as
               // the subst(new,old,expr) form instead of being substituted.
               wxEmptyString, wxS("subst([#2#],#1#);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_ratsubst){
    m_wxMaxima.CommandWiz(_("Smart substitution"),
               _("ratsubst works like subst, but it knows some basic maths, if "
                 "needed."),
               // ratsubst() takes (new, old, expr) and rejects an equation, so
               // use lratsubst(), which accepts a (list of) equation(s).
               wxEmptyString, wxS("lratsubst([#2#],#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_psubst){
    m_wxMaxima.CommandWiz(_("Parallel substitution"),
               _("Substitutes, but makes sure that nothing is substituted into "
                 "the other substituents."),
               // Parallel substitution is psubst(), not ratsubst().
               wxEmptyString, wxS("psubst([#2#],#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u,u=x^2"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_fullratsubst){
    m_wxMaxima.CommandWiz(_("Recursive substitution"),
               _("Substitutes up to lrats_max_iter times, or until the "
                 "expression stops changing when substituting."),
               // fullratsubst() needs the equation(s) wrapped in a list;
               // otherwise comma-separated equations become the (new,old,expr)
               // form and all but the first are silently ignored.
               wxEmptyString, wxS("fullratsubst([#2#],#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_at){
    m_wxMaxima.CommandWiz(
               _("Value at a given point"),
               _("Substitutes, but makes sure that if substituting t=0 in diff(x,t) "
                 "the result isn't 0 (as t no more changes), but %at(diff(x,t),t=0)."),
               // at() expects exactly two arguments: the expression and a
               // (list of) equation(s). Several comma-separated equations have
               // to be wrapped in a list or at() errors out.
               wxEmptyString, wxS("at(#1#,[#2#]);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Substituents"), wxS("x=0"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_substinpart){
    m_wxMaxima.CommandWiz(_("Substitute only in a specific part"),
               _("Replaces the subexpression that part(expr, n_1, n_2, ...) "
                 "would select by a new value. The part numbers form a path: "
                 "n_1 selects a part of the expression, n_2 a part of that, and "
                 "so on."),
               wxEmptyString, wxS("substinpart(#2#,#1#,#3#);"),
               wxS("Expression"), wxS("%"), wxEmptyString, wxS("New value"),
               wxS("z"), _("The expression to substitute in"),
               wxS("Part path"), wxS("1"),
               _("Comma-separated part numbers selecting the subexpression to "
                 "replace (as in part())"));
  }
  else if(event.GetId() == EventIDs::menu_opsubst){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("opsubst:true$");
      else
        cmd = wxS("opsubst:false$");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_expandwrt_denom){
    m_wxMaxima.CommandWiz(_("Expand for variable(s) including denominator:"),
               wxEmptyString, wxEmptyString,
               wxS("expandwrt(#1#,#2#),expandwrt_denom=true;"),
               wxS("Expression"), wxS("%"), wxEmptyString, wxS("Variable(s)"),
               wxS("x"), _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_horner){
      wxString cmd = wxS("horner(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_collapse){
      wxString cmd = wxS("collapse(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_optimize){
      wxString cmd = wxS("optimize(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_mainvar){
    m_wxMaxima.CommandWiz(_("Declare main variable:"), wxEmptyString, wxEmptyString,
               wxS("declare(#1#,mainvar);"), wxS("Variable"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_scanmapfactor){
      wxString cmd = wxS("scanmap('factor,") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_gfactor){
      wxString cmd = wxS("gfactor(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigreduce) ||
          (event.GetId() == EventIDs::menu_trigreduce)){
      wxString cmd = wxS("trigreduce(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigsimp) ||
          (event.GetId() == EventIDs::menu_trigsimp)){
      wxString cmd = wxS("trigsimp(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigexpand) ||
          (event.GetId() == EventIDs::menu_trigexpand)){
      wxString cmd = wxS("trigexpand(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::menu_trigrat) ||
          (event.GetId() == EventIDs::button_trigrat)){
      wxString cmd = wxS("trigrat(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_rectform) ||
          (event.GetId() == EventIDs::menu_rectform)){
      wxString cmd = wxS("rectform(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_polarform){
      wxString cmd = wxS("polarform(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_demoivre){
      wxString cmd = wxS("demoivre(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_exponentialize){
      wxString cmd = wxS("exponentialize(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_realpart){
      wxString cmd = wxS("realpart(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_imagpart){
      wxString cmd = wxS("imagpart(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_talg){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("algebraic:true$");
      else
        cmd = wxS("algebraic:false$");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_tellrat){
    m_wxMaxima.CommandWiz(_("Enter an equation for rational simplification:"),
               wxEmptyString, wxEmptyString, wxS("tellrat(#1#);"),
               wxS("Equation"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_modulus){
    m_wxMaxima.CommandWiz(_("Calculate modulus:"), wxEmptyString, wxEmptyString,
               wxS("modulus : #1#$"), wxS("Modulus"), wxS("%"), wxEmptyString);
  }
}

void MaximaCommandMenus::EquationsMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_allroots){
    m_wxMaxima.CommandWiz(
               _("Solve polynomials numerically"),
               _("Tries to find all solutions of a polynomial numerically.\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    allroots(ratdisrep(taylor(expression,0,30)));"),
               wxEmptyString, wxS("allroots(#1#);"), _("Polynomial:"), expr);
  }
  else if(event.GetId() == EventIDs::menu_bfallroots){
    m_wxMaxima.CommandWiz(
               _("Solve polynomials numerically (bfloats)"),
               _("Tries to find all solutions of a polynomial numerically using "
                 "bfloats.\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    bfallroots(ratdisrep(taylor(expression,0,30)));"),
               wxEmptyString, wxS("bfallroots(#1#);"), _("Polynomial:"), expr);
  }
  else if(event.GetId() == EventIDs::menu_realroots){
    m_wxMaxima.CommandWiz(
               _("Solve polynomials numerically (real roots)"),
               _("Tries to find exact fractions that match the numerical solutions of "
                 "a polynomial.\n"
                 "Is not able to deal with solutions with a imaginary part. "
                 "Numerical constants like %pi% need to be eliminated using float() "
                 "or similar\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    realroots(ratdisrep(taylor(expression,0,30)));\n\n"
                 "See also guess_exact_value()"),
               wxEmptyString, wxS("realroots(#1#,#2#);"), _("Polynomial:"), expr,
               wxEmptyString, _("precision:"), wxS("1e-12"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_solve) ||
          (event.GetId() == EventIDs::menu_solve)){
    m_wxMaxima.CommandWiz(_("Solve equation(s)"),
               _("solve() will solve a list of equations only if for n "
                 "independent equations there are n variables to solve to.\n"
                 "If only one result variable is of interest the other result "
                 "variables solve needs to do its work can be used to tell "
                 "solve() which variables to eliminate in the solution "
                 "for the interesting variable."),
               wxEmptyString, wxS("solve([#1#],[#2#]);"), _("Equation(s)"),
               expr, _("Comma-separated equations"), _("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_solve_to_poly){
    m_wxMaxima.CommandWiz(
               _("Solve equations to polynom"),
               _(wxS("The function to_poly_solve tries to solve the equations "
                     "e for the variables l. The equation(s) e can either be a "
                     "single expression or a set or list of expressions; "
                     "similarly, l can either be a single symbol or a list of "
                     "set of symbols. When a member of e isn’t explicitly an "
                     "equation, for example x^2 -1, the solver assumes that the "
                     "expression vanishes.")),
               wxEmptyString, wxS("to_poly_solve([#1#],[#2#]);"), _("Equation(s)"),
               expr, _("Comma-separated equations"), _("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_solve_num) {
    if (expr.StartsWith(wxS("%")))
      expr = wxS("''(") + expr + wxS(")");
    m_wxMaxima.CommandWiz(
      _("Solve equations numerically"),
      _("Tries to find a value of the variable that solves the equation between the two bonds"), wxEmptyString,
      wxT("find_root(#1#,#2#,#3#,#4#);"),
      _("Equation:"), expr, wxEmptyString,
      _("Variable:"), wxT("x"), wxEmptyString,
      _("Lower bound:"), wxT("-1"), wxEmptyString,
      _("Upper bound:"), wxT("1"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_solve_ode) ||
          (event.GetId() == EventIDs::menu_solve_ode)) {
    m_wxMaxima.CommandWiz(_("Solve ODE"),
               _("solves an equation of the form\n    'diff(y,t) = -y;"),
               _("The solution of an ODE describes the general shape of the "
                 "resulting curve. The actual height of that curve is defined "
                 "by the initial condition or boundary values, later on."),
               wxS("ode2(#1#,#2#,#3#);"), _("Equation:"), expr, wxEmptyString,
               _("y:"), wxS("y"), wxEmptyString, _("t:"), wxS("t"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ivp_1){
    m_wxMaxima.CommandWiz(_("Initial Condition"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s state is known at a point this "
                 "function fills in the correct values for the constants"),
               wxEmptyString, wxS("ic1(#1#,#2#,#3#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point the value is known at:"), wxS("t=0"), wxEmptyString,
               _("Value at that point:"), wxS("y=1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ivp_2){
    m_wxMaxima.CommandWiz(_("Initial Condition"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s state is known at a point this "
                 "function fills in the correct values for the constants"),
               wxEmptyString, wxS("ic2(#1#,#2#,#3#,#4#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point the value is known at:"), wxS("t=0"), wxEmptyString,
               _("Value y at that point:"), wxS("y=1"), wxEmptyString,
               _("Derivate of y at that point:"), wxS("\'diff(y,t)=-1"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_bvp){
    m_wxMaxima.CommandWiz(_("Boundary value problem"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s result is known at two points this "
                 "function fills in the correct values for the  constants"),
               wxEmptyString, wxS("bc2(#1#,#2#,#3#,#4#,#5#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point #1 with known value:"), wxS("t=0"), wxEmptyString,
               _("Value y at that point:"), wxS("y=0"), wxEmptyString,
               _("Point #2 with known value:"), wxS("t=1"), wxEmptyString,
               _("Value y at that point:"), wxS("y=1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_rk){
    m_wxMaxima.CommandWiz(
               _("Numerical solution for 1st degree ODE"),
               _("Tries to find a numerical solution for a 1st order ODE (or in other "
                 "words: a equation of the format depends(x,t);diff(x,t)=(something "
                 "containing x and t)"),
               wxEmptyString, wxS("rk(#1#,#2#,#3#,[#4#,#5#,#6#,#7#]);"),
               _("diff(x,t)="), expr,
               _("Accepts one expression or a list in the format [ode1,ode2,...]"),
               _("Name of x:"), wxS("x"),
               _("Accepts one variable or a list in the format [var1,var2,...]"),
               _("Initial x:"), wxS("1"),
               _("Accepts one variable or a list in the format [1,4,...]"),
               _("Name of t:"), wxS("t"), wxEmptyString, _("Start of t:"), wxS("0"),
               wxEmptyString, _("End of t:"), wxS("10"), wxEmptyString,
               _("Step width:"), wxS(".1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_eliminate){
    m_wxMaxima.CommandWiz(_("Eliminate a variable"), wxEmptyString, wxEmptyString,
               wxS("eliminate([#1#],[#2#]);"), _("Equation(s):"), expr,
               wxEmptyString, _("Variable(s):"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_solve_algsys){
    GetTextFromUser(_("Number of equations:"), _("Solve algebraic system"),
                    &m_wxMaxima.m_configuration, wxS("3"), &m_wxMaxima, [this](wxString sz) {
                      if (sz.Length() == 0)
                        return;
                      long isz;
                      if (!sz.ToLong(&isz) || isz <= 0) {
                        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                                          wxOK | wxICON_ERROR);
                        return;
                      }
                      wxWindowPtr<SysWiz> wiz(new SysWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration,
                                                         _("Solve algebraic system"), isz));
                      // wiz->Centre(wxBOTH);
                      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
                        if (retcode == wxID_OK) {
                          wxString cmd = wxS("algsys") + wiz->GetValue();
                          m_wxMaxima.MenuCommand(cmd);
                        }
                      });
                    });
  }
  else if(event.GetId() == EventIDs::menu_solve_lin){
    GetTextFromUser(_("Number of equations:"), _("Solve linear system"),
                    &m_wxMaxima.m_configuration, wxS("3"), &m_wxMaxima, [this](wxString sz) {
                      if (sz.Length() == 0)
                        return;
                      long isz;
                      if (!sz.ToLong(&isz) || isz <= 0) {
                        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                                          wxOK | wxICON_ERROR);
                        return;
                      }
                      wxWindowPtr<SysWiz> wiz(new SysWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Solve linear system"), isz));
                      // wiz->Centre(wxBOTH);
                      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
                        if (retcode == wxID_OK) {
                          wxString cmd = wxS("linsolve") + wiz->GetValue();
                          m_wxMaxima.MenuCommand(cmd);
                        }
                      });
                    });
  }
  else if(event.GetId() == EventIDs::menu_solve_de){
    m_wxMaxima.CommandWiz(_("Solve differential equations using laplace()"),
               _("The solution variable needs to be in the form\n"
                 "   U(t)=1/2*U(t)+3*diff(U(t),t)\n"
                 "for this to work; Initial conditions can be specified using "
                 "atvalue()."),
               wxEmptyString, wxS("desolve([#1#],[#2#]);"), _("Equation(s):"),
               expr, wxEmptyString, _("Variable(s):"), wxEmptyString,
               wxEmptyString);
  }
  else if((event.GetId() == EventIDs::menu_atvalue) ||
          (event.GetId() == EventIDs::popid_property_atvalue)) {
    m_wxMaxima.CommandWiz(_("Make a function value at a specific point known"),
               _("Tells maxima for an f(x), that f(x=t)=a"), wxEmptyString,
               wxS("atvalue(#1#,#2#,#3#);"), _("Function f(x):"), expr,
               wxEmptyString, _("Point:"), wxS("x=0"), wxEmptyString,
               _("Value:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_lhs) {
    wxString cmd = wxS("lhs(") + expr + wxS(");");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_rhs) {
    wxString cmd = wxS("rhs(") + expr + wxS(");");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_construct_fraction){
    m_wxMaxima.CommandWiz(_("Construct a fraction"), wxEmptyString, wxEmptyString,
               wxS("((#1#)/(#2#))"), _("Enumerator:"), expr, wxEmptyString,
               _("Denominator:"), wxS("1"), wxEmptyString);
  }
}

void MaximaCommandMenus::MatrixMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_csv2mat){
    wxWindowPtr<CsvImportWiz> wiz(new CsvImportWiz(&m_wxMaxima, &m_wxMaxima.m_configuration));
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd = wxS("read_matrix(\"") + wiz->GetFilename() + wxS("\", ") +
          wiz->GetSeparator() + wxS(");");
        m_wxMaxima.MenuCommand(cmd);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_mat2csv){
    wxWindowPtr<CsvExportWiz> wiz(new CsvExportWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, _("Matrix")));
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd = wxS("write_data(") + wiz->GetMatrix() + wxS(", \"") +
          wiz->GetFilename() + wxS("\", ") + wiz->GetSeparator() + wxS(");");
        m_wxMaxima.MenuCommand(cmd);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_matrix_row) {
    m_wxMaxima.CommandWiz(_("Extract a matrix row"), wxEmptyString, wxEmptyString,
               wxS("row(#1#,#2#);"), _("Matrix:"), expr, wxEmptyString,
               _("Row number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_col){
    m_wxMaxima.CommandWiz(_("Extract a matrix column"), wxEmptyString, wxEmptyString,
               wxS("col(#1#,#2#);"), _("Matrix:"), expr, wxEmptyString,
               _("Column number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_row_list){
    m_wxMaxima.CommandWiz(_("Extract a matrix row as a list"), wxEmptyString,
               wxEmptyString, wxS("#1#[#2#];"), _("Matrix:"), expr,
               wxEmptyString, _("Row number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_col_list){
    m_wxMaxima.CommandWiz(_("Extract a matrix column as a list"), wxEmptyString,
               wxEmptyString, wxS("transpose(#1#)[#2#];"), _("Matrix:"), expr,
               wxEmptyString, _("Column number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_submatrix_columns){
    m_wxMaxima.CommandWiz(_("Remove matrix columns"), wxEmptyString, wxEmptyString,
               wxS("submatrix(#1#,#2#);"), _("Matrix:"), expr,
               wxEmptyString, _("Column numbers:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_submatrix_rows){
    m_wxMaxima.CommandWiz(_("Remove matrix rows"), wxEmptyString, wxEmptyString,
               wxS("submatrix(#2#,#1#);"), _("Matrix:"), expr,
               wxEmptyString, _("Row numbers:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_multiply){
    m_wxMaxima.CommandWiz(_("Multiply two matrices"), wxEmptyString, wxEmptyString,
               wxS("#1#.#2#;"), _("Left Matrix:"), expr, wxEmptyString,
               _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_exponent){
    m_wxMaxima.CommandWiz(_("Matrix Exponent"), wxEmptyString, wxEmptyString,
               wxS("#1#^^#2#;"), _("Left Matrix:"), expr, wxEmptyString,
               _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_copymatrix){
    m_wxMaxima.CommandWiz(
               _("Copy a matrix"),
               _("In order to save memory the \":\" operator does clone the matrix, "
                 "not copy it:\n"
                 "If you change an element of one matrix the same element will change "
                 "in all of its clones. copymatrix() instead generates a copy of a "
                 "matrix: "
                 "A new matrix that, if changed in any way, won't change the "
                 "original."),
               wxEmptyString, wxS("copymatrix(#1#);"), _("Matrix:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_hadamard_product){
    m_wxMaxima.CommandWiz(_("Hadamard Product"),
               _("Element-by-element Product of matrices of the same size "
                 "(Hadamard product)"),
               wxEmptyString, wxS("#1#*#2#;"), _("Left Matrix:"), expr,
               wxEmptyString, _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_hadamard_exponent){
    m_wxMaxima.CommandWiz(_("Hadamard exponent"),
               _("Element-by-element exponentiation of two matrices"),
               wxEmptyString, wxS("#1#^#2#;"), _("Left Matrix:"), expr,
               wxEmptyString, _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_loadLapack){
    m_wxMaxima.MenuCommand(wxS("load(\"lapack\");"));
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeev_eigenvaluesOnly){
    m_wxMaxima.CommandWiz(_("Calculate the eigenvalues of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("dgeev(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeev){
    m_wxMaxima.CommandWiz(_("Calculate the eigenvalues and eigenvectors numerically"),
               wxEmptyString, wxEmptyString, wxS("dgeev(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zgeev_eigenvaluesOnly){
    m_wxMaxima.CommandWiz(_("Calculate the eigenvalues of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("zgeev(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zgeev){
    m_wxMaxima.CommandWiz(_("Calculate the eigenvalues and eigenvectors numerically"),
               wxEmptyString, wxEmptyString, wxS("zgeev(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeqrf){
    m_wxMaxima.CommandWiz(_("Numerical QR decomposition of a matrix"), wxEmptyString,
               wxEmptyString, wxS("dgeqrf(#1#)"), _("Matrix"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesv){
    // dgesv(A, b) solves A*x=b. The old template emitted "dgesv(A,true,true)",
    // dropping the b matrix the dialog asks for and passing two bogus args.
    m_wxMaxima.CommandWiz(_("Solve A*x=b numerically"), wxEmptyString, wxEmptyString,
               wxS("dgesv(#1#,#2#)"), _("m×n Matrix A:"), expr,
               wxEmptyString, _("n×1 Matrix b:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesvd){
    m_wxMaxima.CommandWiz(_("Calculate Singular Value Decomposition, left and right "
                 "singular vectors numerically"),
               wxEmptyString, wxEmptyString, wxS("dgesvd(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesvd_valuesOnly){
    m_wxMaxima.CommandWiz(
               _("Calculate Singular Value Decomposition of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("dgesvd(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_max){
    m_wxMaxima.CommandWiz(_("Find the maximum absolute value of a matrix entry"),
               wxEmptyString, wxEmptyString, wxS("dlange('max,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_one){
    m_wxMaxima.CommandWiz(
               _("Find the maximum sum of the absolute values of a matrix column"),
               wxEmptyString, wxEmptyString, wxS("dlange('one_norm,#1#)"), _("Matrix"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_inf){
    m_wxMaxima.CommandWiz(_("Find the maximum sum of the absolute values of a matrix row"),
               wxEmptyString, wxEmptyString, wxS("dlange('inf_norm,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_frobenius){
    m_wxMaxima.CommandWiz(_("Calculate the root of the sum of squares of matrix entries"),
               wxEmptyString, wxEmptyString, wxS("dlange('frobenius,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_max){
    // The z* LAPACK routines are the complex-matrix versions; these used to
    // emit dlange() (the real version), duplicating the dlange_* menu items.
    m_wxMaxima.CommandWiz(_("Find the maximum absolute value of a matrix entry"),
               wxEmptyString, wxEmptyString, wxS("zlange('max,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_one){
    m_wxMaxima.CommandWiz(
               _("Find the maximum sum of the absolute values of a matrix column"),
               wxEmptyString, wxEmptyString, wxS("zlange('one_norm,#1#)"), _("Matrix"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_inf){
    m_wxMaxima.CommandWiz(_("Find the maximum sum of the absolute values of a matrix row"),
               wxEmptyString, wxEmptyString, wxS("zlange('inf_norm,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_frobenius){
    m_wxMaxima.CommandWiz(_("Calculate the root of the sum of squares of matrix entries"),
               wxEmptyString, wxEmptyString, wxS("zlange('frobenius,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zheev){
  }

  else if(event.GetId() == EventIDs::menu_invert_mat){
      wxString cmd = wxS("invert(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_determinant){
      wxString cmd = wxS("determinant(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rank){
      wxString cmd = wxS("rank(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_eigen){
      wxString cmd = wxS("eigenvalues(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_eigvect){
      wxString cmd = wxS("eigenvectors(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_adjoint_mat){
      wxString cmd = wxS("adjoint(") + expr + wxS(");");
      m_wxMaxima.MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_transpose){
    wxString cmd = wxS("transpose(") + expr + wxS(");");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_map_mat){
    wxWindowPtr<Gen3Wiz> wiz(new Gen3Wiz(_("Resulting Matrix name (may be empty):"), _("Function:"),
                                         _("Matrix:"), wxEmptyString, wxEmptyString, expr,
                                         &m_wxMaxima.m_configuration, &m_wxMaxima, -1, _("Matrix map")));
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd;
        if (!wiz->GetValue1().IsEmpty())
          cmd = wiz->GetValue1() + wxS(": ");
        cmd += wxS("matrixmap(") + wiz->GetValue2() + wxS(", ") +
          wiz->GetValue3() + wxS(");");
        m_wxMaxima.MenuCommand(cmd);
      }
    });
  }
  else if((event.GetId() == EventIDs::menu_enter_mat) ||
          (event.GetId() == EventIDs::menu_stats_enterm)){
      wxWindowPtr<MatDim> wiz(new MatDim(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Matrix")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd;
          if (wiz->GetValue0() != wxEmptyString)
            cmd = wiz->GetValue0() + wxS(": ");
          long w = 0, h = 0;
          int type = wiz->GetMatrixType();
          if (!(wiz->GetValue1()).ToLong(&h) || !(wiz->GetValue2()).ToLong(&w) ||
              w <= 0 || h <= 0) {
            LoggingMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                              wxOK | wxICON_ERROR);
            return; //-V773
          }
          if (w != h)
            type = MatWiz::MATRIX_GENERAL;
          wxWindowPtr<MatWiz> mwiz(new MatWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Enter matrix"),
                                              type, h, w));
          // wiz->Centre(wxBOTH);
          mwiz->ShowWindowModalThenDo([this, mwiz, cmd](int retcode) {
            if (retcode == wxID_OK) {
              m_wxMaxima.MenuCommand(cmd + mwiz->GetValue());
            }
          });
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_cpoly){
    m_wxMaxima.CommandWiz(_("Characteristic polynom"), wxEmptyString, wxEmptyString,
               wxS("expand(charpoly(#1#,#2#));"), _("Matrix"), expr,
               wxEmptyString, _("Variable"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_genmatrix){
    m_wxMaxima.CommandWiz(
               _("Extract matrix from 2D array"),
               _("Extracts a rectangle from a 2D array and converts it to a matrix"),
               // genmatrix(array, rowMax, colMax, rowMin, colMin): the bottom/top
               // fields are the row range, the right/left fields the column range.
               wxEmptyString, wxS("genmatrix(#1#,#3#,#2#,#5#,#4#);"), _("Array"), expr,
               wxEmptyString, _("Right end"), wxS("10"), wxEmptyString,
               _("Bottom end"), wxS("10"), wxEmptyString, _("Left end"), wxS("0"),
               wxEmptyString, _("Top end"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gen_mat_lambda){
    m_wxMaxima.CommandWiz(
               _("Generate matrix from a rule"),
               _("Generates a matrix and fills each element with the "
                 "result of the expression \"Rule\"."),
               wxEmptyString,
               wxS("apply('matrix,makelist(makelist(#1#,#3#,1,#4#),#2#,1,#5#));"),
               _("Rule"), expr, wxEmptyString, _("Var #1"), wxS("i"), wxEmptyString,
               _("Var #2"), wxS("j"), wxEmptyString, _("Columns"), wxS("5"),
               wxEmptyString, _("Rows"), wxS("6"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_map) ||
          (event.GetId() == EventIDs::menu_map)){
    m_wxMaxima.CommandWiz(_("Map"),
               _("Runs each element of an object (list, matrix, equation,...) "
                 "through a function individually"),
               wxEmptyString, wxS("map(#1#,#2#);"), _("function"), wxS("sin"),
               wxEmptyString, _("Object composed of elements"), wxS("expr"));
  }
  else if(event.GetId() == EventIDs::menu_map_lambda){
    m_wxMaxima.CommandWiz(
               _("Map an expression"),
               _("Runs each element of an object (list, matrix, equation,...) "
                 "through an expression individually"),
               wxEmptyString, wxS("map(lambda([#2#],#1#),#3#);"), _("Expression"),
               wxS("sin(i)"), wxEmptyString, _("Loop variable"), wxS("i"),
               _("The name of the variable that shall contain the current element"),
               _("Object composed of elements"), wxS("expr"));
  }
}

void MaximaCommandMenus::ListMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_csv2list){
      wxWindowPtr<CsvImportWiz> wiz(new CsvImportWiz(&m_wxMaxima, &m_wxMaxima.m_configuration));
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("read_nested_list(\"") + wiz->GetFilename() + wxS("\", ") +
            wiz->GetSeparator() + wxS(");");
          m_wxMaxima.MenuCommand(cmd);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_list2csv){
      wxWindowPtr<CsvExportWiz> wiz(new CsvExportWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, _("List")));
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("write_data(") + wiz->GetMatrix() + wxS(", \"") +
            wiz->GetFilename() + wxS("\", ") + wiz->GetSeparator() + wxS(");");
          m_wxMaxima.MenuCommand(cmd);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_args){
    m_wxMaxima.CommandWiz(_("Extract function arguments"), wxEmptyString, wxEmptyString,
               wxS("args(#1#)$"),
               _("The function call whose arguments to extract"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_list2matrix){
    // apply('matrix, list-of-rows) builds a matrix from a nested list; the
    // title used to claim the opposite direction.
    m_wxMaxima.CommandWiz(_("Nested list to matrix"), wxEmptyString, wxEmptyString,
               wxS("apply('matrix, #1#);"), _("List:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_matrix2list){
    // args(matrix) returns the matrix rows as a nested list; the title used to
    // claim the opposite direction.
    m_wxMaxima.CommandWiz(_("Matrix to nested list"), wxEmptyString, wxEmptyString,
               wxS("args(#1#);"), _("Matrix:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_elements){
    m_wxMaxima.CommandWiz(_("Create list from comma-separated elements"), wxEmptyString,
               wxEmptyString, wxS("[#1#]"), _("Comma-separated elements"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_rule){
    m_wxMaxima.CommandWiz(
               _("Create a list from a rule"), wxEmptyString, wxEmptyString,
               wxS("makelist(#1#,#2#,#3#,#4#,#5#);"), _("Rule:"), expr,
               _("The rule that explains how to generate the value of a list item.\n"
                 "Might be something like \"i\", \"i^2\" or \"sin(i)\""),
               _("Index variable:"), wxS("i"),
               _("The number of the item which is stepped from \"Index Start\" to "
                 "\"Index End\"."),
               _("Index Start:"), wxS("1"), wxEmptyString, _("Index End:"), wxS("100"),
               wxEmptyString, _("Index Step:"), wxS("1"), wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_list_create_from_list){
    m_wxMaxima.CommandWiz(
               _("Create a list from a list"), wxEmptyString, wxEmptyString,
               wxS("makelist(#1#,#2#,#3#);"), _("Expr:"), expr,
               _("The ‘j’th element is equal to ‘ev (<expr>, <x>=<list>[j])’ \n"
                 "j are the elements of the source list.\n"
                 "Might be something like \"x=i\""),
               _("Index variable:"), wxS("i"),
               _("The variable the value of the current source item is stored in."),
               _("Source list:"), wxS("[1,8,32]"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_actual_values_storage){
      wxWindowPtr<ActualValuesStorageWiz> wiz(new ActualValuesStorageWiz(
                                                                         &m_wxMaxima.m_configuration, &m_wxMaxima, -1,
                                                                         _("Create a list as a storage for the values of variables")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.MenuCommand(wiz->GetValue());
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_list_sort){
      wxWindowPtr<ListSortWiz> wiz(new ListSortWiz(&m_wxMaxima.m_configuration, &m_wxMaxima, -1, _("Sort a list"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.MenuCommand(wiz->GetValue());
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_list_length){
    m_wxMaxima.CommandWiz(_("Returns the number of elements of a list"), wxEmptyString, wxEmptyString,
               wxS("length(#1#);"), _("List:"), expr, wxEmptyString);

  }
  else if(event.GetId() == EventIDs::menu_list_push){
    m_wxMaxima.CommandWiz(_("Push an element to a list"), wxEmptyString, wxEmptyString,
               wxS("push(#2#,#1#);"), _("List:"), expr, wxEmptyString,
               _("Element:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_pop){
    m_wxMaxima.CommandWiz(_("Remove and return the first element of a list"), wxEmptyString, wxEmptyString,
               wxS("pop(#1#);"), _("List:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_reverse){
    m_wxMaxima.CommandWiz(_("Reverses the order of the members of a list"), wxEmptyString, wxEmptyString,
               wxS("reverse(#1#);"), _("List:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_first){
    m_wxMaxima.CommandWiz(_("Returns the first element of a list"), wxEmptyString, wxEmptyString,
               wxS("first(#1#);"), _("List:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_last){
    m_wxMaxima.CommandWiz(_("Returns the last element of a list"), wxEmptyString, wxEmptyString,
               wxS("last(#1#);"), _("List:"), expr, wxEmptyString);

  }
  else if(event.GetId() == EventIDs::menu_list_rest){
    m_wxMaxima.CommandWiz(_("Drop the first n list elements"),
               _("Return the list without its first n elements"), wxEmptyString,
               wxS("rest(#1#,#2#);"), _("List:"), expr, wxEmptyString, _("n:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_restN){
    m_wxMaxima.CommandWiz(_("Drop the last n list elements"),
               _("Return the list without its last n elements"), wxEmptyString,
               wxS("rest(#1#,-#2#);"), _("List:"), expr, wxEmptyString, _("n:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_lastn){
    // rest(list, n) drops the FIRST n elements, so extracting the last n needs
    // rest(list, length(list) - n). The old template returned the wrong slice.
    m_wxMaxima.CommandWiz(_("Extract the last n list elements"),
               _("Extract the last n elements from a list"), wxEmptyString,
               wxS("rest(#1#,length(#1#)-#2#);"), _("List"), expr, wxEmptyString,
               _("Number of elements"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_nth){
    m_wxMaxima.CommandWiz(
               _("Extract the nth list elements"),
               _("Attention: Extracting a random list element isn't efficient for "
                 "long lists."
                 "Iterating over lists using makelist() or for loops is way faster."),
               wxEmptyString, wxS("#1#[#2#];"), _("List"), expr, wxEmptyString,
               _("Element number"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_map){
    m_wxMaxima.CommandWiz(_("Apply a function to each list element"), wxEmptyString,
               wxEmptyString, wxS("map(#1#,#2#);"), _("function"), expr,
               wxEmptyString, _("list"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_use_actual_values){
    m_wxMaxima.CommandWiz(_("Introduce a list of actual values into an equation"),
               // The values have to be wrapped in a list; otherwise several
               // comma-separated equations become the subst(new,old,expr) form
               // and are not substituted.
               wxEmptyString, wxEmptyString, wxS("subst([#1#],#2#);"),
               _("List with values"), wxEmptyString,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Equation"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_extract_value){
    m_wxMaxima.CommandWiz(_("Extract a variable's value from a list of variable values"),
               // The list of values has to be bracketed so all of them are used
               // (a bare comma-separated list is misread as subst(new,old,expr)).
               wxEmptyString, wxEmptyString, wxS("subst([#1#],#2#);"), _("List"),
               expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Variable name"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_as_function_arguments){
    m_wxMaxima.CommandWiz(_("Use a list as parameter list for a function"), wxEmptyString,
               wxEmptyString, wxS("apply(#1#,#2#);"), _("Function name"), expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_do_for_each_element){
    m_wxMaxima.CommandWiz(
               _("Do for each list element"), wxEmptyString, wxEmptyString,
               wxS("for #2# in #1# do #3#;"), _("List:"), expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Iterator name:"), wxS("i"),
               _("The variable the value of the current source item is stored in."),
               _("What to do:"), wxS("disp(i)"),
               _("Either a single expression or a comma-separated list of expressions "
                 "between parenthesis. In the latter case the result of the last "
                 "expression in the parenthesis is used."));
  }
  else if(event.GetId() == EventIDs::menu_list_remove_duplicates){
    m_wxMaxima.CommandWiz(_(" Returns the unique elements of the list"), wxEmptyString, wxEmptyString,
               wxS("unique(#1#);"), _("List:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_remove_element){
    m_wxMaxima.CommandWiz(_("Remove an element from a list"), wxEmptyString, wxEmptyString,
               wxS("delete(#1#,#2#);"), _("Element"), expr, wxEmptyString,
               _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_item_start){
    m_wxMaxima.CommandWiz(_("Add an element to the start of a list"), wxEmptyString,
               wxEmptyString, wxS("cons(#1#,#2#);"), _("Item"), expr,
               wxEmptyString, _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_item_end){
    m_wxMaxima.CommandWiz(_("Add an element to the end of a list"), wxEmptyString,
               wxEmptyString, wxS("append(#1#,[#2#]);"), _("List"), expr,
               wxEmptyString, _("Item"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_list){
    m_wxMaxima.CommandWiz(_("Append a list to another list"), wxEmptyString, wxEmptyString,
               wxS("append(#1#,#2#);"), _("List #1"), expr, wxEmptyString,
               _("List #2"), wxS("[1]"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_interleave){
    m_wxMaxima.CommandWiz(_("Interleave two lists"), wxEmptyString, wxEmptyString,
               wxS("join(#1#,#2#);"), _("List #1"), expr, wxEmptyString,
               _("List #2"), wxEmptyString, wxEmptyString);
  }
}

void MaximaCommandMenus::PropertiesMenu(wxCommandEvent &event) {
  event.Skip();
  if(!m_wxMaxima.GetWorksheet())
    return;
  EditorCell *editor = m_wxMaxima.GetWorksheet()->GetActiveCell();
  if (editor == NULL)
    return;
  wxString obj = editor->GetWordUnderCaret();
  if (obj.IsEmpty())
    obj = editor->GetSelectionString();

  if(event.GetId() == EventIDs::popid_property_real){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", real") + wxS(")$"));
  }
  else if(event.GetId() == EventIDs::popid_property_imaginary){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", imaginary)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_complex){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", complex)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_additive){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", additive)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_alphabetic){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", alphabetic)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_bindtest){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", bindtest)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_antisymmetric){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", antisymmetric)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_commutative){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", commutative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_symmetric){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", symmetric)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_constant){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", constant)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_even){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", even)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_odd){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", odd)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evenfun){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", evenfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_oddfun){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", oddfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_increasing){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", increasing)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_decreasing){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", decreasing)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_integer){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", integer)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_noninteger){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", noninteger)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_integervalued){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", integervalued)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_lassociative){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", lassociative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_rassociative){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", rassociative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_linear){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", linear)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_mainvar){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", mainvar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_multiplicative){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", multiplicative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nary){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", nary)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nonarray){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", nonarray)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nonscalar){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", nonscalar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_scalar){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", scalar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_noun){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", noun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_outative){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", outative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_posfun){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", posfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_rational){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", rational)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_irrational){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", irrational)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evfun){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", evfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evflag){
    m_wxMaxima.MenuCommand(wxS("declare(") + obj + wxS(", evflag)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_greaterThan){
    m_wxMaxima.CommandWiz(_("Assume a value range for a variable"), wxEmptyString,
               wxEmptyString, wxS("assume(#1#)"), _("Variable"),
               obj + wxS(">0"), wxEmptyString);
  }
}

void MaximaCommandMenus::StatsMenu(wxCommandEvent &event) {
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();

  if(event.GetId() == EventIDs::menu_stats_histogram){
    m_wxMaxima.CommandWiz(_("Histogram"), wxEmptyString, wxEmptyString,
               wxS("wxhistogram(#1#,nclasses=#2#);"), _("Data:"), expr,
               wxEmptyString, _("Classes:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_scatterplot){
    m_wxMaxima.CommandWiz(_("Scatterplot"), wxEmptyString, wxEmptyString,
               wxS("wxscatterplot(#1#,nclasses=#2#);"), _("Data:"), expr,
               wxEmptyString, _("Classes:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_barsplot){
    m_wxMaxima.CommandWiz(_("Plot as bars"), wxEmptyString, wxEmptyString,
               wxS("wxbarsplot(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_boxplot){
    m_wxMaxima.CommandWiz(_("Boxplot"), wxEmptyString, wxEmptyString,
               wxS("wxboxplot(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_piechart){
    m_wxMaxima.CommandWiz(_("Plot as pie chart"), wxEmptyString, wxEmptyString,
               wxS("wxpiechart(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_mean){
    m_wxMaxima.CommandWiz(_("Calculate mean value"), wxEmptyString, wxEmptyString,
               wxS("mean(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_median){
    m_wxMaxima.CommandWiz(_("Calculate median value"), wxEmptyString, wxEmptyString,
               wxS("median(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_var){
    m_wxMaxima.CommandWiz(_("Calculate variance"), wxEmptyString, wxEmptyString,
               wxS("var(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_dev){
    m_wxMaxima.CommandWiz(_("Calculate standard deviation"), wxEmptyString, wxEmptyString,
               wxS("std(#1#);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tt1){
    m_wxMaxima.CommandWiz(_("One sample t-test"), wxEmptyString, wxEmptyString,
               wxS("test_mean(#1#,mean=#2#);"), _("Sample:"), expr,
               wxEmptyString, _("Mean:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tt2){
    m_wxMaxima.CommandWiz(_("Two sample t-test"), wxEmptyString, wxEmptyString,
               wxS("test_means_difference(#1#,#2#);"), _("Sample 1:"), expr,
               wxEmptyString, _("Sample 2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tnorm){
    m_wxMaxima.CommandWiz(_("Shapiro-Wilk test for normality"), wxEmptyString,
               wxEmptyString, wxS("test_normality(#1#);"), _("Data:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_linreg){
    m_wxMaxima.CommandWiz(_("Simple linear regression"), wxEmptyString,
               // simple_linear_regression() fits a single predictor (a list of
               // x/y pairs), not a multivariate model.
               wxEmptyString, wxS("simple_linear_regression(#1#);"), _("Data:"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_lsquares){
    m_wxMaxima.CommandWiz(
               _("Least Squares Fit"), wxEmptyString, wxEmptyString,
               wxS("lsquares_estimates(#1#,[#2#],#3#,[#4#],iprint=[-1,0]);"),
               _("Data Matrix:"), expr,
               _("A matrix in which each row is a set of variables"), _("Col. names:"),
               wxS("x,y"), _("The list of variables contained in each matrix row"),
               _("Equation:"), wxS("y=A*x+B"), _("The equation to fit the data to"),
               _("Variables:"), wxS("A,B"),
               _("The variables to search the optimum solution for"));
  }
  else if(event.GetId() == EventIDs::menu_stats_readm){
      wxString file = wxFileSelector(
                                     _("Open matrix"), m_wxMaxima.m_lastPath, wxEmptyString, wxEmptyString,
                                     _("Data file (*.csv, *.tab, *.txt)|*.csv;*.tab;*.txt"), wxFD_OPEN);
      if (file != wxEmptyString) {
        m_wxMaxima.m_lastPath = wxPathOnly(file);

#if defined __WXMSW__
        file.Replace(wxS("\\"), wxS("/"));
#endif

        wxString name =
          wxGetTextFromUser(wxS("Enter matrix name:"), wxS("Matrix name"));
        wxString cmd;

        if (name != wxEmptyString)
          cmd << name << wxS(": ");

        wxString format;
        if (file.Lower().EndsWith(wxS(".csv")))
          format = wxS("csv");
        else if (file.Lower().EndsWith(wxS(".tab")))
          format = wxS("tab");

        if (format != wxEmptyString)
          m_wxMaxima.MenuCommand(cmd + wxS("read_matrix(\"") + file + wxS("\", '") + format +
                      wxS(");"));
        else
          m_wxMaxima.MenuCommand(cmd + wxS("read_matrix(\"") + file + wxS("\");"));
      }
    }
  else if(event.GetId() == EventIDs::menu_stats_subsample){
      wxWindowPtr<Gen4Wiz> wiz(new Gen4Wiz(
                                           _("Data Matrix:"), _("Condition:"), _("Include columns:"),
                                           _("Matrix name:"), expr, wxS("col[1]#'NA"), wxEmptyString,
                                           wxEmptyString, &m_wxMaxima.m_configuration, &m_wxMaxima, -1, _("Select Subsample"), true));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString name = wiz->GetValue4();

          wxString cmd;

          if (name != wxEmptyString)
            cmd << name << wxS(": ");

          cmd += wxS("subsample(\n   ") + wiz->GetValue1() + wxS(",\n   ") +
            wxS("lambda([col], is( ");

          if (wiz->GetValue2() != wxEmptyString)
            cmd += wiz->GetValue2() + wxS(" ))");
          else
            cmd += wxS("true ))");

          if (wiz->GetValue3() != wxEmptyString)
            cmd += wxS(",\n   ") + wiz->GetValue3();

          cmd += wxS(");");
          m_wxMaxima.MenuCommand(cmd);
        }
      });
  }
}
