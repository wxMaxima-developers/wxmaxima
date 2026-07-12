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
#include <wx/colordlg.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/process.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include "wizards/ActualValuesStorageWiz.h"
#include "wizards/CsvWiz.h"
#include "wizards/DrawWiz.h"
#include "wizards/Gen2Wiz.h"
#include "wizards/ListSortWiz.h"
#include "wizards/Gen1Wiz.h"
#include "wizards/GenWiz.h"
#include "cells/AnimationCell.h"
#include "cells/EditorCell.h"
#include "cells/ImgCell.h"
#include "cells/ImgCellBase.h"
#include "sidebars/TableOfContents.h"
#include "sidebars/VariablesPane.h"
#include "graphical_io/Printout.h"
#include "dialogs/AboutDialog.h"
#include "dialogs/ConfigDialogue.h"
#include "dialogs/DiffFrame.h"
#include "dialogs/FindReplaceDialog.h"
#include "dialogs/LoggingMessageDialog.h"
#include "dialogs/MaxSizeChooser.h"
#include "dialogs/ResolutionChooser.h"
#include "sidebars/History.h"
#include "dialogs/ChangeLogDialog.h"
#include "dialogs/LicenseDialog.h"
#include "wizards/Gen3Wiz.h"
#include "wizards/Gen4Wiz.h"
#include "wizards/IntegrateWiz.h"
#include "wizards/MatWiz.h"
#include "wizards/LimitWiz.h"
#include "wizards/Plot2dWiz.h"
#include "wizards/Plot3dWiz.h"
#include "wizards/PlotFormatWiz.h"
#include "wizards/SeriesWiz.h"
#include "wizards/SubstituteWiz.h"
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

void MaximaCommandMenus::DrawMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  if (!m_wxMaxima.m_drawPane)
    return;

  m_wxMaxima.UpdateDrawPane();
  int dimensions = m_wxMaxima.m_drawPane->GetDimensions();

  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr;
  if (dimensions < 2)
    expr = m_wxMaxima.GetDefaultEntry();
  else
    expr = "%";

  if(event.GetId() == EventIDs::menu_draw_2d){
      wxWindowPtr<DrawWiz> wiz(new DrawWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, 2));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.GetWorksheet()->OpenHCaret(wiz->GetValue());
          m_wxMaxima.GetWorksheet()->GetActiveCell()->SetCaretPosition(
                                                         m_wxMaxima.GetWorksheet()->GetActiveCell()->GetCaretPosition() - 3);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_3d){
    if (dimensions < 2) {
      wxWindowPtr<DrawWiz> wiz(new DrawWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, 3));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.GetWorksheet()->OpenHCaret(wiz->GetValue());
          m_wxMaxima.GetWorksheet()->GetActiveCell()->SetCaretPosition(
                                                         m_wxMaxima.GetWorksheet()->GetActiveCell()->GetCaretPosition() - 3);
        }
      });
    } else {
      wxWindowPtr<Wiz3D> wiz(new Wiz3D(&m_wxMaxima, &m_wxMaxima.m_configuration));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
      });
    }
  }
  else if(event.GetId() == EventIDs::menu_draw_fgcolor){
      wxColour col = wxGetColourFromUser(&m_wxMaxima);
      if (col.IsOk())
        m_wxMaxima.AddDrawParameter("color=\"" + col.GetAsString(wxC2S_HTML_SYNTAX) + "\"");
  }
  else if(event.GetId() == EventIDs::menu_draw_fillcolor){
      wxColour col = wxGetColourFromUser(&m_wxMaxima);
      if (col.IsOk())
        m_wxMaxima.AddDrawParameter("fill_color=\"" + col.GetAsString(wxC2S_HTML_SYNTAX) + "\"");
  }
  else if(event.GetId() == EventIDs::menu_draw_title){
      wxWindowPtr<Gen1Wiz> wiz(new Gen1Wiz(
                                           &m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Set the diagram title"),
                                           _("Title (Sub- and superscripts as x_{10} or x^{10})"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("title=\"") + wiz->GetValue() + wxS("\"");
          m_wxMaxima.AddDrawParameter(std::move(cmd));
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_key){
      wxWindowPtr<Gen1Wiz> wiz(new Gen1Wiz(
                                           &m_wxMaxima, -1, &m_wxMaxima.m_configuration,
                                           _("Set the next plot's title. Empty = no title."),
                                           _("Title (Sub- and superscripts as x_{10} or x^{10})"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("key=\"") + wiz->GetValue() + wxS("\"");
          m_wxMaxima.AddDrawParameter(std::move(cmd));
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_explicit){
      wxWindowPtr<ExplicitWiz> wiz(new ExplicitWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, expr, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_implicit){
      wxWindowPtr<ImplicitWiz> wiz(new ImplicitWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, expr, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_parametric){
      wxWindowPtr<ParametricWiz> wiz(new ParametricWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_points){
      wxWindowPtr<WizPoints> wiz(new WizPoints(&m_wxMaxima, &m_wxMaxima.m_configuration, dimensions, expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_grid){
      wxWindowPtr<Gen2Wiz> wiz(new Gen2Wiz(
                                           _("x direction [in multiples of the tick frequency]"),
                                           _("y direction [in multiples of the tick frequency]"), "1", "1",
                                           &m_wxMaxima.m_configuration, &m_wxMaxima, -1, _("Set the grid density.")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd =
            wxS("grid=[") + wiz->GetValue1() + "," + wiz->GetValue2() + wxS("]");
          m_wxMaxima.AddDrawParameter(std::move(cmd));
        }
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_axis){
      wxWindowPtr<AxisWiz> wiz(new AxisWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.AddDrawParameter(wiz->GetValue());
        }
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_contour){
      wxWindowPtr<WizContour> wiz(new WizContour(&m_wxMaxima, &m_wxMaxima.m_configuration));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue(), 3);
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_accuracy){
      wxWindowPtr<WizDrawAccuracy> wiz(new WizDrawAccuracy(&m_wxMaxima, &m_wxMaxima.m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz, dimensions](int retcode) {
        if (retcode == wxID_OK)
          m_wxMaxima.AddDrawParameter(wiz->GetValue(), dimensions);
      });
  }
  m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
}

void MaximaCommandMenus::HelpMenu(wxCommandEvent &event) {
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();

  if(event.GetId() == EventIDs::menu_goto_url){
      wxWindowPtr<GenWiz> wiz(new GenWiz(&m_wxMaxima, &m_wxMaxima.m_configuration, m_wxMaxima.GetWorksheet()->GetMaximaManual(),
                                         _("Go to URL"), wxEmptyString, wxEmptyString, wxEmptyString,
                                         _("URL"), wxEmptyString, wxEmptyString));
      // wiz->Centre(wxBOTH);
#ifdef USE_WEBVIEW
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_wxMaxima.m_helpPane->SetURL((*wiz)[0]);
          m_wxMaxima.wxMaximaFrame::ShowPane(EventIDs::menu_pane_help);
        }
      });
#endif
    }
  else if(event.GetId() == wxID_ABOUT){
    AboutDialog aboutdlg(&m_wxMaxima, &m_wxMaxima.m_configuration);
    }
  else if(event.GetId() == EventIDs::menu_license){
      LicenseDialog *dlg = new LicenseDialog(&m_wxMaxima);
      dlg->Show();
    }

  else if(event.GetId() == EventIDs::menu_changelog){
      ChangeLogDialog *dlg = new ChangeLogDialog(&m_wxMaxima);
      dlg->Show();
    }
  else if(event.GetId() == EventIDs::menu_help_demo_for_command){
    m_wxMaxima.MenuCommand(wxS("demo(\"") + expr + wxS("\");"));
    }
  else if(event.GetId() == wxID_HELP){
    m_wxMaxima.ShowHelp(expr);
  }

  else if(event.GetId() == EventIDs::menu_wxmaximahelp){
    m_wxMaxima.ShowWxMaximaHelp();
  }

  else if(event.GetId() == EventIDs::menu_maximahelp){
    m_wxMaxima.ShowMaximaHelpWithoutAnchor();
  }

  else if(event.GetId() == EventIDs::menu_example){
    m_wxMaxima.CommandWiz(_("Show an example for the command:"), wxEmptyString,
               wxEmptyString, wxS("example(#1#);"), _("Command:"), wxS("%"),
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_apropos){
    m_wxMaxima.CommandWiz(_("Apropos"), wxEmptyString, wxEmptyString, wxS("apropos(#1#);"),
               _("Show all commands similar to:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_wxmaxima_uses_help_browser){
    m_wxMaxima.m_configuration.InternalHelpBrowser(false);
  }
  else if(event.GetId() == EventIDs::menu_wxmaxima_uses_help_sidebar){
    m_wxMaxima.m_configuration.InternalHelpBrowser(true);
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_internal_help){
    m_wxMaxima.m_configuration.MaximaUsesHtmlBrowser(false);
    m_wxMaxima.m_configuration.MaximaUsesWxmaximaBrowser(false);
    m_wxMaxima.MenuCommand(wxS("output_format_for_help:'text$"));
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_html_help){
    m_wxMaxima.m_configuration.MaximaUsesHtmlBrowser(true);
    m_wxMaxima.m_configuration.MaximaUsesWxmaximaBrowser(false);
    m_wxMaxima.MenuCommand(wxS("output_format_for_help:'html$"));
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_wxmaxima_help){
    m_wxMaxima.m_configuration.MaximaUsesWxmaximaBrowser(true);
    m_wxMaxima.MenuCommand(wxS("output_format_for_help:'frontend$"));
  }

  else if(event.GetId() == EventIDs::menu_show_tip){
    m_wxMaxima.ShowTip(true);
  }

  else if(event.GetId() == EventIDs::menu_build_info){
    m_wxMaxima.MenuCommand(wxS("wxbuild_info()$"));
  }

  else if(event.GetId() == EventIDs::menu_bug_report){
    m_wxMaxima.MenuCommand(wxS("wxbug_report()$"));
  }

  else if(event.GetId() == EventIDs::menu_help_tutorials){
    wxLaunchDefaultBrowser(wxS("https://wxMaxima-developers.github.io/wxmaxima/help.html"));
  }
  else if(event.GetId() == EventIDs::menu_help_maxima_homepage){
    wxLaunchDefaultBrowser(wxS("https://maxima.sourceforge.io/documentation.html"));
  }
  else if(event.GetId() == EventIDs::menu_check_updates){
    m_wxMaxima.CheckForUpdates(true);
  }
#ifdef __WXMSW__
  else if(event.GetId() == EventIDs::menu_register_wxmx_difftool){
    m_wxMaxima.RegisterWxmxDiffTool();
  }
#endif
}

void MaximaCommandMenus::FileMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  bool forceSave = false;

  if((event.GetId() == wxID_EXIT) || (event.GetId() == wxID_CLOSE)) {
    m_wxMaxima.CallAfter([this]{m_wxMaxima.Close();});
  }
  else if(event.GetId() == wxID_OPEN) {
    if (m_wxMaxima.SaveNecessary()) {
      int close = m_wxMaxima.SaveDocumentP();

      if (close == wxID_CANCEL)
        return;

      if (close == wxID_YES) {
        if (!m_wxMaxima.SaveFile())
          return;
      }
    }

    wxString file =
      wxFileSelector(_("Open"), m_wxMaxima.m_lastPath, wxEmptyString, wxEmptyString,
                     _("All openable types (*.wxm, *.wxmx, *.mac, *.out, "
                       "*.xml)|*.wxm;*.wxmx;*.mac;*.out;*.xml|"
                       "wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx|"
                       "Maxima session (*.mac)|*.mac|"
                       "Xmaxima session (*.out)|*.out|"
                       "xml from broken .wxmx (*.xml)|*.xml"),
                     wxFD_OPEN);

    if (!file.empty()) {
      // On the mac the "File/New" menu item by default opens a new window instead of
      // reusing the old one.
#ifdef __WXOSX__
      if (m_wxMaxima.GetWorksheet()->IsEmpty())
        m_wxMaxima.OpenFile(file, wxEmptyString);
      else
        wxGetApp().NewWindow(file);
#else
      m_wxMaxima.OpenFile(file, wxEmptyString);
#endif
    }
  }
  else if(event.GetId() == wxID_SAVEAS) {
    forceSave = true;
    m_wxMaxima.m_fileSaved = false;
    m_wxMaxima.SaveFile(forceSave);
    // Seems like resetting the title on "file/save as" is a little bit
    // sluggish, otherwise.
    m_wxMaxima.ResetTitle(m_wxMaxima.GetWorksheet()->IsSaved(), true);
  }
  else if(event.GetId() == wxID_SAVE) {
    m_wxMaxima.SaveFile(forceSave);
    // Seems like resetting the title on "file/save as" is a little bit
    // sluggish, otherwise.
    m_wxMaxima.ResetTitle(m_wxMaxima.GetWorksheet()->IsSaved(), true);
  }
  else if (event.GetId() == EventIDs::menu_compare_files) {
    wxFileDialog fileDialog(&m_wxMaxima, _("Select 2 or 3 files to compare"), m_wxMaxima.m_lastPath,
                            wxEmptyString,
                            _("wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx"),
                            wxFD_OPEN | wxFD_MULTIPLE);
    if (fileDialog.ShowModal() == wxID_OK) {
      wxArrayString paths;
      fileDialog.GetPaths(paths);
      if (paths.size() == 2 || paths.size() == 3) {
        DiffFrame *diffFrame = new DiffFrame(&m_wxMaxima, paths, &m_wxMaxima.m_configuration);
        diffFrame->Show();
      } else {
        wxLogError(_("Please select exactly 2 or 3 files."));
      }
    }
  }
  else if (event.GetId() == EventIDs::menu_jump_to_uuid) {
    wxString uuid = wxGetTextFromUser(_("Enter UUID to jump to:"), _("Jump to UUID"));
    if (!uuid.IsEmpty()) {
      Cell *cell = m_wxMaxima.GetWorksheet()->FindCellByUUID(uuid);
      if (cell) {
        m_wxMaxima.GetWorksheet()->ScrolledAwayFromEvaluation(true);
        m_wxMaxima.GetWorksheet()->ScheduleScrollToCell(cell);
      } else {
        wxLogError(_("Cell with UUID %s not found!"), uuid);
      }
    }
  }
  else if(event.GetId() == EventIDs::menu_export_html) {
    // Determine a sane default file name;
    wxString file = m_wxMaxima.GetWorksheet()->GetCurrentFile();
    if (file.Length() == 0)
      file = _("untitled");
    else
      wxFileName::SplitPath(file, NULL, NULL, &file, NULL);

    wxString fileExt = "html";
    wxConfig::Get()->Read(wxS("defaultExportExt"), &fileExt);

    wxFileDialog fileDialog(&m_wxMaxima, _("Export"), m_wxMaxima.m_lastPath,
                            file + wxS(".") + fileExt,
                            _("HTML file (*.html)|*.html|"
                              "maxima batch file (*.mac)|*.mac|"
                              "LaTeX file (*.tex)|*.tex"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (fileExt == wxS("html"))
      fileDialog.SetFilterIndex(0);
    else if (fileExt == wxS("mac"))
      fileDialog.SetFilterIndex(1);
    else
      fileDialog.SetFilterIndex(2);

    if (fileDialog.ShowModal() == wxID_OK) {
      file = fileDialog.GetPath();
      if (file.Length()) {
        int ext = fileDialog.GetFilterIndex();
        if ((!file.Lower().EndsWith(wxS(".html"))) &&
            (!file.Lower().EndsWith(wxS(".mac"))) &&
            (!file.Lower().EndsWith(wxS(".tex")))) {
          switch (ext) {
          case 0:
            file += wxS(".html");
            break;
          case 1:
            file += wxS(".mac");
            break;
          case 2:
            file += wxS(".tex");
            break;
          default:
            file += wxS(".html");
          }
        }

        if (file.Lower().EndsWith(wxS(".tex"))) {
          m_wxMaxima.StatusExportStart();

          fileExt = wxS("tex");
          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          if (!m_wxMaxima.GetWorksheet()->ExportToTeX(file)) {
            LoggingMessageBox(_("Exporting to TeX failed!"), _("Error!"), wxOK);
            m_wxMaxima.StatusExportFailed();
          } else
            m_wxMaxima.StatusExportFinished();
        } else if (file.Lower().EndsWith(wxS(".mac"))) {
          m_wxMaxima.StatusExportStart();

          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          fileExt = wxS("mac");
          if (!m_wxMaxima.GetWorksheet()->ExportToMAC(file)) {
            LoggingMessageBox(_("Exporting to maxima batch file failed!"),
                              _("Error!"), wxOK);
            m_wxMaxima.StatusExportFailed();
          } else
            m_wxMaxima.StatusExportFinished();
        } else {
          m_wxMaxima.StatusExportStart();

          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          fileExt = wxS("html");
          if (!m_wxMaxima.GetWorksheet()->ExportToHTML(file)) {
            LoggingMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                              wxOK);
            m_wxMaxima.StatusExportFailed();
          } else
            m_wxMaxima.StatusExportFinished();
        }
        m_wxMaxima.StartAutoSaveTimer();

        wxConfig::Get()->Write(wxS("defaultExportExt"), fileExt);
      }
    }
  }
  else if(event.GetId() == EventIDs::menu_load_id) {
    wxString file = wxFileSelector(_("Load Package"), m_wxMaxima.m_lastPath, wxEmptyString,
                                   wxEmptyString,
                                   _("Maxima package (*.mac)|*.mac|"
                                     "Lisp package (*.lisp)|*.lisp|All|*"),
                                   wxFD_OPEN);
    if (!file.empty())
      m_wxMaxima.OpenFile(file, wxS("load"));
  }
  else if(event.GetId() == EventIDs::menu_batch_id) {
    wxString file = wxFileSelector(
                                   _("Batch File"), m_wxMaxima.m_lastPath, wxEmptyString, wxEmptyString,
                                   _("Maxima package (*.mac)|*.mac"), wxFD_OPEN);
    if (file != wxEmptyString)
      m_wxMaxima.OpenFile(file, wxS("batch"));
  }
  else if(event.GetId() == ToolBar::tb_animation_startStop) {
    if (m_wxMaxima.GetWorksheet()->CanAnimate()) {
      const AnimationCell *animation =
        dynamic_cast<AnimationCell *>(m_wxMaxima.GetWorksheet()->GetSelectionStart());
      if (animation->AnimationRunning())
        m_wxMaxima.GetWorksheet()->Animate(false);
      else
        m_wxMaxima.GetWorksheet()->Animate(true);
    }
  }
  else if(event.GetId() == EventIDs::popid_animation_start) {
    if (m_wxMaxima.GetWorksheet()->CanAnimate()) {
      AnimationCell *animation =
        dynamic_cast<AnimationCell *>(m_wxMaxima.GetWorksheet()->GetSelectionStart());
      animation->AnimationRunning(true);
    }
  }
  m_wxMaxima.GetWorksheet()->RequestRedraw();
}

void MaximaCommandMenus::InsertMenu(wxCommandEvent &event) {
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  GroupType type = GC_TYPE_CODE;
  bool output = false;
  if(event.GetId() == EventIDs::popid_never_autoanswer){
    m_wxMaxima.m_configuration.OfferKnownAnswers(!m_wxMaxima.m_configuration.OfferKnownAnswers());
  }
  else if(event.GetId() == EventIDs::popid_auto_answer){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell() &&
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup()->GetGroupType() ==
        GC_TYPE_CODE)
      m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup()->SetAutoAnswer(
                                                              event.IsChecked());
    else if ((m_wxMaxima.GetWorksheet()->GetSelectionStart() != NULL) &&
             (m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_GROUP)) {
      GroupCell *gc =
        dynamic_cast<GroupCell *>(m_wxMaxima.GetWorksheet()->GetSelectionStart());
      while (gc != NULL) {
        if (gc->GetGroupType() == GC_TYPE_CODE)
          gc->SetAutoAnswer(event.IsChecked());

        if (gc == m_wxMaxima.GetWorksheet()->GetSelectionEnd())
          break;
        gc = gc->GetNext();
      }
    }
    m_wxMaxima.m_fileSaved = false;
    m_wxMaxima.GetWorksheet()->RequestRedraw();
    return;
  }
  else if(event.GetId() == EventIDs::popid_add_watch){
    wxString selectionString;
    if (m_wxMaxima.GetWorksheet()->GetActiveCell()) {
      selectionString = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetSelectionString();
      if (selectionString.IsEmpty())
        selectionString = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetWordUnderCaret();
      if(m_wxMaxima.m_variablesPane)
        m_wxMaxima.m_variablesPane->AddWatchCode(selectionString);
      m_wxMaxima.wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    if (selectionString.IsEmpty() && (m_wxMaxima.GetWorksheet()->GetSelectionStart() != NULL))
      selectionString = m_wxMaxima.GetWorksheet()->GetSelectionStart()->ToString();
    if (!selectionString.IsEmpty()) {
      if(m_wxMaxima.m_variablesPane)
        m_wxMaxima.m_variablesPane->AddWatchCode(selectionString);
      m_wxMaxima.wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    return;
  }
  else if(event.GetId() == EventIDs::popid_add_watch_label){
    if (m_wxMaxima.GetWorksheet()->IsSelected(MC_TYPE_LABEL)) {
      wxString selectionString = m_wxMaxima.GetWorksheet()->GetSelectionStart()->ToString();
      selectionString.Trim(true);
      selectionString.Trim(false);
      if (selectionString.StartsWith("("))
        selectionString = selectionString.Right(selectionString.Length() - 1);
      if (selectionString.EndsWith(")"))
        selectionString = selectionString.Left(selectionString.Length() - 1);
      if(m_wxMaxima.m_variablesPane)
        m_wxMaxima.m_variablesPane->AddWatchCode(selectionString);
      m_wxMaxima.wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    return;
  }
  else if(event.GetId() == EventIDs::menu_insert_previous_output){
    output = true;
    type = GC_TYPE_CODE;
  }
  else if((event.GetId() == EventIDs::popid_insert_input) ||
          (event.GetId() == EventIDs::menu_insert_input) ||
          (event.GetId() == EventIDs::menu_insert_previous_input)){
    type = GC_TYPE_CODE;
  }
  else if(event.GetId() == EventIDs::menu_autocomplete){
    m_wxMaxima.GetWorksheet()->Autocomplete();
    return;}
  else if(event.GetId() == EventIDs::menu_autocomplete_templates){
    m_wxMaxima.GetWorksheet()->Autocomplete(AutoComplete::tmplte);
    return;}
  // Converting a cell's type goes through Worksheet::SetCellStyle, which rebuilds
  // the group with the target type (so it is born the right kind) and records the
  // change on the undo stack - unlike the old in-place GroupCell::SetGroupType,
  // which was not undoable. SetCellStyle also refuses to convert image cells
  // (that would discard the drag-and-dropped image) and does the recalc/redraw.
  else if(event.GetId() == EventIDs::menu_convert_to_code){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_CODE);
  }
  else if(event.GetId() == EventIDs::menu_convert_to_comment){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_TEXT);
  }
  else if((event.GetId() == EventIDs::menu_add_comment) ||
          (event.GetId() == EventIDs::popid_add_comment) ||
          (event.GetId() == EventIDs::menu_format_text) ||
          (event.GetId() == EventIDs::popid_insert_text))
    {
      type = GC_TYPE_TEXT;
    }
  else if(event.GetId() == EventIDs::menu_convert_to_title){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_TITLE);
  }
  else if((event.GetId() == EventIDs::menu_add_title) ||
          (event.GetId() == EventIDs::menu_format_title) ||
          (event.GetId() == EventIDs::popid_insert_title)){
    type = GC_TYPE_TITLE;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_section){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_SECTION);
  }
  else if((event.GetId() == EventIDs::menu_add_section) ||
          (event.GetId() == EventIDs::menu_format_section) ||
          (event.GetId() == EventIDs::popid_insert_section)){
    type = GC_TYPE_SECTION;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_subsection){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_SUBSECTION);
  }
  else if((event.GetId() == EventIDs::menu_add_subsection) ||
          (event.GetId() == EventIDs::menu_format_subsection) ||
          (event.GetId() == EventIDs::popid_insert_subsection)){
    type = GC_TYPE_SUBSECTION;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_subsubsection){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_SUBSUBSECTION);
  }
  else if(event.GetId() == EventIDs::menu_convert_to_heading5){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_HEADING5);
  }
  else if(event.GetId() == EventIDs::menu_convert_to_heading6){
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      m_wxMaxima.GetWorksheet()->SetCellStyle(
        m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup(), GC_TYPE_HEADING6);
  }
  else if((event.GetId() == EventIDs::menu_add_subsubsection) ||
          (event.GetId() == EventIDs::menu_format_subsubsection) ||
          (event.GetId() == EventIDs::popid_insert_subsubsection)){
    type = GC_TYPE_SUBSUBSECTION;
  }
  else if((event.GetId() == EventIDs::menu_add_heading5) ||
          (event.GetId() == EventIDs::menu_format_heading5) ||
          (event.GetId() == EventIDs::popid_insert_heading5)){
    type = GC_TYPE_HEADING5;
  }
  else if((event.GetId() == EventIDs::menu_add_heading6) ||
          (event.GetId() == EventIDs::menu_format_heading6) ||
          (event.GetId() == EventIDs::popid_insert_heading6)){
    type = GC_TYPE_HEADING6;
  }
  else if((event.GetId() == EventIDs::menu_add_pagebreak) ||
          (event.GetId() == EventIDs::menu_format_pagebreak)) {
    m_wxMaxima.GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_wxMaxima.m_configuration, GC_TYPE_PAGEBREAK),
                                  m_wxMaxima.GetWorksheet()->GetHCaret());
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    return;}
  else if((event.GetId() == EventIDs::menu_insert_image) ||
          (event.GetId() == EventIDs::menu_format_image)){
      wxString file = wxFileSelector(
                                       _("Insert Image"), m_wxMaxima.m_lastPath, wxEmptyString, wxEmptyString,
                                       _("Image files (") +
#ifdef wxUSE_LIBPNG
                                       "*.png, "
#endif
#ifdef wxUSE_LIBJPEG
                                       "*.jpg, "
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp, "
#endif
#ifdef wxUSE_XPM
                                         "*.xpm, "
#endif
#ifdef wxUSE_GIF
                                         "*.gif, "
#endif

                                         "*.svg, *.svgz, "
                                         "*.bmp)|"
#ifdef wxUSE_LIBJPEG
                                         "*.png;"
#endif
#ifdef wxUSE_LIBJPEG
                                         "*.jpg;"
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp;"
#endif
#ifdef wxUSE_XPM
                                         "*.xpm;"
#endif
#ifdef wxUSE_GIF
                                         "*.gif;"
#endif
                                         "*.svg;*.svgz,"
                                         "*.bmp",
                                       wxFD_OPEN);
      if (file != wxEmptyString)
        m_wxMaxima.GetWorksheet()->OpenHCaret(file, GC_TYPE_IMAGE);
      return;
    }
  else if(event.GetId() == EventIDs::menu_fold_all_cells){
    m_wxMaxima.GetWorksheet()->FoldAll();
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    // send cursor to the top
    m_wxMaxima.GetWorksheet()->SetHCaret(NULL);
  }
  else if(event.GetId() == EventIDs::menu_unfold_all_cells){
    m_wxMaxima.GetWorksheet()->UnfoldAll();
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    // refresh without moving cursor
    m_wxMaxima.GetWorksheet()->SetHCaret(m_wxMaxima.GetWorksheet()->GetHCaret());
  }

  m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});

  if (event.GetId() == EventIDs::menu_insert_previous_input ||
      event.GetId() == EventIDs::menu_insert_previous_output) {
    wxString input;

    if (output == true)
      input = m_wxMaxima.GetWorksheet()->GetOutputAboveCaret();
    else
      input = m_wxMaxima.GetWorksheet()->GetInputAboveCaret();
    if (input != wxEmptyString)
      m_wxMaxima.GetWorksheet()->OpenHCaret(input, type);
  } else if ((event.GetId() == EventIDs::menu_unfold_all_cells) ||
             (event.GetId() == EventIDs::menu_fold_all_cells) ||
             (event.GetId() == EventIDs::menu_convert_to_heading6) ||
             (event.GetId() == EventIDs::menu_convert_to_heading5) ||
             (event.GetId() == EventIDs::menu_convert_to_subsubsection) ||
             (event.GetId() == EventIDs::menu_convert_to_subsection) ||
             (event.GetId() == EventIDs::menu_convert_to_section) ||
             (event.GetId() == EventIDs::menu_convert_to_comment) ||
             (event.GetId() == EventIDs::menu_convert_to_title) ||
             (event.GetId() == EventIDs::menu_convert_to_code)) {
    // don't do anything else
  } else
    m_wxMaxima.GetWorksheet()->OpenHCaret(wxEmptyString, type);
  m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
}

void MaximaCommandMenus::EditMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  // if (m_wxMaxima.GetWorksheet()->m_findDialog != NULL) {
  //   event.Skip();
  //   return;
  // }

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(((event.GetId()) >= EventIDs::popid_labelwidth1) &&
     ((event.GetId()) < EventIDs::popid_labelwidth1 + LABELWIDTH_MAX - LABELWIDTH_MIN)) {
    m_wxMaxima.m_configuration.LabelWidth(EventIDs::popid_labelwidth1 + 1 - event.GetId());
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_20) {
    m_wxMaxima.m_configuration.SetDisplayedDigits(20);
    m_wxMaxima.m_configuration.ShowAllDigits(false);
    m_wxMaxima.m_configuration.LineBreaksInLongNums(false);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_50) {
    m_wxMaxima.m_configuration.SetDisplayedDigits(50);
    m_wxMaxima.m_configuration.ShowAllDigits(false);
    m_wxMaxima.m_configuration.LineBreaksInLongNums(false);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_100) {
    m_wxMaxima.m_configuration.SetDisplayedDigits(100);
    m_wxMaxima.m_configuration.ShowAllDigits(false);
    m_wxMaxima.m_configuration.LineBreaksInLongNums(false);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_all) {
    m_wxMaxima.m_configuration.ShowAllDigits(true);
    m_wxMaxima.m_configuration.LineBreaksInLongNums(false);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_all_linebreak) {
    m_wxMaxima.m_configuration.ShowAllDigits(true);
    m_wxMaxima.m_configuration.LineBreaksInLongNums(true);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_inputlabels_hide) {
    m_wxMaxima.m_configuration.ShowInputLabels(!m_wxMaxima.m_configuration.ShowInputLabels());
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if (event.GetId() == EventIDs::menu_copy_uuid) {
    if (m_wxMaxima.GetWorksheet()->GetSelectionStart()) {
      wxString uuid = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetUUID();
      if (uuid.IsEmpty()) {
        m_wxMaxima.GetWorksheet()->GetSelectionStart()->GenerateUUID();
        uuid = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetUUID();
        m_wxMaxima.GetWorksheet()->SetSaved(false);
      }
      if (wxTheClipboard->Open()) {
        wxTheClipboard->SetData(new wxTextDataObject(uuid));
        wxTheClipboard->Close();
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_labels_autogenerated) {
    m_wxMaxima.m_configuration.SetLabelChoice(Configuration::labels_automatic);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_user) {
    m_wxMaxima.m_configuration.SetLabelChoice(Configuration::labels_prefer_user);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_useronly) {
    m_wxMaxima.m_configuration.SetLabelChoice(Configuration::labels_useronly);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_disable) {
    m_wxMaxima.m_configuration.SetLabelChoice(Configuration::labels_none);
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_popup_gnuplot) {
    if (!m_wxMaxima.GetWorksheet()->GetSelectionStart())
      return;

    wxString gnuplotSource = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GnuplotSource();
    if (gnuplotSource.IsEmpty())
      return;

    if (!wxFileExists(gnuplotSource))
      return;

    // Create a gnuplot file that doesn't select a terminal and output file
    wxFileInputStream input(gnuplotSource);
    if (!input.IsOk())
      return;
    wxTextInputStream textIn(input, wxS('\t'),
                             wxConvAuto(wxFONTENCODING_UTF8));
    wxString gnuplot_popout_tempfilename = wxFileName::CreateTempFileName("wxmaxima_gnuplot_popout_");
    wxFileOutputStream output(gnuplot_popout_tempfilename);
    if (!output.IsOk())
      return;
    wxTextOutputStream textOut(output);
#ifdef __WXMSW__
    textOut << "set term windows\n";
#endif
    textIn.ReadLine();
    textIn.ReadLine();

    wxString line;
    while (!input.Eof()) {
      line = textIn.ReadLine();
      textOut << line + wxS("\n");
    }
    textOut.Flush();

    // Execute gnuplot
    std::vector<char *> argv;
    wxCharBuffer commandnamebuffer = m_wxMaxima.m_gnuplotcommand.mb_str();
    argv.push_back(commandnamebuffer.data());
    wxCharBuffer urlbuffer = wxString(gnuplot_popout_tempfilename).mb_str();
    argv.push_back(urlbuffer.data());
    wxCharBuffer persist_opt = wxString(wxS("--persist")).mb_str();
    argv.push_back(persist_opt.data());
    argv.push_back(NULL);

    wxLogMessage(_("Running %s on the file %s: "), commandnamebuffer, urlbuffer);
    m_wxMaxima.m_gnuplotProcess = new wxProcess(&m_wxMaxima, m_wxMaxima.m_gnuplot_process_id);
    if (wxExecute(argv.data(),
                  wxEXEC_ASYNC | wxEXEC_SHOW_CONSOLE | wxEXEC_MAKE_GROUP_LEADER,
                  m_wxMaxima.m_gnuplotProcess) < 0)
      wxLogMessage(_("Cannot start gnuplot"));
  }
  else if(event.GetId() == wxID_PREFERENCES) {
    // wxGTK uses wxFileConf. ...and wxFileConf loads the config file only once
    // on initialisation => Let's reload the config file before entering the
    // config dialogue.
    m_wxMaxima.ReReadConfig();
    wxConfigBase *config = wxConfig::Get();
    // Write the changes in the configuration to the disk.
    config->Flush(true);
    ConfigDialogue *configW = new ConfigDialogue(&m_wxMaxima);
    configW->Centre(wxBOTH);
    auto result = configW->ShowModal();
    if (result == wxID_OK) {
      configW->WriteSettings();
      // Refresh the display as the settings that affect it might have changed.
      m_wxMaxima.m_configuration.ReadConfig();
      m_wxMaxima.m_configuration.FontChanged();
      if (m_wxMaxima.GetWorksheet()->GetTree())
        m_wxMaxima.GetWorksheet()->GetTree()->FontsChangedList();
      m_wxMaxima.ConfigChanged();
      m_wxMaxima.GetWorksheet()->RequestRecalculation();
      m_wxMaxima.GetWorksheet()->RequestRedraw();
    }
    configW->Destroy();
  }
  else if(event.GetId() == wxID_COPY) {
    m_wxMaxima.GetWorksheet()->Copy();
  }
  else if(event.GetId() == EventIDs::menu_copy_text_from_worksheet) {
    m_wxMaxima.GetWorksheet()->Copy(true);
  }
  else if(event.GetId() == wxID_CUT) {
    if (m_wxMaxima.GetWorksheet()->CanCut())
      m_wxMaxima.GetWorksheet()->CutToClipboard();
  }
  else if(event.GetId() == wxID_SELECTALL) {
    m_wxMaxima.GetWorksheet()->SelectAll();
  }
  else if(event.GetId() == wxID_PASTE) {
    m_wxMaxima.GetWorksheet()->PasteFromClipboard();
  }
  else if(event.GetId() == wxID_UNDO) {
    if (m_wxMaxima.GetWorksheet()->CanUndo())
      m_wxMaxima.GetWorksheet()->Undo();
  }
  else if(event.GetId() == wxID_REDO) {
    if (m_wxMaxima.GetWorksheet()->CanRedo())
      m_wxMaxima.GetWorksheet()->Redo();
  }
  else if(event.GetId() == EventIDs::menu_copy_matlab_from_worksheet) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyMatlab();
  }
  else if(event.GetId() == EventIDs::menu_copy_tex_from_worksheet) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyTeX();
  }
  else if(event.GetId() == EventIDs::popid_copy_mathml) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyMathML();
  }
  else if(event.GetId() == EventIDs::menu_copy_as_bitmap) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyBitmap();
  }
  else if(event.GetId() == EventIDs::menu_copy_as_svg) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopySVG();
  }
#if wxUSE_ENH_METAFILE
  else if(event.GetId() == EventIDs::menu_copy_as_emf) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyEMF();
  }
#endif
  else if(event.GetId() == EventIDs::menu_copy_as_rtf) {
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyRTF();
  }
  else if(event.GetId() == EventIDs::menu_copy_to_file) {
    wxString file = wxFileSelector(_("Save Selection to Image"), m_wxMaxima.m_lastPath,
    // by default use PNG, if not available BMP (always supported by wxWidgets) as default name/type.
#ifdef wxUSE_LIBPNG
                                   wxS("image.png"), wxS("png"),
#else
                                   wxS("image.bmp"), wxS("bmp"),
#endif
                                       _("Image files (") +
#ifdef wxUSE_LIBPNG
                                       "*.png, "
#endif
#ifdef wxUSE_LIBJPEG
                                       "*.jpg, "
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp, "
#endif
#ifdef wxUSE_XPM
                                         "*.xpm, "
#endif
#ifdef wxUSE_GIF
                                         "*.gif, "
#endif

                                         ".svg, *.svgz, "
                                         ".bmp)|"
#ifdef wxUSE_LIBPNG
                                         "*.png;"
#endif
#ifdef wxUSE_LIBJPEG
                                         "*.jpg;"
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp;"
#endif
#ifdef wxUSE_XPM
                                         "*.xpm;"
#endif
#ifdef wxUSE_GIF
                                         "*.gif;"
#endif
                                         "*.svg;*.svgz,"
                                         "*.bmp",
                                       wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (file.Length()) {
      m_wxMaxima.GetWorksheet()->CopyToFile(file);
      m_wxMaxima.m_lastPath = wxPathOnly(file);
    }
  }
  else if(event.GetId() == EventIDs::menu_print_cellbrackets) {
    m_wxMaxima.m_configuration.PrintBrackets(!m_wxMaxima.m_configuration.PrintBrackets());
  }
  else if(event.GetId() == EventIDs::menu_show_cellbrackets) {
    m_wxMaxima.m_configuration.ShowBrackets(!m_wxMaxima.m_configuration.ShowBrackets());
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_delete) {
    if (m_wxMaxima.GetWorksheet()->CanDeleteSelection()) {
      m_wxMaxima.GetWorksheet()->DeleteSelection();
      m_wxMaxima.GetWorksheet()->RequestRecalculation();
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      return;
    }
  }
  else if(event.GetId() == wxID_ZOOM_IN) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(m_wxMaxima.m_configuration.GetZoomFactor() + 0.1);
  }
  else if(event.GetId() == wxID_ZOOM_OUT) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(m_wxMaxima.m_configuration.GetZoomFactor() - 0.1);
  }
  else if(event.GetId() == EventIDs::menu_zoom_80) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(0.8);
  }
  else if(event.GetId() == wxID_ZOOM_100) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(1.0);
  }
  else if(event.GetId() == EventIDs::menu_zoom_120) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(1.2);
  }
  else if(event.GetId() == EventIDs::menu_zoom_150) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(1.5);
  }
  else if(event.GetId() == EventIDs::menu_zoom_200) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(2.0);
  }
  else if(event.GetId() == EventIDs::menu_zoom_300) {
    m_wxMaxima.GetWorksheet()->SetZoomFactor(3.0);
  }
  else if(event.GetId() == EventIDs::menu_math_as_1D_ASCII) {
    m_wxMaxima.MenuCommand(wxS("set_display('none)$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_2D_ASCII) {
    m_wxMaxima.MenuCommand(wxS("set_display('ascii)$display2d_unicode:false$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_2D_UNICODE) {
    m_wxMaxima.MenuCommand(wxS("set_display('ascii)$display2d_unicode:true$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_graphics) {
    m_wxMaxima.MenuCommand(wxS("set_display('xml)$"));
  }
  else if(event.GetId() == EventIDs::internalRepresentation) {
    m_wxMaxima.CommandWiz(_("Display expression in maxima's internal representation"),
               wxEmptyString, wxEmptyString,
               wxS("?print(#1#)$"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::wxMathML) {
    m_wxMaxima.CommandWiz(_("Display expression in wxMaxima's internal representation"),
               wxEmptyString, wxEmptyString,
               wxS("printf(false,\"~m\", #1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_noAutosubscript) {
    m_wxMaxima.MenuCommand(wxS("wxsubscripts: false$"));
  }
  else if(event.GetId() == EventIDs::menu_defaultAutosubscript) {
    m_wxMaxima.MenuCommand(wxS("wxsubscripts: true$"));
  }
  else if(event.GetId() == EventIDs::menu_alwaysAutosubscript) {
    m_wxMaxima.MenuCommand(wxS("wxsubscripts: 'all$"));
  }
  else if(event.GetId() == EventIDs::menu_autosubscriptIndividual) {
    m_wxMaxima.CommandWiz(_("Autosubscript this variable"), wxEmptyString, wxEmptyString,
               wxS("wxdeclare_subscripted(#1#)$"), _("Variable name"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_noAutosubscriptIndividual) {
    m_wxMaxima.CommandWiz(_("Never autosubscript this variable"), wxEmptyString,
               wxEmptyString, wxS("wxdeclare_subscripted(#1#,false)$"),
               _("Variable name"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_declareAutosubscript) {
    m_wxMaxima.CommandWiz(_("Declare a text snippet to always be displayed as subscript"),
               wxEmptyString, wxEmptyString, wxS("wxdeclare_subscript(#1#)$"),
               _("Text snippet"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_roundedMatrixParens) {
    m_wxMaxima.MenuCommand(wxS("lmxchar:\"(\"$rmxchar:\")\"$"));
  }
  else if(event.GetId() == EventIDs::menu_straightMatrixParens) {
    m_wxMaxima.MenuCommand(wxS("lmxchar:\"|\"$rmxchar:\"|\"$"));
  }
  else if(event.GetId() == EventIDs::menu_angledMatrixParens) {
    m_wxMaxima.MenuCommand(wxS("lmxchar:\"<\"$rmxchar:\">\"$"));
  }
  else if(event.GetId() == EventIDs::menu_squareMatrixParens) {
    m_wxMaxima.MenuCommand(wxS("lmxchar:\"[\"$rmxchar:\"]\"$"));
  }
  else if(event.GetId() == EventIDs::menu_noMatrixParens) {
    m_wxMaxima.MenuCommand(wxS("lmxchar:\" \"$rmxchar:\" \"$"));
  }
  else if(event.GetId() == EventIDs::menu_fullscreen) {
    m_wxMaxima.ShowFullScreen(!m_wxMaxima.IsFullScreen());
  }
  else if(event.GetId() == EventIDs::menu_show_logwindow) {
    // FIXME: if the log window was closed as the parent 'disable' the toggle function, otherwise we risk a crash.
    if (MyApp::m_logWindow->GetFrame() != NULL) {
      MyApp::m_logWindow->Show(!MyApp::m_logWindow->GetFrame()->IsShown());
    };
  }

  else if(event.GetId() == ToolBar::tb_hideCode) {
    m_wxMaxima.m_configuration.ShowCodeCells(!m_wxMaxima.m_configuration.ShowCodeCells());
    m_wxMaxima.GetWorksheet()->CodeCellVisibilityChanged();
  }
  else if(event.GetId() == EventIDs::menu_remove_output) {
    m_wxMaxima.GetWorksheet()->RemoveAllOutput();
  }
  else if(event.GetId() == EventIDs::menu_pane_toolbar) {
    m_wxMaxima.ShowToolBar(!m_wxMaxima.ToolbarIsShown());
  }
  else if(event.GetId() == wxID_FIND) {
    wxLogMessage(_("A Ctrl-F event"));
    bool findDialogActiveWas = ((m_wxMaxima.GetWorksheet()->m_findDialog != NULL) &&
                                (m_wxMaxima.GetWorksheet()->m_findDialog->IsShown()));
    if (m_wxMaxima.GetWorksheet()->m_findDialog == NULL)
      new FindReplaceDialog(&m_wxMaxima, &m_wxMaxima.m_findData, _("Find and Replace"), &m_wxMaxima.GetWorksheet()->m_findDialog);
    if (m_wxMaxima.GetWorksheet()->GetActiveCell() != NULL) {
      wxString selected = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetSelectionString();

      // Start incremental search and highlighting of search results again.
      if(findDialogActiveWas)
        m_wxMaxima.m_oldFindString.Clear();
      else
        m_wxMaxima.m_oldFindString = selected;

      if (selected.Length() > 0)
        m_wxMaxima.GetWorksheet()->m_findDialog->SetFindString(selected);
    }
    m_wxMaxima.GetWorksheet()->m_findDialog->Show();
    m_wxMaxima.GetWorksheet()->m_findDialog->Raise();
    m_wxMaxima.GetWorksheet()->FocusFindDialogue();
#ifdef __WXMSW__
    m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->FocusFindDialogue();});
#endif
 }
  else if(event.GetId() == EventIDs::menu_history_next) {
    m_wxMaxima.m_history->UpdateDeferred();
    wxString command = m_wxMaxima.m_history->GetCommand(true);
    if (command != wxEmptyString)
      m_wxMaxima.GetWorksheet()->SetActiveCellText(command);
  }
  else if(event.GetId() == EventIDs::menu_history_previous) {
    m_wxMaxima.m_history->UpdateDeferred();
    wxString command = m_wxMaxima.m_history->GetCommand(false);
    if (command != wxEmptyString)
      m_wxMaxima.GetWorksheet()->SetActiveCellText(command);
  }
  else if(event.GetId() == EventIDs::popid_hide_tooltipMarkerForThisMessage) {
    const Cell *cell = m_wxMaxima.GetWorksheet()->GetSelectionStart();
    if (cell == NULL)
      return;
    wxString toolTip = cell->GetLocalToolTip();
    if (toolTip.IsEmpty())
      toolTip = cell->GetGroup()->GetLocalToolTip();
    if (toolTip.IsEmpty())
      return;
    bool suppress = m_wxMaxima.m_configuration.HideMarkerForThisMessage(toolTip);
    m_wxMaxima.m_configuration.HideMarkerForThisMessage(toolTip, !suppress);
    m_wxMaxima.GetWorksheet()->OutputChanged();
  }
  else if(event.GetId() == EventIDs::popid_hide_tooltipMarker) {
    if (m_wxMaxima.GetWorksheet()->GetSelectionStart() == NULL)
      return;
    GroupCell *cell = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup();
    const GroupCell *end = NULL;
    if (m_wxMaxima.GetWorksheet()->GetSelectionEnd() != NULL)
      end = m_wxMaxima.GetWorksheet()->GetSelectionEnd()->GetGroup();
    bool marked = !cell->GetSuppressTooltipMarker();

    for (auto &tmp : OnList(cell)) {
      tmp.SetSuppressTooltipMarker(marked);
      if (&tmp == end)
        break;
    }
    m_wxMaxima.GetWorksheet()->OutputChanged();
  }
  m_wxMaxima.GetWorksheet()->RequestRedraw();
  // Most edit-menu commands should return the keyboard focus to the worksheet.
  // The Find/Replace command (Ctrl+F) is the exception: it just showed the
  // non-modal find dialog and gave *it* the focus, so re-focusing the worksheet
  // here -- via a CallAfter that runs after the dialog has already been focused
  // -- would immediately steal the focus back before the user can type into the
  // dialog (seen on Windows).
  if (event.GetId() != wxID_FIND)
    m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
}

void MaximaCommandMenus::PopupMenu(wxCommandEvent &event) {
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString selection = m_wxMaxima.GetWorksheet()->GetString();
  if(event.GetId() == EventIDs::enable_unicodePane){
    m_wxMaxima.wxMaximaFrame::ShowPane(EventIDs::menu_pane_unicode, true);
  }
  else if(event.GetId() == EventIDs::popid_fold){
      if (m_wxMaxima.GetWorksheet()->GetActiveCell()) {
        // This "if" is pure paranoia. But - since the costs of an "if" are low...
        GroupCell *group = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup();
        if (group->IsFoldable())
          group->Fold();
        else
          group->Hide(true);
        m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
      }
  }
  else if(event.GetId() == EventIDs::popid_maxsizechooser){
    if (m_wxMaxima.GetWorksheet()->GetSelectionStart()) {
      Cell *output = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if ((output->GetType() != MC_TYPE_IMAGE) &&
          (output->GetType() != MC_TYPE_SLIDE))
        return;

      MaxSizeChooser *chooser = new MaxSizeChooser(
                                                   &m_wxMaxima, -1, dynamic_cast<ImgCellBase *>(output)->GetMaxWidth(),
                                                   dynamic_cast<ImgCellBase *>(output)->GetHeightList());
      chooser->Centre(wxBOTH);
      if (chooser->ShowModal() == wxID_OK) {
        if (dynamic_cast<ImgCellBase *>(output)->GetMaxWidth() !=
            chooser->GetMaxImageWidth())
          m_wxMaxima.GetWorksheet()->SetSaved(false);
        if (dynamic_cast<ImgCellBase *>(output)->GetHeightList() !=
            chooser->GetHeightList())
          m_wxMaxima.GetWorksheet()->SetSaved(false);

        dynamic_cast<ImgCellBase *>(output)->SetMaxWidth(
                                                         chooser->GetMaxWidth());
        dynamic_cast<ImgCellBase *>(output)->SetMaxHeight(
                                                          chooser->GetHeightList());
      }
    }
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_resolutionchooser){
    if (m_wxMaxima.GetWorksheet()->GetSelectionStart()) {
      Cell *output = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if ((output->GetType() != MC_TYPE_IMAGE) &&
          (output->GetType() != MC_TYPE_SLIDE))
        return;

      ResolutionChooser *chooser = new ResolutionChooser(
                                                         &m_wxMaxima, -1, dynamic_cast<ImgCellBase *>(output)->GetPPI());
      chooser->Centre(wxBOTH);
      if (chooser->ShowModal() == wxID_OK) {
        if (dynamic_cast<ImgCellBase *>(output)->GetPPI() !=
            chooser->GetResolution())
          m_wxMaxima.GetWorksheet()->SetSaved(false);

        dynamic_cast<ImgCellBase *>(output)->SetPPI(chooser->GetResolution());
      }
    }
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_reloadimage){
    if (!m_wxMaxima.GetWorksheet()->GetSelectionStart())
      return;

    {
      Cell *output = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if (output->GetType() != MC_TYPE_IMAGE)
        return;

      wxString imgFile = dynamic_cast<ImgCell *>(output)->GetOrigImageFile();

      if (!wxFileExists(imgFile)) {
        LoggingMessageDialog dialog(
                                    &m_wxMaxima,
                                    wxString::Format(_("The image file \"%s\" cannot be found."),
                                                     imgFile),
                                    "wxMaxima", wxCENTER | wxOK);
        dialog.SetOKLabel(_("OK"));

        dialog.ShowModal();

        return;
      }

      wxLogMessage(_("Reloading image file %s."), imgFile);
      dynamic_cast<ImgCell *>(output)->ReloadImage(imgFile,
                                                   wxEmptyString);

      m_wxMaxima.GetWorksheet()->RequestRecalculation();
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      m_wxMaxima.GetWorksheet()->SetSaved(false);

      m_wxMaxima.UpdateMenus();
      m_wxMaxima.UpdateToolBar();
      // ResetTitle(m_wxMaxima.GetWorksheet()->IsSaved());
    }
  }
  else if(event.GetId() == EventIDs::popid_unfold){
      GroupCell *group = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup();
      if (group->IsFoldable())
        group->Unfold();
      else
        group->Hide(false);
      m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_Fold){
    if (m_wxMaxima.m_tableOfContents != NULL) {
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((m_wxMaxima.GetWorksheet()->GetTree()) &&
          (m_wxMaxima.GetWorksheet()->GetTree()->Contains(
                                            m_wxMaxima.m_tableOfContents->RightClickedOn()))) {
        m_wxMaxima.m_tableOfContents->RightClickedOn()->Fold();
        m_wxMaxima.GetWorksheet()->RequestRecalculation();
        m_wxMaxima.GetWorksheet()->RequestRedraw();
        m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_Unfold){
    if (m_wxMaxima.m_tableOfContents != NULL) {
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((m_wxMaxima.GetWorksheet()->GetTree()) &&
          (m_wxMaxima.GetWorksheet()->GetTree()->Contains(
                                            m_wxMaxima.m_tableOfContents->RightClickedOn()))) {
        m_wxMaxima.m_tableOfContents->RightClickedOn()->Unfold();
        m_wxMaxima.GetWorksheet()->RequestRecalculation();
        m_wxMaxima.GetWorksheet()->RequestRedraw();
        m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_SelectTocChapter){
    if (m_wxMaxima.m_tableOfContents != NULL) {
      if (m_wxMaxima.m_tableOfContents->RightClickedOn()) {
        GroupCell *SelectionStart =
          m_wxMaxima.m_tableOfContents->RightClickedOn();
        // We only update the table of contents when there is time => no
        // guarantee that the cell that was clicked at actually still is part of
        // the tree.
        if ((m_wxMaxima.GetWorksheet()->GetTree()) &&
            (m_wxMaxima.GetWorksheet()->GetTree()->Contains(SelectionStart))) {
          GroupCell *SelectionEnd = SelectionStart;
          while ((SelectionEnd->GetNext() != NULL) &&
                 (SelectionEnd->GetNext()->IsLesserGCType(
                                                          SelectionStart->GetGroupType())))
            SelectionEnd = SelectionEnd->GetNext();
          m_wxMaxima.GetWorksheet()->SetActiveCell(NULL);
          m_wxMaxima.GetWorksheet()->ScrolledAwayFromEvaluation(true);
          m_wxMaxima.GetWorksheet()->SetHCaret(SelectionEnd);
          m_wxMaxima.GetWorksheet()->SetSelection(SelectionStart, SelectionEnd);
          m_wxMaxima.GetWorksheet()->RequestRedraw();
        }
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_EvalTocChapter){
      GroupCell *SelectionStart =
        m_wxMaxima.m_tableOfContents->RightClickedOn();
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((m_wxMaxima.GetWorksheet()->GetTree()) &&
          (m_wxMaxima.GetWorksheet()->GetTree()->Contains(SelectionStart))) {
        m_wxMaxima.GetWorksheet()->AddSectionToEvaluationQueue(
                                                 m_wxMaxima.m_tableOfContents->RightClickedOn());
        m_wxMaxima.TriggerEvaluation();
      }
  }
  else if(event.GetId() == EventIDs::popid_ToggleTOCshowsSectionNumbers){
      m_wxMaxima.m_configuration.TocShowsSectionNumbers(true);
      m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_TOCindentation){
      m_wxMaxima.m_configuration.TocShowsSectionNumbers(false);
      m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if((event.GetId() >= EventIDs::popid_tocLevel1) && (event.GetId() < EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels - 2)) {
    m_wxMaxima.m_configuration.TocDepth(event.GetId() - EventIDs::popid_tocLevel1 + 1 );
    m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels -1){
      m_wxMaxima.m_configuration.TocDepth(255);
      m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocdnd){
    m_wxMaxima.GetWorksheet()->TOCdnd(m_wxMaxima.m_tableOfContents->DNDStart(), m_wxMaxima.m_tableOfContents->DNDEnd());
  }
  else if(event.GetId() == EventIDs::popid_tocMoveIn){
    m_wxMaxima.GetWorksheet()->SectioningMoveIn(m_wxMaxima.m_tableOfContents->RightClickedOn());
    m_wxMaxima.GetWorksheet()->NumberSections();
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
    m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocMoveOut){
    m_wxMaxima.GetWorksheet()->SectioningMoveOut(m_wxMaxima.m_tableOfContents->RightClickedOn());
    m_wxMaxima.GetWorksheet()->NumberSections();
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
    m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_evaluate_section){
      GroupCell *group = NULL;
      if (m_wxMaxima.GetWorksheet()->GetActiveCell()) {
        // This "if" is pure paranoia. But - since the costs of an "if" are low...
        if (m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup())
          group = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup();
      } else if (m_wxMaxima.GetWorksheet()->HCaretActive()) {
        if (m_wxMaxima.GetWorksheet()->GetHCaret()) {
          group = m_wxMaxima.GetWorksheet()->GetHCaret();
          if ((false))
            if (group->GetNext())
              group = group->GetNext();
        } else
          group = m_wxMaxima.GetWorksheet()->GetTree();
      }
      if (group) {
        m_wxMaxima.GetWorksheet()->AddSectionToEvaluationQueue(group);
        m_wxMaxima.TriggerEvaluation();
      }
    }
  else if((event.GetId() == EventIDs::popid_evaluate) ||
          (event.GetId() == ToolBar::tb_eval)){
    wxCommandEvent *dummy = new wxCommandEvent;
    m_wxMaxima.EvaluateEvent(*dummy);
  }
  else if(event.GetId() == ToolBar::tb_evaluate_rest){
    m_wxMaxima.GetWorksheet()->AddRestToEvaluationQueue();
    m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                          m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
    m_wxMaxima.TriggerEvaluation();
  }
  else if(event.GetId() == ToolBar::tb_evaltillhere){
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    m_wxMaxima.GetWorksheet()->ResetInputPrompts();
    m_wxMaxima.EvaluationQueueLength(0);
    if (m_wxMaxima.m_configuration.RestartOnReEvaluation())
      m_wxMaxima.StartMaxima();
    m_wxMaxima.GetWorksheet()->AddDocumentTillHereToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                          m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
    m_wxMaxima.TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::popid_copy_matlab){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyMatlab();
  }
  else if(event.GetId() == EventIDs::popid_copy_tex){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyTeX();
  }
  else if(event.GetId() == EventIDs::popid_copy_text){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyText();
  }
  else if(event.GetId() == EventIDs::popid_comment_selection){
    m_wxMaxima.GetWorksheet()->CommentSelection();
  }
  else if(event.GetId() == EventIDs::popid_divide_cell){
    m_wxMaxima.GetWorksheet()->DivideCell();
  }
  else if(event.GetId() == EventIDs::popid_copy_image){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyBitmap();
  }
  else if(event.GetId() == EventIDs::popid_copy_animation){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyAnimation();
  }
  else if(event.GetId() == EventIDs::popid_copy_svg){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopySVG();
  }
#if wxUSE_ENH_METAFILE
  else if(event.GetId() == EventIDs::popid_copy_emf){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyEMF();
  }
#endif
  else if(event.GetId() == EventIDs::popid_copy_rtf){
    if (m_wxMaxima.GetWorksheet()->CanCopy())
      m_wxMaxima.GetWorksheet()->CopyRTF();
  }
  else if(event.GetId() == EventIDs::popid_simplify){
    m_wxMaxima.MenuCommand(wxS("ratsimp(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_expand){
    m_wxMaxima.MenuCommand(wxS("expand(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_factor){
    m_wxMaxima.MenuCommand(wxS("factor(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_solve){
    m_wxMaxima.CommandWiz(
               _("Solve"),
               _("solve() will solve a list of equations only if for n "
                 "independent equations there are n variables to solve to.\n"
                 "If only one result variable is of interest the other result "
                 "variables can be used to to tell solve() which variables to "
                 "eliminate from the solution\n"
                 "solve() searches for a global solution. If a problem has different "
                 "solutions depending on the range its variables are in one way "
                 "to successfully use solve() is to use solve() to eliminate "
                 "variables "
                 "one by one and to manually choose which of the solutions solve() "
                 "found "
                 "matches the current problem."),
               wxEmptyString, wxS("solve([#1#],[#2#]);"), _("Data:"), selection,
               _("Comma-separated equations"), _("Result variables:"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::popid_solve_num){
    m_wxMaxima.CommandWiz(_("Find root (solve numerically)"),
               _("Tries to find a solution of the equation that lies between "
                 "the two bounds."),
               wxEmptyString, wxS("find_root(#1#,#2#,#3#,#4#);"),
               _("Equation:"), selection, wxEmptyString, _("Variable:"),
               wxS("x"), wxEmptyString, _("Lower bound:"), wxS("-1"),
               wxEmptyString, _("Upper bound:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_integrate){
      wxWindowPtr<IntegrateWiz> wiz(new IntegrateWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Integrate")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_diff){
    m_wxMaxima.CommandWiz(_("Differentiate"), _("Differentiates the expression n times"),
               wxEmptyString, wxS("diff(#1#,#2#,#3#);"), _("Expression:"),
               selection, wxEmptyString, _("Variable(s):"), wxS("x"),
               wxEmptyString, _("Times:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_subst){
    m_wxMaxima.CommandWiz(_("Substitute"),
               _("Introduces one or more assignments into an expression"),
               wxEmptyString, wxS("subst(#1#,#2#);"), _("Assignment(s):"),
               wxS("x=sqrt(u)"), _("Assignments of the format a=10,b=20"),
               _("Expression"), selection, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_plot2d){
      wxWindowPtr<Plot2DWiz> wiz(new Plot2DWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Plot 2D")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_plot3d){
      wxWindowPtr<Plot3DWiz> wiz(new Plot3DWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Plot 3D")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          m_wxMaxima.MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_float){
    m_wxMaxima.MenuCommand(wxS("float(") + selection + wxS("), numer;"));
  }
  else if(event.GetId() == EventIDs::popid_image){
      if ((m_wxMaxima.GetWorksheet()->GetSelectionStart() == m_wxMaxima.GetWorksheet()->GetSelectionEnd()) &&
          (m_wxMaxima.GetWorksheet()->GetSelectionStart() != NULL))
        {
          bool canExportSVG = false;

          if ((m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_IMAGE) ||
              (m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            if (dynamic_cast<ImgCellBase *>(m_wxMaxima.GetWorksheet()->GetSelectionStart())
                ->CanExportSVG())
              canExportSVG = true;

          wxString selectorString;

          if (canExportSVG)
            selectorString = _("Scalable Vector image (*.svg)|*.svg|"
                               "Compressed Scalable Vector Image (*.svgz)|*.svgz|"
                               "PNG image (*.png)|*.png|"
                               "JPEG image (*.jpg)|*.jpg|"
                               "GIF image (*.gif)|*.gif|"
                               "Windows bitmap (*.bmp)|*.bmp|") +
#ifdef wxUSE_LIBWEBP
                               _("WebP (*.webp)|*.webp|") +
#endif
                               _("Portable anymap (*.pnm)|*.pnm|"
                               "Tagged image file format (*.tif)|*.tif|"
                               "X pixmap (*.xpm)|*.xpm");
          else
            selectorString = _("PNG image (*.png)|*.png|"
                               "JPEG image (*.jpg)|*.jpg|"
                               "Windows bitmap (*.bmp)|*.bmp|") +
#ifdef wxUSE_LIBWEBP
                               _("WebP (*.webp)|*.webp|") +
#endif
                               _("GIF image (*.gif)|*.gif|"
                               "Portable anymap (*.pnm)|*.pnm|"
                               "Tagged image file format (*.tif)|*.tif|"
                               "X pixmap (*.xpm)|*.xpm");

          wxString file = wxFileSelector(_("Save selection to file"), m_wxMaxima.m_lastPath,
                                         wxS("image.png"), wxS("png"), selectorString,
                                         wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
          if (file.Length()) {
            m_wxMaxima.GetWorksheet()->CopyToFile(file);
            m_wxMaxima.m_lastPath = wxPathOnly(file);
          }
        }
  }
  else if(event.GetId() == EventIDs::popid_change_image){
      if (!m_wxMaxima.GetWorksheet()->GetSelectionStart())
        return;

      Cell *cell = m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (cell == NULL)
        return;

      if (cell->GetType() != MC_TYPE_IMAGE)
        return;

      wxString newImg = wxFileSelector(
                                       _("Change Image"), m_wxMaxima.m_lastPath, wxEmptyString, wxEmptyString,
                                       _("Image files (") +
#ifdef wxUSE_LIBPNG
                                       "*.png, "
#endif
#ifdef wxUSE_LIBJPEG
                                       "*.jpg, "
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp, "
#endif
#ifdef wxUSE_XPM
                                         "*.xpm, "
#endif
#ifdef wxUSE_GIF
                                         "*.gif, "
#endif

                                         ".svg, *.svgz, "
                                         ".bmp)|"
#ifdef wxUSE_LIBPNG
                                         "*.png;"
#endif
#ifdef wxUSE_LIBJPEG
                                         "*.jpg;"
#endif
#ifdef wxUSE_LIBWEBP
                                         "*.webp;"
#endif
#ifdef wxUSE_XPM
                                         "*.xpm;"
#endif
#ifdef wxUSE_GIF
                                         "*.gif;"
#endif
                                         "*.svg;*.svgz,"
                                         "*.bmp",
                                       wxFD_OPEN);

      if (!newImg.Length()) {
        return;
      }

      if (!wxFileExists(newImg)) {
        LoggingMessageDialog dialog(
                                    &m_wxMaxima,
                                    wxString::Format(_("The image file \"%s\" cannot be found."),
                                                     newImg),
                                    "wxMaxima", wxCENTER | wxOK);
        dialog.SetOKLabel(_("OK"));

        dialog.ShowModal();

        return;
      }

      ImgCell *ic = dynamic_cast<ImgCell *>(cell);

      wxLogMessage(_("Changing image originally loaded from file %s to %s."),
                   ic->GetOrigImageFile(), newImg);
      ic->ReloadImage(newImg, wxEmptyString);
      ic->SetOrigImageFile(newImg);

      m_wxMaxima.GetWorksheet()->RequestRecalculation();
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      m_wxMaxima.GetWorksheet()->SetSaved(false);
      m_wxMaxima.m_lastPath = wxPathOnly(newImg);

      m_wxMaxima.UpdateMenus();
      m_wxMaxima.UpdateToolBar();
    }
  else if(event.GetId() == EventIDs::popid_animation_save){
      wxString file = wxFileSelector(_("Save animation to file"), m_wxMaxima.m_lastPath,
                                     wxS("animation.gif"), wxS("gif"),
                                     _("GIF image (*.gif)|*.gif"),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length()) {
        Cell *selectedCell = m_wxMaxima.GetWorksheet()->GetSelectionStart();
        if (selectedCell != NULL && selectedCell->GetType() == MC_TYPE_SLIDE)
          {
            wxBusyCursor crs;
            dynamic_cast<AnimationCell *>(selectedCell)->ToGif(file);
          }
      }
    }
  else if(event.GetId() == EventIDs::popid_merge_cells){
    m_wxMaxima.GetWorksheet()->MergeCells();
  }
}

void MaximaCommandMenus::MaximaMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = m_wxMaxima.GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_jumptoerror){
    GroupCell *lastError = m_wxMaxima.GetWorksheet()->GetErrorList().LastError();
    if (lastError) {
      m_wxMaxima.GetWorksheet()->SetActiveCell(lastError->GetEditable());
      if (lastError->GetEditable()) {
        lastError->GetEditable()->CaretToEnd();
      }
      m_wxMaxima.GetWorksheet()->ScrollToCaret();
    }
  }
  else if(event.GetId() == ToolBar::menu_restart_id){
    m_wxMaxima.m_closing = true;
    m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    m_wxMaxima.GetWorksheet()->ResetInputPrompts();
    m_wxMaxima.m_unsuccessfulConnectionAttempts = 0;
    m_wxMaxima.StartMaxima(true);
  }
  else if(event.GetId() == EventIDs::menu_soft_restart){
    m_wxMaxima.MenuCommand(wxS("kill(all);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_dependencies){
    m_wxMaxima.MenuCommand(wxS("kill(dependencies);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_values){
    m_wxMaxima.MenuCommand(wxS("kill(values);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_functions){
    m_wxMaxima.MenuCommand(wxS("kill(functions);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_arrays){
    m_wxMaxima.MenuCommand(wxS("kill(arrays);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_myoptions){
    m_wxMaxima.MenuCommand(wxS("kill(options);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_rules){
    m_wxMaxima.MenuCommand(wxS("kill(rules);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_aliases){
    m_wxMaxima.MenuCommand(wxS("kill(aliases);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_structures){
    m_wxMaxima.MenuCommand(wxS("kill(structures);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_labels){
    m_wxMaxima.MenuCommand(wxS("kill(labels);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_gradefs){
    m_wxMaxima.MenuCommand(wxS("kill(gradefs);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_props){
    m_wxMaxima.MenuCommand(wxS("kill(props);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_macros){
    m_wxMaxima.MenuCommand(wxS("kill(macros);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_let_rule_packages){
    m_wxMaxima.MenuCommand(wxS("kill(let_rule_packages);"));
  }
  else if(event.GetId() == EventIDs::menu_garbage_collect){
    m_wxMaxima.MenuCommand(wxS("garbage_collect();"));
  }
  else if(event.GetId() == EventIDs::menu_room){
    m_wxMaxima.MenuCommand(wxS("?room();"));
  }
  else if(event.GetId() == EventIDs::menu_functions){
    m_wxMaxima.MenuCommand(wxS("functions;"));
  }
  else if(event.GetId() == EventIDs::menu_variables){
    m_wxMaxima.MenuCommand(wxS("values;"));
  }
  else if(event.GetId() == EventIDs::menu_arrays){
    m_wxMaxima.MenuCommand(wxS("arrays;"));
  }
  else if(event.GetId() == EventIDs::menu_macros){
    m_wxMaxima.MenuCommand(wxS("macros;"));
  }
  else if(event.GetId() == EventIDs::menu_labels){
    m_wxMaxima.MenuCommand(wxS("labels;"));
  }
  else if(event.GetId() == EventIDs::menu_myoptions){
    m_wxMaxima.MenuCommand(wxS("myoptions;"));
  }
  else if(event.GetId() == EventIDs::menu_rules){
    m_wxMaxima.MenuCommand(wxS("rules;"));
  }
  else if(event.GetId() == EventIDs::menu_aliases){
    m_wxMaxima.MenuCommand(wxS("aliases;"));
  }
  else if(event.GetId() == EventIDs::menu_structs){
    m_wxMaxima.MenuCommand(wxS("structures;"));
  }
  else if(event.GetId() == EventIDs::menu_dependencies){
    m_wxMaxima.MenuCommand(wxS("dependencies;"));
  }
  else if(event.GetId() == EventIDs::menu_gradefs){
    m_wxMaxima.MenuCommand(wxS("gradefs;"));
  }
  else if(event.GetId() == EventIDs::menu_let_rule_packages){
    m_wxMaxima.MenuCommand(wxS("let_rule_packages;"));
  }

  else if(event.GetId() == EventIDs::menu_display) {
    wxString choices[] = {wxS("xml"), wxS("ascii"), wxS("none")};
    wxString choice =
      wxGetSingleChoice(_("Select math display algorithm"),
                        _("Display algorithm"), 3, choices, &m_wxMaxima);
    if (choice.Length()) {
      wxString cmd = wxS("set_display('") + choice + wxS(")$");
      m_wxMaxima.MenuCommand(cmd);
    }
  }
  else if(event.GetId() == EventIDs::menu_texform) {
    wxString cmd = wxS("tex(") + expr + wxS(")$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_grind) {
    wxString cmd = wxS("grind(") + expr + wxS(")$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_lisp) {
    wxString cmd = wxS("debugmode: lisp$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_all) {
    wxString cmd = wxS("debugmode: true$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_off) {
    wxString cmd = wxS("debugmode: false$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_time) {
    wxString cmd;
    if (event.IsChecked())
      cmd = wxS("showtime:all$");
    else
      cmd = wxS("showtime:false$");
    m_wxMaxima.MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::gentran_lang_c){
    m_wxMaxima.MenuCommand(wxS("gentranlang:c;"));
  }
  else if(event.GetId() == EventIDs::gentran_lang_fortran){
    m_wxMaxima.MenuCommand(wxS("gentranlang:fortran;"));
  }
  else if(event.GetId() == EventIDs::gentran_lang_ratfor){
    m_wxMaxima.MenuCommand(wxS("gentranlang:ratfor;"));
  }
  else if(event.GetId() == EventIDs::gentran_to_stdout){
    m_wxMaxima.CommandWiz(_("Convert to programming language"), wxEmptyString,
               wxEmptyString, wxS("gentran(#1#);"), wxS("Expression(s)"),
               wxS("%"),
               _("Expression or a list of comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::gentran_to_file){
    m_wxMaxima.CommandWiz(_("Convert to programming language file"), wxEmptyString,
               wxEmptyString, wxS("gentran(#1#,[#2#]);"), wxS("Expression(s)"),
               wxS("%"),
               _("Expression or a list of comma-separated expressions"),
               wxS("Filename(s)"), wxS("%"),
               _("Filename or a list of comma-separated file names"));
  }
  else if(event.GetId() == EventIDs::gentran_load){
    m_wxMaxima.MenuCommand(wxS("load(\"gentran\")$"));
  }
  else if(event.GetId() == EventIDs::menu_fun_def){
    m_wxMaxima.CommandWiz(_("Show the function's definition"), wxEmptyString,
               wxEmptyString, wxS("fundef(#1#);"), wxS("function"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_for){
    m_wxMaxima.CommandWiz(_("For loop"), wxEmptyString, wxEmptyString,
               wxS("for #1#:#2# thru #3# step #4# do #5#;"),
               wxS("loop variable:"), wxS("i"), wxEmptyString, wxS("Start:"),
               wxS("1"), wxEmptyString, wxS("End:"), wxS("10"), wxEmptyString,
               wxS("Step width:"), wxS("1"), wxEmptyString, wxS("What to do:"),
               wxS("disp(i)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_while){
    m_wxMaxima.CommandWiz(_("While loop"), wxEmptyString, wxEmptyString,
               wxS("while #1# do #2#;"),
               wxS("Condition:"), wxS("%"), wxEmptyString, wxS("What to do:"),
               wxS("disp(i)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_block){
    m_wxMaxima.CommandWiz(_("Program block"), wxEmptyString, wxEmptyString,
               wxS("block([#1#], #2#);"), wxS("Local variable(s):"), wxS("i:0"),
               _("Comma-separated variable names, can be initialized by the "
                 "\":\" operator."),
               wxS("What to do:"), wxS("i:i+1,disp(i)"),
               _("Comma-separated commands"));
  }
  else if(event.GetId() == EventIDs::menu_block_noLocal){
    m_wxMaxima.CommandWiz(
               _("Program (no local variables)"),
               _("If a program doesn't need local variables, Maxima allows "
                 "putting the commands between parentheses. The result of the last "
                 "operation is the return value of the program."),
               wxEmptyString, wxS("(#1#);"), wxS("What to do:"), wxS("i:i+1,disp(i)"),
               _("Comma-separated commands"));
  }
  else if(event.GetId() == EventIDs::menu_local){
    m_wxMaxima.CommandWiz(_("Declare a function local to a Program"),
               _("The command local() allows telling Maxima which functions to "
                 "make local to the current program when defined."),
               wxEmptyString, wxS("local(#1#);"), wxS("Function name:"), expr,
               _("Comma-separated function names"));
  }
  else if(event.GetId() == EventIDs::menu_return){
    m_wxMaxima.CommandWiz(
               _("Return from a block or loop"),
               _("Unlike in other programming language return() only exits from the "
                 "current loop or block(), not from the whole function."),
               wxEmptyString, wxS("return(#1#);"), wxS("return value:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_trace){
    m_wxMaxima.CommandWiz(_("Trace function(s)"), wxEmptyString, wxEmptyString,
               wxS("trace(#1#);"), wxS("Function(s):"), expr,
               _("Comma-separated function names."));
  }
  else if(event.GetId() == EventIDs::menu_lambda){
    m_wxMaxima.CommandWiz(
               _("Lambda"),
               _("Lambda generates a function, but doesn't give it a name.\n"
                 "This is useful if you want to use a function only once, perhaps "
                 "as a parameter to another function and don't need it to be named.\n"
                 "Also you can fill a variable with a lambda() construct, effectively "
                 "generating a function pointer: A variable that can be used "
                 "as a function, and filled with a different function, if needed."),
               wxEmptyString, wxS("lambda([#1#],#2#);"),
               wxS("Names for the parameters:"), expr,
               _("Comma-separated names the parameters will be referenced by later."),
               wxS("Contents:"), expr, _("Comma-separated expressions."));
  }
  else if(event.GetId() == EventIDs::menu_quotequote){
    m_wxMaxima.CommandWiz(
               _("Interpret maxima's output as input"),
               _("Sometimes one wants maxima to loose the information that a function "
                 "name was used with the ' operator in order to make maxima not "
                 "evaluate "
                 "it. In other places one wants % to mean \"the last expression at "
                 "the "
                 "time this function was created\", not \"the last expression now\".\n"
                 "In both cases the '' operator will do what is requested."),
               wxEmptyString, wxS("''#1#;"), wxS("Expression:"), expr,
               _("Expression whose output is to be used as maxima's input."),
               wxS("Contents:"), expr, _("Comma-separated expressions."));
  }
  else if(event.GetId() == EventIDs::menu_quote){
    m_wxMaxima.CommandWiz(_("Don't evaluate one command"),
               _("Maxima automatically simplifies expressions it gets as input "
                 "and then tries to evaluate their value. The ' operator "
                 "tells maxima that we want a command to be in noun form, "
                 "which means: "
                 "stand here as is, and unevaluated.\n"
                 "The ' operator can be undone by using the '' operator."),
               wxEmptyString, wxS("'#1#;"), wxS("Command:"), expr,
               _("The name of a function we don't want to be evaluated here"));
  }
  else if(event.GetId() == EventIDs::menu_quoteblock){
    m_wxMaxima.CommandWiz(
               _("Don't evaluate one whole expression"), wxEmptyString, wxEmptyString,
               wxS("'(#1#);"), _("expression:"), expr,
               _("The name of an expression that we don't want to be evaluated."));
  }
  else if(event.GetId() == EventIDs::menu_def_fun){
    m_wxMaxima.CommandWiz(_("Define a function"), wxEmptyString, wxEmptyString,
               wxS("#1#(#2#):=#3#;"), _("Function name:"), expr, wxEmptyString,
               _("Parameter(s):"), wxS("x,[y]"),
               _("Comma-separated parameter names. A parameter in square "
                 "brackets [] will be filled with the list of any additional "
                 "arguments the function gets."),
               _("Function contents:"), wxS("sin(x)+lsum(i,i,y)"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gensym){
    m_wxMaxima.MenuCommand("gensym();");
  }
  else if(event.GetId() == EventIDs::menu_def_macro){
    m_wxMaxima.CommandWiz(_("Define a macro"), wxEmptyString, wxEmptyString,
               wxS("#1#(#2#)::=#3#;"), _("Macro name:"), expr, wxEmptyString,
               _("Parameter(s):"), wxS("x,[y]"),
               _("Comma-separated parameter names. A parameter in square "
                 "brackets [] will be filled with the list of any additional "
                 "arguments the function gets."),
               _("Macro contents:"), wxS("sin(x)+lsum(i,i,y)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_def_variable){
    m_wxMaxima.CommandWiz(_("Define a variable"), wxEmptyString, wxEmptyString,
               wxS("#1#:#2#;"), _("Variable name:"), expr, wxEmptyString,
               _("Contents:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_compile){
    m_wxMaxima.CommandWiz(
               _("Compile a function"),
               _("Compiling a function can generate a considerable speed boost if "
                 "the types of the function parameters are made known to the function "
                 "before it is compiled. Else the generated code has to be "
                 "hideously generic."),
               wxEmptyString, wxS("compile(#1#);"), _("Function name(s):"), expr,
               _("Comma-separated function names"));
  }
  else if(event.GetId() == EventIDs::menu_paramType){
    m_wxMaxima.CommandWiz(_("Declare the type of a function parameter"),
               _("If the type of a function parameter is known when compiling "
                 "a function "
                 "the code can vastly be optimized.\n"
                 "Known types:\n\n"
                 "array, boolean, integer, fixnum (machine-length integer), "
                 "float (machine-size floating-point numbers), "
                 "real or any (which is useful for declaring arrays of any)"),
               wxEmptyString, wxS("mode_declare(#1#);"), _("Parameter name:"),
               expr, wxEmptyString, _("Type:"), wxS("boolean"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_structdef){
    m_wxMaxima.CommandWiz(_("Define a structure type"), wxEmptyString, wxEmptyString,
               wxS("defstruct(#1#(#2#));"), _("Struct type name:"), expr,
               _("The name of the new struct type"), _("Fields:"), wxS("U,I"),
               _("The comma-separated names of the struct fields"));
  }
  else if(event.GetId() == EventIDs::menu_structnew){
    m_wxMaxima.CommandWiz(_("Define a structure"), wxEmptyString, wxEmptyString,
               wxS("new(#1#(#2#));"), _("Struct type name:"), expr,
               _("The name of the struct type"), _("Field contents:"),
               wxS("1,2"),
               _("The comma-separated contents of the struct fields"));
  }
  else if(event.GetId() == EventIDs::menu_structuse){
    m_wxMaxima.CommandWiz(_("Read a structure field"), wxEmptyString, wxEmptyString,
               wxS("#1#@#2#;"), _("Struct :"), expr,
               _("The name of the struct"), _("Field name:"), wxS("U"),
               _("The name of the field to read"));
  }
  else if(event.GetId() == EventIDs::menu_saveLisp){
    m_wxMaxima.CommandWiz(
               _("Save as lisp code"), wxEmptyString, wxEmptyString, wxS("save(#1#);"),
               _("filename:"), wxEmptyString, _("Elements:"), expr,
               _("Comma-separated names of the elements that shall be written"));
  }
  else if(event.GetId() == EventIDs::menu_loadLisp){
    m_wxMaxima.CommandWiz(_("Load lisp code"), wxEmptyString, wxEmptyString,
               wxS("load(#1#);"), _("filename:"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_maximatostring){
    m_wxMaxima.CommandWiz(
               _("Maxima to string"), wxEmptyString, wxEmptyString,
               wxS("sconcat(#1#);"), _("Expression(s):"), expr,
               _("Comma-separated expressions that shall be converted to a string"));
  }
  if(event.GetId() == EventIDs::menu_stringproc_setposition){
    m_wxMaxima.CommandWiz(_("Seek to position"), wxEmptyString, wxEmptyString,
               wxS("fposition(#1#,#2#);"), _("Stream:"), expr, wxEmptyString,
               _("Position:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_getposition){
    m_wxMaxima.CommandWiz(_("Get position in stream"), wxEmptyString, wxEmptyString,
               wxS("fposition(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_flush_output){
    m_wxMaxima.CommandWiz(_("Flush stream"), wxEmptyString, wxEmptyString,
               wxS("flush_output(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_flength){
    m_wxMaxima.CommandWiz(_("Stream length"), wxEmptyString, wxEmptyString,
               wxS("flength(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_close){
    m_wxMaxima.CommandWiz(_("Close Stream"), wxEmptyString, wxEmptyString,
               wxS("close(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_opena){
    m_wxMaxima.CommandWiz(_("Open for appending"), wxEmptyString, wxEmptyString,
               wxS("#1#:opena(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_openr){
    m_wxMaxima.CommandWiz(_("Open for reading"), wxEmptyString, wxEmptyString,
               wxS("#1#:openr(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_openw){
    m_wxMaxima.CommandWiz(_("Open for writing"), wxEmptyString, wxEmptyString,
               wxS("#1#:openw(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_printf){
    m_wxMaxima.CommandWiz(
               _("printf"), wxEmptyString, wxEmptyString, wxS("printf(#1#,#2#,#3#);"),
               _("Stream:"), wxS("false"), wxEmptyString, _("Lisp format string:"),
               wxS("~a"),
               _("Lisp format strings are more powerful than c++ format strings"),
               _("Arguments:"), expr, _("Comma-separated arguments"));
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readline){
    m_wxMaxima.CommandWiz(_("Read line"), wxEmptyString, wxEmptyString,
               wxS("readline(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readchar){
    m_wxMaxima.CommandWiz(_("Read char"), wxEmptyString, wxEmptyString,
               wxS("readchar(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readbyte){
    m_wxMaxima.CommandWiz(_("Read byte"), wxEmptyString, wxEmptyString,
               wxS("readbyte(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_writebyte){
    m_wxMaxima.CommandWiz(_("Read byte"), wxEmptyString, wxEmptyString,
               wxS("writebyte(#1#,#2#);"), _("Byte:"), wxS("65"), wxEmptyString,
               _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charp){
    m_wxMaxima.CommandWiz(
               _("Is a char?"), _("Chars are strings that are one character long"),
               wxEmptyString, wxS("charp(#1#);"), _("Object:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_alphacharp){
    m_wxMaxima.CommandWiz(_("Is an alphabetic char?"), wxEmptyString, wxEmptyString,
               wxS("alphacharp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_alphanumericp){
    m_wxMaxima.CommandWiz(_("Is an alphanumeric char?"), wxEmptyString, wxEmptyString,
               wxS("alphanumericp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_digitcharp){
    m_wxMaxima.CommandWiz(_("Is a digit?"), wxEmptyString, wxEmptyString,
               wxS("alphanumericp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_constituent){
    m_wxMaxima.CommandWiz(_("Is a printable char?"), wxEmptyString, wxEmptyString,
               wxS("constituent(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_uppercasep){
    m_wxMaxima.CommandWiz(_("Is a uppercase char?"), wxEmptyString, wxEmptyString,
               wxS("uppercasep(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_lowercasep){
    m_wxMaxima.CommandWiz(_("Is a lowercase char?"), wxEmptyString, wxEmptyString,
               wxS("lowercasep(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_create_ascii){
    m_wxMaxima.CommandWiz(_("Ascii code to char"), wxEmptyString, wxEmptyString,
               wxS("ascii(#1#);"), _("Code number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cequal){
    m_wxMaxima.CommandWiz(_("Are the chars equal?"), wxEmptyString, wxEmptyString,
               wxS("cequal(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cequalignore){
    m_wxMaxima.CommandWiz(_("Are the chars equal, if case is ignored?"), wxEmptyString,
               wxEmptyString, wxS("cequalignore(#1#,#2#);"), _("Char #1:"),
               expr, wxEmptyString, _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_clessp){
    m_wxMaxima.CommandWiz(_("Is Char 1 less than Char2?"), wxEmptyString, wxEmptyString,
               wxS("clessp(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_clesspignore){
    m_wxMaxima.CommandWiz(_("Is Char 1 less than Char2, if case is ignored?"),
               wxEmptyString, wxEmptyString, wxS("clesspignore(#1#,#2#);"),
               _("Char #1:"), expr, wxEmptyString, _("Char #2:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cgreaterp){
    m_wxMaxima.CommandWiz(_("Is Char 1 greater than Char2?"), wxEmptyString, wxEmptyString,
               wxS("cgreaterp(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cgreaterpignore){
    m_wxMaxima.CommandWiz(_("Is Char 1 greater than Char2, if case is ignored?"),
               wxEmptyString, wxEmptyString, wxS("cgreaterpignore(#1#,#2#);"),
               _("Char #1:"), expr, wxEmptyString, _("Char #2:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sequal){
    m_wxMaxima.CommandWiz(_("Are these strings equal?"), wxEmptyString, wxEmptyString,
               wxS("sequal(#1#, #2#);"), _("String #1:"), expr, wxEmptyString,
               _("String #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sequalignore){
    m_wxMaxima.CommandWiz(_("Are these strings equal if case is ignored?"), wxEmptyString,
               wxEmptyString, wxS("sequalignore(#1#, #2#);"), _("String #1:"), expr,
               wxEmptyString, _("String #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ascii){
    m_wxMaxima.CommandWiz(_("Ascii code to char"), wxEmptyString, wxEmptyString,
               wxS("ascii(#1#);"), _("Code number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cint){
    m_wxMaxima.CommandWiz(_("Char to unicode code point"), wxEmptyString, wxEmptyString,
               wxS("cint(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_unicode){
    m_wxMaxima.CommandWiz(_("Char to unicode code point"), wxEmptyString, wxEmptyString,
               wxS("cint(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_unicode_to_utf8){
    m_wxMaxima.CommandWiz(_("Unicode code point to char"), wxEmptyString, wxEmptyString,
               wxS("unicode(#1#);"), _("Codepoint number:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_utf8_to_unicode){
    m_wxMaxima.CommandWiz(_("Unicode code point to utf8 numbers"), wxEmptyString,
               wxEmptyString, wxS("utf8_to_unicode(#1#);"),
               _("Codepoint number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charat){
    m_wxMaxima.CommandWiz(_("Extract the nth char of a string"), wxEmptyString,
               wxEmptyString, wxS("charat(#1#, #2#);"), _("String:"), expr,
               wxEmptyString, _("n:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charlist){
    m_wxMaxima.CommandWiz(_("String to list of char"), wxEmptyString, wxEmptyString,
               wxS("charlist(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_simplode){
    m_wxMaxima.CommandWiz(_("List of char to String"), wxEmptyString, wxEmptyString,
               wxS("simplode(#1#);"), _("List of chars:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sinsert){
    m_wxMaxima.CommandWiz(_("Insert a string into another"), wxEmptyString, wxEmptyString,
               wxS("sinsert(#1#, #2#, #3#);"), _("New part:"), expr, wxEmptyString,
               _("String:"), wxEmptyString, wxEmptyString, _("Position:"),
               wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_eval_string){
    m_wxMaxima.CommandWiz(_("Evaluate string"), wxEmptyString, wxEmptyString,
               wxS("eval_string(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_parse_string){
    m_wxMaxima.CommandWiz(_("Parse string"), wxEmptyString, wxEmptyString,
               wxS("parse_string(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_scopy){
    m_wxMaxima.CommandWiz(_("Copy string"),
               _("In order to save memory the : operator doesn't create an "
                 "individual copy of the string, but a clone that changes when "
                 "the original string changes."),
               wxEmptyString, wxS("scopy(#1#);"), _("String:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sdowncase){
    m_wxMaxima.CommandWiz(_("Convert string to lowercase"), wxEmptyString, wxEmptyString,
               wxS("sdowncase(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_slength){
    m_wxMaxima.CommandWiz(_("String length"), wxEmptyString, wxEmptyString,
               wxS("slength(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_smake){
    m_wxMaxima.CommandWiz(_("Create empty string"), wxEmptyString, wxEmptyString,
               wxS("smake(#1#,#2#);"), _("String:"), expr, wxEmptyString,
               _("Length:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_smismatch){
    m_wxMaxima.CommandWiz(_("Find first difference"), wxEmptyString, wxEmptyString,
               wxS("smismatch(#1#,#2#);"), _("String #1:"), expr, wxEmptyString,
               _("String #2:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_split){
    m_wxMaxima.CommandWiz(_("Split"), wxEmptyString, wxEmptyString, wxS("split(#1#,#2#);"),
               _("String:"), expr, wxEmptyString, _("Delimiter:"), wxS(";"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sposition){
    m_wxMaxima.CommandWiz(_("Find char in string"), wxEmptyString, wxEmptyString,
               wxS("sposition(#1#,#2#);"), _("Char:"), wxS(";"), wxEmptyString,
               _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sremove){
    m_wxMaxima.CommandWiz(_("Remove all occurrences of part"), wxEmptyString,
               wxEmptyString, wxS("sremove(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sremovefirst){
    m_wxMaxima.CommandWiz(_("Remove first occurrence of part"), wxEmptyString,
               wxEmptyString, wxS("sremovefirst(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_tokens){
    m_wxMaxima.CommandWiz(_("Split string into tokens"), wxEmptyString, wxEmptyString,
               wxS("tokens(#1#,#2#);"), _("String:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssearch){
    m_wxMaxima.CommandWiz(_("Search first occurrence of part"), wxEmptyString,
               wxEmptyString, wxS("ssearch(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssort){
    m_wxMaxima.CommandWiz(_("Sort all characters in string"), wxEmptyString, wxEmptyString,
               wxS("ssort(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssubstfirst){
    m_wxMaxima.CommandWiz(_("Replace the first occurrence of Part"), wxEmptyString,
               wxEmptyString, wxS("ssubstfirst(#1#,#2#);"), _("Part:"),
               wxEmptyString, wxEmptyString, _("String:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_strim){
    m_wxMaxima.CommandWiz(_("Trim string on both ends"), wxEmptyString, wxEmptyString,
               wxS("strim(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_striml){
    m_wxMaxima.CommandWiz(_("Trim string left"), wxEmptyString, wxEmptyString,
               wxS("striml(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_strimr){
    m_wxMaxima.CommandWiz(_("Trim string right"), wxEmptyString, wxEmptyString,
               wxS("strimr(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_number_to_octets){
    m_wxMaxima.CommandWiz(_("Number to octets"), wxEmptyString, wxEmptyString,
               wxS("number_to_octets(#1#);"), _("Number:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_octets_to_number){
    m_wxMaxima.CommandWiz(_("Octets to Number"), wxEmptyString, wxEmptyString,
               wxS("octets_to_number(#1#);"), _("Octets:"),
               _("Comma-separated numbers from 0 to 255"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_octets_to_string){
    m_wxMaxima.CommandWiz(_("Octets to String"), wxEmptyString, wxEmptyString,
               wxS("octets_to_string(#1#);"), _("Octets:"),
               _("Comma-separated numbers from 0 to 255"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_string_to_octets){
    m_wxMaxima.CommandWiz(_("String to octets"), wxEmptyString, wxEmptyString,
               wxS("string_to_octets(#1#);"), _("String:"), wxEmptyString,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_stringtomaxima){
    m_wxMaxima.CommandWiz(_("Interpret string as maxima code"), wxEmptyString,
               wxEmptyString, wxS("parse_string(#1#);"), wxS("String:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_sregex_load){
    m_wxMaxima.MenuCommand("load(\"sregex\");");
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_compile){
    m_wxMaxima.CommandWiz(_("Compile regex"),
               _("Re-using a compiled regex is faster that using the same "
                 "regex string multiple times."),
               wxEmptyString, wxS("regex_compile(#1#);"), wxS("String:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_match_pos){
    m_wxMaxima.CommandWiz(_("Regex match position"), wxEmptyString, wxEmptyString,
               wxS("regex_match_pos(#1#,#2#);"), wxS("Regex:"), expr,
               wxEmptyString, wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_match){
    m_wxMaxima.CommandWiz(_("Regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_match(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  if(event.GetId() == EventIDs::menu_sregex_regex_split){
    m_wxMaxima.CommandWiz(_("Split on regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_split(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_subst_first){
    m_wxMaxima.CommandWiz(_("Replace first regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_subst_first(#1#,#2#);"), wxS("Regex:"), expr,
               wxEmptyString, wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_subst){
    m_wxMaxima.CommandWiz(_("Replace all regex matches"), wxEmptyString, wxEmptyString,
               wxS("regex_subst(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_string_to_regex){
    m_wxMaxima.CommandWiz(_("Convert string to matching regex"),
               _("Escapes all special characters in a string. The result is a "
                 "regex that matches this string exactly."),
               wxEmptyString, wxS("string_to_regex(#1#);"), wxS("String:"),
               expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_load){
    m_wxMaxima.MenuCommand("load(\"operatingsystem\");");
  }

  else if(event.GetId() == EventIDs::menu_opsyst_chdir){
    m_wxMaxima.CommandWiz(_("Change directory"), wxEmptyString, wxEmptyString,
               wxS("chdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_mkdir){
    m_wxMaxima.CommandWiz(_("Create directory"), wxEmptyString, wxEmptyString,
               wxS("mkdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }
  else if(event.GetId() == EventIDs::menu_opsyst_rmdir){
    m_wxMaxima.CommandWiz(_("Remove directory"), wxEmptyString, wxEmptyString,
               wxS("rmdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_getcurrentdirectory){
    m_wxMaxima.MenuCommand(wxS("getcurrentdirectory();"));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_copy_file){
    m_wxMaxima.CommandWiz(_("Copy file"), wxEmptyString, wxEmptyString,
               wxS("copy_file(#1#,#2#);"), wxS("Source:"), expr, wxEmptyString,
               wxS("Destination:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_rename_file){
    m_wxMaxima.CommandWiz(_("Rename file"), wxEmptyString, wxEmptyString,
               wxS("rename_file(#1#,#2#);"), wxS("Source:"), expr,
               wxEmptyString, wxS("Destination:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_delete_file){
    m_wxMaxima.CommandWiz(_("Delete file"), wxEmptyString, wxEmptyString,
               wxS("delete_file(#1#);"), wxS("File:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_getenv){
    m_wxMaxima.CommandWiz(_("Read environment variable"), wxEmptyString, wxEmptyString,
               wxS("getenv(#1#);"), wxS("Variable name:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_directory){
    m_wxMaxima.CommandWiz(
               _("Read Directory"), wxEmptyString, wxEmptyString,
               wxS("directory(#1#);"), wxS("Directory name:"), expr,
               _("\".\" = \"The current directory\"\n\"..\" = \"One directory up\""));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_directory){
    m_wxMaxima.CommandWiz(_("Extract directory part"), wxEmptyString, wxEmptyString,
               wxS("pathname_directory(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_name){
    m_wxMaxima.CommandWiz(_("Extract filename part"), wxEmptyString, wxEmptyString,
               wxS("pathname_name(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_type){
    m_wxMaxima.CommandWiz(_("Extract file type extension"), wxEmptyString, wxEmptyString,
               wxS("pathname_type(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_add_path) {
    if (m_wxMaxima.m_lastPath.Length() == 0)
      m_wxMaxima.m_lastPath = wxGetHomeDir();
    wxString dir = wxDirSelector(_("Add dir to path:"), m_wxMaxima.m_lastPath);
    if (dir.Length()) {
      m_wxMaxima.m_lastPath = dir;
#if defined(__WXMSW__)
      dir.Replace(wxS("\\"), wxS("/"));
#endif
      wxString cmd = wxS("file_search_maxima : cons(sconcat(\"") + dir +
        wxS("/###.{lisp,mac,mc}\"), file_search_maxima)$");
      m_wxMaxima.MenuCommand(cmd);
    }
  }
  else if((event.GetId() == EventIDs::menu_evaluate_all_visible) ||
          (event.GetId() == ToolBar::tb_eval_all)) {
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    m_wxMaxima.GetWorksheet()->ResetInputPrompts();
    m_wxMaxima.EvaluationQueueLength(0);
    if (m_wxMaxima.m_configuration.RestartOnReEvaluation())
      m_wxMaxima.StartMaxima();
    m_wxMaxima.GetWorksheet()->AddDocumentToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                          m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
    m_wxMaxima.TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::menu_evaluate_all) {
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    m_wxMaxima.GetWorksheet()->ResetInputPrompts();
    m_wxMaxima.EvaluationQueueLength(0);
    if (m_wxMaxima.m_configuration.RestartOnReEvaluation())
      m_wxMaxima.StartMaxima();
    m_wxMaxima.GetWorksheet()->AddEntireDocumentToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                          m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
    m_wxMaxima.TriggerEvaluation();
  }
  else if(event.GetId() == ToolBar::tb_evaltillhere) {
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    m_wxMaxima.GetWorksheet()->ResetInputPrompts();
    m_wxMaxima.EvaluationQueueLength(0);
    if (m_wxMaxima.m_configuration.RestartOnReEvaluation())
      m_wxMaxima.StartMaxima();
    m_wxMaxima.GetWorksheet()->AddDocumentTillHereToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                          m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
    m_wxMaxima.TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::menu_clear_var){
    m_wxMaxima.CommandWiz(_("Delete variable(s)"), wxEmptyString, wxEmptyString,
               wxS("remvalue(#1#);"), _("Variable name:"), wxS("all"),
               wxEmptyString);
  }
  if(event.GetId() == EventIDs::menu_kill){
    m_wxMaxima.CommandWiz(_("Delete named object(s)"), wxEmptyString, wxEmptyString,
               wxS("kill(#1#);"), _("Object name:"), wxS("all"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_clear_fun){
    m_wxMaxima.CommandWiz(_("Delete function(s)"), wxEmptyString, wxEmptyString,
               wxS("remfunction(#1#);"), _("Function name:"), wxS("all"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::button_subst) {
    wxWindowPtr<SubstituteWiz> wiz(new SubstituteWiz(&m_wxMaxima, -1, &m_wxMaxima.m_configuration, _("Substitute")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        m_wxMaxima.MenuCommand(val);
      }
    });
  }
}

void MaximaCommandMenus::OnDemoFileMenu(wxCommandEvent &ev)
{
  wxString demoName = m_wxMaxima.GetDemoFile(ev.GetId());
  if(!demoName.IsEmpty())
    m_wxMaxima.MenuCommand(wxS("demo(") + demoName + wxS(");"));
}

void MaximaCommandMenus::EditInputMenu(wxCommandEvent &WXUNUSED(event)) {
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();
  if (!m_wxMaxima.GetWorksheet()->CanEdit())
    return;

  EditorCell *tmp =
    dynamic_cast<EditorCell *>(m_wxMaxima.GetWorksheet()->GetSelectionStart());

  if (tmp == NULL)
    return;

  m_wxMaxima.GetWorksheet()->SetActiveCell(tmp);
}

void MaximaCommandMenus::PrintMenu(wxCommandEvent &event) {
  if(!m_wxMaxima.GetWorksheet())
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  switch (event.GetId()) {
  case wxID_PRINT: {
    wxPrintDialogData printDialogData;
    if (m_wxMaxima.m_printData)
      printDialogData.SetPrintData(*m_wxMaxima.m_printData);
    wxPrinter printer(&printDialogData);
    wxString title(_("wxMaxima document"));

    if (m_wxMaxima.GetWorksheet()->GetCurrentFile().Length()) {
      wxString suffix;
      wxFileName::SplitPath(m_wxMaxima.GetWorksheet()->GetCurrentFile(), NULL, NULL, &title,
                            &suffix);
      title << wxS(".") << suffix;
    }

    {
      // Redraws during printing might end up on paper => temporarily block all
      // redraw events for the console
      //      wxWindowUpdateLocker noUpdates(m_wxMaxima.GetWorksheet());
      wxEventBlocker blocker(m_wxMaxima.GetWorksheet());
      Printout printout(title, m_wxMaxima.GetWorksheet()->GetTree(), m_wxMaxima.GetContentScaleFactor());
      wxBusyCursor crs;
      if (printer.Print(&m_wxMaxima, &printout, true)) {
        m_wxMaxima.m_printData = std::unique_ptr<wxPrintData>(
                                                   new wxPrintData(printer.GetPrintDialogData().GetPrintData()));
      }
    }
    m_wxMaxima.GetWorksheet()->RequestRecalculation();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
    break;
  }
  }
}
