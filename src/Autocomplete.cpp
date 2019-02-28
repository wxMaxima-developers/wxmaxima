// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2018 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class AutoComplete.

  AutoComplete creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#include <wx/sstream.h>
#include "Autocomplete.h"
#include "Dirstructure.h"

#include <wx/textfile.h>
#include <wx/filename.h>
#include <wx/xml/xml.h>

AutoComplete::AutoComplete(Configuration *configuration)
{
  wxASSERT(m_args.Compile(wxT("[[]<([^>]*)>[]]")));
  m_configuration = configuration;
}

void AutoComplete::ClearWorksheetWords()
{
  m_worksheetWords.clear();
}

void AutoComplete::AddSymbols(wxString xml)
{
  wxXmlDocument xmldoc;
  wxStringInputStream xmlStream(xml);
  xmldoc.Load(xmlStream, wxT("UTF-8"));
  wxXmlNode *node = xmldoc.GetRoot();
  if(node != NULL)
  {
    wxXmlNode *children = node->GetChildren();
    while (children != NULL)
    {
      if(children->GetType() == wxXML_ELEMENT_NODE)
      { 
        if (children->GetName() == wxT("function"))
        {
          wxXmlNode *val = children->GetChildren();
          if(val)
          {
            wxString name = val->GetContent();
            AddSymbol(name, command);
          }
        }

        if (children->GetName() == wxT("template"))
        {
          wxXmlNode *val = children->GetChildren();
          if(val)
          {
            wxString name = val->GetContent();
            AddSymbol(name, tmplte);
          }
        }

        if (children->GetName() == wxT("unit"))
        {
          wxXmlNode *val = children->GetChildren();
          if(val)
          {
            wxString name = val->GetContent();
            AddSymbol(name, unit);
          }
        }

        if (children->GetName() == wxT("value"))
        {
          wxXmlNode *val = children->GetChildren();
          if(val)
          {
            wxString name = val->GetContent();
            AddSymbol(name, command);
          }
        }
      }
      children = children->GetNext();
    }
  }
}
void AutoComplete::AddWorksheetWords(wxArrayString wordlist)
{
  wxArrayString::iterator it;
  for (it = wordlist.begin(); it != wordlist.end(); ++it)
  {
    if (m_worksheetWords[*it] != 1)
      m_worksheetWords[*it] = 1;
  }
}

bool AutoComplete::LoadSymbols()
{  
  for (int i = command; i <= unit; i++)
  {
    if (m_wordList[i].GetCount() != 0)
      m_wordList[i].Clear();
  }

  for(Configuration::StringHash::iterator it = m_configuration->m_escCodes.begin();
      it != m_configuration->m_escCodes.end();
      ++it)
    m_wordList[esccommand].Add(it->first);

  wxString line;

  // Add maxima functions
  m_wordList[command].Add("pathname_name"); // FUNCTION
  m_wordList[command].Add("fast_linsolve"); // FUNCTION
  m_wordList[tmplte ].Add("fast_linsolve([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("grobner_basis"); // FUNCTION
  m_wordList[tmplte ].Add("grobner_basis([<expr_1>, ..., <expr_m>])"); // OPTION
  m_wordList[command].Add("set_up_dot_simplifications"); // FUNCTION
  m_wordList[tmplte ].Add("set_up_dot_simplifications(<eqns>, <check_through_degree>)"); // OPTION
  m_wordList[tmplte ].Add("set_up_dot_simplifications(<eqns>)"); // OPTION
  m_wordList[command].Add("declare_weights"); // FUNCTION
  m_wordList[tmplte ].Add("declare_weights(<x_1>, <w_1>, ..., <x_n>, <w_n>)"); // OPTION
  m_wordList[command].Add("nc_degree"); // FUNCTION
  m_wordList[tmplte ].Add("nc_degree(<p>)"); // OPTION
  m_wordList[command].Add("dotsimp"); // FUNCTION
  m_wordList[tmplte ].Add("dotsimp(<f>)"); // OPTION
  m_wordList[command].Add("fast_central_elements"); // FUNCTION
  m_wordList[tmplte ].Add("fast_central_elements([<x_1>, ..., <x_n>], <n>)"); // OPTION
  m_wordList[command].Add("check_overlaps"); // FUNCTION
  m_wordList[tmplte ].Add("check_overlaps(<n>, <add_to_simps>)"); // OPTION
  m_wordList[command].Add("mono"); // FUNCTION
  m_wordList[tmplte ].Add("mono([<x_1>, ..., <x_n>], <n>)"); // OPTION
  m_wordList[command].Add("monomial_dimensions"); // FUNCTION
  m_wordList[tmplte ].Add("monomial_dimensions(<n>)"); // OPTION
  m_wordList[command].Add("extract_linear_equations"); // FUNCTION
  m_wordList[tmplte ].Add("extract_linear_equations([<p_1>, ..., <p_n>], [<m_1>, ..., <m_n>])"); // OPTION
  m_wordList[command].Add("list_nc_monomials"); // FUNCTION
  m_wordList[tmplte ].Add("list_nc_monomials([<p_1>, ..., <p_n>])"); // OPTION
  m_wordList[tmplte ].Add("list_nc_monomials(<p>)"); // OPTION
  m_wordList[command].Add("all_dotsimp_denoms"); // OPTION
  m_wordList[command].Add("array"); // FUNCTION
  m_wordList[tmplte ].Add("array(<name>, <dim_1>, ..., <dim_n>)"); // OPTION
  m_wordList[tmplte ].Add("array(<name>, <type>, <dim_1>, ..., <dim_n>)"); // OPTION
  m_wordList[tmplte ].Add("array([<name_1>, ..., <name_m>], <dim_1>, ..., <dim_n>)"); // OPTION
  m_wordList[command].Add("arrayapply"); // FUNCTION
  m_wordList[tmplte ].Add("arrayapply(<A>, [<i_1>, ..., <i_n>])"); // OPTION
  m_wordList[command].Add("arrayinfo"); // FUNCTION
  m_wordList[tmplte ].Add("arrayinfo(<A>)"); // OPTION
  m_wordList[command].Add("arraymake"); // FUNCTION
  m_wordList[tmplte ].Add("arraymake(<A>, [<i_1>, ..., <i_n>])"); // OPTION
  m_wordList[command].Add("arrays"); // OPTION
  m_wordList[command].Add("bashindices"); // FUNCTION
  m_wordList[tmplte ].Add("bashindices(<expr>)"); // OPTION
  m_wordList[command].Add("fillarray"); // FUNCTION
  m_wordList[tmplte ].Add("fillarray(<A>, <B>)"); // OPTION
  m_wordList[command].Add("listarray"); // FUNCTION
  m_wordList[tmplte ].Add("listarray(<A>)"); // OPTION
  m_wordList[command].Add("make_array"); // FUNCTION
  m_wordList[tmplte ].Add("make_array(<type>, <dim_1>, ..., <dim_n>)"); // OPTION
  m_wordList[command].Add("rearray"); // FUNCTION
  m_wordList[tmplte ].Add("rearray(<A>, <dim_1>, ..., <dim_n>)"); // OPTION
  m_wordList[command].Add("remarray"); // FUNCTION
  m_wordList[tmplte ].Add("remarray(<A_1>, ..., <A_n>)"); // OPTION
  m_wordList[tmplte ].Add("remarray(all)"); // OPTION
  m_wordList[command].Add("subvar"); // FUNCTION
  m_wordList[tmplte ].Add("subvar(<x>, <i>)"); // OPTION
  m_wordList[command].Add("use_fast_arrays"); // OPTION
  m_wordList[command].Add("init_atensor"); // FUNCTION
  m_wordList[tmplte ].Add("init_atensor(<alg_type>, <opt_dims>)"); // OPTION
  m_wordList[tmplte ].Add("init_atensor(<alg_type>)"); // OPTION
  m_wordList[command].Add("atensimp"); // FUNCTION
  m_wordList[tmplte ].Add("atensimp(<expr>)"); // OPTION
  m_wordList[command].Add("adim"); // OPTION
  m_wordList[command].Add("aform"); // OPTION
  m_wordList[command].Add("asymbol"); // OPTION
  m_wordList[command].Add("sf"); // FUNCTION
  m_wordList[tmplte ].Add("sf(<u>, <v>)"); // OPTION
  m_wordList[command].Add("af"); // FUNCTION
  m_wordList[tmplte ].Add("af(<u>, <v>)"); // OPTION
  m_wordList[command].Add("av"); // FUNCTION
  m_wordList[tmplte ].Add("av(<u>, <v>)"); // OPTION
  m_wordList[command].Add("abasep"); // FUNCTION
  m_wordList[tmplte ].Add("abasep(<v>)"); // OPTION
  m_wordList[command].Add("augmented_lagrangian_method"); // FUNCTION
  m_wordList[tmplte ].Add("augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>)"); // OPTION
  m_wordList[tmplte ].Add("augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>, optional_args)"); // OPTION
  m_wordList[tmplte ].Add("augmented_lagrangian_method([<FOM>, <grad>], <xx>, <C>, <yy>)"); // OPTION
  m_wordList[tmplte ].Add("augmented_lagrangian_method([<FOM>, <grad>], <xx>, <C>, <yy>, optional_args)"); // OPTION
  m_wordList[command].Add("bode_gain"); // FUNCTION
  m_wordList[tmplte ].Add("bode_gain(<H>, <range>, ...<plot_opts>...)"); // OPTION
  m_wordList[command].Add("bode_phase"); // FUNCTION
  m_wordList[tmplte ].Add("bode_phase(<H>, <range>, ...<plot_opts>...)"); // OPTION
  m_wordList[command].Add("run_testsuite"); // FUNCTION
  m_wordList[tmplte ].Add("run_testsuite([<options>])"); // OPTION
  m_wordList[command].Add("testsuite_files"); // OPTION
  m_wordList[command].Add("share_testsuite_files"); // OPTION
  m_wordList[command].Add("display_all"); // OPTION
  m_wordList[command].Add("display_known_bugs"); // OPTION
  m_wordList[command].Add("tests"); // OPTION
  m_wordList[command].Add("time"); // OPTION
  m_wordList[command].Add("share_tests"); // OPTION
  m_wordList[command].Add("bug_report"); // FUNCTION
  m_wordList[tmplte ].Add("bug_report()"); // OPTION
  m_wordList[command].Add("build_info"); // FUNCTION
  m_wordList[tmplte ].Add("build_info()"); // OPTION
  m_wordList[command].Add("alias"); // FUNCTION
  m_wordList[tmplte ].Add("alias(<new_name_1>, <old_name_1>, ..., <new_name_n>, <old_name_n>)"); // OPTION
  m_wordList[command].Add("debugmode"); // OPTION
  m_wordList[command].Add("ev"); // FUNCTION
  m_wordList[tmplte ].Add("ev(<expr>, <arg_1>, ..., <arg_n>)"); // OPTION
  m_wordList[command].Add("eval"); // OPTION
  m_wordList[command].Add("evflag"); // OPTION
  m_wordList[command].Add("evfun"); // OPTION
  m_wordList[command].Add("infeval"); // OPTION
  m_wordList[command].Add("kill"); // FUNCTION
  m_wordList[tmplte ].Add("kill(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[tmplte ].Add("kill(labels)"); // OPTION
  m_wordList[tmplte ].Add("kill(inlabels, outlabels, linelabels)"); // OPTION
  m_wordList[tmplte ].Add("kill(<n>)"); // OPTION
  m_wordList[tmplte ].Add("kill([<m>, <n>])"); // OPTION
  m_wordList[tmplte ].Add("kill(values, functions, arrays, ...)"); // OPTION
  m_wordList[tmplte ].Add("kill(all)"); // OPTION
  m_wordList[tmplte ].Add("kill(allbut (<a_1>, ..., <a_n>))"); // OPTION
  m_wordList[command].Add("labels"); // FUNCTION
  m_wordList[tmplte ].Add("labels(<symbol>)"); // OPTION
  m_wordList[command].Add("linenum"); // OPTION
  m_wordList[command].Add("myoptions"); // OPTION
  m_wordList[command].Add("nolabels"); // OPTION
  m_wordList[command].Add("optionset"); // OPTION
  m_wordList[command].Add("playback"); // FUNCTION
  m_wordList[tmplte ].Add("playback()"); // OPTION
  m_wordList[tmplte ].Add("playback(<n>)"); // OPTION
  m_wordList[tmplte ].Add("playback([<m>, <n>])"); // OPTION
  m_wordList[tmplte ].Add("playback([<m>])"); // OPTION
  m_wordList[tmplte ].Add("playback(input)"); // OPTION
  m_wordList[tmplte ].Add("playback(slow)"); // OPTION
  m_wordList[tmplte ].Add("playback(time)"); // OPTION
  m_wordList[tmplte ].Add("playback(grind)"); // OPTION
  m_wordList[command].Add("printprops"); // FUNCTION
  m_wordList[tmplte ].Add("printprops(<a>, <i>)"); // OPTION
  m_wordList[tmplte ].Add("printprops([<a_1>, ..., <a_n>], <i>)"); // OPTION
  m_wordList[tmplte ].Add("printprops(all, <i>)"); // OPTION
  m_wordList[command].Add("prompt"); // OPTION
  m_wordList[command].Add("quit"); // FUNCTION
  m_wordList[tmplte ].Add("quit()"); // OPTION
  m_wordList[command].Add("remfunction"); // FUNCTION
  m_wordList[tmplte ].Add("remfunction(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("remfunction(all)"); // OPTION
  m_wordList[command].Add("reset"); // FUNCTION
  m_wordList[tmplte ].Add("reset()"); // OPTION
  m_wordList[command].Add("showtime"); // OPTION
  m_wordList[command].Add("to_lisp"); // FUNCTION
  m_wordList[tmplte ].Add("to_lisp()"); // OPTION
  m_wordList[command].Add("values"); // OPTION
  m_wordList[command].Add("%e"); // OPTION
  m_wordList[command].Add("%i"); // OPTION
  m_wordList[command].Add("false"); // OPTION
  m_wordList[command].Add("ind"); // OPTION
  m_wordList[command].Add("inf"); // OPTION
  m_wordList[command].Add("infinity"); // OPTION
  m_wordList[command].Add("minf"); // OPTION
  m_wordList[command].Add("%phi"); // OPTION
  m_wordList[command].Add("%pi"); // OPTION
  m_wordList[command].Add("true"); // OPTION
  m_wordList[command].Add("und"); // OPTION
  m_wordList[command].Add("zeroa"); // OPTION
  m_wordList[command].Add("zerob"); // OPTION
  m_wordList[command].Add("activate"); // FUNCTION
  m_wordList[tmplte ].Add("activate(<context_1>, ..., <context_n>)"); // OPTION
  m_wordList[command].Add("activecontexts"); // OPTION
  m_wordList[command].Add("assume"); // FUNCTION
  m_wordList[tmplte ].Add("assume(<pred_1>, ..., <pred_n>)"); // OPTION
  m_wordList[command].Add("assumescalar"); // OPTION
  m_wordList[command].Add("assume_pos"); // OPTION
  m_wordList[command].Add("assume_pos_pred"); // OPTION
  m_wordList[command].Add("context"); // OPTION
  m_wordList[command].Add("contexts"); // OPTION
  m_wordList[command].Add("deactivate"); // FUNCTION
  m_wordList[tmplte ].Add("deactivate(<context_1>, ..., <context_n>)"); // OPTION
  m_wordList[command].Add("facts"); // FUNCTION
  m_wordList[tmplte ].Add("facts(<item>)"); // OPTION
  m_wordList[tmplte ].Add("facts()"); // OPTION
  m_wordList[command].Add("features"); // OPTION
  m_wordList[command].Add("forget"); // FUNCTION
  m_wordList[tmplte ].Add("forget(<pred_1>, ..., <pred_n>)"); // OPTION
  m_wordList[tmplte ].Add("forget(<L>)"); // OPTION
  m_wordList[command].Add("killcontext"); // FUNCTION
  m_wordList[tmplte ].Add("killcontext(<context_1>, ..., <context_n>)"); // OPTION
  m_wordList[command].Add("newcontext"); // FUNCTION
  m_wordList[tmplte ].Add("newcontext(<name>)"); // OPTION
  m_wordList[command].Add("supcontext"); // FUNCTION
  m_wordList[tmplte ].Add("supcontext(<name>, <context>)"); // OPTION
  m_wordList[tmplte ].Add("supcontext(<name>)"); // OPTION
  m_wordList[command].Add("contrib_ode"); // FUNCTION
  m_wordList[tmplte ].Add("contrib_ode(<eqn>, <y>, <x>)"); // OPTION
  m_wordList[command].Add("odelin"); // FUNCTION
  m_wordList[tmplte ].Add("odelin(<eqn>, <y>, <x>)"); // OPTION
  m_wordList[command].Add("ode_check"); // FUNCTION
  m_wordList[tmplte ].Add("ode_check(<eqn>, <soln>)"); // OPTION
  m_wordList[command].Add("method"); // OPTION
  m_wordList[command].Add("%c"); // OPTION
  m_wordList[command].Add("%k1"); // OPTION
  m_wordList[command].Add("%k2"); // OPTION
  m_wordList[command].Add("gauss_a"); // FUNCTION
  m_wordList[tmplte ].Add("gauss_a(<a>, <b>, <c>, <x>)"); // OPTION
  m_wordList[command].Add("gauss_b"); // FUNCTION
  m_wordList[tmplte ].Add("gauss_b(<a>, <b>, <c>, <x>)"); // OPTION
  m_wordList[command].Add("dgauss_a"); // FUNCTION
  m_wordList[tmplte ].Add("dgauss_a(<a>, <b>, <c>, <x>)"); // OPTION
  m_wordList[command].Add("dgauss_b"); // FUNCTION
  m_wordList[tmplte ].Add("dgauss_b(<a>, <b>, <c>, <x>)"); // OPTION
  m_wordList[command].Add("kummer_m"); // FUNCTION
  m_wordList[tmplte ].Add("kummer_m(<a>, <b>, <x>)"); // OPTION
  m_wordList[command].Add("kummer_u"); // FUNCTION
  m_wordList[tmplte ].Add("kummer_u(<a>, <b>, <x>)"); // OPTION
  m_wordList[command].Add("dkummer_m"); // FUNCTION
  m_wordList[tmplte ].Add("dkummer_m(<a>, <b>, <x>)"); // OPTION
  m_wordList[command].Add("dkummer_u"); // FUNCTION
  m_wordList[tmplte ].Add("dkummer_u(<a>, <b>, <x>)"); // OPTION
  m_wordList[command].Add("csetup"); // FUNCTION
  m_wordList[tmplte ].Add("csetup()"); // OPTION
  m_wordList[command].Add("cmetric"); // FUNCTION
  m_wordList[tmplte ].Add("cmetric(<dis>)"); // OPTION
  m_wordList[tmplte ].Add("cmetric()"); // OPTION
  m_wordList[command].Add("ct_coordsys"); // FUNCTION
  m_wordList[tmplte ].Add("ct_coordsys(<coordinate_system>, <extra_arg>)"); // OPTION
  m_wordList[tmplte ].Add("ct_coordsys(<coordinate_system>)"); // OPTION
  m_wordList[command].Add("init_ctensor"); // FUNCTION
  m_wordList[tmplte ].Add("init_ctensor()"); // OPTION
  m_wordList[command].Add("christof"); // FUNCTION
  m_wordList[tmplte ].Add("christof(<dis>)"); // OPTION
  m_wordList[command].Add("ricci"); // FUNCTION
  m_wordList[tmplte ].Add("ricci(<dis>)"); // OPTION
  m_wordList[command].Add("uricci"); // FUNCTION
  m_wordList[tmplte ].Add("uricci(<dis>)"); // OPTION
  m_wordList[command].Add("scurvature"); // FUNCTION
  m_wordList[tmplte ].Add("scurvature()"); // OPTION
  m_wordList[command].Add("einstein"); // FUNCTION
  m_wordList[tmplte ].Add("einstein(<dis>)"); // OPTION
  m_wordList[command].Add("leinstein"); // FUNCTION
  m_wordList[tmplte ].Add("leinstein(<dis>)"); // OPTION
  m_wordList[command].Add("riemann"); // FUNCTION
  m_wordList[tmplte ].Add("riemann(<dis>)"); // OPTION
  m_wordList[command].Add("lriemann"); // FUNCTION
  m_wordList[tmplte ].Add("lriemann(<dis>)"); // OPTION
  m_wordList[command].Add("uriemann"); // FUNCTION
  m_wordList[tmplte ].Add("uriemann(<dis>)"); // OPTION
  m_wordList[command].Add("rinvariant"); // FUNCTION
  m_wordList[tmplte ].Add("rinvariant()"); // OPTION
  m_wordList[command].Add("weyl"); // FUNCTION
  m_wordList[tmplte ].Add("weyl(<dis>)"); // OPTION
  m_wordList[command].Add("ctaylor"); // FUNCTION
  m_wordList[tmplte ].Add("ctaylor()"); // OPTION
  m_wordList[command].Add("frame_bracket"); // FUNCTION
  m_wordList[tmplte ].Add("frame_bracket(<fr>, <fri>, <diagframe>)"); // OPTION
  m_wordList[command].Add("nptetrad"); // FUNCTION
  m_wordList[tmplte ].Add("nptetrad()"); // OPTION
  m_wordList[command].Add("psi"); // FUNCTION
  m_wordList[tmplte ].Add("psi(<dis>)"); // OPTION
  m_wordList[command].Add("petrov"); // FUNCTION
  m_wordList[tmplte ].Add("petrov()"); // OPTION
  m_wordList[command].Add("contortion"); // FUNCTION
  m_wordList[tmplte ].Add("contortion(<tr>)"); // OPTION
  m_wordList[command].Add("nonmetricity"); // FUNCTION
  m_wordList[tmplte ].Add("nonmetricity(<nm>)"); // OPTION
  m_wordList[command].Add("ctransform"); // FUNCTION
  m_wordList[tmplte ].Add("ctransform(<M>)"); // OPTION
  m_wordList[command].Add("findde"); // FUNCTION
  m_wordList[tmplte ].Add("findde(<A>, <n>)"); // OPTION
  m_wordList[command].Add("cograd"); // FUNCTION
  m_wordList[tmplte ].Add("cograd()"); // OPTION
  m_wordList[command].Add("contragrad"); // FUNCTION
  m_wordList[tmplte ].Add("contragrad()"); // OPTION
  m_wordList[command].Add("dscalar"); // FUNCTION
  m_wordList[tmplte ].Add("dscalar()"); // OPTION
  m_wordList[command].Add("checkdiv"); // FUNCTION
  m_wordList[tmplte ].Add("checkdiv()"); // OPTION
  m_wordList[command].Add("cgeodesic"); // FUNCTION
  m_wordList[tmplte ].Add("cgeodesic(<dis>)"); // OPTION
  m_wordList[command].Add("bdvac"); // FUNCTION
  m_wordList[tmplte ].Add("bdvac(<f>)"); // OPTION
  m_wordList[command].Add("invariant1"); // FUNCTION
  m_wordList[tmplte ].Add("invariant1()"); // OPTION
  m_wordList[command].Add("invariant2"); // FUNCTION
  m_wordList[tmplte ].Add("invariant2()"); // OPTION
  m_wordList[command].Add("bimetric"); // FUNCTION
  m_wordList[tmplte ].Add("bimetric()"); // OPTION
  m_wordList[command].Add("diagmatrixp"); // FUNCTION
  m_wordList[tmplte ].Add("diagmatrixp(<M>)"); // OPTION
  m_wordList[command].Add("symmetricp"); // FUNCTION
  m_wordList[tmplte ].Add("symmetricp(<M>)"); // OPTION
  m_wordList[command].Add("ntermst"); // FUNCTION
  m_wordList[tmplte ].Add("ntermst(<f>)"); // OPTION
  m_wordList[command].Add("cdisplay"); // FUNCTION
  m_wordList[tmplte ].Add("cdisplay(<ten>)"); // OPTION
  m_wordList[command].Add("deleten"); // FUNCTION
  m_wordList[tmplte ].Add("deleten(<L>, <n>)"); // OPTION
  m_wordList[command].Add("dim"); // OPTION
  m_wordList[command].Add("diagmetric"); // OPTION
  m_wordList[command].Add("ctrgsimp"); // OPTION
  m_wordList[command].Add("cframe_flag"); // OPTION
  m_wordList[command].Add("ctorsion_flag"); // OPTION
  m_wordList[command].Add("cnonmet_flag"); // OPTION
  m_wordList[command].Add("ctayswitch"); // OPTION
  m_wordList[command].Add("ctayvar"); // OPTION
  m_wordList[command].Add("ctaypov"); // OPTION
  m_wordList[command].Add("ctaypt"); // OPTION
  m_wordList[command].Add("gdet"); // OPTION
  m_wordList[command].Add("ratchristof"); // OPTION
  m_wordList[command].Add("rateinstein"); // OPTION
  m_wordList[command].Add("ratriemann"); // OPTION
  m_wordList[command].Add("ratweyl"); // OPTION
  m_wordList[command].Add("lfg"); // OPTION
  m_wordList[command].Add("ufg"); // OPTION
  m_wordList[command].Add("riem"); // OPTION
  m_wordList[command].Add("lriem"); // OPTION
  m_wordList[command].Add("uriem"); // OPTION
  m_wordList[command].Add("ric"); // OPTION
  m_wordList[command].Add("uric"); // OPTION
  m_wordList[command].Add("lg"); // OPTION
  m_wordList[command].Add("ug"); // OPTION
  m_wordList[command].Add("weyl"); // OPTION
  m_wordList[command].Add("fb"); // OPTION
  m_wordList[command].Add("kinvariant"); // OPTION
  m_wordList[command].Add("np"); // OPTION
  m_wordList[command].Add("npi"); // OPTION
  m_wordList[command].Add("tr"); // OPTION
  m_wordList[command].Add("kt"); // OPTION
  m_wordList[command].Add("nm"); // OPTION
  m_wordList[command].Add("nmc"); // OPTION
  m_wordList[command].Add("tensorkill"); // OPTION
  m_wordList[command].Add("ct_coords"); // OPTION
  m_wordList[command].Add("refcheck"); // OPTION
  m_wordList[command].Add("setcheck"); // OPTION
  m_wordList[command].Add("setcheckbreak"); // OPTION
  m_wordList[command].Add("setval"); // OPTION
  m_wordList[command].Add("timer"); // FUNCTION
  m_wordList[tmplte ].Add("timer(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("timer(all)"); // OPTION
  m_wordList[tmplte ].Add("timer()"); // OPTION
  m_wordList[command].Add("untimer"); // FUNCTION
  m_wordList[tmplte ].Add("untimer(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("untimer()"); // OPTION
  m_wordList[command].Add("timer_devalue"); // OPTION
  m_wordList[command].Add("timer_info"); // FUNCTION
  m_wordList[tmplte ].Add("timer_info(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("timer_info()"); // OPTION
  m_wordList[command].Add("trace"); // FUNCTION
  m_wordList[tmplte ].Add("trace(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("trace(all)"); // OPTION
  m_wordList[tmplte ].Add("trace()"); // OPTION
  m_wordList[command].Add("trace_options"); // FUNCTION
  m_wordList[tmplte ].Add("trace_options(<f>, <option_1>, ..., <option_n>)"); // OPTION
  m_wordList[tmplte ].Add("trace_options(<f>)"); // OPTION
  m_wordList[command].Add("untrace"); // FUNCTION
  m_wordList[tmplte ].Add("untrace(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("untrace()"); // OPTION
  m_wordList[command].Add("%ibes"); // FUNCTION
  m_wordList[tmplte ].Add("%ibes[<n>](<x>) "); // OPTION
  m_wordList[command].Add("%j"); // FUNCTION
  m_wordList[tmplte ].Add("%j[<n>](<x>) "); // OPTION
  m_wordList[command].Add("%k"); // FUNCTION
  m_wordList[tmplte ].Add("%k[<n>](<x>) "); // OPTION
  m_wordList[command].Add("%y"); // FUNCTION
  m_wordList[tmplte ].Add("%y[<n>](<x>) "); // OPTION
  m_wordList[command].Add("airy"); // FUNCTION
  m_wordList[tmplte ].Add("airy(<x>)"); // OPTION
  m_wordList[command].Add("bessel"); // FUNCTION
  m_wordList[tmplte ].Add("bessel(<z>, <a>) "); // OPTION
  m_wordList[command].Add("expint"); // FUNCTION
  m_wordList[tmplte ].Add("expint(<z>)"); // OPTION
  m_wordList[command].Add("g0"); // FUNCTION
  m_wordList[tmplte ].Add("g0(<x>) "); // OPTION
  m_wordList[command].Add("g1"); // FUNCTION
  m_wordList[tmplte ].Add("g1(<x>) "); // OPTION
  m_wordList[command].Add("gn"); // FUNCTION
  m_wordList[tmplte ].Add("gn(<x>, <n>) "); // OPTION
  m_wordList[command].Add("gauss"); // FUNCTION
  m_wordList[tmplte ].Add("gauss(<mean>, <sd>)"); // OPTION
  m_wordList[command].Add("i0"); // FUNCTION
  m_wordList[tmplte ].Add("i0(<x>) "); // OPTION
  m_wordList[command].Add("i1"); // FUNCTION
  m_wordList[tmplte ].Add("i1(<x>) "); // OPTION
  m_wordList[command].Add("in"); // FUNCTION
  m_wordList[tmplte ].Add("in(<x>, <n>) "); // OPTION
  m_wordList[command].Add("j0"); // FUNCTION
  m_wordList[tmplte ].Add("j0(<x>) "); // OPTION
  m_wordList[command].Add("j1"); // FUNCTION
  m_wordList[tmplte ].Add("j1(<x>) "); // OPTION
  m_wordList[command].Add("jn"); // FUNCTION
  m_wordList[tmplte ].Add("jn(<x>, <n>) "); // OPTION
  m_wordList[command].Add("continuous_freq"); // FUNCTION
  m_wordList[tmplte ].Add("continuous_freq(<list>)"); // OPTION
  m_wordList[tmplte ].Add("continuous_freq(<list>, <m>)"); // OPTION
  m_wordList[command].Add("discrete_freq"); // FUNCTION
  m_wordList[tmplte ].Add("discrete_freq(<list>)"); // OPTION
  m_wordList[command].Add("subsample"); // FUNCTION
  m_wordList[tmplte ].Add("subsample(<data_matrix>, <predicate_function>)"); // OPTION
  m_wordList[tmplte ].Add("subsample(<data_matrix>, <predicate_function>, <col_num1>, <col_num2>, ...)"); // OPTION
  m_wordList[command].Add("mean"); // FUNCTION
  m_wordList[tmplte ].Add("mean(<list>)"); // OPTION
  m_wordList[tmplte ].Add("mean(<matrix>)"); // OPTION
  m_wordList[command].Add("var"); // FUNCTION
  m_wordList[tmplte ].Add("var(<list>)"); // OPTION
  m_wordList[tmplte ].Add("var(<matrix>)"); // OPTION
  m_wordList[command].Add("var1"); // FUNCTION
  m_wordList[tmplte ].Add("var1(<list>)"); // OPTION
  m_wordList[tmplte ].Add("var1(<matrix>)"); // OPTION
  m_wordList[command].Add("std"); // FUNCTION
  m_wordList[tmplte ].Add("std(<list>)"); // OPTION
  m_wordList[tmplte ].Add("std(<matrix>)"); // OPTION
  m_wordList[command].Add("std1"); // FUNCTION
  m_wordList[tmplte ].Add("std1(<list>)"); // OPTION
  m_wordList[tmplte ].Add("std1(<matrix>)"); // OPTION
  m_wordList[command].Add("noncentral_moment"); // FUNCTION
  m_wordList[tmplte ].Add("noncentral_moment(<list>, <k>)"); // OPTION
  m_wordList[tmplte ].Add("noncentral_moment(<matrix>, <k>)"); // OPTION
  m_wordList[command].Add("central_moment"); // FUNCTION
  m_wordList[tmplte ].Add("central_moment(<list>, <k>)"); // OPTION
  m_wordList[tmplte ].Add("central_moment(<matrix>, <k>)"); // OPTION
  m_wordList[command].Add("cv"); // FUNCTION
  m_wordList[tmplte ].Add("cv(<list>)"); // OPTION
  m_wordList[tmplte ].Add("cv(<matrix>)"); // OPTION
  m_wordList[command].Add("smin"); // FUNCTION
  m_wordList[tmplte ].Add("smin(<list>)"); // OPTION
  m_wordList[tmplte ].Add("smin(<matrix>)"); // OPTION
  m_wordList[command].Add("smax"); // FUNCTION
  m_wordList[tmplte ].Add("smax(<list>)"); // OPTION
  m_wordList[tmplte ].Add("smax(<matrix>)"); // OPTION
  m_wordList[command].Add("range"); // FUNCTION
  m_wordList[tmplte ].Add("range(<list>)"); // OPTION
  m_wordList[tmplte ].Add("range(<matrix>)"); // OPTION
  m_wordList[command].Add("quantile"); // FUNCTION
  m_wordList[tmplte ].Add("quantile(<list>, <p>)"); // OPTION
  m_wordList[tmplte ].Add("quantile(<matrix>, <p>)"); // OPTION
  m_wordList[command].Add("median"); // FUNCTION
  m_wordList[tmplte ].Add("median(<list>)"); // OPTION
  m_wordList[tmplte ].Add("median(<matrix>)"); // OPTION
  m_wordList[command].Add("qrange"); // FUNCTION
  m_wordList[tmplte ].Add("qrange(<list>)"); // OPTION
  m_wordList[tmplte ].Add("qrange(<matrix>)"); // OPTION
  m_wordList[command].Add("mean_deviation"); // FUNCTION
  m_wordList[tmplte ].Add("mean_deviation(<list>)"); // OPTION
  m_wordList[tmplte ].Add("mean_deviation(<matrix>)"); // OPTION
  m_wordList[command].Add("median_deviation"); // FUNCTION
  m_wordList[tmplte ].Add("median_deviation(<list>)"); // OPTION
  m_wordList[tmplte ].Add("median_deviation(<matrix>)"); // OPTION
  m_wordList[command].Add("harmonic_mean"); // FUNCTION
  m_wordList[tmplte ].Add("harmonic_mean(<list>)"); // OPTION
  m_wordList[tmplte ].Add("harmonic_mean(<matrix>)"); // OPTION
  m_wordList[command].Add("geometric_mean"); // FUNCTION
  m_wordList[tmplte ].Add("geometric_mean(<list>)"); // OPTION
  m_wordList[tmplte ].Add("geometric_mean(<matrix>)"); // OPTION
  m_wordList[command].Add("kurtosis"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis(<list>)"); // OPTION
  m_wordList[tmplte ].Add("kurtosis(<matrix>)"); // OPTION
  m_wordList[command].Add("skewness"); // FUNCTION
  m_wordList[tmplte ].Add("skewness(<list>)"); // OPTION
  m_wordList[tmplte ].Add("skewness(<matrix>)"); // OPTION
  m_wordList[command].Add("pearson_skewness"); // FUNCTION
  m_wordList[tmplte ].Add("pearson_skewness(<list>)"); // OPTION
  m_wordList[tmplte ].Add("pearson_skewness(<matrix>)"); // OPTION
  m_wordList[command].Add("quartile_skewness"); // FUNCTION
  m_wordList[tmplte ].Add("quartile_skewness(<list>)"); // OPTION
  m_wordList[tmplte ].Add("quartile_skewness(<matrix>)"); // OPTION
  m_wordList[command].Add("cov"); // FUNCTION
  m_wordList[tmplte ].Add("cov(<matrix>)"); // OPTION
  m_wordList[command].Add("cov1"); // FUNCTION
  m_wordList[tmplte ].Add("cov1(<matrix>)"); // OPTION
  m_wordList[command].Add("global_variances"); // FUNCTION
  m_wordList[tmplte ].Add("global_variances(<matrix>)"); // OPTION
  m_wordList[tmplte ].Add("global_variances(<matrix>, <logical_value>)"); // OPTION
  m_wordList[command].Add("cor"); // FUNCTION
  m_wordList[tmplte ].Add("cor(<matrix>)"); // OPTION
  m_wordList[tmplte ].Add("cor(<matrix>, <logical_value>)"); // OPTION
  m_wordList[command].Add("list_correlations"); // FUNCTION
  m_wordList[tmplte ].Add("list_correlations(<matrix>)"); // OPTION
  m_wordList[tmplte ].Add("list_correlations(<matrix>, <logical_value>)"); // OPTION
  m_wordList[command].Add("histogram"); // FUNCTION
  m_wordList[tmplte ].Add("histogram(<list>)"); // OPTION
  m_wordList[tmplte ].Add("histogram(<list>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("histogram(<one_column_matrix>)"); // OPTION
  m_wordList[tmplte ].Add("histogram(<one_column_matrix>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("histogram(<one_row_matrix>)"); // OPTION
  m_wordList[tmplte ].Add("histogram(<one_row_matrix>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[command].Add("scatterplot"); // FUNCTION
  m_wordList[tmplte ].Add("scatterplot(<list>)"); // OPTION
  m_wordList[tmplte ].Add("scatterplot(<list>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("scatterplot(<matrix>)"); // OPTION
  m_wordList[tmplte ].Add("scatterplot(<matrix>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[command].Add("barsplot"); // FUNCTION
  m_wordList[tmplte ].Add("barsplot(<data1>, <data2>, ..., <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[command].Add("piechart"); // FUNCTION
  m_wordList[tmplte ].Add("piechart(<list>)"); // OPTION
  m_wordList[tmplte ].Add("piechart(<list>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("piechart(<one_column_matrix>)"); // OPTION
  m_wordList[tmplte ].Add("piechart(<one_column_matrix>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("piechart(<one_row_matrix>)"); // OPTION
  m_wordList[tmplte ].Add("piechart(<one_row_matrix>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[command].Add("boxplot"); // FUNCTION
  m_wordList[tmplte ].Add("boxplot(<data>)"); // OPTION
  m_wordList[tmplte ].Add("boxplot(<data>, <option_1>, <option_2>, ...)"); // OPTION
  m_wordList[command].Add("diag"); // FUNCTION
  m_wordList[tmplte ].Add("diag(<lm>)"); // OPTION
  m_wordList[command].Add("JF"); // FUNCTION
  m_wordList[tmplte ].Add("JF(<lambda>,<n>)"); // OPTION
  m_wordList[command].Add("jordan"); // FUNCTION
  m_wordList[tmplte ].Add("jordan(<mat>)"); // OPTION
  m_wordList[command].Add("dispJordan"); // FUNCTION
  m_wordList[tmplte ].Add("dispJordan(<l>)"); // OPTION
  m_wordList[command].Add("minimalPoly"); // FUNCTION
  m_wordList[tmplte ].Add("minimalPoly(<l>)"); // OPTION
  m_wordList[command].Add("ModeMatrix"); // FUNCTION
  m_wordList[tmplte ].Add("ModeMatrix(<A>,<l>)"); // OPTION
  m_wordList[command].Add("mat_function"); // FUNCTION
  m_wordList[tmplte ].Add("mat_function(<f>,<mat>)"); // OPTION
  m_wordList[command].Add("bc2"); // FUNCTION
  m_wordList[tmplte ].Add("bc2(<solution>, <xval1>, <yval1>, <xval2>, <yval2>)"); // OPTION
  m_wordList[command].Add("desolve"); // FUNCTION
  m_wordList[tmplte ].Add("desolve(<eqn>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("desolve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("ic1"); // FUNCTION
  m_wordList[tmplte ].Add("ic1(<solution>, <xval>, <yval>)"); // OPTION
  m_wordList[command].Add("ic2"); // FUNCTION
  m_wordList[tmplte ].Add("ic2(<solution>, <xval>, <yval>, <dval>)"); // OPTION
  m_wordList[command].Add("ode2"); // FUNCTION
  m_wordList[tmplte ].Add("ode2(<eqn>, <dvar>, <ivar>)"); // OPTION
  m_wordList[command].Add("antid"); // FUNCTION
  m_wordList[tmplte ].Add("antid(<expr>, <x>, <u(x)>) "); // OPTION
  m_wordList[command].Add("antidiff"); // FUNCTION
  m_wordList[tmplte ].Add("antidiff(<expr>, <x>, <u>(<x>))"); // OPTION
  m_wordList[command].Add("atomgrad"); // OPTION
  m_wordList[command].Add("atvalue"); // FUNCTION
  m_wordList[tmplte ].Add("atvalue(<expr>, [<x_1> = <a_1>, ..., <x_m> = <a_m>], <c>)"); // OPTION
  m_wordList[tmplte ].Add("atvalue(<expr>, <x_1> = <a_1>, <c>)"); // OPTION
  m_wordList[command].Add("cartan"); // FUNCTION
  m_wordList[tmplte ].Add("cartan-"); // OPTION
  m_wordList[command].Add("del"); // FUNCTION
  m_wordList[tmplte ].Add("del(<x>)"); // OPTION
  m_wordList[command].Add("delta"); // FUNCTION
  m_wordList[tmplte ].Add("delta(<t>)"); // OPTION
  m_wordList[command].Add("dependencies"); // OPTION
  m_wordList[command].Add("depends"); // FUNCTION
  m_wordList[tmplte ].Add("depends(<f_1>, <x_1>, ..., <f_n>, <x_n>)"); // OPTION
  m_wordList[command].Add("derivabbrev"); // OPTION
  m_wordList[command].Add("derivdegree"); // FUNCTION
  m_wordList[tmplte ].Add("derivdegree(<expr>, <y>, <x>)"); // OPTION
  m_wordList[command].Add("derivlist"); // FUNCTION
  m_wordList[tmplte ].Add("derivlist(<var_1>, ..., <var_k>)"); // OPTION
  m_wordList[command].Add("derivsubst"); // OPTION
  m_wordList[command].Add("diff"); // FUNCTION
  m_wordList[tmplte ].Add("diff(<expr>, <x_1>, <n_1>, ..., <x_m>, <n_m>)"); // OPTION
  m_wordList[tmplte ].Add("diff(<expr>, <x>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("diff(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("diff(<expr>)"); // OPTION
  m_wordList[command].Add("diff"); // OPTION
  m_wordList[command].Add("dscalar"); // FUNCTION
  m_wordList[tmplte ].Add("dscalar(<f>)"); // OPTION
  m_wordList[command].Add("express"); // FUNCTION
  m_wordList[tmplte ].Add("express(<expr>)"); // OPTION
  m_wordList[command].Add("gradef"); // FUNCTION
  m_wordList[tmplte ].Add("gradef(<f>(<x_1>, ..., <x_n>), <g_1>, ..., <g_m>)"); // OPTION
  m_wordList[tmplte ].Add("gradef(<a>, <x>, <expr>)"); // OPTION
  m_wordList[command].Add("gradefs"); // OPTION
  m_wordList[command].Add("laplace"); // FUNCTION
  m_wordList[tmplte ].Add("laplace(<expr>, <t>, <s>)"); // OPTION
  m_wordList[command].Add("pdf_normal"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_normal(<x>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("cdf_normal"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_normal(<x>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("quantile_normal"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_normal(<q>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("mean_normal"); // FUNCTION
  m_wordList[tmplte ].Add("mean_normal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("var_normal"); // FUNCTION
  m_wordList[tmplte ].Add("var_normal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("std_normal"); // FUNCTION
  m_wordList[tmplte ].Add("std_normal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("skewness_normal"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_normal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("kurtosis_normal"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_normal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("random_normal"); // FUNCTION
  m_wordList[tmplte ].Add("random_normal(<m>,<s>)"); // OPTION
  m_wordList[tmplte ].Add("random_normal(<m>,<s>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_student_t(<x>,<n>)"); // OPTION
  m_wordList[command].Add("cdf_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_student_t(<x>,<n>)"); // OPTION
  m_wordList[command].Add("quantile_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_student_t(<q>,<n>)"); // OPTION
  m_wordList[command].Add("mean_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("mean_student_t(<n>)"); // OPTION
  m_wordList[command].Add("var_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("var_student_t(<n>)"); // OPTION
  m_wordList[command].Add("std_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("std_student_t(<n>)"); // OPTION
  m_wordList[command].Add("skewness_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_student_t(<n>)"); // OPTION
  m_wordList[command].Add("kurtosis_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_student_t(<n>)"); // OPTION
  m_wordList[command].Add("random_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("random_student_t(<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_student_t(<n>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_noncentral_student_t(<x>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("cdf_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_noncentral_student_t(<x>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("quantile_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_noncentral_student_t(<q>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("mean_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("mean_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("var_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("var_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("std_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("std_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("skewness_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("kurtosis_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("random_noncentral_student_t"); // FUNCTION
  m_wordList[tmplte ].Add("random_noncentral_student_t(<n>,<ncp>)"); // OPTION
  m_wordList[tmplte ].Add("random_noncentral_student_t(<n>,<ncp>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_chi2(<x>,<n>)"); // OPTION
  m_wordList[command].Add("cdf_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_chi2(<x>,<n>)"); // OPTION
  m_wordList[command].Add("quantile_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_chi2(<q>,<n>)"); // OPTION
  m_wordList[command].Add("mean_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("mean_chi2(<n>)"); // OPTION
  m_wordList[command].Add("var_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("var_chi2(<n>)"); // OPTION
  m_wordList[command].Add("std_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("std_chi2(<n>)"); // OPTION
  m_wordList[command].Add("skewness_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_chi2(<n>)"); // OPTION
  m_wordList[command].Add("kurtosis_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_chi2(<n>)"); // OPTION
  m_wordList[command].Add("random_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("random_chi2(<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_chi2(<n>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_noncentral_chi2(<x>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("cdf_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_noncentral_chi2(<x>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("quantile_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_noncentral_chi2(<q>,<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("mean_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("mean_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("var_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("var_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("std_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("std_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("skewness_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("kurtosis_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[command].Add("random_noncentral_chi2"); // FUNCTION
  m_wordList[tmplte ].Add("random_noncentral_chi2(<n>,<ncp>)"); // OPTION
  m_wordList[tmplte ].Add("random_noncentral_chi2(<n>,<ncp>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_f"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_f(<x>,<m>,<n>)"); // OPTION
  m_wordList[command].Add("cdf_f"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_f(<x>,<m>,<n>)"); // OPTION
  m_wordList[command].Add("quantile_f"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_f(<q>,<m>,<n>)"); // OPTION
  m_wordList[command].Add("mean_f"); // FUNCTION
  m_wordList[tmplte ].Add("mean_f(<m>,<n>)"); // OPTION
  m_wordList[command].Add("var_f"); // FUNCTION
  m_wordList[tmplte ].Add("var_f(<m>,<n>)"); // OPTION
  m_wordList[command].Add("std_f"); // FUNCTION
  m_wordList[tmplte ].Add("std_f(<m>,<n>)"); // OPTION
  m_wordList[command].Add("skewness_f"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_f(<m>,<n>)"); // OPTION
  m_wordList[command].Add("kurtosis_f"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_f(<m>,<n>)"); // OPTION
  m_wordList[command].Add("random_f"); // FUNCTION
  m_wordList[tmplte ].Add("random_f(<m>,<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_f(<m>,<n>,<k>)"); // OPTION
  m_wordList[command].Add("pdf_exp"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_exp(<x>,<m>)"); // OPTION
  m_wordList[command].Add("cdf_exp"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_exp(<x>,<m>)"); // OPTION
  m_wordList[command].Add("quantile_exp"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_exp(<q>,<m>)"); // OPTION
  m_wordList[command].Add("mean_exp"); // FUNCTION
  m_wordList[tmplte ].Add("mean_exp(<m>)"); // OPTION
  m_wordList[command].Add("var_exp"); // FUNCTION
  m_wordList[tmplte ].Add("var_exp(<m>)"); // OPTION
  m_wordList[command].Add("std_exp"); // FUNCTION
  m_wordList[tmplte ].Add("std_exp(<m>)"); // OPTION
  m_wordList[command].Add("skewness_exp"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_exp(<m>)"); // OPTION
  m_wordList[command].Add("kurtosis_exp"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_exp(<m>)"); // OPTION
  m_wordList[command].Add("random_exp"); // FUNCTION
  m_wordList[tmplte ].Add("random_exp(<m>)"); // OPTION
  m_wordList[tmplte ].Add("random_exp(<m>,<k>)"); // OPTION
  m_wordList[command].Add("pdf_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_lognormal(<x>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("cdf_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_lognormal(<x>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("quantile_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_lognormal(<q>,<m>,<s>)"); // OPTION
  m_wordList[command].Add("mean_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("mean_lognormal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("var_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("var_lognormal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("std_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("std_lognormal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("skewness_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_lognormal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("kurtosis_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_lognormal(<m>,<s>)"); // OPTION
  m_wordList[command].Add("random_lognormal"); // FUNCTION
  m_wordList[tmplte ].Add("random_lognormal(<m>,<s>)"); // OPTION
  m_wordList[tmplte ].Add("random_lognormal(<m>,<s>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_gamma(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_gamma(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_gamma(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("mean_gamma(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("var_gamma(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("std_gamma(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_gamma(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_gamma(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("random_gamma(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_gamma(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_beta"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_beta(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_beta"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_beta(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_beta"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_beta(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_beta"); // FUNCTION
  m_wordList[tmplte ].Add("mean_beta(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_beta"); // FUNCTION
  m_wordList[tmplte ].Add("var_beta(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_beta"); // FUNCTION
  m_wordList[tmplte ].Add("std_beta(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_beta"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_beta(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_beta"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_beta(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_beta"); // FUNCTION
  m_wordList[tmplte ].Add("random_beta(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_beta(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_continuous_uniform(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_continuous_uniform(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_continuous_uniform(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("mean_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("var_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("std_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_continuous_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("random_continuous_uniform(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_continuous_uniform(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_logistic(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_logistic(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_logistic(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("mean_logistic(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("var_logistic(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("std_logistic(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_logistic(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_logistic(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_logistic"); // FUNCTION
  m_wordList[tmplte ].Add("random_logistic(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_logistic(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_pareto(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_pareto(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_pareto(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("mean_pareto(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("var_pareto(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("std_pareto(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_pareto(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_pareto(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_pareto"); // FUNCTION
  m_wordList[tmplte ].Add("random_pareto(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_pareto(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_weibull(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_weibull(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_weibull(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("mean_weibull(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("var_weibull(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("std_weibull(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_weibull(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_weibull(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_weibull"); // FUNCTION
  m_wordList[tmplte ].Add("random_weibull(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_weibull(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_rayleigh(<x>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_rayleigh(<x>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_rayleigh(<q>,<b>)"); // OPTION
  m_wordList[command].Add("mean_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("mean_rayleigh(<b>)"); // OPTION
  m_wordList[command].Add("var_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("var_rayleigh(<b>)"); // OPTION
  m_wordList[command].Add("std_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("std_rayleigh(<b>)"); // OPTION
  m_wordList[command].Add("skewness_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_rayleigh(<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_rayleigh(<b>)"); // OPTION
  m_wordList[command].Add("random_rayleigh"); // FUNCTION
  m_wordList[tmplte ].Add("random_rayleigh(<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_rayleigh(<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_laplace(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_laplace(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_laplace(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("mean_laplace(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("var_laplace(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("std_laplace(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_laplace(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_laplace(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_laplace"); // FUNCTION
  m_wordList[tmplte ].Add("random_laplace(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_laplace(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_cauchy"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_cauchy(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_cauchy"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_cauchy(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_cauchy"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_cauchy(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_cauchy"); // FUNCTION
  m_wordList[tmplte ].Add("random_cauchy(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_cauchy(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_gumbel(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("cdf_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_gumbel(<x>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("quantile_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_gumbel(<q>,<a>,<b>)"); // OPTION
  m_wordList[command].Add("mean_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("mean_gumbel(<a>,<b>)"); // OPTION
  m_wordList[command].Add("var_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("var_gumbel(<a>,<b>)"); // OPTION
  m_wordList[command].Add("std_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("std_gumbel(<a>,<b>)"); // OPTION
  m_wordList[command].Add("skewness_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_gumbel(<a>,<b>)"); // OPTION
  m_wordList[command].Add("kurtosis_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_gumbel(<a>,<b>)"); // OPTION
  m_wordList[command].Add("random_gumbel"); // FUNCTION
  m_wordList[tmplte ].Add("random_gumbel(<a>,<b>)"); // OPTION
  m_wordList[tmplte ].Add("random_gumbel(<a>,<b>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_binomial(<x>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("cdf_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_binomial(<x>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("quantile_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_binomial(<q>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("mean_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("mean_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("var_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("var_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("std_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("std_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("skewness_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("kurtosis_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("random_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("random_binomial(<n>,<p>)"); // OPTION
  m_wordList[tmplte ].Add("random_binomial(<n>,<p>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_poisson(<x>,<m>)"); // OPTION
  m_wordList[command].Add("cdf_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_poisson(<x>,<m>)"); // OPTION
  m_wordList[command].Add("quantile_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_poisson(<q>,<m>)"); // OPTION
  m_wordList[command].Add("mean_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("mean_poisson(<m>)"); // OPTION
  m_wordList[command].Add("var_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("var_poisson(<m>)"); // OPTION
  m_wordList[command].Add("std_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("std_poisson(<m>)"); // OPTION
  m_wordList[command].Add("skewness_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_poisson(<m>)"); // OPTION
  m_wordList[command].Add("kurtosis_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_poisson(<m>)"); // OPTION
  m_wordList[command].Add("random_poisson"); // FUNCTION
  m_wordList[tmplte ].Add("random_poisson(<m>)"); // OPTION
  m_wordList[tmplte ].Add("random_poisson(<m>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_bernoulli(<x>,<p>)"); // OPTION
  m_wordList[command].Add("cdf_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_bernoulli(<x>,<p>)"); // OPTION
  m_wordList[command].Add("quantile_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_bernoulli(<q>,<p>)"); // OPTION
  m_wordList[command].Add("mean_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("mean_bernoulli(<p>)"); // OPTION
  m_wordList[command].Add("var_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("var_bernoulli(<p>)"); // OPTION
  m_wordList[command].Add("std_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("std_bernoulli(<p>)"); // OPTION
  m_wordList[command].Add("skewness_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_bernoulli(<p>)"); // OPTION
  m_wordList[command].Add("kurtosis_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_bernoulli(<p>)"); // OPTION
  m_wordList[command].Add("random_bernoulli"); // FUNCTION
  m_wordList[tmplte ].Add("random_bernoulli(<p>)"); // OPTION
  m_wordList[tmplte ].Add("random_bernoulli(<p>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_geometric(<x>,<p>)"); // OPTION
  m_wordList[command].Add("cdf_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_geometric(<x>,<p>)"); // OPTION
  m_wordList[command].Add("quantile_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_geometric(<q>,<p>)"); // OPTION
  m_wordList[command].Add("mean_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("mean_geometric(<p>)"); // OPTION
  m_wordList[command].Add("var_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("var_geometric(<p>)"); // OPTION
  m_wordList[command].Add("std_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("std_geometric(<p>)"); // OPTION
  m_wordList[command].Add("skewness_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_geometric(<p>)"); // OPTION
  m_wordList[command].Add("kurtosis_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_geometric(<p>)"); // OPTION
  m_wordList[command].Add("random_geometric"); // FUNCTION
  m_wordList[tmplte ].Add("random_geometric(<p>)"); // OPTION
  m_wordList[tmplte ].Add("random_geometric(<p>,<n>)"); // OPTION
  m_wordList[command].Add("pdf_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_discrete_uniform(<x>,<n>)"); // OPTION
  m_wordList[command].Add("cdf_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_discrete_uniform(<x>,<n>)"); // OPTION
  m_wordList[command].Add("quantile_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_discrete_uniform(<q>,<n>)"); // OPTION
  m_wordList[command].Add("mean_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("mean_discrete_uniform(<n>)"); // OPTION
  m_wordList[command].Add("var_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("var_discrete_uniform(<n>)"); // OPTION
  m_wordList[command].Add("std_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("std_discrete_uniform(<n>)"); // OPTION
  m_wordList[command].Add("skewness_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_discrete_uniform(<n>)"); // OPTION
  m_wordList[command].Add("kurtosis_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_discrete_uniform(<n>)"); // OPTION
  m_wordList[command].Add("random_discrete_uniform"); // FUNCTION
  m_wordList[tmplte ].Add("random_discrete_uniform(<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_discrete_uniform(<n>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_hypergeometric(<x>,<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("cdf_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_hypergeometric(<x>,<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("quantile_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_hypergeometric(<q>,<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("mean_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("mean_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("var_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("var_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("std_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("std_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("skewness_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("kurtosis_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[command].Add("random_hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("random_hypergeometric(<n1>,<n2>,<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_hypergeometric(<n1>,<n2>,<n>,<m>)"); // OPTION
  m_wordList[command].Add("pdf_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_negative_binomial(<x>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("cdf_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_negative_binomial(<x>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("quantile_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("quantile_negative_binomial(<q>,<n>,<p>)"); // OPTION
  m_wordList[command].Add("mean_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("mean_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("var_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("var_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("std_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("std_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("skewness_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("skewness_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("kurtosis_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("kurtosis_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[command].Add("random_negative_binomial"); // FUNCTION
  m_wordList[tmplte ].Add("random_negative_binomial(<n>,<p>)"); // OPTION
  m_wordList[tmplte ].Add("random_negative_binomial(<n>,<p>,<m>)"); // OPTION
  m_wordList[command].Add("draw"); // FUNCTION
  m_wordList[tmplte ].Add("draw(<gr2d>, ..., <gr3d>, ..., <options>, ...)"); // OPTION
  m_wordList[command].Add("draw2d"); // FUNCTION
  m_wordList[tmplte ].Add("draw2d(<option>, <graphic_object>, ...)"); // OPTION
  m_wordList[command].Add("draw3d"); // FUNCTION
  m_wordList[tmplte ].Add("draw3d(<option>, <graphic_object>, ...)"); // OPTION
  m_wordList[command].Add("draw_file"); // FUNCTION
  m_wordList[tmplte ].Add("draw_file(<graphic option>, ..., <graphic object>, ...)"); // OPTION
  m_wordList[command].Add("multiplot_mode"); // FUNCTION
  m_wordList[tmplte ].Add("multiplot_mode(<term>)"); // OPTION
  m_wordList[command].Add("set_draw_defaults"); // FUNCTION
  m_wordList[tmplte ].Add("set_draw_defaults(<graphic option>, ..., <graphic object>, ...)"); // OPTION
  m_wordList[command].Add("adapt_depth"); // OPTION
  m_wordList[command].Add("axis_3d"); // OPTION
  m_wordList[command].Add("axis_bottom"); // OPTION
  m_wordList[command].Add("axis_left"); // OPTION
  m_wordList[command].Add("axis_right"); // OPTION
  m_wordList[command].Add("axis_top"); // OPTION
  m_wordList[command].Add("border"); // OPTION
  m_wordList[command].Add("cbrange"); // OPTION
  m_wordList[command].Add("cbtics"); // OPTION
  m_wordList[command].Add("color"); // OPTION
  m_wordList[command].Add("colorbox"); // OPTION
  m_wordList[command].Add("columns"); // OPTION
  m_wordList[command].Add("contour"); // OPTION
  m_wordList[command].Add("contour_levels"); // OPTION
  m_wordList[command].Add("data_file_name"); // OPTION
  m_wordList[command].Add("delay"); // OPTION
  m_wordList[command].Add("enhanced3d"); // OPTION
  m_wordList[command].Add("eps_height"); // OPTION
  m_wordList[command].Add("eps_width"); // OPTION
  m_wordList[command].Add("file_bgcolor"); // OPTION
  m_wordList[command].Add("file_name"); // OPTION
  m_wordList[command].Add("fill_color"); // OPTION
  m_wordList[command].Add("fill_density"); // OPTION
  m_wordList[command].Add("filled_func"); // OPTION
  m_wordList[command].Add("font"); // OPTION
  m_wordList[command].Add("font_size"); // OPTION
  m_wordList[command].Add("gnuplot_command"); // OPTION
  m_wordList[command].Add("gnuplot_file_name"); // OPTION
  m_wordList[command].Add("grid"); // OPTION
  m_wordList[command].Add("head_angle"); // OPTION
  m_wordList[command].Add("head_both"); // OPTION
  m_wordList[command].Add("head_length"); // OPTION
  m_wordList[command].Add("head_type"); // OPTION
  m_wordList[command].Add("ip_grid"); // OPTION
  m_wordList[command].Add("ip_grid_in"); // OPTION
  m_wordList[command].Add("key"); // OPTION
  m_wordList[command].Add("key_pos"); // OPTION
  m_wordList[command].Add("label_alignment"); // OPTION
  m_wordList[command].Add("label_orientation"); // OPTION
  m_wordList[command].Add("line_type"); // OPTION
  m_wordList[command].Add("line_width"); // OPTION
  m_wordList[command].Add("logcb"); // OPTION
  m_wordList[command].Add("logx"); // OPTION
  m_wordList[command].Add("logx_secondary"); // OPTION
  m_wordList[command].Add("logy"); // OPTION
  m_wordList[command].Add("logy_secondary"); // OPTION
  m_wordList[command].Add("logz"); // OPTION
  m_wordList[command].Add("nticks"); // OPTION
  m_wordList[command].Add("palette"); // OPTION
  m_wordList[command].Add("pdf_height"); // OPTION
  m_wordList[command].Add("pdf_width"); // OPTION
  m_wordList[command].Add("pic_height"); // OPTION
  m_wordList[command].Add("pic_width"); // OPTION
  m_wordList[command].Add("point_size"); // OPTION
  m_wordList[command].Add("point_type"); // OPTION
  m_wordList[command].Add("points_joined"); // OPTION
  m_wordList[command].Add("proportional_axes"); // OPTION
  m_wordList[command].Add("rot_horizontal"); // OPTION
  m_wordList[command].Add("rot_vertical"); // OPTION
  m_wordList[command].Add("surface_hide"); // OPTION
  m_wordList[command].Add("terminal"); // OPTION
  m_wordList[command].Add("title"); // OPTION
  m_wordList[command].Add("transform"); // OPTION
  m_wordList[command].Add("transparent"); // OPTION
  m_wordList[command].Add("tube_extremes"); // OPTION
  m_wordList[command].Add("unit_vectors"); // OPTION
  m_wordList[command].Add("user_preamble"); // OPTION
  m_wordList[command].Add("wired_surface"); // OPTION
  m_wordList[command].Add("x_voxel"); // OPTION
  m_wordList[command].Add("xaxis"); // OPTION
  m_wordList[command].Add("xaxis_color"); // OPTION
  m_wordList[command].Add("xaxis_secondary"); // OPTION
  m_wordList[command].Add("xaxis_type"); // OPTION
  m_wordList[command].Add("xaxis_width"); // OPTION
  m_wordList[command].Add("xlabel"); // OPTION
  m_wordList[command].Add("xlabel_secondary"); // OPTION
  m_wordList[command].Add("xrange"); // OPTION
  m_wordList[command].Add("xrange_secondary"); // OPTION
  m_wordList[command].Add("xtics"); // OPTION
  m_wordList[command].Add("xtics_axis"); // OPTION
  m_wordList[command].Add("xtics_rotate"); // OPTION
  m_wordList[command].Add("xtics_rotate_secondary"); // OPTION
  m_wordList[command].Add("xtics_secondary"); // OPTION
  m_wordList[command].Add("xtics_secondary_axis"); // OPTION
  m_wordList[command].Add("xu_grid"); // OPTION
  m_wordList[command].Add("xy_file"); // OPTION
  m_wordList[command].Add("xyplane"); // OPTION
  m_wordList[command].Add("y_voxel"); // OPTION
  m_wordList[command].Add("yaxis"); // OPTION
  m_wordList[command].Add("yaxis_color"); // OPTION
  m_wordList[command].Add("yaxis_secondary"); // OPTION
  m_wordList[command].Add("yaxis_type"); // OPTION
  m_wordList[command].Add("yaxis_width"); // OPTION
  m_wordList[command].Add("ylabel"); // OPTION
  m_wordList[command].Add("ylabel_secondary"); // OPTION
  m_wordList[command].Add("yrange"); // OPTION
  m_wordList[command].Add("yrange_secondary"); // OPTION
  m_wordList[command].Add("ytics"); // OPTION
  m_wordList[command].Add("ytics_axis"); // OPTION
  m_wordList[command].Add("ytics_rotate"); // OPTION
  m_wordList[command].Add("ytics_rotate_secondary"); // OPTION
  m_wordList[command].Add("ytics_secondary"); // OPTION
  m_wordList[command].Add("ytics_secondary_axis"); // OPTION
  m_wordList[command].Add("yv_grid"); // OPTION
  m_wordList[command].Add("z_voxel"); // OPTION
  m_wordList[command].Add("zaxis"); // OPTION
  m_wordList[command].Add("zaxis_color"); // OPTION
  m_wordList[command].Add("zaxis_type"); // OPTION
  m_wordList[command].Add("zaxis_width"); // OPTION
  m_wordList[command].Add("zlabel"); // OPTION
  m_wordList[command].Add("zrange"); // OPTION
  m_wordList[command].Add("ztics"); // OPTION
  m_wordList[command].Add("ztics_axis"); // OPTION
  m_wordList[command].Add("ztics_rotate"); // OPTION
  m_wordList[tmplte ].Add("make_level_picture(<data>,<width>,<height>)"); // OPTION
  m_wordList[command].Add("boundaries_array"); // OPTION
  m_wordList[command].Add("chaosgame"); // FUNCTION
  m_wordList[tmplte ].Add("chaosgame(<[[><x1>, <y1><]>...<[><xm>, <ym><]]>, <[><x0>, <y0><]>, <b>, <n>, ..., options, ...);"); // OPTION
  m_wordList[command].Add("evolution"); // FUNCTION
  m_wordList[tmplte ].Add("evolution(<F>, <y0>, <n>, ..., options, ...);"); // OPTION
  m_wordList[command].Add("evolution2d"); // FUNCTION
  m_wordList[tmplte ].Add("evolution2d(<[><F>, <G><]>, <[><u>, <v><]>, <[><u0>, <y0><]>, <n>, ..., options, ...);"); // OPTION
  m_wordList[command].Add("ifs"); // FUNCTION
  m_wordList[tmplte ].Add("ifs(<[><r1>, ..., <rm><]>, <[><A1>, ..., <Am><]>, <[[><x1>, <y1><]>, ..., <[><xm>, <ym><]]>, <[><x0>, <y0><]>, <n>, ..., options, ...);"); // OPTION
  m_wordList[command].Add("julia"); // FUNCTION
  m_wordList[tmplte ].Add("julia(<x>, <y>, ...<options>...)"); // OPTION
  m_wordList[command].Add("mandelbrot"); // FUNCTION
  m_wordList[tmplte ].Add("mandelbrot(<options>)"); // OPTION
  m_wordList[command].Add("orbits"); // FUNCTION
  m_wordList[tmplte ].Add("orbits(<F>, <y0>, <n1>, <n2>, [<x>, <x0>, <xf>, <xstep>], ...options...);"); // OPTION
  m_wordList[command].Add("rk"); // FUNCTION
  m_wordList[tmplte ].Add("rk(<ODE>, <var>, <initial>, <domain>)"); // OPTION
  m_wordList[tmplte ].Add("rk([<ODE1>,...,<ODEm>], [<v1>,...,<vm>], [<init1>,...,<initm>], <domain>)"); // OPTION
  m_wordList[command].Add("staircase"); // FUNCTION
  m_wordList[tmplte ].Add("staircase(<F>, <y0>, <n>, ...options...);"); // OPTION
  m_wordList[command].Add("jacobi_sn"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_sn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_cn"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_cn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_dn"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_dn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_ns"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_ns(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_sc"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_sc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_sd"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_sd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_nc"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_nc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_cs"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_cs(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_cd"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_cd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_nd"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_nd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_ds"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_ds(<u>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi_dc"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_dc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_sn"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_sn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_cn"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_cn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_dn"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_dn(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_ns"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_ns(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_sc"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_sc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_sd"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_sd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_nc"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_nc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_cs"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_cs(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_cd"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_cd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_nd"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_nd(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_ds"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_ds(<u>, <m>)"); // OPTION
  m_wordList[command].Add("inverse_jacobi_dc"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_jacobi_dc(<u>, <m>)"); // OPTION
  m_wordList[command].Add("elliptic_f"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_f(<phi>, <m>)"); // OPTION
  m_wordList[command].Add("elliptic_e"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_e(<phi>, <m>)"); // OPTION
  m_wordList[command].Add("elliptic_eu"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_eu(<u>, <m>)"); // OPTION
  m_wordList[command].Add("elliptic_pi"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_pi(<n>, <phi>, <m>)"); // OPTION
  m_wordList[command].Add("elliptic_kc"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_kc(<m>)"); // OPTION
  m_wordList[command].Add("elliptic_ec"); // FUNCTION
  m_wordList[tmplte ].Add("elliptic_ec(<m>)"); // OPTION
  m_wordList[command].Add("%rnum_list"); // OPTION
  m_wordList[command].Add("algexact"); // OPTION
  m_wordList[command].Add("algsys"); // FUNCTION
  m_wordList[tmplte ].Add("algsys([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[tmplte ].Add("algsys([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("allroots"); // FUNCTION
  m_wordList[tmplte ].Add("allroots(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("allroots(<eqn>)"); // OPTION
  m_wordList[command].Add("bfallroots"); // FUNCTION
  m_wordList[tmplte ].Add("bfallroots(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("bfallroots(<eqn>)"); // OPTION
  m_wordList[command].Add("backsubst"); // OPTION
  m_wordList[command].Add("breakup"); // OPTION
  m_wordList[command].Add("dimension"); // FUNCTION
  m_wordList[tmplte ].Add("dimension(<eqn>)"); // OPTION
  m_wordList[tmplte ].Add("dimension(<eqn_1>, ..., <eqn_n>)"); // OPTION
  m_wordList[command].Add("dispflag"); // OPTION
  m_wordList[command].Add("funcsolve"); // FUNCTION
  m_wordList[tmplte ].Add("funcsolve(<eqn>, <g>(<t>))"); // OPTION
  m_wordList[command].Add("globalsolve"); // OPTION
  m_wordList[command].Add("ieqn"); // FUNCTION
  m_wordList[tmplte ].Add("ieqn(<ie>, <unk>, <tech>, <n>, <guess>)"); // OPTION
  m_wordList[command].Add("ieqnprint"); // OPTION
  m_wordList[command].Add("lhs"); // FUNCTION
  m_wordList[tmplte ].Add("lhs(<expr>)"); // OPTION
  m_wordList[command].Add("linsolve"); // FUNCTION
  m_wordList[tmplte ].Add("linsolve([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("linsolvewarn"); // OPTION
  m_wordList[command].Add("linsolve_params"); // OPTION
  m_wordList[command].Add("multiplicities"); // OPTION
  m_wordList[command].Add("nroots"); // FUNCTION
  m_wordList[tmplte ].Add("nroots(<p>, <low>, <high>)"); // OPTION
  m_wordList[command].Add("nthroot"); // FUNCTION
  m_wordList[tmplte ].Add("nthroot(<p>, <n>)"); // OPTION
  m_wordList[command].Add("polyfactor"); // OPTION
  m_wordList[command].Add("programmode"); // OPTION
  m_wordList[command].Add("realonly"); // OPTION
  m_wordList[command].Add("realroots"); // FUNCTION
  m_wordList[tmplte ].Add("realroots(<expr>, <bound>)"); // OPTION
  m_wordList[tmplte ].Add("realroots(<eqn>, <bound>)"); // OPTION
  m_wordList[tmplte ].Add("realroots(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("realroots(<eqn>)"); // OPTION
  m_wordList[command].Add("rhs"); // FUNCTION
  m_wordList[tmplte ].Add("rhs(<expr>)"); // OPTION
  m_wordList[command].Add("rootsconmode"); // OPTION
  m_wordList[command].Add("rootscontract"); // FUNCTION
  m_wordList[tmplte ].Add("rootscontract(<expr>)"); // OPTION
  m_wordList[command].Add("rootsepsilon"); // OPTION
  m_wordList[command].Add("solve"); // FUNCTION
  m_wordList[tmplte ].Add("solve(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("solve(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("solve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("solvedecomposes"); // OPTION
  m_wordList[command].Add("solveexplicit"); // OPTION
  m_wordList[command].Add("solvefactors"); // OPTION
  m_wordList[command].Add("solvenullwarn"); // OPTION
  m_wordList[command].Add("solveradcan"); // OPTION
  m_wordList[command].Add("solvetrigwarn"); // OPTION
  m_wordList[command].Add("at"); // FUNCTION
  m_wordList[tmplte ].Add("at(<expr>, [<eqn_1>, ..., <eqn_n>])"); // OPTION
  m_wordList[tmplte ].Add("at(<expr>, <eqn>)"); // OPTION
  m_wordList[command].Add("box"); // FUNCTION
  m_wordList[tmplte ].Add("box(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("box(<expr>, <a>)"); // OPTION
  m_wordList[command].Add("boxchar"); // OPTION
  m_wordList[command].Add("carg"); // FUNCTION
  m_wordList[tmplte ].Add("carg(<z>)"); // OPTION
  m_wordList[command].Add("constantp"); // FUNCTION
  m_wordList[tmplte ].Add("constantp(<expr>)"); // OPTION
  m_wordList[command].Add("declare"); // FUNCTION
  m_wordList[tmplte ].Add("declare(<a_1>, <p_1>, <a_2>, <p_2>, ...)"); // OPTION
  m_wordList[command].Add("disolate"); // FUNCTION
  m_wordList[tmplte ].Add("disolate(<expr>, <x_1>, ..., <x_n>)"); // OPTION
  m_wordList[command].Add("dispform"); // FUNCTION
  m_wordList[tmplte ].Add("dispform(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("dispform(<expr>, all)"); // OPTION
  m_wordList[command].Add("distrib"); // FUNCTION
  m_wordList[tmplte ].Add("distrib(<expr>)"); // OPTION
  m_wordList[command].Add("dpart"); // FUNCTION
  m_wordList[tmplte ].Add("dpart(<expr>, <n_1>, ..., <n_k>)"); // OPTION
  m_wordList[command].Add("exp"); // FUNCTION
  m_wordList[tmplte ].Add("exp(<x>)"); // OPTION
  m_wordList[command].Add("%emode"); // OPTION
  m_wordList[command].Add("%enumer"); // OPTION
  m_wordList[command].Add("exptisolate"); // OPTION
  m_wordList[command].Add("exptsubst"); // OPTION
  m_wordList[command].Add("freeof"); // FUNCTION
  m_wordList[tmplte ].Add("freeof(<x_1>, ..., <x_n>, <expr>)"); // OPTION
  m_wordList[command].Add("genfact"); // FUNCTION
  m_wordList[tmplte ].Add("genfact(<x>, <y>, <z>)"); // OPTION
  m_wordList[command].Add("imagpart"); // FUNCTION
  m_wordList[tmplte ].Add("imagpart(<expr>)"); // OPTION
  m_wordList[command].Add("infix"); // FUNCTION
  m_wordList[tmplte ].Add("infix(<op>)"); // OPTION
  m_wordList[tmplte ].Add("infix(<op>, <lbp>, <rbp>)"); // OPTION
  m_wordList[tmplte ].Add("infix(<op>, <lbp>, <rbp>, <lpos>, <rpos>, <pos>)"); // OPTION
  m_wordList[command].Add("inflag"); // OPTION
  m_wordList[command].Add("inpart"); // FUNCTION
  m_wordList[tmplte ].Add("inpart(<expr>, <n_1>, ..., <n_k>)"); // OPTION
  m_wordList[command].Add("isolate"); // FUNCTION
  m_wordList[tmplte ].Add("isolate(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("isolate_wrt_times"); // OPTION
  m_wordList[command].Add("listconstvars"); // OPTION
  m_wordList[command].Add("listdummyvars"); // OPTION
  m_wordList[command].Add("listofvars"); // FUNCTION
  m_wordList[tmplte ].Add("listofvars(<expr>)"); // OPTION
  m_wordList[command].Add("lfreeof"); // FUNCTION
  m_wordList[tmplte ].Add("lfreeof(<list>, <expr>)"); // OPTION
  m_wordList[command].Add("lopow"); // FUNCTION
  m_wordList[tmplte ].Add("lopow(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("lpart"); // FUNCTION
  m_wordList[tmplte ].Add("lpart(<label>, <expr>, <n_1>, ..., <n_k>)"); // OPTION
  m_wordList[command].Add("multthru"); // FUNCTION
  m_wordList[tmplte ].Add("multthru(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("multthru(<expr_1>, <expr_2>)"); // OPTION
  m_wordList[command].Add("nounify"); // FUNCTION
  m_wordList[tmplte ].Add("nounify(<f>)"); // OPTION
  m_wordList[command].Add("nterms"); // FUNCTION
  m_wordList[tmplte ].Add("nterms(<expr>)"); // OPTION
  m_wordList[command].Add("op"); // FUNCTION
  m_wordList[tmplte ].Add("op(<expr>)"); // OPTION
  m_wordList[command].Add("operatorp"); // FUNCTION
  m_wordList[tmplte ].Add("operatorp(<expr>, <op>)"); // OPTION
  m_wordList[tmplte ].Add("operatorp(<expr>, [<op_1>, ..., <op_n>])"); // OPTION
  m_wordList[command].Add("optimize"); // FUNCTION
  m_wordList[tmplte ].Add("optimize(<expr>)"); // OPTION
  m_wordList[command].Add("optimprefix"); // OPTION
  m_wordList[command].Add("ordergreat"); // FUNCTION
  m_wordList[tmplte ].Add("ordergreat(<v_1>, ..., <v_n>)"); // OPTION
  m_wordList[tmplte ].Add("orderless(<v_1>, ..., <v_n>)"); // OPTION
  m_wordList[command].Add("ordergreatp"); // FUNCTION
  m_wordList[tmplte ].Add("ordergreatp(<expr_1>, <expr_2>)"); // OPTION
  m_wordList[tmplte ].Add("orderlessp(<expr_1>, <expr_2>)"); // OPTION
  m_wordList[command].Add("part"); // FUNCTION
  m_wordList[tmplte ].Add("part(<expr>, <n_1>, ..., <n_k>)"); // OPTION
  m_wordList[command].Add("partition"); // FUNCTION
  m_wordList[tmplte ].Add("partition(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("partswitch"); // OPTION
  m_wordList[command].Add("pickapart"); // FUNCTION
  m_wordList[tmplte ].Add("pickapart(<expr>, <n>)"); // OPTION
  m_wordList[command].Add("piece"); // OPTION
  m_wordList[command].Add("polarform"); // FUNCTION
  m_wordList[tmplte ].Add("polarform(<expr>)"); // OPTION
  m_wordList[command].Add("powers"); // FUNCTION
  m_wordList[tmplte ].Add("powers(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("product"); // FUNCTION
  m_wordList[tmplte ].Add("product(<expr>, <i>, <i_0>, <i_1>)"); // OPTION
  m_wordList[command].Add("realpart"); // FUNCTION
  m_wordList[tmplte ].Add("realpart(<expr>)"); // OPTION
  m_wordList[command].Add("rectform"); // FUNCTION
  m_wordList[tmplte ].Add("rectform(<expr>)"); // OPTION
  m_wordList[command].Add("rembox"); // FUNCTION
  m_wordList[tmplte ].Add("rembox(<expr>, unlabelled)"); // OPTION
  m_wordList[tmplte ].Add("rembox(<expr>, <label>)"); // OPTION
  m_wordList[tmplte ].Add("rembox(<expr>)"); // OPTION
  m_wordList[command].Add("sum"); // FUNCTION
  m_wordList[tmplte ].Add("sum(<expr>, <i>, <i_0>, <i_1>)"); // OPTION
  m_wordList[command].Add("lsum"); // FUNCTION
  m_wordList[tmplte ].Add("lsum(<expr>, <x>, <L>)"); // OPTION
  m_wordList[command].Add("unorder"); // FUNCTION
  m_wordList[tmplte ].Add("unorder()"); // OPTION
  m_wordList[command].Add("verbify"); // FUNCTION
  m_wordList[tmplte ].Add("verbify(<f>)"); // OPTION
  m_wordList[command].Add("constvalue"); // FUNCTION
  m_wordList[tmplte ].Add("constvalue(<x>)"); // OPTION
  m_wordList[tmplte ].Add("declare_constvalue(<a>, <x>)"); // OPTION
  m_wordList[command].Add("units"); // FUNCTION
  m_wordList[tmplte ].Add("units(<x>)"); // OPTION
  m_wordList[tmplte ].Add("declare_units(<a>, <u>)"); // OPTION
  m_wordList[command].Add("qty"); // FUNCTION
  m_wordList[tmplte ].Add("qty(<x>)"); // OPTION
  m_wordList[tmplte ].Add("declare_qty(<a>, <x>)"); // OPTION
  m_wordList[command].Add("unitp"); // FUNCTION
  m_wordList[tmplte ].Add("unitp(<x>)"); // OPTION
  m_wordList[command].Add("declare_unit_conversion"); // FUNCTION
  m_wordList[tmplte ].Add("declare_unit_conversion(<u> = <v>, ...)"); // OPTION
  m_wordList[command].Add("declare_dimensions"); // FUNCTION
  m_wordList[tmplte ].Add("declare_dimensions(<a_1>, <d_1>, ..., <a_n>, <d_n>)"); // OPTION
  m_wordList[tmplte ].Add("remove_dimensions(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[command].Add("declare_fundamental_dimensions"); // FUNCTION
  m_wordList[tmplte ].Add("declare_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList[tmplte ].Add("remove_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList[command].Add("declare_fundamental_units"); // FUNCTION
  m_wordList[tmplte ].Add("declare_fundamental_units(<u_1>, <d_1>, ..., <u_n>, <d_n>)"); // OPTION
  m_wordList[tmplte ].Add("remove_fundamental_units(<u_1>, ..., <u_n>)"); // OPTION
  m_wordList[command].Add("dimensions"); // FUNCTION
  m_wordList[tmplte ].Add("dimensions(<x>)"); // OPTION
  m_wordList[tmplte ].Add("dimensions_as_list(<x>)"); // OPTION
  m_wordList[command].Add("fundamental_units"); // FUNCTION
  m_wordList[tmplte ].Add("fundamental_units(<x>)"); // OPTION
  m_wordList[tmplte ].Add("fundamental_units()"); // OPTION
  m_wordList[command].Add("dimensionless"); // FUNCTION
  m_wordList[tmplte ].Add("dimensionless(<L>)"); // OPTION
  m_wordList[command].Add("natural_unit"); // FUNCTION
  m_wordList[tmplte ].Add("natural_unit(<expr>, [<v_1>, ..., <v_n>])"); // OPTION
  m_wordList[command].Add("f90"); // FUNCTION
  m_wordList[tmplte ].Add("f90(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("bffac"); // FUNCTION
  m_wordList[tmplte ].Add("bffac(<expr>, <n>)"); // OPTION
  m_wordList[command].Add("algepsilon"); // OPTION
  m_wordList[command].Add("bfloat"); // FUNCTION
  m_wordList[tmplte ].Add("bfloat(<expr>)"); // OPTION
  m_wordList[command].Add("bfloatp"); // FUNCTION
  m_wordList[tmplte ].Add("bfloatp(<expr>)"); // OPTION
  m_wordList[command].Add("bfpsi"); // FUNCTION
  m_wordList[tmplte ].Add("bfpsi(<n>, <z>, <fpprec>)"); // OPTION
  m_wordList[tmplte ].Add("bfpsi0(<z>, <fpprec>)"); // OPTION
  m_wordList[command].Add("bftorat"); // OPTION
  m_wordList[command].Add("bftrunc"); // OPTION
  m_wordList[command].Add("cbffac"); // FUNCTION
  m_wordList[tmplte ].Add("cbffac(<z>, <fpprec>)"); // OPTION
  m_wordList[command].Add("float"); // FUNCTION
  m_wordList[tmplte ].Add("float(<expr>)"); // OPTION
  m_wordList[command].Add("float2bf"); // OPTION
  m_wordList[command].Add("floatnump"); // FUNCTION
  m_wordList[tmplte ].Add("floatnump(<expr>)"); // OPTION
  m_wordList[command].Add("fpprec"); // OPTION
  m_wordList[command].Add("fpprintprec"); // OPTION
  m_wordList[command].Add("numer_pbranch"); // OPTION
  m_wordList[command].Add("buildq"); // FUNCTION
  m_wordList[tmplte ].Add("buildq(<L>, <expr>)"); // OPTION
  m_wordList[command].Add("macroexpand"); // FUNCTION
  m_wordList[tmplte ].Add("macroexpand(<expr>)"); // OPTION
  m_wordList[command].Add("macroexpand1"); // FUNCTION
  m_wordList[tmplte ].Add("macroexpand1(<expr>)"); // OPTION
  m_wordList[command].Add("macros"); // OPTION
  m_wordList[command].Add("splice"); // FUNCTION
  m_wordList[tmplte ].Add("splice(<a>)"); // OPTION
  m_wordList[command].Add("apply"); // FUNCTION
  m_wordList[tmplte ].Add("apply(<F>, [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("block"); // FUNCTION
  m_wordList[tmplte ].Add("block([<v_1>, ..., <v_m>], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[tmplte ].Add("block(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("break"); // FUNCTION
  m_wordList[tmplte ].Add("break(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("catch"); // FUNCTION
  m_wordList[tmplte ].Add("catch(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("compfile"); // FUNCTION
  m_wordList[tmplte ].Add("compfile(<filename>, <f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("compfile(<filename>, functions)"); // OPTION
  m_wordList[tmplte ].Add("compfile(<filename>, all)"); // OPTION
  m_wordList[command].Add("compile"); // FUNCTION
  m_wordList[tmplte ].Add("compile(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("compile(functions)"); // OPTION
  m_wordList[tmplte ].Add("compile(all)"); // OPTION
  m_wordList[command].Add("define"); // FUNCTION
  m_wordList[tmplte ].Add("define(<f>(<x_1>, ..., <x_n>), <expr>)"); // OPTION
  m_wordList[tmplte ].Add("define(<f>[<x_1>, ..., <x_n>], <expr>)"); // OPTION
  m_wordList[tmplte ].Add("define(funmake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList[tmplte ].Add("define(arraymake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList[tmplte ].Add("define(ev (<expr_1>), <expr_2>)"); // OPTION
  m_wordList[command].Add("define_variable"); // FUNCTION
  m_wordList[tmplte ].Add("define_variable(<name>, <default_value>, <mode>)"); // OPTION
  m_wordList[command].Add("dispfun"); // FUNCTION
  m_wordList[tmplte ].Add("dispfun(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("dispfun(all)"); // OPTION
  m_wordList[command].Add("functions"); // OPTION
  m_wordList[command].Add("fundef"); // FUNCTION
  m_wordList[tmplte ].Add("fundef(<f>)"); // OPTION
  m_wordList[command].Add("funmake"); // FUNCTION
  m_wordList[tmplte ].Add("funmake(<F>, [<arg_1>, ..., <arg_n>])"); // OPTION
  m_wordList[command].Add("lambda"); // FUNCTION
  m_wordList[tmplte ].Add("lambda([<x_1>, ..., <x_m>], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[tmplte ].Add("lambda([[<L>]], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[tmplte ].Add("lambda([<x_1>, ..., <x_m>, [<L>]], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("local"); // FUNCTION
  m_wordList[tmplte ].Add("local(<v_1>, ..., <v_n>)"); // OPTION
  m_wordList[command].Add("macroexpansion"); // OPTION
  m_wordList[command].Add("mode_checkp"); // OPTION
  m_wordList[command].Add("mode_check_errorp"); // OPTION
  m_wordList[command].Add("mode_check_warnp"); // OPTION
  m_wordList[command].Add("mode_declare"); // FUNCTION
  m_wordList[tmplte ].Add("mode_declare(<y_1>, <mode_1>, ..., <y_n>, <mode_n>)"); // OPTION
  m_wordList[command].Add("mode_identity"); // FUNCTION
  m_wordList[tmplte ].Add("mode_identity(<arg_1>, <arg_2>)"); // OPTION
  m_wordList[command].Add("transcompile"); // OPTION
  m_wordList[command].Add("translate"); // FUNCTION
  m_wordList[tmplte ].Add("translate(<f_1>, ..., <f_n>)"); // OPTION
  m_wordList[tmplte ].Add("translate(functions)"); // OPTION
  m_wordList[tmplte ].Add("translate(all)"); // OPTION
  m_wordList[command].Add("translate_file"); // FUNCTION
  m_wordList[tmplte ].Add("translate_file(<maxima_filename>)"); // OPTION
  m_wordList[tmplte ].Add("translate_file(<maxima_filename>, <lisp_filename>)"); // OPTION
  m_wordList[command].Add("transrun"); // OPTION
  m_wordList[command].Add("tr_array_as_ref"); // OPTION
  m_wordList[command].Add("tr_bound_function_applyp"); // OPTION
  m_wordList[command].Add("tr_file_tty_messagesp"); // OPTION
  m_wordList[command].Add("tr_float_can_branch_complex"); // OPTION
  m_wordList[command].Add("tr_function_call_default"); // OPTION
  m_wordList[command].Add("tr_numer"); // OPTION
  m_wordList[command].Add("tr_optimize_max_loop"); // OPTION
  m_wordList[command].Add("tr_semicompile"); // OPTION
  m_wordList[command].Add("tr_state_vars"); // OPTION
  m_wordList[command].Add("tr_warnings_get"); // FUNCTION
  m_wordList[tmplte ].Add("tr_warnings_get()"); // OPTION
  m_wordList[command].Add("tr_warn_bad_function_calls"); // OPTION
  m_wordList[command].Add("tr_warn_fexpr"); // OPTION
  m_wordList[command].Add("tr_warn_meval"); // OPTION
  m_wordList[command].Add("tr_warn_mode"); // OPTION
  m_wordList[command].Add("tr_warn_undeclared"); // OPTION
  m_wordList[command].Add("tr_warn_undefined_variable"); // OPTION
  m_wordList[command].Add("tr_windy"); // OPTION
  m_wordList[command].Add("compile_file"); // FUNCTION
  m_wordList[tmplte ].Add("compile_file(<filename>)"); // OPTION
  m_wordList[tmplte ].Add("compile_file(<filename>, <compiled_filename>)"); // OPTION
  m_wordList[tmplte ].Add("compile_file(<filename>, <compiled_filename>, <lisp_filename>)"); // OPTION
  m_wordList[command].Add("declare_translated"); // FUNCTION
  m_wordList[tmplte ].Add("declare_translated(<f_1>, <f_2>, ...)"); // OPTION
  m_wordList[command].Add("GGFINFINITY"); // OPTION
  m_wordList[command].Add("GGFCFMAX"); // OPTION
  m_wordList[command].Add("ggf"); // FUNCTION
  m_wordList[tmplte ].Add("ggf(<l>)"); // OPTION
  m_wordList[command].Add("create_graph"); // FUNCTION
  m_wordList[tmplte ].Add("create_graph(<v_list>, <e_list>)"); // OPTION
  m_wordList[tmplte ].Add("create_graph(<n>, <e_list>)"); // OPTION
  m_wordList[tmplte ].Add("create_graph(<v_list>, <e_list>, <directed>)"); // OPTION
  m_wordList[command].Add("copy_graph"); // FUNCTION
  m_wordList[tmplte ].Add("copy_graph(<g>)"); // OPTION
  m_wordList[command].Add("circulant_graph"); // FUNCTION
  m_wordList[tmplte ].Add("circulant_graph(<n>, <d>)"); // OPTION
  m_wordList[command].Add("clebsch_graph"); // FUNCTION
  m_wordList[tmplte ].Add("clebsch_graph()"); // OPTION
  m_wordList[command].Add("complement_graph"); // FUNCTION
  m_wordList[tmplte ].Add("complement_graph(<g>)"); // OPTION
  m_wordList[command].Add("complete_bipartite_graph"); // FUNCTION
  m_wordList[tmplte ].Add("complete_bipartite_graph(<n>, <m>)"); // OPTION
  m_wordList[command].Add("complete_graph"); // FUNCTION
  m_wordList[tmplte ].Add("complete_graph(<n>)"); // OPTION
  m_wordList[command].Add("cycle_digraph"); // FUNCTION
  m_wordList[tmplte ].Add("cycle_digraph(<n>)"); // OPTION
  m_wordList[command].Add("cycle_graph"); // FUNCTION
  m_wordList[tmplte ].Add("cycle_graph(<n>)"); // OPTION
  m_wordList[command].Add("cuboctahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("cuboctahedron_graph(<n>)"); // OPTION
  m_wordList[command].Add("cube_graph"); // FUNCTION
  m_wordList[tmplte ].Add("cube_graph(<n>)"); // OPTION
  m_wordList[command].Add("dodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("dodecahedron_graph()"); // OPTION
  m_wordList[command].Add("empty_graph"); // FUNCTION
  m_wordList[tmplte ].Add("empty_graph(<n>)"); // OPTION
  m_wordList[command].Add("flower_snark"); // FUNCTION
  m_wordList[tmplte ].Add("flower_snark(<n>)"); // OPTION
  m_wordList[command].Add("from_adjacency_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("from_adjacency_matrix(<A>)"); // OPTION
  m_wordList[command].Add("frucht_graph"); // FUNCTION
  m_wordList[tmplte ].Add("frucht_graph()"); // OPTION
  m_wordList[command].Add("graph_product"); // FUNCTION
  m_wordList[tmplte ].Add("graph_product(<g1>, <g1>)"); // OPTION
  m_wordList[command].Add("graph_union"); // FUNCTION
  m_wordList[tmplte ].Add("graph_union(<g1>, <g1>)"); // OPTION
  m_wordList[command].Add("grid_graph"); // FUNCTION
  m_wordList[tmplte ].Add("grid_graph(<n>, <m>)"); // OPTION
  m_wordList[command].Add("great_rhombicosidodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("great_rhombicosidodecahedron_graph()"); // OPTION
  m_wordList[command].Add("great_rhombicuboctahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("great_rhombicuboctahedron_graph()"); // OPTION
  m_wordList[command].Add("grotzch_graph"); // FUNCTION
  m_wordList[tmplte ].Add("grotzch_graph()"); // OPTION
  m_wordList[command].Add("heawood_graph"); // FUNCTION
  m_wordList[tmplte ].Add("heawood_graph()"); // OPTION
  m_wordList[command].Add("icosahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("icosahedron_graph()"); // OPTION
  m_wordList[command].Add("icosidodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("icosidodecahedron_graph()"); // OPTION
  m_wordList[command].Add("induced_subgraph"); // FUNCTION
  m_wordList[tmplte ].Add("induced_subgraph(<V>, <g>)"); // OPTION
  m_wordList[command].Add("line_graph"); // FUNCTION
  m_wordList[tmplte ].Add("line_graph(<g>)"); // OPTION
  m_wordList[command].Add("make_graph"); // FUNCTION
  m_wordList[tmplte ].Add("make_graph(<vrt>, <f>)"); // OPTION
  m_wordList[tmplte ].Add("make_graph(<vrt>, <f>, <oriented>)"); // OPTION
  m_wordList[command].Add("mycielski_graph"); // FUNCTION
  m_wordList[tmplte ].Add("mycielski_graph(<g>)"); // OPTION
  m_wordList[command].Add("new_graph"); // FUNCTION
  m_wordList[tmplte ].Add("new_graph()"); // OPTION
  m_wordList[command].Add("path_digraph"); // FUNCTION
  m_wordList[tmplte ].Add("path_digraph(<n>)"); // OPTION
  m_wordList[command].Add("path_graph"); // FUNCTION
  m_wordList[tmplte ].Add("path_graph(<n>)"); // OPTION
  m_wordList[command].Add("petersen_graph"); // FUNCTION
  m_wordList[tmplte ].Add("petersen_graph()"); // OPTION
  m_wordList[tmplte ].Add("petersen_graph(<n>, <d>)"); // OPTION
  m_wordList[command].Add("random_bipartite_graph"); // FUNCTION
  m_wordList[tmplte ].Add("random_bipartite_graph(<a>, <b>, <p>)"); // OPTION
  m_wordList[command].Add("random_digraph"); // FUNCTION
  m_wordList[tmplte ].Add("random_digraph(<n>, <p>)"); // OPTION
  m_wordList[command].Add("random_regular_graph"); // FUNCTION
  m_wordList[tmplte ].Add("random_regular_graph(<n>)"); // OPTION
  m_wordList[tmplte ].Add("random_regular_graph(<n>, <d>)"); // OPTION
  m_wordList[command].Add("random_graph"); // FUNCTION
  m_wordList[tmplte ].Add("random_graph(<n>, <p>)"); // OPTION
  m_wordList[command].Add("random_graph1"); // FUNCTION
  m_wordList[tmplte ].Add("random_graph1(<n>, <m>)"); // OPTION
  m_wordList[command].Add("random_network"); // FUNCTION
  m_wordList[tmplte ].Add("random_network(<n>, <p>, <w>)"); // OPTION
  m_wordList[command].Add("random_tournament"); // FUNCTION
  m_wordList[tmplte ].Add("random_tournament(<n>)"); // OPTION
  m_wordList[command].Add("random_tree"); // FUNCTION
  m_wordList[tmplte ].Add("random_tree(<n>)"); // OPTION
  m_wordList[command].Add("small_rhombicosidodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("small_rhombicosidodecahedron_graph()"); // OPTION
  m_wordList[command].Add("small_rhombicuboctahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("small_rhombicuboctahedron_graph()"); // OPTION
  m_wordList[command].Add("snub_cube_graph"); // FUNCTION
  m_wordList[tmplte ].Add("snub_cube_graph()"); // OPTION
  m_wordList[command].Add("snub_dodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("snub_dodecahedron_graph()"); // OPTION
  m_wordList[command].Add("truncated_cube_graph"); // FUNCTION
  m_wordList[tmplte ].Add("truncated_cube_graph()"); // OPTION
  m_wordList[command].Add("truncated_dodecahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("truncated_dodecahedron_graph()"); // OPTION
  m_wordList[command].Add("truncated_icosahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("truncated_icosahedron_graph()"); // OPTION
  m_wordList[command].Add("truncated_tetrahedron_graph"); // FUNCTION
  m_wordList[tmplte ].Add("truncated_tetrahedron_graph()"); // OPTION
  m_wordList[command].Add("tutte_graph"); // FUNCTION
  m_wordList[tmplte ].Add("tutte_graph()"); // OPTION
  m_wordList[command].Add("underlying_graph"); // FUNCTION
  m_wordList[tmplte ].Add("underlying_graph(<g>)"); // OPTION
  m_wordList[command].Add("wheel_graph"); // FUNCTION
  m_wordList[tmplte ].Add("wheel_graph(<n>)"); // OPTION
  m_wordList[command].Add("adjacency_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("adjacency_matrix(<gr>)"); // OPTION
  m_wordList[command].Add("average_degree"); // FUNCTION
  m_wordList[tmplte ].Add("average_degree(<gr>)"); // OPTION
  m_wordList[command].Add("biconnected_components"); // FUNCTION
  m_wordList[tmplte ].Add("biconnected_components(<gr>)"); // OPTION
  m_wordList[command].Add("bipartition"); // FUNCTION
  m_wordList[tmplte ].Add("bipartition(<gr>)"); // OPTION
  m_wordList[command].Add("chromatic_index"); // FUNCTION
  m_wordList[tmplte ].Add("chromatic_index(<gr>)"); // OPTION
  m_wordList[command].Add("chromatic_number"); // FUNCTION
  m_wordList[tmplte ].Add("chromatic_number(<gr>)"); // OPTION
  m_wordList[command].Add("clear_edge_weight"); // FUNCTION
  m_wordList[tmplte ].Add("clear_edge_weight(<e>, <gr>)"); // OPTION
  m_wordList[command].Add("clear_vertex_label"); // FUNCTION
  m_wordList[tmplte ].Add("clear_vertex_label(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("connected_components"); // FUNCTION
  m_wordList[tmplte ].Add("connected_components(<gr>)"); // OPTION
  m_wordList[command].Add("diameter"); // FUNCTION
  m_wordList[tmplte ].Add("diameter(<gr>)"); // OPTION
  m_wordList[command].Add("edge_coloring"); // FUNCTION
  m_wordList[tmplte ].Add("edge_coloring(<gr>)"); // OPTION
  m_wordList[command].Add("degree_sequence"); // FUNCTION
  m_wordList[tmplte ].Add("degree_sequence(<gr>)"); // OPTION
  m_wordList[command].Add("edge_connectivity"); // FUNCTION
  m_wordList[tmplte ].Add("edge_connectivity(<gr>)"); // OPTION
  m_wordList[command].Add("edges"); // FUNCTION
  m_wordList[tmplte ].Add("edges(<gr>)"); // OPTION
  m_wordList[command].Add("get_edge_weight"); // FUNCTION
  m_wordList[tmplte ].Add("get_edge_weight(<e>, <gr>)"); // OPTION
  m_wordList[tmplte ].Add("get_edge_weight(<e>, <gr>, <ifnot>)"); // OPTION
  m_wordList[command].Add("get_vertex_label"); // FUNCTION
  m_wordList[tmplte ].Add("get_vertex_label(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("graph_charpoly"); // FUNCTION
  m_wordList[tmplte ].Add("graph_charpoly(<gr>, <x>)"); // OPTION
  m_wordList[command].Add("graph_center"); // FUNCTION
  m_wordList[tmplte ].Add("graph_center(<gr>)"); // OPTION
  m_wordList[command].Add("graph_eigenvalues"); // FUNCTION
  m_wordList[tmplte ].Add("graph_eigenvalues(<gr>)"); // OPTION
  m_wordList[command].Add("graph_periphery"); // FUNCTION
  m_wordList[tmplte ].Add("graph_periphery(<gr>)"); // OPTION
  m_wordList[command].Add("graph_size"); // FUNCTION
  m_wordList[tmplte ].Add("graph_size(<gr>)"); // OPTION
  m_wordList[command].Add("graph_order"); // FUNCTION
  m_wordList[tmplte ].Add("graph_order(<gr>)"); // OPTION
  m_wordList[command].Add("girth"); // FUNCTION
  m_wordList[tmplte ].Add("girth(<gr>)"); // OPTION
  m_wordList[command].Add("hamilton_cycle"); // FUNCTION
  m_wordList[tmplte ].Add("hamilton_cycle(<gr>)"); // OPTION
  m_wordList[command].Add("hamilton_path"); // FUNCTION
  m_wordList[tmplte ].Add("hamilton_path(<gr>)"); // OPTION
  m_wordList[command].Add("isomorphism"); // FUNCTION
  m_wordList[tmplte ].Add("isomorphism(<gr1>, <gr2>)"); // OPTION
  m_wordList[command].Add("in_neighbors"); // FUNCTION
  m_wordList[tmplte ].Add("in_neighbors(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("is_biconnected"); // FUNCTION
  m_wordList[tmplte ].Add("is_biconnected(<gr>)"); // OPTION
  m_wordList[command].Add("is_bipartite"); // FUNCTION
  m_wordList[tmplte ].Add("is_bipartite(<gr>)"); // OPTION
  m_wordList[command].Add("is_connected"); // FUNCTION
  m_wordList[tmplte ].Add("is_connected(<gr>)"); // OPTION
  m_wordList[command].Add("is_digraph"); // FUNCTION
  m_wordList[tmplte ].Add("is_digraph(<gr>)"); // OPTION
  m_wordList[command].Add("is_edge_in_graph"); // FUNCTION
  m_wordList[tmplte ].Add("is_edge_in_graph(<e>, <gr>)"); // OPTION
  m_wordList[command].Add("is_graph"); // FUNCTION
  m_wordList[tmplte ].Add("is_graph(<gr>)"); // OPTION
  m_wordList[command].Add("is_graph_or_digraph"); // FUNCTION
  m_wordList[tmplte ].Add("is_graph_or_digraph(<gr>)"); // OPTION
  m_wordList[command].Add("is_isomorphic"); // FUNCTION
  m_wordList[tmplte ].Add("is_isomorphic(<gr1>, <gr2>)"); // OPTION
  m_wordList[command].Add("is_planar"); // FUNCTION
  m_wordList[tmplte ].Add("is_planar(<gr>)"); // OPTION
  m_wordList[command].Add("is_sconnected"); // FUNCTION
  m_wordList[tmplte ].Add("is_sconnected(<gr>)"); // OPTION
  m_wordList[command].Add("is_vertex_in_graph"); // FUNCTION
  m_wordList[tmplte ].Add("is_vertex_in_graph(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("is_tree"); // FUNCTION
  m_wordList[tmplte ].Add("is_tree(<gr>)"); // OPTION
  m_wordList[command].Add("laplacian_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("laplacian_matrix(<gr>)"); // OPTION
  m_wordList[command].Add("max_clique"); // FUNCTION
  m_wordList[tmplte ].Add("max_clique(<gr>)"); // OPTION
  m_wordList[command].Add("max_degree"); // FUNCTION
  m_wordList[tmplte ].Add("max_degree(<gr>)"); // OPTION
  m_wordList[command].Add("max_flow"); // FUNCTION
  m_wordList[tmplte ].Add("max_flow(<net>, <s>, <t>)"); // OPTION
  m_wordList[command].Add("max_independent_set"); // FUNCTION
  m_wordList[tmplte ].Add("max_independent_set(<gr>)"); // OPTION
  m_wordList[command].Add("max_matching"); // FUNCTION
  m_wordList[tmplte ].Add("max_matching(<gr>)"); // OPTION
  m_wordList[command].Add("min_degree"); // FUNCTION
  m_wordList[tmplte ].Add("min_degree(<gr>)"); // OPTION
  m_wordList[command].Add("min_edge_cut"); // FUNCTION
  m_wordList[tmplte ].Add("min_edge_cut(<gr>)"); // OPTION
  m_wordList[command].Add("min_vertex_cover"); // FUNCTION
  m_wordList[tmplte ].Add("min_vertex_cover(<gr>)"); // OPTION
  m_wordList[command].Add("min_vertex_cut"); // FUNCTION
  m_wordList[tmplte ].Add("min_vertex_cut(<gr>)"); // OPTION
  m_wordList[command].Add("minimum_spanning_tree"); // FUNCTION
  m_wordList[tmplte ].Add("minimum_spanning_tree(<gr>)"); // OPTION
  m_wordList[command].Add("neighbors"); // FUNCTION
  m_wordList[tmplte ].Add("neighbors(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("odd_girth"); // FUNCTION
  m_wordList[tmplte ].Add("odd_girth(<gr>)"); // OPTION
  m_wordList[command].Add("out_neighbors"); // FUNCTION
  m_wordList[tmplte ].Add("out_neighbors(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("planar_embedding"); // FUNCTION
  m_wordList[tmplte ].Add("planar_embedding(<gr>)"); // OPTION
  m_wordList[command].Add("print_graph"); // FUNCTION
  m_wordList[tmplte ].Add("print_graph(<gr>)"); // OPTION
  m_wordList[command].Add("radius"); // FUNCTION
  m_wordList[tmplte ].Add("radius(<gr>)"); // OPTION
  m_wordList[command].Add("set_edge_weight"); // FUNCTION
  m_wordList[tmplte ].Add("set_edge_weight(<e>, <w>, <gr>)"); // OPTION
  m_wordList[command].Add("set_vertex_label"); // FUNCTION
  m_wordList[tmplte ].Add("set_vertex_label(<v>, <l>, <gr>)"); // OPTION
  m_wordList[command].Add("shortest_path"); // FUNCTION
  m_wordList[tmplte ].Add("shortest_path(<u>, <v>, <gr>)"); // OPTION
  m_wordList[command].Add("shortest_weighted_path"); // FUNCTION
  m_wordList[tmplte ].Add("shortest_weighted_path(<u>, <v>, <gr>)"); // OPTION
  m_wordList[command].Add("strong_components"); // FUNCTION
  m_wordList[tmplte ].Add("strong_components(<gr>)"); // OPTION
  m_wordList[command].Add("topological_sort"); // FUNCTION
  m_wordList[tmplte ].Add("topological_sort(<dag>)"); // OPTION
  m_wordList[command].Add("vertex_connectivity"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_connectivity(<g>)"); // OPTION
  m_wordList[command].Add("vertex_degree"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_degree(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("vertex_distance"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_distance(<u>, <v>, <gr>)"); // OPTION
  m_wordList[command].Add("vertex_eccentricity"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_eccentricity(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("vertex_in_degree"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_in_degree(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("vertex_out_degree"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_out_degree(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("vertices"); // FUNCTION
  m_wordList[tmplte ].Add("vertices(<gr>)"); // OPTION
  m_wordList[command].Add("vertex_coloring"); // FUNCTION
  m_wordList[tmplte ].Add("vertex_coloring(<gr>)"); // OPTION
  m_wordList[command].Add("wiener_index"); // FUNCTION
  m_wordList[tmplte ].Add("wiener_index(<gr>)"); // OPTION
  m_wordList[command].Add("add_edge"); // FUNCTION
  m_wordList[tmplte ].Add("add_edge(<e>, <gr>)"); // OPTION
  m_wordList[command].Add("add_edges"); // FUNCTION
  m_wordList[tmplte ].Add("add_edges(<e_list>, <gr>)"); // OPTION
  m_wordList[command].Add("add_vertex"); // FUNCTION
  m_wordList[tmplte ].Add("add_vertex(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("add_vertices"); // FUNCTION
  m_wordList[tmplte ].Add("add_vertices(<v_list>, <gr>)"); // OPTION
  m_wordList[command].Add("connect_vertices"); // FUNCTION
  m_wordList[tmplte ].Add("connect_vertices(<v_list>, <u_list>, <gr>)"); // OPTION
  m_wordList[command].Add("contract_edge"); // FUNCTION
  m_wordList[tmplte ].Add("contract_edge(<e>, <gr>)"); // OPTION
  m_wordList[command].Add("remove_edge"); // FUNCTION
  m_wordList[tmplte ].Add("remove_edge(<e>, <gr>)"); // OPTION
  m_wordList[command].Add("remove_vertex"); // FUNCTION
  m_wordList[tmplte ].Add("remove_vertex(<v>, <gr>)"); // OPTION
  m_wordList[command].Add("dimacs_export"); // FUNCTION
  m_wordList[tmplte ].Add("dimacs_export(<gr>, <fl>)"); // OPTION
  m_wordList[tmplte ].Add("dimacs_export(<gr>, <fl>, <comment1>, ..., <commentn>)"); // OPTION
  m_wordList[command].Add("dimacs_import"); // FUNCTION
  m_wordList[tmplte ].Add("dimacs_import(<fl>)"); // OPTION
  m_wordList[command].Add("graph6_decode"); // FUNCTION
  m_wordList[tmplte ].Add("graph6_decode(<str>)"); // OPTION
  m_wordList[command].Add("graph6_encode"); // FUNCTION
  m_wordList[tmplte ].Add("graph6_encode(<gr>)"); // OPTION
  m_wordList[command].Add("graph6_export"); // FUNCTION
  m_wordList[tmplte ].Add("graph6_export(<gr_list>, <fl>)"); // OPTION
  m_wordList[command].Add("graph6_import"); // FUNCTION
  m_wordList[tmplte ].Add("graph6_import(<fl>)"); // OPTION
  m_wordList[command].Add("sparse6_decode"); // FUNCTION
  m_wordList[tmplte ].Add("sparse6_decode(<str>)"); // OPTION
  m_wordList[command].Add("sparse6_encode"); // FUNCTION
  m_wordList[tmplte ].Add("sparse6_encode(<gr>)"); // OPTION
  m_wordList[command].Add("sparse6_export"); // FUNCTION
  m_wordList[tmplte ].Add("sparse6_export(<gr_list>, <fl>)"); // OPTION
  m_wordList[command].Add("sparse6_import"); // FUNCTION
  m_wordList[tmplte ].Add("sparse6_import(<fl>)"); // OPTION
  m_wordList[command].Add("draw_graph"); // FUNCTION
  m_wordList[tmplte ].Add("draw_graph(<graph>)"); // OPTION
  m_wordList[tmplte ].Add("draw_graph(<graph>, <option1>, ..., <optionk>)"); // OPTION
  m_wordList[command].Add("draw_graph_program"); // OPTION
  m_wordList[command].Add("show_id"); // OPTION
  m_wordList[command].Add("show_label"); // OPTION
  m_wordList[command].Add("label_alignment"); // OPTION
  m_wordList[command].Add("show_weight"); // OPTION
  m_wordList[command].Add("vertex_type"); // OPTION
  m_wordList[command].Add("vertex_size"); // OPTION
  m_wordList[command].Add("vertex_color"); // OPTION
  m_wordList[command].Add("show_vertices"); // OPTION
  m_wordList[command].Add("show_vertex_type"); // OPTION
  m_wordList[command].Add("show_vertex_size"); // OPTION
  m_wordList[command].Add("show_vertex_color"); // OPTION
  m_wordList[command].Add("vertex_partition"); // OPTION
  m_wordList[command].Add("vertex_coloring"); // OPTION
  m_wordList[command].Add("edge_color"); // OPTION
  m_wordList[command].Add("edge_width"); // OPTION
  m_wordList[command].Add("edge_type"); // OPTION
  m_wordList[command].Add("show_edges"); // OPTION
  m_wordList[command].Add("show_edge_color"); // OPTION
  m_wordList[command].Add("show_edge_width"); // OPTION
  m_wordList[command].Add("show_edge_type"); // OPTION
  m_wordList[command].Add("edge_partition"); // OPTION
  m_wordList[command].Add("edge_coloring"); // OPTION
  m_wordList[command].Add("redraw"); // OPTION
  m_wordList[command].Add("head_angle"); // OPTION
  m_wordList[command].Add("head_length"); // OPTION
  m_wordList[command].Add("spring_embedding_depth"); // OPTION
  m_wordList[command].Add("terminal"); // OPTION
  m_wordList[command].Add("file_name"); // OPTION
  m_wordList[command].Add("program"); // OPTION
  m_wordList[command].Add("fixed_vertices"); // OPTION
  m_wordList[command].Add("vertices_to_path"); // FUNCTION
  m_wordList[tmplte ].Add("vertices_to_path(<v_list>)"); // OPTION
  m_wordList[command].Add("vertices_to_cycle"); // FUNCTION
  m_wordList[tmplte ].Add("vertices_to_cycle(<v_list>)"); // OPTION
  m_wordList[command].Add("poly_monomial_order"); // OPTION
  m_wordList[command].Add("poly_coefficient_ring"); // OPTION
  m_wordList[command].Add("poly_primary_elimination_order"); // OPTION
  m_wordList[command].Add("poly_secondary_elimination_order"); // OPTION
  m_wordList[command].Add("poly_elimination_order"); // OPTION
  m_wordList[command].Add("poly_return_term_list"); // OPTION
  m_wordList[command].Add("poly_grobner_debug"); // OPTION
  m_wordList[command].Add("poly_grobner_algorithm"); // OPTION
  m_wordList[command].Add("poly_top_reduction_only"); // OPTION
  m_wordList[command].Add("poly_add"); // FUNCTION
  m_wordList[tmplte ].Add("poly_add(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_subtract"); // FUNCTION
  m_wordList[tmplte ].Add("poly_subtract(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_multiply"); // FUNCTION
  m_wordList[tmplte ].Add("poly_multiply(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_s_polynomial"); // FUNCTION
  m_wordList[tmplte ].Add("poly_s_polynomial(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_primitive_part"); // FUNCTION
  m_wordList[tmplte ].Add("poly_primitive_part(<poly1>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_normalize"); // FUNCTION
  m_wordList[tmplte ].Add("poly_normalize(<poly>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_expand"); // FUNCTION
  m_wordList[tmplte ].Add("poly_expand(<poly>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_expt"); // FUNCTION
  m_wordList[tmplte ].Add("poly_expt(<poly>, <number>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_content"); // FUNCTION
  m_wordList[tmplte ].Add("poly_content(<poly>. <varlist>)"); // OPTION
  m_wordList[command].Add("poly_pseudo_divide"); // FUNCTION
  m_wordList[tmplte ].Add("poly_pseudo_divide(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_exact_divide"); // FUNCTION
  m_wordList[tmplte ].Add("poly_exact_divide(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_normal_form"); // FUNCTION
  m_wordList[tmplte ].Add("poly_normal_form(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_buchberger_criterion"); // FUNCTION
  m_wordList[tmplte ].Add("poly_buchberger_criterion(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_buchberger"); // FUNCTION
  m_wordList[tmplte ].Add("poly_buchberger(<polylist_fl>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_reduction"); // FUNCTION
  m_wordList[tmplte ].Add("poly_reduction(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_minimization"); // FUNCTION
  m_wordList[tmplte ].Add("poly_minimization(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_normalize_list"); // FUNCTION
  m_wordList[tmplte ].Add("poly_normalize_list(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_grobner"); // FUNCTION
  m_wordList[tmplte ].Add("poly_grobner(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_reduced_grobner"); // FUNCTION
  m_wordList[tmplte ].Add("poly_reduced_grobner(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_depends_p"); // FUNCTION
  m_wordList[tmplte ].Add("poly_depends_p(<poly>, <var>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_elimination_ideal"); // FUNCTION
  m_wordList[tmplte ].Add("poly_elimination_ideal(<polylist>, <number>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_colon_ideal"); // FUNCTION
  m_wordList[tmplte ].Add("poly_colon_ideal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_ideal_intersection"); // FUNCTION
  m_wordList[tmplte ].Add("poly_ideal_intersection(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_lcm"); // FUNCTION
  m_wordList[tmplte ].Add("poly_lcm(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_gcd"); // FUNCTION
  m_wordList[tmplte ].Add("poly_gcd(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_grobner_equal"); // FUNCTION
  m_wordList[tmplte ].Add("poly_grobner_equal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_grobner_subsetp"); // FUNCTION
  m_wordList[tmplte ].Add("poly_grobner_subsetp(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_grobner_member"); // FUNCTION
  m_wordList[tmplte ].Add("poly_grobner_member(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_ideal_saturation1"); // FUNCTION
  m_wordList[tmplte ].Add("poly_ideal_saturation1(<polylist>, <poly>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_ideal_saturation"); // FUNCTION
  m_wordList[tmplte ].Add("poly_ideal_saturation(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_ideal_polysaturation1"); // FUNCTION
  m_wordList[tmplte ].Add("poly_ideal_polysaturation1(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_ideal_polysaturation"); // FUNCTION
  m_wordList[tmplte ].Add("poly_ideal_polysaturation(<polylist>, <polylistlist>, <varlist>)"); // OPTION
  m_wordList[command].Add("poly_saturation_extension"); // FUNCTION
  m_wordList[tmplte ].Add("poly_saturation_extension(<poly>, <polylist>, <varlist1>, <varlist2>)"); // OPTION
  m_wordList[command].Add("poly_polysaturation_extension"); // FUNCTION
  m_wordList[tmplte ].Add("poly_polysaturation_extension(<poly>, <polylist>, <varlist1>, <varlist2>)"); // OPTION
  m_wordList[command].Add("todd_coxeter"); // FUNCTION
  m_wordList[tmplte ].Add("todd_coxeter(<relations>, <subgroup>)"); // OPTION
  m_wordList[tmplte ].Add("todd_coxeter(<relations>)"); // OPTION
  m_wordList[command].Add("apropos"); // FUNCTION
  m_wordList[tmplte ].Add("apropos(<string>)"); // OPTION
  m_wordList[command].Add("demo"); // FUNCTION
  m_wordList[tmplte ].Add("demo(<filename>)"); // OPTION
  m_wordList[command].Add("describe"); // FUNCTION
  m_wordList[tmplte ].Add("describe(<string>)"); // OPTION
  m_wordList[tmplte ].Add("describe(<string>, exact)"); // OPTION
  m_wordList[tmplte ].Add("describe(<string>, inexact)"); // OPTION
  m_wordList[command].Add("example"); // FUNCTION
  m_wordList[tmplte ].Add("example(<topic>)"); // OPTION
  m_wordList[tmplte ].Add("example()"); // OPTION
  m_wordList[command].Add("manual_demo"); // OPTION
  m_wordList[command].Add("implicit_derivative"); // FUNCTION
  m_wordList[tmplte ].Add("implicit_derivative(<f>,<indvarlist>,<orderlist>,<depvar>)"); // OPTION
  m_wordList[command].Add("implicit_plot"); // FUNCTION
  m_wordList[tmplte ].Add("implicit_plot(<expr>, <x_range>, <y_range>)"); // OPTION
  m_wordList[tmplte ].Add("implicit_plot([<expr_1>, ..., <expr_n>], <x_range>, <y_range>)"); // OPTION
  m_wordList[command].Add("__"); // OPTION
  m_wordList[command].Add("_"); // OPTION
  m_wordList[command].Add("%"); // OPTION
  m_wordList[command].Add("%%"); // OPTION
  m_wordList[command].Add("%edispflag"); // OPTION
  m_wordList[command].Add("%th"); // FUNCTION
  m_wordList[tmplte ].Add("%th(<i>)"); // OPTION
  m_wordList[command].Add("absboxchar"); // OPTION
  m_wordList[command].Add("file_output_append"); // OPTION
  m_wordList[command].Add("appendfile"); // FUNCTION
  m_wordList[tmplte ].Add("appendfile(<filename>)"); // OPTION
  m_wordList[command].Add("batch"); // FUNCTION
  m_wordList[tmplte ].Add("batch(<filename>)"); // OPTION
  m_wordList[command].Add("batchload"); // FUNCTION
  m_wordList[tmplte ].Add("batchload(<filename>)"); // OPTION
  m_wordList[command].Add("closefile"); // FUNCTION
  m_wordList[tmplte ].Add("closefile()"); // OPTION
  m_wordList[command].Add("close"); // FUNCTION
  m_wordList[tmplte ].Add("close(<stream>)"); // OPTION
  m_wordList[command].Add("collapse"); // FUNCTION
  m_wordList[tmplte ].Add("collapse(<expr>)"); // OPTION
  m_wordList[command].Add("concat"); // FUNCTION
  m_wordList[tmplte ].Add("concat(<arg_1>, <arg_2>, ...)"); // OPTION
  m_wordList[command].Add("sconcat"); // FUNCTION
  m_wordList[tmplte ].Add("sconcat(<arg_1>, <arg_2>, ...)"); // OPTION
  m_wordList[command].Add("disp"); // FUNCTION
  m_wordList[tmplte ].Add("disp(<expr_1>, <expr_2>, ...)"); // OPTION
  m_wordList[command].Add("dispcon"); // FUNCTION
  m_wordList[tmplte ].Add("dispcon(<tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("dispcon(all)"); // OPTION
  m_wordList[command].Add("display"); // FUNCTION
  m_wordList[tmplte ].Add("display(<expr_1>, <expr_2>, ...)"); // OPTION
  m_wordList[command].Add("display2d"); // OPTION
  m_wordList[command].Add("display_format_internal"); // OPTION
  m_wordList[command].Add("dispterms"); // FUNCTION
  m_wordList[tmplte ].Add("dispterms(<expr>)"); // OPTION
  m_wordList[command].Add("error_size"); // OPTION
  m_wordList[command].Add("error_syms"); // OPTION
  m_wordList[command].Add("expt"); // FUNCTION
  m_wordList[tmplte ].Add("expt(<a>, <b>)"); // OPTION
  m_wordList[command].Add("exptdispflag"); // OPTION
  m_wordList[command].Add("filename_merge"); // FUNCTION
  m_wordList[tmplte ].Add("filename_merge(<path>, <filename>)"); // OPTION
  m_wordList[command].Add("  file_output_append"); // OPTION
  m_wordList[command].Add("file_search"); // FUNCTION
  m_wordList[tmplte ].Add("file_search(<filename>)"); // OPTION
  m_wordList[tmplte ].Add("file_search(<filename>, <pathlist>)"); // OPTION
  m_wordList[command].Add("file_search_demo"); // OPTION
  m_wordList[command].Add("file_search_lisp"); // OPTION
  m_wordList[command].Add("file_search_maxima"); // OPTION
  m_wordList[command].Add("file_search_usage"); // OPTION
  m_wordList[command].Add("file_search_tests"); // OPTION
  m_wordList[command].Add("file_type"); // FUNCTION
  m_wordList[tmplte ].Add("file_type(<filename>)"); // OPTION
  m_wordList[command].Add("file_type_maxima"); // OPTION
  m_wordList[command].Add("file_type_lisp"); // OPTION
  m_wordList[command].Add("grind"); // FUNCTION
  m_wordList[tmplte ].Add("grind(<expr>)"); // OPTION
  m_wordList[command].Add("ibase"); // OPTION
  m_wordList[command].Add("inchar"); // OPTION
  m_wordList[command].Add("ldisp"); // FUNCTION
  m_wordList[tmplte ].Add("ldisp(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("ldisplay"); // FUNCTION
  m_wordList[tmplte ].Add("ldisplay(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("linechar"); // OPTION
  m_wordList[command].Add("linel"); // OPTION
  m_wordList[command].Add("lispdisp"); // OPTION
  m_wordList[command].Add("load"); // FUNCTION
  m_wordList[tmplte ].Add("load(<filename>)"); // OPTION
  m_wordList[command].Add("loadfile"); // FUNCTION
  m_wordList[tmplte ].Add("loadfile(<filename>)"); // OPTION
  m_wordList[command].Add("loadprint"); // OPTION
  m_wordList[command].Add("obase"); // OPTION
  m_wordList[command].Add("outchar"); // OPTION
  m_wordList[command].Add("packagefile"); // OPTION
  m_wordList[command].Add("pfeformat"); // OPTION
  m_wordList[command].Add("print"); // FUNCTION
  m_wordList[tmplte ].Add("print(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("printfile"); // FUNCTION
  m_wordList[tmplte ].Add("printfile(<path>)"); // OPTION
  m_wordList[command].Add("tcl_output"); // FUNCTION
  m_wordList[tmplte ].Add("tcl_output(<list>, <i0>, <skip>)"); // OPTION
  m_wordList[tmplte ].Add("tcl_output(<list>, <i0>)"); // OPTION
  m_wordList[tmplte ].Add("tcl_output([<list_1>, ..., <list_n>], <i>)"); // OPTION
  m_wordList[command].Add("read"); // FUNCTION
  m_wordList[tmplte ].Add("read(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("readonly"); // FUNCTION
  m_wordList[tmplte ].Add("readonly(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("reveal"); // FUNCTION
  m_wordList[tmplte ].Add("reveal(<expr>, <depth>)"); // OPTION
  m_wordList[command].Add("rmxchar"); // OPTION
  m_wordList[command].Add("save"); // FUNCTION
  m_wordList[tmplte ].Add("save(<filename>, <name_1>, <name_2>, <name_3>, ...)"); // OPTION
  m_wordList[tmplte ].Add("save(<filename>, values, functions, labels, ...)"); // OPTION
  m_wordList[tmplte ].Add("save(<filename>, [<m>, <n>])"); // OPTION
  m_wordList[tmplte ].Add("save(<filename>, <name_1>=<expr_1>, ...)"); // OPTION
  m_wordList[tmplte ].Add("save(<filename>, all)"); // OPTION
  m_wordList[tmplte ].Add("save(<filename>, <name_1>=<expr_1>, <name_2>=<expr_2>, ...)"); // OPTION
  m_wordList[command].Add("savedef"); // OPTION
  m_wordList[command].Add("show"); // FUNCTION
  m_wordList[tmplte ].Add("show(<expr>)"); // OPTION
  m_wordList[command].Add("showratvars"); // FUNCTION
  m_wordList[tmplte ].Add("showratvars(<expr>)"); // OPTION
  m_wordList[command].Add("stardisp"); // OPTION
  m_wordList[command].Add("string"); // FUNCTION
  m_wordList[tmplte ].Add("string(<expr>)"); // OPTION
  m_wordList[command].Add("stringdisp"); // OPTION
  m_wordList[command].Add("stringout"); // FUNCTION
  m_wordList[tmplte ].Add("stringout(<filename>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList[tmplte ].Add("stringout(<filename>, [<m>, <n>])"); // OPTION
  m_wordList[tmplte ].Add("stringout(<filename>, input)"); // OPTION
  m_wordList[tmplte ].Add("stringout(<filename>, functions)"); // OPTION
  m_wordList[tmplte ].Add("stringout(<filename>, values)"); // OPTION
  m_wordList[command].Add("tex"); // FUNCTION
  m_wordList[tmplte ].Add("tex(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("tex(<expr>, <destination>)"); // OPTION
  m_wordList[tmplte ].Add("tex(<expr>, false)"); // OPTION
  m_wordList[tmplte ].Add("tex(<label>)"); // OPTION
  m_wordList[tmplte ].Add("tex(<label>, <destination>)"); // OPTION
  m_wordList[tmplte ].Add("tex(<label>, false)"); // OPTION
  m_wordList[command].Add("tex1"); // FUNCTION
  m_wordList[tmplte ].Add("tex1(<e>)"); // OPTION
  m_wordList[command].Add("texput"); // FUNCTION
  m_wordList[tmplte ].Add("texput(<a>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("texput(<a>, <f>)"); // OPTION
  m_wordList[tmplte ].Add("texput(<a>, <s>, <operator_type>)"); // OPTION
  m_wordList[tmplte ].Add("texput(<a>, [<s_1>, <s_2>], matchfix)"); // OPTION
  m_wordList[tmplte ].Add("texput(<a>, [<s_1>, <s_2>, <s_3>], matchfix)"); // OPTION
  m_wordList[command].Add("get_tex_environment"); // FUNCTION
  m_wordList[tmplte ].Add("get_tex_environment(<op>)"); // OPTION
  m_wordList[tmplte ].Add("set_tex_environment(<op>, <before>, <after>)"); // OPTION
  m_wordList[command].Add("get_tex_environment_default"); // FUNCTION
  m_wordList[tmplte ].Add("get_tex_environment_default()"); // OPTION
  m_wordList[tmplte ].Add("set_tex_environment_default(<before>, <after>)"); // OPTION
  m_wordList[command].Add("system"); // FUNCTION
  m_wordList[tmplte ].Add("system(<command>)"); // OPTION
  m_wordList[command].Add("ttyoff"); // OPTION
  m_wordList[command].Add("with_stdout"); // FUNCTION
  m_wordList[tmplte ].Add("with_stdout(<f>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList[tmplte ].Add("with_stdout(<s>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList[command].Add("writefile"); // FUNCTION
  m_wordList[tmplte ].Add("writefile(<filename>)"); // OPTION
  m_wordList[command].Add("changevar"); // FUNCTION
  m_wordList[tmplte ].Add("changevar(<expr>, <f(x,y)>, <y>, <x>)"); // OPTION
  m_wordList[command].Add("dblint"); // FUNCTION
  m_wordList[tmplte ].Add("dblint(<f>, <r>, <s>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("defint"); // FUNCTION
  m_wordList[tmplte ].Add("defint(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("erfflag"); // OPTION
  m_wordList[command].Add("ilt"); // FUNCTION
  m_wordList[tmplte ].Add("ilt(<expr>, <s>, <t>)"); // OPTION
  m_wordList[command].Add("intanalysis"); // OPTION
  m_wordList[command].Add("integrate"); // FUNCTION
  m_wordList[tmplte ].Add("integrate(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("integrate(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("integration_constant"); // OPTION
  m_wordList[command].Add("integration_constant_counter"); // OPTION
  m_wordList[command].Add("integrate_use_rootsof"); // OPTION
  m_wordList[command].Add("ldefint"); // FUNCTION
  m_wordList[tmplte ].Add("ldefint(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("potential"); // FUNCTION
  m_wordList[tmplte ].Add("potential(<givengradient>)"); // OPTION
  m_wordList[command].Add("residue"); // FUNCTION
  m_wordList[tmplte ].Add("residue(<expr>, <z>, <z_0>)"); // OPTION
  m_wordList[command].Add("risch"); // FUNCTION
  m_wordList[tmplte ].Add("risch(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("tldefint"); // FUNCTION
  m_wordList[tmplte ].Add("tldefint(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("quad_qag"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qag(<f(x)>, <x>, <a>, <b>, <key>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qag(<f>, <x>, <a>, <b>, <key>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add("quad_qags"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qags(<f(x)>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qags(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add("quad_qagi"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qagi(<f(x)>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qagi(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add("quad_qawc"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qawc(<f(x)>, <x>, <c>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qawc(<f>, <x>, <c>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add("quad_qawf"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qawf(<f(x)>, <x>, <a>, <omega>, <trig>, [<epsabs>, <limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qawf(<f>, <x>, <a>, <omega>, <trig>, [<epsabs>, <limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList[command].Add("quad_qawo"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qawo(<f(x)>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, <limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qawo(<f>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, <limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList[command].Add("quad_qaws"); // FUNCTION
  m_wordList[tmplte ].Add("quad_qaws(<f(x)>, <x>, <a>, <b>, <alpha>, <beta>, <wfun>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[tmplte ].Add("quad_qaws(<f>, <x>, <a>, <b>, <alpha>, <beta>, <wfun>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add("lagrange"); // FUNCTION
  m_wordList[tmplte ].Add("lagrange(<points>)"); // OPTION
  m_wordList[tmplte ].Add("lagrange(<points>, <option>)"); // OPTION
  m_wordList[command].Add("charfun2"); // FUNCTION
  m_wordList[tmplte ].Add("charfun2(<x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("linearinterpol"); // FUNCTION
  m_wordList[tmplte ].Add("linearinterpol(<points>)"); // OPTION
  m_wordList[tmplte ].Add("linearinterpol(<points>, <option>)"); // OPTION
  m_wordList[command].Add("cspline"); // FUNCTION
  m_wordList[tmplte ].Add("cspline(<points>)"); // OPTION
  m_wordList[tmplte ].Add("cspline(<points>, <option1>, <option2>, ...)"); // OPTION
  m_wordList[command].Add("ratinterpol"); // FUNCTION
  m_wordList[tmplte ].Add("ratinterpol(<points>, <numdeg>)"); // OPTION
  m_wordList[tmplte ].Add("ratinterpol(<points>, <numdeg>, <option1>, <option2>, ...)"); // OPTION
  m_wordList[command].Add("entertensor"); // FUNCTION
  m_wordList[tmplte ].Add("entertensor(<name>)"); // OPTION
  m_wordList[command].Add("changename"); // FUNCTION
  m_wordList[tmplte ].Add("changename(<old>, <new>, <expr>)"); // OPTION
  m_wordList[command].Add("ishow"); // FUNCTION
  m_wordList[tmplte ].Add("ishow(<expr>)"); // OPTION
  m_wordList[command].Add("indices"); // FUNCTION
  m_wordList[tmplte ].Add("indices(<expr>)"); // OPTION
  m_wordList[command].Add("rename"); // FUNCTION
  m_wordList[tmplte ].Add("rename(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("rename(<expr>, <count>)"); // OPTION
  m_wordList[command].Add("flipflag"); // OPTION
  m_wordList[command].Add("defcon"); // FUNCTION
  m_wordList[tmplte ].Add("defcon(<tensor_1>)"); // OPTION
  m_wordList[tmplte ].Add("defcon(<tensor_1>, <tensor_2>, <tensor_3>)"); // OPTION
  m_wordList[command].Add("remcon"); // FUNCTION
  m_wordList[tmplte ].Add("remcon(<tensor_1>, ..., <tensor_n>)"); // OPTION
  m_wordList[tmplte ].Add("remcon(all)"); // OPTION
  m_wordList[command].Add("contract"); // FUNCTION
  m_wordList[tmplte ].Add("contract(<expr>)"); // OPTION
  m_wordList[command].Add("indexed_tensor"); // FUNCTION
  m_wordList[tmplte ].Add("indexed_tensor(<tensor>)"); // OPTION
  m_wordList[command].Add("components"); // FUNCTION
  m_wordList[tmplte ].Add("components(<tensor>, <expr>)"); // OPTION
  m_wordList[command].Add("remcomps"); // FUNCTION
  m_wordList[tmplte ].Add("remcomps(<tensor>)"); // OPTION
  m_wordList[command].Add("showcomps"); // FUNCTION
  m_wordList[tmplte ].Add("showcomps(<tensor>)"); // OPTION
  m_wordList[command].Add("idummy"); // FUNCTION
  m_wordList[tmplte ].Add("idummy()"); // OPTION
  m_wordList[command].Add("idummyx"); // OPTION
  m_wordList[command].Add("icounter"); // OPTION
  m_wordList[command].Add("kdelta"); // FUNCTION
  m_wordList[tmplte ].Add("kdelta(<L1>, <L2>)"); // OPTION
  m_wordList[command].Add("kdels"); // FUNCTION
  m_wordList[tmplte ].Add("kdels(<L1>, <L2>)"); // OPTION
  m_wordList[command].Add("levi_civita"); // FUNCTION
  m_wordList[tmplte ].Add("levi_civita(<L>)"); // OPTION
  m_wordList[command].Add("lc2kdt"); // FUNCTION
  m_wordList[tmplte ].Add("lc2kdt(<expr>)"); // OPTION
  m_wordList[command].Add("canten"); // FUNCTION
  m_wordList[tmplte ].Add("canten(<expr>)"); // OPTION
  m_wordList[command].Add("concan"); // FUNCTION
  m_wordList[tmplte ].Add("concan(<expr>)"); // OPTION
  m_wordList[command].Add("allsym"); // OPTION
  m_wordList[command].Add("decsym"); // FUNCTION
  m_wordList[tmplte ].Add("decsym(<tensor>, <m>, <n>, [<cov_1>, <cov_2>, ...], [<contr_1>, <contr_2>, ...])"); // OPTION
  m_wordList[command].Add("remsym"); // FUNCTION
  m_wordList[tmplte ].Add("remsym(<tensor>, <m>, <n>)"); // OPTION
  m_wordList[command].Add("canform"); // FUNCTION
  m_wordList[tmplte ].Add("canform(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("canform(<expr>, <rename>)"); // OPTION
  m_wordList[command].Add("diff"); // FUNCTION
  m_wordList[tmplte ].Add("diff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList[command].Add("idiff"); // FUNCTION
  m_wordList[tmplte ].Add("idiff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList[command].Add("liediff"); // FUNCTION
  m_wordList[tmplte ].Add("liediff(<v>, <ten>)"); // OPTION
  m_wordList[command].Add("rediff"); // FUNCTION
  m_wordList[tmplte ].Add("rediff(<ten>)"); // OPTION
  m_wordList[command].Add("undiff"); // FUNCTION
  m_wordList[tmplte ].Add("undiff(<expr>)"); // OPTION
  m_wordList[command].Add("evundiff"); // FUNCTION
  m_wordList[tmplte ].Add("evundiff(<expr>)"); // OPTION
  m_wordList[command].Add("flush"); // FUNCTION
  m_wordList[tmplte ].Add("flush(<expr>, <tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[command].Add("flushd"); // FUNCTION
  m_wordList[tmplte ].Add("flushd(<expr>, <tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[command].Add("flushnd"); // FUNCTION
  m_wordList[tmplte ].Add("flushnd(<expr>, <tensor>, <n>)"); // OPTION
  m_wordList[command].Add("coord"); // FUNCTION
  m_wordList[tmplte ].Add("coord(<tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[command].Add("remcoord"); // FUNCTION
  m_wordList[tmplte ].Add("remcoord(<tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[tmplte ].Add("remcoord(all)"); // OPTION
  m_wordList[command].Add("makebox"); // FUNCTION
  m_wordList[tmplte ].Add("makebox(<expr>)"); // OPTION
  m_wordList[command].Add("conmetderiv"); // FUNCTION
  m_wordList[tmplte ].Add("conmetderiv(<expr>, <tensor>)"); // OPTION
  m_wordList[command].Add("simpmetderiv"); // FUNCTION
  m_wordList[tmplte ].Add("simpmetderiv(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("simpmetderiv(<expr>[, <stop>])"); // OPTION
  m_wordList[command].Add("flush1deriv"); // FUNCTION
  m_wordList[tmplte ].Add("flush1deriv(<expr>, <tensor>)"); // OPTION
  m_wordList[command].Add("imetric"); // FUNCTION
  m_wordList[tmplte ].Add("imetric(<g>)"); // OPTION
  m_wordList[command].Add("idim"); // FUNCTION
  m_wordList[tmplte ].Add("idim(<n>)"); // OPTION
  m_wordList[command].Add("ichr1"); // FUNCTION
  m_wordList[tmplte ].Add("ichr1([<i>, <j>, <k>])"); // OPTION
  m_wordList[command].Add("ichr2"); // FUNCTION
  m_wordList[tmplte ].Add("ichr2([<i>, <j>], [<k>])"); // OPTION
  m_wordList[command].Add("icurvature"); // FUNCTION
  m_wordList[tmplte ].Add("icurvature([<i>, <j>, <k>], [<h>])"); // OPTION
  m_wordList[command].Add("covdiff"); // FUNCTION
  m_wordList[tmplte ].Add("covdiff(<expr>, <v_1>, <v_2>, ...)"); // OPTION
  m_wordList[command].Add("lorentz_gauge"); // FUNCTION
  m_wordList[tmplte ].Add("lorentz_gauge(<expr>)"); // OPTION
  m_wordList[command].Add("igeodesic_coords"); // FUNCTION
  m_wordList[tmplte ].Add("igeodesic_coords(<expr>, <name>)"); // OPTION
  m_wordList[command].Add("iframes"); // FUNCTION
  m_wordList[tmplte ].Add("iframes()"); // OPTION
  m_wordList[command].Add("ifb"); // OPTION
  m_wordList[command].Add("icc1"); // OPTION
  m_wordList[command].Add("icc2"); // OPTION
  m_wordList[command].Add("ifc1"); // OPTION
  m_wordList[command].Add("ifc2"); // OPTION
  m_wordList[command].Add("ifr"); // OPTION
  m_wordList[command].Add("ifri"); // OPTION
  m_wordList[command].Add("ifg"); // OPTION
  m_wordList[command].Add("ifgi"); // OPTION
  m_wordList[command].Add("iframe_bracket_form"); // OPTION
  m_wordList[command].Add("inm"); // OPTION
  m_wordList[command].Add("inmc1"); // OPTION
  m_wordList[command].Add("inmc2"); // OPTION
  m_wordList[command].Add("ikt1"); // OPTION
  m_wordList[command].Add("ikt2"); // OPTION
  m_wordList[command].Add("itr"); // OPTION
  m_wordList[command].Add("extdiff"); // FUNCTION
  m_wordList[tmplte ].Add("extdiff(<expr>, <i>)"); // OPTION
  m_wordList[command].Add("hodge"); // FUNCTION
  m_wordList[tmplte ].Add("hodge(<expr>)"); // OPTION
  m_wordList[command].Add("igeowedge_flag"); // OPTION
  m_wordList[command].Add("tentex"); // FUNCTION
  m_wordList[tmplte ].Add("tentex(<expr>)"); // OPTION
  m_wordList[command].Add("ic_convert"); // FUNCTION
  m_wordList[tmplte ].Add("ic_convert(<eqn>)"); // OPTION
  m_wordList[command].Add("dgeev"); // FUNCTION
  m_wordList[tmplte ].Add("dgeev(<A>)"); // OPTION
  m_wordList[tmplte ].Add("dgeev(<A>, <right_p>, <left_p>)"); // OPTION
  m_wordList[command].Add("dgesv"); // FUNCTION
  m_wordList[tmplte ].Add("dgesv(<A>, <b>)"); // OPTION
  m_wordList[command].Add("dgesvd"); // FUNCTION
  m_wordList[tmplte ].Add("dgesvd(<A>)"); // OPTION
  m_wordList[tmplte ].Add("dgesvd(<A>, <left_p>, <right_p>)"); // OPTION
  m_wordList[command].Add("dlange"); // FUNCTION
  m_wordList[tmplte ].Add("dlange(<norm>, <A>)"); // OPTION
  m_wordList[tmplte ].Add("zlange(<norm>, <A>)"); // OPTION
  m_wordList[command].Add("lbfgs"); // FUNCTION
  m_wordList[tmplte ].Add("lbfgs(<FOM>, <X>, <X0>, <epsilon>, <iprint>)"); // OPTION
  m_wordList[tmplte ].Add("lbfgs([<FOM>, <grad>] <X>, <X0>, <epsilon>, <iprint>)"); // OPTION
  m_wordList[command].Add("lbfgs_nfeval_max"); // OPTION
  m_wordList[command].Add("lbfgs_ncorrections"); // OPTION
  m_wordList[command].Add("lhospitallim"); // OPTION
  m_wordList[command].Add("limit"); // FUNCTION
  m_wordList[tmplte ].Add("limit(<expr>, <x>, <val>, <dir>)"); // OPTION
  m_wordList[tmplte ].Add("limit(<expr>, <x>, <val>)"); // OPTION
  m_wordList[tmplte ].Add("limit(<expr>)"); // OPTION
  m_wordList[command].Add("limsubst"); // OPTION
  m_wordList[command].Add("tlimit"); // FUNCTION
  m_wordList[tmplte ].Add("tlimit(<expr>, <x>, <val>, <dir>)"); // OPTION
  m_wordList[tmplte ].Add("tlimit(<expr>, <x>, <val>)"); // OPTION
  m_wordList[tmplte ].Add("tlimit(<expr>)"); // OPTION
  m_wordList[command].Add("tlimswitch"); // OPTION
  m_wordList[command].Add("Lindstedt"); // FUNCTION
  m_wordList[tmplte ].Add("Lindstedt(<eq>,<pvar>,<torder>,<ic>)"); // OPTION
  m_wordList[command].Add("addmatrices"); // FUNCTION
  m_wordList[tmplte ].Add("addmatrices(<f>, <M_1>, ..., <M_n>)"); // OPTION
  m_wordList[command].Add("blockmatrixp"); // FUNCTION
  m_wordList[tmplte ].Add("blockmatrixp(<M>)"); // OPTION
  m_wordList[command].Add("columnop"); // FUNCTION
  m_wordList[tmplte ].Add("columnop(<M>, <i>, <j>, <theta>)"); // OPTION
  m_wordList[command].Add("columnswap"); // FUNCTION
  m_wordList[tmplte ].Add("columnswap(<M>, <i>, <j>)"); // OPTION
  m_wordList[command].Add("columnspace"); // FUNCTION
  m_wordList[tmplte ].Add("columnspace(<M>)"); // OPTION
  m_wordList[command].Add("copy"); // FUNCTION
  m_wordList[tmplte ].Add("copy(<e>)"); // OPTION
  m_wordList[command].Add("cholesky"); // FUNCTION
  m_wordList[tmplte ].Add("cholesky(<M>)"); // OPTION
  m_wordList[tmplte ].Add("cholesky(<M>, <field>)"); // OPTION
  m_wordList[command].Add("ctranspose"); // FUNCTION
  m_wordList[tmplte ].Add("ctranspose(<M>)"); // OPTION
  m_wordList[command].Add("diag_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("diag_matrix(<d_1>, <d_2>,...,<d_n>)"); // OPTION
  m_wordList[command].Add("dotproduct"); // FUNCTION
  m_wordList[tmplte ].Add("dotproduct(<u>, <v>)"); // OPTION
  m_wordList[command].Add("eigens_by_jacobi"); // FUNCTION
  m_wordList[tmplte ].Add("eigens_by_jacobi(<A>)"); // OPTION
  m_wordList[tmplte ].Add("eigens_by_jacobi(<A>, <field_type>)"); // OPTION
  m_wordList[command].Add("get_lu_factors"); // FUNCTION
  m_wordList[tmplte ].Add("get_lu_factors(<x>) "); // OPTION
  m_wordList[command].Add("hankel"); // FUNCTION
  m_wordList[tmplte ].Add("hankel(<col>)"); // OPTION
  m_wordList[tmplte ].Add("hankel(<col>, <row>)"); // OPTION
  m_wordList[command].Add("hessian"); // FUNCTION
  m_wordList[tmplte ].Add("hessian(<f>, <x>)"); // OPTION
  m_wordList[command].Add("hilbert_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("hilbert_matrix(<n>)"); // OPTION
  m_wordList[command].Add("identfor"); // FUNCTION
  m_wordList[tmplte ].Add("identfor(<M>)"); // OPTION
  m_wordList[tmplte ].Add("identfor(<M>, <fld>)"); // OPTION
  m_wordList[command].Add("invert_by_lu"); // FUNCTION
  m_wordList[tmplte ].Add("invert_by_lu(<M>, <(rng generalring)>)"); // OPTION
  m_wordList[command].Add("jacobian"); // FUNCTION
  m_wordList[tmplte ].Add("jacobian(<f>, <x>)"); // OPTION
  m_wordList[command].Add("kronecker_product"); // FUNCTION
  m_wordList[tmplte ].Add("kronecker_product(<A>, <B>)"); // OPTION
  m_wordList[command].Add("listp"); // FUNCTION
  m_wordList[tmplte ].Add("listp(<e>, <p>)"); // OPTION
  m_wordList[tmplte ].Add("listp(<e>)"); // OPTION
  m_wordList[command].Add("locate_matrix_entry"); // FUNCTION
  m_wordList[tmplte ].Add("locate_matrix_entry(<M>, <r_1>, <c_1>, <r_2>, <c_2>, <f>, <rel>)"); // OPTION
  m_wordList[command].Add("lu_backsub"); // FUNCTION
  m_wordList[tmplte ].Add("lu_backsub(<M>, <b>)"); // OPTION
  m_wordList[command].Add("lu_factor"); // FUNCTION
  m_wordList[tmplte ].Add("lu_factor(<M>, <field>)"); // OPTION
  m_wordList[command].Add("mat_cond"); // FUNCTION
  m_wordList[tmplte ].Add("mat_cond(<M>, 1)"); // OPTION
  m_wordList[tmplte ].Add("mat_cond(<M>, inf)"); // OPTION
  m_wordList[command].Add("mat_norm"); // FUNCTION
  m_wordList[tmplte ].Add("mat_norm(<M>, 1)"); // OPTION
  m_wordList[tmplte ].Add("mat_norm(<M>, inf)"); // OPTION
  m_wordList[tmplte ].Add("mat_norm(<M>, frobenius)"); // OPTION
  m_wordList[command].Add("matrixp"); // FUNCTION
  m_wordList[tmplte ].Add("matrixp(<e>, <p>)"); // OPTION
  m_wordList[tmplte ].Add("matrixp(<e>)"); // OPTION
  m_wordList[command].Add("matrixexp"); // FUNCTION
  m_wordList[tmplte ].Add("matrixexp(<M>, <v>)"); // OPTION
  m_wordList[command].Add("matrix_size"); // FUNCTION
  m_wordList[tmplte ].Add("matrix_size(<M>)"); // OPTION
  m_wordList[command].Add("mat_fullunblocker"); // FUNCTION
  m_wordList[tmplte ].Add("mat_fullunblocker(<M>)"); // OPTION
  m_wordList[command].Add("mat_trace"); // FUNCTION
  m_wordList[tmplte ].Add("mat_trace(<M>)"); // OPTION
  m_wordList[command].Add("mat_unblocker"); // FUNCTION
  m_wordList[tmplte ].Add("mat_unblocker(<M>)"); // OPTION
  m_wordList[command].Add("nonnegintegerp"); // FUNCTION
  m_wordList[tmplte ].Add("nonnegintegerp(<n>)"); // OPTION
  m_wordList[command].Add("nullspace"); // FUNCTION
  m_wordList[tmplte ].Add("nullspace(<M>)"); // OPTION
  m_wordList[command].Add("nullity"); // FUNCTION
  m_wordList[tmplte ].Add("nullity(<M>)"); // OPTION
  m_wordList[command].Add("orthogonal_complement"); // FUNCTION
  m_wordList[tmplte ].Add("orthogonal_complement(<v_1>, ..., <v_n>)"); // OPTION
  m_wordList[command].Add("polynomialp"); // FUNCTION
  m_wordList[tmplte ].Add("polynomialp(<p>, <L>, <coeffp>, <exponp>)"); // OPTION
  m_wordList[tmplte ].Add("polynomialp(<p>, <L>, <coeffp>)"); // OPTION
  m_wordList[tmplte ].Add("polynomialp(<p>, <L>)"); // OPTION
  m_wordList[command].Add("polytocompanion"); // FUNCTION
  m_wordList[tmplte ].Add("polytocompanion(<p>, <x>)"); // OPTION
  m_wordList[command].Add("ptriangularize"); // FUNCTION
  m_wordList[tmplte ].Add("ptriangularize(<M>, <v>)"); // OPTION
  m_wordList[command].Add("rowop"); // FUNCTION
  m_wordList[tmplte ].Add("rowop(<M>, <i>, <j>, <theta>)"); // OPTION
  m_wordList[command].Add("rank"); // FUNCTION
  m_wordList[tmplte ].Add("rank(<M>)"); // OPTION
  m_wordList[command].Add("rowswap"); // FUNCTION
  m_wordList[tmplte ].Add("rowswap(<M>, <i>, <j>)"); // OPTION
  m_wordList[command].Add("toeplitz"); // FUNCTION
  m_wordList[tmplte ].Add("toeplitz(<col>)"); // OPTION
  m_wordList[tmplte ].Add("toeplitz(<col>, <row>)"); // OPTION
  m_wordList[command].Add("vandermonde_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("vandermonde_matrix([<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("zerofor"); // FUNCTION
  m_wordList[tmplte ].Add("zerofor(<M>)"); // OPTION
  m_wordList[tmplte ].Add("zerofor(<M>, <fld>)"); // OPTION
  m_wordList[command].Add("zeromatrixp"); // FUNCTION
  m_wordList[tmplte ].Add("zeromatrixp(<M>)"); // OPTION
  m_wordList[command].Add("append"); // FUNCTION
  m_wordList[tmplte ].Add("append(<list_1>, ..., <list_n>)"); // OPTION
  m_wordList[command].Add("assoc"); // FUNCTION
  m_wordList[tmplte ].Add("assoc(<key>, <list>, <default>)"); // OPTION
  m_wordList[tmplte ].Add("assoc(<key>, <list>)"); // OPTION
  m_wordList[command].Add("atom"); // FUNCTION
  m_wordList[tmplte ].Add("atom(<expr>)"); // OPTION
  m_wordList[command].Add("cons"); // FUNCTION
  m_wordList[tmplte ].Add("cons(<expr>, <list>)"); // OPTION
  m_wordList[command].Add("copylist"); // FUNCTION
  m_wordList[tmplte ].Add("copylist(<list>)"); // OPTION
  m_wordList[command].Add("create_list"); // FUNCTION
  m_wordList[tmplte ].Add("create_list(<form>, <x_1>, <list_1>, ..., <x_n>, <list_n>)"); // OPTION
  m_wordList[command].Add("delete"); // FUNCTION
  m_wordList[tmplte ].Add("delete(<expr_1>, <expr_2>)"); // OPTION
  m_wordList[tmplte ].Add("delete(<expr_1>, <expr_2>, <n>)"); // OPTION
  m_wordList[command].Add("eighth"); // FUNCTION
  m_wordList[tmplte ].Add("eighth(<expr>)"); // OPTION
  m_wordList[command].Add("endcons"); // FUNCTION
  m_wordList[tmplte ].Add("endcons(<expr>, <list>)"); // OPTION
  m_wordList[command].Add("fifth"); // FUNCTION
  m_wordList[tmplte ].Add("fifth(<expr>)"); // OPTION
  m_wordList[command].Add("first"); // FUNCTION
  m_wordList[tmplte ].Add("first(<expr>)"); // OPTION
  m_wordList[command].Add("fourth"); // FUNCTION
  m_wordList[tmplte ].Add("fourth(<expr>)"); // OPTION
  m_wordList[command].Add("get"); // FUNCTION
  m_wordList[tmplte ].Add("get(<a>, <i>)"); // OPTION
  m_wordList[command].Add("join"); // FUNCTION
  m_wordList[tmplte ].Add("join(<l>, <m>)"); // OPTION
  m_wordList[command].Add("last"); // FUNCTION
  m_wordList[tmplte ].Add("last(<expr>)"); // OPTION
  m_wordList[command].Add("length"); // FUNCTION
  m_wordList[tmplte ].Add("length(<expr>)"); // OPTION
  m_wordList[command].Add("listarith"); // OPTION
  m_wordList[command].Add("listp"); // FUNCTION
  m_wordList[tmplte ].Add("listp(<expr>)"); // OPTION
  m_wordList[command].Add("makelist"); // FUNCTION
  m_wordList[tmplte ].Add("makelist(<expr>, <i>, <i_0>, <i_1>)"); // OPTION
  m_wordList[tmplte ].Add("makelist(<expr>, <x>, <list>)"); // OPTION
  m_wordList[command].Add("member"); // FUNCTION
  m_wordList[tmplte ].Add("member(<expr_1>, <expr_2>)"); // OPTION
  m_wordList[command].Add("ninth"); // FUNCTION
  m_wordList[tmplte ].Add("ninth(<expr>)"); // OPTION
  m_wordList[command].Add("pop"); // FUNCTION
  m_wordList[tmplte ].Add("pop(<list>)"); // OPTION
  m_wordList[command].Add("push"); // FUNCTION
  m_wordList[tmplte ].Add("push(<item>, <list>)"); // OPTION
  m_wordList[command].Add("unique"); // FUNCTION
  m_wordList[tmplte ].Add("unique(<L>)"); // OPTION
  m_wordList[command].Add("rest"); // FUNCTION
  m_wordList[tmplte ].Add("rest(<expr>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("rest(<expr>)"); // OPTION
  m_wordList[command].Add("reverse"); // FUNCTION
  m_wordList[tmplte ].Add("reverse(<list>)"); // OPTION
  m_wordList[command].Add("second"); // FUNCTION
  m_wordList[tmplte ].Add("second(<expr>)"); // OPTION
  m_wordList[command].Add("seventh"); // FUNCTION
  m_wordList[tmplte ].Add("seventh(<expr>)"); // OPTION
  m_wordList[command].Add("sixth"); // FUNCTION
  m_wordList[tmplte ].Add("sixth(<expr>)"); // OPTION
  m_wordList[command].Add("sublist_indices"); // FUNCTION
  m_wordList[tmplte ].Add("sublist_indices(<L>, <P>)"); // OPTION
  m_wordList[command].Add("tenth"); // FUNCTION
  m_wordList[tmplte ].Add("tenth(<expr>)"); // OPTION
  m_wordList[command].Add("third"); // FUNCTION
  m_wordList[tmplte ].Add("third(<expr>)"); // OPTION
  m_wordList[command].Add("%e_to_numlog"); // OPTION
  m_wordList[command].Add("li"); // FUNCTION
  m_wordList[tmplte ].Add("li[<s>] (<z>)"); // OPTION
  m_wordList[command].Add("log"); // FUNCTION
  m_wordList[tmplte ].Add("log(<x>)"); // OPTION
  m_wordList[command].Add("logabs"); // OPTION
  m_wordList[command].Add("logarc"); // OPTION
  m_wordList[command].Add("logconcoeffp"); // OPTION
  m_wordList[command].Add("logcontract"); // FUNCTION
  m_wordList[tmplte ].Add("logcontract(<expr>)"); // OPTION
  m_wordList[command].Add("logexpand"); // OPTION
  m_wordList[command].Add("lognegint"); // OPTION
  m_wordList[command].Add("lognumer"); // OPTION
  m_wordList[command].Add("logsimp"); // OPTION
  m_wordList[command].Add("plog"); // FUNCTION
  m_wordList[tmplte ].Add("plog(<x>)"); // OPTION
  m_wordList[command].Add("lsquares_estimates"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_estimates(<D>, <x>, <e>, <a>)"); // OPTION
  m_wordList[tmplte ].Add("lsquares_estimates(<D>, <x>, <e>, <a>, initial = <L>, tol = <t>)"); // OPTION
  m_wordList[command].Add("lsquares_estimates_exact"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_estimates_exact(<MSE>, <a>)"); // OPTION
  m_wordList[command].Add("lsquares_estimates_approximate"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_estimates_approximate(<MSE>, <a>, initial = <L>, tol = <t>)"); // OPTION
  m_wordList[command].Add("lsquares_mse"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_mse(<D>, <x>, <e>)"); // OPTION
  m_wordList[command].Add("lsquares_residuals"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_residuals(<D>, <x>, <e>, <a>)"); // OPTION
  m_wordList[command].Add("lsquares_residual_mse"); // FUNCTION
  m_wordList[tmplte ].Add("lsquares_residual_mse(<D>, <x>, <e>, <a>)"); // OPTION
  m_wordList[command].Add("plsquares"); // FUNCTION
  m_wordList[tmplte ].Add("plsquares(<Mat>,<VarList>,<depvars>)"); // OPTION
  m_wordList[tmplte ].Add("plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>)"); // OPTION
  m_wordList[tmplte ].Add("plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>,<maxdegree>)"); // OPTION
  m_wordList[command].Add("makeOrders"); // FUNCTION
  m_wordList[tmplte ].Add("makeOrders(<indvarlist>,<orderlist>)"); // OPTION
  m_wordList[command].Add("addcol"); // FUNCTION
  m_wordList[tmplte ].Add("addcol(<M>, <list_1>, ..., <list_n>)"); // OPTION
  m_wordList[command].Add("addrow"); // FUNCTION
  m_wordList[tmplte ].Add("addrow(<M>, <list_1>, ..., <list_n>)"); // OPTION
  m_wordList[command].Add("adjoint"); // FUNCTION
  m_wordList[tmplte ].Add("adjoint(<M>)"); // OPTION
  m_wordList[command].Add("augcoefmatrix"); // FUNCTION
  m_wordList[tmplte ].Add("augcoefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("charpoly"); // FUNCTION
  m_wordList[tmplte ].Add("charpoly(<M>, <x>)"); // OPTION
  m_wordList[command].Add("coefmatrix"); // FUNCTION
  m_wordList[tmplte ].Add("coefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add("col"); // FUNCTION
  m_wordList[tmplte ].Add("col(<M>, <i>)"); // OPTION
  m_wordList[command].Add("columnvector"); // FUNCTION
  m_wordList[tmplte ].Add("columnvector(<L>)"); // OPTION
  m_wordList[tmplte ].Add("covect(<L>)"); // OPTION
  m_wordList[command].Add("conjugate"); // FUNCTION
  m_wordList[tmplte ].Add("conjugate(<x>)"); // OPTION
  m_wordList[command].Add("copymatrix"); // FUNCTION
  m_wordList[tmplte ].Add("copymatrix(<M>)"); // OPTION
  m_wordList[command].Add("determinant"); // FUNCTION
  m_wordList[tmplte ].Add("determinant(<M>)"); // OPTION
  m_wordList[command].Add("detout"); // OPTION
  m_wordList[command].Add("diagmatrix"); // FUNCTION
  m_wordList[tmplte ].Add("diagmatrix(<n>, <x>)"); // OPTION
  m_wordList[command].Add("doallmxops"); // OPTION
  m_wordList[command].Add("domxexpt"); // OPTION
  m_wordList[command].Add("domxmxops"); // OPTION
  m_wordList[command].Add("domxnctimes"); // OPTION
  m_wordList[command].Add("dontfactor"); // OPTION
  m_wordList[command].Add("doscmxops"); // OPTION
  m_wordList[command].Add("doscmxplus"); // OPTION
  m_wordList[command].Add("dot0nscsimp"); // OPTION
  m_wordList[command].Add("dot0simp"); // OPTION
  m_wordList[command].Add("dot1simp"); // OPTION
  m_wordList[command].Add("dotassoc"); // OPTION
  m_wordList[command].Add("dotconstrules"); // OPTION
  m_wordList[command].Add("dotdistrib"); // OPTION
  m_wordList[command].Add("dotexptsimp"); // OPTION
  m_wordList[command].Add("dotident"); // OPTION
  m_wordList[command].Add("dotscrules"); // OPTION
  m_wordList[command].Add("echelon"); // FUNCTION
  m_wordList[tmplte ].Add("echelon(<M>)"); // OPTION
  m_wordList[command].Add("eigenvalues"); // FUNCTION
  m_wordList[tmplte ].Add("eigenvalues(<M>)"); // OPTION
  m_wordList[tmplte ].Add("eivals(<M>)"); // OPTION
  m_wordList[command].Add("eigenvectors"); // FUNCTION
  m_wordList[tmplte ].Add("eigenvectors(<M>)"); // OPTION
  m_wordList[tmplte ].Add("eivects(<M>)"); // OPTION
  m_wordList[command].Add("ematrix"); // FUNCTION
  m_wordList[tmplte ].Add("ematrix(<m>, <n>, <x>, <i>, <j>)"); // OPTION
  m_wordList[command].Add("entermatrix"); // FUNCTION
  m_wordList[tmplte ].Add("entermatrix(<m>, <n>)"); // OPTION
  m_wordList[command].Add("genmatrix"); // FUNCTION
  m_wordList[tmplte ].Add("genmatrix(<a>, <i_2>, <j_2>, <i_1>, <j_1>)"); // OPTION
  m_wordList[tmplte ].Add("genmatrix(<a>, <i_2>, <j_2>, <i_1>)"); // OPTION
  m_wordList[tmplte ].Add("genmatrix(<a>, <i_2>, <j_2>)"); // OPTION
  m_wordList[command].Add("gramschmidt"); // FUNCTION
  m_wordList[tmplte ].Add("gramschmidt(<x>)"); // OPTION
  m_wordList[tmplte ].Add("gramschmidt(<x>, <F>)"); // OPTION
  m_wordList[command].Add("ident"); // FUNCTION
  m_wordList[tmplte ].Add("ident(<n>)"); // OPTION
  m_wordList[command].Add("innerproduct"); // FUNCTION
  m_wordList[tmplte ].Add("innerproduct(<x>, <y>)"); // OPTION
  m_wordList[tmplte ].Add("inprod(<x>, <y>)"); // OPTION
  m_wordList[command].Add("invert"); // FUNCTION
  m_wordList[tmplte ].Add("invert(<M>)"); // OPTION
  m_wordList[command].Add("lmxchar"); // OPTION
  m_wordList[command].Add("matrix"); // FUNCTION
  m_wordList[tmplte ].Add("matrix(<row_1>, ..., <row_n>)"); // OPTION
  m_wordList[command].Add("matrixmap"); // FUNCTION
  m_wordList[tmplte ].Add("matrixmap(<f>, <M>)"); // OPTION
  m_wordList[command].Add("matrixp"); // FUNCTION
  m_wordList[tmplte ].Add("matrixp(<expr>)"); // OPTION
  m_wordList[command].Add("matrix_element_add"); // OPTION
  m_wordList[command].Add("matrix_element_mult"); // OPTION
  m_wordList[command].Add("matrix_element_transpose"); // OPTION
  m_wordList[command].Add("mattrace"); // FUNCTION
  m_wordList[tmplte ].Add("mattrace(<M>)"); // OPTION
  m_wordList[command].Add("minor"); // FUNCTION
  m_wordList[tmplte ].Add("minor(<M>, <i>, <j>)"); // OPTION
  m_wordList[command].Add("ncexpt"); // FUNCTION
  m_wordList[tmplte ].Add("ncexpt(<a>, <b>)"); // OPTION
  m_wordList[command].Add("ncharpoly"); // FUNCTION
  m_wordList[tmplte ].Add("ncharpoly(<M>, <x>)"); // OPTION
  m_wordList[command].Add("newdet"); // FUNCTION
  m_wordList[tmplte ].Add("newdet(<M>)"); // OPTION
  m_wordList[command].Add("nonscalar"); // OPTION
  m_wordList[command].Add("nonscalarp"); // FUNCTION
  m_wordList[tmplte ].Add("nonscalarp(<expr>)"); // OPTION
  m_wordList[command].Add("permanent"); // FUNCTION
  m_wordList[tmplte ].Add("permanent(<M>)"); // OPTION
  m_wordList[command].Add("rank"); // FUNCTION
  m_wordList[tmplte ].Add("rank(<M>)"); // OPTION
  m_wordList[command].Add("ratmx"); // OPTION
  m_wordList[command].Add("row"); // FUNCTION
  m_wordList[tmplte ].Add("row(<M>, <i>)"); // OPTION
  m_wordList[command].Add("scalarmatrixp"); // OPTION
  m_wordList[command].Add("scalefactors"); // FUNCTION
  m_wordList[tmplte ].Add("scalefactors(<coordinatetransform>)"); // OPTION
  m_wordList[command].Add("setelmx"); // FUNCTION
  m_wordList[tmplte ].Add("setelmx(<x>, <i>, <j>, <M>)"); // OPTION
  m_wordList[command].Add("similaritytransform"); // FUNCTION
  m_wordList[tmplte ].Add("similaritytransform(<M>)"); // OPTION
  m_wordList[tmplte ].Add("simtran(<M>)"); // OPTION
  m_wordList[command].Add("sparse"); // OPTION
  m_wordList[command].Add("submatrix"); // FUNCTION
  m_wordList[tmplte ].Add("submatrix(<i_1>, ..., <i_m>, <M>, <j_1>, ..., <j_n>)"); // OPTION
  m_wordList[tmplte ].Add("submatrix(<i_1>, ..., <i_m>, <M>)"); // OPTION
  m_wordList[tmplte ].Add("submatrix(<M>, <j_1>, ..., <j_n>)"); // OPTION
  m_wordList[command].Add("transpose"); // FUNCTION
  m_wordList[tmplte ].Add("transpose(<M>)"); // OPTION
  m_wordList[command].Add("triangularize"); // FUNCTION
  m_wordList[tmplte ].Add("triangularize(<M>)"); // OPTION
  m_wordList[command].Add("uniteigenvectors"); // FUNCTION
  m_wordList[tmplte ].Add("uniteigenvectors(<M>)"); // OPTION
  m_wordList[tmplte ].Add("ueivects(<M>)"); // OPTION
  m_wordList[command].Add("unitvector"); // FUNCTION
  m_wordList[tmplte ].Add("unitvector(<x>)"); // OPTION
  m_wordList[tmplte ].Add("uvect(<x>)"); // OPTION
  m_wordList[command].Add("vectorsimp"); // FUNCTION
  m_wordList[tmplte ].Add("vectorsimp(<expr>)"); // OPTION
  m_wordList[command].Add("vect_cross"); // OPTION
  m_wordList[command].Add("zeromatrix"); // FUNCTION
  m_wordList[tmplte ].Add("zeromatrix(<m>, <n>)"); // OPTION
  m_wordList[command].Add("minpack_lsquares"); // FUNCTION
  m_wordList[tmplte ].Add("minpack_lsquares(<flist>, <varlist>, <guess> [, <tolerance>, <jacobian>])"); // OPTION
  m_wordList[command].Add("minpack_solve"); // FUNCTION
  m_wordList[tmplte ].Add("minpack_solve(<flist>, <varlist>, <guess> [, <tolerance>, <jacobian>])"); // OPTION
  m_wordList[command].Add("aliases"); // OPTION
  m_wordList[command].Add("alphabetic"); // OPTION
  m_wordList[command].Add("args"); // FUNCTION
  m_wordList[tmplte ].Add("args(<expr>)"); // OPTION
  m_wordList[command].Add("genindex"); // OPTION
  m_wordList[command].Add("gensumnum"); // OPTION
  m_wordList[tmplte ].Add("gensym(<x>)"); // OPTION
  m_wordList[command].Add("infolists"); // OPTION
  m_wordList[command].Add("integerp"); // FUNCTION
  m_wordList[tmplte ].Add("integerp(<expr>)"); // OPTION
  m_wordList[command].Add("m1pbranch"); // OPTION
  m_wordList[command].Add("numberp"); // FUNCTION
  m_wordList[tmplte ].Add("numberp(<expr>)"); // OPTION
  m_wordList[command].Add("properties"); // FUNCTION
  m_wordList[tmplte ].Add("properties(<a>)"); // OPTION
  m_wordList[command].Add("props"); // OPTION
  m_wordList[command].Add("propvars"); // FUNCTION
  m_wordList[tmplte ].Add("propvars(<prop>)"); // OPTION
  m_wordList[command].Add("put"); // FUNCTION
  m_wordList[tmplte ].Add("put(<atom>, <value>, <indicator>)"); // OPTION
  m_wordList[command].Add("qput"); // FUNCTION
  m_wordList[tmplte ].Add("qput(<atom>, <value>, <indicator>)"); // OPTION
  m_wordList[command].Add("rem"); // FUNCTION
  m_wordList[tmplte ].Add("rem(<atom>, <indicator>)"); // OPTION
  m_wordList[command].Add("remove"); // FUNCTION
  m_wordList[tmplte ].Add("remove(<a_1>, <p_1>, ..., <a_n>, <p_n>)"); // OPTION
  m_wordList[tmplte ].Add("remove([<a_1>, ..., <a_m>], [<p_1>, ..., <p_n>], ...)"); // OPTION
  m_wordList[tmplte ].Add("remove(\"<a>\", operator)"); // OPTION
  m_wordList[tmplte ].Add("remove(<a>, transfun)"); // OPTION
  m_wordList[tmplte ].Add("remove(all, <p>)"); // OPTION
  m_wordList[command].Add("remvalue"); // FUNCTION
  m_wordList[tmplte ].Add("remvalue(<name_1>, ..., <name_n>)"); // OPTION
  m_wordList[tmplte ].Add("remvalue(all)"); // OPTION
  m_wordList[command].Add("rncombine"); // FUNCTION
  m_wordList[tmplte ].Add("rncombine(<expr>)"); // OPTION
  m_wordList[command].Add("scalarp"); // FUNCTION
  m_wordList[tmplte ].Add("scalarp(<expr>)"); // OPTION
  m_wordList[command].Add("setup_autoload"); // FUNCTION
  m_wordList[tmplte ].Add("setup_autoload(<filename>, <function_1>, ..., <function_n>)"); // OPTION
  m_wordList[command].Add("newtonepsilon"); // OPTION
  m_wordList[command].Add("newtonmaxiter"); // OPTION
  m_wordList[command].Add("mnewton"); // FUNCTION
  m_wordList[tmplte ].Add("mnewton(<FuncList>,<VarList>,<GuessList>)"); // OPTION
  m_wordList[command].Add("adjoin"); // FUNCTION
  m_wordList[tmplte ].Add("adjoin(<x>, <a>) "); // OPTION
  m_wordList[command].Add("belln"); // FUNCTION
  m_wordList[tmplte ].Add("belln(<n>)"); // OPTION
  m_wordList[command].Add("cardinality"); // FUNCTION
  m_wordList[tmplte ].Add("cardinality(<a>)"); // OPTION
  m_wordList[command].Add("cartesian_product"); // FUNCTION
  m_wordList[tmplte ].Add("cartesian_product(<b_1>, ... , <b_n>)"); // OPTION
  m_wordList[command].Add("disjoin"); // FUNCTION
  m_wordList[tmplte ].Add("disjoin(<x>, <a>)"); // OPTION
  m_wordList[command].Add("disjointp"); // FUNCTION
  m_wordList[tmplte ].Add("disjointp(<a>, <b>) "); // OPTION
  m_wordList[command].Add("divisors"); // FUNCTION
  m_wordList[tmplte ].Add("divisors(<n>)"); // OPTION
  m_wordList[command].Add("elementp"); // FUNCTION
  m_wordList[tmplte ].Add("elementp(<x>, <a>)"); // OPTION
  m_wordList[command].Add("emptyp"); // FUNCTION
  m_wordList[tmplte ].Add("emptyp(<a>)"); // OPTION
  m_wordList[command].Add("equiv_classes"); // FUNCTION
  m_wordList[tmplte ].Add("equiv_classes(<s>, <F>)"); // OPTION
  m_wordList[command].Add("every"); // FUNCTION
  m_wordList[tmplte ].Add("every(<f>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("every(<f>, <L_1>, ..., <L_n>)"); // OPTION
  m_wordList[command].Add("extremal_subset"); // FUNCTION
  m_wordList[tmplte ].Add("extremal_subset(<s>, <f>, max)"); // OPTION
  m_wordList[tmplte ].Add("extremal_subset(<s>, <f>, min)"); // OPTION
  m_wordList[command].Add("flatten"); // FUNCTION
  m_wordList[tmplte ].Add("flatten(<expr>)"); // OPTION
  m_wordList[command].Add("full_listify"); // FUNCTION
  m_wordList[tmplte ].Add("full_listify(<a>)"); // OPTION
  m_wordList[command].Add("fullsetify"); // FUNCTION
  m_wordList[tmplte ].Add("fullsetify(<a>)"); // OPTION
  m_wordList[command].Add("identity"); // FUNCTION
  m_wordList[tmplte ].Add("identity(<x>)"); // OPTION
  m_wordList[command].Add("integer_partitions"); // FUNCTION
  m_wordList[tmplte ].Add("integer_partitions(<n>)"); // OPTION
  m_wordList[tmplte ].Add("integer_partitions(<n>, <len>)"); // OPTION
  m_wordList[command].Add("intersect"); // FUNCTION
  m_wordList[tmplte ].Add("intersect(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[command].Add("intersection"); // FUNCTION
  m_wordList[tmplte ].Add("intersection(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[command].Add("kron_delta"); // FUNCTION
  m_wordList[tmplte ].Add("kron_delta(<x>, <y>)"); // OPTION
  m_wordList[command].Add("listify"); // FUNCTION
  m_wordList[tmplte ].Add("listify(<a>)"); // OPTION
  m_wordList[command].Add("lreduce"); // FUNCTION
  m_wordList[tmplte ].Add("lreduce(<F>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("lreduce(<F>, <s>, <s_0>)"); // OPTION
  m_wordList[command].Add("makeset"); // FUNCTION
  m_wordList[tmplte ].Add("makeset(<expr>, <x>, <s>)"); // OPTION
  m_wordList[command].Add("moebius"); // FUNCTION
  m_wordList[tmplte ].Add("moebius(<n>)"); // OPTION
  m_wordList[command].Add("multinomial_coeff"); // FUNCTION
  m_wordList[tmplte ].Add("multinomial_coeff(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[tmplte ].Add("multinomial_coeff()"); // OPTION
  m_wordList[command].Add("num_distinct_partitions"); // FUNCTION
  m_wordList[tmplte ].Add("num_distinct_partitions(<n>)"); // OPTION
  m_wordList[tmplte ].Add("num_distinct_partitions(<n>, list)"); // OPTION
  m_wordList[command].Add("num_partitions"); // FUNCTION
  m_wordList[tmplte ].Add("num_partitions(<n>)"); // OPTION
  m_wordList[tmplte ].Add("num_partitions(<n>, list)"); // OPTION
  m_wordList[command].Add("partition_set"); // FUNCTION
  m_wordList[tmplte ].Add("partition_set(<a>, <f>)"); // OPTION
  m_wordList[command].Add("permutations"); // FUNCTION
  m_wordList[tmplte ].Add("permutations(<a>)"); // OPTION
  m_wordList[command].Add("powerset"); // FUNCTION
  m_wordList[tmplte ].Add("powerset(<a>)"); // OPTION
  m_wordList[tmplte ].Add("powerset(<a>, <n>)"); // OPTION
  m_wordList[command].Add("random_permutation"); // FUNCTION
  m_wordList[tmplte ].Add("random_permutation(<a>)"); // OPTION
  m_wordList[command].Add("rreduce"); // FUNCTION
  m_wordList[tmplte ].Add("rreduce(<F>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("rreduce(<F>, <s>, @var{s_@{n + 1@}})"); // OPTION
  m_wordList[command].Add("setdifference"); // FUNCTION
  m_wordList[tmplte ].Add("setdifference(<a>, <b>)"); // OPTION
  m_wordList[command].Add("setequalp"); // FUNCTION
  m_wordList[tmplte ].Add("setequalp(<a>, <b>)"); // OPTION
  m_wordList[command].Add("setify"); // FUNCTION
  m_wordList[tmplte ].Add("setify(<a>)"); // OPTION
  m_wordList[command].Add("setp"); // FUNCTION
  m_wordList[tmplte ].Add("setp(<a>)"); // OPTION
  m_wordList[command].Add("set_partitions"); // FUNCTION
  m_wordList[tmplte ].Add("set_partitions(<a>)"); // OPTION
  m_wordList[tmplte ].Add("set_partitions(<a>, <n>)"); // OPTION
  m_wordList[command].Add("some"); // FUNCTION
  m_wordList[tmplte ].Add("some(<f>, <a>)"); // OPTION
  m_wordList[tmplte ].Add("some(<f>, <L_1>, ..., <L_n>)"); // OPTION
  m_wordList[command].Add("stirling1"); // FUNCTION
  m_wordList[tmplte ].Add("stirling1(<n>, <m>)"); // OPTION
  m_wordList[command].Add("stirling2"); // FUNCTION
  m_wordList[tmplte ].Add("stirling2(<n>, <m>)"); // OPTION
  m_wordList[command].Add("subset"); // FUNCTION
  m_wordList[tmplte ].Add("subset(<a>, <f>)"); // OPTION
  m_wordList[command].Add("subsetp"); // FUNCTION
  m_wordList[tmplte ].Add("subsetp(<a>, <b>)"); // OPTION
  m_wordList[command].Add("symmdifference"); // FUNCTION
  m_wordList[tmplte ].Add("symmdifference(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[command].Add("tree_reduce"); // FUNCTION
  m_wordList[tmplte ].Add("tree_reduce(<F>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("tree_reduce(<F>, <s>, <s_0>)"); // OPTION
  m_wordList[command].Add("union"); // FUNCTION
  m_wordList[tmplte ].Add("union(<a_1>, ..., <a_n>)"); // OPTION
  m_wordList[command].Add("xreduce"); // FUNCTION
  m_wordList[tmplte ].Add("xreduce(<F>, <s>)"); // OPTION
  m_wordList[tmplte ].Add("xreduce(<F>, <s>, <s_0>)"); // OPTION
  m_wordList[command].Add("bern"); // FUNCTION
  m_wordList[tmplte ].Add("bern(<n>)"); // OPTION
  m_wordList[command].Add("bernpoly"); // FUNCTION
  m_wordList[tmplte ].Add("bernpoly(<x>, <n>)"); // OPTION
  m_wordList[command].Add("bfzeta"); // FUNCTION
  m_wordList[tmplte ].Add("bfzeta(<s>, <n>)"); // OPTION
  m_wordList[command].Add("bfhzeta"); // FUNCTION
  m_wordList[tmplte ].Add("bfhzeta(<s>, <h>, <n>)"); // OPTION
  m_wordList[command].Add("binomial"); // FUNCTION
  m_wordList[tmplte ].Add("binomial(<x>, <y>)"); // OPTION
  m_wordList[command].Add("burn"); // FUNCTION
  m_wordList[tmplte ].Add("burn(<n>)"); // OPTION
  m_wordList[command].Add("cf"); // FUNCTION
  m_wordList[tmplte ].Add("cf(<expr>)"); // OPTION
  m_wordList[command].Add("cfdisrep"); // FUNCTION
  m_wordList[tmplte ].Add("cfdisrep(<list>)"); // OPTION
  m_wordList[command].Add("cfexpand"); // FUNCTION
  m_wordList[tmplte ].Add("cfexpand(<x>)"); // OPTION
  m_wordList[command].Add("cflength"); // OPTION
  m_wordList[command].Add("divsum"); // FUNCTION
  m_wordList[tmplte ].Add("divsum(<n>, <k>)"); // OPTION
  m_wordList[tmplte ].Add("divsum(<n>)"); // OPTION
  m_wordList[command].Add("euler"); // FUNCTION
  m_wordList[tmplte ].Add("euler(<n>)"); // OPTION
  m_wordList[command].Add("%gamma"); // OPTION
  m_wordList[command].Add("factorial"); // FUNCTION
  m_wordList[tmplte ].Add("factorial(<x>)"); // OPTION
  m_wordList[command].Add("factorial_expand"); // OPTION
  m_wordList[command].Add("fib"); // FUNCTION
  m_wordList[tmplte ].Add("fib(<n>)"); // OPTION
  m_wordList[command].Add("fibtophi"); // FUNCTION
  m_wordList[tmplte ].Add("fibtophi(<expr>)"); // OPTION
  m_wordList[command].Add("ifactors"); // FUNCTION
  m_wordList[tmplte ].Add("ifactors(<n>)"); // OPTION
  m_wordList[command].Add("inrt"); // FUNCTION
  m_wordList[tmplte ].Add("inrt(<x>, <n>)"); // OPTION
  m_wordList[command].Add("inv_mod"); // FUNCTION
  m_wordList[tmplte ].Add("inv_mod(<n>, <m>)"); // OPTION
  m_wordList[command].Add("jacobi"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi(<p>, <q>)"); // OPTION
  m_wordList[command].Add("lcm"); // FUNCTION
  m_wordList[tmplte ].Add("lcm(<expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add("minfactorial"); // FUNCTION
  m_wordList[tmplte ].Add("minfactorial(<expr>)"); // OPTION
  m_wordList[command].Add("next_prime"); // FUNCTION
  m_wordList[tmplte ].Add("next_prime(<n>)"); // OPTION
  m_wordList[command].Add("partfrac"); // FUNCTION
  m_wordList[tmplte ].Add("partfrac(<expr>, <var>)"); // OPTION
  m_wordList[command].Add("power_mod"); // FUNCTION
  m_wordList[tmplte ].Add("power_mod(<a>, <n>, <m>)"); // OPTION
  m_wordList[command].Add("primep"); // FUNCTION
  m_wordList[tmplte ].Add("primep(<n>)"); // OPTION
  m_wordList[command].Add("primep_number_of_tests"); // OPTION
  m_wordList[command].Add("prev_prime"); // FUNCTION
  m_wordList[tmplte ].Add("prev_prime(<n>)"); // OPTION
  m_wordList[command].Add("qunit"); // FUNCTION
  m_wordList[tmplte ].Add("qunit(<n>)"); // OPTION
  m_wordList[command].Add("totient"); // FUNCTION
  m_wordList[tmplte ].Add("totient(<n>)"); // OPTION
  m_wordList[command].Add("zerobern"); // OPTION
  m_wordList[command].Add("zeta"); // FUNCTION
  m_wordList[tmplte ].Add("zeta(<n>)"); // OPTION
  m_wordList[command].Add("zeta%pi"); // OPTION
  m_wordList[command].Add("polartorect"); // FUNCTION
  m_wordList[tmplte ].Add("polartorect(<r>, <t>)"); // OPTION
  m_wordList[command].Add("recttopolar"); // FUNCTION
  m_wordList[tmplte ].Add("recttopolar(<a>, <b>)"); // OPTION
  m_wordList[command].Add("inverse_fft"); // FUNCTION
  m_wordList[tmplte ].Add("inverse_fft(<y>)"); // OPTION
  m_wordList[command].Add("fft"); // FUNCTION
  m_wordList[tmplte ].Add("fft(<x>)"); // OPTION
  m_wordList[command].Add("fortindent"); // OPTION
  m_wordList[command].Add("fortran"); // FUNCTION
  m_wordList[tmplte ].Add("fortran(<expr>)"); // OPTION
  m_wordList[command].Add("fortspaces"); // OPTION
  m_wordList[command].Add("horner"); // FUNCTION
  m_wordList[tmplte ].Add("horner(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("horner(<expr>)"); // OPTION
  m_wordList[command].Add("find_root"); // FUNCTION
  m_wordList[tmplte ].Add("find_root(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[tmplte ].Add("find_root(<f>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("newton"); // FUNCTION
  m_wordList[tmplte ].Add("newton(<expr>, <x>, <x_0>, <eps>)"); // OPTION
  m_wordList[command].Add("equalp"); // FUNCTION
  m_wordList[tmplte ].Add("equalp(<x>, <y>)"); // OPTION
  m_wordList[command].Add("remfun"); // FUNCTION
  m_wordList[tmplte ].Add("remfun(<f>, <expr>)"); // OPTION
  m_wordList[tmplte ].Add("remfun(<f>, <expr>, <x>)"); // OPTION
  m_wordList[command].Add("funp"); // FUNCTION
  m_wordList[tmplte ].Add("funp(<f>, <expr>)"); // OPTION
  m_wordList[tmplte ].Add("funp(<f>, <expr>, <x>)"); // OPTION
  m_wordList[command].Add("absint"); // FUNCTION
  m_wordList[tmplte ].Add("absint(<f>, <x>, <halfplane>)"); // OPTION
  m_wordList[tmplte ].Add("absint(<f>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("absint(<f>, <x>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("fourier"); // FUNCTION
  m_wordList[tmplte ].Add("fourier(<f>, <x>, <p>)"); // OPTION
  m_wordList[command].Add("foursimp"); // FUNCTION
  m_wordList[tmplte ].Add("foursimp(<l>)"); // OPTION
  m_wordList[command].Add("sinnpiflag"); // OPTION
  m_wordList[command].Add("cosnpiflag"); // OPTION
  m_wordList[command].Add("fourexpand"); // FUNCTION
  m_wordList[tmplte ].Add("fourexpand(<l>, <x>, <p>, <limit>)"); // OPTION
  m_wordList[command].Add("fourcos"); // FUNCTION
  m_wordList[tmplte ].Add("fourcos(<f>, <x>, <p>)"); // OPTION
  m_wordList[command].Add("foursin"); // FUNCTION
  m_wordList[tmplte ].Add("foursin(<f>, <x>, <p>)"); // OPTION
  m_wordList[command].Add("totalfourier"); // FUNCTION
  m_wordList[tmplte ].Add("totalfourier(<f>, <x>, <p>)"); // OPTION
  m_wordList[command].Add("fourint"); // FUNCTION
  m_wordList[tmplte ].Add("fourint(<f>, <x>)"); // OPTION
  m_wordList[command].Add("fourintcos"); // FUNCTION
  m_wordList[tmplte ].Add("fourintcos(<f>, <x>)"); // OPTION
  m_wordList[command].Add("fourintsin"); // FUNCTION
  m_wordList[tmplte ].Add("fourintsin(<f>, <x>)"); // OPTION
  m_wordList[command].Add("read_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("read_matrix(<S>)"); // OPTION
  m_wordList[tmplte ].Add("read_matrix(<S>, <M>)"); // OPTION
  m_wordList[tmplte ].Add("read_matrix(<S>, <separator_flag>)"); // OPTION
  m_wordList[tmplte ].Add("read_matrix(<S>, <M>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("read_array"); // FUNCTION
  m_wordList[tmplte ].Add("read_array(<S>, <A>)"); // OPTION
  m_wordList[tmplte ].Add("read_array(<S>, <A>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("read_hashed_array"); // FUNCTION
  m_wordList[tmplte ].Add("read_hashed_array(<S>, <A>)"); // OPTION
  m_wordList[tmplte ].Add("read_hashed_array(<S>, <A>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("read_nested_list"); // FUNCTION
  m_wordList[tmplte ].Add("read_nested_list(<S>)"); // OPTION
  m_wordList[tmplte ].Add("read_nested_list(<S>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("read_list"); // FUNCTION
  m_wordList[tmplte ].Add("read_list(<S>)"); // OPTION
  m_wordList[tmplte ].Add("read_list(<S>, <L>)"); // OPTION
  m_wordList[tmplte ].Add("read_list(<S>, <separator_flag>)"); // OPTION
  m_wordList[tmplte ].Add("read_list(<S>, <L>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("write_data"); // FUNCTION
  m_wordList[tmplte ].Add("write_data(<X>, <D>)"); // OPTION
  m_wordList[tmplte ].Add("write_data(<X>, <D>, <separator_flag>)"); // OPTION
  m_wordList[command].Add("assume_external_byte_order"); // FUNCTION
  m_wordList[tmplte ].Add("assume_external_byte_order(<byte_order_flag>)"); // OPTION
  m_wordList[command].Add("openr_binary"); // FUNCTION
  m_wordList[tmplte ].Add("openr_binary(<file_name>)"); // OPTION
  m_wordList[command].Add("openw_binary"); // FUNCTION
  m_wordList[tmplte ].Add("openw_binary(<file_name>)"); // OPTION
  m_wordList[command].Add("opena_binary"); // FUNCTION
  m_wordList[tmplte ].Add("opena_binary(<file_name>)"); // OPTION
  m_wordList[command].Add("read_binary_matrix"); // FUNCTION
  m_wordList[tmplte ].Add("read_binary_matrix(<S>, <M>)"); // OPTION
  m_wordList[command].Add("read_binary_array"); // FUNCTION
  m_wordList[tmplte ].Add("read_binary_array(<S>, <A>)"); // OPTION
  m_wordList[command].Add("read_binary_list"); // FUNCTION
  m_wordList[tmplte ].Add("read_binary_list(<S>)"); // OPTION
  m_wordList[tmplte ].Add("read_binary_list(<S>, <L>)"); // OPTION
  m_wordList[command].Add("write_binary_data"); // FUNCTION
  m_wordList[tmplte ].Add("write_binary_data(<X>, <D>)"); // OPTION
  m_wordList[command].Add("abs"); // FUNCTION
  m_wordList[tmplte ].Add("abs(<expr>)"); // OPTION
  m_wordList[command].Add("additive"); // OPTION
  m_wordList[command].Add("allbut"); // OPTION
  m_wordList[command].Add("antisymmetric"); // OPTION
  m_wordList[command].Add("cabs"); // FUNCTION
  m_wordList[tmplte ].Add("cabs(<expr>)"); // OPTION
  m_wordList[command].Add("ceiling"); // FUNCTION
  m_wordList[tmplte ].Add("ceiling(<x>)"); // OPTION
  m_wordList[command].Add("charfun"); // FUNCTION
  m_wordList[tmplte ].Add("charfun(<p>)"); // OPTION
  m_wordList[command].Add("commutative"); // OPTION
  m_wordList[command].Add("compare"); // FUNCTION
  m_wordList[tmplte ].Add("compare(<x>, <y>)"); // OPTION
  m_wordList[command].Add("entier"); // FUNCTION
  m_wordList[tmplte ].Add("entier(<x>)"); // OPTION
  m_wordList[command].Add("equal"); // FUNCTION
  m_wordList[tmplte ].Add("equal(<a>, <b>)"); // OPTION
  m_wordList[command].Add("floor"); // FUNCTION
  m_wordList[tmplte ].Add("floor(<x>)"); // OPTION
  m_wordList[command].Add("notequal"); // FUNCTION
  m_wordList[tmplte ].Add("notequal(<a>, <b>)"); // OPTION
  m_wordList[command].Add("evenp"); // FUNCTION
  m_wordList[tmplte ].Add("evenp(<expr>)"); // OPTION
  m_wordList[command].Add("fix"); // FUNCTION
  m_wordList[tmplte ].Add("fix(<x>)"); // OPTION
  m_wordList[command].Add("fullmap"); // FUNCTION
  m_wordList[tmplte ].Add("fullmap(<f>, <expr_1>, <...>)"); // OPTION
  m_wordList[command].Add("fullmapl"); // FUNCTION
  m_wordList[tmplte ].Add("fullmapl(<f>, <list_1>, <...>)"); // OPTION
  m_wordList[command].Add("is"); // FUNCTION
  m_wordList[tmplte ].Add("is(<expr>)"); // OPTION
  m_wordList[command].Add("maybe"); // FUNCTION
  m_wordList[tmplte ].Add("maybe(<expr>)"); // OPTION
  m_wordList[command].Add("isqrt"); // FUNCTION
  m_wordList[tmplte ].Add("isqrt(<x>)"); // OPTION
  m_wordList[command].Add("lmax"); // FUNCTION
  m_wordList[tmplte ].Add("lmax(<L>)"); // OPTION
  m_wordList[command].Add("lmin"); // FUNCTION
  m_wordList[tmplte ].Add("lmin(<L>)"); // OPTION
  m_wordList[command].Add("max"); // FUNCTION
  m_wordList[tmplte ].Add("max(<x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("min"); // FUNCTION
  m_wordList[tmplte ].Add("min(<x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("polymod"); // FUNCTION
  m_wordList[tmplte ].Add("polymod(<p>)"); // OPTION
  m_wordList[tmplte ].Add("polymod(<p>, <m>)"); // OPTION
  m_wordList[command].Add("mod"); // FUNCTION
  m_wordList[tmplte ].Add("mod(<x>, <y>)"); // OPTION
  m_wordList[command].Add("oddp"); // FUNCTION
  m_wordList[tmplte ].Add("oddp(<expr>)"); // OPTION
  m_wordList[command].Add("psubst"); // FUNCTION
  m_wordList[tmplte ].Add("psubst(<list>, <expr>)"); // OPTION
  m_wordList[tmplte ].Add("psubst(<a>, <b>, <expr>)"); // OPTION
  m_wordList[command].Add("make_random_state"); // FUNCTION
  m_wordList[tmplte ].Add("make_random_state(<n>)"); // OPTION
  m_wordList[tmplte ].Add("make_random_state(<s>)"); // OPTION
  m_wordList[command].Add("set_random_state"); // FUNCTION
  m_wordList[tmplte ].Add("set_random_state(<s>)"); // OPTION
  m_wordList[command].Add("random"); // FUNCTION
  m_wordList[tmplte ].Add("random(<x>)"); // OPTION
  m_wordList[command].Add("rationalize"); // FUNCTION
  m_wordList[tmplte ].Add("rationalize(<expr>)"); // OPTION
  m_wordList[command].Add("round"); // FUNCTION
  m_wordList[tmplte ].Add("round(<x>)"); // OPTION
  m_wordList[command].Add("sign"); // FUNCTION
  m_wordList[tmplte ].Add("sign(<expr>)"); // OPTION
  m_wordList[command].Add("signum"); // FUNCTION
  m_wordList[tmplte ].Add("signum(<x>)"); // OPTION
  m_wordList[command].Add("sort"); // FUNCTION
  m_wordList[tmplte ].Add("sort(<L>, <P>)"); // OPTION
  m_wordList[tmplte ].Add("sort(<L>)"); // OPTION
  m_wordList[command].Add("sqrt"); // FUNCTION
  m_wordList[tmplte ].Add("sqrt(<x>)"); // OPTION
  m_wordList[command].Add("sqrtdispflag"); // OPTION
  m_wordList[command].Add("sublis"); // FUNCTION
  m_wordList[tmplte ].Add("sublis(<list>, <expr>)"); // OPTION
  m_wordList[command].Add("sublist"); // FUNCTION
  m_wordList[tmplte ].Add("sublist(<list>, <p>)"); // OPTION
  m_wordList[command].Add("sublis_apply_lambda"); // OPTION
  m_wordList[command].Add("subst"); // FUNCTION
  m_wordList[tmplte ].Add("subst(<a>, <b>, <c>)"); // OPTION
  m_wordList[command].Add("substinpart"); // FUNCTION
  m_wordList[tmplte ].Add("substinpart(<x>, <expr>, <n_1>, <n_k>)"); // OPTION
  m_wordList[command].Add("substpart"); // FUNCTION
  m_wordList[tmplte ].Add("substpart(<x>, <expr>, <n_1>, <n_k>)"); // OPTION
  m_wordList[command].Add("subvarp"); // FUNCTION
  m_wordList[tmplte ].Add("subvarp(<expr>)"); // OPTION
  m_wordList[command].Add("symbolp"); // FUNCTION
  m_wordList[tmplte ].Add("symbolp(<expr>)"); // OPTION
  m_wordList[command].Add("vectorpotential"); // FUNCTION
  m_wordList[tmplte ].Add("vectorpotential(<givencurl>)"); // OPTION
  m_wordList[command].Add("xthru"); // FUNCTION
  m_wordList[tmplte ].Add("xthru(<expr>)"); // OPTION
  m_wordList[command].Add("zeroequiv"); // FUNCTION
  m_wordList[tmplte ].Add("zeroequiv(<expr>, <v>)"); // OPTION
  m_wordList[command].Add("opsubst"); // FUNCTION
  m_wordList[tmplte ].Add("opsubst(<f>,<g>,<e>)"); // OPTION
  m_wordList[tmplte ].Add("opsubst(<g>=<f>,<e>)"); // OPTION
  m_wordList[tmplte ].Add("opsubst([<g1>=<f1>,<g2>=<f2>,<gn>=<fn>],<e>)"); // OPTION
  m_wordList[command].Add("assoc_legendre_p"); // FUNCTION
  m_wordList[tmplte ].Add("assoc_legendre_p(<n>, <m>, <x>)"); // OPTION
  m_wordList[command].Add("assoc_legendre_q"); // FUNCTION
  m_wordList[tmplte ].Add("assoc_legendre_q(<n>, <m>, <x>)"); // OPTION
  m_wordList[command].Add("chebyshev_t"); // FUNCTION
  m_wordList[tmplte ].Add("chebyshev_t(<n>, <x>)"); // OPTION
  m_wordList[command].Add("chebyshev_u"); // FUNCTION
  m_wordList[tmplte ].Add("chebyshev_u(<n>, <x>)"); // OPTION
  m_wordList[command].Add("gen_laguerre"); // FUNCTION
  m_wordList[tmplte ].Add("gen_laguerre(<n>, <a>, <x>)"); // OPTION
  m_wordList[command].Add("hermite"); // FUNCTION
  m_wordList[tmplte ].Add("hermite(<n>, <x>)"); // OPTION
  m_wordList[command].Add("intervalp"); // FUNCTION
  m_wordList[tmplte ].Add("intervalp(<e>)"); // OPTION
  m_wordList[command].Add("jacobi_p"); // FUNCTION
  m_wordList[tmplte ].Add("jacobi_p(<n>, <a>, <b>, <x>)"); // OPTION
  m_wordList[command].Add("laguerre"); // FUNCTION
  m_wordList[tmplte ].Add("laguerre(<n>, <x>)"); // OPTION
  m_wordList[command].Add("legendre_p"); // FUNCTION
  m_wordList[tmplte ].Add("legendre_p(<n>, <x>)"); // OPTION
  m_wordList[command].Add("legendre_q"); // FUNCTION
  m_wordList[tmplte ].Add("legendre_q(<n>, <x>)"); // OPTION
  m_wordList[command].Add("orthopoly_recur"); // FUNCTION
  m_wordList[tmplte ].Add("orthopoly_recur(<f>, <args>)"); // OPTION
  m_wordList[command].Add("orthopoly_returns_intervals"); // OPTION
  m_wordList[command].Add("orthopoly_weight"); // FUNCTION
  m_wordList[tmplte ].Add("orthopoly_weight(<f>, <args>)"); // OPTION
  m_wordList[command].Add("pochhammer"); // FUNCTION
  m_wordList[tmplte ].Add("pochhammer(<n>, <x>)"); // OPTION
  m_wordList[command].Add("pochhammer_max_index"); // OPTION
  m_wordList[command].Add("spherical_bessel_j"); // FUNCTION
  m_wordList[tmplte ].Add("spherical_bessel_j(<n>, <x>)"); // OPTION
  m_wordList[command].Add("spherical_bessel_y"); // FUNCTION
  m_wordList[tmplte ].Add("spherical_bessel_y(<n>, <x>)"); // OPTION
  m_wordList[command].Add("spherical_hankel1"); // FUNCTION
  m_wordList[tmplte ].Add("spherical_hankel1(<n>, <x>)"); // OPTION
  m_wordList[command].Add("spherical_hankel2"); // FUNCTION
  m_wordList[tmplte ].Add("spherical_hankel2(<n>, <x>)"); // OPTION
  m_wordList[command].Add("spherical_harmonic"); // FUNCTION
  m_wordList[tmplte ].Add("spherical_harmonic(<n>, <m>, <x>, <y>)"); // OPTION
  m_wordList[command].Add("unit_step"); // FUNCTION
  m_wordList[tmplte ].Add("unit_step(<x>)"); // OPTION
  m_wordList[command].Add("ultraspherical"); // FUNCTION
  m_wordList[tmplte ].Add("ultraspherical(<n>, <a>, <x>)"); // OPTION
  m_wordList[command].Add("plotdf"); // FUNCTION
  m_wordList[tmplte ].Add("plotdf(<dydx>, <options>)"); // OPTION
  m_wordList[tmplte ].Add("plotdf(<dvdu>, [<u>,<v>], <options>)"); // OPTION
  m_wordList[tmplte ].Add("plotdf([<dxdt>,<dydt>], <options>)"); // OPTION
  m_wordList[tmplte ].Add("plotdf([<dudt>,<dvdt>], [<u>,<v>], <options>)"); // OPTION
  m_wordList[command].Add("contour_plot"); // FUNCTION
  m_wordList[tmplte ].Add("contour_plot(<expr>, <x_range>, <y_range>, <options>)"); // OPTION
  m_wordList[command].Add("get_plot_option"); // FUNCTION
  m_wordList[tmplte ].Add("get_plot_option(<keyword>, <index>)"); // OPTION
  m_wordList[command].Add("make_transform"); // FUNCTION
  m_wordList[tmplte ].Add("make_transform([<var1>, <var2>, <var3>], <fx>, <fy>, <fz>)"); // OPTION
  m_wordList[command].Add("plot2d"); // FUNCTION
  m_wordList[tmplte ].Add("plot2d(<plot>, <x_range>, <[options]>)"); // OPTION
  m_wordList[tmplte ].Add("plot2d([<plot_1>, <plot_n>], <options>)"); // OPTION
  m_wordList[tmplte ].Add("plot2d([<plot_1>, <plot_n>], <x_range>, <[options]>)"); // OPTION
  m_wordList[command].Add("plot3d"); // FUNCTION
  m_wordList[tmplte ].Add("plot3d(<expr>, <x_range>, <y_range>, <[options]>)"); // OPTION
  m_wordList[tmplte ].Add("plot3d([<expr_1>, <...>, <expr_n>], <x_range>, <y_range>, <[options]>)"); // OPTION
  m_wordList[command].Add("plot_options"); // OPTION
  m_wordList[command].Add("set_plot_option"); // FUNCTION
  m_wordList[tmplte ].Add("set_plot_option(<option>)"); // OPTION
  m_wordList[command].Add("adapth_depth"); // OPTION
  m_wordList[command].Add("axes"); // OPTION
  m_wordList[command].Add("azimut"); // OPTION
  m_wordList[command].Add("box"); // OPTION
  m_wordList[command].Add("color"); // OPTION
  m_wordList[command].Add("colorbox"); // OPTION
  m_wordList[command].Add("elevation"); // OPTION
  m_wordList[command].Add("grid"); // OPTION
  m_wordList[command].Add("legend"); // OPTION
  m_wordList[command].Add("logx"); // OPTION
  m_wordList[command].Add("logy"); // OPTION
  m_wordList[command].Add("mesh_lines_color"); // OPTION
  m_wordList[command].Add("nticks"); // OPTION
  m_wordList[command].Add("palette"); // OPTION
  m_wordList[command].Add("plot_format"); // OPTION
  m_wordList[command].Add("plot_real_part"); // OPTION
  m_wordList[command].Add("point_type"); // OPTION
  m_wordList[command].Add("psfile"); // OPTION
  m_wordList[command].Add("run_viewer"); // OPTION
  m_wordList[command].Add("style"); // OPTION
  m_wordList[command].Add("t"); // OPTION
  m_wordList[command].Add("transform_xy"); // OPTION
  m_wordList[command].Add("x"); // OPTION
  m_wordList[command].Add("xlabel"); // OPTION
  m_wordList[command].Add("y"); // OPTION
  m_wordList[command].Add("ylabel"); // OPTION
  m_wordList[command].Add("z"); // OPTION
  m_wordList[command].Add("zlabel"); // OPTION
  m_wordList[command].Add("gnuplot_term"); // OPTION
  m_wordList[command].Add("gnuplot_out_file"); // OPTION
  m_wordList[command].Add("gnuplot_pm3d"); // OPTION
  m_wordList[command].Add("gnuplot_preamble"); // OPTION
  m_wordList[command].Add("gnuplot_curve_titles"); // OPTION
  m_wordList[command].Add("gnuplot_curve_styles"); // OPTION
  m_wordList[command].Add("gnuplot_default_term_command"); // OPTION
  m_wordList[command].Add("gnuplot_dumb_term_command"); // OPTION
  m_wordList[command].Add("gnuplot_ps_term_command"); // OPTION
  m_wordList[command].Add("gnuplot_start"); // FUNCTION
  m_wordList[tmplte ].Add("gnuplot_start()"); // OPTION
  m_wordList[command].Add("gnuplot_close"); // FUNCTION
  m_wordList[tmplte ].Add("gnuplot_close()"); // OPTION
  m_wordList[command].Add("gnuplot_restart"); // FUNCTION
  m_wordList[tmplte ].Add("gnuplot_restart()"); // OPTION
  m_wordList[command].Add("gnuplot_replot"); // FUNCTION
  m_wordList[tmplte ].Add("gnuplot_replot()"); // OPTION
  m_wordList[tmplte ].Add("gnuplot_replot(<s>)"); // OPTION
  m_wordList[command].Add("gnuplot_reset"); // FUNCTION
  m_wordList[tmplte ].Add("gnuplot_reset()"); // OPTION
  m_wordList[command].Add("algebraic"); // OPTION
  m_wordList[command].Add("berlefact"); // OPTION
  m_wordList[command].Add("bezout"); // FUNCTION
  m_wordList[tmplte ].Add("bezout(<p1>, <p2>, <x>)"); // OPTION
  m_wordList[command].Add("bothcoef"); // FUNCTION
  m_wordList[tmplte ].Add("bothcoef(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("coeff"); // FUNCTION
  m_wordList[tmplte ].Add("coeff(<expr>, <x>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("coeff(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("combine"); // FUNCTION
  m_wordList[tmplte ].Add("combine(<expr>)"); // OPTION
  m_wordList[command].Add("content"); // FUNCTION
  m_wordList[tmplte ].Add("content(<p_1>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("denom"); // FUNCTION
  m_wordList[tmplte ].Add("denom(<expr>)"); // OPTION
  m_wordList[command].Add("divide"); // FUNCTION
  m_wordList[tmplte ].Add("divide(<p_1>, <p_2>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("eliminate"); // FUNCTION
  m_wordList[tmplte ].Add("eliminate([<eqn_1>, <...>, <eqn_n>], [<x_1>, <...>, <x_k>])"); // OPTION
  m_wordList[command].Add("ezgcd"); // FUNCTION
  m_wordList[tmplte ].Add("ezgcd(<p_1>, <p_2>, <p_3>, ...)"); // OPTION
  m_wordList[command].Add("facexpand"); // OPTION
  m_wordList[command].Add("factcomb"); // FUNCTION
  m_wordList[tmplte ].Add("factcomb(<expr>)"); // OPTION
  m_wordList[command].Add("factor"); // FUNCTION
  m_wordList[tmplte ].Add("factor(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("factor(<expr>, <p>)"); // OPTION
  m_wordList[command].Add("factorflag"); // OPTION
  m_wordList[command].Add("factorout"); // FUNCTION
  m_wordList[tmplte ].Add("factorout(<expr>, <x_1>, <x_2>, <...>)"); // OPTION
  m_wordList[command].Add("factorsum"); // FUNCTION
  m_wordList[tmplte ].Add("factorsum(<expr>)"); // OPTION
  m_wordList[command].Add("fasttimes"); // FUNCTION
  m_wordList[tmplte ].Add("fasttimes(<p_1>, <p_2>)"); // OPTION
  m_wordList[command].Add("fullratsimp"); // FUNCTION
  m_wordList[tmplte ].Add("fullratsimp(<expr>)"); // OPTION
  m_wordList[command].Add("fullratsubst"); // FUNCTION
  m_wordList[tmplte ].Add("fullratsubst(<a>, <b>, <c>)"); // OPTION
  m_wordList[command].Add("gcd"); // FUNCTION
  m_wordList[tmplte ].Add("gcd(<p_1>, <p_2>, <x_1>, <...>)"); // OPTION
  m_wordList[command].Add("gcdex"); // FUNCTION
  m_wordList[tmplte ].Add("gcdex(<f>, <g>)"); // OPTION
  m_wordList[tmplte ].Add("gcdex(<f>, <g>, <x>)"); // OPTION
  m_wordList[command].Add("gcfactor"); // FUNCTION
  m_wordList[tmplte ].Add("gcfactor(<n>)"); // OPTION
  m_wordList[command].Add("gfactor"); // FUNCTION
  m_wordList[tmplte ].Add("gfactor(<expr>)"); // OPTION
  m_wordList[command].Add("gfactorsum"); // FUNCTION
  m_wordList[tmplte ].Add("gfactorsum(<expr>)"); // OPTION
  m_wordList[command].Add("hipow"); // FUNCTION
  m_wordList[tmplte ].Add("hipow(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("intfaclim"); // OPTION
  m_wordList[command].Add("keepfloat"); // OPTION
  m_wordList[command].Add("lratsubst"); // FUNCTION
  m_wordList[tmplte ].Add("lratsubst(<L>, <expr>)"); // OPTION
  m_wordList[command].Add("modulus"); // OPTION
  m_wordList[command].Add("num"); // FUNCTION
  m_wordList[tmplte ].Add("num(<expr>)"); // OPTION
  m_wordList[command].Add("polydecomp"); // FUNCTION
  m_wordList[tmplte ].Add("polydecomp(<p>, <x>)"); // OPTION
  m_wordList[command].Add("quotient"); // FUNCTION
  m_wordList[tmplte ].Add("quotient(<p_1>, <p_2>)"); // OPTION
  m_wordList[tmplte ].Add("quotient(<p_1>, <p_2>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("rat"); // FUNCTION
  m_wordList[tmplte ].Add("rat(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("rat(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("ratalgdenom"); // OPTION
  m_wordList[command].Add("ratcoef"); // FUNCTION
  m_wordList[tmplte ].Add("ratcoef(<expr>, <x>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("ratcoef(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("ratdenom"); // FUNCTION
  m_wordList[tmplte ].Add("ratdenom(<expr>)"); // OPTION
  m_wordList[command].Add("ratdenomdivide"); // OPTION
  m_wordList[command].Add("ratdiff"); // FUNCTION
  m_wordList[tmplte ].Add("ratdiff(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("ratdisrep"); // FUNCTION
  m_wordList[tmplte ].Add("ratdisrep(<expr>)"); // OPTION
  m_wordList[command].Add("ratepsilon"); // OPTION
  m_wordList[command].Add("ratexpand"); // FUNCTION
  m_wordList[tmplte ].Add("ratexpand(<expr>)"); // OPTION
  m_wordList[command].Add("ratfac"); // OPTION
  m_wordList[command].Add("ratnumer"); // FUNCTION
  m_wordList[tmplte ].Add("ratnumer(<expr>)"); // OPTION
  m_wordList[command].Add("ratnump"); // FUNCTION
  m_wordList[tmplte ].Add("ratnump(<expr>)"); // OPTION
  m_wordList[command].Add("ratp"); // FUNCTION
  m_wordList[tmplte ].Add("ratp(<expr>)"); // OPTION
  m_wordList[command].Add("ratprint"); // OPTION
  m_wordList[command].Add("float_approx_equal_tolerance"); // OPTION
  m_wordList[command].Add("ratsimp"); // FUNCTION
  m_wordList[tmplte ].Add("ratsimp(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("ratsimp(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("ratsimpexpons"); // OPTION
  m_wordList[command].Add("ratsubst"); // FUNCTION
  m_wordList[tmplte ].Add("ratsubst(<a>, <b>, <c>)"); // OPTION
  m_wordList[command].Add("ratvars"); // FUNCTION
  m_wordList[tmplte ].Add("ratvars(<x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[tmplte ].Add("ratvars()"); // OPTION
  m_wordList[command].Add("ratweight"); // FUNCTION
  m_wordList[tmplte ].Add("ratweight(<x_1>, <w_1>, <...>, <x_n>, <w_n>)"); // OPTION
  m_wordList[tmplte ].Add("ratweight()"); // OPTION
  m_wordList[command].Add("ratweights"); // OPTION
  m_wordList[command].Add("ratwtlvl"); // OPTION
  m_wordList[command].Add("remainder"); // FUNCTION
  m_wordList[tmplte ].Add("remainder(<p_1>, <p_2>)"); // OPTION
  m_wordList[tmplte ].Add("remainder(<p_1>, <p_2>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("resultant"); // FUNCTION
  m_wordList[tmplte ].Add("resultant(<p_1>, <p_2>, <x>)"); // OPTION
  m_wordList[command].Add("savefactors"); // OPTION
  m_wordList[command].Add("sqfr"); // FUNCTION
  m_wordList[tmplte ].Add("sqfr(<expr>)"); // OPTION
  m_wordList[command].Add("tellrat"); // FUNCTION
  m_wordList[tmplte ].Add("tellrat(<p_1>, <...>, <p_n>)"); // OPTION
  m_wordList[tmplte ].Add("tellrat()"); // OPTION
  m_wordList[command].Add("totaldisrep"); // FUNCTION
  m_wordList[tmplte ].Add("totaldisrep(<expr>)"); // OPTION
  m_wordList[command].Add("untellrat"); // FUNCTION
  m_wordList[tmplte ].Add("untellrat(<x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("backtrace"); // FUNCTION
  m_wordList[tmplte ].Add("backtrace()"); // OPTION
  m_wordList[tmplte ].Add("backtrace(<n>)"); // OPTION
  m_wordList[command].Add("errcatch"); // FUNCTION
  m_wordList[tmplte ].Add("errcatch(<expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("error"); // FUNCTION
  m_wordList[tmplte ].Add("error(<expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("errormsg"); // FUNCTION
  m_wordList[tmplte ].Add("errormsg()"); // OPTION
  m_wordList[command].Add("errormsg"); // OPTION
  m_wordList[command].Add("go"); // FUNCTION
  m_wordList[tmplte ].Add("go(<tag>)"); // OPTION
  m_wordList[command].Add("map"); // FUNCTION
  m_wordList[tmplte ].Add("map(<f>, <expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("mapatom"); // FUNCTION
  m_wordList[tmplte ].Add("mapatom(<expr>)"); // OPTION
  m_wordList[command].Add("maperror"); // OPTION
  m_wordList[command].Add("mapprint"); // OPTION
  m_wordList[command].Add("maplist"); // FUNCTION
  m_wordList[tmplte ].Add("maplist(<f>, <expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("prederror"); // OPTION
  m_wordList[command].Add("return"); // FUNCTION
  m_wordList[tmplte ].Add("return(<value>)"); // OPTION
  m_wordList[command].Add("scanmap"); // FUNCTION
  m_wordList[tmplte ].Add("scanmap(<f>, <expr>)"); // OPTION
  m_wordList[tmplte ].Add("scanmap(<f>, <expr>, bottomup)"); // OPTION
  m_wordList[command].Add("throw"); // FUNCTION
  m_wordList[tmplte ].Add("throw(<expr>)"); // OPTION
  m_wordList[command].Add("outermap"); // FUNCTION
  m_wordList[tmplte ].Add("outermap(<f>, <a_1>, <...>, <a_n>)"); // OPTION
  m_wordList[command].Add("romberg"); // FUNCTION
  m_wordList[tmplte ].Add("romberg(<expr>, <x>, <a>, <b>)"); // OPTION
  m_wordList[tmplte ].Add("romberg(<F>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("rombergabs"); // OPTION
  m_wordList[command].Add("rombergit"); // OPTION
  m_wordList[command].Add("rombergmin"); // OPTION
  m_wordList[command].Add("rombergtol"); // OPTION
  m_wordList[command].Add("apply1"); // FUNCTION
  m_wordList[tmplte ].Add("apply1(<expr>, <rule_1>, <...>, <rule_n>)"); // OPTION
  m_wordList[command].Add("apply2"); // FUNCTION
  m_wordList[tmplte ].Add("apply2(<expr>, <rule_1>, <...>, <rule_n>)"); // OPTION
  m_wordList[command].Add("applyb1"); // FUNCTION
  m_wordList[tmplte ].Add("applyb1(<expr>, <rule_1>, <...>, <rule_n>)"); // OPTION
  m_wordList[command].Add("current_let_rule_package"); // OPTION
  m_wordList[command].Add("default_let_rule_package"); // OPTION
  m_wordList[command].Add("defmatch"); // FUNCTION
  m_wordList[tmplte ].Add("defmatch(<progname>, <pattern>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[tmplte ].Add("defmatch(<progname>, <pattern>)"); // OPTION
  m_wordList[command].Add("defrule"); // FUNCTION
  m_wordList[tmplte ].Add("defrule(<rulename>, <pattern>, <replacement>)"); // OPTION
  m_wordList[command].Add("disprule"); // FUNCTION
  m_wordList[tmplte ].Add("disprule(<rulename_1>, <...>, <rulename_2>)"); // OPTION
  m_wordList[tmplte ].Add("disprule(all)"); // OPTION
  m_wordList[command].Add("let"); // FUNCTION
  m_wordList[tmplte ].Add("let(<prod>, <repl>, <predname>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[tmplte ].Add("let([<prod>, <repl>, <predname>, <arg_1>, <...>, <arg_n>], <package_name>)"); // OPTION
  m_wordList[command].Add("letrat"); // OPTION
  m_wordList[command].Add("letrules"); // FUNCTION
  m_wordList[tmplte ].Add("letrules()"); // OPTION
  m_wordList[tmplte ].Add("letrules(<package_name>)"); // OPTION
  m_wordList[command].Add("letsimp"); // FUNCTION
  m_wordList[tmplte ].Add("letsimp(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("letsimp(<expr>, <package_name>)"); // OPTION
  m_wordList[tmplte ].Add("letsimp(<expr>, <package_name_1>, <...>, <package_name_n>)"); // OPTION
  m_wordList[command].Add("let_rule_packages"); // OPTION
  m_wordList[command].Add("matchdeclare"); // FUNCTION
  m_wordList[tmplte ].Add("matchdeclare(<a_1>, <pred_1>, <...>, <a_n>, <pred_n>)"); // OPTION
  m_wordList[command].Add("matchfix"); // FUNCTION
  m_wordList[tmplte ].Add("matchfix(<ldelimiter>, <rdelimiter>)"); // OPTION
  m_wordList[tmplte ].Add("matchfix(<ldelimiter>, <rdelimiter>, <arg_pos>, <pos>)"); // OPTION
  m_wordList[command].Add("remlet"); // FUNCTION
  m_wordList[tmplte ].Add("remlet(<prod>, <name>)"); // OPTION
  m_wordList[tmplte ].Add("remlet()"); // OPTION
  m_wordList[tmplte ].Add("remlet(all)"); // OPTION
  m_wordList[tmplte ].Add("remlet(all, <name>)"); // OPTION
  m_wordList[command].Add("remrule"); // FUNCTION
  m_wordList[tmplte ].Add("remrule(<op>, <rulename>)"); // OPTION
  m_wordList[tmplte ].Add("remrule(<op>, all)"); // OPTION
  m_wordList[command].Add("tellsimp"); // FUNCTION
  m_wordList[tmplte ].Add("tellsimp(<pattern>, <replacement>)"); // OPTION
  m_wordList[command].Add("tellsimpafter"); // FUNCTION
  m_wordList[tmplte ].Add("tellsimpafter(<pattern>, <replacement>)"); // OPTION
  m_wordList[command].Add("clear_rules"); // FUNCTION
  m_wordList[tmplte ].Add("clear_rules()"); // OPTION
  m_wordList[command].Add("feature"); // OPTION
  m_wordList[command].Add("featurep"); // FUNCTION
  m_wordList[tmplte ].Add("featurep(<a>, <f>)"); // OPTION
  m_wordList[command].Add("maxima_tempdir"); // OPTION
  m_wordList[command].Add("maxima_userdir"); // OPTION
  m_wordList[command].Add("room"); // FUNCTION
  m_wordList[tmplte ].Add("room()"); // OPTION
  m_wordList[tmplte ].Add("room(true)"); // OPTION
  m_wordList[tmplte ].Add("room(false)"); // OPTION
  m_wordList[command].Add("rules"); // OPTION
  m_wordList[command].Add("sstatus"); // FUNCTION
  m_wordList[tmplte ].Add("sstatus(<keyword>, <item>)"); // OPTION
  m_wordList[command].Add("status"); // FUNCTION
  m_wordList[tmplte ].Add("status(<feature>)"); // OPTION
  m_wordList[tmplte ].Add("status(<feature>, <item>)"); // OPTION
  m_wordList[command].Add("time"); // FUNCTION
  m_wordList[tmplte ].Add("time(<%o1>, <%o2>, <%o3>, <...>)"); // OPTION
  m_wordList[command].Add("timedate"); // FUNCTION
  m_wordList[tmplte ].Add("timedate()"); // OPTION
  m_wordList[tmplte ].Add("timedate(<T>)"); // OPTION
  m_wordList[command].Add("absolute_real_time"); // FUNCTION
  m_wordList[tmplte ].Add("absolute_real_time()"); // OPTION
  m_wordList[command].Add("elapsed_real_time"); // FUNCTION
  m_wordList[tmplte ].Add("elapsed_real_time()"); // OPTION
  m_wordList[command].Add("elapsed_run_time"); // FUNCTION
  m_wordList[tmplte ].Add("elapsed_run_time()"); // OPTION
  m_wordList[command].Add("cauchysum"); // OPTION
  m_wordList[command].Add("deftaylor"); // FUNCTION
  m_wordList[tmplte ].Add("deftaylor(<f_1>(<x_1>), <expr_1>, <...>, <f_n>(<x_n>), <expr_n>)"); // OPTION
  m_wordList[command].Add("maxtayorder"); // OPTION
  m_wordList[command].Add("niceindices"); // FUNCTION
  m_wordList[tmplte ].Add("niceindices(<expr>)"); // OPTION
  m_wordList[command].Add("niceindicespref"); // OPTION
  m_wordList[command].Add("nusum"); // FUNCTION
  m_wordList[tmplte ].Add("nusum(<expr>, <x>, <i_0>, <i_1>)"); // OPTION
  m_wordList[command].Add("pade"); // FUNCTION
  m_wordList[tmplte ].Add("pade(<taylor_series>, <numer_deg_bound>, <denom_deg_bound>)"); // OPTION
  m_wordList[command].Add("powerdisp"); // OPTION
  m_wordList[command].Add("powerseries"); // FUNCTION
  m_wordList[tmplte ].Add("powerseries(<expr>, <x>, <a>)"); // OPTION
  m_wordList[command].Add("psexpand"); // OPTION
  m_wordList[command].Add("revert"); // FUNCTION
  m_wordList[tmplte ].Add("revert(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("revert2(<expr>, <x>, <n>)"); // OPTION
  m_wordList[command].Add("taylor"); // FUNCTION
  m_wordList[tmplte ].Add("taylor(<expr>, <x>, <a>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("taylor(<expr>, [<x_1>, <x_2>, <...>], <a>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("taylor(<expr>, [<x>, <a>, <n>, 'asymp])"); // OPTION
  m_wordList[tmplte ].Add("taylor(<expr>, [<x_1>, <x_2>, <...>], [<a_1>, <a_2>, <...>], [<n_1>, <n_2>, <...>])"); // OPTION
  m_wordList[tmplte ].Add("taylor(<expr>, [<x_1>, <a_1>, <n_1>], [<x_2>, <a_2>, <n_2>], <...>)"); // OPTION
  m_wordList[command].Add("taylordepth"); // OPTION
  m_wordList[command].Add("taylorinfo"); // FUNCTION
  m_wordList[tmplte ].Add("taylorinfo(<expr>)"); // OPTION
  m_wordList[command].Add("taylorp"); // FUNCTION
  m_wordList[tmplte ].Add("taylorp(<expr>)"); // OPTION
  m_wordList[command].Add("taylor_logexpand"); // OPTION
  m_wordList[command].Add("taylor_order_coefficients"); // OPTION
  m_wordList[command].Add("taylor_simplifier"); // FUNCTION
  m_wordList[tmplte ].Add("taylor_simplifier(<expr>)"); // OPTION
  m_wordList[command].Add("taylor_truncate_polynomials"); // OPTION
  m_wordList[command].Add("taytorat"); // FUNCTION
  m_wordList[tmplte ].Add("taytorat(<expr>)"); // OPTION
  m_wordList[command].Add("trunc"); // FUNCTION
  m_wordList[tmplte ].Add("trunc(<expr>)"); // OPTION
  m_wordList[command].Add("unsum"); // FUNCTION
  m_wordList[tmplte ].Add("unsum(<f>, <n>)"); // OPTION
  m_wordList[command].Add("verbose"); // OPTION
  m_wordList[command].Add("intopois"); // FUNCTION
  m_wordList[tmplte ].Add("intopois(<a>)"); // OPTION
  m_wordList[command].Add("outofpois"); // FUNCTION
  m_wordList[tmplte ].Add("outofpois(<a>)"); // OPTION
  m_wordList[command].Add("poisdiff"); // FUNCTION
  m_wordList[tmplte ].Add("poisdiff(<a>, <b>)"); // OPTION
  m_wordList[command].Add("poisexpt"); // FUNCTION
  m_wordList[tmplte ].Add("poisexpt(<a>, <b>)"); // OPTION
  m_wordList[command].Add("poisint"); // FUNCTION
  m_wordList[tmplte ].Add("poisint(<a>, <b>)"); // OPTION
  m_wordList[command].Add("poislim"); // OPTION
  m_wordList[command].Add("poismap"); // FUNCTION
  m_wordList[tmplte ].Add("poismap(<series>, <sinfn>, <cosfn>)"); // OPTION
  m_wordList[command].Add("poisplus"); // FUNCTION
  m_wordList[tmplte ].Add("poisplus(<a>, <b>)"); // OPTION
  m_wordList[command].Add("poissimp"); // FUNCTION
  m_wordList[tmplte ].Add("poissimp(<a>)"); // OPTION
  m_wordList[command].Add("poisson"); // OPTION
  m_wordList[command].Add("poissubst"); // FUNCTION
  m_wordList[tmplte ].Add("poissubst(<a>, <b>, <c>)"); // OPTION
  m_wordList[command].Add("poistimes"); // FUNCTION
  m_wordList[tmplte ].Add("poistimes(<a>, <b>)"); // OPTION
  m_wordList[command].Add("poistrim"); // FUNCTION
  m_wordList[tmplte ].Add("poistrim()"); // OPTION
  m_wordList[command].Add("printpois"); // FUNCTION
  m_wordList[tmplte ].Add("printpois(<a>)"); // OPTION
  m_wordList[command].Add("epsilon_lp"); // OPTION
  m_wordList[command].Add("linear_program"); // FUNCTION
  m_wordList[tmplte ].Add("linear_program(<A>, <b>, <c>)"); // OPTION
  m_wordList[command].Add("maximize_lp"); // FUNCTION
  m_wordList[tmplte ].Add("maximize_lp(<obj>, <cond>, [<pos>])"); // OPTION
  m_wordList[command].Add("minimize_lp"); // FUNCTION
  m_wordList[tmplte ].Add("minimize_lp(<obj>, <cond>, [<pos>])"); // OPTION
  m_wordList[command].Add("nonegative_lp"); // OPTION
  m_wordList[command].Add("askexp"); // OPTION
  m_wordList[command].Add("askinteger"); // FUNCTION
  m_wordList[tmplte ].Add("askinteger(<expr>, integer)"); // OPTION
  m_wordList[tmplte ].Add("askinteger(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("askinteger(<expr>, even)"); // OPTION
  m_wordList[tmplte ].Add("askinteger(<expr>, odd)"); // OPTION
  m_wordList[command].Add("asksign"); // FUNCTION
  m_wordList[tmplte ].Add("asksign(<expr>)"); // OPTION
  m_wordList[command].Add("demoivre"); // FUNCTION
  m_wordList[tmplte ].Add("demoivre(<expr>)"); // OPTION
  m_wordList[command].Add("distribute_over"); // OPTION
  m_wordList[command].Add("domain"); // OPTION
  m_wordList[command].Add("expand"); // FUNCTION
  m_wordList[tmplte ].Add("expand(<expr>)"); // OPTION
  m_wordList[tmplte ].Add("expand(<expr>, <p>, <n>)"); // OPTION
  m_wordList[command].Add("expandwrt"); // FUNCTION
  m_wordList[tmplte ].Add("expandwrt(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("expandwrt_denom"); // OPTION
  m_wordList[command].Add("expandwrt_factored"); // FUNCTION
  m_wordList[tmplte ].Add("expandwrt_factored(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add("expon"); // OPTION
  m_wordList[command].Add("exponentialize"); // FUNCTION
  m_wordList[tmplte ].Add("exponentialize(<expr>)"); // OPTION
  m_wordList[command].Add("expop"); // OPTION
  m_wordList[command].Add("factlim"); // OPTION
  m_wordList[command].Add("intosum"); // FUNCTION
  m_wordList[tmplte ].Add("intosum(<expr>)"); // OPTION
  m_wordList[command].Add("lassociative"); // OPTION
  m_wordList[command].Add("linear"); // OPTION
  m_wordList[command].Add("mainvar"); // OPTION
  m_wordList[command].Add("maxapplydepth"); // OPTION
  m_wordList[command].Add("maxapplyheight"); // OPTION
  m_wordList[command].Add("maxnegex"); // OPTION
  m_wordList[command].Add("maxposex"); // OPTION
  m_wordList[command].Add("multiplicative"); // OPTION
  m_wordList[command].Add("negdistrib"); // OPTION
  m_wordList[command].Add("negsumdispflag"); // OPTION
  m_wordList[command].Add("noeval"); // OPTION
  m_wordList[command].Add("noun"); // OPTION
  m_wordList[command].Add("noundisp"); // OPTION
  m_wordList[command].Add("nouns"); // OPTION
  m_wordList[command].Add("numer"); // OPTION
  m_wordList[command].Add("numerval"); // FUNCTION
  m_wordList[tmplte ].Add("numerval(<x_1>, <expr_1>, <...>, <var_n>, <expr_n>)"); // OPTION
  m_wordList[command].Add("opproperties"); // OPTION
  m_wordList[command].Add("opsubst"); // OPTION
  m_wordList[command].Add("outative"); // OPTION
  m_wordList[command].Add("posfun"); // OPTION
  m_wordList[command].Add("pred"); // OPTION
  m_wordList[command].Add("radcan"); // FUNCTION
  m_wordList[tmplte ].Add("radcan(<expr>)"); // OPTION
  m_wordList[command].Add("radexpand"); // OPTION
  m_wordList[command].Add("radsubstflag"); // OPTION
  m_wordList[command].Add("rassociative"); // OPTION
  m_wordList[command].Add("scsimp"); // FUNCTION
  m_wordList[tmplte ].Add("scsimp(<expr>, <rule_1>, <...>, <rule_n>)"); // OPTION
  m_wordList[command].Add("simp"); // OPTION
  m_wordList[command].Add("simpsum"); // OPTION
  m_wordList[command].Add("sumcontract"); // FUNCTION
  m_wordList[tmplte ].Add("sumcontract(<expr>)"); // OPTION
  m_wordList[command].Add("sumexpand"); // OPTION
  m_wordList[command].Add("sumsplitfact"); // OPTION
  m_wordList[command].Add("symmetric"); // OPTION
  m_wordList[command].Add("unknown"); // FUNCTION
  m_wordList[tmplte ].Add("unknown(<expr>)"); // OPTION
  m_wordList[command].Add("facsum"); // FUNCTION
  m_wordList[tmplte ].Add("facsum(<expr>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[command].Add("nextlayerfactor"); // OPTION
  m_wordList[command].Add("facsum_combine"); // OPTION
  m_wordList[command].Add("factorfacsum"); // FUNCTION
  m_wordList[tmplte ].Add("factorfacsum(<expr>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[command].Add("collectterms"); // FUNCTION
  m_wordList[tmplte ].Add("collectterms(<expr>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[command].Add("rempart"); // FUNCTION
  m_wordList[tmplte ].Add("rempart(<expr>, <n>)"); // OPTION
  m_wordList[command].Add("wronskian"); // FUNCTION
  m_wordList[tmplte ].Add("wronskian([<f_1>, <...>, <f_n>], <x>)"); // OPTION
  m_wordList[command].Add("tracematrix"); // FUNCTION
  m_wordList[tmplte ].Add("tracematrix(<M>)"); // OPTION
  m_wordList[command].Add("rational"); // FUNCTION
  m_wordList[tmplte ].Add("rational(<z>)"); // OPTION
  m_wordList[command].Add("logand"); // FUNCTION
  m_wordList[tmplte ].Add("logand(<x>,<y>)"); // OPTION
  m_wordList[command].Add("logor"); // FUNCTION
  m_wordList[tmplte ].Add("logor(<x>,<y>)"); // OPTION
  m_wordList[command].Add("logxor"); // FUNCTION
  m_wordList[tmplte ].Add("logxor(<x>,<y>)"); // OPTION
  m_wordList[command].Add("nonzeroandfreeof"); // FUNCTION
  m_wordList[tmplte ].Add("nonzeroandfreeof(<x>, <expr>)"); // OPTION
  m_wordList[command].Add("linear"); // FUNCTION
  m_wordList[tmplte ].Add("linear(<expr>, <x>)"); // OPTION
  m_wordList[command].Add("gcdivide"); // FUNCTION
  m_wordList[tmplte ].Add("gcdivide(<p>, <q>)"); // OPTION
  m_wordList[command].Add("arithmetic"); // FUNCTION
  m_wordList[tmplte ].Add("arithmetic(<a>, <d>, <n>)"); // OPTION
  m_wordList[command].Add("geometric"); // FUNCTION
  m_wordList[tmplte ].Add("geometric(<a>, <r>, <n>)"); // OPTION
  m_wordList[command].Add("harmonic"); // FUNCTION
  m_wordList[tmplte ].Add("harmonic(<a>, <b>, <c>, <n>)"); // OPTION
  m_wordList[command].Add("arithsum"); // FUNCTION
  m_wordList[tmplte ].Add("arithsum(<a>, <d>, <n>)"); // OPTION
  m_wordList[command].Add("geosum"); // FUNCTION
  m_wordList[tmplte ].Add("geosum(<a>, <r>, <n>)"); // OPTION
  m_wordList[command].Add("gaussprob"); // FUNCTION
  m_wordList[tmplte ].Add("gaussprob(<x>)"); // OPTION
  m_wordList[command].Add("gd"); // FUNCTION
  m_wordList[tmplte ].Add("gd(<x>)"); // OPTION
  m_wordList[command].Add("agd"); // FUNCTION
  m_wordList[tmplte ].Add("agd(<x>)"); // OPTION
  m_wordList[command].Add("vers"); // FUNCTION
  m_wordList[tmplte ].Add("vers(<x>)"); // OPTION
  m_wordList[command].Add("covers"); // FUNCTION
  m_wordList[tmplte ].Add("covers(<x>)"); // OPTION
  m_wordList[command].Add("exsec"); // FUNCTION
  m_wordList[tmplte ].Add("exsec(<x>)"); // OPTION
  m_wordList[command].Add("hav"); // FUNCTION
  m_wordList[tmplte ].Add("hav(<x>)"); // OPTION
  m_wordList[command].Add("combination"); // FUNCTION
  m_wordList[tmplte ].Add("combination(<n>, <r>)"); // OPTION
  m_wordList[command].Add("permutation"); // FUNCTION
  m_wordList[tmplte ].Add("permutation(<n>, <r>)"); // OPTION
  m_wordList[command].Add("reduce_consts"); // FUNCTION
  m_wordList[tmplte ].Add("reduce_consts(<expr>)"); // OPTION
  m_wordList[command].Add("gcfac"); // FUNCTION
  m_wordList[tmplte ].Add("gcfac(<expr>)"); // OPTION
  m_wordList[command].Add("sqrtdenest"); // FUNCTION
  m_wordList[tmplte ].Add("sqrtdenest(<expr>)"); // OPTION
  m_wordList[command].Add("reduce_order"); // FUNCTION
  m_wordList[tmplte ].Add("reduce_order(<rec>, <sol>, <var>)"); // OPTION
  m_wordList[command].Add("simplify_products"); // OPTION
  m_wordList[command].Add("simplify_sum"); // FUNCTION
  m_wordList[tmplte ].Add("simplify_sum(<expr>)"); // OPTION
  m_wordList[command].Add("solve_rec"); // FUNCTION
  m_wordList[tmplte ].Add("solve_rec(<eqn>, <var>, [<init>])"); // OPTION
  m_wordList[command].Add("solve_rec_rat"); // FUNCTION
  m_wordList[tmplte ].Add("solve_rec_rat(<eqn>, <var>, [<init>])"); // OPTION
  m_wordList[command].Add("product_use_gamma"); // OPTION
  m_wordList[command].Add("summand_to_rec"); // FUNCTION
  m_wordList[tmplte ].Add("summand_to_rec(<summand>, <k>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("summand_to_rec(<summand>, [<k>, <lo>, <hi>], <n>)"); // OPTION
  m_wordList[command].Add("bessel_j"); // FUNCTION
  m_wordList[tmplte ].Add("bessel_j(<v>, <z>)"); // OPTION
  m_wordList[command].Add("bessel_y"); // FUNCTION
  m_wordList[tmplte ].Add("bessel_y(<v>, <z>)"); // OPTION
  m_wordList[command].Add("bessel_i"); // FUNCTION
  m_wordList[tmplte ].Add("bessel_i(<v>, <z>)"); // OPTION
  m_wordList[command].Add("bessel_k"); // FUNCTION
  m_wordList[tmplte ].Add("bessel_k(<v>, <z>)"); // OPTION
  m_wordList[command].Add("hankel_1"); // FUNCTION
  m_wordList[tmplte ].Add("hankel_1(<v>, <z>)"); // OPTION
  m_wordList[command].Add("hankel_2"); // FUNCTION
  m_wordList[tmplte ].Add("hankel_2(<v>, <z>)"); // OPTION
  m_wordList[command].Add("besselexpand"); // OPTION
  m_wordList[command].Add("scaled_bessel_i"); // FUNCTION
  m_wordList[tmplte ].Add("scaled_bessel_i(<v>, <z>) "); // OPTION
  m_wordList[command].Add("scaled_bessel_i0"); // FUNCTION
  m_wordList[tmplte ].Add("scaled_bessel_i0(<z>) "); // OPTION
  m_wordList[command].Add("scaled_bessel_i1"); // FUNCTION
  m_wordList[tmplte ].Add("scaled_bessel_i1(<z>) "); // OPTION
  m_wordList[command].Add("%s"); // FUNCTION
  m_wordList[tmplte ].Add("%s[<u>,<v>] (<z>) "); // OPTION
  m_wordList[command].Add("airy_ai"); // FUNCTION
  m_wordList[tmplte ].Add("airy_ai(<x>)"); // OPTION
  m_wordList[command].Add("airy_dai"); // FUNCTION
  m_wordList[tmplte ].Add("airy_dai(<x>)"); // OPTION
  m_wordList[command].Add("airy_bi"); // FUNCTION
  m_wordList[tmplte ].Add("airy_bi(<x>)"); // OPTION
  m_wordList[command].Add("airy_dbi"); // FUNCTION
  m_wordList[tmplte ].Add("airy_dbi(<x>)"); // OPTION
  m_wordList[command].Add("gamma"); // FUNCTION
  m_wordList[tmplte ].Add("gamma(<z>)"); // OPTION
  m_wordList[command].Add("log_gamma"); // FUNCTION
  m_wordList[tmplte ].Add("log_gamma(<z>)"); // OPTION
  m_wordList[command].Add("gamma_incomplete"); // FUNCTION
  m_wordList[tmplte ].Add("gamma_incomplete(<a>, <z>)"); // OPTION
  m_wordList[command].Add("gamma_incomplete_regularized"); // FUNCTION
  m_wordList[tmplte ].Add("gamma_incomplete_regularized(<a>,<z>)"); // OPTION
  m_wordList[command].Add("gamma_incomplete_generalized"); // FUNCTION
  m_wordList[tmplte ].Add("gamma_incomplete_generalized(<a>,<z1>,<z1> )"); // OPTION
  m_wordList[command].Add("gammalim"); // OPTION
  m_wordList[command].Add("makegamma"); // FUNCTION
  m_wordList[tmplte ].Add("makegamma(<expr>)"); // OPTION
  m_wordList[command].Add("beta"); // FUNCTION
  m_wordList[tmplte ].Add("beta(<a>, <b>)"); // OPTION
  m_wordList[command].Add("beta_incomplete"); // FUNCTION
  m_wordList[tmplte ].Add("beta_incomplete(<a>, <b>, <z>)"); // OPTION
  m_wordList[command].Add("beta_incomplete_regularized"); // FUNCTION
  m_wordList[tmplte ].Add("beta_incomplete_regularized(<a>, <b>, <z>)"); // OPTION
  m_wordList[command].Add("beta_incomplete_generalized"); // FUNCTION
  m_wordList[tmplte ].Add("beta_incomplete_generalized(<a>, <b>, <z1>, <z2>)"); // OPTION
  m_wordList[command].Add("beta_expand"); // OPTION
  m_wordList[command].Add("beta_args_sum_to_integer"); // OPTION
  m_wordList[command].Add("psi"); // FUNCTION
  m_wordList[tmplte ].Add("psi[<n>](<x>)"); // OPTION
  m_wordList[command].Add("maxpsiposint"); // OPTION
  m_wordList[command].Add("maxpsinegint"); // OPTION
  m_wordList[command].Add("maxpsifracnum"); // OPTION
  m_wordList[command].Add("maxpsifracdenom"); // OPTION
  m_wordList[command].Add("makefact"); // FUNCTION
  m_wordList[tmplte ].Add("makefact(<expr>)"); // OPTION
  m_wordList[command].Add("numfactor"); // FUNCTION
  m_wordList[tmplte ].Add("numfactor(<expr>)"); // OPTION
  m_wordList[command].Add("expintegral_e1"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_e1(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_ei"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_ei(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_li"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_li(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_e"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_e(<n>,<z>)"); // OPTION
  m_wordList[command].Add("expintegral_si"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_si(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_ci"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_ci(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_shi"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_shi(<z>)"); // OPTION
  m_wordList[command].Add("expintegral_chi"); // FUNCTION
  m_wordList[tmplte ].Add("expintegral_chi(<z>)"); // OPTION
  m_wordList[command].Add("expintrep"); // OPTION
  m_wordList[command].Add("expintexpand"); // OPTION
  m_wordList[command].Add("erf"); // FUNCTION
  m_wordList[tmplte ].Add("erf(<z>)"); // OPTION
  m_wordList[command].Add("erfc"); // FUNCTION
  m_wordList[tmplte ].Add("erfc(<z>)"); // OPTION
  m_wordList[command].Add("erfi"); // FUNCTION
  m_wordList[tmplte ].Add("erfi(<z>)"); // OPTION
  m_wordList[command].Add("erf_generalized"); // FUNCTION
  m_wordList[tmplte ].Add("erf_generalized(<z1>,<z2>)"); // OPTION
  m_wordList[command].Add("fresnel_c"); // FUNCTION
  m_wordList[tmplte ].Add("fresnel_c(<z>)"); // OPTION
  m_wordList[command].Add("fresnel_s"); // FUNCTION
  m_wordList[tmplte ].Add("fresnel_s(<z>)"); // OPTION
  m_wordList[command].Add("erf_representation"); // OPTION
  m_wordList[command].Add("hypergeometric_representation"); // OPTION
  m_wordList[command].Add("struve_h"); // FUNCTION
  m_wordList[tmplte ].Add("struve_h(<v>, <z>)"); // OPTION
  m_wordList[command].Add("struve_l"); // FUNCTION
  m_wordList[tmplte ].Add("struve_l(<v>, <z>)"); // OPTION
  m_wordList[command].Add("%m"); // FUNCTION
  m_wordList[tmplte ].Add("%m[<k>,<u>] (<z>) "); // OPTION
  m_wordList[command].Add("%w"); // FUNCTION
  m_wordList[tmplte ].Add("%w[<k>,<u>] (<z>) "); // OPTION
  m_wordList[command].Add("%f"); // FUNCTION
  m_wordList[tmplte ].Add("%f[<p>,<q>] (<[a],[b],z>) "); // OPTION
  m_wordList[command].Add("hypergeometric"); // FUNCTION
  m_wordList[tmplte ].Add("hypergeometric([<a1>, <...>, <ap>],[<b1>, <...> ,<bq>], x)"); // OPTION
  m_wordList[command].Add("parabolic_cylinder_d"); // FUNCTION
  m_wordList[tmplte ].Add("parabolic_cylinder_d(<v>, <z>) "); // OPTION
  m_wordList[command].Add("specint"); // FUNCTION
  m_wordList[tmplte ].Add("specint(exp(- s*<t>) * <expr>, <t>)"); // OPTION
  m_wordList[command].Add("hgfred"); // FUNCTION
  m_wordList[tmplte ].Add("hgfred(<a>, <b>, <t>)"); // OPTION
  m_wordList[command].Add("lambert_w"); // FUNCTION
  m_wordList[tmplte ].Add("lambert_w(<z>)"); // OPTION
  m_wordList[command].Add("nzeta"); // FUNCTION
  m_wordList[tmplte ].Add("nzeta(<z>)"); // OPTION
  m_wordList[command].Add("nzetar"); // FUNCTION
  m_wordList[tmplte ].Add("nzetar(<z>)"); // OPTION
  m_wordList[command].Add("nzetai"); // FUNCTION
  m_wordList[tmplte ].Add("nzetai(<z>)"); // OPTION
  m_wordList[command].Add("inference_result"); // FUNCTION
  m_wordList[tmplte ].Add("inference_result(<title>, <values>, <numbers>)"); // OPTION
  m_wordList[command].Add("inferencep"); // FUNCTION
  m_wordList[tmplte ].Add("inferencep(<obj>)"); // OPTION
  m_wordList[command].Add("items_inference"); // FUNCTION
  m_wordList[tmplte ].Add("items_inference(<obj>)"); // OPTION
  m_wordList[command].Add("take_inference"); // FUNCTION
  m_wordList[tmplte ].Add("take_inference(<n>, <obj>)"); // OPTION
  m_wordList[tmplte ].Add("take_inference(<name>, <obj>)"); // OPTION
  m_wordList[tmplte ].Add("take_inference(<list>, <obj>)"); // OPTION
  m_wordList[command].Add("stats_numer"); // OPTION
  m_wordList[command].Add("test_mean"); // FUNCTION
  m_wordList[tmplte ].Add("test_mean(<x>)"); // OPTION
  m_wordList[tmplte ].Add("test_mean(<x>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_means_difference"); // FUNCTION
  m_wordList[tmplte ].Add("test_means_difference(<x1>, <x2>)"); // OPTION
  m_wordList[tmplte ].Add("test_means_difference(<x1>, <x2>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_variance"); // FUNCTION
  m_wordList[tmplte ].Add("test_variance(<x>)"); // OPTION
  m_wordList[tmplte ].Add("test_variance(<x>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_variance_ratio"); // FUNCTION
  m_wordList[tmplte ].Add("test_variance_ratio(<x1>, <x2>)"); // OPTION
  m_wordList[tmplte ].Add("test_variance_ratio(<x1>, <x2>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_proportion"); // FUNCTION
  m_wordList[tmplte ].Add("test_proportion(<x>, <n>)"); // OPTION
  m_wordList[tmplte ].Add("test_proportion(<x>, <n>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_proportions_difference"); // FUNCTION
  m_wordList[tmplte ].Add("test_proportions_difference(<x1>, <n1>, <x2>, <n2>)"); // OPTION
  m_wordList[tmplte ].Add("test_proportions_difference(<x1>, <n1>, <x2>, <n2>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_sign"); // FUNCTION
  m_wordList[tmplte ].Add("test_sign(<x>)"); // OPTION
  m_wordList[tmplte ].Add("test_sign(<x>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_signed_rank"); // FUNCTION
  m_wordList[tmplte ].Add("test_signed_rank(<x>)"); // OPTION
  m_wordList[tmplte ].Add("test_signed_rank(<x>, <options>, <...>)"); // OPTION
  m_wordList[command].Add("test_rank_sum"); // FUNCTION
  m_wordList[tmplte ].Add("test_rank_sum(<x1>, <x2>)"); // OPTION
  m_wordList[tmplte ].Add("test_rank_sum(<x1>, <x2>, <option>)"); // OPTION
  m_wordList[command].Add("test_normality"); // FUNCTION
  m_wordList[tmplte ].Add("test_normality(<x>)"); // OPTION
  m_wordList[command].Add("simple_linear_regression"); // FUNCTION
  m_wordList[tmplte ].Add("simple_linear_regression(<x>)"); // OPTION
  m_wordList[tmplte ].Add("simple_linear_regression(<x>, <option>)"); // OPTION
  m_wordList[command].Add("pdf_signed_rank"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_signed_rank(<x>, <n>)"); // OPTION
  m_wordList[command].Add("cdf_signed_rank"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_signed_rank(<x>, <n>)"); // OPTION
  m_wordList[command].Add("pdf_rank_sum"); // FUNCTION
  m_wordList[tmplte ].Add("pdf_rank_sum(<x>, <n>, <m>)"); // OPTION
  m_wordList[command].Add("cdf_rank_sum"); // FUNCTION
  m_wordList[tmplte ].Add("cdf_rank_sum(<x>, <n>, <m>)"); // OPTION
  m_wordList[command].Add("stirling"); // FUNCTION
  m_wordList[tmplte ].Add("stirling(<z>,<n>)"); // OPTION
  m_wordList[tmplte ].Add("stirling(<z>,<n>,<pred>)"); // OPTION
  m_wordList[command].Add("close"); // FUNCTION
  m_wordList[tmplte ].Add("close(<stream>) "); // OPTION
  m_wordList[command].Add("flength"); // FUNCTION
  m_wordList[tmplte ].Add("flength(<stream>)"); // OPTION
  m_wordList[command].Add("fposition"); // FUNCTION
  m_wordList[tmplte ].Add("fposition(<stream>)"); // OPTION
  m_wordList[tmplte ].Add("fposition(<stream>, <pos>)"); // OPTION
  m_wordList[command].Add("freshline"); // FUNCTION
  m_wordList[tmplte ].Add("freshline() "); // OPTION
  m_wordList[tmplte ].Add("freshline(<stream>) "); // OPTION
  m_wordList[command].Add("newline"); // FUNCTION
  m_wordList[tmplte ].Add("newline() "); // OPTION
  m_wordList[tmplte ].Add("newline(<stream>) "); // OPTION
  m_wordList[command].Add("opena"); // FUNCTION
  m_wordList[tmplte ].Add("opena(<file>) "); // OPTION
  m_wordList[command].Add("openr"); // FUNCTION
  m_wordList[tmplte ].Add("openr(<file>) "); // OPTION
  m_wordList[tmplte ].Add("openr(<file>,<encoding_name_as_string>) "); // OPTION
  m_wordList[command].Add("openw"); // FUNCTION
  m_wordList[tmplte ].Add("openw(<file>) "); // OPTION
  m_wordList[tmplte ].Add("openw(<file>,<encoding_name_as_string>) "); // OPTION
  m_wordList[command].Add("printf"); // FUNCTION
  m_wordList[tmplte ].Add("printf(<dest>, <string>)"); // OPTION
  m_wordList[tmplte ].Add("printf(<dest>, <string>, <expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("readline"); // FUNCTION
  m_wordList[tmplte ].Add("readline(<stream>) "); // OPTION
  m_wordList[command].Add("sprint"); // FUNCTION
  m_wordList[tmplte ].Add("sprint(<expr_1>, <...>, <expr_n>)"); // OPTION
  m_wordList[command].Add("alphacharp"); // FUNCTION
  m_wordList[tmplte ].Add("alphacharp(<char>)    "); // OPTION
  m_wordList[command].Add("alphanumericp"); // FUNCTION
  m_wordList[tmplte ].Add("alphanumericp(<char>) "); // OPTION
  m_wordList[command].Add("ascii"); // FUNCTION
  m_wordList[tmplte ].Add("ascii(<int>) "); // OPTION
  m_wordList[command].Add("cequal"); // FUNCTION
  m_wordList[tmplte ].Add("cequal(<char_1>, <char_2>)          "); // OPTION
  m_wordList[command].Add("cequalignore"); // FUNCTION
  m_wordList[tmplte ].Add("cequalignore(<char_1>, <char_2>)    "); // OPTION
  m_wordList[command].Add("cgreaterp"); // FUNCTION
  m_wordList[tmplte ].Add("cgreaterp(<char_1>, <char_2>)       "); // OPTION
  m_wordList[command].Add("cgreaterpignore"); // FUNCTION
  m_wordList[tmplte ].Add("cgreaterpignore(<char_1>, <char_2>)"); // OPTION
  m_wordList[command].Add("charp"); // FUNCTION
  m_wordList[tmplte ].Add("charp(<obj>) "); // OPTION
  m_wordList[command].Add("cint"); // FUNCTION
  m_wordList[tmplte ].Add("cint(<char>) "); // OPTION
  m_wordList[command].Add("clessp"); // FUNCTION
  m_wordList[tmplte ].Add("clessp(<char_1>, <char_2>)"); // OPTION
  m_wordList[command].Add("clesspignore"); // FUNCTION
  m_wordList[tmplte ].Add("clesspignore(<char_1>, <char_2>)"); // OPTION
  m_wordList[command].Add("constituent"); // FUNCTION
  m_wordList[tmplte ].Add("constituent(<char>)   "); // OPTION
  m_wordList[command].Add("cunlisp"); // FUNCTION
  m_wordList[tmplte ].Add("cunlisp(<lisp_char>) "); // OPTION
  m_wordList[command].Add("digitcharp"); // FUNCTION
  m_wordList[tmplte ].Add("digitcharp(<char>)    "); // OPTION
  m_wordList[command].Add("lcharp"); // FUNCTION
  m_wordList[tmplte ].Add("lcharp(<obj>) "); // OPTION
  m_wordList[command].Add("lowercasep"); // FUNCTION
  m_wordList[tmplte ].Add("lowercasep(<char>)    "); // OPTION
  m_wordList[command].Add("newline"); // OPTION
  m_wordList[command].Add("space"); // OPTION
  m_wordList[command].Add("tab"); // OPTION
  m_wordList[command].Add("uppercasep"); // FUNCTION
  m_wordList[tmplte ].Add("uppercasep(<char>)    "); // OPTION
  m_wordList[command].Add("stringp"); // FUNCTION
  m_wordList[tmplte ].Add("stringp(<obj>) "); // OPTION
  m_wordList[command].Add("charat"); // FUNCTION
  m_wordList[tmplte ].Add("charat(<string>, <n>) "); // OPTION
  m_wordList[command].Add("charlist"); // FUNCTION
  m_wordList[tmplte ].Add("charlist(<string>) "); // OPTION
  m_wordList[command].Add("eval_string"); // FUNCTION
  m_wordList[tmplte ].Add("eval_string(<str>)"); // OPTION
  m_wordList[command].Add("parse_string"); // FUNCTION
  m_wordList[tmplte ].Add("parse_string(<str>)"); // OPTION
  m_wordList[command].Add("scopy"); // FUNCTION
  m_wordList[tmplte ].Add("scopy(<string>) "); // OPTION
  m_wordList[command].Add("sdowncase"); // FUNCTION
  m_wordList[tmplte ].Add("sdowncase(<string>) "); // OPTION
  m_wordList[tmplte ].Add("sdowncase(<string>, <start>) "); // OPTION
  m_wordList[tmplte ].Add("sdowncase(<string>, <start>, <end>) "); // OPTION
  m_wordList[command].Add("sequal"); // FUNCTION
  m_wordList[tmplte ].Add("sequal(<string_1>, <string_2>) "); // OPTION
  m_wordList[command].Add("sequalignore"); // FUNCTION
  m_wordList[tmplte ].Add("sequalignore(<string_1>, <string_2>)"); // OPTION
  m_wordList[command].Add("sexplode"); // FUNCTION
  m_wordList[tmplte ].Add("sexplode(<string>)"); // OPTION
  m_wordList[command].Add("simplode"); // FUNCTION
  m_wordList[tmplte ].Add("simplode(<list>)  "); // OPTION
  m_wordList[tmplte ].Add("simplode(<list>, <delim>)  "); // OPTION
  m_wordList[command].Add("sinsert"); // FUNCTION
  m_wordList[tmplte ].Add("sinsert(<seq>, <string>, <pos>)  "); // OPTION
  m_wordList[command].Add("sinvertcase"); // FUNCTION
  m_wordList[tmplte ].Add("sinvertcase(<string>)  "); // OPTION
  m_wordList[tmplte ].Add("sinvertcase(<string>, <start>)  "); // OPTION
  m_wordList[tmplte ].Add("sinvertcase(<string>, <start>, <end>)  "); // OPTION
  m_wordList[command].Add("slength"); // FUNCTION
  m_wordList[tmplte ].Add("slength(<string>) "); // OPTION
  m_wordList[command].Add("smake"); // FUNCTION
  m_wordList[tmplte ].Add("smake(<num>, <char>) "); // OPTION
  m_wordList[command].Add("smismatch"); // FUNCTION
  m_wordList[tmplte ].Add("smismatch(<string_1>, <string_2>) "); // OPTION
  m_wordList[tmplte ].Add("smismatch(<string_1>, <string_2>, <test>) "); // OPTION
  m_wordList[command].Add("split"); // FUNCTION
  m_wordList[tmplte ].Add("split(<string>)  "); // OPTION
  m_wordList[tmplte ].Add("split(<string>, <delim>)  "); // OPTION
  m_wordList[tmplte ].Add("split(<string>, <delim>, <multiple>)  "); // OPTION
  m_wordList[command].Add("sposition"); // FUNCTION
  m_wordList[tmplte ].Add("sposition(<char>, <string>) "); // OPTION
  m_wordList[command].Add("sremove"); // FUNCTION
  m_wordList[tmplte ].Add("sremove(<seq>, <string>)  "); // OPTION
  m_wordList[tmplte ].Add("sremove(<seq>, <string>, <test>)  "); // OPTION
  m_wordList[tmplte ].Add("sremove(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte ].Add("sremove(<seq>, <string>, <test>, <start>, <end>)  "); // OPTION
  m_wordList[command].Add("sremovefirst"); // FUNCTION
  m_wordList[tmplte ].Add("sremovefirst(<seq>, <string>)  "); // OPTION
  m_wordList[tmplte ].Add("sremovefirst(<seq>, <string>, <test>)  "); // OPTION
  m_wordList[tmplte ].Add("sremovefirst(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte ].Add("sremovefirst(<seq>, <string>, <test>, <start>, <end>)  "); // OPTION
  m_wordList[command].Add("sreverse"); // FUNCTION
  m_wordList[tmplte ].Add("sreverse(<string>) "); // OPTION
  m_wordList[command].Add("ssearch"); // FUNCTION
  m_wordList[tmplte ].Add("ssearch(<seq>, <string>)  "); // OPTION
  m_wordList[tmplte ].Add("ssearch(<seq>, <string>, <test>)  "); // OPTION
  m_wordList[tmplte ].Add("ssearch(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte ].Add("ssearch(<seq>, <string>, <test>, <start>, <end>)"); // OPTION
  m_wordList[command].Add("ssort"); // FUNCTION
  m_wordList[tmplte ].Add("ssort(<string>) "); // OPTION
  m_wordList[tmplte ].Add("ssort(<string>, <test>) "); // OPTION
  m_wordList[command].Add("ssubst"); // FUNCTION
  m_wordList[tmplte ].Add("ssubst(<new>, <old>, <string>) "); // OPTION
  m_wordList[tmplte ].Add("ssubst(<new>, <old>, <string>, <test>) "); // OPTION
  m_wordList[tmplte ].Add("ssubst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList[tmplte ].Add("ssubst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList[command].Add("ssubstfirst"); // FUNCTION
  m_wordList[tmplte ].Add("ssubstfirst(<new>, <old>, <string>) "); // OPTION
  m_wordList[tmplte ].Add("ssubstfirst(<new>, <old>, <string>, <test>) "); // OPTION
  m_wordList[tmplte ].Add("ssubstfirst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList[tmplte ].Add("ssubstfirst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList[command].Add("strim"); // FUNCTION
  m_wordList[tmplte ].Add("strim(<seq>,<string>) "); // OPTION
  m_wordList[command].Add("striml"); // FUNCTION
  m_wordList[tmplte ].Add("striml(<seq>, <string>) "); // OPTION
  m_wordList[command].Add("strimr"); // FUNCTION
  m_wordList[tmplte ].Add("strimr(<seq>, <string>) "); // OPTION
  m_wordList[command].Add("substring"); // FUNCTION
  m_wordList[tmplte ].Add("substring(<string>, <start>)"); // OPTION
  m_wordList[tmplte ].Add("substring(<string>, <start>, <end>) "); // OPTION
  m_wordList[command].Add("supcase"); // FUNCTION
  m_wordList[tmplte ].Add("supcase(<string>) "); // OPTION
  m_wordList[tmplte ].Add("supcase(<string>, <start>) "); // OPTION
  m_wordList[tmplte ].Add("supcase(<string>, <start>, <end>) "); // OPTION
  m_wordList[command].Add("tokens"); // FUNCTION
  m_wordList[tmplte ].Add("tokens(<string>) "); // OPTION
  m_wordList[tmplte ].Add("tokens(<string>, <test>) "); // OPTION
  m_wordList[command].Add("comp2pui"); // FUNCTION
  m_wordList[tmplte ].Add("comp2pui(<n>, <L>)"); // OPTION
  m_wordList[command].Add("ele2pui"); // FUNCTION
  m_wordList[tmplte ].Add("ele2pui(<m>, <L>)"); // OPTION
  m_wordList[command].Add("ele2comp"); // FUNCTION
  m_wordList[tmplte ].Add("ele2comp(<m>, <L>)"); // OPTION
  m_wordList[command].Add("elem"); // FUNCTION
  m_wordList[tmplte ].Add("elem(<ele>, <sym>, <lvar>)"); // OPTION
  m_wordList[command].Add("mon2schur"); // FUNCTION
  m_wordList[tmplte ].Add("mon2schur(<L>)"); // OPTION
  m_wordList[command].Add("multi_elem"); // FUNCTION
  m_wordList[tmplte ].Add("multi_elem(<l_elem>, <multi_pc>, <l_var>)"); // OPTION
  m_wordList[command].Add("pui"); // FUNCTION
  m_wordList[tmplte ].Add("pui(<L>, <sym>, <lvar>)"); // OPTION
  m_wordList[command].Add("pui2comp"); // FUNCTION
  m_wordList[tmplte ].Add("pui2comp(<n>, <lpui>)"); // OPTION
  m_wordList[command].Add("pui2ele"); // FUNCTION
  m_wordList[tmplte ].Add("pui2ele(<n>, <lpui>)"); // OPTION
  m_wordList[command].Add("puireduc"); // FUNCTION
  m_wordList[tmplte ].Add("puireduc(<n>, <lpui>)"); // OPTION
  m_wordList[command].Add("schur2comp"); // FUNCTION
  m_wordList[tmplte ].Add("schur2comp(<P>, <l_var>)"); // OPTION
  m_wordList[command].Add("cont2part"); // FUNCTION
  m_wordList[tmplte ].Add("cont2part(<pc>, <lvar>)"); // OPTION
  m_wordList[command].Add("contract"); // FUNCTION
  m_wordList[tmplte ].Add("contract(<psym>, <lvar>)"); // OPTION
  m_wordList[command].Add("explose"); // FUNCTION
  m_wordList[tmplte ].Add("explose(<pc>, <lvar>)"); // OPTION
  m_wordList[command].Add("part2cont"); // FUNCTION
  m_wordList[tmplte ].Add("part2cont(<ppart>, <lvar>)"); // OPTION
  m_wordList[command].Add("partpol"); // FUNCTION
  m_wordList[tmplte ].Add("partpol(<psym>, <lvar>)"); // OPTION
  m_wordList[command].Add("tcontract"); // FUNCTION
  m_wordList[tmplte ].Add("tcontract(<pol>, <lvar>)"); // OPTION
  m_wordList[command].Add("tpartpol"); // FUNCTION
  m_wordList[tmplte ].Add("tpartpol(<pol>, <lvar>)"); // OPTION
  m_wordList[command].Add("direct"); // FUNCTION
  m_wordList[tmplte ].Add("direct([<p_1>, <...>, <p_n>], <y>, <f>, [<lvar_1>, <...>, <lvar_n>])"); // OPTION
  m_wordList[command].Add("directory"); // FUNCTION
  m_wordList[tmplte ].Add("directory(<pattern>)"); // OPTION
  m_wordList[command].Add("multi_orbit"); // FUNCTION
  m_wordList[tmplte ].Add("multi_orbit(<P>, [<lvar_1>, <lvar_2>,<...>, <lvar_p>])"); // OPTION
  m_wordList[command].Add("multsym"); // FUNCTION
  m_wordList[tmplte ].Add("multsym(<ppart_1>, <ppart_2>, <n>)"); // OPTION
  m_wordList[command].Add("orbit"); // FUNCTION
  m_wordList[tmplte ].Add("orbit(<P>, <lvar>)"); // OPTION
  m_wordList[command].Add("pui_direct"); // FUNCTION
  m_wordList[tmplte ].Add("pui_direct(<orbite>, [<lvar_1>, <...>, <lvar_n>], [<d_1>, <d_2>, <...>, <d_n>])"); // OPTION
  m_wordList[command].Add("kostka"); // FUNCTION
  m_wordList[tmplte ].Add("kostka(<part_1>, <part_2>)"); // OPTION
  m_wordList[command].Add("lgtreillis"); // FUNCTION
  m_wordList[tmplte ].Add("lgtreillis(<n>, <m>)"); // OPTION
  m_wordList[command].Add("ltreillis"); // FUNCTION
  m_wordList[tmplte ].Add("ltreillis(<n>, <m>)"); // OPTION
  m_wordList[command].Add("treillis"); // FUNCTION
  m_wordList[tmplte ].Add("treillis(<n>)"); // OPTION
  m_wordList[command].Add("treinat"); // FUNCTION
  m_wordList[tmplte ].Add("treinat(<part>)"); // OPTION
  m_wordList[command].Add("ele2polynome"); // FUNCTION
  m_wordList[tmplte ].Add("ele2polynome(<L>, <z>)"); // OPTION
  m_wordList[command].Add("polynome2ele"); // FUNCTION
  m_wordList[tmplte ].Add("polynome2ele(<P>, <x>)"); // OPTION
  m_wordList[command].Add("prodrac"); // FUNCTION
  m_wordList[tmplte ].Add("prodrac(<L>, <k>)"); // OPTION
  m_wordList[command].Add("pui2polynome"); // FUNCTION
  m_wordList[tmplte ].Add("pui2polynome(<x>, <lpui>)"); // OPTION
  m_wordList[command].Add("somrac"); // FUNCTION
  m_wordList[tmplte ].Add("somrac(<L>, <k>)"); // OPTION
  m_wordList[command].Add("resolvante"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante(<P>, <x>, <f>, [<x_1>,<...>, <x_d>]) "); // OPTION
  m_wordList[command].Add("resolvante_alternee1"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_alternee1(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_bipartite"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_bipartite(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_diedrale"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_diedrale(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_klein"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_klein(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_klein3"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_klein3(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_produit_sym"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_produit_sym(<P>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_unitaire"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_unitaire(<P>, <Q>, <x>)"); // OPTION
  m_wordList[command].Add("resolvante_vierer"); // FUNCTION
  m_wordList[tmplte ].Add("resolvante_vierer(<P>, <x>)"); // OPTION
  m_wordList[command].Add("multinomial"); // FUNCTION
  m_wordList[tmplte ].Add("multinomial(<r>, <part>)"); // OPTION
  m_wordList[command].Add("permut"); // FUNCTION
  m_wordList[tmplte ].Add("permut(<L>)"); // OPTION
  m_wordList[command].Add("%piargs"); // OPTION
  m_wordList[command].Add("%iargs"); // OPTION
  m_wordList[command].Add("acos"); // FUNCTION
  m_wordList[tmplte ].Add("acos(<x>)"); // OPTION
  m_wordList[command].Add("acosh"); // FUNCTION
  m_wordList[tmplte ].Add("acosh(<x>)"); // OPTION
  m_wordList[command].Add("acot"); // FUNCTION
  m_wordList[tmplte ].Add("acot(<x>)"); // OPTION
  m_wordList[command].Add("acoth"); // FUNCTION
  m_wordList[tmplte ].Add("acoth(<x>)"); // OPTION
  m_wordList[command].Add("acsc"); // FUNCTION
  m_wordList[tmplte ].Add("acsc(<x>)"); // OPTION
  m_wordList[command].Add("acsch"); // FUNCTION
  m_wordList[tmplte ].Add("acsch(<x>)"); // OPTION
  m_wordList[command].Add("asec"); // FUNCTION
  m_wordList[tmplte ].Add("asec(<x>)"); // OPTION
  m_wordList[command].Add("asech"); // FUNCTION
  m_wordList[tmplte ].Add("asech(<x>)"); // OPTION
  m_wordList[command].Add("asin"); // FUNCTION
  m_wordList[tmplte ].Add("asin(<x>)"); // OPTION
  m_wordList[command].Add("asinh"); // FUNCTION
  m_wordList[tmplte ].Add("asinh(<x>)"); // OPTION
  m_wordList[command].Add("atan"); // FUNCTION
  m_wordList[tmplte ].Add("atan(<x>)"); // OPTION
  m_wordList[command].Add("atan2"); // FUNCTION
  m_wordList[tmplte ].Add("atan2(<y>, <x>)"); // OPTION
  m_wordList[command].Add("atanh"); // FUNCTION
  m_wordList[tmplte ].Add("atanh(<x>)"); // OPTION
  m_wordList[command].Add("atrig1"); // OPTION
  m_wordList[command].Add("cos"); // FUNCTION
  m_wordList[tmplte ].Add("cos(<x>)"); // OPTION
  m_wordList[command].Add("cosh"); // FUNCTION
  m_wordList[tmplte ].Add("cosh(<x>)"); // OPTION
  m_wordList[command].Add("cot"); // FUNCTION
  m_wordList[tmplte ].Add("cot(<x>)"); // OPTION
  m_wordList[command].Add("coth"); // FUNCTION
  m_wordList[tmplte ].Add("coth(<x>)"); // OPTION
  m_wordList[command].Add("csc"); // FUNCTION
  m_wordList[tmplte ].Add("csc(<x>)"); // OPTION
  m_wordList[command].Add("csch"); // FUNCTION
  m_wordList[tmplte ].Add("csch(<x>)"); // OPTION
  m_wordList[command].Add("halfangles"); // OPTION
  m_wordList[command].Add("ntrig"); // OPTION
  m_wordList[command].Add("sec"); // FUNCTION
  m_wordList[tmplte ].Add("sec(<x>)"); // OPTION
  m_wordList[command].Add("sech"); // FUNCTION
  m_wordList[tmplte ].Add("sech(<x>)"); // OPTION
  m_wordList[command].Add("sin"); // FUNCTION
  m_wordList[tmplte ].Add("sin(<x>)"); // OPTION
  m_wordList[command].Add("sinh"); // FUNCTION
  m_wordList[tmplte ].Add("sinh(<x>)"); // OPTION
  m_wordList[command].Add("tan"); // FUNCTION
  m_wordList[tmplte ].Add("tan(<x>)"); // OPTION
  m_wordList[command].Add("tanh"); // FUNCTION
  m_wordList[tmplte ].Add("tanh(<x>)"); // OPTION
  m_wordList[command].Add("trigexpand"); // FUNCTION
  m_wordList[tmplte ].Add("trigexpand(<expr>)"); // OPTION
  m_wordList[command].Add("trigexpandplus"); // OPTION
  m_wordList[command].Add("trigexpandtimes"); // OPTION
  m_wordList[command].Add("triginverses"); // OPTION
  m_wordList[command].Add("trigreduce"); // FUNCTION
  m_wordList[tmplte ].Add("trigreduce(<expr>, <x>)"); // OPTION
  m_wordList[tmplte ].Add("trigreduce(<expr>)"); // OPTION
  m_wordList[command].Add("trigsign"); // OPTION
  m_wordList[command].Add("trigsimp"); // FUNCTION
  m_wordList[tmplte ].Add("trigsimp(<expr>)"); // OPTION
  m_wordList[command].Add("trigrat"); // FUNCTION
  m_wordList[tmplte ].Add("trigrat(<expr>)"); // OPTION
  m_wordList[command].Add("setunits"); // FUNCTION
  m_wordList[tmplte ].Add("setunits(<list>)"); // OPTION
  m_wordList[command].Add("uforget"); // FUNCTION
  m_wordList[tmplte ].Add("uforget(<list>)"); // OPTION
  m_wordList[command].Add("convert"); // FUNCTION
  m_wordList[tmplte ].Add("convert(<expr>, <list>)"); // OPTION
  m_wordList[command].Add("usersetunits"); // OPTION
  m_wordList[command].Add("metricexpandall"); // FUNCTION
  m_wordList[tmplte ].Add("metricexpandall(<x>)"); // OPTION
  m_wordList[command].Add("%unitexpand"); // OPTION
  m_wordList[command].Add("AntiDifference"); // FUNCTION
  m_wordList[tmplte ].Add("AntiDifference(<F_k>, <k>)"); // OPTION
  m_wordList[command].Add("Gosper"); // FUNCTION
  m_wordList[tmplte ].Add("Gosper(<F_k>, <k>)"); // OPTION
  m_wordList[command].Add("GosperSum"); // FUNCTION
  m_wordList[tmplte ].Add("GosperSum(<F_k>, <k>, <a>, <b>)"); // OPTION
  m_wordList[command].Add("parGosper"); // FUNCTION
  m_wordList[tmplte ].Add("parGosper(<F_{n,k}>, <k>, <n>, <d>)"); // OPTION
  m_wordList[command].Add("parGosper"); // FUNCTION
  m_wordList[tmplte ].Add("parGosper(<F_(n,k)>, <k>, <n>, <d>)"); // OPTION
  m_wordList[command].Add("Zeilberger"); // FUNCTION
  m_wordList[tmplte ].Add("Zeilberger(<F_{n,k}>, <k>, <n>)"); // OPTION
  m_wordList[command].Add("Zeilberger"); // FUNCTION
  m_wordList[tmplte ].Add("Zeilberger(<F_(n,k)>, <k>, <n>)"); // OPTION
  m_wordList[command].Add("MAX_ORD"); // OPTION
  m_wordList[command].Add("simplified_output"); // OPTION
  m_wordList[command].Add("linear_solver"); // OPTION
  m_wordList[command].Add("warnings"); // OPTION
  m_wordList[command].Add("Gosper_in_Zeilberger"); // OPTION
  m_wordList[command].Add("trivial_solutions"); // OPTION
  m_wordList[command].Add("mod_test"); // OPTION
  m_wordList[command].Add("modular_linear_solver"); // OPTION
  m_wordList[command].Add("ev_point"); // OPTION
  m_wordList[command].Add("mod_big_prime"); // OPTION
  m_wordList[command].Add("mod_threshold"); // OPTION
  m_wordList[command].Add("days360"); // FUNCTION
  m_wordList[tmplte ].Add("days360(<year1>,<month1>,<day1>,<year2>,<month2>,<day2>)"); // OPTION
  m_wordList[command].Add("fv"); // FUNCTION
  m_wordList[tmplte ].Add("fv(<rate>,<PV>,<num>)"); // OPTION
  m_wordList[command].Add("pv"); // FUNCTION
  m_wordList[tmplte ].Add("pv(<rate>,<FV>,<num>)"); // OPTION
  m_wordList[command].Add("graph_flow"); // FUNCTION
  m_wordList[tmplte ].Add("graph_flow(<val>)"); // OPTION
  m_wordList[command].Add("annuity_pv"); // FUNCTION
  m_wordList[tmplte ].Add("annuity_pv(<rate>,<PV>,<num>)"); // OPTION
  m_wordList[command].Add("annuity_fv"); // FUNCTION
  m_wordList[tmplte ].Add("annuity_fv(<rate>,<FV>,<num>)"); // OPTION
  m_wordList[command].Add("geo_annuity_pv"); // FUNCTION
  m_wordList[tmplte ].Add("geo_annuity_pv(<rate>,<growing_rate>,<PV>,<num>)"); // OPTION
  m_wordList[command].Add("geo_annuity_fv"); // FUNCTION
  m_wordList[tmplte ].Add("geo_annuity_fv(<rate>,<growing_rate>,<FV>,<num>)"); // OPTION
  m_wordList[command].Add("amortization"); // FUNCTION
  m_wordList[tmplte ].Add("amortization(<rate>,<amount>,<num>)"); // OPTION
  m_wordList[command].Add("arit_amortization"); // FUNCTION
  m_wordList[tmplte ].Add("arit_amortization(<rate>,<increment>,<amount>,<num>)"); // OPTION
  m_wordList[command].Add("geo_amortization"); // FUNCTION
  m_wordList[tmplte ].Add("geo_amortization(<rate>,<growing_rate>,<amount>,<num>)"); // OPTION
  m_wordList[command].Add("saving"); // FUNCTION
  m_wordList[tmplte ].Add("saving(<rate>,<amount>,<num>)"); // OPTION
  m_wordList[command].Add("npv"); // FUNCTION
  m_wordList[tmplte ].Add("npv(<rate>,<val>)"); // OPTION
  m_wordList[command].Add("irr"); // FUNCTION
  m_wordList[tmplte ].Add("irr(<val>,<IO>)"); // OPTION
  m_wordList[command].Add("benefit_cost"); // FUNCTION
  m_wordList[tmplte ].Add("benefit_cost(<rate>,<input>,<output>)"); // OPTION
  m_wordList[command].Add("sierpinskiale"); // FUNCTION
  m_wordList[tmplte ].Add("sierpinskiale(<n>)"); // OPTION
  m_wordList[command].Add("treefale"); // FUNCTION
  m_wordList[tmplte ].Add("treefale(<n>)"); // OPTION
  m_wordList[command].Add("fernfale"); // FUNCTION
  m_wordList[tmplte ].Add("fernfale(<n>)"); // OPTION
  m_wordList[command].Add("mandelbrot_set"); // FUNCTION
  m_wordList[tmplte ].Add("mandelbrot_set(<x>, <y>)"); // OPTION
  m_wordList[command].Add("julia_set"); // FUNCTION
  m_wordList[tmplte ].Add("julia_set(<x>, <y>)"); // OPTION
  m_wordList[command].Add("julia_parameter"); // OPTION
  m_wordList[command].Add("julia_sin"); // FUNCTION
  m_wordList[tmplte ].Add("julia_sin(<x>, <y>)"); // OPTION
  m_wordList[command].Add("snowmap"); // FUNCTION
  m_wordList[tmplte ].Add("snowmap(<ent>, <nn>)"); // OPTION
  m_wordList[command].Add("hilbertmap"); // FUNCTION
  m_wordList[tmplte ].Add("hilbertmap(<nn>)"); // OPTION
  m_wordList[command].Add("sierpinskimap"); // FUNCTION
  m_wordList[tmplte ].Add("sierpinskimap(<nn>)"); // OPTION
  m_wordList[command].Add("extra_integration_methods"); // OPTION
  m_wordList[command].Add("extra_definite_integration_methods"); // OPTION
  m_wordList[command].Add("intfugudu"); // FUNCTION
  m_wordList[tmplte ].Add("intfugudu(<e>, <x>)"); // OPTION
  m_wordList[command].Add("signum_to_abs"); // FUNCTION
  m_wordList[tmplte ].Add("signum_to_abs(<e>)"); // OPTION
  m_wordList[command].Add("convert_to_signum"); // FUNCTION
  m_wordList[tmplte ].Add("convert_to_signum(<e>)"); // OPTION
  m_wordList[command].Add("complex_number_p"); // FUNCTION
  m_wordList[tmplte ].Add("complex_number_p(<x>)"); // OPTION
  m_wordList[command].Add("compose_functions"); // FUNCTION
  m_wordList[tmplte ].Add("compose_functions(<l>)"); // OPTION
  m_wordList[command].Add("dfloat"); // FUNCTION
  m_wordList[tmplte ].Add("dfloat(<x>)"); // OPTION
  m_wordList[command].Add("elim"); // FUNCTION
  m_wordList[tmplte ].Add("elim(<l>, <x>)"); // OPTION
  m_wordList[command].Add("elim_allbut"); // FUNCTION
  m_wordList[tmplte ].Add("elim_allbut(<l>, <x>)"); // OPTION
  m_wordList[command].Add("eliminate_using"); // FUNCTION
  m_wordList[tmplte ].Add("eliminate_using(<l>, <e>, <x>)"); // OPTION
  m_wordList[command].Add("fourier_elim"); // FUNCTION
  m_wordList[tmplte ].Add("fourier_elim([<eq1>, <eq2>, <...>], [<var1>, <var>, <...>])"); // OPTION
  m_wordList[command].Add("isreal_p"); // FUNCTION
  m_wordList[tmplte ].Add("isreal_p(<e>)"); // OPTION
  m_wordList[command].Add("new_variable"); // FUNCTION
  m_wordList[tmplte ].Add("new_variable(<type>)"); // OPTION
  m_wordList[command].Add("parg"); // FUNCTION
  m_wordList[tmplte ].Add("parg(<x>)"); // OPTION
  m_wordList[command].Add("real_imagpart_to_conjugate"); // FUNCTION
  m_wordList[tmplte ].Add("real_imagpart_to_conjugate(<e>)"); // OPTION
  m_wordList[command].Add("rectform_log_if_constant"); // FUNCTION
  m_wordList[tmplte ].Add("rectform_log_if_constant(<e>)"); // OPTION
  m_wordList[command].Add("simp_inequality"); // FUNCTION
  m_wordList[tmplte ].Add("simp_inequality(<e>)"); // OPTION
  m_wordList[command].Add("standardize_inverse_trig"); // FUNCTION
  m_wordList[tmplte ].Add("standardize_inverse_trig(<e>)"); // OPTION
  m_wordList[command].Add("to_poly"); // FUNCTION
  m_wordList[tmplte ].Add("to_poly(<e>, <l>)"); // OPTION
  m_wordList[command].Add("to_poly_solve"); // FUNCTION
  m_wordList[tmplte ].Add("to_poly_solve(<e>, <l>, <[options]>)"); // OPTION
  m_wordList[unit   ].Add("A"); // OPTION
  m_wordList[unit   ].Add("acre"); // OPTION
  m_wordList[unit   ].Add("amp"); // OPTION
  m_wordList[unit   ].Add("ampere"); // OPTION
  m_wordList[unit   ].Add("astronomical_unit"); // OPTION
  m_wordList[unit   ].Add("AU"); // OPTION
  m_wordList[unit   ].Add("becquerel"); // OPTION
  m_wordList[unit   ].Add("Bq"); // OPTION
  m_wordList[unit   ].Add("Btu"); // OPTION
  m_wordList[unit   ].Add("C"); // OPTION
  m_wordList[unit   ].Add("candela"); // OPTION
  m_wordList[unit   ].Add("cfm"); // OPTION
  m_wordList[unit   ].Add("cm"); // OPTION
  m_wordList[unit   ].Add("coulomb"); // OPTION
  m_wordList[unit   ].Add("cup"); // OPTION
  m_wordList[unit   ].Add("day"); // OPTION
  m_wordList[unit   ].Add("F"); // OPTION
  m_wordList[unit   ].Add("fA"); // OPTION
  m_wordList[unit   ].Add("farad"); // OPTION
  m_wordList[unit   ].Add("fC"); // OPTION
  m_wordList[unit   ].Add("feet"); // OPTION
  m_wordList[unit   ].Add("fF"); // OPTION
  m_wordList[unit   ].Add("fg"); // OPTION
  m_wordList[unit   ].Add("fH"); // OPTION
  m_wordList[unit   ].Add("fHz"); // OPTION
  m_wordList[unit   ].Add("fJ"); // OPTION
  m_wordList[unit   ].Add("fK"); // OPTION
  m_wordList[unit   ].Add("fl_oz"); // OPTION
  m_wordList[unit   ].Add("fluid_ounce"); // OPTION
  m_wordList[unit   ].Add("fm"); // OPTION
  m_wordList[unit   ].Add("fmol"); // OPTION
  m_wordList[unit   ].Add("fN"); // OPTION
  m_wordList[unit   ].Add("fOhm"); // OPTION
  m_wordList[unit   ].Add("foot"); // OPTION
  m_wordList[unit   ].Add("fPa"); // OPTION
  m_wordList[unit   ].Add("fs"); // OPTION
  m_wordList[unit   ].Add("fS"); // OPTION
  m_wordList[unit   ].Add("ft"); // OPTION
  m_wordList[unit   ].Add("fT"); // OPTION
  m_wordList[unit   ].Add("fV"); // OPTION
  m_wordList[unit   ].Add("fW"); // OPTION
  m_wordList[unit   ].Add("fWb"); // OPTION
  m_wordList[unit   ].Add("g"); // OPTION
  m_wordList[unit   ].Add("GA"); // OPTION
  m_wordList[unit   ].Add("gallon"); // OPTION
  m_wordList[unit   ].Add("GC"); // OPTION
  m_wordList[unit   ].Add("GF"); // OPTION
  m_wordList[unit   ].Add("Gg"); // OPTION
  m_wordList[unit   ].Add("GH"); // OPTION
  m_wordList[unit   ].Add("GHz"); // OPTION
  m_wordList[unit   ].Add("gill"); // OPTION
  m_wordList[unit   ].Add("GJ"); // OPTION
  m_wordList[unit   ].Add("GK"); // OPTION
  m_wordList[unit   ].Add("Gm"); // OPTION
  m_wordList[unit   ].Add("Gmol"); // OPTION
  m_wordList[unit   ].Add("GN"); // OPTION
  m_wordList[unit   ].Add("GOhm"); // OPTION
  m_wordList[unit   ].Add("GPa"); // OPTION
  m_wordList[unit   ].Add("grain"); // OPTION
  m_wordList[unit   ].Add("gram"); // OPTION
  m_wordList[unit   ].Add("gray"); // OPTION
  m_wordList[unit   ].Add("Gs"); // OPTION
  m_wordList[unit   ].Add("GS"); // OPTION
  m_wordList[unit   ].Add("GT"); // OPTION
  m_wordList[unit   ].Add("GV"); // OPTION
  m_wordList[unit   ].Add("GW"); // OPTION
  m_wordList[unit   ].Add("GWb"); // OPTION
  m_wordList[unit   ].Add("Gy"); // OPTION
  m_wordList[unit   ].Add("H"); // OPTION
  m_wordList[unit   ].Add("ha"); // OPTION
  m_wordList[unit   ].Add("hectare"); // OPTION
  m_wordList[unit   ].Add("henry"); // OPTION
  m_wordList[unit   ].Add("hertz"); // OPTION
  m_wordList[unit   ].Add("horsepower"); // OPTION
  m_wordList[unit   ].Add("hour"); // OPTION
  m_wordList[unit   ].Add("hp"); // OPTION
  m_wordList[unit   ].Add("Hz"); // OPTION
  m_wordList[unit   ].Add("inch"); // OPTION
  m_wordList[unit   ].Add("J"); // OPTION
  m_wordList[unit   ].Add("joule"); // OPTION
  m_wordList[unit   ].Add("julian_year"); // OPTION
  m_wordList[unit   ].Add("K"); // OPTION
  m_wordList[unit   ].Add("kA"); // OPTION
  m_wordList[unit   ].Add("kat"); // OPTION
  m_wordList[unit   ].Add("katal"); // OPTION
  m_wordList[unit   ].Add("kC"); // OPTION
  m_wordList[unit   ].Add("kelvin"); // OPTION
  m_wordList[unit   ].Add("kF"); // OPTION
  m_wordList[unit   ].Add("kg"); // OPTION
  m_wordList[unit   ].Add("kH"); // OPTION
  m_wordList[unit   ].Add("kHz"); // OPTION
  m_wordList[unit   ].Add("kilogram"); // OPTION
  m_wordList[unit   ].Add("kilometer"); // OPTION
  m_wordList[unit   ].Add("kJ"); // OPTION
  m_wordList[unit   ].Add("kK"); // OPTION
  m_wordList[unit   ].Add("km"); // OPTION
  m_wordList[unit   ].Add("kmol"); // OPTION
  m_wordList[unit   ].Add("kN"); // OPTION
  m_wordList[unit   ].Add("kOhm"); // OPTION
  m_wordList[unit   ].Add("kPa"); // OPTION
  m_wordList[unit   ].Add("ks"); // OPTION
  m_wordList[unit   ].Add("kS"); // OPTION
  m_wordList[unit   ].Add("kT"); // OPTION
  m_wordList[unit   ].Add("kV"); // OPTION
  m_wordList[unit   ].Add("kW"); // OPTION
  m_wordList[unit   ].Add("kWb"); // OPTION
  m_wordList[unit   ].Add("l"); // OPTION
  m_wordList[unit   ].Add("lbf"); // OPTION
  m_wordList[unit   ].Add("lbm"); // OPTION
  m_wordList[unit   ].Add("light_year"); // OPTION
  m_wordList[unit   ].Add("liter"); // OPTION
  m_wordList[unit   ].Add("lumen"); // OPTION
  m_wordList[unit   ].Add("lux"); // OPTION
  m_wordList[unit   ].Add("m"); // OPTION
  m_wordList[unit   ].Add("mA"); // OPTION
  m_wordList[unit   ].Add("MA"); // OPTION
  m_wordList[unit   ].Add("mC"); // OPTION
  m_wordList[unit   ].Add("MC"); // OPTION
  m_wordList[unit   ].Add("meter"); // OPTION
  m_wordList[unit   ].Add("metric_ton"); // OPTION
  m_wordList[unit   ].Add("mF"); // OPTION
  m_wordList[unit   ].Add("MF"); // OPTION
  m_wordList[unit   ].Add("mg"); // OPTION
  m_wordList[unit   ].Add("Mg"); // OPTION
  m_wordList[unit   ].Add("mH"); // OPTION
  m_wordList[unit   ].Add("MH"); // OPTION
  m_wordList[unit   ].Add("mHz"); // OPTION
  m_wordList[unit   ].Add("MHz"); // OPTION
  m_wordList[unit   ].Add("microA"); // OPTION
  m_wordList[unit   ].Add("microC"); // OPTION
  m_wordList[unit   ].Add("microF"); // OPTION
  m_wordList[unit   ].Add("microg"); // OPTION
  m_wordList[unit   ].Add("microgram"); // OPTION
  m_wordList[unit   ].Add("microH"); // OPTION
  m_wordList[unit   ].Add("microHz"); // OPTION
  m_wordList[unit   ].Add("microJ"); // OPTION
  m_wordList[unit   ].Add("microK"); // OPTION
  m_wordList[unit   ].Add("microm"); // OPTION
  m_wordList[unit   ].Add("micrometer"); // OPTION
  m_wordList[unit   ].Add("micron"); // OPTION
  m_wordList[unit   ].Add("microN"); // OPTION
  m_wordList[unit   ].Add("microOhm"); // OPTION
  m_wordList[unit   ].Add("microPa"); // OPTION
  m_wordList[unit   ].Add("micros"); // OPTION
  m_wordList[unit   ].Add("microS"); // OPTION
  m_wordList[unit   ].Add("microsecond"); // OPTION
  m_wordList[unit   ].Add("microT"); // OPTION
  m_wordList[unit   ].Add("microV"); // OPTION
  m_wordList[unit   ].Add("microW"); // OPTION
  m_wordList[unit   ].Add("microWb"); // OPTION
  m_wordList[unit   ].Add("mile"); // OPTION
  m_wordList[unit   ].Add("minute"); // OPTION
  m_wordList[unit   ].Add("mJ"); // OPTION
  m_wordList[unit   ].Add("MJ"); // OPTION
  m_wordList[unit   ].Add("mK"); // OPTION
  m_wordList[unit   ].Add("MK"); // OPTION
  m_wordList[unit   ].Add("ml"); // OPTION
  m_wordList[unit   ].Add("mm"); // OPTION
  m_wordList[unit   ].Add("Mm"); // OPTION
  m_wordList[unit   ].Add("mmol"); // OPTION
  m_wordList[unit   ].Add("Mmol"); // OPTION
  m_wordList[unit   ].Add("mN"); // OPTION
  m_wordList[unit   ].Add("MN"); // OPTION
  m_wordList[unit   ].Add("mOhm"); // OPTION
  m_wordList[unit   ].Add("MOhm"); // OPTION
  m_wordList[unit   ].Add("mol"); // OPTION
  m_wordList[unit   ].Add("mole"); // OPTION
  m_wordList[unit   ].Add("month"); // OPTION
  m_wordList[unit   ].Add("mPa"); // OPTION
  m_wordList[unit   ].Add("MPa"); // OPTION
  m_wordList[unit   ].Add("ms"); // OPTION
  m_wordList[unit   ].Add("mS"); // OPTION
  m_wordList[unit   ].Add("Ms"); // OPTION
  m_wordList[unit   ].Add("MS"); // OPTION
  m_wordList[unit   ].Add("mT"); // OPTION
  m_wordList[unit   ].Add("MT"); // OPTION
  m_wordList[unit   ].Add("mV"); // OPTION
  m_wordList[unit   ].Add("MV"); // OPTION
  m_wordList[unit   ].Add("mW"); // OPTION
  m_wordList[unit   ].Add("MW"); // OPTION
  m_wordList[unit   ].Add("mWb"); // OPTION
  m_wordList[unit   ].Add("MWb"); // OPTION
  m_wordList[unit   ].Add("N"); // OPTION
  m_wordList[unit   ].Add("nA"); // OPTION
  m_wordList[unit   ].Add("nC"); // OPTION
  m_wordList[unit   ].Add("newton"); // OPTION
  m_wordList[unit   ].Add("nF"); // OPTION
  m_wordList[unit   ].Add("ng"); // OPTION
  m_wordList[unit   ].Add("nH"); // OPTION
  m_wordList[unit   ].Add("nHz"); // OPTION
  m_wordList[unit   ].Add("nJ"); // OPTION
  m_wordList[unit   ].Add("nK"); // OPTION
  m_wordList[unit   ].Add("nm"); // OPTION
  m_wordList[unit   ].Add("nmol"); // OPTION
  m_wordList[unit   ].Add("nN"); // OPTION
  m_wordList[unit   ].Add("nOhm"); // OPTION
  m_wordList[unit   ].Add("nPa"); // OPTION
  m_wordList[unit   ].Add("ns"); // OPTION
  m_wordList[unit   ].Add("nS"); // OPTION
  m_wordList[unit   ].Add("nT"); // OPTION
  m_wordList[unit   ].Add("nV"); // OPTION
  m_wordList[unit   ].Add("nW"); // OPTION
  m_wordList[unit   ].Add("nWb"); // OPTION
  m_wordList[unit   ].Add("ohm"); // OPTION
  m_wordList[unit   ].Add("Ohm"); // OPTION
  m_wordList[unit   ].Add("ounce"); // OPTION
  m_wordList[unit   ].Add("oz"); // OPTION
  m_wordList[unit   ].Add("pA"); // OPTION
  m_wordList[unit   ].Add("Pa"); // OPTION
  m_wordList[unit   ].Add("parsec"); // OPTION
  m_wordList[unit   ].Add("pascal"); // OPTION
  m_wordList[unit   ].Add("pc"); // OPTION
  m_wordList[unit   ].Add("pC"); // OPTION
  m_wordList[unit   ].Add("pF"); // OPTION
  m_wordList[unit   ].Add("pg"); // OPTION
  m_wordList[unit   ].Add("pH"); // OPTION
  m_wordList[unit   ].Add("pHz"); // OPTION
  m_wordList[unit   ].Add("pint"); // OPTION
  m_wordList[unit   ].Add("pJ"); // OPTION
  m_wordList[unit   ].Add("pK"); // OPTION
  m_wordList[unit   ].Add("pm"); // OPTION
  m_wordList[unit   ].Add("pmol"); // OPTION
  m_wordList[unit   ].Add("pN"); // OPTION
  m_wordList[unit   ].Add("pOhm"); // OPTION
  m_wordList[unit   ].Add("pound_force"); // OPTION
  m_wordList[unit   ].Add("pound_mass"); // OPTION
  m_wordList[unit   ].Add("pPa"); // OPTION
  m_wordList[unit   ].Add("ps"); // OPTION
  m_wordList[unit   ].Add("pS"); // OPTION
  m_wordList[unit   ].Add("psi"); // OPTION
  m_wordList[unit   ].Add("pT"); // OPTION
  m_wordList[unit   ].Add("pV"); // OPTION
  m_wordList[unit   ].Add("pW"); // OPTION
  m_wordList[unit   ].Add("pWb"); // OPTION
  m_wordList[unit   ].Add("quart"); // OPTION
  m_wordList[unit   ].Add("R"); // OPTION
  m_wordList[unit   ].Add("rod"); // OPTION
  m_wordList[unit   ].Add("s"); // OPTION
  m_wordList[unit   ].Add("S"); // OPTION
  m_wordList[unit   ].Add("second"); // OPTION
  m_wordList[unit   ].Add("short_ton"); // OPTION
  m_wordList[unit   ].Add("siemens"); // OPTION
  m_wordList[unit   ].Add("sievert"); // OPTION
  m_wordList[unit   ].Add("slug"); // OPTION
  m_wordList[unit   ].Add("Sv"); // OPTION
  m_wordList[unit   ].Add("T"); // OPTION
  m_wordList[unit   ].Add("tablespoon"); // OPTION
  m_wordList[unit   ].Add("tbsp"); // OPTION
  m_wordList[unit   ].Add("teaspoon"); // OPTION
  m_wordList[unit   ].Add("tesla"); // OPTION
  m_wordList[unit   ].Add("tsp"); // OPTION
  m_wordList[unit   ].Add("V"); // OPTION
  m_wordList[unit   ].Add("volt"); // OPTION
  m_wordList[unit   ].Add("W"); // OPTION
  m_wordList[unit   ].Add("watt"); // OPTION
  m_wordList[unit   ].Add("Wb"); // OPTION
  m_wordList[unit   ].Add("weber"); // OPTION
  m_wordList[unit   ].Add("week"); // OPTION
  m_wordList[unit   ].Add("yard"); // OPTION
  m_wordList[unit   ].Add("year"); // OPTION
  m_wordList[command].Add("defstruct"); // FUNCTION
  m_wordList[tmplte ].Add("defstruct(<struct(fields)>)"); // OPTION
  m_wordList[command].Add("structures"); // OPTION
  m_wordList[command].Add("new"); // FUNCTION
  m_wordList[tmplte ].Add("new(<struct(fields)>)"); // OPTION

  /// Add wxMaxima functions
  m_wordList[command].Add(wxT("wxanimate_framerate"));
  m_wordList[command].Add(wxT("wxanimate_autoplay"));
  m_wordList[command].Add(wxT("wxplot_pngcairo"));
  m_wordList[command].Add(wxT("set_display"));
  m_wordList[command].Add(wxT("wxplot2d"));
  m_wordList[tmplte].Add(wxT("wxplot2d(<expr>,<x_range>)"));
  m_wordList[command].Add(wxT("wxplot3d"));
  m_wordList[tmplte].Add(wxT("wxplot3d(<expr>,<x_range>,<y_range>)"));
  m_wordList[command].Add(wxT("wximplicit_plot"));
  m_wordList[command].Add(wxT("wxcontour_plot"));
  m_wordList[command].Add(wxT("wxanimate"));
  m_wordList[command].Add(wxT("wxanimate_draw"));
  m_wordList[command].Add(wxT("wxanimate_draw3d"));
  m_wordList[command].Add(wxT("with_slider"));
  m_wordList[tmplte].Add(wxT("with_slider(<a_var>,<a_list>,<expr>,<x_range>)"));
  m_wordList[command].Add(wxT("with_slider_draw"));
  m_wordList[command].Add(wxT("with_slider_draw2d"));
  m_wordList[command].Add(wxT("with_slider_draw3d"));
  m_wordList[command].Add(wxT("wxdraw"));
  m_wordList[command].Add(wxT("wxdraw2d"));
  m_wordList[command].Add(wxT("wxdraw3d"));
  m_wordList[command].Add(wxT("wxfilename"));
  m_wordList[command].Add(wxT("wxhistogram"));
  m_wordList[command].Add(wxT("wxscatterplot"));
  m_wordList[command].Add(wxT("wxbarsplot"));
  m_wordList[command].Add(wxT("wxpiechart"));
  m_wordList[command].Add(wxT("wxboxplot"));
  m_wordList[command].Add(wxT("wxplot_size"));
  m_wordList[command].Add(wxT("wxdraw_list"));
  m_wordList[command].Add(wxT("wxbuild_info"));
  m_wordList[command].Add(wxT("wxbug_report"));
  m_wordList[command].Add(wxT("show_image"));
  m_wordList[tmplte].Add(wxT("show_image(<imagename>)"));
  m_wordList[command].Add(wxT("table_form"));
  m_wordList[tmplte].Add(wxT("table_form(<data>)"));
  m_wordList[tmplte].Add(wxT("table_form(<data>,<[options]>)"));
  m_wordList[command].Add(wxT("wxsubscripts"));
  m_wordList[command].Add(wxT("wxdeclare_subscripted"));
  m_wordList[tmplte].Add(wxT("wxdeclare_subscripted(<name>,<[false]>)"));
  m_wordList[command].Add(wxT("wxanimate_from_imgfiles"));
  m_wordList[tmplte].Add(wxT("wxanimate_from_imgfiles(<filename>,<[filename,...]>)"));
  m_wordList[command].Add(wxT("wxstatusbar"));
  m_wordList[tmplte].Add(wxT("wxstatusbar(<string>)"));
  m_wordList[command].Add(wxT("wxmaximaversion"));

  /// Load private symbol list (do something different on Windows).
  wxString privateList;
  privateList = Dirstructure::Get()->UserAutocompleteFile();

  if (wxFileExists(privateList))
  {
    wxTextFile priv(privateList);

    priv.Open();

    for (line = priv.GetFirstLine(); !priv.Eof(); line = priv.GetNextLine())
    {
      if (line.StartsWith(wxT("FUNCTION: ")) ||
          line.StartsWith(wxT("OPTION  : ")))
        m_wordList[command].Add(line.Mid(10));
      else if (line.StartsWith(wxT("TEMPLATE: ")))
        m_wordList[tmplte].Add(FixTemplate(line.Mid(10)));
      else if (line.StartsWith(wxT("UNIT: ")))
        m_wordList[unit].Add(FixTemplate(line.Mid(6)));
    }

    priv.Close();
  }
  
  // Prepare a list of all built-in loadable files of maxima.
  {
    GetMacFiles_includingSubdirs maximaLispIterator (m_builtInLoadFiles);
    if(m_configuration->MaximaShareDir() != wxEmptyString)
    {
      wxLogMessage(
        wxString::Format(
          _("Autocompletion: Scanning %s for loadable lisp files."),
          m_configuration->MaximaShareDir()));
      wxDir maximadir(m_configuration->MaximaShareDir());
      if(maximadir.IsOpened())
        maximadir.Traverse(maximaLispIterator);
    }
    GetMacFiles userLispIterator (m_builtInLoadFiles);
    wxDir maximauserfilesdir(Dirstructure::Get()->UserConfDir());
      wxLogMessage(
        wxString::Format(
          _("Autocompletion: Scanning %s for loadable lisp files."),
          Dirstructure::Get()->UserConfDir()));
    if(maximauserfilesdir.IsOpened())
      maximauserfilesdir.Traverse(userLispIterator);
  }
  

  // Prepare a list of all built-in demos of maxima.
  {
    GetDemoFiles_includingSubdirs maximaLispIterator (m_builtInDemoFiles);
    wxLogMessage(
      wxString::Format(
        _("Autocompletion: Scanning %s for loadable demo files."),
        m_configuration->MaximaShareDir()+"/.."));

    wxDir maximadir(m_configuration->MaximaShareDir()+"/..");
    if(maximadir.IsOpened())
      maximadir.Traverse(maximaLispIterator);
    GetDemoFiles userLispIterator (m_builtInDemoFiles);
  }
  
  m_wordList[command].Sort();
  m_wordList[tmplte].Sort();
  m_wordList[unit].Sort();
  m_builtInLoadFiles.Sort();
  m_builtInDemoFiles.Sort();
  return false;
}

void AutoComplete::UpdateDemoFiles(wxString partial, wxString maximaDir)
{
  // Remove the opening quote from the partial.
  if(partial[0] == wxT('\"'))
    partial = partial.Right(partial.Length()-1);
  
  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxT('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxT("/");
  
  // Determine if we need to add the path to maxima's current dir to the path in partial
  if(!wxFileName(partial).IsAbsolute())
  {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }
  
  // Determine the name of the directory
  if((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";

  // Remove all files from the maxima directory from the demo file list
  ClearDemofileList();

  // Add all files from the maxima directory to the demo file list
  if(partial != wxT("//"))
  {
    GetDemoFiles userLispIterator(m_wordList[demofile], prefix);
    wxDir demofilesdir(partial);
    if(demofilesdir.IsOpened())
      demofilesdir.Traverse(userLispIterator);
  }
}

void AutoComplete::UpdateGeneralFiles(wxString partial, wxString maximaDir)
{
  // Remove the opening quote from the partial.
  if(partial[0] == wxT('\"'))
    partial = partial.Right(partial.Length()-1);
  
  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxT('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxT("/");
  
  // Determine if we need to add the path to maxima's current dir to the path in partial
  if(!wxFileName(partial).IsAbsolute())
  {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }
  
  // Determine the name of the directory
  if((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";
  
  // Add all files from the maxima directory to the demo file list
  if(partial != wxT("//"))
  {
    GetGeneralFiles fileIterator(m_wordList[generalfile], prefix);
    wxDir generalfilesdir(partial);
    if(generalfilesdir.IsOpened())
      generalfilesdir.Traverse(fileIterator);
  }
}

void AutoComplete::UpdateLoadFiles(wxString partial, wxString maximaDir)
{
  // Remove the opening quote from the partial.
  if(partial[0] == wxT('\"'))
    partial = partial.Right(partial.Length()-1);
  
  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxT('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxT("/");
  
  // Determine if we need to add the path to maxima's current dir to the path in partial
  if(!wxFileName(partial).IsAbsolute())
  {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }
  
  // Determine the name of the directory
  if((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";

  // Remove all files from the maxima directory from the load file list
  ClearLoadfileList();

  // Add all files from the maxima directory to the load file list
  if(partial != wxT("//"))
  {
    GetMacFiles userLispIterator(m_wordList[loadfile], prefix);
    wxDir loadfilesdir(partial);
    if(loadfilesdir.IsOpened())
      loadfilesdir.Traverse(userLispIterator);
  }
}

/// Returns a string array with functions which start with partial.
wxArrayString AutoComplete::CompleteSymbol(wxString partial, autoCompletionType type)
{
  wxArrayString completions;
  wxArrayString perfectCompletions;
  
  if(
    ((type == AutoComplete::demofile) || (type == AutoComplete::loadfile)) &&
    (partial.EndsWith("\""))
    )
    partial = partial.Left(partial.Length() - 1);
  
  wxASSERT_MSG((type >= command) && (type <= unit), _("Bug: Autocompletion requested for unknown type of item."));
  
  if (type != tmplte)
  {
    for (size_t i = 0; i < m_wordList[type].GetCount(); i++)
    {
      if (m_wordList[type][i].StartsWith(partial) &&
          completions.Index(m_wordList[type][i]) == wxNOT_FOUND)
        completions.Add(m_wordList[type][i]);
    }
  }
  else
  {
    for (size_t i = 0; i < m_wordList[type].GetCount(); i++)
    {
      wxString templ = m_wordList[type][i];
      if (templ.StartsWith(partial))
      {
        if (completions.Index(templ) == wxNOT_FOUND)
          completions.Add(templ);
        if (templ.SubString(0, templ.Find(wxT("(")) - 1) == partial &&
            perfectCompletions.Index(templ) == wxNOT_FOUND)
          perfectCompletions.Add(templ);
      }
    }
  }

  // Add a list of words that were definied on the work sheet but that aren't
  // defined as maxima commands or functions.
  if (type == command)
  {
    WorksheetWords::iterator it;
    for (it = m_worksheetWords.begin(); it != m_worksheetWords.end(); ++it)
    {
      if (it->first.StartsWith(partial))
      {
        if (completions.Index(it->first) == wxNOT_FOUND)
        {
          completions.Add(it->first);
        }
      }
    }
  }

  completions.Sort();

  if (perfectCompletions.Count() > 0)
    return perfectCompletions;
  return completions;
}

void AutoComplete::AddSymbol(wxString fun, autoCompletionType type)
{
  /// Check for function of template
  if (fun.StartsWith(wxT("FUNCTION: ")))
  {
    fun = fun.Mid(10);
    type = command;
  }
  else if (fun.StartsWith(wxT("TEMPLATE: ")))
  {
    fun = fun.Mid(10);
    type = tmplte;
  }
  else if (fun.StartsWith(wxT("UNIT: ")))
  {
    fun = fun.Mid(6);
    type = unit;
  }

  /// Add symbols
  if ((type != tmplte) && m_wordList[type].Index(fun, true, true) == wxNOT_FOUND)
    m_wordList[type].Add(fun);

  /// Add templates - for given function and given argument count we
  /// only add one template. We count the arguments by counting '<'
  if (type == tmplte)
  {
    fun = FixTemplate(fun);
    wxString funName = fun.SubString(0, fun.Find(wxT("(")));
    long count = fun.Freq('<');
    size_t i = 0;
    for (i = 0; i < m_wordList[type].GetCount(); i++)
    {
      wxString t = m_wordList[type][i];
      if (t.StartsWith(funName) && (t.Freq('<') == count))
        break;
    }
    if (i == m_wordList[type].GetCount())
      m_wordList[type].Add(fun);
  }
}

wxString AutoComplete::FixTemplate(wxString templ)
{
  templ.Replace(wxT(" "), wxEmptyString);
  templ.Replace(wxT(",..."), wxEmptyString);

  /// This will change optional arguments
  m_args.ReplaceAll(&templ, wxT("<[\\1]>"));

  return templ;
}
