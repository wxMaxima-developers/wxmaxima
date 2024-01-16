// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2019 Gunter Königsmann    <wxMaxima@physikbuch.de>
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
  This file tells wxMaxima about known builtin commands of maxima.

  The list of builtin commands was long enough to justify splitting it into
  a separate file.
*/

#include "Autocomplete.h"

bool AutoComplete::LoadBuiltinSymbols() {
  // Add maxima functions
  // NOLINT(readability/fn_size)
  m_wordList.at(command).push_back(wxS("%and"));    // FUNCTION
  m_wordList.at(command).push_back(wxS("%if"));    // FUNCTION
  m_wordList.at(command).push_back(wxS("%or"));    // FUNCTION
  m_wordList.at(command).push_back(wxS("%rnum"));    // FUNCTION
  m_wordList.at(command).push_back(wxS("?derivsimp"));    // OPTION
  m_wordList.at(command).push_back(wxS("?room"));    // FUNCTION
  m_wordList.at(command).push_back(wxS("pathname_name")); // FUNCTION
  m_wordList.at(command).push_back(wxS("fast_linsolve")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fast_linsolve([<expr_1>, ..., <expr_m>], [<x_1>, "
                                       "..., <x_n>])"));                // OPTION
  m_wordList.at(command).push_back(wxS("guess_exact_value"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("guess_exact_value([<expr>]))")); // OPTION
  m_wordList.at(command).push_back(wxS("grobner_basis"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("grobner_basis([<expr_1>, ..., <expr_m>])")); // OPTION
  m_wordList.at(command).push_back(wxS("set_up_dot_simplifications")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "set_up_dot_simplifications(<eqns>, <check_through_degree>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("set_up_dot_simplifications(<eqns>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("declare_weights"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "declare_weights(<x_1>, <w_1>, ..., <x_n>, <w_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("nc_degree"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nc_degree(<p>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("dotsimp"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dotsimp(<f>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("fast_central_elements"));        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "fast_central_elements([<x_1>, ..., <x_n>], <n>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("check_overlaps"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("check_overlaps(<n>, <add_to_simps>)")); // OPTION
  m_wordList.at(command).push_back(wxS("mono"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mono([<x_1>, ..., <x_n>], <n>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("monomial_dimensions"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("monomial_dimensions(<n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("extract_linear_equations"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("extract_linear_equations([<p_1>, ..., <p_n>], "
                                       "[<m_1>, ..., <m_n>])"));                   // OPTION
  m_wordList.at(command).push_back(wxS("list_nc_monomials"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("list_nc_monomials([<p_1>, ..., <p_n>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("list_nc_monomials(<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("all_dotsimp_denoms"));                    // OPTION
  m_wordList.at(command).push_back(wxS("array"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("array(<name>, <dim_1>, ..., <dim_n>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "array(<name>, <type>, <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "array([<name_1>, ..., <name_m>], <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("arrayapply"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("arrayapply(<A>, [<i_1>, ..., <i_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("with_default_2d_display"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("with_default_2d_display(<cmd>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("arrayinfo"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("arrayinfo(<A>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("arraymake"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("arraymake(<A>, [<i_1>, ..., <i_n>])"));  // OPTION
  m_wordList.at(command).push_back(wxS("arrays"));                              // OPTION
  m_wordList.at(command).push_back(wxS("bashindices"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bashindices(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("fillarray"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fillarray(<A>, <B>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("listarray"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("listarray(<A>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("make_array"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("make_array(<type>, <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("rearray"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rearray(<A>, <dim_1>, ..., <dim_n>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("remarray"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remarray(<A_1>, ..., <A_n>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remarray(all)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("subvar"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("subvar(<x>, <i>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("use_fast_arrays"));                     // OPTION
  m_wordList.at(command).push_back(wxS("init_atensor"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("init_atensor(<alg_type>, <opt_dims>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("init_atensor(<alg_type>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("atensimp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("atensimp(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("adim"));                                // OPTION
  m_wordList.at(command).push_back(wxS("aform"));                               // OPTION
  m_wordList.at(command).push_back(wxS("asymbol"));                             // OPTION
  m_wordList.at(command).push_back(wxS("sf"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sf(<u>, <v>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("af"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("af(<u>, <v>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("av"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("av(<u>, <v>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("abasep"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("abasep(<v>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("augmented_lagrangian_method"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>, "
                                       "optional_args)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("augmented_lagrangian_method([<FOM>, <grad>], <xx>, "
                                       "<C>, <yy>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("augmented_lagrangian_method([<FOM>, <grad>], <xx>, "
                                       "<C>, <yy>, optional_args)")); // OPTION
  m_wordList.at(command).push_back(wxS("bode_gain"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "bode_gain(<H>, <range>, ...<plot_opts>...)")); // OPTION
  m_wordList.at(command).push_back(wxS("bode_phase"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "bode_phase(<H>, <range>, ...<plot_opts>...)"));   // OPTION
  m_wordList.at(command).push_back(wxS("run_testsuite"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("run_testsuite([<options>])")); // OPTION
  m_wordList.at(command).push_back(wxS("testsuite_files"));           // OPTION
  m_wordList.at(command).push_back(wxS("share_testsuite_files"));     // OPTION
  m_wordList.at(command).push_back(wxS("display_all"));               // OPTION
  m_wordList.at(command).push_back(wxS("display_index_separator"));   // OPTION
  m_wordList.at(command).push_back(wxS("display_known_bugs"));        // OPTION
  m_wordList.at(command).push_back(wxS("tests"));                     // OPTION
  m_wordList.at(command).push_back(wxS("time"));                      // OPTION
  m_wordList.at(command).push_back(wxS("share_tests"));               // OPTION
  m_wordList.at(command).push_back(wxS("bug_report"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bug_report()"));               // OPTION
  m_wordList.at(command).push_back(wxS("build_info"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("build_info()"));               // OPTION
  m_wordList.at(command).push_back(wxS("alias"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("alias(<new_name_1>, <old_name_1>, ..., <new_name_n>, "
                                       "<old_name_n>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("debugmode"));                              // OPTION
  m_wordList.at(command).push_back(wxS("ev"));                                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ev(<expr>, <arg_1>, ..., <arg_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("eval"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("evflag"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("evfun"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("infeval"));                                // OPTION
  m_wordList.at(command).push_back(wxS("kill"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kill(<a_1>, ..., <a_n>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(labels)"));                            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(inlabels, outlabels, linelabels)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(<n>)"));                               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill([<m>, <n>])"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(values, functions, arrays, ...)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(all)"));                               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kill(allbut (<a_1>, ..., <a_n>))"));        // OPTION
  m_wordList.at(command).push_back(wxS("labels"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("labels(<symbol>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("linenum"));                                // OPTION
  m_wordList.at(command).push_back(wxS("myoptions"));                              // OPTION
  m_wordList.at(command).push_back(wxS("nolabels"));                               // OPTION
  m_wordList.at(command).push_back(wxS("optionset"));                              // OPTION
  m_wordList.at(command).push_back(wxS("playback"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("playback()"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback(<n>)"));                           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback([<m>, <n>])"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback([<m>])"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback(input)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback(slow)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback(time)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("playback(grind)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("printprops"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("printprops(<a>, <i>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("printprops([<a_1>, ..., <a_n>], <i>)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("printprops(all, <i>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("prompt"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("quit"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quit()"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("remfunction"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remfunction(<f_1>, ..., <f_n>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remfunction(all)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("reset"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("reset()"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("showtime"));                               // OPTION
  m_wordList.at(command).push_back(wxS("to_lisp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("to_lisp()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("values"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("%e"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("%i"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("false"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("ind"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("inf"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("infinity"));                               // OPTION
  m_wordList.at(command).push_back(wxS("minf"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("%phi"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("%pi"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("true"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("und"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("zeroa"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("zerob"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("activate"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("activate(<context_1>, ..., <context_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("activecontexts"));                         // OPTION
  m_wordList.at(command).push_back(wxS("assume"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("assume(<pred_1>, ..., <pred_n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("assumescalar"));                           // OPTION
  m_wordList.at(command).push_back(wxS("assume_pos"));                             // OPTION
  m_wordList.at(command).push_back(wxS("assume_pos_pred"));                        // OPTION
  m_wordList.at(command).push_back(wxS("context"));                                // OPTION
  m_wordList.at(command).push_back(wxS("contexts"));                               // OPTION
  m_wordList.at(command).push_back(wxS("deactivate"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("deactivate(<context_1>, ..., <context_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("facts"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("facts(<item>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("facts()"));                         // OPTION
  m_wordList.at(command).push_back(wxS("features"));                       // OPTION
  m_wordList.at(command).push_back(wxS("forget"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("forget(<pred_1>, ..., <pred_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("forget(<L>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("killcontext"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "killcontext(<context_1>, ..., <context_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("newcontext"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("newcontext(<name>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("supcontext"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("supcontext(<name>, <context>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("supcontext(<name>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("contrib_ode"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contrib_ode(<eqn>, <y>, <x>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("odelin"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("odelin(<eqn>, <y>, <x>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("ode_check"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ode_check(<eqn>, <soln>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("method"));                       // OPTION
  m_wordList.at(command).push_back(wxS("%c"));                           // OPTION
  m_wordList.at(command).push_back(wxS("%k1"));                          // OPTION
  m_wordList.at(command).push_back(wxS("%k2"));                          // OPTION
  m_wordList.at(command).push_back(wxS("gauss_a"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gauss_a(<a>, <b>, <c>, <x>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("gauss_b"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gauss_b(<a>, <b>, <c>, <x>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("dgauss_a"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dgauss_a(<a>, <b>, <c>, <x>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("dgauss_b"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dgauss_b(<a>, <b>, <c>, <x>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("kummer_m"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kummer_m(<a>, <b>, <x>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("kummer_u"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kummer_u(<a>, <b>, <x>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("dkummer_m"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dkummer_m(<a>, <b>, <x>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("dkummer_u"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dkummer_u(<a>, <b>, <x>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("csetup"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("csetup()"));                      // OPTION
  m_wordList.at(command).push_back(wxS("cmetric"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cmetric(<dis>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("cmetric()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("ct_coordsys"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "ct_coordsys(<coordinate_system>, <extra_arg>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ct_coordsys(<coordinate_system>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("init_ctensor"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("init_ctensor()"));                          // OPTION
  m_wordList.at(command).push_back(wxS("christof"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("christof(<dis>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ricci"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ricci(<dis>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("uricci"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("uricci(<dis>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("scurvature"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scurvature()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("einstein"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("einstein(<dis>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("leinstein"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("leinstein(<dis>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("riemann"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("riemann(<dis>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("lriemann"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lriemann(<dis>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("uriemann"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("uriemann(<dis>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("rinvariant"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rinvariant()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("weyl"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("weyl(<dis>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ctaylor"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ctaylor()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("frame_bracket"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("frame_bracket(<fr>, <fri>, <diagframe>)")); // OPTION
  m_wordList.at(command).push_back(wxS("nptetrad"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nptetrad()"));                              // OPTION
  m_wordList.at(command).push_back(wxS("psi"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("psi(<dis>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("petrov"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("petrov()"));                                // OPTION
  m_wordList.at(command).push_back(wxS("contortion"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contortion(<tr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("nonmetricity"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nonmetricity(<nm>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("ctransform"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ctransform(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("findde"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("findde(<A>, <n>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("cograd"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cograd()"));                                // OPTION
  m_wordList.at(command).push_back(wxS("contragrad"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contragrad()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("dscalar"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dscalar()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("checkdiv"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("checkdiv()"));                              // OPTION
  m_wordList.at(command).push_back(wxS("cgeodesic"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cgeodesic(<dis>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("bdvac"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bdvac(<f>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("invariant1"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("invariant1()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("invariant2"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("invariant2()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("bimetric"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bimetric()"));                              // OPTION
  m_wordList.at(command).push_back(wxS("diagmatrixp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("diagmatrixp(<M>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("symmetricp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("symmetricp(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ntermst"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ntermst(<f>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("cdisplay"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdisplay(<ten>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("deleten"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("deleten(<L>, <n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("dim"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("diagmetric"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ctrgsimp"));                               // OPTION
  m_wordList.at(command).push_back(wxS("cframe_flag"));                            // OPTION
  m_wordList.at(command).push_back(wxS("ctorsion_flag"));                          // OPTION
  m_wordList.at(command).push_back(wxS("cnonmet_flag"));                           // OPTION
  m_wordList.at(command).push_back(wxS("ctayswitch"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ctayvar"));                                // OPTION
  m_wordList.at(command).push_back(wxS("ctaypov"));                                // OPTION
  m_wordList.at(command).push_back(wxS("ctaypt"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("gdet"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("ratchristof"));                            // OPTION
  m_wordList.at(command).push_back(wxS("rateinstein"));                            // OPTION
  m_wordList.at(command).push_back(wxS("ratriemann"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ratweyl"));                                // OPTION
  m_wordList.at(command).push_back(wxS("lfg"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("ufg"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("riem"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("lriem"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("uriem"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("ric"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("uric"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("lg"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("ug"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("weyl"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("fb"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("kinvariant"));                             // OPTION
  m_wordList.at(command).push_back(wxS("np"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("npi"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("tr"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("kt"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("nm"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("nmc"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("tensorkill"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ct_coords"));                              // OPTION
  m_wordList.at(command).push_back(wxS("refcheck"));                               // OPTION
  m_wordList.at(command).push_back(wxS("setcheck"));                               // OPTION
  m_wordList.at(command).push_back(wxS("setcheckbreak"));                          // OPTION
  m_wordList.at(command).push_back(wxS("setval"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("timer"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("timer(<f_1>, ..., <f_n>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("timer(all)"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("timer()"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("untimer"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("untimer(<f_1>, ..., <f_n>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("untimer()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("timer_devalue"));                          // OPTION
  m_wordList.at(command).push_back(wxS("timer_info"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("timer_info(<f_1>, ..., <f_n>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("timer_info()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("trace"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trace(<f_1>, ..., <f_n>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("trace(all)"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("trace()"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("trace_options"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "trace_options(<f>, <option_1>, ..., <option_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("trace_options(<f>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("untrace"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("untrace(<f_1>, ..., <f_n>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("untrace()"));                    // OPTION
  m_wordList.at(command).push_back(wxS("%ibes"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%ibes[<n>](<x>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("%j"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%j[<n>](<x>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("%k"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%k[<n>](<x>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("%y"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%y[<n>](<x>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("airy"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("airy(<x>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("bessel"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bessel(<z>, <a>) "));            // OPTION
  m_wordList.at(command).push_back(wxS("expint"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expint(<z>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("g0"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("g0(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("g1"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("g1(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("gn"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gn(<x>, <n>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("gauss"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gauss(<mean>, <sd>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("i0"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("i0(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("i1"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("i1(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("in"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("in(<x>, <n>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("j0"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("j0(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("j1"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("j1(<x>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("jn"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jn(<x>, <n>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("continuous_freq"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("continuous_freq(<list>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("continuous_freq(<list>, <m>)")); // OPTION
  m_wordList.at(command).push_back(wxS("discrete_freq"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("discrete_freq(<list>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("subsample"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "subsample(<data_matrix>, <predicate_function>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("subsample(<data_matrix>, <predicate_function>, "
                                       "<col_num1>, <col_num2>, ...)"));     // OPTION
  m_wordList.at(command).push_back(wxS("mean"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean(<list>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("mean(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("var"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var(<list>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("var(<matrix>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("var1"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var1(<list>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("var1(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("std"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std(<list>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("std(<matrix>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("std1"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std1(<list>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("std1(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("noncentral_moment"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("noncentral_moment(<list>, <k>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("noncentral_moment(<matrix>, <k>)")); // OPTION
  m_wordList.at(command).push_back(wxS("central_moment"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("central_moment(<list>, <k>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("central_moment(<matrix>, <k>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("cv"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cv(<list>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("cv(<matrix>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("smin"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("smin(<list>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("smin(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("smax"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("smax(<list>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("smax(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("range"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("range(<list>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("range(<matrix>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("quantile"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile(<list>, <p>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quantile(<matrix>, <p>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("median"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("median(<list>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("median(<matrix>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("qrange"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("qrange(<list>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("qrange(<matrix>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("mean_deviation"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_deviation(<list>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("mean_deviation(<matrix>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("median_deviation"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("median_deviation(<list>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("median_deviation(<matrix>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("harmonic_mean"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("harmonic_mean(<list>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("harmonic_mean(<matrix>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("geometric_mean"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("geometric_mean(<list>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("geometric_mean(<matrix>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis(<list>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis(<matrix>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("skewness"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness(<list>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("skewness(<matrix>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pearson_skewness"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pearson_skewness(<list>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("pearson_skewness(<matrix>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("quartile_skewness"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quartile_skewness(<list>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quartile_skewness(<matrix>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("cov"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cov(<matrix>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("cov1"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cov1(<matrix>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("global_variances"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("global_variances(<matrix>)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "global_variances(<matrix>, <logical_value>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("cor"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cor(<matrix>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("cor(<matrix>, <logical_value>)")); // OPTION
  m_wordList.at(command).push_back(wxS("list_correlations"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("list_correlations(<matrix>)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "list_correlations(<matrix>, <logical_value>)")); // OPTION
  m_wordList.at(command).push_back(wxS("histogram"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("histogram(<list>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "histogram(<list>, <option_1>, <option_2>, ...)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("histogram(<one_column_matrix>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "histogram(<one_column_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("histogram(<one_row_matrix>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "histogram(<one_row_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("scatterplot"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scatterplot(<list>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "scatterplot(<list>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("scatterplot(<matrix>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "scatterplot(<matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("barsplot"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "barsplot(<data1>, <data2>, ..., <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("piechart"));        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("piechart(<list>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "piechart(<list>, <option_1>, <option_2>, ...)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("piechart(<one_column_matrix>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "piechart(<one_column_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("piechart(<one_row_matrix>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "piechart(<one_row_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("boxplot"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("boxplot(<data>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "boxplot(<data>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("diag"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("diag(<lm>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("JF"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("JF(<lambda>,<n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jordan"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jordan(<mat>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("dispJordan"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dispJordan(<l>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("minimalPoly"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minimalPoly(<l>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("ModeMatrix"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ModeMatrix(<A>,<l>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("mat_function"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_function(<f>,<mat>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("bc2"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "bc2(<solution>, <xval1>, <yval1>, <xval2>, <yval2>)")); // OPTION
  m_wordList.at(command).push_back(wxS("desolve"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("desolve(<eqn>, <x>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "desolve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])"));      // OPTION
  m_wordList.at(command).push_back(wxS("ic1"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ic1(<solution>, <xval>, <yval>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("ic2"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ic2(<solution>, <xval>, <yval>, <dval>)")); // OPTION
  m_wordList.at(command).push_back(wxS("ode2"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ode2(<eqn>, <dvar>, <ivar>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("antid"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("antid(<expr>, <x>, <u(x)>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("antidiff"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("antidiff(<expr>, <x>, <u>(<x>))"));         // OPTION
  m_wordList.at(command).push_back(wxS("atomgrad"));                               // OPTION
  m_wordList.at(command).push_back(wxS("atvalue"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "atvalue(<expr>, [<x_1> = <a_1>, ..., <x_m> = <a_m>], <c>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("atvalue(<expr>, <x_1> = <a_1>, <c>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("cartan"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cartan-"));                                // OPTION
  m_wordList.at(command).push_back(wxS("del"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("del(<x>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("delta"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("delta(<t>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("dependencies"));                          // OPTION
  m_wordList.at(command).push_back(wxS("depends"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("depends(<f_1>, <x_1>, ..., <f_n>, <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("derivabbrev"));                             // OPTION
  m_wordList.at(command).push_back(wxS("derivdegree"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("derivdegree(<expr>, <y>, <x>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("derivlist"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("derivlist(<var_1>, ..., <var_k>)")); // OPTION
  m_wordList.at(command).push_back(wxS("derivsubst"));                      // OPTION
  m_wordList.at(command).push_back(wxS("diff"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "diff(<expr>, <x_1>, <n_1>, ..., <x_m>, <n_m>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("diff(<expr>, <x>, <n>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("diff(<expr>, <x>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("diff(<expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("diff"));                      // OPTION
  m_wordList.at(command).push_back(wxS("dscalar"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dscalar(<f>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("express"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("express(<expr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("gradef"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "gradef(<f>(<x_1>, ..., <x_n>), <g_1>, ..., <g_m>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("gradef(<a>, <x>, <expr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("gradefs"));                                // OPTION
  m_wordList.at(command).push_back(wxS("laplace"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("laplace(<expr>, <t>, <s>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pdf_normal"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_normal(<x>,<m>,<s>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("cdf_normal"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_normal(<x>,<m>,<s>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("quantile_normal"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_normal(<q>,<m>,<s>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("mean_normal"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_normal(<m>,<s>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("var_normal"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_normal(<m>,<s>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("std_normal"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_normal(<m>,<s>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("skewness_normal"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_normal(<m>,<s>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_normal"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_normal(<m>,<s>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("random_normal"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_normal(<m>,<s>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_normal(<m>,<s>,<n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("pdf_student_t"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_student_t(<x>,<n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("cdf_student_t"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_student_t(<x>,<n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("quantile_student_t"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_student_t(<q>,<n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("mean_student_t"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_student_t(<n>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("var_student_t"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_student_t(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("std_student_t"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_student_t(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("skewness_student_t"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_student_t(<n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_student_t"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_student_t(<n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("random_student_t"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_student_t(<n>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_student_t(<n>,<m>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pdf_noncentral_student_t"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_noncentral_student_t(<x>,<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("cdf_noncentral_student_t"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_noncentral_student_t(<x>,<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("quantile_noncentral_student_t"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "quantile_noncentral_student_t(<q>,<n>,<ncp>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("mean_noncentral_student_t"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("var_noncentral_student_t"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_noncentral_student_t(<n>,<ncp>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("std_noncentral_student_t"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_noncentral_student_t(<n>,<ncp>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("skewness_noncentral_student_t"));       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_noncentral_student_t")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("random_noncentral_student_t"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "random_noncentral_student_t(<n>,<ncp>,<m>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("pdf_chi2"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_chi2(<x>,<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("cdf_chi2"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_chi2(<x>,<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("quantile_chi2"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_chi2(<q>,<n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("mean_chi2"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_chi2(<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("var_chi2"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_chi2(<n>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("std_chi2"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_chi2(<n>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("skewness_chi2"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_chi2(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_chi2"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_chi2(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("random_chi2"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_chi2(<n>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_chi2(<n>,<m>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("pdf_noncentral_chi2"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_noncentral_chi2(<x>,<n>,<ncp>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("cdf_noncentral_chi2"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_noncentral_chi2(<x>,<n>,<ncp>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("quantile_noncentral_chi2"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_noncentral_chi2(<q>,<n>,<ncp>)")); // OPTION
  m_wordList.at(command).push_back(wxS("mean_noncentral_chi2"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_noncentral_chi2(<n>,<ncp>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("var_noncentral_chi2"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_noncentral_chi2(<n>,<ncp>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("std_noncentral_chi2"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_noncentral_chi2(<n>,<ncp>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("skewness_noncentral_chi2"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_noncentral_chi2(<n>,<ncp>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_noncentral_chi2"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_noncentral_chi2(<n>,<ncp>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("random_noncentral_chi2"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_noncentral_chi2(<n>,<ncp>)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_noncentral_chi2(<n>,<ncp>,<m>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("pdf_f"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_f(<x>,<m>,<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("cdf_f"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_f(<x>,<m>,<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("quantile_f"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_f(<q>,<m>,<n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("mean_f"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_f(<m>,<n>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("var_f"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_f(<m>,<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("std_f"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_f(<m>,<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("skewness_f"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_f(<m>,<n>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_f"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_f(<m>,<n>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("random_f"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_f(<m>,<n>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_f(<m>,<n>,<k>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("pdf_exp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_exp(<x>,<m>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("cdf_exp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_exp(<x>,<m>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("quantile_exp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_exp(<q>,<m>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("mean_exp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_exp(<m>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("var_exp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_exp(<m>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("std_exp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_exp(<m>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("skewness_exp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_exp(<m>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_exp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_exp(<m>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("random_exp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_exp(<m>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_exp(<m>,<k>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("pdf_lognormal"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_lognormal(<x>,<m>,<s>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("cdf_lognormal"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_lognormal(<x>,<m>,<s>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("quantile_lognormal"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_lognormal(<q>,<m>,<s>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("mean_lognormal"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_lognormal(<m>,<s>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("var_lognormal"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_lognormal(<m>,<s>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("std_lognormal"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_lognormal(<m>,<s>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("skewness_lognormal"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_lognormal(<m>,<s>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_lognormal"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_lognormal(<m>,<s>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("random_lognormal"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_lognormal(<m>,<s>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_lognormal(<m>,<s>,<n>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("pdf_gamma"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_gamma(<x>,<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("cdf_gamma"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_gamma(<x>,<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("quantile_gamma"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_gamma(<q>,<a>,<b>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("mean_gamma"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_gamma(<a>,<b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("var_gamma"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_gamma(<a>,<b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("std_gamma"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_gamma(<a>,<b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("skewness_gamma"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_gamma(<a>,<b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_gamma"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_gamma(<a>,<b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("random_gamma"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_gamma(<a>,<b>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_gamma(<a>,<b>,<n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pdf_beta"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_beta(<x>,<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("cdf_beta"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_beta(<x>,<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("quantile_beta"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_beta(<q>,<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("mean_beta"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_beta(<a>,<b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("var_beta"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_beta(<a>,<b>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("std_beta"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_beta(<a>,<b>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("skewness_beta"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_beta(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_beta"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_beta(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("random_beta"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_beta(<a>,<b>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_beta(<a>,<b>,<n>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("pdf_continuous_uniform"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_continuous_uniform(<x>,<a>,<b>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("cdf_continuous_uniform"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_continuous_uniform(<x>,<a>,<b>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("quantile_continuous_uniform"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_continuous_uniform(<q>,<a>,<b>)")); // OPTION
  m_wordList.at(command).push_back(wxS("mean_continuous_uniform"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_continuous_uniform(<a>,<b>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("var_continuous_uniform"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_continuous_uniform(<a>,<b>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("std_continuous_uniform"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_continuous_uniform(<a>,<b>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("skewness_continuous_uniform"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_continuous_uniform(<a>,<b>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_continuous_uniform"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_continuous_uniform(<a>,<b>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("random_continuous_uniform"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_continuous_uniform(<a>,<b>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_continuous_uniform(<a>,<b>,<n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("pdf_logistic"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_logistic(<x>,<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("cdf_logistic"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_logistic(<x>,<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("quantile_logistic"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_logistic(<q>,<a>,<b>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("mean_logistic"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_logistic(<a>,<b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("var_logistic"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_logistic(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("std_logistic"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_logistic(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("skewness_logistic"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_logistic(<a>,<b>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_logistic"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_logistic(<a>,<b>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("random_logistic"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_logistic(<a>,<b>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_logistic(<a>,<b>,<n>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("pdf_pareto"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_pareto(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("cdf_pareto"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_pareto(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("quantile_pareto"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_pareto(<q>,<a>,<b>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("mean_pareto"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_pareto(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("var_pareto"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_pareto(<a>,<b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("std_pareto"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_pareto(<a>,<b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("skewness_pareto"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_pareto(<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_pareto"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_pareto(<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("random_pareto"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_pareto(<a>,<b>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_pareto(<a>,<b>,<n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("pdf_weibull"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_weibull(<x>,<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("cdf_weibull"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_weibull(<x>,<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("quantile_weibull"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_weibull(<q>,<a>,<b>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("mean_weibull"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_weibull(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("var_weibull"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_weibull(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("std_weibull"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_weibull(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("skewness_weibull"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_weibull(<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_weibull"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_weibull(<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("random_weibull"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_weibull(<a>,<b>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_weibull(<a>,<b>,<n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("pdf_rayleigh"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_rayleigh(<x>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("cdf_rayleigh"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_rayleigh(<x>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("quantile_rayleigh"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_rayleigh(<q>,<b>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("mean_rayleigh"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_rayleigh(<b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("var_rayleigh"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_rayleigh(<b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("std_rayleigh"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_rayleigh(<b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("skewness_rayleigh"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_rayleigh(<b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_rayleigh"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_rayleigh(<b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("random_rayleigh"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_rayleigh(<b>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_rayleigh(<b>,<n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pdf_laplace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_laplace(<x>,<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("cdf_laplace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_laplace(<x>,<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("quantile_laplace"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_laplace(<q>,<a>,<b>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("mean_laplace"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_laplace(<a>,<b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("var_laplace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_laplace(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("std_laplace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_laplace(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("skewness_laplace"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_laplace(<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_laplace"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_laplace(<a>,<b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("random_laplace"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_laplace(<a>,<b>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_laplace(<a>,<b>,<n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("pdf_cauchy"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_cauchy(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("cdf_cauchy"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_cauchy(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("quantile_cauchy"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_cauchy(<q>,<a>,<b>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("random_cauchy"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_cauchy(<a>,<b>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_cauchy(<a>,<b>,<n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("pdf_gumbel"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_gumbel(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("cdf_gumbel"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_gumbel(<x>,<a>,<b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("quantile_gumbel"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_gumbel(<q>,<a>,<b>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("mean_gumbel"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_gumbel(<a>,<b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("var_gumbel"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_gumbel(<a>,<b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("std_gumbel"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_gumbel(<a>,<b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("skewness_gumbel"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_gumbel(<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_gumbel"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_gumbel(<a>,<b>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("random_gumbel"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_gumbel(<a>,<b>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_gumbel(<a>,<b>,<n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("pdf_binomial"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_binomial(<x>,<n>,<p>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("cdf_binomial"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_binomial(<x>,<n>,<p>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("quantile_binomial"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_binomial(<q>,<n>,<p>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("mean_binomial"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_binomial(<n>,<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("var_binomial"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_binomial(<n>,<p>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("std_binomial"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_binomial(<n>,<p>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("skewness_binomial"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_binomial(<n>,<p>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_binomial"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_binomial(<n>,<p>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("random_binomial"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_binomial(<n>,<p>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_binomial(<n>,<p>,<m>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("pdf_poisson"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_poisson(<x>,<m>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("cdf_poisson"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_poisson(<x>,<m>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("quantile_poisson"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_poisson(<q>,<m>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("mean_poisson"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_poisson(<m>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("var_poisson"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_poisson(<m>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("std_poisson"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_poisson(<m>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("skewness_poisson"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_poisson(<m>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_poisson"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_poisson(<m>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("random_poisson"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_poisson(<m>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_poisson(<m>,<n>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("pdf_bernoulli"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_bernoulli(<x>,<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("cdf_bernoulli"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_bernoulli(<x>,<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("quantile_bernoulli"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_bernoulli(<q>,<p>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("mean_bernoulli"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_bernoulli(<p>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("var_bernoulli"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_bernoulli(<p>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("std_bernoulli"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_bernoulli(<p>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("skewness_bernoulli"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_bernoulli(<p>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_bernoulli"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_bernoulli(<p>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("random_bernoulli"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_bernoulli(<p>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_bernoulli(<p>,<n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("pdf_geometric"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_geometric(<x>,<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("cdf_geometric"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_geometric(<x>,<p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("quantile_geometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_geometric(<q>,<p>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("mean_geometric"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_geometric(<p>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("var_geometric"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_geometric(<p>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("std_geometric"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_geometric(<p>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("skewness_geometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_geometric(<p>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_geometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_geometric(<p>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("random_geometric"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_geometric(<p>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_geometric(<p>,<n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("pdf_discrete_uniform"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_discrete_uniform(<x>,<n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("cdf_discrete_uniform"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_discrete_uniform(<x>,<n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("quantile_discrete_uniform"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_discrete_uniform(<q>,<n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("mean_discrete_uniform"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_discrete_uniform(<n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("var_discrete_uniform"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_discrete_uniform(<n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("std_discrete_uniform"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_discrete_uniform(<n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("skewness_discrete_uniform"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_discrete_uniform(<n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_discrete_uniform"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_discrete_uniform(<n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("random_discrete_uniform"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_discrete_uniform(<n>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_discrete_uniform(<n>,<m>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("pdf_hypergeometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_hypergeometric(<x>,<n1>,<n2>,<n>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("cdf_hypergeometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_hypergeometric(<x>,<n1>,<n2>,<n>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("quantile_hypergeometric"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "quantile_hypergeometric(<q>,<n1>,<n2>,<n>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("mean_hypergeometric"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_hypergeometric(<n1>,<n2>,<n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("var_hypergeometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_hypergeometric(<n1>,<n2>,<n>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("std_hypergeometric"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_hypergeometric(<n1>,<n2>,<n>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("skewness_hypergeometric"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_hypergeometric(<n1>,<n2>,<n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_hypergeometric"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_hypergeometric(<n1>,<n2>,<n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("random_hypergeometric"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_hypergeometric(<n1>,<n2>,<n>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_hypergeometric(<n1>,<n2>,<n>,<m>)")); // OPTION
  m_wordList.at(command).push_back(wxS("pdf_negative_binomial"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_negative_binomial(<x>,<n>,<p>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("cdf_negative_binomial"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_negative_binomial(<x>,<n>,<p>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("quantile_negative_binomial"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quantile_negative_binomial(<q>,<n>,<p>)")); // OPTION
  m_wordList.at(command).push_back(wxS("mean_negative_binomial"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mean_negative_binomial(<n>,<p>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("var_negative_binomial"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("var_negative_binomial(<n>,<p>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("std_negative_binomial"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("std_negative_binomial(<n>,<p>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("skewness_negative_binomial"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("skewness_negative_binomial(<n>,<p>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("kurtosis_negative_binomial"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kurtosis_negative_binomial(<n>,<p>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("random_negative_binomial"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_negative_binomial(<n>,<p>)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_negative_binomial(<n>,<p>,<m>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("get_index_properties"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_index_properties(<var>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("declare_index_properties"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "declare_index_properties(<var>, [<prop1>, <...>])")); // OPTION
  m_wordList.at(command).push_back(wxS("presuperscript"));                // OPTION
  m_wordList.at(command).push_back(wxS("postsuperscript"));               // OPTION
  m_wordList.at(command).push_back(wxS("presubscript"));                  // OPTION
  m_wordList.at(command).push_back(wxS("postsubscript"));                 // OPTION
  m_wordList.at(command).push_back(wxS("draw"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "draw(<gr2d>, ..., <gr3d>, ..., <options>, ...)"));             // OPTION
  m_wordList.at(command).push_back(wxS("draw2d"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("draw2d(<option>, <graphic_object>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("draw3d"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("draw3d(<option>, <graphic_object>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("draw_file"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "draw_file(<graphic option>, ..., <graphic object>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("multiplot_mode"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multiplot_mode(<term>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("set_draw_defaults"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_draw_defaults(<graphic option>, ..., <graphic "
                                       "object>, ...)"));           // OPTION
  m_wordList.at(command).push_back(wxS("adapt_depth"));            // OPTION
  m_wordList.at(command).push_back(wxS("axis_3d"));                // OPTION
  m_wordList.at(command).push_back(wxS("axis_bottom"));            // OPTION
  m_wordList.at(command).push_back(wxS("axis_left"));              // OPTION
  m_wordList.at(command).push_back(wxS("axis_right"));             // OPTION
  m_wordList.at(command).push_back(wxS("axis_top"));               // OPTION
  m_wordList.at(command).push_back(wxS("border"));                 // OPTION
  m_wordList.at(command).push_back(wxS("cbrange"));                // OPTION
  m_wordList.at(command).push_back(wxS("cbtics"));                 // OPTION
  m_wordList.at(command).push_back(wxS("color"));                  // OPTION
  m_wordList.at(command).push_back(wxS("colorbox"));               // OPTION
  m_wordList.at(command).push_back(wxS("columns"));                // OPTION
  m_wordList.at(command).push_back(wxS("contour"));                // OPTION
  m_wordList.at(command).push_back(wxS("contour_levels"));         // OPTION
  m_wordList.at(command).push_back(wxS("data_file_name"));         // OPTION
  m_wordList.at(command).push_back(wxS("delay"));                  // OPTION
  m_wordList.at(command).push_back(wxS("enhanced3d"));             // OPTION
  m_wordList.at(command).push_back(wxS("eps_height"));             // OPTION
  m_wordList.at(command).push_back(wxS("eps_width"));              // OPTION
  m_wordList.at(command).push_back(wxS("file_bgcolor"));           // OPTION
  m_wordList.at(command).push_back(wxS("file_name"));              // OPTION
  m_wordList.at(command).push_back(wxS("fill_color"));             // OPTION
  m_wordList.at(command).push_back(wxS("fill_density"));           // OPTION
  m_wordList.at(command).push_back(wxS("filled_func"));            // OPTION
  m_wordList.at(command).push_back(wxS("font"));                   // OPTION
  m_wordList.at(command).push_back(wxS("font_size"));              // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_command"));        // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_file_name"));      // OPTION
  m_wordList.at(command).push_back(wxS("grid"));                   // OPTION
  m_wordList.at(command).push_back(wxS("head_angle"));             // OPTION
  m_wordList.at(command).push_back(wxS("head_both"));              // OPTION
  m_wordList.at(command).push_back(wxS("head_length"));            // OPTION
  m_wordList.at(command).push_back(wxS("head_type"));              // OPTION
  m_wordList.at(command).push_back(wxS("ip_grid"));                // OPTION
  m_wordList.at(command).push_back(wxS("ip_grid_in"));             // OPTION
  m_wordList.at(command).push_back(wxS("key"));                    // OPTION
  m_wordList.at(command).push_back(wxS("key_pos"));                // OPTION
  m_wordList.at(command).push_back(wxS("label_alignment"));        // OPTION
  m_wordList.at(command).push_back(wxS("label_orientation"));      // OPTION
  m_wordList.at(command).push_back(wxS("line_type"));              // OPTION
  m_wordList.at(command).push_back(wxS("line_width"));             // OPTION
  m_wordList.at(command).push_back(wxS("logcb"));                  // OPTION
  m_wordList.at(command).push_back(wxS("logx"));                   // OPTION
  m_wordList.at(command).push_back(wxS("logx_secondary"));         // OPTION
  m_wordList.at(command).push_back(wxS("logy"));                   // OPTION
  m_wordList.at(command).push_back(wxS("logy_secondary"));         // OPTION
  m_wordList.at(command).push_back(wxS("logz"));                   // OPTION
  m_wordList.at(command).push_back(wxS("nticks"));                 // OPTION
  m_wordList.at(command).push_back(wxS("palette"));                // OPTION
  m_wordList.at(command).push_back(wxS("pdf_height"));             // OPTION
  m_wordList.at(command).push_back(wxS("pdf_width"));              // OPTION
  m_wordList.at(command).push_back(wxS("pic_height"));             // OPTION
  m_wordList.at(command).push_back(wxS("pic_width"));              // OPTION
  m_wordList.at(command).push_back(wxS("point_size"));             // OPTION
  m_wordList.at(command).push_back(wxS("point_type"));             // OPTION
  m_wordList.at(command).push_back(wxS("points_joined"));          // OPTION
  m_wordList.at(command).push_back(wxS("proportional_axes"));      // OPTION
  m_wordList.at(command).push_back(wxS("rot_horizontal"));         // OPTION
  m_wordList.at(command).push_back(wxS("rot_vertical"));           // OPTION
  m_wordList.at(command).push_back(wxS("surface_hide"));           // OPTION
  m_wordList.at(command).push_back(wxS("terminal"));               // OPTION
  m_wordList.at(command).push_back(wxS("title"));                  // OPTION
  m_wordList.at(command).push_back(wxS("transform"));              // OPTION
  m_wordList.at(command).push_back(wxS("transparent"));            // OPTION
  m_wordList.at(command).push_back(wxS("tube_extremes"));          // OPTION
  m_wordList.at(command).push_back(wxS("unit_vectors"));           // OPTION
  m_wordList.at(command).push_back(wxS("user_preamble"));          // OPTION
  m_wordList.at(command).push_back(wxS("wired_surface"));          // OPTION
  m_wordList.at(command).push_back(wxS("x_voxel"));                // OPTION
  m_wordList.at(command).push_back(wxS("xaxis"));                  // OPTION
  m_wordList.at(command).push_back(wxS("xaxis_color"));            // OPTION
  m_wordList.at(command).push_back(wxS("xaxis_secondary"));        // OPTION
  m_wordList.at(command).push_back(wxS("xaxis_type"));             // OPTION
  m_wordList.at(command).push_back(wxS("xaxis_width"));            // OPTION
  m_wordList.at(command).push_back(wxS("xlabel"));                 // OPTION
  m_wordList.at(command).push_back(wxS("xlabel_secondary"));       // OPTION
  m_wordList.at(command).push_back(wxS("xrange"));                 // OPTION
  m_wordList.at(command).push_back(wxS("xrange_secondary"));       // OPTION
  m_wordList.at(command).push_back(wxS("xtics"));                  // OPTION
  m_wordList.at(command).push_back(wxS("xtics_axis"));             // OPTION
  m_wordList.at(command).push_back(wxS("xtics_rotate"));           // OPTION
  m_wordList.at(command).push_back(wxS("xtics_rotate_secondary")); // OPTION
  m_wordList.at(command).push_back(wxS("xtics_secondary"));        // OPTION
  m_wordList.at(command).push_back(wxS("xtics_secondary_axis"));   // OPTION
  m_wordList.at(command).push_back(wxS("xu_grid"));                // OPTION
  m_wordList.at(command).push_back(wxS("xy_file"));                // OPTION
  m_wordList.at(command).push_back(wxS("xyplane"));                // OPTION
  m_wordList.at(command).push_back(wxS("y_voxel"));                // OPTION
  m_wordList.at(command).push_back(wxS("yaxis"));                  // OPTION
  m_wordList.at(command).push_back(wxS("yaxis_color"));            // OPTION
  m_wordList.at(command).push_back(wxS("yaxis_secondary"));        // OPTION
  m_wordList.at(command).push_back(wxS("yaxis_type"));             // OPTION
  m_wordList.at(command).push_back(wxS("yaxis_width"));            // OPTION
  m_wordList.at(command).push_back(wxS("ylabel"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ylabel_secondary"));       // OPTION
  m_wordList.at(command).push_back(wxS("yrange"));                 // OPTION
  m_wordList.at(command).push_back(wxS("yrange_secondary"));       // OPTION
  m_wordList.at(command).push_back(wxS("ytics"));                  // OPTION
  m_wordList.at(command).push_back(wxS("ytics_axis"));             // OPTION
  m_wordList.at(command).push_back(wxS("ytics_rotate"));           // OPTION
  m_wordList.at(command).push_back(wxS("ytics_rotate_secondary")); // OPTION
  m_wordList.at(command).push_back(wxS("ytics_secondary"));        // OPTION
  m_wordList.at(command).push_back(wxS("ytics_secondary_axis"));   // OPTION
  m_wordList.at(command).push_back(wxS("yv_grid"));                // OPTION
  m_wordList.at(command).push_back(wxS("z_voxel"));                // OPTION
  m_wordList.at(command).push_back(wxS("zaxis"));                  // OPTION
  m_wordList.at(command).push_back(wxS("zaxis_color"));            // OPTION
  m_wordList.at(command).push_back(wxS("zaxis_type"));             // OPTION
  m_wordList.at(command).push_back(wxS("zaxis_width"));            // OPTION
  m_wordList.at(command).push_back(wxS("zlabel"));                 // OPTION
  m_wordList.at(command).push_back(wxS("zrange"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ztics"));                  // OPTION
  m_wordList.at(command).push_back(wxS("ztics_axis"));             // OPTION
  m_wordList.at(command).push_back(wxS("ztics_rotate"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "make_level_picture(<data>,<width>,<height>)")); // OPTION
  m_wordList.at(command).push_back(wxS("boundaries_array"));        // OPTION
  m_wordList.at(command).push_back(wxS("chaosgame"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "chaosgame(<[[><x1>, <y1><]>...<[><xm>, <ym><]]>, <[><x0>, <y0><]>, <b>, "
                                       "<n>, ..., options, ...);"));      // OPTION
  m_wordList.at(command).push_back(wxS("evolution")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS(
                                       "evolution(<F>, <y0>, <n>, ..., options, ...);")); // OPTION
  m_wordList.at(command).push_back(wxS("evolution2d"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("evolution2d(<[><F>, <G><]>, <[><u>, <v><]>, <[><u0>, "
                                       "<y0><]>, <n>, ..., options, ...);")); // OPTION
  m_wordList.at(command).push_back(wxS("ifs"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ifs(<[><r1>, ..., <rm><]>, <[><A1>, ..., <Am><]>, "
                                       "<[[><x1>, <y1><]>, ..., <[><xm>, <ym><]]>, <[><x0>, "
                                       "<y0><]>, <n>, ..., options, ...);")); // OPTION
  m_wordList.at(command).push_back(wxS("julia"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("julia(<x>, <y>, ...<options>...)"));  // OPTION
  m_wordList.at(command).push_back(wxS("mandelbrot"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mandelbrot(<options>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("orbits"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("orbits(<F>, <y0>, <n1>, <n2>, [<x>, <x0>, <xf>, "
                                       "<xstep>], ...options...);"));             // OPTION
  m_wordList.at(command).push_back(wxS("rk"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rk(<ODE>, <var>, <initial>, <domain>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rk([<ODE1>,...,<ODEm>], [<v1>,...,<vm>], "
                                       "[<init1>,...,<initm>], <domain>)")); // OPTION
  m_wordList.at(command).push_back(wxS("staircase"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("staircase(<F>, <y0>, <n>, ...options...);")); // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_sn"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_sn(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_cn"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_cn(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_dn"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_dn(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_ns"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_ns(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_sc"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_sc(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_sd"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_sd(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_nc"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_nc(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_cs"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_cs(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_cd"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_cd(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_nd"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_nd(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_ds"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_ds(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_dc"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_dc(<u>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_sn"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_sn(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_cn"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_cn(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_dn"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_dn(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_ns"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_ns(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_sc"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_sc(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_sd"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_sd(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_nc"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_nc(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_cs"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_cs(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_cd"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_cd(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_nd"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_nd(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_ds"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_ds(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("inverse_jacobi_dc"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_jacobi_dc(<u>, <m>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_f"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_f(<phi>, <m>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_e"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_e(<phi>, <m>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_eu"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_eu(<u>, <m>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_pi"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_pi(<n>, <phi>, <m>)")); // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_kc"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_kc(<m>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("elliptic_ec"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elliptic_ec(<m>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("%rnum_list"));                  // OPTION
  m_wordList.at(command).push_back(wxS("algexact"));                    // OPTION
  m_wordList.at(command).push_back(wxS("algsys"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "algsys([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "algsys([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])");       // OPTION
  m_wordList.at(command).push_back(wxS("allroots"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("allroots(<expr>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("allroots(<eqn>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("bfallroots"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfallroots(<expr>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("bfallroots(<eqn>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("backsubst"));                              // OPTION
  m_wordList.at(command).push_back(wxS("breakup"));                                // OPTION
  m_wordList.at(command).push_back(wxS("dimension"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dimension(<eqn>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dimension(<eqn_1>, ..., <eqn_n>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("dispflag"));                               // OPTION
  m_wordList.at(command).push_back(wxS("funcsolve"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("funcsolve(<eqn>, <g>(<t>))"));              // OPTION
  m_wordList.at(command).push_back(wxS("globalsolve"));                            // OPTION
  m_wordList.at(command).push_back(wxS("ieqn"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ieqn(<ie>, <unk>, <tech>, <n>, <guess>)")); // OPTION
  m_wordList.at(command).push_back(wxS("ieqnprint"));                              // OPTION
  m_wordList.at(command).push_back(wxS("lhs"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lhs(<expr>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("linsolve"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "linsolve([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList.at(command).push_back(wxS("linsolvewarn"));                         // OPTION
  m_wordList.at(command).push_back(wxS("linsolve_params"));                      // OPTION
  m_wordList.at(command).push_back(wxS("multiplicities"));                       // OPTION
  m_wordList.at(command).push_back(wxS("nroots"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nroots(<p>, <low>, <high>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("nthroot"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nthroot(<p>, <n>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("polyfactor"));                           // OPTION
  m_wordList.at(command).push_back(wxS("programmode"));                          // OPTION
  m_wordList.at(command).push_back(wxS("realonly"));                             // OPTION
  m_wordList.at(command).push_back(wxS("realroots"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("realroots(<expr>, <bound>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("realroots(<eqn>, <bound>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("realroots(<expr>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("realroots(<eqn>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("rhs"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rhs(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("rootsconmode"));                         // OPTION
  m_wordList.at(command).push_back(wxS("rootscontract"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rootscontract(<expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("rootsepsilon"));                         // OPTION
  m_wordList.at(command).push_back(wxS("solve"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("solve(<expr>, <x>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("solve(<expr>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "solve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])");    // OPTION
  m_wordList.at(command).push_back(wxS("solvedecomposes"));                    // OPTION
  m_wordList.at(command).push_back(wxS("solveexplicit"));                      // OPTION
  m_wordList.at(command).push_back(wxS("solvefactors"));                       // OPTION
  m_wordList.at(command).push_back(wxS("solvenullwarn"));                      // OPTION
  m_wordList.at(command).push_back(wxS("solveradcan"));                        // OPTION
  m_wordList.at(command).push_back(wxS("solvetrigwarn"));                      // OPTION
  m_wordList.at(command).push_back(wxS("at"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("at(<expr>, [<eqn_1>, ..., <eqn_n>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("at(<expr>, <eqn>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("box"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("box(<expr>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("box(<expr>, <a>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("boxchar"));                            // OPTION
  m_wordList.at(command).push_back(wxS("carg"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("carg(<z>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("constantp"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("constantp(<expr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("declare"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("declare(<a_1>, <p_1>, <a_2>, <p_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("disolate"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("disolate(<expr>, <x_1>, ..., <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("dispform"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dispform(<expr>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dispform(<expr>, all)"));               // OPTION
  m_wordList.at(command).push_back(wxS("distrib"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("distrib(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("dpart"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dpart(<expr>, <n_1>, ..., <n_k>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("exp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("exp(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("%emode"));                             // OPTION
  m_wordList.at(command).push_back(wxS("%enumer"));                            // OPTION
  m_wordList.at(command).push_back(wxS("exptisolate"));                        // OPTION
  m_wordList.at(command).push_back(wxS("exptsubst"));                          // OPTION
  m_wordList.at(command).push_back(wxS("freeof"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("freeof(<x_1>, ..., <x_n>, <expr>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("genfact"));                            // FUNCTION
  m_wordList.at(command).push_back(wxS("gentran"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("genfact(<x>, <y>, <z>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("imagpart"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("imagpart(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("infix"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("infix(<op>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("infix(<op>, <lbp>, <rbp>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "infix(<op>, <lbp>, <rbp>, <lpos>, <rpos>, <pos>)");     // OPTION
  m_wordList.at(command).push_back(wxS("inflag"));                           // OPTION
  m_wordList.at(command).push_back(wxS("inpart"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inpart(<expr>, <n_1>, ..., <n_k>)")); // OPTION
  m_wordList.at(command).push_back(wxS("isolate"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("isolate(<expr>, <x>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("isolate_wrt_times"));                // OPTION
  m_wordList.at(command).push_back(wxS("listconstvars"));                    // OPTION
  m_wordList.at(command).push_back(wxS("listdummyvars"));                    // OPTION
  m_wordList.at(command).push_back(wxS("listofvars"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("listofvars(<expr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("lfreeof"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lfreeof(<list>, <expr>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("lopow"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lopow(<expr>, <x>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("lpart"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lpart(<label>, <expr>, <n_1>, ..., <n_k>)")); // OPTION
  m_wordList.at(command).push_back(wxS("multthru"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multthru(<expr>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("multthru(<expr_1>, <expr_2>)")); // OPTION
  m_wordList.at(command).push_back(wxS("nounify"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nounify(<f>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("nterms"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nterms(<expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("op"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("op(<expr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("operatorp"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("operatorp(<expr>, <op>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("operatorp(<expr>, [<op_1>, ..., <op_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("optimize"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("optimize(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("optimprefix"));                            // OPTION
  m_wordList.at(command).push_back(wxS("ordergreat"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ordergreat(<v_1>, ..., <v_n>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("orderless(<v_1>, ..., <v_n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("ordergreatp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ordergreatp(<expr_1>, <expr_2>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("orderlessp(<expr_1>, <expr_2>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("part"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("part(<expr>, <n_1>, ..., <n_k>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("partition"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("partition(<expr>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("partswitch"));                             // OPTION
  m_wordList.at(command).push_back(wxS("pickapart"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pickapart(<expr>, <n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("piece"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("polarform"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polarform(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("powers"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("powers(<expr>, <x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("product"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("product(<expr>, <i>, <i_0>, <i_1>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("realpart"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("realpart(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("rectform"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rectform(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("rembox"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rembox(<expr>, unlabelled)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rembox(<expr>, <label>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rembox(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("sum"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sum(<expr>, <i>, <i_0>, <i_1>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("lsum"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsum(<expr>, <x>, <L>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("unorder"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unorder()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("verbify"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("verbify(<f>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("constvalue"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("constvalue(<x>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("declare_constvalue(<a>, <x>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("units"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("units(<x>)"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("declare_units(<a>, <u>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("qty"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("qty(<x>)"));                                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("declare_qty(<a>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("unitp"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unitp(<x>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("declare_unit_conversion"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("declare_unit_conversion(<u> = <v>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("declare_dimensions"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "declare_dimensions(<a_1>, <d_1>, ..., <a_n>, <d_n>)");     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remove_dimensions(<a_1>, ..., <a_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("declare_fundamental_dimensions"));      // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "declare_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "remove_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList.at(command).push_back(wxS("declare_fundamental_units"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "declare_fundamental_units(<u_1>, <d_1>, ..., <u_n>, <d_n>)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "remove_fundamental_units(<u_1>, ..., <u_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("dimensions"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dimensions(<x>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dimensions_as_list(<x>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("fundamental_units"));       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fundamental_units(<x>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("fundamental_units()"));      // OPTION
  m_wordList.at(command).push_back(wxS("dimensionless"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dimensionless(<L>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("natural_unit"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("natural_unit(<expr>, [<v_1>, ..., <v_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("f90"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("f90(<expr_1>, ..., <expr_n>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("bffac"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bffac(<expr>, <n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("algepsilon"));                     // OPTION
  m_wordList.at(command).push_back(wxS("bfloat"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfloat(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("bfloatp"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfloatp(<expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("bfpsi"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfpsi(<n>, <z>, <fpprec>)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("bfpsi0(<z>, <fpprec>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("bftorat"));                        // OPTION
  m_wordList.at(command).push_back(wxS("bftrunc"));                        // OPTION
  m_wordList.at(command).push_back(wxS("cbffac"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cbffac(<z>, <fpprec>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("float"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("float(<expr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("float2bf"));                       // OPTION
  m_wordList.at(command).push_back(wxS("floatnump"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("floatnump(<expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("fpprec"));                         // OPTION
  m_wordList.at(command).push_back(wxS("fpprintprec"));                    // OPTION
  m_wordList.at(command).push_back(wxS("numer_pbranch"));                  // OPTION
  m_wordList.at(command).push_back(wxS("buildq"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("buildq(<L>, <expr>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("macroexpand"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("macroexpand(<expr>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("macroexpand1"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("macroexpand1(<expr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("macros"));                         // OPTION
  m_wordList.at(command).push_back(wxS("splice"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("splice(<a>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("apply"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("apply(<F>, [<x_1>, ..., <x_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("block"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "block([<v_1>, ..., <v_m>], <expr_1>, ..., <expr_n>)");        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("block(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("break"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("break(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("catch"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("catch(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("compfile"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("compfile(<filename>, <f_1>, ..., <f_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("compfile(<filename>, functions)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("compfile(<filename>, all)"));               // OPTION
  m_wordList.at(command).push_back(wxS("compile"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("compile(<f_1>, ..., <f_n>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("compile(functions)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("compile(all)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("define"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("define(<f>(<x_1>, ..., <x_n>), <expr>)"));  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("define(<f>[<x_1>, ..., <x_n>], <expr>)"));  // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "define(funmake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "define(arraymake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("define(ev (<expr_1>), <expr_2>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("define_variable"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "define_variable(<name>, <default_value>, <mode>)");         // OPTION
  m_wordList.at(command).push_back(wxS("dispfun"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dispfun(<f_1>, ..., <f_n>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dispfun(all)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("functions"));                            // OPTION
  m_wordList.at(command).push_back(wxS("fundef"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fundef(<f>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("funmake"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("funmake(<F>, [<arg_1>, ..., <arg_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("lambda"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "lambda([<x_1>, ..., <x_m>], <expr_1>, ..., <expr_n>)");        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("lambda([[<L>]], <expr_1>, ..., <expr_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "lambda([<x_1>, ..., <x_m>, [<L>]], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("local"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("local(<v_1>, ..., <v_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("macroexpansion"));          // OPTION
  m_wordList.at(command).push_back(wxS("mode_checkp"));             // OPTION
  m_wordList.at(command).push_back(wxS("mode_check_errorp"));       // OPTION
  m_wordList.at(command).push_back(wxS("mode_check_warnp"));        // OPTION
  m_wordList.at(command).push_back(wxS("mode_declare"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "mode_declare(<y_1>, <mode_1>, ..., <y_n>, <mode_n>)");  // OPTION
  m_wordList.at(command).push_back(wxS("mode_identity"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mode_identity(<arg_1>, <arg_2>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("transcompile"));                     // OPTION
  m_wordList.at(command).push_back(wxS("translate"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("translate(<f_1>, ..., <f_n>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("translate(functions)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("translate(all)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("translate_file"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("translate_file(<maxima_filename>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "translate_file(<maxima_filename>, <lisp_filename>)"); // OPTION
  m_wordList.at(command).push_back(wxS("transrun"));                       // OPTION
  m_wordList.at(command).push_back(wxS("tr_array_as_ref"));                // OPTION
  m_wordList.at(command).push_back(wxS("tr_bound_function_applyp"));       // OPTION
  m_wordList.at(command).push_back(wxS("tr_file_tty_messagesp"));          // OPTION
  m_wordList.at(command).push_back(wxS("tr_float_can_branch_complex"));    // OPTION
  m_wordList.at(command).push_back(wxS("tr_function_call_default"));       // OPTION
  m_wordList.at(command).push_back(wxS("tr_numer"));                       // OPTION
  m_wordList.at(command).push_back(wxS("tr_optimize_max_loop"));           // OPTION
  m_wordList.at(command).push_back(wxS("tr_semicompile"));                 // OPTION
  m_wordList.at(command).push_back(wxS("tr_state_vars"));                  // OPTION
  m_wordList.at(command).push_back(wxS("tr_warnings_get"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tr_warnings_get()"));               // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_bad_function_calls"));     // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_fexpr"));                  // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_meval"));                  // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_mode"));                   // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_undeclared"));             // OPTION
  m_wordList.at(command).push_back(wxS("tr_warn_undefined_variable"));     // OPTION
  m_wordList.at(command).push_back(wxS("tr_windy"));                       // OPTION
  m_wordList.at(command).push_back(wxS("compile_file"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("compile_file(<filename>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "compile_file(<filename>, <compiled_filename>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("compile_file(<filename>, <compiled_filename>, "
                                       "<lisp_filename>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("declare_translated"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("declare_translated(<f_1>, <f_2>, ...)")); // OPTION
  m_wordList.at(command).push_back(wxS("GGFINFINITY"));                          // OPTION
  m_wordList.at(command).push_back(wxS("GGFCFMAX"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ggf"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ggf(<l>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("create_graph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("create_graph(<v_list>, <e_list>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("create_graph(<n>, <e_list>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "create_graph(<v_list>, <e_list>, <directed>)");              // OPTION
  m_wordList.at(command).push_back(wxS("copy_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("copy_graph(<g>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("circulant_graph"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("circulant_graph(<n>, <d>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("clebsch_graph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clebsch_graph()"));                        // OPTION
  m_wordList.at(command).push_back(wxS("complement_graph"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("complement_graph(<g>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("complete_bipartite_graph"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("complete_bipartite_graph(<n>, <m>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("complete_graph"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("complete_graph(<n>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("cycle_digraph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cycle_digraph(<n>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("cycle_graph"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cycle_graph(<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("cuboctahedron_graph"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cuboctahedron_graph(<n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("cube_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cube_graph(<n>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("dodecahedron_graph"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dodecahedron_graph()"));                   // OPTION
  m_wordList.at(command).push_back(wxS("empty_graph"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("empty_graph(<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("flower_snark"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("flower_snark(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("from_adjacency_matrix"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("from_adjacency_matrix(<A>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("frucht_graph"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("frucht_graph()"));                         // OPTION
  m_wordList.at(command).push_back(wxS("graph_product"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_product(<g1>, <g1>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("graph_union"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_union(<g1>, <g1>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("grid_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("grid_graph(<n>, <m>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("great_rhombicosidodecahedron_graph"));    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("great_rhombicosidodecahedron_graph()"));   // OPTION
  m_wordList.at(command).push_back(wxS("great_rhombicuboctahedron_graph"));       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("great_rhombicuboctahedron_graph()"));      // OPTION
  m_wordList.at(command).push_back(wxS("grotzch_graph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("grotzch_graph()"));                        // OPTION
  m_wordList.at(command).push_back(wxS("heawood_graph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("heawood_graph()"));                        // OPTION
  m_wordList.at(command).push_back(wxS("icosahedron_graph"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("icosahedron_graph()"));                    // OPTION
  m_wordList.at(command).push_back(wxS("icosidodecahedron_graph"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("icosidodecahedron_graph()"));              // OPTION
  m_wordList.at(command).push_back(wxS("induced_subgraph"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("induced_subgraph(<V>, <g>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("line_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("line_graph(<g>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("make_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("make_graph(<vrt>, <f>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("make_graph(<vrt>, <f>, <oriented>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("mycielski_graph"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mycielski_graph(<g>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("new_graph"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("new_graph()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("path_digraph"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("path_digraph(<n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("path_graph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("path_graph(<n>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("petersen_graph"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("petersen_graph()"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("petersen_graph(<n>, <d>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("random_bipartite_graph"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_bipartite_graph(<a>, <b>, <p>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("random_digraph"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_digraph(<n>, <p>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("random_regular_graph"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_regular_graph(<n>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("random_regular_graph(<n>, <d>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("random_graph"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_graph(<n>, <p>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("random_graph1"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_graph1(<n>, <m>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("random_network"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_network(<n>, <p>, <w>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("random_tournament"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_tournament(<n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("random_tree"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_tree(<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("small_rhombicosidodecahedron_graph"));    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("small_rhombicosidodecahedron_graph()"));   // OPTION
  m_wordList.at(command).push_back(wxS("small_rhombicuboctahedron_graph"));       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("small_rhombicuboctahedron_graph()"));      // OPTION
  m_wordList.at(command).push_back(wxS("snub_cube_graph"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("snub_cube_graph()"));                      // OPTION
  m_wordList.at(command).push_back(wxS("snub_dodecahedron_graph"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("snub_dodecahedron_graph()"));              // OPTION
  m_wordList.at(command).push_back(wxS("truncated_cube_graph"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("truncated_cube_graph()"));                 // OPTION
  m_wordList.at(command).push_back(wxS("truncated_dodecahedron_graph"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("truncated_dodecahedron_graph()"));         // OPTION
  m_wordList.at(command).push_back(wxS("truncated_icosahedron_graph"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("truncated_icosahedron_graph()"));          // OPTION
  m_wordList.at(command).push_back(wxS("truncated_tetrahedron_graph"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("truncated_tetrahedron_graph()"));          // OPTION
  m_wordList.at(command).push_back(wxS("tutte_graph"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tutte_graph()"));                          // OPTION
  m_wordList.at(command).push_back(wxS("underlying_graph"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("underlying_graph(<g>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("wheel_graph"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("wheel_graph(<n>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("adjacency_matrix"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("adjacency_matrix(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("adjust_external_format"));                 // FUNCTION
  m_wordList.at(command).push_back(wxS("average_degree"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("average_degree(<gr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("biconnected_components"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("biconnected_components(<gr>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("bipartition"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bipartition(<gr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("chromatic_index"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("chromatic_index(<gr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("chromatic_number"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("chromatic_number(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("clear_edge_weight"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clear_edge_weight(<e>, <gr>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("clear_vertex_label"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clear_vertex_label(<v>, <gr>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("connected_components"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("connected_components(<gr>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("diameter"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("diameter(<gr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("edge_coloring"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("edge_coloring(<gr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("degree_sequence"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("degree_sequence(<gr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("edge_connectivity"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("edge_connectivity(<gr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("edges"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("edges(<gr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("get_edge_weight"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_edge_weight(<e>, <gr>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("get_edge_weight(<e>, <gr>, <ifnot>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("get_vertex_label"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_vertex_label(<v>, <gr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("graph_charpoly"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_charpoly(<gr>, <x>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("graph_center"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_center(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("graph_eigenvalues"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_eigenvalues(<gr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("graph_periphery"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_periphery(<gr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("graph_size"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_size(<gr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("graph_order"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_order(<gr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("girth"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("girth(<gr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("hamilton_cycle"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hamilton_cycle(<gr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("hamilton_path"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hamilton_path(<gr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("isomorphism"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("isomorphism(<gr1>, <gr2>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("in_neighbors"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("in_neighbors(<v>, <gr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("is_biconnected"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_biconnected(<gr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("is_bipartite"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_bipartite(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("is_connected"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_connected(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("is_digraph"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_digraph(<gr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("is_edge_in_graph"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_edge_in_graph(<e>, <gr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("is_graph"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_graph(<gr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("is_graph_or_digraph"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_graph_or_digraph(<gr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("is_isomorphic"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_isomorphic(<gr1>, <gr2>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("is_planar"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_planar(<gr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("is_sconnected"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_sconnected(<gr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("is_vertex_in_graph"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_vertex_in_graph(<v>, <gr>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("is_tree"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is_tree(<gr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("laplacian_matrix"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("laplacian_matrix(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("max_clique"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max_clique(<gr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("max_degree"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max_degree(<gr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("max_flow"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max_flow(<net>, <s>, <t>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("max_independent_set"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max_independent_set(<gr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("max_matching"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max_matching(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("min_degree"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("min_degree(<gr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("min_edge_cut"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("min_edge_cut(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("min_vertex_cover"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("min_vertex_cover(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("min_vertex_cut"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("min_vertex_cut(<gr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("minimum_spanning_tree"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minimum_spanning_tree(<gr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("neighbors"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("neighbors(<v>, <gr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("odd_girth"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("odd_girth(<gr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("out_neighbors"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("out_neighbors(<v>, <gr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("planar_embedding"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("planar_embedding(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("print_graph"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("print_graph(<gr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("radius"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("radius(<gr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("set_edge_weight"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_edge_weight(<e>, <w>, <gr>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("set_vertex_label"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_vertex_label(<v>, <l>, <gr>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("shortest_path"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("shortest_path(<u>, <v>, <gr>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("shortest_weighted_path"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("shortest_weighted_path(<u>, <v>, <gr>)")); // OPTION
  m_wordList.at(command).push_back(wxS("strong_components"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("strong_components(<gr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("topological_sort"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("topological_sort(<dag>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("vertex_connectivity"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_connectivity(<g>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("vertex_degree"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_degree(<v>, <gr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("vertex_distance"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_distance(<u>, <v>, <gr>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("vertex_eccentricity"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_eccentricity(<v>, <gr>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("vertex_in_degree"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_in_degree(<v>, <gr>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("vertex_out_degree"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_out_degree(<v>, <gr>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("vertices"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertices(<gr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("vertex_coloring"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertex_coloring(<gr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("wiener_index"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("wiener_index(<gr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("add_edge"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("add_edge(<e>, <gr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("add_edges"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("add_edges(<e_list>, <gr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("add_vertex"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("add_vertex(<v>, <gr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("add_vertices"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("add_vertices(<v_list>, <gr>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("connect_vertices"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "connect_vertices(<v_list>, <u_list>, <gr>)");   // OPTION
  m_wordList.at(command).push_back(wxS("contract_edge"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contract_edge(<e>, <gr>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("remove_edge"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remove_edge(<e>, <gr>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("remove_vertex"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remove_vertex(<v>, <gr>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("dimacs_export"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dimacs_export(<gr>, <fl>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "dimacs_export(<gr>, <fl>, <comment1>, ..., <commentn>)"); // OPTION
  m_wordList.at(command).push_back(wxS("dimacs_import"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dimacs_import(<fl>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("graph6_decode"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph6_decode(<str>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("graph6_encode"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph6_encode(<gr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("graph6_export"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph6_export(<gr_list>, <fl>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("graph6_import"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph6_import(<fl>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("sparse6_decode"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sparse6_decode(<str>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("sparse6_encode"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sparse6_encode(<gr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("sparse6_export"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sparse6_export(<gr_list>, <fl>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("sparse6_import"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sparse6_import(<fl>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("draw_graph"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("draw_graph(<graph>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "draw_graph(<graph>, <option1>, ..., <optionk>)");           // OPTION
  m_wordList.at(command).push_back(wxS("draw_graph_program"));                   // OPTION
  m_wordList.at(command).push_back(wxS("show_id"));                              // OPTION
  m_wordList.at(command).push_back(wxS("show_label"));                           // OPTION
  m_wordList.at(command).push_back(wxS("label_alignment"));                      // OPTION
  m_wordList.at(command).push_back(wxS("show_weight"));                          // OPTION
  m_wordList.at(command).push_back(wxS("vertex_type"));                          // OPTION
  m_wordList.at(command).push_back(wxS("vertex_size"));                          // OPTION
  m_wordList.at(command).push_back(wxS("vertex_color"));                         // OPTION
  m_wordList.at(command).push_back(wxS("show_vertices"));                        // OPTION
  m_wordList.at(command).push_back(wxS("show_vertex_type"));                     // OPTION
  m_wordList.at(command).push_back(wxS("show_vertex_size"));                     // OPTION
  m_wordList.at(command).push_back(wxS("show_vertex_color"));                    // OPTION
  m_wordList.at(command).push_back(wxS("vertex_partition"));                     // OPTION
  m_wordList.at(command).push_back(wxS("vertex_coloring"));                      // OPTION
  m_wordList.at(command).push_back(wxS("edge_color"));                           // OPTION
  m_wordList.at(command).push_back(wxS("edge_width"));                           // OPTION
  m_wordList.at(command).push_back(wxS("edge_type"));                            // OPTION
  m_wordList.at(command).push_back(wxS("show_edges"));                           // OPTION
  m_wordList.at(command).push_back(wxS("show_edge_color"));                      // OPTION
  m_wordList.at(command).push_back(wxS("show_edge_width"));                      // OPTION
  m_wordList.at(command).push_back(wxS("show_edge_type"));                       // OPTION
  m_wordList.at(command).push_back(wxS("edge_partition"));                       // OPTION
  m_wordList.at(command).push_back(wxS("edge_coloring"));                        // OPTION
  m_wordList.at(command).push_back(wxS("redraw"));                               // OPTION
  m_wordList.at(command).push_back(wxS("head_angle"));                           // OPTION
  m_wordList.at(command).push_back(wxS("head_length"));                          // OPTION
  m_wordList.at(command).push_back(wxS("spring_embedding_depth"));               // OPTION
  m_wordList.at(command).push_back(wxS("terminal"));                             // OPTION
  m_wordList.at(command).push_back(wxS("file_name"));                            // OPTION
  m_wordList.at(command).push_back(wxS("program"));                              // OPTION
  m_wordList.at(command).push_back(wxS("fixed_vertices"));                       // OPTION
  m_wordList.at(command).push_back(wxS("vertices_to_path"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertices_to_path(<v_list>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("vertices_to_cycle"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vertices_to_cycle(<v_list>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("poly_monomial_order"));                  // OPTION
  m_wordList.at(command).push_back(wxS("poly_coefficient_ring"));                // OPTION
  m_wordList.at(command).push_back(wxS("poly_primary_elimination_order"));       // OPTION
  m_wordList.at(command).push_back(wxS("poly_secondary_elimination_order"));     // OPTION
  m_wordList.at(command).push_back(wxS("poly_elimination_order"));               // OPTION
  m_wordList.at(command).push_back(wxS("poly_return_term_list"));                // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner_debug"));                   // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner_algorithm"));               // OPTION
  m_wordList.at(command).push_back(wxS("poly_top_reduction_only"));              // OPTION
  m_wordList.at(command).push_back(wxS("poly_add"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_add(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_subtract"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_subtract(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_multiply"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_multiply(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_s_polynomial"));      // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_s_polynomial(<poly1>, <poly2>, <varlist>)");             // OPTION
  m_wordList.at(command).push_back(wxS("poly_primitive_part"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_primitive_part(<poly1>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_normalize"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_normalize(<poly>, <varlist>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("poly_expand"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_expand(<poly>, <varlist>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("poly_expt"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_expt(<poly>, <number>, <varlist>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("poly_content"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_content(<poly>. <varlist>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("poly_pseudo_divide"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_pseudo_divide(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_exact_divide"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_exact_divide(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_normal_form"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_normal_form(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_buchberger_criterion"));   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_buchberger_criterion(<polylist>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_buchberger"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_buchberger(<polylist_fl>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_reduction"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_reduction(<polylist>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_minimization"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_minimization(<polylist>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_normalize_list")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_normalize_list(<polylist>, <varlist>)");             // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_grobner(<polylist>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_reduced_grobner"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_reduced_grobner(<polylist>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_depends_p"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_depends_p(<poly>, <var>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_elimination_ideal")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_elimination_ideal(<polylist>, <number>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_colon_ideal"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_colon_ideal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_ideal_intersection"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_ideal_intersection(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_lcm"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_lcm(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_gcd"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_gcd(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner_equal"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_grobner_equal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner_subsetp"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_grobner_subsetp(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_grobner_member"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_grobner_member(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_ideal_saturation1"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_ideal_saturation1(<polylist>, <poly>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_ideal_saturation"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "poly_ideal_saturation(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList.at(command).push_back(wxS("poly_ideal_polysaturation1"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_ideal_polysaturation1(<polylist1>, <polylist2>, "
                                       "<varlist>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("poly_ideal_polysaturation")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_ideal_polysaturation(<polylist>, "
                                       "<polylistlist>, <varlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("poly_saturation_extension")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_saturation_extension(<poly>, <polylist>, "
                                       "<varlist1>, <varlist2>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("poly_polysaturation_extension")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poly_polysaturation_extension(<poly>, <polylist>, "
                                       "<varlist1>, <varlist2>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("todd_coxeter"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("todd_coxeter(<relations>, <subgroup>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("todd_coxeter(<relations>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("apropos"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("apropos(<string>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("demo"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("demo(<filename>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("describe"));                             // FUNCTION
  m_wordList.at(command).push_back(wxS("output_format_for_help"));                // OPTION
  m_wordList.at(command).push_back(wxS("browser"));                               // OPTION
  m_wordList.at(command).push_back(wxS("browser_options"));                       // OPTION
  m_wordList.at(command).push_back(wxS("url_base"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("describe(<string>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("describe(<string>, exact)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("describe(<string>, inexact)"));           // OPTION
  m_wordList.at(command).push_back(wxS("example"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("example(<topic>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("example()"));                             // OPTION
  m_wordList.at(command).push_back(wxS("manual_demo"));                          // OPTION
  m_wordList.at(command).push_back(wxS("implicit_derivative"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "implicit_derivative(<f>,<indvarlist>,<orderlist>,<depvar>)"); // OPTION
  m_wordList.at(command).push_back(wxS("implicit_plot"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "implicit_plot(<expr>, <x_range>, <y_range>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("implicit_plot([<expr_1>, ..., <expr_n>], <x_range>, "
                                       "<y_range>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("__"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("_"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("%"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("%%"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("%edispflag"));                          // OPTION
  m_wordList.at(command).push_back(wxS("%th"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%th(<i>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("absboxchar"));                          // OPTION
  m_wordList.at(command).push_back(wxS("file_output_append"));                  // OPTION
  m_wordList.at(command).push_back(wxS("appendfile"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("appendfile(<filename>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("batch"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("batch(<filename>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("batchload"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("batchload(<filename>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("closefile"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("closefile()"));                          // OPTION
  m_wordList.at(command).push_back(wxS("close"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("close(<stream>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("collapse"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("collapse(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("concat"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("concat(<arg_1>, <arg_2>, ...)"));        // OPTION
  m_wordList.at(command).push_back(wxS("sconcat"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sconcat(<arg_1>, <arg_2>, ...)"));       // OPTION
  m_wordList.at(command).push_back(wxS("disp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("disp(<expr_1>, <expr_2>, ...)"));        // OPTION
  m_wordList.at(command).push_back(wxS("dispcon"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dispcon(<tensor_1>, <tensor_2>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dispcon(all)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("display"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("display(<expr_1>, <expr_2>, ...)"));     // OPTION
  m_wordList.at(command).push_back(wxS("display2d"));                           // OPTION
  m_wordList.at(command).push_back(wxS("display2d_unicode"));                   // OPTION
  m_wordList.at(command).push_back(wxS("display_format_internal"));             // OPTION
  m_wordList.at(command).push_back(wxS("dispterms"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dispterms(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("error_size"));                          // OPTION
  m_wordList.at(command).push_back(wxS("error_syms"));                          // OPTION
  m_wordList.at(command).push_back(wxS("expt"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expt(<a>, <b>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("exptdispflag"));                        // OPTION
  m_wordList.at(command).push_back(wxS("filename_merge"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("filename_merge(<path>, <filename>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("  file_output_append"));                // OPTION
  m_wordList.at(command).push_back(wxS("file_search"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("file_search(<filename>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("file_search(<filename>, <pathlist>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("file_search_demo"));                    // OPTION
  m_wordList.at(command).push_back(wxS("file_search_lisp"));                    // OPTION
  m_wordList.at(command).push_back(wxS("file_search_maxima"));                  // OPTION
  m_wordList.at(command).push_back(wxS("file_search_usage"));                   // OPTION
  m_wordList.at(command).push_back(wxS("file_search_tests"));                   // OPTION
  m_wordList.at(command).push_back(wxS("file_type"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("file_type(<filename>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("file_type_maxima"));                    // OPTION
  m_wordList.at(command).push_back(wxS("file_type_lisp"));                      // OPTION
  m_wordList.at(command).push_back(wxS("grind"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("grind(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("ibase"));                               // OPTION
  m_wordList.at(command).push_back(wxS("inchar"));                              // OPTION
  m_wordList.at(command).push_back(wxS("ldisp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ldisp(<expr_1>, ..., <expr_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("ldisplay"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ldisplay(<expr_1>, ..., <expr_n>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("linechar"));                            // OPTION
  m_wordList.at(command).push_back(wxS("linel"));                               // OPTION
  m_wordList.at(command).push_back(wxS("lispdisp"));                            // OPTION
  m_wordList.at(command).push_back(wxS("load"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("load(<filename>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("loadfile"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("loadfile(<filename>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("loadprint"));                           // OPTION
  m_wordList.at(command).push_back(wxS("obase"));                               // OPTION
  m_wordList.at(command).push_back(wxS("outchar"));                             // OPTION
  m_wordList.at(command).push_back(wxS("packagefile"));                         // OPTION
  m_wordList.at(command).push_back(wxS("pfeformat"));                           // OPTION
  m_wordList.at(command).push_back(wxS("print"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("print(<expr_1>, ..., <expr_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("printfile"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("printfile(<path>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("tcl_output"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tcl_output(<list>, <i0>, <skip>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tcl_output(<list>, <i0>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "tcl_output([<list_1>, ..., <list_n>], <i>)");           // OPTION
  m_wordList.at(command).push_back(wxS("read"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read(<expr_1>, ..., <expr_n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("readonly"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("readonly(<expr_1>, ..., <expr_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("reveal"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("reveal(<expr>, <depth>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("rmxchar"));                          // OPTION
  m_wordList.at(command).push_back(wxS("save"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "save(<filename>, <name_1>, <name_2>, <name_3>, ...)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "save(<filename>, values, functions, labels, ...)");            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("save(<filename>, [<m>, <n>])"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("save(<filename>, <name_1>=<expr_1>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("save(<filename>, all)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "save(<filename>, <name_1>=<expr_1>, <name_2>=<expr_2>, ...)"); // OPTION
  m_wordList.at(command).push_back(wxS("savedef"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("show"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("show(<expr>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("showratvars"));        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("showratvars(<expr>)")); // OPTION
  m_wordList.at(command).push_back(wxS("stardisp"));           // OPTION
  m_wordList.at(command).push_back(wxS("string"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("string(<expr>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("stringdisp"));         // OPTION
  m_wordList.at(command).push_back(wxS("stringout"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "stringout(<filename>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("stringout(<filename>, [<m>, <n>])"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("stringout(<filename>, input)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("stringout(<filename>, functions)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("stringout(<filename>, values)"));         // OPTION
  m_wordList.at(command).push_back(wxS("tex"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<expr>)"));                           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<expr>, <destination>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<expr>, false)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<label>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<label>, <destination>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tex(<label>, false)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("tex1"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tex1(<e>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("texput"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("texput(<a>, <s>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("texput(<a>, <f>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("texput(<a>, <s>, <operator_type>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("texput(<a>, [<s_1>, <s_2>], matchfix)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "texput(<a>, [<s_1>, <s_2>, <s_3>], matchfix)"); // OPTION
  m_wordList.at(command).push_back(wxS("get_tex_environment"));      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_tex_environment(<op>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "set_tex_environment(<op>, <before>, <after>)");     // OPTION
  m_wordList.at(command).push_back(wxS("get_tex_environment_default"));  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_tex_environment_default()")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "set_tex_environment_default(<before>, <after>)"); // OPTION
  m_wordList.at(command).push_back(wxS("system"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("system(<command>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("ttyoff"));                     // OPTION
  m_wordList.at(command).push_back(wxS("with_stdout"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "with_stdout(<f>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "with_stdout(<s>, <expr_1>, <expr_2>, <expr_3>, ...)");      // OPTION
  m_wordList.at(command).push_back(wxS("writefile"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("writefile(<filename>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("changevar"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("changevar(<expr>, <f(x,y)>, <y>, <x>)")); // OPTION
  m_wordList.at(command).push_back(wxS("dblint"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dblint(<f>, <r>, <s>, <a>, <b>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("defint"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("defint(<expr>, <x>, <a>, <b>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("erfflag"));                              // OPTION
  m_wordList.at(command).push_back(wxS("ilt"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ilt(<expr>, <s>, <t>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("intanalysis"));                          // OPTION
  m_wordList.at(command).push_back(wxS("integrate"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("integrate(<expr>, <x>)"));                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("integrate(<expr>, <x>, <a>, <b>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("integration_constant"));                 // OPTION
  m_wordList.at(command).push_back(wxS("integration_constant_counter"));         // OPTION
  m_wordList.at(command).push_back(wxS("integrate_use_rootsof"));                // OPTION
  m_wordList.at(command).push_back(wxS("ldefint"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ldefint(<expr>, <x>, <a>, <b>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("potential"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("potential(<givengradient>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("residue"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("residue(<expr>, <z>, <z_0>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("risch"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("risch(<expr>, <x>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("tldefint"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tldefint(<expr>, <x>, <a>, <b>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("quad_qag"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qag(<f(x)>, <x>, <a>, <b>, <key>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qag(<f>, <x>, <a>, <b>, <key>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(command).push_back(wxS("quad_qags"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qags(<f(x)>, <x>, <a>, <b>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "quad_qags(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList.at(command).push_back(wxS("quad_qagi")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qagi(<f(x)>, <x>, <a>, <b>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "quad_qagi(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList.at(command).push_back(wxS("quad_qawc")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qawc(<f(x)>, <x>, <c>, <a>, <b>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qawc(<f>, <x>, <c>, <a>, <b>, [<epsrel>, "
                                       "<epsabs>, <limit>])")); // OPTION
  m_wordList.at(command).push_back(wxS("quad_qawf"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qawf(<f(x)>, <x>, <a>, <omega>, <trig>, "
                                       "[<epsabs>, <limit>, <maxp1>, <limlst>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qawf(<f>, <x>, <a>, <omega>, <trig>, [<epsabs>, "
                                       "<limit>, <maxp1>, <limlst>])")); // OPTION
  m_wordList.at(command).push_back(wxS("quad_qawo"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "quad_qawo(<f(x)>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, "
                                   "<limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "quad_qawo(<f>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, "
                                   "<limit>, <maxp1>, <limlst>])");  // OPTION
  m_wordList.at(command).push_back(wxS("quad_qaws")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qaws(<f(x)>, <x>, <a>, <b>, <alpha>, <beta>, "
                                       "<wfun>, [<epsrel>, <epsabs>, <limit>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("quad_qaws(<f>, <x>, <a>, <b>, <alpha>, <beta>, "
                                       "<wfun>, [<epsrel>, <epsabs>, <limit>])")); // OPTION
  m_wordList.at(command).push_back(wxS("lagrange"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lagrange(<points>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("lagrange(<points>, <option>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("charfun2"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charfun2(<x>, <a>, <b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("linearinterpol"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("linearinterpol(<points>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("linearinterpol(<points>, <option>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("cspline"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cspline(<points>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "cspline(<points>, <option1>, <option2>, ...)");       // OPTION
  m_wordList.at(command).push_back(wxS("ratinterpol"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratinterpol(<points>, <numdeg>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ratinterpol(<points>, <numdeg>, <option1>, <option2>, ...)"); // OPTION
  m_wordList.at(command).push_back(wxS("entertensor"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("entertensor(<name>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("changename"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("changename(<old>, <new>, <expr>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("ishow"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ishow(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("indices"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("indices(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("rename"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rename(<expr>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rename(<expr>, <count>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("flipflag"));                               // OPTION
  m_wordList.at(command).push_back(wxS("defcon"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("defcon(<tensor_1>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "defcon(<tensor_1>, <tensor_2>, <tensor_3>)");             // OPTION
  m_wordList.at(command).push_back(wxS("remcon"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remcon(<tensor_1>, ..., <tensor_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remcon(all)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("contract"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contract(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("indexed_tensor"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("indexed_tensor(<tensor>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("components"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("components(<tensor>, <expr>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("remcomps"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remcomps(<tensor>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("showcomps"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("showcomps(<tensor>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("idummy"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("idummy()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("idummyx"));                            // OPTION
  m_wordList.at(command).push_back(wxS("icounter"));                           // OPTION
  m_wordList.at(command).push_back(wxS("kdelta"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kdelta(<L1>, <L2>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("kdels"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kdels(<L1>, <L2>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("levi_civita"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("levi_civita(<L>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("lc2kdt"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lc2kdt(<expr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("canten"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("canten(<expr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("concan"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("concan(<expr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("allsym"));                             // OPTION
  m_wordList.at(command).push_back(wxS("decsym"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("decsym(<tensor>, <m>, <n>, [<cov_1>, <cov_2>, ...], "
                                       "[<contr_1>, <contr_2>, ...])")); // OPTION
  m_wordList.at(command).push_back(wxS("remsym"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remsym(<tensor>, <m>, <n>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("canform"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("canform(<expr>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("canform(<expr>, <rename>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("diff"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "diff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList.at(command).push_back(wxS("idiff"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "idiff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList.at(command).push_back(wxS("liediff"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("liediff(<v>, <ten>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("rediff"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rediff(<ten>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("undiff"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("undiff(<expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("evundiff"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("evundiff(<expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("flush"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "flush(<expr>, <tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList.at(command).push_back(wxS("flushd"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "flushd(<expr>, <tensor_1>, <tensor_2>, ...)");              // OPTION
  m_wordList.at(command).push_back(wxS("flushnd"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("flushnd(<expr>, <tensor>, <n>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("coord"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("coord(<tensor_1>, <tensor_2>, ...)"));    // OPTION
  m_wordList.at(command).push_back(wxS("remcoord"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remcoord(<tensor_1>, <tensor_2>, ...)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remcoord(all)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("makebox"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makebox(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("conmetderiv"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("conmetderiv(<expr>, <tensor>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("simpmetderiv"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("simpmetderiv(<expr>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("simpmetderiv(<expr>[, <stop>])"));        // OPTION
  m_wordList.at(command).push_back(wxS("flush1deriv"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("flush1deriv(<expr>, <tensor>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("imetric"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("imetric(<g>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("idim"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("idim(<n>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ichr1"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ichr1([<i>, <j>, <k>])"));                // OPTION
  m_wordList.at(command).push_back(wxS("ichr2"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ichr2([<i>, <j>], [<k>])"));              // OPTION
  m_wordList.at(command).push_back(wxS("icurvature"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("icurvature([<i>, <j>, <k>], [<h>])"));    // OPTION
  m_wordList.at(command).push_back(wxS("covdiff"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("covdiff(<expr>, <v_1>, <v_2>, ...)"));    // OPTION
  m_wordList.at(command).push_back(wxS("lorentz_gauge"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lorentz_gauge(<expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("igeodesic_coords"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("igeodesic_coords(<expr>, <name>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("iframes"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("iframes()"));                             // OPTION
  m_wordList.at(command).push_back(wxS("ifb"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("icc1"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("icc2"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("ifc1"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("ifc2"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("ifr"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("ifri"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("ifg"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("ifgi"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("iframe_bracket_form"));                  // OPTION
  m_wordList.at(command).push_back(wxS("inm"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("inmc1"));                                // OPTION
  m_wordList.at(command).push_back(wxS("inmc2"));                                // OPTION
  m_wordList.at(command).push_back(wxS("ikt1"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("ikt2"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("itr"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("extdiff"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("extdiff(<expr>, <i>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("hodge"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hodge(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("igeowedge_flag"));                       // OPTION
  m_wordList.at(command).push_back(wxS("tentex"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tentex(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("ic_convert"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ic_convert(<eqn>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("dgeev"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dgeev(<A>)"));                            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dgeev(<A>, <right_p>, <left_p>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("dgesv"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dgesv(<A>, <b>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("dgesvd"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dgesvd(<A>)"));                           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("dgesvd(<A>, <left_p>, <right_p>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("dlange"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dlange(<norm>, <A>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("zlange(<norm>, <A>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("lbfgs"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "lbfgs(<FOM>, <X>, <X0>, <epsilon>, <iprint>)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "lbfgs([<FOM>, <grad>] <X>, <X0>, <epsilon>, <iprint>)");     // OPTION
  m_wordList.at(command).push_back(wxS("lbfgs_nfeval_max"));                      // OPTION
  m_wordList.at(command).push_back(wxS("lbfgs_ncorrections"));                    // OPTION
  m_wordList.at(command).push_back(wxS("lhospitallim"));                          // OPTION
  m_wordList.at(command).push_back(wxS("limit"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("limit(<expr>, <x>, <val>, <dir>)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("limit(<expr>, <x>, <val>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("limit(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("limsubst"));                              // OPTION
  m_wordList.at(command).push_back(wxS("tlimit"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tlimit(<expr>, <x>, <val>, <dir>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tlimit(<expr>, <x>, <val>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tlimit(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("tlimswitch"));                            // OPTION
  m_wordList.at(command).push_back(wxS("Lindstedt"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("Lindstedt(<eq>,<pvar>,<torder>,<ic>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("addmatrices"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("addmatrices(<f>, <M_1>, ..., <M_n>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("blockmatrixp"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("blockmatrixp(<M>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("columnop"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("columnop(<M>, <i>, <j>, <theta>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("columnswap"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("columnswap(<M>, <i>, <j>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("columnspace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("columnspace(<M>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("copy"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("copy(<e>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("cholesky"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cholesky(<M>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("cholesky(<M>, <field>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ctranspose"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ctranspose(<M>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("diag_matrix"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("diag_matrix(<d_1>, <d_2>,...,<d_n>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("dotproduct"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dotproduct(<u>, <v>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("eigens_by_jacobi"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eigens_by_jacobi(<A>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("eigens_by_jacobi(<A>, <field_type>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("get_lu_factors"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_lu_factors(<x>) "));                   // OPTION
  m_wordList.at(command).push_back(wxS("hankel"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hankel(<col>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("hankel(<col>, <row>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("hessian"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hessian(<f>, <x>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("hilbert_matrix"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hilbert_matrix(<n>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("identfor"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("identfor(<M>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("identfor(<M>, <fld>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("invert_by_lu"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("invert_by_lu(<M>, <(rng generalring)>)")); // OPTION
  m_wordList.at(command).push_back(wxS("jacobian"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobian(<f>, <x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("kronecker_product"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kronecker_product(<A>, <B>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("listp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("listp(<e>, <p>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("listp(<e>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("locate_matrix_entry"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("locate_matrix_entry(<M>, <r_1>, <c_1>, <r_2>, <c_2>, "
                                       "<f>, <rel>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("lu_backsub"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lu_backsub(<M>, <b>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("lu_factor"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lu_factor(<M>, <field>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("mat_cond"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_cond(<M>, 1)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("mat_cond(<M>, inf)"));       // OPTION
  m_wordList.at(command).push_back(wxS("mat_norm"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_norm(<M>, 1)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("mat_norm(<M>, inf)"));       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("mat_norm(<M>, frobenius)")); // OPTION
  m_wordList.at(command).push_back(wxS("matrixp"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrixp(<e>, <p>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("matrixp(<e>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("matrixexp"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrixexp(<M>, <v>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("matrix_size"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrix_size(<M>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("mat_fullunblocker"));       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_fullunblocker(<M>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("mat_trace"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_trace(<M>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("mat_unblocker"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mat_unblocker(<M>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("nonnegintegerp"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nonnegintegerp(<n>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("nullspace"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nullspace(<M>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("nullity"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nullity(<M>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("orthogonal_complement"));   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("orthogonal_complement(<v_1>, ..., <v_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("polynomialp")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polynomialp(<p>, <L>, <coeffp>, <exponp>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("polynomialp(<p>, <L>, <coeffp>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("polynomialp(<p>, <L>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("polytocompanion"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polytocompanion(<p>, <x>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("ptriangularize"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ptriangularize(<M>, <v>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("rowop"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rowop(<M>, <i>, <j>, <theta>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("rank"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rank(<M>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("rowswap"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rowswap(<M>, <i>, <j>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("toeplitz"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("toeplitz(<col>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("toeplitz(<col>, <row>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("vandermonde_matrix"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vandermonde_matrix([<x_1>, ..., <x_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("zerofor"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("zerofor(<M>)"));                            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("zerofor(<M>, <fld>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("zeromatrixp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("zeromatrixp(<M>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("append"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("append(<list_1>, ..., <list_n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("assoc"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("assoc(<key>, <list>, <default>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("assoc(<key>, <list>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("atom"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("atom(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("cons"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cons(<expr>, <list>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("copylist"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("copylist(<list>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("create_list"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "create_list(<form>, <x_1>, <list_1>, ..., <x_n>, <list_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("delete"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("delete(<expr_1>, <expr_2>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("delete(<expr_1>, <expr_2>, <n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("eighth"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eighth(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("endcons"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("endcons(<expr>, <list>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("fifth"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fifth(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("first"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("first(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("firstn"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("firstn(<expr>, <number>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("fourth"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourth(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("garbage_collect"));                        // FUNCTION
  m_wordList.at(command).push_back(wxS("get"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get(<a>, <i>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("join"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("join(<l>, <m>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("last"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("last(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("lastn"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lastn(<expr>, <number>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("length"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("length(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("listarith"));                              // OPTION
  m_wordList.at(command).push_back(wxS("listp"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("listp(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("makelist"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makelist(<expr>, <i>, <i_0>, <i_1>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("makelist(<expr>, <x>, <list>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("member"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("member(<expr_1>, <expr_2>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("ninth"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ninth(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("pop"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pop(<list>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("push"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("push(<item>, <list>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("unique"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unique(<L>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("rest"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rest(<expr>, <n>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rest(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("reverse"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("reverse(<list>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("second"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("second(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("seventh"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("seventh(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("sixth"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sixth(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("sublist_indices"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sublist_indices(<L>, <P>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("tenth"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tenth(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("third"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("third(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("%e_to_numlog"));                           // OPTION
  m_wordList.at(command).push_back(wxS("li"));                                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("li[<s>] (<z>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("log"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("log(<x>)"));                                // OPTION
  m_wordList.at(command).push_back(wxS("logabs"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("logarc"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("logconcoeffp"));                           // OPTION
  m_wordList.at(command).push_back(wxS("logcontract"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("logcontract(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("logexpand"));                              // OPTION
  m_wordList.at(command).push_back(wxS("lognegint"));                              // OPTION
  m_wordList.at(command).push_back(wxS("lognumer"));                               // OPTION
  m_wordList.at(command).push_back(wxS("logsimp"));                                // OPTION
  m_wordList.at(command).push_back(wxS("plog"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("plog(<x>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_estimates"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_estimates(<D>, <x>, <e>, <a>)"));  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_estimates(<D>, <x>, <e>, <a>, initial = "
                                       "<L>, tol = <t>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_estimates_exact"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_estimates_exact(<MSE>, <a>)")); // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_estimates_approximate"));      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_estimates_approximate(<MSE>, <a>, initial = "
                                       "<L>, tol = <t>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_mse"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_mse(<D>, <x>, <e>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_residuals"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_residuals(<D>, <x>, <e>, <a>)")); // OPTION
  m_wordList.at(command).push_back(wxS("lsquares_residual_mse"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lsquares_residual_mse(<D>, <x>, <e>, <a>)")); // OPTION
  m_wordList.at(command).push_back(wxS("plsquares"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("plsquares(<Mat>,<VarList>,<depvars>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>)"); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>,<maxdegree>)"); // OPTION
  m_wordList.at(command).push_back(wxS("makeOrders"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makeOrders(<indvarlist>,<orderlist>)")); // OPTION
  m_wordList.at(command).push_back(wxS("addcol"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("addcol(<M>, <list_1>, ..., <list_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("addrow"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("addrow(<M>, <list_1>, ..., <list_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("adjoint"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("adjoint(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("augcoefmatrix"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "augcoefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList.at(command).push_back(wxS("charpoly"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charpoly(<M>, <x>)")); // OPTION
  m_wordList.at(command).push_back(wxS("coefmatrix"));        // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "coefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList.at(command).push_back(wxS("col"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("col(<M>, <i>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("columnvector"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("columnvector(<L>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("covect(<L>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("conjugate"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("conjugate(<x>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("copymatrix"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("copymatrix(<M>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("determinant"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("determinant_by_lu(<M>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("determinant_by_lu"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("determinant(<M>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("detout"));                               // OPTION
  m_wordList.at(command).push_back(wxS("diagmatrix"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("diagmatrix(<n>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("doallmxops"));                           // OPTION
  m_wordList.at(command).push_back(wxS("domxexpt"));                             // OPTION
  m_wordList.at(command).push_back(wxS("domxmxops"));                            // OPTION
  m_wordList.at(command).push_back(wxS("domxnctimes"));                          // OPTION
  m_wordList.at(command).push_back(wxS("dontfactor"));                           // OPTION
  m_wordList.at(command).push_back(wxS("doscmxops"));                            // OPTION
  m_wordList.at(command).push_back(wxS("doscmxplus"));                           // OPTION
  m_wordList.at(command).push_back(wxS("dot0nscsimp"));                          // OPTION
  m_wordList.at(command).push_back(wxS("dot0simp"));                             // OPTION
  m_wordList.at(command).push_back(wxS("dot1simp"));                             // OPTION
  m_wordList.at(command).push_back(wxS("dotassoc"));                             // OPTION
  m_wordList.at(command).push_back(wxS("dotconstrules"));                        // OPTION
  m_wordList.at(command).push_back(wxS("dotdistrib"));                           // OPTION
  m_wordList.at(command).push_back(wxS("dotexptsimp"));                          // OPTION
  m_wordList.at(command).push_back(wxS("dotident"));                             // OPTION
  m_wordList.at(command).push_back(wxS("dotscrules"));                           // OPTION
  m_wordList.at(command).push_back(wxS("echelon"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("echelon(<M>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("eigenvalues"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eigenvalues(<M>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("eivals(<M>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("eigenvectors"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eigenvectors(<M>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("eivects(<M>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("ematrix"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ematrix(<m>, <n>, <x>, <i>, <j>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("entermatrix"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("entermatrix(<m>, <n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("genmatrix"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "genmatrix(<a>, <i_2>, <j_2>, <i_1>, <j_1>)");             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("genmatrix(<a>, <i_2>, <j_2>, <i_1>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("genmatrix(<a>, <i_2>, <j_2>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("gramschmidt"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gramschmidt(<x>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("gramschmidt(<x>, <F>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("ident"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ident(<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("innerproduct"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("innerproduct(<x>, <y>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("inprod(<x>, <y>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("invert"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("invert(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("lmxchar"));                            // OPTION
  m_wordList.at(command).push_back(wxS("load_pathname"));                      // OPTION
  m_wordList.at(command).push_back(wxS("matrix"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrix(<row_1>, ..., <row_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("matrixmap"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrixmap(<f>, <M>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("matrixp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matrixp(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("matrix_element_add"));                 // OPTION
  m_wordList.at(command).push_back(wxS("matrix_element_mult"));                // OPTION
  m_wordList.at(command).push_back(wxS("matrix_element_transpose"));           // OPTION
  m_wordList.at(command).push_back(wxS("mattrace"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mattrace(<M>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("minor"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minor(<M>, <i>, <j>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("ncexpt"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ncexpt(<a>, <b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("ncharpoly"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ncharpoly(<M>, <x>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("newdet"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("newdet(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("nonscalar"));                          // OPTION
  m_wordList.at(command).push_back(wxS("nonscalarp"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nonscalarp(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("permanent"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("permanent(<M>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("rank"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rank(<M>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("ratmx"));                              // OPTION
  m_wordList.at(command).push_back(wxS("row"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("row(<M>, <i>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("scalarmatrixp"));                      // OPTION
  m_wordList.at(command).push_back(wxS("scalefactors"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scalefactors(<coordinatetransform>)")); // OPTION
  m_wordList.at(command).push_back(wxS("setelmx"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setelmx(<x>, <i>, <j>, <M>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("similaritytransform"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("similaritytransform(<M>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("simtran(<M>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("sparse"));                             // OPTION
  m_wordList.at(command).push_back(wxS("submatrix"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "submatrix(<i_1>, ..., <i_m>, <M>, <j_1>, ..., <j_n>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("submatrix(<i_1>, ..., <i_m>, <M>)")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("submatrix(<M>, <j_1>, ..., <j_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("tmlinsolve"));                             // FUNCTION
  m_wordList.at(command).push_back(wxS("tminverse"));                             // FUNCTION
  m_wordList.at(command).push_back(wxS("tmlin"));                             // FUNCTION
  m_wordList.at(command).push_back(wxS("tmnewdet"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tmnewdet(<M>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("transpose"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("transpose(<M>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("triangularize"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("triangularize(<M>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("uniteigenvectors"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("uniteigenvectors(<M>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ueivects(<M>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("unitvector"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unitvector(<x>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("uvect(<x>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("vectorsimp"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vectorsimp(<expr>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("vect_cross"));                       // OPTION
  m_wordList.at(command).push_back(wxS("zeromatrix"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("zeromatrix(<m>, <n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("minpack_lsquares"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minpack_lsquares(<flist>, <varlist>, <guess> [, "
                                       "<tolerance>, <jacobian>])")); // OPTION
  m_wordList.at(command).push_back(wxS("minpack_solve"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minpack_solve(<flist>, <varlist>, <guess> [, "
                                       "<tolerance>, <jacobian>])"));               // OPTION
  m_wordList.at(command).push_back(wxS("aliases"));                                // OPTION
  m_wordList.at(command).push_back(wxS("alphabetic"));                             // OPTION
  m_wordList.at(command).push_back(wxS("args"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("args(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("genindex"));                               // OPTION
  m_wordList.at(command).push_back(wxS("gensumnum"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("gensym(<x>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("infolists"));                              // OPTION
  m_wordList.at(command).push_back(wxS("integerp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("integerp(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("m1pbranch"));                              // OPTION
  m_wordList.at(command).push_back(wxS("numberp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("numberp(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("properties"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("properties(<a>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("props"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("propvars"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("propvars(<prop>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("put"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("put(<atom>, <value>, <indicator>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("qput"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("qput(<atom>, <value>, <indicator>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("rem"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rem(<atom>, <indicator>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("remove"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remove(<a_1>, <p_1>, ..., <a_n>, <p_n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "remove([<a_1>, ..., <a_m>], [<p_1>, ..., <p_n>], ...)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remove(\"<a>\", operator)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remove(<a>, transfun)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remove(all, <p>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("remvalue"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remvalue(<name_1>, ..., <name_n>)"));  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remvalue(all)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("rncombine"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rncombine(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("scalarp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scalarp(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("setup_autoload"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "setup_autoload(<filename>, <function_1>, ..., <function_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("newtonepsilon"));                           // OPTION
  m_wordList.at(command).push_back(wxS("newtonmaxiter"));                           // OPTION
  m_wordList.at(command).push_back(wxS("mnewton")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mnewton(<FuncList>,<VarList>,<GuessList>)")); // OPTION
  m_wordList.at(command).push_back(wxS("adjoin"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("adjoin(<x>, <a>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("belln"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("belln(<n>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("cardinality"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cardinality(<a>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("cartesian_product"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cartesian_product(<b_1>, ... , <b_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("cartesian_product_list"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "cartesian_product_list(<l_1>, ... , <l_n>)");                 // OPTION
  m_wordList.at(command).push_back(wxS("disjoin"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("disjoin(<x>, <a>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("disjointp"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("disjointp(<a>, <b>) "));                    // OPTION
  m_wordList.at(command).push_back(wxS("divisors"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("divisors(<n>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("elementp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elementp(<x>, <a>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("emptyp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("emptyp(<a>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("equiv_classes"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("equiv_classes(<s>, <F>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("every"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("every(<f>, <s>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("every(<f>, <L_1>, ..., <L_n>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("extremal_subset"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("extremal_subset(<s>, <f>, max)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("extremal_subset(<s>, <f>, min)"));          // OPTION
  m_wordList.at(command).push_back(wxS("flatten"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("flatten(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("full_listify"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("full_listify(<a>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("fullsetify"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fullsetify(<a>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("identity"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("identity(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("integer_partitions"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("integer_partitions(<n>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("integer_partitions(<n>, <len>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("intersect"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intersect(<a_1>, ..., <a_n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("intersection"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intersection(<a_1>, ..., <a_n>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("kron_delta"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kron_delta(<x>, <y>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("listify"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("listify(<a>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("lreduce"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lreduce(<F>, <s>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("lreduce(<F>, <s>, <s_0>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("makeset"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makeset(<expr>, <x>, <s>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("moebius"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("moebius(<n>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("multinomial_coeff"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multinomial_coeff(<a_1>, ..., <a_n>)"));    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("multinomial_coeff()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("num_distinct_partitions"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("num_distinct_partitions(<n>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("num_distinct_partitions(<n>, list)"));      // OPTION
  m_wordList.at(command).push_back(wxS("num_partitions"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("num_partitions(<n>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("num_partitions(<n>, list)"));               // OPTION
  m_wordList.at(command).push_back(wxS("partition_set"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("partition_set(<a>, <f>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("permutations"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("permutations(<a>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("powerset"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("powerset(<a>)"));                           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("powerset(<a>, <n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("random_permutation"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random_permutation(<a>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("rreduce"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rreduce(<F>, <s>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rreduce(<F>, <s>, @var{s_@{n + 1@}})"));    // OPTION
  m_wordList.at(command).push_back(wxS("setdifference"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setdifference(<a>, <b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("setequalp"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setequalp(<a>, <b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("setify"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setify(<a>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("setp"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setp(<a>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("set_partitions"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_partitions(<a>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("set_partitions(<a>, <n>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("some"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("some(<f>, <a>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("some(<f>, <L_1>, ..., <L_n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("stirling1"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("stirling1(<n>, <m>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("stirling2"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("stirling2(<n>, <m>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("subset"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("subset(<a>, <f>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("subsetp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("subsetp(<a>, <b>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("symmdifference"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("symmdifference(<a_1>, ..., <a_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("tree_reduce"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tree_reduce(<F>, <s>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tree_reduce(<F>, <s>, <s_0>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("union"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("union(<a_1>, ..., <a_n>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("xreduce"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("xreduce(<F>, <s>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("xreduce(<F>, <s>, <s_0>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("bern"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bern(<n>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("bernpoly"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bernpoly(<x>, <n>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("bfzeta"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfzeta(<s>, <n>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("bfhzeta"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bfhzeta(<s>, <h>, <n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("binomial"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("binomial(<x>, <y>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("burn"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("burn(<n>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("cf"));                                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cf(<expr>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("cfdisrep"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cfdisrep(<list>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("cfexpand"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cfexpand(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("cflength"));                               // OPTION
  m_wordList.at(command).push_back(wxS("divsum"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("divsum(<n>, <k>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("divsum(<n>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("euler"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("euler(<n>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("%gamma"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("factorial"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("factorial(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("factorial_expand"));                       // OPTION
  m_wordList.at(command).push_back(wxS("fib"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fib(<n>)"));                                // OPTION
  m_wordList.at(command).push_back(wxS("fibtophi"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fibtophi(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("ifactors"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ifactors(<n>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("inrt"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inrt(<x>, <n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("inv_mod"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inv_mod(<n>, <m>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("jacobi"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi(<p>, <q>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("lcm"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lcm(<expr_1>, ..., <expr_n>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("minfactorial"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minfactorial(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("next_prime"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("next_prime(<n>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("partfrac"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("partfrac(<expr>, <var>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("power_mod"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("power_mod(<a>, <n>, <m>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("primep"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("primep(<n>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("primep_number_of_tests"));                 // OPTION
  m_wordList.at(command).push_back(wxS("prev_prime"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("prev_prime(<n>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("qunit"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("qunit(<n>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("totient"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("totient(<n>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("zerobern"));                               // OPTION
  m_wordList.at(command).push_back(wxS("zeta"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("zeta(<n>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("zeta%pi"));                                // OPTION
  m_wordList.at(command).push_back(wxS("polartorect"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polartorect(<r>, <t>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("recttopolar"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("recttopolar(<a>, <b>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("inverse_fft"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inverse_fft(<y>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("fft"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fft(<x>)"));                                // OPTION
  m_wordList.at(command).push_back(wxS("fortindent"));                             // OPTION
  m_wordList.at(command).push_back(wxS("fortran"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fortran(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("fortspaces"));                             // OPTION
  m_wordList.at(command).push_back(wxS("horner"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("horner(<expr>, <x>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("horner(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("find_root_error"));                        // OPTION
  m_wordList.at(command).push_back(wxS("find_root_abs"));                          // OPTION
  m_wordList.at(command).push_back(wxS("find_root_rel"));                          // OPTION
  m_wordList.at(command).push_back(wxS("find_root"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("find_root(<expr>, <x>, <a>, <b>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("find_root(<f>, <a>, <b>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("bf_find_root"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bf_find_root(<expr>, <x>, <a>, <b>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("bf_find_root(<f>, <a>, <b>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("newton"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("newton(<expr>, <x>, <x_0>, <eps>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("equalp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("equalp(<x>, <y>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("remfun"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remfun(<f>, <expr>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remfun(<f>, <expr>, <x>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("funp"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("funp(<f>, <expr>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("funp(<f>, <expr>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("absint"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("absint(<f>, <x>, <halfplane>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("absint(<f>, <x>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("absint(<f>, <x>, <a>, <b>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("algfac"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("algnorm"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("algtrace"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("aload_mac"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("and"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("announce_rules_firing"));                                 // OPTION
  m_wordList.at(command).push_back(wxS("array_dimension_n"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("arraysetapply"));                                 // FUNCTION
  m_wordList.at(command).push_back(wxS("auto_mexpr"));                                 // FUNCTION
// FUNCTION
  m_wordList.at(command).push_back(wxS("fourier"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourier(<f>, <x>, <p>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("foursimp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("foursimp(<l>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("sinnpiflag"));                             // OPTION
  m_wordList.at(command).push_back(wxS("cosnpiflag"));                             // OPTION
  m_wordList.at(command).push_back(wxS("fourexpand"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourexpand(<l>, <x>, <p>, <limit>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("fourcos"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourcos(<f>, <x>, <p>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("foursin"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("foursin(<f>, <x>, <p>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("totalfourier"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("totalfourier(<f>, <x>, <p>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("fourint"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourint(<f>, <x>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("fourintcos"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourintcos(<f>, <x>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("fourintsin"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fourintsin(<f>, <x>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("read_matrix"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_matrix(<S>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_matrix(<S>, <M>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_matrix(<S>, <separator_flag>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_matrix(<S>, <M>, <separator_flag>)")); // OPTION
  m_wordList.at(command).push_back(wxS("read_array"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_array(<S>, <A>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_array(<S>, <A>, <separator_flag>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("read_hashed_array"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_hashed_array(<S>, <A>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "read_hashed_array(<S>, <A>, <separator_flag>)");              // OPTION
  m_wordList.at(command).push_back(wxS("read_nested_list"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_nested_list(<S>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_nested_list(<S>, <separator_flag>)")); // OPTION
  m_wordList.at(command).push_back(wxS("read_list"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_list(<S>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_list(<S>, <L>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_list(<S>, <separator_flag>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_list(<S>, <L>, <separator_flag>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("write_data"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("write_data(<X>, <D>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("write_data(<X>, <D>, <separator_flag>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("assume_external_byte_order"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "assume_external_byte_order(<byte_order_flag>)");             // OPTION
  m_wordList.at(command).push_back(wxS("openr_binary"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("openr_binary(<file_name>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("openw_binary"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("openw_binary(<file_name>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("opena_binary"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("opena_binary(<file_name>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("read_binary_matrix"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_binary_matrix(<S>, <M>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("read_binary_array"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_binary_array(<S>, <A>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("read_binary_list"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("read_binary_list(<S>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("read_binary_list(<S>, <L>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("write_binary_data"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("write_binary_data(<X>, <D>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("abs"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("abs(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("additive"));                              // OPTION
  m_wordList.at(command).push_back(wxS("allbut"));                                // OPTION
  m_wordList.at(command).push_back(wxS("antisymmetric"));                         // OPTION
  m_wordList.at(command).push_back(wxS("cabs"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cabs(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("ceiling"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ceiling(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("charfun"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charfun(<p>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("commutative"));                           // OPTION
  m_wordList.at(command).push_back(wxS("compare"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("compare(<x>, <y>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("entier"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("entier(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("equal"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("equal(<a>, <b>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("floor"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("floor(<x>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("notequal"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("notequal(<a>, <b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("evenp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("evenp(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("fix"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fix(<x>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("fullmap"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fullmap(<f>, <expr_1>, <...>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("fullmapl"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fullmapl(<f>, <list_1>, <...>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("is"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("is(<expr>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("maybe"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("maybe(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("isqrt"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("isqrt(<x>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("lmax"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lmax(<L>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("lmin"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lmin(<L>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("max"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("max(<x_1>, <...>, <x_n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("min"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("min(<x_1>, <...>, <x_n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("polymod"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polymod(<p>)"));                           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("polymod(<p>, <m>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("mod"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mod(<x>, <y>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("oddp"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("oddp(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("psubst"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("psubst(<list>, <expr>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("psubst(<a>, <b>, <expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("make_random_state"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("make_random_state(<n>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("make_random_state(<s>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("set_random_state"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_random_state(<s>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("random"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("random(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("rationalize"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rationalize(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("round"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("round(<x>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("sign"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sign(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("signum"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("signum(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("sort"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sort(<L>, <P>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sort(<L>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("sqrt"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sqrt(<x>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("sqrtdispflag"));                          // OPTION
  m_wordList.at(command).push_back(wxS("sublis"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sublis(<list>, <expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("sublist"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sublist(<list>, <p>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("sublis_apply_lambda"));                   // OPTION
  m_wordList.at(command).push_back(wxS("subst"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("subst(<a>, <b>, <c>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("substinpart"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("substinpart(<x>, <expr>, <n_1>, <n_k>)")); // OPTION
  m_wordList.at(command).push_back(wxS("substpart"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("substpart(<x>, <expr>, <n_1>, <n_k>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("subvarp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("subvarp(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("symbolp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("symbolp(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("vectorpotential"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vectorpotential(<givencurl>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("xthru"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("xthru(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("zeroequiv"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("zeroequiv(<expr>, <v>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("opsubst"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("opsubst(<f>,<g>,<e>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("opsubst(<g>=<f>,<e>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "opsubst([<g1>=<f1>,<g2>=<f2>,<gn>=<fn>],<e>)");              // OPTION
  m_wordList.at(command).push_back(wxS("assoc_legendre_p"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("assoc_legendre_p(<n>, <m>, <x>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("assoc_legendre_q"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("assoc_legendre_q(<n>, <m>, <x>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("chebyshev_t"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("chebyshev_t(<n>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("chebyshev_u"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("chebyshev_u(<n>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("gen_laguerre"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gen_laguerre(<n>, <a>, <x>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("hermite"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hermite(<n>, <x>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("intervalp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intervalp(<e>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("jacobi_p"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("jacobi_p(<n>, <a>, <b>, <x>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("laguerre"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("laguerre(<n>, <x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("legendre_p"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("legendre_p(<n>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("legendre_q"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("legendre_q(<n>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("orthopoly_recur"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("orthopoly_recur(<f>, <args>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("orthopoly_returns_intervals"));           // OPTION
  m_wordList.at(command).push_back(wxS("orthopoly_weight"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("orthopoly_weight(<f>, <args>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("pochhammer"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pochhammer(<n>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("pochhammer_max_index"));                  // OPTION
  m_wordList.at(command).push_back(wxS("spherical_bessel_j"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("spherical_bessel_j(<n>, <x>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("spherical_bessel_y"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("spherical_bessel_y(<n>, <x>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("spherical_hankel1"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("spherical_hankel1(<n>, <x>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("spherical_hankel2"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("spherical_hankel2(<n>, <x>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("spherical_harmonic"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("spherical_harmonic(<n>, <m>, <x>, <y>)")); // OPTION
  m_wordList.at(command).push_back(wxS("unit_step"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unit_step(<x>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ultraspherical"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ultraspherical(<n>, <a>, <x>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("plotdf"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("plotdf(<dydx>, <options>)"));              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("plotdf(<dvdu>, [<u>,<v>], <options>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("plotdf([<dxdt>,<dydt>], <options>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "plotdf([<dudt>,<dvdt>], [<u>,<v>], <options>)"); // OPTION
  m_wordList.at(command).push_back(wxS("contour_plot"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "contour_plot(<expr>, <x_range>, <y_range>, <options>)");  // OPTION
  m_wordList.at(command).push_back(wxS("get_plot_option"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("get_plot_option(<keyword>, <index>)")); // OPTION
  m_wordList.at(command).push_back(wxS("make_transform"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "make_transform([<var1>, <var2>, <var3>], <fx>, <fy>, <fz>)"); // OPTION
  m_wordList.at(command).push_back(wxS("plot2d"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("plot2d(<plot>, <x_range>, <[options]>)"));  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("plot2d([<plot_1>, <plot_n>], <options>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "plot2d([<plot_1>, <plot_n>], <x_range>, <[options]>)"); // OPTION
  m_wordList.at(command).push_back(wxS("plot3d"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "plot3d(<expr>, <x_range>, <y_range>, <[options]>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("plot3d([<expr_1>, <...>, <expr_n>], <x_range>, "
                                       "<y_range>, <[options]>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("plot_options"));                       // OPTION
  m_wordList.at(command).push_back(wxS("set_plot_option"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("set_plot_option(<option>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("adapt_depth"));                       // OPTION
  m_wordList.at(command).push_back(wxS("axes"));                               // OPTION
  m_wordList.at(command).push_back(wxS("azimut"));                             // OPTION
  m_wordList.at(command).push_back(wxS("box"));                                // OPTION
  m_wordList.at(command).push_back(wxS("color"));                              // OPTION
  m_wordList.at(command).push_back(wxS("colorbox"));                           // OPTION
  m_wordList.at(command).push_back(wxS("elevation"));                          // OPTION
  m_wordList.at(command).push_back(wxS("grid"));                               // OPTION
  m_wordList.at(command).push_back(wxS("legend"));                             // OPTION
  m_wordList.at(command).push_back(wxS("logx"));                               // OPTION
  m_wordList.at(command).push_back(wxS("logy"));                               // OPTION
  m_wordList.at(command).push_back(wxS("mesh_lines_color"));                   // OPTION
  m_wordList.at(command).push_back(wxS("nticks"));                             // OPTION
  m_wordList.at(command).push_back(wxS("palette"));                            // OPTION
  m_wordList.at(command).push_back(wxS("plot_format"));                        // OPTION
  m_wordList.at(command).push_back(wxS("plot_real_part"));                     // OPTION
  m_wordList.at(command).push_back(wxS("point_type"));                         // OPTION
  m_wordList.at(command).push_back(wxS("psfile"));                             // OPTION
  m_wordList.at(command).push_back(wxS("run_viewer"));                         // OPTION
  m_wordList.at(command).push_back(wxS("style"));                              // OPTION
  m_wordList.at(command).push_back(wxS("t"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("transform_xy"));                       // OPTION
  m_wordList.at(command).push_back(wxS("x"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("xlabel"));                             // OPTION
  m_wordList.at(command).push_back(wxS("y"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("ylabel"));                             // OPTION
  m_wordList.at(command).push_back(wxS("z"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("zlabel"));                             // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_term"));                       // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_out_file"));                   // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_pm3d"));                       // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_preamble"));                   // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_curve_titles"));               // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_curve_styles"));               // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_default_term_command"));       // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_dumb_term_command"));          // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_ps_term_command"));            // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_start"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_start()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_close"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_close()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_restart"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_restart()"));                   // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_replot"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_replot()"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_replot(<s>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("gnuplot_reset"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gnuplot_reset()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("algebraic"));                          // OPTION
  m_wordList.at(command).push_back(wxS("berlefact"));                          // OPTION
  m_wordList.at(command).push_back(wxS("bezout"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bezout(<p1>, <p2>, <x>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("bothcoef"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bothcoef(<expr>, <x>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("coeff"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("coeff(<expr>, <x>, <n>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("coeff(<expr>, <x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("combine"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("combine(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("content"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("content(<p_1>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("denom"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("denom(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("divide"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("divide(<p_1>, <p_2>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("eliminate")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "eliminate([<eqn_1>, <...>, <eqn_n>], [<x_1>, <...>, <x_k>])"); // OPTION
  m_wordList.at(command).push_back(wxS("ezgcd"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ezgcd(<p_1>, <p_2>, <p_3>, ...)"));        // OPTION
  m_wordList.at(command).push_back(wxS("facexpand"));                             // OPTION
  m_wordList.at(command).push_back(wxS("factcomb"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("factcomb(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("factor"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("factor(<expr>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("factor(<expr>, <p>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("factorflag"));                            // OPTION
  m_wordList.at(command).push_back(wxS("factorout"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("factorout(<expr>, <x_1>, <x_2>, <...>)")); // OPTION
  m_wordList.at(command).push_back(wxS("factorsum"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("factorsum(<expr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("fasttimes"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fasttimes(<p_1>, <p_2>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("fullratsimp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fullratsimp(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("fullratsubst"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fullratsubst(<a>, <b>, <c>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("gcd"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gcd(<p_1>, <p_2>, <x_1>, <...>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("gcdex"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gcdex(<f>, <g>)"));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("gcdex(<f>, <g>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("gcfactor"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gcfactor(<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("gfactor"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gfactor(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("gfactorsum"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gfactorsum(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("hipow"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hipow(<expr>, <x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("intfaclim"));                             // OPTION
  m_wordList.at(command).push_back(wxS("keepfloat"));                             // OPTION
  m_wordList.at(command).push_back(wxS("lratsubst"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lratsubst(<L>, <expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("modulus"));                               // OPTION
  m_wordList.at(command).push_back(wxS("num"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("num(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("polydecomp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polydecomp(<p>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("quotient"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("quotient(<p_1>, <p_2>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "quotient(<p_1>, <p_2>, <x_1>, <...>, <x_n>)");             // OPTION
  m_wordList.at(command).push_back(wxS("rat"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rat(<expr>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("rat(<expr>, <x_1>, <...>, <x_n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("ratalgdenom"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ratcoef"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratcoef(<expr>, <x>, <n>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ratcoef(<expr>, <x>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ratdenom"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratdenom(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("ratdenomdivide"));                      // OPTION
  m_wordList.at(command).push_back(wxS("ratdiff"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratdiff(<expr>, <x>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ratdisrep"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratdisrep(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("ratepsilon"));                          // OPTION
  m_wordList.at(command).push_back(wxS("ratexpand"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratexpand(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("ratfac"));                              // OPTION
  m_wordList.at(command).push_back(wxS("ratnumer"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratnumer(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("ratnump"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratnump(<expr>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("ratp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratp(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ratprint"));                            // OPTION
  m_wordList.at(command).push_back(wxS("float_approx_equal_tolerance"));        // OPTION
  m_wordList.at(command).push_back(wxS("float_approx_equal"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("float_approx_equal(<f_1>,<f_2>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("ratsimp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratsimp(<expr>)"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ratsimp(<expr>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("ratsimpexpons"));                       // OPTION
  m_wordList.at(command).push_back(wxS("ratsubst"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratsubst(<a>, <b>, <c>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("ratvars"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ratvars(<x_1>, <...>, <x_n>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ratvars()"));                            // OPTION
  m_wordList.at(command).push_back(wxS("ratweight"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "ratweight(<x_1>, <w_1>, <...>, <x_n>, <w_n>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ratweight()"));               // OPTION
  m_wordList.at(command).push_back(wxS("ratweights"));               // OPTION
  m_wordList.at(command).push_back(wxS("ratwtlvl"));                 // OPTION
  m_wordList.at(command).push_back(wxS("remainder"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remainder(<p_1>, <p_2>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "remainder(<p_1>, <p_2>, <x_1>, <...>, <x_n>)");               // OPTION
  m_wordList.at(command).push_back(wxS("resultant"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resultant(<p_1>, <p_2>, <x>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("savefactors"));                            // OPTION
  m_wordList.at(command).push_back(wxS("sqfr"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sqfr(<expr>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("tellrat"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tellrat(<p_1>, <...>, <p_n>)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tellrat()"));                               // OPTION
  m_wordList.at(command).push_back(wxS("totaldisrep"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("totaldisrep(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("untellrat"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("untellrat(<x_1>, <...>, <x_n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("backtrace"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("backtrace()"));                             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("backtrace(<n>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("errcatch"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("errcatch(<expr_1>, <...>, <expr_n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("error"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("error(<expr_1>, <...>, <expr_n>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("errormsg"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("errormsg()"));                              // OPTION
  m_wordList.at(command).push_back(wxS("errormsg"));                               // OPTION
  m_wordList.at(command).push_back(wxS("go"));                                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("go(<tag>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("map"));                                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("map(<f>, <expr_1>, <...>, <expr_n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("mapatom"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mapatom(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("maperror"));                               // OPTION
  m_wordList.at(command).push_back(wxS("mapprint"));                               // OPTION
  m_wordList.at(command).push_back(wxS("maplist"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("maplist(<f>, <expr_1>, <...>, <expr_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("prederror"));                              // OPTION
  m_wordList.at(command).push_back(wxS("return"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("return(<value>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("scanmap"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scanmap(<f>, <expr>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("scanmap(<f>, <expr>, bottomup)"));          // OPTION
  m_wordList.at(command).push_back(wxS("throw"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("throw(<expr>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("outermap"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("outermap(<f>, <a_1>, <...>, <a_n>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("romberg"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("romberg(<expr>, <x>, <a>, <b>)"));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("romberg(<F>, <a>, <b>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("rombergabs"));                             // OPTION
  m_wordList.at(command).push_back(wxS("rombergit"));                              // OPTION
  m_wordList.at(command).push_back(wxS("rombergmin"));                             // OPTION
  m_wordList.at(command).push_back(wxS("rombergtol"));                             // OPTION
  m_wordList.at(command).push_back(wxS("apply1"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("apply1(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("apply2")); // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("apply2(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("applyb1")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "applyb1(<expr>, <rule_1>, <...>, <rule_n>)");   // OPTION
  m_wordList.at(command).push_back(wxS("current_let_rule_package")); // OPTION
  m_wordList.at(command).push_back(wxS("default_let_rule_package")); // OPTION
  m_wordList.at(command).push_back(wxS("defmatch"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "defmatch(<progname>, <pattern>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("defmatch(<progname>, <pattern>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("defrule"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "defrule(<rulename>, <pattern>, <replacement>)"); // OPTION
  m_wordList.at(command).push_back(wxS("disprule"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "disprule(<rulename_1>, <...>, <rulename_2>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("disprule(all)"));            // OPTION
  m_wordList.at(command).push_back(wxS("let"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "let(<prod>, <repl>, <predname>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("let([<prod>, <repl>, <predname>, <arg_1>, <...>, "
                                       "<arg_n>], <package_name>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("letrat"));                         // OPTION
  m_wordList.at(command).push_back(wxS("letrules"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("letrules()"));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("letrules(<package_name>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("letsimp"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("letsimp(<expr>)"));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("letsimp(<expr>, <package_name>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "letsimp(<expr>, <package_name_1>, <...>, <package_name_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("let_rule_packages"));                      // OPTION
  m_wordList.at(command).push_back(wxS("matchdeclare"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "matchdeclare(<a_1>, <pred_1>, <...>, <a_n>, <pred_n>)");   // OPTION
  m_wordList.at(command).push_back(wxS("matchfix"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("matchfix(<ldelimiter>, <rdelimiter>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "matchfix(<ldelimiter>, <rdelimiter>, <arg_pos>, <pos>)");     // OPTION
  m_wordList.at(command).push_back(wxS("remlet"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remlet(<prod>, <name>)"));                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remlet()"));                                // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remlet(all)"));                             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remlet(all, <name>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("remrule"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("remrule(<op>, <rulename>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("remrule(<op>, all)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("tellsimp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tellsimp(<pattern>, <replacement>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("tellsimpafter"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tellsimpafter(<pattern>, <replacement>)")); // OPTION
  m_wordList.at(command).push_back(wxS("clear_rules"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clear_rules()"));                           // OPTION
  m_wordList.at(command).push_back(wxS("feature"));                                // OPTION
  m_wordList.at(command).push_back(wxS("featurep"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("featurep(<a>, <f>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("maxima_tempdir"));                         // OPTION
  m_wordList.at(command).push_back(wxS("maxima_userdir"));                         // OPTION
  m_wordList.at(command).push_back(wxS("maxima_objdir"));                          // OPTION
  m_wordList.at(command).push_back(wxS("maxima_frontend"));                        // OPTION
  m_wordList.at(command).push_back(wxS("maxima_frontend_version"));                // OPTION
  m_wordList.at(command).push_back(wxS("room"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("room()"));                                  // OPTION
  m_wordList.at(tmplte ).push_back(wxS("room(true)"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("room(false)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("rules"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("sstatus"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sstatus(<keyword>, <item>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("status"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("status(<feature>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("status(<feature>, <item>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("time"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("time(<%o1>, <%o2>, <%o3>, <...>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("timedate"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("timedate()"));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("timedate(<T>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("absolute_real_time"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("absolute_real_time()"));                    // OPTION
  m_wordList.at(command).push_back(wxS("elapsed_real_time"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elapsed_real_time()"));                     // OPTION
  m_wordList.at(command).push_back(wxS("elapsed_run_time"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elapsed_run_time()"));                      // OPTION
  m_wordList.at(command).push_back(wxS("cauchysum"));                              // OPTION
  m_wordList.at(command).push_back(wxS("deftaylor"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("deftaylor(<f_1>(<x_1>), <expr_1>, <...>, "
                                       "<f_n>(<x_n>), <expr_n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("maxtayorder"));                     // OPTION
  m_wordList.at(command).push_back(wxS("niceindices"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("niceindices(<expr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("niceindicespref"));                 // OPTION
  m_wordList.at(command).push_back(wxS("nusum"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nusum(<expr>, <x>, <i_0>, <i_1>)")); // OPTION
  m_wordList.at(command).push_back(wxS("pade"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "pade(<taylor_series>, <numer_deg_bound>, <denom_deg_bound>)"); // OPTION
  m_wordList.at(command).push_back(wxS("powerdisp"));                               // OPTION
  m_wordList.at(command).push_back(wxS("powerseries"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("powerseries(<expr>, <x>, <a>)")); // OPTION
  m_wordList.at(command).push_back(wxS("psexpand"));                     // OPTION
  m_wordList.at(command).push_back(wxS("revert"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("revert(<expr>, <x>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("revert2(<expr>, <x>, <n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("taylor"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("taylor(<expr>, <x>, <a>, <n>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "taylor(<expr>, [<x_1>, <x_2>, <...>], <a>, <n>)");            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("taylor(<expr>, [<x>, <a>, <n>, 'asymp])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("taylor(<expr>, [<x_1>, <x_2>, <...>], [<a_1>, <a_2>, "
                                       "<...>], [<n_1>, <n_2>, <...>])")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("taylor(<expr>, [<x_1>, <a_1>, <n_1>], [<x_2>, <a_2>, "
                                       "<n_2>], <...>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("taylordepth"));                           // OPTION
  m_wordList.at(command).push_back(wxS("taylorinfo"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("taylorinfo(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("taylorp"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("taylorp(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("taylor_logexpand"));                      // OPTION
  m_wordList.at(command).push_back(wxS("taylor_order_coefficients"));             // OPTION
  m_wordList.at(command).push_back(wxS("taylor_simplifier"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("taylor_simplifier(<expr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("taylor_truncate_polynomials"));           // OPTION
  m_wordList.at(command).push_back(wxS("taytorat"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("taytorat(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("trunc"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trunc(<expr>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("unsum"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unsum(<f>, <n>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("verbose"));                               // OPTION
  m_wordList.at(command).push_back(wxS("intopois"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intopois(<a>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("outofpois"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("outofpois(<a>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("poisdiff"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poisdiff(<a>, <b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("poisexpt"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poisexpt(<a>, <b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("poisint"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poisint(<a>, <b>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("poislim"));                               // OPTION
  m_wordList.at(command).push_back(wxS("poismap"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poismap(<series>, <sinfn>, <cosfn>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("poisplus"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poisplus(<a>, <b>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("poissimp"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poissimp(<a>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("poisson"));                               // OPTION
  m_wordList.at(command).push_back(wxS("poissubst"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poissubst(<a>, <b>, <c>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("poistimes"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poistimes(<a>, <b>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("poistrim"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("poistrim()"));                             // OPTION
  m_wordList.at(command).push_back(wxS("printpois"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("printpois(<a>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("epsilon_lp"));                            // OPTION
  m_wordList.at(command).push_back(wxS("linear_program"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("linear_program(<A>, <b>, <c>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("maximize_lp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("maximize_lp(<obj>, <cond>, [<pos>])"));    // OPTION
  m_wordList.at(command).push_back(wxS("minimize_lp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("minimize_lp(<obj>, <cond>, [<pos>])"));    // OPTION
  m_wordList.at(command).push_back(wxS("nonegative_lp"));                         // OPTION
  m_wordList.at(command).push_back(wxS("askexp"));                                // OPTION
  m_wordList.at(command).push_back(wxS("askinteger"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("askinteger(<expr>, integer)"));            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("askinteger(<expr>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("askinteger(<expr>, even)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("askinteger(<expr>, odd)"));                // OPTION
  m_wordList.at(command).push_back(wxS("asksign"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("asksign(<expr>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("demoivre"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("demoivre(<expr>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("distribute_over"));                       // OPTION
  m_wordList.at(command).push_back(wxS("domain"));                                // OPTION
  m_wordList.at(command).push_back(wxS("expand"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expand(<expr>)"));                         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("expand(<expr>, <p>, <n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("expandwrt"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expandwrt(<expr>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("expandwrt_denom"));                       // OPTION
  m_wordList.at(command).push_back(wxS("expandwrt_factored"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "expandwrt_factored(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("expon"));                       // OPTION
  m_wordList.at(command).push_back(wxS("exponentialize"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("exponentialize(<expr>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("expop"));                       // OPTION
  m_wordList.at(command).push_back(wxS("factlim"));                     // OPTION
  m_wordList.at(command).push_back(wxS("intosum"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intosum(<expr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("lassociative"));                // OPTION
  m_wordList.at(command).push_back(wxS("linear"));                      // OPTION
  m_wordList.at(command).push_back(wxS("mainvar"));                     // OPTION
  m_wordList.at(command).push_back(wxS("maxapplydepth"));               // OPTION
  m_wordList.at(command).push_back(wxS("maxapplyheight"));              // OPTION
  m_wordList.at(command).push_back(wxS("maxnegex"));                    // OPTION
  m_wordList.at(command).push_back(wxS("maxposex"));                    // OPTION
  m_wordList.at(command).push_back(wxS("multiplicative"));              // OPTION
  m_wordList.at(command).push_back(wxS("negdistrib"));                  // OPTION
  m_wordList.at(command).push_back(wxS("negsumdispflag"));              // OPTION
  m_wordList.at(command).push_back(wxS("noeval"));                      // OPTION
  m_wordList.at(command).push_back(wxS("noun"));                        // OPTION
  m_wordList.at(command).push_back(wxS("noundisp"));                    // OPTION
  m_wordList.at(command).push_back(wxS("nouns"));                       // OPTION
  m_wordList.at(command).push_back(wxS("numer"));                       // OPTION
  m_wordList.at(command).push_back(wxS("numerval"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "numerval(<x_1>, <expr_1>, <...>, <var_n>, <expr_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("opproperties"));                    // OPTION
  m_wordList.at(command).push_back(wxS("opsubst"));                         // OPTION
  m_wordList.at(command).push_back(wxS("outative"));                        // OPTION
  m_wordList.at(command).push_back(wxS("posfun"));                          // OPTION
  m_wordList.at(command).push_back(wxS("pred"));                            // OPTION
  m_wordList.at(command).push_back(wxS("radcan"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("radcan(<expr>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("radexpand"));                       // OPTION
  m_wordList.at(command).push_back(wxS("radsubstflag"));                    // OPTION
  m_wordList.at(command).push_back(wxS("rassociative"));                    // OPTION
  m_wordList.at(command).push_back(wxS("scsimp"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scsimp(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("simp"));                                     // OPTION
  m_wordList.at(command).push_back(wxS("simpsum"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("sumcontract"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sumcontract(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("sumexpand"));                              // OPTION
  m_wordList.at(command).push_back(wxS("sumsplitfact"));                           // OPTION
  m_wordList.at(command).push_back(wxS("symmetric"));                              // OPTION
  m_wordList.at(command).push_back(wxS("unknown"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("unknown(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("facsum"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("facsum(<expr>, <arg_1>, <...>, <arg_n>)")); // OPTION
  m_wordList.at(command).push_back(wxS("nextlayerfactor"));                        // OPTION
  m_wordList.at(command).push_back(wxS("facsum_combine"));                         // OPTION
  m_wordList.at(command).push_back(wxS("factorfacsum"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "factorfacsum(<expr>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList.at(command).push_back(wxS("collectterms"));              // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "collectterms(<expr>, <arg_1>, <...>, <arg_n>)");            // OPTION
  m_wordList.at(command).push_back(wxS("rempart"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rempart(<expr>, <n>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("wronskian"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("wronskian([<f_1>, <...>, <f_n>], <x>)")); // OPTION
  m_wordList.at(command).push_back(wxS("tracematrix"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tracematrix(<M>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("rational"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rational(<z>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("logand"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("logand(<x>,<y>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("logor"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("logor(<x>,<y>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("logxor"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("logxor(<x>,<y>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("nonzeroandfreeof"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nonzeroandfreeof(<x>, <expr>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("linear"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("linear(<expr>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("gcdivide"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gcdivide(<p>, <q>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("arithmetic"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("arithmetic(<a>, <d>, <n>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("geometric"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("geometric(<a>, <r>, <n>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("harmonic"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("harmonic(<a>, <b>, <c>, <n>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("arithsum"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("arithsum(<a>, <d>, <n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("geosum"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("geosum(<a>, <r>, <n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("gaussprob"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gaussprob(<x>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("gd"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gd(<x>)"));                               // OPTION
  m_wordList.at(command).push_back(wxS("agd"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("agd(<x>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("vers"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("vers(<x>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("covers"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("covers(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("exsec"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("exsec(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("hav"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hav(<x>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("combination"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("combination(<n>, <r>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("permutation"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("permutation(<n>, <r>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("reduce_consts"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("reduce_consts(<expr>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("gcfac"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gcfac(<expr>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("sqrtdenest"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sqrtdenest(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("reduce_order"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("reduce_order(<rec>, <sol>, <var>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("simplify_products"));                    // OPTION
  m_wordList.at(command).push_back(wxS("simplify_sum"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("simplify_sum(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("solve_rec"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("solve_rec(<eqn>, <var>, [<init>])"));     // OPTION
  m_wordList.at(command).push_back(wxS("solve_rec_rat"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("solve_rec_rat(<eqn>, <var>, [<init>])")); // OPTION
  m_wordList.at(command).push_back(wxS("product_use_gamma"));                    // OPTION
  m_wordList.at(command).push_back(wxS("summand_to_rec"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("summand_to_rec(<summand>, <k>, <n>)"));   // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "summand_to_rec(<summand>, [<k>, <lo>, <hi>], <n>)");        // OPTION
  m_wordList.at(command).push_back(wxS("bessel_j"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bessel_j(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("bessel_y"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bessel_y(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("bessel_i"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bessel_i(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("bessel_k"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("bessel_k(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("hankel_1"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hankel_1(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("hankel_2"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hankel_2(<v>, <z>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("besselexpand"));                         // OPTION
  m_wordList.at(command).push_back(wxS("scaled_bessel_i"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scaled_bessel_i(<v>, <z>) "));            // OPTION
  m_wordList.at(command).push_back(wxS("scaled_bessel_i0"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scaled_bessel_i0(<z>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("scaled_bessel_i1"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scaled_bessel_i1(<z>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("%s"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%s[<u>,<v>] (<z>) "));                    // OPTION
  m_wordList.at(command).push_back(wxS("airy_ai"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("airy_ai(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("airy_dai"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("airy_dai(<x>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("airy_bi"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("airy_bi(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("airy_dbi"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("airy_dbi(<x>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("gamma"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gamma(<z>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("log_gamma"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("log_gamma(<z>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("gamma_incomplete"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gamma_incomplete(<a>, <z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("gamma_incomplete_regularized"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("gamma_incomplete_regularized(<a>,<z>)")); // OPTION
  m_wordList.at(command).push_back(wxS("gamma_incomplete_generalized"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "gamma_incomplete_generalized(<a>,<z1>,<z1> )");      // OPTION
  m_wordList.at(command).push_back(wxS("gammalim"));                      // OPTION
  m_wordList.at(command).push_back(wxS("makegamma"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makegamma(<expr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("beta"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("beta(<a>, <b>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("beta_incomplete"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("beta_incomplete(<a>, <b>, <z>)")); // OPTION
  m_wordList.at(command).push_back(wxS("beta_incomplete_regularized"));   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "beta_incomplete_regularized(<a>, <b>, <z>)");      // OPTION
  m_wordList.at(command).push_back(wxS("beta_incomplete_generalized")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "beta_incomplete_generalized(<a>, <b>, <z1>, <z2>)"); // OPTION
  m_wordList.at(command).push_back(wxS("beta_expand"));                   // OPTION
  m_wordList.at(command).push_back(wxS("beta_args_sum_to_integer"));      // OPTION
  m_wordList.at(command).push_back(wxS("psi"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("psi[<n>](<x>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("maxpsiposint"));                  // OPTION
  m_wordList.at(command).push_back(wxS("maxpsinegint"));                  // OPTION
  m_wordList.at(command).push_back(wxS("maxpsifracnum"));                 // OPTION
  m_wordList.at(command).push_back(wxS("maxpsifracdenom"));               // OPTION
  m_wordList.at(command).push_back(wxS("makefact"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("makefact(<expr>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("numfactor"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("numfactor(<expr>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_e1"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_e1(<z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_ei"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_ei(<z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_li"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_li(<z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_e"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_e(<n>,<z>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_si"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_si(<z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_ci"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_ci(<z>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_shi"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_shi(<z>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("expintegral_chi"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("expintegral_chi(<z>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("expintrep"));                     // OPTION
  m_wordList.at(command).push_back(wxS("expintexpand"));                  // OPTION
  m_wordList.at(command).push_back(wxS("erf"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("erf(<z>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("erfc"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("erfc(<z>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("erfi"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("erfi(<z>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("erf_generalized"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("erf_generalized(<z1>,<z2>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("fresnel_c"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fresnel_c(<z>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("fresnel_s"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fresnel_s(<z>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("gensym"));                        // FUNCTION
  m_wordList.at(command).push_back(wxS("erf_representation"));            // OPTION
  m_wordList.at(command).push_back(wxS("hypergeometric_representation")); // OPTION
  m_wordList.at(command).push_back(wxS("struve_h"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("struve_h(<v>, <z>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("struve_l"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("struve_l(<v>, <z>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("%m"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%m[<k>,<u>] (<z>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("%w"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%w[<k>,<u>] (<z>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("%f"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("%f[<p>,<q>] (<[a],[b],z>) "));     // OPTION
  m_wordList.at(command).push_back(wxS("hypergeometric"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "hypergeometric([<a1>, <...>, <ap>],[<b1>, <...> ,<bq>], x)"); // OPTION
  m_wordList.at(command).push_back(wxS("parabolic_cylinder_d"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("parabolic_cylinder_d(<v>, <z>) "));         // OPTION
  m_wordList.at(command).push_back(wxS("specint"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("specint(exp(- s*<t>) * <expr>, <t>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("hgfred"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hgfred(<a>, <b>, <t>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("lambert_w"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lambert_w(<z>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("nzeta"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nzeta(<z>)"));                              // OPTION
  m_wordList.at(command).push_back(wxS("nzetar"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nzetar(<z>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("nzetai"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("nzetai(<z>)"));                             // OPTION
  m_wordList.at(command).push_back(wxS("inference_result"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "inference_result(<title>, <values>, <numbers>)");       // OPTION
  m_wordList.at(command).push_back(wxS("inferencep"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("inferencep(<obj>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("items_inference"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("items_inference(<obj>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("take_inference"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("take_inference(<n>, <obj>)"));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("take_inference(<name>, <obj>)"));     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("take_inference(<list>, <obj>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("stats_numer"));                      // OPTION
  m_wordList.at(command).push_back(wxS("test_mean"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_mean(<x>)"));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_mean(<x>, <options>, <...>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("test_means_difference"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_means_difference(<x1>, <x2>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "test_means_difference(<x1>, <x2>, <options>, <...>)");     // OPTION
  m_wordList.at(command).push_back(wxS("test_variance"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_variance(<x>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_variance(<x>, <options>, <...>)")); // OPTION
  m_wordList.at(command).push_back(wxS("test_variance_ratio"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_variance_ratio(<x1>, <x2>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "test_variance_ratio(<x1>, <x2>, <options>, <...>)"); // OPTION
  m_wordList.at(command).push_back(wxS("test_proportion"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_proportion(<x>, <n>)"));      // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "test_proportion(<x>, <n>, <options>, <...>)");     // OPTION
  m_wordList.at(command).push_back(wxS("test_proportions_difference")); // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "test_proportions_difference(<x1>, <n1>, <x2>, <n2>)"); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_proportions_difference(<x1>, <n1>, <x2>, <n2>, "
                                       "<options>, <...>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("test_sign"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_sign(<x>)"));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_sign(<x>, <options>, <...>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("test_signed_rank"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_signed_rank(<x>)"));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_signed_rank(<x>, <options>, <...>)")); // OPTION
  m_wordList.at(command).push_back(wxS("test_rank_sum"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_rank_sum(<x1>, <x2>)"));               // OPTION
  m_wordList.at(tmplte ).push_back(wxS("test_rank_sum(<x1>, <x2>, <option>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("test_normality"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("test_normality(<x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("simple_linear_regression"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("simple_linear_regression(<x>)"));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("simple_linear_regression(<x>, <option>)")); // OPTION
  m_wordList.at(command).push_back(wxS("pdf_signed_rank"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_signed_rank(<x>, <n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("cdf_signed_rank"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_signed_rank(<x>, <n>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("pdf_rank_sum"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pdf_rank_sum(<x>, <n>, <m>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("cdf_rank_sum"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cdf_rank_sum(<x>, <n>, <m>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("stirling"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("stirling(<z>,<n>)"));                       // OPTION
  m_wordList.at(tmplte ).push_back(wxS("stirling(<z>,<n>,<pred>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("close"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("close(<stream>) "));                        // OPTION
  m_wordList.at(command).push_back(wxS("flength"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("flength(<stream>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("fposition"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fposition(<stream>)"));                     // OPTION
  m_wordList.at(tmplte ).push_back(wxS("fposition(<stream>, <pos>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("freshline"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("freshline() "));                            // OPTION
  m_wordList.at(tmplte ).push_back(wxS("freshline(<stream>) "));                    // OPTION
  m_wordList.at(command).push_back(wxS("newline"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("newline() "));                              // OPTION
  m_wordList.at(tmplte ).push_back(wxS("newline(<stream>) "));                      // OPTION
  m_wordList.at(command).push_back(wxS("opena"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("opena(<file>) "));                          // OPTION
  m_wordList.at(command).push_back(wxS("openr"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("openr(<file>) "));                          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("openr(<file>,<encoding_name_as_string>) ")); // OPTION
  m_wordList.at(command).push_back(wxS("openw"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("openw(<file>) ")); // OPTION
  m_wordList.at(tmplte ).push_back(wxS("openw(<file>,<encoding_name_as_string>) ")); // OPTION
  m_wordList.at(command).push_back(wxS("printf"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("printf(<dest>, <string>)")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "printf(<dest>, <string>, <expr_1>, <...>, <expr_n>)");        // OPTION
  m_wordList.at(command).push_back(wxS("readline"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("readline(<stream>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("sprint"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sprint(<expr_1>, <...>, <expr_n>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("alphacharp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("alphacharp(<char>)    "));                  // OPTION
  m_wordList.at(command).push_back(wxS("alphanumericp"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("alphanumericp(<char>) "));                  // OPTION
  m_wordList.at(command).push_back(wxS("askequal"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("askequal(<exp1>,<exp2>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("ascii"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ascii(<int>) "));                           // OPTION
  m_wordList.at(command).push_back(wxS("cequal"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cequal(<char_1>, <char_2>)          "));    // OPTION
  m_wordList.at(command).push_back(wxS("cequalignore"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cequalignore(<char_1>, <char_2>)    "));    // OPTION
  m_wordList.at(command).push_back(wxS("cgreaterp"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cgreaterp(<char_1>, <char_2>)       "));    // OPTION
  m_wordList.at(command).push_back(wxS("cgreaterpignore"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cgreaterpignore(<char_1>, <char_2>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("charp"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charp(<obj>) "));                           // OPTION
  m_wordList.at(command).push_back(wxS("cint"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cint(<char>) "));                           // OPTION
  m_wordList.at(command).push_back(wxS("clessp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clessp(<char_1>, <char_2>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("clesspignore"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("clesspignore(<char_1>, <char_2>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("constituent"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("constituent(<char>)   "));                  // OPTION
  m_wordList.at(command).push_back(wxS("cunlisp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cunlisp(<lisp_char>) "));                   // OPTION
  m_wordList.at(command).push_back(wxS("digitcharp"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("digitcharp(<char>)    "));                  // OPTION
  m_wordList.at(command).push_back(wxS("lcharp"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lcharp(<obj>) "));                          // OPTION
  m_wordList.at(command).push_back(wxS("lowercasep"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lowercasep(<char>)    "));                  // OPTION
  m_wordList.at(command).push_back(wxS("newline"));                                // OPTION
  m_wordList.at(command).push_back(wxS("space"));                                  // OPTION
  m_wordList.at(command).push_back(wxS("tab"));                                    // OPTION
  m_wordList.at(command).push_back(wxS("uppercasep"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("uppercasep(<char>)    "));                  // OPTION
  m_wordList.at(command).push_back(wxS("stringp"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("stringp(<obj>) "));                         // OPTION
  m_wordList.at(command).push_back(wxS("charat"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charat(<string>, <n>) "));                  // OPTION
  m_wordList.at(command).push_back(wxS("charlist"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("charlist(<string>) "));                     // OPTION
  m_wordList.at(command).push_back(wxS("eval_string"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eval_string(<str>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("parse_string"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("parse_string(<str>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("scopy"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("scopy(<string>) "));                        // OPTION
  m_wordList.at(command).push_back(wxS("sdowncase"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sdowncase(<string>) "));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sdowncase(<string>, <start>) "));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sdowncase(<string>, <start>, <end>) "));    // OPTION
  m_wordList.at(command).push_back(wxS("sequal"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sequal(<string_1>, <string_2>) "));         // OPTION
  m_wordList.at(command).push_back(wxS("sequalignore"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sequalignore(<string_1>, <string_2>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("sexplode"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sexplode(<string>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("simplode"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("simplode(<list>)  "));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("simplode(<list>, <delim>)  "));             // OPTION
  m_wordList.at(command).push_back(wxS("sinsert"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sinsert(<seq>, <string>, <pos>)  "));       // OPTION
  m_wordList.at(command).push_back(wxS("sinvertcase"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sinvertcase(<string>)  "));                 // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sinvertcase(<string>, <start>)  "));        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sinvertcase(<string>, <start>, <end>)  ")); // OPTION
  m_wordList.at(command).push_back(wxS("slength"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("slength(<string>) "));                      // OPTION
  m_wordList.at(command).push_back(wxS("smake"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("smake(<num>, <char>) "));                   // OPTION
  m_wordList.at(command).push_back(wxS("smismatch"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("smismatch(<string_1>, <string_2>) "));      // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "smismatch(<string_1>, <string_2>, <test>) ");                // OPTION
  m_wordList.at(command).push_back(wxS("split"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("split(<string>)  "));                      // OPTION
  m_wordList.at(tmplte ).push_back(wxS("split(<string>, <delim>)  "));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("split(<string>, <delim>, <multiple>)  ")); // OPTION
  m_wordList.at(command).push_back(wxS("sposition"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sposition(<char>, <string>) "));           // OPTION
  m_wordList.at(command).push_back(wxS("sremove"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sremove(<seq>, <string>)  "));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sremove(<seq>, <string>, <test>)  "));     // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "sremove(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "sremove(<seq>, <string>, <test>, <start>, <end>)  ");         // OPTION
  m_wordList.at(command).push_back(wxS("sremovefirst"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sremovefirst(<seq>, <string>)  "));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("sremovefirst(<seq>, <string>, <test>)  ")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "sremovefirst(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "sremovefirst(<seq>, <string>, <test>, <start>, <end>)  "); // OPTION
  m_wordList.at(command).push_back(wxS("sreverse"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sreverse(<string>) "));                  // OPTION
  m_wordList.at(command).push_back(wxS("ssearch"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ssearch(<seq>, <string>)  "));           // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ssearch(<seq>, <string>, <test>)  "));   // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssearch(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssearch(<seq>, <string>, <test>, <start>, <end>)");           // OPTION
  m_wordList.at(command).push_back(wxS("ssort"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ssort(<string>) "));                        // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ssort(<string>, <test>) "));                // OPTION
  m_wordList.at(command).push_back(wxS("ssubst"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ssubst(<new>, <old>, <string>) "));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("ssubst(<new>, <old>, <string>, <test>) ")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssubst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssubst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList.at(command).push_back(wxS("ssubstfirst"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ssubstfirst(<new>, <old>, <string>) ")); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssubstfirst(<new>, <old>, <string>, <test>) "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssubstfirst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList.at(tmplte ).push_back(
                                   "ssubstfirst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList.at(command).push_back(wxS("strim"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("strim(<seq>,<string>) "));               // OPTION
  m_wordList.at(command).push_back(wxS("striml"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("striml(<seq>, <string>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("strimr"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("strimr(<seq>, <string>) "));             // OPTION
  m_wordList.at(command).push_back(wxS("substring"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("substring(<string>, <start>)"));         // OPTION
  m_wordList.at(tmplte ).push_back(wxS("substring(<string>, <start>, <end>) ")); // OPTION
  m_wordList.at(command).push_back(wxS("supcase"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("supcase(<string>) "));                   // OPTION
  m_wordList.at(tmplte ).push_back(wxS("supcase(<string>, <start>) "));          // OPTION
  m_wordList.at(tmplte ).push_back(wxS("supcase(<string>, <start>, <end>) "));   // OPTION
  m_wordList.at(command).push_back(wxS("tokens"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tokens(<string>) "));                    // OPTION
  m_wordList.at(tmplte ).push_back(wxS("tokens(<string>, <test>) "));            // OPTION
  m_wordList.at(command).push_back(wxS("comp2pui"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("comp2pui(<n>, <L>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("ele2pui"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ele2pui(<m>, <L>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("ele2comp"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ele2comp(<m>, <L>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("elem"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elem(<ele>, <sym>, <lvar>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("mon2schur"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mon2schur(<L>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("multi_elem"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multi_elem(<l_elem>, <multi_pc>, <l_var>)")); // OPTION
  m_wordList.at(command).push_back(wxS("pui"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pui(<L>, <sym>, <lvar>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("pui2comp"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pui2comp(<n>, <lpui>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("pui2ele"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pui2ele(<n>, <lpui>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("puireduc"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("puireduc(<n>, <lpui>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("schur2comp"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("schur2comp(<P>, <l_var>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("cont2part"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cont2part(<pc>, <lvar>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("contract"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("contract(<psym>, <lvar>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("explose"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("explose(<pc>, <lvar>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("part2cont"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("part2cont(<ppart>, <lvar>)")); // OPTION
  m_wordList.at(command).push_back(wxS("partpol"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("partpol(<psym>, <lvar>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("tcontract"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tcontract(<pol>, <lvar>)"));   // OPTION
  m_wordList.at(command).push_back(wxS("tpartpol"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tpartpol(<pol>, <lvar>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("direct"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("direct([<p_1>, <...>, <p_n>], <y>, <f>, [<lvar_1>, "
                                       "<...>, <lvar_n>])"));    // OPTION
  m_wordList.at(command).push_back(wxS("directory"));           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("directory(<pattern>)")); // OPTION
  m_wordList.at(command).push_back(wxS("multi_orbit"));         // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "multi_orbit(<P>, [<lvar_1>, <lvar_2>,<...>, <lvar_p>])"); // OPTION
  m_wordList.at(command).push_back(wxS("multsym"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multsym(<ppart_1>, <ppart_2>, <n>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("orbit"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("orbit(<P>, <lvar>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("pui_direct"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pui_direct(<orbite>, [<lvar_1>, <...>, <lvar_n>], "
                                       "[<d_1>, <d_2>, <...>, <d_n>])")); // OPTION
  m_wordList.at(command).push_back(wxS("kostka"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("kostka(<part_1>, <part_2>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("lgtreillis"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("lgtreillis(<n>, <m>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("ltreillis"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ltreillis(<n>, <m>)"));           // OPTION
  m_wordList.at(command).push_back(wxS("treillis"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("treillis(<n>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("treinat"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("treinat(<part>)"));               // OPTION
  m_wordList.at(command).push_back(wxS("ele2polynome"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("ele2polynome(<L>, <z>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("polynome2ele"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("polynome2ele(<P>, <x>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("prodrac"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("prodrac(<L>, <k>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("pui2polynome"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pui2polynome(<x>, <lpui>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("somrac"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("somrac(<L>, <k>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("resolvante"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "resolvante(<P>, <x>, <f>, [<x_1>,<...>, <x_d>]) ");       // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_alternee1"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_alternee1(<P>, <x>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_bipartite"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_bipartite(<P>, <x>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_diedrale"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_diedrale(<P>, <x>)"));       // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_klein"));                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_klein(<P>, <x>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_klein3"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_klein3(<P>, <x>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_produit_sym"));             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_produit_sym(<P>, <x>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_unitaire"));                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_unitaire(<P>, <Q>, <x>)"));  // OPTION
  m_wordList.at(command).push_back(wxS("resolvante_vierer"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("resolvante_vierer(<P>, <x>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("multinomial"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("multinomial(<r>, <part>)"));            // OPTION
  m_wordList.at(command).push_back(wxS("permut"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("permut(<L>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("%piargs"));                            // OPTION
  m_wordList.at(command).push_back(wxS("%iargs"));                             // OPTION
  m_wordList.at(command).push_back(wxS("acos"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acos(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("acosh"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acosh(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("acot"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acot(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("acoth"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acoth(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("acsc"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acsc(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("acsch"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("acsch(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("asec"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("asec(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("asech"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("asech(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("asin"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("asin(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("asinh"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("asinh(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("atan"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("atan(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("atan2"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("atan2(<y>, <x>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("atanh"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("atanh(<x>)"));                          // OPTION
  m_wordList.at(command).push_back(wxS("atrig1"));                             // OPTION
  m_wordList.at(command).push_back(wxS("cos"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cos(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("cosh"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cosh(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("cot"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("cot(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("coth"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("coth(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("csc"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("csc(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("csch"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("csch(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("halfangles"));                         // OPTION
  m_wordList.at(command).push_back(wxS("ntrig"));                              // OPTION
  m_wordList.at(command).push_back(wxS("sec"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sec(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("sech"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sech(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("sin"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sin(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("sinh"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sinh(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("tan"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tan(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("tanh"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("tanh(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("trigexpand"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trigexpand(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("trigexpandplus"));                     // OPTION
  m_wordList.at(command).push_back(wxS("trigexpandtimes"));                    // OPTION
  m_wordList.at(command).push_back(wxS("triginverses"));                       // OPTION
  m_wordList.at(command).push_back(wxS("trigreduce"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trigreduce(<expr>, <x>)"));             // OPTION
  m_wordList.at(tmplte ).push_back(wxS("trigreduce(<expr>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("trigsign"));                           // OPTION
  m_wordList.at(command).push_back(wxS("trigsimp"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trigsimp(<expr>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("trigrat"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("trigrat(<expr>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("setunits"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("setunits(<list>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("uforget"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("uforget(<list>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("convert"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("convert(<expr>, <list>)"));             // OPTION
  m_wordList.at(command).push_back(wxS("usersetunits"));                       // OPTION
  m_wordList.at(command).push_back(wxS("metricexpandall"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("metricexpandall(<x>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("%unitexpand"));                        // OPTION
  m_wordList.at(command).push_back(wxS("AntiDifference"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("AntiDifference(<F_k>, <k>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("Gosper"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("Gosper(<F_k>, <k>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("GosperSum"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("GosperSum(<F_k>, <k>, <a>, <b>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("parGosper"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("parGosper(<F_{n,k}>, <k>, <n>, <d>)")); // OPTION
  m_wordList.at(command).push_back(wxS("parGosper"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("parGosper(<F_(n,k)>, <k>, <n>, <d>)")); // OPTION
  m_wordList.at(command).push_back(wxS("Zeilberger"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("Zeilberger(<F_{n,k}>, <k>, <n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("Zeilberger"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("Zeilberger(<F_(n,k)>, <k>, <n>)"));     // OPTION
  m_wordList.at(command).push_back(wxS("MAX_ORD"));                            // OPTION
  m_wordList.at(command).push_back(wxS("simplified_output"));                  // OPTION
  m_wordList.at(command).push_back(wxS("linear_solver"));                      // OPTION
  m_wordList.at(command).push_back(wxS("warnings"));                           // OPTION
  m_wordList.at(command).push_back(wxS("warning"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("warning(<expr>, <...>, <expr_n>)"));    // OPTION
  m_wordList.at(command).push_back(wxS("Gosper_in_Zeilberger"));               // OPTION
  m_wordList.at(command).push_back(wxS("trivial_solutions"));                  // OPTION
  m_wordList.at(command).push_back(wxS("mod_test"));                           // OPTION
  m_wordList.at(command).push_back(wxS("modular_linear_solver"));              // OPTION
  m_wordList.at(command).push_back(wxS("ev_point"));                           // OPTION
  m_wordList.at(command).push_back(wxS("mod_big_prime"));                      // OPTION
  m_wordList.at(command).push_back(wxS("mod_threshold"));                      // OPTION
  m_wordList.at(command).push_back(wxS("days360"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "days360(<year1>,<month1>,<day1>,<year2>,<month2>,<day2>)"); // OPTION
  m_wordList.at(command).push_back(wxS("fv"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fv(<rate>,<PV>,<num>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("pv"));                                   // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("pv(<rate>,<FV>,<num>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("graph_flow"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("graph_flow(<val>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("annuity_pv"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("annuity_pv(<rate>,<PV>,<num>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("annuity_fv"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("annuity_fv(<rate>,<FV>,<num>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("geo_annuity_pv"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "geo_annuity_pv(<rate>,<growing_rate>,<PV>,<num>)"); // OPTION
  m_wordList.at(command).push_back(wxS("geo_annuity_fv"));               // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "geo_annuity_fv(<rate>,<growing_rate>,<FV>,<num>)");       // OPTION
  m_wordList.at(command).push_back(wxS("amortization"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("amortization(<rate>,<amount>,<num>)")); // OPTION
  m_wordList.at(command).push_back(wxS("arit_amortization"));                  // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "arit_amortization(<rate>,<increment>,<amount>,<num>)"); // OPTION
  m_wordList.at(command).push_back(wxS("geo_amortization"));                 // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "geo_amortization(<rate>,<growing_rate>,<amount>,<num>)");   // OPTION
  m_wordList.at(command).push_back(wxS("saving"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("saving(<rate>,<amount>,<num>)"));         // OPTION
  m_wordList.at(command).push_back(wxS("npv"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("npv(<rate>,<val>)"));                     // OPTION
  m_wordList.at(command).push_back(wxS("irr"));                                  // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("irr(<val>,<IO>)"));                       // OPTION
  m_wordList.at(command).push_back(wxS("benefit_cost"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("benefit_cost(<rate>,<input>,<output>)")); // OPTION
  m_wordList.at(command).push_back(wxS("sierpinskiale"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sierpinskiale(<n>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("treefale"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("treefale(<n>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("fernfale"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("fernfale(<n>)"));                         // OPTION
  m_wordList.at(command).push_back(wxS("mandelbrot_set"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("mandelbrot_set(<x>, <y>)"));              // OPTION
  m_wordList.at(command).push_back(wxS("julia_set"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("julia_set(<x>, <y>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("julia_parameter"));                      // OPTION
  m_wordList.at(command).push_back(wxS("julia_sin"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("julia_sin(<x>, <y>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("snowmap"));                              // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("snowmap(<ent>, <nn>)"));                  // OPTION
  m_wordList.at(command).push_back(wxS("hilbertmap"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("hilbertmap(<nn>)"));                      // OPTION
  m_wordList.at(command).push_back(wxS("sierpinskimap"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("sierpinskimap(<nn>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("extra_integration_methods"));            // OPTION
  m_wordList.at(command).push_back(wxS("extra_definite_integration_methods"));   // OPTION
  m_wordList.at(command).push_back(wxS("intfugudu"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("intfugudu(<e>, <x>)"));                   // OPTION
  m_wordList.at(command).push_back(wxS("signum_to_abs"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("signum_to_abs(<e>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("convert_to_signum"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("convert_to_signum(<e>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("complex_number_p"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("complex_number_p(<x>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("compose_functions"));                    // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("compose_functions(<l>)"));                // OPTION
  m_wordList.at(command).push_back(wxS("dfloat"));                               // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("dfloat(<x>)"));                           // OPTION
  m_wordList.at(command).push_back(wxS("elim"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elim(<l>, <x>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("elim_allbut"));                          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("elim_allbut(<l>, <x>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("eliminate_using"));                      // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("eliminate_using(<l>, <e>, <x>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("fourier_elim"));                         // FUNCTION
  m_wordList.at(tmplte ).push_back(
                                   "fourier_elim([<eq1>, <eq2>, <...>], [<var1>, <var>, <...>])"); // OPTION
  m_wordList.at(command).push_back(wxS("isreal_p"));                            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("isreal_p(<e>)"));                        // OPTION
  m_wordList.at(command).push_back(wxS("new_variable"));                        // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("new_variable(<type>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("parg"));                                // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("parg(<x>)"));                            // OPTION
  m_wordList.at(command).push_back(wxS("real_imagpart_to_conjugate"));          // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("real_imagpart_to_conjugate(<e>)"));      // OPTION
  m_wordList.at(command).push_back(wxS("rectform_log_if_constant"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("rectform_log_if_constant(<e>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("simp_inequality"));                     // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("simp_inequality(<e>)"));                 // OPTION
  m_wordList.at(command).push_back(wxS("standardize_inverse_trig"));            // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("standardize_inverse_trig(<e>)"));        // OPTION
  m_wordList.at(command).push_back(wxS("to_poly"));                             // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("to_poly(<e>, <l>)"));                    // OPTION
  m_wordList.at(command).push_back(wxS("to_poly_solve"));                       // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("to_poly_solve(<e>, <l>, <[options]>)")); // OPTION
  m_wordList.at(unit).push_back(wxS("A"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("acre"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("amp"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ampere"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("astronomical_unit"));                      // OPTION
  m_wordList.at(unit).push_back(wxS("AU"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("becquerel"));                              // OPTION
  m_wordList.at(unit).push_back(wxS("Bq"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Btu"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("C"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("candela"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("cfm"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("cm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("coulomb"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("cup"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("day"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("F"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("fA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("farad"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("fC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("feet"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("fF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("fJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fl_oz"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("fluid_ounce"));                            // OPTION
  m_wordList.at(unit).push_back(wxS("fm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("fN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("foot"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("fPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("fs"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("ft"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("fWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("g"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("GA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("gallon"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("GC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Gg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("gill"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("GJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Gm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Gmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("GN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("GPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("grain"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("gram"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("gray"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("Gs"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("GWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("Gy"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("H"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("ha"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("hectare"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("henry"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("hertz"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("horsepower"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("hour"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("hp"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Hz"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("inch"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("J"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("joule"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("julian_year"));                            // OPTION
  m_wordList.at(unit).push_back(wxS("K"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("kA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kat"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("katal"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("kC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kelvin"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("kF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("kilogram"));                               // OPTION
  m_wordList.at(unit).push_back(wxS("kilometer"));                              // OPTION
  m_wordList.at(unit).push_back(wxS("kJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("km"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("kN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("kPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ks"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("kWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("l"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("lbf"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("lbm"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("light_year"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("liter"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("lumen"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("lux"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("m"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("mA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("meter"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("metric_ton"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("mF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Mg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("MHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("microA"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microC"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microF"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microg"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microgram"));                              // OPTION
  m_wordList.at(unit).push_back(wxS("microH"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microHz"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("microJ"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microK"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microm"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("micrometer"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("micron"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microN"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microOhm"));                               // OPTION
  m_wordList.at(unit).push_back(wxS("microPa"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("micros"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microS"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microsecond"));                            // OPTION
  m_wordList.at(unit).push_back(wxS("microT"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microV"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microW"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("microWb"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("mile"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("minute"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("mJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("ml"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Mm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("Mmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("mN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("MOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("mol"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("mole"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("month"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("mPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("MPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ms"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Ms"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("MW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("mWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("MWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("N"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("nA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("newton"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("nF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("ng"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("nJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("nN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("nPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ns"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("nWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ohm"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("Ohm"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ounce"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("oz"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pA"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("Pa"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("parsec"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("pascal"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("pc"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pC"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pF"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pg"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pH"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pHz"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("pint"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("pJ"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pK"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pm"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pmol"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("pN"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pOhm"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("pound_force"));                            // OPTION
  m_wordList.at(unit).push_back(wxS("pound_mass"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("pPa"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("ps"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pS"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("psi"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("pT"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pV"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pW"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("pWb"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("quart"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("R"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("rod"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("s"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("S"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("second"));                                 // OPTION
  m_wordList.at(unit).push_back(wxS("short_ton"));                              // OPTION
  m_wordList.at(unit).push_back(wxS("siemens"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("sievert"));                                // OPTION
  m_wordList.at(unit).push_back(wxS("slug"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("Sv"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("T"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("tablespoon"));                             // OPTION
  m_wordList.at(unit).push_back(wxS("tbsp"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("teaspoon"));                               // OPTION
  m_wordList.at(unit).push_back(wxS("tesla"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("tsp"));                                    // OPTION
  m_wordList.at(unit).push_back(wxS("V"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("volt"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("W"));                                      // OPTION
  m_wordList.at(unit).push_back(wxS("watt"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("Wb"));                                     // OPTION
  m_wordList.at(unit).push_back(wxS("weber"));                                  // OPTION
  m_wordList.at(unit).push_back(wxS("week"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("yard"));                                   // OPTION
  m_wordList.at(unit).push_back(wxS("year"));                                   // OPTION
  m_wordList.at(command).push_back(wxS("defstruct"));                           // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("defstruct(<struct(fields)>)"));          // OPTION
  m_wordList.at(command).push_back(wxS("structures"));                          // OPTION
  m_wordList.at(command).push_back(wxS("new"));                                 // FUNCTION
  m_wordList.at(tmplte ).push_back(wxS("new(<struct(fields)>)"));                // OPTION

  /// Add wxMaxima functions
  m_wordList.at(command).push_back(wxS("wxanimate_framerate"));
  m_wordList.at(command).push_back(wxS("wxanimate_autoplay"));
  m_wordList.at(command).push_back(wxS("wxplot_pngcairo"));
  m_wordList.at(command).push_back(wxS("wxplot_usesvg"));
  m_wordList.at(command).push_back(wxS("set_display"));
  m_wordList.at(command).push_back(wxS("wxplot2d"));
  m_wordList.at(tmplte ).push_back(wxS("wxplot2d(<expr>,<x_range>)"));
  m_wordList.at(command).push_back(wxS("wxplot3d"));
  m_wordList.at(tmplte ).push_back(wxS("wxplot3d(<expr>,<x_range>,<y_range>)"));
  m_wordList.at(command).push_back(wxS("wximplicit_plot"));
  m_wordList.at(command).push_back(wxS("wxcontour_plot"));
  m_wordList.at(command).push_back(wxS("wxanimate"));
  m_wordList.at(command).push_back(wxS("wxanimate_draw"));
  m_wordList.at(command).push_back(wxS("wxanimate_draw3d"));
  m_wordList.at(command).push_back(wxS("with_slider"));
  m_wordList.at(tmplte ).push_back(wxS("with_slider(<a_var>,<a_list>,<expr>,<x_range>)"));
  m_wordList.at(command).push_back(wxS("with_slider_draw"));
  m_wordList.at(command).push_back(wxS("with_slider_draw2d"));
  m_wordList.at(command).push_back(wxS("with_slider_draw3d"));
  m_wordList.at(command).push_back(wxS("wxdirname"));
  m_wordList.at(command).push_back(wxS("wxdraw"));
  m_wordList.at(command).push_back(wxS("wxdraw2d"));
  m_wordList.at(command).push_back(wxS("wxdraw3d"));
  m_wordList.at(command).push_back(wxS("wxfilename"));
  m_wordList.at(command).push_back(wxS("wxhistogram"));
  m_wordList.at(command).push_back(wxS("wxscatterplot"));
  m_wordList.at(command).push_back(wxS("wxbarsplot"));
  m_wordList.at(command).push_back(wxS("wxpiechart"));
  m_wordList.at(command).push_back(wxS("wxboxplot"));
  m_wordList.at(command).push_back(wxS("wxplot_size"));
  m_wordList.at(command).push_back(wxS("wxdraw_list"));
  m_wordList.at(command).push_back(wxS("wxbuild_info"));
  m_wordList.at(command).push_back(wxS("wxbug_report"));
  m_wordList.at(command).push_back(wxS("show_image"));
  m_wordList.at(tmplte ).push_back(wxS("show_image(<imagename>)"));
  m_wordList.at(command).push_back(wxS("table_form"));
  m_wordList.at(tmplte ).push_back(wxS("table_form(<data>)"));
  m_wordList.at(command).push_back(wxS("wxsubscripts"));
  m_wordList.at(command).push_back(wxS("wxdeclare_subscripted"));
  m_wordList.at(tmplte ).push_back(wxS("wxdeclare_subscripted(<name>,<[false]>)"));
  m_wordList.at(command).push_back(wxS("wxdeclare_subscript"));
  m_wordList.at(tmplte ).push_back(wxS("wxdeclare_subscript(<subscript>,<[false]>)"));
  m_wordList.at(command).push_back(wxS("wxanimate_from_imgfiles"));
  m_wordList.at(tmplte ).push_back(
                                   wxS("wxanimate_from_imgfiles(<filename>,<[filename,...]>)"));
  m_wordList.at(command).push_back(wxS("wxstatusbar"));
  m_wordList.at(tmplte ).push_back(wxS("wxstatusbar(<string>)"));
  m_wordList.at(command).push_back(wxS("wxmaximaversion"));
  m_wordList.at(command).push_back(wxS("wxwidgetsversion"));
  return true;
}
