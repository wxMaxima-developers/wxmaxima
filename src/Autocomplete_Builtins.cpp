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
  m_wordList[command].Add(wxS("?derivsimp"));    // OPTION
  m_wordList[command].Add(wxS("pathname_name")); // FUNCTION
  m_wordList[command].Add(wxS("fast_linsolve")); // FUNCTION
  m_wordList[tmplte].Add(wxS("fast_linsolve([<expr_1>, ..., <expr_m>], [<x_1>, "
			     "..., <x_n>])"));                // OPTION
  m_wordList[command].Add(wxS("guess_exact_value"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("guess_exact_value([<expr>]))")); // OPTION
  m_wordList[command].Add(wxS("grobner_basis"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("grobner_basis([<expr_1>, ..., <expr_m>])")); // OPTION
  m_wordList[command].Add(wxS("set_up_dot_simplifications")); // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "set_up_dot_simplifications(<eqns>, <check_through_degree>)")); // OPTION
  m_wordList[tmplte].Add(wxS("set_up_dot_simplifications(<eqns>)"));      // OPTION
  m_wordList[command].Add(wxS("declare_weights"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "declare_weights(<x_1>, <w_1>, ..., <x_n>, <w_n>)")); // OPTION
  m_wordList[command].Add(wxS("nc_degree"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("nc_degree(<p>)"));                // OPTION
  m_wordList[command].Add(wxS("dotsimp"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("dotsimp(<f>)"));                  // OPTION
  m_wordList[command].Add(wxS("fast_central_elements"));        // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "fast_central_elements([<x_1>, ..., <x_n>], <n>)"));        // OPTION
  m_wordList[command].Add(wxS("check_overlaps"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("check_overlaps(<n>, <add_to_simps>)")); // OPTION
  m_wordList[command].Add(wxS("mono"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("mono([<x_1>, ..., <x_n>], <n>)"));      // OPTION
  m_wordList[command].Add(wxS("monomial_dimensions"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("monomial_dimensions(<n>)"));            // OPTION
  m_wordList[command].Add(wxS("extract_linear_equations"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("extract_linear_equations([<p_1>, ..., <p_n>], "
			     "[<m_1>, ..., <m_n>])"));                   // OPTION
  m_wordList[command].Add(wxS("list_nc_monomials"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("list_nc_monomials([<p_1>, ..., <p_n>])")); // OPTION
  m_wordList[tmplte].Add(wxS("list_nc_monomials(<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("all_dotsimp_denoms"));                    // OPTION
  m_wordList[command].Add(wxS("array"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("array(<name>, <dim_1>, ..., <dim_n>)"));   // OPTION
  m_wordList[tmplte].Add(wxS(
			     "array(<name>, <type>, <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList[tmplte].Add(wxS(
			     "array([<name_1>, ..., <name_m>], <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList[command].Add(wxS("arrayapply"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("arrayapply(<A>, [<i_1>, ..., <i_n>])")); // OPTION
  m_wordList[command].Add(wxS("with_default_2d_display"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("with_default_2d_display(<cmd>)"));       // OPTION
  m_wordList[command].Add(wxS("arrayinfo"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("arrayinfo(<A>)"));                       // OPTION
  m_wordList[command].Add(wxS("arraymake"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("arraymake(<A>, [<i_1>, ..., <i_n>])"));  // OPTION
  m_wordList[command].Add(wxS("arrays"));                              // OPTION
  m_wordList[command].Add(wxS("bashindices"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("bashindices(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("fillarray"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("fillarray(<A>, <B>)"));                  // OPTION
  m_wordList[command].Add(wxS("listarray"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("listarray(<A>)"));                       // OPTION
  m_wordList[command].Add(wxS("make_array"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("make_array(<type>, <dim_1>, ..., <dim_n>)")); // OPTION
  m_wordList[command].Add(wxS("rearray"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("rearray(<A>, <dim_1>, ..., <dim_n>)"));  // OPTION
  m_wordList[command].Add(wxS("remarray"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("remarray(<A_1>, ..., <A_n>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("remarray(all)"));                        // OPTION
  m_wordList[command].Add(wxS("subvar"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("subvar(<x>, <i>)"));                     // OPTION
  m_wordList[command].Add(wxS("use_fast_arrays"));                     // OPTION
  m_wordList[command].Add(wxS("init_atensor"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("init_atensor(<alg_type>, <opt_dims>)")); // OPTION
  m_wordList[tmplte].Add(wxS("init_atensor(<alg_type>)"));             // OPTION
  m_wordList[command].Add(wxS("atensimp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("atensimp(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("adim"));                                // OPTION
  m_wordList[command].Add(wxS("aform"));                               // OPTION
  m_wordList[command].Add(wxS("asymbol"));                             // OPTION
  m_wordList[command].Add(wxS("sf"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("sf(<u>, <v>)"));                         // OPTION
  m_wordList[command].Add(wxS("af"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("af(<u>, <v>)"));                         // OPTION
  m_wordList[command].Add(wxS("av"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("av(<u>, <v>)"));                         // OPTION
  m_wordList[command].Add(wxS("abasep"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("abasep(<v>)"));                          // OPTION
  m_wordList[command].Add(wxS("augmented_lagrangian_method"));         // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>)")); // OPTION
  m_wordList[tmplte].Add(wxS("augmented_lagrangian_method(<FOM>, <xx>, <C>, <yy>, "
			     "optional_args)")); // OPTION
  m_wordList[tmplte].Add(wxS("augmented_lagrangian_method([<FOM>, <grad>], <xx>, "
			     "<C>, <yy>)")); // OPTION
  m_wordList[tmplte].Add(wxS("augmented_lagrangian_method([<FOM>, <grad>], <xx>, "
			     "<C>, <yy>, optional_args)")); // OPTION
  m_wordList[command].Add(wxS("bode_gain"));                // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "bode_gain(<H>, <range>, ...<plot_opts>...)")); // OPTION
  m_wordList[command].Add(wxS("bode_phase"));             // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "bode_phase(<H>, <range>, ...<plot_opts>...)"));   // OPTION
  m_wordList[command].Add(wxS("run_testsuite"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("run_testsuite([<options>])")); // OPTION
  m_wordList[command].Add(wxS("testsuite_files"));           // OPTION
  m_wordList[command].Add(wxS("share_testsuite_files"));     // OPTION
  m_wordList[command].Add(wxS("display_all"));               // OPTION
  m_wordList[command].Add(wxS("display_index_separator"));   // OPTION
  m_wordList[command].Add(wxS("display_known_bugs"));        // OPTION
  m_wordList[command].Add(wxS("tests"));                     // OPTION
  m_wordList[command].Add(wxS("time"));                      // OPTION
  m_wordList[command].Add(wxS("share_tests"));               // OPTION
  m_wordList[command].Add(wxS("bug_report"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("bug_report()"));               // OPTION
  m_wordList[command].Add(wxS("build_info"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("build_info()"));               // OPTION
  m_wordList[command].Add(wxS("alias"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("alias(<new_name_1>, <old_name_1>, ..., <new_name_n>, "
			     "<old_name_n>)"));                           // OPTION
  m_wordList[command].Add(wxS("debugmode"));                              // OPTION
  m_wordList[command].Add(wxS("ev"));                                     // FUNCTION
  m_wordList[tmplte].Add(wxS("ev(<expr>, <arg_1>, ..., <arg_n>)"));       // OPTION
  m_wordList[command].Add(wxS("eval"));                                   // OPTION
  m_wordList[command].Add(wxS("evflag"));                                 // OPTION
  m_wordList[command].Add(wxS("evfun"));                                  // OPTION
  m_wordList[command].Add(wxS("infeval"));                                // OPTION
  m_wordList[command].Add(wxS("kill"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("kill(<a_1>, ..., <a_n>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("kill(labels)"));                            // OPTION
  m_wordList[tmplte].Add(wxS("kill(inlabels, outlabels, linelabels)"));   // OPTION
  m_wordList[tmplte].Add(wxS("kill(<n>)"));                               // OPTION
  m_wordList[tmplte].Add(wxS("kill([<m>, <n>])"));                        // OPTION
  m_wordList[tmplte].Add(wxS("kill(values, functions, arrays, ...)"));    // OPTION
  m_wordList[tmplte].Add(wxS("kill(all)"));                               // OPTION
  m_wordList[tmplte].Add(wxS("kill(allbut (<a_1>, ..., <a_n>))"));        // OPTION
  m_wordList[command].Add(wxS("labels"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("labels(<symbol>)"));                        // OPTION
  m_wordList[command].Add(wxS("linenum"));                                // OPTION
  m_wordList[command].Add(wxS("myoptions"));                              // OPTION
  m_wordList[command].Add(wxS("nolabels"));                               // OPTION
  m_wordList[command].Add(wxS("optionset"));                              // OPTION
  m_wordList[command].Add(wxS("playback"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("playback()"));                              // OPTION
  m_wordList[tmplte].Add(wxS("playback(<n>)"));                           // OPTION
  m_wordList[tmplte].Add(wxS("playback([<m>, <n>])"));                    // OPTION
  m_wordList[tmplte].Add(wxS("playback([<m>])"));                         // OPTION
  m_wordList[tmplte].Add(wxS("playback(input)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("playback(slow)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("playback(time)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("playback(grind)"));                         // OPTION
  m_wordList[command].Add(wxS("printprops"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("printprops(<a>, <i>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("printprops([<a_1>, ..., <a_n>], <i>)"));    // OPTION
  m_wordList[tmplte].Add(wxS("printprops(all, <i>)"));                    // OPTION
  m_wordList[command].Add(wxS("prompt"));                                 // OPTION
  m_wordList[command].Add(wxS("quit"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("quit()"));                                  // OPTION
  m_wordList[command].Add(wxS("remfunction"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("remfunction(<f_1>, ..., <f_n>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("remfunction(all)"));                        // OPTION
  m_wordList[command].Add(wxS("reset"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("reset()"));                                 // OPTION
  m_wordList[command].Add(wxS("showtime"));                               // OPTION
  m_wordList[command].Add(wxS("to_lisp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("to_lisp()"));                               // OPTION
  m_wordList[command].Add(wxS("values"));                                 // OPTION
  m_wordList[command].Add(wxS("%e"));                                     // OPTION
  m_wordList[command].Add(wxS("%i"));                                     // OPTION
  m_wordList[command].Add(wxS("false"));                                  // OPTION
  m_wordList[command].Add(wxS("ind"));                                    // OPTION
  m_wordList[command].Add(wxS("inf"));                                    // OPTION
  m_wordList[command].Add(wxS("infinity"));                               // OPTION
  m_wordList[command].Add(wxS("minf"));                                   // OPTION
  m_wordList[command].Add(wxS("%phi"));                                   // OPTION
  m_wordList[command].Add(wxS("%pi"));                                    // OPTION
  m_wordList[command].Add(wxS("true"));                                   // OPTION
  m_wordList[command].Add(wxS("und"));                                    // OPTION
  m_wordList[command].Add(wxS("zeroa"));                                  // OPTION
  m_wordList[command].Add(wxS("zerob"));                                  // OPTION
  m_wordList[command].Add(wxS("activate"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("activate(<context_1>, ..., <context_n>)")); // OPTION
  m_wordList[command].Add(wxS("activecontexts"));                         // OPTION
  m_wordList[command].Add(wxS("assume"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("assume(<pred_1>, ..., <pred_n>)"));         // OPTION
  m_wordList[command].Add(wxS("assumescalar"));                           // OPTION
  m_wordList[command].Add(wxS("assume_pos"));                             // OPTION
  m_wordList[command].Add(wxS("assume_pos_pred"));                        // OPTION
  m_wordList[command].Add(wxS("context"));                                // OPTION
  m_wordList[command].Add(wxS("contexts"));                               // OPTION
  m_wordList[command].Add(wxS("deactivate"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("deactivate(<context_1>, ..., <context_n>)")); // OPTION
  m_wordList[command].Add(wxS("facts"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("facts(<item>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("facts()"));                         // OPTION
  m_wordList[command].Add(wxS("features"));                       // OPTION
  m_wordList[command].Add(wxS("forget"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("forget(<pred_1>, ..., <pred_n>)")); // OPTION
  m_wordList[tmplte].Add(wxS("forget(<L>)"));                     // OPTION
  m_wordList[command].Add(wxS("killcontext"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "killcontext(<context_1>, ..., <context_n>)"));       // OPTION
  m_wordList[command].Add(wxS("newcontext"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("newcontext(<name>)"));            // OPTION
  m_wordList[command].Add(wxS("supcontext"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("supcontext(<name>, <context>)")); // OPTION
  m_wordList[tmplte].Add(wxS("supcontext(<name>)"));            // OPTION
  m_wordList[command].Add(wxS("contrib_ode"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("contrib_ode(<eqn>, <y>, <x>)"));  // OPTION
  m_wordList[command].Add(wxS("odelin"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("odelin(<eqn>, <y>, <x>)"));       // OPTION
  m_wordList[command].Add(wxS("ode_check"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("ode_check(<eqn>, <soln>)"));      // OPTION
  m_wordList[command].Add(wxS("method"));                       // OPTION
  m_wordList[command].Add(wxS("%c"));                           // OPTION
  m_wordList[command].Add(wxS("%k1"));                          // OPTION
  m_wordList[command].Add(wxS("%k2"));                          // OPTION
  m_wordList[command].Add(wxS("gauss_a"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("gauss_a(<a>, <b>, <c>, <x>)"));   // OPTION
  m_wordList[command].Add(wxS("gauss_b"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("gauss_b(<a>, <b>, <c>, <x>)"));   // OPTION
  m_wordList[command].Add(wxS("dgauss_a"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("dgauss_a(<a>, <b>, <c>, <x>)"));  // OPTION
  m_wordList[command].Add(wxS("dgauss_b"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("dgauss_b(<a>, <b>, <c>, <x>)"));  // OPTION
  m_wordList[command].Add(wxS("kummer_m"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kummer_m(<a>, <b>, <x>)"));       // OPTION
  m_wordList[command].Add(wxS("kummer_u"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kummer_u(<a>, <b>, <x>)"));       // OPTION
  m_wordList[command].Add(wxS("dkummer_m"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("dkummer_m(<a>, <b>, <x>)"));      // OPTION
  m_wordList[command].Add(wxS("dkummer_u"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("dkummer_u(<a>, <b>, <x>)"));      // OPTION
  m_wordList[command].Add(wxS("csetup"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("csetup()"));                      // OPTION
  m_wordList[command].Add(wxS("cmetric"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("cmetric(<dis>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("cmetric()"));                     // OPTION
  m_wordList[command].Add(wxS("ct_coordsys"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "ct_coordsys(<coordinate_system>, <extra_arg>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("ct_coordsys(<coordinate_system>)"));        // OPTION
  m_wordList[command].Add(wxS("init_ctensor"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("init_ctensor()"));                          // OPTION
  m_wordList[command].Add(wxS("christof"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("christof(<dis>)"));                         // OPTION
  m_wordList[command].Add(wxS("ricci"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ricci(<dis>)"));                            // OPTION
  m_wordList[command].Add(wxS("uricci"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("uricci(<dis>)"));                           // OPTION
  m_wordList[command].Add(wxS("scurvature"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("scurvature()"));                            // OPTION
  m_wordList[command].Add(wxS("einstein"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("einstein(<dis>)"));                         // OPTION
  m_wordList[command].Add(wxS("leinstein"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("leinstein(<dis>)"));                        // OPTION
  m_wordList[command].Add(wxS("riemann"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("riemann(<dis>)"));                          // OPTION
  m_wordList[command].Add(wxS("lriemann"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("lriemann(<dis>)"));                         // OPTION
  m_wordList[command].Add(wxS("uriemann"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("uriemann(<dis>)"));                         // OPTION
  m_wordList[command].Add(wxS("rinvariant"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("rinvariant()"));                            // OPTION
  m_wordList[command].Add(wxS("weyl"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("weyl(<dis>)"));                             // OPTION
  m_wordList[command].Add(wxS("ctaylor"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("ctaylor()"));                               // OPTION
  m_wordList[command].Add(wxS("frame_bracket"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("frame_bracket(<fr>, <fri>, <diagframe>)")); // OPTION
  m_wordList[command].Add(wxS("nptetrad"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("nptetrad()"));                              // OPTION
  m_wordList[command].Add(wxS("psi"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("psi(<dis>)"));                              // OPTION
  m_wordList[command].Add(wxS("petrov"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("petrov()"));                                // OPTION
  m_wordList[command].Add(wxS("contortion"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("contortion(<tr>)"));                        // OPTION
  m_wordList[command].Add(wxS("nonmetricity"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("nonmetricity(<nm>)"));                      // OPTION
  m_wordList[command].Add(wxS("ctransform"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ctransform(<M>)"));                         // OPTION
  m_wordList[command].Add(wxS("findde"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("findde(<A>, <n>)"));                        // OPTION
  m_wordList[command].Add(wxS("cograd"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("cograd()"));                                // OPTION
  m_wordList[command].Add(wxS("contragrad"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("contragrad()"));                            // OPTION
  m_wordList[command].Add(wxS("dscalar"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("dscalar()"));                               // OPTION
  m_wordList[command].Add(wxS("checkdiv"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("checkdiv()"));                              // OPTION
  m_wordList[command].Add(wxS("cgeodesic"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("cgeodesic(<dis>)"));                        // OPTION
  m_wordList[command].Add(wxS("bdvac"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("bdvac(<f>)"));                              // OPTION
  m_wordList[command].Add(wxS("invariant1"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("invariant1()"));                            // OPTION
  m_wordList[command].Add(wxS("invariant2"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("invariant2()"));                            // OPTION
  m_wordList[command].Add(wxS("bimetric"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("bimetric()"));                              // OPTION
  m_wordList[command].Add(wxS("diagmatrixp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("diagmatrixp(<M>)"));                        // OPTION
  m_wordList[command].Add(wxS("symmetricp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("symmetricp(<M>)"));                         // OPTION
  m_wordList[command].Add(wxS("ntermst"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("ntermst(<f>)"));                            // OPTION
  m_wordList[command].Add(wxS("cdisplay"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cdisplay(<ten>)"));                         // OPTION
  m_wordList[command].Add(wxS("deleten"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("deleten(<L>, <n>)"));                       // OPTION
  m_wordList[command].Add(wxS("dim"));                                    // OPTION
  m_wordList[command].Add(wxS("diagmetric"));                             // OPTION
  m_wordList[command].Add(wxS("ctrgsimp"));                               // OPTION
  m_wordList[command].Add(wxS("cframe_flag"));                            // OPTION
  m_wordList[command].Add(wxS("ctorsion_flag"));                          // OPTION
  m_wordList[command].Add(wxS("cnonmet_flag"));                           // OPTION
  m_wordList[command].Add(wxS("ctayswitch"));                             // OPTION
  m_wordList[command].Add(wxS("ctayvar"));                                // OPTION
  m_wordList[command].Add(wxS("ctaypov"));                                // OPTION
  m_wordList[command].Add(wxS("ctaypt"));                                 // OPTION
  m_wordList[command].Add(wxS("gdet"));                                   // OPTION
  m_wordList[command].Add(wxS("ratchristof"));                            // OPTION
  m_wordList[command].Add(wxS("rateinstein"));                            // OPTION
  m_wordList[command].Add(wxS("ratriemann"));                             // OPTION
  m_wordList[command].Add(wxS("ratweyl"));                                // OPTION
  m_wordList[command].Add(wxS("lfg"));                                    // OPTION
  m_wordList[command].Add(wxS("ufg"));                                    // OPTION
  m_wordList[command].Add(wxS("riem"));                                   // OPTION
  m_wordList[command].Add(wxS("lriem"));                                  // OPTION
  m_wordList[command].Add(wxS("uriem"));                                  // OPTION
  m_wordList[command].Add(wxS("ric"));                                    // OPTION
  m_wordList[command].Add(wxS("uric"));                                   // OPTION
  m_wordList[command].Add(wxS("lg"));                                     // OPTION
  m_wordList[command].Add(wxS("ug"));                                     // OPTION
  m_wordList[command].Add(wxS("weyl"));                                   // OPTION
  m_wordList[command].Add(wxS("fb"));                                     // OPTION
  m_wordList[command].Add(wxS("kinvariant"));                             // OPTION
  m_wordList[command].Add(wxS("np"));                                     // OPTION
  m_wordList[command].Add(wxS("npi"));                                    // OPTION
  m_wordList[command].Add(wxS("tr"));                                     // OPTION
  m_wordList[command].Add(wxS("kt"));                                     // OPTION
  m_wordList[command].Add(wxS("nm"));                                     // OPTION
  m_wordList[command].Add(wxS("nmc"));                                    // OPTION
  m_wordList[command].Add(wxS("tensorkill"));                             // OPTION
  m_wordList[command].Add(wxS("ct_coords"));                              // OPTION
  m_wordList[command].Add(wxS("refcheck"));                               // OPTION
  m_wordList[command].Add(wxS("setcheck"));                               // OPTION
  m_wordList[command].Add(wxS("setcheckbreak"));                          // OPTION
  m_wordList[command].Add(wxS("setval"));                                 // OPTION
  m_wordList[command].Add(wxS("timer"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("timer(<f_1>, ..., <f_n>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("timer(all)"));                              // OPTION
  m_wordList[tmplte].Add(wxS("timer()"));                                 // OPTION
  m_wordList[command].Add(wxS("untimer"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("untimer(<f_1>, ..., <f_n>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("untimer()"));                               // OPTION
  m_wordList[command].Add(wxS("timer_devalue"));                          // OPTION
  m_wordList[command].Add(wxS("timer_info"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("timer_info(<f_1>, ..., <f_n>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("timer_info()"));                            // OPTION
  m_wordList[command].Add(wxS("trace"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("trace(<f_1>, ..., <f_n>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("trace(all)"));                              // OPTION
  m_wordList[tmplte].Add(wxS("trace()"));                                 // OPTION
  m_wordList[command].Add(wxS("trace_options"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "trace_options(<f>, <option_1>, ..., <option_n>)")); // OPTION
  m_wordList[tmplte].Add(wxS("trace_options(<f>)"));           // OPTION
  m_wordList[command].Add(wxS("untrace"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("untrace(<f_1>, ..., <f_n>)"));   // OPTION
  m_wordList[tmplte].Add(wxS("untrace()"));                    // OPTION
  m_wordList[command].Add(wxS("%ibes"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("%ibes[<n>](<x>) "));             // OPTION
  m_wordList[command].Add(wxS("%j"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("%j[<n>](<x>) "));                // OPTION
  m_wordList[command].Add(wxS("%k"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("%k[<n>](<x>) "));                // OPTION
  m_wordList[command].Add(wxS("%y"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("%y[<n>](<x>) "));                // OPTION
  m_wordList[command].Add(wxS("airy"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("airy(<x>)"));                    // OPTION
  m_wordList[command].Add(wxS("bessel"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("bessel(<z>, <a>) "));            // OPTION
  m_wordList[command].Add(wxS("expint"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("expint(<z>)"));                  // OPTION
  m_wordList[command].Add(wxS("g0"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("g0(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("g1"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("g1(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("gn"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("gn(<x>, <n>) "));                // OPTION
  m_wordList[command].Add(wxS("gauss"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("gauss(<mean>, <sd>)"));          // OPTION
  m_wordList[command].Add(wxS("i0"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("i0(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("i1"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("i1(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("in"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("in(<x>, <n>) "));                // OPTION
  m_wordList[command].Add(wxS("j0"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("j0(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("j1"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("j1(<x>) "));                     // OPTION
  m_wordList[command].Add(wxS("jn"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("jn(<x>, <n>) "));                // OPTION
  m_wordList[command].Add(wxS("continuous_freq"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("continuous_freq(<list>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("continuous_freq(<list>, <m>)")); // OPTION
  m_wordList[command].Add(wxS("discrete_freq"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("discrete_freq(<list>)"));        // OPTION
  m_wordList[command].Add(wxS("subsample"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "subsample(<data_matrix>, <predicate_function>)")); // OPTION
  m_wordList[tmplte].Add(wxS("subsample(<data_matrix>, <predicate_function>, "
			     "<col_num1>, <col_num2>, ...)"));     // OPTION
  m_wordList[command].Add(wxS("mean"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("mean(<list>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("mean(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("var"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("var(<list>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("var(<matrix>)"));                    // OPTION
  m_wordList[command].Add(wxS("var1"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("var1(<list>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("var1(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("std"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("std(<list>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("std(<matrix>)"));                    // OPTION
  m_wordList[command].Add(wxS("std1"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("std1(<list>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("std1(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("noncentral_moment"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("noncentral_moment(<list>, <k>)"));   // OPTION
  m_wordList[tmplte].Add(wxS("noncentral_moment(<matrix>, <k>)")); // OPTION
  m_wordList[command].Add(wxS("central_moment"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("central_moment(<list>, <k>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("central_moment(<matrix>, <k>)"));    // OPTION
  m_wordList[command].Add(wxS("cv"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("cv(<list>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("cv(<matrix>)"));                     // OPTION
  m_wordList[command].Add(wxS("smin"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("smin(<list>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("smin(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("smax"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("smax(<list>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("smax(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("range"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("range(<list>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("range(<matrix>)"));                  // OPTION
  m_wordList[command].Add(wxS("quantile"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile(<list>, <p>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("quantile(<matrix>, <p>)"));          // OPTION
  m_wordList[command].Add(wxS("median"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("median(<list>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("median(<matrix>)"));                 // OPTION
  m_wordList[command].Add(wxS("qrange"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("qrange(<list>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("qrange(<matrix>)"));                 // OPTION
  m_wordList[command].Add(wxS("mean_deviation"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_deviation(<list>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("mean_deviation(<matrix>)"));         // OPTION
  m_wordList[command].Add(wxS("median_deviation"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("median_deviation(<list>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("median_deviation(<matrix>)"));       // OPTION
  m_wordList[command].Add(wxS("harmonic_mean"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("harmonic_mean(<list>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("harmonic_mean(<matrix>)"));          // OPTION
  m_wordList[command].Add(wxS("geometric_mean"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("geometric_mean(<list>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("geometric_mean(<matrix>)"));         // OPTION
  m_wordList[command].Add(wxS("kurtosis"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis(<list>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("kurtosis(<matrix>)"));               // OPTION
  m_wordList[command].Add(wxS("skewness"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness(<list>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("skewness(<matrix>)"));               // OPTION
  m_wordList[command].Add(wxS("pearson_skewness"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("pearson_skewness(<list>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("pearson_skewness(<matrix>)"));       // OPTION
  m_wordList[command].Add(wxS("quartile_skewness"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("quartile_skewness(<list>)"));        // OPTION
  m_wordList[tmplte].Add(wxS("quartile_skewness(<matrix>)"));      // OPTION
  m_wordList[command].Add(wxS("cov"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("cov(<matrix>)"));                    // OPTION
  m_wordList[command].Add(wxS("cov1"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("cov1(<matrix>)"));                   // OPTION
  m_wordList[command].Add(wxS("global_variances"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("global_variances(<matrix>)"));       // OPTION
  m_wordList[tmplte].Add(wxS(
			     "global_variances(<matrix>, <logical_value>)"));       // OPTION
  m_wordList[command].Add(wxS("cor"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cor(<matrix>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("cor(<matrix>, <logical_value>)")); // OPTION
  m_wordList[command].Add(wxS("list_correlations"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("list_correlations(<matrix>)"));    // OPTION
  m_wordList[tmplte].Add(wxS(
			     "list_correlations(<matrix>, <logical_value>)")); // OPTION
  m_wordList[command].Add(wxS("histogram"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("histogram(<list>)"));         // OPTION
  m_wordList[tmplte].Add(wxS(
			     "histogram(<list>, <option_1>, <option_2>, ...)"));    // OPTION
  m_wordList[tmplte].Add(wxS("histogram(<one_column_matrix>)")); // OPTION
  m_wordList[tmplte].Add(wxS(
			     "histogram(<one_column_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("histogram(<one_row_matrix>)"));              // OPTION
  m_wordList[tmplte].Add(wxS(
			     "histogram(<one_row_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("scatterplot"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("scatterplot(<list>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS(
			     "scatterplot(<list>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("scatterplot(<matrix>)"));         // OPTION
  m_wordList[tmplte].Add(wxS(
			     "scatterplot(<matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("barsplot"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "barsplot(<data1>, <data2>, ..., <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("piechart"));        // FUNCTION
  m_wordList[tmplte].Add(wxS("piechart(<list>)")); // OPTION
  m_wordList[tmplte].Add(wxS(
			     "piechart(<list>, <option_1>, <option_2>, ...)"));    // OPTION
  m_wordList[tmplte].Add(wxS("piechart(<one_column_matrix>)")); // OPTION
  m_wordList[tmplte].Add(wxS(
			     "piechart(<one_column_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("piechart(<one_row_matrix>)"));              // OPTION
  m_wordList[tmplte].Add(wxS(
			     "piechart(<one_row_matrix>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("boxplot"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("boxplot(<data>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS(
			     "boxplot(<data>, <option_1>, <option_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("diag"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("diag(<lm>)"));                // OPTION
  m_wordList[command].Add(wxS("JF"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("JF(<lambda>,<n>)"));          // OPTION
  m_wordList[command].Add(wxS("jordan"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jordan(<mat>)"));             // OPTION
  m_wordList[command].Add(wxS("dispJordan"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("dispJordan(<l>)"));           // OPTION
  m_wordList[command].Add(wxS("minimalPoly"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("minimalPoly(<l>)"));          // OPTION
  m_wordList[command].Add(wxS("ModeMatrix"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("ModeMatrix(<A>,<l>)"));       // OPTION
  m_wordList[command].Add(wxS("mat_function"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_function(<f>,<mat>)"));   // OPTION
  m_wordList[command].Add(wxS("bc2"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "bc2(<solution>, <xval1>, <yval1>, <xval2>, <yval2>)")); // OPTION
  m_wordList[command].Add(wxS("desolve"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("desolve(<eqn>, <x>)"));              // OPTION
  m_wordList[tmplte].Add(wxS(
			     "desolve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])"));      // OPTION
  m_wordList[command].Add(wxS("ic1"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("ic1(<solution>, <xval>, <yval>)"));         // OPTION
  m_wordList[command].Add(wxS("ic2"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("ic2(<solution>, <xval>, <yval>, <dval>)")); // OPTION
  m_wordList[command].Add(wxS("ode2"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("ode2(<eqn>, <dvar>, <ivar>)"));             // OPTION
  m_wordList[command].Add(wxS("antid"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("antid(<expr>, <x>, <u(x)>) "));             // OPTION
  m_wordList[command].Add(wxS("antidiff"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("antidiff(<expr>, <x>, <u>(<x>))"));         // OPTION
  m_wordList[command].Add(wxS("atomgrad"));                               // OPTION
  m_wordList[command].Add(wxS("atvalue"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "atvalue(<expr>, [<x_1> = <a_1>, ..., <x_m> = <a_m>], <c>)")); // OPTION
  m_wordList[tmplte].Add(wxS("atvalue(<expr>, <x_1> = <a_1>, <c>)"));    // OPTION
  m_wordList[command].Add(wxS("cartan"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("cartan-"));                                // OPTION
  m_wordList[command].Add(wxS("del"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("del(<x>)"));                               // OPTION
  m_wordList[command].Add(wxS("delta"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("delta(<t>)"));                             // OPTION
  m_wordList[command].Add(wxS("dependencies"));                          // OPTION
  m_wordList[command].Add(wxS("depends"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("depends(<f_1>, <x_1>, ..., <f_n>, <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("derivabbrev"));                             // OPTION
  m_wordList[command].Add(wxS("derivdegree"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("derivdegree(<expr>, <y>, <x>)"));    // OPTION
  m_wordList[command].Add(wxS("derivlist"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("derivlist(<var_1>, ..., <var_k>)")); // OPTION
  m_wordList[command].Add(wxS("derivsubst"));                      // OPTION
  m_wordList[command].Add(wxS("diff"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "diff(<expr>, <x_1>, <n_1>, ..., <x_m>, <n_m>)")); // OPTION
  m_wordList[tmplte].Add(wxS("diff(<expr>, <x>, <n>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("diff(<expr>, <x>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("diff(<expr>)"));               // OPTION
  m_wordList[command].Add(wxS("diff"));                      // OPTION
  m_wordList[command].Add(wxS("dscalar"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("dscalar(<f>)"));               // OPTION
  m_wordList[command].Add(wxS("express"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("express(<expr>)"));            // OPTION
  m_wordList[command].Add(wxS("gradef"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "gradef(<f>(<x_1>, ..., <x_n>), <g_1>, ..., <g_m>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("gradef(<a>, <x>, <expr>)"));                // OPTION
  m_wordList[command].Add(wxS("gradefs"));                                // OPTION
  m_wordList[command].Add(wxS("laplace"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("laplace(<expr>, <t>, <s>)"));               // OPTION
  m_wordList[command].Add(wxS("pdf_normal"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_normal(<x>,<m>,<s>)"));                 // OPTION
  m_wordList[command].Add(wxS("cdf_normal"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_normal(<x>,<m>,<s>)"));                 // OPTION
  m_wordList[command].Add(wxS("quantile_normal"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_normal(<q>,<m>,<s>)"));            // OPTION
  m_wordList[command].Add(wxS("mean_normal"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_normal(<m>,<s>)"));                    // OPTION
  m_wordList[command].Add(wxS("var_normal"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("var_normal(<m>,<s>)"));                     // OPTION
  m_wordList[command].Add(wxS("std_normal"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("std_normal(<m>,<s>)"));                     // OPTION
  m_wordList[command].Add(wxS("skewness_normal"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_normal(<m>,<s>)"));                // OPTION
  m_wordList[command].Add(wxS("kurtosis_normal"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_normal(<m>,<s>)"));                // OPTION
  m_wordList[command].Add(wxS("random_normal"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("random_normal(<m>,<s>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("random_normal(<m>,<s>,<n>)"));              // OPTION
  m_wordList[command].Add(wxS("pdf_student_t"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_student_t(<x>,<n>)"));                  // OPTION
  m_wordList[command].Add(wxS("cdf_student_t"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_student_t(<x>,<n>)"));                  // OPTION
  m_wordList[command].Add(wxS("quantile_student_t"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_student_t(<q>,<n>)"));             // OPTION
  m_wordList[command].Add(wxS("mean_student_t"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_student_t(<n>)"));                     // OPTION
  m_wordList[command].Add(wxS("var_student_t"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("var_student_t(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("std_student_t"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("std_student_t(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("skewness_student_t"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_student_t(<n>)"));                 // OPTION
  m_wordList[command].Add(wxS("kurtosis_student_t"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_student_t(<n>)"));                 // OPTION
  m_wordList[command].Add(wxS("random_student_t"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("random_student_t(<n>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("random_student_t(<n>,<m>)"));               // OPTION
  m_wordList[command].Add(wxS("pdf_noncentral_student_t"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_noncentral_student_t(<x>,<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("cdf_noncentral_student_t"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_noncentral_student_t(<x>,<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("quantile_noncentral_student_t"));          // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "quantile_noncentral_student_t(<q>,<n>,<ncp>)"));            // OPTION
  m_wordList[command].Add(wxS("mean_noncentral_student_t"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("var_noncentral_student_t"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("var_noncentral_student_t(<n>,<ncp>)"));  // OPTION
  m_wordList[command].Add(wxS("std_noncentral_student_t"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("std_noncentral_student_t(<n>,<ncp>)"));  // OPTION
  m_wordList[command].Add(wxS("skewness_noncentral_student_t"));       // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("kurtosis_noncentral_student_t")); // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("random_noncentral_student_t"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("random_noncentral_student_t(<n>,<ncp>)")); // OPTION
  m_wordList[tmplte].Add(wxS(
			     "random_noncentral_student_t(<n>,<ncp>,<m>)"));                 // OPTION
  m_wordList[command].Add(wxS("pdf_chi2"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_chi2(<x>,<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("cdf_chi2"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_chi2(<x>,<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("quantile_chi2"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_chi2(<q>,<n>)"));                  // OPTION
  m_wordList[command].Add(wxS("mean_chi2"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_chi2(<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("var_chi2"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("var_chi2(<n>)"));                           // OPTION
  m_wordList[command].Add(wxS("std_chi2"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("std_chi2(<n>)"));                           // OPTION
  m_wordList[command].Add(wxS("skewness_chi2"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_chi2(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("kurtosis_chi2"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_chi2(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("random_chi2"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("random_chi2(<n>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("random_chi2(<n>,<m>)"));                    // OPTION
  m_wordList[command].Add(wxS("pdf_noncentral_chi2"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_noncentral_chi2(<x>,<n>,<ncp>)"));      // OPTION
  m_wordList[command].Add(wxS("cdf_noncentral_chi2"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_noncentral_chi2(<x>,<n>,<ncp>)"));      // OPTION
  m_wordList[command].Add(wxS("quantile_noncentral_chi2"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_noncentral_chi2(<q>,<n>,<ncp>)")); // OPTION
  m_wordList[command].Add(wxS("mean_noncentral_chi2"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_noncentral_chi2(<n>,<ncp>)"));         // OPTION
  m_wordList[command].Add(wxS("var_noncentral_chi2"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("var_noncentral_chi2(<n>,<ncp>)"));          // OPTION
  m_wordList[command].Add(wxS("std_noncentral_chi2"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("std_noncentral_chi2(<n>,<ncp>)"));          // OPTION
  m_wordList[command].Add(wxS("skewness_noncentral_chi2"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_noncentral_chi2(<n>,<ncp>)"));     // OPTION
  m_wordList[command].Add(wxS("kurtosis_noncentral_chi2"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_noncentral_chi2(<n>,<ncp>)"));     // OPTION
  m_wordList[command].Add(wxS("random_noncentral_chi2"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("random_noncentral_chi2(<n>,<ncp>)"));       // OPTION
  m_wordList[tmplte].Add(wxS("random_noncentral_chi2(<n>,<ncp>,<m>)"));   // OPTION
  m_wordList[command].Add(wxS("pdf_f"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_f(<x>,<m>,<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("cdf_f"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_f(<x>,<m>,<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("quantile_f"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_f(<q>,<m>,<n>)"));                 // OPTION
  m_wordList[command].Add(wxS("mean_f"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_f(<m>,<n>)"));                         // OPTION
  m_wordList[command].Add(wxS("var_f"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("var_f(<m>,<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("std_f"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("std_f(<m>,<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("skewness_f"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_f(<m>,<n>)"));                     // OPTION
  m_wordList[command].Add(wxS("kurtosis_f"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_f(<m>,<n>)"));                     // OPTION
  m_wordList[command].Add(wxS("random_f"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("random_f(<m>,<n>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("random_f(<m>,<n>,<k>)"));                   // OPTION
  m_wordList[command].Add(wxS("pdf_exp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_exp(<x>,<m>)"));                        // OPTION
  m_wordList[command].Add(wxS("cdf_exp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_exp(<x>,<m>)"));                        // OPTION
  m_wordList[command].Add(wxS("quantile_exp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_exp(<q>,<m>)"));                   // OPTION
  m_wordList[command].Add(wxS("mean_exp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_exp(<m>)"));                           // OPTION
  m_wordList[command].Add(wxS("var_exp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("var_exp(<m>)"));                            // OPTION
  m_wordList[command].Add(wxS("std_exp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("std_exp(<m>)"));                            // OPTION
  m_wordList[command].Add(wxS("skewness_exp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_exp(<m>)"));                       // OPTION
  m_wordList[command].Add(wxS("kurtosis_exp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_exp(<m>)"));                       // OPTION
  m_wordList[command].Add(wxS("random_exp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("random_exp(<m>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("random_exp(<m>,<k>)"));                     // OPTION
  m_wordList[command].Add(wxS("pdf_lognormal"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_lognormal(<x>,<m>,<s>)"));              // OPTION
  m_wordList[command].Add(wxS("cdf_lognormal"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_lognormal(<x>,<m>,<s>)"));              // OPTION
  m_wordList[command].Add(wxS("quantile_lognormal"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_lognormal(<q>,<m>,<s>)"));         // OPTION
  m_wordList[command].Add(wxS("mean_lognormal"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_lognormal(<m>,<s>)"));                 // OPTION
  m_wordList[command].Add(wxS("var_lognormal"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("var_lognormal(<m>,<s>)"));                  // OPTION
  m_wordList[command].Add(wxS("std_lognormal"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("std_lognormal(<m>,<s>)"));                  // OPTION
  m_wordList[command].Add(wxS("skewness_lognormal"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_lognormal(<m>,<s>)"));             // OPTION
  m_wordList[command].Add(wxS("kurtosis_lognormal"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_lognormal(<m>,<s>)"));             // OPTION
  m_wordList[command].Add(wxS("random_lognormal"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("random_lognormal(<m>,<s>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("random_lognormal(<m>,<s>,<n>)"));           // OPTION
  m_wordList[command].Add(wxS("pdf_gamma"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_gamma(<x>,<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("cdf_gamma"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_gamma(<x>,<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("quantile_gamma"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_gamma(<q>,<a>,<b>)"));             // OPTION
  m_wordList[command].Add(wxS("mean_gamma"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_gamma(<a>,<b>)"));                     // OPTION
  m_wordList[command].Add(wxS("var_gamma"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("var_gamma(<a>,<b>)"));                      // OPTION
  m_wordList[command].Add(wxS("std_gamma"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("std_gamma(<a>,<b>)"));                      // OPTION
  m_wordList[command].Add(wxS("skewness_gamma"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_gamma(<a>,<b>)"));                 // OPTION
  m_wordList[command].Add(wxS("kurtosis_gamma"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_gamma(<a>,<b>)"));                 // OPTION
  m_wordList[command].Add(wxS("random_gamma"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("random_gamma(<a>,<b>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("random_gamma(<a>,<b>,<n>)"));               // OPTION
  m_wordList[command].Add(wxS("pdf_beta"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_beta(<x>,<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("cdf_beta"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_beta(<x>,<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("quantile_beta"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_beta(<q>,<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("mean_beta"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_beta(<a>,<b>)"));                      // OPTION
  m_wordList[command].Add(wxS("var_beta"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("var_beta(<a>,<b>)"));                       // OPTION
  m_wordList[command].Add(wxS("std_beta"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("std_beta(<a>,<b>)"));                       // OPTION
  m_wordList[command].Add(wxS("skewness_beta"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_beta(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("kurtosis_beta"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_beta(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("random_beta"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("random_beta(<a>,<b>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("random_beta(<a>,<b>,<n>)"));                // OPTION
  m_wordList[command].Add(wxS("pdf_continuous_uniform"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_continuous_uniform(<x>,<a>,<b>)"));     // OPTION
  m_wordList[command].Add(wxS("cdf_continuous_uniform"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_continuous_uniform(<x>,<a>,<b>)"));     // OPTION
  m_wordList[command].Add(wxS("quantile_continuous_uniform"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_continuous_uniform(<q>,<a>,<b>)")); // OPTION
  m_wordList[command].Add(wxS("mean_continuous_uniform"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_continuous_uniform(<a>,<b>)"));       // OPTION
  m_wordList[command].Add(wxS("var_continuous_uniform"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("var_continuous_uniform(<a>,<b>)"));        // OPTION
  m_wordList[command].Add(wxS("std_continuous_uniform"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("std_continuous_uniform(<a>,<b>)"));        // OPTION
  m_wordList[command].Add(wxS("skewness_continuous_uniform"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_continuous_uniform(<a>,<b>)"));   // OPTION
  m_wordList[command].Add(wxS("kurtosis_continuous_uniform"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_continuous_uniform(<a>,<b>)"));   // OPTION
  m_wordList[command].Add(wxS("random_continuous_uniform"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("random_continuous_uniform(<a>,<b>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("random_continuous_uniform(<a>,<b>,<n>)")); // OPTION
  m_wordList[command].Add(wxS("pdf_logistic"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_logistic(<x>,<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("cdf_logistic"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_logistic(<x>,<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("quantile_logistic"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_logistic(<q>,<a>,<b>)"));         // OPTION
  m_wordList[command].Add(wxS("mean_logistic"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_logistic(<a>,<b>)"));                 // OPTION
  m_wordList[command].Add(wxS("var_logistic"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("var_logistic(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("std_logistic"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("std_logistic(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("skewness_logistic"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_logistic(<a>,<b>)"));             // OPTION
  m_wordList[command].Add(wxS("kurtosis_logistic"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_logistic(<a>,<b>)"));             // OPTION
  m_wordList[command].Add(wxS("random_logistic"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("random_logistic(<a>,<b>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("random_logistic(<a>,<b>,<n>)"));           // OPTION
  m_wordList[command].Add(wxS("pdf_pareto"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_pareto(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("cdf_pareto"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_pareto(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("quantile_pareto"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_pareto(<q>,<a>,<b>)"));           // OPTION
  m_wordList[command].Add(wxS("mean_pareto"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_pareto(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("var_pareto"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("var_pareto(<a>,<b>)"));                    // OPTION
  m_wordList[command].Add(wxS("std_pareto"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("std_pareto(<a>,<b>)"));                    // OPTION
  m_wordList[command].Add(wxS("skewness_pareto"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_pareto(<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("kurtosis_pareto"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_pareto(<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("random_pareto"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("random_pareto(<a>,<b>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("random_pareto(<a>,<b>,<n>)"));             // OPTION
  m_wordList[command].Add(wxS("pdf_weibull"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_weibull(<x>,<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("cdf_weibull"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_weibull(<x>,<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("quantile_weibull"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_weibull(<q>,<a>,<b>)"));          // OPTION
  m_wordList[command].Add(wxS("mean_weibull"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_weibull(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("var_weibull"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("var_weibull(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("std_weibull"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("std_weibull(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("skewness_weibull"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_weibull(<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("kurtosis_weibull"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_weibull(<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("random_weibull"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("random_weibull(<a>,<b>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("random_weibull(<a>,<b>,<n>)"));            // OPTION
  m_wordList[command].Add(wxS("pdf_rayleigh"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_rayleigh(<x>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("cdf_rayleigh"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_rayleigh(<x>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("quantile_rayleigh"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_rayleigh(<q>,<b>)"));             // OPTION
  m_wordList[command].Add(wxS("mean_rayleigh"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_rayleigh(<b>)"));                     // OPTION
  m_wordList[command].Add(wxS("var_rayleigh"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("var_rayleigh(<b>)"));                      // OPTION
  m_wordList[command].Add(wxS("std_rayleigh"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("std_rayleigh(<b>)"));                      // OPTION
  m_wordList[command].Add(wxS("skewness_rayleigh"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_rayleigh(<b>)"));                 // OPTION
  m_wordList[command].Add(wxS("kurtosis_rayleigh"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_rayleigh(<b>)"));                 // OPTION
  m_wordList[command].Add(wxS("random_rayleigh"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("random_rayleigh(<b>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("random_rayleigh(<b>,<n>)"));               // OPTION
  m_wordList[command].Add(wxS("pdf_laplace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_laplace(<x>,<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("cdf_laplace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_laplace(<x>,<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("quantile_laplace"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_laplace(<q>,<a>,<b>)"));          // OPTION
  m_wordList[command].Add(wxS("mean_laplace"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_laplace(<a>,<b>)"));                  // OPTION
  m_wordList[command].Add(wxS("var_laplace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("var_laplace(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("std_laplace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("std_laplace(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("skewness_laplace"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_laplace(<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("kurtosis_laplace"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_laplace(<a>,<b>)"));              // OPTION
  m_wordList[command].Add(wxS("random_laplace"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("random_laplace(<a>,<b>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("random_laplace(<a>,<b>,<n>)"));            // OPTION
  m_wordList[command].Add(wxS("pdf_cauchy"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_cauchy(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("cdf_cauchy"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_cauchy(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("quantile_cauchy"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_cauchy(<q>,<a>,<b>)"));           // OPTION
  m_wordList[command].Add(wxS("random_cauchy"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("random_cauchy(<a>,<b>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("random_cauchy(<a>,<b>,<n>)"));             // OPTION
  m_wordList[command].Add(wxS("pdf_gumbel"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_gumbel(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("cdf_gumbel"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_gumbel(<x>,<a>,<b>)"));                // OPTION
  m_wordList[command].Add(wxS("quantile_gumbel"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_gumbel(<q>,<a>,<b>)"));           // OPTION
  m_wordList[command].Add(wxS("mean_gumbel"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_gumbel(<a>,<b>)"));                   // OPTION
  m_wordList[command].Add(wxS("var_gumbel"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("var_gumbel(<a>,<b>)"));                    // OPTION
  m_wordList[command].Add(wxS("std_gumbel"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("std_gumbel(<a>,<b>)"));                    // OPTION
  m_wordList[command].Add(wxS("skewness_gumbel"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_gumbel(<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("kurtosis_gumbel"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_gumbel(<a>,<b>)"));               // OPTION
  m_wordList[command].Add(wxS("random_gumbel"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("random_gumbel(<a>,<b>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("random_gumbel(<a>,<b>,<n>)"));             // OPTION
  m_wordList[command].Add(wxS("pdf_binomial"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_binomial(<x>,<n>,<p>)"));              // OPTION
  m_wordList[command].Add(wxS("cdf_binomial"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_binomial(<x>,<n>,<p>)"));              // OPTION
  m_wordList[command].Add(wxS("quantile_binomial"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_binomial(<q>,<n>,<p>)"));         // OPTION
  m_wordList[command].Add(wxS("mean_binomial"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_binomial(<n>,<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("var_binomial"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("var_binomial(<n>,<p>)"));                  // OPTION
  m_wordList[command].Add(wxS("std_binomial"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("std_binomial(<n>,<p>)"));                  // OPTION
  m_wordList[command].Add(wxS("skewness_binomial"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_binomial(<n>,<p>)"));             // OPTION
  m_wordList[command].Add(wxS("kurtosis_binomial"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_binomial(<n>,<p>)"));             // OPTION
  m_wordList[command].Add(wxS("random_binomial"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("random_binomial(<n>,<p>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("random_binomial(<n>,<p>,<m>)"));           // OPTION
  m_wordList[command].Add(wxS("pdf_poisson"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_poisson(<x>,<m>)"));                   // OPTION
  m_wordList[command].Add(wxS("cdf_poisson"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_poisson(<x>,<m>)"));                   // OPTION
  m_wordList[command].Add(wxS("quantile_poisson"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_poisson(<q>,<m>)"));              // OPTION
  m_wordList[command].Add(wxS("mean_poisson"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_poisson(<m>)"));                      // OPTION
  m_wordList[command].Add(wxS("var_poisson"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("var_poisson(<m>)"));                       // OPTION
  m_wordList[command].Add(wxS("std_poisson"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("std_poisson(<m>)"));                       // OPTION
  m_wordList[command].Add(wxS("skewness_poisson"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_poisson(<m>)"));                  // OPTION
  m_wordList[command].Add(wxS("kurtosis_poisson"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_poisson(<m>)"));                  // OPTION
  m_wordList[command].Add(wxS("random_poisson"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("random_poisson(<m>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("random_poisson(<m>,<n>)"));                // OPTION
  m_wordList[command].Add(wxS("pdf_bernoulli"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_bernoulli(<x>,<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("cdf_bernoulli"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_bernoulli(<x>,<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("quantile_bernoulli"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_bernoulli(<q>,<p>)"));            // OPTION
  m_wordList[command].Add(wxS("mean_bernoulli"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_bernoulli(<p>)"));                    // OPTION
  m_wordList[command].Add(wxS("var_bernoulli"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("var_bernoulli(<p>)"));                     // OPTION
  m_wordList[command].Add(wxS("std_bernoulli"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("std_bernoulli(<p>)"));                     // OPTION
  m_wordList[command].Add(wxS("skewness_bernoulli"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_bernoulli(<p>)"));                // OPTION
  m_wordList[command].Add(wxS("kurtosis_bernoulli"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_bernoulli(<p>)"));                // OPTION
  m_wordList[command].Add(wxS("random_bernoulli"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("random_bernoulli(<p>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("random_bernoulli(<p>,<n>)"));              // OPTION
  m_wordList[command].Add(wxS("pdf_geometric"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_geometric(<x>,<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("cdf_geometric"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_geometric(<x>,<p>)"));                 // OPTION
  m_wordList[command].Add(wxS("quantile_geometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_geometric(<q>,<p>)"));            // OPTION
  m_wordList[command].Add(wxS("mean_geometric"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_geometric(<p>)"));                    // OPTION
  m_wordList[command].Add(wxS("var_geometric"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("var_geometric(<p>)"));                     // OPTION
  m_wordList[command].Add(wxS("std_geometric"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("std_geometric(<p>)"));                     // OPTION
  m_wordList[command].Add(wxS("skewness_geometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_geometric(<p>)"));                // OPTION
  m_wordList[command].Add(wxS("kurtosis_geometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_geometric(<p>)"));                // OPTION
  m_wordList[command].Add(wxS("random_geometric"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("random_geometric(<p>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("random_geometric(<p>,<n>)"));              // OPTION
  m_wordList[command].Add(wxS("pdf_discrete_uniform"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_discrete_uniform(<x>,<n>)"));          // OPTION
  m_wordList[command].Add(wxS("cdf_discrete_uniform"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_discrete_uniform(<x>,<n>)"));          // OPTION
  m_wordList[command].Add(wxS("quantile_discrete_uniform"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_discrete_uniform(<q>,<n>)"));     // OPTION
  m_wordList[command].Add(wxS("mean_discrete_uniform"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_discrete_uniform(<n>)"));             // OPTION
  m_wordList[command].Add(wxS("var_discrete_uniform"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("var_discrete_uniform(<n>)"));              // OPTION
  m_wordList[command].Add(wxS("std_discrete_uniform"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("std_discrete_uniform(<n>)"));              // OPTION
  m_wordList[command].Add(wxS("skewness_discrete_uniform"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_discrete_uniform(<n>)"));         // OPTION
  m_wordList[command].Add(wxS("kurtosis_discrete_uniform"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_discrete_uniform(<n>)"));         // OPTION
  m_wordList[command].Add(wxS("random_discrete_uniform"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("random_discrete_uniform(<n>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("random_discrete_uniform(<n>,<m>)"));       // OPTION
  m_wordList[command].Add(wxS("pdf_hypergeometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_hypergeometric(<x>,<n1>,<n2>,<n>)"));  // OPTION
  m_wordList[command].Add(wxS("cdf_hypergeometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_hypergeometric(<x>,<n1>,<n2>,<n>)"));  // OPTION
  m_wordList[command].Add(wxS("quantile_hypergeometric"));               // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "quantile_hypergeometric(<q>,<n1>,<n2>,<n>)"));                // OPTION
  m_wordList[command].Add(wxS("mean_hypergeometric"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_hypergeometric(<n1>,<n2>,<n>)"));     // OPTION
  m_wordList[command].Add(wxS("var_hypergeometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("var_hypergeometric(<n1>,<n2>,<n>)"));      // OPTION
  m_wordList[command].Add(wxS("std_hypergeometric"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("std_hypergeometric(<n1>,<n2>,<n>)"));      // OPTION
  m_wordList[command].Add(wxS("skewness_hypergeometric"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_hypergeometric(<n1>,<n2>,<n>)")); // OPTION
  m_wordList[command].Add(wxS("kurtosis_hypergeometric"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_hypergeometric(<n1>,<n2>,<n>)")); // OPTION
  m_wordList[command].Add(wxS("random_hypergeometric"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("random_hypergeometric(<n1>,<n2>,<n>)"));   // OPTION
  m_wordList[tmplte].Add(wxS("random_hypergeometric(<n1>,<n2>,<n>,<m>)")); // OPTION
  m_wordList[command].Add(wxS("pdf_negative_binomial"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_negative_binomial(<x>,<n>,<p>)"));      // OPTION
  m_wordList[command].Add(wxS("cdf_negative_binomial"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_negative_binomial(<x>,<n>,<p>)"));      // OPTION
  m_wordList[command].Add(wxS("quantile_negative_binomial"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("quantile_negative_binomial(<q>,<n>,<p>)")); // OPTION
  m_wordList[command].Add(wxS("mean_negative_binomial"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("mean_negative_binomial(<n>,<p>)"));         // OPTION
  m_wordList[command].Add(wxS("var_negative_binomial"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("var_negative_binomial(<n>,<p>)"));          // OPTION
  m_wordList[command].Add(wxS("std_negative_binomial"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("std_negative_binomial(<n>,<p>)"));          // OPTION
  m_wordList[command].Add(wxS("skewness_negative_binomial"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("skewness_negative_binomial(<n>,<p>)"));     // OPTION
  m_wordList[command].Add(wxS("kurtosis_negative_binomial"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("kurtosis_negative_binomial(<n>,<p>)"));     // OPTION
  m_wordList[command].Add(wxS("random_negative_binomial"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("random_negative_binomial(<n>,<p>)"));       // OPTION
  m_wordList[tmplte].Add(wxS("random_negative_binomial(<n>,<p>,<m>)"));   // OPTION
  m_wordList[command].Add(wxS("get_index_properties"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("get_index_properties(<var>)"));             // OPTION
  m_wordList[command].Add(wxS("declare_index_properties"));               // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "declare_index_properties(<var>, [<prop1>, <...>])")); // OPTION
  m_wordList[command].Add(wxS("presuperscript"));                // OPTION
  m_wordList[command].Add(wxS("postsuperscript"));               // OPTION
  m_wordList[command].Add(wxS("presubscript"));                  // OPTION
  m_wordList[command].Add(wxS("postsubscript"));                 // OPTION
  m_wordList[command].Add(wxS("draw"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "draw(<gr2d>, ..., <gr3d>, ..., <options>, ...)"));             // OPTION
  m_wordList[command].Add(wxS("draw2d"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("draw2d(<option>, <graphic_object>, ...)")); // OPTION
  m_wordList[command].Add(wxS("draw3d"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("draw3d(<option>, <graphic_object>, ...)")); // OPTION
  m_wordList[command].Add(wxS("draw_file"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "draw_file(<graphic option>, ..., <graphic object>, ...)")); // OPTION
  m_wordList[command].Add(wxS("multiplot_mode"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("multiplot_mode(<term>)"));               // OPTION
  m_wordList[command].Add(wxS("set_draw_defaults"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("set_draw_defaults(<graphic option>, ..., <graphic "
			     "object>, ...)"));           // OPTION
  m_wordList[command].Add(wxS("adapt_depth"));            // OPTION
  m_wordList[command].Add(wxS("axis_3d"));                // OPTION
  m_wordList[command].Add(wxS("axis_bottom"));            // OPTION
  m_wordList[command].Add(wxS("axis_left"));              // OPTION
  m_wordList[command].Add(wxS("axis_right"));             // OPTION
  m_wordList[command].Add(wxS("axis_top"));               // OPTION
  m_wordList[command].Add(wxS("border"));                 // OPTION
  m_wordList[command].Add(wxS("cbrange"));                // OPTION
  m_wordList[command].Add(wxS("cbtics"));                 // OPTION
  m_wordList[command].Add(wxS("color"));                  // OPTION
  m_wordList[command].Add(wxS("colorbox"));               // OPTION
  m_wordList[command].Add(wxS("columns"));                // OPTION
  m_wordList[command].Add(wxS("contour"));                // OPTION
  m_wordList[command].Add(wxS("contour_levels"));         // OPTION
  m_wordList[command].Add(wxS("data_file_name"));         // OPTION
  m_wordList[command].Add(wxS("delay"));                  // OPTION
  m_wordList[command].Add(wxS("enhanced3d"));             // OPTION
  m_wordList[command].Add(wxS("eps_height"));             // OPTION
  m_wordList[command].Add(wxS("eps_width"));              // OPTION
  m_wordList[command].Add(wxS("file_bgcolor"));           // OPTION
  m_wordList[command].Add(wxS("file_name"));              // OPTION
  m_wordList[command].Add(wxS("fill_color"));             // OPTION
  m_wordList[command].Add(wxS("fill_density"));           // OPTION
  m_wordList[command].Add(wxS("filled_func"));            // OPTION
  m_wordList[command].Add(wxS("font"));                   // OPTION
  m_wordList[command].Add(wxS("font_size"));              // OPTION
  m_wordList[command].Add(wxS("gnuplot_command"));        // OPTION
  m_wordList[command].Add(wxS("gnuplot_file_name"));      // OPTION
  m_wordList[command].Add(wxS("grid"));                   // OPTION
  m_wordList[command].Add(wxS("head_angle"));             // OPTION
  m_wordList[command].Add(wxS("head_both"));              // OPTION
  m_wordList[command].Add(wxS("head_length"));            // OPTION
  m_wordList[command].Add(wxS("head_type"));              // OPTION
  m_wordList[command].Add(wxS("ip_grid"));                // OPTION
  m_wordList[command].Add(wxS("ip_grid_in"));             // OPTION
  m_wordList[command].Add(wxS("key"));                    // OPTION
  m_wordList[command].Add(wxS("key_pos"));                // OPTION
  m_wordList[command].Add(wxS("label_alignment"));        // OPTION
  m_wordList[command].Add(wxS("label_orientation"));      // OPTION
  m_wordList[command].Add(wxS("line_type"));              // OPTION
  m_wordList[command].Add(wxS("line_width"));             // OPTION
  m_wordList[command].Add(wxS("logcb"));                  // OPTION
  m_wordList[command].Add(wxS("logx"));                   // OPTION
  m_wordList[command].Add(wxS("logx_secondary"));         // OPTION
  m_wordList[command].Add(wxS("logy"));                   // OPTION
  m_wordList[command].Add(wxS("logy_secondary"));         // OPTION
  m_wordList[command].Add(wxS("logz"));                   // OPTION
  m_wordList[command].Add(wxS("nticks"));                 // OPTION
  m_wordList[command].Add(wxS("palette"));                // OPTION
  m_wordList[command].Add(wxS("pdf_height"));             // OPTION
  m_wordList[command].Add(wxS("pdf_width"));              // OPTION
  m_wordList[command].Add(wxS("pic_height"));             // OPTION
  m_wordList[command].Add(wxS("pic_width"));              // OPTION
  m_wordList[command].Add(wxS("point_size"));             // OPTION
  m_wordList[command].Add(wxS("point_type"));             // OPTION
  m_wordList[command].Add(wxS("points_joined"));          // OPTION
  m_wordList[command].Add(wxS("proportional_axes"));      // OPTION
  m_wordList[command].Add(wxS("rot_horizontal"));         // OPTION
  m_wordList[command].Add(wxS("rot_vertical"));           // OPTION
  m_wordList[command].Add(wxS("surface_hide"));           // OPTION
  m_wordList[command].Add(wxS("terminal"));               // OPTION
  m_wordList[command].Add(wxS("title"));                  // OPTION
  m_wordList[command].Add(wxS("transform"));              // OPTION
  m_wordList[command].Add(wxS("transparent"));            // OPTION
  m_wordList[command].Add(wxS("tube_extremes"));          // OPTION
  m_wordList[command].Add(wxS("unit_vectors"));           // OPTION
  m_wordList[command].Add(wxS("user_preamble"));          // OPTION
  m_wordList[command].Add(wxS("wired_surface"));          // OPTION
  m_wordList[command].Add(wxS("x_voxel"));                // OPTION
  m_wordList[command].Add(wxS("xaxis"));                  // OPTION
  m_wordList[command].Add(wxS("xaxis_color"));            // OPTION
  m_wordList[command].Add(wxS("xaxis_secondary"));        // OPTION
  m_wordList[command].Add(wxS("xaxis_type"));             // OPTION
  m_wordList[command].Add(wxS("xaxis_width"));            // OPTION
  m_wordList[command].Add(wxS("xlabel"));                 // OPTION
  m_wordList[command].Add(wxS("xlabel_secondary"));       // OPTION
  m_wordList[command].Add(wxS("xrange"));                 // OPTION
  m_wordList[command].Add(wxS("xrange_secondary"));       // OPTION
  m_wordList[command].Add(wxS("xtics"));                  // OPTION
  m_wordList[command].Add(wxS("xtics_axis"));             // OPTION
  m_wordList[command].Add(wxS("xtics_rotate"));           // OPTION
  m_wordList[command].Add(wxS("xtics_rotate_secondary")); // OPTION
  m_wordList[command].Add(wxS("xtics_secondary"));        // OPTION
  m_wordList[command].Add(wxS("xtics_secondary_axis"));   // OPTION
  m_wordList[command].Add(wxS("xu_grid"));                // OPTION
  m_wordList[command].Add(wxS("xy_file"));                // OPTION
  m_wordList[command].Add(wxS("xyplane"));                // OPTION
  m_wordList[command].Add(wxS("y_voxel"));                // OPTION
  m_wordList[command].Add(wxS("yaxis"));                  // OPTION
  m_wordList[command].Add(wxS("yaxis_color"));            // OPTION
  m_wordList[command].Add(wxS("yaxis_secondary"));        // OPTION
  m_wordList[command].Add(wxS("yaxis_type"));             // OPTION
  m_wordList[command].Add(wxS("yaxis_width"));            // OPTION
  m_wordList[command].Add(wxS("ylabel"));                 // OPTION
  m_wordList[command].Add(wxS("ylabel_secondary"));       // OPTION
  m_wordList[command].Add(wxS("yrange"));                 // OPTION
  m_wordList[command].Add(wxS("yrange_secondary"));       // OPTION
  m_wordList[command].Add(wxS("ytics"));                  // OPTION
  m_wordList[command].Add(wxS("ytics_axis"));             // OPTION
  m_wordList[command].Add(wxS("ytics_rotate"));           // OPTION
  m_wordList[command].Add(wxS("ytics_rotate_secondary")); // OPTION
  m_wordList[command].Add(wxS("ytics_secondary"));        // OPTION
  m_wordList[command].Add(wxS("ytics_secondary_axis"));   // OPTION
  m_wordList[command].Add(wxS("yv_grid"));                // OPTION
  m_wordList[command].Add(wxS("z_voxel"));                // OPTION
  m_wordList[command].Add(wxS("zaxis"));                  // OPTION
  m_wordList[command].Add(wxS("zaxis_color"));            // OPTION
  m_wordList[command].Add(wxS("zaxis_type"));             // OPTION
  m_wordList[command].Add(wxS("zaxis_width"));            // OPTION
  m_wordList[command].Add(wxS("zlabel"));                 // OPTION
  m_wordList[command].Add(wxS("zrange"));                 // OPTION
  m_wordList[command].Add(wxS("ztics"));                  // OPTION
  m_wordList[command].Add(wxS("ztics_axis"));             // OPTION
  m_wordList[command].Add(wxS("ztics_rotate"));           // OPTION
  m_wordList[tmplte].Add(wxS(
			     "make_level_picture(<data>,<width>,<height>)")); // OPTION
  m_wordList[command].Add(wxS("boundaries_array"));        // OPTION
  m_wordList[command].Add(wxS("chaosgame"));               // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "chaosgame(<[[><x1>, <y1><]>...<[><xm>, <ym><]]>, <[><x0>, <y0><]>, <b>, "
			     "<n>, ..., options, ...);"));      // OPTION
  m_wordList[command].Add(wxS("evolution")); // FUNCTION
  m_wordList[tmplte].Add(wxS(
			     "evolution(<F>, <y0>, <n>, ..., options, ...);")); // OPTION
  m_wordList[command].Add(wxS("evolution2d"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("evolution2d(<[><F>, <G><]>, <[><u>, <v><]>, <[><u0>, "
			     "<y0><]>, <n>, ..., options, ...);")); // OPTION
  m_wordList[command].Add(wxS("ifs"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("ifs(<[><r1>, ..., <rm><]>, <[><A1>, ..., <Am><]>, "
			     "<[[><x1>, <y1><]>, ..., <[><xm>, <ym><]]>, <[><x0>, "
			     "<y0><]>, <n>, ..., options, ...);")); // OPTION
  m_wordList[command].Add(wxS("julia"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("julia(<x>, <y>, ...<options>...)"));  // OPTION
  m_wordList[command].Add(wxS("mandelbrot"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("mandelbrot(<options>)"));             // OPTION
  m_wordList[command].Add(wxS("orbits"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("orbits(<F>, <y0>, <n1>, <n2>, [<x>, <x0>, <xf>, "
			     "<xstep>], ...options...);"));             // OPTION
  m_wordList[command].Add(wxS("rk"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("rk(<ODE>, <var>, <initial>, <domain>)")); // OPTION
  m_wordList[tmplte].Add(wxS("rk([<ODE1>,...,<ODEm>], [<v1>,...,<vm>], "
			     "[<init1>,...,<initm>], <domain>)")); // OPTION
  m_wordList[command].Add(wxS("staircase"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("staircase(<F>, <y0>, <n>, ...options...);")); // OPTION
  m_wordList[command].Add(wxS("jacobi_sn"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_sn(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_cn"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_cn(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_dn"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_dn(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_ns"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_ns(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_sc"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_sc(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_sd"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_sd(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_nc"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_nc(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_cs"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_cs(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_cd"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_cd(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_nd"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_nd(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_ds"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_ds(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("jacobi_dc"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_dc(<u>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_sn"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_sn(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_cn"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_cn(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_dn"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_dn(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_ns"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_ns(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_sc"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_sc(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_sd"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_sd(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_nc"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_nc(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_cs"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_cs(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_cd"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_cd(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_nd"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_nd(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_ds"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_ds(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("inverse_jacobi_dc"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_jacobi_dc(<u>, <m>)"));  // OPTION
  m_wordList[command].Add(wxS("elliptic_f"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_f(<phi>, <m>)"));       // OPTION
  m_wordList[command].Add(wxS("elliptic_e"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_e(<phi>, <m>)"));       // OPTION
  m_wordList[command].Add(wxS("elliptic_eu"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_eu(<u>, <m>)"));        // OPTION
  m_wordList[command].Add(wxS("elliptic_pi"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_pi(<n>, <phi>, <m>)")); // OPTION
  m_wordList[command].Add(wxS("elliptic_kc"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_kc(<m>)"));             // OPTION
  m_wordList[command].Add(wxS("elliptic_ec"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("elliptic_ec(<m>)"));             // OPTION
  m_wordList[command].Add(wxS("%rnum_list"));                  // OPTION
  m_wordList[command].Add(wxS("algexact"));                    // OPTION
  m_wordList[command].Add(wxS("algsys"));                      // FUNCTION
  m_wordList[tmplte].Add(
			 "algsys([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[tmplte].Add(
			 "algsys([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])");       // OPTION
  m_wordList[command].Add(wxS("allroots"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("allroots(<expr>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("allroots(<eqn>)"));                         // OPTION
  m_wordList[command].Add(wxS("bfallroots"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bfallroots(<expr>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("bfallroots(<eqn>)"));                       // OPTION
  m_wordList[command].Add(wxS("backsubst"));                              // OPTION
  m_wordList[command].Add(wxS("breakup"));                                // OPTION
  m_wordList[command].Add(wxS("dimension"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("dimension(<eqn>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("dimension(<eqn_1>, ..., <eqn_n>)"));        // OPTION
  m_wordList[command].Add(wxS("dispflag"));                               // OPTION
  m_wordList[command].Add(wxS("funcsolve"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("funcsolve(<eqn>, <g>(<t>))"));              // OPTION
  m_wordList[command].Add(wxS("globalsolve"));                            // OPTION
  m_wordList[command].Add(wxS("ieqn"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("ieqn(<ie>, <unk>, <tech>, <n>, <guess>)")); // OPTION
  m_wordList[command].Add(wxS("ieqnprint"));                              // OPTION
  m_wordList[command].Add(wxS("lhs"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("lhs(<expr>)"));                             // OPTION
  m_wordList[command].Add(wxS("linsolve"));                               // FUNCTION
  m_wordList[tmplte].Add(
			 "linsolve([<expr_1>, ..., <expr_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add(wxS("linsolvewarn"));                         // OPTION
  m_wordList[command].Add(wxS("linsolve_params"));                      // OPTION
  m_wordList[command].Add(wxS("multiplicities"));                       // OPTION
  m_wordList[command].Add(wxS("nroots"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("nroots(<p>, <low>, <high>)"));            // OPTION
  m_wordList[command].Add(wxS("nthroot"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("nthroot(<p>, <n>)"));                     // OPTION
  m_wordList[command].Add(wxS("polyfactor"));                           // OPTION
  m_wordList[command].Add(wxS("programmode"));                          // OPTION
  m_wordList[command].Add(wxS("realonly"));                             // OPTION
  m_wordList[command].Add(wxS("realroots"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("realroots(<expr>, <bound>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("realroots(<eqn>, <bound>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("realroots(<expr>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("realroots(<eqn>)"));                      // OPTION
  m_wordList[command].Add(wxS("rhs"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("rhs(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("rootsconmode"));                         // OPTION
  m_wordList[command].Add(wxS("rootscontract"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("rootscontract(<expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("rootsepsilon"));                         // OPTION
  m_wordList[command].Add(wxS("solve"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("solve(<expr>, <x>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("solve(<expr>)"));                         // OPTION
  m_wordList[tmplte].Add(
			 "solve([<eqn_1>, ..., <eqn_n>], [<x_1>, ..., <x_n>])");    // OPTION
  m_wordList[command].Add(wxS("solvedecomposes"));                    // OPTION
  m_wordList[command].Add(wxS("solveexplicit"));                      // OPTION
  m_wordList[command].Add(wxS("solvefactors"));                       // OPTION
  m_wordList[command].Add(wxS("solvenullwarn"));                      // OPTION
  m_wordList[command].Add(wxS("solveradcan"));                        // OPTION
  m_wordList[command].Add(wxS("solvetrigwarn"));                      // OPTION
  m_wordList[command].Add(wxS("at"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("at(<expr>, [<eqn_1>, ..., <eqn_n>])")); // OPTION
  m_wordList[tmplte].Add(wxS("at(<expr>, <eqn>)"));                   // OPTION
  m_wordList[command].Add(wxS("box"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("box(<expr>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("box(<expr>, <a>)"));                    // OPTION
  m_wordList[command].Add(wxS("boxchar"));                            // OPTION
  m_wordList[command].Add(wxS("carg"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("carg(<z>)"));                           // OPTION
  m_wordList[command].Add(wxS("constantp"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("constantp(<expr>)"));                   // OPTION
  m_wordList[command].Add(wxS("declare"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("declare(<a_1>, <p_1>, <a_2>, <p_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("disolate"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("disolate(<expr>, <x_1>, ..., <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("dispform"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("dispform(<expr>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("dispform(<expr>, all)"));               // OPTION
  m_wordList[command].Add(wxS("distrib"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("distrib(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("dpart"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("dpart(<expr>, <n_1>, ..., <n_k>)"));    // OPTION
  m_wordList[command].Add(wxS("exp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("exp(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("%emode"));                             // OPTION
  m_wordList[command].Add(wxS("%enumer"));                            // OPTION
  m_wordList[command].Add(wxS("exptisolate"));                        // OPTION
  m_wordList[command].Add(wxS("exptsubst"));                          // OPTION
  m_wordList[command].Add(wxS("freeof"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("freeof(<x_1>, ..., <x_n>, <expr>)"));   // OPTION
  m_wordList[command].Add(wxS("genfact"));                            // FUNCTION
  m_wordList[command].Add(wxS("gentran"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("genfact(<x>, <y>, <z>)"));              // OPTION
  m_wordList[command].Add(wxS("imagpart"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("imagpart(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("infix"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("infix(<op>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("infix(<op>, <lbp>, <rbp>)"));           // OPTION
  m_wordList[tmplte].Add(
			 "infix(<op>, <lbp>, <rbp>, <lpos>, <rpos>, <pos>)");     // OPTION
  m_wordList[command].Add(wxS("inflag"));                           // OPTION
  m_wordList[command].Add(wxS("inpart"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("inpart(<expr>, <n_1>, ..., <n_k>)")); // OPTION
  m_wordList[command].Add(wxS("isolate"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("isolate(<expr>, <x>)"));              // OPTION
  m_wordList[command].Add(wxS("isolate_wrt_times"));                // OPTION
  m_wordList[command].Add(wxS("listconstvars"));                    // OPTION
  m_wordList[command].Add(wxS("listdummyvars"));                    // OPTION
  m_wordList[command].Add(wxS("listofvars"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("listofvars(<expr>)"));                // OPTION
  m_wordList[command].Add(wxS("lfreeof"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("lfreeof(<list>, <expr>)"));           // OPTION
  m_wordList[command].Add(wxS("lopow"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("lopow(<expr>, <x>)"));                // OPTION
  m_wordList[command].Add(wxS("lpart"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("lpart(<label>, <expr>, <n_1>, ..., <n_k>)")); // OPTION
  m_wordList[command].Add(wxS("multthru"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("multthru(<expr>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("multthru(<expr_1>, <expr_2>)")); // OPTION
  m_wordList[command].Add(wxS("nounify"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("nounify(<f>)"));                 // OPTION
  m_wordList[command].Add(wxS("nterms"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("nterms(<expr>)"));               // OPTION
  m_wordList[command].Add(wxS("op"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("op(<expr>)"));                   // OPTION
  m_wordList[command].Add(wxS("operatorp"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("operatorp(<expr>, <op>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("operatorp(<expr>, [<op_1>, ..., <op_n>])")); // OPTION
  m_wordList[command].Add(wxS("optimize"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("optimize(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("optimprefix"));                            // OPTION
  m_wordList[command].Add(wxS("ordergreat"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ordergreat(<v_1>, ..., <v_n>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("orderless(<v_1>, ..., <v_n>)"));            // OPTION
  m_wordList[command].Add(wxS("ordergreatp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ordergreatp(<expr_1>, <expr_2>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("orderlessp(<expr_1>, <expr_2>)"));          // OPTION
  m_wordList[command].Add(wxS("part"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("part(<expr>, <n_1>, ..., <n_k>)"));         // OPTION
  m_wordList[command].Add(wxS("partition"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("partition(<expr>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("partswitch"));                             // OPTION
  m_wordList[command].Add(wxS("pickapart"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("pickapart(<expr>, <n>)"));                  // OPTION
  m_wordList[command].Add(wxS("piece"));                                  // OPTION
  m_wordList[command].Add(wxS("polarform"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("polarform(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("powers"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("powers(<expr>, <x>)"));                     // OPTION
  m_wordList[command].Add(wxS("product"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("product(<expr>, <i>, <i_0>, <i_1>)"));      // OPTION
  m_wordList[command].Add(wxS("realpart"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("realpart(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("rectform"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("rectform(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("rembox"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("rembox(<expr>, unlabelled)"));              // OPTION
  m_wordList[tmplte].Add(wxS("rembox(<expr>, <label>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("rembox(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("sum"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("sum(<expr>, <i>, <i_0>, <i_1>)"));          // OPTION
  m_wordList[command].Add(wxS("lsum"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("lsum(<expr>, <x>, <L>)"));                  // OPTION
  m_wordList[command].Add(wxS("unorder"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("unorder()"));                               // OPTION
  m_wordList[command].Add(wxS("verbify"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("verbify(<f>)"));                            // OPTION
  m_wordList[command].Add(wxS("constvalue"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("constvalue(<x>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("declare_constvalue(<a>, <x>)"));            // OPTION
  m_wordList[command].Add(wxS("units"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("units(<x>)"));                              // OPTION
  m_wordList[tmplte].Add(wxS("declare_units(<a>, <u>)"));                 // OPTION
  m_wordList[command].Add(wxS("qty"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("qty(<x>)"));                                // OPTION
  m_wordList[tmplte].Add(wxS("declare_qty(<a>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("unitp"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("unitp(<x>)"));                              // OPTION
  m_wordList[command].Add(wxS("declare_unit_conversion"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("declare_unit_conversion(<u> = <v>, ...)")); // OPTION
  m_wordList[command].Add(wxS("declare_dimensions"));                     // FUNCTION
  m_wordList[tmplte].Add(
			 "declare_dimensions(<a_1>, <d_1>, ..., <a_n>, <d_n>)");     // OPTION
  m_wordList[tmplte].Add(wxS("remove_dimensions(<a_1>, ..., <a_n>)")); // OPTION
  m_wordList[command].Add(wxS("declare_fundamental_dimensions"));      // FUNCTION
  m_wordList[tmplte].Add(
			 "declare_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList[tmplte].Add(
			 "remove_fundamental_dimensions(<d_1>, <d_2>, <d_3>, ...)"); // OPTION
  m_wordList[command].Add(wxS("declare_fundamental_units"));           // FUNCTION
  m_wordList[tmplte].Add(
			 "declare_fundamental_units(<u_1>, <d_1>, ..., <u_n>, <d_n>)"); // OPTION
  m_wordList[tmplte].Add(
			 "remove_fundamental_units(<u_1>, ..., <u_n>)"); // OPTION
  m_wordList[command].Add(wxS("dimensions"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("dimensions(<x>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("dimensions_as_list(<x>)"));  // OPTION
  m_wordList[command].Add(wxS("fundamental_units"));       // FUNCTION
  m_wordList[tmplte].Add(wxS("fundamental_units(<x>)"));   // OPTION
  m_wordList[tmplte].Add(wxS("fundamental_units()"));      // OPTION
  m_wordList[command].Add(wxS("dimensionless"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("dimensionless(<L>)"));       // OPTION
  m_wordList[command].Add(wxS("natural_unit"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("natural_unit(<expr>, [<v_1>, ..., <v_n>])")); // OPTION
  m_wordList[command].Add(wxS("f90"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("f90(<expr_1>, ..., <expr_n>)"));    // OPTION
  m_wordList[command].Add(wxS("bffac"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("bffac(<expr>, <n>)"));              // OPTION
  m_wordList[command].Add(wxS("algepsilon"));                     // OPTION
  m_wordList[command].Add(wxS("bfloat"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("bfloat(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("bfloatp"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("bfloatp(<expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("bfpsi"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("bfpsi(<n>, <z>, <fpprec>)"));       // OPTION
  m_wordList[tmplte].Add(wxS("bfpsi0(<z>, <fpprec>)"));           // OPTION
  m_wordList[command].Add(wxS("bftorat"));                        // OPTION
  m_wordList[command].Add(wxS("bftrunc"));                        // OPTION
  m_wordList[command].Add(wxS("cbffac"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("cbffac(<z>, <fpprec>)"));           // OPTION
  m_wordList[command].Add(wxS("float"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("float(<expr>)"));                   // OPTION
  m_wordList[command].Add(wxS("float2bf"));                       // OPTION
  m_wordList[command].Add(wxS("floatnump"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("floatnump(<expr>)"));               // OPTION
  m_wordList[command].Add(wxS("fpprec"));                         // OPTION
  m_wordList[command].Add(wxS("fpprintprec"));                    // OPTION
  m_wordList[command].Add(wxS("numer_pbranch"));                  // OPTION
  m_wordList[command].Add(wxS("buildq"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("buildq(<L>, <expr>)"));             // OPTION
  m_wordList[command].Add(wxS("macroexpand"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("macroexpand(<expr>)"));             // OPTION
  m_wordList[command].Add(wxS("macroexpand1"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("macroexpand1(<expr>)"));            // OPTION
  m_wordList[command].Add(wxS("macros"));                         // OPTION
  m_wordList[command].Add(wxS("splice"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("splice(<a>)"));                     // OPTION
  m_wordList[command].Add(wxS("apply"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("apply(<F>, [<x_1>, ..., <x_n>])")); // OPTION
  m_wordList[command].Add(wxS("block"));                          // FUNCTION
  m_wordList[tmplte].Add(
			 "block([<v_1>, ..., <v_m>], <expr_1>, ..., <expr_n>)");        // OPTION
  m_wordList[tmplte].Add(wxS("block(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList[command].Add(wxS("break"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("break(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList[command].Add(wxS("catch"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("catch(<expr_1>, ..., <expr_n>)"));          // OPTION
  m_wordList[command].Add(wxS("compfile"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("compfile(<filename>, <f_1>, ..., <f_n>)")); // OPTION
  m_wordList[tmplte].Add(wxS("compfile(<filename>, functions)"));         // OPTION
  m_wordList[tmplte].Add(wxS("compfile(<filename>, all)"));               // OPTION
  m_wordList[command].Add(wxS("compile"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("compile(<f_1>, ..., <f_n>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("compile(functions)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("compile(all)"));                            // OPTION
  m_wordList[command].Add(wxS("define"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("define(<f>(<x_1>, ..., <x_n>), <expr>)"));  // OPTION
  m_wordList[tmplte].Add(wxS("define(<f>[<x_1>, ..., <x_n>], <expr>)"));  // OPTION
  m_wordList[tmplte].Add(
			 "define(funmake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList[tmplte].Add(
			 "define(arraymake (<f>, [<x_1>, ..., <x_n>]), <expr>)"); // OPTION
  m_wordList[tmplte].Add(wxS("define(ev (<expr_1>), <expr_2>)"));   // OPTION
  m_wordList[command].Add(wxS("define_variable"));                  // FUNCTION
  m_wordList[tmplte].Add(
			 "define_variable(<name>, <default_value>, <mode>)");         // OPTION
  m_wordList[command].Add(wxS("dispfun"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("dispfun(<f_1>, ..., <f_n>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("dispfun(all)"));                          // OPTION
  m_wordList[command].Add(wxS("functions"));                            // OPTION
  m_wordList[command].Add(wxS("fundef"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("fundef(<f>)"));                           // OPTION
  m_wordList[command].Add(wxS("funmake"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("funmake(<F>, [<arg_1>, ..., <arg_n>])")); // OPTION
  m_wordList[command].Add(wxS("lambda"));                               // FUNCTION
  m_wordList[tmplte].Add(
			 "lambda([<x_1>, ..., <x_m>], <expr_1>, ..., <expr_n>)");        // OPTION
  m_wordList[tmplte].Add(wxS("lambda([[<L>]], <expr_1>, ..., <expr_n>)")); // OPTION
  m_wordList[tmplte].Add(
			 "lambda([<x_1>, ..., <x_m>, [<L>]], <expr_1>, ..., <expr_n>)"); // OPTION
  m_wordList[command].Add(wxS("local"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("local(<v_1>, ..., <v_n>)")); // OPTION
  m_wordList[command].Add(wxS("macroexpansion"));          // OPTION
  m_wordList[command].Add(wxS("mode_checkp"));             // OPTION
  m_wordList[command].Add(wxS("mode_check_errorp"));       // OPTION
  m_wordList[command].Add(wxS("mode_check_warnp"));        // OPTION
  m_wordList[command].Add(wxS("mode_declare"));            // FUNCTION
  m_wordList[tmplte].Add(
			 "mode_declare(<y_1>, <mode_1>, ..., <y_n>, <mode_n>)");  // OPTION
  m_wordList[command].Add(wxS("mode_identity"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("mode_identity(<arg_1>, <arg_2>)"));   // OPTION
  m_wordList[command].Add(wxS("transcompile"));                     // OPTION
  m_wordList[command].Add(wxS("translate"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("translate(<f_1>, ..., <f_n>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("translate(functions)"));              // OPTION
  m_wordList[tmplte].Add(wxS("translate(all)"));                    // OPTION
  m_wordList[command].Add(wxS("translate_file"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("translate_file(<maxima_filename>)")); // OPTION
  m_wordList[tmplte].Add(
			 "translate_file(<maxima_filename>, <lisp_filename>)"); // OPTION
  m_wordList[command].Add(wxS("transrun"));                       // OPTION
  m_wordList[command].Add(wxS("tr_array_as_ref"));                // OPTION
  m_wordList[command].Add(wxS("tr_bound_function_applyp"));       // OPTION
  m_wordList[command].Add(wxS("tr_file_tty_messagesp"));          // OPTION
  m_wordList[command].Add(wxS("tr_float_can_branch_complex"));    // OPTION
  m_wordList[command].Add(wxS("tr_function_call_default"));       // OPTION
  m_wordList[command].Add(wxS("tr_numer"));                       // OPTION
  m_wordList[command].Add(wxS("tr_optimize_max_loop"));           // OPTION
  m_wordList[command].Add(wxS("tr_semicompile"));                 // OPTION
  m_wordList[command].Add(wxS("tr_state_vars"));                  // OPTION
  m_wordList[command].Add(wxS("tr_warnings_get"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("tr_warnings_get()"));               // OPTION
  m_wordList[command].Add(wxS("tr_warn_bad_function_calls"));     // OPTION
  m_wordList[command].Add(wxS("tr_warn_fexpr"));                  // OPTION
  m_wordList[command].Add(wxS("tr_warn_meval"));                  // OPTION
  m_wordList[command].Add(wxS("tr_warn_mode"));                   // OPTION
  m_wordList[command].Add(wxS("tr_warn_undeclared"));             // OPTION
  m_wordList[command].Add(wxS("tr_warn_undefined_variable"));     // OPTION
  m_wordList[command].Add(wxS("tr_windy"));                       // OPTION
  m_wordList[command].Add(wxS("compile_file"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("compile_file(<filename>)"));        // OPTION
  m_wordList[tmplte].Add(
			 "compile_file(<filename>, <compiled_filename>)"); // OPTION
  m_wordList[tmplte].Add(wxS("compile_file(<filename>, <compiled_filename>, "
			     "<lisp_filename>)"));                      // OPTION
  m_wordList[command].Add(wxS("declare_translated"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("declare_translated(<f_1>, <f_2>, ...)")); // OPTION
  m_wordList[command].Add(wxS("GGFINFINITY"));                          // OPTION
  m_wordList[command].Add(wxS("GGFCFMAX"));                             // OPTION
  m_wordList[command].Add(wxS("ggf"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ggf(<l>)"));                              // OPTION
  m_wordList[command].Add(wxS("create_graph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("create_graph(<v_list>, <e_list>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("create_graph(<n>, <e_list>)"));           // OPTION
  m_wordList[tmplte].Add(
			 "create_graph(<v_list>, <e_list>, <directed>)");              // OPTION
  m_wordList[command].Add(wxS("copy_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("copy_graph(<g>)"));                        // OPTION
  m_wordList[command].Add(wxS("circulant_graph"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("circulant_graph(<n>, <d>)"));              // OPTION
  m_wordList[command].Add(wxS("clebsch_graph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("clebsch_graph()"));                        // OPTION
  m_wordList[command].Add(wxS("complement_graph"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("complement_graph(<g>)"));                  // OPTION
  m_wordList[command].Add(wxS("complete_bipartite_graph"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("complete_bipartite_graph(<n>, <m>)"));     // OPTION
  m_wordList[command].Add(wxS("complete_graph"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("complete_graph(<n>)"));                    // OPTION
  m_wordList[command].Add(wxS("cycle_digraph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("cycle_digraph(<n>)"));                     // OPTION
  m_wordList[command].Add(wxS("cycle_graph"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cycle_graph(<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("cuboctahedron_graph"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("cuboctahedron_graph(<n>)"));               // OPTION
  m_wordList[command].Add(wxS("cube_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("cube_graph(<n>)"));                        // OPTION
  m_wordList[command].Add(wxS("dodecahedron_graph"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("dodecahedron_graph()"));                   // OPTION
  m_wordList[command].Add(wxS("empty_graph"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("empty_graph(<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("flower_snark"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("flower_snark(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("from_adjacency_matrix"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("from_adjacency_matrix(<A>)"));             // OPTION
  m_wordList[command].Add(wxS("frucht_graph"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("frucht_graph()"));                         // OPTION
  m_wordList[command].Add(wxS("graph_product"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_product(<g1>, <g1>)"));              // OPTION
  m_wordList[command].Add(wxS("graph_union"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_union(<g1>, <g1>)"));                // OPTION
  m_wordList[command].Add(wxS("grid_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("grid_graph(<n>, <m>)"));                   // OPTION
  m_wordList[command].Add(wxS("great_rhombicosidodecahedron_graph"));    // FUNCTION
  m_wordList[tmplte].Add(wxS("great_rhombicosidodecahedron_graph()"));   // OPTION
  m_wordList[command].Add(wxS("great_rhombicuboctahedron_graph"));       // FUNCTION
  m_wordList[tmplte].Add(wxS("great_rhombicuboctahedron_graph()"));      // OPTION
  m_wordList[command].Add(wxS("grotzch_graph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("grotzch_graph()"));                        // OPTION
  m_wordList[command].Add(wxS("heawood_graph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("heawood_graph()"));                        // OPTION
  m_wordList[command].Add(wxS("icosahedron_graph"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("icosahedron_graph()"));                    // OPTION
  m_wordList[command].Add(wxS("icosidodecahedron_graph"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("icosidodecahedron_graph()"));              // OPTION
  m_wordList[command].Add(wxS("induced_subgraph"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("induced_subgraph(<V>, <g>)"));             // OPTION
  m_wordList[command].Add(wxS("line_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("line_graph(<g>)"));                        // OPTION
  m_wordList[command].Add(wxS("make_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("make_graph(<vrt>, <f>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("make_graph(<vrt>, <f>, <oriented>)"));     // OPTION
  m_wordList[command].Add(wxS("mycielski_graph"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("mycielski_graph(<g>)"));                   // OPTION
  m_wordList[command].Add(wxS("new_graph"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("new_graph()"));                            // OPTION
  m_wordList[command].Add(wxS("path_digraph"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("path_digraph(<n>)"));                      // OPTION
  m_wordList[command].Add(wxS("path_graph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("path_graph(<n>)"));                        // OPTION
  m_wordList[command].Add(wxS("petersen_graph"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("petersen_graph()"));                       // OPTION
  m_wordList[tmplte].Add(wxS("petersen_graph(<n>, <d>)"));               // OPTION
  m_wordList[command].Add(wxS("random_bipartite_graph"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("random_bipartite_graph(<a>, <b>, <p>)"));  // OPTION
  m_wordList[command].Add(wxS("random_digraph"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("random_digraph(<n>, <p>)"));               // OPTION
  m_wordList[command].Add(wxS("random_regular_graph"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("random_regular_graph(<n>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("random_regular_graph(<n>, <d>)"));         // OPTION
  m_wordList[command].Add(wxS("random_graph"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("random_graph(<n>, <p>)"));                 // OPTION
  m_wordList[command].Add(wxS("random_graph1"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("random_graph1(<n>, <m>)"));                // OPTION
  m_wordList[command].Add(wxS("random_network"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("random_network(<n>, <p>, <w>)"));          // OPTION
  m_wordList[command].Add(wxS("random_tournament"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("random_tournament(<n>)"));                 // OPTION
  m_wordList[command].Add(wxS("random_tree"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("random_tree(<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("small_rhombicosidodecahedron_graph"));    // FUNCTION
  m_wordList[tmplte].Add(wxS("small_rhombicosidodecahedron_graph()"));   // OPTION
  m_wordList[command].Add(wxS("small_rhombicuboctahedron_graph"));       // FUNCTION
  m_wordList[tmplte].Add(wxS("small_rhombicuboctahedron_graph()"));      // OPTION
  m_wordList[command].Add(wxS("snub_cube_graph"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("snub_cube_graph()"));                      // OPTION
  m_wordList[command].Add(wxS("snub_dodecahedron_graph"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("snub_dodecahedron_graph()"));              // OPTION
  m_wordList[command].Add(wxS("truncated_cube_graph"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("truncated_cube_graph()"));                 // OPTION
  m_wordList[command].Add(wxS("truncated_dodecahedron_graph"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("truncated_dodecahedron_graph()"));         // OPTION
  m_wordList[command].Add(wxS("truncated_icosahedron_graph"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("truncated_icosahedron_graph()"));          // OPTION
  m_wordList[command].Add(wxS("truncated_tetrahedron_graph"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("truncated_tetrahedron_graph()"));          // OPTION
  m_wordList[command].Add(wxS("tutte_graph"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("tutte_graph()"));                          // OPTION
  m_wordList[command].Add(wxS("underlying_graph"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("underlying_graph(<g>)"));                  // OPTION
  m_wordList[command].Add(wxS("wheel_graph"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("wheel_graph(<n>)"));                       // OPTION
  m_wordList[command].Add(wxS("adjacency_matrix"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("adjacency_matrix(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("average_degree"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("average_degree(<gr>)"));                   // OPTION
  m_wordList[command].Add(wxS("biconnected_components"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("biconnected_components(<gr>)"));           // OPTION
  m_wordList[command].Add(wxS("bipartition"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("bipartition(<gr>)"));                      // OPTION
  m_wordList[command].Add(wxS("chromatic_index"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("chromatic_index(<gr>)"));                  // OPTION
  m_wordList[command].Add(wxS("chromatic_number"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("chromatic_number(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("clear_edge_weight"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("clear_edge_weight(<e>, <gr>)"));           // OPTION
  m_wordList[command].Add(wxS("clear_vertex_label"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("clear_vertex_label(<v>, <gr>)"));          // OPTION
  m_wordList[command].Add(wxS("connected_components"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("connected_components(<gr>)"));             // OPTION
  m_wordList[command].Add(wxS("diameter"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("diameter(<gr>)"));                         // OPTION
  m_wordList[command].Add(wxS("edge_coloring"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("edge_coloring(<gr>)"));                    // OPTION
  m_wordList[command].Add(wxS("degree_sequence"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("degree_sequence(<gr>)"));                  // OPTION
  m_wordList[command].Add(wxS("edge_connectivity"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("edge_connectivity(<gr>)"));                // OPTION
  m_wordList[command].Add(wxS("edges"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("edges(<gr>)"));                            // OPTION
  m_wordList[command].Add(wxS("get_edge_weight"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("get_edge_weight(<e>, <gr>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("get_edge_weight(<e>, <gr>, <ifnot>)"));    // OPTION
  m_wordList[command].Add(wxS("get_vertex_label"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("get_vertex_label(<v>, <gr>)"));            // OPTION
  m_wordList[command].Add(wxS("graph_charpoly"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_charpoly(<gr>, <x>)"));              // OPTION
  m_wordList[command].Add(wxS("graph_center"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_center(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("graph_eigenvalues"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_eigenvalues(<gr>)"));                // OPTION
  m_wordList[command].Add(wxS("graph_periphery"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_periphery(<gr>)"));                  // OPTION
  m_wordList[command].Add(wxS("graph_size"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_size(<gr>)"));                       // OPTION
  m_wordList[command].Add(wxS("graph_order"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_order(<gr>)"));                      // OPTION
  m_wordList[command].Add(wxS("girth"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("girth(<gr>)"));                            // OPTION
  m_wordList[command].Add(wxS("hamilton_cycle"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("hamilton_cycle(<gr>)"));                   // OPTION
  m_wordList[command].Add(wxS("hamilton_path"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("hamilton_path(<gr>)"));                    // OPTION
  m_wordList[command].Add(wxS("isomorphism"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("isomorphism(<gr1>, <gr2>)"));              // OPTION
  m_wordList[command].Add(wxS("in_neighbors"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("in_neighbors(<v>, <gr>)"));                // OPTION
  m_wordList[command].Add(wxS("is_biconnected"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("is_biconnected(<gr>)"));                   // OPTION
  m_wordList[command].Add(wxS("is_bipartite"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("is_bipartite(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("is_connected"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("is_connected(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("is_digraph"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("is_digraph(<gr>)"));                       // OPTION
  m_wordList[command].Add(wxS("is_edge_in_graph"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("is_edge_in_graph(<e>, <gr>)"));            // OPTION
  m_wordList[command].Add(wxS("is_graph"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("is_graph(<gr>)"));                         // OPTION
  m_wordList[command].Add(wxS("is_graph_or_digraph"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("is_graph_or_digraph(<gr>)"));              // OPTION
  m_wordList[command].Add(wxS("is_isomorphic"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("is_isomorphic(<gr1>, <gr2>)"));            // OPTION
  m_wordList[command].Add(wxS("is_planar"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("is_planar(<gr>)"));                        // OPTION
  m_wordList[command].Add(wxS("is_sconnected"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("is_sconnected(<gr>)"));                    // OPTION
  m_wordList[command].Add(wxS("is_vertex_in_graph"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("is_vertex_in_graph(<v>, <gr>)"));          // OPTION
  m_wordList[command].Add(wxS("is_tree"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("is_tree(<gr>)"));                          // OPTION
  m_wordList[command].Add(wxS("laplacian_matrix"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("laplacian_matrix(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("max_clique"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("max_clique(<gr>)"));                       // OPTION
  m_wordList[command].Add(wxS("max_degree"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("max_degree(<gr>)"));                       // OPTION
  m_wordList[command].Add(wxS("max_flow"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("max_flow(<net>, <s>, <t>)"));              // OPTION
  m_wordList[command].Add(wxS("max_independent_set"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("max_independent_set(<gr>)"));              // OPTION
  m_wordList[command].Add(wxS("max_matching"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("max_matching(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("min_degree"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("min_degree(<gr>)"));                       // OPTION
  m_wordList[command].Add(wxS("min_edge_cut"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("min_edge_cut(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("min_vertex_cover"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("min_vertex_cover(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("min_vertex_cut"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("min_vertex_cut(<gr>)"));                   // OPTION
  m_wordList[command].Add(wxS("minimum_spanning_tree"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("minimum_spanning_tree(<gr>)"));            // OPTION
  m_wordList[command].Add(wxS("neighbors"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("neighbors(<v>, <gr>)"));                   // OPTION
  m_wordList[command].Add(wxS("odd_girth"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("odd_girth(<gr>)"));                        // OPTION
  m_wordList[command].Add(wxS("out_neighbors"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("out_neighbors(<v>, <gr>)"));               // OPTION
  m_wordList[command].Add(wxS("planar_embedding"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("planar_embedding(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("print_graph"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("print_graph(<gr>)"));                      // OPTION
  m_wordList[command].Add(wxS("radius"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("radius(<gr>)"));                           // OPTION
  m_wordList[command].Add(wxS("set_edge_weight"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("set_edge_weight(<e>, <w>, <gr>)"));        // OPTION
  m_wordList[command].Add(wxS("set_vertex_label"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("set_vertex_label(<v>, <l>, <gr>)"));       // OPTION
  m_wordList[command].Add(wxS("shortest_path"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("shortest_path(<u>, <v>, <gr>)"));          // OPTION
  m_wordList[command].Add(wxS("shortest_weighted_path"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("shortest_weighted_path(<u>, <v>, <gr>)")); // OPTION
  m_wordList[command].Add(wxS("strong_components"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("strong_components(<gr>)"));                // OPTION
  m_wordList[command].Add(wxS("topological_sort"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("topological_sort(<dag>)"));                // OPTION
  m_wordList[command].Add(wxS("vertex_connectivity"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_connectivity(<g>)"));               // OPTION
  m_wordList[command].Add(wxS("vertex_degree"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_degree(<v>, <gr>)"));               // OPTION
  m_wordList[command].Add(wxS("vertex_distance"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_distance(<u>, <v>, <gr>)"));        // OPTION
  m_wordList[command].Add(wxS("vertex_eccentricity"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_eccentricity(<v>, <gr>)"));         // OPTION
  m_wordList[command].Add(wxS("vertex_in_degree"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_in_degree(<v>, <gr>)"));            // OPTION
  m_wordList[command].Add(wxS("vertex_out_degree"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_out_degree(<v>, <gr>)"));           // OPTION
  m_wordList[command].Add(wxS("vertices"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("vertices(<gr>)"));                         // OPTION
  m_wordList[command].Add(wxS("vertex_coloring"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("vertex_coloring(<gr>)"));                  // OPTION
  m_wordList[command].Add(wxS("wiener_index"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("wiener_index(<gr>)"));                     // OPTION
  m_wordList[command].Add(wxS("add_edge"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("add_edge(<e>, <gr>)"));                    // OPTION
  m_wordList[command].Add(wxS("add_edges"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("add_edges(<e_list>, <gr>)"));              // OPTION
  m_wordList[command].Add(wxS("add_vertex"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("add_vertex(<v>, <gr>)"));                  // OPTION
  m_wordList[command].Add(wxS("add_vertices"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("add_vertices(<v_list>, <gr>)"));           // OPTION
  m_wordList[command].Add(wxS("connect_vertices"));                      // FUNCTION
  m_wordList[tmplte].Add(
			 "connect_vertices(<v_list>, <u_list>, <gr>)");   // OPTION
  m_wordList[command].Add(wxS("contract_edge"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("contract_edge(<e>, <gr>)"));  // OPTION
  m_wordList[command].Add(wxS("remove_edge"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("remove_edge(<e>, <gr>)"));    // OPTION
  m_wordList[command].Add(wxS("remove_vertex"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("remove_vertex(<v>, <gr>)"));  // OPTION
  m_wordList[command].Add(wxS("dimacs_export"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("dimacs_export(<gr>, <fl>)")); // OPTION
  m_wordList[tmplte].Add(
			 "dimacs_export(<gr>, <fl>, <comment1>, ..., <commentn>)"); // OPTION
  m_wordList[command].Add(wxS("dimacs_import"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("dimacs_import(<fl>)"));                 // OPTION
  m_wordList[command].Add(wxS("graph6_decode"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("graph6_decode(<str>)"));                // OPTION
  m_wordList[command].Add(wxS("graph6_encode"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("graph6_encode(<gr>)"));                 // OPTION
  m_wordList[command].Add(wxS("graph6_export"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("graph6_export(<gr_list>, <fl>)"));      // OPTION
  m_wordList[command].Add(wxS("graph6_import"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("graph6_import(<fl>)"));                 // OPTION
  m_wordList[command].Add(wxS("sparse6_decode"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("sparse6_decode(<str>)"));               // OPTION
  m_wordList[command].Add(wxS("sparse6_encode"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("sparse6_encode(<gr>)"));                // OPTION
  m_wordList[command].Add(wxS("sparse6_export"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("sparse6_export(<gr_list>, <fl>)"));     // OPTION
  m_wordList[command].Add(wxS("sparse6_import"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("sparse6_import(<fl>)"));                // OPTION
  m_wordList[command].Add(wxS("draw_graph"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("draw_graph(<graph>)"));                 // OPTION
  m_wordList[tmplte].Add(
			 "draw_graph(<graph>, <option1>, ..., <optionk>)");           // OPTION
  m_wordList[command].Add(wxS("draw_graph_program"));                   // OPTION
  m_wordList[command].Add(wxS("show_id"));                              // OPTION
  m_wordList[command].Add(wxS("show_label"));                           // OPTION
  m_wordList[command].Add(wxS("label_alignment"));                      // OPTION
  m_wordList[command].Add(wxS("show_weight"));                          // OPTION
  m_wordList[command].Add(wxS("vertex_type"));                          // OPTION
  m_wordList[command].Add(wxS("vertex_size"));                          // OPTION
  m_wordList[command].Add(wxS("vertex_color"));                         // OPTION
  m_wordList[command].Add(wxS("show_vertices"));                        // OPTION
  m_wordList[command].Add(wxS("show_vertex_type"));                     // OPTION
  m_wordList[command].Add(wxS("show_vertex_size"));                     // OPTION
  m_wordList[command].Add(wxS("show_vertex_color"));                    // OPTION
  m_wordList[command].Add(wxS("vertex_partition"));                     // OPTION
  m_wordList[command].Add(wxS("vertex_coloring"));                      // OPTION
  m_wordList[command].Add(wxS("edge_color"));                           // OPTION
  m_wordList[command].Add(wxS("edge_width"));                           // OPTION
  m_wordList[command].Add(wxS("edge_type"));                            // OPTION
  m_wordList[command].Add(wxS("show_edges"));                           // OPTION
  m_wordList[command].Add(wxS("show_edge_color"));                      // OPTION
  m_wordList[command].Add(wxS("show_edge_width"));                      // OPTION
  m_wordList[command].Add(wxS("show_edge_type"));                       // OPTION
  m_wordList[command].Add(wxS("edge_partition"));                       // OPTION
  m_wordList[command].Add(wxS("edge_coloring"));                        // OPTION
  m_wordList[command].Add(wxS("redraw"));                               // OPTION
  m_wordList[command].Add(wxS("head_angle"));                           // OPTION
  m_wordList[command].Add(wxS("head_length"));                          // OPTION
  m_wordList[command].Add(wxS("spring_embedding_depth"));               // OPTION
  m_wordList[command].Add(wxS("terminal"));                             // OPTION
  m_wordList[command].Add(wxS("file_name"));                            // OPTION
  m_wordList[command].Add(wxS("program"));                              // OPTION
  m_wordList[command].Add(wxS("fixed_vertices"));                       // OPTION
  m_wordList[command].Add(wxS("vertices_to_path"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("vertices_to_path(<v_list>)"));            // OPTION
  m_wordList[command].Add(wxS("vertices_to_cycle"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("vertices_to_cycle(<v_list>)"));           // OPTION
  m_wordList[command].Add(wxS("poly_monomial_order"));                  // OPTION
  m_wordList[command].Add(wxS("poly_coefficient_ring"));                // OPTION
  m_wordList[command].Add(wxS("poly_primary_elimination_order"));       // OPTION
  m_wordList[command].Add(wxS("poly_secondary_elimination_order"));     // OPTION
  m_wordList[command].Add(wxS("poly_elimination_order"));               // OPTION
  m_wordList[command].Add(wxS("poly_return_term_list"));                // OPTION
  m_wordList[command].Add(wxS("poly_grobner_debug"));                   // OPTION
  m_wordList[command].Add(wxS("poly_grobner_algorithm"));               // OPTION
  m_wordList[command].Add(wxS("poly_top_reduction_only"));              // OPTION
  m_wordList[command].Add(wxS("poly_add"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_add(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_subtract"));                        // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_subtract(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_multiply"));          // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_multiply(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_s_polynomial"));      // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_s_polynomial(<poly1>, <poly2>, <varlist>)");             // OPTION
  m_wordList[command].Add(wxS("poly_primitive_part"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_primitive_part(<poly1>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_normalize"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_normalize(<poly>, <varlist>)"));       // OPTION
  m_wordList[command].Add(wxS("poly_expand"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_expand(<poly>, <varlist>)"));          // OPTION
  m_wordList[command].Add(wxS("poly_expt"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_expt(<poly>, <number>, <varlist>)"));  // OPTION
  m_wordList[command].Add(wxS("poly_content"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_content(<poly>. <varlist>)"));         // OPTION
  m_wordList[command].Add(wxS("poly_pseudo_divide"));                     // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_pseudo_divide(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_exact_divide"));             // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_exact_divide(<poly1>, <poly2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_normal_form"));           // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_normal_form(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_buchberger_criterion"));   // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_buchberger_criterion(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_buchberger"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_buchberger(<polylist_fl>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_reduction"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_reduction(<polylist>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_minimization"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_minimization(<polylist>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_normalize_list")); // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_normalize_list(<polylist>, <varlist>)");             // OPTION
  m_wordList[command].Add(wxS("poly_grobner"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_grobner(<polylist>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_reduced_grobner"));               // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_reduced_grobner(<polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_depends_p"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_depends_p(<poly>, <var>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_elimination_ideal")); // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_elimination_ideal(<polylist>, <number>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_colon_ideal"));                    // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_colon_ideal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_ideal_intersection"));           // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_ideal_intersection(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_lcm"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_lcm(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_gcd"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_gcd(<poly1>, <poly2>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_grobner_equal"));                   // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_grobner_equal(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_grobner_subsetp"));                // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_grobner_subsetp(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_grobner_member"));                   // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_grobner_member(<poly>, <polylist>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_ideal_saturation1"));         // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_ideal_saturation1(<polylist>, <poly>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_ideal_saturation"));             // FUNCTION
  m_wordList[tmplte].Add(
			 "poly_ideal_saturation(<polylist1>, <polylist2>, <varlist>)"); // OPTION
  m_wordList[command].Add(wxS("poly_ideal_polysaturation1"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_ideal_polysaturation1(<polylist1>, <polylist2>, "
			     "<varlist>)"));                 // OPTION
  m_wordList[command].Add(wxS("poly_ideal_polysaturation")); // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_ideal_polysaturation(<polylist>, "
			     "<polylistlist>, <varlist>)")); // OPTION
  m_wordList[command].Add(wxS("poly_saturation_extension")); // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_saturation_extension(<poly>, <polylist>, "
			     "<varlist1>, <varlist2>)"));        // OPTION
  m_wordList[command].Add(wxS("poly_polysaturation_extension")); // FUNCTION
  m_wordList[tmplte].Add(wxS("poly_polysaturation_extension(<poly>, <polylist>, "
			     "<varlist1>, <varlist2>)"));               // OPTION
  m_wordList[command].Add(wxS("todd_coxeter"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("todd_coxeter(<relations>, <subgroup>)")); // OPTION
  m_wordList[tmplte].Add(wxS("todd_coxeter(<relations>)"));             // OPTION
  m_wordList[command].Add(wxS("apropos"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("apropos(<string>)"));                     // OPTION
  m_wordList[command].Add(wxS("demo"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("demo(<filename>)"));                      // OPTION
  m_wordList[command].Add(wxS("describe"));                             // FUNCTION
  m_wordList[command].Add(wxS("output_format_for_help"));                // OPTION
  m_wordList[command].Add(wxS("browser"));                               // OPTION
  m_wordList[command].Add(wxS("browser_options"));                       // OPTION
  m_wordList[command].Add(wxS("url_base"));                              // OPTION
  m_wordList[tmplte].Add(wxS("describe(<string>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("describe(<string>, exact)"));             // OPTION
  m_wordList[tmplte].Add(wxS("describe(<string>, inexact)"));           // OPTION
  m_wordList[command].Add(wxS("example"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("example(<topic>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("example()"));                             // OPTION
  m_wordList[command].Add(wxS("manual_demo"));                          // OPTION
  m_wordList[command].Add(wxS("implicit_derivative"));                  // FUNCTION
  m_wordList[tmplte].Add(
			 "implicit_derivative(<f>,<indvarlist>,<orderlist>,<depvar>)"); // OPTION
  m_wordList[command].Add(wxS("implicit_plot"));                          // FUNCTION
  m_wordList[tmplte].Add(
			 "implicit_plot(<expr>, <x_range>, <y_range>)"); // OPTION
  m_wordList[tmplte].Add(wxS("implicit_plot([<expr_1>, ..., <expr_n>], <x_range>, "
			     "<y_range>)"));                           // OPTION
  m_wordList[command].Add(wxS("__"));                                  // OPTION
  m_wordList[command].Add(wxS("_"));                                   // OPTION
  m_wordList[command].Add(wxS("%"));                                   // OPTION
  m_wordList[command].Add(wxS("%%"));                                  // OPTION
  m_wordList[command].Add(wxS("%edispflag"));                          // OPTION
  m_wordList[command].Add(wxS("%th"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("%th(<i>)"));                             // OPTION
  m_wordList[command].Add(wxS("absboxchar"));                          // OPTION
  m_wordList[command].Add(wxS("file_output_append"));                  // OPTION
  m_wordList[command].Add(wxS("appendfile"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("appendfile(<filename>)"));               // OPTION
  m_wordList[command].Add(wxS("batch"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("batch(<filename>)"));                    // OPTION
  m_wordList[command].Add(wxS("batchload"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("batchload(<filename>)"));                // OPTION
  m_wordList[command].Add(wxS("closefile"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("closefile()"));                          // OPTION
  m_wordList[command].Add(wxS("close"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("close(<stream>)"));                      // OPTION
  m_wordList[command].Add(wxS("collapse"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("collapse(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("concat"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("concat(<arg_1>, <arg_2>, ...)"));        // OPTION
  m_wordList[command].Add(wxS("sconcat"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("sconcat(<arg_1>, <arg_2>, ...)"));       // OPTION
  m_wordList[command].Add(wxS("disp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("disp(<expr_1>, <expr_2>, ...)"));        // OPTION
  m_wordList[command].Add(wxS("dispcon"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("dispcon(<tensor_1>, <tensor_2>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("dispcon(all)"));                         // OPTION
  m_wordList[command].Add(wxS("display"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("display(<expr_1>, <expr_2>, ...)"));     // OPTION
  m_wordList[command].Add(wxS("display2d"));                           // OPTION
  m_wordList[command].Add(wxS("display_format_internal"));             // OPTION
  m_wordList[command].Add(wxS("dispterms"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("dispterms(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("error_size"));                          // OPTION
  m_wordList[command].Add(wxS("error_syms"));                          // OPTION
  m_wordList[command].Add(wxS("expt"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("expt(<a>, <b>)"));                       // OPTION
  m_wordList[command].Add(wxS("exptdispflag"));                        // OPTION
  m_wordList[command].Add(wxS("filename_merge"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("filename_merge(<path>, <filename>)"));   // OPTION
  m_wordList[command].Add(wxS("  file_output_append"));                // OPTION
  m_wordList[command].Add(wxS("file_search"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("file_search(<filename>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("file_search(<filename>, <pathlist>)"));  // OPTION
  m_wordList[command].Add(wxS("file_search_demo"));                    // OPTION
  m_wordList[command].Add(wxS("file_search_lisp"));                    // OPTION
  m_wordList[command].Add(wxS("file_search_maxima"));                  // OPTION
  m_wordList[command].Add(wxS("file_search_usage"));                   // OPTION
  m_wordList[command].Add(wxS("file_search_tests"));                   // OPTION
  m_wordList[command].Add(wxS("file_type"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("file_type(<filename>)"));                // OPTION
  m_wordList[command].Add(wxS("file_type_maxima"));                    // OPTION
  m_wordList[command].Add(wxS("file_type_lisp"));                      // OPTION
  m_wordList[command].Add(wxS("grind"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("grind(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("ibase"));                               // OPTION
  m_wordList[command].Add(wxS("inchar"));                              // OPTION
  m_wordList[command].Add(wxS("ldisp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("ldisp(<expr_1>, ..., <expr_n>)"));       // OPTION
  m_wordList[command].Add(wxS("ldisplay"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ldisplay(<expr_1>, ..., <expr_n>)"));    // OPTION
  m_wordList[command].Add(wxS("linechar"));                            // OPTION
  m_wordList[command].Add(wxS("linel"));                               // OPTION
  m_wordList[command].Add(wxS("lispdisp"));                            // OPTION
  m_wordList[command].Add(wxS("load"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("load(<filename>)"));                     // OPTION
  m_wordList[command].Add(wxS("loadfile"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("loadfile(<filename>)"));                 // OPTION
  m_wordList[command].Add(wxS("loadprint"));                           // OPTION
  m_wordList[command].Add(wxS("obase"));                               // OPTION
  m_wordList[command].Add(wxS("outchar"));                             // OPTION
  m_wordList[command].Add(wxS("packagefile"));                         // OPTION
  m_wordList[command].Add(wxS("pfeformat"));                           // OPTION
  m_wordList[command].Add(wxS("print"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("print(<expr_1>, ..., <expr_n>)"));       // OPTION
  m_wordList[command].Add(wxS("printfile"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("printfile(<path>)"));                    // OPTION
  m_wordList[command].Add(wxS("tcl_output"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("tcl_output(<list>, <i0>, <skip>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("tcl_output(<list>, <i0>)"));             // OPTION
  m_wordList[tmplte].Add(
			 "tcl_output([<list_1>, ..., <list_n>], <i>)");           // OPTION
  m_wordList[command].Add(wxS("read"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("read(<expr_1>, ..., <expr_n>)"));     // OPTION
  m_wordList[command].Add(wxS("readonly"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("readonly(<expr_1>, ..., <expr_n>)")); // OPTION
  m_wordList[command].Add(wxS("reveal"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("reveal(<expr>, <depth>)"));           // OPTION
  m_wordList[command].Add(wxS("rmxchar"));                          // OPTION
  m_wordList[command].Add(wxS("save"));                             // FUNCTION
  m_wordList[tmplte].Add(
			 "save(<filename>, <name_1>, <name_2>, <name_3>, ...)"); // OPTION
  m_wordList[tmplte].Add(
			 "save(<filename>, values, functions, labels, ...)");            // OPTION
  m_wordList[tmplte].Add(wxS("save(<filename>, [<m>, <n>])"));             // OPTION
  m_wordList[tmplte].Add(wxS("save(<filename>, <name_1>=<expr_1>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("save(<filename>, all)"));                    // OPTION
  m_wordList[tmplte].Add(
			 "save(<filename>, <name_1>=<expr_1>, <name_2>=<expr_2>, ...)"); // OPTION
  m_wordList[command].Add(wxS("savedef"));                                 // OPTION
  m_wordList[command].Add(wxS("show"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("show(<expr>)"));        // OPTION
  m_wordList[command].Add(wxS("showratvars"));        // FUNCTION
  m_wordList[tmplte].Add(wxS("showratvars(<expr>)")); // OPTION
  m_wordList[command].Add(wxS("stardisp"));           // OPTION
  m_wordList[command].Add(wxS("string"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("string(<expr>)"));      // OPTION
  m_wordList[command].Add(wxS("stringdisp"));         // OPTION
  m_wordList[command].Add(wxS("stringout"));          // FUNCTION
  m_wordList[tmplte].Add(
			 "stringout(<filename>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList[tmplte].Add(wxS("stringout(<filename>, [<m>, <n>])"));     // OPTION
  m_wordList[tmplte].Add(wxS("stringout(<filename>, input)"));          // OPTION
  m_wordList[tmplte].Add(wxS("stringout(<filename>, functions)"));      // OPTION
  m_wordList[tmplte].Add(wxS("stringout(<filename>, values)"));         // OPTION
  m_wordList[command].Add(wxS("tex"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("tex(<expr>)"));                           // OPTION
  m_wordList[tmplte].Add(wxS("tex(<expr>, <destination>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("tex(<expr>, false)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("tex(<label>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("tex(<label>, <destination>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("tex(<label>, false)"));                   // OPTION
  m_wordList[command].Add(wxS("tex1"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("tex1(<e>)"));                             // OPTION
  m_wordList[command].Add(wxS("texput"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("texput(<a>, <s>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("texput(<a>, <f>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("texput(<a>, <s>, <operator_type>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("texput(<a>, [<s_1>, <s_2>], matchfix)")); // OPTION
  m_wordList[tmplte].Add(
			 "texput(<a>, [<s_1>, <s_2>, <s_3>], matchfix)"); // OPTION
  m_wordList[command].Add(wxS("get_tex_environment"));      // FUNCTION
  m_wordList[tmplte].Add(wxS("get_tex_environment(<op>)")); // OPTION
  m_wordList[tmplte].Add(
			 "set_tex_environment(<op>, <before>, <after>)");     // OPTION
  m_wordList[command].Add(wxS("get_tex_environment_default"));  // FUNCTION
  m_wordList[tmplte].Add(wxS("get_tex_environment_default()")); // OPTION
  m_wordList[tmplte].Add(
			 "set_tex_environment_default(<before>, <after>)"); // OPTION
  m_wordList[command].Add(wxS("system"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("system(<command>)"));           // OPTION
  m_wordList[command].Add(wxS("ttyoff"));                     // OPTION
  m_wordList[command].Add(wxS("with_stdout"));                // FUNCTION
  m_wordList[tmplte].Add(
			 "with_stdout(<f>, <expr_1>, <expr_2>, <expr_3>, ...)"); // OPTION
  m_wordList[tmplte].Add(
			 "with_stdout(<s>, <expr_1>, <expr_2>, <expr_3>, ...)");      // OPTION
  m_wordList[command].Add(wxS("writefile"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("writefile(<filename>)"));                 // OPTION
  m_wordList[command].Add(wxS("changevar"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("changevar(<expr>, <f(x,y)>, <y>, <x>)")); // OPTION
  m_wordList[command].Add(wxS("dblint"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("dblint(<f>, <r>, <s>, <a>, <b>)"));       // OPTION
  m_wordList[command].Add(wxS("defint"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("defint(<expr>, <x>, <a>, <b>)"));         // OPTION
  m_wordList[command].Add(wxS("erfflag"));                              // OPTION
  m_wordList[command].Add(wxS("ilt"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ilt(<expr>, <s>, <t>)"));                 // OPTION
  m_wordList[command].Add(wxS("intanalysis"));                          // OPTION
  m_wordList[command].Add(wxS("integrate"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("integrate(<expr>, <x>)"));                // OPTION
  m_wordList[tmplte].Add(wxS("integrate(<expr>, <x>, <a>, <b>)"));      // OPTION
  m_wordList[command].Add(wxS("integration_constant"));                 // OPTION
  m_wordList[command].Add(wxS("integration_constant_counter"));         // OPTION
  m_wordList[command].Add(wxS("integrate_use_rootsof"));                // OPTION
  m_wordList[command].Add(wxS("ldefint"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("ldefint(<expr>, <x>, <a>, <b>)"));        // OPTION
  m_wordList[command].Add(wxS("potential"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("potential(<givengradient>)"));            // OPTION
  m_wordList[command].Add(wxS("residue"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("residue(<expr>, <z>, <z_0>)"));           // OPTION
  m_wordList[command].Add(wxS("risch"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("risch(<expr>, <x>)"));                    // OPTION
  m_wordList[command].Add(wxS("tldefint"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("tldefint(<expr>, <x>, <a>, <b>)"));       // OPTION
  m_wordList[command].Add(wxS("quad_qag"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qag(<f(x)>, <x>, <a>, <b>, <key>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[tmplte].Add(wxS("quad_qag(<f>, <x>, <a>, <b>, <key>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[command].Add(wxS("quad_qags"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qags(<f(x)>, <x>, <a>, <b>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[tmplte].Add(
			 "quad_qags(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add(wxS("quad_qagi")); // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qagi(<f(x)>, <x>, <a>, <b>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[tmplte].Add(
			 "quad_qagi(<f>, <x>, <a>, <b>, [<epsrel>, <epsabs>, <limit>])"); // OPTION
  m_wordList[command].Add(wxS("quad_qawc")); // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qawc(<f(x)>, <x>, <c>, <a>, <b>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[tmplte].Add(wxS("quad_qawc(<f>, <x>, <c>, <a>, <b>, [<epsrel>, "
			     "<epsabs>, <limit>])")); // OPTION
  m_wordList[command].Add(wxS("quad_qawf"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qawf(<f(x)>, <x>, <a>, <omega>, <trig>, "
			     "[<epsabs>, <limit>, <maxp1>, <limlst>])")); // OPTION
  m_wordList[tmplte].Add(wxS("quad_qawf(<f>, <x>, <a>, <omega>, <trig>, [<epsabs>, "
			     "<limit>, <maxp1>, <limlst>])")); // OPTION
  m_wordList[command].Add(wxS("quad_qawo"));                   // FUNCTION
  m_wordList[tmplte].Add(
			 "quad_qawo(<f(x)>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, "
			 "<limit>, <maxp1>, <limlst>])"); // OPTION
  m_wordList[tmplte].Add(
			 "quad_qawo(<f>, <x>, <a>, <b>, <omega>, <trig>, [<epsrel>, <epsabs>, "
			 "<limit>, <maxp1>, <limlst>])");  // OPTION
  m_wordList[command].Add(wxS("quad_qaws")); // FUNCTION
  m_wordList[tmplte].Add(wxS("quad_qaws(<f(x)>, <x>, <a>, <b>, <alpha>, <beta>, "
			     "<wfun>, [<epsrel>, <epsabs>, <limit>])")); // OPTION
  m_wordList[tmplte].Add(wxS("quad_qaws(<f>, <x>, <a>, <b>, <alpha>, <beta>, "
			     "<wfun>, [<epsrel>, <epsabs>, <limit>])")); // OPTION
  m_wordList[command].Add(wxS("lagrange"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("lagrange(<points>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("lagrange(<points>, <option>)"));           // OPTION
  m_wordList[command].Add(wxS("charfun2"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("charfun2(<x>, <a>, <b>)"));                // OPTION
  m_wordList[command].Add(wxS("linearinterpol"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("linearinterpol(<points>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("linearinterpol(<points>, <option>)"));     // OPTION
  m_wordList[command].Add(wxS("cspline"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cspline(<points>)"));                      // OPTION
  m_wordList[tmplte].Add(
			 "cspline(<points>, <option1>, <option2>, ...)");       // OPTION
  m_wordList[command].Add(wxS("ratinterpol"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("ratinterpol(<points>, <numdeg>)")); // OPTION
  m_wordList[tmplte].Add(
			 "ratinterpol(<points>, <numdeg>, <option1>, <option2>, ...)"); // OPTION
  m_wordList[command].Add(wxS("entertensor"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("entertensor(<name>)"));                     // OPTION
  m_wordList[command].Add(wxS("changename"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("changename(<old>, <new>, <expr>)"));        // OPTION
  m_wordList[command].Add(wxS("ishow"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ishow(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("indices"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("indices(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("rename"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("rename(<expr>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("rename(<expr>, <count>)"));                 // OPTION
  m_wordList[command].Add(wxS("flipflag"));                               // OPTION
  m_wordList[command].Add(wxS("defcon"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("defcon(<tensor_1>)"));                      // OPTION
  m_wordList[tmplte].Add(
			 "defcon(<tensor_1>, <tensor_2>, <tensor_3>)");             // OPTION
  m_wordList[command].Add(wxS("remcon"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("remcon(<tensor_1>, ..., <tensor_n>)")); // OPTION
  m_wordList[tmplte].Add(wxS("remcon(all)"));                         // OPTION
  m_wordList[command].Add(wxS("contract"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("contract(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("indexed_tensor"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("indexed_tensor(<tensor>)"));            // OPTION
  m_wordList[command].Add(wxS("components"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("components(<tensor>, <expr>)"));        // OPTION
  m_wordList[command].Add(wxS("remcomps"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("remcomps(<tensor>)"));                  // OPTION
  m_wordList[command].Add(wxS("showcomps"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("showcomps(<tensor>)"));                 // OPTION
  m_wordList[command].Add(wxS("idummy"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("idummy()"));                            // OPTION
  m_wordList[command].Add(wxS("idummyx"));                            // OPTION
  m_wordList[command].Add(wxS("icounter"));                           // OPTION
  m_wordList[command].Add(wxS("kdelta"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("kdelta(<L1>, <L2>)"));                  // OPTION
  m_wordList[command].Add(wxS("kdels"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("kdels(<L1>, <L2>)"));                   // OPTION
  m_wordList[command].Add(wxS("levi_civita"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("levi_civita(<L>)"));                    // OPTION
  m_wordList[command].Add(wxS("lc2kdt"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("lc2kdt(<expr>)"));                      // OPTION
  m_wordList[command].Add(wxS("canten"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("canten(<expr>)"));                      // OPTION
  m_wordList[command].Add(wxS("concan"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("concan(<expr>)"));                      // OPTION
  m_wordList[command].Add(wxS("allsym"));                             // OPTION
  m_wordList[command].Add(wxS("decsym"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("decsym(<tensor>, <m>, <n>, [<cov_1>, <cov_2>, ...], "
			     "[<contr_1>, <contr_2>, ...])")); // OPTION
  m_wordList[command].Add(wxS("remsym"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("remsym(<tensor>, <m>, <n>)"));   // OPTION
  m_wordList[command].Add(wxS("canform"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("canform(<expr>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("canform(<expr>, <rename>)"));    // OPTION
  m_wordList[command].Add(wxS("diff"));                        // FUNCTION
  m_wordList[tmplte].Add(
			 "diff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList[command].Add(wxS("idiff"));                        // FUNCTION
  m_wordList[tmplte].Add(
			 "idiff(<expr>, <v_1>, [<n_1>, [<v_2>, <n_2>] ...])"); // OPTION
  m_wordList[command].Add(wxS("liediff"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("liediff(<v>, <ten>)"));            // OPTION
  m_wordList[command].Add(wxS("rediff"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("rediff(<ten>)"));                  // OPTION
  m_wordList[command].Add(wxS("undiff"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("undiff(<expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("evundiff"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("evundiff(<expr>)"));               // OPTION
  m_wordList[command].Add(wxS("flush"));                         // FUNCTION
  m_wordList[tmplte].Add(
			 "flush(<expr>, <tensor_1>, <tensor_2>, ...)"); // OPTION
  m_wordList[command].Add(wxS("flushd"));                 // FUNCTION
  m_wordList[tmplte].Add(
			 "flushd(<expr>, <tensor_1>, <tensor_2>, ...)");              // OPTION
  m_wordList[command].Add(wxS("flushnd"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("flushnd(<expr>, <tensor>, <n>)"));        // OPTION
  m_wordList[command].Add(wxS("coord"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("coord(<tensor_1>, <tensor_2>, ...)"));    // OPTION
  m_wordList[command].Add(wxS("remcoord"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("remcoord(<tensor_1>, <tensor_2>, ...)")); // OPTION
  m_wordList[tmplte].Add(wxS("remcoord(all)"));                         // OPTION
  m_wordList[command].Add(wxS("makebox"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("makebox(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("conmetderiv"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("conmetderiv(<expr>, <tensor>)"));         // OPTION
  m_wordList[command].Add(wxS("simpmetderiv"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("simpmetderiv(<expr>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("simpmetderiv(<expr>[, <stop>])"));        // OPTION
  m_wordList[command].Add(wxS("flush1deriv"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("flush1deriv(<expr>, <tensor>)"));         // OPTION
  m_wordList[command].Add(wxS("imetric"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("imetric(<g>)"));                          // OPTION
  m_wordList[command].Add(wxS("idim"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("idim(<n>)"));                             // OPTION
  m_wordList[command].Add(wxS("ichr1"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("ichr1([<i>, <j>, <k>])"));                // OPTION
  m_wordList[command].Add(wxS("ichr2"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("ichr2([<i>, <j>], [<k>])"));              // OPTION
  m_wordList[command].Add(wxS("icurvature"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("icurvature([<i>, <j>, <k>], [<h>])"));    // OPTION
  m_wordList[command].Add(wxS("covdiff"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("covdiff(<expr>, <v_1>, <v_2>, ...)"));    // OPTION
  m_wordList[command].Add(wxS("lorentz_gauge"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("lorentz_gauge(<expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("igeodesic_coords"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("igeodesic_coords(<expr>, <name>)"));      // OPTION
  m_wordList[command].Add(wxS("iframes"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("iframes()"));                             // OPTION
  m_wordList[command].Add(wxS("ifb"));                                  // OPTION
  m_wordList[command].Add(wxS("icc1"));                                 // OPTION
  m_wordList[command].Add(wxS("icc2"));                                 // OPTION
  m_wordList[command].Add(wxS("ifc1"));                                 // OPTION
  m_wordList[command].Add(wxS("ifc2"));                                 // OPTION
  m_wordList[command].Add(wxS("ifr"));                                  // OPTION
  m_wordList[command].Add(wxS("ifri"));                                 // OPTION
  m_wordList[command].Add(wxS("ifg"));                                  // OPTION
  m_wordList[command].Add(wxS("ifgi"));                                 // OPTION
  m_wordList[command].Add(wxS("iframe_bracket_form"));                  // OPTION
  m_wordList[command].Add(wxS("inm"));                                  // OPTION
  m_wordList[command].Add(wxS("inmc1"));                                // OPTION
  m_wordList[command].Add(wxS("inmc2"));                                // OPTION
  m_wordList[command].Add(wxS("ikt1"));                                 // OPTION
  m_wordList[command].Add(wxS("ikt2"));                                 // OPTION
  m_wordList[command].Add(wxS("itr"));                                  // OPTION
  m_wordList[command].Add(wxS("extdiff"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("extdiff(<expr>, <i>)"));                  // OPTION
  m_wordList[command].Add(wxS("hodge"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("hodge(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("igeowedge_flag"));                       // OPTION
  m_wordList[command].Add(wxS("tentex"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("tentex(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("ic_convert"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("ic_convert(<eqn>)"));                     // OPTION
  m_wordList[command].Add(wxS("dgeev"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("dgeev(<A>)"));                            // OPTION
  m_wordList[tmplte].Add(wxS("dgeev(<A>, <right_p>, <left_p>)"));       // OPTION
  m_wordList[command].Add(wxS("dgesv"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("dgesv(<A>, <b>)"));                       // OPTION
  m_wordList[command].Add(wxS("dgesvd"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("dgesvd(<A>)"));                           // OPTION
  m_wordList[tmplte].Add(wxS("dgesvd(<A>, <left_p>, <right_p>)"));      // OPTION
  m_wordList[command].Add(wxS("dlange"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("dlange(<norm>, <A>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("zlange(<norm>, <A>)"));                   // OPTION
  m_wordList[command].Add(wxS("lbfgs"));                                // FUNCTION
  m_wordList[tmplte].Add(
			 "lbfgs(<FOM>, <X>, <X0>, <epsilon>, <iprint>)"); // OPTION
  m_wordList[tmplte].Add(
			 "lbfgs([<FOM>, <grad>] <X>, <X0>, <epsilon>, <iprint>)");     // OPTION
  m_wordList[command].Add(wxS("lbfgs_nfeval_max"));                      // OPTION
  m_wordList[command].Add(wxS("lbfgs_ncorrections"));                    // OPTION
  m_wordList[command].Add(wxS("lhospitallim"));                          // OPTION
  m_wordList[command].Add(wxS("limit"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("limit(<expr>, <x>, <val>, <dir>)"));       // OPTION
  m_wordList[tmplte].Add(wxS("limit(<expr>, <x>, <val>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("limit(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("limsubst"));                              // OPTION
  m_wordList[command].Add(wxS("tlimit"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("tlimit(<expr>, <x>, <val>, <dir>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("tlimit(<expr>, <x>, <val>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("tlimit(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("tlimswitch"));                            // OPTION
  m_wordList[command].Add(wxS("Lindstedt"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("Lindstedt(<eq>,<pvar>,<torder>,<ic>)"));   // OPTION
  m_wordList[command].Add(wxS("addmatrices"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("addmatrices(<f>, <M_1>, ..., <M_n>)"));    // OPTION
  m_wordList[command].Add(wxS("blockmatrixp"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("blockmatrixp(<M>)"));                      // OPTION
  m_wordList[command].Add(wxS("columnop"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("columnop(<M>, <i>, <j>, <theta>)"));       // OPTION
  m_wordList[command].Add(wxS("columnswap"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("columnswap(<M>, <i>, <j>)"));              // OPTION
  m_wordList[command].Add(wxS("columnspace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("columnspace(<M>)"));                       // OPTION
  m_wordList[command].Add(wxS("copy"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("copy(<e>)"));                              // OPTION
  m_wordList[command].Add(wxS("cholesky"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("cholesky(<M>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("cholesky(<M>, <field>)"));                 // OPTION
  m_wordList[command].Add(wxS("ctranspose"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ctranspose(<M>)"));                        // OPTION
  m_wordList[command].Add(wxS("diag_matrix"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("diag_matrix(<d_1>, <d_2>,...,<d_n>)"));    // OPTION
  m_wordList[command].Add(wxS("dotproduct"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("dotproduct(<u>, <v>)"));                   // OPTION
  m_wordList[command].Add(wxS("eigens_by_jacobi"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("eigens_by_jacobi(<A>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("eigens_by_jacobi(<A>, <field_type>)"));    // OPTION
  m_wordList[command].Add(wxS("get_lu_factors"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("get_lu_factors(<x>) "));                   // OPTION
  m_wordList[command].Add(wxS("hankel"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("hankel(<col>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("hankel(<col>, <row>)"));                   // OPTION
  m_wordList[command].Add(wxS("hessian"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("hessian(<f>, <x>)"));                      // OPTION
  m_wordList[command].Add(wxS("hilbert_matrix"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("hilbert_matrix(<n>)"));                    // OPTION
  m_wordList[command].Add(wxS("identfor"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("identfor(<M>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("identfor(<M>, <fld>)"));                   // OPTION
  m_wordList[command].Add(wxS("invert_by_lu"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("invert_by_lu(<M>, <(rng generalring)>)")); // OPTION
  m_wordList[command].Add(wxS("jacobian"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobian(<f>, <x>)"));                     // OPTION
  m_wordList[command].Add(wxS("kronecker_product"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("kronecker_product(<A>, <B>)"));            // OPTION
  m_wordList[command].Add(wxS("listp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("listp(<e>, <p>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("listp(<e>)"));                             // OPTION
  m_wordList[command].Add(wxS("locate_matrix_entry"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("locate_matrix_entry(<M>, <r_1>, <c_1>, <r_2>, <c_2>, "
			     "<f>, <rel>)"));              // OPTION
  m_wordList[command].Add(wxS("lu_backsub"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("lu_backsub(<M>, <b>)"));     // OPTION
  m_wordList[command].Add(wxS("lu_factor"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("lu_factor(<M>, <field>)"));  // OPTION
  m_wordList[command].Add(wxS("mat_cond"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_cond(<M>, 1)"));         // OPTION
  m_wordList[tmplte].Add(wxS("mat_cond(<M>, inf)"));       // OPTION
  m_wordList[command].Add(wxS("mat_norm"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_norm(<M>, 1)"));         // OPTION
  m_wordList[tmplte].Add(wxS("mat_norm(<M>, inf)"));       // OPTION
  m_wordList[tmplte].Add(wxS("mat_norm(<M>, frobenius)")); // OPTION
  m_wordList[command].Add(wxS("matrixp"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("matrixp(<e>, <p>)"));        // OPTION
  m_wordList[tmplte].Add(wxS("matrixp(<e>)"));             // OPTION
  m_wordList[command].Add(wxS("matrixexp"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("matrixexp(<M>, <v>)"));      // OPTION
  m_wordList[command].Add(wxS("matrix_size"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("matrix_size(<M>)"));         // OPTION
  m_wordList[command].Add(wxS("mat_fullunblocker"));       // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_fullunblocker(<M>)"));   // OPTION
  m_wordList[command].Add(wxS("mat_trace"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_trace(<M>)"));           // OPTION
  m_wordList[command].Add(wxS("mat_unblocker"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("mat_unblocker(<M>)"));       // OPTION
  m_wordList[command].Add(wxS("nonnegintegerp"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("nonnegintegerp(<n>)"));      // OPTION
  m_wordList[command].Add(wxS("nullspace"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("nullspace(<M>)"));           // OPTION
  m_wordList[command].Add(wxS("nullity"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("nullity(<M>)"));             // OPTION
  m_wordList[command].Add(wxS("orthogonal_complement"));   // FUNCTION
  m_wordList[tmplte].Add(wxS("orthogonal_complement(<v_1>, ..., <v_n>)")); // OPTION
  m_wordList[command].Add(wxS("polynomialp")); // FUNCTION
  m_wordList[tmplte].Add(wxS("polynomialp(<p>, <L>, <coeffp>, <exponp>)")); // OPTION
  m_wordList[tmplte].Add(wxS("polynomialp(<p>, <L>, <coeffp>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("polynomialp(<p>, <L>)"));                     // OPTION
  m_wordList[command].Add(wxS("polytocompanion"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("polytocompanion(<p>, <x>)"));               // OPTION
  m_wordList[command].Add(wxS("ptriangularize"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("ptriangularize(<M>, <v>)"));                // OPTION
  m_wordList[command].Add(wxS("rowop"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("rowop(<M>, <i>, <j>, <theta>)"));           // OPTION
  m_wordList[command].Add(wxS("rank"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("rank(<M>)"));                               // OPTION
  m_wordList[command].Add(wxS("rowswap"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("rowswap(<M>, <i>, <j>)"));                  // OPTION
  m_wordList[command].Add(wxS("toeplitz"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("toeplitz(<col>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("toeplitz(<col>, <row>)"));                  // OPTION
  m_wordList[command].Add(wxS("vandermonde_matrix"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("vandermonde_matrix([<x_1>, ..., <x_n>])")); // OPTION
  m_wordList[command].Add(wxS("zerofor"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("zerofor(<M>)"));                            // OPTION
  m_wordList[tmplte].Add(wxS("zerofor(<M>, <fld>)"));                     // OPTION
  m_wordList[command].Add(wxS("zeromatrixp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("zeromatrixp(<M>)"));                        // OPTION
  m_wordList[command].Add(wxS("append"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("append(<list_1>, ..., <list_n>)"));         // OPTION
  m_wordList[command].Add(wxS("assoc"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("assoc(<key>, <list>, <default>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("assoc(<key>, <list>)"));                    // OPTION
  m_wordList[command].Add(wxS("atom"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("atom(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("cons"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("cons(<expr>, <list>)"));                    // OPTION
  m_wordList[command].Add(wxS("copylist"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("copylist(<list>)"));                        // OPTION
  m_wordList[command].Add(wxS("create_list"));                            // FUNCTION
  m_wordList[tmplte].Add(
			 "create_list(<form>, <x_1>, <list_1>, ..., <x_n>, <list_n>)"); // OPTION
  m_wordList[command].Add(wxS("delete"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("delete(<expr_1>, <expr_2>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("delete(<expr_1>, <expr_2>, <n>)"));         // OPTION
  m_wordList[command].Add(wxS("eighth"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("eighth(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("endcons"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("endcons(<expr>, <list>)"));                 // OPTION
  m_wordList[command].Add(wxS("fifth"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("fifth(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("first"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("first(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("firstn"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("firstn(<expr>, <number>)"));                // OPTION
  m_wordList[command].Add(wxS("fourth"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("fourth(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("garbage_collect"));                        // FUNCTION
  m_wordList[command].Add(wxS("get"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("get(<a>, <i>)"));                           // OPTION
  m_wordList[command].Add(wxS("join"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("join(<l>, <m>)"));                          // OPTION
  m_wordList[command].Add(wxS("last"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("last(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("lastn"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("lastn(<expr>, <number>)"));                 // OPTION
  m_wordList[command].Add(wxS("length"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("length(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("listarith"));                              // OPTION
  m_wordList[command].Add(wxS("listp"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("listp(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("makelist"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("makelist(<expr>, <i>, <i_0>, <i_1>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("makelist(<expr>, <x>, <list>)"));           // OPTION
  m_wordList[command].Add(wxS("member"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("member(<expr_1>, <expr_2>)"));              // OPTION
  m_wordList[command].Add(wxS("ninth"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ninth(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("pop"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("pop(<list>)"));                             // OPTION
  m_wordList[command].Add(wxS("push"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("push(<item>, <list>)"));                    // OPTION
  m_wordList[command].Add(wxS("unique"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("unique(<L>)"));                             // OPTION
  m_wordList[command].Add(wxS("rest"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("rest(<expr>, <n>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("rest(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("reverse"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("reverse(<list>)"));                         // OPTION
  m_wordList[command].Add(wxS("second"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("second(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("seventh"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("seventh(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("sixth"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("sixth(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("sublist_indices"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("sublist_indices(<L>, <P>)"));               // OPTION
  m_wordList[command].Add(wxS("tenth"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("tenth(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("third"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("third(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("%e_to_numlog"));                           // OPTION
  m_wordList[command].Add(wxS("li"));                                     // FUNCTION
  m_wordList[tmplte].Add(wxS("li[<s>] (<z>)"));                           // OPTION
  m_wordList[command].Add(wxS("log"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("log(<x>)"));                                // OPTION
  m_wordList[command].Add(wxS("logabs"));                                 // OPTION
  m_wordList[command].Add(wxS("logarc"));                                 // OPTION
  m_wordList[command].Add(wxS("logconcoeffp"));                           // OPTION
  m_wordList[command].Add(wxS("logcontract"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("logcontract(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("logexpand"));                              // OPTION
  m_wordList[command].Add(wxS("lognegint"));                              // OPTION
  m_wordList[command].Add(wxS("lognumer"));                               // OPTION
  m_wordList[command].Add(wxS("logsimp"));                                // OPTION
  m_wordList[command].Add(wxS("plog"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("plog(<x>)"));                               // OPTION
  m_wordList[command].Add(wxS("lsquares_estimates"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_estimates(<D>, <x>, <e>, <a>)"));  // OPTION
  m_wordList[tmplte].Add(wxS("lsquares_estimates(<D>, <x>, <e>, <a>, initial = "
			     "<L>, tol = <t>)"));                      // OPTION
  m_wordList[command].Add(wxS("lsquares_estimates_exact"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_estimates_exact(<MSE>, <a>)")); // OPTION
  m_wordList[command].Add(wxS("lsquares_estimates_approximate"));      // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_estimates_approximate(<MSE>, <a>, initial = "
			     "<L>, tol = <t>)"));                        // OPTION
  m_wordList[command].Add(wxS("lsquares_mse"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_mse(<D>, <x>, <e>)"));            // OPTION
  m_wordList[command].Add(wxS("lsquares_residuals"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_residuals(<D>, <x>, <e>, <a>)")); // OPTION
  m_wordList[command].Add(wxS("lsquares_residual_mse"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("lsquares_residual_mse(<D>, <x>, <e>, <a>)")); // OPTION
  m_wordList[command].Add(wxS("plsquares"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("plsquares(<Mat>,<VarList>,<depvars>)")); // OPTION
  m_wordList[tmplte].Add(
			 "plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>)"); // OPTION
  m_wordList[tmplte].Add(
			 "plsquares(<Mat>,<VarList>,<depvars>,<maxexpon>,<maxdegree>)"); // OPTION
  m_wordList[command].Add(wxS("makeOrders"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("makeOrders(<indvarlist>,<orderlist>)")); // OPTION
  m_wordList[command].Add(wxS("addcol"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("addcol(<M>, <list_1>, ..., <list_n>)")); // OPTION
  m_wordList[command].Add(wxS("addrow"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("addrow(<M>, <list_1>, ..., <list_n>)")); // OPTION
  m_wordList[command].Add(wxS("adjoint"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("adjoint(<M>)"));                         // OPTION
  m_wordList[command].Add(wxS("augcoefmatrix"));                       // FUNCTION
  m_wordList[tmplte].Add(
			 "augcoefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add(wxS("charpoly"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("charpoly(<M>, <x>)")); // OPTION
  m_wordList[command].Add(wxS("coefmatrix"));        // FUNCTION
  m_wordList[tmplte].Add(
			 "coefmatrix([<eqn_1>, ..., <eqn_m>], [<x_1>, ..., <x_n>])"); // OPTION
  m_wordList[command].Add(wxS("col"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("col(<M>, <i>)"));                         // OPTION
  m_wordList[command].Add(wxS("columnvector"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("columnvector(<L>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("covect(<L>)"));                           // OPTION
  m_wordList[command].Add(wxS("conjugate"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("conjugate(<x>)"));                        // OPTION
  m_wordList[command].Add(wxS("copymatrix"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("copymatrix(<M>)"));                       // OPTION
  m_wordList[command].Add(wxS("determinant"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("determinant(<M>)"));                      // OPTION
  m_wordList[command].Add(wxS("detout"));                               // OPTION
  m_wordList[command].Add(wxS("diagmatrix"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("diagmatrix(<n>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("doallmxops"));                           // OPTION
  m_wordList[command].Add(wxS("domxexpt"));                             // OPTION
  m_wordList[command].Add(wxS("domxmxops"));                            // OPTION
  m_wordList[command].Add(wxS("domxnctimes"));                          // OPTION
  m_wordList[command].Add(wxS("dontfactor"));                           // OPTION
  m_wordList[command].Add(wxS("doscmxops"));                            // OPTION
  m_wordList[command].Add(wxS("doscmxplus"));                           // OPTION
  m_wordList[command].Add(wxS("dot0nscsimp"));                          // OPTION
  m_wordList[command].Add(wxS("dot0simp"));                             // OPTION
  m_wordList[command].Add(wxS("dot1simp"));                             // OPTION
  m_wordList[command].Add(wxS("dotassoc"));                             // OPTION
  m_wordList[command].Add(wxS("dotconstrules"));                        // OPTION
  m_wordList[command].Add(wxS("dotdistrib"));                           // OPTION
  m_wordList[command].Add(wxS("dotexptsimp"));                          // OPTION
  m_wordList[command].Add(wxS("dotident"));                             // OPTION
  m_wordList[command].Add(wxS("dotscrules"));                           // OPTION
  m_wordList[command].Add(wxS("echelon"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("echelon(<M>)"));                          // OPTION
  m_wordList[command].Add(wxS("eigenvalues"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("eigenvalues(<M>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("eivals(<M>)"));                           // OPTION
  m_wordList[command].Add(wxS("eigenvectors"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("eigenvectors(<M>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("eivects(<M>)"));                          // OPTION
  m_wordList[command].Add(wxS("ematrix"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("ematrix(<m>, <n>, <x>, <i>, <j>)"));      // OPTION
  m_wordList[command].Add(wxS("entermatrix"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("entermatrix(<m>, <n>)"));                 // OPTION
  m_wordList[command].Add(wxS("genmatrix"));                            // FUNCTION
  m_wordList[tmplte].Add(
			 "genmatrix(<a>, <i_2>, <j_2>, <i_1>, <j_1>)");             // OPTION
  m_wordList[tmplte].Add(wxS("genmatrix(<a>, <i_2>, <j_2>, <i_1>)")); // OPTION
  m_wordList[tmplte].Add(wxS("genmatrix(<a>, <i_2>, <j_2>)"));        // OPTION
  m_wordList[command].Add(wxS("gramschmidt"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("gramschmidt(<x>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("gramschmidt(<x>, <F>)"));               // OPTION
  m_wordList[command].Add(wxS("ident"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("ident(<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("innerproduct"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("innerproduct(<x>, <y>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("inprod(<x>, <y>)"));                    // OPTION
  m_wordList[command].Add(wxS("invert"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("invert(<M>)"));                         // OPTION
  m_wordList[command].Add(wxS("lmxchar"));                            // OPTION
  m_wordList[command].Add(wxS("load_pathname"));                      // OPTION
  m_wordList[command].Add(wxS("matrix"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("matrix(<row_1>, ..., <row_n>)"));       // OPTION
  m_wordList[command].Add(wxS("matrixmap"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("matrixmap(<f>, <M>)"));                 // OPTION
  m_wordList[command].Add(wxS("matrixp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("matrixp(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("matrix_element_add"));                 // OPTION
  m_wordList[command].Add(wxS("matrix_element_mult"));                // OPTION
  m_wordList[command].Add(wxS("matrix_element_transpose"));           // OPTION
  m_wordList[command].Add(wxS("mattrace"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("mattrace(<M>)"));                       // OPTION
  m_wordList[command].Add(wxS("minor"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("minor(<M>, <i>, <j>)"));                // OPTION
  m_wordList[command].Add(wxS("ncexpt"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ncexpt(<a>, <b>)"));                    // OPTION
  m_wordList[command].Add(wxS("ncharpoly"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("ncharpoly(<M>, <x>)"));                 // OPTION
  m_wordList[command].Add(wxS("newdet"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("newdet(<M>)"));                         // OPTION
  m_wordList[command].Add(wxS("nonscalar"));                          // OPTION
  m_wordList[command].Add(wxS("nonscalarp"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("nonscalarp(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("permanent"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("permanent(<M>)"));                      // OPTION
  m_wordList[command].Add(wxS("rank"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("rank(<M>)"));                           // OPTION
  m_wordList[command].Add(wxS("ratmx"));                              // OPTION
  m_wordList[command].Add(wxS("row"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("row(<M>, <i>)"));                       // OPTION
  m_wordList[command].Add(wxS("scalarmatrixp"));                      // OPTION
  m_wordList[command].Add(wxS("scalefactors"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("scalefactors(<coordinatetransform>)")); // OPTION
  m_wordList[command].Add(wxS("setelmx"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("setelmx(<x>, <i>, <j>, <M>)"));         // OPTION
  m_wordList[command].Add(wxS("similaritytransform"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("similaritytransform(<M>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("simtran(<M>)"));                        // OPTION
  m_wordList[command].Add(wxS("sparse"));                             // OPTION
  m_wordList[command].Add(wxS("submatrix"));                          // FUNCTION
  m_wordList[tmplte].Add(
			 "submatrix(<i_1>, ..., <i_m>, <M>, <j_1>, ..., <j_n>)"); // OPTION
  m_wordList[tmplte].Add(wxS("submatrix(<i_1>, ..., <i_m>, <M>)")); // OPTION
  m_wordList[tmplte].Add(wxS("submatrix(<M>, <j_1>, ..., <j_n>)")); // OPTION
  m_wordList[command].Add(wxS("transpose"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("transpose(<M>)"));                    // OPTION
  m_wordList[command].Add(wxS("triangularize"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("triangularize(<M>)"));                // OPTION
  m_wordList[command].Add(wxS("uniteigenvectors"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("uniteigenvectors(<M>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("ueivects(<M>)"));                     // OPTION
  m_wordList[command].Add(wxS("unitvector"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("unitvector(<x>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("uvect(<x>)"));                        // OPTION
  m_wordList[command].Add(wxS("vectorsimp"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("vectorsimp(<expr>)"));                // OPTION
  m_wordList[command].Add(wxS("vect_cross"));                       // OPTION
  m_wordList[command].Add(wxS("zeromatrix"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("zeromatrix(<m>, <n>)"));              // OPTION
  m_wordList[command].Add(wxS("minpack_lsquares"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("minpack_lsquares(<flist>, <varlist>, <guess> [, "
			     "<tolerance>, <jacobian>])")); // OPTION
  m_wordList[command].Add(wxS("minpack_solve"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("minpack_solve(<flist>, <varlist>, <guess> [, "
			     "<tolerance>, <jacobian>])"));               // OPTION
  m_wordList[command].Add(wxS("aliases"));                                // OPTION
  m_wordList[command].Add(wxS("alphabetic"));                             // OPTION
  m_wordList[command].Add(wxS("args"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("args(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("genindex"));                               // OPTION
  m_wordList[command].Add(wxS("gensumnum"));                              // OPTION
  m_wordList[tmplte].Add(wxS("gensym(<x>)"));                             // OPTION
  m_wordList[command].Add(wxS("infolists"));                              // OPTION
  m_wordList[command].Add(wxS("integerp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("integerp(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("m1pbranch"));                              // OPTION
  m_wordList[command].Add(wxS("numberp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("numberp(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("properties"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("properties(<a>)"));                         // OPTION
  m_wordList[command].Add(wxS("props"));                                  // OPTION
  m_wordList[command].Add(wxS("propvars"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("propvars(<prop>)"));                        // OPTION
  m_wordList[command].Add(wxS("put"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("put(<atom>, <value>, <indicator>)"));       // OPTION
  m_wordList[command].Add(wxS("qput"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("qput(<atom>, <value>, <indicator>)"));      // OPTION
  m_wordList[command].Add(wxS("rem"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("rem(<atom>, <indicator>)"));                // OPTION
  m_wordList[command].Add(wxS("remove"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("remove(<a_1>, <p_1>, ..., <a_n>, <p_n>)")); // OPTION
  m_wordList[tmplte].Add(
			 "remove([<a_1>, ..., <a_m>], [<p_1>, ..., <p_n>], ...)"); // OPTION
  m_wordList[tmplte].Add(wxS("remove(\"<a>\", operator)"));          // OPTION
  m_wordList[tmplte].Add(wxS("remove(<a>, transfun)"));              // OPTION
  m_wordList[tmplte].Add(wxS("remove(all, <p>)"));                   // OPTION
  m_wordList[command].Add(wxS("remvalue"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("remvalue(<name_1>, ..., <name_n>)"));  // OPTION
  m_wordList[tmplte].Add(wxS("remvalue(all)"));                      // OPTION
  m_wordList[command].Add(wxS("rncombine"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("rncombine(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("scalarp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("scalarp(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("setup_autoload"));                    // FUNCTION
  m_wordList[tmplte].Add(
			 "setup_autoload(<filename>, <function_1>, ..., <function_n>)"); // OPTION
  m_wordList[command].Add(wxS("newtonepsilon"));                           // OPTION
  m_wordList[command].Add(wxS("newtonmaxiter"));                           // OPTION
  m_wordList[command].Add(wxS("mnewton")); // FUNCTION
  m_wordList[tmplte].Add(wxS("mnewton(<FuncList>,<VarList>,<GuessList>)")); // OPTION
  m_wordList[command].Add(wxS("adjoin"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("adjoin(<x>, <a>) "));                     // OPTION
  m_wordList[command].Add(wxS("belln"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("belln(<n>)"));                            // OPTION
  m_wordList[command].Add(wxS("cardinality"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("cardinality(<a>)"));                      // OPTION
  m_wordList[command].Add(wxS("cartesian_product"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("cartesian_product(<b_1>, ... , <b_n>)")); // OPTION
  m_wordList[command].Add(wxS("cartesian_product_list"));               // FUNCTION
  m_wordList[tmplte].Add(
			 "cartesian_product_list(<l_1>, ... , <l_n>)");                 // OPTION
  m_wordList[command].Add(wxS("disjoin"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("disjoin(<x>, <a>)"));                       // OPTION
  m_wordList[command].Add(wxS("disjointp"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("disjointp(<a>, <b>) "));                    // OPTION
  m_wordList[command].Add(wxS("divisors"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("divisors(<n>)"));                           // OPTION
  m_wordList[command].Add(wxS("elementp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("elementp(<x>, <a>)"));                      // OPTION
  m_wordList[command].Add(wxS("emptyp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("emptyp(<a>)"));                             // OPTION
  m_wordList[command].Add(wxS("equiv_classes"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("equiv_classes(<s>, <F>)"));                 // OPTION
  m_wordList[command].Add(wxS("every"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("every(<f>, <s>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("every(<f>, <L_1>, ..., <L_n>)"));           // OPTION
  m_wordList[command].Add(wxS("extremal_subset"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("extremal_subset(<s>, <f>, max)"));          // OPTION
  m_wordList[tmplte].Add(wxS("extremal_subset(<s>, <f>, min)"));          // OPTION
  m_wordList[command].Add(wxS("flatten"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("flatten(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("full_listify"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("full_listify(<a>)"));                       // OPTION
  m_wordList[command].Add(wxS("fullsetify"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fullsetify(<a>)"));                         // OPTION
  m_wordList[command].Add(wxS("identity"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("identity(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("integer_partitions"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("integer_partitions(<n>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("integer_partitions(<n>, <len>)"));          // OPTION
  m_wordList[command].Add(wxS("intersect"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("intersect(<a_1>, ..., <a_n>)"));            // OPTION
  m_wordList[command].Add(wxS("intersection"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("intersection(<a_1>, ..., <a_n>)"));         // OPTION
  m_wordList[command].Add(wxS("kron_delta"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("kron_delta(<x>, <y>)"));                    // OPTION
  m_wordList[command].Add(wxS("listify"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("listify(<a>)"));                            // OPTION
  m_wordList[command].Add(wxS("lreduce"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("lreduce(<F>, <s>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("lreduce(<F>, <s>, <s_0>)"));                // OPTION
  m_wordList[command].Add(wxS("makeset"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("makeset(<expr>, <x>, <s>)"));               // OPTION
  m_wordList[command].Add(wxS("moebius"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("moebius(<n>)"));                            // OPTION
  m_wordList[command].Add(wxS("multinomial_coeff"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("multinomial_coeff(<a_1>, ..., <a_n>)"));    // OPTION
  m_wordList[tmplte].Add(wxS("multinomial_coeff()"));                     // OPTION
  m_wordList[command].Add(wxS("num_distinct_partitions"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("num_distinct_partitions(<n>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("num_distinct_partitions(<n>, list)"));      // OPTION
  m_wordList[command].Add(wxS("num_partitions"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("num_partitions(<n>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("num_partitions(<n>, list)"));               // OPTION
  m_wordList[command].Add(wxS("partition_set"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("partition_set(<a>, <f>)"));                 // OPTION
  m_wordList[command].Add(wxS("permutations"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("permutations(<a>)"));                       // OPTION
  m_wordList[command].Add(wxS("powerset"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("powerset(<a>)"));                           // OPTION
  m_wordList[tmplte].Add(wxS("powerset(<a>, <n>)"));                      // OPTION
  m_wordList[command].Add(wxS("random_permutation"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("random_permutation(<a>)"));                 // OPTION
  m_wordList[command].Add(wxS("rreduce"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("rreduce(<F>, <s>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("rreduce(<F>, <s>, @var{s_@{n + 1@}})"));    // OPTION
  m_wordList[command].Add(wxS("setdifference"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("setdifference(<a>, <b>)"));                 // OPTION
  m_wordList[command].Add(wxS("setequalp"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("setequalp(<a>, <b>)"));                     // OPTION
  m_wordList[command].Add(wxS("setify"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("setify(<a>)"));                             // OPTION
  m_wordList[command].Add(wxS("setp"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("setp(<a>)"));                               // OPTION
  m_wordList[command].Add(wxS("set_partitions"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("set_partitions(<a>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("set_partitions(<a>, <n>)"));                // OPTION
  m_wordList[command].Add(wxS("some"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("some(<f>, <a>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("some(<f>, <L_1>, ..., <L_n>)"));            // OPTION
  m_wordList[command].Add(wxS("stirling1"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("stirling1(<n>, <m>)"));                     // OPTION
  m_wordList[command].Add(wxS("stirling2"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("stirling2(<n>, <m>)"));                     // OPTION
  m_wordList[command].Add(wxS("subset"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("subset(<a>, <f>)"));                        // OPTION
  m_wordList[command].Add(wxS("subsetp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("subsetp(<a>, <b>)"));                       // OPTION
  m_wordList[command].Add(wxS("symmdifference"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("symmdifference(<a_1>, ..., <a_n>)"));       // OPTION
  m_wordList[command].Add(wxS("tree_reduce"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("tree_reduce(<F>, <s>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("tree_reduce(<F>, <s>, <s_0>)"));            // OPTION
  m_wordList[command].Add(wxS("union"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("union(<a_1>, ..., <a_n>)"));                // OPTION
  m_wordList[command].Add(wxS("xreduce"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("xreduce(<F>, <s>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("xreduce(<F>, <s>, <s_0>)"));                // OPTION
  m_wordList[command].Add(wxS("bern"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("bern(<n>)"));                               // OPTION
  m_wordList[command].Add(wxS("bernpoly"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("bernpoly(<x>, <n>)"));                      // OPTION
  m_wordList[command].Add(wxS("bfzeta"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("bfzeta(<s>, <n>)"));                        // OPTION
  m_wordList[command].Add(wxS("bfhzeta"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("bfhzeta(<s>, <h>, <n>)"));                  // OPTION
  m_wordList[command].Add(wxS("binomial"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("binomial(<x>, <y>)"));                      // OPTION
  m_wordList[command].Add(wxS("burn"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("burn(<n>)"));                               // OPTION
  m_wordList[command].Add(wxS("cf"));                                     // FUNCTION
  m_wordList[tmplte].Add(wxS("cf(<expr>)"));                              // OPTION
  m_wordList[command].Add(wxS("cfdisrep"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cfdisrep(<list>)"));                        // OPTION
  m_wordList[command].Add(wxS("cfexpand"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cfexpand(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("cflength"));                               // OPTION
  m_wordList[command].Add(wxS("divsum"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("divsum(<n>, <k>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("divsum(<n>)"));                             // OPTION
  m_wordList[command].Add(wxS("euler"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("euler(<n>)"));                              // OPTION
  m_wordList[command].Add(wxS("%gamma"));                                 // OPTION
  m_wordList[command].Add(wxS("factorial"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("factorial(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("factorial_expand"));                       // OPTION
  m_wordList[command].Add(wxS("fib"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("fib(<n>)"));                                // OPTION
  m_wordList[command].Add(wxS("fibtophi"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("fibtophi(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("ifactors"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("ifactors(<n>)"));                           // OPTION
  m_wordList[command].Add(wxS("inrt"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("inrt(<x>, <n>)"));                          // OPTION
  m_wordList[command].Add(wxS("inv_mod"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("inv_mod(<n>, <m>)"));                       // OPTION
  m_wordList[command].Add(wxS("jacobi"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi(<p>, <q>)"));                        // OPTION
  m_wordList[command].Add(wxS("lcm"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("lcm(<expr_1>, ..., <expr_n>)"));            // OPTION
  m_wordList[command].Add(wxS("minfactorial"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("minfactorial(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("next_prime"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("next_prime(<n>)"));                         // OPTION
  m_wordList[command].Add(wxS("partfrac"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("partfrac(<expr>, <var>)"));                 // OPTION
  m_wordList[command].Add(wxS("power_mod"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("power_mod(<a>, <n>, <m>)"));                // OPTION
  m_wordList[command].Add(wxS("primep"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("primep(<n>)"));                             // OPTION
  m_wordList[command].Add(wxS("primep_number_of_tests"));                 // OPTION
  m_wordList[command].Add(wxS("prev_prime"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("prev_prime(<n>)"));                         // OPTION
  m_wordList[command].Add(wxS("qunit"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("qunit(<n>)"));                              // OPTION
  m_wordList[command].Add(wxS("totient"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("totient(<n>)"));                            // OPTION
  m_wordList[command].Add(wxS("zerobern"));                               // OPTION
  m_wordList[command].Add(wxS("zeta"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("zeta(<n>)"));                               // OPTION
  m_wordList[command].Add(wxS("zeta%pi"));                                // OPTION
  m_wordList[command].Add(wxS("polartorect"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("polartorect(<r>, <t>)"));                   // OPTION
  m_wordList[command].Add(wxS("recttopolar"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("recttopolar(<a>, <b>)"));                   // OPTION
  m_wordList[command].Add(wxS("inverse_fft"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("inverse_fft(<y>)"));                        // OPTION
  m_wordList[command].Add(wxS("fft"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("fft(<x>)"));                                // OPTION
  m_wordList[command].Add(wxS("fortindent"));                             // OPTION
  m_wordList[command].Add(wxS("fortran"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("fortran(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("fortspaces"));                             // OPTION
  m_wordList[command].Add(wxS("horner"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("horner(<expr>, <x>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("horner(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("find_root_error"));                        // OPTION
  m_wordList[command].Add(wxS("find_root_abs"));                          // OPTION
  m_wordList[command].Add(wxS("find_root_rel"));                          // OPTION
  m_wordList[command].Add(wxS("find_root"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("find_root(<expr>, <x>, <a>, <b>)"));        // OPTION
  m_wordList[tmplte].Add(wxS("find_root(<f>, <a>, <b>)"));                // OPTION
  m_wordList[command].Add(wxS("bf_find_root"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("bf_find_root(<expr>, <x>, <a>, <b>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("bf_find_root(<f>, <a>, <b>)"));             // OPTION
  m_wordList[command].Add(wxS("newton"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("newton(<expr>, <x>, <x_0>, <eps>)"));       // OPTION
  m_wordList[command].Add(wxS("equalp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("equalp(<x>, <y>)"));                        // OPTION
  m_wordList[command].Add(wxS("remfun"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("remfun(<f>, <expr>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("remfun(<f>, <expr>, <x>)"));                // OPTION
  m_wordList[command].Add(wxS("funp"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("funp(<f>, <expr>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("funp(<f>, <expr>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("absint"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("absint(<f>, <x>, <halfplane>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("absint(<f>, <x>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("absint(<f>, <x>, <a>, <b>)"));              // OPTION
  m_wordList[command].Add(wxS("fourier"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("fourier(<f>, <x>, <p>)"));                  // OPTION
  m_wordList[command].Add(wxS("foursimp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("foursimp(<l>)"));                           // OPTION
  m_wordList[command].Add(wxS("sinnpiflag"));                             // OPTION
  m_wordList[command].Add(wxS("cosnpiflag"));                             // OPTION
  m_wordList[command].Add(wxS("fourexpand"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fourexpand(<l>, <x>, <p>, <limit>)"));      // OPTION
  m_wordList[command].Add(wxS("fourcos"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("fourcos(<f>, <x>, <p>)"));                  // OPTION
  m_wordList[command].Add(wxS("foursin"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("foursin(<f>, <x>, <p>)"));                  // OPTION
  m_wordList[command].Add(wxS("totalfourier"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("totalfourier(<f>, <x>, <p>)"));             // OPTION
  m_wordList[command].Add(wxS("fourint"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("fourint(<f>, <x>)"));                       // OPTION
  m_wordList[command].Add(wxS("fourintcos"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fourintcos(<f>, <x>)"));                    // OPTION
  m_wordList[command].Add(wxS("fourintsin"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fourintsin(<f>, <x>)"));                    // OPTION
  m_wordList[command].Add(wxS("read_matrix"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("read_matrix(<S>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("read_matrix(<S>, <M>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("read_matrix(<S>, <separator_flag>)"));      // OPTION
  m_wordList[tmplte].Add(wxS("read_matrix(<S>, <M>, <separator_flag>)")); // OPTION
  m_wordList[command].Add(wxS("read_array"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("read_array(<S>, <A>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("read_array(<S>, <A>, <separator_flag>)"));  // OPTION
  m_wordList[command].Add(wxS("read_hashed_array"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("read_hashed_array(<S>, <A>)"));             // OPTION
  m_wordList[tmplte].Add(
			 "read_hashed_array(<S>, <A>, <separator_flag>)");              // OPTION
  m_wordList[command].Add(wxS("read_nested_list"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("read_nested_list(<S>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("read_nested_list(<S>, <separator_flag>)")); // OPTION
  m_wordList[command].Add(wxS("read_list"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("read_list(<S>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("read_list(<S>, <L>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("read_list(<S>, <separator_flag>)"));        // OPTION
  m_wordList[tmplte].Add(wxS("read_list(<S>, <L>, <separator_flag>)"));   // OPTION
  m_wordList[command].Add(wxS("write_data"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("write_data(<X>, <D>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("write_data(<X>, <D>, <separator_flag>)"));  // OPTION
  m_wordList[command].Add(wxS("assume_external_byte_order"));             // FUNCTION
  m_wordList[tmplte].Add(
			 "assume_external_byte_order(<byte_order_flag>)");             // OPTION
  m_wordList[command].Add(wxS("openr_binary"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("openr_binary(<file_name>)"));              // OPTION
  m_wordList[command].Add(wxS("openw_binary"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("openw_binary(<file_name>)"));              // OPTION
  m_wordList[command].Add(wxS("opena_binary"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("opena_binary(<file_name>)"));              // OPTION
  m_wordList[command].Add(wxS("read_binary_matrix"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("read_binary_matrix(<S>, <M>)"));           // OPTION
  m_wordList[command].Add(wxS("read_binary_array"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("read_binary_array(<S>, <A>)"));            // OPTION
  m_wordList[command].Add(wxS("read_binary_list"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("read_binary_list(<S>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("read_binary_list(<S>, <L>)"));             // OPTION
  m_wordList[command].Add(wxS("write_binary_data"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("write_binary_data(<X>, <D>)"));            // OPTION
  m_wordList[command].Add(wxS("abs"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("abs(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("additive"));                              // OPTION
  m_wordList[command].Add(wxS("allbut"));                                // OPTION
  m_wordList[command].Add(wxS("antisymmetric"));                         // OPTION
  m_wordList[command].Add(wxS("cabs"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("cabs(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("ceiling"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("ceiling(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("charfun"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("charfun(<p>)"));                           // OPTION
  m_wordList[command].Add(wxS("commutative"));                           // OPTION
  m_wordList[command].Add(wxS("compare"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("compare(<x>, <y>)"));                      // OPTION
  m_wordList[command].Add(wxS("entier"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("entier(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("equal"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("equal(<a>, <b>)"));                        // OPTION
  m_wordList[command].Add(wxS("floor"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("floor(<x>)"));                             // OPTION
  m_wordList[command].Add(wxS("notequal"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("notequal(<a>, <b>)"));                     // OPTION
  m_wordList[command].Add(wxS("evenp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("evenp(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("fix"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("fix(<x>)"));                               // OPTION
  m_wordList[command].Add(wxS("fullmap"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("fullmap(<f>, <expr_1>, <...>)"));          // OPTION
  m_wordList[command].Add(wxS("fullmapl"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("fullmapl(<f>, <list_1>, <...>)"));         // OPTION
  m_wordList[command].Add(wxS("is"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("is(<expr>)"));                             // OPTION
  m_wordList[command].Add(wxS("maybe"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("maybe(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("isqrt"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("isqrt(<x>)"));                             // OPTION
  m_wordList[command].Add(wxS("lmax"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("lmax(<L>)"));                              // OPTION
  m_wordList[command].Add(wxS("lmin"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("lmin(<L>)"));                              // OPTION
  m_wordList[command].Add(wxS("max"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("max(<x_1>, <...>, <x_n>)"));               // OPTION
  m_wordList[command].Add(wxS("min"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("min(<x_1>, <...>, <x_n>)"));               // OPTION
  m_wordList[command].Add(wxS("polymod"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("polymod(<p>)"));                           // OPTION
  m_wordList[tmplte].Add(wxS("polymod(<p>, <m>)"));                      // OPTION
  m_wordList[command].Add(wxS("mod"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("mod(<x>, <y>)"));                          // OPTION
  m_wordList[command].Add(wxS("oddp"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("oddp(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("psubst"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("psubst(<list>, <expr>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("psubst(<a>, <b>, <expr>)"));               // OPTION
  m_wordList[command].Add(wxS("make_random_state"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("make_random_state(<n>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("make_random_state(<s>)"));                 // OPTION
  m_wordList[command].Add(wxS("set_random_state"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("set_random_state(<s>)"));                  // OPTION
  m_wordList[command].Add(wxS("random"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("random(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("rationalize"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("rationalize(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("round"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("round(<x>)"));                             // OPTION
  m_wordList[command].Add(wxS("sign"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("sign(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("signum"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("signum(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("sort"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("sort(<L>, <P>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("sort(<L>)"));                              // OPTION
  m_wordList[command].Add(wxS("sqrt"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("sqrt(<x>)"));                              // OPTION
  m_wordList[command].Add(wxS("sqrtdispflag"));                          // OPTION
  m_wordList[command].Add(wxS("sublis"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("sublis(<list>, <expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("sublist"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("sublist(<list>, <p>)"));                   // OPTION
  m_wordList[command].Add(wxS("sublis_apply_lambda"));                   // OPTION
  m_wordList[command].Add(wxS("subst"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("subst(<a>, <b>, <c>)"));                   // OPTION
  m_wordList[command].Add(wxS("substinpart"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("substinpart(<x>, <expr>, <n_1>, <n_k>)")); // OPTION
  m_wordList[command].Add(wxS("substpart"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("substpart(<x>, <expr>, <n_1>, <n_k>)"));   // OPTION
  m_wordList[command].Add(wxS("subvarp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("subvarp(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("symbolp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("symbolp(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("vectorpotential"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("vectorpotential(<givencurl>)"));           // OPTION
  m_wordList[command].Add(wxS("xthru"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("xthru(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("zeroequiv"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("zeroequiv(<expr>, <v>)"));                 // OPTION
  m_wordList[command].Add(wxS("opsubst"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("opsubst(<f>,<g>,<e>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("opsubst(<g>=<f>,<e>)"));                   // OPTION
  m_wordList[tmplte].Add(
			 "opsubst([<g1>=<f1>,<g2>=<f2>,<gn>=<fn>],<e>)");              // OPTION
  m_wordList[command].Add(wxS("assoc_legendre_p"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("assoc_legendre_p(<n>, <m>, <x>)"));        // OPTION
  m_wordList[command].Add(wxS("assoc_legendre_q"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("assoc_legendre_q(<n>, <m>, <x>)"));        // OPTION
  m_wordList[command].Add(wxS("chebyshev_t"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("chebyshev_t(<n>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("chebyshev_u"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("chebyshev_u(<n>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("gen_laguerre"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("gen_laguerre(<n>, <a>, <x>)"));            // OPTION
  m_wordList[command].Add(wxS("hermite"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("hermite(<n>, <x>)"));                      // OPTION
  m_wordList[command].Add(wxS("intervalp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("intervalp(<e>)"));                         // OPTION
  m_wordList[command].Add(wxS("jacobi_p"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("jacobi_p(<n>, <a>, <b>, <x>)"));           // OPTION
  m_wordList[command].Add(wxS("laguerre"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("laguerre(<n>, <x>)"));                     // OPTION
  m_wordList[command].Add(wxS("legendre_p"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("legendre_p(<n>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("legendre_q"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("legendre_q(<n>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("orthopoly_recur"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("orthopoly_recur(<f>, <args>)"));           // OPTION
  m_wordList[command].Add(wxS("orthopoly_returns_intervals"));           // OPTION
  m_wordList[command].Add(wxS("orthopoly_weight"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("orthopoly_weight(<f>, <args>)"));          // OPTION
  m_wordList[command].Add(wxS("pochhammer"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("pochhammer(<n>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("pochhammer_max_index"));                  // OPTION
  m_wordList[command].Add(wxS("spherical_bessel_j"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("spherical_bessel_j(<n>, <x>)"));           // OPTION
  m_wordList[command].Add(wxS("spherical_bessel_y"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("spherical_bessel_y(<n>, <x>)"));           // OPTION
  m_wordList[command].Add(wxS("spherical_hankel1"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("spherical_hankel1(<n>, <x>)"));            // OPTION
  m_wordList[command].Add(wxS("spherical_hankel2"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("spherical_hankel2(<n>, <x>)"));            // OPTION
  m_wordList[command].Add(wxS("spherical_harmonic"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("spherical_harmonic(<n>, <m>, <x>, <y>)")); // OPTION
  m_wordList[command].Add(wxS("unit_step"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("unit_step(<x>)"));                         // OPTION
  m_wordList[command].Add(wxS("ultraspherical"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("ultraspherical(<n>, <a>, <x>)"));          // OPTION
  m_wordList[command].Add(wxS("plotdf"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("plotdf(<dydx>, <options>)"));              // OPTION
  m_wordList[tmplte].Add(wxS("plotdf(<dvdu>, [<u>,<v>], <options>)"));   // OPTION
  m_wordList[tmplte].Add(wxS("plotdf([<dxdt>,<dydt>], <options>)"));     // OPTION
  m_wordList[tmplte].Add(
			 "plotdf([<dudt>,<dvdt>], [<u>,<v>], <options>)"); // OPTION
  m_wordList[command].Add(wxS("contour_plot"));              // FUNCTION
  m_wordList[tmplte].Add(
			 "contour_plot(<expr>, <x_range>, <y_range>, <options>)");  // OPTION
  m_wordList[command].Add(wxS("get_plot_option"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("get_plot_option(<keyword>, <index>)")); // OPTION
  m_wordList[command].Add(wxS("make_transform"));                     // FUNCTION
  m_wordList[tmplte].Add(
			 "make_transform([<var1>, <var2>, <var3>], <fx>, <fy>, <fz>)"); // OPTION
  m_wordList[command].Add(wxS("plot2d"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("plot2d(<plot>, <x_range>, <[options]>)"));  // OPTION
  m_wordList[tmplte].Add(wxS("plot2d([<plot_1>, <plot_n>], <options>)")); // OPTION
  m_wordList[tmplte].Add(
			 "plot2d([<plot_1>, <plot_n>], <x_range>, <[options]>)"); // OPTION
  m_wordList[command].Add(wxS("plot3d"));                           // FUNCTION
  m_wordList[tmplte].Add(
			 "plot3d(<expr>, <x_range>, <y_range>, <[options]>)"); // OPTION
  m_wordList[tmplte].Add(wxS("plot3d([<expr_1>, <...>, <expr_n>], <x_range>, "
			     "<y_range>, <[options]>)"));             // OPTION
  m_wordList[command].Add(wxS("plot_options"));                       // OPTION
  m_wordList[command].Add(wxS("set_plot_option"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("set_plot_option(<option>)"));           // OPTION
  m_wordList[command].Add(wxS("adapth_depth"));                       // OPTION
  m_wordList[command].Add(wxS("axes"));                               // OPTION
  m_wordList[command].Add(wxS("azimut"));                             // OPTION
  m_wordList[command].Add(wxS("box"));                                // OPTION
  m_wordList[command].Add(wxS("color"));                              // OPTION
  m_wordList[command].Add(wxS("colorbox"));                           // OPTION
  m_wordList[command].Add(wxS("elevation"));                          // OPTION
  m_wordList[command].Add(wxS("grid"));                               // OPTION
  m_wordList[command].Add(wxS("legend"));                             // OPTION
  m_wordList[command].Add(wxS("logx"));                               // OPTION
  m_wordList[command].Add(wxS("logy"));                               // OPTION
  m_wordList[command].Add(wxS("mesh_lines_color"));                   // OPTION
  m_wordList[command].Add(wxS("nticks"));                             // OPTION
  m_wordList[command].Add(wxS("palette"));                            // OPTION
  m_wordList[command].Add(wxS("plot_format"));                        // OPTION
  m_wordList[command].Add(wxS("plot_real_part"));                     // OPTION
  m_wordList[command].Add(wxS("point_type"));                         // OPTION
  m_wordList[command].Add(wxS("psfile"));                             // OPTION
  m_wordList[command].Add(wxS("run_viewer"));                         // OPTION
  m_wordList[command].Add(wxS("style"));                              // OPTION
  m_wordList[command].Add(wxS("t"));                                  // OPTION
  m_wordList[command].Add(wxS("transform_xy"));                       // OPTION
  m_wordList[command].Add(wxS("x"));                                  // OPTION
  m_wordList[command].Add(wxS("xlabel"));                             // OPTION
  m_wordList[command].Add(wxS("y"));                                  // OPTION
  m_wordList[command].Add(wxS("ylabel"));                             // OPTION
  m_wordList[command].Add(wxS("z"));                                  // OPTION
  m_wordList[command].Add(wxS("zlabel"));                             // OPTION
  m_wordList[command].Add(wxS("gnuplot_term"));                       // OPTION
  m_wordList[command].Add(wxS("gnuplot_out_file"));                   // OPTION
  m_wordList[command].Add(wxS("gnuplot_pm3d"));                       // OPTION
  m_wordList[command].Add(wxS("gnuplot_preamble"));                   // OPTION
  m_wordList[command].Add(wxS("gnuplot_curve_titles"));               // OPTION
  m_wordList[command].Add(wxS("gnuplot_curve_styles"));               // OPTION
  m_wordList[command].Add(wxS("gnuplot_default_term_command"));       // OPTION
  m_wordList[command].Add(wxS("gnuplot_dumb_term_command"));          // OPTION
  m_wordList[command].Add(wxS("gnuplot_ps_term_command"));            // OPTION
  m_wordList[command].Add(wxS("gnuplot_start"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("gnuplot_start()"));                     // OPTION
  m_wordList[command].Add(wxS("gnuplot_close"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("gnuplot_close()"));                     // OPTION
  m_wordList[command].Add(wxS("gnuplot_restart"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("gnuplot_restart()"));                   // OPTION
  m_wordList[command].Add(wxS("gnuplot_replot"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("gnuplot_replot()"));                    // OPTION
  m_wordList[tmplte].Add(wxS("gnuplot_replot(<s>)"));                 // OPTION
  m_wordList[command].Add(wxS("gnuplot_reset"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("gnuplot_reset()"));                     // OPTION
  m_wordList[command].Add(wxS("algebraic"));                          // OPTION
  m_wordList[command].Add(wxS("berlefact"));                          // OPTION
  m_wordList[command].Add(wxS("bezout"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bezout(<p1>, <p2>, <x>)"));             // OPTION
  m_wordList[command].Add(wxS("bothcoef"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("bothcoef(<expr>, <x>)"));               // OPTION
  m_wordList[command].Add(wxS("coeff"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("coeff(<expr>, <x>, <n>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("coeff(<expr>, <x>)"));                  // OPTION
  m_wordList[command].Add(wxS("combine"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("combine(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("content"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("content(<p_1>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("denom"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("denom(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("divide"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("divide(<p_1>, <p_2>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("eliminate")); // FUNCTION
  m_wordList[tmplte].Add(
			 "eliminate([<eqn_1>, <...>, <eqn_n>], [<x_1>, <...>, <x_k>])"); // OPTION
  m_wordList[command].Add(wxS("ezgcd"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("ezgcd(<p_1>, <p_2>, <p_3>, ...)"));        // OPTION
  m_wordList[command].Add(wxS("facexpand"));                             // OPTION
  m_wordList[command].Add(wxS("factcomb"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("factcomb(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("factor"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("factor(<expr>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("factor(<expr>, <p>)"));                    // OPTION
  m_wordList[command].Add(wxS("factorflag"));                            // OPTION
  m_wordList[command].Add(wxS("factorout"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("factorout(<expr>, <x_1>, <x_2>, <...>)")); // OPTION
  m_wordList[command].Add(wxS("factorsum"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("factorsum(<expr>)"));                      // OPTION
  m_wordList[command].Add(wxS("fasttimes"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fasttimes(<p_1>, <p_2>)"));                // OPTION
  m_wordList[command].Add(wxS("fullratsimp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("fullratsimp(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("fullratsubst"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("fullratsubst(<a>, <b>, <c>)"));            // OPTION
  m_wordList[command].Add(wxS("gcd"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("gcd(<p_1>, <p_2>, <x_1>, <...>)"));        // OPTION
  m_wordList[command].Add(wxS("gcdex"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("gcdex(<f>, <g>)"));                        // OPTION
  m_wordList[tmplte].Add(wxS("gcdex(<f>, <g>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("gcfactor"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("gcfactor(<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("gfactor"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("gfactor(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("gfactorsum"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("gfactorsum(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("hipow"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("hipow(<expr>, <x>)"));                     // OPTION
  m_wordList[command].Add(wxS("intfaclim"));                             // OPTION
  m_wordList[command].Add(wxS("keepfloat"));                             // OPTION
  m_wordList[command].Add(wxS("lratsubst"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("lratsubst(<L>, <expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("modulus"));                               // OPTION
  m_wordList[command].Add(wxS("num"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("num(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("polydecomp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("polydecomp(<p>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("quotient"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("quotient(<p_1>, <p_2>)"));                 // OPTION
  m_wordList[tmplte].Add(
			 "quotient(<p_1>, <p_2>, <x_1>, <...>, <x_n>)");             // OPTION
  m_wordList[command].Add(wxS("rat"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("rat(<expr>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("rat(<expr>, <x_1>, <...>, <x_n>)"));     // OPTION
  m_wordList[command].Add(wxS("ratalgdenom"));                         // OPTION
  m_wordList[command].Add(wxS("ratcoef"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ratcoef(<expr>, <x>, <n>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("ratcoef(<expr>, <x>)"));                 // OPTION
  m_wordList[command].Add(wxS("ratdenom"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ratdenom(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("ratdenomdivide"));                      // OPTION
  m_wordList[command].Add(wxS("ratdiff"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ratdiff(<expr>, <x>)"));                 // OPTION
  m_wordList[command].Add(wxS("ratdisrep"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("ratdisrep(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("ratepsilon"));                          // OPTION
  m_wordList[command].Add(wxS("ratexpand"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("ratexpand(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("ratfac"));                              // OPTION
  m_wordList[command].Add(wxS("ratnumer"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ratnumer(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("ratnump"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ratnump(<expr>)"));                      // OPTION
  m_wordList[command].Add(wxS("ratp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("ratp(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("ratprint"));                            // OPTION
  m_wordList[command].Add(wxS("float_approx_equal_tolerance"));        // OPTION
  m_wordList[command].Add(wxS("float_approx_equal"));                  // OPTION
  m_wordList[tmplte].Add(wxS("float_approx_equal(<f_1>,<f_2>)"));      // OPTION
  m_wordList[command].Add(wxS("ratsimp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ratsimp(<expr>)"));                      // OPTION
  m_wordList[tmplte].Add(wxS("ratsimp(<expr>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("ratsimpexpons"));                       // OPTION
  m_wordList[command].Add(wxS("ratsubst"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ratsubst(<a>, <b>, <c>)"));              // OPTION
  m_wordList[command].Add(wxS("ratvars"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ratvars(<x_1>, <...>, <x_n>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("ratvars()"));                            // OPTION
  m_wordList[command].Add(wxS("ratweight"));                           // FUNCTION
  m_wordList[tmplte].Add(
			 "ratweight(<x_1>, <w_1>, <...>, <x_n>, <w_n>)"); // OPTION
  m_wordList[tmplte].Add(wxS("ratweight()"));               // OPTION
  m_wordList[command].Add(wxS("ratweights"));               // OPTION
  m_wordList[command].Add(wxS("ratwtlvl"));                 // OPTION
  m_wordList[command].Add(wxS("remainder"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("remainder(<p_1>, <p_2>)"));   // OPTION
  m_wordList[tmplte].Add(
			 "remainder(<p_1>, <p_2>, <x_1>, <...>, <x_n>)");               // OPTION
  m_wordList[command].Add(wxS("resultant"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("resultant(<p_1>, <p_2>, <x>)"));            // OPTION
  m_wordList[command].Add(wxS("savefactors"));                            // OPTION
  m_wordList[command].Add(wxS("sqfr"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("sqfr(<expr>)"));                            // OPTION
  m_wordList[command].Add(wxS("tellrat"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("tellrat(<p_1>, <...>, <p_n>)"));            // OPTION
  m_wordList[tmplte].Add(wxS("tellrat()"));                               // OPTION
  m_wordList[command].Add(wxS("totaldisrep"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("totaldisrep(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("untellrat"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("untellrat(<x_1>, <...>, <x_n>)"));          // OPTION
  m_wordList[command].Add(wxS("backtrace"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("backtrace()"));                             // OPTION
  m_wordList[tmplte].Add(wxS("backtrace(<n>)"));                          // OPTION
  m_wordList[command].Add(wxS("errcatch"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("errcatch(<expr_1>, <...>, <expr_n>)"));     // OPTION
  m_wordList[command].Add(wxS("error"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("error(<expr_1>, <...>, <expr_n>)"));        // OPTION
  m_wordList[command].Add(wxS("errormsg"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("errormsg()"));                              // OPTION
  m_wordList[command].Add(wxS("errormsg"));                               // OPTION
  m_wordList[command].Add(wxS("go"));                                     // FUNCTION
  m_wordList[tmplte].Add(wxS("go(<tag>)"));                               // OPTION
  m_wordList[command].Add(wxS("map"));                                    // FUNCTION
  m_wordList[tmplte].Add(wxS("map(<f>, <expr_1>, <...>, <expr_n>)"));     // OPTION
  m_wordList[command].Add(wxS("mapatom"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("mapatom(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("maperror"));                               // OPTION
  m_wordList[command].Add(wxS("mapprint"));                               // OPTION
  m_wordList[command].Add(wxS("maplist"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("maplist(<f>, <expr_1>, <...>, <expr_n>)")); // OPTION
  m_wordList[command].Add(wxS("prederror"));                              // OPTION
  m_wordList[command].Add(wxS("return"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("return(<value>)"));                         // OPTION
  m_wordList[command].Add(wxS("scanmap"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("scanmap(<f>, <expr>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("scanmap(<f>, <expr>, bottomup)"));          // OPTION
  m_wordList[command].Add(wxS("throw"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("throw(<expr>)"));                           // OPTION
  m_wordList[command].Add(wxS("outermap"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("outermap(<f>, <a_1>, <...>, <a_n>)"));      // OPTION
  m_wordList[command].Add(wxS("romberg"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("romberg(<expr>, <x>, <a>, <b>)"));          // OPTION
  m_wordList[tmplte].Add(wxS("romberg(<F>, <a>, <b>)"));                  // OPTION
  m_wordList[command].Add(wxS("rombergabs"));                             // OPTION
  m_wordList[command].Add(wxS("rombergit"));                              // OPTION
  m_wordList[command].Add(wxS("rombergmin"));                             // OPTION
  m_wordList[command].Add(wxS("rombergtol"));                             // OPTION
  m_wordList[command].Add(wxS("apply1"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("apply1(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList[command].Add(wxS("apply2")); // FUNCTION
  m_wordList[tmplte].Add(wxS("apply2(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList[command].Add(wxS("applyb1")); // FUNCTION
  m_wordList[tmplte].Add(
			 "applyb1(<expr>, <rule_1>, <...>, <rule_n>)");   // OPTION
  m_wordList[command].Add(wxS("current_let_rule_package")); // OPTION
  m_wordList[command].Add(wxS("default_let_rule_package")); // OPTION
  m_wordList[command].Add(wxS("defmatch"));                 // FUNCTION
  m_wordList[tmplte].Add(
			 "defmatch(<progname>, <pattern>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[tmplte].Add(wxS("defmatch(<progname>, <pattern>)"));   // OPTION
  m_wordList[command].Add(wxS("defrule"));                          // FUNCTION
  m_wordList[tmplte].Add(
			 "defrule(<rulename>, <pattern>, <replacement>)"); // OPTION
  m_wordList[command].Add(wxS("disprule"));                  // FUNCTION
  m_wordList[tmplte].Add(
			 "disprule(<rulename_1>, <...>, <rulename_2>)"); // OPTION
  m_wordList[tmplte].Add(wxS("disprule(all)"));            // OPTION
  m_wordList[command].Add(wxS("let"));                     // FUNCTION
  m_wordList[tmplte].Add(
			 "let(<prod>, <repl>, <predname>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[tmplte].Add(wxS("let([<prod>, <repl>, <predname>, <arg_1>, <...>, "
			     "<arg_n>], <package_name>)"));       // OPTION
  m_wordList[command].Add(wxS("letrat"));                         // OPTION
  m_wordList[command].Add(wxS("letrules"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("letrules()"));                      // OPTION
  m_wordList[tmplte].Add(wxS("letrules(<package_name>)"));        // OPTION
  m_wordList[command].Add(wxS("letsimp"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("letsimp(<expr>)"));                 // OPTION
  m_wordList[tmplte].Add(wxS("letsimp(<expr>, <package_name>)")); // OPTION
  m_wordList[tmplte].Add(
			 "letsimp(<expr>, <package_name_1>, <...>, <package_name_n>)"); // OPTION
  m_wordList[command].Add(wxS("let_rule_packages"));                      // OPTION
  m_wordList[command].Add(wxS("matchdeclare"));                           // FUNCTION
  m_wordList[tmplte].Add(
			 "matchdeclare(<a_1>, <pred_1>, <...>, <a_n>, <pred_n>)");   // OPTION
  m_wordList[command].Add(wxS("matchfix"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("matchfix(<ldelimiter>, <rdelimiter>)")); // OPTION
  m_wordList[tmplte].Add(
			 "matchfix(<ldelimiter>, <rdelimiter>, <arg_pos>, <pos>)");     // OPTION
  m_wordList[command].Add(wxS("remlet"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("remlet(<prod>, <name>)"));                  // OPTION
  m_wordList[tmplte].Add(wxS("remlet()"));                                // OPTION
  m_wordList[tmplte].Add(wxS("remlet(all)"));                             // OPTION
  m_wordList[tmplte].Add(wxS("remlet(all, <name>)"));                     // OPTION
  m_wordList[command].Add(wxS("remrule"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("remrule(<op>, <rulename>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("remrule(<op>, all)"));                      // OPTION
  m_wordList[command].Add(wxS("tellsimp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("tellsimp(<pattern>, <replacement>)"));      // OPTION
  m_wordList[command].Add(wxS("tellsimpafter"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("tellsimpafter(<pattern>, <replacement>)")); // OPTION
  m_wordList[command].Add(wxS("clear_rules"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("clear_rules()"));                           // OPTION
  m_wordList[command].Add(wxS("feature"));                                // OPTION
  m_wordList[command].Add(wxS("featurep"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("featurep(<a>, <f>)"));                      // OPTION
  m_wordList[command].Add(wxS("maxima_tempdir"));                         // OPTION
  m_wordList[command].Add(wxS("maxima_userdir"));                         // OPTION
  m_wordList[command].Add(wxS("maxima_objdir"));                          // OPTION
  m_wordList[command].Add(wxS("maxima_frontend"));                        // OPTION
  m_wordList[command].Add(wxS("maxima_frontend_version"));                // OPTION
  m_wordList[command].Add(wxS("room"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("room()"));                                  // OPTION
  m_wordList[tmplte].Add(wxS("room(true)"));                              // OPTION
  m_wordList[tmplte].Add(wxS("room(false)"));                             // OPTION
  m_wordList[command].Add(wxS("rules"));                                  // OPTION
  m_wordList[command].Add(wxS("sstatus"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("sstatus(<keyword>, <item>)"));              // OPTION
  m_wordList[command].Add(wxS("status"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("status(<feature>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("status(<feature>, <item>)"));               // OPTION
  m_wordList[command].Add(wxS("time"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("time(<%o1>, <%o2>, <%o3>, <...>)"));        // OPTION
  m_wordList[command].Add(wxS("timedate"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("timedate()"));                              // OPTION
  m_wordList[tmplte].Add(wxS("timedate(<T>)"));                           // OPTION
  m_wordList[command].Add(wxS("absolute_real_time"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("absolute_real_time()"));                    // OPTION
  m_wordList[command].Add(wxS("elapsed_real_time"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("elapsed_real_time()"));                     // OPTION
  m_wordList[command].Add(wxS("elapsed_run_time"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("elapsed_run_time()"));                      // OPTION
  m_wordList[command].Add(wxS("cauchysum"));                              // OPTION
  m_wordList[command].Add(wxS("deftaylor"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("deftaylor(<f_1>(<x_1>), <expr_1>, <...>, "
			     "<f_n>(<x_n>), <expr_n>)"));          // OPTION
  m_wordList[command].Add(wxS("maxtayorder"));                     // OPTION
  m_wordList[command].Add(wxS("niceindices"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("niceindices(<expr>)"));              // OPTION
  m_wordList[command].Add(wxS("niceindicespref"));                 // OPTION
  m_wordList[command].Add(wxS("nusum"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("nusum(<expr>, <x>, <i_0>, <i_1>)")); // OPTION
  m_wordList[command].Add(wxS("pade"));                            // FUNCTION
  m_wordList[tmplte].Add(
			 "pade(<taylor_series>, <numer_deg_bound>, <denom_deg_bound>)"); // OPTION
  m_wordList[command].Add(wxS("powerdisp"));                               // OPTION
  m_wordList[command].Add(wxS("powerseries"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("powerseries(<expr>, <x>, <a>)")); // OPTION
  m_wordList[command].Add(wxS("psexpand"));                     // OPTION
  m_wordList[command].Add(wxS("revert"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("revert(<expr>, <x>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("revert2(<expr>, <x>, <n>)"));     // OPTION
  m_wordList[command].Add(wxS("taylor"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("taylor(<expr>, <x>, <a>, <n>)")); // OPTION
  m_wordList[tmplte].Add(
			 "taylor(<expr>, [<x_1>, <x_2>, <...>], <a>, <n>)");            // OPTION
  m_wordList[tmplte].Add(wxS("taylor(<expr>, [<x>, <a>, <n>, 'asymp])")); // OPTION
  m_wordList[tmplte].Add(wxS("taylor(<expr>, [<x_1>, <x_2>, <...>], [<a_1>, <a_2>, "
			     "<...>], [<n_1>, <n_2>, <...>])")); // OPTION
  m_wordList[tmplte].Add(wxS("taylor(<expr>, [<x_1>, <a_1>, <n_1>], [<x_2>, <a_2>, "
			     "<n_2>], <...>)"));                         // OPTION
  m_wordList[command].Add(wxS("taylordepth"));                           // OPTION
  m_wordList[command].Add(wxS("taylorinfo"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("taylorinfo(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("taylorp"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("taylorp(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("taylor_logexpand"));                      // OPTION
  m_wordList[command].Add(wxS("taylor_order_coefficients"));             // OPTION
  m_wordList[command].Add(wxS("taylor_simplifier"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("taylor_simplifier(<expr>)"));              // OPTION
  m_wordList[command].Add(wxS("taylor_truncate_polynomials"));           // OPTION
  m_wordList[command].Add(wxS("taytorat"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("taytorat(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("trunc"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("trunc(<expr>)"));                          // OPTION
  m_wordList[command].Add(wxS("unsum"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("unsum(<f>, <n>)"));                        // OPTION
  m_wordList[command].Add(wxS("verbose"));                               // OPTION
  m_wordList[command].Add(wxS("intopois"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("intopois(<a>)"));                          // OPTION
  m_wordList[command].Add(wxS("outofpois"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("outofpois(<a>)"));                         // OPTION
  m_wordList[command].Add(wxS("poisdiff"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poisdiff(<a>, <b>)"));                     // OPTION
  m_wordList[command].Add(wxS("poisexpt"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poisexpt(<a>, <b>)"));                     // OPTION
  m_wordList[command].Add(wxS("poisint"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("poisint(<a>, <b>)"));                      // OPTION
  m_wordList[command].Add(wxS("poislim"));                               // OPTION
  m_wordList[command].Add(wxS("poismap"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("poismap(<series>, <sinfn>, <cosfn>)"));    // OPTION
  m_wordList[command].Add(wxS("poisplus"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poisplus(<a>, <b>)"));                     // OPTION
  m_wordList[command].Add(wxS("poissimp"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poissimp(<a>)"));                          // OPTION
  m_wordList[command].Add(wxS("poisson"));                               // OPTION
  m_wordList[command].Add(wxS("poissubst"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("poissubst(<a>, <b>, <c>)"));               // OPTION
  m_wordList[command].Add(wxS("poistimes"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("poistimes(<a>, <b>)"));                    // OPTION
  m_wordList[command].Add(wxS("poistrim"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("poistrim()"));                             // OPTION
  m_wordList[command].Add(wxS("printpois"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("printpois(<a>)"));                         // OPTION
  m_wordList[command].Add(wxS("epsilon_lp"));                            // OPTION
  m_wordList[command].Add(wxS("linear_program"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("linear_program(<A>, <b>, <c>)"));          // OPTION
  m_wordList[command].Add(wxS("maximize_lp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("maximize_lp(<obj>, <cond>, [<pos>])"));    // OPTION
  m_wordList[command].Add(wxS("minimize_lp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("minimize_lp(<obj>, <cond>, [<pos>])"));    // OPTION
  m_wordList[command].Add(wxS("nonegative_lp"));                         // OPTION
  m_wordList[command].Add(wxS("askexp"));                                // OPTION
  m_wordList[command].Add(wxS("askinteger"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("askinteger(<expr>, integer)"));            // OPTION
  m_wordList[tmplte].Add(wxS("askinteger(<expr>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("askinteger(<expr>, even)"));               // OPTION
  m_wordList[tmplte].Add(wxS("askinteger(<expr>, odd)"));                // OPTION
  m_wordList[command].Add(wxS("asksign"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("asksign(<expr>)"));                        // OPTION
  m_wordList[command].Add(wxS("demoivre"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("demoivre(<expr>)"));                       // OPTION
  m_wordList[command].Add(wxS("distribute_over"));                       // OPTION
  m_wordList[command].Add(wxS("domain"));                                // OPTION
  m_wordList[command].Add(wxS("expand"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("expand(<expr>)"));                         // OPTION
  m_wordList[tmplte].Add(wxS("expand(<expr>, <p>, <n>)"));               // OPTION
  m_wordList[command].Add(wxS("expandwrt"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("expandwrt(<expr>, <x_1>, <...>, <x_n>)")); // OPTION
  m_wordList[command].Add(wxS("expandwrt_denom"));                       // OPTION
  m_wordList[command].Add(wxS("expandwrt_factored"));                    // FUNCTION
  m_wordList[tmplte].Add(
			 "expandwrt_factored(<expr>, <x_1>, <...>, <x_n>)"); // OPTION
  m_wordList[command].Add(wxS("expon"));                       // OPTION
  m_wordList[command].Add(wxS("exponentialize"));              // FUNCTION
  m_wordList[tmplte].Add(wxS("exponentialize(<expr>)"));       // OPTION
  m_wordList[command].Add(wxS("expop"));                       // OPTION
  m_wordList[command].Add(wxS("factlim"));                     // OPTION
  m_wordList[command].Add(wxS("intosum"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("intosum(<expr>)"));              // OPTION
  m_wordList[command].Add(wxS("lassociative"));                // OPTION
  m_wordList[command].Add(wxS("linear"));                      // OPTION
  m_wordList[command].Add(wxS("mainvar"));                     // OPTION
  m_wordList[command].Add(wxS("maxapplydepth"));               // OPTION
  m_wordList[command].Add(wxS("maxapplyheight"));              // OPTION
  m_wordList[command].Add(wxS("maxnegex"));                    // OPTION
  m_wordList[command].Add(wxS("maxposex"));                    // OPTION
  m_wordList[command].Add(wxS("multiplicative"));              // OPTION
  m_wordList[command].Add(wxS("negdistrib"));                  // OPTION
  m_wordList[command].Add(wxS("negsumdispflag"));              // OPTION
  m_wordList[command].Add(wxS("noeval"));                      // OPTION
  m_wordList[command].Add(wxS("noun"));                        // OPTION
  m_wordList[command].Add(wxS("noundisp"));                    // OPTION
  m_wordList[command].Add(wxS("nouns"));                       // OPTION
  m_wordList[command].Add(wxS("numer"));                       // OPTION
  m_wordList[command].Add(wxS("numerval"));                    // FUNCTION
  m_wordList[tmplte].Add(
			 "numerval(<x_1>, <expr_1>, <...>, <var_n>, <expr_n>)"); // OPTION
  m_wordList[command].Add(wxS("opproperties"));                    // OPTION
  m_wordList[command].Add(wxS("opsubst"));                         // OPTION
  m_wordList[command].Add(wxS("outative"));                        // OPTION
  m_wordList[command].Add(wxS("posfun"));                          // OPTION
  m_wordList[command].Add(wxS("pred"));                            // OPTION
  m_wordList[command].Add(wxS("radcan"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("radcan(<expr>)"));                   // OPTION
  m_wordList[command].Add(wxS("radexpand"));                       // OPTION
  m_wordList[command].Add(wxS("radsubstflag"));                    // OPTION
  m_wordList[command].Add(wxS("rassociative"));                    // OPTION
  m_wordList[command].Add(wxS("scsimp"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("scsimp(<expr>, <rule_1>, <...>, <rule_n>)")); // OPTION
  m_wordList[command].Add(wxS("simp"));                                     // OPTION
  m_wordList[command].Add(wxS("simpsum"));                                  // OPTION
  m_wordList[command].Add(wxS("sumcontract"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("sumcontract(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("sumexpand"));                              // OPTION
  m_wordList[command].Add(wxS("sumsplitfact"));                           // OPTION
  m_wordList[command].Add(wxS("symmetric"));                              // OPTION
  m_wordList[command].Add(wxS("unknown"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("unknown(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("facsum"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("facsum(<expr>, <arg_1>, <...>, <arg_n>)")); // OPTION
  m_wordList[command].Add(wxS("nextlayerfactor"));                        // OPTION
  m_wordList[command].Add(wxS("facsum_combine"));                         // OPTION
  m_wordList[command].Add(wxS("factorfacsum"));                           // FUNCTION
  m_wordList[tmplte].Add(
			 "factorfacsum(<expr>, <arg_1>, <...>, <arg_n>)"); // OPTION
  m_wordList[command].Add(wxS("collectterms"));              // FUNCTION
  m_wordList[tmplte].Add(
			 "collectterms(<expr>, <arg_1>, <...>, <arg_n>)");            // OPTION
  m_wordList[command].Add(wxS("rempart"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("rempart(<expr>, <n>)"));                  // OPTION
  m_wordList[command].Add(wxS("wronskian"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("wronskian([<f_1>, <...>, <f_n>], <x>)")); // OPTION
  m_wordList[command].Add(wxS("tracematrix"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("tracematrix(<M>)"));                      // OPTION
  m_wordList[command].Add(wxS("rational"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("rational(<z>)"));                         // OPTION
  m_wordList[command].Add(wxS("logand"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("logand(<x>,<y>)"));                       // OPTION
  m_wordList[command].Add(wxS("logor"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("logor(<x>,<y>)"));                        // OPTION
  m_wordList[command].Add(wxS("logxor"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("logxor(<x>,<y>)"));                       // OPTION
  m_wordList[command].Add(wxS("nonzeroandfreeof"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("nonzeroandfreeof(<x>, <expr>)"));         // OPTION
  m_wordList[command].Add(wxS("linear"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("linear(<expr>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("gcdivide"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("gcdivide(<p>, <q>)"));                    // OPTION
  m_wordList[command].Add(wxS("arithmetic"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("arithmetic(<a>, <d>, <n>)"));             // OPTION
  m_wordList[command].Add(wxS("geometric"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("geometric(<a>, <r>, <n>)"));              // OPTION
  m_wordList[command].Add(wxS("harmonic"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("harmonic(<a>, <b>, <c>, <n>)"));          // OPTION
  m_wordList[command].Add(wxS("arithsum"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("arithsum(<a>, <d>, <n>)"));               // OPTION
  m_wordList[command].Add(wxS("geosum"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("geosum(<a>, <r>, <n>)"));                 // OPTION
  m_wordList[command].Add(wxS("gaussprob"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("gaussprob(<x>)"));                        // OPTION
  m_wordList[command].Add(wxS("gd"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("gd(<x>)"));                               // OPTION
  m_wordList[command].Add(wxS("agd"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("agd(<x>)"));                              // OPTION
  m_wordList[command].Add(wxS("vers"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("vers(<x>)"));                             // OPTION
  m_wordList[command].Add(wxS("covers"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("covers(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("exsec"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("exsec(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("hav"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("hav(<x>)"));                              // OPTION
  m_wordList[command].Add(wxS("combination"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("combination(<n>, <r>)"));                 // OPTION
  m_wordList[command].Add(wxS("permutation"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("permutation(<n>, <r>)"));                 // OPTION
  m_wordList[command].Add(wxS("reduce_consts"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("reduce_consts(<expr>)"));                 // OPTION
  m_wordList[command].Add(wxS("gcfac"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("gcfac(<expr>)"));                         // OPTION
  m_wordList[command].Add(wxS("sqrtdenest"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("sqrtdenest(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("reduce_order"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("reduce_order(<rec>, <sol>, <var>)"));     // OPTION
  m_wordList[command].Add(wxS("simplify_products"));                    // OPTION
  m_wordList[command].Add(wxS("simplify_sum"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("simplify_sum(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("solve_rec"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("solve_rec(<eqn>, <var>, [<init>])"));     // OPTION
  m_wordList[command].Add(wxS("solve_rec_rat"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("solve_rec_rat(<eqn>, <var>, [<init>])")); // OPTION
  m_wordList[command].Add(wxS("product_use_gamma"));                    // OPTION
  m_wordList[command].Add(wxS("summand_to_rec"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("summand_to_rec(<summand>, <k>, <n>)"));   // OPTION
  m_wordList[tmplte].Add(
			 "summand_to_rec(<summand>, [<k>, <lo>, <hi>], <n>)");        // OPTION
  m_wordList[command].Add(wxS("bessel_j"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bessel_j(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("bessel_y"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bessel_y(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("bessel_i"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bessel_i(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("bessel_k"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("bessel_k(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("hankel_1"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("hankel_1(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("hankel_2"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("hankel_2(<v>, <z>)"));                    // OPTION
  m_wordList[command].Add(wxS("besselexpand"));                         // OPTION
  m_wordList[command].Add(wxS("scaled_bessel_i"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("scaled_bessel_i(<v>, <z>) "));            // OPTION
  m_wordList[command].Add(wxS("scaled_bessel_i0"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("scaled_bessel_i0(<z>) "));                // OPTION
  m_wordList[command].Add(wxS("scaled_bessel_i1"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("scaled_bessel_i1(<z>) "));                // OPTION
  m_wordList[command].Add(wxS("%s"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("%s[<u>,<v>] (<z>) "));                    // OPTION
  m_wordList[command].Add(wxS("airy_ai"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("airy_ai(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("airy_dai"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("airy_dai(<x>)"));                         // OPTION
  m_wordList[command].Add(wxS("airy_bi"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("airy_bi(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("airy_dbi"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("airy_dbi(<x>)"));                         // OPTION
  m_wordList[command].Add(wxS("gamma"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("gamma(<z>)"));                            // OPTION
  m_wordList[command].Add(wxS("log_gamma"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("log_gamma(<z>)"));                        // OPTION
  m_wordList[command].Add(wxS("gamma_incomplete"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("gamma_incomplete(<a>, <z>)"));            // OPTION
  m_wordList[command].Add(wxS("gamma_incomplete_regularized"));         // FUNCTION
  m_wordList[tmplte].Add(wxS("gamma_incomplete_regularized(<a>,<z>)")); // OPTION
  m_wordList[command].Add(wxS("gamma_incomplete_generalized"));         // FUNCTION
  m_wordList[tmplte].Add(
			 "gamma_incomplete_generalized(<a>,<z1>,<z1> )");      // OPTION
  m_wordList[command].Add(wxS("gammalim"));                      // OPTION
  m_wordList[command].Add(wxS("makegamma"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("makegamma(<expr>)"));              // OPTION
  m_wordList[command].Add(wxS("beta"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("beta(<a>, <b>)"));                 // OPTION
  m_wordList[command].Add(wxS("beta_incomplete"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("beta_incomplete(<a>, <b>, <z>)")); // OPTION
  m_wordList[command].Add(wxS("beta_incomplete_regularized"));   // FUNCTION
  m_wordList[tmplte].Add(
			 "beta_incomplete_regularized(<a>, <b>, <z>)");      // OPTION
  m_wordList[command].Add(wxS("beta_incomplete_generalized")); // FUNCTION
  m_wordList[tmplte].Add(
			 "beta_incomplete_generalized(<a>, <b>, <z1>, <z2>)"); // OPTION
  m_wordList[command].Add(wxS("beta_expand"));                   // OPTION
  m_wordList[command].Add(wxS("beta_args_sum_to_integer"));      // OPTION
  m_wordList[command].Add(wxS("psi"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("psi[<n>](<x>)"));                  // OPTION
  m_wordList[command].Add(wxS("maxpsiposint"));                  // OPTION
  m_wordList[command].Add(wxS("maxpsinegint"));                  // OPTION
  m_wordList[command].Add(wxS("maxpsifracnum"));                 // OPTION
  m_wordList[command].Add(wxS("maxpsifracdenom"));               // OPTION
  m_wordList[command].Add(wxS("makefact"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("makefact(<expr>)"));               // OPTION
  m_wordList[command].Add(wxS("numfactor"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("numfactor(<expr>)"));              // OPTION
  m_wordList[command].Add(wxS("expintegral_e1"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_e1(<z>)"));            // OPTION
  m_wordList[command].Add(wxS("expintegral_ei"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_ei(<z>)"));            // OPTION
  m_wordList[command].Add(wxS("expintegral_li"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_li(<z>)"));            // OPTION
  m_wordList[command].Add(wxS("expintegral_e"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_e(<n>,<z>)"));         // OPTION
  m_wordList[command].Add(wxS("expintegral_si"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_si(<z>)"));            // OPTION
  m_wordList[command].Add(wxS("expintegral_ci"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_ci(<z>)"));            // OPTION
  m_wordList[command].Add(wxS("expintegral_shi"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_shi(<z>)"));           // OPTION
  m_wordList[command].Add(wxS("expintegral_chi"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("expintegral_chi(<z>)"));           // OPTION
  m_wordList[command].Add(wxS("expintrep"));                     // OPTION
  m_wordList[command].Add(wxS("expintexpand"));                  // OPTION
  m_wordList[command].Add(wxS("erf"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("erf(<z>)"));                       // OPTION
  m_wordList[command].Add(wxS("erfc"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("erfc(<z>)"));                      // OPTION
  m_wordList[command].Add(wxS("erfi"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("erfi(<z>)"));                      // OPTION
  m_wordList[command].Add(wxS("erf_generalized"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("erf_generalized(<z1>,<z2>)"));     // OPTION
  m_wordList[command].Add(wxS("fresnel_c"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("fresnel_c(<z>)"));                 // OPTION
  m_wordList[command].Add(wxS("fresnel_s"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("fresnel_s(<z>)"));                 // OPTION
  m_wordList[command].Add(wxS("gensym"));                        // FUNCTION
  m_wordList[command].Add(wxS("erf_representation"));            // OPTION
  m_wordList[command].Add(wxS("hypergeometric_representation")); // OPTION
  m_wordList[command].Add(wxS("struve_h"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("struve_h(<v>, <z>)"));             // OPTION
  m_wordList[command].Add(wxS("struve_l"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("struve_l(<v>, <z>)"));             // OPTION
  m_wordList[command].Add(wxS("%m"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("%m[<k>,<u>] (<z>) "));             // OPTION
  m_wordList[command].Add(wxS("%w"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("%w[<k>,<u>] (<z>) "));             // OPTION
  m_wordList[command].Add(wxS("%f"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("%f[<p>,<q>] (<[a],[b],z>) "));     // OPTION
  m_wordList[command].Add(wxS("hypergeometric"));                // FUNCTION
  m_wordList[tmplte].Add(
			 "hypergeometric([<a1>, <...>, <ap>],[<b1>, <...> ,<bq>], x)"); // OPTION
  m_wordList[command].Add(wxS("parabolic_cylinder_d"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("parabolic_cylinder_d(<v>, <z>) "));         // OPTION
  m_wordList[command].Add(wxS("specint"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("specint(exp(- s*<t>) * <expr>, <t>)"));     // OPTION
  m_wordList[command].Add(wxS("hgfred"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("hgfred(<a>, <b>, <t>)"));                   // OPTION
  m_wordList[command].Add(wxS("lambert_w"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("lambert_w(<z>)"));                          // OPTION
  m_wordList[command].Add(wxS("nzeta"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("nzeta(<z>)"));                              // OPTION
  m_wordList[command].Add(wxS("nzetar"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("nzetar(<z>)"));                             // OPTION
  m_wordList[command].Add(wxS("nzetai"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("nzetai(<z>)"));                             // OPTION
  m_wordList[command].Add(wxS("inference_result"));                       // FUNCTION
  m_wordList[tmplte].Add(
			 "inference_result(<title>, <values>, <numbers>)");       // OPTION
  m_wordList[command].Add(wxS("inferencep"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("inferencep(<obj>)"));                 // OPTION
  m_wordList[command].Add(wxS("items_inference"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("items_inference(<obj>)"));            // OPTION
  m_wordList[command].Add(wxS("take_inference"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("take_inference(<n>, <obj>)"));        // OPTION
  m_wordList[tmplte].Add(wxS("take_inference(<name>, <obj>)"));     // OPTION
  m_wordList[tmplte].Add(wxS("take_inference(<list>, <obj>)"));     // OPTION
  m_wordList[command].Add(wxS("stats_numer"));                      // OPTION
  m_wordList[command].Add(wxS("test_mean"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("test_mean(<x>)"));                    // OPTION
  m_wordList[tmplte].Add(wxS("test_mean(<x>, <options>, <...>)"));  // OPTION
  m_wordList[command].Add(wxS("test_means_difference"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("test_means_difference(<x1>, <x2>)")); // OPTION
  m_wordList[tmplte].Add(
			 "test_means_difference(<x1>, <x2>, <options>, <...>)");     // OPTION
  m_wordList[command].Add(wxS("test_variance"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("test_variance(<x>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("test_variance(<x>, <options>, <...>)")); // OPTION
  m_wordList[command].Add(wxS("test_variance_ratio"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("test_variance_ratio(<x1>, <x2>)"));      // OPTION
  m_wordList[tmplte].Add(
			 "test_variance_ratio(<x1>, <x2>, <options>, <...>)"); // OPTION
  m_wordList[command].Add(wxS("test_proportion"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("test_proportion(<x>, <n>)"));      // OPTION
  m_wordList[tmplte].Add(
			 "test_proportion(<x>, <n>, <options>, <...>)");     // OPTION
  m_wordList[command].Add(wxS("test_proportions_difference")); // FUNCTION
  m_wordList[tmplte].Add(
			 "test_proportions_difference(<x1>, <n1>, <x2>, <n2>)"); // OPTION
  m_wordList[tmplte].Add(wxS("test_proportions_difference(<x1>, <n1>, <x2>, <n2>, "
			     "<options>, <...>)"));                       // OPTION
  m_wordList[command].Add(wxS("test_sign"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("test_sign(<x>)"));                          // OPTION
  m_wordList[tmplte].Add(wxS("test_sign(<x>, <options>, <...>)"));        // OPTION
  m_wordList[command].Add(wxS("test_signed_rank"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("test_signed_rank(<x>)"));                   // OPTION
  m_wordList[tmplte].Add(wxS("test_signed_rank(<x>, <options>, <...>)")); // OPTION
  m_wordList[command].Add(wxS("test_rank_sum"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("test_rank_sum(<x1>, <x2>)"));               // OPTION
  m_wordList[tmplte].Add(wxS("test_rank_sum(<x1>, <x2>, <option>)"));     // OPTION
  m_wordList[command].Add(wxS("test_normality"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("test_normality(<x>)"));                     // OPTION
  m_wordList[command].Add(wxS("simple_linear_regression"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("simple_linear_regression(<x>)"));           // OPTION
  m_wordList[tmplte].Add(wxS("simple_linear_regression(<x>, <option>)")); // OPTION
  m_wordList[command].Add(wxS("pdf_signed_rank"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_signed_rank(<x>, <n>)"));               // OPTION
  m_wordList[command].Add(wxS("cdf_signed_rank"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_signed_rank(<x>, <n>)"));               // OPTION
  m_wordList[command].Add(wxS("pdf_rank_sum"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("pdf_rank_sum(<x>, <n>, <m>)"));             // OPTION
  m_wordList[command].Add(wxS("cdf_rank_sum"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cdf_rank_sum(<x>, <n>, <m>)"));             // OPTION
  m_wordList[command].Add(wxS("stirling"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("stirling(<z>,<n>)"));                       // OPTION
  m_wordList[tmplte].Add(wxS("stirling(<z>,<n>,<pred>)"));                // OPTION
  m_wordList[command].Add(wxS("close"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("close(<stream>) "));                        // OPTION
  m_wordList[command].Add(wxS("flength"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("flength(<stream>)"));                       // OPTION
  m_wordList[command].Add(wxS("fposition"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("fposition(<stream>)"));                     // OPTION
  m_wordList[tmplte].Add(wxS("fposition(<stream>, <pos>)"));              // OPTION
  m_wordList[command].Add(wxS("freshline"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("freshline() "));                            // OPTION
  m_wordList[tmplte].Add(wxS("freshline(<stream>) "));                    // OPTION
  m_wordList[command].Add(wxS("newline"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("newline() "));                              // OPTION
  m_wordList[tmplte].Add(wxS("newline(<stream>) "));                      // OPTION
  m_wordList[command].Add(wxS("opena"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("opena(<file>) "));                          // OPTION
  m_wordList[command].Add(wxS("openr"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("openr(<file>) "));                          // OPTION
  m_wordList[tmplte].Add(wxS("openr(<file>,<encoding_name_as_string>) ")); // OPTION
  m_wordList[command].Add(wxS("openw"));         // FUNCTION
  m_wordList[tmplte].Add(wxS("openw(<file>) ")); // OPTION
  m_wordList[tmplte].Add(wxS("openw(<file>,<encoding_name_as_string>) ")); // OPTION
  m_wordList[command].Add(wxS("printf"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("printf(<dest>, <string>)")); // OPTION
  m_wordList[tmplte].Add(
			 "printf(<dest>, <string>, <expr_1>, <...>, <expr_n>)");        // OPTION
  m_wordList[command].Add(wxS("readline"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("readline(<stream>) "));                     // OPTION
  m_wordList[command].Add(wxS("sprint"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("sprint(<expr_1>, <...>, <expr_n>)"));       // OPTION
  m_wordList[command].Add(wxS("alphacharp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("alphacharp(<char>)    "));                  // OPTION
  m_wordList[command].Add(wxS("alphanumericp"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("alphanumericp(<char>) "));                  // OPTION
  m_wordList[command].Add(wxS("askequal"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("askequal(<exp1>,<exp2>)"));                 // OPTION
  m_wordList[command].Add(wxS("ascii"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ascii(<int>) "));                           // OPTION
  m_wordList[command].Add(wxS("cequal"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("cequal(<char_1>, <char_2>)          "));    // OPTION
  m_wordList[command].Add(wxS("cequalignore"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("cequalignore(<char_1>, <char_2>)    "));    // OPTION
  m_wordList[command].Add(wxS("cgreaterp"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("cgreaterp(<char_1>, <char_2>)       "));    // OPTION
  m_wordList[command].Add(wxS("cgreaterpignore"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("cgreaterpignore(<char_1>, <char_2>)"));     // OPTION
  m_wordList[command].Add(wxS("charp"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("charp(<obj>) "));                           // OPTION
  m_wordList[command].Add(wxS("cint"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("cint(<char>) "));                           // OPTION
  m_wordList[command].Add(wxS("clessp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("clessp(<char_1>, <char_2>)"));              // OPTION
  m_wordList[command].Add(wxS("clesspignore"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("clesspignore(<char_1>, <char_2>)"));        // OPTION
  m_wordList[command].Add(wxS("constituent"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("constituent(<char>)   "));                  // OPTION
  m_wordList[command].Add(wxS("cunlisp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("cunlisp(<lisp_char>) "));                   // OPTION
  m_wordList[command].Add(wxS("digitcharp"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("digitcharp(<char>)    "));                  // OPTION
  m_wordList[command].Add(wxS("lcharp"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("lcharp(<obj>) "));                          // OPTION
  m_wordList[command].Add(wxS("lowercasep"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("lowercasep(<char>)    "));                  // OPTION
  m_wordList[command].Add(wxS("newline"));                                // OPTION
  m_wordList[command].Add(wxS("space"));                                  // OPTION
  m_wordList[command].Add(wxS("tab"));                                    // OPTION
  m_wordList[command].Add(wxS("uppercasep"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("uppercasep(<char>)    "));                  // OPTION
  m_wordList[command].Add(wxS("stringp"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("stringp(<obj>) "));                         // OPTION
  m_wordList[command].Add(wxS("charat"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("charat(<string>, <n>) "));                  // OPTION
  m_wordList[command].Add(wxS("charlist"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("charlist(<string>) "));                     // OPTION
  m_wordList[command].Add(wxS("eval_string"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("eval_string(<str>)"));                      // OPTION
  m_wordList[command].Add(wxS("parse_string"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("parse_string(<str>)"));                     // OPTION
  m_wordList[command].Add(wxS("scopy"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("scopy(<string>) "));                        // OPTION
  m_wordList[command].Add(wxS("sdowncase"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("sdowncase(<string>) "));                    // OPTION
  m_wordList[tmplte].Add(wxS("sdowncase(<string>, <start>) "));           // OPTION
  m_wordList[tmplte].Add(wxS("sdowncase(<string>, <start>, <end>) "));    // OPTION
  m_wordList[command].Add(wxS("sequal"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("sequal(<string_1>, <string_2>) "));         // OPTION
  m_wordList[command].Add(wxS("sequalignore"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("sequalignore(<string_1>, <string_2>)"));    // OPTION
  m_wordList[command].Add(wxS("sexplode"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("sexplode(<string>)"));                      // OPTION
  m_wordList[command].Add(wxS("simplode"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("simplode(<list>)  "));                      // OPTION
  m_wordList[tmplte].Add(wxS("simplode(<list>, <delim>)  "));             // OPTION
  m_wordList[command].Add(wxS("sinsert"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("sinsert(<seq>, <string>, <pos>)  "));       // OPTION
  m_wordList[command].Add(wxS("sinvertcase"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("sinvertcase(<string>)  "));                 // OPTION
  m_wordList[tmplte].Add(wxS("sinvertcase(<string>, <start>)  "));        // OPTION
  m_wordList[tmplte].Add(wxS("sinvertcase(<string>, <start>, <end>)  ")); // OPTION
  m_wordList[command].Add(wxS("slength"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("slength(<string>) "));                      // OPTION
  m_wordList[command].Add(wxS("smake"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("smake(<num>, <char>) "));                   // OPTION
  m_wordList[command].Add(wxS("smismatch"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("smismatch(<string_1>, <string_2>) "));      // OPTION
  m_wordList[tmplte].Add(
			 "smismatch(<string_1>, <string_2>, <test>) ");                // OPTION
  m_wordList[command].Add(wxS("split"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("split(<string>)  "));                      // OPTION
  m_wordList[tmplte].Add(wxS("split(<string>, <delim>)  "));             // OPTION
  m_wordList[tmplte].Add(wxS("split(<string>, <delim>, <multiple>)  ")); // OPTION
  m_wordList[command].Add(wxS("sposition"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("sposition(<char>, <string>) "));           // OPTION
  m_wordList[command].Add(wxS("sremove"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("sremove(<seq>, <string>)  "));             // OPTION
  m_wordList[tmplte].Add(wxS("sremove(<seq>, <string>, <test>)  "));     // OPTION
  m_wordList[tmplte].Add(
			 "sremove(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte].Add(
			 "sremove(<seq>, <string>, <test>, <start>, <end>)  ");         // OPTION
  m_wordList[command].Add(wxS("sremovefirst"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("sremovefirst(<seq>, <string>)  "));         // OPTION
  m_wordList[tmplte].Add(wxS("sremovefirst(<seq>, <string>, <test>)  ")); // OPTION
  m_wordList[tmplte].Add(
			 "sremovefirst(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte].Add(
			 "sremovefirst(<seq>, <string>, <test>, <start>, <end>)  "); // OPTION
  m_wordList[command].Add(wxS("sreverse"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("sreverse(<string>) "));                  // OPTION
  m_wordList[command].Add(wxS("ssearch"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ssearch(<seq>, <string>)  "));           // OPTION
  m_wordList[tmplte].Add(wxS("ssearch(<seq>, <string>, <test>)  "));   // OPTION
  m_wordList[tmplte].Add(
			 "ssearch(<seq>, <string>, <test>, <start>)  "); // OPTION
  m_wordList[tmplte].Add(
			 "ssearch(<seq>, <string>, <test>, <start>, <end>)");           // OPTION
  m_wordList[command].Add(wxS("ssort"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("ssort(<string>) "));                        // OPTION
  m_wordList[tmplte].Add(wxS("ssort(<string>, <test>) "));                // OPTION
  m_wordList[command].Add(wxS("ssubst"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("ssubst(<new>, <old>, <string>) "));         // OPTION
  m_wordList[tmplte].Add(wxS("ssubst(<new>, <old>, <string>, <test>) ")); // OPTION
  m_wordList[tmplte].Add(
			 "ssubst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList[tmplte].Add(
			 "ssubst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList[command].Add(wxS("ssubstfirst"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("ssubstfirst(<new>, <old>, <string>) ")); // OPTION
  m_wordList[tmplte].Add(
			 "ssubstfirst(<new>, <old>, <string>, <test>) "); // OPTION
  m_wordList[tmplte].Add(
			 "ssubstfirst(<new>, <old>, <string>, <test>, <start>) "); // OPTION
  m_wordList[tmplte].Add(
			 "ssubstfirst(<new>, <old>, <string>, <test>, <start>, <end>) "); // OPTION
  m_wordList[command].Add(wxS("strim"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("strim(<seq>,<string>) "));               // OPTION
  m_wordList[command].Add(wxS("striml"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("striml(<seq>, <string>) "));             // OPTION
  m_wordList[command].Add(wxS("strimr"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("strimr(<seq>, <string>) "));             // OPTION
  m_wordList[command].Add(wxS("substring"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("substring(<string>, <start>)"));         // OPTION
  m_wordList[tmplte].Add(wxS("substring(<string>, <start>, <end>) ")); // OPTION
  m_wordList[command].Add(wxS("supcase"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("supcase(<string>) "));                   // OPTION
  m_wordList[tmplte].Add(wxS("supcase(<string>, <start>) "));          // OPTION
  m_wordList[tmplte].Add(wxS("supcase(<string>, <start>, <end>) "));   // OPTION
  m_wordList[command].Add(wxS("tokens"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("tokens(<string>) "));                    // OPTION
  m_wordList[tmplte].Add(wxS("tokens(<string>, <test>) "));            // OPTION
  m_wordList[command].Add(wxS("comp2pui"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("comp2pui(<n>, <L>)"));                   // OPTION
  m_wordList[command].Add(wxS("ele2pui"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("ele2pui(<m>, <L>)"));                    // OPTION
  m_wordList[command].Add(wxS("ele2comp"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("ele2comp(<m>, <L>)"));                   // OPTION
  m_wordList[command].Add(wxS("elem"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("elem(<ele>, <sym>, <lvar>)"));           // OPTION
  m_wordList[command].Add(wxS("mon2schur"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("mon2schur(<L>)"));                       // OPTION
  m_wordList[command].Add(wxS("multi_elem"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("multi_elem(<l_elem>, <multi_pc>, <l_var>)")); // OPTION
  m_wordList[command].Add(wxS("pui"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("pui(<L>, <sym>, <lvar>)"));    // OPTION
  m_wordList[command].Add(wxS("pui2comp"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("pui2comp(<n>, <lpui>)"));      // OPTION
  m_wordList[command].Add(wxS("pui2ele"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("pui2ele(<n>, <lpui>)"));       // OPTION
  m_wordList[command].Add(wxS("puireduc"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("puireduc(<n>, <lpui>)"));      // OPTION
  m_wordList[command].Add(wxS("schur2comp"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("schur2comp(<P>, <l_var>)"));   // OPTION
  m_wordList[command].Add(wxS("cont2part"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("cont2part(<pc>, <lvar>)"));    // OPTION
  m_wordList[command].Add(wxS("contract"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("contract(<psym>, <lvar>)"));   // OPTION
  m_wordList[command].Add(wxS("explose"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("explose(<pc>, <lvar>)"));      // OPTION
  m_wordList[command].Add(wxS("part2cont"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("part2cont(<ppart>, <lvar>)")); // OPTION
  m_wordList[command].Add(wxS("partpol"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("partpol(<psym>, <lvar>)"));    // OPTION
  m_wordList[command].Add(wxS("tcontract"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("tcontract(<pol>, <lvar>)"));   // OPTION
  m_wordList[command].Add(wxS("tpartpol"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("tpartpol(<pol>, <lvar>)"));    // OPTION
  m_wordList[command].Add(wxS("direct"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("direct([<p_1>, <...>, <p_n>], <y>, <f>, [<lvar_1>, "
			     "<...>, <lvar_n>])"));    // OPTION
  m_wordList[command].Add(wxS("directory"));           // FUNCTION
  m_wordList[tmplte].Add(wxS("directory(<pattern>)")); // OPTION
  m_wordList[command].Add(wxS("multi_orbit"));         // FUNCTION
  m_wordList[tmplte].Add(
			 "multi_orbit(<P>, [<lvar_1>, <lvar_2>,<...>, <lvar_p>])"); // OPTION
  m_wordList[command].Add(wxS("multsym"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("multsym(<ppart_1>, <ppart_2>, <n>)"));  // OPTION
  m_wordList[command].Add(wxS("orbit"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("orbit(<P>, <lvar>)"));                  // OPTION
  m_wordList[command].Add(wxS("pui_direct"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("pui_direct(<orbite>, [<lvar_1>, <...>, <lvar_n>], "
			     "[<d_1>, <d_2>, <...>, <d_n>])")); // OPTION
  m_wordList[command].Add(wxS("kostka"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("kostka(<part_1>, <part_2>)"));    // OPTION
  m_wordList[command].Add(wxS("lgtreillis"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("lgtreillis(<n>, <m>)"));          // OPTION
  m_wordList[command].Add(wxS("ltreillis"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("ltreillis(<n>, <m>)"));           // OPTION
  m_wordList[command].Add(wxS("treillis"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("treillis(<n>)"));                 // OPTION
  m_wordList[command].Add(wxS("treinat"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("treinat(<part>)"));               // OPTION
  m_wordList[command].Add(wxS("ele2polynome"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("ele2polynome(<L>, <z>)"));        // OPTION
  m_wordList[command].Add(wxS("polynome2ele"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("polynome2ele(<P>, <x>)"));        // OPTION
  m_wordList[command].Add(wxS("prodrac"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("prodrac(<L>, <k>)"));             // OPTION
  m_wordList[command].Add(wxS("pui2polynome"));                 // FUNCTION
  m_wordList[tmplte].Add(wxS("pui2polynome(<x>, <lpui>)"));     // OPTION
  m_wordList[command].Add(wxS("somrac"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("somrac(<L>, <k>)"));              // OPTION
  m_wordList[command].Add(wxS("resolvante"));                   // FUNCTION
  m_wordList[tmplte].Add(
			 "resolvante(<P>, <x>, <f>, [<x_1>,<...>, <x_d>]) ");       // OPTION
  m_wordList[command].Add(wxS("resolvante_alternee1"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_alternee1(<P>, <x>)"));      // OPTION
  m_wordList[command].Add(wxS("resolvante_bipartite"));               // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_bipartite(<P>, <x>)"));      // OPTION
  m_wordList[command].Add(wxS("resolvante_diedrale"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_diedrale(<P>, <x>)"));       // OPTION
  m_wordList[command].Add(wxS("resolvante_klein"));                   // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_klein(<P>, <x>)"));          // OPTION
  m_wordList[command].Add(wxS("resolvante_klein3"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_klein3(<P>, <x>)"));         // OPTION
  m_wordList[command].Add(wxS("resolvante_produit_sym"));             // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_produit_sym(<P>, <x>)"));    // OPTION
  m_wordList[command].Add(wxS("resolvante_unitaire"));                // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_unitaire(<P>, <Q>, <x>)"));  // OPTION
  m_wordList[command].Add(wxS("resolvante_vierer"));                  // FUNCTION
  m_wordList[tmplte].Add(wxS("resolvante_vierer(<P>, <x>)"));         // OPTION
  m_wordList[command].Add(wxS("multinomial"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("multinomial(<r>, <part>)"));            // OPTION
  m_wordList[command].Add(wxS("permut"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("permut(<L>)"));                         // OPTION
  m_wordList[command].Add(wxS("%piargs"));                            // OPTION
  m_wordList[command].Add(wxS("%iargs"));                             // OPTION
  m_wordList[command].Add(wxS("acos"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("acos(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("acosh"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("acosh(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("acot"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("acot(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("acoth"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("acoth(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("acsc"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("acsc(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("acsch"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("acsch(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("asec"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("asec(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("asech"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("asech(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("asin"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("asin(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("asinh"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("asinh(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("atan"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("atan(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("atan2"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("atan2(<y>, <x>)"));                     // OPTION
  m_wordList[command].Add(wxS("atanh"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("atanh(<x>)"));                          // OPTION
  m_wordList[command].Add(wxS("atrig1"));                             // OPTION
  m_wordList[command].Add(wxS("cos"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("cos(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("cosh"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("cosh(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("cot"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("cot(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("coth"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("coth(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("csc"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("csc(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("csch"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("csch(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("halfangles"));                         // OPTION
  m_wordList[command].Add(wxS("ntrig"));                              // OPTION
  m_wordList[command].Add(wxS("sec"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("sec(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("sech"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("sech(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("sin"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("sin(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("sinh"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("sinh(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("tan"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("tan(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("tanh"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("tanh(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("trigexpand"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("trigexpand(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("trigexpandplus"));                     // OPTION
  m_wordList[command].Add(wxS("trigexpandtimes"));                    // OPTION
  m_wordList[command].Add(wxS("triginverses"));                       // OPTION
  m_wordList[command].Add(wxS("trigreduce"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("trigreduce(<expr>, <x>)"));             // OPTION
  m_wordList[tmplte].Add(wxS("trigreduce(<expr>)"));                  // OPTION
  m_wordList[command].Add(wxS("trigsign"));                           // OPTION
  m_wordList[command].Add(wxS("trigsimp"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("trigsimp(<expr>)"));                    // OPTION
  m_wordList[command].Add(wxS("trigrat"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("trigrat(<expr>)"));                     // OPTION
  m_wordList[command].Add(wxS("setunits"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("setunits(<list>)"));                    // OPTION
  m_wordList[command].Add(wxS("uforget"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("uforget(<list>)"));                     // OPTION
  m_wordList[command].Add(wxS("convert"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("convert(<expr>, <list>)"));             // OPTION
  m_wordList[command].Add(wxS("usersetunits"));                       // OPTION
  m_wordList[command].Add(wxS("metricexpandall"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("metricexpandall(<x>)"));                // OPTION
  m_wordList[command].Add(wxS("%unitexpand"));                        // OPTION
  m_wordList[command].Add(wxS("AntiDifference"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("AntiDifference(<F_k>, <k>)"));          // OPTION
  m_wordList[command].Add(wxS("Gosper"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("Gosper(<F_k>, <k>)"));                  // OPTION
  m_wordList[command].Add(wxS("GosperSum"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("GosperSum(<F_k>, <k>, <a>, <b>)"));     // OPTION
  m_wordList[command].Add(wxS("parGosper"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("parGosper(<F_{n,k}>, <k>, <n>, <d>)")); // OPTION
  m_wordList[command].Add(wxS("parGosper"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("parGosper(<F_(n,k)>, <k>, <n>, <d>)")); // OPTION
  m_wordList[command].Add(wxS("Zeilberger"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("Zeilberger(<F_{n,k}>, <k>, <n>)"));     // OPTION
  m_wordList[command].Add(wxS("Zeilberger"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("Zeilberger(<F_(n,k)>, <k>, <n>)"));     // OPTION
  m_wordList[command].Add(wxS("MAX_ORD"));                            // OPTION
  m_wordList[command].Add(wxS("simplified_output"));                  // OPTION
  m_wordList[command].Add(wxS("linear_solver"));                      // OPTION
  m_wordList[command].Add(wxS("warnings"));                           // OPTION
  m_wordList[command].Add(wxS("warning"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("warning(<expr>, <...>, <expr_n>)"));    // OPTION
  m_wordList[command].Add(wxS("Gosper_in_Zeilberger"));               // OPTION
  m_wordList[command].Add(wxS("trivial_solutions"));                  // OPTION
  m_wordList[command].Add(wxS("mod_test"));                           // OPTION
  m_wordList[command].Add(wxS("modular_linear_solver"));              // OPTION
  m_wordList[command].Add(wxS("ev_point"));                           // OPTION
  m_wordList[command].Add(wxS("mod_big_prime"));                      // OPTION
  m_wordList[command].Add(wxS("mod_threshold"));                      // OPTION
  m_wordList[command].Add(wxS("days360"));                            // FUNCTION
  m_wordList[tmplte].Add(
			 "days360(<year1>,<month1>,<day1>,<year2>,<month2>,<day2>)"); // OPTION
  m_wordList[command].Add(wxS("fv"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("fv(<rate>,<PV>,<num>)"));                 // OPTION
  m_wordList[command].Add(wxS("pv"));                                   // FUNCTION
  m_wordList[tmplte].Add(wxS("pv(<rate>,<FV>,<num>)"));                 // OPTION
  m_wordList[command].Add(wxS("graph_flow"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("graph_flow(<val>)"));                     // OPTION
  m_wordList[command].Add(wxS("annuity_pv"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("annuity_pv(<rate>,<PV>,<num>)"));         // OPTION
  m_wordList[command].Add(wxS("annuity_fv"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("annuity_fv(<rate>,<FV>,<num>)"));         // OPTION
  m_wordList[command].Add(wxS("geo_annuity_pv"));                       // FUNCTION
  m_wordList[tmplte].Add(
			 "geo_annuity_pv(<rate>,<growing_rate>,<PV>,<num>)"); // OPTION
  m_wordList[command].Add(wxS("geo_annuity_fv"));               // FUNCTION
  m_wordList[tmplte].Add(
			 "geo_annuity_fv(<rate>,<growing_rate>,<FV>,<num>)");       // OPTION
  m_wordList[command].Add(wxS("amortization"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("amortization(<rate>,<amount>,<num>)")); // OPTION
  m_wordList[command].Add(wxS("arit_amortization"));                  // FUNCTION
  m_wordList[tmplte].Add(
			 "arit_amortization(<rate>,<increment>,<amount>,<num>)"); // OPTION
  m_wordList[command].Add(wxS("geo_amortization"));                 // FUNCTION
  m_wordList[tmplte].Add(
			 "geo_amortization(<rate>,<growing_rate>,<amount>,<num>)");   // OPTION
  m_wordList[command].Add(wxS("saving"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("saving(<rate>,<amount>,<num>)"));         // OPTION
  m_wordList[command].Add(wxS("npv"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("npv(<rate>,<val>)"));                     // OPTION
  m_wordList[command].Add(wxS("irr"));                                  // FUNCTION
  m_wordList[tmplte].Add(wxS("irr(<val>,<IO>)"));                       // OPTION
  m_wordList[command].Add(wxS("benefit_cost"));                         // FUNCTION
  m_wordList[tmplte].Add(wxS("benefit_cost(<rate>,<input>,<output>)")); // OPTION
  m_wordList[command].Add(wxS("sierpinskiale"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("sierpinskiale(<n>)"));                    // OPTION
  m_wordList[command].Add(wxS("treefale"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("treefale(<n>)"));                         // OPTION
  m_wordList[command].Add(wxS("fernfale"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("fernfale(<n>)"));                         // OPTION
  m_wordList[command].Add(wxS("mandelbrot_set"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("mandelbrot_set(<x>, <y>)"));              // OPTION
  m_wordList[command].Add(wxS("julia_set"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("julia_set(<x>, <y>)"));                   // OPTION
  m_wordList[command].Add(wxS("julia_parameter"));                      // OPTION
  m_wordList[command].Add(wxS("julia_sin"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("julia_sin(<x>, <y>)"));                   // OPTION
  m_wordList[command].Add(wxS("snowmap"));                              // FUNCTION
  m_wordList[tmplte].Add(wxS("snowmap(<ent>, <nn>)"));                  // OPTION
  m_wordList[command].Add(wxS("hilbertmap"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("hilbertmap(<nn>)"));                      // OPTION
  m_wordList[command].Add(wxS("sierpinskimap"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("sierpinskimap(<nn>)"));                   // OPTION
  m_wordList[command].Add(wxS("extra_integration_methods"));            // OPTION
  m_wordList[command].Add(wxS("extra_definite_integration_methods"));   // OPTION
  m_wordList[command].Add(wxS("intfugudu"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("intfugudu(<e>, <x>)"));                   // OPTION
  m_wordList[command].Add(wxS("signum_to_abs"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("signum_to_abs(<e>)"));                    // OPTION
  m_wordList[command].Add(wxS("convert_to_signum"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("convert_to_signum(<e>)"));                // OPTION
  m_wordList[command].Add(wxS("complex_number_p"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("complex_number_p(<x>)"));                 // OPTION
  m_wordList[command].Add(wxS("compose_functions"));                    // FUNCTION
  m_wordList[tmplte].Add(wxS("compose_functions(<l>)"));                // OPTION
  m_wordList[command].Add(wxS("dfloat"));                               // FUNCTION
  m_wordList[tmplte].Add(wxS("dfloat(<x>)"));                           // OPTION
  m_wordList[command].Add(wxS("elim"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("elim(<l>, <x>)"));                        // OPTION
  m_wordList[command].Add(wxS("elim_allbut"));                          // FUNCTION
  m_wordList[tmplte].Add(wxS("elim_allbut(<l>, <x>)"));                 // OPTION
  m_wordList[command].Add(wxS("eliminate_using"));                      // FUNCTION
  m_wordList[tmplte].Add(wxS("eliminate_using(<l>, <e>, <x>)"));        // OPTION
  m_wordList[command].Add(wxS("fourier_elim"));                         // FUNCTION
  m_wordList[tmplte].Add(
			 "fourier_elim([<eq1>, <eq2>, <...>], [<var1>, <var>, <...>])"); // OPTION
  m_wordList[command].Add(wxS("isreal_p"));                            // FUNCTION
  m_wordList[tmplte].Add(wxS("isreal_p(<e>)"));                        // OPTION
  m_wordList[command].Add(wxS("new_variable"));                        // FUNCTION
  m_wordList[tmplte].Add(wxS("new_variable(<type>)"));                 // OPTION
  m_wordList[command].Add(wxS("parg"));                                // FUNCTION
  m_wordList[tmplte].Add(wxS("parg(<x>)"));                            // OPTION
  m_wordList[command].Add(wxS("real_imagpart_to_conjugate"));          // FUNCTION
  m_wordList[tmplte].Add(wxS("real_imagpart_to_conjugate(<e>)"));      // OPTION
  m_wordList[command].Add(wxS("rectform_log_if_constant"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("rectform_log_if_constant(<e>)"));        // OPTION
  m_wordList[command].Add(wxS("simp_inequality"));                     // FUNCTION
  m_wordList[tmplte].Add(wxS("simp_inequality(<e>)"));                 // OPTION
  m_wordList[command].Add(wxS("standardize_inverse_trig"));            // FUNCTION
  m_wordList[tmplte].Add(wxS("standardize_inverse_trig(<e>)"));        // OPTION
  m_wordList[command].Add(wxS("to_poly"));                             // FUNCTION
  m_wordList[tmplte].Add(wxS("to_poly(<e>, <l>)"));                    // OPTION
  m_wordList[command].Add(wxS("to_poly_solve"));                       // FUNCTION
  m_wordList[tmplte].Add(wxS("to_poly_solve(<e>, <l>, <[options]>)")); // OPTION
  m_wordList[unit].Add(wxS("A"));                                      // OPTION
  m_wordList[unit].Add(wxS("acre"));                                   // OPTION
  m_wordList[unit].Add(wxS("amp"));                                    // OPTION
  m_wordList[unit].Add(wxS("ampere"));                                 // OPTION
  m_wordList[unit].Add(wxS("astronomical_unit"));                      // OPTION
  m_wordList[unit].Add(wxS("AU"));                                     // OPTION
  m_wordList[unit].Add(wxS("becquerel"));                              // OPTION
  m_wordList[unit].Add(wxS("Bq"));                                     // OPTION
  m_wordList[unit].Add(wxS("Btu"));                                    // OPTION
  m_wordList[unit].Add(wxS("C"));                                      // OPTION
  m_wordList[unit].Add(wxS("candela"));                                // OPTION
  m_wordList[unit].Add(wxS("cfm"));                                    // OPTION
  m_wordList[unit].Add(wxS("cm"));                                     // OPTION
  m_wordList[unit].Add(wxS("coulomb"));                                // OPTION
  m_wordList[unit].Add(wxS("cup"));                                    // OPTION
  m_wordList[unit].Add(wxS("day"));                                    // OPTION
  m_wordList[unit].Add(wxS("F"));                                      // OPTION
  m_wordList[unit].Add(wxS("fA"));                                     // OPTION
  m_wordList[unit].Add(wxS("farad"));                                  // OPTION
  m_wordList[unit].Add(wxS("fC"));                                     // OPTION
  m_wordList[unit].Add(wxS("feet"));                                   // OPTION
  m_wordList[unit].Add(wxS("fF"));                                     // OPTION
  m_wordList[unit].Add(wxS("fg"));                                     // OPTION
  m_wordList[unit].Add(wxS("fH"));                                     // OPTION
  m_wordList[unit].Add(wxS("fHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("fJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("fK"));                                     // OPTION
  m_wordList[unit].Add(wxS("fl_oz"));                                  // OPTION
  m_wordList[unit].Add(wxS("fluid_ounce"));                            // OPTION
  m_wordList[unit].Add(wxS("fm"));                                     // OPTION
  m_wordList[unit].Add(wxS("fmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("fN"));                                     // OPTION
  m_wordList[unit].Add(wxS("fOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("foot"));                                   // OPTION
  m_wordList[unit].Add(wxS("fPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("fs"));                                     // OPTION
  m_wordList[unit].Add(wxS("fS"));                                     // OPTION
  m_wordList[unit].Add(wxS("ft"));                                     // OPTION
  m_wordList[unit].Add(wxS("fT"));                                     // OPTION
  m_wordList[unit].Add(wxS("fV"));                                     // OPTION
  m_wordList[unit].Add(wxS("fW"));                                     // OPTION
  m_wordList[unit].Add(wxS("fWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("g"));                                      // OPTION
  m_wordList[unit].Add(wxS("GA"));                                     // OPTION
  m_wordList[unit].Add(wxS("gallon"));                                 // OPTION
  m_wordList[unit].Add(wxS("GC"));                                     // OPTION
  m_wordList[unit].Add(wxS("GF"));                                     // OPTION
  m_wordList[unit].Add(wxS("Gg"));                                     // OPTION
  m_wordList[unit].Add(wxS("GH"));                                     // OPTION
  m_wordList[unit].Add(wxS("GHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("gill"));                                   // OPTION
  m_wordList[unit].Add(wxS("GJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("GK"));                                     // OPTION
  m_wordList[unit].Add(wxS("Gm"));                                     // OPTION
  m_wordList[unit].Add(wxS("Gmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("GN"));                                     // OPTION
  m_wordList[unit].Add(wxS("GOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("GPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("grain"));                                  // OPTION
  m_wordList[unit].Add(wxS("gram"));                                   // OPTION
  m_wordList[unit].Add(wxS("gray"));                                   // OPTION
  m_wordList[unit].Add(wxS("Gs"));                                     // OPTION
  m_wordList[unit].Add(wxS("GS"));                                     // OPTION
  m_wordList[unit].Add(wxS("GT"));                                     // OPTION
  m_wordList[unit].Add(wxS("GV"));                                     // OPTION
  m_wordList[unit].Add(wxS("GW"));                                     // OPTION
  m_wordList[unit].Add(wxS("GWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("Gy"));                                     // OPTION
  m_wordList[unit].Add(wxS("H"));                                      // OPTION
  m_wordList[unit].Add(wxS("ha"));                                     // OPTION
  m_wordList[unit].Add(wxS("hectare"));                                // OPTION
  m_wordList[unit].Add(wxS("henry"));                                  // OPTION
  m_wordList[unit].Add(wxS("hertz"));                                  // OPTION
  m_wordList[unit].Add(wxS("horsepower"));                             // OPTION
  m_wordList[unit].Add(wxS("hour"));                                   // OPTION
  m_wordList[unit].Add(wxS("hp"));                                     // OPTION
  m_wordList[unit].Add(wxS("Hz"));                                     // OPTION
  m_wordList[unit].Add(wxS("inch"));                                   // OPTION
  m_wordList[unit].Add(wxS("J"));                                      // OPTION
  m_wordList[unit].Add(wxS("joule"));                                  // OPTION
  m_wordList[unit].Add(wxS("julian_year"));                            // OPTION
  m_wordList[unit].Add(wxS("K"));                                      // OPTION
  m_wordList[unit].Add(wxS("kA"));                                     // OPTION
  m_wordList[unit].Add(wxS("kat"));                                    // OPTION
  m_wordList[unit].Add(wxS("katal"));                                  // OPTION
  m_wordList[unit].Add(wxS("kC"));                                     // OPTION
  m_wordList[unit].Add(wxS("kelvin"));                                 // OPTION
  m_wordList[unit].Add(wxS("kF"));                                     // OPTION
  m_wordList[unit].Add(wxS("kg"));                                     // OPTION
  m_wordList[unit].Add(wxS("kH"));                                     // OPTION
  m_wordList[unit].Add(wxS("kHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("kilogram"));                               // OPTION
  m_wordList[unit].Add(wxS("kilometer"));                              // OPTION
  m_wordList[unit].Add(wxS("kJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("kK"));                                     // OPTION
  m_wordList[unit].Add(wxS("km"));                                     // OPTION
  m_wordList[unit].Add(wxS("kmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("kN"));                                     // OPTION
  m_wordList[unit].Add(wxS("kOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("kPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("ks"));                                     // OPTION
  m_wordList[unit].Add(wxS("kS"));                                     // OPTION
  m_wordList[unit].Add(wxS("kT"));                                     // OPTION
  m_wordList[unit].Add(wxS("kV"));                                     // OPTION
  m_wordList[unit].Add(wxS("kW"));                                     // OPTION
  m_wordList[unit].Add(wxS("kWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("l"));                                      // OPTION
  m_wordList[unit].Add(wxS("lbf"));                                    // OPTION
  m_wordList[unit].Add(wxS("lbm"));                                    // OPTION
  m_wordList[unit].Add(wxS("light_year"));                             // OPTION
  m_wordList[unit].Add(wxS("liter"));                                  // OPTION
  m_wordList[unit].Add(wxS("lumen"));                                  // OPTION
  m_wordList[unit].Add(wxS("lux"));                                    // OPTION
  m_wordList[unit].Add(wxS("m"));                                      // OPTION
  m_wordList[unit].Add(wxS("mA"));                                     // OPTION
  m_wordList[unit].Add(wxS("MA"));                                     // OPTION
  m_wordList[unit].Add(wxS("mC"));                                     // OPTION
  m_wordList[unit].Add(wxS("MC"));                                     // OPTION
  m_wordList[unit].Add(wxS("meter"));                                  // OPTION
  m_wordList[unit].Add(wxS("metric_ton"));                             // OPTION
  m_wordList[unit].Add(wxS("mF"));                                     // OPTION
  m_wordList[unit].Add(wxS("MF"));                                     // OPTION
  m_wordList[unit].Add(wxS("mg"));                                     // OPTION
  m_wordList[unit].Add(wxS("Mg"));                                     // OPTION
  m_wordList[unit].Add(wxS("mH"));                                     // OPTION
  m_wordList[unit].Add(wxS("MH"));                                     // OPTION
  m_wordList[unit].Add(wxS("mHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("MHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("microA"));                                 // OPTION
  m_wordList[unit].Add(wxS("microC"));                                 // OPTION
  m_wordList[unit].Add(wxS("microF"));                                 // OPTION
  m_wordList[unit].Add(wxS("microg"));                                 // OPTION
  m_wordList[unit].Add(wxS("microgram"));                              // OPTION
  m_wordList[unit].Add(wxS("microH"));                                 // OPTION
  m_wordList[unit].Add(wxS("microHz"));                                // OPTION
  m_wordList[unit].Add(wxS("microJ"));                                 // OPTION
  m_wordList[unit].Add(wxS("microK"));                                 // OPTION
  m_wordList[unit].Add(wxS("microm"));                                 // OPTION
  m_wordList[unit].Add(wxS("micrometer"));                             // OPTION
  m_wordList[unit].Add(wxS("micron"));                                 // OPTION
  m_wordList[unit].Add(wxS("microN"));                                 // OPTION
  m_wordList[unit].Add(wxS("microOhm"));                               // OPTION
  m_wordList[unit].Add(wxS("microPa"));                                // OPTION
  m_wordList[unit].Add(wxS("micros"));                                 // OPTION
  m_wordList[unit].Add(wxS("microS"));                                 // OPTION
  m_wordList[unit].Add(wxS("microsecond"));                            // OPTION
  m_wordList[unit].Add(wxS("microT"));                                 // OPTION
  m_wordList[unit].Add(wxS("microV"));                                 // OPTION
  m_wordList[unit].Add(wxS("microW"));                                 // OPTION
  m_wordList[unit].Add(wxS("microWb"));                                // OPTION
  m_wordList[unit].Add(wxS("mile"));                                   // OPTION
  m_wordList[unit].Add(wxS("minute"));                                 // OPTION
  m_wordList[unit].Add(wxS("mJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("MJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("mK"));                                     // OPTION
  m_wordList[unit].Add(wxS("MK"));                                     // OPTION
  m_wordList[unit].Add(wxS("ml"));                                     // OPTION
  m_wordList[unit].Add(wxS("mm"));                                     // OPTION
  m_wordList[unit].Add(wxS("Mm"));                                     // OPTION
  m_wordList[unit].Add(wxS("mmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("Mmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("mN"));                                     // OPTION
  m_wordList[unit].Add(wxS("MN"));                                     // OPTION
  m_wordList[unit].Add(wxS("mOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("MOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("mol"));                                    // OPTION
  m_wordList[unit].Add(wxS("mole"));                                   // OPTION
  m_wordList[unit].Add(wxS("month"));                                  // OPTION
  m_wordList[unit].Add(wxS("mPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("MPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("ms"));                                     // OPTION
  m_wordList[unit].Add(wxS("mS"));                                     // OPTION
  m_wordList[unit].Add(wxS("Ms"));                                     // OPTION
  m_wordList[unit].Add(wxS("MS"));                                     // OPTION
  m_wordList[unit].Add(wxS("mT"));                                     // OPTION
  m_wordList[unit].Add(wxS("MT"));                                     // OPTION
  m_wordList[unit].Add(wxS("mV"));                                     // OPTION
  m_wordList[unit].Add(wxS("MV"));                                     // OPTION
  m_wordList[unit].Add(wxS("mW"));                                     // OPTION
  m_wordList[unit].Add(wxS("MW"));                                     // OPTION
  m_wordList[unit].Add(wxS("mWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("MWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("N"));                                      // OPTION
  m_wordList[unit].Add(wxS("nA"));                                     // OPTION
  m_wordList[unit].Add(wxS("nC"));                                     // OPTION
  m_wordList[unit].Add(wxS("newton"));                                 // OPTION
  m_wordList[unit].Add(wxS("nF"));                                     // OPTION
  m_wordList[unit].Add(wxS("ng"));                                     // OPTION
  m_wordList[unit].Add(wxS("nH"));                                     // OPTION
  m_wordList[unit].Add(wxS("nHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("nJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("nK"));                                     // OPTION
  m_wordList[unit].Add(wxS("nm"));                                     // OPTION
  m_wordList[unit].Add(wxS("nmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("nN"));                                     // OPTION
  m_wordList[unit].Add(wxS("nOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("nPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("ns"));                                     // OPTION
  m_wordList[unit].Add(wxS("nS"));                                     // OPTION
  m_wordList[unit].Add(wxS("nT"));                                     // OPTION
  m_wordList[unit].Add(wxS("nV"));                                     // OPTION
  m_wordList[unit].Add(wxS("nW"));                                     // OPTION
  m_wordList[unit].Add(wxS("nWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("ohm"));                                    // OPTION
  m_wordList[unit].Add(wxS("Ohm"));                                    // OPTION
  m_wordList[unit].Add(wxS("ounce"));                                  // OPTION
  m_wordList[unit].Add(wxS("oz"));                                     // OPTION
  m_wordList[unit].Add(wxS("pA"));                                     // OPTION
  m_wordList[unit].Add(wxS("Pa"));                                     // OPTION
  m_wordList[unit].Add(wxS("parsec"));                                 // OPTION
  m_wordList[unit].Add(wxS("pascal"));                                 // OPTION
  m_wordList[unit].Add(wxS("pc"));                                     // OPTION
  m_wordList[unit].Add(wxS("pC"));                                     // OPTION
  m_wordList[unit].Add(wxS("pF"));                                     // OPTION
  m_wordList[unit].Add(wxS("pg"));                                     // OPTION
  m_wordList[unit].Add(wxS("pH"));                                     // OPTION
  m_wordList[unit].Add(wxS("pHz"));                                    // OPTION
  m_wordList[unit].Add(wxS("pint"));                                   // OPTION
  m_wordList[unit].Add(wxS("pJ"));                                     // OPTION
  m_wordList[unit].Add(wxS("pK"));                                     // OPTION
  m_wordList[unit].Add(wxS("pm"));                                     // OPTION
  m_wordList[unit].Add(wxS("pmol"));                                   // OPTION
  m_wordList[unit].Add(wxS("pN"));                                     // OPTION
  m_wordList[unit].Add(wxS("pOhm"));                                   // OPTION
  m_wordList[unit].Add(wxS("pound_force"));                            // OPTION
  m_wordList[unit].Add(wxS("pound_mass"));                             // OPTION
  m_wordList[unit].Add(wxS("pPa"));                                    // OPTION
  m_wordList[unit].Add(wxS("ps"));                                     // OPTION
  m_wordList[unit].Add(wxS("pS"));                                     // OPTION
  m_wordList[unit].Add(wxS("psi"));                                    // OPTION
  m_wordList[unit].Add(wxS("pT"));                                     // OPTION
  m_wordList[unit].Add(wxS("pV"));                                     // OPTION
  m_wordList[unit].Add(wxS("pW"));                                     // OPTION
  m_wordList[unit].Add(wxS("pWb"));                                    // OPTION
  m_wordList[unit].Add(wxS("quart"));                                  // OPTION
  m_wordList[unit].Add(wxS("R"));                                      // OPTION
  m_wordList[unit].Add(wxS("rod"));                                    // OPTION
  m_wordList[unit].Add(wxS("s"));                                      // OPTION
  m_wordList[unit].Add(wxS("S"));                                      // OPTION
  m_wordList[unit].Add(wxS("second"));                                 // OPTION
  m_wordList[unit].Add(wxS("short_ton"));                              // OPTION
  m_wordList[unit].Add(wxS("siemens"));                                // OPTION
  m_wordList[unit].Add(wxS("sievert"));                                // OPTION
  m_wordList[unit].Add(wxS("slug"));                                   // OPTION
  m_wordList[unit].Add(wxS("Sv"));                                     // OPTION
  m_wordList[unit].Add(wxS("T"));                                      // OPTION
  m_wordList[unit].Add(wxS("tablespoon"));                             // OPTION
  m_wordList[unit].Add(wxS("tbsp"));                                   // OPTION
  m_wordList[unit].Add(wxS("teaspoon"));                               // OPTION
  m_wordList[unit].Add(wxS("tesla"));                                  // OPTION
  m_wordList[unit].Add(wxS("tsp"));                                    // OPTION
  m_wordList[unit].Add(wxS("V"));                                      // OPTION
  m_wordList[unit].Add(wxS("volt"));                                   // OPTION
  m_wordList[unit].Add(wxS("W"));                                      // OPTION
  m_wordList[unit].Add(wxS("watt"));                                   // OPTION
  m_wordList[unit].Add(wxS("Wb"));                                     // OPTION
  m_wordList[unit].Add(wxS("weber"));                                  // OPTION
  m_wordList[unit].Add(wxS("week"));                                   // OPTION
  m_wordList[unit].Add(wxS("yard"));                                   // OPTION
  m_wordList[unit].Add(wxS("year"));                                   // OPTION
  m_wordList[command].Add(wxS("defstruct"));                           // FUNCTION
  m_wordList[tmplte].Add(wxS("defstruct(<struct(fields)>)"));          // OPTION
  m_wordList[command].Add(wxS("structures"));                          // OPTION
  m_wordList[command].Add(wxS("new"));                                 // FUNCTION
  m_wordList[tmplte].Add(wxS("new(<struct(fields)>)"));                // OPTION

  /// Add wxMaxima functions
  m_wordList[command].Add(wxS("wxanimate_framerate"));
  m_wordList[command].Add(wxS("wxanimate_autoplay"));
  m_wordList[command].Add(wxS("wxplot_pngcairo"));
  m_wordList[command].Add(wxS("wxplot_usesvg"));
  m_wordList[command].Add(wxS("set_display"));
  m_wordList[command].Add(wxS("wxplot2d"));
  m_wordList[tmplte].Add(wxS("wxplot2d(<expr>,<x_range>)"));
  m_wordList[command].Add(wxS("wxplot3d"));
  m_wordList[tmplte].Add(wxS("wxplot3d(<expr>,<x_range>,<y_range>)"));
  m_wordList[command].Add(wxS("wximplicit_plot"));
  m_wordList[command].Add(wxS("wxcontour_plot"));
  m_wordList[command].Add(wxS("wxanimate"));
  m_wordList[command].Add(wxS("wxanimate_draw"));
  m_wordList[command].Add(wxS("wxanimate_draw3d"));
  m_wordList[command].Add(wxS("with_slider"));
  m_wordList[tmplte].Add(wxS("with_slider(<a_var>,<a_list>,<expr>,<x_range>)"));
  m_wordList[command].Add(wxS("with_slider_draw"));
  m_wordList[command].Add(wxS("with_slider_draw2d"));
  m_wordList[command].Add(wxS("with_slider_draw3d"));
  m_wordList[command].Add(wxS("wxdirname"));
  m_wordList[command].Add(wxS("wxdraw"));
  m_wordList[command].Add(wxS("wxdraw2d"));
  m_wordList[command].Add(wxS("wxdraw3d"));
  m_wordList[command].Add(wxS("wxfilename"));
  m_wordList[command].Add(wxS("wxhistogram"));
  m_wordList[command].Add(wxS("wxscatterplot"));
  m_wordList[command].Add(wxS("wxbarsplot"));
  m_wordList[command].Add(wxS("wxpiechart"));
  m_wordList[command].Add(wxS("wxboxplot"));
  m_wordList[command].Add(wxS("wxplot_size"));
  m_wordList[command].Add(wxS("wxdraw_list"));
  m_wordList[command].Add(wxS("wxbuild_info"));
  m_wordList[command].Add(wxS("wxbug_report"));
  m_wordList[command].Add(wxS("show_image"));
  m_wordList[tmplte].Add(wxS("show_image(<imagename>)"));
  m_wordList[command].Add(wxS("table_form"));
  m_wordList[tmplte].Add(wxS("table_form(<data>)"));
  m_wordList[command].Add(wxS("wxsubscripts"));
  m_wordList[command].Add(wxS("wxdeclare_subscripted"));
  m_wordList[tmplte].Add(wxS("wxdeclare_subscripted(<name>,<[false]>)"));
  m_wordList[command].Add(wxS("wxdeclare_subscript"));
  m_wordList[tmplte].Add(wxS("wxdeclare_subscript(<subscript>,<[false]>)"));
  m_wordList[command].Add(wxS("wxanimate_from_imgfiles"));
  m_wordList[tmplte].Add(
			 wxS("wxanimate_from_imgfiles(<filename>,<[filename,...]>)"));
  m_wordList[command].Add(wxS("wxstatusbar"));
  m_wordList[tmplte].Add(wxS("wxstatusbar(<string>)"));
  m_wordList[command].Add(wxS("wxmaximaversion"));
  m_wordList[command].Add(wxS("wxwidgetsversion"));
  return true;
}
