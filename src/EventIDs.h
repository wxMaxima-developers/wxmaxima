// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file

  This file declares the class EventIDs that contains unique IDs for many events
  wxMaxima needs.

  EventIDs have proven to be a weird thing since
  - everything has an ID (-1 or wxID_ANY tells wxWidgets to pick a new ID,
  automatically)
  - Windows allows only for 16-bit-IDs
  - and since dynamic contents can consist of thousands of items and
  sub-items at a time it is highly probable that IDs need to be re-used
  frequently.

  We therefore need to be careful that wxWidgets knows what IDs are currently
  in use and that wxWidgets doesn't believe we have finished using one ID we will
  re-use lateron.
*/

#ifndef EVENTIDS_H
#define EVENTIDS_H

#include <array>
#include <wx/wx.h>
#include <wx/windowid.h>

//! The class that contains all event IDs wxMaxima needs
class EventIDs
{
public:
  EventIDs();
  //! How many IDs we have reserved for autocompletion keywords?
  static constexpr int NumberOfAutocompleteKeywords() {return 25;}
  //! How many IDs we have reserved for recent files/packages?
  static constexpr int NumberOfRecentFiles()  {return 30;}
  //! How many IDs we have reserved for suggestions of similar command names?
  static constexpr int NumberOfSuggestions()  {return 10;}
  //! How many IDs we have reserved for label width choices?
  static constexpr int NumberOfLabelWidths()  {return 10;}
  //! How many IDs have we reserved for table of contents depths
  static constexpr int NumberOfTocLevels()  {return 6;}
/*! @{
  This list serves several purposes:
  - wxwidgets uses this list to tell us what kind of events it has to inform us about.
  - we use these events for inter process communication.\n
  For example the "evaluate this cell" menu is clicked by the enter (or the shift+enter,
  depending on what option is set in the configuration).
  - Thirdly his enum is used for assigning panels an ID that matches the ID of the event
  that toggles them which makes the handling of these IDs easier.
*/


  /*! Hide all panes

    This event is assigned an ID higher than the highest ID wxWidgets assigns to
    its internal events in order to avoid ID clashes.
  */
  static const wxWindowIDRef menu_pane_hideall;
  /*! Both used as the "toggle the math pane" command and as the ID of the math pane

    Since this enum is also used for iterating over the panes it is vital
    that this entry stays that of the first pane in this enum.
  */
  static const wxWindowIDRef wizard_parametric_plot;
  static const wxWindowIDRef wizard_discrete_plot;
  static const wxWindowIDRef wizard_special_from;
  static const wxWindowIDRef wizard_special_to;
  static const wxWindowIDRef wizard_definite_id;
  static const wxWindowIDRef wizard_numeric_id;
  static const wxWindowIDRef menu_pane_math;
  static const wxWindowIDRef menu_pane_history;      //!< Both the "toggle the history pane" command and the history pane
  static const wxWindowIDRef menu_pane_structure;    //!< Both the "toggle the structure pane" command and the structure
  static const wxWindowIDRef menu_pane_xmlInspector; //!< Both the "toggle the xml monitor" command and the monitor pane
  static const wxWindowIDRef menu_pane_format;    //!< Both the "toggle the format pane" command and the format pane
  static const wxWindowIDRef menu_pane_greek;     //!< Both the "toggle the greek pane" command and the "greek" pane
  static const wxWindowIDRef menu_pane_unicode;   //!< Both the "toggle the unicode pane" command and the "unicode" pane
  static const wxWindowIDRef menu_pane_log;       //!< Both the "toggle the log pane" command and the "log" pane
  static const wxWindowIDRef menu_pane_variables; //!< Both the "toggle the variables pane" command and the "variables" pane
  static const wxWindowIDRef menu_pane_draw;      //!< Both the "toggle the draw pane" command for the "draw" pane
  static const wxWindowIDRef menu_pane_help;      //!< Both the "toggle the draw pane" command for the help browser
  static const wxWindowIDRef menu_pane_symbols;   //!< Both the "toggle the symbols pane" command for the "symbols" pane
  static const wxWindowIDRef menu_pane_wizard ;   //!< Both the "toggle the wizard pane" command for the "wizard" pane
  static const wxWindowIDRef menu_pane_toolbar ;   //!< Both the "toggle the toolbar" command for the "toolbar" pane
  static const wxWindowIDRef menu_pane_console ;   //!< The id for the worksheet
  /*! Both used as the "toggle the stats pane" command and as the ID of the stats pane

    Since this enum is also used for iterating over the panes it is vital
    that this entry stays that of the last pane in this enum.
  */
  static const wxWindowIDRef menu_pane_stats;
  static const wxWindowIDRef menu_pane_dockAll;
  static const wxWindowIDRef input_line_id;
  static const wxWindowIDRef refresh_id;
  static const wxWindowIDRef menu_batch_id;
  static const wxWindowIDRef menu_load_id;
  static const wxWindowIDRef menu_sconsole_id;
  static const wxWindowIDRef menu_interrupt_id;
  static const wxWindowIDRef menu_solve;
  static const wxWindowIDRef menu_solve_to_poly;
  static const wxWindowIDRef menu_solve_num;
  static const wxWindowIDRef menu_allroots;
  static const wxWindowIDRef menu_bfallroots;
  static const wxWindowIDRef menu_realroots;
  static const wxWindowIDRef menu_solve_lin;
  static const wxWindowIDRef menu_solve_algsys;
  static const wxWindowIDRef menu_eliminate;
  static const wxWindowIDRef menu_solve_ode;
  static const wxWindowIDRef menu_ivp_1;
  static const wxWindowIDRef menu_ivp_2;
  static const wxWindowIDRef menu_rk;
  static const wxWindowIDRef menu_bvp;
  static const wxWindowIDRef menu_genmatrix;
  static const wxWindowIDRef menu_gen_mat;
  static const wxWindowIDRef menu_gen_mat_lambda;
  static const wxWindowIDRef menu_enter_mat;
  static const wxWindowIDRef menu_csv2mat;
  static const wxWindowIDRef menu_mat2csv;
  static const wxWindowIDRef menu_csv2list;
  static const wxWindowIDRef menu_list2csv;
  static const wxWindowIDRef menu_matrix_row;
  static const wxWindowIDRef menu_matrix_col;
  static const wxWindowIDRef menu_matrix_row_list;
  static const wxWindowIDRef menu_matrix_col_list;
  static const wxWindowIDRef menu_submatrix;
  static const wxWindowIDRef menu_matrix_multiply;
  static const wxWindowIDRef menu_matrix_exponent;
  static const wxWindowIDRef menu_matrix_hadamard_product;
  static const wxWindowIDRef menu_matrix_hadamard_exponent;
  static const wxWindowIDRef menu_matrix_loadLapack;
  static const wxWindowIDRef menu_matrix_dgeev_eigenvaluesOnly;
  static const wxWindowIDRef menu_matrix_dgeev;
  static const wxWindowIDRef menu_matrix_zgeev_eigenvaluesOnly;
  static const wxWindowIDRef menu_matrix_zgeev;
  static const wxWindowIDRef menu_matrix_dgeqrf;
  static const wxWindowIDRef menu_matrix_dgesv;
  static const wxWindowIDRef menu_matrix_dgesvd;
  static const wxWindowIDRef menu_matrix_dgesvd_valuesOnly;
  static const wxWindowIDRef menu_matrix_dlange_max;
  static const wxWindowIDRef menu_matrix_dlange_one;
  static const wxWindowIDRef menu_matrix_dlange_inf;
  static const wxWindowIDRef menu_matrix_dlange_frobenius;
  static const wxWindowIDRef menu_matrix_zlange_max;
  static const wxWindowIDRef menu_matrix_zlange_one;
  static const wxWindowIDRef menu_matrix_zlange_inf;
  static const wxWindowIDRef menu_matrix_zlange_frobenius;
  static const wxWindowIDRef menu_matrix_zheev;
  static const wxWindowIDRef menu_invert_mat;
  static const wxWindowIDRef menu_cpoly;
  static const wxWindowIDRef menu_determinant;
  static const wxWindowIDRef menu_rank;
  static const wxWindowIDRef menu_eigen;
  static const wxWindowIDRef menu_eigvect;
  static const wxWindowIDRef menu_fun_def;
  static const wxWindowIDRef menu_gensym;
  static const wxWindowIDRef menu_adjoint_mat;
  static const wxWindowIDRef menu_transpose;
  static const wxWindowIDRef menu_map_mat;
  static const wxWindowIDRef menu_map;
  static const wxWindowIDRef menu_map_lambda;
  static const wxWindowIDRef menu_copymatrix;
  static const wxWindowIDRef menu_ratsimp;
  static const wxWindowIDRef menu_radsimp;
  static const wxWindowIDRef menu_scanmapfactor;
  static const wxWindowIDRef menu_factor;
  static const wxWindowIDRef menu_horner;
  static const wxWindowIDRef menu_collapse;
  static const wxWindowIDRef menu_optimize;
  static const wxWindowIDRef menu_mainvar;
  static const wxWindowIDRef menu_grind;
  static const wxWindowIDRef menu_gfactor;
  static const wxWindowIDRef menu_expand;
  static const wxWindowIDRef menu_expandwrt;
  static const wxWindowIDRef menu_expandwrt_denom;
  static const wxWindowIDRef menu_stringproc_setposition;
  static const wxWindowIDRef menu_stringproc_getposition;
  static const wxWindowIDRef menu_stringproc_flush_output;
  static const wxWindowIDRef menu_stringproc_flength;
  static const wxWindowIDRef menu_stringproc_close;
  static const wxWindowIDRef menu_stringproc_opena;
  static const wxWindowIDRef menu_stringproc_openr;
  static const wxWindowIDRef menu_stringproc_openw;
  static const wxWindowIDRef menu_stringproc_printf;
  static const wxWindowIDRef menu_stringproc_readline;
  static const wxWindowIDRef menu_stringproc_readchar;
  static const wxWindowIDRef menu_stringproc_readbyte;
  static const wxWindowIDRef menu_stringproc_writebyte;
  static const wxWindowIDRef menu_stringproc_charp;
  static const wxWindowIDRef menu_stringproc_alphacharp;
  static const wxWindowIDRef menu_stringproc_alphanumericp;
  static const wxWindowIDRef menu_stringproc_digitcharp;
  static const wxWindowIDRef menu_stringproc_constituent;
  static const wxWindowIDRef menu_stringproc_uppercasep;
  static const wxWindowIDRef menu_stringproc_lowercasep;
  static const wxWindowIDRef menu_stringproc_create_ascii;
  static const wxWindowIDRef menu_stringproc_cequal;
  static const wxWindowIDRef menu_stringproc_cequalignore;
  static const wxWindowIDRef menu_stringproc_clessp;
  static const wxWindowIDRef menu_stringproc_clesspignore;
  static const wxWindowIDRef menu_stringproc_cgreaterp;
  static const wxWindowIDRef menu_stringproc_cgreaterpignore;
  static const wxWindowIDRef menu_stringproc_sequal;
  static const wxWindowIDRef menu_stringproc_sequalignore;
  static const wxWindowIDRef menu_stringproc_ascii;
  static const wxWindowIDRef menu_stringproc_cint;
  static const wxWindowIDRef menu_stringproc_unicode;
  static const wxWindowIDRef menu_stringproc_unicode_to_utf8;
  static const wxWindowIDRef menu_stringproc_utf8_to_unicode;
  static const wxWindowIDRef menu_stringproc_charat;
  static const wxWindowIDRef menu_stringproc_charlist;
  static const wxWindowIDRef menu_stringproc_simplode;
  static const wxWindowIDRef menu_stringproc_sinsert;
  static const wxWindowIDRef menu_stringproc_eval_string;
  static const wxWindowIDRef menu_stringproc_parse_string;
  static const wxWindowIDRef menu_stringproc_scopy;
  static const wxWindowIDRef menu_stringproc_sdowncase;
  static const wxWindowIDRef menu_stringproc_slength;
  static const wxWindowIDRef menu_stringproc_smake;
  static const wxWindowIDRef menu_stringproc_smismatch;
  static const wxWindowIDRef menu_stringproc_split;
  static const wxWindowIDRef menu_stringproc_sposition;
  static const wxWindowIDRef menu_stringproc_sremove;
  static const wxWindowIDRef menu_stringproc_sremovefirst;
  static const wxWindowIDRef menu_stringproc_tokens;
  static const wxWindowIDRef menu_stringproc_ssearch;
  static const wxWindowIDRef menu_stringproc_ssort;
  static const wxWindowIDRef menu_stringproc_ssubstfirst;
  static const wxWindowIDRef menu_stringproc_strim;
  static const wxWindowIDRef menu_stringproc_striml;
  static const wxWindowIDRef menu_stringproc_strimr;
  static const wxWindowIDRef menu_stringproc_number_to_octets;
  static const wxWindowIDRef menu_stringproc_octets_to_number;
  static const wxWindowIDRef menu_stringproc_octets_to_string;
  static const wxWindowIDRef menu_stringproc_string_to_octets;
  static const wxWindowIDRef menu_sregex_load;
  static const wxWindowIDRef menu_sregex_regex_compile;
  static const wxWindowIDRef menu_sregex_regex_match_pos;
  static const wxWindowIDRef menu_sregex_regex_match;
  static const wxWindowIDRef menu_sregex_regex_split;
  static const wxWindowIDRef menu_sregex_subst_first;
  static const wxWindowIDRef menu_sregex_regex_subst;
  static const wxWindowIDRef menu_sregex_string_to_regex;
  static const wxWindowIDRef menu_opsyst_load;
  static const wxWindowIDRef menu_opsyst_chdir;
  static const wxWindowIDRef menu_opsyst_mkdir;
  static const wxWindowIDRef menu_opsyst_rmdir;
  static const wxWindowIDRef menu_opsyst_getcurrentdirectory;
  static const wxWindowIDRef menu_opsyst_copy_file;
  static const wxWindowIDRef menu_opsyst_rename_file;
  static const wxWindowIDRef menu_opsyst_delete_file;
  static const wxWindowIDRef menu_opsyst_getenv;
  static const wxWindowIDRef menu_opsyst_directory;
  static const wxWindowIDRef menu_opsyst_pathname_directory;
  static const wxWindowIDRef menu_opsyst_pathname_name;
  static const wxWindowIDRef menu_opsyst_pathname_type;
  static const wxWindowIDRef menu_scsimp;
  static const wxWindowIDRef menu_xthru;
  static const wxWindowIDRef menu_talg;
  static const wxWindowIDRef menu_tellrat;
  static const wxWindowIDRef menu_modulus;
  static const wxWindowIDRef menu_trigsimp;
  static const wxWindowIDRef menu_trigreduce;
  static const wxWindowIDRef menu_trigexpand;
  static const wxWindowIDRef menu_trigrat;
  static const wxWindowIDRef menu_rectform;
  static const wxWindowIDRef menu_polarform;
  static const wxWindowIDRef menu_demoivre;
  static const wxWindowIDRef menu_exponentialize;
  static const wxWindowIDRef menu_num_out;
  static const wxWindowIDRef menu_stringdisp;
  static const wxWindowIDRef menu_to_float;
  static const wxWindowIDRef menu_to_bfloat;
  static const wxWindowIDRef menu_to_numer;
  static const wxWindowIDRef menu_rat;
  static const wxWindowIDRef menu_rationalize;
  static const wxWindowIDRef menu_guess_exact_value;
  static const wxWindowIDRef menu_quad_qag;
  static const wxWindowIDRef menu_quad_qags;
  static const wxWindowIDRef menu_quad_qagi;
  static const wxWindowIDRef menu_quad_qawc;
  static const wxWindowIDRef menu_quad_qawf_sin;
  static const wxWindowIDRef menu_quad_qawf_cos;
  static const wxWindowIDRef menu_quad_qawo_sin;
  static const wxWindowIDRef menu_quad_qawo_cos;
  static const wxWindowIDRef menu_quad_qaws1;
  static const wxWindowIDRef menu_quad_qaws2;
  static const wxWindowIDRef menu_quad_qaws3;
  static const wxWindowIDRef menu_quad_qaws4;
  static const wxWindowIDRef menu_quad_qagp;
  static const wxWindowIDRef menu_num_domain;
  static const wxWindowIDRef menu_set_precision;
  static const wxWindowIDRef menu_set_displayprecision;
  static const wxWindowIDRef menu_engineeringFormat;
  static const wxWindowIDRef menu_engineeringFormatSetup;
  static const wxWindowIDRef menu_functions;
  static const wxWindowIDRef menu_variables;
  static const wxWindowIDRef menu_arrays;
  static const wxWindowIDRef menu_macros;
  static const wxWindowIDRef menu_labels;
  static const wxWindowIDRef menu_myoptions;
  static const wxWindowIDRef menu_rules;
  static const wxWindowIDRef menu_aliases;
  static const wxWindowIDRef menu_structs;
  static const wxWindowIDRef menu_dependencies;
  static const wxWindowIDRef menu_gradefs;
  static const wxWindowIDRef menu_let_rule_packages;
  static const wxWindowIDRef menu_wxmaxima_uses_help_sidebar;
  static const wxWindowIDRef menu_wxmaxima_uses_help_browser;
  static const wxWindowIDRef menu_maxima_uses_internal_help;
  static const wxWindowIDRef menu_maxima_uses_html_help;
  static const wxWindowIDRef menu_maxima_uses_wxmaxima_help;
  static const wxWindowIDRef menu_goto_url;
  static const wxWindowIDRef menu_clear_var;
  static const wxWindowIDRef menu_clear_fun;
  static const wxWindowIDRef menu_kill;
  static const wxWindowIDRef menu_integrate;
  static const wxWindowIDRef menu_risch;
  static const wxWindowIDRef menu_laplace;
  static const wxWindowIDRef menu_ilt;
  static const wxWindowIDRef menu_continued_fraction;
  static const wxWindowIDRef menu_gcd;
  static const wxWindowIDRef menu_lcm;
  static const wxWindowIDRef menu_divide;
  static const wxWindowIDRef menu_partfrac;
  static const wxWindowIDRef menu_sum;
  static const wxWindowIDRef menu_simpsum;
  static const wxWindowIDRef menu_limit;
  static const wxWindowIDRef menu_lbfgs;
  static const wxWindowIDRef menu_taylor;
  static const wxWindowIDRef menu_powerseries;
  static const wxWindowIDRef menu_fourier;
  static const wxWindowIDRef menu_pade;
  static const wxWindowIDRef menu_diff;
  static const wxWindowIDRef menu_solve_de;
  static const wxWindowIDRef menu_atvalue;
  static const wxWindowIDRef menu_lhs;
  static const wxWindowIDRef menu_rhs;
  static const wxWindowIDRef menu_wxmaximahelp;
  static const wxWindowIDRef menu_maximahelp;
  static const wxWindowIDRef menu_example;
  static const wxWindowIDRef menu_apropos;
  static const wxWindowIDRef menu_product;
  static const wxWindowIDRef menu_time;
  static const wxWindowIDRef menu_factsimp;
  static const wxWindowIDRef menu_factcomb;
  static const wxWindowIDRef menu_realpart;
  static const wxWindowIDRef menu_imagpart;
  static const wxWindowIDRef menu_subst;
  static const wxWindowIDRef menu_psubst;
  static const wxWindowIDRef menu_ratsubst;
  static const wxWindowIDRef menu_fullratsubst;
  static const wxWindowIDRef menu_at;
  static const wxWindowIDRef menu_substinpart;
  static const wxWindowIDRef menu_opsubst;
  static const wxWindowIDRef menu_jumptoerror;
  static const wxWindowIDRef menu_math_as_1D_ASCII;
  static const wxWindowIDRef menu_math_as_2D_ASCII;
  static const wxWindowIDRef menu_math_as_graphics;
  static const wxWindowIDRef internalRepresentation;
  static const wxWindowIDRef wxMathML;
  static const wxWindowIDRef menu_logexpand_false;
  static const wxWindowIDRef menu_logexpand_true;
  static const wxWindowIDRef menu_logexpand_all;
  static const wxWindowIDRef menu_logexpand_super;
  static const wxWindowIDRef menu_noAutosubscript;
  static const wxWindowIDRef menu_defaultAutosubscript;
  static const wxWindowIDRef menu_alwaysAutosubscript;
  static const wxWindowIDRef menu_autosubscriptIndividual;
  static const wxWindowIDRef menu_declareAutosubscript;
  static const wxWindowIDRef menu_noAutosubscriptIndividual;
  static const wxWindowIDRef menu_roundedMatrixParens;
  static const wxWindowIDRef menu_squareMatrixParens;
  static const wxWindowIDRef menu_straightMatrixParens;
  static const wxWindowIDRef menu_angledMatrixParens;
  static const wxWindowIDRef menu_noMatrixParens;
  static const wxWindowIDRef menu_draw_2d;
  static const wxWindowIDRef menu_draw_3d;
  static const wxWindowIDRef menu_draw_explicit;
  static const wxWindowIDRef menu_draw_implicit;
  static const wxWindowIDRef menu_draw_parametric;
  static const wxWindowIDRef menu_draw_points;
  static const wxWindowIDRef menu_draw_fgcolor;
  static const wxWindowIDRef menu_draw_fillcolor;
  static const wxWindowIDRef menu_draw_title;
  static const wxWindowIDRef menu_draw_key;
  static const wxWindowIDRef menu_draw_grid;
  static const wxWindowIDRef menu_draw_axis;
  static const wxWindowIDRef menu_draw_accuracy;
  static const wxWindowIDRef menu_draw_contour;
  static const wxWindowIDRef menu_license;
  static const wxWindowIDRef menu_changelog;
  static const wxWindowIDRef button_factor_id;
  static const wxWindowIDRef button_solve;
  static const wxWindowIDRef button_solve_ode;
  static const wxWindowIDRef button_limit;
  static const wxWindowIDRef button_taylor;
  static const wxWindowIDRef button_expand;
  static const wxWindowIDRef button_ratsimp;
  static const wxWindowIDRef button_radcan;
  static const wxWindowIDRef button_trigsimp;
  static const wxWindowIDRef button_trigexpand;
  static const wxWindowIDRef button_trigreduce;
  static const wxWindowIDRef button_trigrat;
  static const wxWindowIDRef button_integrate;
  static const wxWindowIDRef button_diff;
  static const wxWindowIDRef button_sum;
  static const wxWindowIDRef button_product;
  static const wxWindowIDRef button_button_constant;
  static const wxWindowIDRef button_factor;
  static const wxWindowIDRef button_subst;
  static const wxWindowIDRef button_plot2;
  static const wxWindowIDRef button_plot3;
  static const wxWindowIDRef button_rectform;
  static const wxWindowIDRef button_map;
  static const wxWindowIDRef gp_plot2;
  static const wxWindowIDRef gp_plot3;
  static const wxWindowIDRef menu_animationautostart;
  static const wxWindowIDRef menu_animationframerate;
  static const wxWindowIDRef menu_display;
  static const wxWindowIDRef menu_soft_restart;
  static const wxWindowIDRef menu_plot_format;
  static const wxWindowIDRef menu_build_info;
  static const wxWindowIDRef menu_bug_report;
  static const wxWindowIDRef menu_add_path;
  static const wxWindowIDRef menu_evaluate_all_visible;
  static const wxWindowIDRef menu_evaluate_all;
  static const wxWindowIDRef menu_show_tip;
  static const wxWindowIDRef menu_show_cellbrackets;
  static const wxWindowIDRef menu_print_cellbrackets;
  static const wxWindowIDRef menu_copy_matlab_from_worksheet;
  static const wxWindowIDRef menu_copy_tex_from_worksheet;
  static const wxWindowIDRef menu_copy_text_from_worksheet;
  static const wxWindowIDRef menu_logcontract;
  static const wxWindowIDRef menu_logexpand;
  static const wxWindowIDRef menu_to_fact;
  static const wxWindowIDRef menu_to_gamma;
  static const wxWindowIDRef menu_texform;
  static const wxWindowIDRef menu_debugmode;
  static const wxWindowIDRef menu_debugmode_off;
  static const wxWindowIDRef menu_debugmode_lisp;
  static const wxWindowIDRef menu_debugmode_all;

  // The programming menu
  static const wxWindowIDRef menu_for;
  static const wxWindowIDRef menu_while;
  static const wxWindowIDRef menu_block;
  static const wxWindowIDRef menu_block_noLocal;
  static const wxWindowIDRef menu_local;
  static const wxWindowIDRef menu_return;
  static const wxWindowIDRef menu_trace;
  static const wxWindowIDRef menu_lambda;
  static const wxWindowIDRef menu_quotequote;
  static const wxWindowIDRef menu_quote;
  static const wxWindowIDRef menu_quoteblock;
  static const wxWindowIDRef menu_def_fun;
  static const wxWindowIDRef menu_def_macro;
  static const wxWindowIDRef menu_def_variable;
  static const wxWindowIDRef menu_compile;
  static const wxWindowIDRef menu_paramType;
  static const wxWindowIDRef menu_structdef;
  static const wxWindowIDRef menu_structnew;
  static const wxWindowIDRef menu_structuse;
  static const wxWindowIDRef menu_saveLisp;
  static const wxWindowIDRef menu_loadLisp;
  static const wxWindowIDRef menu_maximatostring;
  static const wxWindowIDRef menu_stringtomaxima;

  static const wxWindowIDRef button_enter;
  static const wxWindowIDRef menu_zoom_80;
  /* Instead of menu_zoom_100 we use the standard static const int _ZOOM_100; which displays an icon in the menu (currently Unix only) */
  static const wxWindowIDRef menu_zoom_120;
  static const wxWindowIDRef menu_zoom_150;
  static const wxWindowIDRef menu_zoom_200;
  static const wxWindowIDRef menu_zoom_300;
  static const wxWindowIDRef menu_copy_as_bitmap;
  static const wxWindowIDRef menu_copy_as_svg;
  static const wxWindowIDRef menu_save_as_svg;
  static const wxWindowIDRef menu_copy_as_emf;
  static const wxWindowIDRef menu_save_as_emf;
  static const wxWindowIDRef menu_copy_as_rtf;
  static const wxWindowIDRef menu_copy_to_file;
  static const wxWindowIDRef menu_export_html;
  static const wxWindowIDRef menu_change_var;
  static const wxWindowIDRef menu_change_var_evaluate;
  static const wxWindowIDRef menu_nouns;
  static const wxWindowIDRef menu_evaluate;
  static const wxWindowIDRef menu_convert_to_code;
  static const wxWindowIDRef menu_add_comment;
  static const wxWindowIDRef menu_convert_to_comment;
  static const wxWindowIDRef menu_add_subsubsection;
  static const wxWindowIDRef menu_add_heading5;
  static const wxWindowIDRef menu_add_heading6;
  static const wxWindowIDRef menu_convert_to_subsubsection;
  static const wxWindowIDRef menu_convert_to_heading5;
  static const wxWindowIDRef menu_convert_to_heading6;
  static const wxWindowIDRef menu_add_subsection;
  static const wxWindowIDRef menu_convert_to_subsection;
  static const wxWindowIDRef menu_add_section;
  static const wxWindowIDRef menu_convert_to_section;
  static const wxWindowIDRef menu_add_title;
  static const wxWindowIDRef menu_convert_to_title;
  static const wxWindowIDRef menu_add_pagebreak;
  static const wxWindowIDRef menu_fold_all_cells;
  static const wxWindowIDRef menu_unfold_all_cells;
  static const wxWindowIDRef menu_insert_input;
  static const wxWindowIDRef menu_insert_previous_input;
  static const wxWindowIDRef menu_insert_previous_output;
  static const wxWindowIDRef menu_autocomplete;
  static const wxWindowIDRef menu_autocomplete_templates;
  static const wxWindowIDRef menu_paste_input;
  static const wxWindowIDRef menu_fullscreen;
  static const wxWindowIDRef menu_remove_output;
  static const wxWindowIDRef menu_list_create_from_elements;
  static const wxWindowIDRef menu_list_create_from_rule;
  static const wxWindowIDRef menu_list_create_from_list;
  static const wxWindowIDRef menu_list_create_from_args;
  static const wxWindowIDRef menu_list_list2matrix;
  static const wxWindowIDRef menu_list_matrix2list;
  static const wxWindowIDRef menu_list_actual_values_storage;
  static const wxWindowIDRef menu_list_sort;
  static const wxWindowIDRef menu_list_remove_duplicates;
  static const wxWindowIDRef menu_list_length;
  static const wxWindowIDRef menu_list_push;
  static const wxWindowIDRef menu_list_pop;
  static const wxWindowIDRef menu_list_reverse;
  static const wxWindowIDRef menu_list_first;
  static const wxWindowIDRef menu_list_last;
  static const wxWindowIDRef menu_list_lastn;
  static const wxWindowIDRef menu_list_rest;
  static const wxWindowIDRef menu_list_restN;
  static const wxWindowIDRef menu_list_nth;
  static const wxWindowIDRef menu_list_map;
  static const wxWindowIDRef menu_list_use_actual_values;
  static const wxWindowIDRef menu_list_extract_value;
  static const wxWindowIDRef menu_list_as_function_arguments;
  static const wxWindowIDRef menu_list_do_for_each_element;
  static const wxWindowIDRef menu_list_remove_element;
  static const wxWindowIDRef menu_list_append_item_start;
  static const wxWindowIDRef menu_list_append_item_end;
  static const wxWindowIDRef menu_list_append_list;
  static const wxWindowIDRef menu_list_interleave;
  static const wxWindowIDRef menu_recent_packages;
  static const wxWindowIDRef menu_recent_package_0;
  static const wxWindowIDRef menu_recent_documents;
  static const wxWindowIDRef menu_recent_document_0;
  static const wxWindowIDRef menu_unsaved_documents;
  static const wxWindowIDRef menu_unsaved_document_0;
  static const wxWindowIDRef menu_construct_fraction;
  static const wxWindowIDRef menu_insert_image;
  static const wxWindowIDRef menu_stats_mean;
  static const wxWindowIDRef menu_stats_median;
  static const wxWindowIDRef menu_stats_var;
  static const wxWindowIDRef menu_stats_dev;
  static const wxWindowIDRef menu_stats_tt1;
  static const wxWindowIDRef menu_stats_tt2;
  static const wxWindowIDRef menu_stats_tnorm;
  static const wxWindowIDRef menu_stats_linreg;
  static const wxWindowIDRef menu_stats_lsquares;
  static const wxWindowIDRef menu_stats_histogram;
  static const wxWindowIDRef menu_stats_scatterplot;
  static const wxWindowIDRef menu_stats_barsplot;
  static const wxWindowIDRef menu_stats_piechart;
  static const wxWindowIDRef menu_stats_boxplot;
  static const wxWindowIDRef menu_stats_readm;
  static const wxWindowIDRef menu_stats_enterm;
  static const wxWindowIDRef menu_stats_subsample;
  static const wxWindowIDRef menu_format_code;
  static const wxWindowIDRef menu_format_text;
  static const wxWindowIDRef menu_format_heading6;
  static const wxWindowIDRef menu_format_heading5;
  static const wxWindowIDRef menu_format_subsubsection;
  static const wxWindowIDRef menu_format_subsection;
  static const wxWindowIDRef menu_format_section;
  static const wxWindowIDRef menu_format_title;
  static const wxWindowIDRef menu_format_image;
  static const wxWindowIDRef menu_format_pagebreak;
  static const wxWindowIDRef menu_help_tutorials;
  static const wxWindowIDRef menu_help_tutorials_start; //! Start of bundled tutorials
  static const wxWindowIDRef menu_help_solving;
  static const wxWindowIDRef menu_help_casvsprogramming;
  static const wxWindowIDRef menu_help_diffequations;
  static const wxWindowIDRef menu_help_numberformats;
  static const wxWindowIDRef menu_help_tolerances;
  static const wxWindowIDRef menu_help_listaccess;
  static const wxWindowIDRef menu_help_memoizing;
  static const wxWindowIDRef menu_help_3d;
  static const wxWindowIDRef menu_help_varnames;
  static const wxWindowIDRef menu_help_fittingData;
  static const wxWindowIDRef menu_help_tutorials_end; //! End of bundled tutorials
  static const wxWindowIDRef menu_history_previous;
  static const wxWindowIDRef menu_history_next;
  static const wxWindowIDRef menu_check_updates;
  static const wxWindowIDRef gentran_load;
  static const wxWindowIDRef gentran_lang_c;
  static const wxWindowIDRef gentran_lang_fortran;
  static const wxWindowIDRef gentran_lang_ratfor;
  static const wxWindowIDRef gentran_to_stdout;
  static const wxWindowIDRef gentran_to_file;
  static const wxWindowIDRef socket_client_id;
  static const wxWindowIDRef socket_server_id;
  static const wxWindowIDRef gnuplot_query_terminals_id;
  static const wxWindowIDRef menu_additionalSymbols;
  static const wxWindowIDRef enable_unicodePane;
  static const wxWindowIDRef menu_showLatinGreekLookalikes;
  static const wxWindowIDRef menu_showGreekMu;
  static const wxWindowIDRef menu_invertWorksheetBackground;
  static const wxWindowIDRef popid_comment_selection;
  static const wxWindowIDRef popid_divide_cell;
  static const wxWindowIDRef popid_copy_image;
  static const wxWindowIDRef popid_copy_animation;
  static const wxWindowIDRef popid_copy_svg;
  static const wxWindowIDRef popid_copy_emf;
  static const wxWindowIDRef popid_copy_rtf;
  static const wxWindowIDRef popid_add_watch;
  static const wxWindowIDRef popid_add_watch_label;
  static const wxWindowIDRef popid_special_constant_percent;
  static const wxWindowIDRef popid_changeasterisk;
  static const wxWindowIDRef popid_hideasterisk;
  static const wxWindowIDRef popid_delete;
  static const wxWindowIDRef popid_simplify;
  static const wxWindowIDRef popid_expand;
  static const wxWindowIDRef popid_factor;
  static const wxWindowIDRef popid_solve;
  static const wxWindowIDRef popid_solve_num;
  static const wxWindowIDRef popid_integrate;
  static const wxWindowIDRef popid_diff;
  static const wxWindowIDRef popid_subst;
  static const wxWindowIDRef popid_plot2d;
  static const wxWindowIDRef popid_plot3d;
  static const wxWindowIDRef popid_float;
  static const wxWindowIDRef popid_edit;
  static const wxWindowIDRef popid_add_comment;
  static const wxWindowIDRef popid_insert_input;
  static const wxWindowIDRef popid_copy_matlab;
  static const wxWindowIDRef popid_copy_tex;
  static const wxWindowIDRef popid_copy_text;
  static const wxWindowIDRef popid_copy_mathml;
  static const wxWindowIDRef popid_labels_disable;
  static const wxWindowIDRef popid_labels_user;
  static const wxWindowIDRef popid_labels_useronly;
  static const wxWindowIDRef popid_labels_autogenerated;
  static const wxWindowIDRef popid_inputlabels_hide;
  static const wxWindowIDRef popid_labelwidth;
  static const wxWindowIDRef popid_labelwidth1;
  static const wxWindowIDRef popid_digits_20;
  static const wxWindowIDRef popid_digits_50;
  static const wxWindowIDRef popid_digits_100;
  static const wxWindowIDRef popid_digits_all;
  static const wxWindowIDRef popid_digits_all_linebreak;
  static const wxWindowIDRef popid_image;
  static const wxWindowIDRef popid_change_image;
  static const wxWindowIDRef popid_svg;
  static const wxWindowIDRef popid_emf;
  static const wxWindowIDRef popid_animation_save;
  static const wxWindowIDRef popid_animation_start;
  static const wxWindowIDRef popid_evaluate;
  static const wxWindowIDRef popid_evaluate_section;
  static const wxWindowIDRef popid_merge_cells;
  static const wxWindowIDRef popid_insert_text;
  static const wxWindowIDRef popid_insert_title;
  static const wxWindowIDRef popid_insert_section;
  static const wxWindowIDRef popid_insert_subsection;
  static const wxWindowIDRef popid_insert_subsubsection;
  static const wxWindowIDRef popid_insert_heading5;
  static const wxWindowIDRef popid_insert_heading6;
  static const wxWindowIDRef popid_auto_answer;
  static const wxWindowIDRef popid_autocomplete_keyword1;
  static const wxWindowIDRef popid_never_autoanswer;
  static const wxWindowIDRef popid_popup_gnuplot;
  static const wxWindowIDRef popid_fold;
  static const wxWindowIDRef popid_unfold;
  static const wxWindowIDRef popid_maxsizechooser;
  static const wxWindowIDRef popid_resolutionchooser;
  static const wxWindowIDRef popid_reloadimage;
  static const wxWindowIDRef popid_suggestion1;
  static const wxWindowIDRef popid_suggestion2;
  static const wxWindowIDRef popid_suggestion3;
  static const wxWindowIDRef popid_suggestion4;
  static const wxWindowIDRef popid_suggestion5;
  static const wxWindowIDRef popid_suggestion6;
  static const wxWindowIDRef popid_suggestion7;
  static const wxWindowIDRef popid_suggestion8;
  static const wxWindowIDRef popid_suggestion9;
  static const wxWindowIDRef popid_suggestion10;
  static const wxWindowIDRef popid_hide_tooltipMarker;
  static const wxWindowIDRef popid_hide_tooltipMarkerForThisMessage;
  static const wxWindowIDRef popid_property_real;
  static const wxWindowIDRef popid_property_imaginary;
  static const wxWindowIDRef popid_property_complex;
  static const wxWindowIDRef popid_property_additive;
  static const wxWindowIDRef popid_property_alphabetic;
  static const wxWindowIDRef popid_property_bindtest;
  static const wxWindowIDRef popid_property_antisymmetric;
  static const wxWindowIDRef popid_property_commutative;
  static const wxWindowIDRef popid_property_symmetric;
  static const wxWindowIDRef popid_property_constant;
  static const wxWindowIDRef popid_property_even;
  static const wxWindowIDRef popid_property_odd;
  static const wxWindowIDRef popid_property_evenfun;
  static const wxWindowIDRef popid_property_oddfun;
  static const wxWindowIDRef popid_property_increasing;
  static const wxWindowIDRef popid_property_decreasing;
  static const wxWindowIDRef popid_property_integer;
  static const wxWindowIDRef popid_property_noninteger;
  static const wxWindowIDRef popid_property_integervalued;
  static const wxWindowIDRef popid_property_lassociative;
  static const wxWindowIDRef popid_property_rassociative;
  static const wxWindowIDRef popid_property_linear;
  static const wxWindowIDRef popid_property_mainvar;
  static const wxWindowIDRef popid_property_multiplicative;
  static const wxWindowIDRef popid_property_nary;
  static const wxWindowIDRef popid_property_nonarray;
  static const wxWindowIDRef popid_property_nonscalar;
  static const wxWindowIDRef popid_property_scalar;
  static const wxWindowIDRef popid_property_noun;
  static const wxWindowIDRef popid_property_outative;
  static const wxWindowIDRef popid_property_posfun;
  static const wxWindowIDRef popid_property_rational;
  static const wxWindowIDRef popid_property_irrational;
  static const wxWindowIDRef popid_property_greaterThan;
  static const wxWindowIDRef popid_property_evfun;
  static const wxWindowIDRef popid_property_evflag;
  static const wxWindowIDRef popid_property_atvalue;
  static const wxWindowIDRef popid_Fold;
  static const wxWindowIDRef popid_Unfold;
  static const wxWindowIDRef popid_SelectTocChapter;
  static const wxWindowIDRef popid_EvalTocChapter;
  static const wxWindowIDRef popid_ToggleTOCshowsSectionNumbers;
  static const wxWindowIDRef popid_tocLevel1;
  static const wxWindowIDRef popid_tocdnd;
  static const wxWindowIDRef popid_tocMoveIn;
  static const wxWindowIDRef popid_tocMoveOut;
  static const wxWindowIDRef popid_var_newVar;
  static const wxWindowIDRef popid_var_addAll;
  static const wxWindowIDRef popid_addToSymbols;

private:
/*! @}
 */
};

#endif // EVENTIDS_H
