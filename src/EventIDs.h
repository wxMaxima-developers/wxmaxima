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
  
  This file defines the class EventIDs that contains unique IDs for all events 
  wxMaxima needs
*/

#ifndef EVENTIDS_H
#define EVENTIDS_H

#include <array>

//! The class that contains all event IDs wxMaxima needs
class EventIDs
{
public:
  EventIDs();
  using EventId = int;
  static constexpr int NumberOfAutocompleteKeywords() {return 25;}
  static constexpr int NumberOfRecentFiles()  {return 30;}
  static constexpr int NumberOfSuggestions()  {return 10;}
  static constexpr int NumberOfLabelWidths()  {return 10;}
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
  static const EventId menu_pane_hideall;
  /*! Both used as the "toggle the math pane" command and as the ID of the math pane

    Since this enum is also used for iterating over the panes it is vital 
    that this entry stays that of the first pane in this enum.
  */
  static const EventId menu_pane_math;
  static const EventId menu_pane_history;      //!< Both the "toggle the history pane" command and the history pane
  static const EventId menu_pane_structure;    //!< Both the "toggle the structure pane" command and the structure
  static const EventId menu_pane_xmlInspector; //!< Both the "toggle the xml monitor" command and the monitor pane
  static const EventId menu_pane_format;    //!< Both the "toggle the format pane" command and the format pane
  static const EventId menu_pane_greek;     //!< Both the "toggle the greek pane" command and the "greek" pane
  static const EventId menu_pane_unicode;   //!< Both the "toggle the unicode pane" command and the "unicode" pane
  static const EventId menu_pane_log;       //!< Both the "toggle the log pane" command and the "log" pane
  static const EventId menu_pane_variables; //!< Both the "toggle the variables pane" command and the "variables" pane
  static const EventId menu_pane_draw;      //!< Both the "toggle the draw pane" command for the "draw" pane
  static const EventId menu_pane_help;      //!< Both the "toggle the draw pane" command for the help browser
  static const EventId menu_pane_symbols;   //!< Both the "toggle the symbols pane" command for the "symbols" pane
  static const EventId menu_pane_wizard ;   //!< Both the "toggle the wizard pane" command for the "wizard" pane
  static const EventId menu_pane_toolbar ;   //!< Both the "toggle the toolbar" command for the "toolbar" pane
  static const EventId menu_pane_console ;   //!< The id for the worksheet
  /*! Both used as the "toggle the stats pane" command and as the ID of the stats pane

    Since this enum is also used for iterating over the panes it is vital 
    that this entry stays that of the last pane in this enum.
  */
  static const EventId menu_pane_stats;
  static const EventId menu_pane_dockAll;
  static const EventId input_line_id;
  static const EventId refresh_id;
  static const EventId menu_batch_id;
  static const EventId menu_load_id;
  static const EventId menu_sconsole_id;
  static const EventId menu_interrupt_id;
  static const EventId menu_solve;
  static const EventId menu_solve_to_poly;
  static const EventId menu_solve_num;
  static const EventId menu_allroots;
  static const EventId menu_bfallroots;
  static const EventId menu_realroots;
  static const EventId menu_solve_lin;
  static const EventId menu_solve_algsys;
  static const EventId menu_eliminate;
  static const EventId menu_solve_ode;
  static const EventId menu_ivp_1;
  static const EventId menu_ivp_2;
  static const EventId menu_rk;
  static const EventId menu_bvp;
  static const EventId menu_genmatrix;
  static const EventId menu_gen_mat;
  static const EventId menu_gen_mat_lambda;
  static const EventId menu_enter_mat;
  static const EventId menu_csv2mat;
  static const EventId menu_mat2csv;
  static const EventId menu_csv2list;
  static const EventId menu_list2csv;
  static const EventId menu_matrix_row;
  static const EventId menu_matrix_col;
  static const EventId menu_matrix_row_list;
  static const EventId menu_matrix_col_list;
  static const EventId menu_submatrix;
  static const EventId menu_matrix_multiply;
  static const EventId menu_matrix_exponent;
  static const EventId menu_matrix_hadamard_product;
  static const EventId menu_matrix_hadamard_exponent;
  static const EventId menu_matrix_loadLapack;
  static const EventId menu_matrix_dgeev_eigenvaluesOnly;
  static const EventId menu_matrix_dgeev;
  static const EventId menu_matrix_zgeev_eigenvaluesOnly;
  static const EventId menu_matrix_zgeev;
  static const EventId menu_matrix_dgeqrf;
  static const EventId menu_matrix_dgesv;
  static const EventId menu_matrix_dgesvd;
  static const EventId menu_matrix_dgesvd_valuesOnly;
  static const EventId menu_matrix_dlange_max;
  static const EventId menu_matrix_dlange_one;
  static const EventId menu_matrix_dlange_inf;
  static const EventId menu_matrix_dlange_frobenius;
  static const EventId menu_matrix_zlange_max;
  static const EventId menu_matrix_zlange_one;
  static const EventId menu_matrix_zlange_inf;
  static const EventId menu_matrix_zlange_frobenius;
  static const EventId menu_matrix_zheev;
  static const EventId menu_invert_mat;
  static const EventId menu_cpoly;
  static const EventId menu_determinant;
  static const EventId menu_rank;
  static const EventId menu_eigen;
  static const EventId menu_eigvect;
  static const EventId menu_fun_def;
  static const EventId menu_gensym;
  static const EventId menu_adjoint_mat;
  static const EventId menu_transpose;
  static const EventId menu_map_mat;
  static const EventId menu_map;
  static const EventId menu_map_lambda;
  static const EventId menu_copymatrix;
  static const EventId menu_ratsimp;
  static const EventId menu_radsimp;
  static const EventId menu_scanmapfactor;
  static const EventId menu_factor;
  static const EventId menu_horner;
  static const EventId menu_collapse;
  static const EventId menu_optimize;
  static const EventId menu_mainvar;
  static const EventId menu_grind;
  static const EventId menu_gfactor;
  static const EventId menu_expand;
  static const EventId menu_expandwrt;
  static const EventId menu_expandwrt_denom;
  static const EventId menu_stringproc_setposition;
  static const EventId menu_stringproc_getposition;
  static const EventId menu_stringproc_flush_output;
  static const EventId menu_stringproc_flength;
  static const EventId menu_stringproc_close;
  static const EventId menu_stringproc_opena;
  static const EventId menu_stringproc_openr;
  static const EventId menu_stringproc_openw;
  static const EventId menu_stringproc_printf;
  static const EventId menu_stringproc_readline;
  static const EventId menu_stringproc_readchar;
  static const EventId menu_stringproc_readbyte;
  static const EventId menu_stringproc_writebyte;
  static const EventId menu_stringproc_charp;
  static const EventId menu_stringproc_alphacharp;
  static const EventId menu_stringproc_alphanumericp;
  static const EventId menu_stringproc_digitcharp;
  static const EventId menu_stringproc_constituent;
  static const EventId menu_stringproc_uppercasep;
  static const EventId menu_stringproc_lowercasep;
  static const EventId menu_stringproc_create_ascii;
  static const EventId menu_stringproc_cequal;
  static const EventId menu_stringproc_cequalignore;
  static const EventId menu_stringproc_clessp;
  static const EventId menu_stringproc_clesspignore;
  static const EventId menu_stringproc_cgreaterp;
  static const EventId menu_stringproc_cgreaterpignore;
  static const EventId menu_stringproc_sequal;
  static const EventId menu_stringproc_sequalignore;
  static const EventId menu_stringproc_ascii;
  static const EventId menu_stringproc_cint;
  static const EventId menu_stringproc_unicode;
  static const EventId menu_stringproc_unicode_to_utf8;
  static const EventId menu_stringproc_utf8_to_unicode;
  static const EventId menu_stringproc_charat;
  static const EventId menu_stringproc_charlist;
  static const EventId menu_stringproc_simplode;
  static const EventId menu_stringproc_sinsert;
  static const EventId menu_stringproc_eval_string;
  static const EventId menu_stringproc_parse_string;
  static const EventId menu_stringproc_scopy;
  static const EventId menu_stringproc_sdowncase;
  static const EventId menu_stringproc_slength;
  static const EventId menu_stringproc_smake;
  static const EventId menu_stringproc_smismatch;
  static const EventId menu_stringproc_split;
  static const EventId menu_stringproc_sposition;
  static const EventId menu_stringproc_sremove;
  static const EventId menu_stringproc_sremovefirst;
  static const EventId menu_stringproc_tokens;
  static const EventId menu_stringproc_ssearch;
  static const EventId menu_stringproc_ssort;
  static const EventId menu_stringproc_ssubstfirst;
  static const EventId menu_stringproc_strim;
  static const EventId menu_stringproc_striml;
  static const EventId menu_stringproc_strimr;
  static const EventId menu_stringproc_number_to_octets;
  static const EventId menu_stringproc_octets_to_number;
  static const EventId menu_stringproc_octets_to_string;
  static const EventId menu_stringproc_string_to_octets;
  static const EventId menu_sregex_load;
  static const EventId menu_sregex_regex_compile;
  static const EventId menu_sregex_regex_match_pos;
  static const EventId menu_sregex_regex_match;
  static const EventId menu_sregex_regex_split;
  static const EventId menu_sregex_subst_first;
  static const EventId menu_sregex_regex_subst;
  static const EventId menu_sregex_string_to_regex;
  static const EventId menu_opsyst_load;
  static const EventId menu_opsyst_chdir;
  static const EventId menu_opsyst_mkdir;
  static const EventId menu_opsyst_rmdir;
  static const EventId menu_opsyst_getcurrentdirectory;
  static const EventId menu_opsyst_copy_file;
  static const EventId menu_opsyst_rename_file;
  static const EventId menu_opsyst_delete_file;
  static const EventId menu_opsyst_getenv;
  static const EventId menu_opsyst_directory;
  static const EventId menu_opsyst_pathname_directory;
  static const EventId menu_opsyst_pathname_name;
  static const EventId menu_opsyst_pathname_type;
  static const EventId menu_scsimp;
  static const EventId menu_xthru;
  static const EventId menu_talg;
  static const EventId menu_tellrat;
  static const EventId menu_modulus;
  static const EventId menu_trigsimp;
  static const EventId menu_trigreduce;
  static const EventId menu_trigexpand;
  static const EventId menu_trigrat;
  static const EventId menu_rectform;
  static const EventId menu_polarform;
  static const EventId menu_demoivre;
  static const EventId menu_exponentialize;
  static const EventId menu_num_out;
  static const EventId menu_stringdisp;
  static const EventId menu_to_float;
  static const EventId menu_to_bfloat;
  static const EventId menu_to_numer;
  static const EventId menu_rat;
  static const EventId menu_rationalize;
  static const EventId menu_guess_exact_value;
  static const EventId menu_quad_qag;
  static const EventId menu_quad_qags;
  static const EventId menu_quad_qagi;
  static const EventId menu_quad_qawc;
  static const EventId menu_quad_qawf_sin;
  static const EventId menu_quad_qawf_cos;
  static const EventId menu_quad_qawo_sin;
  static const EventId menu_quad_qawo_cos;
  static const EventId menu_quad_qaws1;
  static const EventId menu_quad_qaws2;
  static const EventId menu_quad_qaws3;
  static const EventId menu_quad_qaws4;
  static const EventId menu_quad_qagp;
  static const EventId menu_num_domain;
  static const EventId menu_set_precision;
  static const EventId menu_set_displayprecision;
  static const EventId menu_engineeringFormat;
  static const EventId menu_engineeringFormatSetup;
  static const EventId menu_functions;
  static const EventId menu_variables;
  static const EventId menu_arrays;
  static const EventId menu_macros;
  static const EventId menu_labels;
  static const EventId menu_myoptions;
  static const EventId menu_rules;
  static const EventId menu_aliases;
  static const EventId menu_structs;
  static const EventId menu_dependencies;
  static const EventId menu_gradefs;
  static const EventId menu_let_rule_packages;
  static const EventId menu_maxima_uses_internal_help;
  static const EventId menu_maxima_uses_html_help;
  static const EventId menu_maxima_uses_wxmaxima_help;
  static const EventId menu_goto_url;
  static const EventId menu_clear_var;
  static const EventId menu_clear_fun;
  static const EventId menu_kill;
  static const EventId menu_integrate;
  static const EventId menu_risch;
  static const EventId menu_laplace;
  static const EventId menu_ilt;
  static const EventId menu_continued_fraction;
  static const EventId menu_gcd;
  static const EventId menu_lcm;
  static const EventId menu_divide;
  static const EventId menu_partfrac;
  static const EventId menu_sum;
  static const EventId menu_simpsum;
  static const EventId menu_limit;
  static const EventId menu_lbfgs;
  static const EventId menu_taylor;
  static const EventId menu_powerseries;
  static const EventId menu_fourier;
  static const EventId menu_pade;
  static const EventId menu_diff;
  static const EventId menu_solve_de;
  static const EventId menu_atvalue;
  static const EventId menu_lhs;
  static const EventId menu_rhs;
  static const EventId menu_wxmaximahelp;
  static const EventId menu_maximahelp;
  static const EventId menu_example;
  static const EventId menu_apropos;
  static const EventId menu_product;
  static const EventId menu_time;
  static const EventId menu_factsimp;
  static const EventId menu_factcomb;
  static const EventId menu_realpart;
  static const EventId menu_imagpart;
  static const EventId menu_subst;
  static const EventId menu_psubst;
  static const EventId menu_ratsubst;
  static const EventId menu_fullratsubst;
  static const EventId menu_at;
  static const EventId menu_substinpart;
  static const EventId menu_opsubst;
  static const EventId menu_jumptoerror;
  static const EventId menu_math_as_1D_ASCII;
  static const EventId menu_math_as_2D_ASCII;
  static const EventId menu_math_as_graphics;
  static const EventId menu_logexpand_false;
  static const EventId menu_logexpand_true;
  static const EventId menu_logexpand_all;
  static const EventId menu_logexpand_super;
  static const EventId menu_noAutosubscript;
  static const EventId menu_defaultAutosubscript;
  static const EventId menu_alwaysAutosubscript;
  static const EventId menu_autosubscriptIndividual;
  static const EventId menu_declareAutosubscript;
  static const EventId menu_noAutosubscriptIndividual;
  static const EventId menu_roundedMatrixParens;
  static const EventId menu_squareMatrixParens;
  static const EventId menu_straightMatrixParens;
  static const EventId menu_angledMatrixParens;
  static const EventId menu_noMatrixParens;
  static const EventId menu_draw_2d;
  static const EventId menu_draw_3d;
  static const EventId menu_draw_explicit;
  static const EventId menu_draw_implicit;
  static const EventId menu_draw_parametric;
  static const EventId menu_draw_points;
  static const EventId menu_draw_fgcolor;
  static const EventId menu_draw_fillcolor;
  static const EventId menu_draw_title;
  static const EventId menu_draw_key;
  static const EventId menu_draw_grid;
  static const EventId menu_draw_axis;
  static const EventId menu_draw_accuracy;
  static const EventId menu_draw_contour;
  static const EventId menu_license;
  static const EventId menu_changelog;
  static const EventId button_factor_id;
  static const EventId button_solve;
  static const EventId button_solve_ode;
  static const EventId button_limit;
  static const EventId button_taylor;
  static const EventId button_expand;
  static const EventId button_ratsimp;
  static const EventId button_radcan;
  static const EventId button_trigsimp;
  static const EventId button_trigexpand;
  static const EventId button_trigreduce;
  static const EventId button_trigrat;
  static const EventId button_integrate;
  static const EventId button_diff;
  static const EventId button_sum;
  static const EventId button_product;
  static const EventId button_button_constant;
  static const EventId button_factor;
  static const EventId button_subst;
  static const EventId button_plot2;
  static const EventId button_plot3;
  static const EventId button_rectform;
  static const EventId button_map;
  static const EventId gp_plot2;
  static const EventId gp_plot3;
  static const EventId menu_animationautostart;
  static const EventId menu_animationframerate;
  static const EventId menu_display;
  static const EventId menu_soft_restart;
  static const EventId menu_plot_format;
  static const EventId menu_build_info;
  static const EventId menu_bug_report;
  static const EventId menu_add_path;
  static const EventId menu_evaluate_all_visible;
  static const EventId menu_evaluate_all;
  static const EventId menu_show_tip;
  static const EventId menu_copy_matlab_from_worksheet;
  static const EventId menu_copy_tex_from_worksheet;
  static const EventId menu_copy_text_from_worksheet;
  static const EventId menu_logcontract;
  static const EventId menu_logexpand;
  static const EventId menu_to_fact;
  static const EventId menu_to_gamma;
  static const EventId menu_texform;
  static const EventId menu_debugmode;
  static const EventId menu_debugmode_off;
  static const EventId menu_debugmode_lisp;
  static const EventId menu_debugmode_all;

  // The programming menu
  static const EventId menu_for;
  static const EventId menu_while;
  static const EventId menu_block;
  static const EventId menu_block_noLocal;
  static const EventId menu_local;
  static const EventId menu_return;
  static const EventId menu_trace;
  static const EventId menu_lambda;
  static const EventId menu_quotequote;
  static const EventId menu_quote;
  static const EventId menu_quoteblock;
  static const EventId menu_def_fun;
  static const EventId menu_def_macro;
  static const EventId menu_def_variable;
  static const EventId menu_compile;
  static const EventId menu_paramType;
  static const EventId menu_structdef;
  static const EventId menu_structnew;
  static const EventId menu_structuse;
  static const EventId menu_saveLisp;
  static const EventId menu_loadLisp;
  static const EventId menu_maximatostring;
  static const EventId menu_stringtomaxima;
    
  static const EventId button_enter;
  static const EventId menu_zoom_80;
  /* Instead of menu_zoom_100 we use the standard static const int _ZOOM_100; which displays an icon in the menu (currently Unix only) */
  static const EventId menu_zoom_120;
  static const EventId menu_zoom_150;
  static const EventId menu_zoom_200;
  static const EventId menu_zoom_300;
  static const EventId menu_copy_as_bitmap;
  static const EventId menu_copy_as_svg;
  static const EventId menu_save_as_svg;
  static const EventId menu_copy_as_emf;
  static const EventId menu_save_as_emf;
  static const EventId menu_copy_as_rtf;
  static const EventId menu_copy_to_file;
  static const EventId menu_export_html;
  static const EventId menu_change_var;
  static const EventId menu_change_var_evaluate;
  static const EventId menu_nouns;
  static const EventId menu_evaluate;
  static const EventId menu_convert_to_code;
  static const EventId menu_add_comment;
  static const EventId menu_convert_to_comment;
  static const EventId menu_add_subsubsection;
  static const EventId menu_add_heading5;
  static const EventId menu_add_heading6;
  static const EventId menu_convert_to_subsubsection;
  static const EventId menu_convert_to_heading5;
  static const EventId menu_convert_to_heading6;
  static const EventId menu_add_subsection;
  static const EventId menu_convert_to_subsection;
  static const EventId menu_add_section;
  static const EventId menu_convert_to_section;
  static const EventId menu_add_title;
  static const EventId menu_convert_to_title;
  static const EventId menu_add_pagebreak;
  static const EventId menu_fold_all_cells;
  static const EventId menu_unfold_all_cells;
  static const EventId menu_insert_input;
  static const EventId menu_insert_previous_input;
  static const EventId menu_insert_previous_output;
  static const EventId menu_autocomplete;
  static const EventId menu_autocomplete_templates;
  static const EventId menu_paste_input;
  static const EventId menu_fullscreen;
  static const EventId menu_remove_output;
  static const EventId menu_list_create_from_elements;
  static const EventId menu_list_create_from_rule;
  static const EventId menu_list_create_from_list;
  static const EventId menu_list_create_from_args;
  static const EventId menu_list_list2matrix;
  static const EventId menu_list_matrix2list;
  static const EventId menu_list_actual_values_storage;
  static const EventId menu_list_sort;
  static const EventId menu_list_remove_duplicates;
  static const EventId menu_list_length;
  static const EventId menu_list_push;
  static const EventId menu_list_pop;
  static const EventId menu_list_reverse;
  static const EventId menu_list_first;
  static const EventId menu_list_last;
  static const EventId menu_list_lastn;
  static const EventId menu_list_rest;
  static const EventId menu_list_restN;
  static const EventId menu_list_nth;
  static const EventId menu_list_map;
  static const EventId menu_list_use_actual_values;
  static const EventId menu_list_extract_value;
  static const EventId menu_list_as_function_arguments;
  static const EventId menu_list_do_for_each_element;
  static const EventId menu_list_remove_element;
  static const EventId menu_list_append_item_start;
  static const EventId menu_list_append_item_end;
  static const EventId menu_list_append_list;
  static const EventId menu_list_interleave;
  static const EventId menu_recent_packages;
  static const EventId menu_recent_package_0;
  static const EventId menu_recent_package_1;
  static const EventId menu_recent_package_2;
  static const EventId menu_recent_package_3;
  static const EventId menu_recent_package_4;
  static const EventId menu_recent_package_5;
  static const EventId menu_recent_package_6;
  static const EventId menu_recent_package_7;
  static const EventId menu_recent_package_8;
  static const EventId menu_recent_package_9;
  static const EventId menu_recent_package_10;
  static const EventId menu_recent_package_11;
  static const EventId menu_recent_package_12;
  static const EventId menu_recent_package_13;
  static const EventId menu_recent_package_14;
  static const EventId menu_recent_package_15;
  static const EventId menu_recent_package_16;
  static const EventId menu_recent_package_17;
  static const EventId menu_recent_package_18;
  static const EventId menu_recent_package_19;
  static const EventId menu_recent_package_20;
  static const EventId menu_recent_package_21;
  static const EventId menu_recent_package_22;
  static const EventId menu_recent_package_23;
  static const EventId menu_recent_package_24;
  static const EventId menu_recent_package_25;
  static const EventId menu_recent_package_26;
  static const EventId menu_recent_package_27;
  static const EventId menu_recent_package_28;
  static const EventId menu_recent_package_29;
  static const EventId menu_recent_documents;
  static const EventId menu_recent_document_0;
  static const EventId menu_recent_document_separator;
  static const EventId menu_unsaved_documents;
  static const EventId menu_unsaved_document_0;
  static const EventId menu_construct_fraction;
  static const EventId menu_insert_image;
  static const EventId menu_stats_mean;
  static const EventId menu_stats_median;
  static const EventId menu_stats_var;
  static const EventId menu_stats_dev;
  static const EventId menu_stats_tt1;
  static const EventId menu_stats_tt2;
  static const EventId menu_stats_tnorm;
  static const EventId menu_stats_linreg;
  static const EventId menu_stats_lsquares;
  static const EventId menu_stats_histogram;
  static const EventId menu_stats_scatterplot;
  static const EventId menu_stats_barsplot;
  static const EventId menu_stats_piechart;
  static const EventId menu_stats_boxplot;
  static const EventId menu_stats_readm;
  static const EventId menu_stats_enterm;
  static const EventId menu_stats_subsample;
  static const EventId menu_format_code;
  static const EventId menu_format_text;
  static const EventId menu_format_heading6;
  static const EventId menu_format_heading5;
  static const EventId menu_format_subsubsection;
  static const EventId menu_format_subsection;
  static const EventId menu_format_section;
  static const EventId menu_format_title;
  static const EventId menu_format_image;
  static const EventId menu_format_pagebreak;
  static const EventId menu_help_tutorials;
  static const EventId menu_help_tutorials_start; //! Start of bundled tutorials
  static const EventId menu_help_solving;
  static const EventId menu_help_diffequations;
  static const EventId menu_help_numberformats;
  static const EventId menu_help_tolerances;
  static const EventId menu_help_listaccess;
  static const EventId menu_help_memoizing;
  static const EventId menu_help_3d;
  static const EventId menu_help_varnames;
  static const EventId menu_help_fittingData;
  static const EventId menu_help_tutorials_end; //! End of bundled tutorials
  static const EventId menu_history_previous;
  static const EventId menu_history_next;
  static const EventId menu_check_updates;
  static const EventId gentran_load;
  static const EventId gentran_lang_c;
  static const EventId gentran_lang_fortran;
  static const EventId gentran_lang_ratfor;
  static const EventId gentran_to_stdout;
  static const EventId gentran_to_file;
  static const EventId socket_client_id;
  static const EventId socket_server_id;
  static const EventId gnuplot_query_terminals_id;
  static const EventId menu_additionalSymbols;
  static const EventId enable_unicodePane;
  static const EventId menu_showLatinGreekLookalikes;
  static const EventId menu_showGreekMu;
  static const EventId menu_invertWorksheetBackground;
  static const EventId popid_comment_selection;
  static const EventId popid_divide_cell;
  static const EventId popid_copy_image;
  static const EventId popid_copy_animation;
  static const EventId popid_copy_svg;
  static const EventId popid_copy_emf;
  static const EventId popid_copy_rtf;
  static const EventId popid_add_watch;
  static const EventId popid_add_watch_label;
  static const EventId popid_special_constant_percent;
  static const EventId popid_changeasterisk;
  static const EventId popid_hideasterisk;
  static const EventId popid_delete;
  static const EventId popid_simplify;
  static const EventId popid_expand;
  static const EventId popid_factor;
  static const EventId popid_solve;
  static const EventId popid_solve_num;
  static const EventId popid_integrate;
  static const EventId popid_diff;
  static const EventId popid_subst;
  static const EventId popid_plot2d;
  static const EventId popid_plot3d;
  static const EventId popid_float;
  static const EventId popid_edit;
  static const EventId popid_add_comment;
  static const EventId popid_insert_input;
  static const EventId popid_copy_matlab;
  static const EventId popid_copy_tex;
  static const EventId popid_copy_text;
  static const EventId popid_copy_mathml;
  static const EventId popid_labels_disable;
  static const EventId popid_labels_user;
  static const EventId popid_labels_useronly;
  static const EventId popid_labels_autogenerated;
  static const EventId popid_labelwidth;
  static const EventId popid_labelwidth1;
  static const EventId popid_digits_20;
  static const EventId popid_digits_50;
  static const EventId popid_digits_100;
  static const EventId popid_digits_all;
  static const EventId popid_digits_all_linebreak;
  static const EventId popid_image;
  static const EventId popid_change_image;
  static const EventId popid_svg;
  static const EventId popid_emf;
  static const EventId popid_animation_save;
  static const EventId popid_animation_start;
  static const EventId popid_evaluate;
  static const EventId popid_evaluate_section;
  static const EventId popid_merge_cells;
  static const EventId popid_insert_text;
  static const EventId popid_insert_title;
  static const EventId popid_insert_section;
  static const EventId popid_insert_subsection;
  static const EventId popid_insert_subsubsection;
  static const EventId popid_insert_heading5;
  static const EventId popid_insert_heading6;
  static const EventId popid_auto_answer;
  static const EventId popid_autocomplete_keyword1;
  static const EventId popid_never_autoanswer;
  static const EventId popid_popup_gnuplot;
  static const EventId popid_fold;
  static const EventId popid_unfold;
  static const EventId popid_maxsizechooser;
  static const EventId popid_resolutionchooser;
  static const EventId popid_reloadimage;
  static const EventId popid_suggestion1;
  static const EventId popid_suggestion2;
  static const EventId popid_suggestion3;
  static const EventId popid_suggestion4;
  static const EventId popid_suggestion5;
  static const EventId popid_suggestion6;
  static const EventId popid_suggestion7;
  static const EventId popid_suggestion8;
  static const EventId popid_suggestion9;
  static const EventId popid_suggestion10;
  static const EventId popid_hide_tooltipMarker;
  static const EventId popid_hide_tooltipMarkerForThisMessage;
  static const EventId popid_property_real;
  static const EventId popid_property_imaginary;
  static const EventId popid_property_complex;
  static const EventId popid_property_additive;
  static const EventId popid_property_alphabetic;
  static const EventId popid_property_bindtest;
  static const EventId popid_property_antisymmetric;
  static const EventId popid_property_commutative;
  static const EventId popid_property_symmetric;
  static const EventId popid_property_constant;
  static const EventId popid_property_even;
  static const EventId popid_property_odd;
  static const EventId popid_property_evenfun;
  static const EventId popid_property_oddfun;
  static const EventId popid_property_increasing;
  static const EventId popid_property_decreasing;
  static const EventId popid_property_integer;
  static const EventId popid_property_noninteger;
  static const EventId popid_property_integervalued;
  static const EventId popid_property_lassociative;
  static const EventId popid_property_rassociative;
  static const EventId popid_property_linear;
  static const EventId popid_property_mainvar;
  static const EventId popid_property_multiplicative;
  static const EventId popid_property_nary;
  static const EventId popid_property_nonarray;
  static const EventId popid_property_nonscalar;
  static const EventId popid_property_scalar;
  static const EventId popid_property_noun;
  static const EventId popid_property_outative;
  static const EventId popid_property_posfun;
  static const EventId popid_property_rational;
  static const EventId popid_property_irrational;
  static const EventId popid_property_greaterThan;
  static const EventId popid_property_evfun;
  static const EventId popid_property_evflag;
  static const EventId popid_property_atvalue;
  static const EventId popid_Fold;
  static const EventId popid_Unfold;
  static const EventId popid_SelectTocChapter;
  static const EventId popid_EvalTocChapter;
  static const EventId popid_ToggleTOCshowsSectionNumbers;
  static const EventId popid_tocLevel1;
  static const EventId popid_tocdnd;
  static const EventId popid_tocMoveIn;
  static const EventId popid_tocMoveOut;
  
private:
/*! @}
   */
};

#endif // EVENTIDS_H
