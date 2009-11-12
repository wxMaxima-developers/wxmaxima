///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef WXMAXIMAFRAME_H
#define WXMAXIMAFRAME_H

#include <wx/wx.h>

#include <wx/dirctrl.h>
#include <wx/listbox.h>
#include <wx/bmpbuttn.h>
#include <wx/arrstr.h>
#include <wx/aui/aui.h>

#include "MathCtrl.h"
#include "Setup.h"
#include "History.h"
#include "Inspector.h"

enum {
  socket_client_id = wxID_HIGHEST,
  socket_server_id,
  plot_slider_id,
  input_line_id,
  menu_open_id,
  menu_batch_id,
  menu_save_id,
  menu_save_as_id,
  menu_load_id,
  menu_sconsole_id,
  menu_interrupt_id,
  menu_restart_id,
  menu_solve,
  menu_solve_to_poly,
  menu_solve_num,
  menu_allroots,
  menu_bfallroots,
  menu_realroots,
  menu_solve_lin,
  menu_solve_algsys,
  menu_eliminate,
  menu_solve_ode,
  menu_ivp_1,
  menu_ivp_2,
  menu_bvp,
  menu_gen_mat,
  menu_enter_mat,
  menu_invert_mat,
  menu_cpoly,
  menu_determinant,
  menu_eigen,
  menu_eigvect,
  menu_fun_def,
  menu_adjoint_mat,
  menu_transpose,
  menu_map_mat,
  menu_ratsimp,
  menu_radsimp,
  menu_factor,
  menu_gfactor,
  menu_expand,
  menu_talg,
  menu_tellrat,
  menu_modulus,
  menu_trigsimp,
  menu_trigreduce,
  menu_trigexpand,
  menu_trigrat,
  menu_rectform,
  menu_polarform,
  menu_demoivre,
  menu_exponentialize,
  menu_num_out,
  menu_to_float,
  menu_to_bfloat,
  menu_set_precision,
  menu_functions,
  menu_variables,
  menu_clear_var,
  menu_clear_fun,
  menu_integrate,
  menu_risch,
  menu_laplace,
  menu_ilt,
  menu_continued_fraction,
  menu_gcd,
  menu_lcm,
  menu_divide,
  menu_partfrac,
  menu_sum,
  menu_limit,
  menu_lbfgs,
  menu_series,
  menu_pade,
  menu_map,
  menu_diff,
  menu_solve_de,
  menu_atvalue,
  menu_example,
  menu_apropos,
  menu_product,
  menu_make_list,
  menu_apply,
  menu_time,
  menu_factsimp,
  menu_factcomb,
  menu_realpart,
  menu_imagpart,
  menu_subst,
  button_factor_id,
  button_solve,
  button_solve_ode,
  button_limit,
  button_taylor,
  button_expand,
  button_ratsimp,
  button_radcan,
  button_trigsimp,
  button_trigexpand,
  button_trigreduce,
  button_trigrat,
  button_integrate,
  button_diff,
  button_sum,
  button_product,
  button_button_constant,
  button_factor,
  button_subst,
  button_plot2,
  button_plot3,
  button_rectform,
  button_map,
  gp_plot2,
  gp_plot3,
  menu_display,
  menu_soft_restart,
  menu_plot_format,
  menu_build_info,
  menu_bug_report,
  menu_add_path,
  menu_evaluate_all,
  menu_show_tip,
  menu_copy_from_console,
  menu_copy_tex_from_console,
  menu_copy_text_from_console,
  menu_undo,
  menu_delete_selection,
  menu_select_all,
  menu_logcontract,
  menu_logexpand,
  menu_to_fact,
  menu_to_gamma,
  menu_texform,
  button_enter,
  menu_zoom_in,
  menu_zoom_out,
  menu_zoom_80,
  menu_zoom_100,
  menu_zoom_120,
  menu_zoom_150,
  menu_zoom_200,
  menu_zoom_300,
  menu_copy_as_bitmap,
  menu_copy_to_file,
  menu_export_html,
  menu_change_var,
  menu_nouns,
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  tb_open,
  tb_save,
  tb_copy,
  tb_paste,
  tb_cut,
  tb_print,
  tb_pref,
  tb_interrupt,
  tb_help,
  tb_animation_start,
  tb_animation_stop,
  tb_find,
#endif
  menu_evaluate,
  menu_add_comment,
  menu_add_subsection,
  menu_add_section,
  menu_add_title,
  menu_add_pagebreak,
  menu_insert_input,
  menu_insert_previous_input,
  menu_autocomplete,
  menu_autocomplete_templates,
  menu_cut,
  menu_paste,
  menu_paste_input,
  menu_fullscreen,
  menu_remove_output,
#if defined (__WXMAC__)
  mac_newId,
  mac_openId,
#endif
  menu_recent_documents,
  menu_recent_document_0,
  menu_recent_document_1,
  menu_recent_document_2,
  menu_recent_document_3,
  menu_recent_document_4,
  menu_recent_document_5,
  menu_recent_document_6,
  menu_recent_document_7,
  menu_recent_document_8,
  menu_recent_document_9,
  menu_insert_image,
  menu_pane_hideall,
  menu_pane_math,
  menu_pane_history,
  menu_pane_inspector,
  menu_pane_format,
  menu_pane_stats,
  menu_stats_mean,
  menu_stats_median,
  menu_stats_var,
  menu_stats_dev,
  menu_stats_tt1,
  menu_stats_tt2,
  menu_stats_tnorm,
  menu_stats_linreg,
  menu_stats_lsquares,
  menu_stats_histogram,
  menu_stats_scatterplot,
  menu_stats_barsplot,
  menu_stats_piechart,
  menu_stats_boxplot,
  menu_stats_readm,
  menu_stats_enterm,
  menu_stats_subsample,
  menu_format_code,
  menu_format_text,
  menu_format_subsection,
  menu_format_section,
  menu_format_title,
  menu_format_image,
  menu_format_pagebreak,
  menu_help_tutorials,
  menu_show_toolbar,
  menu_edit_find
};

#define FIRST_PANE menu_pane_hideall
#define LAST_PANE  menu_pane_stats

class wxMaximaFrame: public wxFrame
{
public:
  wxMaximaFrame(wxWindow* parent, int id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE);
  ~wxMaximaFrame();
  void UpdateRecentDocuments();
  void AddRecentDocument(wxString file);
  void RemoveRecentDocument(wxString file);
  wxString GetRecentDocument(int i) { return m_recentDocuments[i]; }
  bool IsPaneDisplayed(int id);
  void ShowPane(int id, bool hide);
  void AddToHistory(wxString cmd) { m_history->AddToHistory(cmd); }
  void ShowToolBar(bool show);
private:
  void set_properties();
  void do_layout();
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  void SetupToolBar();
#endif
  void SetupMenu();
  wxPanel *CreateStatPane();
  wxPanel *CreateMathPane();
  wxPanel *CreateFormatPane();
protected:
  void LoadRecentDocuments();
  void SaveRecentDocuments();
  wxAuiManager m_manager;
  wxMenuBar* frame_1_menubar;

  MathCtrl* m_console;
  History * m_history;
  Inspector * m_inspector;
  wxStatusBar* frame_1_statusbar;
  wxToolBar* frame_1_toolbar;
  wxSlider* m_plotSlider;
  wxArrayString m_recentDocuments;
  wxMenu* m_recentDocumentsMenu;
};

#endif // WXMAXIMAFRAME_H
