///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "CommandLine.h"
#include "MathCtrl.h"
#include "Setup.h"

enum {
  socket_client_id = wxID_HIGHEST,
  socket_server_id,
  plot_slider_id,
  input_line_id,
  menu_open_id,
  menu_read_id,
  menu_batch_id,
  menu_save_id,
  menu_save_as_id,
  menu_load_id,
  menu_sconsole_id,
  menu_interrupt_id,
  menu_restart_id,
  menu_solve,
  menu_solve_num,
  menu_allroots,
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
  menu_describe,
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
  button_integrate,
  button_diff,
  button_sum,
  button_product,
  button_button_constant,
  button_factor,
  button_long_input,
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
  menu_reeval_all,
  menu_show_tip,
  menu_select_file,
  menu_monitor_file,
  menu_clear_screen,
  menu_copy_from_console,
  menu_copy_lb_from_console,
  menu_copy_tex_from_console,
  menu_copy_input_from_console,
  menu_delete_selection,
  menu_goto_input,
  menu_logcontract,
  menu_logexpand,
  menu_to_fact,
  menu_to_gamma,
  menu_texform,
  button_enter,
  menu_inc_fontsize,
  menu_dec_fontsize,
  menu_copy_as_bitmap,
  menu_copy_to_file,
  menu_export_html,
  menu_selection_to_input,
  menu_change_var,
  menu_nouns,
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  tb_open,
  tb_save,
  tb_copy,
  tb_delete,
  tb_print,
  tb_pref,
  tb_interrupt,
  tb_help,
  tb_insert_text,
  tb_insert_input,
  tb_animation_start,
  tb_animation_stop,
#endif
  menu_edit_input,
  menu_reeval_input,
  menu_long_input,
  menu_add_comment,
  menu_add_section,
  menu_add_title,
  menu_insert_input,
  menu_unfold,
  menu_select_last,
  menu_goto_output,
  activate_cell,
  deactivate_cell_ok,
  deactivate_cell_cancel,
  menu_cut,
  menu_paste,
  menu_paste_input,
#if defined (__WXMAC__)
  mac_newId,
  mac_openId
#endif
};

class wxMaximaFrame: public wxFrame
{
public:
  wxMaximaFrame(wxWindow* parent, int id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE);
private:
  void set_properties();
  void do_layout();
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  void SetupToolBar();
#endif
  void SetupMenu();
protected:
  wxMenuBar* frame_1_menubar;
  wxPanel *panel;
  MathCtrl* m_console;
  wxStaticText* label_1;
  CommandLine* m_inputLine;
  wxBitmapButton* button_0;
  wxBitmapButton* button_1;
  wxButton* button_2;
  wxButton* button_3;
  wxButton* button_4;
  wxButton* button_5;
  wxButton* button_6;
  wxButton* button_7;
  wxButton* button_8;
  wxButton* button_9;
  wxButton* button_10;
  wxButton* button_11;
  wxButton* button_12;
  wxButton* button_13;
  wxButton* button_14;
  wxButton* button_15;
  wxButton* button_16;
  wxButton* button_17;
  wxButton* button_18;
  wxButton* button_19;
  wxButton* button_20;
  wxButton* button_21;
  wxStatusBar* frame_1_statusbar;
  wxToolBar* frame_1_toolbar;
  wxSlider* m_plotSlider;
};

#endif // WXMAXIMAFRAME_H
