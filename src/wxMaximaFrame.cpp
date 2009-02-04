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

#include "wxMaximaFrame.h"

#include <wx/artprov.h>
#include <wx/config.h>
#include <wx/image.h>


wxMaximaFrame::wxMaximaFrame(wxWindow* parent, int id, const wxString& title,
                             const wxPoint& pos, const wxSize& size,
                             long style):
    wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)
{
  panel = new wxPanel(this, -1);

  // buttons
  int panelSize = 1;
  wxConfig::Get()->Read(wxT("panelSize"), &panelSize);
  if (panelSize == 2)
  {
    button_2 = new wxButton(panel, button_ratsimp, _("Simplify"));
    button_3 = new wxButton(panel, button_radcan, _("Simplify (r)"));
    button_4 = new wxButton(panel, button_factor, _("Factor"));
    button_5 = new wxButton(panel, button_expand, _("Expand"));
    button_6 = new wxButton(panel, button_trigsimp, _("Simplify (tr)"));
    button_7 = new wxButton(panel, button_trigexpand, _("Expand (tr)"));
    button_8 = new wxButton(panel, button_trigreduce, _("Reduce (tr)"));
    button_9 = new wxButton(panel, button_rectform, _("Rectform"));
    button_10 = new wxButton(panel, button_sum, _("Sum..."));
    button_11 = new wxButton(panel, button_product, _("Product..."));
    button_12 = new wxButton(panel, button_solve, _("Solve..."));
    button_13 = new wxButton(panel, button_solve_ode, _("Solve ODE..."));
    button_14 = new wxButton(panel, button_diff, _("Diff..."));
    button_15 = new wxButton(panel, button_integrate, _("Integrate..."));
    button_16 = new wxButton(panel, button_limit, _("Limit..."));
    button_17 = new wxButton(panel, button_taylor, _("Series..."));
    button_18 = new wxButton(panel, button_subst, _("Substitute..."));
    button_19 = new wxButton(panel, button_map, _("Map..."));
    button_20 = new wxButton(panel, button_plot2, _("Plot 2D..."));
    button_21 = new wxButton(panel, button_plot3, _("Plot 3D..."));
  }
  else if (panelSize == 1)
  {
    button_2 = new wxButton(panel, button_ratsimp, _("Simplify"));
    button_3 = new wxButton(panel, button_radcan, _("Simplify (r)"));
    button_4 = new wxButton(panel, button_factor, _("Factor"));
    button_5 = new wxButton(panel, button_expand, _("Expand"));
    button_12 = new wxButton(panel, button_solve, _("Solve..."));
    button_20 = new wxButton(panel, button_plot2, _("Plot 2D..."));
    button_6 = new wxButton(panel, button_trigsimp, _("Simplify (tr)"));
    button_7 = new wxButton(panel, button_trigexpand, _("Expand (tr)"));
    button_8 = new wxButton(panel, button_trigreduce, _("Reduce (tr)"));
    button_9 = new wxButton(panel, button_rectform, _("Rectform"));
    button_13 = new wxButton(panel, button_solve_ode, _("Solve ODE..."));
    button_21 = new wxButton(panel, button_plot3, _("Plot 3D..."));
  }

  // console
  m_console = new MathCtrl(panel, -1, wxDefaultPosition, wxDefaultSize);

  SetupMenu();
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  SetupToolBar();
#endif

  frame_1_statusbar = CreateStatusBar(2);
  int widths[] =
    {
      -1, 300
    };
  SetStatusWidths(2, widths);

#if defined __WXMSW__
  wxAcceleratorEntry entries[0];
  entries[0].Set(wxACCEL_CTRL,  WXK_RETURN, menu_reeval_input);
  wxAcceleratorTable accel(1, entries);
  SetAcceleratorTable(accel);
#endif

  set_properties();
  do_layout();
}

void wxMaximaFrame::set_properties()
{
#if defined (__WXMSW__)
  SetIcon(wxICON(maximaicon));
#elif defined (__WXGTK__)
  wxString icon(wxT(PREFIX));
  icon += wxT("/share/wxMaxima/wxmaxima.png");
  SetIcon(wxIcon(icon, wxBITMAP_TYPE_PNG));
#endif
  SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) + _("[ unsaved ]"));

  m_console->SetBackgroundColour(wxColour(wxT("WHITE")));
  m_console->SetMinSize(wxSize(100, 100));
  frame_1_statusbar->SetStatusText(_("Welcome to wxMaxima"), 0);
}

void wxMaximaFrame::do_layout()
{
  int panelSize = 1;
  wxConfig::Get()->Read(wxT("panelSize"), &panelSize);

  wxFlexGridSizer* grid_sizer_1;
  if (panelSize > 1)
    grid_sizer_1 = new wxFlexGridSizer(3, 1, 0, 0);
  else
    grid_sizer_1 = new wxFlexGridSizer(2, 1, 0, 0);

  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);

  wxGridSizer* grid_sizer_2;
  if (panelSize == 2)
    grid_sizer_2 = new wxGridSizer(2, 10, 0, 0);
  else
    grid_sizer_2 = new wxGridSizer(2, 6, 0, 0);

  // buttons
  if (panelSize == 2)
  {
    grid_sizer_2->Add(button_2, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_3, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_4, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_5, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_6, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_7, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_8, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_9, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_10, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_11, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_12, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_13, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_14, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_15, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_16, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_17, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_18, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_19, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_20, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_21, 0, wxALL | wxEXPAND, 0);
  }
  else if (panelSize == 1)
  {
    grid_sizer_2->Add(button_2, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_3, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_4, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_5, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_12, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_20, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_6, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_7, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_8, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_9, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_13, 0, wxALL | wxEXPAND, 0);
    grid_sizer_2->Add(button_21, 0, wxALL | wxEXPAND, 0);
  }

  // all
  grid_sizer_1->Add(m_console, 1, wxALL | wxEXPAND, 0);

  if (panelSize > 0)
    grid_sizer_1->Add(grid_sizer_2, 1, wxALL, 2);

  panel->SetAutoLayout(true);
  panel->SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(panel);
  grid_sizer_1->SetSizeHints(panel);
  grid_sizer_1->AddGrowableRow(0);
  grid_sizer_1->AddGrowableCol(0);

  sizer_1->Add(panel, 1, wxEXPAND, 0);
  SetAutoLayout(true);
  SetSizer(sizer_1);
  Layout();
}

void wxMaximaFrame::SetupMenu()
{
  frame_1_menubar = new wxMenuBar();

#if defined __WXGTK20__
  wxMenuItem *tmp_menu_item;
#define APPEND_MENU_ITEM(menu, id, label, help, stock)                         \
  tmp_menu_item = new wxMenuItem((menu), (id), (label), (help), wxITEM_NORMAL); \
  tmp_menu_item->SetBitmap(wxArtProvider::GetBitmap((stock), wxART_MENU));      \
  (menu)->Append(tmp_menu_item);
#else
#define APPEND_MENU_ITEM(menu, id, label, help, stock) \
  (menu)->Append((id), (label), (help), wxITEM_NORMAL);
#endif

  // File menu
  wxMenu* wxglade_tmp_menu_1 = new wxMenu;
#if defined __WXMAC__
  wxglade_tmp_menu_1->Append(mac_newId, _("&New window\tCtrl-N"),
			     _("Open a new window"));
#endif
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, menu_open_id, _("&Open\tCtrl-O"),
                   _("Open session from a file"), wxT("gtk-open"));
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, menu_save_id, _("&Save\tCtrl-S"),
                   _("Save session to a file"), wxT("gtk-save"));
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, menu_save_as_id, _("Save as"),
                   _("Save session to a file"), wxT("gtk-save"));
  wxglade_tmp_menu_1->Append(menu_load_id, _("&Load package\tCtrl-L"),
                             _("Load a maxima package file"), wxITEM_NORMAL);
  wxglade_tmp_menu_1->Append(menu_batch_id, _("&Batch file\tCtrl-B"),
                             _("Load a maxima file using batch command"), wxITEM_NORMAL);
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, menu_export_html, _("&Export"),
                   _("Export document output to HTML file"), wxT("stock_export"));
  wxglade_tmp_menu_1->AppendSeparator();
  m_recentDocumentsMenu = new wxMenu();
  wxglade_tmp_menu_1->Append(menu_recent_documents, _("Recent"), m_recentDocumentsMenu);
#if WXM_PRINT
  wxglade_tmp_menu_1->AppendSeparator();
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, wxID_PRINT, _("&Print\tCtrl-P"),
                   _("Print document"), wxT("gtk-print"));
#endif

  wxglade_tmp_menu_1->AppendSeparator();
  APPEND_MENU_ITEM(wxglade_tmp_menu_1, wxID_EXIT, _("E&xit\tCtrl-Q"),
                   _("Exit wxMaxima"), wxT("gtk-quit"));
  frame_1_menubar->Append(wxglade_tmp_menu_1, _("&File"));

  // Edit menu
  wxMenu* wxglade_tmp_menu_2 = new wxMenu;
  wxglade_tmp_menu_2->Append(menu_undo, _("Undo\tCtrl-Z"),
                             _("Undo last change"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->AppendSeparator();
  wxglade_tmp_menu_2->Append(menu_cut, _("Cut\tCtrl-X"),
                             _("Cut selection from document"),
                             wxITEM_NORMAL);
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_copy_from_console, _("&Copy\tCtrl-C"),
                   _("Copy selection from document"), wxT("gtk-copy"));
  wxglade_tmp_menu_2->Append(menu_copy_tex_from_console, _("Copy TeX"),
                                  _("Copy selection from document in TeX format"),
                                  wxITEM_NORMAL);
#if defined __WXMSW__ || defined __WXMAC__
  wxglade_tmp_menu_2->Append(menu_copy_as_bitmap, _("Copy as image"),
                             _("Copy selection from document as image"),
                             wxITEM_NORMAL);
#endif
  wxglade_tmp_menu_2->Append(menu_paste, _("Paste\tCtrl-V"),
                             _("Paste text from clipboard"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->AppendSeparator();
  wxglade_tmp_menu_2->Append(menu_select_all, _("Select all\tCtrl-A"),
                             _("Select all"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_copy_to_file, _("Selection to image"),
                             _("Copy selection from document to a file"),
                             wxITEM_NORMAL);
  wxMenu* wxglade_tmp_menu_2_sub1 = new wxMenu;
  wxglade_tmp_menu_2_sub1->Append(menu_cut_input_from_console, _("Cut cells"),
                                  _("Cut selected cells"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_2_sub1->Append(menu_copy_input_from_console, _("Copy cells"),
                                  _("Copy selected cells"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_2_sub1->Append(menu_paste_input, _("Paste cells"),
                             _("Paste cells to document"),
                             wxITEM_NORMAL);
  APPEND_MENU_ITEM(wxglade_tmp_menu_2_sub1, menu_delete_selection,
                   _("&Delete cells"),
                   _("Delete selected cells"), wxT("gtk-delete"));
  wxglade_tmp_menu_2_sub1->AppendSeparator();
  wxglade_tmp_menu_2_sub1->Append(menu_reeval_input, _("Evaluate cell"),
                             _("Evaluate selected cell"), wxITEM_NORMAL);
  wxglade_tmp_menu_2_sub1->Append(menu_reeval_all, _("Evaluate all cells\tCtrl-Shift-R"),
                               _("Evaluate all cells"), wxITEM_NORMAL);
  wxglade_tmp_menu_2_sub1->AppendSeparator();
  wxglade_tmp_menu_2_sub1->Append(menu_insert_input, _("New input &cell\tF7"),
                             _("Insert new input cell"));
  wxglade_tmp_menu_2_sub1->Append(menu_add_comment, _("New &text cell\tF6"),
                             _("Insert new text cell"));
  wxglade_tmp_menu_2_sub1->Append(menu_add_section, _("New &section cell\tCtrl-F6"),
                             _("Insert new section cell"));
  wxglade_tmp_menu_2_sub1->Append(menu_add_title, _("New t&itle cell\tCtrl-Shift-F6"),
                             _("Insert new title cell"));
  wxglade_tmp_menu_2->AppendSeparator();
  wxglade_tmp_menu_2->Append(wxNewId(), _("Cell"), wxglade_tmp_menu_2_sub1, _("Input"));

  wxglade_tmp_menu_2->AppendSeparator();
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_inc_fontsize, _("Zoom &in\tAlt-I"),
                   _("Increase fontsize in document"), wxT("gtk-zoom-in"));
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_dec_fontsize, _("Zoom ou&t\tAlt-O"),
                   _("Decrease fontsize in document"), wxT("gtk-zoom-out"));
  wxglade_tmp_menu_2->Append(menu_fullscreen, _("Full Screen\tAlt-Enter"),
                             _("Toggle full screen editing"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->AppendSeparator();
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, wxID_PREFERENCES, _("C&onfigure"),
                   _("Configure wxMaxima"), wxT("gtk-preferences"));
  frame_1_menubar->Append(wxglade_tmp_menu_2, _("&Edit"));

  // Maxima menu
  wxglade_tmp_menu_2 = new wxMenu;
#if defined (__WXMAC__)
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_interrupt_id,
                   _("&Interrupt\tCtrl-."), // command-. interrupts (mac standard)
                   _("Interrupt current computation"), wxT("gtk-stop"));
#else
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_interrupt_id,
                   _("&Interrupt\tCtrl-G"),
                   _("Interrupt current computation"), wxT("gtk-stop"));
#endif
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_restart_id,
                   _("&Restart maxima"), _("Restart maxima"), wxT("gtk-refresh"));
  wxglade_tmp_menu_2->Append(menu_soft_restart, _("&Clear memory"),
                             _("Delete all values from memory"), wxITEM_NORMAL);
  APPEND_MENU_ITEM(wxglade_tmp_menu_2, menu_add_path, _("Add to &path"),
                   _("Add a directory to search path"), wxT("gtk-add"));
  wxglade_tmp_menu_2->AppendSeparator();
  wxglade_tmp_menu_2->Append(menu_functions, _("Show &functions"),
                             _("Show defined functions"), wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_fun_def, _("Show &definition"),
                             _("Show definition of a function"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_variables, _("Show &variables"),
                             _("Show defined variables"), wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_clear_fun, _("Delete f&unction"),
                             _("Delete a function"), wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_clear_var, _("Delete v&ariable"),
                             _("Delete a variable"), wxITEM_NORMAL);
  wxglade_tmp_menu_2->AppendSeparator();
  wxglade_tmp_menu_2->Append(menu_time, _("Toggle &time display"),
                             _("Display time used for execution"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_display, _("Change &2d display"),
                             _("Change the 2d display algorithm used to display math output."),
			     wxITEM_NORMAL);
  wxglade_tmp_menu_2->Append(menu_texform, _("Display Te&X form"),
                             _("Display expression in TeX form"), wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_2, _("&Maxima"));

  // Equations menu
  wxMenu* wxglade_tmp_menu_3 = new wxMenu;
  wxglade_tmp_menu_3->Append(menu_solve, _("&Solve ..."),
                             _("Solve equation(s)"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_solve_num, _("&Find root ..."),
                             _("Find a root of an equation on an interval"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_allroots, _("Roots of &polynomial"),
                             _("Find all roots of a polynomial"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_realroots, _("&Roots of polynomial (real)"),
                             _("Find real roots of a polynomial"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_solve_lin, _("Solve &linear system ..."),
                             _("Solve linear system of equations"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_solve_algsys, _("Solve &algebraic system ..."),
                             _("Solve algebraic system of equations"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_eliminate, _("&Eliminate variable ..."),
                             _("Eliminate a variable from a system "
                               "of equations"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->AppendSeparator();
  wxglade_tmp_menu_3->Append(menu_solve_ode, _("Solve &ODE ..."),
                             _("Solve ordinary differential equation "
                               "of maximum degree 2"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_ivp_1, _("Initial value problem (&1) ..."),
                             _("Solve initial value problem for first"
                               " degree ODE"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_ivp_2, _("Initial value problem (&2) ..."),
                             _("Solve initial value problem for second "
                               "degree ODE"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_bvp, _("&Boundary value problem ..."),
                             _("Solve boundary value problem for second "
                               "degree ODE"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->AppendSeparator();
  wxglade_tmp_menu_3->Append(menu_solve_de, _("Solve ODE with Lapla&ce ..."),
                             _("Solve ordinary differential equations "
                               "with Laplace transformation"), wxITEM_NORMAL);
  wxglade_tmp_menu_3->Append(menu_atvalue, _("A&t value ..."),
                             _("Setup atvalues for solving ODE with "
                               "Laplace transformation"), wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_3, _("E&quations"));

  // Algebra menu
  wxMenu* wxglade_tmp_menu_4 = new wxMenu;
  wxglade_tmp_menu_4->Append(menu_gen_mat, _("&Generate matrix ..."),
                             _("Generate a matrix from a 2-dimensional array"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_enter_mat, _("&Enter matrix ..."),
                             _("Enter a matrix"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_invert_mat, _("&Invert matrix"),
                             _("Compute the inverse of a matrix"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_cpoly, _("&Characteristic polynomial ..."),
                             _("Compute the characteristic polynomial "
                               "of a matrix"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_determinant, _("&Determinant"),
                             _("Compute the determinant of a matrix"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_eigen, _("Eigen&values"),
                             _("Find eigenvalues of a matrix"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_eigvect, _("Eige&nvectors"),
                             _("Find eigenvectors of a matrix"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_adjoint_mat, _("Ad&joint matrix"),
                             _("Compute the adjoint maxrix"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_transpose, _("&Transpose matrix"),
                             _("Transpose a matrix"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->AppendSeparator();
  wxglade_tmp_menu_4->Append(menu_make_list, _("Make &list ..."),
                             _("Make list from expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_apply, _("&Apply to list ..."),
                             _("Apply function to a list"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_map, _("&Map to list ..."),
                             _("Map function to a list"), wxITEM_NORMAL);
  wxglade_tmp_menu_4->Append(menu_map_mat, _("Ma&p to matrix ..."),
                             _("Map function to a matrix"), wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_4, _("&Algebra"));

  // Calculus menu
  wxMenu* wxglade_tmp_menu_6 = new wxMenu;
  wxglade_tmp_menu_6->Append(menu_integrate, _("&Integrate ..."),
                             _("Integrate expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_risch, _("Risch integration ..."),
                             _("Integrate expression with Risch algorithm"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_change_var, _("C&hange variable ..."),
                             _("Change variable in integral or sum"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_diff, _("&Differentiate ..."),
                             _("Differentiate expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_limit, _("Find &limit ..."),
                             _("Find a limit of an expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_series, _("Get &series ..."),
                             _("Get the Taylor or power series of expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_pade, _("P&ade approximation ..."),
                             _("Pade approximation of a Taylor series"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_sum, _("Calculate su&m ..."),
                             _("Calculate sums"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_product, _("Calculate &product ..."),
                             _("Calculate products"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_laplace, _("Laplace &transform ..."),
                             _("Get Laplace transformation of an expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_ilt, _("Inverse Laplace t&ransform ..."),
                             _("Get inverse Laplace transformation of an expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_gcd, _("&Greatest common divisor ..."),
                             _("Compute the greatest common divisor"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_lcm, _("Least common multiple ..."),
                             _("Compute the least common multiple "
                               "(do load(functs) before using)"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_divide, _("Di&vide polynomials ..."),
                             _("Divide numbers or polynomials"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_partfrac, _("Partial &fractions ..."),
                             _("Decompose rational function to partial fractions"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_continued_fraction, _("&Continued fraction"),
                             _("Compute continued fraction of a value"),
                             wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_6, _("&Calculus"));

  // Simplify menu
  wxMenu* wxglade_tmp_menu_5 = new wxMenu;
  wxglade_tmp_menu_5->Append(menu_ratsimp, _("&Simplify expression"),
                             _("Simplify rational expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_radsimp, _("Simplify &radicals"),
                             _("Simplify expression containing radicals"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_factor, _("&Factor expression"),
                             _("Factor an expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_gfactor, _("Factor complex"),
                             _("Factor an expression in Gaussian numbers"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_expand, _("&Expand expression"),
                             _("Expand an expression"), wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_logexpand, _("Expand logarithms"),
                             _("Convert logarithm of product to sum of logarithms"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_logcontract, _("Contract logarithms"),
                             _("Convert sum of logarithms to logarithm of product"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->AppendSeparator();
  // Factorials and gamma
  wxMenu* wxglade_tmp_menu_5_sub1 = new wxMenu;
  wxglade_tmp_menu_5_sub1->Append(menu_to_fact, _("Convert to &factorials"),
                                  _("Convert binomials, beta and gamma function to factorials"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub1->Append(menu_to_gamma, _("Convert to &gamma"),
                                  _("Convert binomials, factorials and beta function to gamma function"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub1->Append(menu_factsimp, _("&Simplify factorials"),
                                  _("Simplify an expression containing factorials"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub1->Append(menu_factcomb, _("&Combine factorials"),
                                  _("Combine factorials in an expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(wxNewId(), _("Factorials and &gamma"),
                             wxglade_tmp_menu_5_sub1,
                             _("Functions for simplifying factorials and gamma function"));
  // Trigonometric
  wxMenu* wxglade_tmp_menu_5_sub2 = new wxMenu;
  wxglade_tmp_menu_5_sub2->Append(menu_trigsimp, _("&Simplify trigonometric"),
                                  _("Simplify trigonometric expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub2->Append(menu_trigreduce, _("&Reduce trigonometric"),
                                  _("Reduce trigonometric expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub2->Append(menu_trigexpand, _("&Expand trigonometric"),
                                  _("Expand trigonometric expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub2->Append(menu_trigrat, _("&Canonical form"),
                                  _("Convert trigonometric expression to canonical quasilinear form"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(wxNewId(), _("&Trigonometric simplification"),
                             wxglade_tmp_menu_5_sub2,
                             _("Functions for simplifying trigonometric expressions"));
  // Complex
  wxMenu* wxglade_tmp_menu_5_sub3 = new wxMenu;
  wxglade_tmp_menu_5_sub3->Append(menu_rectform, _("Convert to &rectform"),
                                  _("Convert complex expression to rect form"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub3->Append(menu_polarform, _("Convert to &polarform"),
                                  _("Convert complex expression to polar form"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub3->Append(menu_realpart, _("Get real p&art"),
                                  _("Get the real part of complex expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub3->Append(menu_imagpart, _("Get &imaginary part"),
                                  _("Get the imaginary part of complex expression"),
                                  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub3->Append(menu_demoivre, _("&Demoivre"),
                                  _("Convert exponential function of imaginary argument to trigonometric form"),
				  wxITEM_NORMAL);
  wxglade_tmp_menu_5_sub3->Append(menu_exponentialize, _("&Exponentialize"),
                                  _("Conver trigonometric functions to exponential form"),
				  wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(wxNewId(), _("&Complex simplification"),
                             wxglade_tmp_menu_5_sub3,
                             _("Functions for complex simplification"));
  wxglade_tmp_menu_5->AppendSeparator();
  wxglade_tmp_menu_5->Append(menu_subst, _("Substitute ..."),
                             _("Make substitution in expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_nouns, _("Evaluate &noun forms"),
                             _("Evaluate all noun forms in expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_talg, _("Toggle &algebraic flag"),
                             _("Toggle algebraic flag"), wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_tellrat, _("Add algebraic e&quality ..."),
                             _("Add equality to the rational simplifier"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_5->Append(menu_modulus, _("&Modulus computation ..."),
                             _("Setup modulus computation"), wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_5, _("&Simplify"));

  // Plot menu
  wxglade_tmp_menu_6 = new wxMenu;
  wxglade_tmp_menu_6->Append(gp_plot2, _("Plot &2d ..."),
                             _("Plot in 2 dimensions"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(gp_plot3, _("Plot &3d ..."),
                             _("Plot in 3 dimensions"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_plot_format, _("Plot &format ..."),
                             _("Set plot format"), wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_6, _("&Plot"));

  // Numeric menu
  wxglade_tmp_menu_6 = new wxMenu;
  wxglade_tmp_menu_6->Append(menu_num_out, _("Toggle &numeric output"),
                             _("Toggle numeric output"), wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_to_float, _("To &float\tCtrl-F"),
                             _("The float value of an expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_to_bfloat, _("To &bigfloat"),
                             _("The bigfloat value of an expression"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_6->Append(menu_set_precision, _("Set &precision ..."),
                             _("Set the floating point precision"),
                             wxITEM_NORMAL);
  frame_1_menubar->Append(wxglade_tmp_menu_6, _("&Numeric"));

  // Help menu
  wxMenu* wxglade_tmp_menu_7 = new wxMenu;
  APPEND_MENU_ITEM(wxglade_tmp_menu_7, wxID_HELP, _("Maxima &help\tF1"),
                   _("Show maxima help"), wxT("gtk-help"));
  wxglade_tmp_menu_7->Append(menu_example, _("&Example"),
                             _("Show an example of usage"),
                             wxITEM_NORMAL);
  wxglade_tmp_menu_7->Append(menu_apropos, _("&Apropos"),
                             _("Show commands similar to"),
                             wxITEM_NORMAL);
//  APPEND_MENU_ITEM(wxglade_tmp_menu_7, menu_show_tip, _("Show &tip"),
//                   _("Show a tip"), wxART_TIP);
  wxglade_tmp_menu_7->AppendSeparator();
  wxglade_tmp_menu_7->Append(menu_build_info, _("Build &info"),
                             _("Info about maxima build"), wxITEM_NORMAL);
  wxglade_tmp_menu_7->Append(menu_bug_report, _("&Bug report"),
                             _("Report bug"), wxITEM_NORMAL);
#ifndef __WXMAC__
  wxglade_tmp_menu_7->AppendSeparator();
#endif
  APPEND_MENU_ITEM(wxglade_tmp_menu_7, wxID_ABOUT, _("About"),
                   _("About wxMaxima"), wxT("stock_about"));
  frame_1_menubar->Append(wxglade_tmp_menu_7, _("&Help"));

  SetMenuBar(frame_1_menubar);

#undef APPEND_MENU_ITEM

}

#if defined (__WXMSW__) || defined (__WXMAC__)

#if defined (__WXMSW__)
#define IMAGE(img) wxImage(wxT("art/toolbar/") wxT(img))
#else
#define IMAGE(img) wxImage(wxT("wxMaxima.app/Contents/Resources/toolbar/") wxT(img))
#endif

void wxMaximaFrame::SetupToolBar()
{
  wxToolBar* frame_1_toolbar = CreateToolBar();

  frame_1_toolbar->SetToolBitmapSize(wxSize(22, 22));

  frame_1_toolbar->AddTool(tb_open, _("Open"),
                           IMAGE("open.png"),
			                     _("Open session"));
  frame_1_toolbar->AddTool(tb_save, _("Save"),
                           IMAGE("save.png"),
                           _("Save session"));
  frame_1_toolbar->AddSeparator();
#if WXM_PRINT
  frame_1_toolbar->AddTool(tb_print, _("Print"),
                           IMAGE("print.png"),
                           _("Print document"));
#endif
  frame_1_toolbar->AddTool(tb_pref, _("Options"),
                           IMAGE("configure.png"),
                           _("Configure wxMaxima"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_copy, _("Copy"),
                           IMAGE("copy.png"),
                           _("Copy selection"));
  frame_1_toolbar->AddTool(tb_delete, _("Delete"),
                           IMAGE("cut.png"),
                           _("Delete selection"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_insert_text, _("Insert text"),
                           IMAGE("text.png"),
                           _("Insert text"));
  frame_1_toolbar->AddTool(tb_insert_input, _("Insert input group"),
                           IMAGE("input.png"),
                           _("Insert input cell"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_interrupt, _("Interrupt"),
                           IMAGE("stop.png"),
                           _("Interrupt current computation"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_animation_start, _("Start animation"),
                           IMAGE("playback-start.png"),
                           _("Start animation"));
  frame_1_toolbar->AddTool(tb_animation_stop, _("Stop animation"),
			   IMAGE("playback-stop.png"),
			   _("Stop animation"));
  m_plotSlider = new wxSlider(frame_1_toolbar, plot_slider_id, 0, 0, 10,
			      wxDefaultPosition, wxDefaultSize,
			      wxSL_HORIZONTAL | !wxSL_AUTOTICKS);
  frame_1_toolbar->AddControl(m_plotSlider);
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_help, _("Help"),
                           IMAGE("help.png"),
                           _("Show maxima help"));
  frame_1_toolbar->Realize();
  SetToolBar(frame_1_toolbar);
}

#elif defined (__WXGTK20__)

void wxMaximaFrame::SetupToolBar()
{
  wxToolBar* frame_1_toolbar = CreateToolBar();

  frame_1_toolbar->AddTool(tb_open, _("Open"),
                           wxArtProvider::GetBitmap(wxT("gtk-open"),
                                                    wxART_TOOLBAR),
                           _("Open session"));
  frame_1_toolbar->AddTool(tb_save, _("Save"),
                           wxArtProvider::GetBitmap(wxT("gtk-save"),
                                                    wxART_TOOLBAR),
                           _("Save session"));
  frame_1_toolbar->AddSeparator();
#if WXM_PRINT
  frame_1_toolbar->AddTool(tb_print, _("Print"),
                           wxArtProvider::GetBitmap(wxT("gtk-print"),
                                                    wxART_TOOLBAR),
                           _("Print document"));
#endif
  frame_1_toolbar->AddTool(tb_pref, _("Options"),
                           wxArtProvider::GetBitmap(wxT("gtk-preferences"),
                                                    wxART_TOOLBAR),
                           _("Configure wxMaxima"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_copy, _("Copy"),
                           wxArtProvider::GetBitmap(wxT("gtk-copy"),
                                                    wxART_TOOLBAR),
                           _("Copy selection"));
  frame_1_toolbar->AddTool(tb_delete, _("Delete"),
                           wxArtProvider::GetBitmap(wxT("gtk-cut"),
                                                    wxART_TOOLBAR),
                           _("Delete selection"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_insert_text, _("Insert text"),
                           wxArtProvider::GetBitmap(wxART_NORMAL_FILE,
                                                    wxART_TOOLBAR),
                           _("Insert text"));
  frame_1_toolbar->AddTool(tb_insert_input, _("Insert input cell"),
                           wxArtProvider::GetBitmap(wxART_EXECUTABLE_FILE,
                                                    wxART_TOOLBAR),
                           _("Insert input cell"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_interrupt, _("Interrupt"),
                           wxArtProvider::GetBitmap(wxT("gtk-stop"),
                                                    wxART_TOOLBAR),
                           _("Interrupt current computation"));
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_animation_start, _("Animation"),
			   wxArtProvider::GetBitmap(wxT("media-playback-start"),
						    wxART_TOOLBAR));
  frame_1_toolbar->AddTool(tb_animation_stop, _("Stop animation"),
			   wxArtProvider::GetBitmap(wxT("media-playback-stop"),
						    wxART_TOOLBAR));
  m_plotSlider = new wxSlider(frame_1_toolbar, plot_slider_id, 0, 0, 10,
			      wxDefaultPosition, wxSize(200, -1),
			      wxSL_HORIZONTAL | !wxSL_AUTOTICKS);
  frame_1_toolbar->AddControl(m_plotSlider);
  frame_1_toolbar->AddSeparator();
  frame_1_toolbar->AddTool(tb_help, _("Help"),
                           wxArtProvider::GetBitmap(wxT("gtk-help"),
                                                    wxART_TOOLBAR),
                           _("Show maxima help"));
  frame_1_toolbar->Realize();
  SetToolBar(frame_1_toolbar);
}

#endif

void wxMaximaFrame::LoadRecentDocuments()
{
  wxConfigBase *config = wxConfig::Get();
  m_recentDocuments.Clear();

  for (int i=0; i<=5; i++)
  {
    wxString recent = wxString::Format(wxT("RecentDocuments/document_%d"), i);
    wxString file;
    if (config->Read(recent, &file))
      m_recentDocuments.Add(file);
  }
}

void wxMaximaFrame::SaveRecentDocuments()
{
  wxConfigBase *config = wxConfig::Get();

  for (int i=0; i<m_recentDocuments.Count(); i++)
  {
    wxString recent = wxString::Format(wxT("RecentDocuments/document_%d"), i);
    config->Write(recent, m_recentDocuments[i]);
  }
}

void wxMaximaFrame::UpdateRecentDocuments()
{
  wxMenu* fileMenu = GetMenuBar()->GetMenu(GetMenuBar()->FindMenu(_("&File")));

  for (int i=menu_recent_document_0; i<= menu_recent_document_5; i++)
  {
    wxMenuItem *item = m_recentDocumentsMenu->Remove(i);

    if (i-menu_recent_document_0 < m_recentDocuments.Count())
    {
      if (item == NULL)
        m_recentDocumentsMenu->Append(i, m_recentDocuments[i - menu_recent_document_0]);
      else
      {
        item->SetItemLabel(m_recentDocuments[i - menu_recent_document_0]);
        m_recentDocumentsMenu->Append(item);
      }
    }
  }

  SaveRecentDocuments();
}

void wxMaximaFrame::AddRecentDocument(wxString file)
{
  m_recentDocuments.Remove(file);
  m_recentDocuments.Insert(file, 0);

  UpdateRecentDocuments();
}
