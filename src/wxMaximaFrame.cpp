// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2011-2011 cw.ahbong <cwahbong@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "wxMaximaFrame.h"
#include "Dirstructure.h"

#include <wx/artprov.h>
#include <wx/config.h>
#include <wx/image.h>
#include <wx/filename.h>

wxMaximaFrame::wxMaximaFrame(wxWindow* parent, int id, const wxString& title,
                             const wxPoint& pos, const wxSize& size,
                             long style):
  wxFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE)

{
  m_EvaluationQueueLength = 0;
  m_forceStatusbarUpdate = false;
  m_manager.SetManagedWindow(this);
  // console
  m_console = new MathCtrl(this, -1, wxDefaultPosition, wxDefaultSize);

  // history
  m_history = new History(this, -1);

  // The table of contents
  m_console->m_structure = new Structure(this, -1);

  m_xmlInspector = new XmlInspector(this, -1);
  SetupMenu();

  CreateStatusBar(2);
  int widths[] = { -1, 300 };
  SetStatusWidths(2, widths);

  m_StatusSaving = false;
  // If we need to set the status manually for the first time using StatusMaximaBusy
  // we first have to manually set the last state to something else.
  m_StatusMaximaBusy = calculating;
  StatusMaximaBusy(waiting);

  // Add some shortcuts that aren't automatically set by menu entries.
  wxAcceleratorEntry entries[6];
  entries[0].Set(wxACCEL_CTRL,  WXK_TAB,    menu_autocomplete);
  entries[1].Set(wxACCEL_CTRL,  WXK_SPACE,  menu_autocomplete);
  entries[2].Set(wxACCEL_CTRL|wxACCEL_SHIFT,  WXK_TAB,    menu_autocomplete_templates);
  entries[3].Set(wxACCEL_CTRL|wxACCEL_SHIFT,  WXK_SPACE,  menu_autocomplete_templates);
  entries[4].Set(wxACCEL_ALT,  wxT('I'),   MathCtrl::menu_zoom_in);
  entries[5].Set(wxACCEL_ALT,  wxT('O'),   MathCtrl::menu_zoom_out);
  wxAcceleratorTable accel(6, entries);
  SetAcceleratorTable(accel);

  set_properties();
  do_layout();

  m_console->SetFocus();
}

void wxMaximaFrame::EvaluationQueueLength(int length)
{
  if(length != m_EvaluationQueueLength)
  {
    m_EvaluationQueueLength = length;
    if(length>0)
      SetStatusText(wxString::Format(_("%i cells in evaluation queue"),length),0);
    else
      SetStatusText(_("Welcome to wxMaxima"),0);
  }
}


void wxMaximaFrame::StatusMaximaBusy(ToolbarStatus status)
{
  if((m_StatusMaximaBusy != status) || (m_forceStatusbarUpdate))
  {
    if(!m_StatusSaving)
    {
      switch(status)
      {
      case userinput:	
        m_MenuBar->Enable(menu_remove_output,false);
        if(m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,    false);
          m_console->m_mainToolBar->ShowUserInputBitmap();
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,    true);
        }
        SetStatusText(_("Maxima got a question"), 1);
        break;
      case waiting:
        m_console->SetWorkingGroup(NULL);
        // If we evaluated a cell that produces no output we still want the
        // cell to be unselected after evaluating it.
        if(m_console->FollowEvaluation())
          m_console->SetSelection(NULL);

        m_MenuBar->Enable(menu_remove_output,true);
        if (m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,    false);
          m_console->m_mainToolBar->ShowFollowBitmap();
        }
        SetStatusText(_("Ready for user input"), 1);
        // We don't evaluate any cell right now.
        break;
      case calculating:
        m_MenuBar->Enable(menu_remove_output,false);
        if (m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,    false);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,
                                               m_console->ScrolledAwayFromEvaluation()
            );
        }
        SetStatusText(_("Maxima is calculating"), 1);
        break;
      case transferring:
        m_MenuBar->Enable(menu_remove_output,false);
        if (m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,
                                               m_console->ScrolledAwayFromEvaluation()
            );
        }
        SetStatusText(_("Reading Maxima output"), 1);
        break;	
      case parsing:
        m_MenuBar->Enable(menu_remove_output,false);
        if (m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,
                                               m_console->ScrolledAwayFromEvaluation()
            );
        }
        SetStatusText(_("Parsing output"), 1);
        break;
      case disconnected:
        m_MenuBar->Enable(menu_remove_output,false);
        if (m_console->m_mainToolBar)
        {
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
          m_console->m_mainToolBar->EnableTool(ToolBar::tb_follow,    false);
        }
        SetStatusText(_("Not connected to maxima"), 1);
        break;
      }
    }
  }
  m_StatusMaximaBusy = status;
  m_forceStatusbarUpdate = false;
}

void wxMaximaFrame::StatusSaveStart()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  SetStatusText(_("Saving..."), 1);
}

void wxMaximaFrame::StatusSaveFinished()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  if(m_StatusMaximaBusy != waiting)
    StatusMaximaBusy(m_StatusMaximaBusy);
  else
    SetStatusText(_("Saving successful."), 1);
}

void wxMaximaFrame::StatusExportStart()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  SetStatusText(_("Exporting..."), 1);
}

void wxMaximaFrame::StatusExportFinished()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  if(m_StatusMaximaBusy != waiting)
    StatusMaximaBusy(m_StatusMaximaBusy);
  else
    SetStatusText(_("Export successful."), 1);
}

void wxMaximaFrame::StatusSaveFailed()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  SetStatusText(_("Saving failed."), 1);
}

void wxMaximaFrame::StatusExportFailed()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  SetStatusText(_("Export failed."), 1);
}

wxMaximaFrame::~wxMaximaFrame()
{
  wxString perspective = m_manager.SavePerspective();

  wxConfig::Get()->Write(wxT("AUI/perspective"), perspective);
#if defined __WXMAC__ || defined __WXMSW__
  wxConfig::Get()->Write(wxT("AUI/toolbar"),
                         GetToolBar() != NULL && GetToolBar()->IsShown());
#else
  wxConfig::Get()->Write(wxT("AUI/toolbar"), (m_console->m_mainToolBar!=NULL));
#endif
  m_manager.UnInit();
  delete m_history;
  //  delete m_console->m_structure;
  delete m_console;
}

void wxMaximaFrame::set_properties()
{
#if defined (__WXMSW__)
  SetIcon(wxICON(icon0));
#elif defined (__WXGTK__)
  wxString icon(wxT(PREFIX));
  icon += wxT("/share/wxMaxima/wxmaxima.png");
  SetIcon(wxIcon(icon, wxBITMAP_TYPE_PNG));
#endif
#ifndef __WXMAC__
  SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) + _("[ unsaved ]"));
#else
  SetTitle(_("untitled"));
#endif

  m_console->SetBackgroundColour(wxColour(wxT("WHITE")));
  m_console->SetMinSize(wxSize(100, 100));
  SetStatusText(_("Welcome to wxMaxima"), 0);
}

void wxMaximaFrame::do_layout()
{
  m_manager.AddPane(m_console,
                    wxAuiPaneInfo().Name(wxT("console")).
                    Center().
                    CloseButton(false).
                    CaptionVisible(false).
                    PaneBorder(false));

  m_manager.AddPane(m_history,
                    wxAuiPaneInfo().Name(wxT("history")).
                    Caption(_("History")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Right());

  m_manager.AddPane(m_console->m_structure,
                    wxAuiPaneInfo().Name(wxT("structure")).
                    Caption(_("Table of Contents")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Right());

  m_manager.AddPane(m_xmlInspector,
                    wxAuiPaneInfo().Name(wxT("XmlInspector")).
                    Caption(_("Raw XML monitor")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Right());

  m_manager.AddPane(CreateStatPane(),
                    wxAuiPaneInfo().Name(wxT("stats")).
                    Caption(_("Statistics")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Fixed().
                    Left());

  m_manager.AddPane(CreateMathPane(),
                    wxAuiPaneInfo().Name(wxT("math")).
                    Caption(_("General Math")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Fixed().
                    Left());

  m_manager.AddPane(CreateFormatPane(),
                    wxAuiPaneInfo().Name(wxT("format")).
                    Caption(_("Insert")).
                    Show(false).
                    TopDockable(true).
                    BottomDockable(true).
                    PaneBorder(true).
                    Fixed().
                    Left());

  wxConfigBase *config = wxConfig::Get();
  bool loadPanes = true;
  config->Read(wxT("AUI/savePanes"), &loadPanes);

  if (loadPanes) {
    wxString perspective;
    bool toolbar = true;
    if (config->Read(wxT("AUI/perspective"), &perspective))
    {
      if(!perspective.IsEmpty())
        m_manager.LoadPerspective(perspective);
    }
    config->Read(wxT("AUI/toolbar"), &toolbar);
    ShowToolBar(toolbar);
  }
  else
    m_manager.Update();
}

void wxMaximaFrame::SetupMenu()
{
  m_MenuBar = new wxMenuBar();

#if defined __WXGTK20__
  wxMenuItem *tmp_menu_item;
#define APPEND_MENU_ITEM(menu, id, label, help, stock)                  \
  tmp_menu_item = new wxMenuItem((menu), (id), (label), (help), wxITEM_NORMAL); \
  tmp_menu_item->SetBitmap(wxArtProvider::GetBitmap((stock), wxART_MENU)); \
  (menu)->Append(tmp_menu_item);
#else
#define APPEND_MENU_ITEM(menu, id, label, help, stock)  \
  (menu)->Append((id), (label), (help), wxITEM_NORMAL);
#endif

  // File menu
  m_FileMenu = new wxMenu;
#if defined __WXMAC__
  m_FileMenu->Append(mac_newId, _("New\tCtrl-N"),
                     _("Open a new window"));
#else
  APPEND_MENU_ITEM(m_FileMenu, menu_new_id, _("New\tCtrl-N"),
		   _("Open a new window"), wxT("gtk-new"));
#endif
  APPEND_MENU_ITEM(m_FileMenu, menu_open_id, _("&Open...\tCtrl-O"),
                   _("Open a document"), wxT("gtk-open"));
  m_recentDocumentsMenu = new wxMenu();
  m_FileMenu->Append(menu_recent_documents, _("Open Recent"), m_recentDocumentsMenu);
#if defined __WXMAC__
  m_FileMenu->AppendSeparator();
  m_FileMenu->Append(mac_closeId, _("Close\tCtrl-W"),
                     _("Close window"), wxITEM_NORMAL);
#endif
  APPEND_MENU_ITEM(m_FileMenu, menu_save_id, _("&Save\tCtrl-S"),
                   _("Save document"), wxT("gtk-save"));
  APPEND_MENU_ITEM(m_FileMenu, menu_save_as_id, _("Save As...\tShift-Ctrl-S"),
		   _("Save document as"), wxT("gtk-save"));
  m_FileMenu->Append(menu_load_id, _("&Load Package...\tCtrl-L"),
                     _("Load a Maxima package file"), wxITEM_NORMAL);
  m_FileMenu->Append(menu_batch_id, _("&Batch File...\tCtrl-B"),
                     _("Load a Maxima file using the batch command"), wxITEM_NORMAL);
  m_FileMenu->Append(menu_export_html, _("&Export..."),
                     _("Export document to a HTML or pdfLaTeX file"), wxITEM_NORMAL);
  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_PRINT, _("&Print...\tCtrl-P"),
                   _("Print document"), wxT("gtk-print"));

  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_EXIT, _("E&xit\tCtrl-Q"),
                   _("Exit wxMaxima"), wxT("gtk-quit"));
  m_MenuBar->Append(m_FileMenu, _("&File"));

  m_EditMenu = new wxMenu;
  m_EditMenu->Append(menu_undo, _("Undo\tCtrl-Z"),
                     _("Undo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_redo, _("Redo\tCtrl-Y"),
                     _("Redo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_cut, _("Cut\tCtrl-X"),
                     _("Cut selection"),
                     wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_EditMenu, menu_copy_from_console, _("&Copy\tCtrl-C"),
                   _("Copy selection"), wxT("gtk-copy"));
  m_EditMenu->Append(menu_copy_text_from_console, _("Copy as Text\tCtrl-Shift-C"),
                     _("Copy selection from document as text"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_tex_from_console, _("Copy as LaTeX"),
                     _("Copy selection from document in LaTeX format"),
                     wxITEM_NORMAL);
#if defined __WXMSW__ || defined __WXMAC__
  m_EditMenu->Append(menu_copy_as_bitmap, _("Copy as Image"),
                     _("Copy selection from document as an image"),
                     wxITEM_NORMAL);
#endif
  m_EditMenu->Append(menu_paste, _("Paste\tCtrl-V"),
                     _("Paste text from clipboard"),
                     wxITEM_NORMAL);

  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_edit_find, _("Find\tCtrl-F"), _("Find and replace"), wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_select_all, _("Select All\tCtrl-A"),
                     _("Select all"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_to_file, _("Save Selection to Image..."),
                     _("Save selection from document to an image file"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(MathCtrl::popid_comment_selection, _("Comment selection\tCtrl-/"),
                     _("Comment out the currently selected text"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
#if defined __WXMAC__
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("Preferences...\tCtrl+,"),
                   _("Configure wxMaxima"), wxT("gtk-preferences"));
#else
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("C&onfigure"),
                   _("Configure wxMaxima"), wxT("gtk-preferences"));
#endif
  m_MenuBar->Append(m_EditMenu, _("&Edit"));

  // panes
  m_Maxima_Panes_Sub = new wxMenu;
  m_Maxima_Panes_Sub->AppendCheckItem(menu_show_toolbar, _("Main Toolbar\tAlt-Shift-B"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_math, _("General Math\tAlt-Shift-M"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_stats, _("Statistics\tAlt-Shift-S"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_history, _("History\tAlt-Shift-I"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_structure,  _("Table of contents\tAlt-Shift-T"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_xmlInspector,  _("XML Inspector"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_format, _("Insert Cell\tAlt-Shift-C"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->Append(menu_pane_hideall, _("Hide All Toolbars\tAlt-Shift--"), _("Hide all panes"), wxITEM_NORMAL);
  m_Maxima_Panes_Sub->AppendSeparator();
  APPEND_MENU_ITEM(m_Maxima_Panes_Sub, MathCtrl::menu_zoom_in, _("Zoom &In\tCtrl-+"),
                   _("Zoom in 10%"), wxT("gtk-zoom-in"));
  APPEND_MENU_ITEM(m_Maxima_Panes_Sub, MathCtrl::menu_zoom_out, _("Zoom Ou&t\tCtrl--"),
                   _("Zoom out 10%"), wxT("gtk-zoom-out"));
  // zoom submenu
  m_Edit_Zoom_Sub = new wxMenu;
  m_Edit_Zoom_Sub->Append(menu_zoom_80,  wxT("80%"),  _("Set zoom to 80%"),  wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_100, wxT("100%"), _("Set zoom to 100%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_120, wxT("120%"), _("Set zoom to 120%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_150, wxT("150%"), _("Set zoom to 150%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_200, wxT("200%"), _("Set zoom to 200%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_300, wxT("300%"), _("Set zoom to 300%"), wxITEM_NORMAL);

  m_Maxima_Panes_Sub->Append(wxNewId(), _("Set Zoom"), m_Edit_Zoom_Sub, _("Set Zoom"));
  m_Maxima_Panes_Sub->Append(menu_fullscreen, _("Full Screen\tAlt-Enter"),
                     _("Toggle full screen editing"),
                     wxITEM_NORMAL);

  m_MenuBar->Append(m_Maxima_Panes_Sub,_("View"));

  
  // Cell menu
  m_CellMenu = new wxMenu;
  m_CellMenu->Append(menu_evaluate, _("Evaluate Cell(s)"),
                     _("Evaluate active or selected cell(s)"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_evaluate_all_visible, _("Evaluate All Visible Cells\tCtrl-R"),
                     _("Evaluate all visible cells in the document"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_evaluate_all, _("Evaluate All Cells\tCtrl-Shift-R"),
                     _("Evaluate all cells in the document"), wxITEM_NORMAL);
  m_CellMenu->Append(ToolBar::tb_evaltillhere, _("Evaluate Cells above this point\tCtrl-Shift-P"),
                     _("Re-evaluate all cells above the one the cursor is in"), wxITEM_NORMAL);

  m_CellMenu->Append(menu_remove_output, _("Remove All Output"),
                     _("Remove output from input cells"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_insert_previous_input, _("Copy Previous Input\tCtrl-I"),
                     _("Create a new cell with previous input"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_insert_previous_output, _("Copy Previous Output\tCtrl-U"),
                     _("Create a new cell with previous output"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_autocomplete, _("Complete Word\tCtrl-K"),
                     _("Complete word"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_autocomplete_templates, _("Show Template\tCtrl-Shift-K"),
                     _("Show function template"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_insert_input, _("Insert Input &Cell"),
                     _("Insert a new input cell"));
  m_CellMenu->Append(menu_add_comment, _("Insert &Text Cell\tCtrl-1"),
                     _("Insert a new text cell"));
  m_CellMenu->Append(menu_add_title, _("Insert T&itle Cell\tCtrl-2"),
                     _("Insert a new title cell"));
  m_CellMenu->Append(menu_add_section, _("Insert &Section Cell\tCtrl-3"),
                     _("Insert a new section cell"));
  m_CellMenu->Append(menu_add_subsection, _("Insert S&ubsection Cell\tCtrl-4"),
                     _("Insert a new subsection cell"));
  m_CellMenu->Append(menu_add_subsubsection, _("Insert S&ubsubsection Cell\tCtrl-5"),
                     _("Insert a new subsubsection cell"));
  m_CellMenu->Append(menu_add_pagebreak, _("Insert Page Break"),
                     _("Insert a page break"));
  m_CellMenu->Append(menu_insert_image, _("Insert Image..."),
                     _("Insert image"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_fold_all_cells, _("Fold All\tCtrl-Alt-["),
                     _("Fold all sections"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_unfold_all_cells, _("Unfold All\tCtrl-Alt-]"),
                     _("Unfold all folded sections"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_history_previous, _("Previous Command\tAlt-Up"),
                     _("Recall previous command from history"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_history_next, _("Next Command\tAlt-Down"),
                     _("Recall next command from history"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(MathCtrl::popid_merge_cells, _("Merge Cells"),
                     _("Merge the text from two input cells into one"), wxITEM_NORMAL);
  m_CellMenu->Append(MathCtrl::popid_divide_cell, _("Divide Cell"),
                     _("Divide this input cell into two cells"), wxITEM_NORMAL);

  m_MenuBar->Append(m_CellMenu, _("Ce&ll"));

  // Maxima menu
  m_MaximaMenu = new wxMenu;

#if defined (__WXMAC__)
  APPEND_MENU_ITEM(m_MaximaMenu, menu_interrupt_id,
                   _("&Interrupt\tCtrl-."), // command-. interrupts (mac standard)
                   _("Interrupt current computation"), wxT("gtk-stop"));
#else
  APPEND_MENU_ITEM(m_MaximaMenu, menu_interrupt_id,
                   _("&Interrupt\tCtrl-G"),
                   _("Interrupt current computation"), wxT("gtk-stop"));
#endif
  APPEND_MENU_ITEM(m_MaximaMenu, ToolBar::menu_restart_id,
                   _("&Restart Maxima"), _("Restart Maxima"), wxT("gtk-refresh"));
  m_MaximaMenu->Append(menu_soft_restart, _("&Clear Memory"),
                       _("Delete all values from memory"), wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_MaximaMenu, menu_add_path, _("Add to &Path..."),
                   _("Add a directory to search path"), wxT("gtk-add"));

  m_MaximaMenu->AppendSeparator();
  m_MaximaMenu->Append(menu_functions, _("Show &Functions"),
                       _("Show defined functions"), wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_fun_def, _("Show &Definition..."),
                       _("Show definition of a function"),
                       wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_variables, _("Show &Variables"),
                       _("Show defined variables"), wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_clear_fun, _("Delete F&unction..."),
                       _("Delete a function"), wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_clear_var, _("Delete V&ariable..."),
                       _("Delete a variable"), wxITEM_NORMAL);

  m_MaximaMenu->AppendSeparator();
  m_MaximaMenu->Append(menu_time, _("Toggle &Time Display"),
                       _("Display time used for evaluation"),
                       wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_display, _("Change &2d Display"),
                       _("Change the 2d display algorithm used to display math output"),
                       wxITEM_NORMAL);
  m_MaximaMenu->Append(menu_texform, _("Display Te&X Form"),
                       _("Display last result in TeX form"), wxITEM_NORMAL);
  m_MaximaMenu->AppendSeparator();
  m_MaximaMenu->Append(menu_triggerEvaluation, _("Manually trigger evaluation"),
                       _("If maxima ever finishes evaluating without wxMaxima realizing this this menu item can force wxMaxima to try to send commands to maxima again."), wxITEM_NORMAL);
  m_MenuBar->Append(m_MaximaMenu, _("&Maxima"));

  // Equations menu
  m_EquationsMenu = new wxMenu;
  m_EquationsMenu->Append(menu_solve, _("&Solve..."),
                          _("Solve equation(s)"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_solve_to_poly, _("Solve (to_poly)..."),
                          _("Solve equation(s) with to_poly_solve"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_solve_num, _("&Find Root..."),
                          _("Find a root of an equation on an interval"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_allroots, _("Roots of &Polynomial"),
                          _("Find all roots of a polynomial"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_bfallroots, _("Roots of Polynomial (bfloat)"),
                          _("Find all roots of a polynomial (bfloat)"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_realroots, _("&Roots of Polynomial (Real)"),
                          _("Find real roots of a polynomial"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_solve_lin, _("Solve &Linear System..."),
                          _("Solve linear system of equations"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_solve_algsys, _("Solve &Algebraic System..."),
                          _("Solve algebraic system of equations"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_eliminate, _("&Eliminate Variable..."),
                          _("Eliminate a variable from a system "
                            "of equations"),
                          wxITEM_NORMAL);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(menu_solve_ode, _("Solve &ODE..."),
                          _("Solve ordinary differential equation "
                            "of maximum degree 2"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_ivp_1, _("Initial Value Problem (&1)..."),
                          _("Solve initial value problem for first"
                            " degree ODE"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_ivp_2, _("Initial Value Problem (&2)..."),
                          _("Solve initial value problem for second "
                            "degree ODE"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_bvp, _("&Boundary Value Problem..."),
                          _("Solve boundary value problem for second "
                            "degree ODE"), wxITEM_NORMAL);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(menu_solve_de, _("Solve ODE with Lapla&ce..."),
                          _("Solve ordinary differential equations "
                            "with Laplace transformation"), wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_atvalue, _("A&t Value..."),
                          _("Setup atvalues for solving ODE with "
                            "Laplace transformation"), wxITEM_NORMAL);
  m_MenuBar->Append(m_EquationsMenu, _("E&quations"));

  // Algebra menu
  m_Algebra_Menu = new wxMenu;
  m_Algebra_Menu->Append(menu_gen_mat, _("&Generate Matrix..."),
                         _("Generate a matrix from a 2-dimensional array"),
                         wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_gen_mat_lambda, _("Generate Matrix from Expression..."),
                         _("Generate a matrix from a lambda expression"),
                         wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_enter_mat, _("&Enter Matrix..."),
                         _("Enter a matrix"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_invert_mat, _("&Invert Matrix"),
                         _("Compute the inverse of a matrix"),
                         wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_cpoly, _("&Characteristic Polynomial..."),
                         _("Compute the characteristic polynomial "
                           "of a matrix"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_determinant, _("&Determinant"),
                         _("Compute the determinant of a matrix"),
                         wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_eigen, _("Eigen&values"),
                         _("Find eigenvalues of a matrix"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_eigvect, _("Eige&nvectors"),
                         _("Find eigenvectors of a matrix"),
                         wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_adjoint_mat, _("Ad&joint Matrix"),
                         _("Compute the adjoint matrix"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_transpose, _("&Transpose Matrix"),
                         _("Transpose a matrix"), wxITEM_NORMAL);
  m_Algebra_Menu->AppendSeparator();
  m_Algebra_Menu->Append(menu_make_list, _("Make &List..."),
                         _("Make list from expression"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_apply, _("&Apply to List..."),
                         _("Apply function to a list"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_map, _("&Map to List..."),
                         _("Map function to a list"), wxITEM_NORMAL);
  m_Algebra_Menu->Append(menu_map_mat, _("Ma&p to Matrix..."),
                         _("Map function to a matrix"), wxITEM_NORMAL);
  m_MenuBar->Append(m_Algebra_Menu, _("&Algebra"));

  // Calculus menu
  m_CalculusMenu = new wxMenu;
  m_CalculusMenu->Append(menu_integrate, _("&Integrate..."),
                         _("Integrate expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_risch, _("Risch Integration..."),
                         _("Integrate expression with Risch algorithm"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_change_var, _("C&hange Variable..."),
                         _("Change variable in integral or sum"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_diff, _("&Differentiate..."),
                         _("Differentiate expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_limit, _("Find &Limit..."),
                         _("Find a limit of an expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_lbfgs, _("Find Minimum..."),
                         _("Find a (unconstrained) minimum of an expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_series, _("Get &Series..."),
                         _("Get the Taylor or power series of expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_pade, _("P&ade Approximation..."),
                         _("Pade approximation of a Taylor series"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_sum, _("Calculate Su&m..."),
                         _("Calculate sums"), wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_product, _("Calculate &Product..."),
                         _("Calculate products"), wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_laplace, _("Laplace &Transform..."),
                         _("Get Laplace transformation of an expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_ilt, _("Inverse Laplace T&ransform..."),
                         _("Get inverse Laplace transformation of an expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_gcd, _("&Greatest Common Divisor..."),
                         _("Compute the greatest common divisor"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_lcm, _("Least Common Multiple..."),
                         _("Compute the least common multiple "
                           "(do load(functs) before using)"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_divide, _("Di&vide Polynomials..."),
                         _("Divide numbers or polynomials"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_partfrac, _("Partial &Fractions..."),
                         _("Decompose rational function to partial fractions"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(menu_continued_fraction, _("&Continued Fraction"),
                         _("Compute continued fraction of a value"),
                         wxITEM_NORMAL);
  m_MenuBar->Append(m_CalculusMenu, _("&Calculus"));

  // Simplify menu
  m_SimplifyMenu = new wxMenu;
  m_SimplifyMenu->Append(menu_ratsimp, _("&Simplify Expression"),
                         _("Simplify rational expression"), wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_radsimp, _("Simplify &Radicals"),
                         _("Simplify expression containing radicals"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_factor, _("&Factor Expression"),
                         _("Factor an expression"), wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_gfactor, _("Factor Complex"),
                         _("Factor an expression in Gaussian numbers"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_expand, _("&Expand Expression"),
                         _("Expand an expression"), wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_logexpand, _("Expand Logarithms"),
                         _("Convert logarithm of product to sum of logarithms"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_logcontract, _("Contract Logarithms"),
                         _("Convert sum of logarithms to logarithm of product"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->AppendSeparator();
  // Factorials and gamma 
  m_Simplify_Gamma_Sub = new wxMenu;
  m_Simplify_Gamma_Sub->Append(menu_to_fact, _("Convert to &Factorials"),
                               _("Convert binomials, beta and gamma function to factorials"),
                               wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(menu_to_gamma, _("Convert to &Gamma"),
                               _("Convert binomials, factorials and beta function to gamma function"),
                               wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(menu_factsimp, _("&Simplify Factorials"),
                               _("Simplify an expression containing factorials"),
                               wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(menu_factcomb, _("&Combine Factorials"),
                               _("Combine factorials in an expression"),
                               wxITEM_NORMAL);
  m_SimplifyMenu->Append(wxNewId(), _("Factorials and &Gamma"),
                         m_Simplify_Gamma_Sub,
                         _("Functions for simplifying factorials and gamma function"));
  // Trigonometric submenu
  m_Simplify_Trig_Sub = new wxMenu;
  m_Simplify_Trig_Sub->Append(menu_trigsimp, _("&Simplify Trigonometric"),
                              _("Simplify trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(menu_trigreduce, _("&Reduce Trigonometric"),
                              _("Reduce trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(menu_trigexpand, _("&Expand Trigonometric"),
                              _("Expand trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(menu_trigrat, _("&Canonical Form"),
                              _("Convert trigonometric expression to canonical quasilinear form"),
                              wxITEM_NORMAL);
  m_SimplifyMenu->Append(wxNewId(), _("&Trigonometric Simplification"),
                         m_Simplify_Trig_Sub,
                         _("Functions for simplifying trigonometric expressions"));
  // Complex submenu
  m_Simplify_Complex_Sub = new wxMenu;
  m_Simplify_Complex_Sub->Append(menu_rectform, _("Convert to &Rectform"),
                                 _("Convert complex expression to rect form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(menu_polarform, _("Convert to &Polarform"),
                                 _("Convert complex expression to polar form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(menu_realpart, _("Get Real P&art"),
                                 _("Get the real part of complex expression"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(menu_imagpart, _("Get &Imaginary Part"),
                                 _("Get the imaginary part of complex expression"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(menu_demoivre, _("&Demoivre"),
                                 _("Convert exponential function of imaginary argument to trigonometric form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(menu_exponentialize, _("&Exponentialize"),
                                 _("Convert trigonometric functions to exponential form"),
                                 wxITEM_NORMAL);
  m_SimplifyMenu->Append(wxNewId(), _("&Complex Simplification"),
                         m_Simplify_Complex_Sub,
                         _("Functions for complex simplification"));
  m_SimplifyMenu->AppendSeparator();
  m_SimplifyMenu->Append(menu_subst, _("Substitute..."),
                         _("Make substitution in expression"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_nouns, _("Evaluate &Noun Forms"),
                         _("Evaluate all noun forms in expression"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_talg, _("Toggle &Algebraic Flag"),
                         _("Toggle algebraic flag"), wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_tellrat, _("Add Algebraic E&quality..."),
                         _("Add equality to the rational simplifier"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(menu_modulus, _("&Modulus Computation..."),
                         _("Setup modulus computation"), wxITEM_NORMAL);
  m_MenuBar->Append(m_SimplifyMenu, _("&Simplify"));

  // Plot menu
  m_PlotMenu = new wxMenu;
  m_PlotMenu->Append(gp_plot2, _("Plot &2d..."),
                     _("Plot in 2 dimensions"), wxITEM_NORMAL);
  m_PlotMenu->Append(gp_plot3, _("Plot &3d..."),
                     _("Plot in 3 dimensions"), wxITEM_NORMAL);
  m_PlotMenu->Append(menu_plot_format, _("Plot &Format..."),
                     _("Set plot format"), wxITEM_NORMAL);
  m_MenuBar->Append(m_PlotMenu, _("&Plot"));

  // Numeric menu
  m_NumericMenu = new wxMenu;
  m_NumericMenu->Append(menu_num_out, _("Toggle &Numeric Output"),
                        _("Toggle numeric output"), wxITEM_NORMAL);
  m_NumericMenu->Append(menu_to_float, _("To &Float"),
                        _("Calculate float value of the last result"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(menu_to_bfloat, _("To &Bigfloat"),
                        _("Calculate bigfloat value of the last result"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(menu_to_numer, _("To Numeri&c\tCtrl+Shift+N"),
                        _("Calculate numeric value of the last result"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(menu_set_precision, _("Set bigfloat &Precision..."),
                        _("Set the precision for numbers that are defined as bigfloat. Such numbers can be generated by entering 1.5b12 or as bfloat(1.234)"),
                        wxITEM_NORMAL);
  m_MenuBar->Append(m_NumericMenu, _("&Numeric"));

  // Help menu
  m_HelpMenu = new wxMenu;
#if defined __WXMAC__
  m_HelpMenu->Append(wxID_HELP, _("wxMaxima &Help\tCtrl+?"),
                     _("Show wxMaxima help"), wxITEM_NORMAL);
#else
  APPEND_MENU_ITEM(m_HelpMenu, wxID_HELP, _("wxMaxima &Help\tF1"),
                   _("Show wxMaxima help"), wxT("gtk-help"));
#endif
  m_HelpMenu->Append(menu_maximahelp, _("&Maxima help"),
                     _("The offline manual of maxima"),
                     wxITEM_NORMAL);
  m_HelpMenu->Append(menu_example, _("&Example..."),
                     _("Show an example of usage"),
                     wxITEM_NORMAL);
  m_HelpMenu->Append(menu_apropos, _("&Apropos..."),
                     _("Show commands similar to"),
                     wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_HelpMenu, menu_show_tip, _("Show &Tips..."),
                   _("Show a tip"), wxART_TIP);
  m_HelpMenu->AppendSeparator();
  m_HelpMenu->Append(menu_help_tutorials, _("Tutorials"),
                     _("Online tutorials"), wxITEM_NORMAL);
  m_HelpMenu->AppendSeparator();
  m_HelpMenu->Append(menu_build_info, _("Build &Info"),
                     _("Info about Maxima build"), wxITEM_NORMAL);
  m_HelpMenu->Append(menu_bug_report, _("&Bug Report"),
                     _("Report bug"), wxITEM_NORMAL);
  m_HelpMenu->AppendSeparator();
  m_HelpMenu->Append(menu_check_updates, _("Check for Updates"),
                     _("Check if a newer version of wxMaxima/Maxima exist."),
                     wxITEM_NORMAL);
#ifndef __WXMAC__
  m_HelpMenu->AppendSeparator();
#endif
  APPEND_MENU_ITEM(m_HelpMenu, wxID_ABOUT,
#ifndef __WXMAC__
                   _("About"),
#else
                   _("About wxMaxima"),
#endif
                   _("About wxMaxima"), wxT("stock_about"));
  m_MenuBar->Append(m_HelpMenu, _("&Help"));

  SetMenuBar(m_MenuBar);

#undef APPEND_MENU_ITEM

}

void wxMaximaFrame::LoadRecentDocuments()
{
  wxConfigBase *config = wxConfig::Get();
  m_recentDocuments.Clear();

  for (int i=0; i<10; i++)
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

  // Delete previous recent documents
  for (unsigned int i=0; i<10; i++) {
    wxString recent = wxString::Format(wxT("RecentDocuments/document_%d"), i);
    config->DeleteEntry(recent);
  }

  // Save new recent documents
  for (unsigned int i=0; i<m_recentDocuments.Count(); i++) {
    wxString recent = wxString::Format(wxT("RecentDocuments/document_%d"), i);
    config->Write(recent, m_recentDocuments[i]);
  }
}

void wxMaximaFrame::UpdateRecentDocuments()
{
  // Iterate through all the entries to the recent documents menu.
  for (int i=menu_recent_document_0; i<= menu_recent_document_9; i++)
  {
    if (m_recentDocumentsMenu->FindItem(i) != NULL)
    {
      wxMenuItem *item = m_recentDocumentsMenu->Remove(i);
      delete item;
      item = NULL;
    }

    if (i-menu_recent_document_0 < (signed)m_recentDocuments.Count())
    {
      wxFileName filename(m_recentDocuments[i - menu_recent_document_0]);
      wxString path(filename.GetPath()), fullname(filename.GetFullName());
      wxString label(fullname + wxT("   [ ") + path + wxT(" ]"));

      m_recentDocumentsMenu->Append(i, label);
    }
  }

  SaveRecentDocuments();
}

void wxMaximaFrame::AddRecentDocument(wxString file)
{
  wxFileName FileName=file;
  FileName.MakeAbsolute();
  wxString CanonicalFilename=FileName.GetFullPath();

  // Append the current document to the list of recent documents
  // or move it to the top, if it is already there.
  if (m_recentDocuments.Index(CanonicalFilename) != wxNOT_FOUND)
    m_recentDocuments.Remove(CanonicalFilename);
  m_recentDocuments.Insert(CanonicalFilename, 0);

  UpdateRecentDocuments();
}

void wxMaximaFrame::RemoveRecentDocument(wxString file)
{
  m_recentDocuments.Remove(file);

  UpdateRecentDocuments();
}

bool wxMaximaFrame::IsPaneDisplayed(Event id)
{
  bool displayed = false;

  switch (id) {
  case menu_pane_math:
    displayed = m_manager.GetPane(wxT("math")).IsShown();
    break;
  case menu_pane_history:
    displayed = m_manager.GetPane(wxT("history")).IsShown();
    break;
  case menu_pane_structure:
    displayed = m_manager.GetPane(wxT("structure")).IsShown();
    break;
  case menu_pane_xmlInspector:
    displayed = m_manager.GetPane(wxT("XmlInspector")).IsShown();
    break;
  case menu_pane_stats:
    displayed = m_manager.GetPane(wxT("stats")).IsShown();
    break;
  case menu_pane_format:
    displayed = m_manager.GetPane(wxT("format")).IsShown();
    break;
  default:
    wxASSERT(false);
    break;
  }

  return displayed;
}

void wxMaximaFrame::ShowPane(Event id, bool show)
{
  switch (id) {
  case menu_pane_math:
    m_manager.GetPane(wxT("math")).Show(show);
    break;
  case menu_pane_history:
    m_manager.GetPane(wxT("history")).Show(show);
    break;
  case menu_pane_structure:
  {
    m_manager.GetPane(wxT("structure")).Show(show);
    m_console->m_structure->Update(m_console->GetTree(),m_console->GetHCaret());
    break;
  }
  case menu_pane_xmlInspector:
  {
    m_manager.GetPane(wxT("XmlInspector")).Show(show);
    break;
  }
  case menu_pane_stats:
    m_manager.GetPane(wxT("stats")).Show(show);
    break;
  case menu_pane_format:
    m_manager.GetPane(wxT("format")).Show(show);
    break;
  case menu_pane_hideall:
    m_manager.GetPane(wxT("math")).Show(false);
    m_manager.GetPane(wxT("history")).Show(false);
    m_manager.GetPane(wxT("structure")).Show(false);
    m_manager.GetPane(wxT("XmlInspector")).Show(false);
    m_manager.GetPane(wxT("stats")).Show(false);
    m_manager.GetPane(wxT("format")).Show(false);
    break;
  default:
    wxASSERT(false);
    break;
  }

  m_manager.Update();
}

wxPanel* wxMaximaFrame::CreateMathPane()
{
  wxGridSizer *grid = new wxGridSizer(2);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
#if defined __WXMSW__
  int border = 1;
#else
  int border = 0;
#endif

  grid->Add(new wxButton(panel, button_ratsimp, _("Simplify")), 0, style, border);
  grid->Add(new wxButton(panel, button_radcan, _("Simplify (r)")), 0, style, border);
  grid->Add(new wxButton(panel, button_factor, _("Factor")), 0, style, border);
  grid->Add(new wxButton(panel, button_expand, _("Expand")), 0, style, border);
  grid->Add(new wxButton(panel, button_rectform, _("Rectform")), 0, style, border);
  grid->Add(new wxButton(panel, button_subst, _("Subst...")), 0, style, border);
  grid->Add(new wxButton(panel, button_trigrat, _("Canonical (tr)")), 0, style, border);
  grid->Add(new wxButton(panel, button_trigsimp, _("Simplify (tr)")), 0, style, border);
  grid->Add(new wxButton(panel, button_trigexpand, _("Expand (tr)")), 0, style, border);
  grid->Add(new wxButton(panel, button_trigreduce, _("Reduce (tr)")), 0, style, border);
  grid->Add(new wxButton(panel, button_solve, _("Solve...")), 0, style, border);
  grid->Add(new wxButton(panel, button_solve_ode, _("Solve ODE...")), 0, style, border);
  grid->Add(new wxButton(panel, button_diff, _("Diff...")), 0, style, border);
  grid->Add(new wxButton(panel, button_integrate, _("Integrate...")), 0, style, border);
  grid->Add(new wxButton(panel, button_limit, _("Limit...")), 0, style, border);
  grid->Add(new wxButton(panel, button_taylor, _("Series...")), 0, style, border);
  grid->Add(new wxButton(panel, button_plot2, _("Plot 2D...")), 0, style, border);
  grid->Add(new wxButton(panel, button_plot3, _("Plot 3D...")), 0, style, border);

  panel->SetSizer(grid);
  grid->Fit(panel);
  grid->SetSizeHints(panel);

  return panel;
}

wxPanel* wxMaximaFrame::CreateStatPane()
{
  wxGridSizer *grid1 = new wxGridSizer(2);
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *box1 = new wxBoxSizer(wxVERTICAL);
  wxGridSizer *grid2 = new wxGridSizer(2);
  wxGridSizer *grid3 = new wxGridSizer(2);
  wxBoxSizer *box3 = new wxBoxSizer(wxVERTICAL);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
#if defined __WXMSW__
  int border = 1;
#else
  int border = 0;
#endif
  int sizerBorder = 2;

  grid1->Add(new wxButton(panel, menu_stats_mean, _("Mean...")), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_median, _("Median...")), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_var, _("Variance...")), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_dev, _("Deviation...")), 0, style, border);

  box->Add(grid1, 0, style, sizerBorder);

  box1->Add(new wxButton(panel, menu_stats_tt1, _("Mean Test...")), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_tt2, _("Mean Difference Test...")), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_tnorm, _("Normality Test...")), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_linreg, _("Linear Regression...")), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_lsquares, _("Least Squares Fit...")), 0, style, border);

  box->Add(box1, 0, style, sizerBorder);

  grid2->Add(new wxButton(panel, menu_stats_histogram, _("Histogram...")), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_scatterplot, _("Scatterplot...")), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_barsplot, _("Barsplot...")), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_piechart, _("Piechart...")), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_boxplot, _("Boxplot...")), 0, style, border);

  box->Add(grid2, 0, style, sizerBorder);

  grid3->Add(new wxButton(panel, menu_stats_readm, _("Read Matrix...")), 0, style, border);
  grid3->Add(new wxButton(panel, menu_stats_enterm, _("Enter Matrix...")), 0, style, border);

  box->Add(grid3, 0, style, sizerBorder);

  box3->Add(new wxButton(panel, menu_stats_subsample, _("Subsample...")), 0, style, border);

  box->Add(box3, 0, style, sizerBorder);

  panel->SetSizer(box);
  box->Fit(panel);
  box->SetSizeHints(panel);

  return panel;
}

wxPanel *wxMaximaFrame::CreateFormatPane()
{
  wxGridSizer *grid = new wxGridSizer(2);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
#if defined __WXMSW__
  int border = 1;
#else
  int border = 0;
#endif

  grid->Add(new wxButton(panel, menu_format_text, _("Text")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_title, _("Title")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_subsection, _("Subsection")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_subsubsection, _("Subsubsection")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_section, _("Section")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_image, _("Image")), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_pagebreak, _("Pagebreak")), 0, style, border);

  panel->SetSizer(grid);
  grid->Fit(panel);
  grid->SetSizeHints(panel);

  return panel;
}

void wxMaximaFrame::ShowToolBar(bool show)
{
#if defined __WXMAC__ || defined __WXMSW__
  wxToolBar *tbar = GetToolBar();
  if (tbar == NULL) {
    tbar = CreateToolBar();
    m_console->m_mainToolBar = new ToolBar(tbar);
    SetToolBar(m_console->m_mainToolBar->GetToolBar());
  }
  tbar->Show(show);
#if defined __WXMSW__
  PostSizeEvent();
#endif
#else
  if (show) {
    if (m_console->m_mainToolBar == NULL) {
      wxToolBar *tbar = CreateToolBar();
      m_console->m_mainToolBar=new ToolBar(tbar);
    }
    SetToolBar(m_console->m_mainToolBar->GetToolBar());
  }
  else
  {
    if(m_console->m_mainToolBar)
    {
      if(m_console->m_mainToolBar->GetToolBar())
        delete m_console->m_mainToolBar->GetToolBar();
      delete m_console->m_mainToolBar;
      m_console->m_mainToolBar=NULL;
      SetToolBar(NULL);
    }
  }
#endif
}

