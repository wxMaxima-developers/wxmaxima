// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2011-2011 cw.ahbong <cwahbong@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class wxMaximaFrame

  wxMaximaFrame is responsible for everything that is displayed around the actual
  worksheet - which is displayed by Worksheet and whose logic partially is defined in
  wxMaxima.
 */
#include "wxMaximaFrame.h"
#include "LogPane.h"

#include <wx/artprov.h>
#include <wx/config.h>
#include <wx/image.h>
#include <wx/filename.h>
#include <wx/fileconf.h>
#include <wx/stdpaths.h>
#include <wx/persist/toplevel.h>
#include <wx/display.h>
#include <wx/wupdlock.h>
#include "wxMaximaIcon.h"

wxMaximaFrame::wxMaximaFrame(wxWindow *parent, int id, const wxString &title,
                             const wxPoint &pos, const wxSize &size,
                             long style, bool becomeLogTarget) :
  wxFrame(parent, id, title, pos, size, style),
  m_recentDocuments(wxT("document")),
  m_unsavedDocuments(wxT("unsaved")),
  m_recentPackages(wxT("packages"))
{
  m_bytesFromMaxima = 0;
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  wxWindowUpdateLocker noUpdates(this);

  // Add some shortcuts that aren't automatically set by menu entries.
  wxAcceleratorEntry entries[23];
  entries[0].Set(wxACCEL_CTRL, WXK_TAB, menu_autocomplete);
  entries[1].Set(wxACCEL_CTRL, WXK_SPACE, menu_autocomplete);
  entries[2].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_TAB, menu_autocomplete_templates);
  entries[3].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_SPACE, menu_autocomplete_templates);
  entries[4].Set(wxACCEL_ALT, wxT('I'), Worksheet::menu_zoom_in);
  entries[5].Set(wxACCEL_ALT, wxT('O'), Worksheet::menu_zoom_out);
  entries[6].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_ESCAPE, menu_convert_to_code);
  entries[7].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('1'), menu_convert_to_comment);
  entries[8].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('2'), menu_convert_to_title);
  entries[9].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('3'), menu_convert_to_section);
  entries[10].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('4'), menu_convert_to_subsection);
  entries[11].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('5'), menu_convert_to_subsubsection);
  entries[12].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxT('6'), menu_convert_to_heading5);
  // wxWidgets can read the hotkeys that now follow from the menus. But it doesn't
  // do so on debian if the input method is xim.
  entries[13].Set(wxACCEL_CTRL, wxT('+'), Worksheet::menu_zoom_in);
  entries[14].Set(wxACCEL_CTRL, wxT('-'), Worksheet::menu_zoom_out);
  entries[15].Set(wxACCEL_CTRL, wxT('0'), menu_insert_input);
  entries[16].Set(wxACCEL_CTRL, wxT('1'), menu_add_comment);
  entries[17].Set(wxACCEL_CTRL, wxT('2'), menu_add_title);
  entries[18].Set(wxACCEL_CTRL, wxT('3'), menu_add_section);
  entries[19].Set(wxACCEL_CTRL, wxT('4'), menu_add_subsection);
  entries[20].Set(wxACCEL_CTRL, wxT('5'), menu_add_subsubsection);
  entries[21].Set(wxACCEL_CTRL, wxT('6'), menu_add_heading5);
  entries[22].Set(wxACCEL_CTRL, wxT('7'), menu_add_heading6);
  wxAcceleratorTable accel(23, entries);
  SetAcceleratorTable(accel);
    
  // Redirect all debug messages to a dockable panel and output some info
  // about this program.
  m_logPane = new LogPane(this, -1, becomeLogTarget);
  wxLogMessage(wxString::Format(_("wxMaxima version %s"), GITVERSION));
  #ifdef __WXMSW__
  wxLogMessage(_("Running on MS Windows"));
  #endif
  #ifdef __WXMOTIF__
  wxLogMessage(_("Running on Motif"));
  #endif
  #ifdef __WXDFB__
  wxLogMessage(_("Running on DirectFB"));
  #endif
  #ifdef __WXUNIVERSAL__
  wxLogMessage(_("Running on the universal wxWidgets port"));
  #endif
  #ifdef __WXOSX__
  wxLogMessage(_("Running on Mac OS"));
  #endif
  #ifdef __WXGTK__
  #ifdef __WXGTK3__
  wxLogMessage(_("wxWidgets is using GTK 3"));
  #else
  wxLogMessage(_("wxWidgets is using GTK 2"));
  #endif
  #endif

  // Make wxWidgets remember the size and position of the wxMaxima window
  SetName(title);
  if(!wxPersistenceManager::Get().RegisterAndRestore(this))
  {
    // We don't remember the window size from a previous wxMaxima run
    // => Make sure the window is at least half-way big enough to make sense.
    wxSize winSize = wxSize(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )*.75,
                     wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )*.75);
    if (winSize.x<800) winSize.x=800;
    if (winSize.y<600) winSize.y=600;
    SetSize(winSize);
  }

  // Some default values
  m_isNamed = false;
  m_updateEvaluationQueueLengthDisplay = true;
  m_recentDocumentsMenu = NULL;
  m_recentPackagesMenu = NULL;
  m_userSymbols = NULL;
  m_drawPane = NULL;
  m_EvaluationQueueLength = 0;
  m_commandsLeftInCurrentCell = 0;
  m_forceStatusbarUpdate = false;
  m_manager.SetManagedWindow(this);

  // Better support for low-resolution netbook screens.
  wxDialog::EnableLayoutAdaptation(wxDIALOG_ADAPTATION_MODE_ENABLED);

  // Now it is time to construct the window contents.
  
  // console
  m_worksheet = new Worksheet(this, -1);

  // history
  m_history = new History(this, -1);

  // The table of contents
  m_worksheet->m_tableOfContents = new TableOfContents(this, -1, &m_worksheet->m_configuration);

  m_xmlInspector = new XmlInspector(this, -1);
  SetupMenu();

  m_statusBar = new StatusBar(this, -1);
  SetStatusBar(m_statusBar);
  m_StatusSaving = false;
  // If we need to set the status manually for the first time using StatusMaximaBusy
  // we first have to manually set the last state to something else.
  m_StatusMaximaBusy = calculating;
  StatusMaximaBusy(waiting);

  #if defined (__WXMSW__)
  // On Windows the taskbar icon needs to reside in the Ressources file the linker
  // includes. Also it needs to be in Microsoft's own .ico format =>
  // This file we don't ship with the source, but take it from the Resources
  // file instead.
  SetIcon(wxICON(icon0));
#elif defined (__WXGTK__)
  // This icon we include in the executable [in its compressed form] so we avoid
  // the questions "Was the icon file packaged with wxMaxima?" and "Can we
  // find it?".
  SetIcon(wxMaximaIcon());
#endif
#ifndef __WXOSX__
  SetTitle(wxString::Format(_("wxMaxima %s "), wxT(GITVERSION)) + _("[ unsaved ]"));
#else
  SetTitle(_("untitled"));
#endif

  m_manager.AddPane(m_worksheet,
                    wxAuiPaneInfo().Name(wxT("console")).
                            Center().
                            CloseButton(false).
                            CaptionVisible(false).
                            MinSize(wxSize(100,100)).
                            PaneBorder(false));

  m_manager.AddPane(m_history,
                    wxAuiPaneInfo().Name(wxT("history")).
                            Show(false).CloseButton().PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Right());

  m_manager.AddPane(m_worksheet->m_tableOfContents,
                    wxAuiPaneInfo().Name(wxT("structure")).
                            Show(true).CloseButton().PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Right());

  m_manager.AddPane(m_xmlInspector,
                    wxAuiPaneInfo().Name(wxT("XmlInspector")).
                            Show(false).CloseButton().PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Right());

  m_manager.AddPane(CreateStatPane(),
                    wxAuiPaneInfo().Name(wxT("stats")).
                            Show(false).CloseButton().PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Left());

  wxPanel *greekPane = CreateGreekPane();
  m_manager.AddPane(greekPane,
                    wxAuiPaneInfo().Name(wxT("greek")).
                            Show(false).CloseButton().PinButton().
                            DockFixed(false).
                            Gripper(false).
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            MinSize(greekPane->GetEffectiveMinSize()).
                            BestSize(greekPane->GetEffectiveMinSize()).
                            MaxSize(greekPane->GetEffectiveMinSize()).
                            FloatingSize(greekPane->GetEffectiveMinSize()).
                            Left());

  m_manager.AddPane(m_logPane,
                    wxAuiPaneInfo().Name(wxT("log")).
                            Show(false).CloseButton().PinButton().
                            DockFixed(false).
                            Gripper(false).
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            MinSize(m_logPane->GetEffectiveMinSize()).
                            FloatingSize(m_logPane->GetEffectiveMinSize()).
                            Left());

  m_manager.AddPane(m_variablesPane = new Variablespane(this,-1),
                    wxAuiPaneInfo().Name(wxT("variables")).
                            Show(false).CloseButton().PinButton().
                            DockFixed(false).
                            Gripper(false).
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            MinSize(m_logPane->GetEffectiveMinSize()).
                            FloatingSize(m_logPane->GetEffectiveMinSize()).
                            Left());

  wxPanel *symbolsPane = CreateSymbolsPane();
  m_manager.AddPane(symbolsPane,
                    wxAuiPaneInfo().Name(wxT("symbols")).
                            Show(false).
                            DockFixed(false).CloseButton().PinButton().
                            Gripper(false).
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            MinSize(symbolsPane->GetEffectiveMinSize()).
                            BestSize(symbolsPane->GetEffectiveMinSize()).
                            MaxSize(symbolsPane->GetEffectiveMinSize()).
                            FloatingSize(symbolsPane->GetEffectiveMinSize()).
                            Left());
  m_manager.AddPane(CreateMathPane(),
                    wxAuiPaneInfo().Name(wxT("math")).
                            Show(false).CloseButton(true).PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Left());

  m_manager.AddPane(CreateFormatPane(),
                    wxAuiPaneInfo().Name(wxT("format")).
                            Show(false).CloseButton(true).PinButton().
                            TopDockable(true).
                            BottomDockable(true).
                            LeftDockable(true).
                            RightDockable(true).
                            PaneBorder(true).
                            Left());

  m_manager.AddPane(m_drawPane = new DrawPane(this, -1),
                    wxAuiPaneInfo().Name(wxT("draw")).
                    Show(false).CloseButton(true).PinButton().
                    TopDockable(true).
                    BottomDockable(true).
                    LeftDockable(true).
                    RightDockable(true).
                    PaneBorder(true).
                    Left());

  m_worksheet->m_mainToolBar = new ToolBar(this);
  
  m_manager.AddPane(m_worksheet->m_mainToolBar,
                    wxAuiPaneInfo().Name(wxT("toolbar")).
                    Top().TopDockable(true).Show(true).
                    BottomDockable(true).//ToolbarPane().
                    CaptionVisible(false).CloseButton(false).
                    LeftDockable(false).DockFixed().
                    RightDockable(false).Gripper(false).Row(1)
    );

  m_manager.GetPane(wxT("greek")) = m_manager.GetPane(wxT("greek")).
    MinSize(greekPane->GetEffectiveMinSize()).
    BestSize(greekPane->GetEffectiveMinSize()).
    Show(true).Gripper(false).CloseButton().PinButton().
    MaxSize(greekPane->GetEffectiveMinSize());

  m_manager.GetPane(wxT("log")) = m_manager.GetPane(wxT("log")).
    Show(false).Gripper(false).CloseButton().PinButton();

  m_manager.GetPane(wxT("variables")) = m_manager.GetPane(wxT("variables")).
    Gripper(false).CloseButton().PinButton();

  m_manager.GetPane(wxT("symbols")) = m_manager.GetPane(wxT("symbols")).
    MinSize(symbolsPane->GetEffectiveMinSize()).
    BestSize(symbolsPane->GetEffectiveMinSize()).
    Show(true).Gripper(false).CloseButton().PinButton().
    MaxSize(symbolsPane->GetEffectiveMinSize());

  m_manager.GetPane(wxT("draw")) = m_manager.GetPane(wxT("draw")).
    MinSize(symbolsPane->GetEffectiveMinSize()).
    BestSize(symbolsPane->GetEffectiveMinSize()).
    Show(true).CloseButton().PinButton().
    MaxSize(symbolsPane->GetEffectiveMinSize());


  // Read the perspektive (the sidebar state and positions).
  wxConfigBase *config = wxConfig::Get();
  bool loadPanes = true;
  bool toolbarEnabled = true;
  wxString perspective;
  config->Read(wxT("AUI/toolbarEnabled"), &toolbarEnabled);
  config->Read(wxT("AUI/savePanes"), &loadPanes);
  config->Read(wxT("AUI/perspective"), &perspective);

  if(perspective != wxEmptyString)
  {
    // Loads the window states. We tell wxaui not to recalculate and display the
    // results of this step now as we will do so manually after
    // eventually adding the toolbar.
    m_manager.LoadPerspective(perspective,false);
  }

  // Make sure that some of the settings that comprise the perspektive actualle
  // make sense.
  m_worksheet->m_mainToolBar->Realize();
  // It somehow is possible to hide the maxima worksheet - which renders wxMaxima
  // basically useless => force it to be enabled.
  m_manager.GetPane(wxT("console")).Show(true);
  m_manager.GetPane(wxT("toolbar")).Show(toolbarEnabled);

  // LoadPerspective overwrites the pane names with the saved ones -which can
  // belong to a translation different to the one selected currently =>
  // let's overwrite the names here.
  m_manager.GetPane(wxT("symbols")) =
    m_manager.GetPane(wxT("symbols")).Caption(_("Mathematical Symbols")).CloseButton().PinButton().Resizable().Gripper(false);
  m_manager.GetPane(wxT("format")) =
    m_manager.GetPane(wxT("format")).Caption(_("Insert")).CloseButton().PinButton().Resizable();
  m_manager.GetPane(wxT("draw")) =
    m_manager.GetPane(wxT("draw")).Caption(_("Plot using Draw")).CloseButton().PinButton().Resizable();
  m_manager.GetPane(wxT("greek")) =
    m_manager.GetPane(wxT("greek")).Caption(_("Greek Letters")).CloseButton().PinButton().Resizable().Gripper(false);
  m_manager.GetPane(wxT("log")) =
    m_manager.GetPane(wxT("log")).Caption(_("Debug Messages")).CloseButton().PinButton().Resizable().Gripper(false);
  m_manager.GetPane(wxT("variables")) =
    m_manager.GetPane(wxT("variables")).Caption(_("Variables")).CloseButton().PinButton().Resizable().Gripper(false);
  m_manager.GetPane(wxT("math")) = m_manager.GetPane(wxT("math")).Caption(_("General Math")).
    CloseButton().PinButton().Resizable();
  m_manager.GetPane(wxT("stats")) = m_manager.GetPane(wxT("stats")).Caption(_("Statistics")).
    CloseButton().PinButton().Resizable();
  m_manager.GetPane(wxT("XmlInspector")) =
    m_manager.GetPane(wxT("XmlInspector")).Caption(_("Raw XML monitor")).CloseButton().PinButton().Resizable();
  // The XML inspector scares many users and displaying long XML responses there slows
  // down wxMaxima => disable the XML inspector on startup.
  m_manager.GetPane(wxT("XmlInspector")).Show(false);

  m_manager.GetPane(wxT("structure")) =
    m_manager.GetPane(wxT("structure")).Caption(_("Table of Contents")).CloseButton().PinButton().Resizable();
  m_manager.GetPane(wxT("history")) = m_manager.GetPane(wxT("history")).Caption(_("History"))
    .CloseButton().PinButton().Resizable();

  m_manager.Update();
}

wxSize wxMaximaFrame::DoGetBestClientSize() const
{
  wxSize size(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )*.6,
              wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )*.6);
  if (size.x<800) size.x=800;
  if (size.y<600) size.y=600;
  return size;
}


void wxMaximaFrame::EvaluationQueueLength(int length, int numberOfCommands)
{
  if ((length != m_EvaluationQueueLength) || (m_commandsLeftInCurrentCell != numberOfCommands))
  {
    m_updateEvaluationQueueLengthDisplay = true;
    m_commandsLeftInCurrentCell = numberOfCommands;
    m_EvaluationQueueLength = length;
  }
}

void wxMaximaFrame::UpdateStatusMaximaBusy()
{
  if ((m_StatusMaximaBusy != m_StatusMaximaBusy_next) || (m_forceStatusbarUpdate) ||
      (!m_bytesReadDisplayTimer.IsRunning() && (m_bytesFromMaxima != m_bytesFromMaxima_last) &&
       (m_StatusMaximaBusy_next == transferring)))
  {
    m_StatusMaximaBusy = m_StatusMaximaBusy_next;
    if (!m_StatusSaving)
    {
      switch (m_StatusMaximaBusy)
      {
        case process_wont_start:
          m_bytesFromMaxima_last = 0;
          RightStatusText(_("Cannot start the maxima binary"));
          break;
        case userinput:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, false);
          RightStatusText(_("Maxima asks a question"));
          break;
        case sending:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, true);
          RightStatusText(_("Sending a command to maxima"));
          // We don't evaluate any cell right now.
          break;
        case waiting:
          m_bytesFromMaxima_last = 0;
          m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
          // If we evaluated a cell that produces no output we still want the
          // cell to be unselected after evaluating it.
          if (m_worksheet->FollowEvaluation())
            m_worksheet->SetSelection(NULL);

          m_MenuBar->Enable(menu_remove_output, true);
          RightStatusText(_("Ready for user input"));
          // We don't evaluate any cell right now.
          break;
        case calculating:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, false);
          RightStatusText(_("Maxima is calculating"), false);
          break;
        case transferring:
          m_MenuBar->Enable(menu_remove_output, false);
          m_bytesFromMaxima_last = m_bytesFromMaxima;
          m_bytesReadDisplayTimer.StartOnce(300);
          if(m_bytesFromMaxima == 0)
            RightStatusText(_("Reading Maxima output"),false);
          else
            RightStatusText(wxString::Format(
                              _("Reading Maxima output: %li bytes"), m_bytesFromMaxima),
                            false);
          break;
        case parsing:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, false);
          RightStatusText(_("Parsing output"),false);
          break;
        case disconnected:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, false);
          RightStatusText(_("Not connected to maxima"));
          break;
        case wait_for_start:
          m_bytesFromMaxima_last = 0;
          m_MenuBar->Enable(menu_remove_output, false);
          RightStatusText(_("Maxima started. Waiting for connection..."));
          break;
      }
    }
  }
  m_forceStatusbarUpdate = false;
}

void wxMaximaFrame::StatusSaveStart()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  RightStatusText(_("Saving..."));
}

void wxMaximaFrame::StatusSaveFinished()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  if (m_StatusMaximaBusy != waiting)
    StatusMaximaBusy(m_StatusMaximaBusy);
  else
    RightStatusText(_("Saving successful."));
}

void wxMaximaFrame::StatusExportStart()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  RightStatusText(_("Exporting..."));
}

void wxMaximaFrame::StatusExportFinished()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  if (m_StatusMaximaBusy != waiting)
    StatusMaximaBusy(m_StatusMaximaBusy);
  else
    RightStatusText(_("Export successful."));
}

void wxMaximaFrame::StatusSaveFailed()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  RightStatusText(_("Saving failed."));
}

void wxMaximaFrame::StatusExportFailed()
{
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  RightStatusText(_("Export failed."));
}

wxMaximaFrame::~wxMaximaFrame()
{
  wxString perspective = m_manager.SavePerspective();

  wxConfig::Get()->Write(wxT("AUI/perspective"), perspective);
  m_manager.UnInit();
}

void wxMaximaFrame::SetupMenu()
{
  m_MenuBar = new MainMenuBar();

#define APPEND_MENU_ITEM(menu, id, label, help, stock)  \
  (menu)->Append((id), (label), (help), wxITEM_NORMAL);

  // File menu
  m_FileMenu = new wxMenu;
#if defined __WXOSX__
  m_FileMenu->Append(mac_newId, _("New\tCtrl+N"),
                     _("Open a new window"));
#else
  APPEND_MENU_ITEM(m_FileMenu, menu_new_id, _("New\tCtrl+N"),
       _("Open a new window"), wxT("gtk-new"));
#endif
  APPEND_MENU_ITEM(m_FileMenu, menu_open_id, _("&Open...\tCtrl+O"),
                   _("Open a document"), wxT("gtk-open"));
  m_recentDocumentsMenu = new wxMenu();
  m_FileMenu->Append(menu_recent_documents, _("Open Recent"), m_recentDocumentsMenu);
#if defined __WXOSX__
  m_FileMenu->AppendSeparator();
  m_FileMenu->Append(mac_closeId, _("Close\tCtrl+W"),
                     _("Close window"), wxITEM_NORMAL);
#endif
  APPEND_MENU_ITEM(m_FileMenu, menu_save_id, _("&Save\tCtrl+S"),
                   _("Save document"), wxT("gtk-save"));
  APPEND_MENU_ITEM(m_FileMenu, menu_save_as_id, _("Save As...\tShift+Ctrl+S"),
                   _("Save document as"), wxT("gtk-save"));
  m_FileMenu->Append(menu_load_id, _("&Load Package...\tCtrl+L"),
                     _("Load a Maxima package file"), wxITEM_NORMAL);
  m_recentPackagesMenu = new wxMenu();
  m_FileMenu->Append(menu_recent_packages, _("Load Recent Package"), m_recentPackagesMenu);
  m_FileMenu->Append(menu_batch_id, _("&Batch File...\tCtrl+B"),
                     _("Load a Maxima file using the batch command"), wxITEM_NORMAL);
  m_FileMenu->Append(menu_export_html, _("&Export..."),
                     _("Export document to a HTML or pdfLaTeX file"), wxITEM_NORMAL);
  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_PRINT, _("&Print...\tCtrl+P"),
                   _("Print document"), wxT("gtk-print"));

  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_EXIT, _("E&xit\tCtrl+Q"),
                   _("Exit wxMaxima"), wxT("gtk-quit"));
  m_MenuBar->Append(m_FileMenu, _("&File"));

  m_EditMenu = new wxMenu;
  m_EditMenu->Append(menu_undo, _("Undo\tCtrl+Z"),
                     _("Undo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_redo, _("Redo\tCtrl+Y"),
                     _("Redo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_cut, _("Cut\tCtrl+X"),
                     _("Cut selection"),
                     wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_EditMenu, menu_copy_from_worksheet, _("&Copy\tCtrl+C"),
                   _("Copy selection"), wxT("gtk-copy"));
  m_EditMenu->Append(menu_copy_text_from_worksheet, _("Copy as Text\tCtrl+Shift+C"),
                     _("Copy selection from document as text"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_matlab_from_worksheet, _("Copy for Octave/Matlab"),
					 _("Copy selection from document in Matlab format"),
					 wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_tex_from_worksheet, _("Copy as LaTeX"),
                     _("Copy selection from document in LaTeX format"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(Worksheet::popid_copy_mathml, _("Copy as MathML"),
                     _("Copy selection from document in a MathML format many word processors can display as 2d equation"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_as_bitmap, _("Copy as Image"),
                     _("Copy selection from document as an image"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_as_svg, _("Copy as SVG"),
                     _("Copy selection from document as an SVG image"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_as_rtf, _("Copy as RTF"),
                     _("Copy selection from document as rtf that a word processor might understand"),
                     wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
  m_EditMenu->Append(menu_copy_as_emf, _("Copy as EMF"),
                     _("Copy selection from document as an Enhanced Metafile"),
                     wxITEM_NORMAL);
#endif
  m_EditMenu->Append(menu_paste, _("Paste\tCtrl+V"),
                     _("Paste text from clipboard"),
                     wxITEM_NORMAL);

  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_edit_find, _("Find\tCtrl+F"), _("Find and replace"), wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(menu_select_all, _("Select All\tCtrl+A"),
                     _("Select all"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(menu_copy_to_file, _("Save Selection to Image..."),
                     _("Save selection from document to an image file"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(Worksheet::popid_comment_selection, _("Comment selection\tCtrl+/"),
                     _("Comment out the currently selected text"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
#if defined __WXOSX__
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("Preferences...\tCtrl+,"),
                   _("Configure wxMaxima"), wxT("gtk-preferences"));
#else
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("C&onfigure"),
                   _("Configure wxMaxima"), wxT("gtk-preferences"));
#endif
  m_MenuBar->Append(m_EditMenu, _("&Edit"));

  // panes
  m_Maxima_Panes_Sub = new wxMenu;
  m_Maxima_Panes_Sub->AppendCheckItem(menu_show_toolbar, _("Main Toolbar\tAlt+Shift+B"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_math, _("General Math\tAlt+Shift+M"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_stats, _("Statistics\tAlt+Shift+S"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_greek, _("Greek Letters\tAlt+Shift+G"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_symbols, _("Symbols\tAlt+Shift+Y"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_history, _("History\tAlt+Shift+I"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_structure, _("Table of Contents\tAlt+Shift+T"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_format, _("Insert Cell\tAlt+Shift+C"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_draw, _("Plot using Draw"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_log,   _("Debug messages"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_variables,   _("Variables"));
  m_Maxima_Panes_Sub->AppendCheckItem(menu_pane_xmlInspector, _("Raw XML Monitor"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->AppendCheckItem(ToolBar::tb_hideCode, _("Hide Code Cells\tAlt+Ctrl+H"));
  m_Maxima_Panes_Sub->Append(menu_pane_hideall, _("Hide All Toolbars\tAlt+Shift+-"), _("Hide all panes"),
                             wxITEM_NORMAL);
  m_Maxima_Panes_Sub->AppendSeparator();
  // equation display type submenu
  wxMenu *equationType = new wxMenu;
  equationType->Append(menu_math_as_1D_ASCII, _("as 1D ASCII"), _("Show equations in their linear form"), wxITEM_NORMAL);
  equationType->Append(menu_math_as_2D_ASCII, _("as ASCII Art"), _("2D equations using ASCII Art"), wxITEM_NORMAL);
  equationType->Append(menu_math_as_graphics, _("in 2D"), _("Nice Graphical Equations"), wxITEM_NORMAL);

  m_Maxima_Panes_Sub->Append(wxNewId(), _("Display equations"), equationType, _("How to display new equations"));

  wxMenu *autoSubscript = new wxMenu;
  autoSubscript->Append(menu_noAutosubscript, _("Never"), _("Don't autosubscript after an underscore"), wxITEM_NORMAL);
  autoSubscript->Append(menu_defaultAutosubscript, _("Integers and single letters"), _("Autosubscript numbers and text following single letters"), wxITEM_NORMAL);
  autoSubscript->Append(menu_alwaysAutosubscript, _("Always"), _("Always autosubscript after an underscore"), wxITEM_NORMAL);
  m_Maxima_Panes_Sub->Append(wxNewId(), _("Autosubscript"), autoSubscript, _("Autosubscript chars after an underscore"));

  wxMenu *roundedMatrixParens = new wxMenu;
  roundedMatrixParens->Append(menu_roundedMatrixParensYes, _("Rounded"), _("Use rounded parenthesis for matrices"), wxITEM_NORMAL);
  roundedMatrixParens->Append(menu_roundedMatrixParensNo, _("Square"), _("Use square parenthesis for matrices"), wxITEM_NORMAL);
  m_Maxima_Panes_Sub->Append(wxNewId(), _("Matrix parenthesis"), roundedMatrixParens, _("Choose the parenthesis type for Matrices"));


  m_Maxima_Panes_Sub->AppendSeparator();
  APPEND_MENU_ITEM(m_Maxima_Panes_Sub, Worksheet::menu_zoom_in, _("Zoom &In\tCtrl++"),
                   _("Zoom in 10%"), wxT("gtk-zoom-in"));
  APPEND_MENU_ITEM(m_Maxima_Panes_Sub, Worksheet::menu_zoom_out, _("Zoom Ou&t\tCtrl+-"),
                   _("Zoom out 10%"), wxT("gtk-zoom-out"));
  // zoom submenu
  m_Edit_Zoom_Sub = new wxMenu;
  m_Edit_Zoom_Sub->Append(menu_zoom_80, wxT("80%"), _("Set zoom to 80%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_100, wxT("100%"), _("Set zoom to 100%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_120, wxT("120%"), _("Set zoom to 120%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_150, wxT("150%"), _("Set zoom to 150%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_200, wxT("200%"), _("Set zoom to 200%"), wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(menu_zoom_300, wxT("300%"), _("Set zoom to 300%"), wxITEM_NORMAL);

  m_Maxima_Panes_Sub->Append(wxNewId(), _("Set Zoom"), m_Edit_Zoom_Sub, _("Set Zoom"));
  m_Maxima_Panes_Sub->Append(menu_fullscreen, _("Full Screen\tAlt+Enter"),
                             _("Toggle full screen editing"),
                             wxITEM_NORMAL);

  m_MenuBar->Append(m_Maxima_Panes_Sub, _("View"));


  // Cell menu
  m_CellMenu = new wxMenu;
  m_CellMenu->Append(menu_evaluate, _("Evaluate Cell(s)"),
                     _("Evaluate active or selected cell(s)"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_evaluate_all_visible, _("Evaluate All Visible Cells\tCtrl+R"),
                     _("Evaluate all visible cells in the document"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_evaluate_all, _("Evaluate All Cells\tCtrl+Shift+R"),
                     _("Evaluate all cells in the document"), wxITEM_NORMAL);
  m_CellMenu->Append(ToolBar::tb_evaltillhere, _("Evaluate Cells Above\tCtrl+Shift+P"),
                     _("Re-evaluate all cells above the one the cursor is in"), wxITEM_NORMAL);
  m_CellMenu->Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"),
                     _("Re-evaluate all cells below the one the cursor is in"), wxITEM_NORMAL);

  m_CellMenu->Append(menu_remove_output, _("Remove All Output"),
                     _("Remove output from input cells"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_insert_previous_input, _("Copy Previous Input\tCtrl+I"),
                     _("Create a new cell with previous input"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_insert_previous_output, _("Copy Previous Output\tCtrl+U"),
                     _("Create a new cell with previous output"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_autocomplete, _("Complete Word\tCtrl+K"),
                     _("Complete word"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_autocomplete_templates, _("Show Template\tCtrl+Shift+K"),
                     _("Show function template"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_insert_input, _("Insert Input &Cell\tCtrl+0"),
                     _("Insert a new input cell"));
  m_CellMenu->Append(menu_add_comment, _("Insert &Text Cell\tCtrl+1"),
                     _("Insert a new text cell"));
  m_CellMenu->Append(menu_add_title, _("Insert T&itle Cell\tCtrl+2"),
                     _("Insert a new title cell"));
  m_CellMenu->Append(menu_add_section, _("Insert &Section Cell\tCtrl+3"),
                     _("Insert a new section cell"));
  m_CellMenu->Append(menu_add_subsection, _("Insert S&ubsection Cell\tCtrl+4"),
                     _("Insert a new subsection cell"));
  m_CellMenu->Append(menu_add_subsubsection, _("Insert S&ubsubsection Cell\tCtrl+5"),
                     _("Insert a new subsubsection cell"));
  m_CellMenu->Append(menu_add_heading5, _("Insert heading5 Cell\tCtrl+6"),
                     _("Insert a new heading5 cell"));
  m_CellMenu->Append(menu_add_heading6, _("Insert heading6 Cell\tCtrl+7"),
                     _("Insert a new heading7 cell"));
  m_CellMenu->Append(menu_add_pagebreak, _("Insert Page Break"),
                     _("Insert a page break"));
  m_CellMenu->Append(menu_insert_image, _("Insert Image..."),
                     _("Insert image"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_fold_all_cells, _("Fold All\tCtrl+Alt+["),
                     _("Fold all sections"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_unfold_all_cells, _("Unfold All\tCtrl+Alt+]"),
                     _("Unfold all folded sections"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(menu_history_previous, _("Previous Command\tAlt+Up"),
                     _("Recall previous command from history"), wxITEM_NORMAL);
  m_CellMenu->Append(menu_history_next, _("Next Command\tAlt+Down"),
                     _("Recall next command from history"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(Worksheet::popid_merge_cells, _("Merge Cells\tCtrl+M"),
                     _("Merge the text from two input cells into one"), wxITEM_NORMAL);
  m_CellMenu->Append(Worksheet::popid_divide_cell, _("Divide Cell\tCtrl+D"),
                     _("Divide this input cell into two cells"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->AppendCheckItem(Worksheet::popid_auto_answer, _("Automatically answer questions"),
                     _("Automatically fill in answers known from the last run"));

  m_MenuBar->Append(m_CellMenu, _("Ce&ll"));

  // Maxima menu
  m_MaximaMenu = new wxMenu;

#if defined (__WXOSX__)
  APPEND_MENU_ITEM(m_MaximaMenu, menu_interrupt_id,
                   _("&Interrupt\tCtrl+."), // command-. interrupts (mac standard)
                   _("Interrupt current computation"), wxT("gtk-stop"));
#else
  APPEND_MENU_ITEM(m_MaximaMenu, menu_interrupt_id,
                   _("&Interrupt\tCtrl+G"),
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
  m_MaximaMenu->Append(menu_jumptoerror, _("Jump to first error"),
                       _("Jump to the first cell maxima has reported an error in."),
                       wxITEM_NORMAL);
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
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(menu_lhs, _("Left side to the \"=\""),
                          _("The half of the equation that is to the left of the \"=\""),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(menu_rhs, _("Right side to the \"=\""),
                          _("The half of the equation that is to the right of the \"=\""),
                          wxITEM_NORMAL);
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
  m_Algebra_Menu->Append(menu_map, _("&Map to List(s)..."),
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

  // List menu
  m_listMenu = new wxMenu;
  wxMenu *listcreateSub = new wxMenu;
  listcreateSub->Append(menu_list_create_from_elements, _("from individual elements"),
                        _("Create a list from comma-separated elements"),
                        wxITEM_NORMAL);
  listcreateSub->Append(menu_list_create_from_rule, _("from a rule"),
                        _("Generate list elements using a rule"),
                        wxITEM_NORMAL);
  listcreateSub->Append(menu_list_create_from_list, _("from a list"),
                        _("Generate a new list using a lists' elements"),
                        wxITEM_NORMAL);
  listcreateSub->Append(menu_list_actual_values_storage, _("as storage for actual values for variables"),
                        _("Generate a storage for variable values that can be introduced into equations at any time"),
                        wxITEM_NORMAL);
  listcreateSub->Append(menu_list_create_from_args, _("from function arguments"),
                        _("Extract the argument list from a function call"),
                        wxITEM_NORMAL);
  m_listMenu->Append(wxNewId(), _("Create list"),
                     listcreateSub,
                     _("Create a list"));
  wxMenu *listuseSub = new wxMenu;
  listuseSub->Append(menu_list_map, _("apply function to each element"),
                        _("Runs each element through a function"),
                        wxITEM_NORMAL);
  listuseSub->Append(menu_list_use_actual_values, _("use the actual values stored"),
                        _("Introduce the actual values for variables stored in the list"),
                        wxITEM_NORMAL);
  listuseSub->Append(menu_list_as_function_arguments, _("use as function arguments"),
                        _("Use list as the arguments of a function"),
                        wxITEM_NORMAL);
  listuseSub->Append(menu_list_do_for_each_element, _("do for each element"),
                        _("Execute a command for each element of the list"),
                        wxITEM_NORMAL);

  m_listMenu->Append(wxNewId(), _("Use list"),
                     listuseSub,
                     _("Use a list"));
  wxMenu *listextractmenu = new wxMenu;
  listextractmenu->Append(menu_list_nth, _("nth"), _("Returns an arbitrary list item"));
  listextractmenu->Append(menu_list_first, _("First"), _("Returns the first item of the list"));
  listextractmenu->Append(menu_list_rest, _("All but the 1st n elements"), _("Returns the list without its first n elements"));
  listextractmenu->Append(menu_list_restN, _("All but the last n elements"), _("Returns the list without its last n elements"));
  listextractmenu->Append(menu_list_last, _("Last"), _("Returns the last item of the list"));
  listextractmenu->Append(menu_list_lastn, _("Last n"), _("Returns the last n items of the list"));
  listextractmenu->Append(menu_list_extract_value, _("Extract an actual value for a variable"),
                        _("Extract the value for one variable assigned in a list"),
                        wxITEM_NORMAL);
  m_listMenu->Append(wxNewId(), _("Extract Elements"),
                     listextractmenu,
                     _("Extract list Elements"));
  wxMenu *listappendSub = new wxMenu;
  listappendSub->Append(menu_list_append_item, _("Append an element"),
                        _("Append an element to an existing list"),
                        wxITEM_NORMAL);
  listappendSub->Append(menu_list_append_list, _("Append a list"),
                        _("Append a list to an existing list"),
                        wxITEM_NORMAL);
  listappendSub->Append(menu_list_interleave, _("Interleave"),
                        _("Interleave the values of two lists"),
                        wxITEM_NORMAL);
  m_listMenu->Append(wxNewId(), _("Append"),
                     listappendSub,
                     _("Use a list"));

  m_listMenu->Append(menu_list_length, _("Length"), _("Returns the length of the list"));
  m_listMenu->Append(menu_list_reverse, _("Reverse"), _("Reverse the order of the list items"));
  m_listMenu->AppendSeparator();
  m_listMenu->Append(menu_list_sort, _("Sort"));
  m_listMenu->Append(menu_list_remove_duplicates, _("Remove duplicates"),_("Remove all list elements that appear twice in a row. Normally used in conjunction with sort."));
m_listMenu->AppendSeparator();
  m_listMenu->Append(menu_list_push, _("Push"), _("Add a new item to the beginning of the list. Useful for creating stacks.") );
  m_listMenu->Append(menu_list_pop, _("Pop"), _("Return the first item of the list and remove it from the list. Useful for creating stacks."));
  m_listMenu->AppendSeparator();
  m_listMenu->Append(menu_list_list2matrix, _("Nested list to Matrix"), _("Converts a nested list like [[1,2],[3,4]] to a matrix"));
  m_listMenu->Append(menu_list_matrix2list, _("Matrix to nested List"), _("Converts a matrix to a list of lists"));
  m_MenuBar->Append(m_listMenu, _("&List"));
  // Plot menu
  m_PlotMenu = new wxMenu;
  m_PlotMenu->Append(gp_plot2, _("Plot &2d..."),
                     _("Plot in 2 dimensions"), wxITEM_NORMAL);
  m_PlotMenu->Append(gp_plot3, _("Plot &3d..."),
                     _("Plot in 3 dimensions"), wxITEM_NORMAL);
  m_PlotMenu->Append(menu_plot_format, _("Plot &Format..."),
                     _("Set plot format"), wxITEM_NORMAL);
  m_PlotMenu->AppendSeparator();
  m_PlotMenu->Append(menu_animationautostart, _("Toggle animation autoplay"),
                     _("Defines if an animation is automatically started or only by clicking on it."), wxITEM_NORMAL);
  m_PlotMenu->Append(menu_animationframerate, _("Animation framerate..."),
                     _("Set the frame rate for animations."));
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
  m_NumericMenu->AppendSeparator();
  m_NumericMenu->Append(menu_set_precision, _("Set bigfloat &Precision..."),
                        _("Set the precision for numbers that are defined as bigfloat. Such numbers can be generated by entering 1.5b12 or as bfloat(1.234)"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(menu_set_displayprecision, _("Set &displayed Precision..."),
                        _("Shows how many digits of a numbers are displayed"),
                        wxITEM_NORMAL);
  m_NumericMenu->AppendSeparator();
  m_NumericMenu->Append(menu_engineeringFormat, _("Engineering format (12.1e6 etc.)"),
                        _("Print floating-point numbers with exponents dividable by 2"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(menu_engineeringFormatSetup, _("Setup the engineering format..."),
                        _("Fine-tune the display of engineering-format numbers"),
                        wxITEM_NORMAL);
  m_MenuBar->Append(m_NumericMenu, _("&Numeric"));

  // Help menu
  m_HelpMenu = new wxMenu;
#if defined __WXOSX__
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
#ifndef __WXOSX__
  m_HelpMenu->AppendSeparator();
#endif
#ifndef __WXOSX__
  APPEND_MENU_ITEM(m_HelpMenu, wxID_ABOUT,
    _("About"),
    _("About wxMaxima"), wxT("stock_about"));
#else
  APPEND_MENU_ITEM(m_HelpMenu, wxID_ABOUT,
                   _("About wxMaxima"),
                   _("About wxMaxima"), wxT("stock_about"));
#endif

  m_MenuBar->Append(m_HelpMenu, _("&Help"));

  SetMenuBar(m_MenuBar);

#undef APPEND_MENU_ITEM

}

bool wxMaximaFrame::ToolbarIsShown()
{
  return m_manager.GetPane(wxT("toolbar")).IsShown();
}

void wxMaximaFrame::UpdateRecentDocuments()
{
  if(m_recentDocumentsMenu == NULL)
    m_recentDocumentsMenu = new wxMenu();
  while(m_recentDocumentsMenu->GetMenuItemCount() > 0)
    m_recentDocumentsMenu->Destroy(m_recentDocumentsMenu->FindItemByPosition(0));

  if(m_recentPackagesMenu == NULL)
    m_recentPackagesMenu = new wxMenu();
  while(m_recentPackagesMenu->GetMenuItemCount() > 0)
    m_recentPackagesMenu->Destroy(m_recentPackagesMenu->FindItemByPosition(0));

  long recentItems = 10;
  wxConfig::Get()->Read(wxT("recentItems"), &recentItems);

  if (recentItems < 5) recentItems = 5;
  if (recentItems > 30) recentItems = 30;

  std::list<wxString> recentDocuments = m_recentDocuments.Get();
  std::list<wxString> unsavedDocuments = m_unsavedDocuments.Get();
  std::list<wxString> recentPackages = m_recentPackages.Get();

  // Populate the recent documents menu
  for (int i = menu_recent_document_0; i <= menu_recent_document_0 + recentItems; i++)
  {
    if (!recentDocuments.empty())
    {
      wxFileName filename(recentDocuments.front());
      wxString path(filename.GetPath()), fullname(filename.GetFullName());
      wxString label(fullname + wxT("   [ ") + path + wxT(" ]"));
      recentDocuments.pop_front();

      m_recentDocumentsMenu->Append(i, label);
      if(wxFileExists(filename.GetFullPath()))
        m_recentDocumentsMenu->Enable(i, true);
      else
        m_recentDocumentsMenu->Enable(i, false);
    }
  }

  bool separatorAdded = false;

  // Populate the unsaved documents menu
  for (int i = menu_unsaved_document_0; i <= menu_unsaved_document_0 + recentItems; i++)
  {
    if (!unsavedDocuments.empty())
    {
      wxString filename = unsavedDocuments.front();
      if(!wxFileExists(filename))
        continue;
      wxStructStat stat;
      wxStat(filename,&stat);
      wxDateTime modified(stat.st_mtime);
      wxString label= filename + wxT(" (") +
        modified.FormatDate() + wxT(" ") +
        modified.FormatTime() + wxT(")");

      if (!separatorAdded)
        m_recentDocumentsMenu->Append(menu_recent_document_separator,
                                      wxEmptyString, wxEmptyString, wxITEM_SEPARATOR);

      m_recentDocumentsMenu->Append(i, label);
      unsavedDocuments.pop_front();
    }
  }

  // Populate the recent packages menu
  for (int i = menu_recent_package_0; i <= menu_recent_package_0 + recentItems; i++)
  {
    if (!recentPackages.empty())
    {
      wxFileName filename = recentPackages.front();
      wxString path(filename.GetPath()), fullname(filename.GetFullName());
      wxString label;
      if(path != wxEmptyString)
        label = fullname + wxT("   [ ") + path + wxT(" ]");
      else
        label = fullname;
      recentPackages.pop_front();

      m_recentPackagesMenu->Append(i, label);
    }
  }
}

void wxMaximaFrame::ReReadConfig()
{
  if (Configuration::m_configfileLocation_override != wxEmptyString)
  {
    wxConfigBase *config = wxConfig::Get();
    config->Flush();
    wxDELETE(config);
    config = NULL;
    wxConfig::Set(new wxFileConfig(wxT("wxMaxima"),
                                   wxEmptyString, Configuration::m_configfileLocation_override));
  }
  // Re-Reading the config isn't necessary on the Mac where all windows share the same
  // window and on Windows where the registry is re-read every time the configuration
  // is accessed.
#ifdef __WXGTK__
  else
  {
    wxConfigBase *config = wxConfig::Get();
    config->Flush();
    wxDELETE(config);
    config = NULL;
    if(Configuration::m_configfileLocation_override != wxEmptyString)
      wxConfig::Set(new wxConfig(wxT("wxMaxima")));
    else
      wxConfig::Set(new wxFileConfig(wxT("wxMaxima"), wxEmptyString,
                                     Configuration::m_configfileLocation_override));
    }
#endif
}

wxString wxMaximaFrame::GetTempAutosavefileName()
{
  wxString name = wxStandardPaths::Get().GetTempDir()+
    wxString::Format("/untitled_%li_%li.wxmx",
                     wxGetProcessId(),m_pid);

  // If the file name changes there is no reason to let the old file lie around.
  if (name != m_tempfileName)
    RemoveTempAutosavefile();

  m_tempfileName = name;
  return m_tempfileName;
}

void wxMaximaFrame::RegisterAutoSaveFile()
{
  wxString autoSaveFiles;
  ReReadConfig();
  wxConfigBase *config = wxConfig::Get();
  config->Read("AutoSaveFiles",&autoSaveFiles);
  m_unsavedDocuments.AddDocument(m_tempfileName);
  ReReadConfig();
}

void wxMaximaFrame::RemoveTempAutosavefile()
{
  if(m_tempfileName != wxEmptyString)
  {
    // Don't delete the file if we have opened it and haven't saved it under a
    // different name yet.
    if(wxFileExists(m_tempfileName) && (m_tempfileName != m_worksheet->m_currentFile))
      wxRemoveFile(m_tempfileName);
  }
  m_tempfileName = wxEmptyString;
}

bool wxMaximaFrame::IsPaneDisplayed(Event id)
{
  bool displayed = false;

  switch (id)
  {
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
    case menu_pane_greek:
      displayed = m_manager.GetPane(wxT("greek")).IsShown();
      break;
    case menu_pane_log:
      displayed = m_manager.GetPane(wxT("log")).IsShown();
      break;
    case menu_pane_variables:
      displayed = m_manager.GetPane(wxT("variables")).IsShown();
      break;
    case menu_pane_symbols:
      displayed = m_manager.GetPane(wxT("symbols")).IsShown();
      break;
    case menu_pane_format:
      displayed = m_manager.GetPane(wxT("format")).IsShown();
      break;
    case menu_pane_draw:
      displayed = m_manager.GetPane(wxT("draw")).IsShown();
      break;
    default:
      wxASSERT(false);
      break;
  }

  return displayed;
}

void wxMaximaFrame::ShowPane(Event id, bool show)
{
  switch (id)
  {
    case menu_pane_math:
      m_manager.GetPane(wxT("math")).Show(show);
      break;
    case menu_pane_history:
      m_manager.GetPane(wxT("history")).Show(show);
      break;
    case menu_pane_structure:
      m_manager.GetPane(wxT("structure")).Show(show);
      m_worksheet->m_tableOfContents->UpdateTableOfContents(m_worksheet->GetTree(), m_worksheet->GetHCaret());
      break;
    case menu_pane_xmlInspector:
      m_manager.GetPane(wxT("XmlInspector")).Show(show);
      break;
    case menu_pane_stats:
      m_manager.GetPane(wxT("stats")).Show(show);
      break;
    case menu_pane_greek:
      m_manager.GetPane(wxT("greek")).Show(show);
      break;
    case menu_pane_log:
      m_manager.GetPane(wxT("log")).Show(show);
      break;
    case menu_pane_variables:
      m_manager.GetPane(wxT("variables")).Show(show);
      break;
    case menu_pane_symbols:
      m_manager.GetPane(wxT("symbols")).Show(show);
      break;
    case menu_pane_format:
      m_manager.GetPane(wxT("format")).Show(show);
      break;
    case menu_pane_draw:
      m_manager.GetPane(wxT("draw")).Show(show);
      break;
    case menu_pane_hideall:
      m_manager.GetPane(wxT("math")).Show(false);
      m_manager.GetPane(wxT("history")).Show(false);
      m_manager.GetPane(wxT("structure")).Show(false);
      m_manager.GetPane(wxT("XmlInspector")).Show(false);
      m_manager.GetPane(wxT("stats")).Show(false);
      m_manager.GetPane(wxT("greek")).Show(false);
      m_manager.GetPane(wxT("log")).Show(false);
      m_manager.GetPane(wxT("variables")).Show(false);
      m_manager.GetPane(wxT("symbols")).Show(false);
      m_manager.GetPane(wxT("format")).Show(false);
      ShowToolBar(false);
      break;
    default:
      wxASSERT(false);
      break;
  }

  m_manager.Update();
}

wxPanel *wxMaximaFrame::CreateMathPane()
{
  wxGridSizer *grid = new wxGridSizer(2);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(panel, button_ratsimp, _("Simplify"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_radcan, _("Simplify (r)"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_factor, _("Factor"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_expand, _("Expand"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_rectform, _("Rectform"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_subst, _("Subst..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_trigrat, _("Canonical (tr)"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_trigsimp, _("Simplify (tr)"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_trigexpand, _("Expand (tr)"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_trigreduce, _("Reduce (tr)"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_solve, _("Solve..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_solve_ode, _("Solve ODE..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_diff, _("Diff..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_integrate, _("Integrate..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_limit, _("Limit..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_taylor, _("Series..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_plot2, _("Plot 2D..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, button_plot3, _("Plot 3D..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  panel->SetSizer(grid);
  grid->Fit(panel);
  grid->SetSizeHints(panel);

  return panel;
}

wxPanel *wxMaximaFrame::CreateStatPane()
{
  wxGridSizer *grid1 = new wxGridSizer(2);
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *box1 = new wxBoxSizer(wxVERTICAL);
  wxGridSizer *grid2 = new wxGridSizer(2);
  wxGridSizer *grid3 = new wxGridSizer(2);
  wxBoxSizer *box3 = new wxBoxSizer(wxVERTICAL);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
  int border = 0;
  int sizerBorder = 2;

  grid1->Add(new wxButton(panel, menu_stats_mean, _("Mean..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_median, _("Median..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_var, _("Variance..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid1->Add(new wxButton(panel, menu_stats_dev, _("Deviation..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  box->Add(grid1, 0, style, sizerBorder);

  box1->Add(new wxButton(panel, menu_stats_tt1, _("Mean Test..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_tt2, _("Mean Difference Test..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_tnorm, _("Normality Test..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_linreg, _("Linear Regression..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  box1->Add(new wxButton(panel, menu_stats_lsquares, _("Least Squares Fit..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  box->Add(box1, 0, style, sizerBorder);

  grid2->Add(new wxButton(panel, menu_stats_histogram, _("Histogram..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_scatterplot, _("Scatterplot..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_barsplot, _("Barsplot..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_piechart, _("Piechart..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid2->Add(new wxButton(panel, menu_stats_boxplot, _("Boxplot..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  box->Add(grid2, 0, style, sizerBorder);

  grid3->Add(new wxButton(panel, menu_stats_readm, _("Read Matrix..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid3->Add(new wxButton(panel, menu_stats_enterm, _("Enter Matrix..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  box->Add(grid3, 0, style, sizerBorder);

  box3->Add(new wxButton(panel, menu_stats_subsample, _("Subsample..."),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  box->Add(box3, 0, style, sizerBorder);

  panel->SetSizer(box);
  box->Fit(panel);
  box->SetSizeHints(panel);

  return panel;
}

void wxMaximaFrame::CharacterButtonPressed(wxMouseEvent &event)
{
  wxChar ch = event.GetId();
  wxString ch_string(ch);
  m_worksheet->InsertText(ch_string);
}

wxPanel *wxMaximaFrame::CharButton(wxPanel *parent, wxChar ch, wxString description, bool WXUNUSED(matchesMaximaCommand))
{
  wxPanel *panel = new wxPanel(parent, ch);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxStaticText *text = new wxStaticText(panel, ch, wxString(ch));
  vbox->Add(text, 1, wxALL | wxCENTER, 0);

  if (description.Length() > 0)
    text->SetToolTip(description);
  text->Connect(wxEVT_LEFT_UP, wxMouseEventHandler(wxMaximaFrame::CharacterButtonPressed), NULL, this);
  panel->Connect(wxEVT_LEFT_UP, wxMouseEventHandler(wxMaximaFrame::CharacterButtonPressed), NULL, this);
  panel->SetSizerAndFit(vbox);
  return panel;
}

wxPanel *wxMaximaFrame::CreateGreekPane()
{
  wxPanel *panel = new wxPanel(this, -1);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  int style = wxALL | wxEXPAND;
  int border = 0;

  wxFlexGridSizer *lowercase = new wxFlexGridSizer(8);
  lowercase->SetFlexibleDirection(wxBOTH);
  for (int i = 0; i < 8; i++)
    lowercase->AddGrowableCol(i, 1);
  lowercase->Add(CharButton(panel, wxT('\x03B1'), _("alpha")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B2'), _("beta")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B3'), _("gamma")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B4'), _("delta")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B5'), _("epsilon")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B6'), _("zeta")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B7'), _("eta")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B8'), _("theta")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03B9'), _("iota")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BA'), _("kappa")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BB'), _("lambda")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BC'), _("mu")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BD'), _("nu")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BE'), _("xi")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03BF'), _("omicron")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C0'), _("pi")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C1'), _("rho")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C3'), _("sigma")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C4'), _("tau")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C5'), _("upsilon")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C6'), _("phi")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C7'), _("chi")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C8'), _("psi")), 0, wxALL | wxEXPAND, 2);
  lowercase->Add(CharButton(panel, wxT('\x03C9'), _("omega")), 0, wxALL | wxEXPAND, 2);
  vbox->Add(lowercase, 0, style, border);

  wxFlexGridSizer *uppercase = new wxFlexGridSizer(8);
  uppercase->SetFlexibleDirection(wxBOTH);
  for (int i = 0; i < 8; i++)
    uppercase->AddGrowableCol(i, 1);

  uppercase->Add(CharButton(panel, wxT('\x0391'), _("Alpha")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0392'), _("Beta")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0393'), _("Gamma")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0394'), _("Delta")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0395'), _("Epsilon")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0396'), _("Zeta")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0397'), _("Eta")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0398'), _("Theta")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x0399'), _("Iota")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039A'), _("Kappa")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039B'), _("Lambda")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039C'), _("Mu")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039D'), _("Nu")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039E'), _("Xi")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x039F'), _("Omicron")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A0'), _("Pi")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A1'), _("Rho")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A3'), _("Sigma")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A4'), _("Tau")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A5'), _("Upsilon")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A6'), _("Phi")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A7'), _("Chi")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A8'), _("Psi")), 0, wxALL | wxEXPAND, 2);
  uppercase->Add(CharButton(panel, wxT('\x03A9'), _("Omega")), 0, wxALL | wxEXPAND, 2);
  vbox->Add(uppercase, 0, style, border);


  panel->SetSizerAndFit(vbox);
  vbox->SetSizeHints(panel);

  return panel;
}

wxPanel *wxMaximaFrame::CreateSymbolsPane()
{
  wxPanel *panel = new wxPanel(this, -1);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  int style = wxALL | wxEXPAND;
  int border = 0;

  wxFlexGridSizer *builtInSymbolsSizer = new wxFlexGridSizer(8);
  wxPanel *builtInSymbols = new wxPanel(panel);
  builtInSymbolsSizer->SetFlexibleDirection(wxBOTH);
  for (int i = 0; i < 8; i++)
    builtInSymbolsSizer->AddGrowableCol(i, 1);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x00BD'), _("1/2"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x00B2'), _("to the power of 2"), true), 0, wxALL | wxEXPAND,
                           2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x00B3'), _("to the power of 3"), true), 0, wxALL | wxEXPAND,
                           2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x221A'),
                                      _("sqrt (needs parenthesis for its argument to work as a maxima command)"), true),
                           0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2148')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2147')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x210F')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2208'), _("in")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2203'), _("exists")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2204'), _("there is no")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x21D2'), _("\"implies\" symbol"), true), 0,
                           wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x221E'), _("Infinity"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2205'), _("empty")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x25b6')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x25b8')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22C0'), _("and"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22C1'), _("or"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22BB'), _("xor"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22BC'), _("nand"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22BD'), _("nor"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x21D4'), _("equivalent"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x00b1'), _("plus or minus")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x00AC'), _("not"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22C3'), _("union")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x22C2'), _("intersection")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2286'), _("subset or equal")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2282'), _("subset")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2288'), _("not subset or equal")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2284'), _("not subset")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x0127')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x0126')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2202'), _("partial sign")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x222b'), _("Integral sign")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2245')), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x221d'), _("proportional to")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2260'), _("not bytewise identical"), true), 0,
                           wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2264'), _("less or equal"), true), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2265'), _("greater than or equal"), true), 0,
                           wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x226A'), _("much less than")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x226B'), _("much greater than")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2263'), _("Identical to")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2211'), _("Sum sign")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x220F'), _("Product sign")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2225'), _("Parallel to")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x27C2'), _("Perpendicular to")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x219D'), _("Leads to")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x2192'), _("Right arrow")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x27F6'), _("Long Right arrow")), 0, wxALL | wxEXPAND, 2);
  builtInSymbolsSizer->Add(CharButton(builtInSymbols, wxT('\x220e'), _("End of proof")), 0, wxALL | wxEXPAND, 2);
  builtInSymbols->SetSizer(builtInSymbolsSizer);
  vbox->Add(builtInSymbols, 0, style, border);

  m_userSymbols = new wxPanel(panel);
  m_userSymbolsSizer = new wxGridSizer(8);
  UpdateUserSymbols();
  m_userSymbols->SetSizer(m_userSymbolsSizer);
  vbox->Add(m_userSymbols, 0, style, border);
  panel->SetSizerAndFit(vbox);
  vbox->SetSizeHints(panel);
  return panel;
}

void wxMaximaFrame::UpdateUserSymbols()
{

  while (!m_userSymbolButtons.empty())
  {
    m_userSymbolButtons.front()->Destroy();
    m_userSymbolButtons.pop_front();
  }

  if (m_userSymbols == NULL)
    return;
  // Clear the user symbols pane
  m_userSymbols->DestroyChildren();

  // Populate the pane with a button per user symbol
  wxString symbolPaneAdditionalChars = wxT("Øü§");
  wxConfig::Get()->Read(wxT("symbolPaneAdditionalChars"), &symbolPaneAdditionalChars);
  for (size_t i = 0; i < symbolPaneAdditionalChars.Length(); i++)
  {
    wxPanel *button = CharButton(m_userSymbols, symbolPaneAdditionalChars[i],
                                 _("A symbol from the configuration dialogue"));
    m_userSymbolButtons.push_back(button);
    m_userSymbolsSizer->Add(button, 0, wxALL | wxEXPAND, 2);
  }
}

wxPanel *wxMaximaFrame::CreateFormatPane()
{
  wxGridSizer *grid = new wxGridSizer(2);
  wxPanel *panel = new wxPanel(this, -1);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(panel, menu_format_text, _("Text"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_title, _("Title"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_section, _("Section"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_subsection, _("Subsection"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_subsubsection, _("Subsubsection"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_heading5, _("Heading 5"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_heading6, _("Heading 6"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_image, _("Image"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);
  grid->Add(new wxButton(panel, menu_format_pagebreak, _("Pagebreak"),
                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT), 0, style, border);

  panel->SetSizer(grid);
  grid->Fit(panel);
  grid->SetSizeHints(panel);

  return panel;
}

void wxMaximaFrame::DrawPane::SetDimensions(int dimensions)
{
  if(dimensions == m_dimensions)
    return;

  if(dimensions > 0)
  {
    m_draw_explicit->Enable(true);
    m_draw_implicit->Enable(true);
    m_draw_parametric->Enable(true);
    m_draw_points->Enable(true);
    m_draw_title->Enable(true);
    m_draw_key->Enable(true);
    m_draw_fgcolor->Enable(true);
    m_draw_fillcolor->Enable(true);
    m_draw_setup2d->Enable(false);
    m_draw_grid->Enable(true);
    m_draw_axis->Enable(true);
    m_draw_accuracy->Enable(true);
    if(dimensions > 2)
    {
      m_draw_contour->Enable(true);
      m_draw_setup3d->Enable(true);
    }
    else
    {
      m_draw_contour->Enable(false);
      m_draw_setup3d->Enable(false);
    }
  }
  else
  {
    m_draw_accuracy->Enable(true);
    m_draw_explicit->Enable(true);
    m_draw_implicit->Enable(true);
    m_draw_parametric->Enable(true);
    m_draw_points->Enable(true);
    m_draw_title->Enable(true);
    m_draw_key->Enable(true);
    m_draw_fgcolor->Enable(true);
    m_draw_fillcolor->Enable(true);
    m_draw_setup2d->Enable(true);
    m_draw_setup3d->Enable(true);
    m_draw_grid->Enable(true);
    m_draw_axis->Enable(true);
  }
  m_dimensions = dimensions;
}

wxMaximaFrame::DrawPane::DrawPane(wxWindow *parent, int id) : wxPanel(parent, id)
{
  wxBoxSizer  *vbox = new wxBoxSizer(wxVERTICAL);
  wxGridSizer *grid = new wxGridSizer(2);
  m_dimensions = -1;
  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(m_draw_setup2d = new wxButton(this, menu_draw_2d, _("2D"),
                                          wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_setup2d->SetToolTip(_("Setup a 2D plot"));
  grid->Add(m_draw_setup3d = new wxButton(this, menu_draw_3d, _("3D"),
                                          wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
                0, style, border);
  m_draw_setup3d->SetToolTip(_("Setup a 3D plot"));
//  grid->AddSpacer(1);
//  grid->AddSpacer(1);
  grid->Add(m_draw_explicit = new wxButton(this, menu_draw_explicit, _("Expression"),
                                           wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_explicit->SetToolTip(_("The standard plot command: Plot an equation as a curve"));
  grid->Add(m_draw_implicit = new wxButton(this, menu_draw_implicit, _("Implicit Plot"),
                                           wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  grid->Add(m_draw_parametric = new wxButton(this, menu_draw_parametric, _("Parametric Plot"),
                                             wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  grid->Add(m_draw_points = new wxButton(this, menu_draw_points, _("Points"),
                                         wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
//  grid->AddSpacer(1);
//  grid->AddSpacer(1);
  grid->Add(m_draw_title = new wxButton(this, menu_draw_title, _("Diagram title"),
                                        wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_title->SetToolTip(_("The diagram title"));
  grid->Add(m_draw_axis = new wxButton(this, menu_draw_axis, _("Axis"),wxDefaultPosition,
                                       wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_axis->SetToolTip(_("Setup the axis"));
  grid->Add(m_draw_contour = new wxButton(this, menu_draw_contour, _("Contour"),
                                          wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  grid->Add(m_draw_key = new wxButton(this, menu_draw_key, _("Plot name"),
                                      wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_key->SetToolTip(_("The next plot's title"));
  grid->Add(m_draw_fgcolor = new wxButton(this, menu_draw_fgcolor, _("Line color"),
                                          wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_fgcolor->SetToolTip(_("The color of the next line to draw"));
  grid->Add(m_draw_fillcolor = new wxButton(this, menu_draw_fillcolor, _("Fill color"),
                                            wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_fillcolor->SetToolTip(_("The fill color for the next objects"));
  grid->Add(m_draw_grid = new wxButton(this, menu_draw_grid, _("Grid"),wxDefaultPosition,
                                       wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_grid->SetToolTip(_("The grid in the background of the diagram"));
  m_draw_contour->SetToolTip(_("Contour lines for 3d plots"));
  grid->Add(m_draw_accuracy = new wxButton(this, menu_draw_accuracy, _("Accuracy"),
                                           wxDefaultPosition, wxDefaultSize, wxBORDER_NONE | wxBU_EXACTFIT),
            0, style, border);
  m_draw_accuracy->SetToolTip(_("The Accuracy versus speed tradeoff"));
  vbox->Add(grid, wxSizerFlags().Expand());
  SetSizerAndFit(vbox);
  vbox->SetSizeHints(this);
  SetDimensions(0);
}

void wxMaximaFrame::ShowToolBar(bool show)
{
  m_manager.GetPane(wxT("toolbar")).Show(show);
  m_manager.Update();
}
