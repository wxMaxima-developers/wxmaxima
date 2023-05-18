// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class wxMaximaFrame

  wxMaximaFrame is responsible for everything that is displayed around the
  actual worksheet - which is displayed by Worksheet and whose logic partially
  is defined in wxMaxima.
*/
#include "wxMaximaFrame.h"
#include "Dirstructure.h"
#include <string>
#include <memory>

#include "CharButton.h"
#include "Gen1Wiz.h"
#include "UnicodeSidebar.h"
#include "wxMaximaIcon.h"
#include <wx/artprov.h>
#include <wx/config.h>
#include <wx/display.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/image.h>
#include <wx/persist/toplevel.h>
#include <wx/stdpaths.h>
#include <wx/sysopt.h>
#include <wx/wupdlock.h>
#include <wx/windowptr.h>
#include <functional>
#include <unordered_map>

wxMaximaFrame::wxMaximaFrame(wxWindow *parent, int id, 
                             const wxString &title, const wxPoint &pos,
                             const wxSize &size, long style,
                             bool becomeLogTarget)
  : wxFrame(parent, id, title, pos, size, style),
    m_manager(this, wxAUI_MGR_ALLOW_FLOATING | wxAUI_MGR_ALLOW_ACTIVE_PANE |
	      wxAUI_MGR_TRANSPARENT_HINT | wxAUI_MGR_HINT_FADE),
    m_recentDocuments(wxS("document")),
    m_recentPackages(wxS("packages")) {

  m_bytesFromMaxima = 0;
  m_drawDimensions_last = -1;
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  //  wxWindowUpdateLocker noUpdates(this);    m_worksheet->m_unsavedDocuments(wxS("unsaved")),

  // Add some shortcuts that aren't automatically set by menu entries.
  wxAcceleratorEntry entries[18];
  entries[0].Set(wxACCEL_CTRL, wxS('K'), EventIDs::menu_autocomplete);
  entries[1].Set(wxACCEL_CTRL, WXK_TAB, EventIDs::menu_autocomplete);
  entries[2].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_TAB,
                 EventIDs::menu_autocomplete_templates);
  entries[3].Set(wxACCEL_CTRL, WXK_SPACE, EventIDs::menu_autocomplete);
  entries[4].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('K'),
                 EventIDs::menu_autocomplete_templates);
  entries[5].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_SPACE,
                 EventIDs::menu_autocomplete_templates);
  entries[6].Set(wxACCEL_ALT, wxS('I'), wxID_ZOOM_IN);
  entries[7].Set(wxACCEL_ALT, wxS('O'), wxID_ZOOM_OUT);
  entries[8].Set(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_ESCAPE,
                 EventIDs::menu_convert_to_code);
  entries[9].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('1'),
                 EventIDs::menu_convert_to_comment);
  entries[10].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('2'),
                  EventIDs::menu_convert_to_title);
  entries[11].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('3'),
                  EventIDs::menu_convert_to_section);
  entries[12].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('4'),
                  EventIDs::menu_convert_to_subsection);
  entries[13].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('5'),
                  EventIDs::menu_convert_to_subsubsection);
  entries[14].Set(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('6'),
                  EventIDs::menu_convert_to_heading5);
  entries[15].Set(wxACCEL_CTRL, wxS('.'),
                  EventIDs::menu_interrupt_id); // Standard on the Mac
  entries[16].Set(wxACCEL_NORMAL, WXK_F1, wxID_HELP);
  entries[17].Set(wxACCEL_NORMAL, WXK_F11, EventIDs::menu_fullscreen);
  wxAcceleratorTable accel(sizeof(entries) / sizeof(entries[0]), entries);
  SetAcceleratorTable(accel);

  // Redirect all debug messages to a dockable panel and output some info
  // about this program.
  m_logPane = new LogPane(this, -1, becomeLogTarget);

  wxLogMessage(wxString::Format(_("wxMaxima version %s"), GITVERSION));
#ifdef __WXMSW__
  if (wxSystemOptions::IsFalse("msw.display.directdraw"))
    wxLogMessage(_("Running on MS Windows"));
  else
    wxLogMessage(_("Running on MS Windows using DirectDraw"));
#endif

  int major = 0;
  int minor = 0;
  wxGetOsVersion(&major, &minor);
  wxLogMessage(_("OS: %s Version %i.%i"),
	       wxGetOsDescription().utf8_str(), major, minor);

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
  wxLogMessage("%s", wxVERSION_STRING);

#ifdef __WXGTK__
#ifdef __WXGTK3__
  wxLogMessage(_("wxWidgets is using GTK 3"));
#else
  wxLogMessage(_("wxWidgets is using GTK 2"));
#endif
#endif

  if (Configuration::m_configfileLocation_override != wxEmptyString)
    wxLogMessage(_("Reading the config from %s."),
		 Configuration::m_configfileLocation_override.utf8_str());
  else
    wxLogMessage(_("Reading the config from the default location."));
  
  // Some default values
  m_updateEvaluationQueueLengthDisplay = true;
  m_recentDocumentsMenu = NULL;
  m_unsavedDocumentsMenu = NULL;
  m_recentPackagesMenu = NULL;
  m_drawPane = NULL;
  m_EvaluationQueueLength = 0;
  m_commandsLeftInCurrentCell = 0;
  m_forceStatusbarUpdate = false;

  // Now it is time to construct the window contents.

  // console
  m_worksheet = new Worksheet(this, wxID_ANY, &m_configuration);
  //  wxWindowUpdateLocker worksheetBlocker(m_worksheet);

  // We need to create one pane which doesn't do a lot before the log pane
  // Otherwise the log pane will be displayed in a very strange way
  // The history pane was chosen randomly
  m_history = new History(this, -1, &m_configuration);

  // The table of contents
  m_worksheet->m_tableOfContents = new TableOfContents(
						       this, -1,
						       &m_configuration,
						       m_worksheet->GetTreeAddress());

  m_xmlInspector = new XmlInspector(this, -1);
  //  wxWindowUpdateLocker xmlInspectorBlocker(m_xmlInspector);
  m_statusBar = new StatusBar(this, -1);
  //  wxWindowUpdateLocker statusbarBlocker(m_statusBar);
  SetStatusBar(m_statusBar);
  m_StatusSaving = false;
  // If we need to set the status manually for the first time using
  // StatusMaximaBusy we first have to manually set the last state to something
  // else.
  m_StatusMaximaBusy = StatusBar::MaximaStatus::calculating;
  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

#if defined(__WXMSW__)
  // On Windows the taskbar icon needs to reside in the Resources file the
  // linker includes. Also it needs to be in Microsoft's own .ico format => This
  // file we don't ship with the source, but take it from the Resources file
  // instead.
  SetIcon(wxICON(icon0));
#elif defined(__WXGTK__)
  // This icon we include in the executable [in its compressed form] so we avoid
  // the questions "Was the icon file packaged with wxMaxima?" and "Can we
  // find it?".
  SetIcon(wxMaximaIcon());
#endif
#ifndef __WXOSX__
  SetTitle(
	   wxString::Format(_("wxMaxima %s (%s) "), wxS(GITVERSION),
			    wxPlatformInfo::Get().GetOperatingSystemDescription()) +
	   _("[ unsaved ]"));
#else
  SetTitle(_("untitled"));
#endif

  m_sidebarNames[EventIDs::menu_pane_console] = wxS("console");
  m_sidebarCaption[EventIDs::menu_pane_console] = _("The worksheet");
  m_manager.AddPane(m_worksheet,
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_console])
		    .Center()
		    .CloseButton(false)
		    .CaptionVisible(false)
		    .TopDockable(true)
		    .BottomDockable(true)
		    .LeftDockable(true)
		    .RightDockable(true)
		    .MinSize(wxSize(100 * GetContentScaleFactor(),
				    100 * GetContentScaleFactor()))
		    .PaneBorder(false)
		    .Row(2));

  m_worksheet->m_mainToolBar = new ToolBar(this);
  m_sidebarNames[EventIDs::menu_pane_toolbar] = wxS("toolbar");
  m_sidebarCaption[EventIDs::menu_pane_toolbar] = _("The main toolbar");
  m_manager.AddPane(m_worksheet->m_mainToolBar,
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_toolbar])
		    .Top()
		    .TopDockable(true)
		    .BottomDockable(true)
		    // .ToolbarPane().
                    .CaptionVisible(false)
		    .CloseButton(false)
		    .LeftDockable(false)
		    .DockFixed()
		    .Floatable(false)
		    .RightDockable(false)
		    .Gripper(false)
		    .Row(0));

  m_sidebarNames[EventIDs::menu_pane_history] = wxS("history");
  m_sidebarCaption[EventIDs::menu_pane_history] = _("History");
  m_manager.AddPane(m_history, wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_history])
		    .Right());

  m_sidebarNames[EventIDs::menu_pane_structure] = wxS("structure");
  m_sidebarCaption[EventIDs::menu_pane_structure] = _("Table of Contents");
  m_manager.AddPane(m_worksheet->m_tableOfContents, wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_structure])
		    .Right());

  m_sidebarNames[EventIDs::menu_pane_xmlInspector] = wxS("XmlInspector");
  m_sidebarCaption[EventIDs::menu_pane_xmlInspector] = _("Raw XML monitor");
  m_manager.AddPane(m_xmlInspector, wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_xmlInspector])
		    .Right());

  m_sidebarNames[EventIDs::menu_pane_stats] = wxS("stats");
  m_sidebarCaption[EventIDs::menu_pane_stats] = _("Statistics");
  wxWindow *statPane;
  m_manager.AddPane(statPane = CreateStatPane(), wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_stats])
		    .Left());
  //  wxWindowUpdateLocker statBlocker(statPane);

  m_sidebarNames[EventIDs::menu_pane_greek] = wxS("greek");
  m_sidebarCaption[EventIDs::menu_pane_greek] = _("Greek Letters");
  wxPanel *greekPane = new GreekPane(this, &m_configuration, m_worksheet);
  //  wxWindowUpdateLocker greekBlocker(greekPane);
  m_manager.AddPane(greekPane, wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_greek])
		    .Left());

  m_sidebarNames[EventIDs::menu_pane_unicode] = wxS("unicode");
  m_sidebarCaption[EventIDs::menu_pane_unicode] = _("Unicode characters");
  wxPanel *unicodePane =
    new UnicodeSidebar(this, m_worksheet, &m_configuration);
  //  wxWindowUpdateLocker unicodeBlocker(unicodePane);
  m_manager.AddPane(unicodePane,wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_unicode])
		    .Left());

  m_sidebarNames[EventIDs::menu_pane_log] = wxS("log");
  m_sidebarCaption[EventIDs::menu_pane_log] = _("Debug messages");
  m_manager.AddPane(m_logPane, wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_log])
		    .Left());

  // Avoid the strange non-responsive see-through log window on the mac
  wxWindowUpdateLocker avoidMacError(m_logPane);
  
  m_sidebarNames[EventIDs::menu_pane_variables] = wxS("variables");
  m_sidebarCaption[EventIDs::menu_pane_variables] = _("Variables");
  m_worksheet->m_variablesPane = new Variablespane(this, wxID_ANY);
  m_manager.AddPane(
		    m_worksheet->m_variablesPane,
		    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_variables])
		    .Bottom());

  m_sidebarNames[EventIDs::menu_pane_symbols] = wxS("symbols");
  m_sidebarCaption[EventIDs::menu_pane_symbols] = _("Mathematical Symbols");
  m_symbolsPane = new SymbolsPane(this, &m_configuration, m_worksheet);
  //  wxWindowUpdateLocker symbolsBlocker(m_symbolsPane);
  m_manager.AddPane(m_symbolsPane,
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_symbols])
		    .Left());

  m_sidebarNames[EventIDs::menu_pane_math] = wxS("math");
  m_sidebarCaption[EventIDs::menu_pane_math] = _("General Math");
  m_manager.AddPane(CreateMathPane(), wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_math])
		    .Left());

  m_sidebarNames[EventIDs::menu_pane_wizard] = wxS("wizard");
  m_sidebarCaption[EventIDs::menu_pane_wizard] = _("The current Wizard");
  m_manager.AddPane(m_wizard =
		    new ScrollingGenWizPanel(
					     this, &m_configuration,
					     m_worksheet->GetMaximaManual()),
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_wizard])
		    .Caption(wxS("Example Wizard")));

  
  m_sidebarNames[EventIDs::menu_pane_format] = wxS("format");
  m_sidebarCaption[EventIDs::menu_pane_format] = _("Insert");
  m_manager.AddPane(CreateFormatPane(), wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_format])
		    .Left());

  m_sidebarNames[EventIDs::menu_pane_draw] = wxS("draw");
  m_sidebarCaption[EventIDs::menu_pane_draw] = _("Plot using Draw");
  m_manager.AddPane(m_drawPane = new DrawPane(this, -1),
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_draw])
		    .Left());
  //  wxWindowUpdateLocker drawBlocker(m_drawPane);

#ifdef USE_WEBVIEW
  m_sidebarNames[EventIDs::menu_pane_help] = wxS("help");
  m_sidebarCaption[EventIDs::menu_pane_help] = _("Help");
  m_manager.AddPane(m_helpPane = new HelpBrowser(
						 this, &m_configuration, m_worksheet->GetMaximaManual(),
						 wxS("file://") + wxMaximaManualLocation()),
                    wxAuiPaneInfo()
		    .Name(m_sidebarNames[EventIDs::menu_pane_help])
		    .Right());
#endif  

  for(const auto &pane: m_sidebarNames)
    if(m_manager.GetPane(pane.second).IsOk())
      m_manager.GetPane(pane.second).
	Show(
	     (pane.first == EventIDs::menu_pane_toolbar) ||
	     (pane.first == EventIDs::menu_pane_console) ||
	     (pane.first == EventIDs::menu_pane_symbols) ||
	     (pane.first == EventIDs::menu_pane_draw) ||
	     (pane.first == EventIDs::menu_pane_greek) ||
	     (pane.first == EventIDs::menu_pane_structure));

  SetupMenu();
  
  // Read the perspektive (the sidebar state and positions).
  wxConfigBase *config = wxConfig::Get();
  wxString perspective;
  if(config->Read(wxS("AUI/perspective"), &perspective)) {
    // Loads the window states. We tell wxaui not to recalculate and display the
    // results of this step now as we will do so manually after
    // eventually adding the toolbar.
    m_manager.LoadPerspective(perspective, false);
  }

  m_worksheet->m_mainToolBar->Realize();

  // Loading the perspective rarely fails. But it might - and in this case we want
  // to set the common properties of our sidebars after loading them: This way we
  // overwrite wrong settings.
  //
  // The sidebar captions need to be set after LoadPerspective() since
  // LoadPerspective() loads the translations of the captions that were correct
  // when SavePerspective() was called.
  // The system's language might have changed since then.
  for(const auto &pane: m_sidebarNames)
    {
      wxSize minSiz = wxSize(0, 0);
      if(m_manager.GetPane(pane.second).IsOk())
	{
	  if(m_manager.GetPane(pane.second).window != NULL)
	    minSiz = m_manager.GetPane(pane.second).window->GetMinClientSize();
	  else
	    minSiz = wxSize(300 * GetContentScaleFactor(), 300 * GetContentScaleFactor());

	  if(minSiz.x < 100 * GetContentScaleFactor())
	    minSiz.x = 100 * GetContentScaleFactor();
	  
	  if(minSiz.y < 100 * GetContentScaleFactor())
	    minSiz.y = 100 * GetContentScaleFactor();
	  if(
	     (pane.first != EventIDs::menu_pane_console) &&
	     (pane.first != EventIDs::menu_pane_toolbar)
	     )
	    m_manager.GetPane(pane.second)
	      .Caption(m_sidebarCaption[pane.first])
	      .CloseButton(true)
	      .Layer(0)
	      .Row(1)
	      .MinSize(minSiz)
	      .BestSize(minSiz)
	      .FloatingSize(minSiz)
	      .PinButton(false)
	      .DockFixed(false)
	      .Gripper(false)
	      .PinButton(false)
	      .TopDockable(true)
	      .BottomDockable(true)
	      .LeftDockable(true)
	      .RightDockable(true)
	      .PaneBorder(true);
	}
    }

  // We still have no wizard we can show => hide the wizard until it is invoked.
  m_manager.GetPane(m_sidebarNames[EventIDs::menu_pane_wizard]).Show(false);
  // The xml inspector slows down everything => close it at startup
  m_manager.GetPane(m_sidebarNames[EventIDs::menu_pane_xmlInspector]).Show(false);
  // The unicode selector needs loads of time for starting up
  // => close it at startup
  m_manager.GetPane(m_sidebarNames[EventIDs::menu_pane_unicode]).Show(false);

  // It somehow is possible to hide the Maxima worksheet - which renders
  // wxMaxima basically useless => force it to be enabled.
  m_manager.GetPane(m_sidebarNames[EventIDs::menu_pane_console]).Show(true)
    .Center()
    .Show(true)
    .CloseButton(false)
    .CaptionVisible(false)
    .TopDockable(true)
    .BottomDockable(true)
    .LeftDockable(true)
    .RightDockable(true)
    .MinSize(wxSize(100, 100))
    .PaneBorder(false)
    .Row(2);

  m_manager.Update();

  Connect(wxEVT_MENU_HIGHLIGHT,
	  wxMenuEventHandler(wxMaximaFrame::OnMenuStatusText), NULL, this);
  Connect(EventIDs::menu_pane_dockAll, wxEVT_MENU,
          wxCommandEventHandler(wxMaximaFrame::DockAllSidebars), NULL, this);
}

wxSize wxMaximaFrame::DoGetBestClientSize() const {
  wxSize size(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * .6,
              wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * .6);
  if (size.x < 800)
    size.x = 800;
  if (size.y < 600)
    size.y = 600;
  return size;
}

void wxMaximaFrame::EvaluationQueueLength(int length, int numberOfCommands) {
  if ((length != m_EvaluationQueueLength) ||
      (m_commandsLeftInCurrentCell != numberOfCommands)) {
    m_updateEvaluationQueueLengthDisplay = true;
    m_commandsLeftInCurrentCell = numberOfCommands;
    m_EvaluationQueueLength = length;
  }
}

void wxMaximaFrame::UpdateStatusMaximaBusy() {
  // Do not block the events here, before we even know that the update is
  // needed. It causes a request for an idle event and causes constant idle
  // processing cpu use when wxMaxima is sitting idle.
  if ((m_StatusMaximaBusy != m_StatusMaximaBusy_next) ||
      (m_forceStatusbarUpdate) ||
      (!m_bytesReadDisplayTimer.IsRunning() &&
       (m_bytesFromMaxima != m_bytesFromMaxima_last) &&
       (m_StatusMaximaBusy_next == StatusBar::MaximaStatus::transferring))) {
    m_StatusMaximaBusy = m_StatusMaximaBusy_next;
    m_statusBar->UpdateStatusMaximaBusy(m_StatusMaximaBusy, m_bytesFromMaxima);
    if (!m_StatusSaving) {
      switch (m_StatusMaximaBusy) {
      case StatusBar::MaximaStatus::process_wont_start:
        m_bytesFromMaxima_last = 0;
        break;
      case StatusBar::MaximaStatus::userinput:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, false);
        break;
      case StatusBar::MaximaStatus::sending:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        // We don't evaluate any cell right now.
        break;
      case StatusBar::MaximaStatus::waiting:
        m_bytesFromMaxima_last = 0;
        m_worksheet->SetWorkingGroup(NULL);
        // If we evaluated a cell that produces no output we still want the
        // cell to be unselected after evaluating it.
        if (m_worksheet->FollowEvaluation())
          m_worksheet->ClearSelection();

        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        // We don't evaluate any cell right now.
        break;
      case StatusBar::MaximaStatus::calculating:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, false);
        break;
      case StatusBar::MaximaStatus::transferring:
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, false);
        m_bytesFromMaxima_last = m_bytesFromMaxima;
        m_bytesReadDisplayTimer.StartOnce(300);
        break;
      case StatusBar::MaximaStatus::parsing:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, false);
        break;
      case StatusBar::MaximaStatus::disconnected:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        break;
      case StatusBar::MaximaStatus::wait_for_start:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        break;
      case StatusBar::MaximaStatus::waitingForAuth:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        break;
      case StatusBar::MaximaStatus::waitingForPrompt:
        m_bytesFromMaxima_last = 0;
        m_MenuBar->EnableItem(EventIDs::menu_remove_output, true);
        break;
      }
    }
  }
  m_forceStatusbarUpdate = false;
}

void wxMaximaFrame::StatusSaveStart() {
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  StatusText(_("Saving..."));
}

void wxMaximaFrame::StatusSaveFinished() {
  m_forceStatusbarUpdate = true;
  StatusText(_("Saving successful."));
}

void wxMaximaFrame::StatusExportStart() {
  m_forceStatusbarUpdate = true;
  m_StatusSaving = true;
  StatusText(_("Exporting..."));
}

void wxMaximaFrame::StatusExportFinished() {
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  StatusText(_("Export successful."));
}

void wxMaximaFrame::StatusSaveFailed() {
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  StatusText(_("Saving failed."));
}

void wxMaximaFrame::StatusExportFailed() {
  m_forceStatusbarUpdate = true;
  m_StatusSaving = false;
  StatusText(_("Export failed."));
}

wxMaximaFrame::~wxMaximaFrame() {
  wxString perspective = m_manager.SavePerspective();

  wxConfig::Get()->Write(wxS("AUI/perspective"), perspective);
  m_manager.UnInit();
}

void wxMaximaFrame::SetupMenu() {
  // Silence a few warnings about non-existing icons.
  SuppressErrorDialogs iconWarningBlocker;

  m_MenuBar = new MainMenuBar();
  // Enables the window list on MacOs.
#ifdef __WXMAC__
  m_MenuBar->SetAutoWindowMenu(true);
#endif

#define APPEND_MENU_ITEM(menu, id, label, help, stock)	\
  (menu)->Append((id), (label), (help), wxITEM_NORMAL);

  // File menu
  m_FileMenu = new wxMenu;
#if defined __WXOSX__
  m_FileMenu->Append(wxID_NEW, _("New\tCtrl+N"), _("Open a new window"));
#else
  APPEND_MENU_ITEM(m_FileMenu, wxID_NEW, _("New\tCtrl+N"),
                   _("Open a new window"), wxS("gtk-new"));
#endif
  APPEND_MENU_ITEM(m_FileMenu, wxID_OPEN, _("&Open...\tCtrl+O"),
                   _("Open a document"), wxS("gtk-open"));
  m_recentDocumentsMenu = new wxMenu();
  m_FileMenu->Append(EventIDs::menu_recent_documents, _("Open recent"),
                     m_recentDocumentsMenu);
  m_unsavedDocumentsMenu = new wxMenu();
  m_FileMenu->Append(EventIDs::menu_unsaved_documents, _("Recover unsaved"),
                     m_unsavedDocumentsMenu);
  m_FileMenu->AppendSeparator();
  m_FileMenu->Append(wxID_CLOSE, _("Close\tCtrl+W"), _("Close window"),
                     wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_FileMenu, wxID_SAVE, _("&Save\tCtrl+S"),
                   _("Save document"), wxS("gtk-save"));
  APPEND_MENU_ITEM(m_FileMenu, wxID_SAVEAS, _("Save As...\tShift+Ctrl+S"),
                   _("Save document as"), wxS("gtk-save"));
  m_FileMenu->Append(EventIDs::menu_load_id, _("&Load Package...\tCtrl+L"),
                     _("Load a Maxima package file"), wxITEM_NORMAL);
  m_recentPackagesMenu = new wxMenu();
  m_FileMenu->Append(EventIDs::menu_recent_packages, _("Load Recent Package"),
                     m_recentPackagesMenu);
  m_FileMenu->Append(EventIDs::menu_batch_id, _("&Batch File...\tCtrl+B"),
                     _("Load a Maxima file using the batch command"),
                     wxITEM_NORMAL);
  m_FileMenu->Append(EventIDs::menu_export_html, _("&Export..."),
                     _("Export document to a HTML or LaTeX file"),
                     wxITEM_NORMAL);
  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_PRINT, _("&Print...\tCtrl+P"),
                   _("Print document"), wxS("gtk-print"));

  m_FileMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_FileMenu, wxID_EXIT, _("E&xit\tCtrl+Q"),
                   _("Exit wxMaxima"), wxS("gtk-quit"));
  m_MenuBar->Append(m_FileMenu, _("&File"));

  m_EditMenu = new wxMenu;
  m_EditMenu->Append(wxID_UNDO, _("Undo\tCtrl+Z"), _("Undo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(wxID_REDO, _("Redo\tCtrl+Y"), _("Redo last change"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(wxID_CUT, _("Cut\tCtrl+X"), _("Cut selection"),
                     wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_EditMenu, wxID_COPY, _("&Copy\tCtrl+C"),
                   _("Copy selection"), wxS("gtk-copy"));
  m_EditMenu->Append(EventIDs::menu_copy_text_from_worksheet,
                     _("Copy as Text\tCtrl+Shift+C"),
                     _("Copy selection from document as text"), wxITEM_NORMAL);
  m_EditMenu->Append(
		     EventIDs::menu_copy_matlab_from_worksheet, _("Copy for Octave/Matlab"),
		     _("Copy selection from document in Matlab format"), wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::menu_copy_tex_from_worksheet, _("Copy as LaTeX"),
                     _("Copy selection from document in LaTeX format"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::popid_copy_mathml, _("Copy as MathML"),
                     _("Copy selection from document in a MathML format many "
                       "word processors can display as 2d equation"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::menu_copy_as_bitmap, _("Copy as Image"),
                     _("Copy selection from document as an image"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::menu_copy_as_svg, _("Copy as SVG"),
                     _("Copy selection from document as an SVG image"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::menu_copy_as_rtf, _("Copy as RTF"),
                     _("Copy selection from document as rtf that a word "
                       "processor might understand"),
                     wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
  m_EditMenu->Append(EventIDs::menu_copy_as_emf, _("Copy as EMF"),
                     _("Copy selection from document as an Enhanced Metafile"),
                     wxITEM_NORMAL);
#endif
  m_EditMenu->Append(wxID_PASTE, _("Paste\tCtrl+V"),
                     _("Paste text from clipboard"), wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(wxID_FIND, _("Find\tCtrl+F"), _("Find and replace"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(wxID_SELECTALL, _("Select All\tCtrl+A"), _("Select all"),
                     wxITEM_NORMAL);
  m_EditMenu->Append(EventIDs::menu_copy_to_file, _("Save Selection to Image..."),
                     _("Save selection from document to an image file"),
                     wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
  m_EditMenu->Append(
		     EventIDs::popid_comment_selection, _("Comment selection\tCtrl+/"),
		     _("Comment out the currently selected text"), wxITEM_NORMAL);
  m_EditMenu->AppendSeparator();
#if defined __WXOSX__
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("Preferences...\tCtrl+,"),
                   _("Configure wxMaxima"), wxS("gtk-preferences"));
#else
  APPEND_MENU_ITEM(m_EditMenu, wxID_PREFERENCES, _("C&onfigure"),
                   _("Configure wxMaxima"), wxS("gtk-preferences"));
#endif
  m_MenuBar->Append(m_EditMenu, _("&Edit"));

  m_viewMenu = new wxMenu;
  // Sidebars
  m_Maxima_Panes_Sub = new wxMenu;
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_toolbar,
                                      _("Main Toolbar\tAlt+Shift+B"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_math,
                                      _("General Math\tAlt+Shift+M"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_stats,
                                      _("Statistics\tAlt+Shift+S"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_greek,
                                      _("Greek Letters\tAlt+Shift+G"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_symbols,
                                      _("Symbols\tAlt+Shift+Y"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_unicode,
                                      _("Unicode chars\tAlt+Shift+U"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_history,
                                      _("History\tAlt+Shift+I"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_structure,
                                      _("Table of Contents\tAlt+Shift+T"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_format,
                                      _("Insert Cell\tAlt+Shift+C"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_draw, _("Plot using Draw"));
  
#ifdef USE_WEBVIEW
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_help,
                                      _("The integrated help browser"));
#endif
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_log, _("Debug messages"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_variables, _("Variables"));
  m_Maxima_Panes_Sub->AppendCheckItem(EventIDs::menu_pane_xmlInspector,
                                      _("Raw XML monitor"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->Append(EventIDs::menu_pane_dockAll, _("Dock all Sidebars"));
  m_Maxima_Panes_Sub->AppendSeparator();
  m_Maxima_Panes_Sub->AppendCheckItem(ToolBar::tb_hideCode,
                                      _("Hide Code Cells\tAlt+Ctrl+H"));
  m_Maxima_Panes_Sub->Append(EventIDs::menu_pane_hideall,
                             _("Hide All Toolbars\tAlt+Shift+-"),
                             _("Hide all panes"), wxITEM_NORMAL);

  m_viewMenu->Append(wxWindow::NewControlId(), _("Sidebars"), m_Maxima_Panes_Sub,
                     _("All visible sidebars"));
  m_viewMenu->AppendSeparator();

  // equation display type submenu
  m_equationTypeMenuMenu = new wxMenu;
  m_equationTypeMenuMenu->AppendRadioItem(
					  EventIDs::menu_math_as_1D_ASCII, _("as 1D ASCII"),
					  _("Show equations in their linear form"));
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_2D_ASCII,
                                          _("as ASCII Art"),
                                          _("2D equations using ASCII Art"));
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_graphics, _("in 2D"),
                                          _("Nice Graphical Equations"));
  m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_graphics, true);

  m_viewMenu->Append(wxWindow::NewControlId(), _("Display equations"), m_equationTypeMenuMenu,
                     _("How to display new equations"));

  m_autoSubscriptMenu = new wxMenu;
  m_autoSubscriptMenu->AppendRadioItem(
				       EventIDs::menu_alwaysAutosubscript, _("Always after underscores"),
				       _("Always autosubscript after an underscore"));
  m_autoSubscriptMenu->AppendRadioItem(
				       EventIDs::menu_defaultAutosubscript, _("Only Integers and single letters"),
				       _("Autosubscript numbers and text following single letters"));
  m_autoSubscriptMenu->AppendRadioItem(
				       EventIDs::menu_noAutosubscript, _("Never"),
				       _("Don't autosubscript after an underscore"));
  m_autoSubscriptMenu->Check(EventIDs::menu_defaultAutosubscript, true);
  m_autoSubscriptMenu->AppendSeparator();
  m_autoSubscriptMenu->Append(EventIDs::menu_autosubscriptIndividual,
                              _("Always display this variable with subscript"));
  m_autoSubscriptMenu->Append(EventIDs::menu_noAutosubscriptIndividual,
                              _("Never display this variable with subscript"));
  m_autoSubscriptMenu->Append(
			      EventIDs::menu_declareAutosubscript,
			      _("Declare Text to always be displayed as subscript"));

  m_viewMenu->Append(wxWindow::NewControlId(), _("Autosubscript"), m_autoSubscriptMenu,
                     _("Autosubscript chars after an underscore"));
  m_roundedMatrixParensMenu = new wxMenu;
  m_roundedMatrixParensMenu->AppendRadioItem(
					     EventIDs::menu_roundedMatrixParens, _("Rounded"),
					     _("Use rounded parenthesis for matrices"));
  m_roundedMatrixParensMenu->AppendRadioItem(
					     EventIDs::menu_squareMatrixParens, _("Square"),
					     _("Use square parenthesis for matrices"));
  m_roundedMatrixParensMenu->AppendRadioItem(
					     EventIDs::menu_angledMatrixParens, _("Angles"),
					     _("Use \"<\" and \">\" as parenthesis for matrices"));
  m_roundedMatrixParensMenu->AppendRadioItem(
					     EventIDs::menu_straightMatrixParens, _("Straight lines"),
					     _("Use vertical lines instead of parenthesis for matrices"));
  m_roundedMatrixParensMenu->AppendRadioItem(
					     EventIDs::menu_noMatrixParens, _("None"), _("Don't use parenthesis for matrices"));
  m_viewMenu->Append(wxWindow::NewControlId(), _("Matrix parenthesis"),
                     m_roundedMatrixParensMenu,
                     _("Choose the parenthesis type for Matrices"));
  m_viewMenu->AppendCheckItem(EventIDs::menu_stringdisp,
                              _("Display string quotation marks"));

  m_viewMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_viewMenu, wxID_ZOOM_IN, _("Zoom &In\tCtrl++"),
                   _("Zoom in 10%"), wxS("gtk-zoom-in"));
  APPEND_MENU_ITEM(m_viewMenu, wxID_ZOOM_OUT, _("Zoom Ou&t\tCtrl+-"),
                   _("Zoom out 10%"), wxS("gtk-zoom-out"));
  // zoom submenu
  m_Edit_Zoom_Sub = new wxMenu;
  m_Edit_Zoom_Sub->Append(EventIDs::menu_zoom_80, wxS("80%"), _("Set zoom to 80%"),
                          wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(wxID_ZOOM_100, wxS("100%"), _("Set zoom to 100%"),
                          wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(EventIDs::menu_zoom_120, wxS("120%"), _("Set zoom to 120%"),
                          wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(EventIDs::menu_zoom_150, wxS("150%"), _("Set zoom to 150%"),
                          wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(EventIDs::menu_zoom_200, wxS("200%"), _("Set zoom to 200%"),
                          wxITEM_NORMAL);
  m_Edit_Zoom_Sub->Append(EventIDs::menu_zoom_300, wxS("300%"), _("Set zoom to 300%"),
                          wxITEM_NORMAL);

  m_viewMenu->Append(wxWindow::NewControlId(), _("Set Zoom"), m_Edit_Zoom_Sub, _("Set Zoom"));
#ifdef __UNIX__
  m_viewMenu->Append(EventIDs::menu_fullscreen, _("Full Screen\tF11"),
                     _("Toggle full screen editing"), wxITEM_NORMAL);
#else
  m_viewMenu->Append(EventIDs::menu_fullscreen, _("Full Screen\tAlt-Enter"),
                     _("Toggle full screen editing"), wxITEM_NORMAL);
#endif
  m_viewMenu->AppendSeparator();
  m_viewMenu->AppendCheckItem(EventIDs::menu_invertWorksheetBackground,
                              _("Invert worksheet brightness"));
  m_viewMenu->Check(EventIDs::menu_invertWorksheetBackground,
                    m_configuration.InvertBackground());

  m_MenuBar->Append(m_viewMenu, _("View"));

  // Cell menu
  m_CellMenu = new wxMenu;
  {
    wxMenuItem *it =
      new wxMenuItem(m_CellMenu, EventIDs::menu_evaluate, _("Evaluate Cell(s)"),
		     _("Evaluate active or selected cell(s)"), wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetEvalBitmap(
							    wxRendererNative::Get().GetCheckBoxSize(this)));
    m_CellMenu->Append(it);
  }
  m_CellMenu->Append(
		     EventIDs::EventIDs::menu_evaluate_all_visible, _("Evaluate All Visible Cells\tCtrl+R"),
		     _("Evaluate all visible cells in the document"), wxITEM_NORMAL);
  {
    wxMenuItem *it = new wxMenuItem(
				    m_CellMenu, EventIDs::menu_evaluate_all, _("Evaluate All Cells\tCtrl+Shift+R"),
				    _("Evaluate all cells in the document"), wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetEvalAllBitmap(
							       wxRendererNative::Get().GetCheckBoxSize(this)));
    m_CellMenu->Append(it);
  }
  {
    wxMenuItem *it = new wxMenuItem(
				    m_CellMenu, ToolBar::tb_evaltillhere,
				    _("Evaluate Cells Above\tCtrl+Shift+P"),
				    _("Re-evaluate all cells above the one the cursor is in"),
				    wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetEvalTillHereBitmap(
								    wxRendererNative::Get().GetCheckBoxSize(this)));
    m_CellMenu->Append(it);
  }
  {
    wxMenuItem *it = new wxMenuItem(
				    m_CellMenu, ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"),
				    _("Re-evaluate all cells below the one the cursor is in"),
				    wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetEvalRestBitmap(
								wxRendererNative::Get().GetCheckBoxSize(this)));
    m_CellMenu->Append(it);
  }
  m_CellMenu->Append(EventIDs::menu_remove_output, _("Remove All Output"),
                     _("Remove output from input cells"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(EventIDs::menu_insert_previous_input,
                     _("Copy Previous Input\tCtrl+I"),
                     _("Create a new cell with previous input"), wxITEM_NORMAL);
  m_CellMenu->Append(
		     EventIDs::menu_insert_previous_output, _("Copy Previous Output\tCtrl+U"),
		     _("Create a new cell with previous output"), wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::menu_autocomplete, _("Complete Word\tCtrl+Space"),
                     _("Complete word"), wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::menu_autocomplete_templates,
                     _("Show Template\tCtrl+Shift+Space"),
                     _("Show function template"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  wxMenu *insert_sub = new wxMenu;
  insert_sub->Append(EventIDs::menu_insert_input, _("Insert Input &Cell\tCtrl+0"),
                     _("Insert a new input cell"));
  insert_sub->Append(EventIDs::menu_add_comment, _("Insert &Text Cell\tCtrl+1"),
                     _("Insert a new text cell"));
  insert_sub->Append(EventIDs::menu_add_title, _("Insert T&itle Cell\tCtrl+2"),
                     _("Insert a new title cell"));
  insert_sub->Append(EventIDs::menu_add_section, _("Insert &Section Cell\tCtrl+3"),
                     _("Insert a new section cell"));
  insert_sub->Append(EventIDs::menu_add_subsection, _("Insert S&ubsection Cell\tCtrl+4"),
                     _("Insert a new subsection cell"));
  insert_sub->Append(EventIDs::menu_add_subsubsection,
                     _("Insert S&ubsubsection Cell\tCtrl+5"),
                     _("Insert a new subsubsection cell"));
  insert_sub->Append(EventIDs::menu_add_heading5, _("Insert heading5 Cell\tCtrl+6"),
                     _("Insert a new heading5 cell"));
  insert_sub->Append(EventIDs::menu_add_heading6, _("Insert heading6 Cell\tCtrl+7"),
                     _("Insert a new heading6 cell"));
  insert_sub->Append(EventIDs::menu_add_pagebreak, _("Insert Page Break"),
                     _("Insert a page break"));
  m_CellMenu->Append(wxWindow::NewControlId(), _("Insert textbased cell"), insert_sub);
  m_CellMenu->Append(EventIDs::menu_insert_image, _("Insert Image..."), _("Insert image"),
                     wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(EventIDs::menu_fold_all_cells, _("Fold All\tCtrl+Alt+["),
                     _("Fold all sections"), wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::menu_unfold_all_cells, _("Unfold All\tCtrl+Alt+]"),
                     _("Unfold all folded sections"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(EventIDs::menu_history_previous, _("Previous Command\tAlt+Up"),
                     _("Recall previous command from history"), wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::menu_history_next, _("Next Command\tAlt+Down"),
                     _("Recall next command from history"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->Append(EventIDs::popid_merge_cells, _("Merge Cells\tCtrl+M"),
                     _("Merge the text from two input cells into one"),
                     wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::popid_divide_cell, _("Divide Cell\tCtrl+D"),
                     _("Divide this input cell into two cells"), wxITEM_NORMAL);
  m_CellMenu->AppendSeparator();
  m_CellMenu->AppendCheckItem(
			      EventIDs::popid_auto_answer, _("Automatically answer questions"),
			      _("Automatically fill in answers known from the last run"));

  m_MenuBar->Append(m_CellMenu, _("Ce&ll"));

  // Maxima menu
  m_MaximaMenu = new wxMenu;

  {
    wxMenuItem *it =
      new wxMenuItem(m_MaximaMenu, EventIDs::menu_interrupt_id, _("&Interrupt\tCtrl+G"),
		     _("Interrupt current computation"), wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetInterruptBitmap(
								 wxRendererNative::Get().GetCheckBoxSize(this)));
    m_MaximaMenu->Append(it);
  }

  {
    wxMenuItem *it = new wxMenuItem(m_MaximaMenu, ToolBar::menu_restart_id,
                                    _("&Restart Maxima"), _("Restart Maxima"),
                                    wxITEM_NORMAL);
    it->SetBitmap(m_worksheet->m_mainToolBar->GetRestartBitmap(
							       wxRendererNative::Get().GetCheckBoxSize(this)));
    m_MaximaMenu->Append(it);
  }
  m_MaximaMenu->Append(EventIDs::menu_soft_restart, _("&Clear Memory"),
                       _("Delete all values from memory"), wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_MaximaMenu, EventIDs::menu_add_path, _("Add to &Path..."),
                   _("Add a directory to search path"), wxS("gtk-add"));

  m_MaximaMenu->AppendSeparator();
  wxMenu *infolists_sub = new wxMenu;
  infolists_sub->Append(EventIDs::menu_functions, _("&Functions"),
                        _("Show defined functions"), wxITEM_NORMAL);
  infolists_sub->Append(EventIDs::menu_variables, _("&Variables"),
                        _("Show defined variables"), wxITEM_NORMAL);
  infolists_sub->Append(EventIDs::menu_arrays, _("Arrays"));
  infolists_sub->Append(EventIDs::menu_macros, _("Macros"));
  infolists_sub->Append(EventIDs::menu_labels, _("Labels"));
  infolists_sub->Append(EventIDs::menu_myoptions, _("Changed option variables"));
  infolists_sub->Append(EventIDs::menu_rules, _("Rules"));
  infolists_sub->Append(EventIDs::menu_aliases, _("Aliases"));
  infolists_sub->Append(EventIDs::menu_structs, _("Structs"));
  infolists_sub->Append(EventIDs::menu_dependencies, _("Dependencies"));
  infolists_sub->Append(EventIDs::menu_gradefs, _("Gradefs"));
  infolists_sub->Append(EventIDs::menu_let_rule_packages, _("Letrules"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("Show user definitions"), infolists_sub);

  m_MaximaMenu->Append(EventIDs::menu_clear_fun, _("Delete F&unction..."),
                       _("Delete a function"), wxITEM_NORMAL);
  m_MaximaMenu->Append(EventIDs::menu_clear_var, _("Delete V&ariable..."),
                       _("Delete a variable"), wxITEM_NORMAL);
  m_MaximaMenu->Append(EventIDs::menu_kill, _("Delete named object..."),
                       _("Delete all objects with a given name"),
                       wxITEM_NORMAL);

  m_MaximaMenu->AppendSeparator();
  m_MaximaMenu->AppendCheckItem(EventIDs::menu_time, _("&Time Display"),
                                _("Display time used for evaluation"));
  m_MaximaMenu->Append(
		       EventIDs::menu_display, _("Change &2d Display"),
		       _("Change the 2d display algorithm used to display math output"),
		       wxITEM_NORMAL);
  m_MaximaMenu->Append(EventIDs::menu_texform, _("Display Te&X Form"),
                       _("Display last result in TeX form"), wxITEM_NORMAL);
  m_MaximaMenu->Append(EventIDs::menu_grind, _("Maxima input"),
                       _("Convert a command to maxima code"), wxITEM_NORMAL);

  m_MaximaMenu->AppendSeparator();
  m_MaximaMenu->Append(
		       EventIDs::menu_jumptoerror, _("Jump to first error"),
		       _("Jump to the first cell Maxima has reported an error in."),
		       wxITEM_NORMAL);
  // debugger type submenu
  m_debugTypeMenu = new wxMenu;
  m_debugTypeMenu->AppendRadioItem(EventIDs::menu_debugmode_off, _("Never"));
  m_debugTypeMenu->AppendRadioItem(EventIDs::menu_debugmode_lisp, _("On lisp errors"));
  m_debugTypeMenu->AppendRadioItem(EventIDs::menu_debugmode_all, _("On all errors"));
  m_debugTypeMenu->Check(EventIDs::menu_debugmode_off, true);
  m_MaximaMenu->Append(EventIDs::menu_debugmode, _("Debugger trigger"), m_debugTypeMenu,
                       _("When to invoke the lisp compiler's debugger"));
  m_MaximaMenu->Enable(EventIDs::menu_debugmode, false);

  wxMenu *programMenu = new wxMenu;
  programMenu->Append(EventIDs::menu_for, _("For loop..."));
  programMenu->Append(EventIDs::menu_while, _("While loop..."));
  programMenu->Append(EventIDs::menu_list_do_for_each_element, _("do for each element"));
  programMenu->AppendSeparator();
  programMenu->Append(EventIDs::menu_block, _("Program block with local variables..."));
  programMenu->Append(EventIDs::menu_block_noLocal,
                      _("Program block, no local variables..."));
  programMenu->Append(EventIDs::menu_return, _("Return from current block/loop..."));
  programMenu->Append(EventIDs::menu_local, _("Make function local..."));
  programMenu->AppendSeparator();
  programMenu->Append(EventIDs::menu_def_fun, _("Define function..."));
  m_MaximaMenu->Append(EventIDs::menu_fun_def, _("Show function &Definition..."),
                       _("Show definition of a function"), wxITEM_NORMAL);
  programMenu->Append(EventIDs::menu_lambda, _("unnamed function (lambda)..."));
  programMenu->Append(EventIDs::menu_quotequote, _("Compile function..."));
  programMenu->Append(EventIDs::menu_paramType, _("Define parameter type..."));
  programMenu->Append(EventIDs::menu_trace, _("Trace a function..."));
  programMenu->Append(EventIDs::menu_def_macro, _("Define macro..."));
  programMenu->Append(EventIDs::menu_def_variable, _("Define variable..."));
  programMenu->Append(EventIDs::menu_structdef, _("Define struct..."));
  programMenu->Append(EventIDs::menu_structnew, _("Create struct..."));
  programMenu->Append(EventIDs::menu_structuse, _("Use struct field..."));
  programMenu->AppendSeparator();
  programMenu->Append(EventIDs::menu_quote, _("Variable name, not contents..."));
  programMenu->Append(EventIDs::menu_quoteblock, _("Don't evaluate Expression..."));
  programMenu->Append(EventIDs::menu_quotequote, _("Interpret like input..."));
  programMenu->Append(EventIDs::menu_maximatostring, _("Expression to string..."));
  programMenu->Append(EventIDs::menu_stringtomaxima, _("Interpret string..."));
  programMenu->AppendSeparator();
  programMenu->Append(EventIDs::menu_saveLisp, _("Save as lisp..."));
  programMenu->Append(EventIDs::menu_loadLisp, _("Load lisp..."));
  programMenu->AppendSeparator();
  programMenu->Append(EventIDs::menu_gensym, _("Generate unused symbol name"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("Program"), programMenu);
  wxMenu *stringMenu = new wxMenu;
  wxMenu *streamMenu = new wxMenu;
  streamMenu->Append(EventIDs::menu_stringproc_openr, _("Open for reading..."));
  streamMenu->Append(EventIDs::menu_stringproc_opena, _("Open for appending..."));
  streamMenu->Append(EventIDs::menu_stringproc_openw, _("Open for writing..."));
  streamMenu->Append(EventIDs::menu_stringproc_setposition, _("Set position..."));
  streamMenu->Append(EventIDs::menu_stringproc_getposition, _("Get position..."));
  streamMenu->Append(EventIDs::menu_stringproc_flush_output, _("Flush..."));
  streamMenu->Append(EventIDs::menu_stringproc_flength, _("Length..."));
  streamMenu->Append(EventIDs::menu_stringproc_close, _("Close..."));
  stringMenu->Append(wxWindow::NewControlId(), _("Stream"), streamMenu);
  wxMenu *ioMenu = new wxMenu;
  ioMenu->Append(EventIDs::menu_stringproc_printf, _("printf..."));
  ioMenu->Append(EventIDs::menu_stringproc_readline, _("readline..."));
  ioMenu->Append(EventIDs::menu_stringproc_readchar, _("readchar..."));
  ioMenu->Append(EventIDs::menu_stringproc_readbyte, _("readbyte..."));
  ioMenu->Append(EventIDs::menu_stringproc_writebyte, _("writebyte..."));
  stringMenu->Append(wxWindow::NewControlId(), _("I/O"), ioMenu);
  wxMenu *charTestMenu = new wxMenu;
  charTestMenu->Append(EventIDs::menu_stringproc_charp, _("Is a char?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_alphacharp, _("Is alphabetic?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_alphanumericp, _("Is alphanumeric?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_digitcharp, _("Is a digit?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_constituent, _("Is printable?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_uppercasep, _("Is uppercase?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_lowercasep, _("Is lowercase?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_cequal, _("Is equal?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_cequalignore,
                       _("Equal, ignoring case?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_clessp, _("Is lower?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_clesspignore,
                       _("Lower, ignoring case?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_cgreaterp, _("Is greater?..."));
  charTestMenu->Append(EventIDs::menu_stringproc_cgreaterpignore,
                       _("Greater, ignoring case?..."));
  stringMenu->Append(wxWindow::NewControlId(), _("Character Tests"), charTestMenu);
  wxMenu *stringtestMenu = new wxMenu;
  stringtestMenu->Append(EventIDs::menu_stringproc_sequal, _("Equal?..."));
  stringtestMenu->Append(EventIDs::menu_stringproc_sequalignore,
                         _("Equal, ignoring case?..."));
  stringMenu->Append(wxWindow::NewControlId(), _("String Tests"), stringtestMenu);
  wxMenu *convertMenu = new wxMenu;
  convertMenu->Append(EventIDs::menu_stringproc_create_ascii, _("Ascii code to char..."));
  convertMenu->Append(EventIDs::menu_stringproc_ascii, _("char to Ascii code..."));
  convertMenu->Append(EventIDs::menu_stringproc_cint, _("Char to Unicode code point..."));
  convertMenu->Append(EventIDs::menu_stringproc_unicode,
                      _("Unicode code point to char..."));
  convertMenu->Append(EventIDs::menu_stringproc_unicode_to_utf8,
                      _("Unicode code point to UTF8..."));
  convertMenu->Append(EventIDs::menu_stringproc_utf8_to_unicode,
                      _("UTF8 to Unicode code point..."));
  convertMenu->Append(EventIDs::menu_stringproc_charlist,
                      _("String to list of chars..."));
  convertMenu->Append(EventIDs::menu_stringproc_simplode,
                      _("List of chars to string..."));
  convertMenu->Append(EventIDs::menu_stringproc_eval_string, _("Evaluate string..."));
  convertMenu->Append(EventIDs::menu_stringproc_parse_string, _("Parse string only..."));
  convertMenu->Append(EventIDs::menu_stringproc_number_to_octets,
                      _("Number to octets..."));
  convertMenu->Append(EventIDs::menu_stringproc_octets_to_number,
                      _("Octets to number..."));
  convertMenu->Append(EventIDs::menu_stringproc_octets_to_string,
                      _("Octets to string..."));
  convertMenu->Append(EventIDs::menu_stringproc_string_to_octets,
                      _("String to octets..."));
  stringMenu->Append(wxWindow::NewControlId(), _("Conversions"), convertMenu);
  wxMenu *transformMenu = new wxMenu;
  transformMenu->Append(EventIDs::menu_stringproc_charat, _("Extract char..."));
  transformMenu->Append(EventIDs::menu_stringproc_sinsert, _("Insert char..."));
  transformMenu->Append(EventIDs::menu_stringproc_scopy, _("Copy string..."));
  transformMenu->Append(EventIDs::menu_stringproc_sdowncase, _("Convert to downcase..."));
  transformMenu->Append(EventIDs::menu_stringproc_slength, _("Length..."));
  transformMenu->Append(EventIDs::menu_stringproc_smake, _("Create empty string..."));
  transformMenu->Append(EventIDs::menu_stringproc_smismatch,
                        _("Find first difference..."));
  transformMenu->Append(EventIDs::menu_stringproc_split, _("Split..."));
  transformMenu->Append(EventIDs::menu_stringproc_sposition, _("Find char in string..."));
  transformMenu->Append(EventIDs::menu_stringproc_sremove,
                        _("Remove all occurrences of a word..."));
  transformMenu->Append(EventIDs::menu_stringproc_sremovefirst,
                        _("Remove first occurrence of a word..."));
  transformMenu->Append(EventIDs::menu_stringproc_tokens, _("Tokenize..."));
  transformMenu->Append(EventIDs::menu_stringproc_ssearch, _("Search..."));
  transformMenu->Append(EventIDs::menu_stringproc_ssort, _("Sort the characters..."));
  transformMenu->Append(EventIDs::menu_stringproc_ssubstfirst, _("Replace..."));
  transformMenu->Append(EventIDs::menu_stringproc_strim, _("Trim both ends..."));
  transformMenu->Append(EventIDs::menu_stringproc_striml, _("Trim left..."));
  transformMenu->Append(EventIDs::menu_stringproc_strimr, _("Trim right..."));
  stringMenu->Append(wxWindow::NewControlId(), _("Transformations"), transformMenu);
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("String"), stringMenu);
  wxMenu *regexMenu = new wxMenu;
  regexMenu->Append(EventIDs::menu_sregex_load, _("Load the regex processor"));
  regexMenu->Append(EventIDs::menu_sregex_regex_compile, _("Compile a regex"));
  regexMenu->Append(EventIDs::menu_sregex_regex_match_pos, _("Position of a match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_match, _("Return a match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_split, _("Split on match"));
  regexMenu->Append(EventIDs::menu_sregex_subst_first, _("Substitute first match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_subst, _("Substitute all matches"));
  regexMenu->Append(EventIDs::menu_sregex_string_to_regex,
                    _("Regex that matches a string"));
  stringMenu->Append(wxWindow::NewControlId(), _("RegEx"), regexMenu);
  wxMenu *operatingSystemMenu = new wxMenu;
  regexMenu->Append(EventIDs::menu_opsyst_load, _("Load the file/dir operations"));
  wxMenu *dirMenu = new wxMenu;
  dirMenu->Append(EventIDs::menu_opsyst_directory, _("List directory"));
  dirMenu->Append(EventIDs::menu_opsyst_getcurrentdirectory, _("Get current directory"));
  dirMenu->Append(EventIDs::menu_opsyst_chdir, _("Change directory..."));
  dirMenu->Append(EventIDs::menu_opsyst_mkdir, _("Create directory..."));
  dirMenu->Append(EventIDs::menu_opsyst_rmdir, _("Remove directory..."));
  dirMenu->Append(EventIDs::menu_opsyst_directory, _("List directory..."));
  dirMenu->Append(EventIDs::menu_opsyst_pathname_directory,
                  _("Extract dir from path..."));
  dirMenu->Append(EventIDs::menu_opsyst_pathname_name,
                  _("Extract filename from path..."));
  dirMenu->Append(EventIDs::menu_opsyst_pathname_type,
                  _("Extract filetype from path..."));
  operatingSystemMenu->Append(wxWindow::NewControlId(), _("Directory operations"), dirMenu);
  wxMenu *fileMenu = new wxMenu;
  fileMenu->Append(EventIDs::menu_opsyst_copy_file, _("Copy file..."));
  fileMenu->Append(EventIDs::menu_opsyst_rename_file, _("Rename file..."));
  fileMenu->Append(EventIDs::menu_opsyst_delete_file, _("Delete file..."));
  operatingSystemMenu->Append(wxWindow::NewControlId(), _("File operations"), fileMenu);
  wxMenu *envMenu = new wxMenu;
  envMenu->Append(EventIDs::menu_opsyst_getenv, _("Read environment variable..."));
  operatingSystemMenu->Append(wxWindow::NewControlId(), _("Environment variables"), envMenu);
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("mkdir,..."), operatingSystemMenu);

  m_gentranMenu = new wxMenu;
  m_gentranMenu->Append(EventIDs::gentran_load, _("Load the translation generator"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_c, _("Output C"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_fortran, _("Output Fortran"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_ratfor,
                                 _("Output Rational Fortran"));
  m_gentranMenu->Append(EventIDs::gentran_to_stdout, _("Convert"));
  m_gentranMenu->Append(EventIDs::gentran_to_file, _("Convert + Write to file"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("maxima to other language"), m_gentranMenu);
  m_MenuBar->Append(m_MaximaMenu, _("&Maxima"));

  // Equations menu
  m_EquationsMenu = new wxMenu;
  wxMenu *solve_sub = new wxMenu;
  solve_sub->Append(EventIDs::menu_solve, _("&Solve..."), _("Solve equation(s)"),
                    wxITEM_NORMAL);
  solve_sub->Append(EventIDs::menu_solve_to_poly, _("Solve (to_poly)..."),
                    _("Solve equation(s) with to_poly_solve"), wxITEM_NORMAL);
  solve_sub->Append(EventIDs::menu_solve_lin, _("Solve &Linear System..."),
                    _("Solve linear system of equations"), wxITEM_NORMAL);
  solve_sub->Append(EventIDs::menu_solve_algsys, _("Solve &Algebraic System..."),
                    _("Solve algebraic system of equations"), wxITEM_NORMAL);
  solve_sub->Append(EventIDs::menu_eliminate, _("&Eliminate Variable..."),
                    _("Eliminate a variable from a system "
                      "of equations"));
  m_EquationsMenu->Append(wxWindow::NewControlId(), _("Solve symbolically"), solve_sub);
  wxMenu *solveNum1_sub = new wxMenu;
  solveNum1_sub->Append(EventIDs::menu_solve_num, _("Find numerical solution..."),
                        _("Find a root of an equation on an interval"),
                        wxITEM_NORMAL);
  solveNum1_sub->Append(
			EventIDs::menu_realroots, _("Dito, but as fraction (real only)..."),
			_("Find fractions that real roots of a polynomial and "), wxITEM_NORMAL);
  solveNum1_sub->Append(EventIDs::menu_allroots,
                        _("Numerical solutions of polynomial..."),
                        _("Find all roots of a polynomial"), wxITEM_NORMAL);
  solveNum1_sub->Append(
			EventIDs::menu_bfallroots, _("Numerical solutions of polynomial..."),
			_("Find all roots of a polynomial (bfloat)"), wxITEM_NORMAL);
  m_EquationsMenu->Append(wxWindow::NewControlId(), _("Solve numerical, 1 Variable"),
                          solveNum1_sub);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(EventIDs::menu_solve_ode, _("Solve &ODE..."),
                          _("Solve ordinary differential equation "
                            "of maximum degree 2"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(EventIDs::menu_ivp_1, _("Initial Value Problem (&1)..."),
                          _("Solve initial value problem for first"
                            " degree ODE"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(EventIDs::menu_ivp_2, _("Initial Value Problem (&2)..."),
                          _("Solve initial value problem for second "
                            "degree ODE"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(EventIDs::menu_bvp, _("&Boundary Value Problem..."),
                          _("Solve boundary value problem for second "
                            "degree ODE"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(EventIDs::menu_rk, _("Numerical solution..."),
                          _("Find a numerical solution for a first "
                            "degree ODE"),
                          wxITEM_NORMAL);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(EventIDs::menu_solve_de, _("Solve ODE with Lapla&ce..."),
                          _("Solve ordinary differential equations "
                            "with Laplace transformation"),
                          wxITEM_NORMAL);
  m_EquationsMenu->Append(EventIDs::menu_atvalue, _("A&t Value..."),
                          _("Setup atvalues for solving ODE with "
                            "Laplace transformation"),
                          wxITEM_NORMAL);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(
			  EventIDs::menu_lhs, _("Left side to the \"=\""),
			  _("The half of the equation that is to the left of the \"=\""),
			  wxITEM_NORMAL);
  m_EquationsMenu->Append(
			  EventIDs::menu_rhs, _("Right side to the \"=\""),
			  _("The half of the equation that is to the right of the \"=\""),
			  wxITEM_NORMAL);
  m_EquationsMenu->AppendSeparator();
  m_EquationsMenu->Append(EventIDs::menu_construct_fraction, _("Construct fraction..."));
  m_MenuBar->Append(m_EquationsMenu, _("E&quations"));

  // Matrix menu
  m_matrix_menu = new wxMenu;
  wxMenu *gen_matrix_menu = new wxMenu;
  gen_matrix_menu->Append(EventIDs::menu_genmatrix, _("2D Array to Matrix..."),
                          _("Extract a matrix from a 2-dimensional array"),
                          wxITEM_NORMAL);
  gen_matrix_menu->Append(
			  EventIDs::menu_gen_mat_lambda, _("Generate Matrix from Expression..."),
			  _("Generate a matrix from a lambda expression"), wxITEM_NORMAL);
  gen_matrix_menu->Append(EventIDs::menu_enter_mat, _("&Enter Matrix..."),
                          _("Enter a matrix"), wxITEM_NORMAL);
  gen_matrix_menu->Append(EventIDs::menu_list_list2matrix, _("Nested list to Matrix"),
                          _("Convert a list of lists to a matrix"),
                          wxITEM_NORMAL);
  gen_matrix_menu->Append(EventIDs::menu_csv2mat, _("Matrix from csv file..."),
                          _("Load a matrix from a csv file"), wxITEM_NORMAL);
  m_matrix_menu->Append(wxWindow::NewControlId(), _("Create Matrix"), gen_matrix_menu,
                        _("Methods of generating a matrix"));

  wxMenu *fileio_menu = new wxMenu;
  fileio_menu->Append(EventIDs::menu_csv2mat, _("Matrix from csv file"),
                      _("Load a matrix from a csv file"), wxITEM_NORMAL);
  fileio_menu->Append(EventIDs::menu_mat2csv, _("Matrix to csv file"),
                      _("Export a matrix to a csv file"), wxITEM_NORMAL);
  m_matrix_menu->Append(wxWindow::NewControlId(), _("File I/O"), fileio_menu,
                        _("Matrix to file or Matrix from file"));
  m_matrix_menu->AppendSeparator();

  wxMenu *matrix_basic_sub = new wxMenu;
  matrix_basic_sub->Append(EventIDs::menu_matrix_multiply, _("Multiply matrices..."));
  matrix_basic_sub->Append(EventIDs::menu_matrix_exponent, _("Matrix exponent..."));
  matrix_basic_sub->Append(EventIDs::menu_matrix_hadamard_product,
                           _("Hadamard (element-by-element) product..."),
                           _("Element-by-element multiplication"),
                           wxITEM_NORMAL);
  matrix_basic_sub->Append(
			   EventIDs::menu_matrix_hadamard_exponent, _("Hadamard exponent..."),
			   _("Repetitive element-by-element multiplication"), wxITEM_NORMAL);
  matrix_basic_sub->Append(
			   EventIDs::menu_copymatrix, _("Create copy, not clone..."),
			   _("Creates an independent matrix with the same contents"), wxITEM_NORMAL);
  m_matrix_menu->Append(wxWindow::NewControlId(), _("Basic matrix operations"),
                        matrix_basic_sub,
                        _("Multiplication, exponent and similar"));

  wxMenu *matrix_classicOP_menu = new wxMenu;
  matrix_classicOP_menu->Append(EventIDs::menu_invert_mat, _("&Invert Matrix"),
                                _("Compute the inverse of a matrix"),
                                wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_cpoly, _("&Characteristic Polynomial..."),
                                _("Compute the characteristic polynomial "
                                  "of a matrix"),
                                wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_determinant, _("&Determinant"),
                                _("Compute the determinant of a matrix"),
                                wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_eigen, _("Eigen&values"),
                                _("Find eigenvalues of a matrix"),
                                wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_eigvect, _("Eige&nvectors"),
                                _("Find eigenvectors of a matrix"),
                                wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_adjoint_mat, _("Ad&joint Matrix"),
                                _("Compute the adjoint matrix"), wxITEM_NORMAL);
  matrix_classicOP_menu->Append(
				EventIDs::menu_rank, _("Rank"), _("Compute the rank of a matrix"), wxITEM_NORMAL);
  matrix_classicOP_menu->Append(EventIDs::menu_transpose, _("&Transpose Matrix"),
                                _("Transpose a matrix"), wxITEM_NORMAL);
  m_matrix_menu->Append(
			wxWindow::NewControlId(), _("Classic matrix operations"), matrix_classicOP_menu,
			_("The classic operations one typically uses matrices for"));
  wxMenu *lapack_menu = new wxMenu;
  lapack_menu->Append(EventIDs::menu_matrix_loadLapack, _("Load lapack"),
                      _("Load lapack"), wxITEM_NORMAL);
  lapack_menu->Append(EventIDs::menu_matrix_dgeev_eigenvaluesOnly,
                      _("Eigenvalues (real)"),
                      _("Compile eigenvalues using dgeev"), wxITEM_NORMAL);
  lapack_menu->Append(EventIDs::menu_matrix_zgeev_eigenvaluesOnly,
                      _("Eigenvalues (complex)"),
                      _("Compile eigenvalues using zgeev"), wxITEM_NORMAL);
  lapack_menu->Append(
		      EventIDs::menu_matrix_dgeev, _("Eigenvalues, left+right eigenvectors (real)"),
		      _("Compile eigenvalues+eigenvectors using dgeev"), wxITEM_NORMAL);
  lapack_menu->Append(EventIDs::menu_matrix_zgeev,
                      _("Eigenvalues, left+right eigenvectors (complex)"),
                      _("Compile eigenvalues+eigenvectors using zgeev"));
  lapack_menu->Append(EventIDs::menu_matrix_dgeqrf, _("QR decomposition"),
                      _("Compile a QR decomposition using dgeqrf"));
  lapack_menu->Append(EventIDs::menu_matrix_dgesv, _("Solve Ax=b"),
                      _("Solve a linear equation system using dgesv"));
  lapack_menu->Append(EventIDs::EventIDs::menu_matrix_dgesvd_valuesOnly, _("Singular Values"),
                      _("Singular values using dgesvd"));
  lapack_menu->Append(EventIDs::menu_matrix_dgesvd,
                      _("Singular values, left + right vectors"),
                      _("Singular values using dgesvd"));
  lapack_menu->Append(EventIDs::menu_matrix_dlange_max, _("max(abs(A(i,j))) (real)"));
  lapack_menu->Append(EventIDs::menu_matrix_zlange_max, _("max(abs(A(i,j))) (complex)"));
  lapack_menu->Append(EventIDs::menu_matrix_dlange_one, _("L[1] Norm (real)"));
  lapack_menu->Append(EventIDs::menu_matrix_zlange_one, _("L[1] Norm (complex)"));
  lapack_menu->Append(EventIDs::menu_matrix_dlange_inf, _("L[inf] Norm (real)"));
  lapack_menu->Append(EventIDs::menu_matrix_zlange_inf, _("L[inf] Norm (complex)"));
  lapack_menu->Append(EventIDs::menu_matrix_dlange_frobenius,
                      _("Frobenius Norm sqrt(sum((A(i,j))^2)) (real)"));
  lapack_menu->Append(EventIDs::menu_matrix_zlange_frobenius,
                      _("Frobenius Norm sqrt(sum((A(i,j))^2)) (complex)"));
  // TODO: What is EventIDs::menu_matrix_zheev (means: lapack's function zheev) for?

  m_matrix_menu->Append(
			wxWindow::NewControlId(), _("Numerical operations (lapack)"), lapack_menu,
			_("Fast fortran routines that perform numerical tasks"));
  m_matrix_menu->AppendSeparator();
  wxMenu *matrix_rowOp_sub = new wxMenu;
  matrix_rowOp_sub->Append(EventIDs::menu_matrix_row, _("Extract Row..."),
                           _("Extract a row from the matrix"), wxITEM_NORMAL);
  matrix_rowOp_sub->Append(EventIDs::menu_matrix_col, _("Extract Column..."),
                           _("Extract a column from the matrix"),
                           wxITEM_NORMAL);
  matrix_rowOp_sub->Append(EventIDs::menu_submatrix, _("Remove Rows or Columns..."),
                           _("Remove rows and/or columns from the matrix"),
                           wxITEM_NORMAL);
  matrix_rowOp_sub->Append(
			   EventIDs::menu_matrix_row_list, _("Convert Row to list..."),
			   _("Extract a row from the matrix and convert it to a list"),
			   wxITEM_NORMAL);
  matrix_rowOp_sub->Append(
			   EventIDs::menu_matrix_col_list, _("Convert Column to list..."),
			   _("Extract a column from the matrix and convert it to a list"),
			   wxITEM_NORMAL);
  m_matrix_menu->Append(wxWindow::NewControlId(), _("Row and column operations"),
                        matrix_rowOp_sub,
                        _("Extract, append or delete rows or columns"));
  m_matrix_menu->AppendSeparator();
  m_matrix_menu->Append(EventIDs::menu_map_mat,
                        _("A&pply function to each Matrix element..."),
                        _("Map function to a matrix"), wxITEM_NORMAL);
  m_matrix_menu->Append(
			EventIDs::menu_map, _("Dito, but affect all clones of the Matrix..."),
			_("Map function to a matrix, affecting all of its clones"),
			wxITEM_NORMAL);
  m_MenuBar->Append(m_matrix_menu, _("M&atrix"));

  // Calculus menu
  m_CalculusMenu = new wxMenu;
  m_CalculusMenu->Append(EventIDs::menu_integrate, _("&Integrate..."),
                         _("Integrate expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_risch, _("Risch Integration..."),
                         _("Integrate expression with Risch algorithm"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_change_var, _("C&hange Variable in Integrate..."),
                         _("Change variable in integral or sum"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(
			 EventIDs::menu_change_var_evaluate, _("Dito, and evaluate the result..."),
			 _("Change variable in integral or sum and evaluate the result"),
			 wxITEM_NORMAL);
  m_CalculusMenu->AppendSeparator();
  m_CalculusMenu->Append(EventIDs::menu_diff, _("&Differentiate..."),
                         _("Differentiate expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_limit, _("Find &Limit..."),
                         _("Find a limit of an expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_lbfgs, _("Find Minimum..."),
                         _("Find a (unconstrained) minimum of an expression"),
                         wxITEM_NORMAL);
  wxMenu *series_sub = new wxMenu;
  series_sub->Append(EventIDs::menu_taylor, _("Taylor series..."));
  series_sub->Append(EventIDs::menu_powerseries, _("Power series..."));
  series_sub->Append(EventIDs::menu_fourier, _("Fourier coefficients..."));
  series_sub->Append(EventIDs::menu_pade, _(wxS("P&ad\u00E9 Approximation...")),
                     _("Pade approximation of a Taylor series"));
  m_CalculusMenu->Append(wxWindow::NewControlId(), _("Series approximation"), series_sub);
  m_CalculusMenu->Append(EventIDs::menu_sum, _("Calculate Su&m..."), _("Calculate sums"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_product, _("Calculate &Product..."),
                         _("Calculate products"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_laplace, _("Laplace &Transform..."),
                         _("Get Laplace transformation of an expression"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(
			 EventIDs::menu_ilt, _("Inverse Laplace T&ransform..."),
			 _("Get inverse Laplace transformation of an expression"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_gcd, _("&Greatest Common Divisor..."),
                         _("Compute the greatest common divisor"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_lcm, _("Least Common Multiple..."),
                         _("Compute the least common multiple "
                           "(do load(functs) before using)"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_divide, _("Di&vide Polynomials..."),
                         _("Divide numbers or polynomials"), wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_partfrac, _("Partial &Fractions..."),
                         _("Decompose rational function to partial fractions"),
                         wxITEM_NORMAL);
  m_CalculusMenu->Append(EventIDs::menu_continued_fraction, _("&Continued Fraction"),
                         _("Compute continued fraction of a value"),
                         wxITEM_NORMAL);
  m_MenuBar->Append(m_CalculusMenu, _("&Calculus"));

  // Simplify menu
  m_SimplifyMenu = new wxMenu;
  wxMenu *simplify_sub = new wxMenu;
  simplify_sub->Append(EventIDs::menu_mainvar, _("Set main variable..."));
  simplify_sub->Append(EventIDs::menu_ratsimp, _("Try to guess which form is &simple"),
                       _("Simplify rational expression"), wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_radsimp, _("Simplify &Radicals..."),
                       _("Simplify expression containing radicals"),
                       wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_factor, _("&Factor Expression"),
                       _("Factor an expression"), wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_scanmapfactor,
                       _("Factor Expression including subexpressions"),
                       _("Factor an expression"), wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_gfactor, _("Factor Complex"),
                       _("Factor an expression in Gaussian numbers"),
                       wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_expand, _("&Expand Expression"),
                       _("Expand an expression"), wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_horner, _("Horner's rule"),
                       _("Reorganize an expression using horner's rule"),
                       wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_collapse, _("Optimize for memory"));
  simplify_sub->Append(EventIDs::menu_optimize, _("Optimize for CPU time"));
  simplify_sub->Append(EventIDs::menu_expandwrt, _("Expand for given variables"));
  simplify_sub->Append(EventIDs::EventIDs::menu_expandwrt_denom, _("Dito, including denominator"));
  simplify_sub->Append(EventIDs::menu_scsimp, _("Sequential Comparative Simplification"));
  simplify_sub->Append(EventIDs::menu_xthru, _("Find common denominator"));
  simplify_sub->Append(EventIDs::menu_partfrac, _("Partial &Fractions..."),
                       _("Decompose rational function to partial fractions"),
                       wxITEM_NORMAL);
  simplify_sub->Append(EventIDs::menu_simpsum, _("Simplify sum() commands..."));
  m_SimplifyMenu->Append(wxWindow::NewControlId(), _("Simplify equations"), simplify_sub);
  m_logexpand_Sub = new wxMenu;
  m_logexpand_Sub->Append(
			  EventIDs::menu_logcontract, _("Contract Logarithms"),
			  _("Convert sum of logarithms to logarithm of product"), wxITEM_NORMAL);
  m_logexpand_Sub->Append(EventIDs::menu_logexpand,
                          _("Expand log in previous expression"),
                          _("Warning: No test if the argument of the log is "
                            "complex, positive or negative"),
                          wxITEM_NORMAL);
  m_logexpand_Sub->AppendSeparator();
  m_logexpand_Sub->AppendRadioItem(EventIDs::menu_logexpand_false, _("No"),
                                   _("Switch off simplifications of log(). Set "
                                     "Maxima option variable logexpand:false"));

  wxString warningSign = wxS("\u26A0");
  if (!m_configuration.FontRendersChar(wxS('\u26A0'), *wxNORMAL_FONT))
    warningSign = _("Warning:");
  m_logexpand_Sub->AppendRadioItem(
				   EventIDs::menu_logexpand_true,
				   wxS("log(a^b)=b*log(a) ") + warningSign + _(" Wrong, if a is complex"),
				   _("Set Maxima option variable logexpand:true"));
  m_logexpand_Sub->AppendRadioItem(
				   EventIDs::menu_logexpand_all, _("Additionally: log(a*b)=log(a)+log(b)"),
				   _("Set Maxima option variable logexpand:all"));
  m_logexpand_Sub->AppendRadioItem(
				   EventIDs::menu_logexpand_super,
				   _("Additionally: log(a/b)=log(a)-log(b), a and b positive integers"),
				   _("Set Maxima option variable logexpand:super"));
  m_SimplifyMenu->Append(wxWindow::NewControlId(), _("Simplify Logarithms"), m_logexpand_Sub);
  m_SimplifyMenu->AppendSeparator();
  // Factorials and gamma
  m_Simplify_Gamma_Sub = new wxMenu;
  m_Simplify_Gamma_Sub->Append(
			       EventIDs::menu_to_fact, _("Convert to &Factorials"),
			       _("Convert binomials, beta and gamma function to factorials"),
			       wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(
			       EventIDs::menu_to_gamma, _("Convert to &Gamma"),
			       _("Convert binomials, factorials and beta function to gamma function"),
			       wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(
			       EventIDs::menu_factsimp, _("&Simplify Factorials"),
			       _("Simplify an expression containing factorials"), wxITEM_NORMAL);
  m_Simplify_Gamma_Sub->Append(EventIDs::menu_factcomb, _("&Combine Factorials"),
                               _("Combine factorials in an expression"),
                               wxITEM_NORMAL);
  m_SimplifyMenu->Append(
			 wxWindow::NewControlId(), _("Factorials and &Gamma"), m_Simplify_Gamma_Sub,
			 _("Functions for simplifying factorials and gamma function"));
  // Trigonometric submenu
  m_Simplify_Trig_Sub = new wxMenu;
  m_Simplify_Trig_Sub->Append(EventIDs::menu_trigsimp, _("&Simplify Trigonometric"),
                              _("Simplify trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(EventIDs::menu_trigreduce, _("&Reduce Trigonometric"),
                              _("Reduce trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(EventIDs::menu_trigexpand, _("&Expand Trigonometric"),
                              _("Expand trigonometric expression"),
                              wxITEM_NORMAL);
  m_Simplify_Trig_Sub->Append(
			      EventIDs::menu_trigrat, _("&Canonical Form"),
			      _("Convert trigonometric expression to canonical quasilinear form"),
			      wxITEM_NORMAL);
  m_SimplifyMenu->Append(
			 wxWindow::NewControlId(), _("&Trigonometric Simplification"), m_Simplify_Trig_Sub,
			 _("Functions for simplifying trigonometric expressions"));
  // Complex submenu
  m_Simplify_Complex_Sub = new wxMenu;
  m_Simplify_Complex_Sub->Append(EventIDs::menu_rectform, _("Convert to &Rectform"),
                                 _("Convert complex expression to rect form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(EventIDs::menu_polarform, _("Convert to &Polarform"),
                                 _("Convert complex expression to polar form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(EventIDs::menu_realpart, _("Get Real P&art"),
                                 _("Get the real part of complex expression"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(
				 EventIDs::menu_imagpart, _("Get &Imaginary Part"),
				 _("Get the imaginary part of complex expression"), wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(EventIDs::menu_demoivre, _("&Demoivre"),
                                 _("Convert exponential function of imaginary "
                                   "argument to trigonometric form"),
                                 wxITEM_NORMAL);
  m_Simplify_Complex_Sub->Append(
				 EventIDs::menu_exponentialize, _("&Exponentialize"),
				 _("Convert trigonometric functions to exponential form"), wxITEM_NORMAL);
  m_SimplifyMenu->Append(wxWindow::NewControlId(), _("&Complex Simplification"),
                         m_Simplify_Complex_Sub,
                         _("Functions for complex simplification"));
  m_SimplifyMenu->AppendSeparator();
  m_subst_Sub = new wxMenu;
  m_subst_Sub->Append(EventIDs::menu_subst, _("Substitute..."),
                      _("A search-and-replace for equations"), wxITEM_NORMAL);
  m_subst_Sub->Append(EventIDs::menu_ratsubst, _("Smart Substitute..."),
                      _("A subst with basic maths knowledge"), wxITEM_NORMAL);
  m_subst_Sub->Append(EventIDs::menu_psubst, _("Parallel Substitute..."),
                      _("Substitutes, but not in the other substituents"),
                      wxITEM_NORMAL);
  m_subst_Sub->Append(EventIDs::menu_fullratsubst, _("Recursive Substitute..."),
                      _("Substitutes until the equation no more changes"),
                      wxITEM_NORMAL);
  m_subst_Sub->Append(EventIDs::menu_at, _("Subst constant t into eq with diff(x,t)..."),
                      _("Substitutes until the equation no more changes"),
                      wxITEM_NORMAL);
  m_subst_Sub->Append(EventIDs::menu_substinpart, _("Substitute only in parts..."),
                      _("Substitute only in the elements n_1, n_2,..."),
                      wxITEM_NORMAL);
  m_subst_Sub->AppendCheckItem(EventIDs::menu_opsubst,
                               _("Allow to substitute operators"));
  m_SimplifyMenu->Append(wxWindow::NewControlId(), _("Substitute"), m_subst_Sub);
  m_SimplifyMenu->Append(EventIDs::menu_nouns, _("Evaluate &Noun Forms..."),
                         _("Evaluate all noun forms in expression"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->AppendCheckItem(EventIDs::menu_talg, _("&Algebraic Mode"),
                                  _("Set the \"algebraic\" flag"));
  m_SimplifyMenu->Append(EventIDs::menu_tellrat, _("Add Algebraic E&quality..."),
                         _("Add equality to the rational simplifier"),
                         wxITEM_NORMAL);
  m_SimplifyMenu->Append(EventIDs::menu_modulus, _("&Modulus Computation..."),
                         _("Setup modulus computation"), wxITEM_NORMAL);
  m_MenuBar->Append(m_SimplifyMenu, _("&Simplify"));

  // List menu
  m_listMenu = new wxMenu;
  wxMenu *listcreateSub = new wxMenu;
  listcreateSub->Append(
			EventIDs::menu_list_create_from_elements, _("from individual elements"),
			_("Create a list from comma-separated elements"), wxITEM_NORMAL);
  listcreateSub->Append(EventIDs::menu_list_create_from_rule, _("from a rule"),
                        _("Generate list elements using a rule"),
                        wxITEM_NORMAL);
  listcreateSub->Append(EventIDs::menu_list_create_from_list, _("from a list"),
                        _("Generate a new list using a lists' elements"),
                        wxITEM_NORMAL);
  listcreateSub->Append(EventIDs::menu_csv2list, _("Read List from csv file..."),
                        _("Load a list from a csv file"), wxITEM_NORMAL);
  listcreateSub->Append(EventIDs::menu_list_actual_values_storage,
                        _("as storage for actual values for variables"),
                        _("Generate a storage for variable values that can be "
                          "introduced into equations at any time"),
                        wxITEM_NORMAL);
  listcreateSub->Append(
			EventIDs::menu_list_create_from_args, _("from function arguments"),
			_("Extract the argument list from a function call"), wxITEM_NORMAL);
  m_listMenu->Append(wxWindow::NewControlId(), _("Create list"), listcreateSub,
                     _("Create a list"));
  wxMenu *listuseSub = new wxMenu;
  listuseSub->Append(EventIDs::menu_list_map, _("apply function to each element"),
                     _("Runs each element through a function"), wxITEM_NORMAL);
  listuseSub->Append(
		     EventIDs::menu_map_lambda, _("Run each element through an expression"),
		     _("Runs each element through an expression"), wxITEM_NORMAL);
  listuseSub->Append(
		     EventIDs::menu_list_use_actual_values, _("use the actual values stored"),
		     _("Introduce the actual values for variables stored in the list"),
		     wxITEM_NORMAL);
  listuseSub->Append(
		     EventIDs::menu_list_as_function_arguments, _("use as function arguments"),
		     _("Use list as the arguments of a function"), wxITEM_NORMAL);
  listuseSub->Append(EventIDs::menu_list_do_for_each_element, _("do for each element"),
                     _("Execute a command for each element of the list"),
                     wxITEM_NORMAL);
  listuseSub->Append(EventIDs::menu_list2csv, _("Export List to csv file..."),
                     _("Export a list to a csv file"), wxITEM_NORMAL);

  m_listMenu->Append(wxWindow::NewControlId(), _("Use list"), listuseSub, _("Use a list"));
  wxMenu *listextractmenu = new wxMenu;
  listextractmenu->Append(EventIDs::menu_list_nth, _("nth"),
                          _("Returns an arbitrary list item"));
  listextractmenu->Append(EventIDs::menu_list_first, _("First"),
                          _("Returns the first item of the list"));
  listextractmenu->Append(EventIDs::menu_list_rest, _("All but the 1st n elements"),
                          _("Returns the list without its first n elements"));
  listextractmenu->Append(EventIDs::menu_list_restN, _("All but the last n elements"),
                          _("Returns the list without its last n elements"));
  listextractmenu->Append(EventIDs::menu_list_last, _("Last"),
                          _("Returns the last item of the list"));
  listextractmenu->Append(EventIDs::menu_list_lastn, _("Last n"),
                          _("Returns the last n items of the list"));
  listextractmenu->Append(
			  EventIDs::menu_list_extract_value, _("Extract an actual value for a variable"),
			  _("Extract the value for one variable assigned in a list"),
			  wxITEM_NORMAL);
  m_listMenu->Append(wxWindow::NewControlId(), _("Extract Elements"), listextractmenu,
                     _("Extract list Elements"));
  wxMenu *listappendSub = new wxMenu;
  listappendSub->Append(EventIDs::menu_list_append_item_end, _("Append an element"),
                        _("Append an element to the end of an existing list"),
                        wxITEM_NORMAL);
  listappendSub->Append(
			EventIDs::menu_list_append_item_start, _("Prepend an element"),
			_("Append an element to the beginning an existing list"), wxITEM_NORMAL);
  listappendSub->Append(EventIDs::menu_list_append_list, _("Append a list"),
                        _("Append a list to an existing list"), wxITEM_NORMAL);
  listappendSub->Append(EventIDs::menu_list_interleave, _("Interleave"),
                        _("Interleave the values of two lists"), wxITEM_NORMAL);
  m_listMenu->Append(wxWindow::NewControlId(), _("Append"), listappendSub, _("Use a list"));

  m_listMenu->Append(EventIDs::menu_list_length, _("Length"),
                     _("Returns the length of the list"));
  m_listMenu->Append(EventIDs::menu_list_reverse, _("Reverse"),
                     _("Reverse the order of the list items"));
  m_listMenu->AppendSeparator();
  m_listMenu->Append(EventIDs::menu_list_sort, _("Sort"));
  m_listMenu->Append(EventIDs::menu_list_remove_duplicates, _("Remove duplicates"),
                     _("Remove all list elements that appear twice in a row. "
                       "Normally used in conjunction with sort."));
  m_listMenu->AppendSeparator();
  m_listMenu->Append(EventIDs::menu_list_push, _("Push"),
                     _("Add a new item to the beginning of the list. Useful "
                       "for creating stacks."));
  m_listMenu->Append(EventIDs::menu_list_pop, _("Pop"),
                     _("Return the first item of the list and remove it from "
                       "the list. Useful for creating stacks."));
  m_listMenu->AppendSeparator();
  m_listMenu->Append(
		     EventIDs::menu_list_list2matrix, _("Nested list to Matrix"),
		     _("Converts a nested list like [[1,2],[3,4]] to a matrix"));
  m_listMenu->Append(EventIDs::menu_list_matrix2list, _("Matrix to nested List"),
                     _("Converts a matrix to a list of lists"));
  m_MenuBar->Append(m_listMenu, _("&List"));
  // Plot menu
  m_PlotMenu = new wxMenu;
  m_PlotMenu->Append(EventIDs::gp_plot2, _("Plot &2d..."), _("Plot in 2 dimensions"),
                     wxITEM_NORMAL);
  m_PlotMenu->Append(EventIDs::gp_plot3, _("Plot &3d..."), _("Plot in 3 dimensions"),
                     wxITEM_NORMAL);
  m_PlotMenu->Append(EventIDs::menu_plot_format, _("Plot &Format..."),
                     _("Set plot format"), wxITEM_NORMAL);
  m_PlotMenu->AppendSeparator();
  m_PlotMenu->AppendCheckItem(EventIDs::menu_animationautostart, _("Animation autoplay"),
                              _("Defines if an animation is automatically "
                                "started or only by clicking on it."));
  m_PlotMenu->Append(EventIDs::menu_animationframerate, _("Animation framerate..."),
                     _("Set the frame rate for animations."));
  m_MenuBar->Append(m_PlotMenu, _("&Plot"));

  // Numeric menu
  m_NumericMenu = new wxMenu;
  m_NumericMenu->AppendCheckItem(EventIDs::menu_num_out, _("&Numeric Output"),
                                 _("Numeric output"));
  m_NumericMenu->AppendCheckItem(
				 EventIDs::menu_num_domain, _("Expect numbers harder to be complex"),
				 _("Expect variables to contain complex numbers"));
  m_NumericMenu->Check(EventIDs::menu_num_domain, false);
  m_NumericMenu->Append(EventIDs::menu_to_float, _("To &Float"),
                        _("Calculate float value of the last result"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(EventIDs::menu_to_bfloat, _("To &Bigfloat"),
                        _("Calculate bigfloat value of the last result"),
                        wxITEM_NORMAL);
  m_NumericMenu->Append(EventIDs::menu_to_numer, _("To Numeri&c\tCtrl+Shift+N"),
                        _("Calculate numeric value of the last result"),
                        wxITEM_NORMAL);

  wxMenu *floatToExactSub = new wxMenu;
  floatToExactSub->Append(
			  EventIDs::menu_rationalize, _("To exact fraction"),
			  _("Find a fraction that represents this float exactly"), wxITEM_NORMAL);
  floatToExactSub->Append(
			  EventIDs::menu_rat, _("Approximate by nice fraction"),
			  _("Find a nice fraction that is similar to this float"), wxITEM_NORMAL);
  floatToExactSub->Append(
			  EventIDs::menu_guess_exact_value, _("Advanced guess (PSLQ algorithm)"),
			  _("Approximate by a fraction using log(), sqrt() and %pi, when needed. "
			    "Only available in maxima > 5.46.0"),
			  wxITEM_NORMAL);
  m_NumericMenu->Append(
			wxWindow::NewControlId(), _("To exact number"), floatToExactSub,
			_("Guess an exact number that could be meant by this float"));

  m_NumericMenu->AppendSeparator();
  m_NumericMenu->Append(
			EventIDs::menu_set_precision, _("Set bigfloat &Precision..."),
			_("Set the precision for numbers that are defined as bigfloat. Such "
			  "numbers can be generated by entering 1.5b12 or as bfloat(1.234)"),
			wxITEM_NORMAL);
  m_NumericMenu->Append(
			EventIDs::menu_set_displayprecision, _("Set &displayed Precision..."),
			_("Shows how many digits of a numbers are displayed"), wxITEM_NORMAL);
  m_NumericMenu->AppendSeparator();
  m_NumericMenu->AppendCheckItem(
				 EventIDs::menu_engineeringFormat, _("Engineering format (12.1e6 etc.)"),
				 _("Print floating-point numbers with exponents dividable by 3"));
  m_NumericMenu->Append(
			EventIDs::menu_engineeringFormatSetup, _("Setup the engineering format..."),
			_("Fine-tune the display of engineering-format numbers"), wxITEM_NORMAL);

  wxMenu *quadpack_sub = new wxMenu;
  wxString integralSign = wxS("\u222B");
  if (!m_configuration.FontRendersChar(wxS('\u222B'), *wxNORMAL_FONT))
    integralSign = wxS("integrate");
  quadpack_sub->Append(EventIDs::menu_quad_qag,
                       integralSign + _("(f(x),x,a,b), strategy of Aind"));
  quadpack_sub->Append(EventIDs::menu_quad_qags,
                       integralSign + _("(f(x),x,a,b), Epsilon algorithm"));
  quadpack_sub->Append(EventIDs::menu_quad_qagi,
                       integralSign + _("(f(x),x,a,b), infinite interval"));
  quadpack_sub->Append(EventIDs::menu_quad_qawc,
                       _("Cauchy principal value, finite interval"));
  quadpack_sub->Append(EventIDs::menu_quad_qawf_sin,
                       integralSign + wxS("(f(x)*sin(ω·x),x,a,∞)"));
  quadpack_sub->Append(EventIDs::menu_quad_qawf_cos,
                       integralSign + wxS("(f(x)*cos(ω·x),x,a,∞)"));
  quadpack_sub->Append(EventIDs::menu_quad_qawo_sin,
                       integralSign + wxS("(f(x)*sin(ω·x),x,a,b)"));
  quadpack_sub->Append(EventIDs::menu_quad_qawo_cos,
                       integralSign + wxS("(f(x)*cos(ω·x),x,a,b)"));
  quadpack_sub->Append(EventIDs::menu_quad_qaws1,
                       integralSign + wxS("(f(x)*(x-a)^α(b-x)^β,x,a,b)"));
  quadpack_sub->Append(EventIDs::menu_quad_qaws2,
                       integralSign +
		       wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a),x,a,b)"));
  quadpack_sub->Append(EventIDs::menu_quad_qaws3,
                       integralSign +
		       wxS("(f(x)*(x-a)^α(b-x)^β·log(b-x),x,a,b)"));
  quadpack_sub->Append(
		       EventIDs::menu_quad_qaws4,
		       integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a)·log(b-x),x,a,b)"));
  quadpack_sub->Append(EventIDs::menu_quad_qagp,
                       integralSign +
		       _("(f(x),x,y) with singularities+discontinuities"));

  m_NumericMenu->Append(wxWindow::NewControlId(), _("Integrate numerically"), quadpack_sub);
  m_MenuBar->Append(m_NumericMenu, _("&Numeric"));

  // Help menu
  m_HelpMenu = new wxMenu;
#if defined __WXOSX__
  m_HelpMenu->Append(wxID_HELP, _("Context-sensitive &Help\tCtrl+?"),
                     _("Show wxMaxima help"), wxITEM_NORMAL);
#else
  APPEND_MENU_ITEM(m_HelpMenu, wxID_HELP, _("Context-sensitive &Help\tF1"),
                   _("Show wxMaxima help"), wxS("gtk-help"));
#endif
  m_HelpMenu->Append(EventIDs::menu_wxmaximahelp, _("wxMaxima help"),
                     _("The manual of wxMaxima"), wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_maximahelp, _("&Maxima help"),
                     _("The manual of Maxima"), wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_example, _("&Example..."),
                     _("Show an example of usage"), wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_apropos, _("&Apropos..."),
                     _("Show commands similar to"), wxITEM_NORMAL);
  APPEND_MENU_ITEM(m_HelpMenu, EventIDs::menu_show_tip, _("Show &Tips..."),
                   _("Show a tip"), wxART_TIP);

  m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_internal_help,
                              _("Maxima shows help in the console"),
                              _("Tells maxima to show the help for ?, ?? and "
                                "describe() on the console"));
  if (m_configuration.OfferInternalHelpBrowser())
    m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_wxmaxima_help,
                                _("Maxima shows help in a sidebar"),
                                _("Tells maxima to show the help for ?, ?? and "
                                  "describe() in a wxMaxima sidebar"));
  m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_html_help,
                              _("Maxima shows help in a browser"),
                              _("Tells maxima to show the help for ?, ?? and "
                                "describe() in a separate browser window"));
  m_HelpMenu->AppendSeparator();
#ifdef USE_WEBVIEW
  m_HelpMenu->Append(EventIDs::menu_goto_url, _("Go to URL"));
#endif
  wxMenu *tutorials_sub = new wxMenu;
  tutorials_sub->Append(EventIDs::menu_help_solving, _("Solving equations with Maxima"),
                        "", wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_numberformats, _("Number types"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_tolerances,
                        _("Tolerance calculations with Maxima"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_3d, _("Displaying 3d curves"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_diffequations,
                        _("Solving differential equations"), "", wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_fittingData,
                        _("Fitting curves to measurement data"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_varnames, _("Advanced variable names"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_listaccess, _("Fast list access"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_memoizing, _("Memoizing"), "", wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_casvsprogramming, _("Maxima vs. Programming languages"), "", wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_tutorials, _(wxS("↗Tutorials on the web")),
                        _("Online tutorials"), wxITEM_NORMAL);
  m_HelpMenu->Append(wxWindow::NewControlId(), _("Tutorials"), tutorials_sub);

  m_HelpMenu->AppendSeparator();
  m_HelpMenu->Append(EventIDs::menu_build_info, _("Build &Info"),
                     _("Info about Maxima build"), wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_bug_report, _("&Bug Report"), _("Report bug"),
                     wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_license, _("&License"), _("wxMaxima's license"),
                     wxITEM_NORMAL);
  m_HelpMenu->Append(EventIDs::menu_changelog, _("Change Log"), _("wxMaxima's ChangeLog"),
                     wxITEM_NORMAL);
  m_HelpMenu->AppendSeparator();
  m_HelpMenu->Append(EventIDs::menu_check_updates, _("Check for Updates"),
                     _("Check if a newer version of wxMaxima is available."),
                     wxITEM_NORMAL);
#ifndef __WXOSX__
  m_HelpMenu->AppendSeparator();
  APPEND_MENU_ITEM(m_HelpMenu, wxID_ABOUT, _("About"), _("About wxMaxima"),
                   wxS("stock_about"));
#else
  APPEND_MENU_ITEM(m_HelpMenu, wxID_ABOUT, _("About wxMaxima"),
                   _("About wxMaxima"), wxS("stock_about"));
#endif

  m_MenuBar->Append(m_HelpMenu, _("&Help"));

  SetMenuBar(m_MenuBar);
#undef APPEND_MENU_ITEM
}

wxString wxMaximaFrame::wxMaximaManualLocation() {
  wxString helpfile;
  wxString lang_long =
    wxLocale().GetCanonicalName(); /* two- or five-letter string in xx or xx_YY
				      format. Examples: "en", "en_GB", "en_US"
				      or "fr_FR" */
  wxString lang_short = lang_long.Left(lang_long.Find('_'));

  helpfile =
    Dirstructure::Get()->HelpDir() + wxS("/wxmaxima.") + lang_long + ".html";
  if (!wxFileExists(helpfile))
    helpfile = Dirstructure::Get()->HelpDir() + wxS("/wxmaxima.") + lang_short +
      ".html";
  if (!wxFileExists(helpfile))
    helpfile = Dirstructure::Get()->HelpDir() + wxS("/wxmaxima.html");

  /* If wxMaxima is called via ./wxmaxima-local directly from the build
   * directory and *not* installed */
  /* the help files are in the "info/" subdirectory of the current (build)
   * directory */
  if (!wxFileExists(helpfile))
    helpfile = wxGetCwd() + wxS("/info/wxmaxima.") + lang_long + ".html";
  if (!wxFileExists(helpfile))
    helpfile = wxGetCwd() + wxS("/info/wxmaxima.") + lang_short + ".html";
  if (!wxFileExists(helpfile))
    helpfile = wxGetCwd() + wxS("/info/wxmaxima.html");
  return helpfile;
}

bool wxMaximaFrame::ToolbarIsShown() {
  return IsPaneDisplayed(EventIDs::menu_pane_toolbar);
}

void wxMaximaFrame::PopulateRecentDocumentsMenu(wxMenu *menu, int firstEntry,
						const std::list<wxString> &items)
{
  int id = firstEntry;
  for(const auto &name:items)
    {
      wxFileName filename(name);
      wxString path(filename.GetPath()), fullname(filename.GetFullName());
      wxString label(fullname + wxS("   [ ") + path + wxS(" ]"));
      if(menu->FindItem(id))
	  menu->SetLabel(id, label);
      else
	menu->Append(id, label);
      menu->Enable(id, wxFileExists(name));

      id++;
      if(id >= firstEntry + EventIDs::NumberOfRecentFiles())
	return;
    }
}

void wxMaximaFrame::PopulateRecentPackagesMenu(wxMenu *menu, int firstEntry,
					       const std::list<wxString> &items)
{
  int id = firstEntry;
  for(const auto &name:items)
    {
      if(menu->FindItem(id))
	menu->SetLabel(id, name);
      else
	menu->Append(id, name);
      id++;
      if(id >= firstEntry + EventIDs::NumberOfRecentFiles())
	return;
    }
}

void wxMaximaFrame::UpdateRecentDocuments() {
  long recentItems = 10;
  wxConfig::Get()->Read(wxS("recentItems"), &recentItems);

  if (recentItems < 5)
    recentItems = 5;
  if (recentItems > EventIDs::NumberOfRecentFiles())
    recentItems = 30;

  PopulateRecentDocumentsMenu(m_recentDocumentsMenu, EventIDs::menu_recent_document_0,
			      m_recentDocuments.Get());

  PopulateRecentDocumentsMenu(m_unsavedDocumentsMenu, EventIDs::menu_unsaved_document_0,
			      m_worksheet->m_unsavedDocuments.Get());
  
  PopulateRecentPackagesMenu(m_recentPackagesMenu, EventIDs::menu_recent_package_0,
			     m_recentPackages.Get());
}

void wxMaximaFrame::ReadConfig() { m_worksheet->UpdateConfig(); }

void wxMaximaFrame::ReReadConfig() {
  // On wxMac re-reading the config isn't necessary as all windows share the
  // same process and the same configuration.
#ifndef __WXMAC__
  // On MSW re-reading the config is only necessary if the config is read from
  // the registry
#ifdef __WXMSW__
  if (Configuration::m_configfileLocation_override != wxEmptyString)
#endif
    {
      // Delete the old config
      wxConfigBase *config = wxConfig::Get();
      config->Flush();
      wxDELETE(config);
      config = NULL;

      if (Configuration::m_configfileLocation_override == wxEmptyString) {
	wxLogMessage(_("Re-Reading the config from the default location."));
	wxConfig::Set(new wxConfig(wxS("wxMaxima")));
      } else {
	wxLogMessage(_("Re-Reading the config from %s."),
		     Configuration::m_configfileLocation_override.utf8_str());
	wxConfig::Set(
		      new wxFileConfig(wxS("wxMaxima"), wxEmptyString,
				       Configuration::m_configfileLocation_override));
      }
    }
#endif
  for(auto &window: m_topLevelWindows)
    {
      window->ReadConfig();
    }
}

void wxMaximaFrame::RegisterAutoSaveFile() {
  wxString autoSaveFiles;
  ReReadConfig();
  wxConfigBase *config = wxConfig::Get();
  config->Read("AutoSaveFiles", &autoSaveFiles);
  m_worksheet->m_unsavedDocuments.AddDocument(m_tempfileName);
  ReReadConfig();
}

void wxMaximaFrame::RemoveTempAutosavefile() {
  if (m_tempfileName != wxEmptyString) {
    // Don't delete the file if we have opened it and haven't saved it under a
    // different name yet.
    if (wxFileExists(m_tempfileName) &&
        (m_tempfileName != m_worksheet->m_currentFile)) {
      SuppressErrorDialogs logNull;
      wxRemoveFile(m_tempfileName);
    }
  }
  m_tempfileName = wxEmptyString;
}

bool wxMaximaFrame::IsPaneDisplayed(int id) {
  auto item = m_sidebarNames.find(id);
  //  wxASSERT(item != m_sidebarNames.end());
  if(item != m_sidebarNames.end())
    return m_manager.GetPane(item->second).IsShown();
  else
    return false;
}

void wxMaximaFrame::OnMenuStatusText(wxMenuEvent &event)
{
  if(event.GetId() <= 0)    
    StatusText(wxEmptyString, false);
  else
    {
      wxMenu *menu = event.GetMenu();
      if(menu != NULL)
	StatusText(menu->GetHelpString(event.GetId()), false);
    }
}
void wxMaximaFrame::DockAllSidebars(wxCommandEvent &WXUNUSED(ev)) {
  for(const auto &pane: m_sidebarNames)
    m_manager.GetPane(pane.second).Dock();
}

void  wxMaximaFrame::StatusText(const wxString &text, bool saveInLog)
{
  m_newStatusText = true;
  m_leftStatusText = text;
  if(saveInLog)
    {
      wxLogMessage("%s", text.mb_str());
      for(auto i = m_statusTextHistory.size() - 1; i > 0; i--)
	m_statusTextHistory[i] = m_statusTextHistory[i-1];
      m_statusTextHistory[0] = text;
    }
}

void wxMaximaFrame::TogglePaneVisibility(int id)
{
  wxASSERT(id != EventIDs::menu_pane_hideall);
  if(id == EventIDs::menu_pane_hideall)
    return;
  
  ShowPane(id, !IsPaneDisplayed(id)); 
}

void wxMaximaFrame::ShowPane(int id, bool show) {
  if(id == EventIDs::menu_pane_hideall)
    {
        for(const auto &pane: m_sidebarNames)
	  {
	    if(
	       (pane.first != EventIDs::menu_pane_console)
	       )
	      m_manager.GetPane(pane.second).Show(false);
	  }
    }
  else
    {
      wxASSERT((id != EventIDs::menu_pane_console) || (show));
      if((id != EventIDs::menu_pane_console) || (show)) {
	auto item = m_sidebarNames.find(id);
	//	wxASSERT(item != m_sidebarNames.end());
	if(item != m_sidebarNames.end())
	  m_manager.GetPane(item->second).Show(show);
      }
    }
  m_manager.Update();
}

wxWindow *wxMaximaFrame::CreateMathPane() {
  wxSizer *grid = new Buttonwrapsizer();
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(this, -1);
  panel->SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(panel, EventIDs::button_ratsimp, _("Simplify"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_radcan, _("Simplify (r)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_factor, _("Factor"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_expand, _("Expand"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_rectform, _("Rectform"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_subst, _("Subst..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_trigrat, _("Canonical (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_trigsimp, _("Simplify (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_trigexpand, _("Expand (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_trigreduce, _("Reduce (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_solve, _("Solve..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_solve_ode, _("Solve ODE..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_diff, _("Diff..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_integrate, _("Integrate..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_limit, _("Limit..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_plot2, _("Plot 2D..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::button_plot3, _("Plot 3D..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  panel->SetSizer(grid);
  panel->FitInside();
  return panel;
}

wxWindow *wxMaximaFrame::CreateStatPane() {
  wxSizer *grid1 = new Buttonwrapsizer();
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *box1 = new wxBoxSizer(wxVERTICAL);
  wxGridSizer *grid2 = new wxGridSizer(2);
  wxGridSizer *grid3 = new wxGridSizer(2);
  wxBoxSizer *box3 = new wxBoxSizer(wxVERTICAL);
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(this, -1);
  panel->SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;
  int sizerBorder = 2;

  grid1->Add(new wxButton(panel, EventIDs::menu_stats_mean, _("Mean..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(panel, EventIDs::menu_stats_median, _("Median..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(panel, EventIDs::menu_stats_var, _("Variance..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(panel, EventIDs::menu_stats_dev, _("Deviation..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid1, 0, style, sizerBorder);

  box1->Add(new wxButton(panel, EventIDs::menu_stats_tt1, _("Mean Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(panel, EventIDs::menu_stats_tt2, _("Mean Difference Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(panel, EventIDs::menu_stats_tnorm, _("Normality Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(panel, EventIDs::menu_stats_linreg, _("Linear Regression..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(panel, EventIDs::menu_stats_lsquares, _("Least Squares Fit..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  box->Add(box1, 0, style, sizerBorder);

  grid2->Add(new wxButton(panel, EventIDs::menu_stats_histogram, _("Histogram..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(panel, EventIDs::menu_stats_scatterplot, _("Scatterplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(panel, EventIDs::menu_stats_barsplot, _("Barsplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(panel, EventIDs::menu_stats_piechart, _("Piechart..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(panel, EventIDs::menu_stats_boxplot, _("Boxplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid2, 0, style, sizerBorder);

  grid3->Add(new wxButton(panel, EventIDs::menu_stats_readm, _("Read Matrix..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid3->Add(new wxButton(panel, EventIDs::menu_stats_enterm, _("Enter Matrix..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid3, 0, style, sizerBorder);

  box3->Add(new wxButton(panel, EventIDs::menu_stats_subsample, _("Subsample..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  box->Add(box3, 0, style, sizerBorder);

  panel->SetSizer(box);
  panel->FitInside();
  return panel;
}

wxMaximaFrame::GreekPane::GreekPane(wxWindow *parent,
                                    Configuration *configuration,
                                    Worksheet *worksheet, int ID)
  : wxScrolled<wxPanel>(parent, ID), m_configuration(configuration),
    m_lowercaseSizer(new wxWrapSizer(wxHORIZONTAL)),
    m_uppercaseSizer(new wxWrapSizer(wxHORIZONTAL)), m_worksheet(worksheet) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  ShowScrollbars(wxSHOW_SB_NEVER, wxSHOW_SB_DEFAULT);
  EnableScrolling(false, true);
  SetScrollRate(5, 5);
  UpdateSymbols();

  vbox->Add(m_lowercaseSizer, wxSizerFlags().Expand());
  vbox->Add(m_uppercaseSizer, wxSizerFlags().Expand());

  Connect(wxEVT_SIZE, wxSizeEventHandler(wxMaximaFrame::GreekPane::OnSize),
          NULL, this);
  Connect(EventIDs::menu_showLatinGreekLookalikes, wxEVT_MENU,
          wxCommandEventHandler(wxMaximaFrame::GreekPane::OnMenu), NULL, this);
  Connect(EventIDs::menu_showGreekMu, wxEVT_MENU,
          wxCommandEventHandler(wxMaximaFrame::GreekPane::OnMenu), NULL, this);
  Connect(wxEVT_RIGHT_DOWN,
          wxMouseEventHandler(wxMaximaFrame::GreekPane::OnMouseRightDown));

  SetSizer(vbox);
  FitInside();
  SetMinSize(wxSize(GetContentScaleFactor() * 50, GetMinSize().y));
}

void wxMaximaFrame::GreekPane::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}

void wxMaximaFrame::SymbolsPane::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}

void wxMaximaFrame::GreekPane::OnMenu(wxCommandEvent &event) {
  std::unordered_map<int,std::function<void()>> m{
    {EventIDs::menu_showLatinGreekLookalikes, [&](){
      m_configuration->GreekSidebar_ShowLatinLookalikes(
							!m_configuration->GreekSidebar_ShowLatinLookalikes());
      UpdateSymbols();
      Layout();
    }},
    {EventIDs::menu_showGreekMu, [&](){
      m_configuration->GreekSidebar_Show_mu(
					    !m_configuration->GreekSidebar_Show_mu());
      UpdateSymbols();
      Layout();
    }
    }};
  auto varFunc = m.find(event.GetId());
  if(varFunc == m.end())
    {
      wxASSERT(false);
    }
  else
    varFunc->second();
}

void wxMaximaFrame::GreekPane::UpdateSymbols() {
  //  wxWindowUpdateLocker drawBlocker(this);
  enum class Cond { None, Show_mu, ShowLatinLookalikes };
  struct EnabledDefinition : CharButton::Definition {
    Cond condition;
    EnabledDefinition(wchar_t sym, const wxString &descr,
                      Cond cond = Cond::None)
      : CharButton::Definition{sym, descr}, condition(cond) {}
    explicit EnabledDefinition(wchar_t sym)
      : EnabledDefinition(sym, wxm::emptyString) {}
  };

  static const EnabledDefinition lowerCaseDefs[] = {
    {L'\u03B1', _("alpha")},
    {L'\u03B2', _("beta")},
    {L'\u03B3', _("gamma")},
    {L'\u03B4', _("delta")},
    {L'\u03B5', _("epsilon")},
    {L'\u03B6', _("zeta")},
    {L'\u03B7', _("eta")},
    {L'\u03B8', _("theta")},
    {L'\u03B9', _("iota")},
    {L'\u03BA', _("kappa")},
    {L'\u03BB', _("lambda")},
    {L'\u03BC', _("mu"), Cond::Show_mu},
    {L'\u03BD', _("nu")},
    {L'\u03BE', _("xi")},
    {L'\u03BF', _("omicron"), Cond::ShowLatinLookalikes},
    {L'\u03C0', _("pi")},
    {L'\u03C1', _("rho")},
    {L'\u03C3', _("sigma")},
    {L'\u03C4', _("tau")},
    {L'\u03C5', _("upsilon")},
    {L'\u03C6', _("phi")},
    {L'\u03C7', _("chi")},
    {L'\u03C8', _("psi")},
    {L'\u03C9', _("omega")},
  };

  static const EnabledDefinition upperCaseDefs[] = {
    {L'\u0391', ("Alpha"), Cond::ShowLatinLookalikes},
    {L'\u0392', _("Beta"), Cond::ShowLatinLookalikes},
    {L'\u0393', _("Gamma")},
    {L'\u0394', _("Delta")},
    {L'\u0395', _("Epsilon"), Cond::ShowLatinLookalikes},
    {L'\u0396', _("Zeta"), Cond::ShowLatinLookalikes},
    {L'\u0397', _("Eta"), Cond::ShowLatinLookalikes},
    {L'\u0398', _("Theta")},
    {L'\u0399', _("Iota"), Cond::ShowLatinLookalikes},
    {L'\u039A', _("Kappa"), Cond::ShowLatinLookalikes},
    {L'\u039B', _("Lambda")},
    {L'\u039C', _("Mu"), Cond::ShowLatinLookalikes},
    {L'\u039D', _("Nu"), Cond::ShowLatinLookalikes},
    {L'\u039E', _("Xi")},
    {L'\u039F', _("Omicron"), Cond::ShowLatinLookalikes},
    {L'\u03A0', _("Pi")},
    {L'\u03A1', _("Rho"), Cond::ShowLatinLookalikes},
    {L'\u03A3', _("Sigma")},
    {L'\u03A4', _("Tau"), Cond::ShowLatinLookalikes},
    {L'\u03A5', _("Upsilon"), Cond::ShowLatinLookalikes},
    {L'\u03A6', _("Phi")},
    {L'\u03A7', _("Chi"), Cond::ShowLatinLookalikes},
    {L'\u03A8', _("Psi")},
    {L'\u03A9', _("Omega")},
  };

  bool const Show_mu = m_configuration->GreekSidebar_Show_mu();
  bool const ShowLatinLookalikes =
    m_configuration->GreekSidebar_ShowLatinLookalikes();

  m_lowercaseSizer->Clear(true);
  for (auto &def : lowerCaseDefs)
    if (def.condition == Cond::None ||
        (def.condition == Cond::Show_mu && Show_mu) ||
        (def.condition == Cond::ShowLatinLookalikes && ShowLatinLookalikes))
      m_lowercaseSizer->Add(
			    new CharButton(this, m_worksheet, m_configuration, def, true),
			    wxSizerFlags().Expand());

  m_uppercaseSizer->Clear(true);
  for (auto &def : upperCaseDefs)
    if (def.condition == Cond::None ||
        (def.condition == Cond::Show_mu && Show_mu) ||
        (def.condition == Cond::ShowLatinLookalikes && ShowLatinLookalikes))
      m_uppercaseSizer->Add(
			    new CharButton(this, m_worksheet, m_configuration, def, true),
			    wxSizerFlags().Expand());
}

void wxMaximaFrame::GreekPane::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  popupMenu->AppendCheckItem(EventIDs::menu_showLatinGreekLookalikes,
                             _(wxS("Show greek \u21D4 latin lookalikes")));
  popupMenu->Check(EventIDs::menu_showLatinGreekLookalikes,
                   m_configuration->GreekSidebar_ShowLatinLookalikes());
  popupMenu->AppendCheckItem(EventIDs::menu_showGreekMu,
                             _(wxS("Show lookalike for unit prefix µ")));
  popupMenu->Check(EventIDs::menu_showGreekMu, m_configuration->GreekSidebar_Show_mu());
  PopupMenu(&*popupMenu);
}

wxMaximaFrame::SymbolsPane::SymbolsPane(wxWindow *parent,
                                        Configuration *configuration,
                                        Worksheet *worksheet, int ID)
  : wxScrolled<wxPanel>(parent, ID), m_configuration(configuration),
    m_worksheet(worksheet) {
  ShowScrollbars(wxSHOW_SB_NEVER, wxSHOW_SB_DEFAULT);
  EnableScrolling(false, true);
  SetScrollRate(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());
  const CharButton::Definition symbolButtonDefinitions[] = {
    {L'\u00BD', _("1/2"), true},
    {L'\u00B2', _("to the power of 2"), true},
    {L'\u00B3', _("to the power of 3"), true},
    {L'\u221A',
     _("sqrt (needs parenthesis for its argument to work as a Maxima "
       "command)"),
     true},
    {L'\u2148'},
    {L'\u2147'},
    {L'\u210F'},
    {L'\u2208', _("in")},
    {L'\u2203', _("exists")},
    {L'\u2204', _("there is no")},
    {L'\u21D2', _("\"implies\" symbol"), true},
    {L'\u221E', _("Infinity"), true},
    {L'\u2205', _("empty")},
    {L'\u25b6'},
    {L'\u25b8'},
    {L'\u22C0', _("and"), true},
    {L'\u22C1', _("or"), true},
    {L'\u22BB', _("xor"), true},
    {L'\u22BC', _("nand"), true},
    {L'\u22BD', _("nor"), true},
    {L'\u21D4', _("equivalent"), true},
    {L'\u00b1', _("plus or minus")},
    {L'\u00AC', _("not"), true},
    {L'\u22C3', _("union")},
    {L'\u22C2', _("intersection")},
    {L'\u2286', _("subset or equal")},
    {L'\u2282', _("subset")},
    {L'\u2288', _("not subset or equal")},
    {L'\u2284', _("not subset")},
    {L'\u0127'},
    {L'\u0126'},
    {L'\u2202', _("partial sign")},
    {L'\u2207', _("nabla sign")},
    {L'\u222b', _("Integral sign")},
    {L'\u2245'},
    {L'\u221d', _("proportional to")},
    {L'\u2260', _("not bytewise identical"), true},
    {L'\u2264', _("less or equal"), true},
    {L'\u2265', _("greater than or equal"), true},
    {L'\u226A', _("much less than")},
    {L'\u226B', _("much greater than")},
    {L'\u2263', _("Identical to")},
    {L'\u2211', _("Sum sign")},
    {L'\u220F', _("Product sign")},
    {L'\u2225', _("Parallel to")},
    {L'\u27C2', _("Perpendicular to")},
    {L'\u219D', _("Leads to")},
    {L'\u2192', _("Right arrow")},
    {L'\u27F6', _("Long Right arrow")},
    {L'\u220e', _("End of proof")},
  };

  m_userSymbols = NULL;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  wxSizer *builtInSymbolsSizer = new wxWrapSizer(wxHORIZONTAL);
  wxPanel *builtInSymbols = new wxPanel(this);
  for (auto &def : symbolButtonDefinitions)
    builtInSymbolsSizer->Add(
			     new CharButton(builtInSymbols, m_worksheet, m_configuration, def),
			     wxSizerFlags().Expand());
  builtInSymbols->SetSizer(builtInSymbolsSizer);
  vbox->Add(builtInSymbols, wxSizerFlags().Expand());

  m_userSymbols = new wxPanel(this);
  m_userSymbolsSizer = new wxWrapSizer(wxHORIZONTAL);
  UpdateUserSymbols();
  m_userSymbols->SetSizer(m_userSymbolsSizer);
  vbox->Add(m_userSymbols, wxSizerFlags().Expand());
  Connect(wxEVT_SIZE, wxSizeEventHandler(wxMaximaFrame::SymbolsPane::OnSize),
          NULL, this);
  Connect(EventIDs::menu_additionalSymbols, wxEVT_MENU,
          wxCommandEventHandler(wxMaximaFrame::SymbolsPane::OnMenu), NULL,
          this);
  Connect(wxEVT_RIGHT_DOWN,
          wxMouseEventHandler(wxMaximaFrame::SymbolsPane::OnMouseRightDown));
  builtInSymbols->Connect(
			  wxEVT_RIGHT_DOWN,
			  wxMouseEventHandler(wxMaximaFrame::SymbolsPane::OnMouseRightDown));
  m_userSymbols->Connect(
			 wxEVT_RIGHT_DOWN,
			 wxMouseEventHandler(wxMaximaFrame::SymbolsPane::OnMouseRightDown));
  SetSizer(vbox);
  FitInside();
  SetMinSize(wxSize(GetContentScaleFactor() * 50, GetMinSize().y));
}

void wxMaximaFrame::SymbolsPane::OnMenu(wxCommandEvent &event) {
  std::unordered_map<int,std::function<void()>> m{
    {EventIDs::menu_additionalSymbols, [&](){
      wxWindowPtr<Gen1Wiz> wiz(new Gen1Wiz(
					   this, -1, m_configuration, _("Non-builtin symbols"),
					   _("Unicode symbols:"), m_configuration->SymbolPaneAdditionalChars(),
					   _("Allows to specify which not-builtin unicode symbols should be "
					     "displayed in the symbols sidebar along with the built-in symbols.")));
      // wiz->Centre(wxBOTH);
      wiz->SetLabel1ToolTip(_("Drag-and-drop unicode symbols here"));
      wiz->ShowWindowModalThenDo([this,wiz](int retcode) {
	if (retcode == wxID_OK)
	  m_configuration->SymbolPaneAdditionalChars(wiz->GetValue());
	UpdateUserSymbols();
      });
    }
    }};
  auto varFunc = m.find(event.GetId());
  if(varFunc == m.end())
    {
      wxASSERT(false);
    }
  else
    varFunc->second();
}

void wxMaximaFrame::SymbolsPane::OnMouseRightDown(
						  wxMouseEvent &WXUNUSED(event)) {
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  popupMenu->Append(EventIDs::menu_additionalSymbols, _("Add more symbols"),
                    wxEmptyString, wxITEM_NORMAL);
  popupMenu->Append(EventIDs::enable_unicodePane, _("Show all unicode symbols"),
                    wxEmptyString, wxITEM_NORMAL);
  PopupMenu(&*popupMenu);
}

void wxMaximaFrame::SymbolsPane::UpdateUserSymbols() {
  wxLogNull blocker;
  //  wxWindowUpdateLocker drawBlocker(this);
  while (!m_userSymbolButtons.empty()) {
    m_userSymbolButtons.front()->Destroy();
    m_userSymbolButtons.pop_front();
  }

  if (m_userSymbols == NULL)
    return;
  // Clear the user symbols pane
  m_userSymbols->DestroyChildren();

  // Populate the pane with a button per user symbol
  for (auto ch : m_configuration->SymbolPaneAdditionalChars()) {
    wxWindow *button = new CharButton(
				      m_userSymbols, m_worksheet, m_configuration,
				      {ch, _("A symbol from the configuration dialogue")}, true);
    m_userSymbolButtons.push_back(button);
    m_userSymbolsSizer->Add(button, wxSizerFlags().Expand());
  }
  Layout();
}

wxWindow *wxMaximaFrame::CreateFormatPane() {
  wxSizer *grid = new Buttonwrapsizer();
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(this, -1);
  panel->SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(panel, EventIDs::menu_format_text, _("Text"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_title, _("Title"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_section, _("Section"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_subsection, _("Subsection"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_subsubsection, _("Subsubsection"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_heading5, _("Heading 5"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_heading6, _("Heading 6"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_image, _("Image"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(panel, EventIDs::menu_format_pagebreak, _("Pagebreak"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  panel->SetSizer(grid);
  panel->FitInside();
  return panel;
}

void wxMaximaFrame::DrawPane::SetDimensions(int dimensions) {
  if (dimensions == m_dimensions)
    return;

  if (dimensions > 0) {
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
    if (dimensions > 2) {
      m_draw_contour->Enable(true);
      m_draw_setup3d->Enable(true);
    } else {
      m_draw_contour->Enable(false);
      m_draw_setup3d->Enable(false);
    }
  } else {
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

void wxMaximaFrame::DrawPane::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}

wxMaximaFrame::DrawPane::DrawPane(wxWindow *parent, int id)
  : wxScrolled<wxPanel>(parent, id) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  SetScrollRate(5, 5);
  m_grid = new Buttonwrapsizer(wxHORIZONTAL);
  m_dimensions = -1;
  int style = wxALL | wxEXPAND;
  int border = 0;

  m_grid->Add(m_draw_setup2d = new wxButton(this, EventIDs::menu_draw_2d, _("2D"),
                                            wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_setup2d->SetToolTip(_("Setup a 2D plot"));
  m_grid->Add(m_draw_setup3d = new wxButton(this, EventIDs::menu_draw_3d, _("3D"),
                                            wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_setup3d->SetToolTip(_("Setup a 3D plot"));
  m_grid->Add(m_draw_explicit =
	      new wxButton(this, EventIDs::menu_draw_explicit, _("Expression"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_explicit->SetToolTip(
			      _("The standard plot command: Plot an equation as a curve"));
  m_grid->Add(m_draw_implicit =
	      new wxButton(this, EventIDs::menu_draw_implicit, _("Implicit Plot"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_parametric =
	      new wxButton(this, EventIDs::menu_draw_parametric, _("Parametric Plot"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_points = new wxButton(this, EventIDs::menu_draw_points, _("Points"),
                                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_title =
	      new wxButton(this, EventIDs::menu_draw_title, _("Diagram title"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_title->SetToolTip(_("The diagram title"));
  m_grid->Add(m_draw_axis = new wxButton(this, EventIDs::menu_draw_axis, _("Axis"),
                                         wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_axis->SetToolTip(_("Setup the axis"));
  m_grid->Add(m_draw_contour =
	      new wxButton(this, EventIDs::menu_draw_contour, _("Contour"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_key = new wxButton(this, EventIDs::menu_draw_key, _("Plot name"),
                                        wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_key->SetToolTip(_("The next plot's title"));
  m_grid->Add(m_draw_fgcolor =
	      new wxButton(this, EventIDs::menu_draw_fgcolor, _("Line color"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_fgcolor->SetToolTip(_("The color of the next line to draw"));
  m_grid->Add(m_draw_fillcolor =
	      new wxButton(this, EventIDs::menu_draw_fillcolor, _("Fill color"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_fillcolor->SetToolTip(_("The fill color for the next objects"));
  m_grid->Add(m_draw_grid = new wxButton(this, EventIDs::menu_draw_grid, _("Grid"),
                                         wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_grid->SetToolTip(_("The grid in the background of the diagram"));
  m_draw_contour->SetToolTip(_("Contour lines for 3d plots"));
  m_grid->Add(m_draw_accuracy =
	      new wxButton(this, EventIDs::menu_draw_accuracy, _("Accuracy"),
			   wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_accuracy->SetToolTip(_("The accuracy versus speed tradeoff"));
  Connect(wxEVT_SIZE, wxSizeEventHandler(wxMaximaFrame::DrawPane::OnSize), NULL,
          this);
  vbox->Add(m_grid, wxSizerFlags(2).Expand());

  SetSizer(vbox);
  FitInside();
}

void wxMaximaFrame::ShowToolBar(bool show) {
  m_manager.GetPane(wxS("toolbar")).Show(show);
  m_manager.Update();
}

void wxMaximaFrame::BecomeLogTarget() {
  if (m_logPane != NULL)
    m_logPane->BecomeLogTarget();
}


std::vector<wxMaximaFrame *> wxMaximaFrame::m_topLevelWindows;
