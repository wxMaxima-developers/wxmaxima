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
#include "ArtProvider.h"
#include "Dirstructure.h"
#include <string>
#include <memory>
#include <algorithm>
#include "sidebars/CharButton.h"
#include "sidebars/StatSidebar.h"
#include "sidebars/FormatSidebar.h"
#include "sidebars/MathSidebar.h"
#include "wizards/Gen1Wiz.h"
#include "sidebars/GreekSidebar.h"
#include "sidebars/UnicodeSidebar.h"
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
#include <vector>
#include <unordered_map>

wxMaximaFrame::wxMaximaFrame(wxWindow *parent, int id,
                             const wxString &title, const wxPoint &pos,
                             const wxSize &size, long style)
: wxFrame(parent, id, title, pos, size, style),
  m_manager(new wxAuiManager(this, wxAUI_MGR_ALLOW_FLOATING | wxAUI_MGR_ALLOW_ACTIVE_PANE |
                             wxAUI_MGR_TRANSPARENT_HINT | wxAUI_MGR_HINT_FADE)),
  m_worksheet(new Worksheet(this, wxID_ANY, &m_configuration)),
  m_history(new History(this, -1, &m_configuration)),
  m_recentDocuments(wxS("document")),
  m_recentPackages(wxS("packages")) {
  // console
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  // wxWindowUpdateLocker noUpdates(this);

  // Add some shortcuts that aren't automatically set by menu entries.
  std::vector<wxAcceleratorEntry> entries;
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, wxS('K'), EventIDs::menu_autocomplete));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, WXK_TAB, EventIDs::menu_autocomplete));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_TAB,
                                       EventIDs::menu_autocomplete_templates));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, WXK_SPACE, EventIDs::menu_autocomplete));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('K'),
                                       EventIDs::menu_autocomplete_templates));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_SPACE,
                                       EventIDs::menu_autocomplete_templates));
  entries.push_back(wxAcceleratorEntry(wxACCEL_ALT, wxS('I'), wxID_ZOOM_IN));
  entries.push_back(wxAcceleratorEntry(wxACCEL_ALT, wxS('O'), wxID_ZOOM_OUT));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, WXK_ESCAPE,
                                       EventIDs::menu_convert_to_code));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('1'),
                                       EventIDs::menu_convert_to_comment));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('2'),
                                       EventIDs::menu_convert_to_title));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('3'),
                                       EventIDs::menu_convert_to_section));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('4'),
                                       EventIDs::menu_convert_to_subsection));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('5'),
                                       EventIDs::menu_convert_to_subsubsection));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL | wxACCEL_SHIFT, wxS('6'),
                                       EventIDs::menu_convert_to_heading5));
  entries.push_back(wxAcceleratorEntry(wxACCEL_CTRL, wxS('.'),
                                       EventIDs::menu_interrupt_id)); // Standard on the Mac
  entries.push_back(wxAcceleratorEntry(wxACCEL_NORMAL, WXK_F1, wxID_HELP));
  entries.push_back(wxAcceleratorEntry(wxACCEL_NORMAL, WXK_F11, EventIDs::menu_fullscreen));
  wxAcceleratorTable accel(entries.size(), entries.data());
  SetAcceleratorTable(accel);

  wxLogMessage(wxString::Format(_("wxMaxima version %s"), WXMAXIMA_VERSION));
#ifdef __WXMSW__
  if (wxSystemOptions::IsFalse("msw.display.directdraw"))
    wxLogMessage(_("Running on MS Windows"));
  else
    wxLogMessage(_("Running on MS Windows using DirectDraw"));
#endif

  int major = 0;
  int minor = 0;
  wxGetOsVersion(&major, &minor);
  wxLogMessage(_("OS: %s Version %li.%li"),
               wxGetOsDescription().utf8_str(),
               static_cast<long>(major), static_cast<long>(minor));

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

  // Now it is time to construct more of the window contents.
  // The table of contents
  m_tableOfContents = new TableOfContents(this, -1,
                                          &GetConfiguration(),
                                          GetWorksheet()->GetTreeAddress());

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
           wxString::Format(_("wxMaxima %s (%s) "), wxS(WXMAXIMA_VERSION),
                            wxPlatformInfo::Get().GetOperatingSystemDescription()) +
           _("[ unsaved ]"));
#else
  SetTitle(_("untitled"));
#endif

  m_sidebarNames[EventIDs::menu_pane_console] = wxS("console");
  m_sidebarCaption[EventIDs::menu_pane_console] = _("The worksheet");
  if(GetWorksheet())
    {
      m_manager->AddPane(GetWorksheet(),
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


      GetWorksheet()->m_mainToolBar = new ToolBar(this);
      m_sidebarNames[EventIDs::menu_pane_toolbar] = wxS("toolbar");
      m_sidebarCaption[EventIDs::menu_pane_toolbar] = _("The main toolbar");
      m_manager->AddPane(GetWorksheet()->m_mainToolBar,
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
    }
  m_sidebarNames[EventIDs::menu_pane_history] = wxS("history");
  m_sidebarCaption[EventIDs::menu_pane_history] = _("History");
  m_manager->AddPane(m_history, wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_history])
                    .Right());

  m_sidebarNames[EventIDs::menu_pane_structure] = wxS("structure");
  m_sidebarCaption[EventIDs::menu_pane_structure] = _("Table of Contents");
  m_manager->AddPane(m_tableOfContents, wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_structure])
                    .Right());

  if(GetWorksheet())
    {
      m_sidebarNames[EventIDs::menu_pane_xmlInspector] = wxS("XmlInspector");
      m_sidebarCaption[EventIDs::menu_pane_xmlInspector] = _("Raw XML monitor");
      m_manager->AddPane(m_xmlInspector, wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_xmlInspector])
                        .Right());
    }
  m_sidebarNames[EventIDs::menu_pane_stats] = wxS("stats");
  m_sidebarCaption[EventIDs::menu_pane_stats] = _("Statistics");
  m_manager->AddPane(new StatSidebar(this), wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_stats])
                    .Left());

  if(GetWorksheet())
    {
      m_sidebarNames[EventIDs::menu_pane_greek] = wxS("greek");
      m_sidebarCaption[EventIDs::menu_pane_greek] = _("Greek Letters");
      m_manager->AddPane(new GreekSidebar(this, &GetConfiguration(), GetWorksheet()), wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_greek])
                        .Left());

      m_sidebarNames[EventIDs::menu_pane_unicode] = wxS("unicode");
      m_sidebarCaption[EventIDs::menu_pane_unicode] = _("Unicode characters");
      //  wxWindowUpdateLocker unicodeBlocker(unicodePane);
      m_manager->AddPane(new UnicodeSidebar(this, GetWorksheet(), &GetConfiguration()), wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_unicode])
                        .Left());
    }


  if(GetWorksheet())
    {
      m_sidebarNames[EventIDs::menu_pane_variables] = wxS("variables");
      m_sidebarCaption[EventIDs::menu_pane_variables] = _("Variables");
      m_variablesPane = new Variablespane(this, wxID_ANY);
      m_manager->AddPane(
                        m_variablesPane,
                        wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_variables])
                        .Bottom());

      m_sidebarNames[EventIDs::menu_pane_symbols] = wxS("symbols");
      m_sidebarCaption[EventIDs::menu_pane_symbols] = _("Mathematical Symbols");
      m_symbolsSidebar = new SymbolsSidebar(this, &GetConfiguration(), GetWorksheet());
      m_manager->AddPane(m_symbolsSidebar,
                        wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_symbols])
                        .Left());
    }

  m_sidebarNames[EventIDs::menu_pane_math] = wxS("math");
  m_sidebarCaption[EventIDs::menu_pane_math] = _("General Math");
  m_manager->AddPane(new MathSidebar(this, wxID_ANY), wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_math])
                    .Left());

  if(GetWorksheet())
    {
      m_sidebarNames[EventIDs::menu_pane_wizard] = wxS("wizard");
      m_sidebarCaption[EventIDs::menu_pane_wizard] = _("The current Wizard");
      m_manager->AddPane(m_wizard =
                        new ScrollingGenWizPanel(
                                                 this, &GetConfiguration(),
                                                 GetWorksheet()->GetMaximaManual()),
                        wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_wizard])
                        .Caption(wxS("Example Wizard")));
    }

  m_sidebarNames[EventIDs::menu_pane_format] = wxS("format");
  m_sidebarCaption[EventIDs::menu_pane_format] = _("Insert");
  m_manager->AddPane(new FormatSidebar(this), wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_format])
                    .Left());

  m_sidebarNames[EventIDs::menu_pane_draw] = wxS("draw");
  m_sidebarCaption[EventIDs::menu_pane_draw] = _("Plot using Draw");
  m_manager->AddPane(m_drawPane = new DrawSidebar(this, -1),
                    wxAuiPaneInfo()
                    .Name(m_sidebarNames[EventIDs::menu_pane_draw])
                    .Left());

#ifdef USE_WEBVIEW
  if(GetWorksheet())
    {
      m_sidebarNames[EventIDs::menu_pane_help] = wxS("help");
      m_sidebarCaption[EventIDs::menu_pane_help] = _("Help");
      m_manager->AddPane(m_helpPane = new HelpBrowser(
                                                     this, &GetConfiguration(), GetWorksheet()->GetMaximaManual(),
                                                     wxS("file://") + wxMaximaManualLocation()),
                        wxAuiPaneInfo()
                        .Name(m_sidebarNames[EventIDs::menu_pane_help])
                        .Right());
    }
#endif

  for(const auto &pane: m_sidebarNames)
    if(m_manager->GetPane(pane.second).IsOk())
      m_manager->GetPane(pane.second).
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
    m_manager->LoadPerspective(perspective, false);
  }

  if(GetWorksheet())
    GetWorksheet()->m_mainToolBar->Realize();

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
      wxSize minSiz;
      if(m_manager->GetPane(pane.second).IsOk())
        {
          if(m_manager->GetPane(pane.second).window != NULL)
            minSiz = m_manager->GetPane(pane.second).window->GetMinClientSize();
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
            m_manager->GetPane(pane.second)
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
  m_manager->GetPane(m_sidebarNames[EventIDs::menu_pane_wizard]).Show(false);
  // The xml inspector slows down everything => close it at startup
  m_manager->GetPane(m_sidebarNames[EventIDs::menu_pane_xmlInspector]).Show(false);
  // The unicode selector needs loads of time for starting up
  // => close it at startup
  m_manager->GetPane(m_sidebarNames[EventIDs::menu_pane_unicode]).Show(false);

  // It somehow is possible to hide the Maxima worksheet - which renders
  // wxMaxima basically useless => force it to be enabled.
  m_manager->GetPane(m_sidebarNames[EventIDs::menu_pane_console]).Show(true)
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

  m_manager->Update();

  Connect(wxEVT_MENU_HIGHLIGHT,
          wxMenuEventHandler(wxMaximaFrame::OnMenuStatusText), NULL, this);
  Connect(EventIDs::menu_pane_dockAll, wxEVT_MENU,
          wxCommandEventHandler(wxMaximaFrame::DockAllSidebars), NULL, this);
}

std::size_t wxMaximaFrame::CountWindows() {
  size_t numberOfWindows = 1;

  wxWindowList::compatibility_iterator node = wxTopLevelWindows.GetFirst();
  while (node) {
    // Only count windows of the type wxMaxima
    if(dynamic_cast<wxMaximaFrame *>(node->GetData()) != NULL)
      numberOfWindows++;
    node = node->GetNext();
  }
  return numberOfWindows;
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
        if(GetWorksheet())
          {
            GetWorksheet()->SetWorkingGroup(NULL);
            // If we evaluated a cell that produces no output we still want the
            // cell to be unselected after evaluating it.
            if (GetWorksheet()->FollowEvaluation())
              GetWorksheet()->ClearSelection();
          }
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
}

void wxMaximaFrame::StatusSaveFinished() {
  m_forceStatusbarUpdate = true;
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
  wxString perspective = m_manager->SavePerspective();
  wxConfig::Get()->Write(wxS("AUI/perspective"), perspective);
  wxConfig::Get()->Flush();

  // In modern wxWidgets wxAUIManager does UnInit() itself.
#if !wxCHECK_VERSION(3, 1, 4)
  m_manager->UnInit();
#endif
  m_manager = NULL;
}

#define APPEND_MENU_ITEM(menu, id, label, help, stock)  \
  (menu)->Append((id), (label), (help), wxITEM_NORMAL);

void wxMaximaFrame::SetupFileMenu() {
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
}

void wxMaximaFrame::SetupEditMenu() {
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
}

void wxMaximaFrame::SetupViewMenu() {
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
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_1D_ASCII,
                                          _("as 1D ASCII"),
                                          _("Show equations in their linear form"));
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_2D_ASCII,
                                          _("as ASCII Art"),
                                          _("2D equations using ASCII Art"));
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_2D_UNICODE,
                                          _("as ASCII+Unicode Art"),
                                          _("2D equations using ASCII Art with unicode characters, if available"));
  m_equationTypeMenuMenu->AppendRadioItem(EventIDs::menu_math_as_graphics, _("in 2D"),
                                          _("Nice Graphical Equations"));
  m_equationTypeMenuMenu->AppendSeparator();
  m_equationTypeMenuMenu->Append(EventIDs::internalRepresentation, _("Show lisp representation"),
                                 _("Display an expression as maxima's internal "
                                   "lisp representation"));
  m_equationTypeMenuMenu->Append(EventIDs::wxMathML, _("Show XML representation"),
                                 _("Display an expression as wxMaxima's internal "
                                   "XML representation"));
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
                    GetConfiguration().InvertBackground());
  m_viewMenu->Append(EventIDs::menu_show_logwindow, wxS("Toggle log window"), _("Show or hide the wxMaxima logging window"), wxITEM_NORMAL);
  m_MenuBar->Append(m_viewMenu, _("View"));
}
void wxMaximaFrame::SetupCellMenu() {
  // Cell menu
  m_CellMenu = new wxMenu;
  if(GetWorksheet())
    {
      wxMenuItem *it =
        new wxMenuItem(m_CellMenu, EventIDs::menu_evaluate, _("Evaluate Cell(s)"),
                       _("Evaluate active or selected cell(s)"), wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetEvalBitmap(
                                                                 wxRendererNative::Get().GetCheckBoxSize(this)));
      m_CellMenu->Append(it);
    }
  m_CellMenu->Append(
                     EventIDs::EventIDs::menu_evaluate_all_visible, _("Evaluate All Visible Cells\tCtrl+R"),
                     _("Evaluate all visible cells in the document"), wxITEM_NORMAL);
  if(GetWorksheet())
    {
      wxMenuItem *it = new wxMenuItem(
                                      m_CellMenu, EventIDs::menu_evaluate_all, _("Evaluate All Cells\tCtrl+Shift+R"),
                                      _("Evaluate all cells in the document"), wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetEvalAllBitmap(
                                                                    wxRendererNative::Get().GetCheckBoxSize(this)));
      m_CellMenu->Append(it);
    }
  if(GetWorksheet())
    {
      wxMenuItem *it = new wxMenuItem(
                                      m_CellMenu, ToolBar::tb_evaltillhere,
                                      _("Evaluate Cells Above\tCtrl+Shift+P"),
                                      _("Re-evaluate all cells above the one the cursor is in"),
                                      wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetEvalTillHereBitmap(
                                                                         wxRendererNative::Get().GetCheckBoxSize(this)));
      m_CellMenu->Append(it);
    }
  if(GetWorksheet())
    {
      wxMenuItem *it = new wxMenuItem(
                                      m_CellMenu, ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"),
                                      _("Re-evaluate all cells below the one the cursor is in"),
                                      wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetEvalRestBitmap(
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
  m_CellMenu->Append(wxWindow::NewControlId(), _("Insert textbased cell"), insert_sub);
  m_CellMenu->Append(EventIDs::menu_insert_image, _("Insert Image..."), _("Insert image"),
                     wxITEM_NORMAL);
  m_CellMenu->Append(EventIDs::menu_add_pagebreak, _("Insert Page Break"),
                     _("Insert a page break"));
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
  {
    wxMenuItem *item = new wxMenuItem(m_CellMenu,
                                      EventIDs::popid_merge_cells,
                                      _("Merge Cells"));
#if wxCHECK_VERSION(3, 2, 0)
    item->SetBitmap(ArtProvider::GetCellMergeBundle());
#endif
    m_CellMenu->Append(item);
  }
  {
        wxMenuItem *item = new wxMenuItem(m_CellMenu,
                                          EventIDs::popid_divide_cell,
                                          _("Divide Cell"));
#if wxCHECK_VERSION(3, 2, 0)
        item->SetBitmap(ArtProvider::GetDivideCellBundle());
#endif
        m_CellMenu->Append(item);
  }
  m_CellMenu->AppendSeparator();
  m_CellMenu->AppendCheckItem(
                              EventIDs::popid_auto_answer, _("Automatically answer questions"),
                              _("Automatically fill in answers known from the last run"));

  m_MenuBar->Append(m_CellMenu, _("Ce&ll"));
}

void wxMaximaFrame::SetupMaximaMenu() {
  m_MaximaMenu = new wxMenu;

  if(GetWorksheet())
    {
      wxMenuItem *it =
        new wxMenuItem(m_MaximaMenu, EventIDs::menu_interrupt_id, _("&Interrupt\tCtrl+G"),
                       _("Interrupt current computation"), wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetInterruptBitmap(
                                                                      wxRendererNative::Get().GetCheckBoxSize(this)));
      m_MaximaMenu->Append(it);
    }

  if(GetWorksheet())
    {
      wxMenuItem *it = new wxMenuItem(m_MaximaMenu, ToolBar::menu_restart_id,
                                      _("&Restart Maxima\tCtrl+Alt+R"), _("Restart Maxima"),
                                      wxITEM_NORMAL);
      it->SetBitmap(GetWorksheet()->m_mainToolBar->GetRestartBitmap(
                                                                    wxRendererNative::Get().GetCheckBoxSize(this)));
      m_MaximaMenu->Append(it);
    }
  wxMenu *m_undefSub = new wxMenu;
  m_undefSub->Append(EventIDs::menu_kill, _("Delete named object..."),
                      _("Delete all objects with a given name"),
                      wxITEM_NORMAL);
  m_undefSub->AppendSeparator();
  m_undefSub->Append(EventIDs::menu_clear_fun, _("Delete F&unction..."),
                      _("Delete a function"), wxITEM_NORMAL);
  m_undefSub->Append(EventIDs::menu_clear_var, _("Delete V&ariable..."),
                      _("Delete a variable"), wxITEM_NORMAL);
  m_undefSub->AppendSeparator();
  m_undefSub->Append(EventIDs::menu_soft_restart, _("&Clear Memory"),
                      _("Delete all values from memory"), wxITEM_NORMAL);
  m_undefSub->Append(EventIDs::menu_kill_dependencies, _("Clear all dependencies"));
  m_undefSub->Append(EventIDs::menu_kill_values, _("Clear all variables"));
  m_undefSub->Append(EventIDs::menu_kill_functions, _("Clear all functions"));
  m_undefSub->Append(EventIDs::menu_kill_arrays, _("Clear all arrays"));
  m_undefSub->Append(EventIDs::menu_kill_myoptions, _("Reset option variables"));
  m_undefSub->Append(EventIDs::menu_kill_rules, _("Clear all rules"));
  m_undefSub->Append(EventIDs::menu_kill_aliases, _("Clear all aliases"));
  m_undefSub->Append(EventIDs::menu_kill_structures, _("Clear all structures"));
  m_undefSub->Append(EventIDs::menu_kill_labels, _("Clear all labels"));
  m_undefSub->Append(EventIDs::menu_kill_gradefs, _("Clear all gradefs"));
  m_undefSub->Append(EventIDs::menu_kill_props, _("Clear all props"));
  m_undefSub->Append(EventIDs::menu_kill_macros, _("Clear all macros"));
  m_undefSub->Append(EventIDs::menu_kill_let_rule_packages, _("Clear all let rule packages"));
  m_undefSub->Append(EventIDs::menu_garbage_collect, _("Free all unused items now"));
  m_undefSub->Append(EventIDs::menu_room, _("Show used + to-be-freed memory"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("Undefine"), m_undefSub);
  wxMenu *m_memorySub = new wxMenu;
  m_memorySub->Append(EventIDs::menu_garbage_collect, _("Free all unused items now"),
                      _("Lisp normally frees memory only when it has time or runs out of space"));
  m_memorySub->Append(EventIDs::menu_room, _("Show used + to-be-freed memory"),
                      _("Lisp normally frees memory only when it has time or runs out of space"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("Memory"), m_memorySub);

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
  regexMenu->Append(EventIDs::menu_sregex_load, _("Load the regular expression package (sregex)"));
  regexMenu->Append(EventIDs::menu_sregex_regex_compile, _("Compile a regular expression"));
  regexMenu->Append(EventIDs::menu_sregex_regex_match_pos, _("Position of a match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_match, _("Return a match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_split, _("Split on match"));
  regexMenu->Append(EventIDs::menu_sregex_subst_first, _("Substitute first match"));
  regexMenu->Append(EventIDs::menu_sregex_regex_subst, _("Substitute all matches"));
  regexMenu->Append(EventIDs::menu_sregex_string_to_regex,
                    _("Regular expression that matches a string"));
  stringMenu->Append(wxWindow::NewControlId(), _("Regular expressions"), regexMenu);
  wxMenu *operatingSystemMenu = new wxMenu;
  operatingSystemMenu->Append(EventIDs::menu_opsyst_load, _("Load the file/dir operations (operatingsystem)"));
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
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("File/directory functions"), operatingSystemMenu);

  m_gentranMenu = new wxMenu;
  m_gentranMenu->Append(EventIDs::gentran_load, _("Load the translation generator (gentran)"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_c, _("Output C"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_fortran, _("Output Fortran"));
  m_gentranMenu->AppendRadioItem(EventIDs::gentran_lang_ratfor,
                                 _("Output Rational Fortran"));
  m_gentranMenu->Append(EventIDs::gentran_to_stdout, _("Convert"));
  m_gentranMenu->Append(EventIDs::gentran_to_file, _("Convert + Write to file"));
  m_MaximaMenu->Append(wxWindow::NewControlId(), _("Maxima to other language"), m_gentranMenu);
  m_MenuBar->Append(m_MaximaMenu, _("&Maxima"));
}

void wxMaximaFrame::SetupEquationsMenu() {
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
}

void wxMaximaFrame::SetupMatrixMenu() {
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
}

void wxMaximaFrame::SetupCalculusMenu() {
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
}
void wxMaximaFrame::SetupSimplifyMenu() {
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

  wxString warningSign = wxT("\u26A0");
  if (!GetConfiguration().FontRendersChar(L'\u26A0', *wxNORMAL_FONT))
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
}

void wxMaximaFrame::SetupListMenu() {
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
}

void wxMaximaFrame::SetupPlotMenu() {
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
}

void wxMaximaFrame::SetupNumericMenu() {
  m_NumericMenu = new wxMenu;
  m_NumericMenu->AppendCheckItem(EventIDs::menu_num_out, _("&Numeric Output"),
                                 _("Numeric output"));
  m_NumericMenu->AppendCheckItem(EventIDs::menu_num_domain,
                                 _("Expect numbers harder to be complex"),
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
  m_NumericMenu->AppendCheckItem(EventIDs::menu_engineeringFormat,
                                 _("Engineering format (12.1e6 etc.)"),
                                 _("Print floating-point numbers with exponents dividable by 3"));
  m_NumericMenu->Append(EventIDs::menu_engineeringFormatSetup,
                        _("Setup the engineering format..."),
                        _("Fine-tune the display of engineering-format numbers"), wxITEM_NORMAL);

  wxMenu *quadpack_sub = new wxMenu;
  wxString integralSign = wxS("\u222B");
  if (!GetConfiguration().FontRendersChar(L'\u222B', *wxNORMAL_FONT))
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
}

void wxMaximaFrame::SetupHelpMenu() {
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
#ifdef USE_WEBVIEW
  if(GetConfiguration().GetDebugmode())
    m_HelpMenu->Append(EventIDs::menu_goto_url, _("Go to URL"));
#endif
  APPEND_MENU_ITEM(m_HelpMenu, EventIDs::menu_show_tip, _("Show &Tips..."),
                   _("Show a tip"), wxART_TIP);
  if(GetConfiguration().OfferInternalHelpBrowser())
    {
      m_HelpMenu->AppendSeparator();
      m_HelpMenu->AppendRadioItem(EventIDs::menu_wxmaxima_uses_help_sidebar,
                                  _("wxMaxima shows help in the sidebar"));
      m_HelpMenu->AppendRadioItem(EventIDs::menu_wxmaxima_uses_help_browser,
                                  _("wxMaxima shows help in a browser"));
      if(GetConfiguration().InternalHelpBrowser())
        m_HelpMenu->Check(EventIDs::menu_wxmaxima_uses_help_sidebar, true);
      else
        m_HelpMenu->Check(EventIDs::menu_wxmaxima_uses_help_browser, true);
    }
  m_HelpMenu->AppendSeparator();
  wxMenu *tutorials_sub = new wxMenu;
  tutorials_sub->Append(EventIDs::menu_help_solving, _("Solving equations with Maxima"),
                        "", wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_numberformats, _("Number types"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_tolerances,
                        _("Tolerance calculations with Maxima"), "",
                        wxITEM_NORMAL);
  tutorials_sub->Append(EventIDs::menu_help_2d, _("Advanced 2d plotting"), "",
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
  m_demo_sub = new wxMenu;
  m_HelpMenu->Append(wxWindow::NewControlId(), _("Demos"), m_demo_sub);
  m_HelpMenu->Append(EventIDs::menu_help_maxima_homepage,
                     _(wxS("↗Documentation on maxima's homepage")));
  m_HelpMenu->AppendSeparator();
  m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_internal_help,
                              _("Maxima shows help in the console"),
                              _("Tells maxima to show the help for ?, ?? and "
                                "describe() on the console"));
  if (GetConfiguration().OfferInternalHelpBrowser())
    m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_wxmaxima_help,
                                _("Maxima shows help in a sidebar"),
                                _("Tells maxima to show the help for ?, ?? and "
                                  "describe() in a wxMaxima sidebar"));
  m_HelpMenu->AppendRadioItem(EventIDs::menu_maxima_uses_html_help,
                              _("Maxima shows help in a browser"),
                              _("Tells maxima to show the help for ?, ?? and "
                                "describe() in a separate browser window"));
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
}

void wxMaximaFrame::SetupMenu() {
  m_MenuBar = new MainMenuBar();
  // Enables the window list on MacOs.
#ifdef __WXMAC__
  m_MenuBar->SetAutoWindowMenu(true);
#endif
  SetupFileMenu();
  SetupEditMenu();
  SetupViewMenu();
  SetupCellMenu();
  SetupMaximaMenu();
  SetupEquationsMenu();
  SetupMatrixMenu();
  SetupCalculusMenu();
  SetupSimplifyMenu();
  SetupListMenu();
  SetupPlotMenu();
  SetupNumericMenu();
  SetupHelpMenu();

  SetMenuBar(m_MenuBar);
#undef APPEND_MENU_ITEM
}

wxString wxMaximaFrame::GetDemoFile(wxWindowID id) const
{
  for(const auto &i: m_demoFilesIDs)
    if(i.first == id)
      return i.second;
  return wxEmptyString;
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
  if(!menu)
    return;
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
      if(id >= firstEntry + EventIDs::NumberOfRecentFiles)
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
      if(id >= firstEntry + EventIDs::NumberOfRecentFiles)
        return;
    }
}

void wxMaximaFrame::UpdateRecentDocuments() {
  long recentItems = 10;
  wxConfig::Get()->Read(wxS("recentItems"), &recentItems);

  if (recentItems < 5)
    recentItems = 5;
  if (recentItems > EventIDs::NumberOfRecentFiles)
    recentItems = 30;

  PopulateRecentDocumentsMenu(m_recentDocumentsMenu, EventIDs::menu_recent_document_0,
                              m_recentDocuments.Get());

  if(GetWorksheet())
    PopulateRecentDocumentsMenu(m_unsavedDocumentsMenu, EventIDs::menu_unsaved_document_0,
                                GetWorksheet()->m_unsavedDocuments.Get());

  PopulateRecentPackagesMenu(m_recentPackagesMenu, EventIDs::menu_recent_package_0,
                             m_recentPackages.Get());
}

void wxMaximaFrame::ReadConfig() {
  if(GetWorksheet())
    GetWorksheet()->UpdateConfig();
}

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
      config->Flush(true);

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
      wxDELETE(config);
      config = NULL;
    }
#endif
}

void wxMaximaFrame::RegisterAutoSaveFile() {
  wxString autoSaveFiles;
  ReReadConfig();
  wxConfigBase *config = wxConfig::Get();
  config->Read("AutoSaveFiles", &autoSaveFiles);
  if(GetWorksheet())
    GetWorksheet()->m_unsavedDocuments.AddDocument(m_tempfileName);
  ReReadConfig();
}

void wxMaximaFrame::RemoveTempAutosavefile() {
  if (m_tempfileName != wxEmptyString) {
    // Don't delete the file if we have opened it and haven't saved it under a
    // different name yet.
    if ((!!GetWorksheet()) && wxFileExists(m_tempfileName) &&
        (m_tempfileName != GetWorksheet()->m_currentFile)) {
      wxRemoveFile(m_tempfileName);
    }
  }
  m_tempfileName.Clear();
}

bool wxMaximaFrame::IsPaneDisplayed(int id) {
  auto item = m_sidebarNames.find(id);
  //  wxASSERT(item != m_sidebarNames.end());
  if(item != m_sidebarNames.end())
    return m_manager->GetPane(item->second).IsShown();
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
    m_manager->GetPane(pane.second).Dock();
  m_manager->Update();
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
            m_manager->GetPane(pane.second).Show(false);
        }
    }
  else
    {
      wxASSERT((id != EventIDs::menu_pane_console) || (show));
      if((id != EventIDs::menu_pane_console) || (show)) {
        auto item = m_sidebarNames.find(id);
        //  wxASSERT(item != m_sidebarNames.end());
        if(item != m_sidebarNames.end())
          m_manager->GetPane(item->second).Show(show);
      }
    }
  m_manager->Update();
}

void wxMaximaFrame::ShowToolBar(bool show) {
  m_manager->GetPane(wxS("toolbar")).Show(show);
  m_manager->Update();
}


