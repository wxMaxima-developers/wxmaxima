// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  This file declares the class wxMaximaFrame

  wxMaximaFrame draws everything which can be seen
  surrounding the worksheet.
*/
#ifndef WXMAXIMAFRAME_H
#define WXMAXIMAFRAME_H

#include "precomp.h"
#include <array>
#include <wx/wx.h>
#include "EventIDs.h"
#include <wx/dirctrl.h>
#include <wx/filehistory.h>
#include <wx/listbox.h>
#include <wx/bmpbuttn.h>
#include <wx/arrstr.h>
#include <wx/aui/aui.h>
#include <wx/notifmsg.h>
#include <wx/wrapsizer.h>
#include "sidebars/DrawSidebar.h"

#include "wizards/ScrollingGenWizPanel.h"
#include "Worksheet.h"
#include "sidebars/SymbolsSidebar.h"
#include "sidebars/HelpBrowser.h"
#include "RecentDocuments.h"
#include "Version.h"
#include "MainMenuBar.h"
#include "sidebars/History.h"
#include "sidebars/XmlInspector.h"
#include "StatusBar.h"
#include "sidebars/LogPane.h"
#include "sidebars/ButtonWrapSizer.h"
#include <list>


/*! The frame containing the menu and the sidebars
 */
class wxMaximaFrame : public wxFrame
{
public:
  wxMaximaFrame(wxWindow *parent, int id, const wxString &title,
                const wxPoint &pos = wxDefaultPosition,
                const wxSize &size = wxDefaultSize,
                long style = wxDEFAULT_FRAME_STYLE | wxSYSTEM_MENU | wxCAPTION, bool becomeLogTarget = true);

  /*! The destructor
   */
  virtual ~wxMaximaFrame();

  /*! Shows or hides the toolbar
    \param show
    - true:  Show the toolbar
    - false: Hide the toolbar
  */
  void ShowToolBar(bool show);

  /*! Update the recent documents list

    Copies the string array containing the list of recent documents to the
    recent documents menu.
  */
  void UpdateRecentDocuments();

  //! Used by UpdateRecentDocuments for populating a menu
  void PopulateRecentDocumentsMenu(wxMenu *menu, int firstEntry, const std::list<wxString> &items);
  //! Used by UpdateRecentPackages for populating a menu
  void PopulateRecentPackagesMenu(wxMenu *menu, int firstEntry, const std::list<wxString> &items);

  /*! true, if a Pane is currently enabled

    \param id The event that toggles the visibility of the pane that is
    to be queried
  */
  bool IsPaneDisplayed(int id);

  /*! Show or hide a sidebar

    \param id The type of the sidebar to show/hide
    \param show
    - true: show the sidebar
    - false: hide it
  */
  void ShowPane(int id, bool show = true);
  void TogglePaneVisibility(int id);

  //! Adds a command to the list  of recently used maxima commands
  void AddToHistory(const wxString &cmd)
    { m_history->AddToHistory(cmd); }

  /*! Inform the user about the length of the evaluation queue.

   */
  void EvaluationQueueLength(int length, int numberOfCommands = -1);

  /*! Set the status according to if maxima is calculating

    \param status
    - true:  Maxima is calculating
    - false: Maxima is waiting for input
  */
  void StatusMaximaBusy(StatusBar::MaximaStatus status){m_StatusMaximaBusy_next = status;}
  void UpdateStatusMaximaBusy();

  //! True=Maxima is currently busy.
  StatusBar::MaximaStatus m_StatusMaximaBusy;

  StatusBar::MaximaStatus m_StatusMaximaBusy_next;

  //! Set the status to "Maxima is saving"
  void StatusSaveStart();

  //! Set the status to "Maxima has finished saving"
  void StatusSaveFinished();

  //! Set the status to "Saving has failed"
  void StatusSaveFailed();

  //! Set the status to "Maxima is exporting"
  void StatusExportStart();

  //! Set the status to "Maxima has finished exporting"
  void StatusExportFinished();

  //! Set the status to "Exporting has failed"
  void StatusExportFailed();

protected:
  Configuration m_configuration;
  //! How many bytes did maxima send us until now?
  long m_bytesFromMaxima = 0;
  //! The process id of maxima. Is determined by ReadFirstPrompt.
  long m_pid;
  //! The last name GetTempAutosavefileName() has returned.
  wxString m_tempfileName;
  //! Issued if a notification is closed.
  void OnNotificationClose(wxCommandEvent WXUNUSED(&event));
  //! The status bar
  StatusBar *m_statusBar;
  //! The menu bar
  MainMenuBar *m_MenuBar;
  //! The "demo" sub-menu
  wxMenu *m_demo_sub;
  //! The submenus for the various "demo" sub-submenus
  std::vector<wxMenu *> m_demoSubs;
  //! The "view" menu
  wxMenu *m_viewMenu;
  //! The gentran menu
  wxMenu *m_gentranMenu;
  //! The subst submenu
  wxMenu *m_subst_Sub;
  //! The logexpand submenu
  wxMenu * m_logexpand_Sub;
  //! The file menu.
  wxMenu *m_FileMenu;
  //! The edit menu.
  wxMenu *m_EditMenu;
  //! The cell menu.
  wxMenu *m_CellMenu;
  //! The zoom submenu
  wxMenu *m_Edit_Zoom_Sub;
  //! The panes submenu
  wxMenu *m_Maxima_Panes_Sub;
  //! The equations menu.
  wxMenu *m_EquationsMenu;
  //! The maxima menu.
  wxMenu *m_MaximaMenu;
  //! The matrix menu.
  wxMenu *m_matrix_menu;
  //! The simplify menu
  wxMenu *m_SimplifyMenu;
  //! The factorials and gamma submenu
  wxMenu *m_Simplify_Gamma_Sub;
  //! Contains the menu for the debugger trigger settingxc
  wxMenu *m_debugTypeMenu;
  //! The trigonometric submenu
  wxMenu *m_Simplify_Trig_Sub;
  //! The complex submenu
  wxMenu *m_Simplify_Complex_Sub;
  //! The calculus menu
  wxMenu *m_CalculusMenu;

  //! The plot menu
  wxMenu *m_PlotMenu;
  //! The list menu
  wxMenu *m_listMenu;
  //! The numeric menu
  wxMenu *m_NumericMenu;
  //! The help menu
  wxMenu *m_HelpMenu;
  //! Remove an eventual temporary autosave file.
  void RemoveTempAutosavefile();
  //! Re-read the configuration.
  void ReReadConfig();
  //! Read the configuration from the OS's configuration storage.
  void ReadConfig();
  //! Remember a temporary autosave file name.
  void RegisterAutoSaveFile();
  /*! An instant single-window mode

    A last resort if https://trac.wxwidgets.org/ticket/18815 hinders one from
    re-docking windows.
  */
  void DockAllSidebars(wxCommandEvent &ev);

  wxString wxMaximaManualLocation();
public:
  //! The list of toplevel windows we currently maintain
  static std::vector<wxMaximaFrame *> m_topLevelWindows;

  /*! Makes this window the debug log target of all windows from this maxima process

    Only necessary on the mac where the same process creates loads of windows.
  */
  void BecomeLogTarget();

  //! Get the list of human-readable sidebarnames and IDs
  const std::unordered_map<int, wxString>  &GetSidebarNames() const {return m_sidebarNames;}
protected:
  Worksheet *GetWorksheet() const {return m_worksheet;}
private:
  //! The names our dockable sidebars are identified with in the config
  std::unordered_map<int, wxString> m_sidebarNames;
  //! The names our dockable sidebars are shown with
  std::unordered_map<int, wxString> m_sidebarCaption;
  //! How many bytes did maxima send us when we updated the statusbar?
  long m_bytesFromMaxima_last;
  wxTimer m_bytesReadDisplayTimer;
  //! True=We are currently saving.
  bool m_StatusSaving;

  void SetupToolBar();

/*!
  Create the menus.
*/
  void SetupMenu();

public:
  void StatusText(const wxString &text, bool saveInLog = true);
protected:
  ScrollingGenWizPanel *m_wizard = NULL;
  //! Are we inside a 2d or 3d draw command?
  long m_drawDimensions_last = -1;
  //! Do we have new text to output in the Status Bar?
  bool m_newStatusText;
  //! The text for the Right half of the Status Bar
  wxString m_rightStatusText;
  //! The text for the Left half of the Status Bar
  wxString m_leftStatusText;
  //! The default size for the window.
  virtual wxSize DoGetBestClientSize() const;
  //! The sidebar with the draw commands
  DrawSidebar *m_drawPane;
#ifdef USE_WEBVIEW
  HelpBrowser *m_helpPane;
#endif

  //! Looks up which demo file belongs to a wxWindowID
  wxString GetDemoFile(wxWindowID id) const;
protected:
  std::array<wxString,10> m_statusTextHistory;
  void OnMenuStatusText(wxMenuEvent &event);
  std::unordered_map<wxWindowID, wxString> m_demoFilesIDs;

  SymbolsSidebar *m_symbolsSidebar;
  //! The current length of the evaluation queue of commands we still need to send to maxima
  int m_EvaluationQueueLength;
  //! Do we need to update the display showing the evaluation queue length?
  bool m_updateEvaluationQueueLengthDisplay;
  //! The number of commands left in the current of the evaluation queue item
  int m_commandsLeftInCurrentCell;

  //! Do we expect the 1st prompt from maxima to appear?
  bool m_first;

  bool ToolbarIsShown();
  //! The manager for dynamic screen layouts
  wxAuiManager m_manager;
private:
  //! The worksheet itself
protected:
  //! The worksheet
  Worksheet * const m_worksheet = NULL;
  //! The history pane
  History * const m_history;
  //! A XmlInspector-like xml monitor
  XmlInspector *m_xmlInspector;
  //! true=force an update of the status bar at the next call of StatusMaximaBusy()
  bool m_forceStatusbarUpdate;
  //! The panel the log and debug messages will appear on
  LogPane *m_logPane;
  RecentDocuments m_recentDocuments;
  RecentDocuments m_recentPackages;
  wxMenu *m_recentDocumentsMenu;
  wxMenu *m_unsavedDocumentsMenu;
  wxMenu *m_recentPackagesMenu;
  wxMenu *m_autoSubscriptMenu;
  wxMenu *m_equationTypeMenuMenu;
  wxMenu *m_roundedMatrixParensMenu;
};

#endif // WXMAXIMAFRAME_H
