// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
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

/*!\file
  This file defines the contents of the class wxMaxima that contains most of the program's logic.

  The worksheet is defined in the class Worksheet instead and
  everything surrounding it in wxMaximaFrame.
 */

#include <wx/notifmsg.h>
#include "MaximaTokenizer.h"
#if defined __WXMSW__
//#include <wchar.h>
#endif
#include <wx/app.h>
#include "LoggingMessageDialog.h"
#include "wxMaxima.h"
#include <wx/wupdlock.h>
#include "wxMathml.h"
#include "CellList.h"
#include "ImgCell.h"
#include "DrawWiz.h"
#include "LicenseDialog.h"
#include "SubstituteWiz.h"
#include "IntegrateWiz.h"
#include "LimitWiz.h"
#include "Plot2dWiz.h"
#include "SeriesWiz.h"
#include "SumWiz.h"
#include "Version.h"
#include "Plot3dWiz.h"
#include "ConfigDialogue.h"
#include "CsvWiz.h"
#include "Gen1Wiz.h"
#include "Gen2Wiz.h"
#include "Gen3Wiz.h"
#include "Gen4Wiz.h"
#include "Gen5Wiz.h"
#include "BC2Wiz.h"
#include "MatWiz.h"
#include "SystemWiz.h"
#include "Printout.h"
#include "TipOfTheDay.h"
#include "EditorCell.h"
#include "SlideShowCell.h"
#include "PlotFormatWiz.h"
#include "ActualValuesStorageWiz.h"
#include "MaxSizeChooser.h"
#include "ListSortWiz.h"
#include "StringUtils.h"
#include "Maxima.h"
#include "wxMaximaIcon.h"
#include "WXMformat.h"
#include "ErrorRedirector.h"
#include "LabelCell.h"
#include "../data/manual_anchors.xml.gz.h"
#include <wx/colordlg.h>
#include <wx/clipbrd.h>
#include <wx/filedlg.h>
#include <wx/utils.h>
#include <wx/uri.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>
#include <wx/mimetype.h>
#include <wx/dynlib.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/artprov.h>
#include <wx/aboutdlg.h>
#include <wx/mstream.h>

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/sckstrm.h>
#include <wx/fs_mem.h>
#include <wx/persist/toplevel.h>

#include <wx/url.h>
#include <wx/sstream.h>
#include <list>
#include <memory>

#if defined __WXOSX__
#define MACPREFIX "wxMaxima.app/Contents/Resources/"
#endif

/*! Calls a member function from a function pointer

  \todo Replace this by a C++17 construct when we switch to C++17
 */
#define CALL_MEMBER_FN(object, ptrToMember)  ((object).*(ptrToMember))

wxDECLARE_APP (MyApp);

void MyApp::DelistTopLevelWindow(wxMaxima *window)
{
  auto pos = std::find(m_topLevelWindows.begin(), m_topLevelWindows.end(), window);
  if (pos != m_topLevelWindows.end())
    m_topLevelWindows.erase(pos);
}

void wxMaxima::ConfigChanged()
{
  if(m_worksheet->GetTree())
    m_worksheet->GetTree()->FontsChangedList();
  
  wxConfigBase *config = wxConfig::Get();
  int showLength = 0;

  config->Read(wxT("showLength"), &showLength);

  switch (showLength)
  {
  case 0:
    m_maxOutputCellsPerCommand = 600;
    break;
  case 1:
    m_maxOutputCellsPerCommand = 1200;
    break;
  case 2:
    m_maxOutputCellsPerCommand = 5000;
    break;
  case 3:
    m_maxOutputCellsPerCommand = -1;
    break;
  }
  m_worksheet->RecalculateForce();
  m_worksheet->RequestRedraw();

  wxLogMessage(_("Sending configuration data to maxima."));
  if(m_worksheet->m_configuration->UseSVG())
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_usesvg t)\n");
  else
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_usesvg nil)\n");
  if (m_worksheet->m_configuration->UsePngCairo())
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo t)\n");
  else
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo nil)\n");

  m_configCommands += wxT(":lisp-quiet (setq $wxsubscripts ") +
    m_worksheet->m_configuration->GetAutosubscript_string() +
    wxT(")\n");

  // A few variables for additional debug info in wxbuild_info();
  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq wxUserConfDir \"%s\")\n"),
                                       EscapeForLisp(Dirstructure::Get()->UserConfDir()).utf8_str());
  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq wxHelpDir \"%s\")\n"),
                                       EscapeForLisp(Dirstructure::Get()->HelpDir()).utf8_str());

  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq $wxplot_size '((mlist simp) %i %i))\n"),
                                       m_worksheet->m_configuration->DefaultPlotWidth(),
                                       m_worksheet->m_configuration->DefaultPlotHeight());

  if (m_worksheet->m_currentFile != wxEmptyString)
  {
    wxString filename(m_worksheet->m_currentFile);

    SetCWD(filename);
  }
  m_symbolsPane->UpdateUserSymbols();
}

wxMaxima::wxMaxima(wxWindow *parent, int id, wxLocale *locale, const wxString title,
                   const wxString &filename, const wxPoint pos, const wxSize size) :
  wxMaximaFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE | wxSYSTEM_MENU | wxCAPTION,
                MyApp::m_topLevelWindows.empty()),
  m_openFile(filename),
  m_gnuplotcommand("gnuplot"),
  m_parser(&m_worksheet->m_configuration)
{
  GnuplotCommandName("gnuplot");
  if(m_knownXMLTags.empty())
  {
    m_knownXMLTags[wxT("PROMPT")] = &wxMaxima::ReadPrompt;
    m_knownXMLTags[wxT("suppressOutput")] = &wxMaxima::ReadSuppressedOutput;
    m_knownXMLTags[wxT("wxxml-symbols")] = &wxMaxima::ReadLoadSymbols;
    m_knownXMLTags[wxT("variables")] = &wxMaxima::ReadVariables;
    m_knownXMLTags[wxT("watch_variables_add")] = &wxMaxima::ReadAddVariables;
    m_knownXMLTags[wxT("statusbar")] = &wxMaxima::ReadStatusBar;
    m_knownXMLTags[wxT("mth")] = &wxMaxima::ReadMath;
    m_knownXMLTags[wxT("math")] = &wxMaxima::ReadMath;
    m_knownXMLTags[wxT("ipc")] = &wxMaxima::ReadMaximaIPC;
  }

  if(m_variableReadActions.empty())
  {
    
    m_variableReadActions[wxT("maxima_userdir")] = &wxMaxima::VariableActionUserDir;
    m_variableReadActions[wxT("maxima_tempdir")] = &wxMaxima::VariableActionTempDir;
    m_variableReadActions[wxT("*autoconf-version*")] = &wxMaxima::VariableActionAutoconfVersion;
    m_variableReadActions[wxT("*autoconf-host*")] = &wxMaxima::VariableActionAutoconfHost;
    m_variableReadActions[wxT("*maxima-infodir*")] = &wxMaxima::VariableActionMaximaInfodir;
    m_variableReadActions[wxT("gnuplot_command")] = &wxMaxima::VariableActionGnuplotCommand;
    m_variableReadActions[wxT("*maxima-sharedir*")] = &wxMaxima::VariableActionMaximaSharedir;
    m_variableReadActions[wxT("*lisp-name*")] = &wxMaxima::VariableActionLispName;
    m_variableReadActions[wxT("*lisp-version*")] = &wxMaxima::VariableActionLispVersion;
    m_variableReadActions[wxT("*wx-load-file-name*")] = &wxMaxima::VariableActionWxLoadFileName;
    m_variableReadActions[wxT("wxsubscripts")] = &wxMaxima::VariableActionWxSubscripts;
    m_variableReadActions[wxT("lmxchar")] = &wxMaxima::VariableActionLmxChar;
    m_variableReadActions[wxT("numer")] = &wxMaxima::VariableActionNumer;
    m_variableReadActions[wxT("algebraic")] = &wxMaxima::VariableActionAlgebraic;
    m_variableReadActions[wxT("domain")] = &wxMaxima::VariableActionDomain;
    m_variableReadActions[wxT("wxanimate_autoplay")] = &wxMaxima::VariableActionAutoplay;
    m_variableReadActions[wxT("showtime")] = &wxMaxima::VariableActionShowtime;
    m_variableReadActions[wxT("engineering_format_floats")] = &wxMaxima::VariableActionEngineeringFormat;
    m_variableReadActions[wxT("display2d")] = &wxMaxima::VariableActionDisplay2D;
    m_variableReadActions[wxT("*alt-display2d*")] = &wxMaxima::VariableActionAltDisplay2D;
  }
  
  #ifdef HAVE_OMP_HEADER
  omp_init_lock(&m_helpFileAnchorsLock);
  #endif
  // Needed for making wxSocket work for multiple threads. We currently don't
  // use this feature.
  // wxSocketBase::Initialize();
  
  // Will be corrected by ConfigChanged()
  m_maxOutputCellsPerCommand = -1;
  m_exitAfterEval = false;
  m_exitOnError = false;
  m_locale = locale;
  wxLogMessage(_("Selected language: ") + m_locale->GetCanonicalName() +
               " (" + wxString::Format("%i", m_locale->GetLanguage()) + ")");
  wxString lang;
  if(wxGetEnv("LANG", &lang))
    wxLogMessage("LANG=" + lang);
  m_isLogTarget = MyApp::m_topLevelWindows.empty();
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  wxWindowUpdateLocker noUpdates(this);
  m_maximaBusy = true;
  m_evalOnStartup = false;
  m_dataFromMaximaIs = false;
  m_gnuplotProcess = NULL;
  m_openInitialFileError = false;
  m_maximaJiffies_old = 0;
  m_cpuTotalJiffies_old = 0;

  m_commandIndex = -1;
  m_isActive = true;
  wxConfigBase *config = wxConfig::Get();
  // If maxima fails to come up directly on startup of wxMaxima there is no need to retry.
  m_unsuccessfulConnectionAttempts = 11;
  m_outputCellsFromCurrentCommand = 0;
  m_CWD = wxEmptyString;
  m_pid = -1;
  m_hasEvaluatedCells = false;
  m_process = NULL;
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;
  m_ready = false;
  m_first = true;
  m_dispReadOut = false;

  m_server = NULL;

  config->Read(wxT("lastPath"), &m_lastPath);
  m_lastPrompt = wxEmptyString;

  m_closing = false;
  m_fileSaved = true;


  wxFileSystem::AddHandler(new wxMemoryFSHandler); // for saving wxmx

  UpdateRecentDocuments();

  m_worksheet->m_findDialog = NULL;
  m_oldFindString = wxEmptyString;
  m_oldFindFlags = 0;
  m_worksheet->m_currentFile = wxEmptyString;
  int findFlags = wxFR_DOWN | wxFR_MATCHCASE;
  wxConfig::Get()->Read(wxT("findFlags"), &findFlags);
  m_findData.SetFlags(findFlags);
  m_worksheet->SetFocus();
  m_worksheet->m_keyboardInactiveTimer.SetOwner(this, KEYBOARD_INACTIVITY_TIMER_ID);
  m_maximaStdoutPollTimer.SetOwner(this, MAXIMA_STDOUT_POLL_ID);
  m_compileHelpAnchorsTimer.SetOwner(this, COMPILEHELPANCHORS_ID);
  
  m_autoSaveTimer.SetOwner(this, AUTO_SAVE_TIMER_ID);
  Connect(
    wxEVT_TIMER,
    wxTimerEventHandler(wxMaxima::OnTimerEvent), NULL, this);

#if wxUSE_DRAG_AND_DROP
  m_worksheet->SetDropTarget(new MyDropTarget(this));
#endif

  StatusMaximaBusy(disconnected);

  m_statusBar->GetNetworkStatusElement()->Connect(wxEVT_LEFT_DCLICK,
                                                  wxCommandEventHandler(wxMaxima::NetworkDClick),
                                                  NULL, this);
  bool server = false;
  m_port = m_worksheet->m_configuration->DefaultPort();
  while (!(server = StartServer()))
  {
    wxLogMessage(
      wxString::Format(
        _("Trying to start a server on port %i instead"),m_port));
    m_port++;
    if ((m_port > m_worksheet->m_configuration->DefaultPort() + 15000) || (m_port > 65535))
    {
      LoggingMessageBox(_("wxMaxima could not start the server.\n\n"
                          "Please check you have network support\n"
                          "enabled and try again!"),
                        _("Fatal error"),
                        wxOK | wxICON_ERROR);
      break;
    }
  }

  if (!server)
    LeftStatusText(_("Starting server failed"));
  else
  {
    if(m_openFile.IsEmpty())
    {
      if (!StartMaxima())
        LeftStatusText(_("Starting Maxima process failed"));
    }
  }
  Connect(wxEVT_SCROLL_CHANGED,
          wxScrollEventHandler(wxMaxima::SliderEvent), NULL, this);
  Connect(wxID_CLOSE, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_check_updates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(Worksheet::popid_copy_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_animation, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_svg, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_emf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_rtf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_insert_text, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_subsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_subsubsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_heading5, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_heading6, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_popup_gnuplot, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_delete, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_simplify, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_factor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_expand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_solve, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_solve_num, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(enable_unicodePane, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_subst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_plot2d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_plot3d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_diff, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_integrate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_float, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_matlab, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_tex, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_copy_text, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_animation_save, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_animation_start, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(button_integrate, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_diff, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_solve, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::EquationsMenu), NULL, this);
  Connect(button_solve_ode, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::EquationsMenu), NULL, this);
  Connect(button_sum, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_expand, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_factor, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_taylor, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_limit, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_ratsimp, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_trigexpand, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_trigreduce, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_trigsimp, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_product, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::CalculusMenu), NULL, this);
  Connect(button_radcan, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_subst, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::MaximaMenu), NULL, this);
  Connect(button_plot2, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::PlotMenu), NULL, this);
  Connect(button_plot3, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::PlotMenu), NULL, this);
  Connect(button_map, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_matrix_row, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_matrix_col, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_matrix_row_list, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_matrix_col_list, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_csv2mat, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_mat2csv, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_submatrix, wxEVT_MENU,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(button_rectform, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(button_trigrat, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_polarform, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(ToolBar::menu_restart_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_EXIT, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(wxID_ABOUT, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_license, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(wxID_SAVE, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(wxID_SAVEAS, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_load_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_functions, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_variables, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_PREFERENCES, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_sconsole_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_export_html, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(wxID_HELP, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_help_tutorials, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_bug_report, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_build_info, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_interrupt_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::Interrupt), NULL, this);
  Connect(wxID_OPEN, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_batch_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(menu_ratsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_radsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_expand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_factor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_gfactor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_trigsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_trigexpand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_trigreduce, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_rectform, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_demoivre, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_num_out, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_num_domain, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_to_float, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_to_bfloat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_to_numer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(Worksheet::popid_special_constant_percent, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(Worksheet::popid_changeasterisk, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(Worksheet::popid_hideasterisk, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_exponentialize, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_invert_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_determinant, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_eigen, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_eigvect, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_adjoint_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_transpose, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_set_precision, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_set_displayprecision, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_engineeringFormat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_engineeringFormatSetup, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(menu_talg, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_tellrat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_modulus, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_allroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_bfallroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_realroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_solve, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_solve_to_poly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_solve_num, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_solve_ode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_map_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_enter_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_cpoly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_solve_lin, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_solve_algsys, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_eliminate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_clear_var, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_clear_fun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_ivp_1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_ivp_2, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_bvp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_bvp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_fun_def, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_divide, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_gcd, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_lcm, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_continued_fraction, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_partfrac, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_risch, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_integrate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_laplace, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_ilt, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_diff, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_series, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_limit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_lbfgs, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_gen_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_gen_mat_lambda, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_map, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_sum, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_maximahelp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_wxmaximahelp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_example, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_apropos, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_show_tip, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(menu_trigrat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_solve_de, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_atvalue, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_lhs, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_rhs, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(menu_sum, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_product, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_change_var, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_make_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_apply, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_time, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_factsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_factcomb, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_realpart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_imagpart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_nouns, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_logcontract, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_logexpand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(gp_plot2, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(gp_plot3, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(menu_animationautostart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(menu_animationframerate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(menu_plot_format, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(menu_soft_restart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_jumptoerror, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_display, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_pade, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(menu_add_path, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_COPY, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_text_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_tex_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_matlab_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_copy_mathml, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_UNDO, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_REDO, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_texform, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_to_fact, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(menu_to_gamma, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(wxID_PRINT, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PrintMenu), NULL, this);
  Connect(wxID_ZOOM_IN, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_ZOOM_OUT, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_zoom_80, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_ZOOM_100, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_zoom_120, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_zoom_150, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_zoom_200, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_zoom_300, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_labelwidth3, Worksheet::popid_labelwidth10, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_digits_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_digits_20, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_digits_50, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_digits_100, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_labels_autogenerated, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_labels_user, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_labels_useronly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_labels_disable, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_math_as_1D_ASCII, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_math_as_2D_ASCII, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_math_as_graphics, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_noAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_defaultAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_alwaysAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_roundedMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_straightMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_angledMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_squareMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_noMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_fullscreen, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_invertWorksheetBackground, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(ToolBar::tb_hideCode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_as_bitmap, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_as_svg, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_as_emf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_as_rtf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_copy_to_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_subst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(ToolBar::tb_interrupt, wxEVT_TOOL,
          wxCommandEventHandler(wxMaxima::Interrupt), NULL, this);
  Connect(ToolBar::tb_animation_startStop, wxEVT_TOOL,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(ToolBar::tb_animation_start, wxEVT_TOOL,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(ToolBar::tb_animation_stop, wxEVT_TOOL,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(ToolBar::tb_follow, wxEVT_TOOL,
          wxCommandEventHandler(wxMaxima::OnFollow), NULL, this);
  Connect(wxEVT_SOCKET,
          wxSocketEventHandler(wxMaxima::ServerEvent), NULL, this);
  Connect(wxEVT_CLOSE_WINDOW,
          wxCloseEventHandler(wxMaxima::OnClose), NULL, this);
  Connect(wxEVT_QUERY_END_SESSION,
          wxCloseEventHandler(wxMaxima::OnClose), NULL, this);
  Connect(wxEVT_END_SESSION,
          wxCloseEventHandler(wxMaxima::OnClose), NULL, this);
  Connect(maxima_process_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnProcessEvent), NULL, this);
  Connect(gnuplot_query_terminals_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnGnuplotQueryTerminals), NULL, this);
  Connect(gnuplot_process_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnGnuplotClose), NULL, this);
  Connect(Worksheet::popid_edit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditInputMenu), NULL, this);
  Connect(menu_evaluate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EvaluateEvent), NULL, this);
  Connect(Variablespane::varID_newVar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::VarReadEvent), NULL, this);
  Connect(Variablespane::varID_add_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::VarAddAllEvent), NULL, this);
  Connect(menu_add_comment, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_subsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_subsubsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_heading5, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_heading6, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_add_pagebreak, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_fold_all_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_unfold_all_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_add_comment, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_add_watch, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_add_watch_label, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_insert_previous_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_insert_previous_output, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_autocomplete, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_autocomplete_templates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_insert_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::popid_insert_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_history_previous, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_history_next, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_PASTE, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_paste_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_CUT, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_SELECTALL, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_comment_selection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_divide_cell, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_evaluate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_evaluate_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_eval, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_eval_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(ToolBar::tb_evaluate_rest, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_evaltillhere, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_merge_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_maxsizechooser, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(TableOfContents::popid_Fold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(TableOfContents::popid_Unfold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(TableOfContents::popid_SelectTocChapter, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(TableOfContents::popid_EvalTocChapter, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_evaluate_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(TableOfContents::popid_ToggleTOCshowsSectionNumbers, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_fold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(Worksheet::popid_unfold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(menu_evaluate_all_visible, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_evaluate_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(ToolBar::tb_evaltillhere, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(menu_list_create_from_elements, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_create_from_rule, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_create_from_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_actual_values_storage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_sort, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_length, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_push, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_pop, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_reverse, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_first, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_last, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_rest, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_restN, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_lastn, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_nth, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_map, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_use_actual_values, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_as_function_arguments, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_extract_value, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_do_for_each_element, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_remove_duplicates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_remove_element, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_append_item, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_append_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_interleave, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_list2matrix, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_matrix2list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_list_create_from_args, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(menu_draw_2d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_2d, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_3d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_3d, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_fgcolor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_fgcolor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_fillcolor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_fillcolor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_title, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_key, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_key, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_explicit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_explicit, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_implicit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_implicit, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_parametric, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_parametric, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_points, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_points, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_axis, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_axis, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_contour, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_contour, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_accuracy, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_accuracy, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_grid, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(menu_draw_grid, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(wxEVT_IDLE,
          wxIdleEventHandler(wxMaxima::OnIdle), NULL, this);
  Connect(menu_remove_output, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_hide_tooltipMarker, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_hide_tooltipMarkerForThisMessage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(menu_recent_document_0, menu_recent_document_29, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::OnRecentDocument), NULL, this);
  Connect(menu_recent_package_0, menu_recent_package_29, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::OnRecentPackage));
  Connect(menu_unsaved_document_0, menu_unsaved_document_29, wxEVT_MENU,        
          wxCommandEventHandler(wxMaxima::OnUnsavedDocument));
  Connect(menu_insert_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(menu_pane_hideall, menu_pane_stats, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ShowPane));
  Connect(menu_show_toolbar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(Worksheet::popid_auto_answer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(Worksheet::Worksheet::popid_never_autoanswer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(history_ctrl_id, wxEVT_LISTBOX_DCLICK,
          wxCommandEventHandler(wxMaxima::HistoryDClick), NULL, this);
  Connect(structure_ctrl_id, wxEVT_LIST_ITEM_ACTIVATED,
          wxListEventHandler(wxMaxima::TableOfContentsSelection), NULL, this);
  Connect(menu_stats_histogram, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_piechart, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_scatterplot, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_barsplot, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_boxplot, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_mean, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_median, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_var, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_dev, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_tt1, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_tt2, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_tnorm, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_linreg, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_lsquares, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_readm, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_stats_enterm, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::MatrixMenu), NULL, this);
  Connect(menu_stats_subsample, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::StatsMenu), NULL, this);
  Connect(menu_format_title, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_text, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_heading6, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_heading5, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_subsubsection, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_subsection, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_section, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_pagebreak, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(menu_format_image, wxEVT_BUTTON,
          wxCommandEventHandler( wxMaxima::InsertMenu), NULL, this);
  Connect(wxEVT_CHAR,
          wxCharEventHandler(wxMaxima::OnChar), NULL, this);
  Connect(wxEVT_KEY_DOWN,
          wxCharEventHandler(wxMaxima::OnKeyDown), NULL, this);
  Connect(ToolBar::tb_changeStyle, wxEVT_CHOICE,
          wxCommandEventHandler(wxMaxima::ChangeCellStyle), NULL, this);
  Connect(wxID_FIND, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxEVT_FIND,
          wxFindDialogEventHandler(wxMaxima::OnFind), NULL, this);
  Connect(wxEVT_FIND_NEXT,
          wxFindDialogEventHandler(wxMaxima::OnFind), NULL, this);
  Connect(wxEVT_FIND_REPLACE,
          wxFindDialogEventHandler(wxMaxima::OnReplace), NULL, this);
  Connect(wxEVT_FIND_REPLACE_ALL,
          wxFindDialogEventHandler(wxMaxima::OnReplaceAll), NULL, this);
  Connect(wxEVT_FIND_CLOSE,
          wxFindDialogEventHandler(wxMaxima::OnFindClose), NULL, this);
  Connect(wxEVT_ACTIVATE,
          wxActivateEventHandler(wxMaxima::OnActivate), NULL, this);
  Connect(wxEVT_ICONIZE,
          wxIconizeEventHandler(wxMaxima::OnMinimize), NULL, this);
  Connect(SYMBOLADDEVENT, wxCommandEventHandler(wxMaxima::OnSymbolAdd), NULL, this);
  Connect(Worksheet::popid_suggestion1,Worksheet::popid_suggestion10, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ReplaceSuggestion), NULL, this);
  m_worksheet->SetFocus();
  StartAutoSaveTimer();
}

void wxMaxima::StartAutoSaveTimer()
{
  m_autoSaveTimer.StartOnce(180000);
}

wxMaxima::~wxMaxima()
{
  if(m_server)
  {
    m_server->Destroy();
    m_server = NULL;
  }
#ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  KillMaxima(false);
  MyApp::DelistTopLevelWindow(this);

  if(MyApp::m_topLevelWindows.empty())
    wxExit();
  else
  {
    if(m_isLogTarget)
    {
      m_logPane->DropLogTarget();
      MyApp::m_topLevelWindows.back()->BecomeLogTarget();
    }
  }
  wxSocketBase::Shutdown();
}

#if wxUSE_DRAG_AND_DROP

bool MyDropTarget::OnDropFiles(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), const wxArrayString &files)
{

  if (files.GetCount() != 1)
    return true;

  if (wxGetKeyState(WXK_SHIFT))
  {
    m_wxmax->m_worksheet->InsertText(files[0]);
    return true;
  }

  if (files[0].Lower().EndsWith(wxT(".wxm")) ||
      files[0].Lower().EndsWith(wxT(".wxmx")))
  {
    if (m_wxmax->m_worksheet->GetTree() != NULL &&
        !m_wxmax->DocumentSaved())
    {
      int close = m_wxmax->SaveDocumentP();

      if (close == wxID_CANCEL)
        return false;

      if (close == wxID_YES)
      {
        if (!m_wxmax->SaveFile())
          return false;
      }
    }

    m_wxmax->OpenFile(files[0]);
    return true;
  }

  if (files[0].Lower().EndsWith(wxT(".png")) ||
      files[0].Lower().EndsWith(wxT(".jpeg")) ||
      files[0].Lower().EndsWith(wxT(".jpg")) ||
      files[0].Lower().EndsWith(wxT(".gif")) ||
      files[0].Lower().EndsWith(wxT(".svg")) ||
      files[0].Lower().EndsWith(wxT(".svgz"))
    )
  {
    m_wxmax->LoadImage(files[0]);
    return true;
  }

  m_wxmax->m_worksheet->InsertText(files[0]);
  return true;
}

#endif

void wxMaxima::FirstOutput()
{
  m_lastPrompt = wxT("(%i1) ");

  m_worksheet->SetFocus();
}

///--------------------------------------------------------------------------------
///  Appending stuff to output
///--------------------------------------------------------------------------------

/*! ConsoleAppend adds a new line s of type to the console window.
 *
 * It will call
 * DoConsoleAppend if s is in xml and DoRawCosoleAppend if s is not in xml.
 */
TextCell *wxMaxima::ConsoleAppend(wxString s, CellType type, const wxString &userLabel)
{
  TextCell *lastLine = NULL;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_worksheet->GetTree() == NULL)
    m_worksheet->InsertGroupCells(
      std::make_unique<GroupCell>(&(m_worksheet->m_configuration), GC_TYPE_CODE));

  m_dispReadOut = false;
  s.Replace(m_promptSuffix, wxEmptyString);

  // If the string we have to append only contains whitespace we return immediately.
  // TODO: Is a printf(false,"~%")$ a real use-case?
  wxString t(s);
  t.Trim();t.Trim(false);
  if (t.IsEmpty())
    return NULL;

  if (m_maxOutputCellsPerCommand > 0)
  {
    // If we already have output more lines than we are allowed to we a inform the user
    // about this and return.
    if (m_outputCellsFromCurrentCommand > m_maxOutputCellsPerCommand)
    {
      DoRawConsoleAppend(
        _("... [suppressed additional lines since the output is longer than allowed in the configuration] "),
        MC_TYPE_ERROR);
      return NULL;
    }
    else
    {
      m_outputCellsFromCurrentCommand++;
    }


    // If we already have output more lines than we are allowed to and we already
    // have informed the user about this we return immediately
    if (m_outputCellsFromCurrentCommand > m_maxOutputCellsPerCommand)
      return NULL;
  }

  if ((type != MC_TYPE_ERROR) && (type != MC_TYPE_WARNING))
    StatusMaximaBusy(parsing);

  if (type == MC_TYPE_DEFAULT)
  {
    // Show a busy cursor whilst interpreting and layouting potentially long data from maxima.
    wxBusyCursor crs;

    if (s.StartsWith(m_mathPrefix1) || s.StartsWith(m_mathPrefix2))
      DoConsoleAppend("<span>" + s + "</span>", type, AppendOpt(AppendOpt::NewLine | AppendOpt::BigSkip), userLabel);
    else
      lastLine = DoRawConsoleAppend(s, type);
  }
  else if (type == MC_TYPE_PROMPT)
  {
    m_lastPrompt = s;

    if (s.StartsWith(wxT("MAXIMA> ")))
    {
      s = s.Right(8);
    }
    else
      s = s + wxT(" ");

    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type,
                    AppendOpt(AppendOpt::NewLine | AppendOpt::BigSkip), userLabel);
  }
  else if (type == MC_TYPE_ERROR)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ERROR);
    GroupCell *tmp = m_worksheet->GetWorkingGroup(true);

    if (tmp == NULL)
    {
      if (m_worksheet->GetActiveCell())
        tmp = m_worksheet->GetActiveCell()->GetGroup();
    }

    if(tmp != NULL)
    {
      m_worksheet->GetErrorList().Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_commandIndex - 1);
    }
  }
  else if (type == MC_TYPE_WARNING)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_WARNING);
  }
  else if (type == MC_TYPE_TEXT)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_TEXT);
  }
  else if (type == MC_TYPE_ASCIIMATHS)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ASCIIMATHS);
  }
  else
    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type, AppendOpt::BigSkip);

  return lastLine;
}

void wxMaxima::DoConsoleAppend(wxString s, CellType type, AppendOpt opts, const wxString &userLabel)
{
  if (s.IsEmpty())
    return;

  s.Replace(wxT("\n"), wxT(" "), true);

  m_parser.SetUserLabel(userLabel);
  std::unique_ptr<Cell> cell(m_parser.ParseLine(s, type));

  wxASSERT_MSG(cell, _("There was an error in generated XML!\n\n"
                       "Please report this as a bug."));
  if (!cell)
    return;

  cell->SetBigSkip(opts & AppendOpt::BigSkip);
  auto *textCell = dynamic_cast<TextCell*>(cell.get());
  if (textCell)
    textCell->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
  m_worksheet->InsertLine(std::move(cell), (opts & AppendOpt::NewLine) || cell->BreakLineHere());
}

TextCell *wxMaxima::DoRawConsoleAppend(wxString s, CellType type, AppendOpt opts)
{
  TextCell *cell = nullptr;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_worksheet->GetTree() == NULL)
    m_worksheet->InsertGroupCells(
      std::make_unique<GroupCell>(&(m_worksheet->m_configuration), GC_TYPE_CODE));

  if (s.IsEmpty())
    return NULL;

  bool scrollToCaret = (!m_worksheet->FollowEvaluation() && m_worksheet->CaretVisibleIs());

  if (type == MC_TYPE_MAIN_PROMPT)
  {
    auto owned = std::make_unique<LabelCell>(m_worksheet->GetTree(), &(m_worksheet->m_configuration), s, TS_MAIN_PROMPT);
    owned->SetType(type);
    owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
    cell = owned.get();
    m_worksheet->InsertLine(std::move(owned), true);
  }
  else
  {
    std::unique_ptr<LabelCell> ownedCell;
    TextCell *incompleteTextCell = {};

    if (type == MC_TYPE_PROMPT)
    {
      ownedCell = std::make_unique<LabelCell>(m_worksheet->GetTree(),
                                              &(m_worksheet->m_configuration),
                                              wxEmptyString, TS_OTHER_PROMPT);
      incompleteTextCell = ownedCell.get();
      incompleteTextCell->ForceBreakLine(true);
    }
    else
      incompleteTextCell = m_worksheet->GetCurrentTextCell();

    if (incompleteTextCell)
    {
      int pos = s.Find("\n");
      wxString newVal = incompleteTextCell->GetValue();
      if(pos != wxNOT_FOUND)
      {
        newVal += s.Left(pos);
        s = s.Right(s.Length() - pos - 1);
      }
      else
      {
        newVal += s;
        s = wxEmptyString;
      }

      incompleteTextCell->SetValue(newVal);
      if (ownedCell)
        m_worksheet->InsertLine(std::move(ownedCell));
      if(s.IsEmpty())
      {
        incompleteTextCell->GetGroup()->ResetSize();
        incompleteTextCell->GetGroup()->Recalculate();
        return incompleteTextCell;
      }
    }

    wxStringTokenizer tokens(s, wxT("\n"));
    int count = 0;
    CellListBuilder<Cell> tree;
    while (tokens.HasMoreTokens())
    {
      wxString token = tokens.GetNextToken();
      // Move endles streams of compilation messages to the status bar...
      if(m_sbclCompilationRegEx.Matches(token))
      {
        wxString fileName = token;
        m_sbclCompilationRegEx.Replace(&fileName, wxT("\\1"));
        LeftStatusText(wxString::Format(_("Compiling %s"),fileName.utf8_str()));
      }
      else
      {
        auto owned = std::make_unique<TextCell>(
            m_worksheet->GetTree(), &(m_worksheet->m_configuration), token);
        owned->SetType(type);
        owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
        cell = owned.get();

        if (tokens.HasMoreTokens())
          cell->SetBigSkip(false);

        auto breakLine = bool(tree);
        tree.Append(std::move(owned));
        if (breakLine)
          tree.GetLastAppended()->ForceBreakLine(true);
      }
      count++;
    }
    m_worksheet->InsertLine(std::move(tree), true);
  }

  if(cell)
  {
    m_worksheet->m_configuration->AdjustWorksheetSize();
    m_worksheet->Recalculate(cell->GetGroup());
    if (scrollToCaret)
      m_worksheet->ScrollToCaret();
    m_worksheet->RequestRedraw();
  }
  return cell;
}

/*! Remove empty statements
 *
 * We need to remove any statement which would be considered empty
 * and thus cause an error. Comments within non-empty expressions seem to
 * be fine.
 *
 * What we need to remove is any statements which are any amount of whitespace
 * and any amount of comments, in any order, ended by a semicolon,
 * and nothing else.
 *
 * The most that should be left over is a single empty statement, ";".
 *
 * @param s The command string from which to remove comment expressions.
 */
void wxMaxima::StripLispComments(wxString &s)
{
  if (s.StartsWith(wxT(":lisp\n")) || s.StartsWith(wxT(":lisp ")))
  {
    int start = 0;
    int commentStart = 0;
    while ((commentStart = s.find(wxT(';'), start)) != wxNOT_FOUND)
    {
      int commentEnd = s.find(wxT('\n'), commentStart);
      if (commentEnd == wxNOT_FOUND)
        commentEnd = s.length();
      s = s.SubString(0, commentStart - 1) + s.SubString(commentEnd, s.length());
    }
  }
  else
    m_blankStatementRegEx.Replace(&s, wxT(";"));
}

void wxMaxima::SendMaxima(wxString s, bool addToHistory)
{
  // Normally we catch parenthesis errors before adding cells to the
  // evaluation queue. But if the error is introduced only after the
  // cell is placed in the evaluation queue we need to catch it here.
  int index;
  wxString parenthesisError = GetUnmatchedParenthesisState(s,index);
  if (parenthesisError.IsEmpty())
  {
    s = m_worksheet->UnicodeToMaxima(s);

    if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
      m_xmlInspector->Add_ToMaxima(s);

    m_dispReadOut = false;

    if (addToHistory)
      AddToHistory(s);

    StripLispComments(s);

    if (s.StartsWith(wxT(":lisp ")) || s.StartsWith(wxT(":lisp\n")))
      s.Replace(wxT("\n"), wxT(" "));

    s.Trim(true);
    s.Append(wxT("\n"));

    /// Check for function/variable definitions
    wxStringTokenizer commands(s, wxT(";$"));
    while (commands.HasMoreTokens())
    {
      wxString line = commands.GetNextToken();
      if (m_varRegEx.Matches(line))
        m_worksheet->AddSymbol(m_varRegEx.GetMatch(line, 1));

      if (m_funRegEx.Matches(line))
      {
        wxString funName = m_funRegEx.GetMatch(line, 1);
        m_worksheet->AddSymbol(funName);

        /// Create a template from the input
        wxString args = m_funRegEx.GetMatch(line, 2);
        wxStringTokenizer argTokens(args, wxT(","));
        funName << wxT("(");
        int count = 0;
        while (argTokens.HasMoreTokens())
        {
          if (count > 0)
            funName << wxT(",");
          wxString a = argTokens.GetNextToken().Trim().Trim(false);
          if (a != wxEmptyString)
          {
            if (a[0] == '[')
              funName << wxT("[<") << a.SubString(1, a.Length() - 2) << wxT(">]");
            else
              funName << wxT("<") << a << wxT(">");
            count++;
          }
        }
        funName << wxT(")");
        m_worksheet->AddSymbol(funName, AutoComplete::tmplte);
      }
    }

    if ((m_client) && (m_client->IsConnected()) && (s.Length() >= 1))
    {
      // If there is no working group and we still are trying to send something
      // we are trying to change maxima's settings from the background and might never
      // get an answer that changes the status again.
      if (m_worksheet->GetWorkingGroup())
        StatusMaximaBusy(calculating);
      else
        StatusMaximaBusy(waiting);

      wxScopedCharBuffer const data_raw = s.utf8_str();
      m_client->Write(data_raw.data(), data_raw.length());
      m_statusBar->NetworkStatus(StatusBar::transmit);
    }
  }
  else
  {
    DoRawConsoleAppend(
      _("Refusing to send cell to maxima: ") +
      parenthesisError + wxT("\n"),              MC_TYPE_ERROR);
    m_worksheet->SetWorkingGroup(nullptr);
    m_worksheet->m_evaluationQueue.Clear();
  }
  if(!m_maximaStdoutPollTimer.IsRunning())
    m_statusBar->SetMaximaCPUPercentage(-1);
  m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
}

///--------------------------------------------------------------------------------
///  Socket stuff
///--------------------------------------------------------------------------------

void wxMaxima::MaximaEvent(::MaximaEvent &event)
{
  using std::swap;
  switch (event.GetCause())
  {
  case MaximaEvent::READ_DATA:
    // Read out stderr: We will do that in the background on a regular basis, anyway.
    // But if we do it manually now, too, the probability that things are presented
    // to the user in chronological order increases a bit.
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    InterpretDataFromMaxima(event.GetData());
    break;
  case MaximaEvent::READ_PENDING:
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    break;
  case MaximaEvent::READ_TIMEOUT:
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    if (InterpretDataFromMaxima(event.GetData()))
      wxLogMessage(_("String from maxima apparently didn't end in a newline"));
    break;
  case MaximaEvent::WRITE_PENDING:
    m_statusBar->NetworkStatus(StatusBar::transmit);
    break;
  case MaximaEvent::WRITE_ERROR:
    DoRawConsoleAppend(_("Error writing to Maxima"), MC_TYPE_ERROR);
    break;
  case MaximaEvent::DISCONNECTED:
  {
    wxLogMessage(_("Connection to Maxima lost."));
    //  KillMaxima();
    #ifdef HAVE_OPENMP_TASKS
    #pragma omp taskwait
    #endif
    break;
  }
  }
}

/*!
 * ServerEvent is triggered when maxima connects to the socket server.
 */
void wxMaxima::ServerEvent(wxSocketEvent &event)
{
  switch (event.GetSocketEvent())
  {
  case wxSOCKET_CONNECTION :
    OnMaximaConnect();
    break;
  
  default:
    wxLogMessage(_("Encountered an unknown socket event."));
    break;
  }
}

void wxMaxima::OnMaximaConnect()
{
  if (m_client && (m_client->IsConnected()))
  {
    wxLogMessage(_("New connection attempt whilst already connected."));
    return;
  }
  if(m_process == NULL)
  {
    wxLogMessage(_("New connection attempt, but no currently running maxima process."));
    return;
  }
    
  m_statusBar->NetworkStatus(StatusBar::idle);
  m_worksheet->QuestionAnswered();
  m_currentOutput = wxEmptyString;

  m_client = std::make_unique<Maxima>(m_server->Accept(false));
  if (m_client)
  {
    m_client->Bind(EVT_MAXIMA, &wxMaxima::MaximaEvent, this);
    m_client->SetPipeToStdOut(m_pipeToStdout);
    SetupVariables();
  }
  else
  {
    wxLogMessage(_("Connection attempt, but connection failed."));
    m_unsuccessfulConnectionAttempts++;
    if(m_unsuccessfulConnectionAttempts < 12)
    {
      wxLogMessage(_("Trying to restart maxima."));          
      StartMaxima(true);
      return;
    }
  }
}

bool wxMaxima::StartServer()
{
  if(m_server)
  {
    m_server->Destroy();
    m_server = NULL;
  }
  
  RightStatusText(wxString::Format(_("Starting server on port %d"), m_port));

  wxIPV4address addr;
  if(!addr.AnyAddress())
    wxLogMessage(_("Cannot set the communication address to localhost."));
  if(!addr.Service(m_port))
    wxLogMessage(wxString::Format(_("Cannot set the communication port to %i."), m_port));

  m_server = new wxSocketServer(addr);
  if (!m_server->IsOk())
  {
    m_server->Destroy();
    m_server = NULL;
    RightStatusText(_("Starting server failed"));
    m_statusBar->NetworkStatus(StatusBar::error);
    return false;
  }
  m_server->SetEventHandler(*GetEventHandler());
  m_server->Notify(true);
  m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
  m_server->SetTimeout(30);
  RightStatusText(_("Server started"));
  return true;
}

///--------------------------------------------------------------------------------
///  Maxima process stuff
///--------------------------------------------------------------------------------

bool wxMaxima::StartMaxima(bool force)
{
  if(!m_server)
    StartServer();
  // cppcheck-suppress duplicateCondition
  if(!m_server)
    return false;
  
  wxString dirname;
  {
    wxString filename = m_worksheet->m_currentFile;
    if(filename.IsEmpty())
      filename = m_openFile;
    
    if(!filename.IsEmpty())
    {
      wxFileName dir(filename);
      dir.MakeAbsolute();
      dirname = dir.GetPath();
    }
  }
  // We only need to start or restart maxima if we aren't connected to a maxima
  // that till now never has done anything and therefore is in perfect working
  // order.
  wxString dirname_Old;
  wxGetEnv("MAXIMA_INITIAL_FOLDER", &dirname_Old);
  
  if ((m_process == NULL) || (m_hasEvaluatedCells) || force ||
      (dirname != dirname_Old))
  {
    if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
      m_xmlInspector->Clear();
    
    // Maxima isn't in lisp mode
    m_worksheet->m_configuration->InLispMode(false);
    
    // Maxima isn't asking questions
    m_worksheet->QuestionAnswered();
    
    // If we have an open file tell maxima to start in the directory the file is in
    wxUnsetEnv("MAXIMA_INITIAL_FOLDER");
    if(!dirname.IsEmpty())
    {
      if(wxDirExists(dirname))
      {
        // Tell maxima to start in the directory the file is in
        wxSetEnv(wxT("MAXIMA_INITIAL_FOLDER"),dirname);
      }
      else
      {
        wxLogWarning(wxString::Format(
                       wxT("Directory %s doesn't exist. Maxima might complain about that."),
                       dirname.utf8_str())
          );
      }
    }
    if((m_process != NULL) || (m_pid >= 0) || (m_client))
      KillMaxima();

    m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

    wxString command = GetCommand();
    if(!command.IsEmpty())
    {
      command.Append(wxString::Format(wxT(" -s %d "), m_port));

// TODO: Is this still necessary?
#if defined __WXOSX__
      wxSetEnv(wxT("DISPLAY"), wxT(":0.0"));
#endif

      // Tell maxima we want to be able to kill it on Ctrl+G by sending it a signal
      // Strictly necessary only on MS Windows where we don'r have a kill() command.
      wxSetEnv(wxT("MAXIMA_SIGNALS_THREAD"), wxT("1"));
      m_process = new wxProcess(this, maxima_process_id);
      m_process->Redirect();
//      m_process->SetPriority(wxPRIORITY_MAX);
      m_first = true;
      m_pid = -1;
      wxLogMessage(wxString::Format(_("Running maxima as: %s"), command.utf8_str()));
      
      wxEnvVariableHashMap environment;
      environment = m_worksheet->m_configuration->MaximaEnvVars();
      wxGetEnvMap(&environment);
      
      wxExecuteEnv *env = new wxExecuteEnv;
      env->env = environment;
      if (wxExecute(command, wxEXEC_ASYNC | wxEXEC_MAKE_GROUP_LEADER, m_process, env) <= 0 )
    {
      StatusMaximaBusy(process_wont_start);
      RightStatusText(_("Cannot start the maxima binary"));
      m_process = NULL;
      m_maximaStdout = NULL;
      m_maximaStderr = NULL;
      m_statusBar->NetworkStatus(StatusBar::offline);
      LoggingMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."), _("Error"),
                        wxOK | wxICON_ERROR);
      return false;
    }
    m_maximaStdout = m_process->GetInputStream();
    m_maximaStderr = m_process->GetErrorStream();
    m_lastPrompt = wxT("(%i1) ");
    StatusMaximaBusy(wait_for_start);
    }
    else
    {
      m_statusBar->NetworkStatus(StatusBar::offline);
      wxLogMessage(_("Cannot find a maxima binary and no binary chosen in the config dialogue."));
      return false;
    }
    m_worksheet->GetErrorList().Clear();
    
// Initialize the performance counter.
    GetMaximaCPUPercentage();
  }
  return true;
}


void wxMaxima::Interrupt(wxCommandEvent& WXUNUSED(event))
{
    if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  if (m_pid < 0)
  {
    m_MenuBar->EnableItem(menu_interrupt_id, false);
    return;
  }

#if defined (__WXMSW__)
  if(m_pid > 0)
  {
    // The following lines are adapted from maxima's winkill which William Schelter has
    // written and which has been improved by David Billinghurst and
    // Andrej Vodopivec.
    //
    // Winkill tries to find a shared memory region maxima provides we can set signals
    // in that maxima can listen to.
    //
    // For maxima's end of this means of communication see
    // interfaces/xmaxima/win32/win_signals.lisp
    // and interfaces/xmaxima/win32/winkill_lib.c in maxima's tree.
    HANDLE sharedMemoryHandle = 0;
    LPVOID sharedMemoryAddress = NULL;
    wchar_t sharedMemoryName[51];
    sharedMemoryName[50] = 0;

    // wxMaxima doesn't want to get interrupt signals.
    // SetConsoleCtrlHandler(NULL, true);

    /* First try to send the signal to gcl. */
    wxString sharedMemoryName1 = wxString::Format("gcl-%d", m_pid);
    wcsncpy(sharedMemoryName, sharedMemoryName1.wchar_str(), 50);
    sharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE,     /*  Read/write permission.   */
                                         FALSE,              /*  Do not inherit the name  */
                                         sharedMemoryName); /*  of the mapping object.   */

    /* If gcl is not running, send to maxima. */
    wxString sharedMemoryName2 = wxString::Format("maxima-%d", m_pid);
    if (sharedMemoryHandle == NULL) {
      wcsncpy(sharedMemoryName, sharedMemoryName2.wchar_str(), 50);
      sharedMemoryHandle = OpenFileMapping(FILE_MAP_WRITE,     /*  Read/write permission.   */
                                           FALSE,              /*  Do not inherit the name  */
                                           sharedMemoryName); /*  of the mapping object.   */
    }

    if (sharedMemoryHandle == NULL)
    {
      wxLogMessage(_("The Maxima process doesn't offer a shared memory segment we can send an interrupt signal to."));

      // No shared memory location we can send break signals to => send a
      // console interrupt.
      // Before we do that we stop our program from closing on receiving a Ctrl+C
      // from the console.
      SetConsoleCtrlHandler(NULL,TRUE);

      // We could send a CTRL_BREAK_EVENT instead of a CTRL_C_EVENT that
      // isn't handled in the 2010 clisp release (see:
      // https://sourceforge.net/p/clisp/bugs/735/)
      // ...but CTRL_BREAK_EVENT seems to crash clisp, see
      // https://sourceforge.net/p/clisp/bugs/736/
      //
      // And we need to send the CTRL_BREAK_EVENT to our own console, which
      // has the group ID 0, see
      // https://docs.microsoft.com/en-us/windows/console/generateconsolectrlevent
      if (GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0) == 0)
      {
        LPTSTR errorText = NULL;

        FormatMessage(
          FORMAT_MESSAGE_FROM_SYSTEM
          |FORMAT_MESSAGE_ALLOCATE_BUFFER
          |FORMAT_MESSAGE_IGNORE_INSERTS,
          NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
          errorText,0,NULL);

        wxString errorMessage;
        if (!errorText)
          errorMessage = _("Could not send an interrupt signal to maxima.");
        else
        {
          errorMessage = wxString::Format(_("Interrupting maxima: %s"),
                                          errorText);
          LocalFree(errorText);
        }

        LeftStatusText(errorMessage);
        wxLogMessage(errorMessage);
        return;
      }
    }
    else
    {
      sharedMemoryAddress = MapViewOfFile(sharedMemoryHandle, /* Handle to mapping object.  */
                                          FILE_MAP_WRITE,      /* Read/write permission.  */
                                          0,                   /* Max.  object size.  */
                                          0,                   /* Size of hFile.  */
                                          0);                  /* Map entire file.  */

      if (sharedMemoryAddress == NULL)
      {
        wxLogMessage(_("Could not map view of the file needed in order to "
                       "send an interrupt signal to maxima."));
        return;
      }

      // Set the bit for the SIGINT handler
      int value = (1 << (wxSIGINT));
      volatile int *sharedMemoryContents = (int *)(sharedMemoryAddress);
      *sharedMemoryContents = *sharedMemoryContents | value;
      wxLogMessage(_("Sending an interrupt signal to Maxima."));
      UnmapViewOfFile(sharedMemoryAddress);
      CloseHandle(sharedMemoryHandle);
      sharedMemoryAddress = NULL;
      sharedMemoryHandle = NULL;
    }
  }
  else
  {
    if(m_process)
    {
      // We need to send the CTRL_BREAK_EVENT to the process group, not
      // to the lisp.
      long pid = m_process->GetPid();
      if (!GenerateConsoleCtrlEvent(CTRL_C_EVENT, pid))
      {
        wxLogMessage(_("Could not send an interrupt signal to maxima."));
        return;
      }
      else
        wxLogMessage(_("Sending an interactive Interrupt signal (Ctrl+C) to Maxima."));
    }
  }
  #else
  wxLogMessage(_("Sending Maxima a SIGINT signal."));
  wxProcess::Kill(m_pid, wxSIGINT);
  #endif
}

void wxMaxima::BecomeLogTarget()
{
  if(m_logPane != NULL)
    m_logPane->BecomeLogTarget();
}

void wxMaxima::KillMaxima(bool logMessage)
{
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  if(logMessage && (m_closing || (m_process == NULL) || (m_pid > 0)))
  {
    wxLogMessage(_("Killing Maxima."));
    if(m_history)
      m_history->MaximaSessionStart();
  }
  m_closing = true;
  m_worksheet->m_variablesPane->ResetValues();
  m_varNamesToQuery = m_worksheet->m_variablesPane->GetEscapedVarnames();
  m_configCommands = wxEmptyString;
  // The new maxima process will be in its initial condition => mark it as such.
  m_hasEvaluatedCells = false;

  m_worksheet->SetWorkingGroup(nullptr);
  m_worksheet->m_evaluationQueue.Clear();
  EvaluationQueueLength(0);

  // We start checking for maximas output again as soon as we send some data to the program.
  m_statusBar->SetMaximaCPUPercentage(0);
  m_CWD = wxEmptyString;
  m_worksheet->QuestionAnswered();
  m_currentOutput = wxEmptyString;
  // If we did close maxima by hand we already might have a new process
  // and therefore invalidate the wrong process in this step
  if (m_process)
    m_process->Detach();
  m_process = NULL;
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;

  if(m_client && (m_client->IsConnected()))
  {
    // Make wxWidgets close the connection only after we have sent the close command.
    m_client->Socket()->SetFlags(wxSOCKET_WAITALL);
    // Try to gracefully close maxima.
    if (m_worksheet->m_configuration->InLispMode())
      SendMaxima(wxT("($quit)"));
    else
      SendMaxima(wxT("quit();"));

    // The following command should close maxima, as well.
    m_client = nullptr;
  }

  // Just to be absolutely sure: Additionally try to kill maxima
  if (m_pid > 0)
  {
 // wxProcess::kill will fail on MSW. Something with a console.
    SuppressErrorDialogs logNull;
    if(wxProcess::Kill(m_pid, wxSIGKILL,  wxKILL_CHILDREN) != wxKILL_OK)
    {
      if(wxProcess::Kill(m_pid, wxSIGKILL) != wxKILL_OK)
        wxLogMessage(_("Sending a wxSIGKILL to maxima has failed"));
      else
        wxLogMessage(_("Sent wxSIGKILL to maxima, but not to its child processes"));
    }
  }
  m_worksheet->m_configuration->InLispMode(false);

  // As we might have killed maxima before it was able to clean up its
  // temp files we try to do so manually now:
  if(m_maximaTempDir != wxEmptyString)
  {
    SuppressErrorDialogs logNull;
    if(wxFileExists(m_maximaTempDir + wxT("/maxout") + wxString::Format("%li.gnuplot",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/maxout") + wxString::Format("%li.gnuplot",m_pid));
    if(wxFileExists(m_maximaTempDir + wxT("/data") + wxString::Format("%li.gnuplot",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/data") + wxString::Format("%li.gnuplot",m_pid));
    if(wxFileExists(m_maximaTempDir + wxT("/maxout") + wxString::Format("%li.xmaxima",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/maxout") + wxString::Format("%li.xmaxima",m_pid));
    if(wxFileExists(m_maximaTempDir + wxT("/maxout_") + wxString::Format("%li.gnuplot",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/maxout_") + wxString::Format("%li.gnuplot",m_pid));
    if(wxFileExists(m_maximaTempDir + wxT("/data_") + wxString::Format("%li.gnuplot",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/data_") + wxString::Format("%li.gnuplot",m_pid));
    if(wxFileExists(m_maximaTempDir + wxT("/maxout_") + wxString::Format("%li.xmaxima",m_pid)))
      wxRemoveFile(m_maximaTempDir + wxT("/maxout_") + wxString::Format("%li.xmaxima",m_pid));
  }
  m_pid = -1;
}

void wxMaxima::OnGnuplotQueryTerminals(wxProcessEvent& WXUNUSED(event))
{
  if(!m_gnuplotTerminalQueryProcess)
    return;
  wxString gnuplotMessage;
  {
    wxInputStream* istream = m_gnuplotTerminalQueryProcess->GetInputStream();
    wxTextInputStream textin(*istream);
    while(!istream->Eof())
      gnuplotMessage += textin.ReadLine()+"\n";
  }
  {
    wxInputStream* istream = m_gnuplotTerminalQueryProcess->GetErrorStream();
    wxTextInputStream textin(*istream);
    while(!istream->Eof())
      gnuplotMessage += textin.ReadLine()+"\n";
  }
  gnuplotMessage.Trim(true);
  gnuplotMessage.Trim(false);
  wxLogMessage("Terminals supported by gnuplot: "+gnuplotMessage);
  if(gnuplotMessage.Contains(wxT("pngcairo")))
  {
    wxLogMessage(_("Using gnuplot's pngcairo driver for embedded plots"));
    if (!m_worksheet->m_configuration->UsePngCairo())
      m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo t)\n");
    m_worksheet->m_configuration->UsePngCairo(true);
  }
  else
  {
    wxLogMessage(_("Using gnuplot's antialiassing-less png driver for embedded plots as pngcairo could not be found"));
    if (m_worksheet->m_configuration->UsePngCairo())
      m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo nil)\n");
    m_worksheet->m_configuration->UsePngCairo(false);
  }
  m_gnuplotTerminalQueryProcess->CloseOutput();
  m_gnuplotTerminalQueryProcess = NULL;
  
}

void wxMaxima::OnGnuplotClose(wxProcessEvent& WXUNUSED(event))
{
  m_gnuplotProcess = NULL;
  wxLogMessage(_("Gnuplot has closed."));
}

void wxMaxima::OnProcessEvent(wxProcessEvent& event)
{
  wxLogMessage(_("Maxima process (pid %i) has terminated with exit code %i."),
               event.GetPid(), event.GetExitCode());
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  if(m_maximaStdout)
  {
    wxTextInputStream istrm(*m_maximaStdout, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStdout->CanRead()))
      o += ch;
    
    wxString o_trimmed = o;
    o_trimmed.Trim();
    if(!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stdout: %s"), o.utf8_str());
  }
  if(m_maximaStderr)
  {
    wxTextInputStream istrm(*m_maximaStderr, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStderr->CanRead()))
      o += ch;
    
    wxString o_trimmed = o;
    o_trimmed.Trim();
    if(!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stderr: %s"), o.utf8_str());
  }
  m_statusBar->NetworkStatus(StatusBar::offline);
  if (!m_closing)
  {
    RightStatusText(_("Maxima process terminated unexpectedly."));

    if(m_first)
    {
      LoggingMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."), _("Error"),
                   wxOK | wxICON_ERROR);
    }
    
    // Let's see if maxima has told us why this did happen.
    ReadStdErr();
    ConsoleAppend(wxT("\nMaxima exited...\n"),
                  MC_TYPE_ERROR);

    if (m_unsuccessfulConnectionAttempts > 10)
      ConsoleAppend(wxT("Restart Maxima with 'Maxima->Restart Maxima'.\n"),
                    MC_TYPE_ERROR);
    else
    {
      ConsoleAppend(wxT("Trying to restart Maxima.\n"),
                    MC_TYPE_ERROR);
      // Perhaps we shouldn't restart maxima again if it outputs a prompt and
      // crashes immediately after => Each prompt is deemed as but one hint
      // for a working maxima while each crash counts twice.
      m_unsuccessfulConnectionAttempts += 2;
      StartMaxima(true);
    }
    m_worksheet->m_evaluationQueue.Clear();
  }
  StatusMaximaBusy(disconnected);
  UpdateToolBar();
  UpdateMenus();
}

///--------------------------------------------------------------------------------
///  Dealing with stuff read from the socket
///--------------------------------------------------------------------------------

void wxMaxima::ReadFirstPrompt(wxString &data)
{
  int end;
  if((end = m_currentOutput.Find(m_firstPrompt)) == wxNOT_FOUND)
    return;

  m_bytesFromMaxima = 0;

  int start = 0;
  start = data.Find(wxT("Maxima "));
  if (start == wxNOT_FOUND)
    start = 0;
  FirstOutput();

  m_maximaBusy = false;

  // Wait for a line maxima informs us about it's process id in.
  int s = data.Find(wxT("pid=")) + 4;
  int t = s + data.SubString(s, data.Length()).Find(wxT("\n")) - 1;

  // Read this pid
  if (s < t)
    data.SubString(s, t).ToLong(&m_pid);

  if (m_pid > 0)
    m_MenuBar->EnableItem(menu_interrupt_id, true);

  m_client->ClearFirstPrompt();
  m_first = false;
  StatusMaximaBusy(waiting);
  m_closing = false; // when restarting maxima this is temporarily true

  wxString prompt_compact = data.Left(start + end + m_firstPrompt.Length() - 1);
  prompt_compact.Replace(wxT("\n"), wxT("\u21b2"));


  wxLogMessage(wxString::Format(_("Received maxima's first prompt: %s"),
                                prompt_compact.utf8_str()));

  wxLogMessage(wxString::Format(_("Maxima's PID is %li"),(long)m_pid));
  // Remove the first prompt from Maxima's answer.
  data = data.Right(data.Length() - end - m_firstPrompt.Length());

  if (m_worksheet->m_evaluationQueue.Empty())
  {
    // Inform the user that the evaluation queue is empty.
    EvaluationQueueLength(0);
    if (m_evalOnStartup)
    {
      wxLogMessage(_("Starting evaluation of the document"));
      m_evalOnStartup = false;
      m_worksheet->AddDocumentToEvaluationQueue();
      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }
    else
    {
      m_evalOnStartup = false;
      if ((m_worksheet->m_configuration->GetOpenHCaret()) && (m_worksheet->GetActiveCell() == NULL))
        m_worksheet->OpenNextOrCreateCell();
    }
    if (m_exitAfterEval && m_worksheet->m_evaluationQueue.Empty())
      Close();
  }
  else
    TriggerEvaluation();
}

bool wxMaxima::ParseNextChunkFromMaxima(wxString &data)
{
  wxString miscText;
  miscText.reserve(data.Length());
  wxString tagName;
  tagName.reserve(64);
  auto tagIndex = m_knownXMLTags.end();
  bool tagFound = false;
  wxString::const_iterator it;
  for (it = data.begin(); (it != data.end()) && !tagFound; ++it)
  {
    if(*it == wxT('<'))
    {
      tagName = wxEmptyString;
      wxString::const_iterator it2 = it;
      ++it2;
      for (;
           (it2 != data.end());
           ++it2
        )
      {
        if (((*it2 >= wxT('a')) &&
             (*it2 <= wxT('z'))) ||
            ((*it2 >= wxT('A')) &&
             (*it2 <= wxT('Z'))) ||
            (*it2 == wxT('_')) ||
            (*it2 == wxT('-'))
          )
          tagName += *it2;
        else
        {
          if(*it2 == wxT('>'))
          {
            tagIndex = m_knownXMLTags.find(tagName);
            tagFound = tagIndex != m_knownXMLTags.end();
            if (!tagFound)
            {
              miscText += wxT("<") + tagName + wxT(">");
              tagName = wxEmptyString;
              it = it2;
            }
            break;
          }
          else
          {
            miscText += wxT("<") + tagName;
            tagName = wxEmptyString;
            
            it = it2;
            break;
          }
        }
      }
      it = it2;
      if(!tagFound)
        miscText += wxT("<") + tagName;
    }
    else
      miscText += *it;
  }
  bool retval = false;
  if(!miscText.IsEmpty())
  {
    retval = true;
    ReadMiscText(miscText);

    // Remove the miscellaneous text we just have processed
    wxString rest;
    rest.reserve(data.Length());
    if(tagFound)
      rest = wxT("<") + tagName + wxT(">");
    for (; (it != data.end()); ++it)
      rest += *it;
    data = rest;
  }
  if(tagFound)
  {
    retval = true;
    CALL_MEMBER_FN(*this, tagIndex->second)(data);
  }
  return retval;
}

void wxMaxima::ReadMiscText(const wxString &data)
{
  if (data.IsEmpty())
    return;

  if(data == "\r")
    return;

  if(data.StartsWith("\n"))
    m_worksheet->SetCurrentTextCell(nullptr);

  // A version of the text where each line begins with non-whitespace and whitespace
  // characters are merged.
  wxString mergedWhitespace = wxT("\n");
  bool whitespace = true;
  for ( wxString::const_iterator it = data.begin(); it!=data.end(); ++it)
  {
    if((*it == wxT(' ')) || (*it == wxT('\t')))
    {
      // Merge non-newline whitespace to a space.
      if(!whitespace)
        mergedWhitespace += wxT(' ');
    }
    else
      mergedWhitespace += *it;

    if((*it == wxT(' ')) || (*it == wxT('\t')) || (*it == wxT('\n')))
      whitespace = true;
    else
      whitespace = false;
  }

  bool error   = false;
  bool warning = false;
  if (
    (mergedWhitespace.Contains(wxT("\n-- an error."))) ||
    (mergedWhitespace.Contains(wxT(":incorrect syntax:"))) ||
    (mergedWhitespace.Contains(wxT("\nincorrect syntax"))) ||
    (mergedWhitespace.Contains(wxT("\nMaxima encountered a Lisp error"))) ||
    (mergedWhitespace.Contains(wxT("\nkillcontext: no such context"))) ||
    (mergedWhitespace.Contains(wxT("\ndbl:MAXIMA>>"))) ||  // a gcl error message
    (mergedWhitespace.Contains(wxT("\nTo enable the Lisp debugger set *debugger-hook* to nil."))) // a scbl error message
    )
    error = true;

  if ((mergedWhitespace.StartsWith(wxT("Warning:"))) ||
      (mergedWhitespace.StartsWith(wxT("warning:"))) ||
      (mergedWhitespace.StartsWith(wxT("WARNING:"))) ||
      (mergedWhitespace.Contains(wxT("\nWarning:"))) ||
      (mergedWhitespace.Contains(wxT("\nWARNING:"))) ||
      (mergedWhitespace.Contains(wxT("\nwarning:"))) ||
      (mergedWhitespace.Contains(wxT(": Warning:"))) ||
      (mergedWhitespace.Contains(wxT(": warning:")))
    )
    warning = true;
  else
  {
    // Gnuplot errors differ from gnuplot warnings by not containing a "warning:"
    if (m_gnuplotErrorRegex.Matches(mergedWhitespace))
      error = true;
  }

  // Add all text lines to the console
  wxStringTokenizer lines(data, wxT("\n"));
  while (lines.HasMoreTokens())
  {
    // extract a string from the Data lines
    wxString textline = lines.GetNextToken();
    wxString trimmedLine = textline;

    trimmedLine.Trim(true);
    trimmedLine.Trim(false);

    if (!textline.empty() && textline != wxT("\n"))
    {
      if (error)
      {
        m_worksheet->SetCurrentTextCell(ConsoleAppend(textline, MC_TYPE_ERROR));
        AbortOnError();
      }
      else
      {
        if (warning)
          m_worksheet->SetCurrentTextCell(ConsoleAppend(textline, MC_TYPE_WARNING));
        else
        {
          if(m_worksheet->m_configuration->DisplayMode() != Configuration::display_2dASCII)
            m_worksheet->SetCurrentTextCell(ConsoleAppend(textline, MC_TYPE_TEXT));
          else
            m_worksheet->SetCurrentTextCell(ConsoleAppend(textline, MC_TYPE_ASCIIMATHS));
        }
      }
    }
    if (lines.HasMoreTokens())
      m_worksheet->SetCurrentTextCell(nullptr);
  }
  if (data.EndsWith("\n"))
    m_worksheet->SetCurrentTextCell(nullptr);

  if (!data.empty())
    m_worksheet->SetCurrentTextCell(nullptr);
}

int wxMaxima::FindTagEnd(const wxString &data, const wxString &tag)
{
  if((m_currentOutputEnd.IsEmpty()) || (m_currentOutputEnd.Find(tag) != wxNOT_FOUND))
    return data.Find(tag);
  else
    return wxNOT_FOUND;
}

void wxMaxima::ReadStatusBar(wxString &data)
{
  if (!data.StartsWith(m_statusbarPrefix))
    return;

  m_worksheet->SetCurrentTextCell(nullptr);

  int end;
  if ((end = FindTagEnd(data,m_statusbarSuffix)) != wxNOT_FOUND)
  {
    wxXmlDocument xmldoc;
    wxString xml = data.Left( end + m_statusbarSuffix.Length());
    wxStringInputStream xmlStream(xml);
    xmldoc.Load(xmlStream, wxT("UTF-8"));
    wxXmlNode *node = xmldoc.GetRoot();
    if(node != NULL)
    {
      wxXmlNode *contents = node->GetChildren();
      if(contents)
        LeftStatusText(contents->GetContent(), false);
    }
    // Remove the status bar info from the data string
    data = data.Right(data.Length()-end-m_statusbarSuffix.Length());
  }
}

/***
 * Checks if maxima displayed a new chunk of math
 */
void wxMaxima::ReadMath(wxString &data)
{
  if ((!data.StartsWith(m_mathPrefix1)) && (!data.StartsWith(m_mathPrefix2)))
    return;

  m_worksheet->SetCurrentTextCell(nullptr);

  // Append everything from the "beginning of math" to the "end of math" marker
  // to the console and remove it from the data we got.
  int mthTagLen;
  int end = FindTagEnd(data, m_mathSuffix1);
  if(end >= 0)
    mthTagLen = m_mathSuffix1.Length();
  else
  {
    end = FindTagEnd(data, m_mathSuffix2);
    mthTagLen = m_mathSuffix2.Length();
  }
  if(end >= 0)
  {
    wxString o = data.Left(end + mthTagLen);
    data = data.Right(data.Length() - end - mthTagLen);
    o.Trim(true);
    o.Trim(false);
    if (o.Length() > 0)
    {
      if (m_worksheet->m_configuration->UseUserLabels())
      {
        ConsoleAppend(o, MC_TYPE_DEFAULT,m_worksheet->m_evaluationQueue.GetUserLabel());
      }
      else
      {
        ConsoleAppend(o, MC_TYPE_DEFAULT);
      }
    }
  }
}

void wxMaxima::ReadSuppressedOutput(wxString &data)
{
  if (!data.StartsWith(m_suppressOutputPrefix))
    return;
  int end = FindTagEnd(data, m_suppressOutputSuffix);
  if (end != wxNOT_FOUND)
  {
    data = data.Right(data.Length()-end-m_suppressOutputSuffix.Length());
  }
}

void wxMaxima::ReadLoadSymbols(wxString &data)
{
  if (!data.StartsWith(m_symbolsPrefix))
    return;

  m_worksheet->SetCurrentTextCell(nullptr);

  int end = FindTagEnd(data, m_symbolsSuffix);

  if (end != wxNOT_FOUND)
  {
    // Put the symbols into a separate string
    wxString symbols = data.Left( end + m_symbolsSuffix.Length());
    m_worksheet->AddSymbols(symbols);

    // Remove the symbols from the data string
    data = data.Right(data.Length()-end-m_symbolsSuffix.Length());
  }
}

void wxMaxima::ReadVariables(wxString &data)
{
  if (!data.StartsWith(m_variablesPrefix))
    return;

  int end = FindTagEnd(data, m_variablesSuffix);

  if (end != wxNOT_FOUND)
  {
    int num = 0;
    wxXmlDocument xmldoc;
    wxString xml = data.Left( end + m_variablesSuffix.Length());
    wxStringInputStream xmlStream(xml);
    xmldoc.Load(xmlStream, wxT("UTF-8"));
    wxXmlNode *node = xmldoc.GetRoot();
    if(node != NULL)
    {
      wxXmlNode *vars = node->GetChildren();
      while (vars != NULL)
      {
        wxXmlNode *var = vars->GetChildren();

        wxString name;
        wxString value;
        bool bound = false;
        while(var != NULL)
        {
          if(var->GetName() == wxT("name"))
          {
            num++;
            wxXmlNode *namenode = var->GetChildren();
            if(namenode)
              name = namenode->GetContent();
          }
          if(var->GetName() == wxT("value"))
          {
            wxXmlNode *valnode = var->GetChildren();
            if(valnode)
            {
              bound = true;
              value = valnode->GetContent();
            }
          }

          if(bound)
          {
            auto varFunc = m_variableReadActions.find(name);
            if(varFunc != m_variableReadActions.end())
              CALL_MEMBER_FN(*this, varFunc->second)(value);
            m_worksheet->m_variablesPane->VariableValue(name, value);
          }
          else
            m_worksheet->m_variablesPane->VariableUndefined(name);

          var = var->GetNext();
        }
        vars = vars->GetNext();
      }
    }

    if(num>1)
      wxLogMessage(_("Maxima sends a new set of auto-completable symbols."));
    else
      wxLogMessage(_("Maxima has sent a new variable value."));

    // Remove the symbols from the data string
    data = data.Right(data.Length()-end-m_variablesSuffix.Length());
    TriggerEvaluation();
    QueryVariableValue();
  }
}

void wxMaxima::VariableActionUserDir(const wxString &value)
{
  Dirstructure::Get()->UserConfDir(value);
  wxLogMessage(wxString::Format(
                 _("Maxima user configuration lies in directory %s"),value.utf8_str()));
}

void wxMaxima::VariableActionTempDir(const wxString &value)
{
  m_maximaTempDir = value;
  wxLogMessage(wxString::Format(_("Maxima uses temp directory %s"),value.utf8_str()));
  {
    // Sometimes people delete their temp dir
    // and gnuplot won't create a new one for them.
    wxLogNull logNull;
    wxMkDir(value, wxS_DIR_DEFAULT);
  }
}

void wxMaxima::VariableActionAutoconfVersion(const wxString &value)
{
  m_maximaVersion = value;
  wxLogMessage(wxString::Format(_("Maxima version: %s"),value.utf8_str()));
}
void wxMaxima::VariableActionAutoconfHost(const wxString &value)
{
  m_maximaArch = value;
  wxLogMessage(wxString::Format(_("Maxima architecture: %s"),value.utf8_str()));
}
void wxMaxima::VariableActionMaximaInfodir(const wxString &value)
{
              m_maximaDocDir = value;
              wxLogMessage(wxString::Format(_("Maxima's manual lies in directory %s"),value.utf8_str()));
}

void wxMaxima::GnuplotCommandName(wxString gnuplot)
{
  m_gnuplotcommand = gnuplot;
  if(!wxFileName(m_gnuplotcommand).IsAbsolute())
  {
    wxPathList pathlist;

    // Add paths relative to the path of the wxMaxima executable
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath());
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot/bin");
    // Add paths from the PATH environment variable
    pathlist.AddEnvList(wxT("PATH"));

    // Add OSX specific paths
#ifdef __WXOSX__
    // MacPorts:
    // The MacPorts default binary path /opt/local/bin/ is not in the PATH for applications.
    // It is added to .profile, but this is only used by shells.
    // => Add the default MacPorts binary path /opt/local/bin/ to our search path list.
    //
    // Homebrew:
    // Homebrew installs binaries in /usr/local/bin, which is in the PATH by default.
    //
    // Application packages including gnuplot:
    // The above wxMaxima executable relative logic should work
    //
    // If gnuplot is somewhere else (e.g. non default MacPort or Homebrew path), the command
    //   gnuplot_command:"/opt/local/bin/gnuplot"$
    // must be added manually to ~/.maxima/wxmaxima-init.mac
    // This should be documented for the installer packages, e.g. as MacPorts "notes" field.
    pathlist.Add(OSX_MACPORTS_PREFIX "/bin");
#endif

    // Find executable "gnuplot" in our list of paths
    m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot);
    #ifdef __WXMSW__
    // If not successful, Find executable "gnuplot.exe" in our list of paths
    if(m_gnuplotcommand == wxEmptyString)
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot + wxT(".exe"));
    // If not successful, Find executable "gnuplot.bat" in our list of paths
    if(m_gnuplotcommand == wxEmptyString)
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot + wxT(".bat"));
    #endif
    #ifdef __WXOSX__
    if(m_gnuplotcommand == wxEmptyString)
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot + wxT(".app"));
    #endif
    // If not successful, use the original command (better than empty for error messages)
    if(m_gnuplotcommand == wxEmptyString)
    {
      wxLogMessage(_("Gnuplot not found, using the default: ") + gnuplot);
      m_gnuplotcommand = gnuplot;
    }
    else
    {
      wxLogMessage(_("Gnuplot found at: ") + m_gnuplotcommand);
    }    
  }
  if(
    m_gnuplotcommand.Contains(" ") &&
    (!m_gnuplotcommand.StartsWith("\"")) &&
    (!m_gnuplotcommand.StartsWith("\'")))
    m_gnuplotcommand = "\"" + m_gnuplotcommand + "\"";
}

void wxMaxima::VariableActionGnuplotCommand(const wxString &value)
{
  GnuplotCommandName(value);

  wxLogMessage(_("Querying gnuplot which graphics drivers it supports."));
  wxEnvVariableHashMap environment;
  // gnuplot uses the PAGER variable only on un*x - and on un*x there is cat.
  environment["PAGER"] = "cat";
  wxGetEnvMap(&environment);
  
  m_gnuplotTerminalQueryProcess = new wxProcess(this, gnuplot_query_terminals_id);
  m_gnuplotTerminalQueryProcess->Redirect();
  // We don't want error dialogues here.
  SuppressErrorDialogs suppressor;
  wxExecuteEnv *env = new wxExecuteEnv;
  env->env = environment;
  if (wxExecute(m_gnuplotcommand,
                wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE,
                m_gnuplotTerminalQueryProcess, env) < 0)
    wxLogMessage(_("Cannot start gnuplot"));
  else
  {
    wxOutputStream* ostream = m_gnuplotTerminalQueryProcess->GetOutputStream();
    if(ostream != NULL)
    {
      wxTextOutputStream textout(*ostream);
      textout << "print GPVAL_TERMINALS;quit;\n\n\n\n\n\n\n\n\n\n\n:q\n:q\n:q\n:q\n:q\n:q\n:q\n:q\n:q\n:q\n";
//      ostream->Close();
    }
    else
      wxLogMessage("Cannot get gnuplot's output stream!");
  }
}

void wxMaxima::VariableActionMaximaSharedir(const wxString &value)
{
  wxString dir = value;
  dir.Trim(true);
  m_worksheet->m_configuration->MaximaShareDir(dir);
  wxLogMessage(wxString::Format(_("Maxima's share files lie in directory %s"),dir.utf8_str()));
  /// READ FUNCTIONS FOR AUTOCOMPLETION
  m_worksheet->LoadSymbols();
  if(m_worksheet->m_helpFileAnchors.empty())
  {
    if(!LoadManualAnchorsFromCache())
    {
      if(wxFileExists(GetMaximaHelpFile()))
        m_compileHelpAnchorsTimer.StartOnce(4000);
      else
        LoadBuiltInManualAnchors();
    }
  }
}

void wxMaxima::VariableActionLispName(const wxString &value)
{
  m_worksheet->m_configuration->LispType(value);
  wxLogMessage(wxString::Format(_("Maxima was compiled using %s"),value.utf8_str()));
}
void wxMaxima::VariableActionLispVersion(const wxString &value)
{
  m_lispVersion = value;
  wxLogMessage(wxString::Format(_("Lisp version: %s"),value.utf8_str()));
}
void wxMaxima::VariableActionWxLoadFileName(const wxString &value)
{
  m_recentPackages.AddDocument(value);
  wxLogMessage(wxString::Format(_("Maxima has loaded the file %s."),value.utf8_str()));
}

void wxMaxima::VariableActionWxSubscripts(const wxString &value)
{
  if(m_maximaVariable_wxSubscripts != value)
  {
    m_maximaVariable_wxSubscripts = value;
    if(value == wxT("false"))
      m_autoSubscriptMenu->Check(menu_noAutosubscript, true);
    else if(value == wxT("true"))
      m_autoSubscriptMenu->Check(menu_defaultAutosubscript, true);
    else if(value == wxT("all"))
      m_autoSubscriptMenu->Check(menu_alwaysAutosubscript, true);
  }
}
void wxMaxima::VariableActionLmxChar(const wxString &value)
{
  if(m_maximaVariable_lmxchar != value)
  {
    m_maximaVariable_lmxchar = value;
    if(m_maximaVariable_lmxchar.EndsWith("("))
      m_roundedMatrixParensMenu->Check(menu_roundedMatrixParens, true);
    if(m_maximaVariable_lmxchar.EndsWith("<"))
      m_roundedMatrixParensMenu->Check(menu_angledMatrixParens, true);
    if(m_maximaVariable_lmxchar.EndsWith("|"))
      m_roundedMatrixParensMenu->Check(menu_straightMatrixParens, true);
    if(m_maximaVariable_lmxchar.EndsWith("["))
      m_roundedMatrixParensMenu->Check(menu_squareMatrixParens, true);
    if(m_maximaVariable_lmxchar.EndsWith(" "))
      m_roundedMatrixParensMenu->Check(menu_noMatrixParens, true);
  }
}
void wxMaxima::VariableActionNumer(const wxString &value)
{
  if(value == wxT("true"))
  {
    if(!m_NumericMenu->IsChecked(menu_num_out))
      m_NumericMenu->Check(menu_num_out, true);
  }
  else
  {
    if(m_NumericMenu->IsChecked(menu_num_out))
      m_NumericMenu->Check(menu_num_out, false);
  }
}
void wxMaxima::VariableActionAlgebraic(const wxString &value)
{
  if(value == wxT("true"))
  {
    if(!m_SimplifyMenu->IsChecked(menu_talg))
      m_SimplifyMenu->Check(menu_talg, true);
  }
  else
  {
    if(m_SimplifyMenu->IsChecked(menu_talg))
      m_SimplifyMenu->Check(menu_talg, false);
  }
}
void wxMaxima::VariableActionShowtime(const wxString &value)
{
  if(value == wxT("false"))
  {
    if(m_MaximaMenu->IsChecked(menu_time))
      m_MaximaMenu->Check(menu_time, false);
  }
  else
  {
    if(!m_MaximaMenu->IsChecked(menu_time))
      m_MaximaMenu->Check(menu_time, true);
  }
}
void wxMaxima::VariableActionEngineeringFormat(const wxString &value)
{
  m_maximaVariable_engineeringFormat = value;
  if(value == wxT("true"))
  {
    if(!m_NumericMenu->IsChecked(menu_engineeringFormat))
      m_NumericMenu->Check(menu_engineeringFormat, true);
  }
  else
  {
    if(m_NumericMenu->IsChecked(menu_engineeringFormat))
      m_NumericMenu->Check(menu_engineeringFormat, false);
  }
}
void wxMaxima::VariableActionAutoplay(const wxString &value)
{
  if(value == wxT("true"))
  {
    if(!m_PlotMenu->IsChecked(menu_animationautostart))
      m_PlotMenu->Check(menu_animationautostart, true);
  }
  else
  {
    if(m_PlotMenu->IsChecked(menu_animationautostart))
      m_PlotMenu->Check(menu_animationautostart, false);
  }
}
void wxMaxima::VariableActionDomain(const wxString &value)
{
  if(value == wxT("complex"))
  {
    if(!m_NumericMenu->IsChecked(menu_num_domain))
      m_NumericMenu->Check(menu_num_domain, true);
  }
  else
  {
    if(m_NumericMenu->IsChecked(menu_num_domain))
      m_NumericMenu->Check(menu_num_domain, false);
  }
}
void wxMaxima::VariableActionDisplay2D(const wxString &value)
{
  if(m_maximaVariable_display2d != value)
  {
    m_maximaVariable_display2d = value;
    if(m_maximaVariable_display2d == wxT("false"))
    {
      m_worksheet->m_configuration->DisplayMode(Configuration::display_1dASCII);
      m_equationTypeMenuMenu->Check(menu_math_as_1D_ASCII, true);
    }
    else
    {
      if(m_maximaVariable_altdisplay2d == wxT("false"))
      {
        m_worksheet->m_configuration->DisplayMode(Configuration::display_2dASCII);
        m_equationTypeMenuMenu->Check(menu_math_as_2D_ASCII, true);
      }
      else
      {
        m_worksheet->m_configuration->DisplayMode(Configuration::display_2d);
        m_equationTypeMenuMenu->Check(menu_math_as_graphics, true);
      }
    }
  }
}
void wxMaxima::VariableActionAltDisplay2D(const wxString &value)
{
  if(m_maximaVariable_altdisplay2d != value)
  {
    m_maximaVariable_altdisplay2d = value;
    if(m_maximaVariable_display2d == wxT("false"))
    {
      m_worksheet->m_configuration->DisplayMode(Configuration::display_1dASCII);
      m_equationTypeMenuMenu->Check(menu_math_as_1D_ASCII, true);
    }
    else
    {
      if(m_maximaVariable_altdisplay2d == wxT("false"))
      {
        m_worksheet->m_configuration->DisplayMode(Configuration::display_2dASCII);
        m_equationTypeMenuMenu->Check(menu_math_as_2D_ASCII, true);
      }
      else
      {
        m_worksheet->m_configuration->DisplayMode(Configuration::display_2d);
        m_equationTypeMenuMenu->Check(menu_math_as_graphics, true);
      }
    }
  }
}

void wxMaxima::ReadAddVariables(wxString &data)
{
  if (!data.StartsWith(m_addVariablesPrefix))
    return;

  int end = FindTagEnd(data, m_addVariablesSuffix);

  if (end != wxNOT_FOUND)
  {
    wxLogMessage(_("Maxima sends us a new set of variables for the watch list."));
    wxXmlDocument xmldoc;
    wxString xml = data.Left( end + m_addVariablesSuffix.Length());
    wxStringInputStream xmlStream(xml);
    xmldoc.Load(xmlStream, wxT("UTF-8"));
    wxXmlNode *node = xmldoc.GetRoot();
    if(node != NULL)
    {
      wxXmlNode *var = node->GetChildren();
      while (var != NULL)
      {
        wxString name;
        {
          if(var->GetName() == wxT("variable"))
          {
            wxXmlNode *valnode = var->GetChildren();
            if(valnode)
              m_worksheet->m_variablesPane->AddWatch(valnode->GetContent());
          }
        }
        var = var->GetNext();
      }
    }
    data = data.Right(data.Length()-end-m_addVariablesSuffix.Length());
  }
}

bool wxMaxima::QueryVariableValue()
{
  if(!m_worksheet->m_evaluationQueue.Empty())
    return false;

  if(m_maximaBusy)
    return false;

  if(m_worksheet->QuestionPending())
    return false;

  if(m_varNamesToQuery.GetCount() > 0)
  {
    SendMaxima(wxT(":lisp-quiet (wx-query-variable \"") +
               m_varNamesToQuery.Last()+wxT("\")\n"));
    m_varNamesToQuery.RemoveAt(m_varNamesToQuery.GetCount()-1);
    return true;
  }
  else
  {
    if(m_readMaximaVariables)
    {
      SendMaxima(wxT(":lisp-quiet (wx-print-gui-variables)\n"));
      m_readMaximaVariables = false;
    }
    if(!m_worksheet->m_variablesPane->GetEscapedVarnames().IsEmpty())
      m_worksheet->m_variablesPane->UpdateSize();
    
    return false;
  }
}

/***
 * Checks if maxima displayed a new prompt.
 */
void wxMaxima::ReadPrompt(wxString &data)
{
  m_evalOnStartup = false;
  if (!data.StartsWith(m_promptPrefix))
    return;

  m_worksheet->SetCurrentTextCell(nullptr);

  // Assume we don't have a question prompt
  m_worksheet->m_questionPrompt = false;
  m_ready = true;
  int end = FindTagEnd(data,m_promptSuffix);
  // Did we find a prompt?
  if (end == wxNOT_FOUND)
    return;

  m_maximaBusy = false;
  m_bytesFromMaxima = 0;

  wxString label = data.SubString(m_promptPrefix.Length(), end - 1);
  // Remove the prompt we will process from the string.
  data = data.Right(data.Length()-end-m_promptSuffix.Length());
  if(data == wxT(" "))
    data = wxEmptyString;

  // If we got a prompt our connection to maxima was successful.
  if(m_unsuccessfulConnectionAttempts > 0)
    m_unsuccessfulConnectionAttempts--;
  label.Trim(true);
  label.Trim(false);
  // Input prompts have a length > 0 and end in a number followed by a ")".
  // Depending on ibase the digits of the number might lie between 'A' and 'Z',
  // too. Input prompts also begin with a "(". Questions (hopefully)
  // don't do that; Lisp prompts look like question prompts.
  if (
    (
      (label.Length() > 2) &&
      label.StartsWith("(%") &&
      label.EndsWith(")") &&
      (((label[label.Length()-2] >= (wxT('0'))) &&
        (label[label.Length()-2] <= (wxT('9')))) ||
       ((label[label.Length()-2] >= (wxT('A'))) &&
        (label[label.Length()-2] <= (wxT('Z'))))
        )
      ) ||
      m_worksheet->m_configuration->InLispMode() ||
    (label.StartsWith(wxT("MAXIMA>"))) ||
    (label.StartsWith(wxT("\nMAXIMA>")))
    )
  {
    // Maxima displayed a new main prompt => We don't have a question
    m_worksheet->QuestionAnswered();
    // And we can remove one command from the evaluation queue.
    m_worksheet->m_evaluationQueue.RemoveFirst();

    m_lastPrompt = label;
    // remove the event maxima has just processed from the evaluation queue
    // if we remove a command from the evaluation queue the next output line will be the
    // first from the next command.
    m_outputCellsFromCurrentCommand = 0;
    if (m_worksheet->m_evaluationQueue.Empty())
    { // queue empty.
      m_exitOnError = false;
      StatusMaximaBusy(waiting);
      // If we have selected a cell in order to show we are evaluating it
      // we should now remove this marker.
      if (m_worksheet->FollowEvaluation())
      {
        if (m_worksheet->GetActiveCell())
          m_worksheet->GetActiveCell()->SelectNone();
        m_worksheet->ClearSelection();
      }
      m_worksheet->FollowEvaluation(false);
      if (m_exitAfterEval)
      {
        SaveFile(false);
        Close();
      }
      // Inform the user that the evaluation queue is empty.
      EvaluationQueueLength(0);
      m_worksheet->SetWorkingGroup(nullptr);
      m_worksheet->m_evaluationQueue.RemoveFirst();
      m_worksheet->RequestRedraw();
      // Now that maxima is idle we can ask for the contents of its variables
      QueryVariableValue();
    }
    else
    { // we don't have an empty queue
      m_ready = false;
      m_worksheet->RequestRedraw();
      m_worksheet->SetWorkingGroup(nullptr);
      StatusMaximaBusy(sending);
      TriggerEvaluation();
    }

    if (m_worksheet->m_evaluationQueue.Empty())
    {
      if ((m_worksheet->m_configuration->GetOpenHCaret()) && (m_worksheet->GetActiveCell() == NULL))
        m_worksheet->OpenNextOrCreateCell();
    }

    if (m_exitAfterEval && m_worksheet->m_evaluationQueue.Empty())
      Close();
  }
  else
  {  // We have a question
    m_worksheet->SetLastQuestion(label);
    m_worksheet->QuestionAnswered();
    m_worksheet->QuestionPending(true);
    // If the user answers a question additional output might be required even
    // if the question has been preceded by many lines.
    m_outputCellsFromCurrentCommand = 0;
    if((m_worksheet->GetWorkingGroup() == NULL) ||
       ((m_worksheet->GetWorkingGroup()->m_knownAnswers.empty()) &&
        m_worksheet->GetWorkingGroup()->AutoAnswer()))
       m_worksheet->SetNotification(_("Maxima asks a question!"), wxICON_INFORMATION);
    if (!label.IsEmpty())
    {
      int options = AppendOpt::NewLine | AppendOpt::BigSkip;
      if ((!m_worksheet->GetWorkingGroup()) || (!m_worksheet->GetWorkingGroup()->AutoAnswer()))
        options |= AppendOpt::PromptToolTip;

      if (wxMax(label.Find(m_mathPrefix1), label.Find(m_mathPrefix2)) >= 0)
        DoConsoleAppend(label, MC_TYPE_PROMPT, AppendOpt(options));
      else
        DoRawConsoleAppend(label, MC_TYPE_PROMPT, AppendOpt(options));
    }
    if (m_worksheet->ScrolledAwayFromEvaluation())
    {
      if (m_worksheet->m_mainToolBar)
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    }
    else
    {
      m_worksheet->OpenQuestionCaret();
    }
    StatusMaximaBusy(userinput);
  }
  label.Trim(false);
  if (label.StartsWith(wxT("MAXIMA>")))
  {
    if(!m_worksheet->m_configuration->InLispMode())
      wxLogMessage(_("Switched to lisp mode after receiving a lisp prompt!"));
    m_worksheet->m_configuration->InLispMode(true);
  }
  else
  {
    if(  m_worksheet->m_configuration->InLispMode())
      wxLogMessage(_("Ended lisp mode after receiving a maxima prompt!"));
    m_worksheet->m_configuration->InLispMode(false);
  }
}

void wxMaxima::SetCWD(wxString file)
{
  // If maxima isn't connected we cannot do anything
  if (!m_client || (!m_client->IsConnected()))
    return;

  // Tell the math parser where to search for local files.
  m_worksheet->m_configuration->SetWorkingDirectory(wxFileName(file).GetPath());

#if defined __WXMSW__
  file.Replace(wxT("\\"), wxT("/"));
#endif

  wxFileName filename(file);

  if (filename.GetPath().IsEmpty())
    filename.AssignDir(wxGetCwd());

  // Escape all backslashes in the filename if needed by the OS.
  wxString filenamestring = filename.GetFullPath();
  wxString dirname = filename.GetPath();

#if defined (__WXMSW__)
  // On MSW filenames with a "\" are widely used - but only partially supported.
  filenamestring.Replace(wxT("\\"), wxT("/"));
  dirname.Replace(wxT("\\"), wxT("/"));
#endif

  wxString workingDirectory = filename.GetPath();

  if (workingDirectory != GetCWD())
  {
    wxLogMessage(_("Telling maxima about the new working directory."));
    m_configCommands += wxT(":lisp-quiet (setf $wxfilename \"") +
      filenamestring +
      wxT("\")\n");
    m_configCommands += wxT(":lisp-quiet (setf $wxdirname \"") +
      dirname +
      wxT("\")\n");

    m_configCommands += wxT(":lisp-quiet (wx-cd \"") + filenamestring + wxT("\")\n");
    if (m_ready)
    {
      if (m_worksheet->m_evaluationQueue.Empty())
        StatusMaximaBusy(waiting);
    }
    m_CWD = workingDirectory;
  }
}

bool wxMaxima::OpenMACFile(const wxString &file, Worksheet *document, bool clearDocument)
{
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));
  wxWindowUpdateLocker noUpdates(document);

  bool xMaximaFile = file.Lower().EndsWith(wxT(".out"));

  // open mac file
  wxTextFile inputFile(file);

  if (!inputFile.Open())
  {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  if (clearDocument)
    document->ClearDocument();

  auto tree = Format::ParseMACFile(inputFile, xMaximaFile, &document->m_configuration);

  document->InsertGroupCells(std::move(tree), nullptr);

  if (clearDocument)
  {
    StartMaxima();
    m_worksheet->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  }
  else
  {
    ResetTitle(false);
    m_worksheet->UpdateTableOfContents();
  }

  document->RequestRedraw();

  m_worksheet->SetDefaultHCaret();
  m_worksheet->SetFocus();

  SetCWD(file);

  StatusMaximaBusy(waiting);

  m_worksheet->SetHCaret(NULL);
  m_worksheet->ScrollToCaret();
  return true;
}

// OpenWXMFile
// Clear document (if clearDocument == true), then insert file
bool wxMaxima::OpenWXMFile(const wxString &file, Worksheet *document, bool clearDocument)
{
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));
  wxWindowUpdateLocker noUpdates(document);

  // open wxm file
  wxTextFile inputFile(file);

  if (!inputFile.Open())
  {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  if (inputFile.GetFirstLine() != Format::WXMFirstLine)
  {
    inputFile.Close();
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    return false;
  }

  auto tree = Format::ParseWXMFile(inputFile, &m_worksheet->m_configuration);
  inputFile.Close();

  // from here on code is identical for wxm and wxmx
  if (clearDocument)
  {
    document->ClearDocument();
    StartMaxima();
  }

  document->InsertGroupCells(std::move(tree)); // this also requests a recalculate

  if (clearDocument)
  {
    m_worksheet->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  }
  else
    ResetTitle(false);

  document->RequestRedraw();

  m_worksheet->SetDefaultHCaret();
  m_worksheet->SetFocus();

  SetCWD(file);

  StatusMaximaBusy(waiting);

  m_worksheet->SetHCaret(NULL);
  m_worksheet->ScrollToCaret();
  return true;
}

wxString wxMaxima::ReadPotentiallyUnclosedTag(wxStringTokenizer &lines, wxString firstLine)
{
  wxString result = firstLine + wxT("\n");
  wxString closingTag = firstLine;
  m_xmlOpeningTagName.Replace(&closingTag, wxT("</\\1>"));
  
  while(lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    if(
      (line.Contains(wxT("<line"))) ||
      (line.Contains(wxT("</line"))) ||
      (!line.Contains(wxT("<"))))
    {
      // TODO: Handle broken line tags and line tags that are split into lines.
      result += line + wxT("\n");
    }
    else
    {
      if(line.Contains(wxT("</")))
        break;
      else
      {
        if(line.Contains(wxT("<")))
          result += ReadPotentiallyUnclosedTag(lines, line);
        else
          result += line + wxT("\n"); 
      }
    }
  }
  if(!m_xmlOpeningTag.Matches(firstLine))
    return wxEmptyString;
  else
  {
    result += closingTag + wxT("\n");
    return result;
  }
}

wxRegEx wxMaxima::m_xmlOpeningTagName(wxT(".*<([a-zA-Z0-9_]*)[ >].*"));
wxRegEx wxMaxima::m_xmlOpeningTag(wxT("<[^/].*>"));

bool wxMaxima::OpenWXMXFile(const wxString &file, Worksheet *document, bool clearDocument)
{
  wxLogMessage(_("Opening a wxmx file"));
  // Show a busy cursor while we open a file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));

  wxWindowUpdateLocker noUpdates(document);

  // If the file is empty we don't want to generate an error, but just
  // open an empty file.
  //
  // This makes the following thing work on windows without the need of an
  // empty template file:
  //
  // - Create a registry key named HKEY_LOKAL_MACHINE\SOFTWARE\CLASSES\.wxmx\ShellNew
  // - Create a string named "NullFile" within this key
  //
  // => After the next reboot the right-click context menu's "new" submenu contains
  //    an entry that creates valid empty .wxmx files.
  if (wxFile(file, wxFile::read).Eof())
  {
    document->ClearDocument();
    StartMaxima();

    m_worksheet->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
    return true;
  }

  // open wxmx file
  wxXmlDocument xmldoc;

  wxFileSystem fs;
  
  wxString wxmxURI = wxURI(wxT("file://") + file).BuildURI();
  // wxURI doesn't know that a "#" in a file name is a literal "#" and
  // not an anchor within the file so we have to care about url-encoding
  // this char by hand.
  wxmxURI.Replace("#", "%23");

#ifdef  __WXMSW__
  // Fixes a missing "///" after the "file:". This works because we always get absolute
  // file names.
  wxRegEx uriCorector1("^file:([a-zA-Z]):");
  wxRegEx uriCorector2("^file:([a-zA-Z][a-zA-Z]):");

  uriCorector1.ReplaceFirst(&wxmxURI, wxT("file:///\\1:"));
  uriCorector2.ReplaceFirst(&wxmxURI, wxT("file:///\\1:"));
#endif
  // The URI of the wxm code contained within the .wxmx file
  wxString filename = wxmxURI + wxT("#zip:content.xml");

  // Open the file
  std::shared_ptr<wxFSFile> fsfile;
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp critical (OpenFSFile)
  #endif
  fsfile.reset(fs.OpenFile(filename));
  if (fsfile)
  {
    xmldoc.Load(*(fsfile->GetStream()), wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
  }
  if(!xmldoc.IsOk())
  {
    // If we cannot read the file a typical error in old wxMaxima versions was to include
    // a letter of ascii code 27 in content.xml. Let's filter this char out.
    
    // Re-open the file.
    std::shared_ptr<wxFSFile> fsfile2;
    #ifdef HAVE_OPENMP_TASKS
    #pragma omp critical (OpenFSFile)
    #endif
    fsfile2.reset(fs.OpenFile(filename));
    wxString contents;
    if (fsfile2)
    {
      // Read the file into a string
      wxTextInputStream istream1(*fsfile2->GetStream(), wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      while (!fsfile2->GetStream()->Eof())
        contents += istream1.ReadLine() + wxT("\n");
    }
    else
    {
      wxLogMessage(_("Trying to recover a broken .wxmx file."));
      // Let's try to recover the uncompressed text from a truncated .zip file
      wxFileInputStream input(file);
      if(input.IsOk())
      {
        wxLogMessage(_("Trying to extract contents.xml out of a broken .zip file."));
        wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
        while(input.IsOk() && !input.Eof())
        {
          contents = text.ReadLine();
          if(contents.StartsWith(wxT("<wxMaximaDocument")))
          {
            contents+= wxT("\n");
            break;
          }
        }
        while(input.IsOk() && !input.Eof())
        {
          wxString line = text.ReadLine();
          if((!input.Eof()) || (line != wxEmptyString))
          {
            if(line.StartsWith(wxT("</wxMaximaDocument>")))
            {
              contents += wxT("</wxMaximaDocument>\n");
              break;
            }
            else
              contents += line + wxT("\n");
          }
        }
      }    
    }
    // Remove the illegal character
    contents.Replace(wxT('\u001b'), wxT("\u238B"));
    
    {
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(contents);
      wxMemoryInputStream istream(ostream);
      
      // Try to load the file from the memory buffer.
      xmldoc.Load(istream, wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
    }

    // If the xml document still cannot be loaded let's extract only the input cells.
    if (!xmldoc.IsOk())
    {
      wxLogMessage(_("Trying to discard all output."));
      contents.Replace(wxT("><"), wxT(">\n<"));
      wxStringTokenizer lines(contents, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_inputOnly;
      while(lines.HasMoreTokens())
      {
        wxString line = lines.GetNextToken();
        if(line.Contains(wxT("<output")))
        {
          while(lines.HasMoreTokens() && (!line.Contains(wxT("</output>"))))
            line = lines.GetNextToken();
        }
        else
        {
          contents_inputOnly += line + wxT("\n");
        }
      }
      contents = contents_inputOnly;
      
      {
        // Write the string into a memory buffer
        wxMemoryOutputStream ostream;
        wxTextOutputStream txtstrm(ostream);
        txtstrm.WriteString(contents);
        wxMemoryInputStream istream(ostream);
        
        // Try to load the file from the memory buffer.
        xmldoc.Load(istream, wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
      }
    }

    // If even that cannot be loaded let's try to reconstruct the closing markers.
    if (!xmldoc.IsOk())
    {
      wxLogMessage(_("Trying to reconstruct the xml closing markers."));
      wxStringTokenizer lines(contents, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_reconstructed;
      wxString line;
      while(lines.HasMoreTokens() && (!line.Contains(wxT("<wxMaximaDocument"))))
      {        
        line = lines.GetNextToken();
        contents_reconstructed += line;
      }
      while(lines.HasMoreTokens())
      {
        line = lines.GetNextToken();
        if(line.Contains(wxT("<cell")))
          contents_reconstructed += 
            ReadPotentiallyUnclosedTag(lines, line);
      }
      contents_reconstructed += wxT("</wxMaximaDocument>\n");
      contents = contents_reconstructed;
      {
        // Write the string into a memory buffer
        wxMemoryOutputStream ostream;
        wxTextOutputStream txtstrm(ostream);
        txtstrm.WriteString(contents);
        wxMemoryInputStream istream(ostream);
        
        // Try to load the file from the memory buffer.
        xmldoc.Load(istream, wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
      }
    }
}
  if (!xmldoc.IsOk())
  {
    LoggingMessageBox(_("wxMaxima cannot read the xml contents of ") + file, _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  // start processing the XML file
  if (xmldoc.GetRoot()->GetName() != wxT("wxMaximaDocument"))
  {
    LoggingMessageBox(_("xml contained in the file claims not to be a wxMaxima worksheet. ") + file, _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion = xmldoc.GetRoot()->GetAttribute(wxT("version"), wxT("1.0"));
  if (!CheckWXMXVersion(docversion))
  {
    StatusMaximaBusy(waiting);
    return false;
  }

  // Determine where the cursor was before saving
  wxString ActiveCellNumber_String = xmldoc.GetRoot()->GetAttribute(wxT("activecell"), wxT("-1"));
  long ActiveCellNumber;
  if (!ActiveCellNumber_String.ToLong(&ActiveCellNumber))
    ActiveCellNumber = -1;

  wxString VariablesNumberString = xmldoc.GetRoot()->GetAttribute(wxT("variables_num"), wxT("0"));
  long VariablesNumber;
  if (!VariablesNumberString.ToLong(&VariablesNumber))
    VariablesNumber = 0;
  if(VariablesNumber > 0)
  {
    m_worksheet->m_variablesPane->Clear();

    for(long i=0; i<VariablesNumber; i++)
    {
      wxString variable = xmldoc.GetRoot()->GetAttribute(
        wxString::Format("variables_%li", i));
      m_worksheet->m_variablesPane->AddWatch(variable);
    }
  }

  // read the zoom factor
  wxString doczoom = xmldoc.GetRoot()->GetAttribute(wxT("zoom"), wxT("100"));

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = CreateTreeFromXMLNode(xmlcells, wxmxURI);

  // from here on code is identical for wxm and wxmx
  if (clearDocument)
  {
    document->ClearDocument();
    StartMaxima();
    long int zoom = 100;
    if (!(doczoom.ToLong(&zoom)))
      zoom = 100;
    document->SetZoomFactor(double(zoom) / 100.0, false); // Set zoom if opening, don't recalculate
  }

  document->InsertGroupCells(std::move(tree)); // this also requests a recalculate
  if (clearDocument)
  {
    m_worksheet->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  }
  else
    ResetTitle(false);

  m_worksheet->SetDefaultHCaret();
  m_worksheet->SetFocus();

  SetCWD(file);

  // We can set the cursor to the last known position.
  if (ActiveCellNumber == 0)
    m_worksheet->SetHCaret(NULL);
  if (ActiveCellNumber > 0)
  {
    GroupCell *pos = m_worksheet->GetTree();

    for (long i = 1; i < ActiveCellNumber; i++)
      if (pos)
        pos = pos->GetNext();

    if (pos)
      m_worksheet->SetHCaret(pos);
  }
  StatusMaximaBusy(waiting);

  return true;
}

bool wxMaxima::CheckWXMXVersion(const wxString &docversion)
{
  double version = 1.0;
  if (docversion.ToDouble(&version))
  {
    int version_major = int(version);
    int version_minor = int(10 * (version - double(version_major)));

    if (version_major > DOCUMENT_VERSION_MAJOR)
    {
      LoggingMessageBox(_("Document was saved using a newer version of wxMaxima. Please update your wxMaxima."),
                   _("Error"), wxOK | wxICON_EXCLAMATION);
      RightStatusText(_("File could not be opened"));
      return false;
    }
    if (version_minor > DOCUMENT_VERSION_MINOR)
      LoggingMessageBox(
              _("Document was saved using a newer version of wxMaxima so it may not load correctly. Please update your wxMaxima."),
              _("Warning"), wxOK | wxICON_EXCLAMATION);
  }
  return true;
}

bool wxMaxima::OpenXML(const wxString &file, Worksheet *document)
{
  // Show a busy cursor as long as we open a file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));

  wxWindowUpdateLocker noUpdates(document);

  wxXmlDocument xmldoc;

  // Let's see if we can load the XML contained in this file.
  xmldoc.Load(file);

  if (!xmldoc.IsOk())
  {
    LoggingMessageBox(
            _("The .xml file doesn't seem to be valid xml or isn't a content.xml extracted from a .wxmx zip archive"),
            _("Error"),
            wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  // Process the XML document
  if (xmldoc.GetRoot()->GetName() != wxT("wxMaximaDocument"))
  {
    LoggingMessageBox(_("xml contained in the file claims not to be a wxMaxima worksheet. ") + file, _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion = xmldoc.GetRoot()->GetAttribute(wxT("version"), wxT("1.0"));
  if (!CheckWXMXVersion(docversion))
  {
    StatusMaximaBusy(waiting);
    return false;
  }

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = CreateTreeFromXMLNode(xmlcells, file);

  document->ClearDocument();
  StartMaxima();
  document->InsertGroupCells(std::move(tree)); // this also requests a recalculate
  m_worksheet->m_currentFile = file;
  ResetTitle(true, true);
  document->RequestRedraw();
  m_worksheet->SetDefaultHCaret();
  m_worksheet->SetFocus();
  SetCWD(file);

  StatusMaximaBusy(waiting);
  return true;
}

std::unique_ptr<GroupCell> wxMaxima::CreateTreeFromXMLNode(wxXmlNode *xmlcells, const wxString &wxmxfilename)
{
  // Show a busy cursor as long as we export a .gif file (which might be a lengthy
  // action).
  wxBusyCursor crs;

  MathParser mp(&m_worksheet->m_configuration, wxmxfilename);
  CellListBuilder<GroupCell> tree;

  bool warning = true;

  if (xmlcells)
    xmlcells = xmlcells->GetChildren();

  for (; xmlcells; xmlcells = xmlcells->GetNext())
  {
    if (xmlcells->GetType() != wxXML_TEXT_NODE)
    {
      bool ok = tree.DynamicAppend(mp.ParseTag(xmlcells, false));
      if (!ok && warning)
      {
        LoggingMessageBox(_("Parts of the document will not be loaded correctly!"), _("Warning"),
                     wxOK | wxICON_WARNING);
        warning = false;
      }
    }
  }
  return std::move(tree);
}

wxString wxMaxima::EscapeForLisp(wxString str)
{
  str.Replace(wxT("\\"), wxT("\\\\"));
  str.Replace(wxT("\""), wxT("\\\""));
  return(str);
}

void wxMaxima::SetupVariables()
{
  wxLogMessage(_("Setting a few prerequisites for wxMaxima"));
  SendMaxima(wxT(":lisp-quiet (progn (setf *prompt-suffix* \"") +
             m_promptSuffix +
             wxT("\") (setf *prompt-prefix* \"") +
             m_promptPrefix +
             wxT("\") (setf $in_netmath nil) (setf $show_openplot t))\n"));

  wxLogMessage(_("Sending maxima the info how to express 2d maths as XML"));
  wxMathML wxmathml;
  SendMaxima(wxmathml.GetCmd());
  wxString cmd;

#if defined (__WXOSX__)
  wxString gnuplot_binary = m_gnuplotcommand;

  gnuplot_binary.Replace("\\","\\\\");
  gnuplot_binary.Replace("\"","\\\"");
  if (wxFileExists(m_gnuplotcommand))
    cmd += wxT("\n:lisp-quiet (setf $gnuplot_command \"") + m_gnuplotcommand + wxT("\")\n");
  wxLogMessage(wxString::Format(_("Setting gnuplot_binary to %s"), m_gnuplotcommand.utf8_str()));
#endif
  cmd.Replace(wxT("\\"), wxT("/"));
  SendMaxima(cmd);

  wxString wxmaximaversion_lisp(wxT(GITVERSION));

  #ifdef __WXMSW__
  wxmaximaversion_lisp += "_MSW";
  #endif
  #ifdef __WXMOTIF__
  wxmaximaversion_lisp += "_MOTIF";
  #endif
  #ifdef __WXDFB__
  wxmaximaversion_lisp += "_DIRECTFB";
  #endif
  #ifdef __WXUNIVERSAL__
  wxmaximaversion_lisp += "_WXUNIVERSAL";
  #endif
  #ifdef __WXOSX__
  wxmaximaversion_lisp += "_MAC";
  #endif
    
  #ifdef __WXGTK__
  #ifdef __WXGTK3__
  wxmaximaversion_lisp += "_GTK3";
  #else
  #ifdef __WXGTK2__
  wxmaximaversion_lisp += "_GTK2";
  #else
  wxmaximaversion_lisp += "_GTKX";
  #endif
  #endif
  #endif
  
  #if HAVE_OPENMP_TASKS
  wxmaximaversion_lisp += "_OpenMP";
  #ifdef OPENMP_SPEC_DATE
  wxmaximaversion_lisp += + OPENMP_SPEC_DATE;
  #ifdef HAVE_OMP_HEADER
  wxmaximaversion_lisp += + "+Locks";
  #endif
  #endif
  #endif
  
  wxmaximaversion_lisp.Replace("\\","\\\\");
  wxmaximaversion_lisp.Replace("\"","\\\"");
  wxLogMessage(_("Updating maxima's configuration"));
  SendMaxima(wxString(wxT(":lisp-quiet (progn (setq $wxmaximaversion \"")) +
             wxString(wxmaximaversion_lisp) +
             wxT("\") ($put \'$wxmaxima (read-wxmaxima-version \"" +
             wxString(wxmaximaversion_lisp) +
             wxT("\") '$version) (setq $wxwidgetsversion \"")) + wxString(wxVERSION_STRING) +
             wxT("\")   (if (boundp $maxima_frontend_version) (setq $maxima_frontend_version \"" +
                 wxmaximaversion_lisp + "\")) (ignore-errors (setf (symbol-value '*lisp-quiet-suppressed-prompt*) \"" + m_promptPrefix + "(%i1)" + m_promptSuffix + "\")))\n")
    );

  ConfigChanged();
}

///--------------------------------------------------------------------------------
///  Getting configuration
///--------------------------------------------------------------------------------

wxString wxMaxima::GetCommand(bool params)
{
  Configuration *configuration = m_worksheet->m_configuration;
  wxString command = configuration->MaximaLocation();

#if defined (__WXOSX__)
  if (command.EndsWith(wxT(".app"))) // if pointing to a Maxima.app
    command.Append(wxT("/Contents/Resources/maxima.sh"));
#endif
   // if 'maxima' is not searched in the path, check, if the file exists.
  if (command.Cmp("maxima")!=0) {
    if (!wxFileExists(command)) {
      LoggingMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."),
                   _("Warning"),
                   wxOK | wxICON_EXCLAMATION);
      LeftStatusText(_("Please configure wxMaxima with 'Edit->Configure'."));
    }
  }
  if (params) {
    // escape quotes
    command.Replace(wxT("\""), wxT("\\\""));
    // surround with quotes
    command = wxT("\"") + command + wxT("\" ") + m_worksheet->m_configuration->MaximaParameters();
  }
  else
  {
    command = wxT("\"") + command + wxT("\"");
  }
  if(!m_extraMaximaArgs.IsEmpty())
    wxASSERT(m_extraMaximaArgs.StartsWith(" "));
  command += m_extraMaximaArgs;
  return command;
  }

///--------------------------------------------------------------------------------
///  Tips and help
///--------------------------------------------------------------------------------

void wxMaxima::ShowTip(bool force)
{
  bool ShowTips = true;

  // A block with a local config variable:
  // The config can change between before showing the tooltip and afterwards.
  {
    wxConfigBase *config = wxConfig::Get();
    config->Read(wxT("ShowTips"), &ShowTips);
    if ((!ShowTips && !force) || m_evalOnStartup )
      return;
  }

  TipOfTheDay *tip = new TipOfTheDay(this);
  tip->Show();
}

wxString wxMaxima::GetMaximaHelpFile()
{
  // Some operating systems don't like "//" or similar in paths.
  wxFileName helpFile(GetMaximaHelpFile2());
  if(!helpFile.GetFullPath().IsEmpty())
    helpFile.MakeAbsolute();
  return helpFile.GetFullPath();
}

wxString wxMaxima::GetMaximaHelpFile2()
{
  wxString searchText = _("Searching for maxima help file %s");

  wxString headerFile;
  wxConfig::Get()->Read(wxT("helpFile"), &headerFile);

#if defined (__WXMSW__)
  // Cygwin uses /c/something instead of c:/something and passes this path to the
  // web browser - which doesn't support cygwin paths => convert the path to a
  // native windows pathname if needed.
  if(headerFile.Length()>1 && headerFile[1]==wxT('/'))
  {
    headerFile[1]=headerFile[2];
    headerFile[2]=wxT(':');
  }
#endif

  if (headerFile.Length() && wxFileExists(headerFile))
    return headerFile;
  else
    headerFile = m_maximaDocDir + wxT("/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/../html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/../doc/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/doc/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile.utf8_str()));
  if(wxFileExists(headerFile))
    return headerFile;
  
  return wxEmptyString;
}

wxString wxMaxima::SearchwxMaximaHelp()
{
  if(!m_wxMaximaHelpFile.IsEmpty())
    return m_wxMaximaHelpFile;

  wxString failmsg = _("No helpfile found at %s.");
  wxString helpfile;
  wxString lang_long = m_locale->GetCanonicalName();
  wxString lang_short = lang_long.Left(lang_long.Find('_'));
  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima_") + lang_long + ".html";
#if defined (__WXMSW__)
  // Cygwin uses /c/something instead of c:/something and passes this path to the
  // web browser - which doesn't support cygwin paths => convert the path to a
  // native windows pathname if needed.
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(wxFileExists(helpfile))
    return (m_wxMaximaHelpFile = helpfile);
  wxLogMessage(wxString::Format(failmsg, helpfile.utf8_str()));
    
  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima_") + lang_short + ".html";
#if defined (__WXMSW__)
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(wxFileExists(helpfile))
    return (m_wxMaximaHelpFile = helpfile);
  wxLogMessage(wxString::Format(failmsg, helpfile.utf8_str()));

  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.") + lang_long + ".html";
#if defined (__WXMSW__)
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(wxFileExists(helpfile))
    return (m_wxMaximaHelpFile = helpfile);
  wxLogMessage(wxString::Format(failmsg, helpfile.utf8_str()));
    
  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.") + lang_short + ".html";
#if defined (__WXMSW__)
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(wxFileExists(helpfile))
    return (m_wxMaximaHelpFile = helpfile);
  wxLogMessage(wxString::Format(failmsg, helpfile.utf8_str()));
  
  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.html");
#if defined (__WXMSW__)
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(!wxFileExists(helpfile))
    wxLogMessage(wxString::Format(failmsg, helpfile.utf8_str()));
  return helpfile;
}

void wxMaxima::LaunchHelpBrowser(wxString uri)
{
  if(m_worksheet->m_configuration->AutodetectHelpBrowser())
  {
    bool helpBrowserLaunched;
    {
      SuppressErrorDialogs suppressor;
      helpBrowserLaunched = wxLaunchDefaultBrowser(uri);
    }
    if(!helpBrowserLaunched)
    {
      // see https://docs.wxwidgets.org/3.0/classwx_mime_types_manager.html
      auto *manager = wxTheMimeTypesManager;
      wxFileType *filetype = manager->GetFileTypeFromExtension("html");
      wxString command = filetype->GetOpenCommand(uri);
      wxLogMessage(wxString::Format(_("Launching the system's default help browser failed. Trying to execute %s instead."), command.utf8_str()));
      wxExecute(command);
    }
  }
  else
  {
    wxString command;
    command = m_worksheet->m_configuration->HelpBrowserUserLocation() + wxT(" ") + uri;
    wxExecute(command);
  }
}

void wxMaxima::ShowWxMaximaHelp()
{
  wxString helpfile = SearchwxMaximaHelp();
#ifdef __WINDOWS__
  // Replace \ with / in the path as directory separator.
  helpfile.Replace("\\", "/", true);
#endif

  if(!helpfile.IsEmpty())
  {
    // A Unix absolute path starts with a "/", so a valid file URI
    // file:///path/to/helpfile (3 slashes) is constructed.
    // On Windows the path starts e.g. with C:/path/to/helpfile
    // so a third "/" must be inserted.
    // Otherwise "C" might be considered as hostname.
    wxString URI = wxString("file://")+
#ifdef __WINDOWS__
                   wxString("/") +
#endif
                   helpfile;
    wxLogMessage("wxMaxima help file URI: " + URI);
    // On gnome 3.35.91 wxLaunchDefaultBrowser outputs an error message to stdout
    // (No application is registered as handling this file) and returns true.
    // Let's work around this by finding the default browser the Hard Way.
    // if(!wxLaunchDefaultBrowser(URI))
    LaunchHelpBrowser(URI);
  }
  else
  {
    wxLogMessage(_(wxT("No offline manual found ⇒ Redirecting to the wxMaxima homepage")));
    LaunchHelpBrowser("https://htmlpreview.github.io/?https://github.com/wxMaxima-developers/wxmaxima/blob/master/info/wxmaxima.html");
  }
}

void wxMaxima::ShowHelp(const wxString &keyword)
{
  if((keyword.IsEmpty()) || (keyword == "%"))
    ShowWxMaximaHelp();
  else
    ShowMaximaHelp(keyword);
}

void wxMaxima::CompileHelpFileAnchors()
{
  SuppressErrorDialogs suppressor;
  
  wxString MaximaHelpFile = GetMaximaHelpFile();

  #ifdef HAVE_OMP_HEADER
  omp_set_lock(&m_helpFileAnchorsLock);
  #else
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif  
  #endif

  if(m_worksheet->m_helpFileAnchors.empty() && (!(MaximaHelpFile.IsEmpty())))
  {
    m_worksheet->m_helpFileAnchors["wxbarsplot"] = "barsplot";
    m_worksheet->m_helpFileAnchors["wxboxplot"] = "boxplot";
    m_worksheet->m_helpFileAnchors["wxhistogram"] = "histogram";
    m_worksheet->m_helpFileAnchors["wxpiechart"] = "piechart";
    m_worksheet->m_helpFileAnchors["wxscatterplot"] = "scatterplot";
    m_worksheet->m_helpFileAnchors["wxstarplot"] = "starplot";
    m_worksheet->m_helpFileAnchors["wxdrawdf"] = "drawdf";
    m_worksheet->m_helpFileAnchors["wxdraw"] = "draw";
    m_worksheet->m_helpFileAnchors["wxdraw2d"] = "draw2d";
    m_worksheet->m_helpFileAnchors["wxdraw3d"] = "draw3d";
    m_worksheet->m_helpFileAnchors["with_slider_draw"] = "draw";
    m_worksheet->m_helpFileAnchors["with_slider_draw2d"] = "draw2d";
    m_worksheet->m_helpFileAnchors["with_slider_draw3d"] = "draw3d";
    m_worksheet->m_helpFileAnchorsUsable = true;

    int foundAnchors = 0;
    wxLogMessage(_("Compiling the list of anchors the maxima manual provides"));
    wxRegEx idExtractor(".*<span id=\\\"([a-zAZ0-9_-]*)\\\"");
    wxRegEx idExtractor_oldManual(".*<a name=\\\"([a-zAZ0-9_-]*)\\\"");
    wxString escapeChars = "<=>[]`%?;\\$%&+-*/.!\'@#:^_";
    if(wxFileExists(MaximaHelpFile))
    {
      wxFileInputStream input(MaximaHelpFile);
      if(input.IsOk())
      {
        wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
        while(input.IsOk() && !input.Eof())
        {
          wxString line = text.ReadLine();
          wxStringTokenizer tokens(line, wxT(">"));
          while(tokens.HasMoreTokens())
          {
            wxString token = tokens.GetNextToken();
            wxString oldToken(token);
            wxString id;
            if(idExtractor.Replace(&token, "\\1")>0)
              id = token;
            else
            {
              if(idExtractor_oldManual.Replace(&token, "\\1")>0)
                id = token;
            }
            if(!id.IsEmpty())
            {
              // In anchors a space is represented by a hyphen
              token.Replace("-", " ");
              // Some other chars including the minus are represented by "_00xx"
              // where xx is being the ascii code of the char.
              for(wxString::const_iterator it = escapeChars.begin(); it != escapeChars.end(); ++it)
                token.Replace(wxString::Format("_00%x",(char)*it), *it);
              // What the g_t means I don't know. But we don't need it
              if(token.StartsWith("g_t"))
                token = token.Right(token.Length()-3);
              //! Tokens that end with "-1" aren't too useful, normally.
              if((!token.EndsWith("-1")) && (!token.Contains(" ")))
              {
                m_worksheet->m_helpFileAnchors[token] = id;
                foundAnchors++;
              }
            }
          }
        }
      }
    }
    if(m_worksheet->m_helpFileAnchors["%solve"].IsEmpty())
      m_worksheet->m_helpFileAnchors["%solve"] = m_worksheet->m_helpFileAnchors["to_poly_solve"];
    
    wxLogMessage(wxString::Format(_("Found %i anchors."), foundAnchors));
    if(foundAnchors > 50)
      SaveManualAnchorsToCache();
    else
      LoadBuiltInManualAnchors();
  }

  #ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_helpFileAnchorsLock);
  #endif
}

void wxMaxima::SaveManualAnchorsToCache()
{
  long num = m_worksheet->m_helpFileAnchors.size();
  if(num <= 50)
  {
    wxLogMessage(
      wxString::Format(
        _("Found only %li keywords in maxima's manual. Not caching them to disc."),
        num));
    return;
  }
  wxXmlAttribute *maximaVersion = new wxXmlAttribute(
    wxT("maxima_version"),
    m_maximaVersion);
  wxXmlNode *topNode = new wxXmlNode(
    NULL,
    wxXML_DOCUMENT_NODE, wxEmptyString,
    wxEmptyString
    );
  wxXmlNode *headNode = new wxXmlNode(
    topNode,
    wxXML_ELEMENT_NODE, wxT("maxima_toc"),
    wxEmptyString,
    maximaVersion
    );
  
  Worksheet::HelpFileAnchors::const_iterator it;
  for (it = m_worksheet->m_helpFileAnchors.begin();
       it != m_worksheet->m_helpFileAnchors.end();
       ++it)
  {
    wxXmlNode *manualEntry =
      new wxXmlNode(
        headNode,
          wxXML_ELEMENT_NODE,
        "entry");
    wxXmlNode *anchorNode = new wxXmlNode(
      manualEntry,
      wxXML_ELEMENT_NODE,
      "anchor");
    new wxXmlNode(
      anchorNode,
      wxXML_TEXT_NODE,
      wxEmptyString,
      it->second);
    wxXmlNode *keyNode = new wxXmlNode(
      manualEntry,
      wxXML_ELEMENT_NODE,
        "key");
    new wxXmlNode(
        keyNode,
        wxXML_TEXT_NODE,
        wxEmptyString,
        it->first);
  }
  wxXmlDocument xmlDoc;
  xmlDoc.SetDocumentNode(topNode);
  wxXmlNode *commentNode = new wxXmlNode(
    NULL,
    wxXML_COMMENT_NODE,
    wxEmptyString,
    _("This file is generated by wxMaxima\n"
      "It caches the list of subjects maxima's manual offers and is automatically\n"
      "overwritten if maxima's version changes or the file cannot be read"));
  
  xmlDoc.AppendToProlog(commentNode);
  
  wxString saveName = Dirstructure::AnchorsCacheFile();
  wxLogMessage(wxString::Format(_("Trying to cache the list of subjects the manual contains in the file %s."),
                                saveName.utf8_str()));
  xmlDoc.Save(saveName);
}

bool wxMaxima::LoadManualAnchorsFromXML(wxXmlDocument xmlDocument, bool checkManualVersion)
{
  wxXmlNode *headNode = xmlDocument.GetDocumentNode();
  if(!headNode)
  {
    wxLogMessage(_("The cache for the subjects the manual contains has no head node."));
    return false;
  }
  headNode = headNode->GetChildren();
  while((headNode) && (headNode->GetName() != wxT("maxima_toc")))
    headNode = headNode->GetNext();
  if(!headNode)
  {
    wxLogMessage(_("Anchors file has no top node."));
    return false;
  }
  if(checkManualVersion && ((headNode->GetAttribute(wxT("maxima_version")) != m_maximaVersion)))
  {
    wxLogMessage(_("The cache for the subjects the manual contains is from a different Maxima version."));
    return false;
  }
  wxXmlNode *entry = headNode->GetChildren();
  if(entry == NULL)
  {
    wxLogMessage(_("No entries in the caches for the subjects the manual contains."));
    return false;
  }
  while(entry)
  {
    if(entry->GetName() == wxT("entry"))
    {
      wxString key;
      wxString anchor;
      wxXmlNode *node = entry->GetChildren();
      while(node)
      {
        if((node->GetName() == wxT("anchor")) && (node->GetChildren()))
          anchor = node->GetChildren()->GetContent();
        if((node->GetName() == wxT("key")) && (node->GetChildren()))
          key = node->GetChildren()->GetContent();
        node = node->GetNext();
      }
      if((!key.IsEmpty()) && (!anchor.IsEmpty()))
        m_worksheet->m_helpFileAnchors[key] = anchor;
    }
    entry = entry->GetNext();
  }
  return !m_worksheet->m_helpFileAnchors.empty();
}

bool wxMaxima::LoadBuiltInManualAnchors()
{
  wxLogMessage(_("Using the built-in list of manual anchors."));
  wxMemoryInputStream istream(manual_anchors_xml_gz, manual_anchors_xml_gz_len);
  wxZlibInputStream zstream(istream);
  wxXmlDocument xmlDoc;
  xmlDoc.Load(zstream, wxT("UTF-8"));
  if(!LoadManualAnchorsFromXML(xmlDoc, false))
    return false;
  m_worksheet->m_helpFileAnchorsUsable = true;
  return true;
}

bool wxMaxima::LoadManualAnchorsFromCache()
{
  SuppressErrorDialogs suppressor;
  wxString anchorsFile = Dirstructure::Get()->AnchorsCacheFile();
  if(!wxFileExists(anchorsFile))
  {
    wxLogMessage(_("No file with the subjects the manual contained in the last wxMaxima run."));
    return false;
  }
  wxXmlDocument xmlDocument(anchorsFile);
  if(!xmlDocument.IsOk())
  {
    wxLogMessage(_("The cache for the subjects the manual contains cannot be read."));
    wxRemoveFile(anchorsFile);
    return false;
  }
  
  if(LoadManualAnchorsFromXML(xmlDocument))
  {
    wxLogMessage(wxString::Format(_("Read the entries the maxima manual offers from %s"), Dirstructure::Get()->AnchorsCacheFile().utf8_str()));
    m_worksheet->m_helpFileAnchorsUsable = true;
    return true;
  }
  return !m_worksheet->m_helpFileAnchors.empty();
}

void wxMaxima::ShowMaximaHelp(wxString keyword)
{
  if(keyword.StartsWith("(%i"))
    keyword = "inchar";
  if(keyword.StartsWith("(%o"))
    keyword = "outchar";
  wxString MaximaHelpFile = GetMaximaHelpFile();
#ifdef __WINDOWS__
  // replace \ with / als directory separator
  MaximaHelpFile.Replace("\\", "/", true);
#endif
  
  wxBusyCursor crs;
  CompileHelpFileAnchors();
  keyword = m_worksheet->m_helpFileAnchors[keyword];
  if(keyword.IsEmpty())
    keyword = "Function-and-Variable-Index";
  if(!MaximaHelpFile.IsEmpty())
  {
    // A Unix absolute path starts with a "/", so a valid file URI
    // file:///path/to/helpfile (3 slashes!!) is constructed.
    // On Windows the path starts e.g. with C:/path/to/helpfile
    // so a third "/" must be inserted.
    // Otherwise "C" might be considered as hostname.
    wxString maximaHelpfileURI = wxString("file://")+
#ifdef __WINDOWS__
      wxString("/") +
#endif
      MaximaHelpFile;
    if(!keyword.IsEmpty()) {
      maximaHelpfileURI = maximaHelpfileURI + "#" + keyword;
    }
    wxLogMessage(wxString::Format(_("Opening help file %s"),maximaHelpfileURI.utf8_str()));
    LaunchHelpBrowser(maximaHelpfileURI);
  }
  else
  {
    wxLogMessage(_(wxT("No offline manual found ⇒ Redirecting to the maxima homepage")));
    LaunchHelpBrowser("http://maxima.sourceforge.net/docs/manual/maxima_singlepage.html#"+keyword);
  }
}

bool wxMaxima::InterpretDataFromMaxima(const wxString &newData)
{
  if (newData.empty())
    return false;

  // Speed up things if we want to output more than one line of data in this step

  if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
    m_xmlInspector->Add_FromMaxima(newData);
  // This way we can avoid searching the whole string for a
  // ending tag if we have received only a few bytes of the
  // data between 2 tags
  m_currentOutputEnd = m_currentOutput.Right(30) + newData;

  m_currentOutput += newData;
  if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
    m_xmlInspector->Add_FromMaxima(wxm::emptyString);

  if (!m_dispReadOut &&
      (m_currentOutput != wxT("\n")) &&
      (m_currentOutput != m_emptywxxmlSymbols))
  {
    StatusMaximaBusy(transferring);
    m_dispReadOut = true;
  }

  size_t length_old = 0;

  while (length_old != m_currentOutput.Length())
  {
    if (m_currentOutput.StartsWith("\n<"))
      m_currentOutput = m_currentOutput.Right(m_currentOutput.Length() - 1);

    length_old = m_currentOutput.Length();

    GroupCell *oldActiveCell = NULL;
    GroupCell *newActiveCell = NULL;

    // Handle text that isn't wrapped in a known tag
    if (!m_first)
    {
      // First read the prompt that tells us that maxima awaits the next command:
      // If that is the case ReadPrompt() sends the next command to maxima and
      // maxima can work while we interpret its output.
      oldActiveCell = m_worksheet->GetWorkingGroup();
      ReadPrompt(m_currentOutput);
      newActiveCell = m_worksheet->GetWorkingGroup();

      // Temporarily switch to the WorkingGroup the output we don't have interpreted yet
      // was for
      if (newActiveCell != oldActiveCell)
        m_worksheet->SetWorkingGroup(oldActiveCell);

      ParseNextChunkFromMaxima(m_currentOutput);
    }
    else
      // This function determines the port maxima is running on from the text
      // maxima outputs at startup. This piece of text is afterwards discarded.
      ReadFirstPrompt(m_currentOutput);

    // Switch to the WorkingGroup the next bunch of data is for.
    if (newActiveCell != oldActiveCell)
      m_worksheet->SetWorkingGroup(newActiveCell);
  }
  return true;
}

///--------------------------------------------------------------------------------
///  Idle event
///--------------------------------------------------------------------------------

void wxMaxima::OnIdle(wxIdleEvent &event)
{
  // Update the info what maxima is currently doing
  UpdateStatusMaximaBusy();

  // Update the info how long the evaluation queue is
  if(m_updateEvaluationQueueLengthDisplay)
  {
    m_updateEvaluationQueueLengthDisplay = false;
    if ((m_EvaluationQueueLength > 0) || (m_commandsLeftInCurrentCell >= 1))
    {
      wxString statusLine = wxString::Format(_("%i cells in evaluation queue"),
                                             m_EvaluationQueueLength);
      if (m_commandsLeftInCurrentCell > 1)
        statusLine += wxString::Format(_("; %i commands left in the current cell"),
                                       m_commandsLeftInCurrentCell - 1);
      LeftStatusText(statusLine,false);
    }
    else
    {
      if (m_first)
      {
        if(!m_openInitialFileError)
          LeftStatusText(_("Welcome to wxMaxima"));
      }
      else
      {
        if(  m_worksheet->m_configuration->InLispMode())
          LeftStatusText(_("Lisp mode."));
        else
          LeftStatusText(_("Maxima is ready for input."));
      }
      m_openInitialFileError = false;
    }
    
    event.RequestMore();
    return;
  }

  if(m_worksheet != NULL)
  {
    bool requestMore = m_worksheet->RecalculateIfNeeded();
    m_worksheet->ScrollToCellIfNeeded();
    m_worksheet->ScrollToCaretIfNeeded();
    if(requestMore)
    {
      event.RequestMore();
      return;
    }
  }

  if(m_worksheet != NULL)
    m_worksheet->UpdateScrollPos();

  // Incremental search is done from the idle task. This means that we don't forcefully
  // need to do a new search on every character that is entered into the search box.
  if (m_worksheet->m_findDialog != NULL)
  {
    if (
      (m_oldFindString != m_worksheet->m_findDialog->GetData()->GetFindString()) ||
      (m_oldFindFlags != m_worksheet->m_findDialog->GetData()->GetFlags())
      )
    {
      m_oldFindFlags = m_worksheet->m_findDialog->GetData()->GetFlags();
      m_oldFindString = m_worksheet->m_findDialog->GetData()->GetFindString();

      if ((m_worksheet->m_configuration->IncrementalSearch()) && (m_worksheet->m_findDialog != NULL))
      {
        m_worksheet->FindIncremental(m_findData.GetFindString(),
                                     m_findData.GetFlags() & wxFR_DOWN,
                                     !(m_findData.GetFlags() & wxFR_MATCHCASE));
      }
      
      m_worksheet->RequestRedraw();
      event.RequestMore();
      return;
    }
  }

  if(m_worksheet->RedrawIfRequested())
  {
    event.RequestMore();
    return;
  }

  // If nothing which is visible has changed nothing that would cause us to need
  // update the menus and toolbars has.
  if (m_worksheet->UpdateControlsNeeded())
  {
    UpdateMenus();
    UpdateToolBar();
    ResetTitle(m_worksheet->IsSaved());
    event.RequestMore();
    return;
  }

  if((m_newLeftStatusText) || (m_newRightStatusText))
  {
    if(m_newRightStatusText)
      SetStatusText(m_rightStatusText, 1);

    if(m_newLeftStatusText)
      SetStatusText(m_leftStatusText,0);

    m_newRightStatusText = false;
    m_newLeftStatusText = false;

    event.RequestMore();
    return;
  }

  // If we have set the flag that tells us we should update the table of
  // contents sooner or later we should do so now that wxMaxima is idle.
  if (m_worksheet->m_scheduleUpdateToc)
  {
    m_worksheet->m_scheduleUpdateToc = false;
    if (m_worksheet->m_tableOfContents)
    {
      GroupCell *cursorPos;
      cursorPos = m_worksheet->GetHCaret();
      if ((!m_worksheet->HCaretActive()) && (cursorPos == m_worksheet->GetLastCell()))
      {
        if (m_worksheet->GetActiveCell())
          cursorPos = m_worksheet->GetActiveCell()->GetGroup();
        else
          cursorPos = m_worksheet->FirstVisibleGC();
      }
      m_worksheet->m_tableOfContents->UpdateTableOfContents(m_worksheet->GetTree(), cursorPos);
    }
    m_worksheet->m_scheduleUpdateToc = false;

    event.RequestMore();
    return;
  }

  if((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded()))
  {
    m_xmlInspector->UpdateContents();
    event.RequestMore();
    return;
  }
  
  if(UpdateDrawPane())
  {
    event.RequestMore();
    return;
  }

  // If wxMaxima has to open a file on startup we wait for that until we have
  // a valid draw context for size calculations.
  //
  // The draw context is created on displaying the worksheet for the 1st time
  // and after drawing the worksheet onIdle is called => we won't miss this
  // event when we wait for it here.
  if ((m_worksheet != NULL) && (m_worksheet->m_configuration->GetDC() != NULL))
  {
    if(!m_openFile.IsEmpty())
    {
      wxString file = m_openFile;
      m_openFile = wxEmptyString;
      m_openInitialFileError = !OpenFile(file);
      
      // After doing such big a thing we should end our idle event and request
      // a new one to be issued once the computer has time for doing real
      // background stuff.
      event.RequestMore();
      return;
    }
    //! wxm data the worksheet is populated from 
    if(!m_initialWorkSheetContents.IsEmpty())
    {
      //  Convert the comment block to an array of lines
      wxStringTokenizer tokenizer(m_initialWorkSheetContents, "\n");
      wxArrayString lines;
      while ( tokenizer.HasMoreTokens() )
        lines.Add(tokenizer.GetNextToken());
      m_worksheet->InsertGroupCells(
        Format::TreeFromWXM(lines, &m_worksheet->m_configuration));
      m_worksheet->UpdateMLast();
      m_worksheet->SetSaved(true);
      m_initialWorkSheetContents = wxEmptyString;
      event.RequestMore();
      return;
    }
  }

  UpdateSlider();
  
  if (m_ipc.DrainQueue())
  {
    event.RequestMore();
    return;
  }

  // If we reach this point wxMaxima truly is idle
  // => Tell wxWidgets it can process its own idle commands, as well.
  event.Skip();
}

bool wxMaxima::UpdateDrawPane()
{
  int dimensions = 0;
  if(m_drawPane)
  {
    EditorCell *editor = m_worksheet->GetActiveCell();
    if(editor)
    {
      wxString command = m_worksheet->GetActiveCell()->GetFullCommandUnderCursor();
      if(command.Contains(wxT("gr2d")))
        dimensions = 2;
      if(command.Contains(wxT("with_slider_draw")))
        dimensions = 2;
      if(command.Contains(wxT("gr3d")))
        dimensions = 3;
      if(command.Contains(wxT("draw2d")))
        dimensions = 2;
      if(command.Contains(wxT("draw3d")))
        dimensions = 3;
    }
    else
      dimensions = 0;
  }

  if(m_drawDimensions_last != dimensions)
  {
    m_drawPane->SetDimensions(dimensions);
    m_drawDimensions_last = dimensions;
    return true;
  }
  return false;
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::MenuCommand(const wxString &cmd)
{
  m_worksheet->SetFocus();
  m_worksheet->OpenHCaret(cmd);
  m_worksheet->AddCellToEvaluationQueue(m_worksheet->GetActiveCell()->GetGroup());
  TriggerEvaluation();
  m_worksheet->RequestRedraw();
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::PrintMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  switch (event.GetId())
  {
    case wxID_PRINT:
    {
      wxPrintDialogData printDialogData;
      if (m_printData)
        printDialogData.SetPrintData(*m_printData);
      wxPrinter printer(&printDialogData);
      wxString title(_("wxMaxima document"));

      if (m_worksheet->m_currentFile.Length())
      {
        wxString suffix;
        wxFileName::SplitPath(m_worksheet->m_currentFile, NULL, NULL, &title, &suffix);
        title << wxT(".") << suffix;
      }

      {
        // Redraws during printing might end up on paper => temporarily block all redraw
        // events for the console
        wxWindowUpdateLocker noUpdates(m_worksheet);
        wxEventBlocker blocker(m_worksheet);
        Printout printout(title, &m_worksheet->m_configuration, GetContentScaleFactor());
        auto copy = m_worksheet->CopyTree();
        printout.SetData(std::move(copy));
        wxBusyCursor crs;
        if (printer.Print(this, &printout, true))
        {
          m_printData = std::unique_ptr<wxPrintData>(
            new wxPrintData(printer.GetPrintDialogData().GetPrintData()));
        }
      }
      m_worksheet->RecalculateForce();
      m_worksheet->RequestRedraw();
      break;
    }
  }
}

void wxMaxima::UpdateMenus()
{
  if (!m_worksheet)
    return;
  wxASSERT_MSG((!m_worksheet->HCaretActive()) || (m_worksheet->GetActiveCell() == NULL),
               _("Both horizontal and vertical cursor active at the same time"));
  m_MenuBar->EnableItem(wxID_COPY, m_worksheet->CanCopy(true));
  m_MenuBar->EnableItem(wxID_CUT, m_worksheet->CanCut());
  m_MenuBar->EnableItem(menu_copy_tex_from_worksheet, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(menu_copy_matlab_from_worksheet, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(Worksheet::popid_copy_mathml, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(menu_copy_as_bitmap, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(menu_copy_as_svg, m_worksheet->CanCopy());
  #if wxUSE_ENH_METAFILE
  m_MenuBar->EnableItem(menu_copy_as_emf, m_worksheet->CanCopy());
  #endif
  m_MenuBar->EnableItem(menu_copy_as_rtf, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(menu_copy_to_file, m_worksheet->CanCopy());
  m_MenuBar->EnableItem(menu_copy_text_from_worksheet, m_worksheet->CanCopy(true));
  m_MenuBar->EnableItem(wxID_SELECTALL, m_worksheet->GetTree() != NULL);
  m_MenuBar->EnableItem(wxID_UNDO, m_worksheet->CanUndo());
  m_MenuBar->EnableItem(wxID_REDO, m_worksheet->CanRedo());
  m_MenuBar->EnableItem(menu_interrupt_id, m_pid > 0);
  m_MenuBar->EnableItem(Worksheet::popid_comment_selection,
                        m_worksheet->GetActiveCell() && m_worksheet->GetActiveCell()->SelectionActive());
  m_MenuBar->EnableItem(menu_evaluate,
                        m_worksheet->GetActiveCell() || m_worksheet->HasCellsSelected());

  m_MenuBar->EnableItem(menu_evaluate_all_visible, m_worksheet->GetTree());
  m_MenuBar->EnableItem(ToolBar::tb_evaltillhere,
                        m_worksheet->GetTree() &&
                          m_worksheet->CanPaste() &&
                          m_worksheet->GetHCaret()
                        );

  m_MenuBar->EnableItem(menu_jumptoerror, !m_worksheet->GetErrorList().Empty());
  m_MenuBar->EnableItem(wxID_SAVE, (!m_fileSaved));

  for (int id = menu_pane_math; id <= menu_pane_stats; id++)
    m_MenuBar->Check(id, IsPaneDisplayed(static_cast<Event>(id)));
  m_MenuBar->Check(menu_show_toolbar, ToolbarIsShown());

  bool hidecode = !(m_worksheet->m_configuration->ShowCodeCells());
  m_MenuBar->Check(ToolBar::tb_hideCode, hidecode);

  if (m_worksheet->GetTree())
  {
    m_MenuBar->EnableItem(Worksheet::popid_divide_cell, m_worksheet->GetActiveCell());
    m_MenuBar->EnableItem(Worksheet::popid_merge_cells, m_worksheet->CanMergeSelection());
    m_MenuBar->EnableItem(wxID_PRINT, true);
  }
  else
  {
    m_MenuBar->EnableItem(Worksheet::popid_divide_cell, false);
    m_MenuBar->EnableItem(Worksheet::popid_merge_cells, false);
    m_MenuBar->EnableItem(wxID_PRINT, false);
  }
  double zf = m_worksheet->m_configuration->GetZoomFactor();
  if (zf < Configuration::GetMaxZoomFactor())
    m_MenuBar->EnableItem(wxID_ZOOM_IN, true);
  else
    m_MenuBar->EnableItem(wxID_ZOOM_IN, false);
  if (zf > Configuration::GetMinZoomFactor())
    m_MenuBar->EnableItem(wxID_ZOOM_OUT, true);
  else
    m_MenuBar->EnableItem(wxID_ZOOM_OUT, false);

}

void wxMaxima::UpdateToolBar()
{
  if (!m_worksheet->m_mainToolBar)
    return;
  
  m_worksheet->m_mainToolBar->CanCopy(m_worksheet->CanCopy(true));
  m_worksheet->m_mainToolBar->CanCut(m_worksheet->CanCut());
  m_worksheet->m_mainToolBar->CanSave((!m_fileSaved));
  m_worksheet->m_mainToolBar->CanPrint(m_worksheet->GetTree() != NULL);
  m_worksheet->m_mainToolBar->CanEvalTillHere(
          (m_worksheet->GetTree() != NULL) &&
          (m_worksheet->CanPaste()) &&
          (m_worksheet->GetHCaret() != NULL) &&
          ((m_client && m_client->IsConnected())));

  // On MSW it seems we cannot change an icon without side-effects that somehow
  // stop the animation => on this OS we have separate icons for the
  // animation start and stop. On the rest of the OSes we use one combined
  // start/stop button instead.
  if (m_worksheet->CanAnimate())
  {
    SlideShow *slideShow = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());
    if (slideShow->AnimationRunning())
      m_worksheet->m_mainToolBar->AnimationButtonState(ToolBar::Running);
    else
      m_worksheet->m_mainToolBar->AnimationButtonState(ToolBar::Stopped);
  }
  else
    m_worksheet->m_mainToolBar->AnimationButtonState(ToolBar::Inactive);

  bool follow = m_worksheet->ScrolledAwayFromEvaluation();
  switch (m_StatusMaximaBusy)
  {
    case userinput:
      m_worksheet->m_mainToolBar->ShowUserInputBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
      break;
    case waiting:
    case sending:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      if (m_worksheet->GetWorkingGroup() == NULL)
      {
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      }
      break;
    case calculating:
    case transferring:
    case parsing:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, follow);
      break;
    case wait_for_start:
    case disconnected:
    case process_wont_start:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      break;
  }
  EditorCell *editor = m_worksheet->GetActiveCell();

  if(editor == NULL)
  {
    GroupCell *group = NULL;
    if(m_worksheet->GetSelectionStart())
      group = m_worksheet->GetSelectionStart()->GetGroup();

    if(group != NULL)
      editor = group->GetEditable();
  }

  bool canEvaluateNext = ((editor != NULL) && (editor->GetStyle() == TS_INPUT));
  
  if(!canEvaluateNext)
  {

    if(m_worksheet->HCaretActive())
    {
      GroupCell *group = m_worksheet->GetHCaret();
      if(group == NULL)
        group = m_worksheet->GetTree();
      else
        group = group->GetNext();
      while(
        (group != NULL) &&
        (!((group->GetEditable() != NULL) &&
           (group->GetEditable()->GetType() == MC_TYPE_INPUT)) &&
         (!m_worksheet->m_evaluationQueue.IsLastInQueue(group))
          ))
        group = group->GetNext();

      if(group != NULL)
        canEvaluateNext = true;
    }
  }
  if(canEvaluateNext)
    m_worksheet->m_mainToolBar->CanEvalThisCell(true);
  else
    m_worksheet->m_mainToolBar->CanEvalThisCell(false);
  m_worksheet->m_mainToolBar->WorksheetEmpty(m_worksheet->GetTree() == NULL);
  
  m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
}

wxString wxMaxima::ExtractFirstExpression(const wxString &entry)
{
  int semicolon = entry.Find(';');
  int dollar = entry.Find('$');
  bool semiFound = (semicolon != wxNOT_FOUND);
  bool dollarFound = (dollar != wxNOT_FOUND);

  int index;
  if (semiFound && dollarFound)
    index = wxMin(semicolon, dollar);
  else if (semiFound && !dollarFound)
    index = semicolon;
  else if (!semiFound && dollarFound)
    index = dollar;
  else // neither semicolon nor dollar found
    index = entry.Length();

  return entry.SubString(0, index - 1);
}

wxString wxMaxima::GetDefaultEntry()
{
  if (m_worksheet->CanCopy(true))
    return (m_worksheet->GetString()).Trim().Trim(false);
  wxString retval;
  if (m_worksheet->GetActiveCell() != NULL)
    return retval = m_worksheet->GetActiveCell()->GetWordUnderCaret();
  if(m_worksheet->IsSelected(MC_TYPE_DEFAULT))
    return m_worksheet->GetSelectionStart()->ToString(); 
  if (retval.IsEmpty())
    retval = "%";
  return retval;
}

bool wxMaxima::OpenFile(const wxString &file, const wxString &command)
{
  wxBusyCursor crs;
  bool retval = true;
  if (file.IsEmpty())
  {
    wxLogError(_("Trying to open a file with an empty name!"));
    return false;
  }
  if(!(wxFileExists(file)))
  {
    wxLogError(_("Trying to open the non-existing file %s"), file.utf8_str());
    return false;
  }

  m_lastPath = wxPathOnly(file);
  wxString unixFilename(file);
#if defined __WXMSW__
  unixFilename.Replace(wxT("\\"), wxT("/"));
#endif

  wxWindowUpdateLocker dontUpdateTheWorksheet (m_worksheet);
  
  if (command.Length() > 0)
  {
    MenuCommand(command + wxT("(\"") + unixFilename + wxT("\")$"));
    if(command == wxT("load"))
    {
      ReReadConfig();
      m_recentPackages.AddDocument(unixFilename);
      ReReadConfig();
    }
  }
  else if (file.Lower().EndsWith(wxT(".wxm")))
  {
    retval = OpenWXMFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      if(!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Lower().EndsWith(wxT(".mac")))
  {
    retval = OpenMACFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      if(!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }
  else if (file.Lower().EndsWith(wxT(".out")))
  {
    retval = OpenMACFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      if(!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Right(5).Lower() == wxT(".wxmx"))
  {
    retval = OpenWXMXFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      if(!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Right(4).Lower() == wxT(".zip"))
  {
    retval = OpenWXMXFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      if(!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Right(4).Lower() == wxT(".dem"))
  {
    MenuCommand(wxT("demo(\"") + unixFilename + wxT("\")$"));
    ReReadConfig();
    m_recentPackages.AddDocument(file);
    ReReadConfig();
  }

  else if (file.Right(4).Lower() == wxT(".xml"))
    retval = OpenXML(file, m_worksheet); // clearDocument = true

  else
  {
    MenuCommand(wxT("load(\"") + unixFilename + wxT("\")$"));
    ReReadConfig();
    m_recentPackages.AddDocument(unixFilename);
    ReReadConfig();
  }

  UpdateRecentDocuments();
  RemoveTempAutosavefile();
  StartAutoSaveTimer();

  m_worksheet->TreeUndo_ClearBuffers();
  if (m_worksheet->m_currentFile != wxEmptyString)
  {
    wxString filename(m_worksheet->m_currentFile);
    SetCWD(filename);
  }
  if (m_worksheet->m_tableOfContents != NULL)
  {
    m_worksheet->m_scheduleUpdateToc = false;
    m_worksheet->m_tableOfContents->UpdateTableOfContents(m_worksheet->GetTree(), m_worksheet->GetHCaret());
  }

  if(!retval)
    LeftStatusText(wxString::Format("Errors trying to open the file %s.", file.utf8_str()));

  if(retval)
  {
    m_worksheet->RequestRedraw();
    RightStatusText(_("File opened"));
    if (m_evalOnStartup && m_ready)
    {
      wxLogMessage(_("Starting evaluation of the document"));
      m_evalOnStartup = false;
      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }

  }
  else
    RightStatusText(_("File could not be opened"));

  m_worksheet->RecalculateForce();
  UpdateMenus();
  m_worksheet->UpdateMLast();
  
  return retval;
}

bool wxMaxima::SaveFile(bool forceSave)
{
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;

  wxString file = m_worksheet->m_currentFile;
  wxString fileExt = wxT("wxmx");
  int ext = 0;

  wxConfigBase *config = wxConfig::Get();

  if (file.Length() == 0 || forceSave)
  {
    if (file.Length() == 0)
    {
      config->Read(wxT("defaultExt"), &fileExt);
      file = _("untitled") + wxT(".") + fileExt;
    }
    else
      wxFileName::SplitPath(file, NULL, NULL, &file, &fileExt);

    wxFileDialog fileDialog(this,
                            _("Save As"), m_lastPath,
                            file,
                            _("Whole document (*.wxmx)|*.wxmx|"
                                      "The input, readable by load() (maxima > 5.38) (*.wxm)|*.wxm"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (fileExt == wxT("wxmx"))
      fileDialog.SetFilterIndex(0);
    else if (fileExt == wxT("wxm"))
      fileDialog.SetFilterIndex(1);
    else
    {
      fileDialog.SetFilterIndex(0);
      fileExt = wxT("wxmx");
    }
    if (fileDialog.ShowModal() == wxID_OK)
    {
      file = fileDialog.GetPath();
      ext = fileDialog.GetFilterIndex();
    }
    else
    {
      StartAutoSaveTimer();
      return false;
    }
  }

  if (file.Length())
  {
    if (!file.Lower().EndsWith(wxT(".wxm")) &&
        (!file.Lower().EndsWith(wxT(".wxmx")))
            )
    {
      switch (ext)
      {
        case 0:
          file += wxT(".wxmx");
          break;
        case 1:
          file += wxT(".wxm");
          break;
        default:
          file += wxT(".wxmx");
      }
    }

    StatusSaveStart();
    config->Write(wxT("defaultExt"), wxT("wxmx"));

    m_lastPath = wxPathOnly(file);
    if (file.Lower().EndsWith(wxT(".wxm")))
    {
      config->Write(wxT("defaultExt"), wxT("wxm"));
      if (!m_worksheet->ExportToMAC(file))
      {
        StatusSaveFailed();
        StartAutoSaveTimer();
        return false;
      }
      else
      {
        RemoveTempAutosavefile();
        if(file != m_tempfileName)
          m_worksheet->m_currentFile = file;
      }
    }
    else
    {
      if (!m_worksheet->ExportToWXMX(file))
      {
        StatusSaveFailed();
        StartAutoSaveTimer();
        return false;
      }
      else
      {
        RemoveTempAutosavefile();
        if(file != m_tempfileName)
          m_worksheet->m_currentFile = file;
      }
    }

    if(!m_exitAfterEval)
      m_recentDocuments.AddDocument(file);
    SetCWD(file);
    StatusSaveFinished();
    UpdateRecentDocuments();
  }

  StartAutoSaveTimer();

  return true;
}

void wxMaxima::ReadStdErr()
{
  SuppressErrorDialogs blocker;
  // Maxima will never send us any data via stderr after it has finished
  // starting up and will send data via stdout only in rare cases:
  // It rather sends us the data over the network.
  //
  // If something is severely broken this might not be true, though, and we want
  // to inform the user about it.

  if (m_process == NULL) return;

  if (m_process->IsInputAvailable())
  {
    wxASSERT_MSG(m_maximaStdout != NULL, wxT("Bug: Trying to read from maxima but don't have a input stream"));
    wxTextInputStream istrm(*m_maximaStdout, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStdout->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = _("Message from the stdout of Maxima: ") + o;
    if ((o_trimmed != wxEmptyString) && (!o.StartsWith("Connecting Maxima to server on port")) &&
        (!m_first))
    {
      DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
      if(m_pipeToStdout)
        std::cout << o;
    }
  }
  if (m_process->IsErrorAvailable())
  {
    wxASSERT_MSG(m_maximaStderr != NULL, wxT("Bug: Trying to read from maxima but don't have a error input stream"));
    wxTextInputStream istrm(*m_maximaStderr, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStderr->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = wxT("Message from maxima's stderr stream: ") + o;

    if((o != wxT("Message from maxima's stderr stream: End of animation sequence")) &&
       !o.Contains("frames in animation sequence") && (o_trimmed != wxEmptyString) &&
       (o.Length() > 1))
    {
      DoRawConsoleAppend(o, MC_TYPE_ERROR);
      AbortOnError();
      TriggerEvaluation();
      m_worksheet->GetErrorList().Add(m_worksheet->GetWorkingGroup(true));
      
      if(m_pipeToStdout)
        std::cout << o;
    }
    else
      DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
  }
}

bool wxMaxima::AbortOnError()
{
  // Maxima encountered an error.
  // The question is now if we want to try to send it something new to evaluate.

  ExitAfterEval(false);
  EvalOnStartup(false);

  if (m_worksheet->m_notificationMessage)
  {
    if (m_worksheet->GetWorkingGroup(true) !=
        m_worksheet->m_notificationMessage->m_errorNotificationCell)
      m_worksheet->SetNotification(_("Maxima has issued an error!"), wxICON_ERROR);
    m_worksheet->m_notificationMessage->m_errorNotificationCell = m_worksheet->GetWorkingGroup(true);
  }

  m_exitAfterEval = false;
  if(m_exitOnError)
  {
    wxMaxima::m_exitCode = -1;
    wxExit();
  }
  if (m_worksheet->m_configuration->GetAbortOnError())
  {
    m_worksheet->m_evaluationQueue.Clear();
    // Inform the user that the evaluation queue is empty.
    EvaluationQueueLength(0);
    m_worksheet->ScrollToError();
    return true;
  }
  else
    return false;
}

long long wxMaxima::GetTotalCpuTime()
{
#ifdef __WXMSW__
  FILETIME systemtime;
  GetSystemTimeAsFileTime(&systemtime);
  return (long long) systemtime.dwLowDateTime +
        ((long long) systemtime.dwHighDateTime << 32);
#else
  int CpuJiffies = 0;
  if(wxFileExists("/proc/stat"))
  {
    wxFileInputStream input("/proc/stat");
    if(input.IsOk())
    {
      wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      wxString line;
      while((!input.Eof()) && (!line.StartsWith("cpu ")))
        line = text.ReadLine();

      // Strip the "cpu" from the line
      line = line.Right(line.Length() - 4);
      line.Trim(false);
      wxStringTokenizer tokens(line, wxT(" "));
      for(int i = 0; i < 3; i++)
      {
        if(tokens.HasMoreTokens())
        {
          long additionalJiffies;
          if(!tokens.GetNextToken().ToLong(&additionalJiffies))
            return -1;
          CpuJiffies += additionalJiffies;
        }
        else
          return -1;
      }
    }
  }
  return CpuJiffies;
#endif
}

long long wxMaxima::GetMaximaCpuTime()
{
  #ifdef __WXMSW__
  HANDLE maximaHandle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, m_pid);
  if(maximaHandle != NULL)
  {
    FILETIME creationTime, exitTime, kernelTime, userTime;
    if(GetProcessTimes(maximaHandle, &creationTime, &exitTime, &kernelTime, &userTime))
    {
      long long retval =
        (long long)kernelTime.dwLowDateTime + userTime.dwLowDateTime +
        (2^32)*((long long)kernelTime.dwHighDateTime + userTime.dwHighDateTime);
      CloseHandle(maximaHandle);

      return retval;
    }
  }
  #endif
  int maximaJiffies = 0;
  wxString statFileName = wxString::Format("/proc/%li/stat",m_pid);
  if(wxFileExists(statFileName))
  {
    wxFileInputStream input(statFileName);
    if(input.IsOk())
    {
      wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      wxString line = text.ReadLine();

      wxStringTokenizer tokens(line, wxT(" "));
      for(int i = 0; i < 13; i++)
      {
        if(tokens.HasMoreTokens())
          tokens.GetNextToken();
        else return -1;
      }

      for(int i = 0; i < 4; i++)
      {
        {
          if(tokens.HasMoreTokens())
          {
            long additionalJiffies;
            if(!tokens.GetNextToken().ToLong(&additionalJiffies))
            {
              maximaJiffies = -1;
              break;
            }
            maximaJiffies += additionalJiffies;
          }
          else return -1;
        }
      }
    }
  }
  return maximaJiffies;
}

double wxMaxima::GetMaximaCPUPercentage()
{

  int CpuJiffies = GetTotalCpuTime();
  if(CpuJiffies < 0)
    return -1;

  // If no time has passed since the last call to this function the number of CPU cycles
  // per timespan is infinite - and this function will cause an error if we don't abort
  // it now.
  if(CpuJiffies == m_cpuTotalJiffies_old)
    return -1;

  if(CpuJiffies <= m_cpuTotalJiffies_old)
  {
    m_cpuTotalJiffies_old = CpuJiffies;
    return -1;
  }

  int maximaJiffies = GetMaximaCpuTime();
  if(maximaJiffies < 0)
    return -1;

  double retval =
    (double)(maximaJiffies - m_maximaJiffies_old)/(CpuJiffies - m_cpuTotalJiffies_old) * 100;

  m_maximaJiffies_old = maximaJiffies;
  m_cpuTotalJiffies_old = CpuJiffies;
  return retval;
}

void wxMaxima::OnTimerEvent(wxTimerEvent &event)
{
  switch (event.GetId())
  {
    case COMPILEHELPANCHORS_ID:
      #if HAVE_OPENMP_TASKS
      #pragma omp task
      CompileHelpFileAnchors();
      #endif
      break;
    case MAXIMA_STDOUT_POLL_ID:
      ReadStdErr();

      if (m_process != NULL)
      {
        // The atexit() of maxima informs us if the process dies. But it sometimes doesn't do
        // so if it dies due to an out of memory => Periodically check if it really lives.
        if (!wxProcess::Exists(m_process->GetPid()))
        {
          wxProcessEvent *processEvent;
          processEvent = new wxProcessEvent();
          GetEventHandler()->QueueEvent(processEvent);
        }

        double cpuPercentage = GetMaximaCPUPercentage();
        m_statusBar->SetMaximaCPUPercentage(cpuPercentage);

        if((m_process != NULL) && (m_pid > 0) &&
           ((cpuPercentage > 0) || (m_maximaBusy)))
          m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
      }

      break;
    case KEYBOARD_INACTIVITY_TIMER_ID:
    case AUTO_SAVE_TIMER_ID:
      if ((!m_worksheet->m_keyboardInactiveTimer.IsRunning()) && (!m_autoSaveTimer.IsRunning()))
      {
        AutoSave();
        StartAutoSaveTimer();
      }
      break;
  }
}

bool wxMaxima::AutoSave()
{
  if(!SaveNecessary())
    return true;
  
  bool savedWas = m_worksheet->IsSaved();
  wxString oldTempFile = m_tempfileName;
  wxString oldFilename = m_worksheet->m_currentFile;
  m_tempfileName = wxStandardPaths::Get().GetTempDir()+
    wxString::Format("/untitled_%li_%li.wxmx",
                     wxGetProcessId(),m_pid);

  
  if (m_worksheet->m_configuration->AutoSaveAsTempFile() ||
      m_worksheet->m_currentFile.IsEmpty())
  {
    bool saved = m_worksheet->ExportToWXMX(m_tempfileName);

    wxLogMessage(wxString::Format(_("Autosaving as temp file %s"), m_tempfileName.utf8_str()));
    if((m_tempfileName != oldTempFile) && saved)
    {
      if(!oldTempFile.IsEmpty())
      {
        if(wxFileExists(oldTempFile))
        {
          SuppressErrorDialogs blocker;
          wxLogMessage(wxString::Format(_("Trying to remove the old temp file %s"), oldTempFile.utf8_str()));
          wxRemoveFile(oldTempFile);
        }
      }
    }
    RegisterAutoSaveFile();
  }
  else
  {
    wxLogMessage(wxString::Format(_("Autosaving the .wxmx file as %s"),
                                  m_worksheet->m_currentFile.utf8_str()));
    savedWas = SaveFile(false);
  }

  m_worksheet->SetSaved(savedWas);
  ResetTitle(savedWas, true);

  oldTempFile = m_tempfileName;
  m_worksheet->m_currentFile = oldFilename;
  return savedWas;
}

void wxMaxima::FileMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  bool forceSave = false;
#if defined __WXMSW__
  wxString b = wxT("\\");
  wxString f = wxT("/");
#endif

  switch (event.GetId())
  {
    case wxID_EXIT:
    case wxID_CLOSE:
      Close();
      break;

    case wxID_OPEN:
    {
      if (SaveNecessary())
      {
        int close = SaveDocumentP();

        if (close == wxID_CANCEL)
          return;

        if (close == wxID_YES)
        {
          if (!SaveFile())
            return;
        }
      }

      wxString file = wxFileSelector(_("Open"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("All openable types (*.wxm, *.wxmx, *.mac, *.out, *.xml)|*.wxm;*.wxmx;*.mac;*.out;*.xml|"
                                      "wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx|"
                                      "Maxima session (*.mac)|*.mac|"
                                      "Xmaxima session (*.out)|*.out|"
                                      "xml from broken .wxmx (*.xml)|*.xml"),
                                     wxFD_OPEN);

      if (!file.empty())
      {
        // On the mac the "File/New" menu item by default opens a new window instead od
        // reusing the old one.
        #ifdef __WXOSX__
        if(m_worksheet->IsEmpty())
          OpenFile(file, wxEmptyString);
        else
          wxGetApp().NewWindow(file);
        #else
        OpenFile(file, wxEmptyString);
        #endif
      }
    }
      break;

    case wxID_SAVEAS:
      forceSave = true;
      m_fileSaved = false;
      SaveFile(forceSave);
      // Seems like resetting the title on "file/save as" is a little bit
      // sluggish, otherwise.
      ResetTitle(m_worksheet->IsSaved(), true);
      break;
    case wxID_SAVE:
      SaveFile(forceSave);
      // Seems like resetting the title on "file/save as" is a little bit
      // sluggish, otherwise.
      ResetTitle(m_worksheet->IsSaved(), true);
      break;

    case menu_export_html:
    {
      // Determine a sane default file name;
      wxString file = m_worksheet->m_currentFile;

      if (file.Length() == 0)
        file = _("untitled");
      else
        wxFileName::SplitPath(file, NULL, NULL, &file, NULL);

      wxString fileExt = "html";
      wxConfig::Get()->Read(wxT("defaultExportExt"), &fileExt);

      wxFileDialog fileDialog(this,
                              _("Export"), m_lastPath,
                              file + wxT(".") + fileExt,
                              _("HTML file (*.html)|*.html|"
                                        "maxima batch file (*.mac)|*.mac|"
                                        "LaTeX file (*.tex)|*.tex"
                              ),
                              wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

      if (fileExt == wxT("html"))
        fileDialog.SetFilterIndex(0);
      else if (fileExt == wxT("mac"))
        fileDialog.SetFilterIndex(1);
      else
        fileDialog.SetFilterIndex(2);

      if (fileDialog.ShowModal() == wxID_OK)
      {
        file = fileDialog.GetPath();
        if (file.Length())
        {
          int ext = fileDialog.GetFilterIndex();
          if ((!file.Lower().EndsWith(wxT(".html"))) &&
              (!file.Lower().EndsWith(wxT(".mac"))) &&
              (!file.Lower().EndsWith(wxT(".tex")))
                  )
          {
            switch (ext)
            {
              case 0:
                file += wxT(".html");
                break;
              case 1:
                file += wxT(".mac");
                break;
              case 2:
                file += wxT(".tex");
                break;
              default:
                file += wxT(".html");
            }
          }

          if (file.Lower().EndsWith(wxT(".tex")))
          {
            StatusExportStart();

            fileExt = wxT("tex");
            // Show a busy cursor as long as we export a file.
            wxBusyCursor crs;
            if (!m_worksheet->ExportToTeX(file))
            {
              LoggingMessageBox(_("Exporting to TeX failed!"), _("Error!"),
                           wxOK);
              StatusExportFailed();
            }
            else
              StatusExportFinished();
          }
          else if (file.Lower().EndsWith(wxT(".mac")))
          {
            StatusExportStart();

            // Show a busy cursor as long as we export a file.
            wxBusyCursor crs;
            fileExt = wxT("mac");
            if (!m_worksheet->ExportToMAC(file))
            {
              LoggingMessageBox(_("Exporting to maxima batch file failed!"), _("Error!"),
                           wxOK);
              StatusExportFailed();
            }
            else
              StatusExportFinished();
          }
          else
          {
            StatusExportStart();

            // Show a busy cursor as long as we export a file.
            wxBusyCursor crs;
            fileExt = wxT("html");
            if (!m_worksheet->ExportToHTML(file))
            {
              LoggingMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                           wxOK);
              StatusExportFailed();
            }
            else
              StatusExportFinished();
          }
          StartAutoSaveTimer();

          wxConfig::Get()->Write(wxT("defaultExportExt"), fileExt);
        }
      }
    }
      break;

    case menu_load_id:
    {
      wxString file = wxFileSelector(_("Load Package"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Maxima package (*.mac)|*.mac|"
                                               "Lisp package (*.lisp)|*.lisp|All|*"),
                                     wxFD_OPEN);
      if (!file.empty())
        OpenFile(file, wxT("load"));
    }
      break;

    case menu_batch_id:
    {
      wxString file = wxFileSelector(_("Batch File"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Maxima package (*.mac)|*.mac"),
                                     wxFD_OPEN);
      if(file != wxEmptyString)
        OpenFile(file, wxT("batch"));
    }
      break;

    case ToolBar::tb_animation_startStop:
      if (m_worksheet->CanAnimate())
      {
        SlideShow *slideShow = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());
        if (slideShow->AnimationRunning())
          m_worksheet->Animate(false);
        else
          m_worksheet->Animate(true);
      }
      break;

    case Worksheet::popid_animation_start:
      if (m_worksheet->CanAnimate())
      {
        SlideShow *slideShow = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());
        slideShow->AnimationRunning(true);
      }
      break;

    default:
      break;
  }
  m_worksheet->RequestRedraw();
}

void wxMaxima::EditMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  //if (m_worksheet->m_findDialog != NULL) {
  //  event.Skip();
  //  return;
  //}

  switch (event.GetId())
  {
  case Worksheet::popid_labelwidth3:
  case Worksheet::popid_labelwidth4:
  case Worksheet::popid_labelwidth5:
  case Worksheet::popid_labelwidth6:
  case Worksheet::popid_labelwidth7:
  case Worksheet::popid_labelwidth8:
  case Worksheet::popid_labelwidth9:
  case Worksheet::popid_labelwidth10:
    m_worksheet->m_configuration->LabelWidth(event.GetId() - Worksheet::popid_labelwidth3 + 3);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  case Worksheet::popid_digits_20:
  {
    m_worksheet->m_configuration->SetDisplayedDigits(20);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_digits_50:
  {
    m_worksheet->m_configuration->SetDisplayedDigits(50);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_digits_100:
  {
    m_worksheet->m_configuration->SetDisplayedDigits(100);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_digits_all:
  {
    m_worksheet->m_configuration->SetDisplayedDigits(0);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_labels_autogenerated:
  {
    m_worksheet->m_configuration->SetLabelChoice(Configuration::labels_automatic);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_labels_user:
  {
    m_worksheet->m_configuration->SetLabelChoice(Configuration::labels_prefer_user);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_labels_useronly:
  {
    m_worksheet->m_configuration->SetLabelChoice(Configuration::labels_useronly);
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  }
  case Worksheet::popid_labels_disable:
  {
    m_worksheet->m_configuration->SetLabelChoice(Configuration::labels_none);
    m_worksheet->RequestRedraw();
    break;
  }

  case Worksheet::popid_popup_gnuplot:
  {
    if (!m_worksheet->GetSelectionStart())
      return;

    wxString gnuplotSource =
      m_worksheet->GetSelectionStart()->GnuplotSource();
    if(gnuplotSource.IsEmpty())
      return;

    if(!wxFileExists(gnuplotSource))
      return;

    // Create a gnuplot file that doesn't select a terminal and output file
    {
      wxFileInputStream input(gnuplotSource);
      if(!input.IsOk())
        return;
      wxTextInputStream textIn(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));

      wxFileOutputStream output(gnuplotSource + wxT(".popout"));
      if(!output.IsOk())
        return;
      wxTextOutputStream textOut(output);

      textIn.ReadLine();textIn.ReadLine();

      wxString line;
      while(!input.Eof())
      {
        line = textIn.ReadLine();
        textOut << line + wxT("\n");
      }
      // tell gnuplot to wait for the window to close - or for 10 minutex
      // if gnuplot is too old to understand that.
      textOut<<"if(GPVAL_VERSION >= 5.0) bind \"Close\" \"exit gnuplot\"\n";
      textOut<<"if(GPVAL_VERSION >= 5.0) pause mouse close; else pause 600\n";
      textOut<<"quit\n";
      textOut.Flush();
    }
      
    // Execute gnuplot
    wxString cmdline = m_gnuplotcommand + wxT(" " + gnuplotSource + wxT(".popout"));
    wxLogMessage(_("Running gnuplot as: " + cmdline));
    
    m_gnuplotProcess = new wxProcess(this, gnuplot_process_id);
    if (wxExecute(cmdline,
                  wxEXEC_ASYNC|wxEXEC_SHOW_CONSOLE,
                  m_gnuplotProcess) < 0)
      wxLogMessage(_("Cannot start gnuplot"));
    break;
  }
  case wxID_PREFERENCES:
  {
    // wxGTK uses wxFileConf. ...and wxFileConf loads the config file only once
    // on inintialisation => Let's reload the config file before entering the
    // config dialogue.
    ReReadConfig();
    wxConfigBase *config = wxConfig::Get();

    ConfigDialogue *configW = new ConfigDialogue(this, m_worksheet->m_configuration);
    configW->Centre(wxBOTH);
    if (configW->ShowModal() == wxID_OK)
    {
      configW->WriteSettings();
      // Write the changes in the configuration to the disk.
      config->Flush();
      // Refresh the display as the settings that affect it might have changed.
      m_worksheet->m_configuration->ReadStyles();
      m_worksheet->RecalculateForce();
      m_worksheet->m_configuration->FontChanged(true);
      m_worksheet->RequestRedraw();
      ConfigChanged();
    }

    configW->Destroy();
    break;
  }
  case wxID_COPY:
    m_worksheet->Copy();
    break;
  case menu_copy_text_from_worksheet:
    m_worksheet->Copy(true);
    break;
  case wxID_CUT:
    if (m_worksheet->CanCut())
      m_worksheet->CutToClipboard();
    break;
  case wxID_SELECTALL:
    m_worksheet->SelectAll();
    break;
  case wxID_PASTE:
    m_worksheet->PasteFromClipboard();
    break;
  case wxID_UNDO:
    if (m_worksheet->CanUndo())
      m_worksheet->Undo();
    break;
  case wxID_REDO:
    if (m_worksheet->CanRedo())
      m_worksheet->Redo();
    break;
  case menu_copy_matlab_from_worksheet:
	if (m_worksheet->CanCopy())
	  m_worksheet->CopyMatlab();
	break;
  case menu_copy_tex_from_worksheet:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyTeX();
    break;
  case Worksheet::popid_copy_mathml:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyMathML();
    break;
  case menu_copy_as_bitmap:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyBitmap();
    break;
  case menu_copy_as_svg:
    if (m_worksheet->CanCopy())
      m_worksheet->CopySVG();
    break;
#if wxUSE_ENH_METAFILE
  case menu_copy_as_emf:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyEMF();
    break;
#endif
  case menu_copy_as_rtf:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyRTF();
    break;
  case menu_copy_to_file:
  {
    wxString file = wxFileSelector(_("Save Selection to Image"), m_lastPath,
                                   wxT("image.png"), wxT("png"),
                                   _("PNG image (*.png)|*.png|"
                                     "JPEG image (*.jpg)|*.jpg|"
                                     "GIF image (*.gif)|*.gif|"
                                     "Scaleable vector graphics (*.svg)|*.svg|"
                                     "Windows bitmap (*.bmp)|*.bmp|"
                                     "Portable anymap (*.pnm)|*.pnm|"
                                     "Tagged image file format (*.tif)|*.tif|"
                                     "X pixmap (*.xpm)|*.xpm"
                                     ),
                                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        m_worksheet->CopyToFile(file);
        m_lastPath = wxPathOnly(file);
      }
    }
      break;
    case Worksheet::popid_delete:
      if (m_worksheet->CanDeleteSelection())
      {
        m_worksheet->DeleteSelection();
        m_worksheet->Recalculate();
        m_worksheet->RequestRedraw();
        return;
      }
      break;
    case wxID_ZOOM_IN:
      m_worksheet->SetZoomFactor(m_worksheet->m_configuration->GetZoomFactor() + 0.1);
      break;
    case wxID_ZOOM_OUT:
      m_worksheet->SetZoomFactor(m_worksheet->m_configuration->GetZoomFactor() - 0.1);
      break;
    case menu_zoom_80:
      m_worksheet->SetZoomFactor(0.8);
      break;
    case wxID_ZOOM_100:
      m_worksheet->SetZoomFactor(1.0);
      break;
    case menu_zoom_120:
      m_worksheet->SetZoomFactor(1.2);
      break;
    case menu_zoom_150:
      m_worksheet->SetZoomFactor(1.5);
      break;
    case menu_zoom_200:
      m_worksheet->SetZoomFactor(2.0);
      break;
    case menu_zoom_300:
      m_worksheet->SetZoomFactor(3.0);
      break;
    case menu_math_as_1D_ASCII:
      MenuCommand(wxT("set_display('none)$"));
      break;
    case menu_math_as_2D_ASCII:
      MenuCommand(wxT("set_display('ascii)$"));
      break;
    case menu_math_as_graphics:
      MenuCommand(wxT("set_display('xml)$"));
      break;
    case menu_noAutosubscript:
      MenuCommand(wxT("wxsubscripts: false$"));
      break;
    case menu_defaultAutosubscript:
      MenuCommand(wxT("wxsubscripts: true$"));
      break;
    case menu_alwaysAutosubscript:
      MenuCommand(wxT("wxsubscripts: 'all$"));
      break;
    case menu_roundedMatrixParens:
      MenuCommand(wxT("lmxchar:\"(\"$rmxchar:\")\"$"));
      break;
    case menu_straightMatrixParens:
      MenuCommand(wxT("lmxchar:\"|\"$rmxchar:\"|\"$"));
      break;
    case menu_angledMatrixParens:
      MenuCommand(wxT("lmxchar:\"<\"$rmxchar:\">\"$"));
      break;
    case menu_squareMatrixParens:
      MenuCommand(wxT("lmxchar:\"[\"$rmxchar:\"]\"$"));
      break;
    case menu_noMatrixParens:
      MenuCommand(wxT("lmxchar:\" \"$rmxchar:\" \"$"));
      break;
    case menu_fullscreen:
      ShowFullScreen(!IsFullScreen());
      break;
    case menu_invertWorksheetBackground:
      m_worksheet->m_configuration->InvertBackground(
        !m_worksheet->m_configuration->InvertBackground());
      m_Maxima_Panes_Sub->Check(menu_invertWorksheetBackground,
                        m_worksheet->m_configuration->InvertBackground());
      m_worksheet->RequestRedraw();
      break;
    case ToolBar::tb_hideCode:
      m_worksheet->m_configuration->ShowCodeCells(!m_worksheet->m_configuration->ShowCodeCells());
      m_worksheet->CodeCellVisibilityChanged();
      break;
    case menu_remove_output:
      m_worksheet->RemoveAllOutput();
      break;
    case menu_show_toolbar:
      ShowToolBar(!ToolbarIsShown());
      break;
    case wxID_FIND:
      if (m_worksheet->m_findDialog == NULL)
        m_worksheet->m_findDialog = new FindReplaceDialog(
                this,
                &m_findData,
                _("Find and Replace"));

      if (m_worksheet->GetActiveCell() != NULL)
      {
        // Start incremental search and highlighting of search results again.
        m_oldFindString = wxEmptyString;

        wxString selected = m_worksheet->GetActiveCell()->GetSelectionString();
        if (selected.Length() > 0)
          m_worksheet->m_findDialog->SetFindString(selected);
      }

      m_worksheet->m_findDialog->Show(true);
      m_worksheet->m_findDialog->SetFocus();
      m_worksheet->m_findDialog->Raise();
      break;
    case menu_history_next:
    {
      wxString command = m_history->GetCommand(true);
      if (command != wxEmptyString)
        m_worksheet->SetActiveCellText(command);
    }
      break;
    case menu_history_previous:
    {
      wxString command = m_history->GetCommand(false);
      if (command != wxEmptyString)
        m_worksheet->SetActiveCellText(command);
    }
    break;

  case Worksheet::popid_hide_tooltipMarkerForThisMessage:
    {
      if(m_worksheet->GetSelectionStart() == NULL)
        return;
      Cell *cell = m_worksheet->GetSelectionStart();
      if(!cell)
        return;
      wxString toolTip = cell->GetLocalToolTip();
      if(toolTip.IsEmpty())
        return;
      bool suppress = m_worksheet->m_configuration->HideMarkerForThisMessage(toolTip);
      m_worksheet->m_configuration->HideMarkerForThisMessage(toolTip, !suppress);
      m_worksheet->OutputChanged();
      break;
    }
  case Worksheet::popid_hide_tooltipMarker:
    {
      if(m_worksheet->GetSelectionStart() == NULL)
        return;
      GroupCell *cell = m_worksheet->GetSelectionStart()->GetGroup();
      GroupCell *end = NULL;
      if(m_worksheet->GetSelectionEnd() != NULL)
        end = m_worksheet->GetSelectionEnd()->GetGroup();
      bool marked = !cell->GetSuppressTooltipMarker();

      for (auto &tmp : OnList(cell))
      {
        tmp.SetSuppressTooltipMarker(marked);
        if(&tmp == end)
          break;
      }
      m_worksheet->OutputChanged();
      break;
    }
  }
  m_worksheet->RequestRedraw();
}

void wxMaxima::OnFind(wxFindDialogEvent &event)
{
  if (!m_worksheet->FindNext(event.GetFindString(),
                           event.GetFlags() & wxFR_DOWN,
                           !(event.GetFlags() & wxFR_MATCHCASE)))
    LoggingMessageBox(_("No matches found!"));
}

void wxMaxima::OnFindClose(wxFindDialogEvent &WXUNUSED(event))
{
  if (m_worksheet->m_findDialog != NULL)
    m_worksheet->m_findDialog->Destroy();
  m_oldFindString = wxEmptyString;
  m_worksheet->m_findDialog = NULL;
}

void wxMaxima::OnReplace(wxFindDialogEvent &event)
{
  m_worksheet->Replace(event.GetFindString(),
                     event.GetReplaceString(),
                     !(event.GetFlags() & wxFR_MATCHCASE)
  );

  if (!m_worksheet->FindNext(event.GetFindString(),
                           event.GetFlags() & wxFR_DOWN,
                           !(event.GetFlags() & wxFR_MATCHCASE)
  )
          )
    LoggingMessageBox(_("No matches found!"));
  else
    m_worksheet->UpdateTableOfContents();
}

void wxMaxima::OnReplaceAll(wxFindDialogEvent &event)
{
  int count = m_worksheet->ReplaceAll(
          event.GetFindString(),
          event.GetReplaceString(),
          !(event.GetFlags() & wxFR_MATCHCASE)
  );

  LoggingMessageBox(wxString::Format(_("Replaced %d occurrences."), count));
  if (count > 0)
    m_worksheet->UpdateTableOfContents();
}

void wxMaxima::OnSymbolAdd(wxCommandEvent &event)
{
  m_worksheet->m_configuration->SymbolPaneAdditionalChars(
    m_worksheet->m_configuration->SymbolPaneAdditionalChars() +
    wxString(wxChar(event.GetId())));
  m_symbolsPane->UpdateUserSymbols();
}

void wxMaxima::MaximaMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  wxString b = wxT("\\");
  wxString f = wxT("/");
  switch (event.GetId())
  {
    case menu_jumptoerror:
      if (m_worksheet->GetErrorList().FirstError())
      {
        m_worksheet->SetActiveCell(dynamic_cast<GroupCell *>(m_worksheet->GetErrorList().FirstError())->GetEditable());
        dynamic_cast<GroupCell *>(m_worksheet->GetErrorList().FirstError())->GetEditable()->CaretToEnd();
      }
      break;
    case ToolBar::menu_restart_id:
      m_closing = true;
      m_worksheet->SetWorkingGroup(nullptr);
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->ResetInputPrompts();
      m_unsuccessfulConnectionAttempts = 0;
      StartMaxima(true);
      break;
    case menu_soft_restart:
      MenuCommand(wxT("kill(all);"));
      break;
    case menu_functions:
      MenuCommand(wxT("functions;"));
      break;
    case menu_variables:
      MenuCommand(wxT("values;"));
      break;
    case menu_display:
    {
      wxString choices[] =
              {
                      wxT("xml"), wxT("ascii"), wxT("none")
              };
      wxString choice = wxGetSingleChoice(
              _("Select math display algorithm"),
              _("Display algorithm"),
              3,
              choices,
              this
      );
      if (choice.Length())
      {
        cmd = wxT("set_display('") + choice + wxT(")$");
        MenuCommand(cmd);
      }
    }
      break;
    case menu_texform:
      cmd = wxT("tex(") + expr + wxT(")$");
      MenuCommand(cmd);
      break;
    case menu_time:
      if(event.IsChecked())
        cmd = wxT("showtime:all$");
      else
        cmd = wxT("showtime:false$");
      MenuCommand(cmd);
      break;
    case menu_fun_def:
      cmd = GetTextFromUser(_("Show the definition of function:"),
                            _("Function"), m_worksheet->m_configuration, wxEmptyString, this);
      if (cmd.Length())
      {
        cmd = wxT("fundef(") + cmd + wxT(");");
        MenuCommand(cmd);
      }
      break;
    case menu_add_path:
    {
      if (m_lastPath.Length() == 0)
        m_lastPath = wxGetHomeDir();
      wxString dir = wxDirSelector(_("Add dir to path:"), m_lastPath);
      if (dir.Length())
      {
        m_lastPath = dir;
#if defined (__WXMSW__)
        dir.Replace(wxT("\\"), wxT("/"));
#endif
        cmd = wxT("file_search_maxima : cons(sconcat(\"") + dir +
              wxT("/###.{lisp,mac,mc}\"), file_search_maxima)$");
        MenuCommand(cmd);
      }
    }
      break;
    case menu_evaluate_all_visible:
    case ToolBar::tb_eval_all:
    {
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->ResetInputPrompts();
      EvaluationQueueLength(0);
      if (m_worksheet->m_configuration->RestartOnReEvaluation())
        StartMaxima();
      m_worksheet->AddDocumentToEvaluationQueue();
      // Inform the user about the length of the evaluation queue.
      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }
      break;
    case menu_evaluate_all:
    {
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->ResetInputPrompts();
      EvaluationQueueLength(0);
      if (m_worksheet->m_configuration->RestartOnReEvaluation())
        StartMaxima();
      m_worksheet->AddEntireDocumentToEvaluationQueue();
      // Inform the user about the length of the evaluation queue.
      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }
      break;
    case ToolBar::tb_evaltillhere:
    {
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->ResetInputPrompts();
      EvaluationQueueLength(0);
      if (m_worksheet->m_configuration->RestartOnReEvaluation())
        StartMaxima();
      m_worksheet->AddDocumentTillHereToEvaluationQueue();
      // Inform the user about the length of the evaluation queue.
      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }
      break;
    case menu_clear_var:
      cmd = GetTextFromUser(_("Delete variable(s):"), _("Delete"),
                            m_worksheet->m_configuration,
                            wxT("all"), this);
      if (cmd.Length())
      {
        cmd = wxT("remvalue(") + cmd + wxT(");");
        MenuCommand(cmd);
      }
      break;
    case menu_clear_fun:
      cmd = GetTextFromUser(_("Delete function(s):"), _("Delete"),
                            m_worksheet->m_configuration,
                            wxT("all"), this);
      if (cmd.Length())
      {
        cmd = wxT("remfunction(") + cmd + wxT(");");
        MenuCommand(cmd);
      }
      break;
    case menu_subst:
    case button_subst:
    {
      SubstituteWiz *wiz = new SubstituteWiz(this, -1, m_worksheet->m_configuration, _("Substitute"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    default:
      break;
  }
}

void wxMaxima::EquationsMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case menu_allroots:
      cmd = wxT("allroots(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_bfallroots:
      cmd = wxT("bfallroots(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_realroots:
      cmd = wxT("realroots(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_solve:
    case menu_solve:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Variable(s):"),
                                 expr, wxT("x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Solve"), true,
                                 _("solve() will solve a list of equations only if for n "
                                   "independent equations there are n variables to solve to.\n"
                                   "If only one result variable is of interest the other result "
                                   "variables solve needs to do its work can be used to tell "
                                   "solve() which variables to eliminate in the solution "
                                   "for the interesting variable.")
        );
      //wiz->Centre(wxBOTH);
      wiz->SetLabel1ToolTip(_("Comma-separated equations"));
      wiz->SetLabel2ToolTip(_("Comma-separated variables"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("solve([") + wiz->GetValue1() + wxT("], [") +
              wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_solve_to_poly:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Variable(s):"),
                                 expr, wxT("x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Solve"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("to_poly_solve([") + wiz->GetValue1() + wxT("], [") +
              wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_solve_num:
    {
      if (expr.StartsWith(wxT("%")))
        expr = wxT("''(") + expr + wxT(")");
      Gen4Wiz *wiz = new Gen4Wiz(_("Equation:"), _("Variable:"),
                                 _("Lower bound:"), _("Upper bound:"),
                                 expr, wxT("x"), wxT("-1"), wxT("1"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Find root"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("find_root(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case button_solve_ode:
    case menu_solve_ode:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Equation:"), _("Function:"), _("Variable:"),
                                 expr, wxT("y"), wxT("x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Solve ODE"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ode2(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_ivp_1:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Solution:"), _("Point:"), _("Value:"),
                                 expr, wxT("x="), wxT("y="),
                                 m_worksheet->m_configuration,
                                 this, -1, _("IC1"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ic1(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_ivp_2:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Solution:"), _("Point:"),
                                 _("Value:"), _("Derivative:"),
                                 expr, wxT("x="), wxT("y="), wxT("'diff(y,x)="),
                                 m_worksheet->m_configuration,
                                 this, -1, _("IC2"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ic2(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() +
                       wxT(", ") + wiz->GetValue4() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_bvp:
    {
      BC2Wiz *wiz = new BC2Wiz(this, -1, m_worksheet->m_configuration, _("BC2"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_eliminate:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equations:"),
                                 _("Variables:"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Eliminate"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("eliminate([") + wiz->GetValue1() + wxT("],[")
              + wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_solve_algsys:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve algebraic system"),
                                    m_worksheet->m_configuration,
                                    wxT("3"), this);
      if (sz.Length() == 0)
        return;
      long isz;
      if (!sz.ToLong(&isz) || isz <= 0)
      {
        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK | wxICON_ERROR);
        return;
      }
      SysWiz *wiz = new SysWiz(this, -1, m_worksheet->m_configuration, _("Solve algebraic system"), isz);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("algsys") + wiz->GetValue();
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_solve_lin:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve linear system"),
                                    m_worksheet->m_configuration,
                                    wxT("3"), this);
      if (sz.Length() == 0)
        return;
      long isz;
      if (!sz.ToLong(&isz) || isz <= 0)
      {
        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK | wxICON_ERROR);
        return;
      }
      SysWiz *wiz = new SysWiz(this, -1, m_worksheet->m_configuration, _("Solve linear system"), isz);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("linsolve") + wiz->GetValue();
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_solve_de:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Function(s):"),
                                 expr, wxT("y(x)"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Solve ODE"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("desolve([") + wiz->GetValue1() + wxT("],[")
              + wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_atvalue:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Point:"),
                                 _("Value:"), expr, wxT("x=0"), wxT("0"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("At value"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("atvalue(") + wiz->GetValue1() + wxT(", ")
                       + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_lhs:
      cmd = wxT("lhs(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_rhs:
      cmd = wxT("rhs(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    default:
      break;
  }
}

void wxMaxima::MatrixMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case menu_csv2mat:
    {
      CsvImportWiz *wiz = new CsvImportWiz(this, m_worksheet->m_configuration);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("read_matrix(\"") + wiz->GetFilename() +
          wxT("\", ") +
          wiz->GetSeparator() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();      
      break;
    }
    case menu_mat2csv:
    {
      CsvExportWiz *wiz = new CsvExportWiz(this, m_worksheet->m_configuration, _("Matrix"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("write_data(") +
          wiz->GetMatrix() +
          wxT(", \"") +
          wiz->GetFilename() +
          wxT("\", ") +
          wiz->GetSeparator() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();      
      break;
    }
    case menu_matrix_row:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Row number:"),
                                 expr, wxT("%"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract a matrix row"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("row(") + wiz->GetValue1() + wxT(", ") + wiz->GetValue2() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
      break;
    }
    case menu_matrix_col:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Row number:"),
                                 expr, wxT("%"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract a matrix row"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("col(") + wiz->GetValue1() + wxT(", ") + wiz->GetValue2() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
      break;
    }
    case menu_matrix_row_list:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Row number:"),
                                 expr, wxT("%"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract a matrix row"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wiz->GetValue1() + wxT("[") + wiz->GetValue2() +
          wxT("];");
        MenuCommand(cmd);
      }
      wiz->Destroy();
      break;
    }
    case menu_matrix_col_list:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Row number:"),
                                 expr, wxT("%"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract a matrix row"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("transpose(") + wiz->GetValue1() + wxT(")[") + wiz->GetValue2() +
          wxT("];");
        MenuCommand(cmd);
      }
      wiz->Destroy();
      break;
    }
    case menu_submatrix:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Matrix:"), _("comma-separated row numbers:"), _("comma-separated column numbers:"),
                                 expr, wxEmptyString, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract a matrix row"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("submatrix(");
        if(!wiz->GetValue2().IsEmpty())
          cmd += wiz->GetValue2() + wxT(",");
        cmd += wiz->GetValue1();
        if(!wiz->GetValue3().IsEmpty())
          cmd += wxT(",") + wiz->GetValue3();
        cmd += wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
      break;
    }
    case menu_invert_mat:
      cmd = wxT("invert(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_determinant:
      cmd = wxT("determinant(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_eigen:
      cmd = wxT("eigenvalues(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_eigvect:
      cmd = wxT("eigenvectors(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_adjoint_mat:
      cmd = wxT("adjoint(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_transpose:
      cmd = wxT("transpose(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_map_mat:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Resulting Matrix name (may be empty):"), _("Function:"), _("Matrix:"),
                                 wxEmptyString, wxEmptyString, expr,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Matrix map"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxEmptyString;
        if(wiz->GetValue1().IsEmpty())
          cmd = wiz->GetValue1() + wxT(":");
        cmd += wxT("matrixmap(") + wiz->GetValue2() + wxT(", ")
              + wiz->GetValue3() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_enter_mat:
    case menu_stats_enterm:
    {
      MatDim *wiz = new MatDim(this, -1,
                               m_worksheet->m_configuration,
                               _("Matrix"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        if (wiz->GetValue0() != wxEmptyString)
          cmd = wiz->GetValue0() + wxT(": ");
        long w, h;
        int type = wiz->GetMatrixType();
        if (!(wiz->GetValue2()).ToLong(&h) ||
            !(wiz->GetValue1()).ToLong(&w) ||
            w <= 0 || h <= 0)
        {
          LoggingMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                       wxOK | wxICON_ERROR);
          return; //-V773
        }
        if (w != h)
          type = MatWiz::MATRIX_GENERAL;
        MatWiz *mwiz = new MatWiz(this, -1, m_worksheet->m_configuration, _("Enter matrix"),
                                  type, w, h);
        //wiz->Centre(wxBOTH);
        if (mwiz->ShowModal() == wxID_OK)
        {
          cmd += mwiz->GetValue();
          MenuCommand(cmd);
        }
        mwiz->Destroy();
      }
      wiz->Destroy();
    }
      break;
    case menu_cpoly:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Variable:"),
                                 expr, wxT("x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Char poly"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("expand(charpoly(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT("));");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_gen_mat:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Array:"), _("Rows:"), _("Columns:"), _("Name:"),
                                 expr, wxT("3"), wxT("3"), wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Generate Matrix"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("genmatrix(") + wiz->GetValue1() +
                       wxT(", ") + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        if (wiz->GetValue4() != wxEmptyString)
          val = wiz->GetValue4() + wxT(": ") + val;
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_gen_mat_lambda:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("matrix[i,j]:"), _("Rows:"), _("Columns:"), _("Name:"),
                                 expr, wxT("3"), wxT("3"), wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Generate Matrix"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("genmatrix(lambda([i,j], ") + wiz->GetValue1() +
                       wxT("), ") + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        if (wiz->GetValue4() != wxEmptyString)
          val = wiz->GetValue4() + wxT(": ") + val;
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case button_map:
    case menu_map:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function:"), _("List(s):"),
                                 wxEmptyString, expr,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Map"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("map(") + wiz->GetValue1() + wxT(", ") + wiz->GetValue2() +
              wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_make_list:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Expression:"), _("Variable:"),
                                 _("From:"), _("To:"),
                                 expr, wxT("k"), wxT("1"), wxT("10"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Make list"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("makelist(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_apply:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function:"), _("List:"),
                                 wxT("\"+\""), expr,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Apply"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("apply(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    default:
      break;
  }
}

void wxMaxima::AddDrawParameter(wxString cmd, int dimensionsOfNewDrawCommand)
{
  if(!m_drawPane)
    return;

  int dimensions = 0;
  dimensions = m_drawPane->GetDimensions();

  if(dimensions < 2)
  {
    if(dimensionsOfNewDrawCommand < 3)
      cmd = wxT("wxdraw2d(\n    ") + cmd + wxT("\n)$");
    else
      cmd = wxT("wxdraw3d(\n    ") + cmd + wxT("\n)$");
    m_worksheet->OpenHCaret(cmd);
    m_worksheet->GetActiveCell()->SetCaretPosition(
      m_worksheet->GetActiveCell()->GetCaretPosition() - 3);
  }
  else
  {
    if(m_worksheet->GetActiveCell())
    {
      m_worksheet->GetActiveCell()->AddDrawParameter(cmd);
      m_worksheet->Recalculate();
      m_worksheet->RequestRedraw();
    }
  }
  m_worksheet->SetFocus();
}

void wxMaxima::DrawMenu(wxCommandEvent &event)
{
  if(!m_drawPane)
    return;

  UpdateDrawPane();
  int dimensions = m_drawPane->GetDimensions();

  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr;
  if(dimensions < 2)
    expr = GetDefaultEntry();
  else
    expr = "%";

  wxString cmd;
  switch (event.GetId())
  {
  case menu_draw_2d:
  {
    DrawWiz *wiz = new DrawWiz(this, m_worksheet->m_configuration, 2);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      m_worksheet->SetFocus();

      m_worksheet->OpenHCaret(wiz->GetValue());
      m_worksheet->GetActiveCell()->SetCaretPosition(
        m_worksheet->GetActiveCell()->GetCaretPosition() - 3);
    }
    wiz->Destroy();
    break;
  }
  case menu_draw_3d:
    if(dimensions < 2)
    {
      DrawWiz *wiz = new DrawWiz(this, m_worksheet->m_configuration, 3);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        m_worksheet->SetFocus();

        m_worksheet->OpenHCaret(wiz->GetValue());
        m_worksheet->GetActiveCell()->SetCaretPosition(
        m_worksheet->GetActiveCell()->GetCaretPosition() - 3);
      }
      wiz->Destroy();
      break;
    }
    else
    {
      Wiz3D *wiz = new Wiz3D(this, m_worksheet->m_configuration);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
        AddDrawParameter(wiz->GetValue());
      wiz->Destroy();
      break;
    }
  case menu_draw_fgcolor:
  {
    wxColour col = wxGetColourFromUser(this);
    if (col.IsOk())
      AddDrawParameter(
        wxString::Format("color=\"#%02x%02x%02x\"",
                         col.Red(),col.Green(),col.Blue()));
    break;
  }
  case menu_draw_fillcolor:
  {
    wxColour col = wxGetColourFromUser(this);
    if (col.IsOk())
      AddDrawParameter(
        wxString::Format("fill_color=\"#%02x%02x%02x\"",
                         col.Red(),col.Green(),col.Blue()));
  break;
  }
  case menu_draw_title:
  {
    Gen1Wiz *wiz = new Gen1Wiz(this, -1, m_worksheet->m_configuration,
                               _("Set the diagram title"),
                               _("Title (Sub- and superscripts as x_{10} or x^{10})"),expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("title=\"") + wiz->GetValue() + wxT("\"");
      AddDrawParameter(cmd);
    }
    wiz->Destroy();
    break;
  }
  case menu_draw_key:
  {
    Gen1Wiz *wiz = new Gen1Wiz(this, -1, m_worksheet->m_configuration,
                               _("Set the next plot's title. Empty = no title."),
                               _("Title (Sub- and superscripts as x_{10} or x^{10})"),expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("key=\"") + wiz->GetValue() + wxT("\"");
      AddDrawParameter(cmd);
    }
    wiz->Destroy();
    break;
  }
  case menu_draw_explicit:
  {
    ExplicitWiz *wiz = new ExplicitWiz(this, m_worksheet->m_configuration, expr, dimensions);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue());
    wiz->Destroy();
    break;
  }

  case menu_draw_implicit:
  {
    ImplicitWiz *wiz = new ImplicitWiz(this, m_worksheet->m_configuration, expr, dimensions);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue());
    wiz->Destroy();
    break;
  }

  case menu_draw_parametric:
  {
    ParametricWiz *wiz = new ParametricWiz(this, m_worksheet->m_configuration, dimensions);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue());
    wiz->Destroy();
    break;
  }

  case menu_draw_points:
  {
    WizPoints *wiz = new WizPoints(this, m_worksheet->m_configuration, dimensions, expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue());
    wiz->Destroy();
    break;
  }

  case menu_draw_grid:
  {
    Gen2Wiz *wiz = new Gen2Wiz(
                               _("x direction [in multiples of the tick frequency]"),
                               _("y direction [in multiples of the tick frequency]"),
                               "1","1",
                               m_worksheet->m_configuration, this, -1,
                               _("Set the grid density.")
      );
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("grid=[") + wiz->GetValue1() + "," + wiz->GetValue2() + wxT("]");
      AddDrawParameter(cmd);
    }
    wiz->Destroy();
    break;
  }

  case menu_draw_axis:
  {
    AxisWiz *wiz = new AxisWiz(this, m_worksheet->m_configuration, dimensions);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      AddDrawParameter(wiz->GetValue());
    }
    wiz->Destroy();
    break;
  }

  case menu_draw_contour:
  {
    WizContour *wiz = new WizContour(this, m_worksheet->m_configuration);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue(), 3);
    wiz->Destroy();
    break;
  }

  case menu_draw_accuracy:
  {
    WizDrawAccuracy *wiz = new WizDrawAccuracy(this, m_worksheet->m_configuration, dimensions);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
      AddDrawParameter(wiz->GetValue(), dimensions);
    wiz->Destroy();
    break;
  }

  }
}

void wxMaxima::ListMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case menu_csv2list:
    {
      CsvImportWiz *wiz = new CsvImportWiz(this, m_worksheet->m_configuration);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("read_nested_list(\"") + wiz->GetFilename() +
          wxT("\", ") +
          wiz->GetSeparator() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();      
      break;
    }
    case menu_list2csv:
    {
      CsvExportWiz *wiz = new CsvExportWiz(this, m_worksheet->m_configuration, _("List"));
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("write_data(") +
          wiz->GetMatrix() +
          wxT(", \"") +
          wiz->GetFilename() +
          wxT("\", ") +
          wiz->GetSeparator() +
          wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();      
      break;
    }
  case menu_list_create_from_args:
  {
    wxString arg;
    Gen1Wiz *wiz = new Gen1Wiz(this, -1, m_worksheet->m_configuration,
                               _("Extract function arguments"),
                               _("The function call whose arguments to extract"),
                               expr);
    wiz->SetLabel1ToolTip(_("Something like f(x_1,x_2)"));
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("args(") + wiz->GetValue() + wxT(")");
      MenuCommand(cmd);
    }
    wiz->Destroy();
  }
    break;
  case menu_list_list2matrix:
    MenuCommand(wxT("apply('matrix,") + expr + wxT(")"));
    break;
  case menu_list_matrix2list:
    MenuCommand(wxT("args(") + expr + wxT(")"));
    break;
  case menu_list_create_from_elements:
  {
    Gen1Wiz *wiz = new Gen1Wiz(this, -1, m_worksheet->m_configuration,
                               _("Create list from comma-separated elements"),
                               _("Comma-separated elements"),expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("[") + wiz->GetValue() + wxT("]");
      MenuCommand(cmd);
    }
    wiz->Destroy();
  }
  break;
  case menu_list_create_from_rule:
  {
    Gen5Wiz *wiz = new Gen5Wiz(_("Rule:"), _("Index variable:"),
                               _("Index Start:"), _("Index End:"), _("Index Step:"),
                               expr, wxT("i"), wxT("1"), wxT("100"), wxT("1"),
                               m_worksheet->m_configuration,
                               this, -1, _("Create a list from a rule"), true);
    wiz->SetLabel1ToolTip(_("The rule that explains how to generate the value of a list item.\n"
                            "Might be something like \"i\", \"i^2\" or \"sin(i)\""));
    wiz->SetLabel2ToolTip(_("The number of the item which is stepped from \"Index Start\" to \"Index End\"."));
    wiz->SetValue(expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wxT("makelist(") + wiz->GetValue1() + wxT(", ") +
        wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(", ") +
        wiz->GetValue4();
      wxString tst = wiz->GetValue5();
      tst.Trim(true);
      tst.Trim(false);
      if(tst != wxT("1"))
      val += wxT(",") + wiz->GetValue5();
      val += wxT(")");
      MenuCommand(val);
    }
    wiz->Destroy();
  }
    break;
  case menu_list_create_from_list:
  {
    Gen3Wiz *wiz = new Gen3Wiz(_("Rule:"), _("Iterator:"),
                               _("Source list:"),
                               expr, wxT("i"), wxT("list"),
                               m_worksheet->m_configuration,
                               this, -1, _("Create a list from another list"), true);
    wiz->SetLabel1ToolTip(_("The rule that explains how to generate the value of a list item.\n"
                            "Might be something like \"i\", \"i^2\" or \"sin(i)\""));
    wiz->SetLabel2ToolTip(_("The variable the value of the current source item is stored in."));
    wiz->SetValue(expr);
    //wiz->Centre(wxBOTH);    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wxT("makelist(") + wiz->GetValue1() + wxT(", ") +
        wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(")");
      MenuCommand(val);
    }
    wiz->Destroy();
  }
    break;
  case menu_list_actual_values_storage:
  {
    ActualValuesStorageWiz *wiz = new ActualValuesStorageWiz(m_worksheet->m_configuration,
                               this, -1, _("Create a list as a storage for the values of variables"));
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      MenuCommand(wiz->GetValue());
    }
    wiz->Destroy();
  }
    break;
  case menu_list_sort:
  {
    ListSortWiz *wiz = new ListSortWiz(m_worksheet->m_configuration,
                                       this, -1, _("Sort a list"), expr);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      MenuCommand(wiz->GetValue());
    }
    wiz->Destroy();
  }
    break;
  case menu_list_length:
    MenuCommand(wxT("length(") + expr + wxT(")"));
    break;
  case menu_list_push:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List:"), _("Element:"),
                                 expr, wxT("1"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("LCM"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("push(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_pop:
    MenuCommand(wxT("pop(") + expr + wxT(")"));
    break;
  case menu_list_reverse:
    MenuCommand(wxT("reverse(") + expr + wxT(")"));
    break;
  case menu_list_first:
    MenuCommand(wxT("first(") + expr + wxT(")"));
    break;
  case menu_list_last:
    MenuCommand(wxT("last(") + expr + wxT(")"));
    break;
  case menu_list_rest:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("n"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Return the list without its last n elements"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("rest(") + wiz->GetValue1();
        wxString num = wiz->GetValue2();
        num.Trim(true);
        num.Trim(false);
        if(num != wxT("1"))
        {
          cmd += wxT(",") + wiz->GetValue2();
        }
        cmd += wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_restN:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("n"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Return the list without its first n elements"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("rest(") + wiz->GetValue1();
        wxString num = wiz->GetValue2();
        num.Trim(true);
        num.Trim(false);
        cmd += wxT(", -") + num + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_lastn:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("Number of elements"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract the last n elements from a list"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("rest(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_nth:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("element number n"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Extract the nth element from a list. Slow for n>>0"),
                                 true,
                                 _("This function is slow for large n.\n"
                                   "For efficiently iterating through every element of a large list see \"Create list from list\" instead, which uses the makelist command."),
                                 _("Other than declared arrays in lists there is no way to jump to "
                                   "determine the address of the nth element other than iterating "
                                   "from one element to the other until the nth element is reached. "
                                   "Which isn't a maxima-specific phenomenon but the price one has "
                                   "to pay for lists being way easier to resize than declared "
                                   "arrays. If the address of the current element is known "
                                   "iterating to the next one is trivial, though, so\n\n"
                                   "   for i in list do <something>\n\n"
                                   "or\n\n"
                                   "   makelist(expression,i,list)\n\n"
                                   "provide highly efficient ways to do something on every list "
                                   "element.")
        );
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wiz->GetValue1() + wxT("[")
          + wiz->GetValue2() + wxT("]");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
  break;
  case menu_list_map:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function"), _("List"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Apply a function to each list element"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("map(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_use_actual_values:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation"), _("List with values"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Introduce a list of actual values into an equation"), true);
      wiz->SetLabel2ToolTip(_("The list with values can be generated by \"solve()\" or using "
                              "\"Create list/as storage for actual values for variables\"."));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("subst(") + wiz->GetValue2() + wxT(",")
          + wiz->GetValue1() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_extract_value:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("Variable name"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Extract a variable's value from a list of variable values"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("subst(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_as_function_arguments:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function name"), _("List"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Use a list as parameter list for a function"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("apply(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_do_for_each_element:
  {
    Gen3Wiz *wiz = new Gen3Wiz(_("List:"), _("Iterator:"),
                               _("What to do:"),
                               expr, wxT("i"), wxT("disp(i)"),
                               m_worksheet->m_configuration,
                               this, -1, _("Do for each list element"), true);
    wiz->SetValue(expr);
    wiz->SetLabel2ToolTip(_("The variable the value of the current source item is stored in."));
    wiz->SetLabel3ToolTip(_("Either a single expression or a comma-separated list of expressions "
                            "between parenthesis. In the latter case the result of the last "
                            "expression in the parenthesis is used."));
    //wiz->Centre(wxBOTH);    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wxT("for ") + wiz->GetValue2() + wxT(" in ") +
        wiz->GetValue1() + wxT(" do ") + wiz->GetValue3();
      MenuCommand(val);
    }
    wiz->Destroy();
  }
    break;
  case menu_list_remove_duplicates:
    MenuCommand(wxT("unique(") + expr + wxT(")"));
    break;
  case menu_list_remove_element:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Element"), _("List"),
                                 wxT("1"), expr,
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Remove an element from a list"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("delete(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_append_item:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List"), _("Item"),
                                 expr, wxT("1"),
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Add an element to a list"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("append(") + wiz->GetValue1() + wxT(",[")
          + wiz->GetValue2() + wxT("])");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_append_list:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List1"), _("List2"),
                                 expr, wxT("[1]"),
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Append a list to a list"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("append(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_list_interleave:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("List1"), _("List2"),
                                 expr, wxT("[1]"),
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Interleave two lists"),
                                 true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("join(") + wiz->GetValue1() + wxT(",")
          + wiz->GetValue2() + wxT(")");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  }
}

void wxMaxima::SimplifyMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case menu_nouns:
      cmd = wxT("ev(") + expr + wxT(", nouns);");
      MenuCommand(cmd);
      break;
    case button_ratsimp:
    case menu_ratsimp:
      cmd = wxT("ratsimp(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_radcan:
    case menu_radsimp:
      cmd = wxT("radcan(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_to_fact:
      cmd = wxT("makefact(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_to_gamma:
      cmd = wxT("makegamma(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_factcomb:
      cmd = wxT("factcomb(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_factsimp:
      cmd = wxT("minfactorial(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_logcontract:
      cmd = wxT("logcontract(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_logexpand:
      cmd = expr + wxT(", logexpand=super;");
      MenuCommand(cmd);
      break;
    case button_expand:
    case menu_expand:
      cmd = wxT("expand(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_factor:
    case menu_factor:
      cmd = wxT("factor(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_gfactor:
      cmd = wxT("gfactor(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_trigreduce:
    case menu_trigreduce:
      cmd = wxT("trigreduce(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_trigsimp:
    case menu_trigsimp:
      cmd = wxT("trigsimp(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_trigexpand:
    case menu_trigexpand:
      cmd = wxT("trigexpand(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_trigrat:
    case button_trigrat:
      cmd = wxT("trigrat(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case button_rectform:
    case menu_rectform:
      cmd = wxT("rectform(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_polarform:
      cmd = wxT("polarform(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_demoivre:
      cmd = wxT("demoivre(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_exponentialize:
      cmd = wxT("exponentialize(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_realpart:
      cmd = wxT("realpart(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_imagpart:
      cmd = wxT("imagpart(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_talg:
      if(event.IsChecked())
        cmd = wxT("algebraic:true$");
      else
        cmd = wxT("algebraic:false$");
      MenuCommand(cmd);
      break;
    case menu_tellrat:
      cmd = GetTextFromUser(_("Enter an equation for rational simplification:"),
                            _("Tellrat"),
                            m_worksheet->m_configuration,
                            wxEmptyString, this);
      if (cmd.Length())
      {
        cmd = wxT("tellrat(") + cmd + wxT(");");
        MenuCommand(cmd);
      }
      break;
    case menu_modulus:
      cmd = GetTextFromUser(_("Calculate modulus:"),
                            _("Modulus"),
                            m_worksheet->m_configuration,
                            wxT("false"), this);
      if (cmd.Length())
      {
        cmd = wxT("modulus : ") + cmd + wxT(";");
        MenuCommand(cmd);
      }
      break;
    default:
      break;
  }
}

void wxMaxima::CalculusMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case menu_change_var:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Integral/Sum:"), _("Old variable:"),
                                 _("New variable:"), _("Equation:"),
                                 expr, wxT("x"), wxT("y"), wxT("y=x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Change variable"), true);
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("changevar(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue4() + wxT(", ") + wiz->GetValue3() + wxT(", ") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_pade:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Taylor series:"), _("Num. deg:"),
                                 _("Denom. deg:"), expr, wxT("4"), wxT("4"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Pade approximation"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("pade(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_continued_fraction:
      cmd += wxT("cfdisrep(cf(") + expr + wxT("));");
      MenuCommand(cmd);
      break;
    case menu_lcm:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxEmptyString, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("LCM"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("lcm(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_gcd:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxEmptyString, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("GCD"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("gcd(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_divide:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 expr, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Divide"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("divide(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_partfrac:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("Variable:"),
                                 expr, wxT("n"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Partial fractions"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("partfrac(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_risch:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("Variable:"),
                                 expr, wxT("x"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Integrate (risch)"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("risch(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case button_integrate:
    case menu_integrate:
    {
      IntegrateWiz *wiz = new IntegrateWiz(this, -1, m_worksheet->m_configuration, _("Integrate"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_laplace:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Old variable:"),
                                 _("New variable:"), expr, wxT("t"), wxT("s"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Laplace"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("laplace(") + wiz->GetValue1() + wxT(", ")
                       + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_ilt:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Old variable:"),
                                 _("New variable:"), expr, wxT("s"), wxT("t"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Inverse Laplace"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ilt(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case button_diff:
    case menu_diff:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Variable(s):"),
                                 _("Times:"), expr, wxT("x"), wxT("1"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Differentiate"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxStringTokenizer vars(wiz->GetValue2(), wxT(","));
        wxStringTokenizer times(wiz->GetValue3(), wxT(","));

        wxString val = wxT("diff(") + wiz->GetValue1();

        while (vars.HasMoreTokens() && times.HasMoreTokens())
        {
          val += wxT(",") + vars.GetNextToken();
          val += wxT(",") + times.GetNextToken();
        }

        val += wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case button_taylor:
    case menu_series:
    {
      SeriesWiz *wiz = new SeriesWiz(this, -1, m_worksheet->m_configuration, _("Series"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case button_limit:
    case menu_limit:
    {
      LimitWiz *wiz = new LimitWiz(this, -1, m_worksheet->m_configuration, _("Limit"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_lbfgs:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Expression:"),
                                 _("Variables:"),
                                 _("Initial Estimates:"),
                                 _("Epsilon:"),
                                 expr, wxT("x"), wxT("1.0"), wxT("1e-4"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Find minimum"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("lbfgs(") + wiz->GetValue1() + wxT(", [") +
              wiz->GetValue2() + wxT("], [") +
              wiz->GetValue3() + wxT("], ") +
              wiz->GetValue4() + wxT(", [-1,0]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case button_sum:
    case menu_sum:
    {
      SumWiz *wiz = new SumWiz(this, -1, m_worksheet->m_configuration, _("Sum"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case button_product:
    case menu_product:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Expression:"), _("Variable:"), _("From:"),
                                 _("To:"), expr, wxT("k"), wxT("1"), wxT("n"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Product"));
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("product(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    default:
      break;
  }
}

void wxMaxima::PlotMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case button_plot3:
    case gp_plot3:
    {
      Plot3DWiz *wiz = new Plot3DWiz(this, -1, m_worksheet->m_configuration, _("Plot 3D"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_animationautostart:
      if(event.IsChecked())
        MenuCommand(wxT("wxanimate_autoplay:true$"));
      else
        MenuCommand(wxT("wxanimate_autoplay:false$"));
      break;
    case menu_animationframerate:
    {
      cmd = GetTextFromUser(_("Enter new animation frame rate [Hz, integer]:"), _("Frame rate"),
                            m_worksheet->m_configuration,
                            wxT("2"), this);
      wxRegEx number("^[0-9][0-9]*$");

      if (number.Matches(cmd))
      {
        cmd = wxT("wxanimate_framerate : ") + cmd + wxT(";");
        MenuCommand(cmd);
      }
    }
    break;
    case button_plot2:
    case gp_plot2:
    {
      Plot2DWiz *wiz = new Plot2DWiz(this, -1, m_worksheet->m_configuration, _("Plot 2D"));
      wiz->SetValue(expr);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
      break;
    case menu_plot_format:
    {
      PlotFormatWiz *wiz = new PlotFormatWiz(this, -1, m_worksheet->m_configuration, _("Plot format"));
      wiz->Center(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        MenuCommand(wiz->GetValue());
      }
      wiz->Destroy();
      /*wxString format = GetTextFromUser(_("Enter new plot format:"),
      _("Plot format"),
      m_worksheet->m_configuration,
      wxT("gnuplot"), this);
      if (format.Length())
      {
      MenuCommand(wxT("set_plot_option(['plot_format, '") + format +
      wxT("])$"));
      }*/
    }
    default:
      break;
  }
}

void wxMaxima::NumericalMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
    case Worksheet::popid_special_constant_percent:
    {
      m_worksheet->m_configuration->SetKeepPercent(event.IsChecked());
      m_worksheet->RequestRedraw();
      break;
    }
    case Worksheet::popid_hideasterisk:
    {
      m_worksheet->m_configuration->HidemultiplicationSign(event.IsChecked());
      m_worksheet->GetTree()->ResetDataList();
      m_worksheet->RequestRedraw();
      break;
    }
    case Worksheet::popid_changeasterisk:
    {
      m_worksheet->m_configuration->SetChangeAsterisk(event.IsChecked());
      m_worksheet->GetTree()->ResetDataList();
      m_worksheet->RequestRedraw();
      break;
    }
  case menu_num_domain:
      if(event.IsChecked())
        cmd = wxT("domain:'complex$");
      else
        cmd = wxT("domain:'real$");
      MenuCommand(cmd);
      break;
    case menu_to_float:
      cmd = wxT("float(") + expr + wxT("), numer;");
      MenuCommand(cmd);
      break;
    case menu_to_bfloat:
      cmd = wxT("bfloat(") + expr + wxT(");");
      MenuCommand(cmd);
      break;
    case menu_to_numer:
      cmd = expr + wxT(",numer;");
      MenuCommand(cmd);
      break;
    case menu_num_out:
      if(!event.IsChecked())
        cmd = wxT("numer:false$");
      else
        cmd = wxT("numer:true$");
      MenuCommand(cmd);
      break;
    case menu_set_precision:
      cmd = GetTextFromUser(_("Enter new precision for bigfloats:"), _("Precision"),
                            m_worksheet->m_configuration,
                            wxT("16"), this);
      if (cmd.Length())
      {
        cmd = wxT("fpprec : ") + cmd + wxT(";");
        MenuCommand(cmd);
      }
      break;
    case menu_set_displayprecision:
      cmd = GetTextFromUser(_("How many digits to show:"), _("Displayed Precision"),
                            m_worksheet->m_configuration,
                            wxT("0"), this);
      if (cmd.Length())
      {
        cmd = wxT("fpprintprec : ") + cmd + wxT(";");
        MenuCommand(cmd);
      }
      break;
  case menu_engineeringFormat:
    if((m_maximaVariable_engineeringFormat != wxT("true")) &&
       (m_maximaVariable_engineeringFormat != wxT("false")))
      MenuCommand(wxT("load(\"engineering-format\")$"));
    if (m_maximaVariable_engineeringFormat == wxT("true"))
      MenuCommand(wxT("engineering_format_floats:false$"));
    if (m_maximaVariable_engineeringFormat == wxT("false"))
      MenuCommand(wxT("engineering_format_floats:true$"));
    break;
  case menu_engineeringFormatSetup:
  {
    Gen4Wiz *wiz = new Gen4Wiz(_("Enable:"),
                               _("Minimum absolute value printed without exponent:"),
                               _("Maximum absolute value printed without exponent:"),
                               _("Maximum number of digits to be displayed:"),
                               wxT("true"), wxT(".01"), wxT("1000"), wxT("6"),
                               m_worksheet->m_configuration,
                               this, -1, _("Engineering format setup"));
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      cmd = wxT("engineering_format_floats: ") + wiz->GetValue1() + wxT("$\n") +
        wxT("engineering_format_min: ") + wiz->GetValue2() + wxT("$\n") +
        wxT("engineering_format_max: ") + wiz->GetValue3() + wxT("$\n") +
        wxT("fpprintprec: ") + wiz->GetValue4() + wxT("$");
      MenuCommand(cmd);
    }
    wiz->Destroy();
  }
  break;
  default:
    break;
  }
}

void wxMaxima::HelpMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;

  switch (event.GetId())
  {
    case wxID_ABOUT:
    {
      wxAboutDialogInfo info;
      wxString description;

      description = _("wxMaxima is a cross-platform graphical user interface for the computer algebra system Maxima based on wxWidgets.\nFor rendering svg graphics it uses nanosvg (https://github.com/memononen/nanosvg).\nThe unicode character list has been compiled by the Unicode Consortium.");

#if defined(WXMAXIMA_GIT_VERSION)
      description += wxString::Format("\n(Build from Git version: " WXMAXIMA_GIT_VERSION ")");
#endif
      description += wxString::Format(
        _("\n\nwxWidgets: %d.%d.%d\nUnicode support: %s"),
        wxMAJOR_VERSION, wxMINOR_VERSION, wxRELEASE_NUMBER,
        _("yes").utf8_str()
        );
      #ifdef HAVE_OPENMP_TASKS
      description += wxString::Format(_("\nMultiprocessing using OpenMP %s"), OPENMP_SPEC_DATE);
      #ifdef HAVE_OMP_HEADER
      description += _("\nUsing fine-grained OpenMP locks");
      #else
      description += _("\nNo fine-grained OpenMP locks built in");
      #endif
      #else
      description += _("\nNo OpenMP support");
      #endif
      
      if (m_maximaVersion != wxEmptyString)
        description += _("\nMaxima version: ") + m_maximaVersion + " ("+m_maximaArch+")";
      else
        description += _("\nNot connected.");
      if (m_lispVersion != wxEmptyString)
        description += _("\nMaxima compiled using: ") + m_worksheet->m_configuration->LispType() +
          " " + m_lispVersion;

      info.SetIcon(wxMaximaIcon());
      info.SetDescription(description);
      info.SetName(_("wxMaxima"));
      info.SetVersion(wxT(GITVERSION));
      info.SetCopyright(wxT("(C) 2004-2018 Andrej Vodopivec"));
      info.SetWebSite(wxT("https://wxMaxima-developers.github.io/wxmaxima/"));

      info.AddDeveloper(wxT("Andrej Vodopivec <andrej.vodopivec@gmail.com>"));
      info.AddDeveloper(wxT("Gunter Königsmann <wxMaxima@physikbuch.de>"));
      info.AddDeveloper(wxT("Wolfgang Dautermann"));
      info.AddDeveloper(wxT("Ziga Lenarcic <ziga.lenarcic@gmail.com>"));
      info.AddDeveloper(wxT("Doug Ilijev <doug.ilijev@gmail.com>"));
      info.AddDeveloper(wxT("Elias Mårtenson"));
      info.AddDeveloper(wxT("Steven McDonald"));
      info.AddDeveloper(wxT("Tomio Arisaka"));
      info.AddDeveloper(wxT("Rene Hansen"));
      info.AddDeveloper(wxT("Adam Mróz"));
      info.AddDeveloper(wxT("Mizuki Arata"));
      info.AddDeveloper(wxT("Atri Bhattacharya"));
      info.AddDeveloper(wxT("aullan"));
      info.AddDeveloper(wxT("Belkacem Mohammed"));
      info.AddDeveloper(wxT("Benito van der Zander"));
      info.AddDeveloper(wxT("beshenov"));
      info.AddDeveloper(wxT("BlackEdder"));
      info.AddDeveloper(wxT("Blahota István"));
      info.AddDeveloper(wxT("Christian Gosch"));
      info.AddDeveloper(wxT("Christian Wolf"));
      info.AddDeveloper(wxT("crategus <crategus@users.sourceforge.net>"));
      info.AddDeveloper(wxT("cucharro <cucharro@users.sourceforge.net>"));
      info.AddDeveloper(wxT("cw.ahbong <cw.ahbong@gmail.com>"));
      info.AddDeveloper(wxT("Dave <dave@dave-VirtualBox.(none)>"));
      info.AddDeveloper(wxT("Drew <5396418+djh101@users.noreply.github.com>"));
      info.AddDeveloper(wxT("Eduardo M. Kalinowski"));
      info.AddDeveloper(wxT("endolith <endolith@gmail.com>"));
      info.AddDeveloper(wxT("Frank S. Thomas"));
      info.AddDeveloper(wxT("Frédéric Chapoton"));
      info.AddDeveloper(wxT("fweng <fweng@users.sourceforge.net>"));
      info.AddDeveloper(wxT("hut <hut@lepus.uberspace.de>"));
      info.AddDeveloper(wxT("Innocent De Marchi"));
      info.AddDeveloper(wxT("Johan Kristensen"));
      info.AddDeveloper(wxT("Jonathan Wakely"));
      info.AddDeveloper(wxT("Mika Kahkonen"));
      info.AddDeveloper(wxT("Kuba Ober"));
      info.AddDeveloper(wxT("Lauri Nurmi"));
      info.AddDeveloper(wxT("Lennart Ferlemann"));
      info.AddDeveloper(wxT("Lennart Jern"));
      info.AddDeveloper(wxT("Liu Lizhi"));
      info.AddDeveloper(wxT("Max Musatov"));
      info.AddDeveloper(wxT("Marco Ciampa"));
      info.AddDeveloper(wxT("Mario Rodriguez"));
      info.AddDeveloper(wxT("Mariusz Libera"));
      info.AddDeveloper(wxT("Matcha Zaq"));
      info.AddDeveloper(wxT("Mihai Moldovan"));
      info.AddDeveloper(wxT("MSoegtropIMC"));
      info.AddDeveloper(wxT("Murmele <Murmele@users.noreply.github.com>"));
      info.AddDeveloper(wxT("Nicola Stanislao Vitale"));
      info.AddDeveloper(wxT("Olesya Gerasimenko"));
      info.AddDeveloper(wxT("Robert Dodier"));
      info.AddDeveloper(wxT("Robert Pollak"));
      info.AddDeveloper(wxT("rtopolnicki <rtopolnicki@users.sourceforge.net>"));
      info.AddDeveloper(wxT("scx <scx.mail@gmail.com>"));
      info.AddDeveloper(wxT("Simon Ruderich"));
      info.AddDeveloper(wxT("Tufan Şirin"));
      info.AddDeveloper(wxT("vvzhy <vvzhy@users.sourceforge.net>"));
      info.AddDeveloper(wxT("Warren MacEvoy"));
      info.AddDeveloper(wxT("Yinhe Zhang"));
      info.AddDeveloper(wxT("Yuri Chornoivan"));
      info.AddDeveloper(wxT("zufus <zufus@users.sourceforge.net>"));      
      info.AddDeveloper(wxT("The team behind nanoSVG"));
      info.AddTranslator(wxT("Innocent De Marchi (ca)"));
      info.AddTranslator(wxT("Josef Barak (cs)"));
      info.AddTranslator(wxT("Robert Marik (cs)"));
      info.AddTranslator(wxT("Jens Thostrup (da)"));
      info.AddTranslator(wxT("Harald Geyer (de)"));
      info.AddTranslator(wxT("Dieter Kaiser (de)"));
      info.AddTranslator(wxT("Gunter Königsmann (de)"));
      info.AddTranslator(wxT("Wolfgang Dautermann (de)"));
      info.AddTranslator(wxT("Alkis Akritas (el)"));
      info.AddTranslator(wxT("Evgenia Kelepesi-Akritas (el)"));
      info.AddTranslator(wxT("Kostantinos Derekas (el)"));
      info.AddTranslator(wxT("Mario Rodriguez Riotorto (es)"));
      info.AddTranslator(wxT("Antonio Ullan (es)"));
      info.AddTranslator(wxT("Eric Delevaux (fr)"));
      info.AddTranslator(wxT("Michele Gosse (fr)"));
      info.AddTranslator(wxT("Blahota István (hu)"));
      info.AddTranslator(wxT("Marco Ciampa (it)"));
      info.AddTranslator(wxT("Asbjørn Apeland (nb)"));
      info.AddTranslator(wxT("Rafal Topolnicki (pl)"));
      info.AddTranslator(wxT("Eduardo M. Kalinowski (pt_br)"));
      info.AddTranslator(wxT("Alexey Beshenov (ru)"));
      info.AddTranslator(wxT("Vadim V. Zhytnikov (ru)"));
      info.AddTranslator(wxT("Tufan Şirin (tr)"));
      info.AddTranslator(wxT("Sergey Semerikov (uk)"));
      info.AddTranslator(wxT("Frank Weng (zh_TW)"));
      info.AddTranslator(wxT("cw.ahbong (zh_TW)"));

      info.AddArtist(wxT("wxMaxima icon: Sven Hodapp"));
      info.AddArtist(wxT("SVG version of the icon: Gunter Königsmann"));
      info.AddArtist(wxT("Other icons: The TANGO Project"));

      wxAboutBox(info);
    }
    break;

    case menu_license:
    {
      LicenseDialog *dlg = new LicenseDialog(this);
      dlg->Show();
    }
    break;

    case wxID_HELP:
      ShowHelp(expr);
      break;

    case menu_wxmaximahelp:
      ShowWxMaximaHelp();
      break;

    case menu_maximahelp:
      ShowMaximaHelp(expr);
      break;

    case menu_example:
      if (expr == wxT("%"))
        cmd = GetTextFromUser(_("Show an example for the command:"), _("Example"),
                              m_worksheet->m_configuration,
                              wxEmptyString, this);
      else
        cmd = expr;
      if (cmd.Length())
      {
        cmd = wxT("example(") + cmd + wxT(");");
        MenuCommand(cmd);
      }
      break;

    case menu_apropos:
      if (expr == wxT("%"))
        cmd = GetTextFromUser(_("Show all commands similar to:"), _("Apropos"),
                              m_worksheet->m_configuration,
                              wxEmptyString, this);
      else
        cmd = expr;
      if (cmd.Length())
      {
        cmd = wxT("apropos(\"") + cmd + wxT("\");");
        MenuCommand(cmd);
      }
      break;

    case menu_show_tip:
      ShowTip(true);
      break;

    case menu_build_info:
      MenuCommand(wxT("build_info();"));
      break;

    case menu_bug_report:
      MenuCommand(wxT("wxbug_report()$"));
      break;

    case menu_help_tutorials:
      wxLaunchDefaultBrowser(wxT("https://wxMaxima-developers.github.io/wxmaxima/help.html"));
      break;

    case menu_check_updates:
      CheckForUpdates(true);
      break;

    default:
      break;
  }
}

void wxMaxima::StatsMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();

  switch (event.GetId())
  {
    case menu_stats_histogram:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Data:"), _("Classes:"),
                                 expr, wxT("10"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Histogram"), false);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("wxhistogram(") + wiz->GetValue1() + wxT(", nclasses=") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_stats_scatterplot:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Data:"), _("Classes:"),
                                 expr, wxT("10"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Scatterplot"), false);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("wxscatterplot(") + wiz->GetValue1() + wxT(", nclasses=") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_stats_barsplot:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("wxbarsplot(") + data + wxT(");"));
    }
      break;
    case menu_stats_boxplot:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("wxboxplot([") + data + wxT("]);"));
    }
      break;
    case menu_stats_piechart:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("wxpiechart(") + data + wxT(");"));
    }
      break;
    case menu_stats_mean:
    {

      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("mean(") + data + wxT(");"));
    }
      break;
    case menu_stats_median:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("median(") + data + wxT(");"));
    }
      break;
    case menu_stats_var:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("var(") + data + wxT(");"));
    }
      break;
    case menu_stats_dev:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("std(") + data + wxT(");"));
    }
      break;
    case menu_stats_tt1:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Sample:"), _("Mean:"),
                                 expr, wxT("0"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("One sample t-test"), false);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("test_mean(") + wiz->GetValue1() + wxT(", mean=") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_stats_tt2:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Sample 1:"), _("Sample 2:"),
                                 wxEmptyString, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1,
                                 _("Two sample t-test"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("test_means_difference(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_stats_tnorm:
    {
      wxString data = GetTextFromUser(_("Data:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("test_normality(") + data + wxT(");"));
    }
      break;
    case menu_stats_linreg:
    {

      wxString data = GetTextFromUser(_("Data Matrix:"), _("Enter Data"),
                                      m_worksheet->m_configuration,
                                      expr, this);
      if (data.Length() > 0)
        MenuCommand(wxT("simple_linear_regression(") + data + wxT(");"));
    }
      break;
    case menu_stats_lsquares:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Data Matrix:"), _("Col. names:"),
                                 _("Equation:"), _("Variables:"),
                                 expr, wxT("x,y"), wxT("y=A*x+B"), wxT("A,B"),
                                 m_worksheet->m_configuration,
                                 this, -1, _("Least Squares Fit"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("lsquares_estimates(") + wiz->GetValue1() + wxT(", [") +
                       wiz->GetValue2() + wxT("], ") +
                       wiz->GetValue3() + wxT(", [") +
                       wiz->GetValue4() + wxT("], iprint=[-1,0]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
    case menu_stats_readm:
    {
      wxString file = wxFileSelector(_("Open matrix"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Data file (*.csv, *.tab, *.txt)|*.csv;*.tab;*.txt"),
                                     wxFD_OPEN);
      if (file != wxEmptyString)
      {
        m_lastPath = wxPathOnly(file);

#if defined __WXMSW__
        file.Replace(wxT("\\"), wxT("/"));
#endif

        wxString name = wxGetTextFromUser(wxT("Enter matrix name:"), wxT("Marix name"));
        wxString cmd;

        if (name != wxEmptyString)
          cmd << name << wxT(": ");

        wxString format;
        if (file.Lower().EndsWith(wxT(".csv")))
          format = wxT("csv");
        else if (file.Lower().EndsWith(wxT(".tab")))
          format = wxT("tab");

        if (format != wxEmptyString)
          MenuCommand(cmd + wxT("read_matrix(\"") + file + wxT("\", '") + format + wxT(");"));
        else
          MenuCommand(cmd + wxT("read_matrix(\"") + file + wxT("\");"));
      }
    }
    break;
    case menu_stats_subsample:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Data Matrix:"), _("Condition:"),
                                 _("Include columns:"), _("Matrix name:"),
                                 expr, wxT("col[1]#'NA"),
                                 wxEmptyString, wxEmptyString,
                                 m_worksheet->m_configuration,
                                 this, -1, _("Select Subsample"), true);
      //wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString name = wiz->GetValue4();

        wxString cmd;

        if (name != wxEmptyString)
          cmd << name << wxT(": ");

        cmd += wxT("subsample(\n   ") + wiz->GetValue1() + wxT(",\n   ") +
               wxT("lambda([col], is( ");

        if (wiz->GetValue2() != wxEmptyString)
          cmd += wiz->GetValue2() + wxT(" ))");
        else
          cmd += wxT("true ))");

        if (wiz->GetValue3() != wxEmptyString)
          cmd += wxT(",\n   ") + wiz->GetValue3();

        cmd += wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
      break;
  }
}

bool wxMaxima::SaveOnClose()
{
  if (!SaveNecessary())
  {
    wxLogMessage(_("No saving on close necessary."));
    return true;
  }

  // If we want to keep the file saved we automatically save the file on closing.
  if(m_worksheet->m_configuration->AutoSaveAsTempFile())
  {
    int close = SaveDocumentP();
    
    if (close == wxID_CANCEL)
      return false;
    else
    {
      if (close == wxID_YES)
      {
        if (!SaveFile())
        {
          if(!SaveFile(true))
            return false;
        }
      }
      return true;
    }
  }
  else
  {
    bool saved;
    {
      wxLogStderr blocker;
      saved = SaveFile();
    }
    if (!saved)
    {
      int close = SaveDocumentP();
        
      if (close == wxID_CANCEL)
        return false;
      else
      {
        if (close == wxID_YES)
        {
          if (!SaveFile())
          {
            if(!SaveFile(true))
              return false;
          }
        }
      }
    }
  }
  return true;
}


void wxMaxima::OnClose(wxCloseEvent &event)
{
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  
  if(event.GetEventType() == wxEVT_END_SESSION)
    KillMaxima();

  if(!SaveOnClose())
  {
    event.Veto();
    return;
  }
  
  // Stop log events from appearing on the log panel
  wxLogStderr blocker;

  // We have saved the file and will close now => No need to have the
  // timer around any longer.
  m_autoSaveTimer.Stop();
  m_closing = true;
  wxConfigBase *config = wxConfig::Get();
  if (m_lastPath.Length() > 0)
    config->Write(wxT("lastPath"), m_lastPath);
  KillMaxima();
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;
  // Allow the operating system to keep the clipboard's contents even after we
  // exit - if that option is supported by the OS.
  if(wxTheClipboard->Open())
  {
    wxTheClipboard->Flush();
    wxTheClipboard->Close();
  }
  event.Skip();
  if(m_fileSaved)
    RemoveTempAutosavefile();
  KillMaxima();
  MyApp::DelistTopLevelWindow(this);
}

void wxMaxima::PopupMenu(wxCommandEvent &event)
{
  m_worksheet->CloseAutoCompletePopup();

  wxString selection = m_worksheet->GetString();
  switch (event.GetId())
  {
  case enable_unicodePane:
    wxMaximaFrame::ShowPane(wxMaximaFrame::menu_pane_unicode, true);
  break;
  case Worksheet::popid_fold:
  {
    if (m_worksheet->GetActiveCell())
    {
      // This "if" is pure paranoia. But - since the costs of an "if" are low...
      GroupCell *group = m_worksheet->GetActiveCell()->GetGroup();
      if (group->IsFoldable())
        group->Fold();
      else
        group->Hide(true);
      m_worksheet->UpdateTableOfContents();
    }
    break;
  }
  case Worksheet::popid_maxsizechooser:
    if (m_worksheet->GetSelectionStart())
    {
      Cell *output = m_worksheet->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if(output->GetType() != MC_TYPE_IMAGE)
        return;

      MaxSizeChooser *chooser = new MaxSizeChooser(this, -1,
                                                   dynamic_cast<ImgCell *>(output)->GetMaxWidth(),
                                                   dynamic_cast<ImgCell *>(output)->GetHeightList()
        );
      chooser->Centre(wxBOTH);
      if (chooser->ShowModal() == wxID_OK)
      {
        if(dynamic_cast<ImgCell *>(output)->GetMaxWidth() != chooser->GetMaxWidth())
          m_fileSaved = false;
        if(dynamic_cast<ImgCell *>(output)->GetHeightList() != chooser->GetHeightList())
          m_fileSaved = false;

        dynamic_cast<ImgCell *>(output)->SetMaxWidth(chooser->GetMaxWidth());
        dynamic_cast<ImgCell *>(output)->SetMaxHeight(chooser->GetHeightList());
      }
    }
    m_worksheet->RecalculateForce();
    m_worksheet->RequestRedraw();
    break;
  case Worksheet::popid_unfold:
  {
    GroupCell *group = m_worksheet->GetActiveCell()->GetGroup();
    if (group->IsFoldable())
      group->Unfold();
    else
      group->Hide(false);
    m_worksheet->UpdateTableOfContents();
    break;
  }
  case TableOfContents::popid_Fold:
    if (m_worksheet->m_tableOfContents != NULL)
    {
      // We only update the table of contents when there is time => no guarantee that the
      // cell that was clicked at actually still is part of the tree.
      if ((m_worksheet->GetTree()) &&
          (m_worksheet->GetTree()->Contains(m_worksheet->m_tableOfContents->RightClickedOn())))
      {
        m_worksheet->m_tableOfContents->RightClickedOn()->Fold();
        m_worksheet->Recalculate();
        m_worksheet->RequestRedraw();
        m_worksheet->UpdateTableOfContents();
      }
    }
    break;
  case TableOfContents::popid_Unfold:
    if (m_worksheet->m_tableOfContents != NULL)
    {
      // We only update the table of contents when there is time => no guarantee that the
      // cell that was clicked at actually still is part of the tree.
      if ((m_worksheet->GetTree()) &&
          (m_worksheet->GetTree()->Contains(m_worksheet->m_tableOfContents->RightClickedOn())))
      {
        m_worksheet->m_tableOfContents->RightClickedOn()->Unfold();
        m_worksheet->Recalculate();
        m_worksheet->RequestRedraw();
        m_worksheet->UpdateTableOfContents();
      }
    }
    break;
  case TableOfContents::popid_SelectTocChapter:
    if (m_worksheet->m_tableOfContents != NULL)
    {
      if (m_worksheet->m_tableOfContents->RightClickedOn())
      {
        GroupCell *SelectionStart = m_worksheet->m_tableOfContents->RightClickedOn();
        // We only update the table of contents when there is time => no guarantee that the
        // cell that was clicked at actually still is part of the tree.
        if((m_worksheet->GetTree()) && (m_worksheet->GetTree()->Contains(SelectionStart)))
        {
          GroupCell *SelectionEnd = SelectionStart;
          while (
            (SelectionEnd->GetNext() != NULL)
            && (SelectionEnd->GetNext()->IsLesserGCType(SelectionStart->GetGroupType()))
            )
            SelectionEnd = SelectionEnd->GetNext();
          m_worksheet->SetActiveCell(NULL);
          m_worksheet->ScrolledAwayFromEvaluation();
          m_worksheet->SetHCaret(SelectionEnd);
          m_worksheet->SetSelection(SelectionStart, SelectionEnd);
          m_worksheet->RequestRedraw();
        }
      }
    }
    break;
  case TableOfContents::popid_EvalTocChapter:
  {
    GroupCell *SelectionStart = m_worksheet->m_tableOfContents->RightClickedOn();
    // We only update the table of contents when there is time => no guarantee that the
    // cell that was clicked at actually still is part of the tree.
    if ((m_worksheet->GetTree()) && (m_worksheet->GetTree()->Contains(SelectionStart)))
    {
      m_worksheet->AddSectionToEvaluationQueue(m_worksheet->m_tableOfContents->RightClickedOn());
      TriggerEvaluation();
    }
    break;
  }
  case TableOfContents::popid_ToggleTOCshowsSectionNumbers:
  {
    m_worksheet->m_configuration->TocShowsSectionNumbers(event.IsChecked());
    m_worksheet->UpdateTableOfContents();
    break;
  }
  case Worksheet::popid_evaluate_section:
  {
    GroupCell *group = NULL;
    if (m_worksheet->GetActiveCell())
    {
      // This "if" is pure paranoia. But - since the costs of an "if" are low...
      if (m_worksheet->GetActiveCell()->GetGroup())
        group = m_worksheet->GetActiveCell()->GetGroup();
    }
    else if (m_worksheet->HCaretActive())
    {
      if (m_worksheet->GetHCaret())
      {
        group = m_worksheet->GetHCaret();
        if ((false))
          if (group->GetNext())
            group = group->GetNext();
      }
      else
        group = m_worksheet->GetTree();
    }
    if (group)
    {
      m_worksheet->AddSectionToEvaluationQueue(group);
      TriggerEvaluation();
    }
  }
  break;
  case Worksheet::popid_evaluate:
  case ToolBar::tb_eval:
  {
    wxCommandEvent *dummy = new wxCommandEvent;
    EvaluateEvent(*dummy);
  }
  break;
  case ToolBar::tb_evaluate_rest:
    m_worksheet->AddRestToEvaluationQueue();
    EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
    break;
  case ToolBar::tb_evaltillhere:
    m_worksheet->m_evaluationQueue.Clear();
    m_worksheet->ResetInputPrompts();
    EvaluationQueueLength(0);
    if (m_worksheet->m_configuration->RestartOnReEvaluation())
      StartMaxima();
    m_worksheet->AddDocumentTillHereToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
    break;
  case Worksheet::popid_copy_matlab:
    if (m_worksheet->CanCopy(true))
      m_worksheet->CopyMatlab();
    break;
  case Worksheet::popid_copy_tex:
    if (m_worksheet->CanCopy(true))
      m_worksheet->CopyTeX();
    break;
  case Worksheet::popid_copy_text:
    if (m_worksheet->CanCopy(true))
      m_worksheet->CopyText();
    break;
  case Worksheet::popid_comment_selection:
    m_worksheet->CommentSelection();
    break;
  case Worksheet::popid_divide_cell:
    m_worksheet->DivideCell();
    break;
  case Worksheet::popid_copy_image:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyBitmap();
    break;
  case Worksheet::popid_copy_animation:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyAnimation();
    break;
  case Worksheet::popid_copy_svg:
    if (m_worksheet->CanCopy())
      m_worksheet->CopySVG();
    break;
#if wxUSE_ENH_METAFILE
  case Worksheet::popid_copy_emf:
    if (m_worksheet->CanCopy())
      m_worksheet->CopyEMF();
    break;
#endif
  case Worksheet::popid_copy_rtf:
    if (m_worksheet->CanCopy(true))
      m_worksheet->CopyRTF();
    break;
  case Worksheet::popid_simplify:
    MenuCommand(wxT("ratsimp(") + selection + wxT(");"));
    break;
  case Worksheet::popid_expand:
    MenuCommand(wxT("expand(") + selection + wxT(");"));
    break;
  case Worksheet::popid_factor:
    MenuCommand(wxT("factor(") + selection + wxT(");"));
    break;
  case Worksheet::popid_solve:
  {
    Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Variable(s):"),
                               selection, wxT("x"),
                               m_worksheet->m_configuration,
                               this, -1, _("Solve"), true,
                               _("solve() will solve a list of equations only if for n "
                                 "independent equations there are n variables to solve to.\n"
                                 "If only one result variable is of interest the other result "
                                 "variables solve needs to do its work can be used to tell "
                                 "solve() which variables to eliminate in the solution "
                                 "for the interesting variable.")
      );
    //wiz->Centre(wxBOTH);
    wiz->SetLabel1ToolTip(_("Comma-separated equations"));
    wiz->SetLabel2ToolTip(_("Comma-separated variables"));
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString cmd = wxT("solve([") + wiz->GetValue1() + wxT("], [") +
        wiz->GetValue2() + wxT("]);");
      MenuCommand(cmd);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_solve_num:
  {
    Gen4Wiz *wiz = new Gen4Wiz(_("Equation:"), _("Variable:"),
                               _("Lower bound:"), _("Upper bound:"),
                               selection, wxT("x"), wxT("-1"), wxT("1"),
                               m_worksheet->m_configuration,
                               this, -1, _("Find root"), true);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString cmd = wxT("find_root(") + wiz->GetValue1() + wxT(", ") +
        wiz->GetValue2() + wxT(", ") +
        wiz->GetValue3() + wxT(", ") +
        wiz->GetValue4() + wxT(");");
      MenuCommand(cmd);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_integrate:
  {
    IntegrateWiz *wiz = new IntegrateWiz(this, -1, m_worksheet->m_configuration, _("Integrate"));
    wiz->SetValue(selection);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wiz->GetValue();
      MenuCommand(val);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_diff:
  {
    Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Variable(s):"),
                               _("Times:"), selection, wxT("x"), wxT("1"),
                               m_worksheet->m_configuration,
                               this, -1, _("Differentiate"));
    wiz->SetValue(selection);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxStringTokenizer vars(wiz->GetValue2(), wxT(","));
      wxStringTokenizer times(wiz->GetValue3(), wxT(","));

      wxString val = wxT("diff(") + wiz->GetValue1();

      while (vars.HasMoreTokens() && times.HasMoreTokens())
      {
        val += wxT(",") + vars.GetNextToken();
        val += wxT(",") + times.GetNextToken();
      }

      val += wxT(");");
      MenuCommand(val);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_subst:
  {
    SubstituteWiz *wiz = new SubstituteWiz(this, -1, m_worksheet->m_configuration, _("Substitute"));
    wiz->SetValue(selection);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wiz->GetValue();
      MenuCommand(val);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_plot2d:
  {
    Plot2DWiz *wiz = new Plot2DWiz(this, -1, m_worksheet->m_configuration, _("Plot 2D"));
    wiz->SetValue(selection);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wiz->GetValue();
      MenuCommand(val);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_plot3d:
  {
    Plot3DWiz *wiz = new Plot3DWiz(this, -1, m_worksheet->m_configuration, _("Plot 3D"));
    wiz->SetValue(selection);
    //wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK)
    {
      wxString val = wiz->GetValue();
      MenuCommand(val);
    }
    wiz->Destroy();
  }
  break;
  case Worksheet::popid_float:
    MenuCommand(wxT("float(") + selection + wxT("), numer;"));
    break;
  case Worksheet::popid_image:
  {
    if(m_worksheet->GetSelectionStart() != m_worksheet->GetSelectionEnd())
      break;

    bool canExportSVG = false;
      
    if(m_worksheet->GetSelectionStart()->GetType() == MC_TYPE_IMAGE)
      if(dynamic_cast<ImgCell *>(m_worksheet->GetSelectionStart())->CanExportSVG())
        canExportSVG = true;

    if(m_worksheet->GetSelectionStart()->GetType() == MC_TYPE_SLIDE)
      if(dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart())->CanExportSVG())
        canExportSVG = true;

    wxString selectorString;

    if(canExportSVG)
      selectorString = _(
        "Scalable Vector image (*.svg)|*.svg|"
        "Compressed Scalable Vector Image (*.svgz)|*.svgz|"
        "PNG image (*.png)|*.png|"
        "JPEG image (*.jpg)|*.jpg|"
        "GIF image (*.gif)|*.gif|"
        "Windows bitmap (*.bmp)|*.bmp|"
        "Portable anymap (*.pnm)|*.pnm|"
        "Tagged image file format (*.tif)|*.tif|"
        "X pixmap (*.xpm)|*.xpm"
        );
    else
      selectorString = _("PNG image (*.png)|*.png|"
                         "JPEG image (*.jpg)|*.jpg|"
                         "Windows bitmap (*.bmp)|*.bmp|"
                         "GIF image (*.gif)|*.gif|"
                         "Portable anymap (*.pnm)|*.pnm|"
                         "Tagged image file format (*.tif)|*.tif|"
                         "X pixmap (*.xpm)|*.xpm"
        );
      
    wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                   wxT("image.png"), wxT("png"),
                                   selectorString,
                                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (file.Length())
    {
      m_worksheet->CopyToFile(file);
      m_lastPath = wxPathOnly(file);
    }
  }
  break;
  case Worksheet::popid_animation_save:
  {
    wxString file = wxFileSelector(_("Save animation to file"), m_lastPath,
                                   wxT("animation.gif"), wxT("gif"),
                                   _("GIF image (*.gif)|*.gif"),
                                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (file.Length())
    {
      Cell *selectedCell = m_worksheet->GetSelectionStart();
      if (selectedCell != NULL && selectedCell->GetType() == MC_TYPE_SLIDE)
        dynamic_cast<SlideShow *>(selectedCell)->ToGif(file);
    }
  }
  break;
  case Worksheet::popid_merge_cells:
    m_worksheet->MergeCells();
    break;
  }
}

void wxMaxima::OnRecentDocument(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString file = m_recentDocuments.Get(event.GetId() - menu_recent_document_0);

  if (SaveNecessary() &&
      (
              (file.Lower().EndsWith(wxT(".wxmx"))) ||
              (file.Lower().EndsWith(wxT(".wxm")))
      )
          )
  {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES)
    {
      if (!SaveFile())
        return;
    }
  }

  if (wxFileExists(file))
    OpenFile(file);
  else
  {
    LoggingMessageBox(_("File you tried to open does not exist."), _("File not found"), wxOK);
  }
}

void wxMaxima::OnRecentPackage(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString file = m_recentPackages.Get(event.GetId() - menu_recent_package_0);
  #ifdef __WXMSW__
  file.Replace(wxT("\\"), wxT("/"));
  #endif
  MenuCommand(wxT("load(\"") + file + wxT("\")$"));
}

void wxMaxima::OnUnsavedDocument(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();


  wxString file = m_unsavedDocuments.Get(event.GetId() - menu_unsaved_document_0);

  if(file.IsEmpty())
    return;

  if (SaveNecessary() &&
      (
              (file.Lower().EndsWith(wxT(".wxmx"))) ||
              (file.Lower().EndsWith(wxT(".wxm")))
      )
          )
  {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES)
    {
      if (!SaveFile())
        return;
    }
  }

  if (wxFileExists(file))
  {
    OpenWXMXFile(file, m_worksheet, true);
    m_tempfileName = file;
    m_worksheet->m_currentFile = wxEmptyString;
    m_worksheet->SetSaved(false);
  }
  else
    LoggingMessageBox(_("File you tried to open does not exist."), _("File not found"), wxOK);
}

bool wxMaxima::SaveNecessary()
{
  // No need to save an empty document
  if(m_worksheet->GetTree() == NULL)
    return false;

  // No need to save a document only consisting of a prompt
  if(m_worksheet->GetTree()->Empty())
    return false;

  if(m_worksheet->m_currentFile.IsEmpty())
    return true;
  
  return !m_fileSaved;
}

void wxMaxima::EditInputMenu(wxCommandEvent &WXUNUSED(event))
{
  m_worksheet->CloseAutoCompletePopup();
  if (!m_worksheet->CanEdit())
    return;

  EditorCell *tmp = dynamic_cast<EditorCell *>(m_worksheet->GetSelectionStart());

  if (tmp == NULL)
    return;

  m_worksheet->SetActiveCell(tmp);
}

void wxMaxima::VarAddAllEvent(wxCommandEvent &WXUNUSED(event))
{
  wxString command = "\n:lisp-quiet (wx-add-all-variables)\n";
  if((!m_worksheet->m_evaluationQueue.Empty()) ||
     (m_maximaBusy) ||
     (m_worksheet->QuestionPending()))
    m_configCommands += command;
  else SendMaxima(command);
}

void wxMaxima::VarReadEvent(wxCommandEvent &WXUNUSED(event))
{
  m_varNamesToQuery = m_worksheet->m_variablesPane->GetEscapedVarnames();
  QueryVariableValue();
}

//! Handle the evaluation event
//
// User tried to evaluate, find out what is the case
// Normally just add the respective groupcells to evaluationqueue
// If there is a special case - eg sending from output section
// of the working group, handle it carefully.
void wxMaxima::EvaluateEvent(wxCommandEvent &WXUNUSED(event))
{
  if(m_worksheet == NULL)
    return;
  m_worksheet->CloseAutoCompletePopup();

  bool evaluating = !m_worksheet->m_evaluationQueue.Empty();
  if (!evaluating)
    m_worksheet->FollowEvaluation(true);

  EditorCell *editor = m_worksheet->GetActiveCell();

  if(editor == NULL)
  {
    GroupCell *group = NULL;
    if(m_worksheet->HCaretActive())
    {
      group = m_worksheet->GetHCaret();
      if(group == NULL)
        group = m_worksheet->GetTree();
      else
        group = group->GetNext();
      while(
        (group != NULL) &&
        (!((group->GetEditable() != NULL) &&
           (group->GetEditable()->GetType() == MC_TYPE_INPUT)) &&
         (!m_worksheet->m_evaluationQueue.IsLastInQueue(group))
          ))
        group = group->GetNext();
    }
    if(
      (group != NULL) &&
      (group->GetEditable() != NULL) &&
      (group->GetEditable()->GetType() == MC_TYPE_INPUT))
    editor = group->GetEditable();
  }

  if (editor != NULL) // we have an active cell
  {
    if (editor->GetType() == MC_TYPE_INPUT && (!m_worksheet->m_configuration->InLispMode()))
      editor->AddEnding();
    // if active cell is part of a working group, we have a special
    // case - answering 1a question. Manually send answer to Maxima.
    GroupCell *cell = editor->GetGroup();
    if (m_worksheet->GCContainsCurrentQuestion(cell))
    {
      wxString answer = editor->ToString(true);
      // Add the answer to the current working cell or update the answer
      // that is stored within it.
      cell->SetAnswer(m_worksheet->GetLastQuestion(), answer);
      SendMaxima(answer, true);
      StatusMaximaBusy(calculating);
      m_worksheet->SetHCaret(cell);
      m_worksheet->ScrollToCaret();
    }
    else
    { // normally just add to queue (and mark the cell as no more containing an error message)
      m_worksheet->GetErrorList().Remove(cell);
      m_worksheet->AddCellToEvaluationQueue(cell);
    }
  }
  else
  { // no evaluate has been called on no active cell?
    m_worksheet->AddSelectionToEvaluationQueue();
  }
  // Inform the user about the length of the evaluation queue.
  EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
}

wxString wxMaxima::GetUnmatchedParenthesisState(wxString text,int &index)
{
  text.Trim(true);
  text.Trim(false);
  if(text.IsEmpty())
    return (wxEmptyString);
  if (text.EndsWith(wxT("\\")))
    return (_("Cell ends in a backslash"));


  index = 0;
  bool endingNeeded = true;
  wxChar lastnonWhitespace;
  wxChar lastnonWhitespace_Next = wxT(' ');
  std::list<wxChar> delimiters;

  for (auto const &tok : MaximaTokenizer(text, m_worksheet->m_configuration).PopTokens())
  {
    auto &itemText = tok.GetText();
    const TextStyle itemStyle = tok.GetStyle();
    index += itemText.Length();

    lastnonWhitespace = lastnonWhitespace_Next;

    // Handle comments
    if(itemStyle == TS_CODE_COMMENT)
    {
      if(!itemText.EndsWith("*/"))
        return (_("Unterminated comment."));
      continue;
    }

    wxChar firstC = itemText[0];
    wxChar lastC = itemText.Last();

    // Remember the last non-whitespace character that isn't part
    // of a comment.
    if((firstC != ' ') && (firstC != '\t') && (firstC != '\r') && (firstC != '\n'))
      lastnonWhitespace_Next = lastC;

    // Handle opening parenthesis
    if(itemText == "(")
    {
      delimiters.push_back(wxT(')'));
      continue;
    }
    if(itemText == "[")
    {
      delimiters.push_back(wxT(']'));
      continue;
    }
    if(itemText == "{")
    {
      delimiters.push_back(wxT('}'));
      continue;
    }

    // Handle closing parenthesis
    if((itemText == ')') || (itemText == ']') || (itemText == '}'))
    {
      endingNeeded = true;
      if (delimiters.empty()) return (_("Mismatched parenthesis"));
      if (firstC != delimiters.back()) return (_("Mismatched parenthesis"));
      delimiters.pop_back();
      if (lastnonWhitespace == wxT(','))
        return (_("Comma directly followed by a closing parenthesis"));
      continue;
    }

    if(itemStyle == TS_CODE_STRING)
    {
      endingNeeded = true;
      if(!itemText.EndsWith("\""))
        return (_("Unterminated string."));
      continue;
    }

    if(itemStyle == TS_CODE_ENDOFLINE)
    {
      if(!delimiters.empty())
        return _("Un-closed parenthesis on encountering ; or $");
      endingNeeded = false;
      continue;
    }

    if(itemStyle == TS_CODE_LISP)
    {
      endingNeeded = false;
      continue;
    }
  }

  if (!delimiters.empty())
    return _("Un-closed parenthesis");

  if((endingNeeded) && (!m_worksheet->m_configuration->InLispMode()))
    return _("No dollar ($) or semicolon (;) at the end of command");
  else
    return wxEmptyString;
}

//! Tries to evaluate next group cell in queue
//
// Calling this function should not do anything dangerous
void wxMaxima::TriggerEvaluation()
{
  // If evaluation is already running we don't have anything to do
  if(m_maximaBusy)
    return;

  // While we wait for an answer we cannot send new commands.
  if (m_worksheet->QuestionPending())
    return;

  // If we aren't connected yet this function will be triggered as soon as maxima
  // connects to wxMaxima
  if (!m_client || (!m_client->IsConnected()))
    return;

  // Maxima is connected. Let's test if the evaluation queue is empty.
  GroupCell *const tmp = m_worksheet->m_evaluationQueue.GetCell();
  if (!tmp)
  {
    // Maxima is no more busy.
    StatusMaximaBusy(waiting);
    // Inform the user that the evaluation queue length now is 0.
    EvaluationQueueLength(0);
    // The cell from the last evaluation might still be shown in it's "evaluating" state
    // so let's refresh the console to update the display of this.
    m_worksheet->RequestRedraw();

    // If the window isn't active we can inform the user that maxima in the meantime
    // has finished working.
    if((m_worksheet->m_configuration->NotifyIfIdle()) && (m_worksheet->GetTree() != NULL))
      m_worksheet->SetNotification(_("Maxima has finished calculating."));

    if(m_configCommands != wxEmptyString)
      SendMaxima(m_configCommands);
    m_configCommands = wxEmptyString;
    return; //empty queue
  }

  // Add a semicolon at the end of the cell, if needed.
  if(tmp->AddEnding())
    m_worksheet->m_evaluationQueue.AddEnding();

  // Display the evaluation queue's status.
  EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(), m_worksheet->m_evaluationQueue.CommandsLeftInCell());

  // We don't want to evaluate a new cell if the user still has to answer
  // a question.
  if (m_worksheet->QuestionPending())
    return;

  // Maxima is connected and the queue contains an item.

  // From now on we look every second if we got some output from a crashing
  // maxima: Is maxima is working correctly the stdout and stderr descriptors we
  // poll don't offer any data.
  ReadStdErr();
  m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

  if (m_worksheet->m_evaluationQueue.m_workingGroupChanged)
  {
    // Clear the monitor that shows the xml representation of the output of the
    // current maxima command.
    if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
      m_xmlInspector->Clear();

    // If the cell's output that we are about to remove contains the currently
    // selected cells we undo the selection.
    if (m_worksheet->GetSelectionStart())
    {
      if (m_worksheet->GetSelectionStart()->GetGroup() == tmp)
        m_worksheet->ClearSelection();
    }
    if (m_worksheet->GetSelectionEnd())
    {
      if (m_worksheet->GetSelectionEnd()->GetGroup() == tmp)
        m_worksheet->ClearSelection();
    }
    tmp->RemoveOutput();
    m_worksheet->RequestRedraw();
  }
  wxString text = m_worksheet->m_evaluationQueue.GetCommand();
  m_commandIndex = m_worksheet->m_evaluationQueue.GetIndex();
  if ((text != wxEmptyString) && (text != wxT(";")) && (text != wxT("$")))
  {
    int index;
    wxString parenthesisError = GetUnmatchedParenthesisState(tmp->GetEditable()->ToString(true),index);
    if (parenthesisError.IsEmpty())
    {
      if (m_worksheet->FollowEvaluation())
      {
        m_worksheet->SetSelection(tmp);
        if (!m_worksheet->GetWorkingGroup())
        {
          m_worksheet->SetHCaret(tmp);
          m_worksheet->ScrollToCaret();
        }
      }

      m_worksheet->SetWorkingGroup(tmp);
      tmp->GetPrompt()->SetValue(m_lastPrompt);
      tmp->ResetSize();

      SendMaxima(m_configCommands);
      SendMaxima(text, true);
      m_maximaBusy = true;
      // Now that we have sent a command we need to query all variable values anew
      m_varNamesToQuery = m_worksheet->m_variablesPane->GetEscapedVarnames();
      // And the gui is interested in a few variable names
      m_readMaximaVariables = true;
      m_configCommands = wxEmptyString;

      EvaluationQueueLength(m_worksheet->m_evaluationQueue.Size(),
                            m_worksheet->m_evaluationQueue.CommandsLeftInCell()
      );

      text.Trim(false);
      if (!m_hasEvaluatedCells)
      {
        if (text.StartsWith(wxT(":lisp")))
          LeftStatusText(_("A \":lisp\" as the first command might fail to send a \"finished\" signal."));
      }

      // Mark the current maxima process as "no more in its initial condition".
      m_hasEvaluatedCells = true;
    }
    else
    {
      // Manually mark the current cell as the one that has caused an error.
      m_worksheet->GetErrorList().Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_commandIndex - 1);
      // Inform the user about the error (which automatically causes the worksheet
      // to the cell we marked as erroneous a few seconds ago.
      auto cell = std::make_unique<TextCell>(tmp, &(m_worksheet->m_configuration),
                                            _("Refusing to send cell to maxima: ") +
                                             parenthesisError + wxT("\n"));
      cell->SetType(MC_TYPE_ERROR);
      tmp->SetOutput(std::move(cell));
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->SetWorkingGroup(nullptr);
      tmp->GetEditable()->SetCaretPosition(index);
      tmp->GetEditable()->SetErrorIndex((m_commandIndex = index) - 1);

      if (m_worksheet->FollowEvaluation())
        m_worksheet->SetSelection(NULL);

      m_worksheet->SetWorkingGroup(nullptr);
      m_worksheet->RequestRedraw();
      if(!AbortOnError())
      {
        m_outputCellsFromCurrentCommand = 0;
        TriggerEvaluation();
      }
      if(tmp->GetEditable())
        m_worksheet->SetActiveCell(tmp->GetEditable());
    }
  }
  else
  {
    m_outputCellsFromCurrentCommand = 0;
    m_worksheet->m_evaluationQueue.RemoveFirst();
    TriggerEvaluation();
  }
}

void wxMaxima::ReplaceSuggestion(wxCommandEvent &event)
{
  int index = event.GetId() - Worksheet::popid_suggestion1;

  EditorCell *editor = m_worksheet->GetActiveCell();
  if(editor == NULL)
    return;
  editor->SelectWordUnderCaret(false);
  editor->ReplaceSelection(editor->GetWordUnderCaret(), m_worksheet->m_replacementsForCurrentWord[index]);

}

void wxMaxima::InsertMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  GroupType type = GC_TYPE_CODE;
  bool output = false;
  switch (event.GetId())
  {
    case Worksheet::popid_never_autoanswer:
      m_worksheet->m_configuration->OfferKnownAnswers(
        !m_worksheet->m_configuration->OfferKnownAnswers());
      break;
    case Worksheet::popid_auto_answer:
      if (m_worksheet->GetActiveCell() &&
          m_worksheet->GetActiveCell()->GetGroup()->GetGroupType() == GC_TYPE_CODE)
        m_worksheet->GetActiveCell()->GetGroup()->SetAutoAnswer(event.IsChecked());
      else if((m_worksheet->GetSelectionStart() != NULL)&&
              (m_worksheet->GetSelectionStart()->GetType() == MC_TYPE_GROUP))
      {
        GroupCell *gc = dynamic_cast<GroupCell *>(m_worksheet->GetSelectionStart());
        while(gc != NULL)
        {
          if(gc->GetGroupType() == GC_TYPE_CODE)
            gc->SetAutoAnswer(event.IsChecked());

          if(gc == m_worksheet->GetSelectionEnd())
            break;
          gc = gc->GetNext();
        }
      }
      m_fileSaved = false;
      m_worksheet->RequestRedraw();
      return;
      break;
    case Worksheet::popid_add_watch:
    {
      wxString selectionString;
      if(m_worksheet->GetActiveCell())
      {
        selectionString = m_worksheet->GetActiveCell()->GetSelectionString();
        if(selectionString.IsEmpty())
          selectionString = m_worksheet->GetActiveCell()->GetWordUnderCaret();
        m_worksheet->m_variablesPane->AddWatchCode(selectionString);
        wxMaximaFrame::ShowPane(menu_pane_variables,true);
      }
      if(selectionString.IsEmpty() && (m_worksheet->GetSelectionStart() != NULL))
        selectionString = m_worksheet->GetSelectionStart()->ToString();
      if(!selectionString.IsEmpty())
      {
        m_worksheet->m_variablesPane->AddWatchCode(selectionString);
        wxMaximaFrame::ShowPane(menu_pane_variables,true);
      }
      return;
    }
    case Worksheet::popid_add_watch_label:
      if(m_worksheet->IsSelected(MC_TYPE_LABEL))
      {
        wxString selectionString = m_worksheet->GetSelectionStart()->ToString();
        selectionString.Trim(true);
        selectionString.Trim(false);
        if(selectionString.StartsWith("("))
          selectionString = selectionString.Right(selectionString.Length()-1);
        if(selectionString.EndsWith(")"))
          selectionString = selectionString.Left(selectionString.Length()-1);
        m_worksheet->m_variablesPane->AddWatchCode(selectionString);
        wxMaximaFrame::ShowPane(menu_pane_variables,true);
      }
      return;
    case menu_insert_previous_output:
      output = true;
      type = GC_TYPE_CODE;
      break;
    case Worksheet::popid_insert_input:
    case menu_insert_input:
    case menu_insert_previous_input:
      type = GC_TYPE_CODE;
      break;
    case menu_autocomplete:
      m_worksheet->Autocomplete();
      return;
    case menu_autocomplete_templates:
      m_worksheet->Autocomplete(AutoComplete::tmplte);
      return;
    case menu_convert_to_code:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_CODE);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_comment:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_TEXT);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_add_comment:
    case Worksheet::popid_add_comment:
    case menu_format_text:
    case Worksheet::popid_insert_text:
      type = GC_TYPE_TEXT;
      break;
    case menu_convert_to_title:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_TITLE);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_add_title:
    case menu_format_title:
    case Worksheet::popid_insert_title:
      type = GC_TYPE_TITLE;
      break;
    case menu_convert_to_section:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_SECTION);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_add_section:
    case menu_format_section:
    case Worksheet::popid_insert_section:
      type = GC_TYPE_SECTION;
      break;
    case menu_convert_to_subsection:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_SUBSECTION);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_add_subsection:
    case menu_format_subsection:
    case Worksheet::popid_insert_subsection:
      type = GC_TYPE_SUBSECTION;
      break;
    case menu_convert_to_subsubsection:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_SUBSUBSECTION);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_heading5:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_HEADING5);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_heading6:
      if (m_worksheet->GetActiveCell())
      {
        m_worksheet->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_HEADING6);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_add_subsubsection:
    case menu_format_subsubsection:
    case Worksheet::popid_insert_subsubsection:
      type = GC_TYPE_SUBSUBSECTION;
      break;
    case menu_add_heading5:
    case menu_format_heading5:
    case Worksheet::popid_insert_heading5:
      type = GC_TYPE_HEADING5;
      break;
    case menu_add_heading6:
    case menu_format_heading6:
    case Worksheet::popid_insert_heading6:
      type = GC_TYPE_HEADING6;
      break;
    case menu_add_pagebreak:
    case menu_format_pagebreak:
      m_worksheet->InsertGroupCells(
              std::make_unique<GroupCell>(&(m_worksheet->m_configuration), GC_TYPE_PAGEBREAK),
              m_worksheet->GetHCaret());
      m_worksheet->Recalculate();
      m_worksheet->SetFocus();
      return;
    case menu_insert_image:
    case menu_format_image:
    {
      wxString file = wxFileSelector(_("Insert Image"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Image files (*.png, *.jpg, *.bmp, *.xpm, *.gif, *.svg, *.svgz)|*.png;*.jpg;*.bmp;*.xpm;*.gif;*.svg;*.svgz"),
                                     wxFD_OPEN);
      if (file != wxEmptyString)
        m_worksheet->OpenHCaret(file, GC_TYPE_IMAGE);
      m_worksheet->SetFocus();
      return;
    }
      break;
    case menu_fold_all_cells:
      m_worksheet->FoldAll();
      m_worksheet->Recalculate(true);
      // send cursor to the top
      m_worksheet->SetHCaret(NULL);
      break;
    case menu_unfold_all_cells:
      m_worksheet->UnfoldAll();
      m_worksheet->Recalculate(true);
      // refresh without moving cursor
      m_worksheet->SetHCaret(m_worksheet->GetHCaret());
      break;
  }

  m_worksheet->SetFocus();

  if (event.GetId() == menu_insert_previous_input ||
      event.GetId() == menu_insert_previous_output)
  {
    wxString input;

    if (output == true)
      input = m_worksheet->GetOutputAboveCaret();
    else
      input = m_worksheet->GetInputAboveCaret();
    if (input != wxEmptyString)
      m_worksheet->OpenHCaret(input, type);
  }
  else if (
    (event.GetId() == menu_unfold_all_cells) ||
    (event.GetId() == menu_fold_all_cells) ||
    (event.GetId() == menu_convert_to_heading6) ||
    (event.GetId() == menu_convert_to_heading5) ||
    (event.GetId() == menu_convert_to_subsubsection) ||
    (event.GetId() == menu_convert_to_subsection) ||
    (event.GetId() == menu_convert_to_section) ||
    (event.GetId() == menu_convert_to_comment) ||
    (event.GetId() == menu_convert_to_title) ||
    (event.GetId() == menu_convert_to_code)
    )
  {
    // don't do anything else
  }
  else
    m_worksheet->OpenHCaret(wxEmptyString, type);
}

void wxMaxima::ResetTitle(bool saved, bool force)
{
  SetRepresentedFilename(m_worksheet->m_currentFile);
  OSXSetModified((saved != m_fileSaved) || (force));

  if ((saved != m_fileSaved) || (force))
  {
    m_fileSaved = saved;
    if (m_worksheet->m_currentFile.Length() == 0)
    {
#ifndef __WXOSX__
      if (saved)
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(GITVERSION)) + _("[ unsaved ]"));
      else
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(GITVERSION)) + _("[ unsaved* ]"));
#endif
    }
    else
    {
      wxString name, ext;
      wxFileName::SplitPath(m_worksheet->m_currentFile, NULL, NULL, &name, &ext);
#ifndef __WXOSX__
      if (m_fileSaved)
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(GITVERSION)) +
                 wxT(" [ ") + name + wxT(".") + ext + wxT(" ]"));
      else
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(GITVERSION)) +
                 wxT(" [ ") + name + wxT(".") + ext + wxT("* ]"));
#else
      SetTitle(name + wxT(".") + ext);
#endif
    }
#if defined __WXOSX__
#if defined __WXOSX_COCOA__
    OSXSetModified(!saved);
    if (m_worksheet->m_currentFile != wxEmptyString)
      SetRepresentedFilename(m_worksheet->m_currentFile);
#else
    WindowRef win = (WindowRef)MacGetTopLevelWindowRef();
    SetWindowModified(win,!saved);
    if (m_worksheet->m_currentFile != wxEmptyString)
    {
      FSRef fsref;
      wxMacPathToFSRef(m_worksheet->m_currentFile, &fsref);
      HIWindowSetProxyFSRef(win, &fsref);
    }
#endif
#endif
  }
}

///--------------------------------------------------------------------------------
///  Plot Slider
///--------------------------------------------------------------------------------

void wxMaxima::UpdateSlider()
{
  if (m_worksheet->m_mainToolBar)
  {
    if (m_worksheet->m_mainToolBar->m_plotSlider)
    {
      if (m_worksheet->IsSelected(MC_TYPE_SLIDE))
      {
        SlideShow *cell = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());

        m_worksheet->m_mainToolBar->UpdateSlider(cell);
      }
    }
  }
}

void wxMaxima::SliderEvent(wxScrollEvent &event)
{
  SlideShow *slideShow = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());

  if (slideShow != NULL)
  {
    slideShow->AnimationRunning(false);
    slideShow->SetDisplayedIndex(event.GetPosition());

    wxRect rect = slideShow->GetRect();
    m_worksheet->RequestRedraw(rect);
    if(m_worksheet->m_mainToolBar)
      m_worksheet->m_mainToolBar->UpdateSlider(slideShow);
  }
}

void wxMaxima::ShowPane(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  int id = event.GetId();

  if (id == menu_pane_hideall)
    wxMaximaFrame::ShowPane(static_cast<Event>(id), true);
  else
    wxMaximaFrame::ShowPane(static_cast<Event>(id),
                            !IsPaneDisplayed(static_cast<Event>(id)));

  if((id == menu_pane_structure) && (IsPaneDisplayed(static_cast<Event>(id))))
    m_worksheet->UpdateTableOfContents();
}

void wxMaxima::OnChar(wxKeyEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->OnChar(event);
  event.Skip();
}

void wxMaxima::OnKeyDown(wxKeyEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->OnKeyDown(event);
  event.Skip();
}

void wxMaxima::NetworkDClick(wxCommandEvent &WXUNUSED(event))
{
  m_manager.GetPane(wxT("XmlInspector")).Show(
          !m_manager.GetPane(wxT("XmlInspector")).IsShown()
  );
  m_manager.Update();
}

void wxMaxima::HistoryDClick(wxCommandEvent &event)
{
  m_worksheet->CloseAutoCompletePopup();
  m_worksheet->OpenHCaret(event.GetString(), GC_TYPE_CODE);
  m_worksheet->SetFocus();
}

void wxMaxima::TableOfContentsSelection(wxListEvent &event)
{
  GroupCell *selection = m_worksheet->m_tableOfContents->GetCell(event.GetIndex())->GetGroup();

  // We only update the table of contents when there is time => no guarantee that the
  // cell that was clicked at actually still is part of the tree.
  if ((m_worksheet->GetTree()) && (m_worksheet->GetTree()->Contains(selection)))
  {
    m_worksheet->SetHCaret(selection);
    m_worksheet->ScrollToCaret();
    m_worksheet->SetFocus();
  }
}

void wxMaxima::OnFollow(wxCommandEvent &WXUNUSED(event))
{
  m_worksheet->CloseAutoCompletePopup();
  m_worksheet->OnFollow();
}

wxMaxima::VersionNumber::VersionNumber(const wxString &version) :
  m_major(0),
  m_minor(0),
  m_patchlevel(0)
{
  wxStringTokenizer tokens(version, wxT("._-~$"));

  if(tokens.HasMoreTokens())
    tokens.GetNextToken().ToLong(&m_major);
  if(tokens.HasMoreTokens()) //-V581
    tokens.GetNextToken().ToLong(&m_minor);
  if(tokens.HasMoreTokens()) //-V581
    tokens.GetNextToken().ToLong(&m_patchlevel);
}

bool operator<(const wxMaxima::VersionNumber& v1, const wxMaxima::VersionNumber& v2)
{
  if(v1.Major() < v2.Major())
    return true;
  if(v1.Major() > v2.Major())
    return false;
  if(v1.Minor() < v2.Minor())
    return true;
  if(v1.Minor() > v2.Minor())
    return false;
  if(v1.Patchlevel() < v2.Patchlevel())
    return true;
  return false;
}

bool operator>(const wxMaxima::VersionNumber& v1, const wxMaxima::VersionNumber& v2)
{
  if(v1.Major() > v2.Major())
    return true;
  if(v1.Major() < v2.Major())
    return false;
  if(v1.Minor() > v2.Minor())
    return true;
  if(v1.Minor() < v2.Minor())
    return false;
  if(v1.Patchlevel() > v2.Patchlevel())
    return true;
  return false;
}

/***
 * Checks the file http://wxMaxima-developers.github.io/wxmaxima/version.txt to
 * see if there is a newer version available.
 */
void wxMaxima::CheckForUpdates(bool reportUpToDate)
{
  wxHTTP connection;
  connection.SetHeader(wxT("Content-type"), wxT("text/html; charset=utf-8"));
  connection.SetTimeout(2);

  if (!connection.Connect(wxT("wxMaxima-developers.github.io")))
  {
    LoggingMessageBox(_("Can not connect to the web server."), _("Error"),
                 wxOK | wxICON_ERROR);
    return;
  }

  std::unique_ptr<wxInputStream> inputStream =
    std::unique_ptr<wxInputStream>(connection.GetInputStream(_T("/wxmaxima/version.txt")));

  if (connection.GetError() == wxPROTO_NOERR)
  {
    wxString version;
    wxStringOutputStream outputStream(&version);
    inputStream->Read(outputStream);

    if (version.StartsWith(wxT("wxmaxima =")))
    {
      version = version.Mid(11, version.Length());
      version.Trim(true);
      version.Trim(false);
      VersionNumber myVersion(wxT(GITVERSION));
      VersionNumber currVersion(version);

      if (myVersion < currVersion)
      {
        bool visit = LoggingMessageBox(wxString::Format(
                                          _("You have version %s. Current version is %s.\n\n"
                                                    "Select OK to visit the wxMaxima webpage."),
                                          wxT(GITVERSION), version.utf8_str()),
                                  _("Upgrade"),
                                  wxOK | wxCANCEL | wxICON_INFORMATION) == wxOK;

        if (visit)
          wxLaunchDefaultBrowser(wxT("https://wxMaxima-developers.github.io/wxmaxima"));
      }
      else if (reportUpToDate)
        LoggingMessageBox(_("Your version of wxMaxima is up to date."), _("Upgrade"),
                     wxOK | wxICON_INFORMATION);
    }
    else
    {
      LoggingMessageBox(
              _("Unable to interpret the version info I got from http://wxMaxima-developers.github.io/wxmaxima/version.txt: ") +
              version, _("Upgrade"),
              wxOK | wxICON_INFORMATION);

    }
  }
  else
  {
    LoggingMessageBox(_("Can not download version info."), _("Error"),
                 wxOK | wxICON_ERROR);
  }
  connection.Close();
}

int wxMaxima::SaveDocumentP()
{
  wxString file, ext;
  if (m_worksheet->m_currentFile.IsEmpty())
  {
    // Check if we want to save modified untitled documents on exit
    if (!m_worksheet->m_configuration->SaveUntitled())
      return wxID_NO;

#if defined __WXOSX__
    file = GetTitle();
#else
    file = _("unsaved");
#endif
  }
  else
  {
    if (!m_worksheet->m_configuration->AutoSaveAsTempFile())
    {
      if (SaveFile())
        return wxID_NO;
    }

    wxFileName::SplitPath(m_worksheet->m_currentFile, NULL, NULL, &file, &ext);
    file += wxT(".") + ext;
  }

  LoggingMessageDialog dialog(this,
                         wxString::Format(_("Do you want to save the changes you made in the document \"%s\"?"),
                                          file.utf8_str()),
                         "wxMaxima", wxCENTER | wxYES_NO | wxCANCEL);

  dialog.SetExtendedMessage(_("Your changes will be lost if you don't save them."));
  dialog.SetYesNoCancelLabels(_("Save"), _("Don't save"), _("Cancel"));

  return dialog.ShowModal();
}

void wxMaxima::OnActivate(wxActivateEvent &event)
{
  if (m_worksheet)
    m_worksheet->WindowActive(event.GetActive());
  event.Skip();
}

void wxMaxima::OnMinimize(wxIconizeEvent &event)
{
  m_worksheet->WindowActive(!event.IsIconized());
  if(!event.IsIconized())
    m_worksheet->SetFocus();
  event.Skip();
}

void wxMaxima::ChangeCellStyle(wxCommandEvent& WXUNUSED(event))
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  if ((m_worksheet == NULL) || (m_worksheet->m_mainToolBar == NULL))
    return;

  if(m_worksheet->GetActiveCell())
  {
    GroupCell *group = m_worksheet->GetActiveCell()->GetGroup();
    switch(group->GetGroupType())
    {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_TITLE:
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
    case GC_TYPE_SUBSUBSECTION:
    case GC_TYPE_HEADING5:
    case GC_TYPE_HEADING6:
      m_worksheet->SetCellStyle(group, m_worksheet->m_mainToolBar->GetCellType());
      break;
    default:
      break;
    }
    m_worksheet->NumberSections();
    m_worksheet->SetFocus();
  }
  else
    m_worksheet->m_mainToolBar->SetDefaultCellStyle();
  m_worksheet->SetFocus();
}


bool wxMaxima::m_pipeToStdout = false;
bool wxMaxima::m_exitOnError = false;
wxString wxMaxima::m_extraMaximaArgs;
int wxMaxima::m_exitCode = 0;
//wxRegEx  wxMaxima::m_outputPromptRegEx(wxT("<lbl>.*</lbl>"));
wxRegEx  wxMaxima::m_funRegEx(wxT("^ *([[:alnum:]%_]+) *\\(([[:alnum:]%_,[[.].] ]*)\\) *:="));
wxRegEx  wxMaxima::m_varRegEx(wxT("^ *([[:alnum:]%_]+) *:"));
wxRegEx  wxMaxima::m_blankStatementRegEx(wxT("(^;)|((^|;)(((\\/\\*.*\\*\\/)?([[:space:]]*))+;)+)"));
wxRegEx  wxMaxima::m_sbclCompilationRegEx(wxT("; compiling (.* \\.*)"));
wxRegEx  wxMaxima::m_gnuplotErrorRegex(wxT("\".*\\.gnuplot\", line [0-9][0-9]*: "));
wxString wxMaxima::m_promptPrefix(wxT("<PROMPT>"));
const wxString wxMaxima::m_promptSuffix(wxT("</PROMPT>"));
wxString wxMaxima::m_suppressOutputPrefix(wxT("<suppressOutput>"));
wxString wxMaxima::m_suppressOutputSuffix(wxT("</suppressOutput>"));
wxString wxMaxima::m_symbolsPrefix(wxT("<wxxml-symbols>"));
wxString wxMaxima::m_symbolsSuffix(wxT("</wxxml-symbols>"));
wxString wxMaxima::m_variablesPrefix(wxT("<variables>"));
wxString wxMaxima::m_variablesSuffix(wxT("</variables>"));
wxString wxMaxima::m_addVariablesPrefix(wxT("<watch_variables_add>"));
wxString wxMaxima::m_addVariablesSuffix(wxT("</watch_variables_add>"));
wxString wxMaxima::m_statusbarPrefix(wxT("<statusbar>"));
wxString wxMaxima::m_statusbarSuffix(wxT("</statusbar>\n"));
wxString wxMaxima::m_mathPrefix1(wxT("<mth>"));
wxString wxMaxima::m_mathPrefix2(wxT("<math>"));
wxString wxMaxima::m_mathSuffix1(wxT("</mth>"));
wxString wxMaxima::m_mathSuffix2(wxT("</math>"));
wxString wxMaxima::m_emptywxxmlSymbols(wxT("<wxxml-symbols></wxxml-symbols>"));
wxString wxMaxima::m_firstPrompt(wxT("(%i1) "));
//wxString wxMaxima::m_outputPromptPrefix(wxT("<lbl>"));
//wxString wxMaxima::m_outputPromptSuffix(wxT("</lbl"));
wxMaxima::ParseFunctionHash wxMaxima::m_knownXMLTags;
wxMaxima::VarReadFunctionHash wxMaxima::m_variableReadActions;
