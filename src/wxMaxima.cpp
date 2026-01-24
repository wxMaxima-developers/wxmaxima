// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2022 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the contents of the class wxMaxima that contains most of the
  program's logic.

  The worksheet is defined in the class Worksheet instead and
  everything surrounding it in wxMaximaFrame.
*/

#include "MaximaTokenizer.h"
#include <wx/notifmsg.h>
#if defined __WXMSW__
//#include <wchar.h>
#endif
#include <functional>
#include <unordered_map>
#include <utility>
#include <vector>
#include <time.h>
#include <algorithm>
#include <wx/zipstrm.h>
#include "wizards/ActualValuesStorageWiz.h"
#include "cells/AnimationCell.h"
#include "wizards/BC2Wiz.h"
#include "cells/CellList.h"
#include "dialogs/ConfigDialogue.h"
#include "dialogs/MaximaNotStartingDialog.h"
#include "dialogs/AboutDialog.h"
#include "wizards/CsvWiz.h"
#include "wizards/DrawWiz.h"
#include "cells/EditorCell.h"
#include "wizards/Gen1Wiz.h"
#include "wizards/Gen2Wiz.h"
#include "wizards/Gen3Wiz.h"
#include "wizards/Gen4Wiz.h"
#include "wizards/Gen5Wiz.h"
#include "wizards/GenWiz.h"
#include "cells/ImgCell.h"
#include "wizards/IntegrateWiz.h"
#include "cells/LabelCell.h"
#include "dialogs/LicenseDialog.h"
#include "dialogs/ChangeLogDialog.h"
#include "wizards/LimitWiz.h"
#include "wizards/ListSortWiz.h"
#include "wizards/MatWiz.h"
#include "dialogs/MaxSizeChooser.h"
#include "Maxima.h"
#include "wizards/Plot2dWiz.h"
#include "wizards/Plot3dWiz.h"
#include "wizards/PlotFormatWiz.h"
#include "graphical_io/Printout.h"
#include "dialogs/ResolutionChooser.h"
#include "wizards/SeriesWiz.h"
#include "StringUtils.h"
#include "wizards/SubstituteWiz.h"
#include "wizards/SumWiz.h"
#include "wizards/SystemWiz.h"
#include "dialogs/TipOfTheDay.h"
#include "Version.h"
#include "WXMformat.h"
#include "WXMXformat.h"
#include "wxMathml.h"
#include "wxMaxima.h"
#include <wx/app.h>
#include <wx/apptrait.h>
#include <wx/base64.h>
#include <wx/buffer.h>
#include <wx/artprov.h>
#include <wx/clipbrd.h>
#include <wx/colordlg.h>
#include <wx/dir.h>
#include <wx/dynlib.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/mimetype.h>
#include <wx/msgdlg.h>
#include <wx/mstream.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>
#include <wx/uri.h>
#include <wx/utils.h>
#include <wx/wupdlock.h>
#include <wx/windowptr.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/persist/toplevel.h>
#include <wx/sckstrm.h>
#include <wx/txtstrm.h>

#include "main.h"
#include <list>
#include <memory>
#include <wx/sstream.h>
#include <wx/url.h>
#include "Configuration.h"

wxDECLARE_APP(MyApp);

#if defined __WXOSX__
#define MACPREFIX "wxMaxima.app/Contents/Resources/"
#endif

/*! Calls a member function from a function pointer

  \todo Replace this by a C++17 construct when we switch to C++17
*/
#define CALL_MEMBER_FN(object, ptrToMember) ((object).*(ptrToMember))

void wxMaxima::ConfigChanged() {

  if (GetWorksheet() && (GetWorksheet()->GetTree()))
    GetWorksheet()->GetTree()->FontsChangedList();

  wxConfigBase *config = wxConfig::Get();
  int showLength = 0;

  config->Read(wxS("showLength"), &showLength);

  switch (showLength) {
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
  if(GetWorksheet())
    {
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  wxLogMessage(_("Sending configuration data to maxima."));
  if (m_configuration.UseSVG())
    m_configCommands += wxS(":lisp-quiet (setq $wxplot_usesvg t)\n");
  else
    m_configCommands += wxS(":lisp-quiet (setq $wxplot_usesvg nil)\n");
  if (m_configuration.UsePngCairo())
    m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo t)\n");
  else
    m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo nil)\n");

  m_configCommands += wxS(":lisp-quiet (setq $wxsubscripts ") +
    m_configuration.GetAutosubscript_string() + wxS(")\n");

  // A few variables for additional debug info in wxbuild_info();
  m_configCommands += wxString::Format(wxS(":lisp-quiet (setq wxUserConfDir \"%s\")\n"),
                                       EscapeForLisp(Dirstructure::Get()->UserConfDir()));
  m_configCommands += wxString::Format(wxS(":lisp-quiet (setq wxHelpDir \"%s\")\n"),
                                       EscapeForLisp(Dirstructure::Get()->HelpDir()));

  m_configCommands += wxString::Format(wxS(":lisp-quiet (setq $wxplot_size '((mlist simp) %i %i))\n"),
                                       m_configuration.DefaultPlotWidth(),
                                       m_configuration.DefaultPlotHeight());

  if (GetWorksheet() && (GetWorksheet()->m_currentFile != wxEmptyString)) {
    wxString filename(GetWorksheet()->m_currentFile);

    SetCWD(filename);
  }
  CallAfter([this]{if(m_symbolsSidebar != NULL) m_symbolsSidebar->UpdateUserSymbols();});
}

wxMaxima::wxMaxima(wxWindow *parent, int id,
                   const wxString &title, const wxString &filename,
                   const wxString &initialWorksheetContents,
                   const wxPoint pos, const wxSize size)
  : wxMaximaFrame(parent, id, title, pos, size,
                  wxDEFAULT_FRAME_STYLE | wxSYSTEM_MENU | wxCAPTION),
    m_gnuplotcommand(wxS("gnuplot")),
    m_parser(&m_configuration) {
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT
  wxHandleFatalExceptions();
  wxLogMessage(_("Will try to generate a stack backtrace, if the program ever crashes"));
#endif
  GnuplotCommandName(wxS("gnuplot"));



  if (m_variableReadActions.empty()) {
    m_variableReadActions[wxS("gentranlang")] =
      &wxMaxima::VariableActionGentranlang;
    m_variableReadActions[wxS("numer")] =
      &wxMaxima::VariableActionNumer;
    m_variableReadActions[wxS("display2d_unicode")] =
      &wxMaxima::VariableActionDisplay2d_Unicode;
    m_variableReadActions[wxS("maxima_userdir")] =
      &wxMaxima::VariableActionUserDir;
    m_variableReadActions[wxS("sinnpiflag")] =
      &wxMaxima::VariableActionSinnpiflag;
    m_variableReadActions[wxS("logexpand")] =
      &wxMaxima::VariableActionLogexpand;
    m_variableReadActions[wxS("opsubst")] = &wxMaxima::VariableActionOpSubst;
    m_variableReadActions[wxS("maxima_tempdir")] =
      &wxMaxima::VariableActionTempDir;
    m_variableReadActions[wxS("debugmode")] =
      &wxMaxima::VariableActionDebugmode;
    m_variableReadActions[wxS("*autoconf-version*")] =
      &wxMaxima::VariableActionAutoconfVersion;
    m_variableReadActions[wxS("*autoconf-host*")] =
      &wxMaxima::VariableActionAutoconfHost;
    m_variableReadActions[wxS("*maxima-infodir*")] =
      &wxMaxima::VariableActionMaximaInfodir;
    m_variableReadActions[wxS("*maxima-htmldir*")] =
      &wxMaxima::VariableActionMaximaHtmldir;
    m_variableReadActions[wxS("gnuplot_command")] =
      &wxMaxima::VariableActionGnuplotCommand;
    m_variableReadActions[wxS("*maxima-sharedir*")] =
      &wxMaxima::VariableActionMaximaSharedir;
    m_variableReadActions[wxS("*maxima-demodir*")] =
      &wxMaxima::VariableActionMaximaDemodir;
    m_variableReadActions[wxS("*lisp-name*")] =
      &wxMaxima::VariableActionLispName;
    m_variableReadActions[wxS("*lisp-version*")] =
      &wxMaxima::VariableActionLispVersion;
    m_variableReadActions[wxS("*wx-load-file-name*")] =
      &wxMaxima::VariableActionWxLoadFileName;
    m_variableReadActions[wxS("wxsubscripts")] =
      &wxMaxima::VariableActionWxSubscripts;
    m_variableReadActions[wxS("lmxchar")] = &wxMaxima::VariableActionLmxChar;
    m_variableReadActions[wxS("stringdisp")] =
      &wxMaxima::VariableActionStringdisp;
    m_variableReadActions[wxS("algebraic")] =
      &wxMaxima::VariableActionAlgebraic;
    m_variableReadActions[wxS("domain")] = &wxMaxima::VariableActionDomain;
    m_variableReadActions[wxS("wxanimate_autoplay")] =
      &wxMaxima::VariableActionAutoplay;
    m_variableReadActions[wxS("output_format_for_help")] =
      &wxMaxima::VariableActionHtmlHelp;
    m_variableReadActions[wxS("showtime")] = &wxMaxima::VariableActionShowtime;
    m_variableReadActions[wxS("engineering_format_floats")] =
      &wxMaxima::VariableActionEngineeringFormat;
    m_variableReadActions[wxS("display2d")] =
      &wxMaxima::VariableActionDisplay2D;
    m_variableReadActions[wxS("*alt-display2d*")] =
      &wxMaxima::VariableActionAltDisplay2D;
    m_variableReadActions[wxS("*maxima-operators*")] =
      &wxMaxima::VariableActionOperators;
  }

  if (m_variableUndefinedActions.empty()) {
    m_variableUndefinedActions[wxS("sinnpiflag")] =
      &wxMaxima::VariableActionSinnpiflagUndefined;
  }

  wxString lang;
  if (wxGetEnv("LANG", &lang))
    wxLogMessage("LANG=%s", lang);
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  //  wxWindowUpdateLocker noUpdates(this);

  wxConfigBase *config = wxConfig::Get();
  // If maxima fails to come up directly on startup of wxMaxima there is no need
  // to retry.
  m_CWD.Clear();
  m_gnuplot_process_id = wxWindow::NewControlId();
  m_maxima_process_id = wxWindow::NewControlId();
  config->Read(wxS("lastPath"), &m_lastPath);
  m_lastPrompt.Clear();

  UpdateRecentDocuments();
  bool logwindow_shown;
  // Read the last state (shown/not shown) of the log window from the configuration. If it was hidden before, hide it, else show it.
  // This is for issue #2033.
  if (config->Read(wxS("LogWindow"), &logwindow_shown)) {
    MyApp::m_logWindow->GetFrame()->Show(logwindow_shown);
  }

  m_oldFindString.Clear();
  int findFlags = wxFR_DOWN | wxFR_MATCHCASE;
  wxConfig::Get()->Read(wxS("Find/Flags"), &findFlags);
  m_findData.SetFlags(findFlags);
  bool findRegex = false;
  wxConfig::Get()->Read(wxS("Find/RegexSearch"), &findRegex);
  m_findData.SetRegexSearch(findRegex);
  if(GetWorksheet())
    GetWorksheet()->m_keyboardInactiveTimer.SetOwner(this,
                                                     KEYBOARD_INACTIVITY_TIMER_ID);
  m_maximaStdoutPollTimer.SetOwner(this, MAXIMA_STDOUT_POLL_ID);

  m_autoSaveTimer.SetOwner(this, AUTO_SAVE_TIMER_ID);
  Connect(wxEVT_SIZE, wxSizeEventHandler(wxMaxima::OnSize),
          NULL, this);
  Connect(wxEVT_MOVE, wxMoveEventHandler(wxMaxima::OnMove),
          NULL, this);
  Connect(wxEVT_MAXIMIZE, wxCommandEventHandler(wxMaxima::OnMaximize),
          NULL, this);

  Connect(wxEVT_TIMER, wxTimerEventHandler(wxMaxima::OnTimerEvent), NULL, this);

  if(m_wizard)
    {
      m_wizard->GetOKButton()->Connect(wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::OnWizardOK),
                                       NULL, this);
      m_wizard->GetAbortButton()->Connect(wxEVT_BUTTON,
                                          wxCommandEventHandler(wxMaxima::OnWizardAbort),
                                          NULL, this);
      m_wizard->GetInsertButton()->Connect(wxEVT_BUTTON,
                                           wxCommandEventHandler(wxMaxima::OnWizardInsert), NULL,
                                           this);
      m_wizard->Connect(wxEVT_BUTTON,
                        wxCommandEventHandler(wxMaxima::OnWizardHelpButton), NULL,
                        this);
    }
#ifdef wxHAS_POWER_EVENTS
  Connect(wxEVT_POWER_SUSPENDED, wxPowerEventHandler(wxMaxima::OnPowerEvent),
          NULL, this);
#endif

#if wxUSE_DRAG_AND_DROP
  if(GetWorksheet())
    GetWorksheet()->SetDropTarget(new MyDropTarget(this));
#endif

  StatusMaximaBusy(StatusBar::MaximaStatus::disconnected);

  m_statusBar->GetNetworkStatusElement()->Connect(
                                                  wxEVT_LEFT_DCLICK,
                                                  wxCommandEventHandler(wxMaxima::NetworkDClick),
                                                  NULL,
                                                  this);
  m_statusBar->GetMaximaStatusElement()->Connect(
                                                 wxEVT_LEFT_DCLICK,
                                                 wxCommandEventHandler(wxMaxima::MaximaDClick),
                                                 NULL,
                                                 this);

  m_statusBar->GetStatusTextElement()->Connect(
                                               wxEVT_LEFT_DCLICK,
                                               wxCommandEventHandler(wxMaxima::StatusMsgDClick),
                                               NULL,
                                               this);

  if (!filename.IsEmpty()) {
    m_openInitialFileError = !OpenFile(filename);
    //! wxm data the worksheet is populated from
  }
  else {
    if (!initialWorksheetContents.IsEmpty()) {
      //  Convert the comment block to an array of lines
      wxStringTokenizer tokenizer(initialWorksheetContents, "\n");
      std::vector<wxString> lines;
      while (tokenizer.HasMoreTokens())
        lines.push_back(tokenizer.GetNextToken());
      if(GetWorksheet())
        {
          GetWorksheet()->InsertGroupCells(Format::TreeFromWXM(lines, &m_configuration));
          GetWorksheet()->SetSaved(true);
        }
    }
  }

  if (!StartMaxima())
    StatusText(_("Starting Maxima process failed"));
  Connect(wxEVT_SCROLL_CHANGED, wxScrollEventHandler(wxMaxima::SliderEvent),
          NULL, this);
  Connect(wxID_CLOSE, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(EventIDs::menu_check_updates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::popid_copy_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_animation, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_svg, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_emf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_rtf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_insert_text, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_subsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_subsubsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_heading5, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_heading6, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_popup_gnuplot, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_show_cellbrackets, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_print_cellbrackets, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_delete, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_simplify, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_expand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_solve, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::enable_unicodePane, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_subst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_plot2d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_plot3d, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_diff, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_integrate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_float, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_matlab, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_copy_tex, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::EventIDs::popid_copy_text, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_animation_save, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_animation_start, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(EventIDs::button_integrate, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_diff, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_solve, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::button_solve_ode, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::button_sum, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_expand, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_factor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_taylor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_limit, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_ratsimp, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_trigexpand, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_trigreduce, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_trigsimp, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_product, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::button_radcan, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_subst, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::button_plot2, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::PlotMenu),
          NULL, this);
  Connect(EventIDs::button_plot3, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::PlotMenu),
          NULL, this);
  Connect(EventIDs::button_map, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_map, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_map_lambda, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_row, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_col, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_row_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_col_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_csv2mat, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_mat2csv, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_submatrix, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_multiply, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_exponent, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_hadamard_product, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_hadamard_exponent, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_copymatrix, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_loadLapack, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dgeev_eigenvaluesOnly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dgeev, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zgeev_eigenvaluesOnly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zgeev, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dgeqrf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dgesv, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dgesvd, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::EventIDs::menu_matrix_dgesvd_valuesOnly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dlange_max, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dlange_one, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dlange_inf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_dlange_frobenius, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zlange_max, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zlange_one, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zlange_inf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zlange_frobenius, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_matrix_zheev, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::button_rectform, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::button_trigrat, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_polarform, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(ToolBar::menu_restart_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_EXIT, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(wxID_ABOUT, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_license, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_changelog, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(wxID_SAVE, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(wxID_SAVEAS, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(EventIDs::menu_load_id, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(EventIDs::menu_functions, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_variables, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_arrays, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_macros, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_labels, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_myoptions, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_rules, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_aliases, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_structs, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_dependencies, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_gradefs, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_let_rule_packages, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_PREFERENCES, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_sconsole_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  Connect(EventIDs::menu_export_html, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::FileMenu), NULL, this);
  if(GetWorksheet())
    GetWorksheet()->m_autocomplete.Connect(NEW_DEMO_FILES_EVENT,
                                           wxCommandEventHandler(wxMaxima::OnNewDemoFiles),
                                           NULL, this);
  Connect(TOC_UPDATE_NEEDED_EVENT,
          wxCommandEventHandler(wxMaxima::OnUpdateTOCEvent),
          NULL, this);
  Connect(wxID_HELP, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_help_demo_for_command, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_help_maxima_homepage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_help_tutorials, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_goto_url, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_bug_report, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_build_info, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_interrupt_id, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::Interrupt), NULL, this);
  Connect(wxID_OPEN, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(EventIDs::menu_batch_id, wxEVT_MENU, wxCommandEventHandler(wxMaxima::FileMenu),
          NULL, this);
  Connect(EventIDs::menu_ratsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_radsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_expand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_expandwrt, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::EventIDs::menu_expandwrt_denom, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_scsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_xthru, wxEVT_MENU, wxCommandEventHandler(wxMaxima::SimplifyMenu),
          NULL, this);
  Connect(EventIDs::menu_factor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_horner, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_collapse, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_optimize, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_mainvar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_scanmapfactor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_gfactor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_trigsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_trigexpand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_trigreduce, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_rectform, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_demoivre, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_num_out, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_stringdisp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_num_domain, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_to_float, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_rat, wxEVT_MENU, wxCommandEventHandler(wxMaxima::NumericalMenu),
          NULL, this);
  Connect(EventIDs::menu_rationalize, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_guess_exact_value, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_to_bfloat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_to_numer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::popid_special_constant_percent, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::popid_changeasterisk, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::popid_hideasterisk, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_exponentialize, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_invert_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_determinant, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_rank, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_eigen, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_eigvect, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_adjoint_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_transpose, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_set_precision, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_set_displayprecision, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_engineeringFormat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_engineeringFormatSetup, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qag, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qags, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qagi, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qawc, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qawf_sin, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qawf_cos, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qawo_sin, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qawo_cos, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qaws1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qaws2, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qaws3, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qaws4, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);
  Connect(EventIDs::menu_quad_qagp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::NumericalMenu), NULL, this);

  Connect(EventIDs::menu_talg, wxEVT_MENU, wxCommandEventHandler(wxMaxima::SimplifyMenu),
          NULL, this);
  Connect(EventIDs::menu_tellrat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_modulus, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_allroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_bfallroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_realroots, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_solve, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_solve_to_poly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_solve_num, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_solve_ode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_map_mat, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_enter_mat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_cpoly, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_genmatrix, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_solve_lin, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_solve_algsys, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_eliminate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_clear_var, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_clear_fun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_ivp_1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_ivp_2, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_bvp, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EquationsMenu),
          NULL, this);
  Connect(EventIDs::menu_bvp, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EquationsMenu),
          NULL, this);
  Connect(EventIDs::menu_rk, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EquationsMenu),
          NULL, this);
  Connect(EventIDs::menu_fun_def, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_gensym, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_divide, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_gcd, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_lcm, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_continued_fraction, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_partfrac, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_risch, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_integrate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_laplace, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_ilt, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_diff, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_taylor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_powerseries, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_fourier, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_limit, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_lbfgs, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_gen_mat, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MatrixMenu),
          NULL, this);
  Connect(EventIDs::menu_gen_mat_lambda, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_sum, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_maximahelp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_wxmaximahelp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_example, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_apropos, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_wxmaxima_uses_help_sidebar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_wxmaxima_uses_help_browser, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_maxima_uses_html_help, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_maxima_uses_internal_help, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_maxima_uses_wxmaxima_help, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::HelpMenu), NULL, this);
  Connect(EventIDs::menu_show_tip, wxEVT_MENU, wxCommandEventHandler(wxMaxima::HelpMenu),
          NULL, this);
  Connect(EventIDs::menu_trigrat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_solve_de, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_atvalue, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_lhs, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EquationsMenu),
          NULL, this);
  Connect(EventIDs::menu_rhs, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EquationsMenu),
          NULL, this);
  Connect(EventIDs::menu_construct_fraction, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::menu_sum, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_product, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_change_var, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::CalculusMenu), NULL, this);
  Connect(EventIDs::menu_time, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_factsimp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_factcomb, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_realpart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_imagpart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_nouns, wxEVT_MENU, wxCommandEventHandler(wxMaxima::SimplifyMenu),
          NULL, this);
  Connect(EventIDs::menu_simpsum, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_subst, wxEVT_MENU, wxCommandEventHandler(wxMaxima::SimplifyMenu),
          NULL, this);
  Connect(EventIDs::menu_psubst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_ratsubst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_fullratsubst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_at, wxEVT_MENU, wxCommandEventHandler(wxMaxima::SimplifyMenu),
          NULL, this);
  Connect(EventIDs::menu_substinpart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_opsubst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logcontract, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand_false, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand_true, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_logexpand_super, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::gp_plot2, wxEVT_MENU, wxCommandEventHandler(wxMaxima::PlotMenu), NULL,
          this);
  Connect(EventIDs::gp_plot3, wxEVT_MENU, wxCommandEventHandler(wxMaxima::PlotMenu), NULL,
          this);
  Connect(EventIDs::menu_animationautostart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(EventIDs::menu_animationframerate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(EventIDs::menu_plot_format, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PlotMenu), NULL, this);
  Connect(EventIDs::menu_soft_restart, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs:: menu_kill_dependencies, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_values, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_functions, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_arrays, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_myoptions, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_rules, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_aliases, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_structures, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_labels, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_gradefs, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_props, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_macros, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_kill_let_rule_packages, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_garbage_collect, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_room, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_jumptoerror, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_display, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_pade, wxEVT_MENU, wxCommandEventHandler(wxMaxima::CalculusMenu),
          NULL, this);
  Connect(EventIDs::menu_add_path, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(wxID_COPY, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_copy_text_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_tex_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_matlab_from_worksheet, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_copy_mathml, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_UNDO, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxID_UNDO, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxID_REDO, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxID_REDO, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_texform, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_grind, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_debugmode_lisp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_debugmode_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_debugmode_off, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_for, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_while, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_block, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_block_noLocal, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_local, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_return, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_trace, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_lambda, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_quotequote, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_quote, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_quoteblock, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_def_fun, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_def_macro, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_def_variable, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_compile, wxEVT_MENU, wxCommandEventHandler(wxMaxima::MaximaMenu),
          NULL, this);
  Connect(EventIDs::menu_paramType, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_structdef, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_structnew, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_structuse, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_saveLisp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_loadLisp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_maximatostring, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringtomaxima, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);

  Connect(EventIDs::menu_stringproc_setposition, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_getposition, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_flush_output, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_flength, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_close, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_opena, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_openr, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_openw, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_printf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_readline, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_readchar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_readbyte, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_writebyte, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_charp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_alphacharp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_alphanumericp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_digitcharp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_constituent, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_uppercasep, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_lowercasep, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_create_ascii, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_cequal, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_cequalignore, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_clessp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_clesspignore, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_cgreaterp, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_cgreaterpignore, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sequal, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sequalignore, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_ascii, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_cint, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_unicode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_unicode_to_utf8, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_utf8_to_unicode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_charat, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_charlist, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_simplode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sinsert, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_eval_string, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_parse_string, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_scopy, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sdowncase, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_slength, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_smake, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_smismatch, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_split, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sposition, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sremove, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_sremovefirst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_tokens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_ssearch, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_ssort, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_ssubstfirst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_strim, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_striml, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_strimr, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_number_to_octets, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_octets_to_number, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_octets_to_string, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_stringproc_string_to_octets, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_sregex_load, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_regex_compile, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_regex_match_pos, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_regex_match, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_regex_split, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_subst_first, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_regex_subst, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_sregex_string_to_regex, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));

  Connect(EventIDs::menu_opsyst_load, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_chdir, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_mkdir, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_rmdir, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_getcurrentdirectory, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_copy_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_rename_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_delete_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_getenv, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_directory, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_pathname_directory, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_pathname_name, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::menu_opsyst_pathname_type, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));

  Connect(EventIDs::gentran_load, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::gentran_lang_c, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::gentran_lang_fortran, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::gentran_lang_ratfor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::gentran_to_stdout, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));
  Connect(EventIDs::gentran_to_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu));

  Connect(EventIDs::menu_to_fact, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(EventIDs::menu_to_gamma, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::SimplifyMenu), NULL, this);
  Connect(wxID_PRINT, wxEVT_MENU, wxCommandEventHandler(wxMaxima::PrintMenu),
          NULL, this);
  Connect(wxID_ZOOM_IN, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxID_ZOOM_OUT, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_zoom_80, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxID_ZOOM_100, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_zoom_120, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_zoom_150, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_zoom_200, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_zoom_300, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::popid_labelwidth1, EventIDs::popid_labelwidth1 + LABELWIDTH_MAX - LABELWIDTH_MIN,
          wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_digits_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_digits_all_linebreak, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_digits_20, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_digits_50, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_digits_100, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_labels_autogenerated, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_inputlabels_hide, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_labels_user, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_labels_useronly, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_labels_disable, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_math_as_1D_ASCII, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_math_as_2D_ASCII, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_math_as_2D_UNICODE, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_math_as_graphics, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::internalRepresentation, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::wxMathML, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_noAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_defaultAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_alwaysAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_declareAutosubscript, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_autosubscriptIndividual, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_noAutosubscriptIndividual, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_roundedMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_straightMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_angledMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_squareMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_noMatrixParens, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_fullscreen, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_invertWorksheetBackground, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_show_logwindow, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(ToolBar::tb_hideCode, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_as_bitmap, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_as_svg, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_as_emf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_as_rtf, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_copy_to_file, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
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
  Connect(wxEVT_SOCKET, wxSocketEventHandler(wxMaxima::ServerEvent), NULL,
          this);
  Connect(wxEVT_CLOSE_WINDOW, wxCloseEventHandler(wxMaxima::OnClose), NULL,
          this);
  Connect(wxEVT_QUERY_END_SESSION, wxCloseEventHandler(wxMaxima::OnClose), NULL,
          this);
  Connect(wxEVT_END_SESSION, wxCloseEventHandler(wxMaxima::OnClose), NULL,
          this);
  Connect(m_maxima_process_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnMaximaClose), NULL, this);
  Connect(EventIDs::gnuplot_query_terminals_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnGnuplotQueryTerminals), NULL, this);
  Connect(m_gnuplot_process_id, wxEVT_END_PROCESS,
          wxProcessEventHandler(wxMaxima::OnGnuplotClose), NULL, this);
  Connect(EventIDs::popid_edit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditInputMenu), NULL, this);
  Connect(EventIDs::menu_evaluate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EvaluateEvent), NULL, this);
  Connect(EventIDs::popid_var_newVar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::VarReadEvent), NULL, this);
  Connect(EventIDs::popid_var_addAll, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::VarAddAllEvent), NULL, this);
  Connect(EventIDs::menu_add_comment, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_subsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_subsubsection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_heading5, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_heading6, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_add_pagebreak, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_fold_all_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_unfold_all_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_add_comment, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_add_watch, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_add_watch_label, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_insert_previous_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_insert_previous_output, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_autocomplete, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_autocomplete_templates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_insert_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_insert_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_history_previous, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_history_next, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_PASTE, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::menu_paste_input, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(wxID_CUT, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu), NULL,
          this);
  Connect(wxID_SELECTALL, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(EventIDs::popid_comment_selection, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_divide_cell, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_evaluate, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_evaluate_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_eval, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_eval_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(ToolBar::tb_evaluate_rest, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(ToolBar::tb_evaltillhere, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_merge_cells, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_maxsizechooser, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_resolutionchooser, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_reloadimage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_change_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_Fold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_Unfold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_SelectTocChapter, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_EvalTocChapter, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_evaluate_section, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_ToggleTOCshowsSectionNumbers, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_tocLevel1, EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_tocMoveIn, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_tocMoveOut, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_tocdnd, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_fold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::popid_unfold, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PopupMenu), NULL, this);
  Connect(EventIDs::menu_evaluate_all_visible, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_evaluate_all, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(ToolBar::tb_evaltillhere, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::MaximaMenu), NULL, this);
  Connect(EventIDs::menu_list_create_from_elements, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_create_from_rule, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_create_from_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_actual_values_storage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_sort, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_length, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_push, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_pop, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_reverse, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_first, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_last, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_rest, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_restN, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_lastn, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_nth, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_map, wxEVT_MENU, wxCommandEventHandler(wxMaxima::ListMenu),
          NULL, this);
  Connect(EventIDs::menu_list_use_actual_values, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_as_function_arguments, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_extract_value, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_do_for_each_element, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_remove_duplicates, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_remove_element, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_append_item_start, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_append_item_end, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_append_list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_interleave, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_list2matrix, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_matrix2list, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_list_create_from_args, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ListMenu), NULL, this);
  Connect(EventIDs::menu_draw_2d, wxEVT_MENU, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_2d, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_3d, wxEVT_MENU, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_3d, wxEVT_BUTTON, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_fgcolor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_fgcolor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_fillcolor, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_fillcolor, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_title, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_title, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_key, wxEVT_MENU, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_key, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_explicit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_explicit, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_implicit, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_implicit, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_parametric, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_parametric, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_points, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_points, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_axis, wxEVT_MENU, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_axis, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_contour, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_contour, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_accuracy, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_accuracy, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(EventIDs::menu_draw_grid, wxEVT_MENU, wxCommandEventHandler(wxMaxima::DrawMenu),
          NULL, this);
  Connect(EventIDs::menu_draw_grid, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::DrawMenu), NULL, this);
  Connect(wxEVT_IDLE, wxIdleEventHandler(wxMaxima::OnIdle), NULL, this);
  Connect(EventIDs::menu_remove_output, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_hide_tooltipMarker, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_hide_tooltipMarkerForThisMessage, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::menu_recent_document_0, EventIDs::menu_recent_document_0 + EventIDs::NumberOfRecentFiles - 1,
          wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::OnRecentDocument), NULL, this);
  Connect(EventIDs::menu_recent_package_0, EventIDs::menu_recent_package_0 + EventIDs::NumberOfRecentFiles - 1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::OnRecentPackage));
  Connect(EventIDs::menu_unsaved_document_0, EventIDs::menu_unsaved_document_0 + EventIDs::NumberOfRecentFiles - 1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::OnUnsavedDocument));
  Connect(EventIDs::menu_insert_image, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  for(const auto &sidebar: GetSidebarNames())
    Connect(sidebar.first, wxEVT_MENU,
            wxCommandEventHandler(wxMaxima::ShowPane));
  Connect(EventIDs::menu_pane_toolbar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EditMenu), NULL, this);
  Connect(EventIDs::popid_auto_answer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::popid_never_autoanswer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(history_ctrl_id, wxEVT_LISTBOX_DCLICK,
          wxCommandEventHandler(wxMaxima::HistoryDClick), NULL, this);
  Connect(structure_ctrl_id, wxEVT_LIST_ITEM_ACTIVATED,
          wxListEventHandler(wxMaxima::TableOfContentsSelection), NULL, this);
  Connect(EventIDs::menu_stats_histogram, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_piechart, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_scatterplot, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_barsplot, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_boxplot, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_mean, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_median, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_var, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_dev, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_tt1, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_tt2, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_tnorm, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_linreg, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_lsquares, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_readm, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_stats_enterm, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::MatrixMenu), NULL, this);
  Connect(EventIDs::menu_stats_subsample, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::StatsMenu), NULL, this);
  Connect(EventIDs::menu_format_title, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_text, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_heading6, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_heading5, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_subsubsection, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_subsection, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_section, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_pagebreak, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(EventIDs::menu_format_image, wxEVT_BUTTON,
          wxCommandEventHandler(wxMaxima::InsertMenu), NULL, this);
  Connect(wxEVT_CHAR, wxCharEventHandler(wxMaxima::OnChar), NULL, this);
  Connect(wxEVT_KEY_DOWN, wxCharEventHandler(wxMaxima::OnKeyDown), NULL, this);
  Connect(ToolBar::tb_changeStyle, wxEVT_CHOICE,
          wxCommandEventHandler(wxMaxima::ChangeCellStyle), NULL, this);
  Connect(wxID_FIND, wxEVT_MENU, wxCommandEventHandler(wxMaxima::EditMenu),
          NULL, this);
  Connect(wxEVT_FIND, wxFindDialogEventHandler(wxMaxima::OnFind), NULL, this);
  Connect(wxEVT_FIND_NEXT, wxFindDialogEventHandler(wxMaxima::OnFind), NULL,
          this);
  Connect(wxEVT_FIND_REPLACE, wxFindDialogEventHandler(wxMaxima::OnReplace),
          NULL, this);
  Connect(wxEVT_FIND_REPLACE_ALL,
          wxFindDialogEventHandler(wxMaxima::OnReplaceAll), NULL, this);
  Connect(wxEVT_SET_FOCUS, wxFocusEventHandler(wxMaxima::OnFocus), NULL, this);
  Connect(wxEVT_ICONIZE, wxIconizeEventHandler(wxMaxima::OnMinimize), NULL,
          this);
  Connect(SYMBOLADDEVENT, wxCommandEventHandler(wxMaxima::OnSymbolAdd), NULL,
          this);
  Connect(EventIDs::popid_suggestion1, EventIDs::popid_suggestion1 + EventIDs::NumberOfSuggestions - 1, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::ReplaceSuggestion), NULL, this);
  Connect(EventIDs::popid_property_real, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_imaginary, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_complex, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_additive, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_alphabetic, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_bindtest, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_antisymmetric, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_commutative, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_symmetric, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_constant, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_even, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_odd, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_evenfun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_atvalue, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::EquationsMenu), NULL, this);
  Connect(EventIDs::popid_property_oddfun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_increasing, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_decreasing, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_integer, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_noninteger, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_integervalued, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_lassociative, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_rassociative, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_linear, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_mainvar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_multiplicative, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_nary, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_nonarray, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_nonscalar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_scalar, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_noun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_outative, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_posfun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_rational, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_irrational, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_greaterThan, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_evfun, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  Connect(EventIDs::popid_property_evflag, wxEVT_MENU,
          wxCommandEventHandler(wxMaxima::PropertiesMenu), NULL, this);
  // Make wxWidgets remember the size and position of the wxMaxima window
  SetName(wxS("wxMaxima"));
  if (!wxPersistenceManager::Get().RegisterAndRestore(this)) {
    // We don't remember the window size from a previous wxMaxima run
    // => Make sure the window is at least half-way big enough to make sense.
    wxSize winSize = wxSize(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * .75,
                            wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * .75);
    if (winSize.x < 800)
      winSize.x = 800;
    if (winSize.y < 600)
      winSize.y = 600;
    SetSize(winSize);
  }

  if(GetWorksheet())
    CallAfter([this]{GetWorksheet()->SetFocus();});
  StartAutoSaveTimer();
  MyApp::m_windowcount++;
  Layout();
}

#ifdef wxHAS_POWER_EVENTS
void wxMaxima::OnPowerEvent(wxPowerEvent &event) {
  AutoSave();
  event.Skip();
}
#endif

void wxMaxima::OnSize(wxSizeEvent &event){
  wxConfig::Get()->Write("MainWindowPos/width", event.GetSize().GetWidth());
  wxConfig::Get()->Write("MainWindowPos/height", event.GetSize().GetHeight());
  bool maximized = false;
  wxConfig::Get()->Write("MainWindowPos/maximized", maximized);
  event.Skip();
}

void wxMaxima::OnNewDemoFiles(wxCommandEvent &WXUNUSED(event))
{
  if(!GetWorksheet())
    return;

  m_demoFilesIDs.clear();
  while(m_demo_sub->GetMenuItemCount() > 0)
      m_demo_sub->Delete(m_demo_sub->FindItemByPosition(0));

  auto filesList = GetWorksheet()->m_autocomplete.GetDemoFilesList();
  if(filesList.size() < 1)
    return;

  std::vector<wxString> subMenuContents;
  for(const auto &i : filesList)
    {
      wxString name = i.SubString(1,i.Length() - 2);
      if(!name.IsEmpty())
        {
          subMenuContents.push_back(name);
          if(subMenuContents.size() > 11)
            {
              wxMenu *subMenu = new wxMenu();
              for(const auto &i : subMenuContents)
                {
                  wxWindowID id = wxWindow::NewControlId();
                  m_demoFilesIDs[id] = i;
                  subMenu->Append(id, i);
                }
              m_demo_sub->Append(wxWindow::NewControlId(),
                                 subMenuContents.front() + wxS("-") + subMenuContents.back(),
                                 subMenu);
              subMenu->Connect(wxEVT_MENU,
                                   wxCommandEventHandler(wxMaxima::OnDemoFileMenu), NULL, this);
              subMenuContents.clear();
            }
        }
    }
  if(subMenuContents.size() > 0)
    {
      wxMenu *subMenu = new wxMenu();
      for(const auto &i : subMenuContents)
        {
          wxWindowID id = wxWindow::NewControlId();
          m_demoFilesIDs[id] = i;
          subMenu->Append(id, i);
        }
      m_demo_sub->Append(wxWindow::NewControlId(),
                         subMenuContents.front() + wxS(" - ") + subMenuContents.back(),
                         subMenu);
    }
}

void wxMaxima::OnDemoFileMenu(wxCommandEvent &ev)
{
  wxString demoName = GetDemoFile(ev.GetId());
  if(!demoName.IsEmpty())
    MenuCommand(wxS("demo(") + demoName + wxS(");"));
}

void wxMaxima::OnMove(wxMoveEvent &event){
  wxConfig::Get()->Write("MainWindowPos/x", event.GetPosition().x);
  wxConfig::Get()->Write("MainWindowPos/y", event.GetPosition().y);
  bool maximized = false;
  wxConfig::Get()->Write("MainWindowPos/maximized", maximized);
  event.Skip();
}
void wxMaxima::OnMaximize(wxCommandEvent &event){
  bool maximized = true;
  wxConfig::Get()->Write("MainWindowPos/maximized", maximized);
  event.Skip();
}

void wxMaxima::StartAutoSaveTimer() {
  m_autoSaveTimer.StartOnce(60000 * m_configuration.AutosaveMinutes());
}

wxMaxima::~wxMaxima() {
  // If the gnuplot processes still exist we sever bonds with them
  // so they don't inform us about anything if wxMaxima no more
  // exists
  if(m_gnuplotTerminalQueryProcess)
    m_gnuplotTerminalQueryProcess ->Detach();
  if(m_gnuplotProcess)
    m_gnuplotProcess ->Detach();

  // Kill maxima
  KillMaxima(false);


  // In debug mode: Create a file describing what we know about maxima commands
  if(m_configuration.GetDebugmode() && (!Dirstructure::Get()->UserConfDir().IsEmpty()))
    {
      std::unordered_map<wxString, std::int_fast8_t, wxStringHash> knownWords;
      for(const auto &i : GetWorksheet()->m_autocomplete.GetSymbolList())
          knownWords[i] |= 1;
      for(const auto &i : GetWorksheet()->m_maximaManual.GetHelpfileAnchors())
        knownWords[i.first] |= 2;

      wxString maxima_share_dir = m_configuration.MaximaShareDir();

      if (maxima_share_dir.length () > 0)
        {
          wxFileInputStream builtintxt(maxima_share_dir + "/builtins-list.txt");
          if(builtintxt.IsOk())
            {
              wxTextInputStream txt(builtintxt);
              while(!builtintxt.Eof())
                {
                  wxString line;
                  line = txt.ReadLine();
                  knownWords[line] |= 4;
                }
            }
        }

      std::vector<wxString> knownwords_sorted;
      for(const auto &i : knownWords)
        knownwords_sorted.push_back(i.first);
      std::sort(knownwords_sorted.begin(), knownwords_sorted.end());

      wxFile fil(Dirstructure::Get()->UserConfDir() + "/knownSymbols.txt",
                 wxFile::write);
      wxFileOutputStream fstrm(fil);
      wxTextOutputStream txtstrm(fstrm);
      txtstrm.WriteString("Autodetection\tdescribe()\tbuiltins-list.txt\tsymbol\n");
      for(const auto &i : knownwords_sorted)
        {
          wxString line;
          std::int_fast8_t flags = knownWords[i];
          if(flags & 1)
            line = wxS("\u2717");
          line += wxS("\t");
          if(flags & 2)
            line += wxS("\u2717");
          line += wxS("\t");
          if(flags & 4)
            line += wxS("\u2717");
          line += wxS("\t");
          line += i + wxS("\n");
          txtstrm.WriteString(line);
        }
      fil.Close();
    }

  // Allow the operating system to keep the clipboard's contents even after we
  // exit - if that option is supported by the OS.
  if (wxTheClipboard->Open()) {
    wxTheClipboard->Flush();
    wxTheClipboard->Close();
  }
  if (m_fileSaved)
    RemoveTempAutosavefile();
  wxLogMessage("Window count (before closing the current window): %d", MyApp::m_windowcount);
  MyApp::m_windowcount--;
  // If we deleted the last Window, delete the log Window
  if (MyApp::m_windowcount == 0) {
    // save the current state of the log window (shown/hidden) in the configuration.
    wxConfig::Get()->Write("LogWindow", MyApp::m_logWindow->GetFrame()->IsShown());
    wxDELETE(MyApp::m_logWindow);
    /* On the mac wxMaxima might still run if the last window has closed.
       This means we have no log window and therefore cannot log anything without
       creating pop-ups. */
    wxLog::EnableLogging(false);
  }
}

#if wxUSE_DRAG_AND_DROP

bool MyDropTarget::OnDropFiles(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y),
                               const wxArrayString &files) {
  if(!m_wxmax->GetWorksheet())
    return false;

  bool success = true;
  for(const auto &file:files)
    {
      if (wxGetKeyState(WXK_SHIFT)) {
        m_wxmax->GetWorksheet()->InsertText(file);
      }
      else if (file.Lower().EndsWith(wxS(".wxm")) ||
               file.Lower().EndsWith(wxS(".wxm~")) ||
               file.Lower().EndsWith(wxS("contents.xml")) ||
               file.Lower().EndsWith(wxS(".mac")) ||
               file.Lower().EndsWith(wxS(".wxmx")) ||
               file.Lower().EndsWith(wxS(".wxmx~"))) {
        if (m_wxmax->GetWorksheet()->IsEmpty())
          {
            m_wxmax->OpenFile(file);
            continue;
          }
        else
          MyApp::NewWindow(file);
      }
      else if (file.Lower().EndsWith(wxS(".png")) ||
               file.Lower().EndsWith(wxS(".jpeg")) ||
               file.Lower().EndsWith(wxS(".jpg")) ||
               file.Lower().EndsWith(wxS(".gif")) ||
               file.Lower().EndsWith(wxS(".svg")) ||
               file.Lower().EndsWith(wxS(".svgz"))) {
        m_wxmax->LoadImage(file);
      }
      else if (file.Lower().EndsWith(wxS(".txt")))
        {
          m_wxmax->GetWorksheet()->InsertText(file);
        }
      else
        success = false;
    }
  return success;
}

#endif

void wxMaxima::FirstOutput() {
  m_lastPrompt = wxS("(%i1) ");
  if(GetWorksheet())
    CallAfter([this]{GetWorksheet()->SetFocus();});
}

///--------------------------------------------------------------------------------
///  Appending stuff to output
///--------------------------------------------------------------------------------

void wxMaxima::ConsoleAppend(wxXmlDocument xml, CellType type,
                                  const wxString &userLabel) {
  if(!GetWorksheet())
    return;

  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (GetWorksheet()->GetTree() == NULL)
    GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_configuration, GC_TYPE_CODE));
  m_dispReadOut = false;
  GroupCell *tmp = GetWorksheet()->GetWorkingGroup(true);

  if (tmp == NULL) {
    if (GetWorksheet()->GetActiveCell())
      tmp = GetWorksheet()->GetActiveCell()->GetGroup();
  }
  if(tmp != NULL)
    {
      m_parser.SetUserLabel(userLabel);
      m_parser.SetGroup(GetWorksheet()->GetInsertGroup());
      std::unique_ptr<Cell> cell(m_parser.ParseLine(xml, type));
      m_parser.SetGroup(nullptr);
      // TODO: Does using || make any sense here?
      GetWorksheet()->InsertLine(std::move(cell),
                                 (AppendOpt::DefaultOpt & AppendOpt::NewLine) ||
                                 cell->BreakLineHere());
    }
}

/*! ConsoleAppend adds a new line s of type to the console window.
 *
 * It will call
 * DoConsoleAppend if s is in xml and DoRawCosoleAppend if s is not in xml.
 */
TextCell *wxMaxima::ConsoleAppend(wxString s, CellType type) {
  if(!GetWorksheet())
    return NULL;

  TextCell *lastLine = NULL;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (GetWorksheet()->GetTree() == NULL)
    GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_configuration, GC_TYPE_CODE));

  m_dispReadOut = false;
  s.Replace(m_promptSuffix, wxEmptyString);

  // If the string we have to append only contains whitespace we return
  // immediately.
  // TODO: Is a printf(false,"~%")$ a real use-case?
  wxString t(s);
  t.Trim();
  t.Trim(false);
  if (t.IsEmpty())
    return NULL;

  if (m_maxOutputCellsPerCommand > 0) {
    // If we already have output more lines than we are allowed, we inform the
    // user about this and return.
    if (m_outputCellsFromCurrentCommand == m_maxOutputCellsPerCommand) {
      {
        DoRawConsoleAppend(_("... [suppressed additional lines as the output "
                             "is longer than allowed in the wxMaxima configuration] "),
                           MC_TYPE_ERROR);
        m_outputCellsFromCurrentCommand++;
      }
      return NULL;
    } else {
      m_outputCellsFromCurrentCommand++;
    }

    // If we already have output more lines than we are allowed to and we
    // already have informed the user about this we return immediately
    if (m_outputCellsFromCurrentCommand > m_maxOutputCellsPerCommand)
      return NULL;
  }

  if ((type != MC_TYPE_ERROR) && (type != MC_TYPE_WARNING))
    StatusMaximaBusy(StatusBar::MaximaStatus::parsing);

  if (type == MC_TYPE_DEFAULT) {
    // Show a busy cursor whilst interpreting and layouting potentially long
    // data from maxima.
    wxBusyCursor crs;

    lastLine = DoRawConsoleAppend(s, type);
  } else if (type == MC_TYPE_PROMPT) {
    m_lastPrompt = s;

    if (s.StartsWith(wxS("MAXIMA> "))) {
      s = s.Right(8);
    } else
      s = s + wxS(" ");

    DoConsoleAppend(wxS("<span>") + s + wxS("</span>"), type,
                    AppendOpt(AppendOpt::NewLine | AppendOpt::BigSkip));
  } else if (type == MC_TYPE_ERROR) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ERROR);
    GroupCell *tmp = GetWorksheet()->GetWorkingGroup(true);

    if (tmp == NULL) {
      if (GetWorksheet()->GetActiveCell())
        tmp = GetWorksheet()->GetActiveCell()->GetGroup();
    }

    if (tmp != NULL) {
      GetWorksheet()->GetErrorList().Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_commandIndex - 1);
    }
  } else if (type == MC_TYPE_WARNING) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_WARNING);
  } else if (type == MC_TYPE_TEXT) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_TEXT);
  } else if (type == MC_TYPE_ASCIIMATHS) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ASCIIMATHS);
  } else
    DoConsoleAppend(wxS("<span>") + s + wxS("</span>"), type,
                    AppendOpt::BigSkip);

  return lastLine;
}

void wxMaxima::DoConsoleAppend(wxString s, CellType type, AppendOpt opts,
                               const wxString &userLabel) {
  if (s.IsEmpty())
    return;

  if(!GetWorksheet())
    return;

  s.Replace(wxS("\n"), wxS(" "), true);

  m_parser.SetUserLabel(userLabel);
  m_parser.SetGroup(GetWorksheet()->GetInsertGroup());
  std::unique_ptr<Cell> cell(m_parser.ParseLine(s, type));
  m_parser.SetGroup(nullptr);

  if (!cell)
    {
      DoRawConsoleAppend(_("There was an error in the XML maxima has generated.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
      return;
    }

  cell->SetBigSkip(opts & AppendOpt::BigSkip);
  auto *textCell = dynamic_cast<TextCell *>(cell.get());
  if (textCell)
    {
      textCell->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
      bool breakLine = cell->BreakLineHere();
      GetWorksheet()->InsertLine(std::move(cell),
                                 (opts & AppendOpt::NewLine) || breakLine);
    }
}

TextCell *wxMaxima::DoRawConsoleAppend(wxString s, CellType type,
                                       AppendOpt opts) {
  if(!GetWorksheet())
    return NULL;

  TextCell *cell = nullptr;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (GetWorksheet()->GetTree() == NULL)
    GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_configuration, GC_TYPE_CODE));

  if (s.IsEmpty())
    return NULL;

  bool scrollToCaret =
    (!GetWorksheet()->FollowEvaluation() && GetWorksheet()->CaretVisibleIs());

  if (type == MC_TYPE_MAIN_PROMPT) {
    auto owned = std::make_unique<LabelCell>(
                                             GetWorksheet()->GetTree(), &m_configuration, s,
                                             TS_MAIN_PROMPT);
    owned->SetType(type);
    owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
    cell = owned.get();
    GetWorksheet()->InsertLine(std::move(owned), true);
  } else if (type == MC_TYPE_PROMPT) {
    auto owned = std::make_unique<TextCell>(
                                             GetWorksheet()->GetTree(), &m_configuration, s,
                                             TS_OTHER_PROMPT);
    owned->SetType(type);
    owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
    cell = owned.get();
    GetWorksheet()->InsertLine(std::move(owned), true);
  } else {
    std::unique_ptr<LabelCell> ownedCell;
    TextCell *incompleteTextCell = nullptr;
    if (type == MC_TYPE_PROMPT) {
      incompleteTextCell = new LabelCell(GetWorksheet()->GetTree(),
                                         &m_configuration,
                                         wxEmptyString, TS_OTHER_PROMPT);
      incompleteTextCell->ForceBreakLine(true);
    } else
      incompleteTextCell = GetWorksheet()->GetCurrentTextCell();

    if (incompleteTextCell) {
      auto pos = s.Find("\n");
      wxString newVal = incompleteTextCell->GetValue();
      if (pos != wxNOT_FOUND) {
        newVal += s.Left(pos);
        s = s.Right(s.Length() - pos - 1);
      } else {
        newVal += s;
        s.Clear();
      }

      incompleteTextCell->SetValue(newVal);
      GetWorksheet()->InsertLine(std::move(ownedCell));
      if (s.IsEmpty()) {
        return incompleteTextCell;
      }
    }

    wxStringTokenizer tokens(s, wxS("\n"));
    CellListBuilder<Cell> tree;
    while (tokens.HasMoreTokens()) {
      wxString token = tokens.GetNextToken();
      // Move endless streams of compilation messages to the status bar...
      if (m_sbclCompilationRegEx.Matches(token)) {
        wxString fileName = token;
        m_sbclCompilationRegEx.Replace(&fileName, wxS("\\1"));
        StatusText(
                   wxString::Format(_("Compiling %s"), fileName));
      } else {
        auto owned = std::make_unique<TextCell>(GetWorksheet()->GetTree(),
                                                &m_configuration, token);
        owned->SetType(type);
        owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
        cell = owned.get();

        if (tokens.HasMoreTokens())
          cell->SetBigSkip(false);

        auto breakLine = static_cast<bool>(tree);
        tree.Append(std::move(owned));
        if (breakLine)
          tree.GetLastAppended()->ForceBreakLine(true);
      }
    }
    GetWorksheet()->InsertLine(std::move(tree), true);
  }

  if (cell) {
    GetWorksheet()->Recalculate(cell->GetGroup());
    if (scrollToCaret)
      GetWorksheet()->ScrollToCaret();
    GetWorksheet()->RequestRedraw();
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
void wxMaxima::StripLispComments(wxString &s) {
  if (s.StartsWith(wxS(":lisp\n")) || s.StartsWith(wxS(":lisp "))) {
    int start = 0;
    int commentStart = 0;
    while ((commentStart = s.find(wxS(';'), start)) != wxNOT_FOUND) {
      int commentEnd = s.find(wxS('\n'), commentStart);
      if (commentEnd == wxNOT_FOUND)
        commentEnd = s.length();
      s = s.SubString(0, static_cast<std::size_t>(commentStart) - 1) +
        s.SubString(static_cast<std::size_t>(commentEnd), s.length());
    }
  } else
    m_blankStatementRegEx.Replace(&s, wxS(";"));
}

void wxMaxima::SendMaxima(wxString s, bool addToHistory) {
  // Normally we catch parenthesis errors before adding cells to the
  // evaluation queue. But if the error is introduced only after the
  // cell is placed in the evaluation queue we need to catch it here.
  std::size_t index;
  wxString parenthesisError = GetUnmatchedParenthesisState(s, index);
  if (parenthesisError.IsEmpty()) {
    if(GetWorksheet())
      s = GetWorksheet()->UnicodeToMaxima(s);

    if ((m_xmlInspector) && (IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_xmlInspector->Add_ToMaxima(s);

    m_dispReadOut = false;

    if (addToHistory)
      AddToHistory(s);

    if (s.StartsWith(wxS(":lisp ")) || s.StartsWith(wxS(":lisp\n")))
      s.Replace(wxS("\n"), wxS(" "));

    s.Trim(true);
    s.Append(wxS("\n"));
    /// Check for function/variable definitions
    wxStringTokenizer commands(s, wxS(";$"));
    while (commands.HasMoreTokens()) {
      wxString line = commands.GetNextToken();
      if(GetWorksheet())
        {
          if (m_varRegEx.Matches(line))
            GetWorksheet()->AddSymbol(m_varRegEx.GetMatch(line, 1));

          if (m_funRegEx.Matches(line)) {
            wxString funName = m_funRegEx.GetMatch(line, 1);
            GetWorksheet()->AddSymbol(funName);
        /// Create a template from the input
        wxString args = m_funRegEx.GetMatch(line, 2);
        wxStringTokenizer argTokens(args, wxS(","));
        funName << wxS("(");
        int count = 0;
        while (argTokens.HasMoreTokens()) {
          if (count > 0)
            funName << wxS(",");
          wxString a = argTokens.GetNextToken().Trim().Trim(false);
          if (a != wxEmptyString) {
            if (a.at(0) == '[')
              funName << wxS("[<") << a.SubString(1, a.Length() - 2)
                      << wxS(">]");
            else
              funName << wxS("<") << a << wxS(">");
            count++;
          }
        }
        funName << wxS(")");
          GetWorksheet()->AddSymbol(funName, AutoComplete::tmplte);
}
      }
    }

    if ((m_client) && (m_client->IsConnected()) && (s.Length() >= 1)) {
      // If there is no working group and we still are trying to send something
      // we are trying to change maxima's settings from the background and might
      // never get an answer that changes the status again.
      if (GetWorksheet() && (GetWorksheet()->GetWorkingGroup()))
        StatusMaximaBusy(StatusBar::MaximaStatus::calculating);
      else
        StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

      wxScopedCharBuffer const data_raw = s.utf8_str();
      m_client->Write(data_raw.data(), data_raw.length());
      m_statusBar->NetworkStatus(StatusBar::transmit);
    }
  } else {
    DoRawConsoleAppend(_("Refusing to send cell to maxima: ") +
                       parenthesisError + wxS("\n"),
                       MC_TYPE_ERROR);
    if(GetWorksheet())
      {
        GetWorksheet()->SetWorkingGroup(nullptr);
        GetWorksheet()->m_evaluationQueue.Clear();
      }
  }
  if (!m_maximaStdoutPollTimer.IsRunning())
    m_statusBar->SetMaximaCPUPercentage(-1);
  m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
}

///--------------------------------------------------------------------------------
///  Socket stuff
///--------------------------------------------------------------------------------

void wxMaxima::MaximaEvent(wxThreadEvent &event) {
  using std::swap;
  switch (event.GetInt()) {
  case Maxima::READ_MISC_TEXT:
    // Read out stderr: We will do that in the background on a regular basis,
    // anyway. But if we do it manually now, too, the probability that things
    // are presented to the user in chronological order increases a bit.
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    if(m_first)
      ReadFirstPrompt(event.GetString());
    else
      ReadMiscText(event.GetString());
    break;
  case Maxima::STRING_FOR_XMLINSPECTOR:
    if(m_xmlInspector)
      m_xmlInspector->Add_FromMaxima(event.GetString());
    if (Maxima::GetPipeToStdErr())
      {
        std::cerr << event.GetString();
        std::cerr.flush();
      }
    break;
  case Maxima::XML_PROMPT:
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadPrompt(event.GetString());
    break;
  case Maxima::XML_SUPPRESSOUTPUT:
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadSuppressedOutput(event.GetString());
    break;
  case Maxima::XML_WXXMLSYMBOLS:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadLoadSymbols(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_VARIABLES:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadVariables(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_WATCH_VARIABLES_ADD:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadAddVariables(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_STATUSBAR:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadStatusBar(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_HTML_MANUAL_KEYWORDS:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadManualTopicNames(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_MATHS:
    m_statusBar->NetworkStatus(StatusBar::receive);
    ReadMath(event.GetPayload<wxXmlDocument>());
    break;
  case Maxima::XML_TOOLONGMATHS:
    m_statusBar->NetworkStatus(StatusBar::receive);
    DoRawConsoleAppend(_("(Config tells to suppress the output of long cells)"),
                       MC_TYPE_WARNING);
    break;
  case Maxima::XML_WXXML_KEY: // TODO: Should the key be outside the SuppressOutput?
    break;
  case Maxima::READ_PENDING:
    ReadStdErr();
    m_statusBar->NetworkStatus(StatusBar::receive);
    break;
  case Maxima::WRITE_PENDING:
    m_statusBar->NetworkStatus(StatusBar::transmit);
    break;
  case Maxima::WRITE_ERROR:
    DoRawConsoleAppend(_("Error writing to Maxima"), MC_TYPE_ERROR);
    break;
  case Maxima::DISCONNECTED: {
    wxLogMessage(_("Connection to Maxima lost."));
    //  KillMaxima();
    break;
  }
  }
}

/*!
 * ServerEvent is triggered when maxima connects to the socket server.
 */
void wxMaxima::ServerEvent(wxSocketEvent &event) {
  switch (event.GetSocketEvent()) {
  case wxSOCKET_CONNECTION:
    OnMaximaConnect();
    break;

  default:
    wxLogMessage(_("Encountered an unknown socket event."));
    break;
  }
}

void wxMaxima::OnMaximaConnect() {
  if (m_client && (m_client->IsConnected())) {
    wxLogMessage(_("New connection attempt whilst already connected."));
    return;
  }
  if (m_maximaProcess == NULL) {
    wxLogMessage(_("New connection attempt, but no currently running maxima process."));
    return;
  }
  if (m_server == NULL) {
    wxLogMessage(_("New connection attempt, but no currently no socket maxima could connect to."));
    return;
  }

  m_statusBar->NetworkStatus(StatusBar::idle);
  if(GetWorksheet())
    GetWorksheet()->QuestionAnswered();

  m_client = std::make_unique<Maxima>(m_server->Accept(false), &m_configuration);
  if (m_client->IsConnected()) {
    m_client->Bind(EVT_MAXIMA, &wxMaxima::MaximaEvent, this);
    SetupVariables();
  } else {
    wxLogMessage(_("Connection attempt, but connection failed."));
    m_unsuccessfulConnectionAttempts++;
    if (m_unsuccessfulConnectionAttempts < 12) {
      wxLogMessage(_("Trying to restart maxima."));
      StartMaxima(true);
      return;
    }
  }
}

bool wxMaxima::StartServer() {
  if (m_server) {
    if(m_server->IsOk())
      m_server->Close();
    m_server.reset();
  }
  m_port = m_configuration.DefaultPort() + m_unsuccessfulConnectionAttempts;

  do {
    wxLogMessage(_("Trying to start the socket a Maxima on the local "
                   "machine can connect to on port %i"), m_port);

    // Currently, only wxIPV4address is implemented (current wxWidgets 3.2.6)
    // https://docs.wxwidgets.org/3.2.6/classwx_i_paddress.html
    wxIPV4address addr;
    if (!addr.LocalHost())
      wxLogMessage(_("Cannot set the communication address to localhost."));
    if (!addr.Service(m_port))
      wxLogMessage(_("Cannot set the communication port to %i."), m_port);
    m_server = std::unique_ptr<wxSocketServer,
                               ServerDeleter>(
                                              new wxSocketServer(addr, wxSOCKET_WAITALL_WRITE));
    if (!m_server->IsOk()) {
      m_port++;
      m_server.reset();
    }
  } while (((m_port < m_configuration.DefaultPort() + 15000) &&
            (m_port < 65535) && (!m_server)));

  if (!m_server) {
    StatusText(_("Starting server failed"));
    m_statusBar->NetworkStatus(StatusBar::error);
    LoggingMessageBox(_("wxMaxima could not start a server.\n\n"
                        "Please check you have network support\n"
                        "enabled, that no internet safety appliance is configured "
                        "to intercept creation of servers even if said server only accepts "
                        "connections from local application (maxima insists on the graphical "
                        "frontend acting as a server it can connect on over a local socket) "
                        "and try again!"),
                      _("Fatal error"), wxOK | wxICON_ERROR);

    return false;
  } else {
    m_server->SetEventHandler(*GetEventHandler());
    m_server->Notify(true);
    m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
    m_server->SetTimeout(30);
    StatusText(_("Server started"));
    return true;
  }
}

///--------------------------------------------------------------------------------
///  Maxima process stuff
///--------------------------------------------------------------------------------

bool wxMaxima::StartMaxima(bool force) {
  if (!StartServer())
    return false;

  if ((m_maximaProcess != NULL) || (m_pid >= 0) || (m_client))
    {
      m_unsuccessfulConnectionAttempts = 0;
      KillMaxima();
    }

  wxString dirname;
  {
    wxString filename;
    if(GetWorksheet())
      filename = GetWorksheet()->m_currentFile;
    if (!filename.IsEmpty()) {
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

  if ((m_maximaProcess == NULL) || (m_hasEvaluatedCells) || force ||
      (dirname != dirname_Old)) {
    if ((m_xmlInspector) && (IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_xmlInspector->Clear();

    // Maxima isn't in lisp mode
    m_configuration.InLispMode(false);

    // Maxima isn't asking questions
    QuestionAnswered();

    // If we have an open file tell Maxima to start in the directory the file is
    // in
    wxUnsetEnv("MAXIMA_INITIAL_FOLDER");
    if (!dirname.IsEmpty()) {
      if (wxDirExists(dirname)) {
        // Tell Maxima to start in the directory the file is in
        wxSetEnv(wxS("MAXIMA_INITIAL_FOLDER"), dirname);
      } else {
        wxLogWarning(wxS("Directory %s doesn't exist. Maxima "
                         "might complain about that."),
                     dirname);
      }
    }

    m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

    wxString command = GetCommand();
    if (!command.IsEmpty()) {
      command.Append(wxString::Format(wxS(" -s %d "), (int)m_port));

      m_maximaProcess = new wxProcess(this, m_maxima_process_id);
      m_maximaProcess->Redirect();
      //      m_maximaProcess->SetPriority(wxPRIORITY_MAX);
      m_first = true;
      m_pid = -1;
      wxLogMessage(_("Running maxima as: %s"), command);

      wxEnvVariableHashMap environment;
      environment = m_configuration.MaximaEnvVars();
      wxGetEnvMap(&environment);
      // Tell Maxima we want to be able to kill it on Ctrl+G by sending it a
      // signal. Strictly necessary only on MS Windows where we don't have a
      // kill() command.
      environment["MAXIMA_SIGNALS_THREAD"] = "1";
      if(!Configuration::GetMaximaLang().IsEmpty())
        environment["LANG"] = Configuration::GetMaximaLang();
      // TODO: Is this still necessary for gnuplot on MacOs?
#if defined __WXOSX__
      environment["DISPLAY"] = ":0.0";
#endif
      m_maximaAuthenticated = false;
      m_discardAllData = false;
      std::uniform_real_distribution<double> urd(0.0, 256.0);
      std::unique_ptr<wxExecuteEnv> env = std::unique_ptr<wxExecuteEnv>(new wxExecuteEnv);
      wxMemoryBuffer membuf(512);
      for(auto i = 0 ; i < 512; i++)
        membuf.AppendByte(static_cast<char>(urd(m_configuration.m_eng)));
      m_maximaAuthString = wxBase64Encode(membuf);
      environment["MAXIMA_AUTH_CODE"] = m_maximaAuthString;

      env->env = std::move(environment);
      m_pid=wxExecute(command, wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER, m_maximaProcess, env.get());
      if (m_pid <= 0) {
        StatusMaximaBusy(StatusBar::MaximaStatus::process_wont_start);
        StatusText(_("Cannot start the maxima binary"));
        m_maximaProcess = NULL;
        m_maximaStdout = NULL;
        m_maximaStderr = NULL;
        m_statusBar->NetworkStatus(StatusBar::offline);
        MaximaNotStartingDialog *dlg = new MaximaNotStartingDialog(this, -1, &m_configuration, _("Failed to start Maxima"));
        dlg->ShowModal();
        return false;
      }
      m_maximaStdout = m_maximaProcess->GetInputStream();
      m_maximaStderr = m_maximaProcess->GetErrorStream();
      m_lastPrompt = wxS("(%i1) ");
      StatusMaximaBusy(StatusBar::MaximaStatus::wait_for_start);
    } else {
      m_statusBar->NetworkStatus(StatusBar::offline);
      wxLogMessage(_("Cannot find a maxima binary and no binary chosen in the "
                     "config dialogue."));
      return false;
    }
    if(GetWorksheet())
      GetWorksheet()->GetErrorList().Clear();

    // Initialize the performance counter.
    GetMaximaCPUPercentage();
  }
  return true;
}

void wxMaxima::Interrupt(wxCommandEvent &WXUNUSED(event)) {
  if(GetWorksheet())
    GetWorksheet()->CloseAutoCompletePopup();

  if (m_pid < 0) {
    m_MenuBar->EnableItem(EventIDs::menu_interrupt_id, false);
    return;
  }

#if defined(__WXMSW__)
  if (m_pid > 0) {
    // The following lines are adapted from maxima's winkill which William
    // Schelter has written and which has been improved by David Billinghurst
    // and Andrej Vodopivec.
    //
    // Winkill tries to find a shared memory region maxima provides we can set
    // signals in that maxima can listen to.
    //
    // For Maxima's end of this means of communication see
    // interfaces/xmaxima/win32/win_signals.lisp
    // and interfaces/xmaxima/win32/winkill_lib.c in maxima's tree.
    HANDLE sharedMemoryHandle = 0;
    LPVOID sharedMemoryAddress = NULL;

    // wxMaxima doesn't want to get interrupt signals.
    // SetConsoleCtrlHandler(NULL, true);

    /* First try to send the signal to gcl. */
    wxWCharBuffer sharedMemoryName(wxString::Format("gcl-%d", m_pid).wc_str());
    sharedMemoryHandle =
      OpenFileMapping(FILE_MAP_WRITE,    /*  Read/write permission.   */
                      FALSE,             /*  Do not inherit the name  */
                      sharedMemoryName.data()); /*  of the mapping object.   */

    /* If gcl is not running, send to maxima. */
    wxWCharBuffer sharedMemoryName2(wxString::Format("maxima-%d", m_pid).wc_str());
    if (sharedMemoryHandle == NULL) {
      sharedMemoryHandle =
        OpenFileMapping(FILE_MAP_WRITE,    /*  Read/write permission.   */
                        FALSE,             /*  Do not inherit the name  */
                        sharedMemoryName2.data()); /*  of the mapping object.   */
    }

    if (sharedMemoryHandle == NULL) {
      wxLogMessage(_("The Maxima process doesn't offer a shared memory segment "
                     "we can send an interrupt signal to."));

      // No shared memory location we can send break signals to => send a
      // console interrupt.
      // Before we do that we stop our program from closing on receiving a
      // Ctrl+C from the console.
      SetConsoleCtrlHandler(NULL, TRUE);

      // We could send a CTRL_BREAK_EVENT instead of a CTRL_C_EVENT that
      // isn't handled in the 2010 clisp release (see:
      // https://sourceforge.net/p/clisp/bugs/735/)
      // ...but CTRL_BREAK_EVENT seems to crash clisp, see
      // https://sourceforge.net/p/clisp/bugs/736/
      //
      // And we need to send the CTRL_BREAK_EVENT to our own console, which
      // has the group ID 0, see
      // https://docs.microsoft.com/en-us/windows/console/generateconsolectrlevent
      if (GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0) == 0) {
        LPTSTR errorText = NULL;

        FormatMessage(
                      FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                      errorText, 0, NULL);

        wxString errorMessage;
        if (!errorText)
          errorMessage = _("Could not send an interrupt signal to maxima.");
        else {
          errorMessage =
            wxString::Format(_("Interrupting maxima: %s"), errorText);
          LocalFree(errorText);
        }

        StatusText(errorMessage);
        wxLogMessage("%s", errorMessage);
        return;
      }
    } else {
      sharedMemoryAddress =
        MapViewOfFile(sharedMemoryHandle, /* Handle to mapping object.  */
                      FILE_MAP_WRITE,     /* Read/write permission.  */
                      0,                  /* Max.  object size.  */
                      0,                  /* Size of hFile.  */
                      0);                 /* Map entire file.  */

      if (sharedMemoryAddress == NULL) {
        wxLogMessage(_("Could not map view of the file needed in order to "
                       "send an interrupt signal to maxima."));
        return;
      }

      // Set the bit for the SIGINT handler
      int value = (1 << (wxSIGINT));
      volatile int *sharedMemoryContents = reinterpret_cast<int *>(sharedMemoryAddress);
      *sharedMemoryContents = *sharedMemoryContents | value;
      wxLogMessage(_("Sending an interrupt signal to Maxima."));
      UnmapViewOfFile(sharedMemoryAddress);
      CloseHandle(sharedMemoryHandle);
      sharedMemoryAddress = NULL;
      sharedMemoryHandle = NULL;
    }
  }

  if (m_maximaProcess) {
    // We need to send the CTRL_BREAK_EVENT to the process group, not
    // to the lisp.
    auto pid = m_maximaProcess->GetPid();
    if (!GenerateConsoleCtrlEvent(CTRL_C_EVENT, pid)) {
      wxLogMessage(_("Could not send an interrupt signal to maxima."));
      return;
    }
  }
#else
  wxLogMessage(_("Sending Maxima a SIGINT signal."));
  wxProcess::Kill(m_pid, wxSIGINT);
#endif
  if (m_maximaProcess) {
    wxProcess::Kill(m_maximaProcess->GetPid(), wxSIGINT);
  }
}

void wxMaxima::KillMaxima(bool logMessage) {
  if (logMessage && (m_closing || (m_maximaProcess == NULL) || (m_pid > 0))) {
    wxLogMessage("Killing Maxima. PID=%ld", m_pid);
    if (m_history)
      m_history->MaximaSessionStart();
  }
  m_discardAllData = true;
  m_closing = true;
  if(GetWorksheet() && (m_variablesPane))
    {
      m_variablesPane->ResetValues();
      m_varNamesToQuery = m_variablesPane->GetEscapedVarnames();
    }
  m_configCommands.Clear();
  // The new maxima process will be in its initial condition => mark it as such.
  m_hasEvaluatedCells = false;

  if(GetWorksheet())
    {
      GetWorksheet()->SetWorkingGroup(nullptr);
      GetWorksheet()->m_evaluationQueue.Clear();
    }
  EvaluationQueueLength(0);

  // We start checking for Maximas output again as soon as we send some data to
  // the program.
  m_statusBar->SetMaximaCPUPercentage(0);
  m_CWD.Clear();
  QuestionAnswered();
  // Finally found a long outstanding problem with leftover Lisp processes
  // (using debugging with command line Maxima and netcat):
  //
  // On Windows just closing the network (kill the netcat server) does NOT
  // terminate the Lisp, which is running in the background.
  // Don't know why. But do not try to do that, just kill the Maxima process (using "taskkill")!
#ifndef __WINDOWS__
  if(m_maximaProcess) {
    // If Maxima no more has a stdout it should automatically close
    m_maximaProcess->CloseOutput();
  }
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;
  // This closes Maxima's network connection - which should close maxima, as well.
  // Additionally closing the client automatically sends a "quit();" command to Maxima.
  m_client.reset();
  // If the process really exists after that, kill it with Signals.
  wxKillError killresult;
  if (wxProcess::Exists(m_pid)) {
    killresult = wxProcess::Kill(m_pid, wxSIGTERM, wxKILL_CHILDREN);
    wxLogMessage("Kill Maxima (SIGTERM). killresult=%d  [0=wxKILL_OK]", killresult);
  }
  if (wxProcess::Exists(m_pid)) {
    killresult = wxProcess::Kill(m_pid, wxSIGKILL, wxKILL_CHILDREN);
    wxLogMessage("Kill Maxima (SIGKILL). killresult=%d  [0=wxKILL_OK]", killresult);
  }
#else
  // Windows: it is complicated
  // wxSIGTERM seems not be enough, we need wxSIGKILL. See:
  // https://github.com/wxWidgets/wxWidgets/issues/15356
  //
  // However - I identified a wxWidgets issue, that only the direct children are killed
  // with wxKill(..., wxKILL_CHILDREN):
  // https://github.com/wxWidgets/wxWidgets/issues/25069
  // Since it will take some time until wxWidgets distributions with this fix are released and in use,
  // use the "taskkill" solution now.
  wxArrayString taskkill_out, taskkill_err;
  wxExecute(wxString::Format("taskkill /PID %d /F /T", m_pid), taskkill_out, taskkill_err, wxEXEC_SYNC);
  for (size_t i=0; i<taskkill_out.GetCount(); ++i)
    wxLogMessage("taskkill_out: " + taskkill_out.Item(i));
  for (size_t i=0; i<taskkill_err.GetCount(); ++i)
    wxLogMessage("taskkill_err: " + taskkill_err.Item(i));
#endif
  // Wait for Maxima to actually exit before we clean up its temporary files
  int count = 40;
  while (wxProcess::Exists(m_pid) && (count > 0)) {
    wxMilliSleep(50);
    count--;
  }

  // As we might have killed maxima before it was able to clean up its
  // temp files we try to do so manually now:
  if (m_maximaTempDir != wxEmptyString) {
    if (wxFileExists(m_maximaTempDir + wxS("/maxout") +
                     wxString::Format("%li.gnuplot", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/maxout") +
                   wxString::Format("%li.gnuplot", m_pid));
    if (wxFileExists(m_maximaTempDir + wxS("/data") +
                     wxString::Format("%li.gnuplot", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/data") +
                   wxString::Format("%li.gnuplot", m_pid));
    if (wxFileExists(m_maximaTempDir + wxS("/maxout") +
                     wxString::Format("%li.xmaxima", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/maxout") +
                   wxString::Format("%li.xmaxima", m_pid));
    if (wxFileExists(m_maximaTempDir + wxS("/maxout_") +
                     wxString::Format("%li.gnuplot", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/maxout_") +
                   wxString::Format("%li.gnuplot", m_pid));
    if (wxFileExists(m_maximaTempDir + wxS("/data_") +
                     wxString::Format("%li.gnuplot", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/data_") +
                   wxString::Format("%li.gnuplot", m_pid));
    if (wxFileExists(m_maximaTempDir + wxS("/maxout_") +
                     wxString::Format("%li.xmaxima", m_pid)))
      wxRemoveFile(m_maximaTempDir + wxS("/maxout_") +
                   wxString::Format("%li.xmaxima", m_pid));
  }
  // Set m_pid to -1.The process really shouldn't exist any more.
  m_pid = -1;

  // We don't need to be informed any more if the maxima process we just tried to
  // kill actually exits.
  if(m_maximaProcess) {
    m_maximaProcess->Detach();
    m_maximaProcess = NULL;
  }
}

void wxMaxima::OnGnuplotQueryTerminals(wxProcessEvent &event) {
  if (!m_gnuplotTerminalQueryProcess)
    return;
  wxString gnuplotMessage;
  {
    wxInputStream *istream = m_gnuplotTerminalQueryProcess->GetInputStream();
    wxTextInputStream textin(*istream);
    while (!istream->Eof())
      gnuplotMessage += textin.ReadLine() + "\n";
  }
  {
    wxInputStream *istream = m_gnuplotTerminalQueryProcess->GetErrorStream();
    wxTextInputStream textin(*istream);
    while (!istream->Eof())
      gnuplotMessage += textin.ReadLine() + "\n";
  }
  gnuplotMessage.Trim(true);
  gnuplotMessage.Trim(false);
  wxLogMessage("Terminals supported by gnuplot: %s", gnuplotMessage);
  if (gnuplotMessage.Contains(wxS("pngcairo"))) {
    wxLogMessage(_("Using gnuplot's pngcairo driver for embedded plots"));
    if (!m_configuration.UsePngCairo())
      m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo t)\n");
    m_configuration.UsePngCairo(true);
  } else {
    wxLogMessage(_("Using gnuplot's antialiassing-less png driver for embedded "
                   "plots as pngcairo could not be found"));
    if (m_configuration.UsePngCairo())
      m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo nil)\n");
    m_configuration.UsePngCairo(false);
  }
  m_gnuplotTerminalQueryProcess->CloseOutput();
  m_gnuplotTerminalQueryProcess = NULL;
  event.Skip();
}

void wxMaxima::OnGnuplotClose(wxProcessEvent &event) {
  m_gnuplotProcess = NULL;
  wxLogMessage(_("Gnuplot has closed."));
  event.Skip();
}

void wxMaxima::OnMaximaClose(){
  if (wxProcess::Exists(m_pid)) KillMaxima();
  m_maximaProcess = NULL;
  m_pid = -1;
  if (m_maximaStdout) {
    wxTextInputStream istrm(*m_maximaStdout, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_maximaStdout->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();
    if (!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stdout: %s"), o);
  }
  if (m_maximaStderr) {
    wxTextInputStream istrm(*m_maximaStderr, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_maximaStderr->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();
    if (!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stderr: %s"), o);
  }
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;
  m_statusBar->NetworkStatus(StatusBar::offline);
  if (!m_closing) {
    StatusText(_("Maxima process terminated unexpectedly."));

    if (m_first) {
      LoggingMessageBox(
                        _("Can not start Maxima. The most probable cause is that Maxima "
                          "isn't installed (it can be downloaded from "
                          "https://maxima.sourceforge.io) or in wxMaxima's config dialogue "
                          "the setting for Maxima's location is wrong."),
                        _("Error"), wxOK | wxICON_ERROR);
    }

    // Let's see if maxima has told us why this did happen.
    ReadStdErr();
    ConsoleAppend(wxS("\nMaxima exited...\n"), MC_TYPE_ERROR);

    if (m_unsuccessfulConnectionAttempts > 10)
      ConsoleAppend(wxS("Restart Maxima with 'Maxima->Restart Maxima'.\n"),
                    MC_TYPE_ERROR);
    else {
      ConsoleAppend(wxS("Trying to restart Maxima.\n"), MC_TYPE_ERROR);
      // Perhaps we shouldn't restart maxima again if it outputs a prompt and
      // crashes immediately after => Each prompt is deemed as but one hint
      // for a working maxima while each crash counts twice.
      m_unsuccessfulConnectionAttempts += 2;
      StartMaxima(true);
    }
    if(GetWorksheet())
      GetWorksheet()->m_evaluationQueue.Clear();
  }
  // We did close Maxima on purpose (m_closing==true) - do not restart it.
  //else
  //  StartMaxima(true);

  StatusMaximaBusy(StatusBar::MaximaStatus::disconnected);
  UpdateToolBar();
  UpdateMenus();
}

void wxMaxima::OnMaximaClose(wxProcessEvent &event) {
  if(event.GetPid() != m_pid)
    return;
  OnMaximaClose();
}

///--------------------------------------------------------------------------------
///  Dealing with stuff read from the socket
///--------------------------------------------------------------------------------

void wxMaxima::ReadFirstPrompt(const wxString &data) {
 auto end = data.Find(m_firstPrompt);
  if (end  == wxNOT_FOUND)
    return;

  m_bytesFromMaxima = 0;

  int start = 0;
  start = data.Find(wxS("Maxima "));
  if (start == wxNOT_FOUND)
    start = 0;
  FirstOutput();

  m_maximaBusy = false;

  // Wait for a line maxima informs us about it's process id in.
  int s = data.Find(wxS("pid=")) + 4;
  int t = s + data.SubString(s, data.Length()).Find(wxS("\n")) - 1;

  // Read this pid
  if (s < t)
    if(!data.SubString(s, t).ToLong(&m_pid))
      wxLogMessage(_("Cannot interpret the numeric value of pid %s"),
                   data.SubString(s, t));

  if (m_pid > 0)
    m_MenuBar->EnableItem(EventIDs::menu_interrupt_id, true);

  m_client->ClearFirstPrompt();
  m_first = false;
  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  m_closing = false; // when restarting maxima this is temporarily true

  wxString prompt_compact = data.Left(start + static_cast<std::size_t>(end) +
                                      m_firstPrompt.Length() - 1);
  prompt_compact.Replace(wxS("\n"), wxS("\u21b2"));

  wxLogMessage(_("Received maxima's first prompt: %s"), prompt_compact);

  wxLogMessage(_("Maxima's PID is %li"), m_pid);

  if (GetWorksheet() && (GetWorksheet()->m_evaluationQueue.Empty())) {
    // Inform the user that the evaluation queue is empty.
    EvaluationQueueLength(0);
    if (m_evalOnStartup) {
      wxLogMessage(_("Starting evaluation of the document"));
      m_evalOnStartup = false;
      GetWorksheet()->AddDocumentToEvaluationQueue();
      EvaluationQueueLength(
                            GetWorksheet()->m_evaluationQueue.Size(),
                            GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    } else {
      m_evalOnStartup = false;
      if (GetWorksheet() && (m_configuration.GetOpenHCaret()) &&
          (GetWorksheet()->GetActiveCell() == NULL))
        GetWorksheet()->OpenNextOrCreateCell();
    }
  } else
    TriggerEvaluation();
}

void wxMaxima::ReadMiscText(const wxString &data) {
  if(!m_maximaAuthenticated)
    return;

  auto style = MC_TYPE_ASCIIMATHS;

  if (data.StartsWith(wxS("(%")))
    style = MC_TYPE_TEXT;

  if (data.IsEmpty())
    return;

  if (data == "\r")
    return;

  if (GetWorksheet() && (data.StartsWith("\n")))
    GetWorksheet()->SetCurrentTextCell(nullptr);

  // A version of the text where each line begins with non-whitespace and
  // whitespace characters are merged.
  wxString mergedWhitespace = wxS("\n");
  bool whitespace = true;
  for (wxString::const_iterator it = data.begin(); it != data.end(); ++it) {
    if ((*it == wxS(' ')) || (*it == wxS('\t'))) {
      // Merge non-newline whitespace to a space.
      if (!whitespace)
        mergedWhitespace += wxS(' ');
    } else
      mergedWhitespace += *it;

    if ((*it == wxS(' ')) || (*it == wxS('\t')) || (*it == wxS('\n')))
      whitespace = true;
    else
      whitespace = false;
  }

  if ((mergedWhitespace.Contains(wxS("\n-- an error."))) ||
      (mergedWhitespace.Contains(wxS(":incorrect syntax:"))) ||
      (mergedWhitespace.Contains(wxS("\nincorrect syntax"))) ||
      (mergedWhitespace.Contains(wxS("\nMaxima encountered a Lisp error"))) ||
      (mergedWhitespace.Contains(wxS("\nkillcontext: no such context"))) ||
      (mergedWhitespace.Contains(
                                 wxS("\ndbl:MAXIMA>>"))) || // a gcl error message
      (mergedWhitespace.Contains(
                                 wxS("\nTo enable the Lisp debugger set *debugger-hook* to "
                                     "nil."))) // a scbl error message
      )
    style = MC_TYPE_ERROR;

  if ((mergedWhitespace.StartsWith(wxS("Warning:"))) ||
      (mergedWhitespace.StartsWith(wxS("warning:"))) ||
      (mergedWhitespace.StartsWith(wxS("WARNING:"))) ||
      (mergedWhitespace.Contains(wxS("\nWarning:"))) ||
      (mergedWhitespace.Contains(wxS("\nWARNING:"))) ||
      (mergedWhitespace.Contains(wxS("\nwarning:"))) ||
      (mergedWhitespace.Contains(wxS(": Warning:"))) ||
      (mergedWhitespace.Contains(wxS(": warning:"))))
    style = MC_TYPE_WARNING;
  else {
    // Gnuplot errors differ from gnuplot warnings by not containing a
    // "warning:"
    if (m_gnuplotErrorRegex.Matches(mergedWhitespace))
      style = MC_TYPE_ERROR;
  }

  // Add the text line to the console
  if (GetWorksheet() && (!data.empty())) {
    GetWorksheet()->SetCurrentTextCell(ConsoleAppend(data, style));
    if (style == MC_TYPE_ERROR)
      AbortOnError();
  }
  if (GetWorksheet() && (data.EndsWith("\n")))
    GetWorksheet()->SetCurrentTextCell(nullptr);
}

void wxMaxima::ReadStatusBar(const wxXmlDocument &xmldoc) {
  if(GetWorksheet())
    GetWorksheet()->SetCurrentTextCell(nullptr);
  if(!xmldoc.IsOk())
    {
      DoRawConsoleAppend(_("There was an error in the XML that should describe the status bar message.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
    }
  else
    {
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *contents = node->GetChildren();
        if (contents)
          StatusText(contents->GetContent(), false);
      }
    }
}

void wxMaxima::ReadManualTopicNames(const wxXmlDocument &xmldoc) {
  if(xmldoc.IsOk())
    {
      std::vector<wxString> topics;
      wxXmlNode *node = xmldoc.GetRoot();
      while ((node) && (node->GetName() != wxS("html-manual-keywords")))
        node = node->GetNext();

      if (node == NULL) {
        wxLogMessage(_("No topics found in topic tag"));
      } else {
        wxXmlNode *entry = node->GetChildren();
        while(entry != NULL)
          {
            if (entry->GetName() == wxS("keyword")) {
              wxXmlNode *topic = entry->GetChildren();
              if (topic) {
                wxLogMessage(_("Received manual topic request: %s"),
                             topic->GetContent().ToUTF8().data());
                topics.push_back(topic->GetContent());
              }
              if (topics.size() == 0)
                wxLogMessage(_("No topics found in topic flag"));
#ifdef USE_WEBVIEW
              else
                {
                  m_helpPane->SelectKeywords(topics);
                  wxMaximaFrame::ShowPane(EventIDs::menu_pane_help);
                }
#else
              ShowMaximaHelp(topics[1]);
#endif
            }
            entry = entry->GetNext();
          }
      }
    }
  else
    {
      DoRawConsoleAppend(_("There was an error in the XML that should describe the manual topics.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
    }
}


/***
 * Checks if maxima displayed a new chunk of math
 */
void wxMaxima::ReadMath(const wxXmlDocument &xml) {
  if(!GetWorksheet())
    return;

  GetWorksheet()->SetCurrentTextCell(nullptr);

  // Append everything from the "beginning of math" to the "end of math" marker
  // to the console
  if (m_configuration.UseUserLabels()) {
    ConsoleAppend(xml, MC_TYPE_DEFAULT,
                  GetWorksheet()->m_evaluationQueue.GetUserLabel());
  } else {
    ConsoleAppend(xml, MC_TYPE_DEFAULT);
  }
}

void wxMaxima::ReadSuppressedOutput(const wxString &data) {
  if(!m_maximaAuthenticated)
    {
      if(data.Find("</wxxml-key>") != wxNOT_FOUND) {
        if(data.Find("<wxxml-key>" + m_maximaAuthString + "</wxxml-key>")){
          wxLogMessage(_("Maxima has authenticated!"));
          m_maximaAuthenticated = true;
        } else {
          wxLogMessage(_("Cannot authenticate Maxima!"));
          LoggingMessageBox(
                            _("Could not make sure that we talk to the maxima we started => "
                              "discarding all data it sends."),
                            _("Warning"), wxOK | wxICON_EXCLAMATION);
          m_discardAllData = true;
        }
      }
    }

  if(!m_maximaAuthenticated)
    {
      wxLogMessage(_("Maxima didn't attempt to authenticate!"));
      LoggingMessageBox(
                        _("Could not make sure that we talk to the maxima we started => "
                          "discarding all data it sends."),
                        _("Warning"), wxOK | wxICON_EXCLAMATION);
      m_discardAllData = true;
    }
}

void wxMaxima::ReadLoadSymbols(const wxXmlDocument &data) {
  if(GetWorksheet())
    GetWorksheet()->AddSymbols(data);
}

void wxMaxima::ReadVariables(const wxXmlDocument &xmldoc) {
  if(!xmldoc.IsOk())
    {
      DoRawConsoleAppend(_("There was an error in the XML that should describe the contents of some variables.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
    }
  else
    {
      int num = 0;
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *vars = node->GetChildren();
        while (vars != NULL) {
          wxXmlNode *var = vars->GetChildren();

          wxString name;
          wxString value;
          bool bound = false;
          while (var != NULL) {
            if (var->GetName() == wxS("name")) {
              num++;
              wxXmlNode *namenode = var->GetChildren();
              if (namenode)
                name = namenode->GetContent();
            }
            if (var->GetName() == wxS("value")) {
              wxXmlNode *valnode = var->GetChildren();
              if (valnode) {
                bound = true;
                value = valnode->GetContent();
              }
            }

            if (bound) {
              if(GetWorksheet() && (m_variablesPane))
                m_variablesPane->VariableValue(name, value);

              // Undo an eventual stringdisp:true adding quoting marks to strings
              if (value.StartsWith("\"") && value.EndsWith("\""))
                value = value.SubString(1, value.Length() - 2);

              auto varFunc = m_variableReadActions.find(name);
              if (varFunc != m_variableReadActions.end())
                CALL_MEMBER_FN(*this, varFunc->second)(value);
            } else {
              if(GetWorksheet() && (m_variablesPane))
                m_variablesPane->VariableUndefined(name);
              auto varFunc = m_variableUndefinedActions.find(name);
              if (varFunc != m_variableUndefinedActions.end())
                CALL_MEMBER_FN(*this, varFunc->second)();
            }

            var = var->GetNext();
          }

          vars = vars->GetNext();
        }
      }

      if (num > 1)
        wxLogMessage(_("Maxima sends a new set of auto-completable symbols."));
      else
        wxLogMessage(_("Maxima has sent a new variable value."));
    }
  TriggerEvaluation();
  QueryVariableValue();
}

void wxMaxima::VariableActionSinnpiflag(const wxString &WXUNUSED(value)) {
  m_fourierLoaded = true;
}

void wxMaxima::VariableActionSinnpiflagUndefined() { m_fourierLoaded = false; }

void wxMaxima::VariableActionUserDir(const wxString &value) {
  Dirstructure::Get()->UserConfDir(value);
  m_history->SetSavePlace(value);
  wxLogMessage(_("Maxima user configuration is located in directory %s"), value);
}

void wxMaxima::VariableActionDisplay2d_Unicode(const wxString &value) {
 if (value == wxS("true"))
   m_configuration.Display2d_Unicode(true);
 if (value == wxS("false"))
   m_configuration.Display2d_Unicode(false);
 }

void wxMaxima::VariableActionGentranlang(const wxString &value) {
  if (value == wxS("c"))
    m_gentranMenu->Check(EventIDs::gentran_lang_c, true);
  if (value == wxS("fortran"))
    m_gentranMenu->Check(EventIDs::gentran_lang_fortran, true);
  if (value == wxS("ratfor"))
    m_gentranMenu->Check(EventIDs::gentran_lang_ratfor, true);
}

void wxMaxima::VariableActionOpSubst(const wxString &value) {
  if (value == wxS("false"))
    m_subst_Sub->Check(EventIDs::menu_opsubst, false);
  else if (value == wxS("true"))
    m_subst_Sub->Check(EventIDs::menu_opsubst, true);
}

void wxMaxima::VariableActionLogexpand(const wxString &value) {
  m_logexpand = value;
  if (value == wxS("false"))
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_false, true);
  else if (value == wxS("true"))
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_true, true);
  else if (value == wxS("all"))
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_all, true);
  else if (value == wxS("super"))
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_super, true);
  else {
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_false, false);
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_true, false);
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_all, false);
    m_logexpand_Sub->Check(EventIDs::menu_logexpand_super, false);
  }
}

void wxMaxima::VariableActionTempDir(const wxString &value) {
  m_maximaTempDir = value;
  wxLogMessage(_("Maxima uses temp directory %s"), value);
  if (!wxDirExists(value)) {
    // Sometimes people delete their temp dir
    // and gnuplot won't create a new one for them.
    wxMkdir(value, wxS_DIR_DEFAULT);
  }
}

void wxMaxima::VariableActionDebugmode(const wxString &value) {
  if (value == wxS("true")) {
    m_MaximaMenu->Enable(EventIDs::menu_debugmode, true);
    m_debugTypeMenu->Check(EventIDs::menu_debugmode_all, true);
  }
  if (value == wxS("false")) {
    m_MaximaMenu->Enable(EventIDs::menu_debugmode, true);
    m_debugTypeMenu->Check(EventIDs::menu_debugmode_off, true);
  }
  if (value == wxS("lisp")) {
    m_MaximaMenu->Enable(EventIDs::menu_debugmode, true);
    m_debugTypeMenu->Check(EventIDs::menu_debugmode_lisp, true);
  }
}

void wxMaxima::VariableActionAutoconfVersion(const wxString &value) {
  if(GetWorksheet())
    m_configuration.SetMaximaVersion(value);
  wxLogMessage(_("Maxima version: %s"), value);
}
void wxMaxima::VariableActionAutoconfHost(const wxString &value) {
  m_configuration.SetMaximaArch(value);
  wxLogMessage(_("Maxima architecture: %s"), value);
}
void wxMaxima::VariableActionMaximaInfodir(const wxString &value) {
  // Make sure that we get out all ".." and "~" of the path as they seem to
  // confuse the help browser logic
  wxLogMessage(_("Maxima's manual is located in directory %s"),
               value);
}

void wxMaxima::VariableActionMaximaHtmldir(const wxString &value) {
  m_maximaHtmlDir = value;
  wxFileName dir(value);
  dir.MakeAbsolute();
  wxString dir_canonical = dir.GetPath();
  wxLogMessage(_("Maxima's HTML manuals are in directory %s"),
               dir_canonical);
  if(GetWorksheet())
    {
      GetWorksheet()->SetMaximaDocDir(dir_canonical);
      GetWorksheet()->LoadHelpFileAnchors(dir_canonical,
                                          m_configuration.GetMaximaVersion());
    }
}
void wxMaxima::GnuplotCommandName(wxString gnuplot) {
  m_gnuplotcommand = gnuplot;
  if (!wxFileName(m_gnuplotcommand).IsAbsolute()) {
    wxPathList pathlist;

    // Add paths relative to the path of the wxMaxima executable
    pathlist.Add(
                 wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath());
    pathlist.Add(
                 wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath() +
                 "/../");
    pathlist.Add(
                 wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath() +
                 "/../gnuplot");
    pathlist.Add(
                 wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath() +
                 "/../gnuplot/bin");
#ifdef __WXMSW__
    // Windows: Gnuplot is stored in the directory ../gnuplot/bin relative to *maxima.bat*
    // If wxMaxima is packaged separate (we provide Windows installers with wxMaxima only now),
    // add that path too:
    // One must specify the path to Maxima in that case, so use the m_maximaUserLocation.
    wxString maximapath = wxFileName(m_configuration.MaximaUserLocation()).GetPath();
    if (!maximapath.IsEmpty()) {
      pathlist.Add(maximapath + "/../gnuplot/bin");
    }
#endif
    // Add paths from the PATH environment variable
    pathlist.AddEnvList(wxS("PATH"));

    // Add OSX specific paths
#ifdef __WXOSX__
    // MacPorts:
    // The MacPorts default binary path /opt/local/bin/ is not in the PATH for
    // applications. It is added to .profile, but this is only used by shells.
    // => Add the default MacPorts binary path /opt/local/bin/ to our search
    // path list.
    //
    // Homebrew:
    // Homebrew installs binaries in /usr/local/bin, which is in the PATH by
    // default.
    //
    // Application packages including gnuplot:
    // The above wxMaxima executable relative logic should work
    //
    // If gnuplot is somewhere else (e.g. non default MacPort or Homebrew path),
    // the command
    //   gnuplot_command:"/opt/local/bin/gnuplot"$
    // must be added manually to ~/.maxima/wxmaxima-init.mac
    // This should be documented for the installer packages, e.g. as MacPorts
    // "notes" field.
    pathlist.Add(OSX_MACPORTS_PREFIX "/bin");
#endif

#ifdef __WXMSW__
    wxString wgnuplot = gnuplot;
    if(m_configuration.UseWGnuplot())
      {
        wxLogMessage(_("Instructed to prefer wgnuplot over gnuplot"));
        long pos = gnuplot.rfind(wxS("gnuplot"));
        // if pos is not wxNOT_FOUND it is 6 or higher.
        if(pos != wxNOT_FOUND)
          {
            wgnuplot = gnuplot.Left(pos) + wxS("w") + gnuplot.Right(gnuplot.Length() - pos);
            if (!m_gnuplotcommand.IsEmpty())
              m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wgnuplot);
          }
      }
    else
      {
        wxLogMessage(_("Instructed to prefer gnuplot over wgnuplot"));
      }
    if (m_gnuplotcommand.IsEmpty())
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot);
    m_gnuplotcommand_commandline = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));

    if(m_configuration.UseWGnuplot())
      {
        // If not successful, Find executable "wgnuplot.exe" in our list of paths
        if (m_gnuplotcommand.IsEmpty()) {
          m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("wgnuplot.exe"));
          m_gnuplotcommand_commandline = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));
        }
      }
    // If not successful, Find executable "gnuplot.exe" in our list of paths
    if (m_gnuplotcommand.IsEmpty())
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));
    // If not successful, Find executable "gnuplot.bat" in our list of paths
    if (m_gnuplotcommand.IsEmpty())
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("gnuplot.bat"));
#endif
#ifdef __WXOSX__
    if (m_gnuplotcommand.IsEmpty())
      m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot + wxS(".app"));
#endif
    // If not successful, use the original command (better than empty for error
    // messages)
    if (m_gnuplotcommand.IsEmpty()) {
#ifdef __WXMSW__
      if(m_configuration.UseWGnuplot())
        {
          m_gnuplotcommand = wgnuplot;
          wxLogMessage(_("Gnuplot not found, using the default: %s"), wgnuplot);
        }
      else
        {
          m_gnuplotcommand = gnuplot;
          wxLogMessage(_("Gnuplot not found, using the default: %s"), gnuplot);
        }
#endif
    } else {
      wxLogMessage(_("Gnuplot found at: %s"), m_gnuplotcommand);
#ifdef __WINDOWS__
      wxLogMessage(_("Gnuplot (command line) found at: %s"), m_gnuplotcommand_commandline);
#endif
    }
  }
  if (m_gnuplotcommand.Contains(" ") && (!m_gnuplotcommand.StartsWith("\"")) &&
      (!m_gnuplotcommand.StartsWith("\'")))
    m_gnuplotcommand = "\"" + m_gnuplotcommand + "\"";
}

void wxMaxima::VariableActionGnuplotCommand(const wxString &value) {
  GnuplotCommandName(value);

  wxLogMessage(_("Querying gnuplot which graphics drivers it supports."));
  wxEnvVariableHashMap environment;
  // gnuplot uses the PAGER variable only on un*x - and on un*x there is cat.
  environment["PAGER"] = "cat";
  environment["LINES"] = "10000";
  environment["LESS_LINES"] = "10000";
  if(!Configuration::GetMaximaLang().IsEmpty())
    environment["LANG"] = Configuration::GetMaximaLang();
  wxGetEnvMap(&environment);

  m_gnuplotTerminalQueryProcess =
    new wxProcess(this, EventIDs::gnuplot_query_terminals_id);
  m_gnuplotTerminalQueryProcess->Redirect();
  std::unique_ptr<wxExecuteEnv> env(new wxExecuteEnv);
  env->env = std::move(environment);
  wxString gnuplotcommand = m_gnuplotcommand;
#if defined __WXMSW__
  // We want this gnuplot process to exit after it has finished its work,
  // not to stay around until someone closes its terminal
  if(m_configuration.UseWGnuplot())
    {
      long pos = gnuplotcommand.rfind(wxS("gnuplot"));
      // if pos is not wxNOT_FOUND it is 6 or higher.
      if(pos != wxNOT_FOUND)
        gnuplotcommand = gnuplotcommand.Left(pos) +
          gnuplotcommand.Right(gnuplotcommand.Length() - pos - 1);
    }
#endif
  wxArrayString gnuplot_output, gnuplot_errors;
#ifdef __WINDOWS__
  wxString gnuplot_query=m_gnuplotcommand_commandline + " -e \"print GPVAL_TERMINALS\"";
#else
  wxString gnuplot_query=m_gnuplotcommand + " -e \"print GPVAL_TERMINALS\"";
#endif
  if (wxExecute(gnuplot_query, gnuplot_output, gnuplot_errors, wxEXEC_SYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER, NULL) < 0)
    wxLogMessage(_("Cannot start gnuplot"));
  else {
    wxString gnuplot_terminals = wxJoin(gnuplot_errors, ' ', 0); // REMARK: In the documentation of wxJoin, NULL is suggested as 3rd argument ("If the escape character is non-NULL, ..."), but NULL causes an warning. (warning: passing NULL to non-pointer argument 3). Therefore I use 0 here.
    wxLogMessage(wxS("Gnuplot terminals: ") + gnuplot_terminals);
    /* FIXME: What should happen with the result? Returned by the function? Stored anywhere? Currently the result is nowhere used... */
    m_configuration.UsePngCairo(gnuplot_terminals.Contains(wxS("pngcairo")));
    wxLogMessage(m_configuration.UsePngCairo() ? wxS("Gnuplot pngcairo terminal: yes") : wxS("Gnuplot pngcairo terminal: no"));
  }
}

void wxMaxima::VariableActionMaximaSharedir(const wxString &value) {
  wxString dir = value;
  dir.Trim(true);
  m_configuration.MaximaShareDir(dir);
  wxLogMessage(_("Maxima's share files are located in directory %s"),
               dir);
  /// READ FUNCTIONS FOR AUTOCOMPLETION
  if(GetWorksheet())
    GetWorksheet()->LoadSymbols();
}

void wxMaxima::VariableActionMaximaDemodir(const wxString &value) {
  wxString dir = value;
  dir.Trim(true);
  m_configuration.MaximaDemoDir(dir);
  wxLogMessage(_("Maxima's demo files are located in directory %s"),
               dir);
}

void wxMaxima::VariableActionLispName(const wxString &value) {
  m_configuration.SetLispType(value);
  wxLogMessage(_("Maxima was compiled using %s"), value);
}
void wxMaxima::VariableActionLispVersion(const wxString &value) {
  m_configuration.SetLispVersion(value);
  wxLogMessage(_("Lisp version: %s"), value);
}
void wxMaxima::VariableActionWxLoadFileName(const wxString &value) {
  m_recentPackages.AddDocument(value);
  UpdateRecentDocuments();
  wxLogMessage(_("Maxima has loaded the file %s."), value);
  m_updateAutocompletion = true;
}

void wxMaxima::VariableActionWxSubscripts(const wxString &value) {
  if (m_maximaVariable_wxSubscripts != value) {
    m_maximaVariable_wxSubscripts = value;
    if (value == wxS("false"))
      m_autoSubscriptMenu->Check(EventIDs::menu_noAutosubscript, true);
    else if (value == wxS("true"))
      m_autoSubscriptMenu->Check(EventIDs::menu_defaultAutosubscript, true);
    else if (value == wxS("all"))
      m_autoSubscriptMenu->Check(EventIDs::menu_alwaysAutosubscript, true);
  }
}
void wxMaxima::VariableActionLmxChar(const wxString &value) {
  if (m_maximaVariable_lmxchar != value) {
    m_maximaVariable_lmxchar = value;
    if (m_maximaVariable_lmxchar.EndsWith("("))
      m_roundedMatrixParensMenu->Check(EventIDs::menu_roundedMatrixParens, true);
    if (m_maximaVariable_lmxchar.EndsWith("<"))
      m_roundedMatrixParensMenu->Check(EventIDs::menu_angledMatrixParens, true);
    if (m_maximaVariable_lmxchar.EndsWith("|"))
      m_roundedMatrixParensMenu->Check(EventIDs::menu_straightMatrixParens, true);
    if (m_maximaVariable_lmxchar.EndsWith("["))
      m_roundedMatrixParensMenu->Check(EventIDs::menu_squareMatrixParens, true);
    if (m_maximaVariable_lmxchar.EndsWith(" "))
      m_roundedMatrixParensMenu->Check(EventIDs::menu_noMatrixParens, true);
  }
}

void wxMaxima::VariableActionStringdisp(const wxString &value) {
  if (value == wxS("true")) {
    if (!m_viewMenu->IsChecked(EventIDs::menu_stringdisp))
      m_viewMenu->Check(EventIDs::menu_stringdisp, true);
  } else {
    if (m_viewMenu->IsChecked(EventIDs::menu_stringdisp))
      m_viewMenu->Check(EventIDs::menu_stringdisp, false);
  }
}

void wxMaxima::VariableActionNumer(const wxString &value) {
  if (value == wxS("true")) {
    if (!m_NumericMenu->IsChecked(EventIDs::menu_num_out))
      m_NumericMenu->Check(EventIDs::menu_num_out, true);
  } else {
    if (m_NumericMenu->IsChecked(EventIDs::menu_num_out))
      m_NumericMenu->Check(EventIDs::menu_num_out, false);
  }
}

void wxMaxima::VariableActionAlgebraic(const wxString &value) {
  if (value == wxS("true")) {
    if (!m_SimplifyMenu->IsChecked(EventIDs::menu_talg))
      m_SimplifyMenu->Check(EventIDs::menu_talg, true);
  } else {
    if (m_SimplifyMenu->IsChecked(EventIDs::menu_talg))
      m_SimplifyMenu->Check(EventIDs::menu_talg, false);
  }
}
void wxMaxima::VariableActionShowtime(const wxString &value) {
  if (value == wxS("false")) {
    if (m_MaximaMenu->IsChecked(EventIDs::menu_time))
      m_MaximaMenu->Check(EventIDs::menu_time, false);
  } else {
    if (!m_MaximaMenu->IsChecked(EventIDs::menu_time))
      m_MaximaMenu->Check(EventIDs::menu_time, true);
  }
}
void wxMaxima::VariableActionEngineeringFormat(const wxString &value) {
  m_maximaVariable_engineeringFormat = value;
  if (value == wxS("true")) {
    if (!m_NumericMenu->IsChecked(EventIDs::menu_engineeringFormat))
      m_NumericMenu->Check(EventIDs::menu_engineeringFormat, true);
  } else {
    if (m_NumericMenu->IsChecked(EventIDs::menu_engineeringFormat))
      m_NumericMenu->Check(EventIDs::menu_engineeringFormat, false);
  }
}
void wxMaxima::VariableActionHtmlHelp(const wxString &value) {
  if (value == wxS("text")) {
    if (!m_HelpMenu->IsChecked(EventIDs::menu_maxima_uses_internal_help))
      m_HelpMenu->Check(EventIDs::menu_maxima_uses_internal_help, true);
    m_configuration.MaximaHelpFormat(Configuration::maxima);
  }
  if (value == wxS("html")) {
    if (!m_HelpMenu->IsChecked(EventIDs::menu_maxima_uses_html_help))
      m_HelpMenu->Check(EventIDs::menu_maxima_uses_html_help, true);
    m_configuration.MaximaHelpFormat(Configuration::browser);
  }
  if(m_configuration.OfferInternalHelpBrowser())
    {
      if ((value == wxS("wxmaxima")) || (value == wxS("frontend"))) {
        if (!m_HelpMenu->IsChecked(EventIDs::menu_maxima_uses_wxmaxima_help))
          m_HelpMenu->Check(EventIDs::menu_maxima_uses_wxmaxima_help, true);
        m_configuration.MaximaHelpFormat(Configuration::frontend);
      }
    }
}

void wxMaxima::VariableActionAutoplay(const wxString &value) {
  if (value == wxS("true")) {
    if (!m_PlotMenu->IsChecked(EventIDs::menu_animationautostart))
      m_PlotMenu->Check(EventIDs::menu_animationautostart, true);
  } else {
    if (m_PlotMenu->IsChecked(EventIDs::menu_animationautostart))
      m_PlotMenu->Check(EventIDs::menu_animationautostart, false);
  }
}
void wxMaxima::VariableActionDomain(const wxString &value) {
  if (value == wxS("complex")) {
    if (!m_NumericMenu->IsChecked(EventIDs::menu_num_domain))
      m_NumericMenu->Check(EventIDs::menu_num_domain, true);
  } else {
    if (m_NumericMenu->IsChecked(EventIDs::menu_num_domain))
      m_NumericMenu->Check(EventIDs::menu_num_domain, false);
  }
}
void wxMaxima::VariableActionDisplay2D(const wxString &value) {
  if (m_maximaVariable_display2d != value) {
    m_maximaVariable_display2d = value;
    if (m_maximaVariable_display2d == wxS("false")) {
      m_configuration.DisplayMode(Configuration::display_1dASCII);
      m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_1D_ASCII, true);
    } else {
      if (m_maximaVariable_altdisplay2d == wxS("false")) {
        if(m_configuration.Display2d_Unicode())
          {
            m_configuration.DisplayMode(Configuration::display_2dUNICODE);
            m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_2D_UNICODE, true);
          } else {
            m_configuration.DisplayMode(Configuration::display_2dASCII);
            m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_2D_ASCII, true);
          }
      } else {
        m_configuration.DisplayMode(Configuration::display_2d);
        m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_graphics, true);
      }
    }
  }
}
void wxMaxima::VariableActionAltDisplay2D(const wxString &value) {
  if (m_maximaVariable_altdisplay2d != value) {
    m_maximaVariable_altdisplay2d = value;
    if (m_maximaVariable_display2d == wxS("false")) {
      m_configuration.DisplayMode(Configuration::display_1dASCII);
      m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_1D_ASCII, true);
    } else {
      if (m_maximaVariable_altdisplay2d == wxS("false")) {
        if(m_configuration.Display2d_Unicode()) {
          m_configuration.DisplayMode(Configuration::display_2dUNICODE);
          m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_2D_UNICODE, true);
        } else {
          m_configuration.DisplayMode(Configuration::display_2dASCII);
          m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_2D_ASCII, true);
        }
      } else {
        m_configuration.DisplayMode(Configuration::display_2d);
        m_equationTypeMenuMenu->Check(EventIDs::menu_math_as_graphics, true);
      }
    }
  }
}

void wxMaxima::VariableActionOperators(const wxString &value) {
  wxXmlDocument xmldoc;
  wxString newOperators;
  wxStringInputStream xmlStream(value);
  {
    xmldoc.Load(xmlStream);
  }
  if(!xmldoc.IsOk())
    {
      DoRawConsoleAppend(_("There was an error in the XML that should contain the list of operators.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
    }
  else
    {
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *contents = node->GetChildren();
        while (contents) {
          if (contents->GetName() == wxS("operator")) {
            wxXmlNode *innernode = contents->GetChildren();
            if (innernode) {
              wxString content = innernode->GetContent();
              if ((!content.IsEmpty()) &&
                  (m_configuration.m_maximaOperators.find(content) ==
                   m_configuration.m_maximaOperators.end())) {
                if ((content.at(0) > '9') || (content.at(0) < '0')) {
                  m_configuration.m_maximaOperators[content] = 1;
                  if (!newOperators.IsEmpty())
                    newOperators += wxS(", ");
                  newOperators += content;
                }
              }
            }
          }
          contents = contents->GetNext();
        }
        if (!newOperators.IsEmpty()) {
          wxLogMessage(_("New maxima Operators detected: %s"), newOperators);
          if(GetWorksheet())
            GetWorksheet()->Recalculate();
        }
      }
    }
}

void wxMaxima::ReadAddVariables(const wxXmlDocument &xmldoc) {
  wxLogMessage(_("Maxima sends us a new set of variables for the watch list."));
  if(!xmldoc.IsOk())
    {
      DoRawConsoleAppend(_("There was an error in the XML that should contain a list of watch variables.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      AbortOnError();
    }
  else
    {
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *var = node->GetChildren();
        while (var != NULL) {
          wxString name;
          {
            if (var->GetName() == wxS("variable")) {
              wxXmlNode *valnode = var->GetChildren();
              if (valnode)
                {
                  if(GetWorksheet() && (m_variablesPane))
                    m_variablesPane->AddWatch(valnode->GetContent());
                }
            }
          }
          var = var->GetNext();
        }
      }
  }
}

bool wxMaxima::QueryVariableValue() {
  if (GetWorksheet() && (!GetWorksheet()->m_evaluationQueue.Empty()))
    return false;

  if (m_maximaBusy)
    return false;

  if (m_configuration.InLispMode())
    return false;

  if (GetWorksheet() && (GetWorksheet()->QuestionPending()))
    return false;

  if (m_varNamesToQuery.size() > 0) {
    SendMaxima(wxS(":lisp-quiet (wx-query-variable \"") +
               m_varNamesToQuery.back() + wxS("\")\n"));
    m_varNamesToQuery.pop_back();
    return true;
  } else {
    if (m_readMaximaVariables) {
      SendMaxima(wxS(":lisp-quiet (wx-print-gui-variables)\n"));
      m_readMaximaVariables = false;
    } else
      {
        if(m_updateAutocompletion)
          SendMaxima(wxS(":lisp-quiet (wxPrint_autocompletesymbols)\n"));
        m_updateAutocompletion = false;
      }
    if (GetWorksheet() && (m_variablesPane) &&
        (m_variablesPane->GetEscapedVarnames().size() != 0))
      m_variablesPane->UpdateSize();

    return false;
  }
}

/***
 * Checks if maxima displayed a new prompt.
 */
void wxMaxima::ReadPrompt(const wxString &data) {
  m_evalOnStartup = false;
  if(!GetWorksheet())
    return;

  GetWorksheet()->SetCurrentTextCell(nullptr);

  // Assume we don't have a question prompt
  GetWorksheet()->m_questionPrompt = false;
  m_ready = true;

  wxLogMessage(_("Got a new input prompt!"));
  m_maximaBusy = false;
  m_bytesFromMaxima = 0;

  wxString label = data.SubString(m_promptPrefix.Length(),
                                  data.Length() - m_promptSuffix.Length() - 1);

  // If we got a prompt our connection to maxima was successful.
  if (m_unsuccessfulConnectionAttempts > 0)
    m_unsuccessfulConnectionAttempts--;
  label.Trim(true);
  label.Trim(false);
  // Input prompts have a length > 0 and end in a number followed by a ")".
  // Depending on ibase the digits of the number might be between 'A' and 'Z',
  // too. Input prompts also begin with a "(". Questions (hopefully)
  // don't do that; Lisp prompts look like question prompts.
  //
  // sbcl debug prompts have the format "(dbm:1)".
  if (((label.Length() > 2) && label.StartsWith("(%") &&
       (!label.StartsWith("(dbm:")) && label.EndsWith(")") &&
       (((label[label.Length() - 2] >= (wxS('0'))) &&
         (label[label.Length() - 2] <= (wxS('9')))) ||
        ((label[label.Length() - 2] >= (wxS('A'))) &&
         (label[label.Length() - 2] <= (wxS('Z')))))) ||
      m_configuration.InLispMode() || (label.StartsWith(wxS("MAXIMA>"))) ||
      (label.StartsWith(wxS("\nMAXIMA>")))) {
    // Maxima displayed a new main prompt => We don't have a question
    GetWorksheet()->QuestionAnswered();
    // And we can remove one command from the evaluation queue.
    GetWorksheet()->m_evaluationQueue.RemoveFirst();

    m_lastPrompt = label;
    // remove the event maxima has just processed from the evaluation queue
    // if we remove a command from the evaluation queue the next output line
    // will be the first from the next command.
    m_outputCellsFromCurrentCommand = 0;
    if (GetWorksheet()->m_evaluationQueue.Empty()) { // queue empty.
      m_exitOnError = false;
      StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
      // If we have selected a cell in order to show we are evaluating it
      // we should now remove this marker.
      if (GetWorksheet()->FollowEvaluation()) {
        if (GetWorksheet()->GetActiveCell())
          GetWorksheet()->GetActiveCell()->SelectNone();
        GetWorksheet()->ClearSelection();
      }
      GetWorksheet()->FollowEvaluation(false);
      // Inform the user that the evaluation queue is empty.
      EvaluationQueueLength(0);
      GetWorksheet()->SetWorkingGroup(nullptr);
      GetWorksheet()->m_evaluationQueue.RemoveFirst();
      GetWorksheet()->RequestRedraw();
      // Now that maxima is idle we can ask for the contents of its variables
      QueryVariableValue();
    } else { // we don't have an empty queue
      m_ready = false;
      GetWorksheet()->RequestRedraw();
      GetWorksheet()->SetWorkingGroup(nullptr);
      StatusMaximaBusy(StatusBar::MaximaStatus::sending);
      TriggerEvaluation();
    }

    if (GetWorksheet()->m_evaluationQueue.Empty()) {
      if ((m_configuration.GetOpenHCaret()) &&
          (GetWorksheet()->GetActiveCell() == NULL))
        GetWorksheet()->OpenNextOrCreateCell();
    }
  } else { // We have a question
    GetWorksheet()->SetLastQuestion(label);
    GetWorksheet()->QuestionAnswered();
    GetWorksheet()->QuestionPending(true);
    // If the user answers a question additional output might be required even
    // if the question has been preceded by many lines.
    m_outputCellsFromCurrentCommand = 0;
    if ((GetWorksheet()->GetWorkingGroup() == NULL) ||
        ((GetWorksheet()->GetWorkingGroup()->m_knownAnswers.empty()) &&
         GetWorksheet()->GetWorkingGroup()->AutoAnswer()))
      GetWorksheet()->SetNotification(_("Maxima asks a question!"),
                                   wxICON_INFORMATION);
    if (!label.IsEmpty()) {
      int options = AppendOpt::NewLine | AppendOpt::BigSkip;
      if ((!GetWorksheet()->GetWorkingGroup()) ||
          (!GetWorksheet()->GetWorkingGroup()->AutoAnswer()))
        options |= AppendOpt::PromptToolTip;

      if (std::max(label.Find(m_mathPrefix1), label.Find(m_mathPrefix2)) >= 0)
        DoConsoleAppend(label, MC_TYPE_PROMPT, AppendOpt(options));
      else
        DoRawConsoleAppend(label, MC_TYPE_PROMPT, AppendOpt(options));
    }
    if (GetWorksheet()->ScrolledAwayFromEvaluation()) {
      if (GetWorksheet()->m_mainToolBar)
        GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    } else
      GetWorksheet()->OpenQuestionCaret();
    StatusMaximaBusy(StatusBar::MaximaStatus::userinput);
  }
  label.Trim(false);
  if (label.StartsWith(wxS("MAXIMA>")) || label.StartsWith("(dbm:")) {
    if (!m_configuration.InLispMode()) {
      if (label.StartsWith("(dbm:"))
        wxLogMessage(_("Switched to lisp mode after receiving a lisp debug prompt!"));
      else
        wxLogMessage(_("Switched to lisp mode after receiving a lisp prompt!"));
    }
    m_configuration.InLispMode(true);
  } else {
    if (m_configuration.InLispMode())
      wxLogMessage(_("Ended lisp mode after receiving a maxima prompt!"));
    m_configuration.InLispMode(false);
  }
}

void wxMaxima::SetCWD(wxString file) {
  // If maxima isn't connected we cannot do anything
  if (!m_client || (!m_client->IsConnected()))
    return;

  // Tell the math parser where to search for local files.
  m_configuration.SetWorkingDirectory(wxFileName(file).GetPath());

#if defined __WXMSW__
  file.Replace(wxS("\\"), wxS("/"));
#endif

  wxFileName filename(file);

  if (filename.GetPath().IsEmpty())
    filename.AssignDir(wxGetCwd());

  // Escape all backslashes in the filename if needed by the OS.
  wxString filenamestring = filename.GetFullPath();
  wxString dirname = filename.GetPath();

#if defined(__WXMSW__)
  // On MSW filenames with a "\" are widely used - but only partially supported.
  filenamestring.Replace(wxS("\\"), wxS("/"));
  dirname.Replace(wxS("\\"), wxS("/"));
#endif

  wxString workingDirectory = filename.GetPath();

  if (workingDirectory != GetCWD()) {
    wxLogMessage(_("Telling maxima about the new working directory."));
    m_configCommands +=
      wxS(":lisp-quiet (setf $wxfilename \"") + filenamestring + wxS("\")\n");
    m_configCommands +=
      wxS(":lisp-quiet (setf $wxdirname \"") + dirname + wxS("\")\n");

    m_configCommands +=
      wxS(":lisp-quiet (wx-cd \"") + filenamestring + wxS("\")\n");
    if (m_ready) {
      if (GetWorksheet() && (GetWorksheet()->m_evaluationQueue.Empty()))
        StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    }
    m_CWD = workingDirectory;
  }
}

bool wxMaxima::OpenMACFile(const wxString &file, Worksheet *document,
                           bool clearDocument) {
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  StatusText(_("Opening file"));
  //  wxWindowUpdateLocker noUpdates(document);

  bool xMaximaFile = file.Lower().EndsWith(wxS(".out"));

  // open mac file
  wxTextFile inputFile(file);

  if (!inputFile.Open()) {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  if (clearDocument)
    document->ClearDocument();

  auto tree = Format::ParseMACFile(inputFile, xMaximaFile, &m_configuration);

  document->InsertGroupCells(std::move(tree), nullptr);

  if (clearDocument) {
    StartMaxima();
    if(GetWorksheet())
      GetWorksheet()->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  } else {
    ResetTitle(false);
    if(GetWorksheet())
      GetWorksheet()->UpdateTableOfContents();
  }

  document->RequestRedraw();

  if(GetWorksheet())
    {
      GetWorksheet()->SetDefaultHCaret();
      CallAfter([this]{GetWorksheet()->SetFocus();});
    }
  SetCWD(file);

  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

  if(GetWorksheet())
    {
      GetWorksheet()->SetHCaret(NULL);
      GetWorksheet()->ScrollToCaret();
    }
  return true;
}

// OpenWXMFile
// Clear document (if clearDocument == true), then insert file
bool wxMaxima::OpenWXMFile(const wxString &file, Worksheet *document,
                           bool clearDocument) {
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  StatusText(_("Opening file"));
  //  wxWindowUpdateLocker noUpdates(document);

  // open wxm file
  wxTextFile inputFile(file);

  if (!inputFile.Open()) {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file + " " + _("(Maybe a permission problem?)"),
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  if (inputFile.GetFirstLine() != Format::WXMFirstLine) {
    inputFile.Close();
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file + " " + _(" (File format (WXM version 1, first line of the file) not recognized)"), // FIXME: The error message could be improved...
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    return false;
  }

  auto tree = Format::ParseWXMFile(inputFile, &m_configuration);
  inputFile.Close();

  // from here on code is identical for wxm and wxmx
  if (clearDocument) {
    document->ClearDocument();
    StartMaxima();
  }

  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate

  if (clearDocument) {
    if(GetWorksheet())
      GetWorksheet()->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  } else
    ResetTitle(false);

  document->RequestRedraw();

  if(GetWorksheet())
    {
      GetWorksheet()->SetDefaultHCaret();
      CallAfter([this]{GetWorksheet()->SetFocus();});
    }
  SetCWD(file);

  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  if(GetWorksheet())
    {
      GetWorksheet()->SetHCaret(NULL);
      GetWorksheet()->ScrollToCaret();
    }
  return true;
}

wxString wxMaxima::ReadPotentiallyUnclosedTag(wxStringTokenizer &lines,
                                              wxString firstLine) {
  wxString result = firstLine + wxS("\n");
  wxString closingTag = firstLine;
  m_xmlOpeningTagName.Replace(&closingTag, wxS("</\\1>"));

  while (lines.HasMoreTokens()) {
    wxString line = lines.GetNextToken();
    if ((line.Contains(wxS("<line"))) || (line.Contains(wxS("</line"))) ||
        (!line.Contains(wxS("<")))) {
      // TODO: Handle broken line tags and line tags that are split into lines.
      result += line + wxS("\n");
    } else {
      if (line.Contains(wxS("</")))
        break;
      else {
        if (line.Contains(wxS("<")))
          result += ReadPotentiallyUnclosedTag(lines, line);
        else
          result += line + wxS("\n");
      }
    }
  }
  if (!m_xmlOpeningTag.Matches(firstLine))
    return wxEmptyString;
  else {
    result += closingTag + wxS("\n");
    return result;
  }
}

wxRegEx wxMaxima::m_xmlOpeningTagName(wxS(".*<([a-zA-Z0-9_]*)[ >].*"));
wxRegEx wxMaxima::m_xmlOpeningTag(wxS("<[^/].*>"));

bool wxMaxima::OpenWXMXFile(const wxString &file, Worksheet *document,
                            bool clearDocument) {
  wxLogMessage(_("Opening a wxmx file"));
  // Show a busy cursor while we open a file.
  wxBusyCursor crs;

  StatusText(_("Opening file"));

  //  wxWindowUpdateLocker noUpdates(document);

  // If the file is empty we don't want to generate an error, but just
  // open an empty file.
  //
  // This makes the following thing work on windows without the need of an
  // empty template file:
  //
  // - Create a registry key named
  // HKEY_LOKAL_MACHINE\SOFTWARE\CLASSES\.wxmx\ShellNew
  // - Create a string named "NullFile" within this key
  //
  // => After the next reboot the right-click context menu's "new" submenu
  // contains
  //    an entry that creates valid empty .wxmx files.
  if (wxFile(file, wxFile::read).Eof()) {
    document->ClearDocument();
    StartMaxima();

    if(GetWorksheet())
      GetWorksheet()->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
    return true;
  }

  // open wxmx file
  wxXmlDocument xmldoc;

  wxFileInputStream wxmxFile(file);
  wxZipInputStream wxmxContents(wxmxFile);
  wxZipEntry *contentsEntry = NULL;
  while(!wxmxContents.Eof())
    {
      contentsEntry = wxmxContents.GetNextEntry();
      if((!contentsEntry) || (contentsEntry->GetName() == wxS("content.xml")))
        break;
    }

  // Open the file
#if wxCHECK_VERSION(3, 3, 0)
  xmldoc.Load(wxmxContents, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
  xmldoc.Load(wxmxContents, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif

  if (!xmldoc.IsOk()) {
    // If we cannot read the file a typical error in old wxMaxima versions was
    // to include a letter of ascii code 27 in content.xml. Let's filter this
    // char out.

    wxString contents;
    if (contentsEntry) {
      // Re-open the file.
      wxmxContents.OpenEntry(*contentsEntry);
      // Read the file into a string
      wxTextInputStream istream1(wxmxContents, wxS('\t'),
                                 wxConvAuto(wxFONTENCODING_UTF8));
      while (!wxmxContents.Eof())
        contents += istream1.ReadLine() + wxS("\n");
    } else {
      wxLogMessage(_("Trying to recover a broken .wxmx file."));
      // Let's try to recover the uncompressed text from a truncated .zip file
      wxFileInputStream input(file);
      if (input.IsOk()) {
        wxLogMessage(_("Trying to extract content.xml out of a broken .zip file."));
        wxTextInputStream text(input, wxS('\t'),
                               wxConvAuto(wxFONTENCODING_UTF8));
        while (input.IsOk() && !input.Eof()) {
          contents = text.ReadLine();
          if (contents.StartsWith(wxS("<wxMaximaDocument"))) {
            contents += wxS("\n");
            break;
          }
        }
        while (input.IsOk() && !input.Eof()) {
          wxString line = text.ReadLine();
          if ((!input.Eof()) || (line != wxEmptyString)) {
            if (line.StartsWith(wxS("</wxMaximaDocument>"))) {
              contents += wxS("</wxMaximaDocument>\n");
              break;
            } else
              contents += line + wxS("\n");
          }
        }
      }
    }
    // Remove the illegal character
    contents.Replace(wxS('\u001b'), wxS("\u238B"));

    {
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(contents);
      wxMemoryInputStream istream(ostream);

      // Try to load the file from the memory buffer.
#if wxCHECK_VERSION(3, 3, 0)
      xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
      xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
    }

    // If the xml document still cannot be loaded let's extract only the input
    // cells.
    if (!xmldoc.IsOk()) {
      wxLogMessage(_("Trying to discard all output."));
      contents.Replace(wxS("><"), wxS(">\n<"));
      wxStringTokenizer lines(contents, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_inputOnly;
      while (lines.HasMoreTokens()) {
        wxString line = lines.GetNextToken();
        if (line.Contains(wxS("<output"))) {
          while (lines.HasMoreTokens() && (!line.Contains(wxS("</output>"))))
            line = lines.GetNextToken();
        } else {
          contents_inputOnly += line + wxS("\n");
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
#if wxCHECK_VERSION(3, 3, 0)
        xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
        xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
      }
    }

    // If even that cannot be loaded let's try to reconstruct the closing
    // markers.
    if (!xmldoc.IsOk()) {
      wxLogMessage(_("Trying to reconstruct the xml closing markers."));
      wxStringTokenizer lines(contents, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_reconstructed;
      wxString line;
      while (lines.HasMoreTokens() &&
             (!line.Contains(wxS("<wxMaximaDocument")))) {
        line = lines.GetNextToken();
        contents_reconstructed += line;
      }
      while (lines.HasMoreTokens()) {
        line = lines.GetNextToken();
        if (line.Contains(wxS("<cell")))
          contents_reconstructed += ReadPotentiallyUnclosedTag(lines, line);
      }
      contents_reconstructed += wxS("</wxMaximaDocument>\n");
      contents = contents_reconstructed;
      {
        // Write the string into a memory buffer
        wxMemoryOutputStream ostream;
        wxTextOutputStream txtstrm(ostream);
        txtstrm.WriteString(contents);
        wxMemoryInputStream istream(ostream);

        // Try to load the file from the memory buffer.
#if wxCHECK_VERSION(3, 3, 0)
        xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
        xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
      }
    }
  }
  if (!xmldoc.IsOk()) {
    LoggingMessageBox(_("wxMaxima cannot read the xml contents of ") + file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  // start processing the XML file
  if (xmldoc.GetRoot()->GetName() != wxS("wxMaximaDocument")) {
    LoggingMessageBox(
                      _("xml contained in the file claims not to be a wxMaxima worksheet. ") +
                      file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion =
    xmldoc.GetRoot()->GetAttribute(wxS("version"), wxS("1.0"));
  if (!CheckWXMXVersion(docversion)) {
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    return false;
  }

  // Determine where the cursor was before saving
  wxString ActiveCellNumber_String =
    xmldoc.GetRoot()->GetAttribute(wxS("activecell"), wxS("-1"));
  long ActiveCellNumber;
  if (!ActiveCellNumber_String.ToLong(&ActiveCellNumber))
    ActiveCellNumber = -1;

  wxString VariablesNumberString =
    xmldoc.GetRoot()->GetAttribute(wxS("variables_num"), wxS("0"));
  long VariablesNumber;
  if (!VariablesNumberString.ToLong(&VariablesNumber))
    VariablesNumber = 0;
  if (VariablesNumber > 0) {
    if(GetWorksheet() && (m_variablesPane))
      m_variablesPane->Clear();

    for (long i = 0; i < VariablesNumber; i++) {
      wxString variable =
        xmldoc.GetRoot()->GetAttribute(wxString::Format("variables_%li", i));
      if(GetWorksheet() && (m_variablesPane))
        m_variablesPane->AddWatch(variable);
    }
  }

  // read the zoom factor
  wxString doczoom = xmldoc.GetRoot()->GetAttribute(wxS("zoom"), wxS("100"));

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = CreateTreeFromXMLNode(xmlcells, file);

  // from here on code is identical for wxm and wxmx
  if (clearDocument) {
    document->ClearDocument();
    StartMaxima();
    long int zoom = 100;
    if (!(doczoom.ToLong(&zoom)))
      zoom = 100;
    document->SetZoomFactor(static_cast<double>(zoom) / 100.0);
  }

  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate
  if (clearDocument) {
    if(GetWorksheet())
      GetWorksheet()->m_currentFile = file;
    ResetTitle(true, true);
    document->SetSaved(true);
  } else
    ResetTitle(false);

  if(GetWorksheet())
    {
      GetWorksheet()->SetDefaultHCaret();
      CallAfter([this]{GetWorksheet()->SetFocus();});
    }
  SetCWD(file);

  // We can set the cursor to the last known position.
  if (GetWorksheet() && (ActiveCellNumber == 0))
    GetWorksheet()->SetHCaret(NULL);
  if (GetWorksheet() && (ActiveCellNumber > 0)) {
    GroupCell *pos = GetWorksheet()->GetTree();

    for (long i = 1; i < ActiveCellNumber; i++)
      if (pos)
        pos = pos->GetNext();

    if (GetWorksheet() && pos)
      GetWorksheet()->SetHCaret(pos);
  }
  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

  if(GetWorksheet())
    GetWorksheet()->Recalculate();

  return true;
}

bool wxMaxima::CheckWXMXVersion(const wxString &docversion) {
  double version = 1.0;
  if (docversion.ToDouble(&version)) {
    int version_major = static_cast<int>(version);
    int version_minor = static_cast<int>(10 * (version - static_cast<double>(version_major)));

    if (version_major > DOCUMENT_VERSION_MAJOR) {
      LoggingMessageBox(_("Document was saved using a newer version of "
                          "wxMaxima. Please update your wxMaxima."),
                        _("Error"), wxOK | wxICON_EXCLAMATION);
      StatusText(_("File could not be opened"));
      return false;
    }
    if (version_minor > DOCUMENT_VERSION_MINOR)
      LoggingMessageBox(
                        _("Document was saved using a newer version of wxMaxima so it may "
                          "not load correctly. Please update your wxMaxima."),
                        _("Warning"), wxOK | wxICON_EXCLAMATION);
  }
  return true;
}

bool wxMaxima::OpenXML(const wxString &file, Worksheet *document) {
  // Show a busy cursor as long as we open a file.
  wxBusyCursor crs;

  StatusText(_("Opening file"));

  //  wxWindowUpdateLocker noUpdates(document);

  wxXmlDocument xmldoc;

  // Let's see if we can load the XML contained in this file.
  xmldoc.Load(file);

  if (!xmldoc.IsOk()) {
    LoggingMessageBox(_("The .xml file doesn't seem to be valid xml or isn't a "
                        "content.xml extracted from a .wxmx zip archive"),
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  // Process the XML document
  if (xmldoc.GetRoot()->GetName() != wxS("wxMaximaDocument")) {
    LoggingMessageBox(
                      _("xml contained in the file claims not to be a wxMaxima worksheet. ") +
                      file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    StatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion =
    xmldoc.GetRoot()->GetAttribute(wxS("version"), wxS("1.0"));
  if (!CheckWXMXVersion(docversion)) {
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    return false;
  }

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = CreateTreeFromXMLNode(xmlcells, file);

  document->ClearDocument();
  StartMaxima();
  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate
  if(GetWorksheet())
    GetWorksheet()->m_currentFile = file;
  ResetTitle(true, true);
  document->RequestRedraw();
  if(GetWorksheet())
    {
      GetWorksheet()->SetDefaultHCaret();
      CallAfter([this]{GetWorksheet()->SetFocus();});
    }
  SetCWD(file);

  StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  return true;
}

std::unique_ptr<GroupCell>
wxMaxima::CreateTreeFromXMLNode(wxXmlNode *xmlcells,
                                const wxString &wxmxfilename) {
  // Show a busy cursor as long as we export a .gif file (which might be a
  // lengthy action).
  wxBusyCursor crs;

  MathParser mp(&m_configuration, wxmxfilename);
  CellListBuilder<GroupCell> tree;

  bool warning = true;

  if (xmlcells)
    xmlcells = xmlcells->GetChildren();

  for (; xmlcells; xmlcells = xmlcells->GetNext()) {
    if (xmlcells->GetType() != wxXML_TEXT_NODE) {
      bool ok = tree.DynamicAppend(mp.ParseTag(xmlcells, false));
      if (!ok && warning) {
        LoggingMessageBox(
                          _("Parts of the document will not be loaded correctly!"),
                          _("Warning"), wxOK | wxICON_WARNING);
        warning = false;
      }
    }
  }
  /* The warning from gcc is correct. But an old MacOs compiler errors out
     on correct code, here. */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-move"
#endif
  return std::move(tree);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
}

wxString wxMaxima::EscapeForLisp(wxString str) {
  str.Replace(wxS("\\"), wxS("\\\\"));
  str.Replace(wxS("\""), wxS("\\\""));
  return (str);
}

void wxMaxima::SetupVariables() {
  wxLogMessage(_("Sending maxima the info how to express 2d maths as XML"));
  wxMathML wxmathml(&m_configuration);
  SendMaxima(wxmathml.GetCmd());
  wxString cmd;

#if defined(__WXOSX__)
  wxString gnuplot_binary = m_gnuplotcommand;

  gnuplot_binary.Replace("\\", "\\\\");
  gnuplot_binary.Replace("\"", "\\\"");
  if (wxFileExists(m_gnuplotcommand))
    cmd += wxS("\n:lisp-quiet (setf $gnuplot_command \"") + m_gnuplotcommand +
      wxS("\")\n");
  wxLogMessage(_("Setting gnuplot_binary to %s"),
               m_gnuplotcommand);
#endif
  cmd.Replace(wxS("\\"), wxS("/"));
  SendMaxima(cmd);

  switch(m_configuration.MaximaHelpFormat())
    {
    case Configuration::frontend:
      SendMaxima(":lisp-quiet (msetq $output_format_for_help '$frontend)");
      break;

    case Configuration::maxima:
      SendMaxima(":lisp-quiet (msetq $output_format_for_help '$text)");
      break;

    case Configuration::browser:
      SendMaxima(":lisp-quiet (msetq $output_format_for_help '$html)");
      break;

    default:
      SendMaxima(":lisp-quiet (msetq $output_format_for_help '$frontend)");
    }
  wxString wxmaximaversion_lisp(WXMAXIMA_VERSION);

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

  wxmaximaversion_lisp.Replace("\\", "\\\\");
  wxmaximaversion_lisp.Replace("\"", "\\\"");
  wxLogMessage(_("Updating maxima's configuration"));
  SendMaxima(wxString(wxS(":lisp-quiet (setq $wxmaximaversion \"")) +
             wxString(wxmaximaversion_lisp) +
             wxS("\") ($put \'$wxmaxima (read-wxmaxima-version \"" +
                 wxString(wxmaximaversion_lisp) +
                 wxS("\") '$version) (setq $wxwidgetsversion \"")) +
             wxString(wxVERSION_STRING) +
             wxS("\")   (if (boundp '$maxima_frontend_version) (setq "
                 "$maxima_frontend_version \"" +
                 wxmaximaversion_lisp +
                 "\")) (ignore-errors (setf (symbol-value "
                 "'*lisp-quiet-suppressed-prompt*) \"" +
                 m_promptPrefix + "(%i1)" + m_promptSuffix + "\"))\n"));
  wxString useHtml = wxS("'$text");
  if (m_configuration.MaximaUsesHtmlBrowser())
    useHtml = wxS("'$html");
  if (m_configuration.MaximaUsesWxmaximaBrowser())
    useHtml = wxS("'$frontend");
  wxLogMessage(_("Setting prompt and help format"));
  SendMaxima(wxS(":lisp-quiet (setf *prompt-suffix* \"") +
             m_promptSuffix + wxS("\") (setf *prompt-prefix* \"") +
             m_promptPrefix +
             wxS("\") (setf $in_netmath nil) (setf $show_openplot t) ") +
             wxS("\n"));

  ConfigChanged();
}

///--------------------------------------------------------------------------------
///  Getting configuration
///--------------------------------------------------------------------------------

wxString wxMaxima::GetCommand(bool params) {
  wxString command;
  if (Get_Maxima_Commandline_Filename().IsEmpty()) {
    command = Configuration::FindProgram(m_configuration.MaximaLocation());
    wxLogMessage(_("Using configured Maxima path."));
  } else {
    command = Get_Maxima_Commandline_Filename();
    wxLogMessage(_("Using Maxima path from command line."));
  }

#if defined(__WXOSX__)
  if (command.EndsWith(wxS(".app"))) // if pointing to a Maxima.app
    command.Append(wxS("/Contents/Resources/maxima.sh"));
#endif
  // if 'maxima' is not searched in the path, check, if the file exists.
  if (command.Cmp("maxima") != 0) {
    if (!wxFileExists(command)) {
      LoggingMessageBox(
                        _("Can not start Maxima. The most probable cause is that Maxima "
                          "isn't installed (it can be downloaded from "
                          "https://maxima.sourceforge.io) or in wxMaxima's config dialogue "
                          "the setting for Maxima's location is wrong."),
                        _("Warning"), wxOK | wxICON_EXCLAMATION);
      StatusText(_("Please configure wxMaxima with 'Edit->Configure'."));
    }
  }
  if (params) {
    // escape quotes
    command.Replace(wxS("\""), wxS("\\\""));
    // surround with quotes
    command =
      wxS("\"") + command + wxS("\" ") + m_configuration.MaximaParameters();
  } else {
    command = wxS("\"") + command + wxS("\"");
  }
  wxString extraMaximaArgs = ExtraMaximaArgs();
  if (!extraMaximaArgs.IsEmpty())
    {
      if (!extraMaximaArgs.StartsWith(" "))
        extraMaximaArgs = " " + extraMaximaArgs;
    }
  command += extraMaximaArgs;
  return command;
}

///--------------------------------------------------------------------------------
///  Tips and help
///--------------------------------------------------------------------------------

void wxMaxima::ShowTip(bool force) {
  bool ShowTips = true;

  // A block with a local config variable:
  // The config can change between before showing the tooltip and afterwards.
  {
    wxConfigBase *config = wxConfig::Get();
    config->Read(wxS("ShowTips"), &ShowTips);
    if ((!ShowTips && !force) || m_evalOnStartup)
      return;
  }

  TipOfTheDay *tip = new TipOfTheDay(this);
  tip->Show();
}

void wxMaxima::LaunchHelpBrowser(wxString uri) {
#ifdef USE_WEBVIEW
  if (m_configuration.InternalHelpBrowser()) {
    m_helpPane->SetURL(uri);
    wxMaximaFrame::ShowPane(EventIDs::menu_pane_help);
  } else
#endif
    {
      if (m_configuration.AutodetectHelpBrowser()) {
        // see https://docs.wxwidgets.org/3.0/classwx_mime_types_manager.html
        auto *manager = wxTheMimeTypesManager;
        wxFileType *filetype = manager->GetFileTypeFromExtension("html");

        wxString command = filetype->GetOpenCommand(uri);
        if(!command.IsEmpty())
          {
            wxLogMessage(_("Launching the system's default help browser as %s."),
                         command);
            wxExecute(command, wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER);
          }
        else
          {
#ifdef USE_WEBVIEW
            if(wxLaunchDefaultBrowser(uri))
              wxLogMessage(_("Didn't get a help browser launch program command line, but can request the system's default help browser."));
            else
              wxLogMessage(_("Launching the system's default help browser failed."));
#endif
          }
      } else {
        wxString command;
        std::vector<char *>argv;
        wxCharBuffer commandnamebuffer = Configuration::FindProgram(m_configuration.HelpBrowserUserLocation()).mb_str();
        wxCharBuffer urlbuffer = uri.mb_str();
        argv.push_back(commandnamebuffer.data());
        argv.push_back(urlbuffer.data());
        argv.push_back(NULL);
        wxExecute(argv.data(), wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER);
      }
    }
}

void wxMaxima::ShowWxMaximaHelp() {
  wxString helpfile = wxMaximaManualLocation();

  if (!wxFileExists(helpfile)) {
    if (!HelpBrowser::AllowOnlineManualP(&m_configuration, this))
      return;

    wxLogMessage(_(wxS("No offline manual found => Redirecting to the wxMaxima homepage")));
    helpfile =
      wxString("https://htmlpreview.github.io/?https://github.com/"
               "wxMaxima-developers/wxmaxima/blob/main/info/wxmaxima.html");
  } else {
#ifdef __WINDOWS__
    // Replace \ with / in the path as directory separator.
    helpfile.Replace("\\", "/", true);
#endif // __WINDOWS__

#ifdef __CYGWIN__
    // Cygwin uses /c/something instead of c:/something and passes this path to
    // the web browser - which doesn't support cygwin paths => convert the path
    // to a native windows pathname if needed.
    if (helpfile.Length() > 1 && helpfile.at(0) == wxS('/')) {
      helpfile.at(0) = helpfile[1];
      helpfile.at(1) = wxS(':');
    }
#endif // __CYGWIN__

    helpfile = wxURI(wxString("file://") +
#ifdef __WINDOWS__
                     wxString("/") +
#endif
                     helpfile)
      .BuildURI();
  }
  LaunchHelpBrowser(helpfile);
}

// Show's the complete Maxima help (offline if available, or offline)
// No handling of anchors for contextsensitive help here
void wxMaxima::ShowMaximaHelpWithoutAnchor() {
  wxString helpfile;
  // That may allow access to translated manuals, similar to
  // wxMaxima::ShowWxMaximaHelp But the translated Maxima manuals are not really
  // good maintained, so leave that for now... wxString lang_long =
  // m_locale->GetCanonicalName(); /* two- or five-letter string in xx or xx_YY
  // format. Examples: "en", "en_GB", "en_US" or "fr_FR" */ wxString lang_short
  // = lang_long.Left(lang_long.Find('_'));
  helpfile = m_maximaHtmlDir.Trim() + wxString("/maxima_singlepage.html");
  if (!wxFileExists(helpfile)) {
    if (!HelpBrowser::AllowOnlineManualP(&m_configuration, this))
      return;

    wxLogMessage(_(wxS("No offline manual found => Redirecting to the Maxima homepage")));
    helpfile = wxString(
                        "https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html");
  } else {
#ifdef __WINDOWS__
    // Replace \ with / in the path as directory separator.
    helpfile.Replace("\\", "/", true);
#endif // __WINDOWS__

#ifdef __CYGWIN__
    // Cygwin uses /c/something instead of c:/something and passes this path to
    // the web browser - which doesn't support cygwin paths => convert the path
    // to a native windows pathname if needed.
    if (helpfile.Length() > 1 && helpfile.at(0) == wxS('/')) {
      helpfile.at(0) = helpfile.at(1);
      helpfile.at(1) = wxS(':');
    }
#endif // __CYGWIN__

    helpfile = wxURI(wxString("file://") +
#ifdef __WINDOWS__
                     wxString("/") +
#endif
                     helpfile)
      .BuildURI();
  }
  LaunchHelpBrowser(helpfile);
}

void wxMaxima::ShowHelp(const wxString &keyword) {
  if ((keyword.IsEmpty()) || (keyword == "%"))
    ShowWxMaximaHelp();
  else
    ShowMaximaHelp(keyword);
}

void wxMaxima::ShowMaximaHelp(wxString keyword) {
  if (keyword.StartsWith("(%i"))
    keyword = "inchar";
  if (keyword.StartsWith("(%o"))
    keyword = "outchar";
  wxString tmp;
  if(GetWorksheet())
    tmp = GetWorksheet()->GetHelpfileAnchorName(keyword);
  if (tmp.IsEmpty())
    keyword = "Function-and-Variable-Index";

  wxString maximaHelpURL;
  if(GetWorksheet())
    maximaHelpURL = GetWorksheet()->GetHelpfileURL(keyword);

  wxBusyCursor crs;
  if (!maximaHelpURL.IsEmpty()) {
    wxLogMessage(_("Opening help file %s"), maximaHelpURL);
    LaunchHelpBrowser(maximaHelpURL);
  } else {
    if (HelpBrowser::AllowOnlineManualP(&m_configuration, this)) {
      wxLogMessage(_(wxS("No offline manual found => Redirecting to the Maxima homepage")));
      LaunchHelpBrowser(
                        "https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#" +
                        keyword);
    }
  }
}

///--------------------------------------------------------------------------------
///  Idle event
///--------------------------------------------------------------------------------

void wxMaxima::OnIdle(wxIdleEvent &event) {
  // Update the info what maxima is currently doing
  UpdateStatusMaximaBusy();

  // Update the info how long the evaluation queue is
  if (m_updateEvaluationQueueLengthDisplay) {
    if ((m_EvaluationQueueLength > 0) || (m_commandsLeftInCurrentCell >= 1)) {
      wxString statusLine = wxString::Format(_("%i cells in evaluation queue"),
                                             m_EvaluationQueueLength);
      if (m_commandsLeftInCurrentCell > 1)
        statusLine +=
          wxString::Format(_("; %i commands left in the current cell"),
                           m_commandsLeftInCurrentCell - 1);
      StatusText(statusLine, false);
    } else {
      if (m_first) {
        if (!m_openInitialFileError)
          StatusText(_("Welcome to wxMaxima"));
      } else {
        if (m_configuration.InLispMode())
          StatusText(_("Lisp mode."));
        else
          StatusText(_("Maxima is ready for input."));
      }
      m_openInitialFileError = false;
    }

    event.RequestMore();
    m_updateEvaluationQueueLengthDisplay = false;
    return;
  }

  if (GetWorksheet() == NULL)
    return;

  GetWorksheet()->UpdateScrollPos();

  // Incremental search is done from the idle task. This means that we don't
  // forcefully need to do a new search on every character that is entered into
  // the search box.
  if (GetWorksheet()->m_findDialog != NULL) {
    if ((m_oldFindString !=
         GetWorksheet()->m_findDialog->GetData()->GetFindString()) ||
        (m_oldFindFlags != GetWorksheet()->m_findDialog->GetData()->GetFlags())) {

      if ((m_configuration.IncrementalSearch()) &&
          (GetWorksheet()->m_findDialog != NULL)) {
        if(!GetWorksheet()->m_findDialog->GetRegexSearch())
          GetWorksheet()->FindIncremental(m_findData.GetFindString(),
                                       m_findData.GetFlags() & wxFR_DOWN,
                                       !(m_findData.GetFlags() & wxFR_MATCHCASE));
        else
          GetWorksheet()->FindIncremental_RegEx(m_findData.GetFindString(),
                                             m_findData.GetFlags() & wxFR_DOWN);
        GetWorksheet()->RequestRedraw();
        m_oldFindFlags = GetWorksheet()->m_findDialog->GetData()->GetFlags();
        m_oldFindString = GetWorksheet()->m_findDialog->GetData()->GetFindString();
        event.RequestMore();
        return;
      }

    }
  }

  if (!m_fastResponseTimer.IsRunning()) {
    bool requestMore = GetWorksheet()->RecalculateIfNeeded(true);
    GetWorksheet()->ScrollToCellIfNeeded();
    GetWorksheet()->ScrollToCaretIfNeeded();
    if (requestMore) {
      event.RequestMore();
      return;
    }
    if (GetWorksheet()->RedrawIfRequested())
      {
        event.RequestMore();
        return;
      }
  }

  // If nothing which is visible has changed nothing that would cause us to need
  // update the menus and toolbars has.
  if (GetWorksheet()->UpdateControlsNeeded()) {
    UpdateMenus();
    UpdateToolBar();
    ResetTitle(GetWorksheet()->IsSaved());
    GetWorksheet()->UpdateControlsNeeded(false);
    event.RequestMore();
    return;
  }

  if (GetWorksheet()->StatusTextChangedHas()) {
    if (GetWorksheet()->StatusTextHas()) {
      m_statusBar->SetStatusText(GetWorksheet()->GetStatusText());
    }
    else
      m_statusBar->SetStatusText(m_leftStatusText);
  }

  if ((m_newStatusText) && (!GetWorksheet()->StatusTextHas())) {
    m_statusBar->SetStatusText(m_leftStatusText);

    m_newStatusText = false;

    wxString toolTip;
    for( auto const &i: m_statusTextHistory)
      if(!i.IsEmpty()) toolTip += i + "\n";

    toolTip += _("\nUse the menu \"View->Toggle log window\" to show/hide a log window with all debug messages.");

    m_statusBar->GetStatusTextElement()->SetToolTip(toolTip);

    event.RequestMore();
    return;
  }

  // If we have set the flag that tells us we should update the table of
  // contents sooner or later we should do so now that wxMaxima is idle.
  if (m_scheduleUpdateToc && m_tableOfContents) {
    m_scheduleUpdateToc = false;
    GroupCell *cursorPos;
    if (GetWorksheet()->GetActiveCell())
      cursorPos = GetWorksheet()->GetActiveCell()->GetGroup();
    else {
      if (GetWorksheet()->HCaretActive())
        cursorPos = GetWorksheet()->GetHCaret();
      else
        cursorPos = GetWorksheet()->FirstVisibleGC();
    }
    m_tableOfContents->UpdateTableOfContents(cursorPos);
    event.RequestMore();
    return;
  }

  if ((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded())) {
    m_xmlInspector->UpdateContents();
    event.RequestMore();
    return;
  }

  if (UpdateDrawPane()) {
    event.RequestMore();
    return;
  }

  UpdateSlider();

  // Update the history sidebar in case it is visible
  if (IsPaneDisplayed(EventIDs::menu_pane_history) && m_history->UpdateDeferred()) {
    event.RequestMore();
    return;
  }

  if (m_ipc.DrainQueue()) {
    event.RequestMore();
    return;
  }
  if(m_configuration.UpdateNeeded())
    {
      m_configuration.ReadConfig();
      GetWorksheet()->RequestRedraw();
      GetWorksheet()->UpdateControlsNeeded(true);
    }

  // Tell our maxima interface if it needs to send events to the XML inspector
  if(m_client)
    m_client->XmlInspectorActive(IsXMLInspectorShown());

  if (m_exitAfterEval && GetWorksheet()->m_evaluationQueue.Empty())
    {
      SaveFile(false);
      CallAfter([this]{Close();});
    }
  // If we reach this point wxMaxima truly is idle
  // => Tell wxWidgets it can process its own idle commands, as well.
  event.Skip();
}

bool wxMaxima::UpdateDrawPane() {
  if(!GetWorksheet())
    return false;
  if (m_drawPane) {
    int dimensions = 0;
    EditorCell *editor = GetWorksheet()->GetActiveCell();
    if (editor) {
      wxString command =
        GetWorksheet()->GetActiveCell()->GetFullCommandUnderCursor();
      if (command.Contains(wxS("gr2d")))
        dimensions = 2;
      if (command.Contains(wxS("with_slider_draw")))
        dimensions = 2;
      if (command.Contains(wxS("gr3d")))
        dimensions = 3;
      if (command.Contains(wxS("draw2d")))
        dimensions = 2;
      if (command.Contains(wxS("draw3d")))
        dimensions = 3;
    } else
      dimensions = 0;

    if (m_drawDimensions_last != dimensions) {
      m_drawPane->SetDimensions(dimensions);
      m_drawDimensions_last = dimensions;
      return true;
    }
  }
  return false;
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::MenuCommand(const wxString &cmd) {
  if(!GetWorksheet())
    return;

  CallAfter([this]{GetWorksheet()->SetFocus();});
  GetWorksheet()->OpenHCaret(cmd);
  GetWorksheet()->AddCellToEvaluationQueue(
                                        GetWorksheet()->GetActiveCell()->GetGroup());
  TriggerEvaluation();
  GetWorksheet()->RequestRedraw();
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::PrintMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  switch (event.GetId()) {
  case wxID_PRINT: {
    wxPrintDialogData printDialogData;
    if (m_printData)
      printDialogData.SetPrintData(*m_printData);
    wxPrinter printer(&printDialogData);
    wxString title(_("wxMaxima document"));

    if (GetWorksheet()->m_currentFile.Length()) {
      wxString suffix;
      wxFileName::SplitPath(GetWorksheet()->m_currentFile, NULL, NULL, &title,
                            &suffix);
      title << wxS(".") << suffix;
    }

    {
      // Redraws during printing might end up on paper => temporarily block all
      // redraw events for the console
      //      wxWindowUpdateLocker noUpdates(GetWorksheet());
      wxEventBlocker blocker(GetWorksheet());
      Printout printout(title, GetWorksheet()->GetTree(), GetContentScaleFactor());
      wxBusyCursor crs;
      if (printer.Print(this, &printout, true)) {
        m_printData = std::unique_ptr<wxPrintData>(
                                                   new wxPrintData(printer.GetPrintDialogData().GetPrintData()));
      }
    }
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
    break;
  }
  }
}

void wxMaxima::UpdateMenus() {
  if (!GetWorksheet())
    return;
  wxASSERT_MSG(
               (!GetWorksheet()->HCaretActive()) || (GetWorksheet()->GetActiveCell() == NULL),
               _("Both horizontal and vertical cursor active at the same time"));
  m_MenuBar->EnableItem(wxID_COPY, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(wxID_CUT, GetWorksheet()->CanCut());
  m_MenuBar->EnableItem(EventIDs::menu_copy_tex_from_worksheet, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::menu_copy_matlab_from_worksheet,
                        GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::popid_copy_mathml, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::menu_copy_as_bitmap, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::menu_copy_as_svg, GetWorksheet()->CanCopy());
#if wxUSE_ENH_METAFILE
  m_MenuBar->EnableItem(EventIDs::menu_copy_as_emf, GetWorksheet()->CanCopy());
#endif
  m_MenuBar->EnableItem(EventIDs::menu_copy_as_rtf, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::menu_copy_to_file, GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(EventIDs::menu_copy_text_from_worksheet,
                        GetWorksheet()->CanCopy());
  m_MenuBar->EnableItem(wxID_SELECTALL, GetWorksheet()->GetTree() != NULL);
  m_MenuBar->EnableItem(wxID_UNDO, GetWorksheet()->CanUndo());
  m_MenuBar->EnableItem(wxID_REDO, GetWorksheet()->CanRedo());
  m_MenuBar->EnableItem(EventIDs::menu_interrupt_id, m_pid > 0);
  m_MenuBar->EnableItem(EventIDs::popid_comment_selection,
                        GetWorksheet()->GetActiveCell() &&
                        GetWorksheet()->GetActiveCell()->SelectionActive());
  m_MenuBar->EnableItem(EventIDs::menu_evaluate, GetWorksheet()->GetActiveCell() ||
                        GetWorksheet()->HasCellsSelected());

  m_MenuBar->EnableItem(EventIDs::menu_evaluate_all_visible, GetWorksheet()->GetTree());
  m_MenuBar->EnableItem(ToolBar::tb_evaltillhere, GetWorksheet()->GetTree() &&
                        GetWorksheet()->CanPaste() &&
                        GetWorksheet()->GetHCaret());

  m_MenuBar->EnableItem(EventIDs::menu_jumptoerror, !GetWorksheet()->GetErrorList().Empty());
  m_MenuBar->EnableItem(wxID_SAVE, (!m_fileSaved));

  for(const auto &pane: GetSidebarNames())
    if(m_MenuBar->FindItem(pane.first) != NULL)
      m_MenuBar->Check(pane.first, IsPaneDisplayed(pane.first));

  bool hidecode = !(m_configuration.ShowCodeCells());
  m_MenuBar->Check(ToolBar::tb_hideCode, hidecode);

  if (GetWorksheet()->GetTree()) {
    m_MenuBar->EnableItem(EventIDs::popid_divide_cell,
                          GetWorksheet()->GetActiveCell());
    m_MenuBar->EnableItem(EventIDs::popid_merge_cells,
                          GetWorksheet()->CanMergeSelection());
    m_MenuBar->EnableItem(wxID_PRINT, true);
  } else {
    m_MenuBar->EnableItem(EventIDs::popid_divide_cell, false);
    m_MenuBar->EnableItem(EventIDs::popid_merge_cells, false);
    m_MenuBar->EnableItem(wxID_PRINT, false);
  }
  double zf = m_configuration.GetZoomFactor();
  if (zf < Configuration::GetMaxZoomFactor())
    m_MenuBar->EnableItem(wxID_ZOOM_IN, true);
  else
    m_MenuBar->EnableItem(wxID_ZOOM_IN, false);
  if (zf > Configuration::GetMinZoomFactor())
    m_MenuBar->EnableItem(wxID_ZOOM_OUT, true);
  else
    m_MenuBar->EnableItem(wxID_ZOOM_OUT, false);
}

void wxMaxima::UpdateToolBar() {
  if((!GetWorksheet()) || (!GetWorksheet()->m_mainToolBar))
    return;

  GetWorksheet()->m_mainToolBar->CanUndo(GetWorksheet()->CanUndo());
  GetWorksheet()->m_mainToolBar->CanRedo(GetWorksheet()->CanRedo());
  GetWorksheet()->m_mainToolBar->CanCopy(GetWorksheet()->CanCopy());
  GetWorksheet()->m_mainToolBar->CanCut(GetWorksheet()->CanCut());
  GetWorksheet()->m_mainToolBar->CanSave((!m_fileSaved));
  GetWorksheet()->m_mainToolBar->CanPrint(GetWorksheet()->GetTree() != NULL);
  GetWorksheet()->m_mainToolBar->CanEvalTillHere(
                                              (GetWorksheet()->GetTree() != NULL) && (GetWorksheet()->CanPaste()) &&
                                              (GetWorksheet()->GetHCaret() != NULL) &&
                                              ((m_client && m_client->IsConnected())));

  // On MSW it seems we cannot change an icon without side-effects that somehow
  // stop the animation => on this OS we have separate icons for the
  // animation start and stop. On the rest of the OSes we use one combined
  // start/stop button instead.
  if (GetWorksheet()->CanAnimate()) {
    const AnimationCell *animation =
      dynamic_cast<AnimationCell *>(GetWorksheet()->GetSelectionStart());
    if (animation->AnimationRunning())
      GetWorksheet()->m_mainToolBar->AnimationButtonState(ToolBar::Running);
    else
      GetWorksheet()->m_mainToolBar->AnimationButtonState(ToolBar::Stopped);
  } else
    GetWorksheet()->m_mainToolBar->AnimationButtonState(ToolBar::Inactive);

  bool follow = GetWorksheet()->ScrolledAwayFromEvaluation();
  switch (m_StatusMaximaBusy) {
  case StatusBar::MaximaStatus::userinput:
    GetWorksheet()->m_mainToolBar->ShowUserInputBitmap();
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    break;
  case StatusBar::MaximaStatus::waitingForAuth:
  case StatusBar::MaximaStatus::waitingForPrompt:
  case StatusBar::MaximaStatus::waiting:
  case StatusBar::MaximaStatus::maximaerror:
  case StatusBar::MaximaStatus::sending:
    GetWorksheet()->m_mainToolBar->ShowFollowBitmap();
    if (GetWorksheet()->GetWorkingGroup() == NULL) {
      GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
      GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
    }
    break;
  case StatusBar::MaximaStatus::calculating:
  case StatusBar::MaximaStatus::transferring:
  case StatusBar::MaximaStatus::parsing:
    GetWorksheet()->m_mainToolBar->ShowFollowBitmap();
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, follow);
    break;
  case StatusBar::MaximaStatus::wait_for_start:
  case StatusBar::MaximaStatus::disconnected:
  case StatusBar::MaximaStatus::process_wont_start:
    GetWorksheet()->m_mainToolBar->ShowFollowBitmap();
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
    GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
    break;
  }
  const EditorCell *editor = GetWorksheet()->GetActiveCell();

  if (editor == NULL) {
    const GroupCell *group = NULL;
    if (GetWorksheet()->GetSelectionStart())
      group = GetWorksheet()->GetSelectionStart()->GetGroup();

    if (group != NULL)
      editor = group->GetEditable();
  }

  bool canEvaluateNext = ((editor != NULL) && (editor->GetTextStyle() == TS_CODE_DEFAULT));

  if (!canEvaluateNext) {
    if (GetWorksheet()->HCaretActive()) {
      const GroupCell *group = GetWorksheet()->GetHCaret();
      if (group == NULL)
        group = GetWorksheet()->GetTree();
      else
        group = group->GetNext();
      while ((group != NULL) &&
             (!((group->GetEditable() != NULL) &&
                (group->GetEditable()->GetType() == MC_TYPE_INPUT)) &&
              (!GetWorksheet()->m_evaluationQueue.IsLastInQueue(group))))
        group = group->GetNext();

      if (group != NULL)
        canEvaluateNext = true;
    }
  }
  if (canEvaluateNext)
    GetWorksheet()->m_mainToolBar->CanEvalThisCell(true);
  else
    GetWorksheet()->m_mainToolBar->CanEvalThisCell(false);
  GetWorksheet()->m_mainToolBar->WorksheetEmpty(GetWorksheet()->GetTree() == NULL);

  GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
}

wxString wxMaxima::ExtractFirstExpression(const wxString &entry) {
  long semicolon = entry.Find(';');
  long dollar = entry.Find('$');
  bool semiFound = (semicolon != wxNOT_FOUND);
  bool dollarFound = (dollar != wxNOT_FOUND);

  std::size_t index;
  if (semiFound && dollarFound)
    index = std::min(semicolon, dollar);
  else if (semiFound && !dollarFound)
    index = semicolon;
  else if (!semiFound && dollarFound)
    index = dollar;
  else // neither semicolon nor dollar found
    index = entry.Length();

  return entry.SubString(0, index - 1);
}

wxString wxMaxima::GetDefaultEntry() {
  if(!GetWorksheet())
    return wxEmptyString;
  if (GetWorksheet()->CanCopy())
    return (GetWorksheet()->GetString()).Trim().Trim(false);
  wxString retval;
  if (GetWorksheet()->GetActiveCell() != NULL)
    return retval = GetWorksheet()->GetActiveCell()->GetWordUnderCaret();
  if (GetWorksheet()->IsSelected(MC_TYPE_DEFAULT))
    return GetWorksheet()->GetSelectionStart()->ToString();
  if (retval.IsEmpty())
    retval = "%";
  return retval;
}

bool wxMaxima::OpenFile(const wxString &file, const wxString &command) {
  wxBusyCursor crs;
  bool retval = true;
  if (file.IsEmpty()) {
    wxLogError(_("Trying to open a file with an empty name!"));
    return false;
  }
  if (!(wxFileExists(file))) {
    wxLogError(_("Trying to open the non-existing file %s"), file);
    return false;
  }

  m_lastPath = wxPathOnly(file);
  wxString unixFilename(file);
#if defined __WXMSW__
  unixFilename.Replace(wxS("\\"), wxS("/"));
#endif

  //  wxWindowUpdateLocker dontUpdateTheWorksheet(GetWorksheet());

  if (command.Length() > 0) {
    MenuCommand(command + wxS("(\"") + unixFilename + wxS("\")$"));
    if (command == wxS("load")) {
      ReReadConfig();
      m_recentPackages.AddDocument(unixFilename);
      UpdateRecentDocuments();
      ReReadConfig();
    }
  } else if (file.Lower().EndsWith(wxS(".wxm"))) {
    retval = OpenWXMFile(file, GetWorksheet());
    if (retval) {
      ReReadConfig();
      if (!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Lower().EndsWith(wxS(".mac"))) {
    retval = OpenMACFile(file, GetWorksheet());
    if (retval) {
      ReReadConfig();
      if (!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  } else if (file.Lower().EndsWith(wxS(".out"))) {
    retval = OpenMACFile(file, GetWorksheet());
    if (retval) {
      ReReadConfig();
      if (!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.EndsWith(wxS(".wxmx")) || file.EndsWith(wxS(".wxmx~")) ) {
    retval = OpenWXMXFile(file, GetWorksheet());
    if (retval) {
      ReReadConfig();
      if (!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Right(4).Lower() == wxS(".zip")) {
    retval = OpenWXMXFile(file, GetWorksheet());
    if (retval) {
      ReReadConfig();
      if (!m_exitAfterEval)
        m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.Right(4).Lower() == wxS(".dem")) {
    MenuCommand(wxS("demo(\"") + unixFilename + wxS("\")$"));
    ReReadConfig();
    m_recentPackages.AddDocument(file);
    UpdateRecentDocuments();
    ReReadConfig();
  }

  else if (file.Right(4).Lower() == wxS(".xml"))
    retval = OpenXML(file, GetWorksheet()); // clearDocument = true

  else {
    MenuCommand(wxS("load(\"") + unixFilename + wxS("\")$"));
    ReReadConfig();
    m_recentPackages.AddDocument(unixFilename);
    UpdateRecentDocuments();
    ReReadConfig();
  }

  UpdateRecentDocuments();
  RemoveTempAutosavefile();
  StartAutoSaveTimer();

  GetWorksheet()->TreeUndo_ClearBuffers();
  if (GetWorksheet()->m_currentFile != wxEmptyString) {
    wxString filename(GetWorksheet()->m_currentFile);
    SetCWD(std::move(filename));
  }
  if (m_tableOfContents != NULL) {
    m_scheduleUpdateToc = false;
    m_tableOfContents->UpdateTableOfContents(
                                             GetWorksheet()->GetHCaret());
  }

  if (!retval)
    StatusText(wxString::Format("Errors trying to open the file %s.",
                                file));

  if (retval) {
    GetWorksheet()->RequestRedraw();
    StatusText(_("File opened"));
    if (m_evalOnStartup && m_ready) {
      wxLogMessage(_("Starting evaluation of the document"));
      m_evalOnStartup = false;
      EvaluationQueueLength(
                            GetWorksheet()->m_evaluationQueue.Size(),
                            GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
      TriggerEvaluation();
    }
  } else
    StatusText(_("File could not be opened"));

  GetWorksheet()->Recalculate();
  UpdateMenus();
  return retval;
}

void wxMaxima::OnUpdateTOCEvent(wxCommandEvent &WXUNUSED(event))
{
  m_scheduleUpdateToc = true;
}


bool wxMaxima::SaveFile(bool forceSave) {
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;

  wxString file = GetWorksheet()->m_currentFile;
  wxString fileExt = wxS("wxmx");
  int ext = 0;

  wxConfigBase *config = wxConfig::Get();

  if (file.Length() == 0 || forceSave) {
    if (file.Length() == 0) {
      config->Read(wxS("defaultExt"), &fileExt);
      file = _("untitled") + wxS(".") + fileExt;
    } else
      wxFileName::SplitPath(file, NULL, NULL, &file, &fileExt);

    wxFileDialog fileDialog(
                            this, _("Save As"), m_lastPath, file,
                            _("Whole document (*.wxmx)|*.wxmx|"
                              "The input, readable by load() (maxima > 5.38) (*.wxm)|*.wxm|"
                              "All Files (*.*)|*"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (fileExt == wxS("wxmx"))
      fileDialog.SetFilterIndex(0);
    else if (fileExt == wxS("wxm"))
      fileDialog.SetFilterIndex(1);
    else {
      fileDialog.SetFilterIndex(0);
      fileExt = wxS("wxmx");
    }
    if (fileDialog.ShowModal() == wxID_OK) {
      file = fileDialog.GetPath();
      ext = fileDialog.GetFilterIndex();
    } else {
      StartAutoSaveTimer();
      return false;
    }
  }

  if (file.Length()) {
    if (!file.Lower().EndsWith(wxS(".wxm")) &&
        (!file.Lower().EndsWith(wxS(".wxmx")))) {
      switch (ext) {
      case 0:
        file += wxS(".wxmx");
        break;
      case 1:
        file += wxS(".wxm");
        break;
      default:
        file += wxS(".wxmx");
      }
    }

    StatusSaveStart();
    config->Write(wxS("defaultExt"), wxS("wxmx"));

    m_lastPath = wxPathOnly(file);
    if (file.Lower().EndsWith(wxS(".wxm"))) {
      config->Write(wxS("defaultExt"), wxS("wxm"));
      if (!GetWorksheet()->ExportToMAC(file)) {
        StatusSaveFailed();
        LoggingMessageBox(_("Saving failed!"), _("Error!"), wxOK);
        StartAutoSaveTimer();
        return false;
      } else {
        RemoveTempAutosavefile();
        if (file != m_tempfileName)
          GetWorksheet()->m_currentFile = file;
      }
    } else {
      if (!Format::ExportToWXMX(GetWorksheet()->GetTree(), file, &m_configuration,
                                &GetWorksheet()->GetCellPointers(),
                                m_variablesPane->GetVarnames(),
                                GetWorksheet()->GetHCaret())) {
        StatusSaveFailed();
        LoggingMessageBox(_("Saving failed!"), _("Error!"), wxOK);
        StartAutoSaveTimer();
        return false;
      } else {
        GetWorksheet()->SetSaved(true);
        RemoveTempAutosavefile();
        if (file != m_tempfileName)
          GetWorksheet()->m_currentFile = file;
      }
    }

    if (!m_exitAfterEval)
      m_recentDocuments.AddDocument(file);
    SetCWD(file);
    StatusSaveFinished();
    UpdateRecentDocuments();
  }

  StartAutoSaveTimer();

  return true;
}

void wxMaxima::ReadStdErr() {
  // Maxima will never send us any data via stderr after it has finished
  // starting up and will send data via stdout only in rare cases:
  // It rather sends us the data over the network.
  //
  // If something is severely broken this might not be true, though, and we want
  // to inform the user about it.

  if (m_maximaProcess == NULL)
    return;

  if (m_maximaProcess->IsInputAvailable()) {
    wxASSERT_MSG(
                 m_maximaStdout != NULL,
                 wxS("Bug: Trying to read from maxima but don't have an input stream"));
    if(m_maximaStdout == NULL)
      return;
    wxTextInputStream istrm(*m_maximaStdout, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_maximaStdout->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = _("Message from the stdout of Maxima: ") + o;
    if ((o_trimmed != wxEmptyString) &&
        (!o_trimmed.StartsWith("Connecting Maxima to server on port")) && (!m_first)) {
      DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
      if (Maxima::GetPipeToStdErr())
        std::cerr << o;
    }
  }
  if (m_maximaProcess->IsErrorAvailable()) {
    wxASSERT_MSG(m_maximaStderr != NULL,
                 wxS("Bug: Trying to read from maxima but don't have a error "
                     "input stream"));
    if(m_maximaStderr == NULL)
      return;
    wxTextInputStream istrm(*m_maximaStderr, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_maximaStderr->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = wxS("Message from maxima's stderr stream: ") + o;

    if ((o != wxS("Message from maxima's stderr stream: End of animation sequence")) &&
        (o != wxS("Message from maxima's stderr stream: Fontconfig warning: using without calling FcInit()")) &&  // harmless warning, which may occur with Gnuplot 6 and fontconfig
        (o != wxS("Message from maxima's stderr stream: QSocketNotifier: Can only be used with threads started with QThread")) &&  // Maybe related to Gnuplot / Wayland?
        !o.Contains("frames in animation sequence") &&
        (o_trimmed != wxEmptyString) && (o.Length() > 1)) {
      DoRawConsoleAppend(o, MC_TYPE_ERROR);
      AbortOnError();
      TriggerEvaluation();
      GetWorksheet()->GetErrorList().Add(GetWorksheet()->GetWorkingGroup(true));

      if (Maxima::GetPipeToStdErr())
        std::cerr << o;
    } else
      DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
  }
}

bool wxMaxima::AbortOnError() {
  // Maxima encountered an error.
  // The question is now if we want to try to send it something new to evaluate.

  ExitAfterEval(false);
  EvalOnStartup(false);

  if (GetWorksheet()->m_notificationMessage) {
    if (GetWorksheet()->GetWorkingGroup(true) !=
        GetWorksheet()->m_notificationMessage->m_errorNotificationCell)
      GetWorksheet()->SetNotification(_("Maxima has issued an error!"),
                                   wxICON_ERROR);
    GetWorksheet()->m_notificationMessage->m_errorNotificationCell =
      GetWorksheet()->GetWorkingGroup(true);
  }

  if (GetExitOnError()) {
    wxMaxima::m_exitCode = 1;
    Close();
  }
  if (m_configuration.GetAbortOnError()) {
    GetWorksheet()->m_evaluationQueue.Clear();
    // Inform the user that the evaluation queue is empty.
    EvaluationQueueLength(0);
    GetWorksheet()->ScrollToError();
    return true;
  } else
    return false;
}

long long wxMaxima::GetTotalCpuTime() {
#ifdef __WXMSW__
  FILETIME systemtime;
  GetSystemTimeAsFileTime(&systemtime);
  return (long long)systemtime.dwLowDateTime +
    ((long long)systemtime.dwHighDateTime << 32);
#else
  int CpuJiffies = 0;
  if (wxFileExists("/proc/stat")) {
    wxFileInputStream input("/proc/stat");
    if (input.IsOk()) {
      wxTextInputStream text(input, wxS('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      wxString line;
      while ((!input.Eof()) && (!line.StartsWith("cpu ")))
        line = text.ReadLine();

      // Strip the "cpu" from the line
      line = line.Right(line.Length() - 4);
      line.Trim(false);
      wxStringTokenizer tokens(line, wxS(" "));
      for (int i = 0; i < 3; i++) {
        if (tokens.HasMoreTokens()) {
          long additionalJiffies;
          if (!tokens.GetNextToken().ToLong(&additionalJiffies))
            return -1;
          CpuJiffies += additionalJiffies;
        } else
          return -1;
      }
    }
  }
  return CpuJiffies;
#endif
}

long long wxMaxima::GetMaximaCpuTime() {
#ifdef __WXMSW__
  HANDLE maximaHandle =
    OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, m_pid);
  if (maximaHandle != NULL) {
    FILETIME creationTime, exitTime, kernelTime, userTime;
    if (GetProcessTimes(maximaHandle, &creationTime, &exitTime, &kernelTime,
                        &userTime)) {
      long long retval =
        (long long)kernelTime.dwLowDateTime + userTime.dwLowDateTime +
        (1LL << 32) *
        ((long long)kernelTime.dwHighDateTime + userTime.dwHighDateTime);
      CloseHandle(maximaHandle);

      return retval;
    }
  }
#endif
  int maximaJiffies = 0;
  wxString statFileName = wxString::Format("/proc/%li/stat", m_pid);
  if (wxFileExists(statFileName)) {
    wxFileInputStream input(statFileName);
    if (input.IsOk()) {
      wxTextInputStream text(input, wxS('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      wxString line = text.ReadLine();

      wxStringTokenizer tokens(line, wxS(" "));
      for (int i = 0; i < 13; i++) {
        if (tokens.HasMoreTokens())
          tokens.GetNextToken();
        else
          return -1;
      }

      for (int i = 0; i < 4; i++) {
        {
          if (tokens.HasMoreTokens()) {
            long additionalJiffies;
            if (!tokens.GetNextToken().ToLong(&additionalJiffies)) {
              maximaJiffies = -1;
              break;
            }
            maximaJiffies += additionalJiffies;
          } else
            return -1;
        }
      }
    }
  }
  return maximaJiffies;
}

double wxMaxima::GetMaximaCPUPercentage() {
  int CpuJiffies = GetTotalCpuTime();
  if (CpuJiffies < 0)
    return -1;

  // If no time has passed since the last call to this function the number of
  // CPU cycles per timespan is infinite - and this function will cause an error
  // if we don't abort it now.
  if (CpuJiffies == m_cpuTotalJiffies_old)
    return -1;

  if (CpuJiffies <= m_cpuTotalJiffies_old) {
    m_cpuTotalJiffies_old = CpuJiffies;
    return -1;
  }

  int maximaJiffies = GetMaximaCpuTime();
  if (maximaJiffies < 0)
    return -1;

  double retval = static_cast<double>(maximaJiffies - m_maximaJiffies_old) /
    (CpuJiffies - m_cpuTotalJiffies_old) * 100;

  m_maximaJiffies_old = maximaJiffies;
  m_cpuTotalJiffies_old = CpuJiffies;
  return retval;
}

void wxMaxima::OnTimerEvent(wxTimerEvent &event) {
  switch (event.GetId()) {
  case MAXIMA_STDOUT_POLL_ID:
    ReadStdErr();

    if (m_maximaProcess != NULL) {
      // The atexit() of maxima informs us if the process dies. But it sometimes
      // doesn't do so if it dies due to an out of memory => Periodically check
      // if it really lives.
      if (!wxProcess::Exists(m_maximaProcess->GetPid())) {
//        OnMaximaClose();
      }

      double cpuPercentage = GetMaximaCPUPercentage();
      m_statusBar->SetMaximaCPUPercentage(cpuPercentage);

      if ((m_maximaProcess != NULL) && (m_pid > 0) &&
          ((cpuPercentage > 0) || (m_maximaBusy)))
        m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
    }

    break;
  case KEYBOARD_INACTIVITY_TIMER_ID:
  case AUTO_SAVE_TIMER_ID:
    if ((!GetWorksheet()->m_keyboardInactiveTimer.IsRunning()) &&
        (!m_autoSaveTimer.IsRunning())) {
      AutoSave();
      StartAutoSaveTimer();
    }
    break;
  }
}

bool wxMaxima::AutoSave() {
  if (!SaveNecessary())
    return true;

  bool savedWas = GetWorksheet()->IsSaved();
  wxString oldTempFile = m_tempfileName;
  wxString oldFilename = GetWorksheet()->m_currentFile;
  m_tempfileName = wxFileName::CreateTempFileName("untitled_");
  wxRenameFile(m_tempfileName,  m_tempfileName + ".wxmx");
  m_tempfileName = m_tempfileName + ".wxmx";

  /* if the current filename is empty - the file was not saved under a given name - save it using a temporary file name */
  if (m_configuration.AutoSaveAsTempFile() || GetWorksheet()->m_currentFile.IsEmpty()) {
    bool saved = Format::ExportToWXMX(GetWorksheet()->GetTree(), m_tempfileName,
                                      &m_configuration,
                                      &GetWorksheet()->GetCellPointers(),
                                      m_variablesPane->GetVarnames(),
                                      GetWorksheet()->GetHCaret());
    wxFileName m_tempfileName_permissions(m_tempfileName);
    m_tempfileName_permissions.SetPermissions(wxPOSIX_USER_READ | wxPOSIX_USER_WRITE);

    wxLogMessage(_("Autosaving as temp file %s"), m_tempfileName);
    if ((m_tempfileName != oldTempFile) && saved) {
      GetWorksheet()->SetSaved(true);
      if (!oldTempFile.IsEmpty()) {
        if (wxFileExists(oldTempFile)) {
          wxLogMessage(_("Trying to remove the old temp file %s"), oldTempFile);
          wxRemoveFile(oldTempFile);
        }
      }
    }
    RegisterAutoSaveFile();
  } else {
    wxLogMessage(_("Autosaving the .wxmx file as %s"), GetWorksheet()->m_currentFile);
    savedWas = SaveFile(false);
  }

  GetWorksheet()->SetSaved(savedWas);
  ResetTitle(savedWas, true);

  oldTempFile = m_tempfileName;
  GetWorksheet()->m_currentFile = oldFilename;
  return savedWas;
}

void wxMaxima::FileMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  bool forceSave = false;
#if defined __WXMSW__
  wxString b = wxS("\\");
  wxString f = wxS("/");
#endif

  if((event.GetId() == wxID_EXIT) || (event.GetId() == wxID_CLOSE)) {
    CallAfter([this]{Close();});
  }
  else if(event.GetId() == wxID_OPEN) {
    if (SaveNecessary()) {
      int close = SaveDocumentP();

      if (close == wxID_CANCEL)
        return;

      if (close == wxID_YES) {
        if (!SaveFile())
          return;
      }
    }

    wxString file =
      wxFileSelector(_("Open"), m_lastPath, wxEmptyString, wxEmptyString,
                     _("All openable types (*.wxm, *.wxmx, *.mac, *.out, "
                       "*.xml)|*.wxm;*.wxmx;*.mac;*.out;*.xml|"
                       "wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx|"
                       "Maxima session (*.mac)|*.mac|"
                       "Xmaxima session (*.out)|*.out|"
                       "xml from broken .wxmx (*.xml)|*.xml"),
                     wxFD_OPEN);

    if (!file.empty()) {
      // On the mac the "File/New" menu item by default opens a new window instead of
      // reusing the old one.
#ifdef __WXOSX__
      if (GetWorksheet()->IsEmpty())
        OpenFile(file, wxEmptyString);
      else
        wxGetApp().NewWindow(file);
#else
      OpenFile(file, wxEmptyString);
#endif
    }
  }
  else if(event.GetId() == wxID_SAVEAS) {
    forceSave = true;
    m_fileSaved = false;
    SaveFile(forceSave);
    // Seems like resetting the title on "file/save as" is a little bit
    // sluggish, otherwise.
    ResetTitle(GetWorksheet()->IsSaved(), true);
  }
  else if(event.GetId() == wxID_SAVE) {
    SaveFile(forceSave);
    // Seems like resetting the title on "file/save as" is a little bit
    // sluggish, otherwise.
    ResetTitle(GetWorksheet()->IsSaved(), true);
  }
  else if(event.GetId() == EventIDs::menu_export_html) {
    // Determine a sane default file name;
    wxString file = GetWorksheet()->m_currentFile;
    if (file.Length() == 0)
      file = _("untitled");
    else
      wxFileName::SplitPath(file, NULL, NULL, &file, NULL);

    wxString fileExt = "html";
    wxConfig::Get()->Read(wxS("defaultExportExt"), &fileExt);

    wxFileDialog fileDialog(this, _("Export"), m_lastPath,
                            file + wxS(".") + fileExt,
                            _("HTML file (*.html)|*.html|"
                              "maxima batch file (*.mac)|*.mac|"
                              "LaTeX file (*.tex)|*.tex"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (fileExt == wxS("html"))
      fileDialog.SetFilterIndex(0);
    else if (fileExt == wxS("mac"))
      fileDialog.SetFilterIndex(1);
    else
      fileDialog.SetFilterIndex(2);

    if (fileDialog.ShowModal() == wxID_OK) {
      file = fileDialog.GetPath();
      if (file.Length()) {
        int ext = fileDialog.GetFilterIndex();
        if ((!file.Lower().EndsWith(wxS(".html"))) &&
            (!file.Lower().EndsWith(wxS(".mac"))) &&
            (!file.Lower().EndsWith(wxS(".tex")))) {
          switch (ext) {
          case 0:
            file += wxS(".html");
            break;
          case 1:
            file += wxS(".mac");
            break;
          case 2:
            file += wxS(".tex");
            break;
          default:
            file += wxS(".html");
          }
        }

        if (file.Lower().EndsWith(wxS(".tex"))) {
          StatusExportStart();

          fileExt = wxS("tex");
          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          if (!GetWorksheet()->ExportToTeX(file)) {
            LoggingMessageBox(_("Exporting to TeX failed!"), _("Error!"), wxOK);
            StatusExportFailed();
          } else
            StatusExportFinished();
        } else if (file.Lower().EndsWith(wxS(".mac"))) {
          StatusExportStart();

          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          fileExt = wxS("mac");
          if (!GetWorksheet()->ExportToMAC(file)) {
            LoggingMessageBox(_("Exporting to maxima batch file failed!"),
                              _("Error!"), wxOK);
            StatusExportFailed();
          } else
            StatusExportFinished();
        } else {
          StatusExportStart();

          // Show a busy cursor as long as we export a file.
          wxBusyCursor crs;
          fileExt = wxS("html");
          if (!GetWorksheet()->ExportToHTML(file)) {
            LoggingMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                              wxOK);
            StatusExportFailed();
          } else
            StatusExportFinished();
        }
        StartAutoSaveTimer();

        wxConfig::Get()->Write(wxS("defaultExportExt"), fileExt);
      }
    }
  }
  else if(event.GetId() == EventIDs::menu_load_id) {
    wxString file = wxFileSelector(_("Load Package"), m_lastPath, wxEmptyString,
                                   wxEmptyString,
                                   _("Maxima package (*.mac)|*.mac|"
                                     "Lisp package (*.lisp)|*.lisp|All|*"),
                                   wxFD_OPEN);
    if (!file.empty())
      OpenFile(file, wxS("load"));
  }
  else if(event.GetId() == EventIDs::menu_batch_id) {
    wxString file = wxFileSelector(
                                   _("Batch File"), m_lastPath, wxEmptyString, wxEmptyString,
                                   _("Maxima package (*.mac)|*.mac"), wxFD_OPEN);
    if (file != wxEmptyString)
      OpenFile(file, wxS("batch"));
  }
  else if(event.GetId() == ToolBar::tb_animation_startStop) {
    if (GetWorksheet()->CanAnimate()) {
      const AnimationCell *animation =
        dynamic_cast<AnimationCell *>(GetWorksheet()->GetSelectionStart());
      if (animation->AnimationRunning())
        GetWorksheet()->Animate(false);
      else
        GetWorksheet()->Animate(true);
    }
  }
  else if(event.GetId() == EventIDs::popid_animation_start) {
    if (GetWorksheet()->CanAnimate()) {
      AnimationCell *animation =
        dynamic_cast<AnimationCell *>(GetWorksheet()->GetSelectionStart());
      animation->AnimationRunning(true);
    }
  }
  GetWorksheet()->RequestRedraw();
}

void wxMaxima::EditMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  // if (GetWorksheet()->m_findDialog != NULL) {
  //   event.Skip();
  //   return;
  // }

  wxString expr = GetDefaultEntry();
  if(((event.GetId()) >= EventIDs::popid_labelwidth1) &&
     ((event.GetId()) < EventIDs::popid_labelwidth1 + LABELWIDTH_MAX - LABELWIDTH_MIN)) {
    m_configuration.LabelWidth(EventIDs::popid_labelwidth1 + 1 - event.GetId());
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_20) {
    m_configuration.SetDisplayedDigits(20);
    m_configuration.ShowAllDigits(false);
    m_configuration.LineBreaksInLongNums(false);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_50) {
    m_configuration.SetDisplayedDigits(50);
    m_configuration.ShowAllDigits(false);
    m_configuration.LineBreaksInLongNums(false);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_100) {
    m_configuration.SetDisplayedDigits(100);
    m_configuration.ShowAllDigits(false);
    m_configuration.LineBreaksInLongNums(false);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_all) {
    m_configuration.ShowAllDigits(true);
    m_configuration.LineBreaksInLongNums(false);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if((event.GetId()) == EventIDs::popid_digits_all_linebreak) {
    m_configuration.ShowAllDigits(true);
    m_configuration.LineBreaksInLongNums(true);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_inputlabels_hide) {
    m_configuration.ShowInputLabels(!m_configuration.ShowInputLabels());
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_autogenerated) {
    m_configuration.SetLabelChoice(Configuration::labels_automatic);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_user) {
    m_configuration.SetLabelChoice(Configuration::labels_prefer_user);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_useronly) {
    m_configuration.SetLabelChoice(Configuration::labels_useronly);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_labels_disable) {
    m_configuration.SetLabelChoice(Configuration::labels_none);
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_popup_gnuplot) {
    if (!GetWorksheet()->GetSelectionStart())
      return;

    wxString gnuplotSource = GetWorksheet()->GetSelectionStart()->GnuplotSource();
    if (gnuplotSource.IsEmpty())
      return;

    if (!wxFileExists(gnuplotSource))
      return;

    // Create a gnuplot file that doesn't select a terminal and output file
    wxFileInputStream input(gnuplotSource);
    if (!input.IsOk())
      return;
    wxTextInputStream textIn(input, wxS('\t'),
                             wxConvAuto(wxFONTENCODING_UTF8));
    wxString gnuplot_popout_tempfilename = wxFileName::CreateTempFileName("wxmaxima_gnuplot_popout_");
    wxFileOutputStream output(gnuplot_popout_tempfilename);
    if (!output.IsOk())
      return;
    wxTextOutputStream textOut(output);
#ifdef __WXMSW__
    textOut << "set term windows\n";
#endif
    textIn.ReadLine();
    textIn.ReadLine();

    wxString line;
    while (!input.Eof()) {
      line = textIn.ReadLine();
      textOut << line + wxS("\n");
    }
    textOut.Flush();

    // Execute gnuplot
    std::vector<char *> argv;
    wxCharBuffer commandnamebuffer = m_gnuplotcommand.mb_str();
    argv.push_back(commandnamebuffer.data());
    wxCharBuffer urlbuffer = wxString(gnuplot_popout_tempfilename).mb_str();
    argv.push_back(urlbuffer.data());
    wxCharBuffer persist_opt = wxString(wxS("--persist")).mb_str();
    argv.push_back(persist_opt.data());
    argv.push_back(NULL);

    wxLogMessage(_("Running %s on the file %s: "), commandnamebuffer, urlbuffer);
    m_gnuplotProcess = new wxProcess(this, m_gnuplot_process_id);
    if (wxExecute(argv.data(), wxEXEC_ASYNC | wxEXEC_SHOW_CONSOLE | wxEXEC_MAKE_GROUP_LEADER,
                  m_gnuplotProcess) < 0)
      wxLogMessage(_("Cannot start gnuplot"));
  }
  else if(event.GetId() == wxID_PREFERENCES) {
    // wxGTK uses wxFileConf. ...and wxFileConf loads the config file only once
    // on initialisation => Let's reload the config file before entering the
    // config dialogue.
    ReReadConfig();
    wxConfigBase *config = wxConfig::Get();
    // Write the changes in the configuration to the disk.
    config->Flush(true);
    ConfigDialogue *configW = new ConfigDialogue(this);
    configW->Centre(wxBOTH);
    auto result = configW->ShowModal();
    if (result == wxID_OK) {
      configW->WriteSettings();
      // Refresh the display as the settings that affect it might have changed.
      m_configuration.ReadConfig();
      m_configuration.FontChanged();
      if (GetWorksheet()->GetTree())
        GetWorksheet()->GetTree()->FontsChangedList();
      ConfigChanged();
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
    configW->Destroy();
  }
  else if(event.GetId() == wxID_COPY) {
    GetWorksheet()->Copy();
  }
  else if(event.GetId() == EventIDs::menu_copy_text_from_worksheet) {
    GetWorksheet()->Copy(true);
  }
  else if(event.GetId() == wxID_CUT) {
    if (GetWorksheet()->CanCut())
      GetWorksheet()->CutToClipboard();
  }
  else if(event.GetId() == wxID_SELECTALL) {
    GetWorksheet()->SelectAll();
  }
  else if(event.GetId() == wxID_PASTE) {
    GetWorksheet()->PasteFromClipboard();
  }
  else if(event.GetId() == wxID_UNDO) {
    if (GetWorksheet()->CanUndo())
      GetWorksheet()->Undo();
  }
  else if(event.GetId() == wxID_REDO) {
    if (GetWorksheet()->CanRedo())
      GetWorksheet()->Redo();
  }
  else if(event.GetId() == EventIDs::menu_copy_matlab_from_worksheet) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyMatlab();
  }
  else if(event.GetId() == EventIDs::menu_copy_tex_from_worksheet) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyTeX();
  }
  else if(event.GetId() == EventIDs::popid_copy_mathml) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyMathML();
  }
  else if(event.GetId() == EventIDs::menu_copy_as_bitmap) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyBitmap();
  }
  else if(event.GetId() == EventIDs::menu_copy_as_svg) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopySVG();
  }
#if wxUSE_ENH_METAFILE
  else if(event.GetId() == EventIDs::menu_copy_as_emf) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyEMF();
  }
#endif
  else if(event.GetId() == EventIDs::menu_copy_as_rtf) {
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyRTF();
  }
  else if(event.GetId() == EventIDs::menu_copy_to_file) {
    wxString file = wxFileSelector(_("Save Selection to Image"), m_lastPath,
                                   wxS("image.png"), wxS("png"),
                                   _("PNG image (*.png)|*.png|"
                                     "JPEG image (*.jpg)|*.jpg|"
                                     "GIF image (*.gif)|*.gif|"
                                     "Scaleable vector graphics (*.svg)|*.svg|"
                                     "Windows bitmap (*.bmp)|*.bmp|") +
#ifdef wwxUSE_LIBWEB
                                     _("WebP (*.webp)|*.webp|") +
#endif
                                     _("Portable anymap (*.pnm)|*.pnm|"
                                     "Tagged image file format (*.tif)|*.tif|"
                                     "X pixmap (*.xpm)|*.xpm"),
                                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (file.Length()) {
      GetWorksheet()->CopyToFile(file);
      m_lastPath = wxPathOnly(file);
    }
  }
  else if(event.GetId() == EventIDs::menu_print_cellbrackets) {
    m_configuration.PrintBrackets(!m_configuration.PrintBrackets());
  }
  else if(event.GetId() == EventIDs::menu_show_cellbrackets) {
    m_configuration.ShowBrackets(!m_configuration.ShowBrackets());
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_delete) {
    if (GetWorksheet()->CanDeleteSelection()) {
      GetWorksheet()->DeleteSelection();
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
      return;
    }
  }
  else if(event.GetId() == wxID_ZOOM_IN) {
    GetWorksheet()->SetZoomFactor(m_configuration.GetZoomFactor() + 0.1);
  }
  else if(event.GetId() == wxID_ZOOM_OUT) {
    GetWorksheet()->SetZoomFactor(m_configuration.GetZoomFactor() - 0.1);
  }
  else if(event.GetId() == EventIDs::menu_zoom_80) {
    GetWorksheet()->SetZoomFactor(0.8);
  }
  else if(event.GetId() == wxID_ZOOM_100) {
    GetWorksheet()->SetZoomFactor(1.0);
  }
  else if(event.GetId() == EventIDs::menu_zoom_120) {
    GetWorksheet()->SetZoomFactor(1.2);
  }
  else if(event.GetId() == EventIDs::menu_zoom_150) {
    GetWorksheet()->SetZoomFactor(1.5);
  }
  else if(event.GetId() == EventIDs::menu_zoom_200) {
    GetWorksheet()->SetZoomFactor(2.0);
  }
  else if(event.GetId() == EventIDs::menu_zoom_300) {
    GetWorksheet()->SetZoomFactor(3.0);
  }
  else if(event.GetId() == EventIDs::menu_math_as_1D_ASCII) {
    MenuCommand(wxS("set_display('none)$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_2D_ASCII) {
    MenuCommand(wxS("set_display('ascii)$display2d_unicode:false$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_2D_UNICODE) {
    MenuCommand(wxS("set_display('ascii)$display2d_unicode:true$"));
  }
  else if(event.GetId() == EventIDs::menu_math_as_graphics) {
    MenuCommand(wxS("set_display('xml)$"));
  }
  else if(event.GetId() == EventIDs::internalRepresentation) {
    CommandWiz(_("Display expression in maxima's internal representation"),
               wxEmptyString, wxEmptyString,
               wxS("?print(#1#)$"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::wxMathML) {
    CommandWiz(_("Display expression in wxMaxima's internal representation"),
               wxEmptyString, wxEmptyString,
               wxS("printf(false,\"~m\", #1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_noAutosubscript) {
    MenuCommand(wxS("wxsubscripts: false$"));
  }
  else if(event.GetId() == EventIDs::menu_defaultAutosubscript) {
    MenuCommand(wxS("wxsubscripts: true$"));
  }
  else if(event.GetId() == EventIDs::menu_alwaysAutosubscript) {
    MenuCommand(wxS("wxsubscripts: 'all$"));
  }
  else if(event.GetId() == EventIDs::menu_autosubscriptIndividual) {
    CommandWiz(_("Autosubscript this variable"), wxEmptyString, wxEmptyString,
               wxS("wxdeclare_subscripted(#1#)$"), _("Variable name"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_noAutosubscriptIndividual) {
    CommandWiz(_("Never autosubscript this variable"), wxEmptyString,
               wxEmptyString, wxS("wxdeclare_subscripted(#1#,false)$"),
               _("Variable name"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_declareAutosubscript) {
    CommandWiz(_("Declare a text snippet to always be displayed as subscript"),
               wxEmptyString, wxEmptyString, wxS("wxdeclare_subscript(#1#)$"),
               _("Text snippet"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_roundedMatrixParens) {
    MenuCommand(wxS("lmxchar:\"(\"$rmxchar:\")\"$"));
  }
  else if(event.GetId() == EventIDs::menu_straightMatrixParens) {
    MenuCommand(wxS("lmxchar:\"|\"$rmxchar:\"|\"$"));
  }
  else if(event.GetId() == EventIDs::menu_angledMatrixParens) {
    MenuCommand(wxS("lmxchar:\"<\"$rmxchar:\">\"$"));
  }
  else if(event.GetId() == EventIDs::menu_squareMatrixParens) {
    MenuCommand(wxS("lmxchar:\"[\"$rmxchar:\"]\"$"));
  }
  else if(event.GetId() == EventIDs::menu_noMatrixParens) {
    MenuCommand(wxS("lmxchar:\" \"$rmxchar:\" \"$"));
  }
  else if(event.GetId() == EventIDs::menu_fullscreen) {
    ShowFullScreen(!IsFullScreen());
  }
  else if(event.GetId() == EventIDs::menu_invertWorksheetBackground) {
    m_configuration.InvertBackground(!m_configuration.InvertBackground());
    // Save the changed invert-status in the configuration.
    m_configuration.WriteStyles();
    m_viewMenu->Check(EventIDs::menu_invertWorksheetBackground,
                      m_configuration.InvertBackground());
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::menu_show_logwindow) {
    // FIXME: if the log window was closed as the parent 'disable' the toggle function, otherwise we risk a crash.
    if (MyApp::m_logWindow->GetFrame() != NULL) {
      MyApp::m_logWindow->Show(!MyApp::m_logWindow->GetFrame()->IsShown());
    };
  }

  else if(event.GetId() == ToolBar::tb_hideCode) {
    m_configuration.ShowCodeCells(!m_configuration.ShowCodeCells());
    GetWorksheet()->CodeCellVisibilityChanged();
  }
  else if(event.GetId() == EventIDs::menu_remove_output) {
    GetWorksheet()->RemoveAllOutput();
  }
  else if(event.GetId() == EventIDs::menu_pane_toolbar) {
    ShowToolBar(!ToolbarIsShown());
  }
  else if(event.GetId() == wxID_FIND) {
    wxLogMessage(_("A Ctrl-F event"));
    bool findDialogActiveWas = ((GetWorksheet()->m_findDialog != NULL) &&
                                (GetWorksheet()->m_findDialog->IsShown()));
    if (GetWorksheet()->m_findDialog == NULL)
      new FindReplaceDialog(this, &m_findData, _("Find and Replace"), &GetWorksheet()->m_findDialog);
    if (GetWorksheet()->GetActiveCell() != NULL) {
      wxString selected = GetWorksheet()->GetActiveCell()->GetSelectionString();

      // Start incremental search and highlighting of search results again.
      if(findDialogActiveWas)
        m_oldFindString.Clear();
      else
        m_oldFindString = selected;

      if (selected.Length() > 0)
        GetWorksheet()->m_findDialog->SetFindString(selected);
    }
    GetWorksheet()->m_findDialog->Show();
    GetWorksheet()->m_findDialog->Raise();
    GetWorksheet()->FocusFindDialogue();
    CallAfter([this]{GetWorksheet()->FocusFindDialogue();});
 }
  else if(event.GetId() == EventIDs::menu_history_next) {
    m_history->UpdateDeferred();
    wxString command = m_history->GetCommand(true);
    if (command != wxEmptyString)
      GetWorksheet()->SetActiveCellText(command);
  }
  else if(event.GetId() == EventIDs::menu_history_previous) {
    m_history->UpdateDeferred();
    wxString command = m_history->GetCommand(false);
    if (command != wxEmptyString)
      GetWorksheet()->SetActiveCellText(command);
  }
  else if(event.GetId() == EventIDs::popid_hide_tooltipMarkerForThisMessage) {
    const Cell *cell = GetWorksheet()->GetSelectionStart();
    if (cell == NULL)
      return;
    wxString toolTip = cell->GetLocalToolTip();
    if (toolTip.IsEmpty())
      toolTip = cell->GetGroup()->GetLocalToolTip();
    if (toolTip.IsEmpty())
      return;
    bool suppress = m_configuration.HideMarkerForThisMessage(toolTip);
    m_configuration.HideMarkerForThisMessage(toolTip, !suppress);
    GetWorksheet()->OutputChanged();
  }
  else if(event.GetId() == EventIDs::popid_hide_tooltipMarker) {
    if (GetWorksheet()->GetSelectionStart() == NULL)
      return;
    GroupCell *cell = GetWorksheet()->GetSelectionStart()->GetGroup();
    const GroupCell *end = NULL;
    if (GetWorksheet()->GetSelectionEnd() != NULL)
      end = GetWorksheet()->GetSelectionEnd()->GetGroup();
    bool marked = !cell->GetSuppressTooltipMarker();

    for (auto &tmp : OnList(cell)) {
      tmp.SetSuppressTooltipMarker(marked);
      if (&tmp == end)
        break;
    }
    GetWorksheet()->OutputChanged();
  }
  GetWorksheet()->RequestRedraw();
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::OnFind(wxFindDialogEvent &event) {
  if(!GetWorksheet())
    return;
  wxLogMessage(_("A find event, %s"), event.GetFindString());
  if(GetWorksheet()->m_findDialog)
    {
      if(!GetWorksheet()->m_findDialog->GetRegexSearch())
        {
          if (!GetWorksheet()->FindNext(event.GetFindString(),
                                     event.GetFlags() & wxFR_DOWN,
                                     !(event.GetFlags() & wxFR_MATCHCASE)
                                     ))
            LoggingMessageBox(_("No matches found!"));
        }
      else
        {
          if (!GetWorksheet()->FindNext_Regex(event.GetFindString(),
                                           !!(event.GetFlags() & wxFR_DOWN)))
            LoggingMessageBox(_("No matches found!"));
        }
      CallAfter([this]{GetWorksheet()->FocusFindDialogue();});
    }
  event.Skip();
}

void wxMaxima::OnReplace(wxFindDialogEvent &event) {
  event.Skip();
  if(!GetWorksheet())
    return;
  if(!GetWorksheet()->m_findDialog->GetRegexSearch())
    {
      GetWorksheet()->Replace(event.GetFindString(), event.GetReplaceString(),
                           !(event.GetFlags() & wxFR_MATCHCASE));

      if (!GetWorksheet()->FindNext(event.GetFindString(),
                                 event.GetFlags() & wxFR_DOWN,
                                 !(event.GetFlags() & wxFR_MATCHCASE)))
        LoggingMessageBox(_("No matches found!"));
      else
        GetWorksheet()->UpdateTableOfContents();
    }
  else
    {
      GetWorksheet()->Replace_RegEx(event.GetFindString(), event.GetReplaceString());

      if (!GetWorksheet()->FindNext_Regex(event.GetFindString(),
                                       event.GetFlags() & wxFR_DOWN))
        LoggingMessageBox(_("No matches found!"));
      else
        GetWorksheet()->UpdateTableOfContents();
    }
    CallAfter([this]{GetWorksheet()->FocusFindDialogue();});
}

void wxMaxima::OnReplaceAll(wxFindDialogEvent &event) {
  event.Skip();
  if(!GetWorksheet())
    return;
  long count;

  if(!GetWorksheet()->m_findDialog->GetRegexSearch())
    {
      count =
        GetWorksheet()->ReplaceAll(event.GetFindString(), event.GetReplaceString(),
                                !(event.GetFlags() & wxFR_MATCHCASE));
    }
  else
    count =
      GetWorksheet()->ReplaceAll_RegEx(event.GetFindString(), event.GetReplaceString());
  LoggingMessageBox(wxString::Format(_("Replaced %li occurrences."), count));
  if (count > 0)
    GetWorksheet()->UpdateTableOfContents();
}

void wxMaxima::OnSymbolAdd(wxCommandEvent &event) {
  if(m_symbolsSidebar == NULL)
    return;
  event.Skip();
  m_configuration.SymbolPaneAdditionalChars(
                                            m_configuration.SymbolPaneAdditionalChars() +
                                            wxString(wxChar(event.GetId())));
  m_symbolsSidebar->UpdateUserSymbols();
}

void wxMaxima::PropertiesMenu(wxCommandEvent &event) {
  event.Skip();
  if(!GetWorksheet())
    return;
  EditorCell *editor = GetWorksheet()->GetActiveCell();
  if (editor == NULL)
    return;
  wxString obj = editor->GetWordUnderCaret();
  if (obj.IsEmpty())
    obj = editor->GetSelectionString();

  if(event.GetId() == EventIDs::popid_property_real){
    MenuCommand(wxS("declare(") + obj + wxS(", real") + wxS(")$"));
  }
  else if(event.GetId() == EventIDs::popid_property_imaginary){
    MenuCommand(wxS("declare(") + obj + wxS(", imaginary)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_complex){
    MenuCommand(wxS("declare(") + obj + wxS(", complex)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_additive){
    MenuCommand(wxS("declare(") + obj + wxS(", additive)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_alphabetic){
    MenuCommand(wxS("declare(") + obj + wxS(", alphabetic)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_bindtest){
    MenuCommand(wxS("declare(") + obj + wxS(", bindtest)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_antisymmetric){
    MenuCommand(wxS("declare(") + obj + wxS(", antisymmetric)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_commutative){
    MenuCommand(wxS("declare(") + obj + wxS(", commutative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_symmetric){
    MenuCommand(wxS("declare(") + obj + wxS(", symmetric)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_constant){
    MenuCommand(wxS("declare(") + obj + wxS(", constant)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_even){
    MenuCommand(wxS("declare(") + obj + wxS(", even)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_odd){
    MenuCommand(wxS("declare(") + obj + wxS(", odd)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evenfun){
    MenuCommand(wxS("declare(") + obj + wxS(", evenfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_oddfun){
    MenuCommand(wxS("declare(") + obj + wxS(", oddfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_increasing){
    MenuCommand(wxS("declare(") + obj + wxS(", increasing)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_decreasing){
    MenuCommand(wxS("declare(") + obj + wxS(", decreasing)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_integer){
    MenuCommand(wxS("declare(") + obj + wxS(", integer)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_noninteger){
    MenuCommand(wxS("declare(") + obj + wxS(", noninteger)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_integervalued){
    MenuCommand(wxS("declare(") + obj + wxS(", integervalued)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_lassociative){
    MenuCommand(wxS("declare(") + obj + wxS(", lassociative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_rassociative){
    MenuCommand(wxS("declare(") + obj + wxS(", rassociative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_linear){
    MenuCommand(wxS("declare(") + obj + wxS(", linear)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_mainvar){
    MenuCommand(wxS("declare(") + obj + wxS(", mainvar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_multiplicative){
    MenuCommand(wxS("declare(") + obj + wxS(", multiplicative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nary){
    MenuCommand(wxS("declare(") + obj + wxS(", nary)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nonarray){
    MenuCommand(wxS("declare(") + obj + wxS(", nonarray)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_nonscalar){
    MenuCommand(wxS("declare(") + obj + wxS(", nonscalar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_scalar){
    MenuCommand(wxS("declare(") + obj + wxS(", scalar)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_noun){
    MenuCommand(wxS("declare(") + obj + wxS(", noun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_outative){
    MenuCommand(wxS("declare(") + obj + wxS(", outative)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_posfun){
    MenuCommand(wxS("declare(") + obj + wxS(", posfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_rational){
    MenuCommand(wxS("declare(") + obj + wxS(", rational)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_irrational){
    MenuCommand(wxS("declare(") + obj + wxS(", irrational)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evfun){
    MenuCommand(wxS("declare(") + obj + wxS(", evfun)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_evflag){
    MenuCommand(wxS("declare(") + obj + wxS(", evflag)$"));
  }
  else if(event.GetId() == EventIDs::popid_property_greaterThan){
    CommandWiz(_("Assume a value range for a variable"), wxEmptyString,
               wxEmptyString, wxS("assume(#1#)"), _("Variable"),
               obj + wxS(">0"), wxEmptyString);
  }
}

void wxMaxima::MaximaMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString b = wxS("\\");
  wxString f = wxS("/");
  if(event.GetId() == EventIDs::menu_jumptoerror){
    if (GetWorksheet()->GetErrorList().FirstError()) {
      GetWorksheet()->SetActiveCell(
                                 dynamic_cast<GroupCell *>(GetWorksheet()->GetErrorList().FirstError())
                                 ->GetEditable());
      dynamic_cast<GroupCell *>(GetWorksheet()->GetErrorList().FirstError())
        ->GetEditable()
        ->CaretToEnd();
    }
  }
  else if(event.GetId() == ToolBar::menu_restart_id){
    m_closing = true;
    GetWorksheet()->SetWorkingGroup(nullptr);
    GetWorksheet()->m_evaluationQueue.Clear();
    GetWorksheet()->ResetInputPrompts();
    m_unsuccessfulConnectionAttempts = 0;
    StartMaxima(true);
  }
  else if(event.GetId() == EventIDs::menu_soft_restart){
    MenuCommand(wxS("kill(all);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_dependencies){
    MenuCommand(wxS("kill(dependencies);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_values){
    MenuCommand(wxS("kill(values);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_functions){
    MenuCommand(wxS("kill(functions);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_arrays){
    MenuCommand(wxS("kill(arrays);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_myoptions){
    MenuCommand(wxS("kill(options);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_rules){
    MenuCommand(wxS("kill(rules);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_aliases){
    MenuCommand(wxS("kill(aliases);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_structures){
    MenuCommand(wxS("kill(structures);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_labels){
    MenuCommand(wxS("kill(labels);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_gradefs){
    MenuCommand(wxS("kill(gradefs);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_props){
    MenuCommand(wxS("kill(props);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_macros){
    MenuCommand(wxS("kill(macros);"));
  }
  else if(event.GetId() == EventIDs::menu_kill_let_rule_packages){
    MenuCommand(wxS("kill(let_rule_packages);"));
  }
  else if(event.GetId() == EventIDs::menu_garbage_collect){
    MenuCommand(wxS("garbage_collect();"));
  }
  else if(event.GetId() == EventIDs::menu_room){
    MenuCommand(wxS("?room();"));
  }
  else if(event.GetId() == EventIDs::menu_functions){
    MenuCommand(wxS("functions;"));
  }
  else if(event.GetId() == EventIDs::menu_variables){
    MenuCommand(wxS("values;"));
  }
  else if(event.GetId() == EventIDs::menu_arrays){
    MenuCommand(wxS("arrays;"));
  }
  else if(event.GetId() == EventIDs::menu_macros){
    MenuCommand(wxS("macros;"));
  }
  else if(event.GetId() == EventIDs::menu_labels){
    MenuCommand(wxS("labels;"));
  }
  else if(event.GetId() == EventIDs::menu_myoptions){
    MenuCommand(wxS("myoptions;"));
  }
  else if(event.GetId() == EventIDs::menu_rules){
    MenuCommand(wxS("rules;"));
  }
  else if(event.GetId() == EventIDs::menu_aliases){
    MenuCommand(wxS("aliases;"));
  }
  else if(event.GetId() == EventIDs::menu_structs){
    MenuCommand(wxS("structures;"));
  }
  else if(event.GetId() == EventIDs::menu_dependencies){
    MenuCommand(wxS("dependencies;"));
  }
  else if(event.GetId() == EventIDs::menu_gradefs){
    MenuCommand(wxS("gradefs;"));
  }
  else if(event.GetId() == EventIDs::menu_let_rule_packages){
    MenuCommand(wxS("let_rule_packages;"));
  }

  else if(event.GetId() == EventIDs::menu_display) {
    wxString choices[] = {wxS("xml"), wxS("ascii"), wxS("none")};
    wxString choice =
      wxGetSingleChoice(_("Select math display algorithm"),
                        _("Display algorithm"), 3, choices, this);
    if (choice.Length()) {
      wxString cmd = wxS("set_display('") + choice + wxS(")$");
      MenuCommand(cmd);
    }
  }
  else if(event.GetId() == EventIDs::menu_texform) {
    wxString cmd = wxS("tex(") + expr + wxS(")$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_grind) {
    wxString cmd = wxS("grind(") + expr + wxS(")$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_lisp) {
    wxString cmd = wxS("debugmode: lisp$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_all) {
    wxString cmd = wxS("debugmode: true$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_debugmode_off) {
    wxString cmd = wxS("debugmode: false$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_time) {
    wxString cmd;
    if (event.IsChecked())
      cmd = wxS("showtime:all$");
    else
      cmd = wxS("showtime:false$");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::gentran_lang_c){
    MenuCommand(wxS("gentranlang:c;"));
  }
  else if(event.GetId() == EventIDs::gentran_lang_fortran){
    MenuCommand(wxS("gentranlang:fortran;"));
  }
  else if(event.GetId() == EventIDs::gentran_lang_ratfor){
    MenuCommand(wxS("gentranlang:ratfor;"));
  }
  else if(event.GetId() == EventIDs::gentran_to_stdout){
    CommandWiz(_("Convert to programming language"), wxEmptyString,
               wxEmptyString, wxS("gentran(#1#);"), wxS("Expression(s)"),
               wxS("%"),
               _("Expression or a list of comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::gentran_to_file){
    CommandWiz(_("Convert to programming language file"), wxEmptyString,
               wxEmptyString, wxS("gentran(#1#,[#2#]);"), wxS("Expression(s)"),
               wxS("%"),
               _("Expression or a list of comma-separated expressions"),
               wxS("Filename(s)"), wxS("%"),
               _("Filename or a list of comma-separated file names"));
  }
  else if(event.GetId() == EventIDs::gentran_load){
    MenuCommand(wxS("load(\"gentran\")$"));
  }
  else if(event.GetId() == EventIDs::menu_fun_def){
    CommandWiz(_("Show the function's definition"), wxEmptyString,
               wxEmptyString, wxS("fundef(#1#);"), wxS("function"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_for){
    CommandWiz(_("For loop"), wxEmptyString, wxEmptyString,
               wxS("for #1#:#2# thru #3# step #4# do #5#;"),
               wxS("loop variable:"), wxS("i"), wxEmptyString, wxS("Start:"),
               wxS("1"), wxEmptyString, wxS("End:"), wxS("10"), wxEmptyString,
               wxS("Step width:"), wxS("1"), wxEmptyString, wxS("What to do:"),
               wxS("disp(i)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_while){
    CommandWiz(_("While loop"), wxEmptyString, wxEmptyString,
               wxS("while #1# do #2#;for #1#:#2# thru #3# step #4# do"),
               wxS("Condition:"), wxS("%"), wxEmptyString, wxS("What to do:"),
               wxS("disp(i)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_block){
    CommandWiz(_("Program block"), wxEmptyString, wxEmptyString,
               wxS("block([#1#], #2#);"), wxS("Local variable(s):"), wxS("i:0"),
               _("Comma-separated variable names, can be initialized by the "
                 "\":\" operator."),
               wxS("What to do:"), wxS("i:i+1,disp(i)"),
               _("Comma-separated commands"));
  }
  else if(event.GetId() == EventIDs::menu_block_noLocal){
    CommandWiz(
               _("Program (no local variables)"),
               _("If a program doesn't need local variables maxima allows "
                 "to put the commands between parenthesis. The result of the last "
                 "operation is the return value of the program."),
               wxEmptyString, wxS("(#1#);"), wxS("What to do:"), wxS("i:i+1,disp(i)"),
               _("Comma-separated commands"));
  }
  else if(event.GetId() == EventIDs::menu_local){
    CommandWiz(_("Declare a function local to a Program"),
               _("The command local() allows to tell maxima which functions to "
                 "make local to the current program when defined."),
               wxEmptyString, wxS("local(#1#);"), wxS("Function name:"), expr,
               _("Comma-separated function names"));
  }
  else if(event.GetId() == EventIDs::menu_return){
    CommandWiz(
               _("Return from a block or loop"),
               _("Unlike in other programming language return() only exits from the "
                 "current loop or block(), not from the whole function."),
               wxEmptyString, wxS("return(#1#);"), wxS("return value:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_trace){
    CommandWiz(_("Trace function(s)"), wxEmptyString, wxEmptyString,
               wxS("trace(#1#);"), wxS("Function(s):"), expr,
               _("Comma-separated function names."));
  }
  else if(event.GetId() == EventIDs::menu_lambda){
    CommandWiz(
               _("Lambda"),
               _("Lambda generates a function, but doesn't give it a name.\n"
                 "This is useful if you want to use a function only once, perhaps "
                 "as a parameter to another function and don't need it to be named.\n"
                 "Also you can fill a variable with a lambda() construct, effectively "
                 "generating a function pointer: A variable that can be used "
                 "as a function, and filled with a different function, if needed."),
               wxEmptyString, wxS("lambda([#1#],#2#);"),
               wxS("Names for the parameters:"), expr,
               _("Comma-separated names the parameters will be referenced by later."),
               wxS("Contents:"), expr, _("Comma-separated expressions."));
  }
  else if(event.GetId() == EventIDs::menu_quotequote){
    CommandWiz(
               _("Interpret maxima's output as input"),
               _("Sometimes one wants maxima to loose the information that a function "
                 "name was used with the ' operator in order to make maxima not "
                 "evaluate "
                 "it. In other places one wants % to mean \"the last expression at "
                 "the "
                 "time this function was created\", not \"the last expression now\".\n"
                 "In both cases the '' operator will do what is requested."),
               wxEmptyString, wxS("''#1#;"), wxS("Expression:"), expr,
               _("Expression whose output is to be used as maxima's input."),
               wxS("Contents:"), expr, _("Comma-separated expressions."));
  }
  else if(event.GetId() == EventIDs::menu_quote){
    CommandWiz(_("Don't evaluate one command"),
               _("Maxima automatically simplifies expressions it gets as input "
                 "and then tries to evaluate their value. The ' operator "
                 "tells maxima that we want a command to be in noun form, "
                 "which means: "
                 "stand here as is, and unevaluated.\n"
                 "The ' operator can be undone by using the '' operator."),
               wxEmptyString, wxS("'#1#;"), wxS("Command:"), expr,
               _("The name of a function we don't want to be evaluated here"));
  }
  else if(event.GetId() == EventIDs::menu_quoteblock){
    CommandWiz(
               _("Don't evaluate one whole expression"), wxEmptyString, wxEmptyString,
               wxS("'(#1#);"), _("expression:"), expr,
               _("The name of an expression that we don't want to be evaluated."));
  }
  else if(event.GetId() == EventIDs::menu_def_fun){
    CommandWiz(_("Define a function"), wxEmptyString, wxEmptyString,
               wxS("#1#(#2#):=#3#;"), _("Function name:"), expr, wxEmptyString,
               _("Parameter(s):"), wxS("x,[y]"),
               _("Comma-separated parameter names. A parameter in square "
                 "brackets [] will be filled with the list of any additional "
                 "arguments the function gets."),
               _("Function contents:"), wxS("sin(x)+lsum(i,i,y)"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gensym){
    MenuCommand("gensym();");
  }
  else if(event.GetId() == EventIDs::menu_def_macro){
    CommandWiz(_("Define a macro"), wxEmptyString, wxEmptyString,
               wxS("#1#(#2#)::=#3#;"), _("Macro name:"), expr, wxEmptyString,
               _("Parameter(s):"), wxS("x,[y]"),
               _("Comma-separated parameter names. A parameter in square "
                 "brackets [] will be filled with the list of any additional "
                 "arguments the function gets."),
               _("Macro contents:"), wxS("sin(x)+lsum(i,i,y)"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_def_variable){
    CommandWiz(_("Define a variable"), wxEmptyString, wxEmptyString,
               wxS("#1#:#2#;"), _("Variable name:"), expr, wxEmptyString,
               _("Contents:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_compile){
    CommandWiz(
               _("Compile a function"),
               _("Compiling a function can generate a considerable speed boost if "
                 "the types of the function parameters are made known to the function "
                 "before it is compiled. Else the generated code has to be "
                 "hideously generic."),
               wxEmptyString, wxS("compile(#1#);"), _("Function name(s):"), expr,
               _("Comma-separated function names"));
  }
  else if(event.GetId() == EventIDs::menu_paramType){
    CommandWiz(_("Declare the type of a function parameter"),
               _("If the type of a function parameter is known when compiling "
                 "a function "
                 "the code can vastly be optimized.\n"
                 "Known types:\n\n"
                 "array, boolean, integer, fixnum (machine-length integer), "
                 "float (machine-size floating-point numbers), "
                 "real or any (which is useful for declaring arrays of any)"),
               wxEmptyString, wxS("mode_declare(#1#);"), _("Parameter name:"),
               expr, wxEmptyString, _("Type:"), wxS("boolean"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_structdef){
    CommandWiz(_("Define a structure type"), wxEmptyString, wxEmptyString,
               wxS("defstruct(#1#(#2#));"), _("Struct type name:"), expr,
               _("The name of the new struct type"), _("Fields:"), wxS("U,I"),
               _("The comma-separated names of the struct fields"));
  }
  else if(event.GetId() == EventIDs::menu_structnew){
    CommandWiz(_("Define a structure"), wxEmptyString, wxEmptyString,
               wxS("new(#1#(#2#));"), _("Struct type name:"), expr,
               _("The name of the struct type"), _("Field contents:"),
               wxS("1,2"),
               _("The comma-separated contents of the struct fields"));
  }
  else if(event.GetId() == EventIDs::menu_structuse){
    CommandWiz(_("Read a structure field"), wxEmptyString, wxEmptyString,
               wxS("#1#@#2#;"), _("Struct :"), expr,
               _("The name of the struct"), _("Field name:"), wxS("U"),
               _("The name of the field to read"));
  }
  else if(event.GetId() == EventIDs::menu_saveLisp){
    CommandWiz(
               _("Save as lisp code"), wxEmptyString, wxEmptyString, wxS("save(#1#);"),
               _("filename:"), wxEmptyString, _("Elements:"), expr,
               _("Comma-separated names of the elements that shall be written"));
  }
  else if(event.GetId() == EventIDs::menu_loadLisp){
    CommandWiz(_("Load lisp code"), wxEmptyString, wxEmptyString,
               wxS("load(#1#);"), _("filename:"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_maximatostring){
    CommandWiz(
               _("Maxima to string"), wxEmptyString, wxEmptyString,
               wxS("sconcat(#1#);"), _("Expression(s):"), expr,
               _("Comma-separated expressions that shall be converted to a string"));
  }
  if(event.GetId() == EventIDs::menu_stringproc_setposition){
    CommandWiz(_("Seek to position"), wxEmptyString, wxEmptyString,
               wxS("fposition(#1#,#2#);"), _("Stream:"), expr, wxEmptyString,
               _("Position:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_getposition){
    CommandWiz(_("Get position in stream"), wxEmptyString, wxEmptyString,
               wxS("fposition(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_flush_output){
    CommandWiz(_("Flush stream"), wxEmptyString, wxEmptyString,
               wxS("flush_output(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_flength){
    CommandWiz(_("Stream length"), wxEmptyString, wxEmptyString,
               wxS("flength(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_close){
    CommandWiz(_("Close Stream"), wxEmptyString, wxEmptyString,
               wxS("close(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_opena){
    CommandWiz(_("Open for appending"), wxEmptyString, wxEmptyString,
               wxS("#1#:opena(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_openr){
    CommandWiz(_("Open for reading"), wxEmptyString, wxEmptyString,
               wxS("#1#:openr(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_openw){
    CommandWiz(_("Open for writing"), wxEmptyString, wxEmptyString,
               wxS("#1#:openw(#2#);"), _("Stream:"), expr, wxEmptyString,
               _("File name:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_printf){
    CommandWiz(
               _("printf"), wxEmptyString, wxEmptyString, wxS("printf(#1#,#2#,#3#);"),
               _("Stream:"), wxS("false"), wxEmptyString, _("Lisp format string:"),
               wxS("~a"),
               _("Lisp format strings are more powerful than c++ format strings"),
               _("Arguments:"), expr, _("Comma-separated arguments"));
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readline){
    CommandWiz(_("Read line"), wxEmptyString, wxEmptyString,
               wxS("readline(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readchar){
    CommandWiz(_("Read char"), wxEmptyString, wxEmptyString,
               wxS("readchar(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_readbyte){
    CommandWiz(_("Read byte"), wxEmptyString, wxEmptyString,
               wxS("readbyte(#1#);"), _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_writebyte){
    CommandWiz(_("Read byte"), wxEmptyString, wxEmptyString,
               wxS("writebyte(#1#,#2#);"), _("Byte:"), wxS("65"), wxEmptyString,
               _("Stream:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charp){
    CommandWiz(
               _("Is a char?"), _("Chars are strings that are one character long"),
               wxEmptyString, wxS("charp(#1#);"), _("Object:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_alphacharp){
    CommandWiz(_("Is an alphabetic char?"), wxEmptyString, wxEmptyString,
               wxS("alphacharp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_alphanumericp){
    CommandWiz(_("Is an alphanumeric char?"), wxEmptyString, wxEmptyString,
               wxS("alphanumericp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_digitcharp){
    CommandWiz(_("Is a digit?"), wxEmptyString, wxEmptyString,
               wxS("alphanumericp(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_constituent){
    CommandWiz(_("Is a printable char?"), wxEmptyString, wxEmptyString,
               wxS("constituent(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_uppercasep){
    CommandWiz(_("Is a uppercase char?"), wxEmptyString, wxEmptyString,
               wxS("uppercasep(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_lowercasep){
    CommandWiz(_("Is a lowercase char?"), wxEmptyString, wxEmptyString,
               wxS("lowercasep(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_create_ascii){
    CommandWiz(_("Ascii code to char"), wxEmptyString, wxEmptyString,
               wxS("ascii(#1#);"), _("Code number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cequal){
    CommandWiz(_("Are the chars equal?"), wxEmptyString, wxEmptyString,
               wxS("cequal(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cequalignore){
    CommandWiz(_("Are the chars equal, if case is ignored?"), wxEmptyString,
               wxEmptyString, wxS("cequalignore(#1#,#2#);"), _("Char #1:"),
               expr, wxEmptyString, _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_clessp){
    CommandWiz(_("Is Char 1 less than Char2?"), wxEmptyString, wxEmptyString,
               wxS("clessp(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_clesspignore){
    CommandWiz(_("Is Char 1 less than Char2, if case is ignored?"),
               wxEmptyString, wxEmptyString, wxS("clesspignore(#1#,#2#);"),
               _("Char #1:"), expr, wxEmptyString, _("Char #2:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cgreaterp){
    CommandWiz(_("Is Char 1 greater than Char2?"), wxEmptyString, wxEmptyString,
               wxS("cgreaterp(#1#,#2#);"), _("Char #1:"), expr, wxEmptyString,
               _("Char #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cgreaterpignore){
    CommandWiz(_("Is Char 1 greater than Char2, if case is ignored?"),
               wxEmptyString, wxEmptyString, wxS("cgreaterpignore(#1#,#2#);"),
               _("Char #1:"), expr, wxEmptyString, _("Char #2:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sequal){
    CommandWiz(_("Are these strings equal?"), wxEmptyString, wxEmptyString,
               wxS("sequal(#1#);"), _("String #1:"), expr, wxEmptyString,
               _("String #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sequalignore){
    CommandWiz(_("Are these strings equal if case is ignored?"), wxEmptyString,
               wxEmptyString, wxS("sequalignore(#1#);"), _("String #1:"), expr,
               wxEmptyString, _("String #2:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ascii){
    CommandWiz(_("Ascii code to char"), wxEmptyString, wxEmptyString,
               wxS("ascii(#1#);"), _("Code number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_cint){
    CommandWiz(_("Char to unicode code point"), wxEmptyString, wxEmptyString,
               wxS("cint(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_unicode){
    CommandWiz(_("Char to unicode code point"), wxEmptyString, wxEmptyString,
               wxS("cint(#1#);"), _("Char:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_unicode_to_utf8){
    CommandWiz(_("Unicode code point to char"), wxEmptyString, wxEmptyString,
               wxS("unicode(#1#);"), _("Codepoint number:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_utf8_to_unicode){
    CommandWiz(_("Unicode code point to utf8 numbers"), wxEmptyString,
               wxEmptyString, wxS("utf8_to_unicode(#1#);"),
               _("Codepoint number:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charat){
    CommandWiz(_("Extract the nth char of a string"), wxEmptyString,
               wxEmptyString, wxS("charat(#1#, #2#);"), _("String:"), expr,
               wxEmptyString, _("n:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_charlist){
    CommandWiz(_("String to list of char"), wxEmptyString, wxEmptyString,
               wxS("charlist(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_simplode){
    CommandWiz(_("List of char to String"), wxEmptyString, wxEmptyString,
               wxS("simplode(#1#);"), _("List of chars:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sinsert){
    CommandWiz(_("List of char to String"), wxEmptyString, wxEmptyString,
               wxS("simplode(#1#);"), _("New part:"), expr, wxEmptyString,
               _("String:"), wxEmptyString, wxEmptyString, _("Position:"),
               wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_eval_string){
    CommandWiz(_("Evaluate string"), wxEmptyString, wxEmptyString,
               wxS("eval_string(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_parse_string){
    CommandWiz(_("Parse string"), wxEmptyString, wxEmptyString,
               wxS("parse_string(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_scopy){
    CommandWiz(_("Copy string"),
               _("In order to save memory the : operator doesn't create an "
                 "individual copy of the string, but a clone that changes when "
                 "the original string changes."),
               wxEmptyString, wxS("scopy(#1#);"), _("String:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sdowncase){
    CommandWiz(_("Convert string to lowercase"), wxEmptyString, wxEmptyString,
               wxS("sdowncase(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_slength){
    CommandWiz(_("String length"), wxEmptyString, wxEmptyString,
               wxS("slength(#1#);"), _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_smake){
    CommandWiz(_("Create empty string"), wxEmptyString, wxEmptyString,
               wxS("smake(#1#,#2#);"), _("String:"), expr, wxEmptyString,
               _("Length:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_smismatch){
    CommandWiz(_("Find first difference"), wxEmptyString, wxEmptyString,
               wxS("smismatch(#1#,#2#);"), _("String #1:"), expr, wxEmptyString,
               _("String #2:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_split){
    CommandWiz(_("Split"), wxEmptyString, wxEmptyString, wxS("split(#1#,#2#);"),
               _("String:"), expr, wxEmptyString, _("Delimiter:"), wxS(";"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sposition){
    CommandWiz(_("Find char in string"), wxEmptyString, wxEmptyString,
               wxS("sposition(#1#,#2#);"), _("Char:"), wxS(";"), wxEmptyString,
               _("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sremove){
    CommandWiz(_("Remove all occurrences of part"), wxEmptyString,
               wxEmptyString, wxS("sremove(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_sremovefirst){
    CommandWiz(_("Remove first occurrence of part"), wxEmptyString,
               wxEmptyString, wxS("sremovefirst(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_tokens){
    CommandWiz(_("Split string into tokens"), wxEmptyString, wxEmptyString,
               wxS("tokens(#1#,#2#);"), _("String:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssearch){
    CommandWiz(_("Search first occurrence of part"), wxEmptyString,
               wxEmptyString, wxS("ssearch(#1#,#2#);"), _("part:"), expr,
               wxEmptyString, _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssort){
    CommandWiz(_("Sort all characters in string"), wxEmptyString, wxEmptyString,
               wxS("ssort(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_ssubstfirst){
    CommandWiz(_("Replace the first occurrence of Part"), wxEmptyString,
               wxEmptyString, wxS("ssubstfirst(#1#,#2#);"), _("Part:"),
               wxEmptyString, wxEmptyString, _("String:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_strim){
    CommandWiz(_("Trim string on both ends"), wxEmptyString, wxEmptyString,
               wxS("strim(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_striml){
    CommandWiz(_("Trim string left"), wxEmptyString, wxEmptyString,
               wxS("striml(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_strimr){
    CommandWiz(_("Trim string right"), wxEmptyString, wxEmptyString,
               wxS("strimr(#1#);"), _("String:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_number_to_octets){
    CommandWiz(_("Number to octets"), wxEmptyString, wxEmptyString,
               wxS("number_to_octets(#1#);"), _("Number:"), wxEmptyString,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_octets_to_number){
    CommandWiz(_("Octets to Number"), wxEmptyString, wxEmptyString,
               wxS("octets_to_number(#1#);"), _("Octets:"),
               _("Comma-separated numbers from 0 to 255"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_octets_to_string){
    CommandWiz(_("Octets to String"), wxEmptyString, wxEmptyString,
               wxS("octets_to_string(#1#);"), _("Octets:"),
               _("Comma-separated numbers from 0 to 255"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stringproc_string_to_octets){
    CommandWiz(_("String to octets"), wxEmptyString, wxEmptyString,
               wxS("string_to_octets(#1#);"), _("String:"), wxEmptyString,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_stringtomaxima){
    CommandWiz(_("Interpret string as maxima code"), wxEmptyString,
               wxEmptyString, wxS("parse_string(#1#);"), wxS("String:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_sregex_load){
    MenuCommand("load(\"sregex\");");
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_compile){
    CommandWiz(_("Compile regex"),
               _("Re-using a compiled regex is faster that using the same "
                 "regex string multiple times."),
               wxEmptyString, wxS("regex_compile(#1#);"), wxS("String:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_match_pos){
    CommandWiz(_("Regex match position"), wxEmptyString, wxEmptyString,
               wxS("regex_match_pos(#1#,#2#);"), wxS("Regex:"), expr,
               wxEmptyString, wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_match){
    CommandWiz(_("Regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_match(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  if(event.GetId() == EventIDs::menu_sregex_regex_split){
    CommandWiz(_("Split on regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_split(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_subst_first){
    CommandWiz(_("Replace first regex match"), wxEmptyString, wxEmptyString,
               wxS("regex_subst_first(#1#,#2#);"), wxS("Regex:"), expr,
               wxEmptyString, wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_regex_subst){
    CommandWiz(_("Replace all regex matches"), wxEmptyString, wxEmptyString,
               wxS("regex_subst(#1#,#2#);"), wxS("Regex:"), expr, wxEmptyString,
               wxS("String:"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_sregex_string_to_regex){
    CommandWiz(_("Convert string to matching regex"),
               _("Escapes all special characters in a string. The result is a "
                 "regex that matches this string exactly."),
               wxEmptyString, wxS("string_to_regex(#1#);"), wxS("String:"),
               expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_load){
    MenuCommand("load(\"operatingsystem\");");
  }

  else if(event.GetId() == EventIDs::menu_opsyst_chdir){
    CommandWiz(_("Change directory"), wxEmptyString, wxEmptyString,
               wxS("chdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_mkdir){
    CommandWiz(_("Create directory"), wxEmptyString, wxEmptyString,
               wxS("mkdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }
  else if(event.GetId() == EventIDs::menu_opsyst_rmdir){
    CommandWiz(_("Remove directory"), wxEmptyString, wxEmptyString,
               wxS("rmdir(#1#);"), wxS("Directory:"), expr,
               _("\"..\" means \"one directory up\"."));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_getcurrentdirectory){
    MenuCommand(wxS("getcurrentdirectory();"));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_copy_file){
    CommandWiz(_("Copy file"), wxEmptyString, wxEmptyString,
               wxS("copy_file(#1#,#2#);"), wxS("Source:"), expr, wxEmptyString,
               wxS("Destination:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_rename_file){
    CommandWiz(_("Rename file"), wxEmptyString, wxEmptyString,
               wxS("rename_file(#1#,#2#);"), wxS("Source:"), expr,
               wxEmptyString, wxS("Destination:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_delete_file){
    CommandWiz(_("Delete file"), wxEmptyString, wxEmptyString,
               wxS("delete_file(#1#);"), wxS("File:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_getenv){
    CommandWiz(_("Read environment variable"), wxEmptyString, wxEmptyString,
               wxS("getenv(#1#);"), wxS("Variable name:"), expr, wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_directory){
    CommandWiz(
               _("Read Directory"), wxEmptyString, wxEmptyString,
               wxS("dierectory(#1#);"), wxS("Directory name:"), expr,
               _("\".\" = \"The current directory\"\n\"..\" = \"One directory up\""));
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_directory){
    CommandWiz(_("Extract directory part"), wxEmptyString, wxEmptyString,
               wxS("pathname_directory(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_name){
    CommandWiz(_("Extract filename part"), wxEmptyString, wxEmptyString,
               wxS("pathname_name(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_opsyst_pathname_type){
    CommandWiz(_("Extract file type extension"), wxEmptyString, wxEmptyString,
               wxS("pathname_type(#1#);"), wxS("Path name:"), expr,
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_add_path) {
    if (m_lastPath.Length() == 0)
      m_lastPath = wxGetHomeDir();
    wxString dir = wxDirSelector(_("Add dir to path:"), m_lastPath);
    if (dir.Length()) {
      m_lastPath = dir;
#if defined(__WXMSW__)
      dir.Replace(wxS("\\"), wxS("/"));
#endif
      wxString cmd = wxS("file_search_maxima : cons(sconcat(\"") + dir +
        wxS("/###.{lisp,mac,mc}\"), file_search_maxima)$");
      MenuCommand(cmd);
    }
  }
  else if((event.GetId() == EventIDs::menu_evaluate_all_visible) ||
          (event.GetId() == ToolBar::tb_eval_all)) {
    GetWorksheet()->m_evaluationQueue.Clear();
    GetWorksheet()->ResetInputPrompts();
    EvaluationQueueLength(0);
    if (m_configuration.RestartOnReEvaluation())
      StartMaxima();
    GetWorksheet()->AddDocumentToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                          GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::menu_evaluate_all) {
    GetWorksheet()->m_evaluationQueue.Clear();
    GetWorksheet()->ResetInputPrompts();
    EvaluationQueueLength(0);
    if (m_configuration.RestartOnReEvaluation())
      StartMaxima();
    GetWorksheet()->AddEntireDocumentToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                          GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
  }
  else if(event.GetId() == ToolBar::tb_evaltillhere) {
    GetWorksheet()->m_evaluationQueue.Clear();
    GetWorksheet()->ResetInputPrompts();
    EvaluationQueueLength(0);
    if (m_configuration.RestartOnReEvaluation())
      StartMaxima();
    GetWorksheet()->AddDocumentTillHereToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                          GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::menu_clear_var){
    CommandWiz(_("Delete variable(s)"), wxEmptyString, wxEmptyString,
               wxS("remvalue(#1#);"), _("Variable name:"), wxS("all"),
               wxEmptyString);
  }
  if(event.GetId() == EventIDs::menu_kill){
    CommandWiz(_("Delete named object(s)"), wxEmptyString, wxEmptyString,
               wxS("kill(#1#);"), _("Object name:"), wxS("all"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_clear_fun){
    CommandWiz(_("Delete function(s)"), wxEmptyString, wxEmptyString,
               wxS("remfunction(#1#);"), _("Function name:"), wxS("all"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::button_subst) {
    wxWindowPtr<SubstituteWiz> wiz(new SubstituteWiz(this, -1, &m_configuration, _("Substitute")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
    });
  }
}

void wxMaxima::EquationsMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_allroots){
    CommandWiz(
               _("Solve polynomials numerically"),
               _("Tries to find all solutions of a polynomial numerically.\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    allroots(ratdisrep(taylor(expression,0,30)));"),
               wxEmptyString, wxS("allroots(#1#);"), _("Polynomial:"), expr);
  }
  else if(event.GetId() == EventIDs::menu_bfallroots){
    CommandWiz(
               _("Solve polynomials numerically (bfloats)"),
               _("Tries to find all solutions of a polynomial numerically using "
                 "bfloats.\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    bfallroots(ratdisrep(taylor(expression,0,30)));"),
               wxEmptyString, wxS("bfallroots(#1#);"), _("Polynomial:"), expr);
  }
  else if(event.GetId() == EventIDs::menu_realroots){
    CommandWiz(
               _("Solve polynomials numerically (real roots)"),
               _("Tries to find exact fractions that match the numerical solutions of "
                 "a polynomial.\n"
                 "Is not able to deal with solutions with a imaginary part. "
                 "Numerical constants like %pi% need to be eliminated using float() "
                 "or similar\n"
                 "Might be able to detect solutions in non-polynomials, as well, if "
                 "the expression is approximated by an polynomial, beforehand:\n\n"
                 "    realroots(ratdisrep(taylor(expression,0,30)));\n\n"
                 "See also guess_exact_value()"),
               wxEmptyString, wxS("realroots(#1#,#2#);"), _("Polynomial:"), expr,
               wxEmptyString, _("precision:"), wxS("1e-12"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_solve) ||
          (event.GetId() == EventIDs::menu_solve)){
    CommandWiz(_("Solve equation(s)"),
               _("solve() will solve a list of equations only if for n "
                 "independent equations there are n variables to solve to.\n"
                 "If only one result variable is of interest the other result "
                 "variables solve needs to do its work can be used to tell "
                 "solve() which variables to eliminate in the solution "
                 "for the interesting variable."),
               wxEmptyString, wxS("solve([#1#],[#2#]);"), _("Equation(s)"),
               expr, _("Comma-separated equations"), _("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_solve_to_poly){
    CommandWiz(
               _("Solve equations to polynom"),
               _(wxS("The function to_poly_solve tries to solve the equations "
                     "e for the variables l. The equation(s) e can either be a "
                     "single expression or a set or list of expressions; "
                     "similarly, l can either be a single symbol or a list of "
                     "set of symbols. When a member of e isn’t explicitly an "
                     "equation, for example x^2 -1, the solver assumes that the "
                     "expression vanishes.")),
               wxEmptyString, wxS("to_poly_solve([#1#],[#2#]);"), _("Equation(s)"),
               expr, _("Comma-separated equations"), _("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_solve_num) {
    if (expr.StartsWith(wxS("%")))
      expr = wxS("''(") + expr + wxS(")");
    CommandWiz(
      _("Solve equations numerically"),
      _("Tries to find a value of the variable that solves the equation between the two bonds"), wxEmptyString,
      wxT("find_root(#1#,#2#,#3#,#4#);"),
      _("Equation:"), expr, wxEmptyString,
      _("Variable:"), wxT("x"), wxEmptyString,
      _("Lower bound:"), wxT("-1"), wxEmptyString,
      _("Upper bound:"), wxT("1"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_solve_ode) ||
          (event.GetId() == EventIDs::menu_solve_ode)) {
    CommandWiz(_("Solve ODE"),
               _("solves an equation of the form\n    'diff(y,t) = -y;"),
               _("The solution of an ODE describes the general shape of the "
                 "resulting curve. The actual height of that curve is defined "
                 "by the initial condition or boundary values, later on."),
               wxS("ode2(#1#,#2#,#3#);"), _("Equation:"), expr, wxEmptyString,
               _("y:"), wxS("y"), wxEmptyString, _("t:"), wxS("t"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ivp_1){
    CommandWiz(_("Initial Condition"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s state is known at a point this "
                 "function fills in the correct values for the constants"),
               wxEmptyString, wxS("ic1(#1#,#2#,#3#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point the value is known at:"), wxS("t=0"), wxEmptyString,
               _("Value at that point:"), wxS("y=1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ivp_2){
    CommandWiz(_("Initial Condition"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s state is known at a point this "
                 "function fills in the correct values for the constants"),
               wxEmptyString, wxS("ic2(#1#,#2#,#3#,#4#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point the value is known at:"), wxS("t=0"), wxEmptyString,
               _("Value y at that point:"), wxS("y=1"), wxEmptyString,
               _("Derivate of y at that point:"), wxS("\'diff(y,t)=-1"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_bvp){
    CommandWiz(_("Boundary value problem"),
               _("The solution of an ODE tells the shape, but not the height "
                 "of the solution.\n"
                 "If the ODE\'s result is known at two points this "
                 "function fills in the correct values for the  constants"),
               wxEmptyString, wxS("bc2(#1#,#2#,#3#,#4#,#5#);"),
               _("Solution of the ODE:"), expr, wxEmptyString,
               _("Point #1 with known value:"), wxS("t=0"), wxEmptyString,
               _("Value y at that point:"), wxS("y=0"), wxEmptyString,
               _("Point #2 with known value:"), wxS("t=1"), wxEmptyString,
               _("Value y at that point:"), wxS("y=1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_rk){
    CommandWiz(
               _("Numerical solution for 1st degree ODE"),
               _("Tries to find a numerical solution for a 1st order ODE (or in other "
                 "words: a equation of the format depends(x,t);diff(x,t)=(something "
                 "containing x and t)"),
               wxEmptyString, wxS("rk(#1#,#2#,#3#,[#4#,#5#,#6#,#7#]);"),
               _("diff(x,t)="), expr,
               _("Accepts one expression or a list in the format [ode1,ode2,...]"),
               _("Name of x:"), wxS("x"),
               _("Accepts one variable or a list in the format [var1,var2,...]"),
               _("Initial x:"), wxS("1"),
               _("Accepts one variable or a list in the format [1,4,...]"),
               _("Name of t:"), wxS("t"), wxEmptyString, _("Start of t:"), wxS("0"),
               wxEmptyString, _("End of t:"), wxS("10"), wxEmptyString,
               _("Step width:"), wxS(".1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_eliminate){
    CommandWiz(_("Eliminate a variable"), wxEmptyString, wxEmptyString,
               wxS("eliminate([#1#],[#2#]);"), _("Equation(s):"), expr,
               wxEmptyString, _("Variable(s):"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_solve_algsys){
    GetTextFromUser(_("Number of equations:"), _("Solve algebraic system"),
                    &m_configuration, wxS("3"), this, [this](wxString sz) {
                      if (sz.Length() == 0)
                        return;
                      long isz;
                      if (!sz.ToLong(&isz) || isz <= 0) {
                        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                                          wxOK | wxICON_ERROR);
                        return;
                      }
                      wxWindowPtr<SysWiz> wiz(new SysWiz(this, -1, &m_configuration,
                                                         _("Solve algebraic system"), isz));
                      // wiz->Centre(wxBOTH);
                      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
                        if (retcode == wxID_OK) {
                          wxString cmd = wxS("algsys") + wiz->GetValue();
                          MenuCommand(cmd);
                        }
                      });
                    });
  }
  else if(event.GetId() == EventIDs::menu_solve_lin){
    GetTextFromUser(_("Number of equations:"), _("Solve linear system"),
                    &m_configuration, wxS("3"), this, [this](wxString sz) {
                      if (sz.Length() == 0)
                        return;
                      long isz;
                      if (!sz.ToLong(&isz) || isz <= 0) {
                        LoggingMessageBox(_("Not a valid number of equations!"), _("Error!"),
                                          wxOK | wxICON_ERROR);
                        return;
                      }
                      wxWindowPtr<SysWiz> wiz(new SysWiz(this, -1, &m_configuration, _("Solve linear system"), isz));
                      // wiz->Centre(wxBOTH);
                      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
                        if (retcode == wxID_OK) {
                          wxString cmd = wxS("linsolve") + wiz->GetValue();
                          MenuCommand(cmd);
                        }
                      });
                    });
  }
  else if(event.GetId() == EventIDs::menu_solve_de){
    CommandWiz(_("Solve differential equations using laplace()"),
               _("The solution variable needs to be in the form\n"
                 "   U(t)=1/2*U(t)+3*diff(U(t),t)\n"
                 "for this to work; Initial conditions can be specified using "
                 "atvalue()."),
               wxEmptyString, wxS("desolve([#1#],[#2#]);"), _("Equation(s):"),
               expr, wxEmptyString, _("Variable(s):"), wxEmptyString,
               wxEmptyString);
  }
  else if((event.GetId() == EventIDs::menu_atvalue) ||
          (event.GetId() == EventIDs::popid_property_atvalue)) {
    CommandWiz(_("Make a function value at a specific point known"),
               _("Tells maxima for an f(x), that f(x=t)=a"), wxEmptyString,
               wxS("atvalue(#1#,#2#,#3#);"), _("Function f(x):"), expr,
               wxEmptyString, _("Point:"), wxS("x=0"), wxEmptyString,
               _("Value:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_lhs) {
    wxString cmd = wxS("lhs(") + expr + wxS(");");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_rhs) {
    wxString cmd = wxS("rhs(") + expr + wxS(");");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_construct_fraction){
    CommandWiz(_("Construct a fraction"), wxEmptyString, wxEmptyString,
               wxS("((#1#)/(#2#))"), _("Enumerator:"), expr, wxEmptyString,
               _("Denominator:"), wxS("1"), wxEmptyString);
  }
}


void wxMaxima::MatrixMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_csv2mat){
    wxWindowPtr<CsvImportWiz> wiz(new CsvImportWiz(this, &m_configuration));
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd = wxS("read_matrix(\"") + wiz->GetFilename() + wxS("\", ") +
          wiz->GetSeparator() + wxS(");");
        MenuCommand(cmd);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_mat2csv){
    wxWindowPtr<CsvExportWiz> wiz(new CsvExportWiz(this, &m_configuration, _("Matrix")));
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd = wxS("write_data(") + wiz->GetMatrix() + wxS(", \"") +
          wiz->GetFilename() + wxS("\", ") + wiz->GetSeparator() + wxS(");");
        MenuCommand(cmd);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_matrix_row) {
    CommandWiz(_("Extract a matrix row"), wxEmptyString, wxEmptyString,
               wxS("row(#1#,#2#);"), _("Matrix:"), expr, wxEmptyString,
               _("Row number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_col){
    CommandWiz(_("Extract a matrix column"), wxEmptyString, wxEmptyString,
               wxS("col(#1#,#2#);"), _("Matrix:"), expr, wxEmptyString,
               _("Column number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_row_list){
    CommandWiz(_("Extract a matrix row as a list"), wxEmptyString,
               wxEmptyString, wxS("#1#[#2#];"), _("Matrix:"), expr,
               wxEmptyString, _("Row number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_col_list){
    CommandWiz(_("Extract a matrix column as a list"), wxEmptyString,
               wxEmptyString, wxS("transpose(#1#)[#2#];"), _("Matrix:"), expr,
               wxEmptyString, _("Row number:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_submatrix){
    CommandWiz(_("Remove rows and/or columns"), wxEmptyString, wxEmptyString,
               wxS("submatrix(#1,[#2#],[#3#]);"), _("Matrix:"), expr,
               wxEmptyString, _("Row numbers:"), wxEmptyString, wxEmptyString,
               _("Column numbers:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_multiply){
    CommandWiz(_("Multiply two matrices"), wxEmptyString, wxEmptyString,
               wxS("#1#.#2#;"), _("Left Matrix:"), expr, wxEmptyString,
               _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_exponent){
    CommandWiz(_("Matrix Exponent"), wxEmptyString, wxEmptyString,
               wxS("#1#^^#2#;"), _("Left Matrix:"), expr, wxEmptyString,
               _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_copymatrix){
    CommandWiz(
               _("Copy a matrix"),
               _("In order to save memory the \":\" operator does clone the matrix, "
                 "not copy it:\n"
                 "If you change an element of one matrix the same element will change "
                 "in all of its clones. copymatrix() instead generates a copy of a "
                 "matrix: "
                 "A new matrix that, if changed in any way, won't change the "
                 "original."),
               wxEmptyString, wxS("copymatrix(#1#);"), _("Matrix:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_hadamard_product){
    CommandWiz(_("Hadamard Product"),
               _("Element-by-element Product of matrices of the same size "
                 "(Hadamard product)"),
               wxEmptyString, wxS("#1#*#2#;"), _("Left Matrix:"), expr,
               wxEmptyString, _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_hadamard_exponent){
    CommandWiz(_("Hadamard exponent"),
               _("Element-by-element exponentiation of two matrices"),
               wxEmptyString, wxS("#1#^#2#;"), _("Left Matrix:"), expr,
               wxEmptyString, _("Right Matrix:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_loadLapack){
    MenuCommand(wxS("load(\"lapack\");"));
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeev_eigenvaluesOnly){
    CommandWiz(_("Calculate the eigenvalues of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("dgeev(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeev){
    CommandWiz(_("Calculate the eigenvalues and eigenvectors numerically"),
               wxEmptyString, wxEmptyString, wxS("dgeev(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zgeev_eigenvaluesOnly){
    CommandWiz(_("Calculate the eigenvalues of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("zgeev(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zgeev){
    CommandWiz(_("Calculate the eigenvalues and eigenvectors numerically"),
               wxEmptyString, wxEmptyString, wxS("zgeev(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgeqrf){
    CommandWiz(_("Numerical QR decomposition of a matrix"), wxEmptyString,
               wxEmptyString, wxS("dgeqrf(#1#)"), _("Matrix"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesv){
    CommandWiz(_("Solve A*x=b numerically"), wxEmptyString, wxEmptyString,
               wxS("dgesv(#1#,true,true)"), _("m×n Matrix A:"), expr,
               wxEmptyString, _("1×n Matrix b:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesvd){
    CommandWiz(_("Calculate Singular Value Decomposition, left and right "
                 "singular vectors numerically"),
               wxEmptyString, wxEmptyString, wxS("dgesvd(#1#,true,true)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dgesvd_valuesOnly){
    CommandWiz(
               _("Calculate Singular Value Decomposition of a matrix numerically"),
               wxEmptyString, wxEmptyString, wxS("dgesvd(#1#,false,false)[1]"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_max){
    CommandWiz(_("Find the maximum absolute value of a matrix entry"),
               wxEmptyString, wxEmptyString, wxS("dlange('max,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_one){
    CommandWiz(
               _("Find the maximum sum of the absolute values of a matrix column"),
               wxEmptyString, wxEmptyString, wxS("dlange('one_norm,#1#)"), _("Matrix"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_inf){
    CommandWiz(_("Find the maximum sum of the absolute values of a matrix row"),
               wxEmptyString, wxEmptyString, wxS("dlange('inf_norm,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_dlange_frobenius){
    CommandWiz(_("Calculate the root of the sum of squares of matrix entries"),
               wxEmptyString, wxEmptyString, wxS("dlange('frobenius,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_max){
    CommandWiz(_("Find the maximum absolute value of a matrix entry"),
               wxEmptyString, wxEmptyString, wxS("dlange('max,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_one){
    CommandWiz(
               _("Find the maximum sum of the absolute values of a matrix column"),
               wxEmptyString, wxEmptyString, wxS("dlange('one_norm,#1#)"), _("Matrix"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_inf){
    CommandWiz(_("Find the maximum sum of the absolute values of a matrix row"),
               wxEmptyString, wxEmptyString, wxS("dlange('inf_norm,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zlange_frobenius){
    CommandWiz(_("Calculate the root of the sum of squares of matrix entries"),
               wxEmptyString, wxEmptyString, wxS("dlange('frobenius,#1#)"),
               _("Matrix"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_matrix_zheev){
  }

  else if(event.GetId() == EventIDs::menu_invert_mat){
      wxString cmd = wxS("invert(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_determinant){
      wxString cmd = wxS("determinant(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rank){
      wxString cmd = wxS("rank(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_eigen){
      wxString cmd = wxS("eigenvalues(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_eigvect){
      wxString cmd = wxS("eigenvectors(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_adjoint_mat){
      wxString cmd = wxS("adjoint(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_transpose){
    wxString cmd = wxS("transpose(") + expr + wxS(");");
    MenuCommand(cmd);
  }
  else if(event.GetId() == EventIDs::menu_map_mat){
    wxWindowPtr<Gen3Wiz> wiz(new Gen3Wiz(_("Resulting Matrix name (may be empty):"), _("Function:"),
                                         _("Matrix:"), wxEmptyString, wxEmptyString, expr,
                                         &m_configuration, this, -1, _("Matrix map")));
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString cmd;
        if (wiz->GetValue1().IsEmpty())
          cmd = wiz->GetValue1() + wxS(":");
        cmd += wxS("matrixmap(") + wiz->GetValue2() + wxS(", ") +
          wiz->GetValue3() + wxS(");");
        MenuCommand(cmd);
      }
    });
  }
  else if((event.GetId() == EventIDs::menu_enter_mat) ||
          (event.GetId() == EventIDs::menu_stats_enterm)){
      wxWindowPtr<MatDim> wiz(new MatDim(this, -1, &m_configuration, _("Matrix")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd;
          if (wiz->GetValue0() != wxEmptyString)
            cmd = wiz->GetValue0() + wxS(": ");
          long w, h;
          int type = wiz->GetMatrixType();
          if (!(wiz->GetValue1()).ToLong(&h) || !(wiz->GetValue2()).ToLong(&w) ||
              w <= 0 || h <= 0) {
            LoggingMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                              wxOK | wxICON_ERROR);
            return; //-V773
          }
          if (w != h)
            type = MatWiz::MATRIX_GENERAL;
          wxWindowPtr<MatWiz> mwiz(new MatWiz(this, -1, &m_configuration, _("Enter matrix"),
                                              type, h, w));
          // wiz->Centre(wxBOTH);
          mwiz->ShowWindowModalThenDo([this, mwiz, cmd](int retcode) {
            if (retcode == wxID_OK) {
              MenuCommand(cmd + mwiz->GetValue());
            }
          });
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_cpoly){
    CommandWiz(_("Characteristic polynom"), wxEmptyString, wxEmptyString,
               wxS("expand(charpoly(#1#,#2#));"), _("Matrix"), expr,
               wxEmptyString, _("Variable"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_genmatrix){
    CommandWiz(
               _("Extract matrix from 2D array"),
               _("Extracts a rectangle from a 2D array and converts it to a matrix"),
               wxEmptyString, wxS("genmatrix(#1#,#2#,#3#,#4#,#5#);"), _("Array"), expr,
               wxEmptyString, _("Right end"), wxS("10"), wxEmptyString,
               _("Bottom end"), wxS("10"), wxEmptyString, _("Left end"), wxS("0"),
               wxEmptyString, _("Top end"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gen_mat_lambda){
    CommandWiz(
               _("Generate matrix from a rule"),
               _("Generates a matrix and fills each element with the "
                 "result of the expression \"Rule\"."),
               wxEmptyString,
               wxS("apply('matrix,makelist(makelist(#1#,#3#,1,#4#),#2#,1,#5#));"),
               _("Rule"), expr, wxEmptyString, _("Var #1"), wxS("i"), wxEmptyString,
               _("Var #2"), wxS("j"), wxEmptyString, _("Columns"), wxS("5"),
               wxEmptyString, _("Rows"), wxS("6"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_map) ||
          (event.GetId() == EventIDs::menu_map)){
    CommandWiz(_("Map"),
               _("Runs each element of an object (list, matrix, equation,...) "
                 "through a function individually"),
               wxEmptyString, wxS("map(#1#,#2#);"), _("function"), wxS("sin"),
               wxEmptyString, _("Object composed of elements"), wxS("expr"));
  }
  else if(event.GetId() == EventIDs::menu_map_lambda){
    CommandWiz(
               _("Map an expression"),
               _("Runs each element of an object (list, matrix, equation,...) "
                 "through an expression individually"),
               wxEmptyString, wxS("map(lambda([#2#],#1#),#3#);"), _("Expression"),
               wxS("sin(i)"), wxEmptyString, _("Loop variable"), wxS("i"),
               _("The name of the variable that shall contain the current element"),
               _("Object composed of elements"), wxS("expr"));
  }
}

void wxMaxima::AddDrawParameter(wxString cmd, int dimensionsOfNewDrawCommand) {
  if (!m_drawPane)
    return;
  if(!GetWorksheet())
    return;

  int dimensions = 0;
  dimensions = m_drawPane->GetDimensions();

  if (dimensions < 2) {
    if (dimensionsOfNewDrawCommand < 3)
      cmd = wxS("wxdraw2d(\n    ") + cmd + wxS("\n)$");
    else
      cmd = wxS("wxdraw3d(\n    ") + cmd + wxS("\n)$");
    GetWorksheet()->OpenHCaret(cmd);
    GetWorksheet()->GetActiveCell()->SetCaretPosition(
                                                   GetWorksheet()->GetActiveCell()->GetCaretPosition() - 3);
  } else {
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->AddDrawParameter(std::move(cmd));
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::DrawMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  if (!m_drawPane)
    return;

  UpdateDrawPane();
  int dimensions = m_drawPane->GetDimensions();

  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr;
  if (dimensions < 2)
    expr = GetDefaultEntry();
  else
    expr = "%";

  if(event.GetId() == EventIDs::menu_draw_2d){
      wxWindowPtr<DrawWiz> wiz(new DrawWiz(this, &m_configuration, 2));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          GetWorksheet()->OpenHCaret(wiz->GetValue());
          GetWorksheet()->GetActiveCell()->SetCaretPosition(
                                                         GetWorksheet()->GetActiveCell()->GetCaretPosition() - 3);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_3d){
    if (dimensions < 2) {
      wxWindowPtr<DrawWiz> wiz(new DrawWiz(this, &m_configuration, 3));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          GetWorksheet()->OpenHCaret(wiz->GetValue());
          GetWorksheet()->GetActiveCell()->SetCaretPosition(
                                                         GetWorksheet()->GetActiveCell()->GetCaretPosition() - 3);
        }
      });
    } else {
      wxWindowPtr<Wiz3D> wiz(new Wiz3D(this, &m_configuration));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue());
      });
    }
  }
  else if(event.GetId() == EventIDs::menu_draw_fgcolor){
      wxColour col = wxGetColourFromUser(this);
      if (col.IsOk())
        AddDrawParameter(wxString::Format("color=\"#%02x%02x%02x\"", col.Red(),
                                          col.Green(), col.Blue()));
  }
  else if(event.GetId() == EventIDs::menu_draw_fillcolor){
      wxColour col = wxGetColourFromUser(this);
      if (col.IsOk())
        AddDrawParameter(wxString::Format("fill_color=\"#%02x%02x%02x\"",
                                          col.Red(), col.Green(), col.Blue()));
  }
  else if(event.GetId() == EventIDs::menu_draw_title){
      wxWindowPtr<Gen1Wiz> wiz(new Gen1Wiz(
                                           this, -1, &m_configuration, _("Set the diagram title"),
                                           _("Title (Sub- and superscripts as x_{10} or x^{10})"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("title=\"") + wiz->GetValue() + wxS("\"");
          AddDrawParameter(std::move(cmd));
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_key){
      wxWindowPtr<Gen1Wiz> wiz(new Gen1Wiz(
                                           this, -1, &m_configuration,
                                           _("Set the next plot's title. Empty = no title."),
                                           _("Title (Sub- and superscripts as x_{10} or x^{10})"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("key=\"") + wiz->GetValue() + wxS("\"");
          AddDrawParameter(std::move(cmd));
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_draw_explicit){
      wxWindowPtr<ExplicitWiz> wiz(new ExplicitWiz(this, &m_configuration, expr, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_implicit){
      wxWindowPtr<ImplicitWiz> wiz(new ImplicitWiz(this, &m_configuration, expr, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_parametric){
      wxWindowPtr<ParametricWiz> wiz(new ParametricWiz(this, &m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_points){
      wxWindowPtr<WizPoints> wiz(new WizPoints(this, &m_configuration, dimensions, expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue());
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_grid){
      wxWindowPtr<Gen2Wiz> wiz(new Gen2Wiz(
                                           _("x direction [in multiples of the tick frequency]"),
                                           _("y direction [in multiples of the tick frequency]"), "1", "1",
                                           &m_configuration, this, -1, _("Set the grid density.")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd =
            wxS("grid=[") + wiz->GetValue1() + "," + wiz->GetValue2() + wxS("]");
          AddDrawParameter(std::move(cmd));
        }
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_axis){
      wxWindowPtr<AxisWiz> wiz(new AxisWiz(this, &m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          AddDrawParameter(wiz->GetValue());
        }
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_contour){
      wxWindowPtr<WizContour> wiz(new WizContour(this, &m_configuration));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue(), 3);
      });
  }

  else if(event.GetId() == EventIDs::menu_draw_accuracy){
      wxWindowPtr<WizDrawAccuracy> wiz(new WizDrawAccuracy(this, &m_configuration, dimensions));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz, dimensions](int retcode) {
        if (retcode == wxID_OK)
          AddDrawParameter(wiz->GetValue(), dimensions);
      });
  }
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::ListMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_csv2list){
      wxWindowPtr<CsvImportWiz> wiz(new CsvImportWiz(this, &m_configuration));
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("read_nested_list(\"") + wiz->GetFilename() + wxS("\", ") +
            wiz->GetSeparator() + wxS(");");
          MenuCommand(cmd);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_list2csv){
      wxWindowPtr<CsvExportWiz> wiz(new CsvExportWiz(this, &m_configuration, _("List")));
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString cmd = wxS("write_data(") + wiz->GetMatrix() + wxS(", \"") +
            wiz->GetFilename() + wxS("\", ") + wiz->GetSeparator() + wxS(");");
          MenuCommand(cmd);
        }
      });
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_args){
    CommandWiz(_("Extract function arguments"), wxEmptyString, wxEmptyString,
               wxS("args(#1#)$"),
               _("The function call whose arguments to extract"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_list2matrix){
    MenuCommand(wxS("apply('matrix,") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_matrix2list){
    MenuCommand(wxS("args(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_elements){
    CommandWiz(_("Create list from comma-separated elements"), wxEmptyString,
               wxEmptyString, wxS("[#1#]"), _("Comma-separated elements"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_create_from_rule){
    CommandWiz(
               _("Create a list from a rule"), wxEmptyString, wxEmptyString,
               wxS("makelist(#1#,#2#,#3#,#4#,#5#)$"), _("Rule:"), expr,
               _("The rule that explains how to generate the value of a list item.\n"
                 "Might be something like \"i\", \"i^2\" or \"sin(i)\""),
               _("Index variable:"), wxS("i"),
               _("The number of the item which is stepped from \"Index Start\" to "
                 "\"Index End\"."),
               _("Index Start:"), wxS("1"), wxEmptyString, _("Index End:"), wxS("100"),
               wxEmptyString, _("Index Step:"), wxS("1"), wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_list_create_from_list){
    CommandWiz(
               _("Create a list from a rule"), wxEmptyString, wxEmptyString,
               wxS("makelist(#1#,#2#,#3#)$"), _("Rule:"), expr,
               _("The rule that explains how to generate the value of a list item.\n"
                 "Might be something like \"i\", \"i^2\" or \"sin(i)\""),
               _("Index variable:"), wxS("i"),
               _("The variable the value of the current source item is stored in."),
               _("Source list:"), wxS("[1,8,32]"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_actual_values_storage){
      wxWindowPtr<ActualValuesStorageWiz> wiz(new ActualValuesStorageWiz(
                                                                         &m_configuration, this, -1,
                                                                         _("Create a list as a storage for the values of variables")));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          MenuCommand(wiz->GetValue());
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_list_sort){
      wxWindowPtr<ListSortWiz> wiz(new ListSortWiz(&m_configuration, this, -1, _("Sort a list"), expr));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          MenuCommand(wiz->GetValue());
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_list_length){
    MenuCommand(wxS("length(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_push){
    CommandWiz(_("Push an element to a list"), wxEmptyString, wxEmptyString,
               wxS("push(#1#,#2#);"), _("List:"), expr, wxEmptyString,
               _("Element:"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_pop){
    MenuCommand(wxS("pop(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_reverse){
    MenuCommand(wxS("reverse(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_first){
    MenuCommand(wxS("first(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_last){
    MenuCommand(wxS("last(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_rest){
    CommandWiz(_("Drop the first n list elements"),
               _("Return the list without its first n elements"), wxEmptyString,
               wxS("rest(#1#,#2#);"), _("List:"), expr, wxEmptyString, _("n:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_restN){
    CommandWiz(_("Drop the last n list elements"),
               _("Return the list without its last n elements"), wxEmptyString,
               wxS("rest(#1#,-#2#);"), _("List:"), expr, wxEmptyString, _("n:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_lastn){
    CommandWiz(_("Drop the last n list elements"),
               _("Extract the last n elements from a list"), wxEmptyString,
               wxS("rest(#1#,#2#);"), _("List"), expr, wxEmptyString,
               _("Number of elements"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_nth){
    CommandWiz(
               _("Extract the nth list elements"),
               _("Attention: Extracting a random list element isn't efficient for "
                 "long lists."
                 "Iterating over lists using makelist() or for loops is way faster."),
               wxEmptyString, wxS("#1#[#2];"), _("List"), expr, wxEmptyString,
               _("Element number"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_map){
    CommandWiz(_("Apply a function to each list element"), wxEmptyString,
               wxEmptyString, wxS("map(#1#,#2);"), _("function"), expr,
               wxEmptyString, _("list"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_use_actual_values){
    CommandWiz(_("Introduce a list of actual values into an equation"),
               wxEmptyString, wxEmptyString, wxS("subst(#1#,#2#);"),
               _("List with values"), wxEmptyString,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Equation"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_extract_value){
    CommandWiz(_("Extract a variable's value from a list of variable values"),
               wxEmptyString, wxEmptyString, wxS("subst(#1#,#2#);"), _("List"),
               expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Variable name"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_as_function_arguments){
    CommandWiz(_("Use a list as parameter list for a function"), wxEmptyString,
               wxEmptyString, wxS("apply(#1#,#2#);"), _("Function name"), expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_do_for_each_element){
    CommandWiz(
               _("Do for each list element"), wxEmptyString, wxEmptyString,
               wxS("for #2# om #1# do #3#;"), _("List:"), expr,
               _("Comma-separated list entry in the format val1=1,val2=2"),
               _("Iterator name:"), wxS("i"),
               _("The variable the value of the current source item is stored in."),
               _("What to do:"), wxS("disp(i)"),
               _("Either a single expression or a comma-separated list of expressions "
                 "between parenthesis. In the latter case the result of the last "
                 "expression in the parenthesis is used."));
  }
  else if(event.GetId() == EventIDs::menu_list_remove_duplicates){
    MenuCommand(wxS("unique(") + expr + wxS(")"));
  }
  else if(event.GetId() == EventIDs::menu_list_remove_element){
    CommandWiz(_("Remove an element from a list"), wxEmptyString, wxEmptyString,
               wxS("delete(#1#,#2#);"), _("Element"), expr, wxEmptyString,
               _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_item_start){
    CommandWiz(_("Add an element to the start of a list"), wxEmptyString,
               wxEmptyString, wxS("cons(#1#,#2#);"), _("Item"), expr,
               wxEmptyString, _("List"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_item_end){
    CommandWiz(_("Add an element to the end of a list"), wxEmptyString,
               wxEmptyString, wxS("append(#1#,[#2#]);"), _("List"), expr,
               wxEmptyString, _("Item"), wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_append_list){
    CommandWiz(_("Append a list to another list"), wxEmptyString, wxEmptyString,
               wxS("append(#1#,#2#);"), _("List #1"), expr, wxEmptyString,
               _("List #2"), wxS("[1]"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_list_interleave){
    CommandWiz(_("Interleave two lists"), wxEmptyString, wxEmptyString,
               wxS("join(#1#,#2#);"), _("List #1"), expr, wxEmptyString,
               _("List #2"), wxEmptyString, wxEmptyString);
  }
}

void wxMaxima::SimplifyMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_nouns){
    CommandWiz(
               _("Evaluate Nouns"),
               _("Maxima allows to make functions \"nouns\", which means that they "
                 "aren't automatically evaluated as soon as maxima encounters them.\n"
                 "Ways make a function a noun include declaring it a noun, preceding "
                 "it with a  \' or putting it between the parenthesis of \'().\n\n"
                 "This command tells maxima that the nouns in this expression "
                 "shall now be evaluated, too."),
               wxEmptyString, wxS("#1#,nouns;"), _("Expression"), expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_simpsum){
    CommandWiz(_("Simplify sums"),
               _("Try to simplify sums that result from sum() commands."),
               wxEmptyString, wxS("simpsum(#1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_ratsimp) ||
          (event.GetId() == EventIDs::menu_ratsimp)){
      wxString cmd = wxS("ratsimp(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_radcan) ||
          (event.GetId() == EventIDs::menu_radsimp)) {
    CommandWiz(
               _("Simplify radicals"),
               _("radcan() is a powerful tools for simplification trigonometric "
                 "functions "
                 "but needs to be taken with care: If a function has more than one "
                 "branch "
                 "radcan uses the one that looks like it would fit best, not "
                 "necessarily "
                 "the one that makes sense for the problem that resulted in the "
                 "Expression that is to be simplified."),
               wxEmptyString, wxS("radcan(#1#);"), _("Expression"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_to_fact){
      wxString cmd = wxS("makefact(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_gamma){
      wxString cmd = wxS("makegamma(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_factcomb){
      wxString cmd = wxS("factcomb(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_factsimp){
      wxString cmd = wxS("minfactorial(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logcontract){
      wxString cmd = wxS("logcontract(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logexpand){
      wxString cmd = expr + wxS(", logexpand=super;");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_logexpand_false){
    MenuCommand(wxS("logexpand:false$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_true){
    MenuCommand(wxS("logexpand:true$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_all){
    MenuCommand(wxS("logexpand:all$"));
  }
  else if(event.GetId() == EventIDs::menu_logexpand_super){
    MenuCommand(wxS("logexpand:super$"));
  }
  else if((event.GetId() == EventIDs::button_expand) ||
          (event.GetId() == EventIDs::menu_expand)){
      wxString cmd = wxS("expand(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_scsimp){
      wxString cmd = wxS("scsimp(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_xthru){
      wxString cmd = wxS("xthru(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_factor) ||
          (event.GetId() == EventIDs::menu_factor)){
      wxString cmd = wxS("factor(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_expandwrt){
    CommandWiz(_("Expand for variable(s):"), wxEmptyString, wxEmptyString,
               wxS("expandwrt(#1#,#2#);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Variable(s)"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_subst){
    CommandWiz(
               _("Substitute"),
               _("Subst is a better string-search-and-replace for expressions."),
               wxEmptyString, wxS("subst(#2#,#1#);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_ratsubst){
    CommandWiz(_("Smart substitution"),
               _("ratsubst works like subst, but it knows some basic maths, if "
                 "needed."),
               wxEmptyString, wxS("ratsubst(#2#,#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_psubst){
    CommandWiz(_("Parallel substitution"),
               _("Substitutes, but makes sure that nothing is substituted into "
                 "the other substituents."),
               wxEmptyString, wxS("ratsubst(#2#,#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u,u=x^2"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_fullratsubst){
    CommandWiz(_("Recursive substitution"),
               _("Substitutes up to lrats_max_iter times, or until the "
                 "expression stops changing when substituting."),
               wxEmptyString, wxS("fullratsubst(#2#,#1#);"), wxS("Expression"),
               wxS("%"), wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_at){
    CommandWiz(
               _("Value at a given point"),
               _("Substitutes, but makes sure that if substituting t=0 in diff(x,t) "
                 "the result isn't 0 (as t no more changes), but %at(diff(x,t),t=0)."),
               wxEmptyString, wxS("at(#1#,#2#);"), wxS("Expression"), wxS("%"),
               wxEmptyString, wxS("Substituents"), wxS("x^2=u"),
               _("Comma-separated expressions"));
  }
  else if(event.GetId() == EventIDs::menu_substinpart){
    CommandWiz(_("Substitute only in specific parts"),
               _("Substitutes, but only in the n_1th, n_2th and so on term of "
                 "the equation."),
               wxEmptyString, wxS("substinpart(#2#,#1#,#3#);"),
               wxS("Expression"), wxS("%"), wxEmptyString, wxS("Substituents"),
               wxS("x^2=u,u=x^2"), _("Comma-separated expressions"),
               wxS("Term numbers"), wxS("x^2=u,u=x^2"),
               _("Comma-separated numbers of the terms to substitute in"));
  }
  else if(event.GetId() == EventIDs::menu_opsubst){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("opsubst:true$");
      else
        cmd = wxS("opsubst:false$");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_expandwrt_denom){
    CommandWiz(_("Expand for variable(s) including denominator:"),
               wxEmptyString, wxEmptyString,
               wxS("expandwrt(#1#,#2#),expandwrt_denom=true;"),
               wxS("Expression"), wxS("%"), wxEmptyString, wxS("Variable(s)"),
               wxS("x"), _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::menu_horner){
      wxString cmd = wxS("horner(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_collapse){
      wxString cmd = wxS("collapse(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_optimize){
      wxString cmd = wxS("optimize(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_mainvar){
    CommandWiz(_("Declare main variable:"), wxEmptyString, wxEmptyString,
               wxS("declare(#1#,mainvar);"), wxS("Variable"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_scanmapfactor){
      wxString cmd = wxS("scanmap('factor,") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_gfactor){
      wxString cmd = wxS("gfactor(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigreduce) ||
          (event.GetId() == EventIDs::menu_trigreduce)){
      wxString cmd = wxS("trigreduce(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigsimp) ||
          (event.GetId() == EventIDs::menu_trigsimp)){
      wxString cmd = wxS("trigsimp(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_trigexpand) ||
          (event.GetId() == EventIDs::menu_trigexpand)){
      wxString cmd = wxS("trigexpand(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::menu_trigrat) ||
          (event.GetId() == EventIDs::button_trigrat)){
      wxString cmd = wxS("trigrat(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if((event.GetId() == EventIDs::button_rectform) ||
          (event.GetId() == EventIDs::menu_rectform)){
      wxString cmd = wxS("rectform(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_polarform){
      wxString cmd = wxS("polarform(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_demoivre){
      wxString cmd = wxS("demoivre(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_exponentialize){
      wxString cmd = wxS("exponentialize(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_realpart){
      wxString cmd = wxS("realpart(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_imagpart){
      wxString cmd = wxS("imagpart(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_talg){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("algebraic:true$");
      else
        cmd = wxS("algebraic:false$");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_tellrat){
    CommandWiz(_("Enter an equation for rational simplification:"),
               wxEmptyString, wxEmptyString, wxS("tellrat(#1#);"),
               wxS("Equation"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_modulus){
    CommandWiz(_("Calculate modulus:"), wxEmptyString, wxEmptyString,
               wxS("modulus : #1#$"), wxS("Modulus"), wxS("%"), wxEmptyString);
  }
}


void wxMaxima::CalculusMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if(event.GetId() == EventIDs::menu_change_var){
    CommandWiz(
               _("Change variable"),
               wxS("Takes an integral or sum in respect to the Old Variable and "
                   "replaces "
                   "that variable by a new one both adjusting the integrand and the "
                   "limits "
                   "accordingly. The field \"Equation\" specifies how the Old "
                   "Variable and "
                   "the New Variable are related to each other; For sums changevar "
                   "isn't "
                   "intelligent enough to do more than a shift of the variable.\n\n"
                   "changevar(integrate(f(x),x,1,10),u=sqrt(x),u,x);\n"
                   "results in\n"
                   "2*integrate(u*f(u^2),u,1,sqrt(10))\n\n"),
               wxEmptyString, wxS("changevar(#1#,#2#,#3#,#4#);"), _("Integral/Sum:"),
               expr, wxEmptyString, _("Equation:"), wxS("u=sqrt(x)"), wxEmptyString,
               _("New variable:"), wxS("u"), wxEmptyString, _("Old variable:"),
               wxS("x"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_change_var_evaluate){
    CommandWiz(
               _("Change variable and evaluate"),
               wxS("Takes an integral or sum in respect to the Old Variable and "
                   "replaces "
                   "that variable by a new one both adjusting the integrand and the "
                   "limits "
                   "accordingly. Then evaluates the resulting integral or sum. "
                   "The field \"Equation\" specifies how the Old Variable and "
                   "the New Variable are related to each other; For sums changevar "
                   "isn't "
                   "intelligent enough to do more than a shift of the variable.\n\n"
                   "changevar(integrate(f(x),x,1,10),u=sqrt(x),u,x);\n"
                   "results in\n"
                   "2*integrate(u*f(u^2),u,1,sqrt(10))\n\n"),
               wxEmptyString, wxS("changevar(#1#,#2#,#3#,#4#),nouns;"),
               _("Integral/Sum:"), expr, wxEmptyString, _("Equation:"),
               wxS("u=sqrt(x)"), wxEmptyString, _("New variable:"), wxS("u"),
               wxEmptyString, _("Old variable:"), wxS("x"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_pade){
    CommandWiz(_("Pade approximation"), wxEmptyString, wxEmptyString,
               wxS("pade(#1#,#2#,#3#);"), _("Taylor series:"), expr,
               wxEmptyString, _("Num. deg:"), wxS("4"), wxEmptyString,
               _("Denom. deg:"), wxS("4"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_taylor){
    CommandWiz(_("Taylor series"),
               _("Approximates a expression around a point as a polynom\n"
                 "The trailing \"...\" can be removed by using ratdisrep()"),
               wxEmptyString, wxS("taylor(#1#,#2#,#3#,#4#);"), _("Expression:"),
               expr, wxEmptyString, _("Variable:"), wxS("x"), wxEmptyString,
               _("Point:"), wxS("0"), wxEmptyString, _("Degree:"), wxS("3"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_powerseries){
    CommandWiz(_("Power series"), _("Approximates a expression as a polynom"),
               wxEmptyString, wxS("niceindices(powerseries(#1#,#2#,#3#);"),
               _("Expression:"), expr, wxEmptyString, _("Variable:"), wxS("x"),
               wxEmptyString, _("point:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_fourier){
      wxString loadCmd;
      if (!m_fourierLoaded)
        loadCmd = wxS("load(\"fourie\")$\n");
      CommandWiz(_("Fourier coefficients"),
                 _("Calculates the fourier coefficients for the expression from "
                   "-p to p"),
                 wxEmptyString, loadCmd + wxS("fourier(#1#,#2#,#3#);"),
                 _("Expression:"), expr, wxEmptyString, _("Variable:"), wxS("x"),
                 wxEmptyString, _("Range radius:"), wxS("2"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_continued_fraction){
      wxString cmd = wxS("cfdisrep(cf(") + expr + wxS("));");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_lcm){
    CommandWiz(_("LCM"), wxEmptyString, wxEmptyString, wxS("lcm(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_gcd){
    CommandWiz(_("GCD"), wxEmptyString, wxEmptyString, wxS("gcd(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_divide){
    CommandWiz(_("GCD"), wxEmptyString, wxEmptyString, wxS("divide(#1#,#2#);"),
               _("Polynomial 1:"), expr, wxEmptyString, _("Polynomial 2:"),
               wxEmptyString, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_partfrac){
    CommandWiz(_("Partial Fractions"), wxEmptyString, wxEmptyString,
               wxS("partfrac(#1#,#2#);"), _("Expression:"), expr, wxEmptyString,
               _("Variable:"), wxS("n"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_risch){
    CommandWiz(_("Integrate (risch)"), wxEmptyString, wxEmptyString,
               wxS("risch(#1#,#2#);"), _("Expression:"), expr, wxEmptyString,
               _("Variable:"), wxS("x"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_integrate) ||
          (event.GetId() == EventIDs::menu_integrate)){
      wxWindowPtr<IntegrateWiz> wiz(new IntegrateWiz(this, -1, &m_configuration, _("Integrate")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_laplace){
    CommandWiz(_("Laplace"), wxEmptyString, wxEmptyString,
               wxS("laplace(#1#,#2#,#3#);"), _("Expression:"), expr,
               wxEmptyString, _("Old variable:"), wxS("t"), wxEmptyString,
               _("New variable:"), wxS("s"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_ilt){
    CommandWiz(_("Inverse Laplace"), wxEmptyString, wxEmptyString,
               wxS("ilt(#1#,#2#,#3#);"), _("Expression:"), expr, wxEmptyString,
               _("Old variable:"), wxS("s"), wxEmptyString, _("New variable:"),
               wxS("t"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_diff) ||
          (event.GetId() == EventIDs::menu_diff)){
    CommandWiz(_("Differentiate"), _("Differentiates an expression n times"),
               wxEmptyString, wxS("diff(#1#,#2#,#3#);"), _("Expression:"), expr,
               wxEmptyString, _("Variable(s):"), wxS("x"), wxEmptyString,
               _("Times:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::button_taylor){
      wxWindowPtr<SeriesWiz> wiz(new SeriesWiz(this, -1, &m_configuration, _("Series")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if((event.GetId() == EventIDs::button_limit) ||
          (event.GetId() == EventIDs::menu_limit)){
      wxWindowPtr<LimitWiz> wiz(new LimitWiz(this, -1, &m_configuration, _("Limit")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::menu_lbfgs){
    CommandWiz(_("Find minimum"),
               _("Allows to vary the parameters of a function until it fits "
                 "experimental data."),
               wxEmptyString, wxS("lbfgs(#1#,#2#,#3#,#4#,[1,1]);"),
               _("Expression:"), expr, wxEmptyString, _("Variables:"), wxS("x"),
               wxEmptyString, _("Initial estimates:"), wxS("1.0"),
               wxEmptyString, _("Epsilon:"), wxS("1e-4"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_sum) ||
          (event.GetId() == EventIDs::menu_sum)){
      wxWindowPtr<SumWiz> wiz(new SumWiz(this, -1, &m_configuration, _("Sum")));
      wiz->SetValue(expr);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if((event.GetId() == EventIDs::button_product) ||
          (event.GetId() == EventIDs::menu_product)){
    CommandWiz(_("Product"), wxEmptyString, wxEmptyString,
               wxS("product(#1#,#2#,#3#,#4#);"), _("Expression:"), expr,
               wxEmptyString, _("Variable:"), wxS("k"), wxEmptyString,
               _("From:"), wxS("1"), wxEmptyString, _("To:"), wxS("n"),
               wxEmptyString);
  }
}


void wxMaxima::PlotMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  if((event.GetId() == EventIDs::button_plot3) ||
     (event.GetId() == EventIDs::gp_plot3)){
    wxWindowPtr<Plot3DWiz> wiz(new Plot3DWiz(this, -1, &m_configuration, _("Plot 3D")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_animationautostart){
    if (event.IsChecked())
      MenuCommand(wxS("wxanimate_autoplay:true$"));
    else
      MenuCommand(wxS("wxanimate_autoplay:false$"));
  }
  else if(event.GetId() == EventIDs::menu_animationframerate){
    CommandWiz(_("Enter new animation frame rate [Hz, integer]:"),
               wxEmptyString, wxEmptyString, wxS("wxanimate_framerate : #1#$"),
               _("Frame rate"), wxS("%"), wxEmptyString);
  }
  else if((event.GetId() == EventIDs::button_plot2) ||
          (event.GetId() == EventIDs::gp_plot2)){
    wxWindowPtr<Plot2DWiz> wiz(new Plot2DWiz(this, -1, &m_configuration, _("Plot 2D")));
    wiz->SetValue(expr);
    // wiz->Centre(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
    });
  }
  else if(event.GetId() == EventIDs::menu_plot_format){
    wxWindowPtr<PlotFormatWiz> wiz(new PlotFormatWiz(this, -1, &m_configuration, _("Plot format")));
    wiz->Center(wxBOTH);
    wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
      if (retcode == wxID_OK) {
        MenuCommand(wiz->GetValue());
      }
    });
    /*wxString format = GetTextFromUser(_("Enter new plot format:"),
      _("Plot format"),
      m_configuration,
      wxS("gnuplot"), this);
      if (format.Length())
      {
      MenuCommand(wxS("set_plot_option(['plot_format, '") + format +
      wxS("])$"));
      }*/
  }
}


void wxMaxima::NumericalMenu(wxCommandEvent &event) {
  if(!GetWorksheet())
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString integralSign = wxS("∫");
  if (!m_configuration.FontRendersChar(L'\u222B', *wxNORMAL_FONT)) //  \u222B = integral sign
    integralSign = wxS("integrate");

  if(event.GetId() == EventIDs::popid_special_constant_percent){
      m_configuration.SetKeepPercent(event.IsChecked());
      GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_hideasterisk){
      m_configuration.HidemultiplicationSign(event.IsChecked());
      GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_changeasterisk){
      m_configuration.SetChangeAsterisk(event.IsChecked());
      GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::menu_num_domain){
      wxString cmd;
      if (event.IsChecked())
        cmd = wxS("domain:'complex$");
      else
        cmd = wxS("domain:'real$");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_float){
      wxString cmd = wxS("float(") + expr + wxS("), numer;");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_bfloat){
      wxString cmd = wxS("bfloat(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_to_numer){
      wxString cmd = expr + wxS(",numer;");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rat){
      wxString cmd = wxS("rat(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_rationalize){
      wxString cmd = wxS("rationalize(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_guess_exact_value){
      wxString cmd = wxS("guess_exact_value(") + expr + wxS(");");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_num_out) {
    if (!event.IsChecked())
      MenuCommand("numer:false$");
    else
      MenuCommand("numer:true$");
  }
  else if(event.GetId() == EventIDs::menu_stringdisp){
      wxString cmd;
      if (!event.IsChecked())
        cmd = wxS("stringdisp:false$");
      else
        cmd = wxS("stringdisp:true$");
      MenuCommand(cmd);
    }
  else if(event.GetId() == EventIDs::menu_set_precision){
    CommandWiz(_("Enter new precision for bigfloats:"), wxEmptyString,
               wxEmptyString, wxS("fpprec : #1#$"), _("Precision"), wxS("%"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_set_displayprecision){
    CommandWiz(_("Displayed Precision"), wxEmptyString, wxEmptyString,
               wxS("fpprintprec : #1#$"), _("How many digits to show:"),
               wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_engineeringFormat){
    if ((m_maximaVariable_engineeringFormat != wxS("true")) &&
        (m_maximaVariable_engineeringFormat != wxS("false")))
      MenuCommand(wxS("load(\"engineering-format\")$"));
    if (m_maximaVariable_engineeringFormat == wxS("true"))
      MenuCommand(wxS("engineering_format_floats:false$"));
    if (m_maximaVariable_engineeringFormat == wxS("false"))
      MenuCommand(wxS("engineering_format_floats:true$"));
  }
  else if(event.GetId() == EventIDs::menu_engineeringFormatSetup){
    CommandWiz(_("Setup the engineering format"), wxEmptyString, wxEmptyString,
               wxS("engineering_format_floats: #1#$\n"
                   "engineering_format_min: #2#$\n"
                   "engineering_format_max: #3#$\n"
                   "fpprintprec: #4#$"),
               _("Enable:"), wxS("true"), wxEmptyString,
               _("Minimum absolute value printed without exponent:"),
               wxS(".01"), wxEmptyString,
               _("Maximum absolute value printed without exponent"),
               wxS("1000"), wxEmptyString,
               _("Maximum number of digits to be displayed:"), wxS("6"),
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qag){
      CommandWiz(
                 integralSign + _("(f(x),x,a,b)), Strategy of Aind"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qag(#1#,#2#,#3#,#4#,#5#,epsrel=#6#,epsabs=#7#,limit=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString, wxS("b"), wxS("10"), wxEmptyString,
                 wxS("key"), wxS("4"),
                 _("An integer between 1..6; Higher numbers work better for oscillating "
                   "integrands"),
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qags){
      CommandWiz(
                 integralSign + _("(f(x),x,a,b)), Epsilon algorithm"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qags(#1#,#2#,#3#,#4#,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString, wxS("b"), wxS("10"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qagi){
      CommandWiz(
                 integralSign + _("(f(x),x,a,b), (semi-) infinite interval"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qagi(#1#,#2#,#3#,#4#,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("0"), wxEmptyString, wxS("b"), wxS("10"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qawc){
      CommandWiz(
                 _("Cauchy principal value of f(x)/(x-c), finite interval"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qawc(#1#,#2#,#3#,#4#,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("c"), wxS("4"), wxEmptyString, wxS("a"), wxS("0"), wxEmptyString,
                 wxS("b"), wxS("10"), wxEmptyString, wxS("epsrel"), wxS("1d-8"),
                 wxEmptyString, wxS("epsabs"), wxS("0"), wxEmptyString, wxS("limit"),
                 wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qawf_sin){
      CommandWiz(integralSign + wxS("(f(x)*sin(ω·x),x,a,∞)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawf(#1#,#2#,#3#,#4#,sin,epsabs=#5#,limit=#6#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"),
                 wxEmptyString, wxS("a"), wxS("a"), wxEmptyString, wxS("ω"),
                 wxS("2"), wxEmptyString, wxS("epsabs"), wxS("0"), wxEmptyString,
                 wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qawf_cos){
      CommandWiz(integralSign + wxS("(f(x)*cos(ω·x),x,a,∞)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawf(#1#,#2#,#3#,#4#,cos,epsabs=#5#,limit=#6#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"),
                 wxEmptyString, wxS("a"), wxS("a"), wxEmptyString, wxS("ω"),
                 wxS("2"), wxEmptyString, wxS("epsabs"), wxS("0"), wxEmptyString,
                 wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qawo_sin){
      CommandWiz(integralSign + wxS("(f(x)*sin(ω·x),x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawo(#1#,#2#,#3#,#4#,#5#sin,epsrel=#6#,epsabs=#7#,"
                     "limit=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"),
                 wxEmptyString, wxS("a"), wxS("a"), wxEmptyString, wxS("b"),
                 wxS("a"), wxEmptyString, wxS("ω"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"),
                 wxS("0"), wxEmptyString, wxS("limit"), wxS("200"),
                 wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qawo_cos){
      CommandWiz(integralSign + wxS("(f(x)*cos(ω·x),x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qawo(#1#,#2#,#3#,#4#,#5#,cos,epsrel=#6#,epsabs=#7#,"
                     "limit=#8#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"),
                 wxEmptyString, wxS("a"), wxS("a"), wxEmptyString, wxS("ω"),
                 wxS("2"), wxEmptyString, wxS("epsrel"), wxS("1d-8"),
                 wxEmptyString, wxS("epsabs"), wxS("0"), wxEmptyString,
                 wxS("limit"), wxS("200"), wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_quad_qaws1){
      CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β,x,a,b)"), wxEmptyString,
                 wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,1,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString, wxS("b"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws2){
      CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,2,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString, wxS("b"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws3){
      CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(b-x),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,3,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString, wxS("b"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qaws4){
      CommandWiz(
                 integralSign + wxS("(f(x)*(x-a)^α(b-x)^β·log(x-a)·log(b-x),x,a,b)"),
                 wxEmptyString, wxEmptyString,
                 wxS("quad_qaws(#1#,#2#,#3#,#4#,4,epsrel=#5#,epsabs=#6#,limit=#7#)"),
                 wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
                 wxS("a"), wxS("1"), wxEmptyString, wxS("b"), wxS("2"), wxEmptyString,
                 wxS("epsrel"), wxS("1d-8"), wxEmptyString, wxS("epsabs"), wxS("0"),
                 wxEmptyString, wxS("limit"), wxS("200"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_quad_qagp){
    CommandWiz(
               integralSign + _("(f(x),x,y) with singularities+discontinuities"),
               wxEmptyString, wxEmptyString,
               wxS("qagp(#1#,#2#,#3#,#4#,[#5#],epsrel=#6#,epsabs=#7#,limit=#8#)"),
               wxS("f(x)"), wxS("%"), wxEmptyString, wxS("x"), wxS("x"), wxEmptyString,
               wxS("a"), wxS("1"), wxEmptyString, wxS("b"), wxS("2"), wxEmptyString,
               wxS("points"), wxS(".5,.75"), wxEmptyString, wxS("epsrel"), wxS("1d-8"),
               wxEmptyString, wxS("epsabs"), wxS("0"), wxEmptyString, wxS("limit"),
               wxS("200"), wxEmptyString);
  }
}

void wxMaxima::CommandWiz(
                          const wxString &title, const wxString &description,
                          const wxString &description_tooltip, const wxString &commandRule,
                          wxString label1, wxString defaultval1, wxString tooltip1, wxString label2,
                          wxString defaultval2, wxString tooltip2, wxString label3,
                          wxString defaultval3, wxString tooltip3, wxString label4,
                          wxString defaultval4, wxString tooltip4, wxString label5,
                          wxString defaultval5, wxString tooltip5, wxString label6,
                          wxString defaultval6, wxString tooltip6, wxString label7,
                          wxString defaultval7, wxString tooltip7, wxString label8,
                          wxString defaultval8, wxString tooltip8, wxString label9,
                          wxString defaultval9, wxString tooltip9) {
  m_wizard->NewWizard(
                      description, description_tooltip,
                      commandRule, std::move(label1), std::move(defaultval1),
                      std::move(tooltip1), std::move(label2), std::move(defaultval2),
                      std::move(tooltip2), std::move(label3), std::move(defaultval3),
                      std::move(tooltip3), std::move(label4), std::move(defaultval4),
                      std::move(tooltip4), std::move(label5), std::move(defaultval5),
                      std::move(tooltip5), std::move(label6), std::move(defaultval6),
                      std::move(tooltip6), std::move(label7), std::move(defaultval7),
                      std::move(tooltip7), std::move(label8), std::move(defaultval8),
                      std::move(tooltip8), std::move(label9), std::move(defaultval9),
                      std::move(tooltip9));
  ShowWizardPane(title);
}

void wxMaxima::OnWizardAbort(wxCommandEvent &WXUNUSED(event)) {
  HideWizardPane();
  m_configuration.LastActiveTextCtrl(NULL);
}

void wxMaxima::OnWizardOK(wxCommandEvent &event) {
  OnWizardInsert(event);
  OnWizardAbort(event);
  m_configuration.LastActiveTextCtrl(NULL);
}

void wxMaxima::OnWizardHelpButton(wxCommandEvent &event) {
  ShowMaximaHelp(m_wizard->GetHelpKeyword(event.GetId()));
}

void wxMaxima::OnWizardInsert(wxCommandEvent &WXUNUSED(event)) {
  MenuCommand(m_wizard->GetOutput());
  m_configuration.LastActiveTextCtrl(NULL);
}

void wxMaxima::HelpMenu(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();

  if(event.GetId() == EventIDs::menu_goto_url){
      wxWindowPtr<GenWiz> wiz(new GenWiz(this, &m_configuration, GetWorksheet()->GetMaximaManual(),
                                         _("Go to URL"), wxEmptyString, wxEmptyString, wxEmptyString,
                                         _("URL"), wxEmptyString, wxEmptyString));
      // wiz->Centre(wxBOTH);
#ifdef USE_WEBVIEW
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          m_helpPane->SetURL((*wiz)[0]);
          wxMaximaFrame::ShowPane(EventIDs::menu_pane_help);
        }
      });
#endif
    }
  else if(event.GetId() == wxID_ABOUT){
    AboutDialog aboutdlg(this, &m_configuration);
    }
  else if(event.GetId() == EventIDs::menu_license){
      LicenseDialog *dlg = new LicenseDialog(this);
      dlg->Show();
    }

  else if(event.GetId() == EventIDs::menu_changelog){
      ChangeLogDialog *dlg = new ChangeLogDialog(this);
      dlg->Show();
    }
  else if(event.GetId() == EventIDs::menu_help_demo_for_command){
    MenuCommand(wxS("demo(\"") + expr + wxS("\");"));
    }
  else if(event.GetId() == wxID_HELP){
    ShowHelp(expr);
  }

  else if(event.GetId() == EventIDs::menu_wxmaximahelp){
    ShowWxMaximaHelp();
  }

  else if(event.GetId() == EventIDs::menu_maximahelp){
    ShowMaximaHelpWithoutAnchor();
  }

  else if(event.GetId() == EventIDs::menu_example){
    CommandWiz(_("Show an example for the command:"), wxEmptyString,
               wxEmptyString, wxS("example(#1);"), _("Command:"), wxS("%"),
               wxEmptyString);
  }

  else if(event.GetId() == EventIDs::menu_apropos){
    CommandWiz(_("Apropos"), wxEmptyString, wxEmptyString, wxS("apropos(#1);"),
               _("Show all commands similar to:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_wxmaxima_uses_help_browser){
    m_configuration.InternalHelpBrowser(false);
  }
  else if(event.GetId() == EventIDs::menu_wxmaxima_uses_help_sidebar){
    m_configuration.InternalHelpBrowser(true);
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_internal_help){
    m_configuration.MaximaUsesHtmlBrowser(false);
    m_configuration.MaximaUsesWxmaximaBrowser(false);
    MenuCommand(wxS("output_format_for_help:'text"));
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_html_help){
    m_configuration.MaximaUsesHtmlBrowser(true);
    m_configuration.MaximaUsesWxmaximaBrowser(false);
    MenuCommand(wxS("output_format_for_help:'html"));
  }
  else if(event.GetId() == EventIDs::menu_maxima_uses_wxmaxima_help){
    m_configuration.MaximaUsesWxmaximaBrowser(true);
    MenuCommand(wxS("output_format_for_help:'frontend"));
  }

  else if(event.GetId() == EventIDs::menu_show_tip){
    ShowTip(true);
  }

  else if(event.GetId() == EventIDs::menu_build_info){
    MenuCommand(wxS("wxbuild_info()$"));
  }

  else if(event.GetId() == EventIDs::menu_bug_report){
    MenuCommand(wxS("wxbug_report()$"));
  }

  else if(event.GetId() == EventIDs::menu_help_tutorials){
    wxLaunchDefaultBrowser(wxS("https://wxMaxima-developers.github.io/wxmaxima/help.html"));
  }
  else if(event.GetId() == EventIDs::menu_help_maxima_homepage){
    wxLaunchDefaultBrowser(wxS("https://maxima.sourceforge.io/documentation.html"));
  }
  else if(event.GetId() == EventIDs::menu_check_updates){
    CheckForUpdates(true);
  }
}


void wxMaxima::StatsMenu(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();

  if(event.GetId() == EventIDs::menu_stats_histogram){
    CommandWiz(_("Histogram"), wxEmptyString, wxEmptyString,
               wxS("wxhistogram(#1#,nclasses=#2#);"), _("Data:"), expr,
               wxEmptyString, _("Classes:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_scatterplot){
    CommandWiz(_("Scatterplot"), wxEmptyString, wxEmptyString,
               wxS("wxscatterplot(#1#,nclasses=#2#);"), _("Data:"), expr,
               wxEmptyString, _("Classes:"), wxS("10"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_barsplot){
    CommandWiz(_("Plot as bars"), wxEmptyString, wxEmptyString,
               wxS("wxbarsplot(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_boxplot){
    CommandWiz(_("Plot as error bars"), wxEmptyString, wxEmptyString,
               wxS("wxboxplot(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_piechart){
    CommandWiz(_("Plot as pie chart"), wxEmptyString, wxEmptyString,
               wxS("wxpiechart(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_mean){
    CommandWiz(_("Calculate mean value"), wxEmptyString, wxEmptyString,
               wxS("mean(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_median){
    CommandWiz(_("Calculate median value"), wxEmptyString, wxEmptyString,
               wxS("median(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_var){
    CommandWiz(_("Calculate variation"), wxEmptyString, wxEmptyString,
               wxS("var(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_dev){
    CommandWiz(_("Calculate standard deviation"), wxEmptyString, wxEmptyString,
               wxS("std(#1);"), _("Data:"), wxS("%"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tt1){
    CommandWiz(_("One sample t-test"), wxEmptyString, wxEmptyString,
               wxS("test_mean(#1#,mean=#2#);"), _("Sample:"), expr,
               wxEmptyString, _("Mean:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tt2){
    CommandWiz(_("Two sample t-test"), wxEmptyString, wxEmptyString,
               wxS("test_means_difference(#1#,#2#);"), _("Sample 1:"), expr,
               wxEmptyString, _("Sample 2:"), wxS("0"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_tnorm){
    CommandWiz(_("Shapiro-Wilk test for normality"), wxEmptyString,
               wxEmptyString, wxS("test_normality(#1#);"), _("Data:"), expr,
               wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_linreg){
    CommandWiz(_("Multivariate linear regression"), wxEmptyString,
               wxEmptyString, wxS("simple_linear_regression(#1#);"), _("Data:"),
               expr, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::menu_stats_lsquares){
    CommandWiz(
               _("Least Squares Fit"), wxEmptyString, wxEmptyString,
               wxS("lsquares_estimates(#1#,[#2#],#3#,[#4#],iprint=[-1,0]);"),
               _("Data Matrix:"), expr,
               _("A matrix in which each row is a set of variables"), _("Col. names:"),
               wxS("x,y"), _("The list of variables contained in each matrix row"),
               _("Equation:"), wxS("y=A*x+B"), _("The equation to fit the data to"),
               _("Variables:"), wxS("A,B"),
               _("The variables to search the optimum solution for"));
  }
  else if(event.GetId() == EventIDs::menu_stats_readm){
      wxString file = wxFileSelector(
                                     _("Open matrix"), m_lastPath, wxEmptyString, wxEmptyString,
                                     _("Data file (*.csv, *.tab, *.txt)|*.csv;*.tab;*.txt"), wxFD_OPEN);
      if (file != wxEmptyString) {
        m_lastPath = wxPathOnly(file);

#if defined __WXMSW__
        file.Replace(wxS("\\"), wxS("/"));
#endif

        wxString name =
          wxGetTextFromUser(wxS("Enter matrix name:"), wxS("Matrix name"));
        wxString cmd;

        if (name != wxEmptyString)
          cmd << name << wxS(": ");

        wxString format;
        if (file.Lower().EndsWith(wxS(".csv")))
          format = wxS("csv");
        else if (file.Lower().EndsWith(wxS(".tab")))
          format = wxS("tab");

        if (format != wxEmptyString)
          MenuCommand(cmd + wxS("read_matrix(\"") + file + wxS("\", '") + format +
                      wxS(");"));
        else
          MenuCommand(cmd + wxS("read_matrix(\"") + file + wxS("\");"));
      }
    }
  else if(event.GetId() == EventIDs::menu_stats_subsample){
      wxWindowPtr<Gen4Wiz> wiz(new Gen4Wiz(
                                           _("Data Matrix:"), _("Condition:"), _("Include columns:"),
                                           _("Matrix name:"), expr, wxS("col[1]#'NA"), wxEmptyString,
                                           wxEmptyString, &m_configuration, this, -1, _("Select Subsample"), true));
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString name = wiz->GetValue4();

          wxString cmd;

          if (name != wxEmptyString)
            cmd << name << wxS(": ");

          cmd += wxS("subsample(\n   ") + wiz->GetValue1() + wxS(",\n   ") +
            wxS("lambda([col], is( ");

          if (wiz->GetValue2() != wxEmptyString)
            cmd += wiz->GetValue2() + wxS(" ))");
          else
            cmd += wxS("true ))");

          if (wiz->GetValue3() != wxEmptyString)
            cmd += wxS(",\n   ") + wiz->GetValue3();

          cmd += wxS(");");
          MenuCommand(cmd);
        }
      });
  }
}

bool wxMaxima::SaveOnClose() {
  if (!SaveNecessary()) {
    return true;
  }

  // If we want to keep the file saved we automatically save the file on
  // closing.
  if (m_configuration.AutoSaveAsTempFile()) {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return false;
    if (close == wxID_NO)
      return true;
    else {
      if (close == wxID_YES) {
        if (!SaveFile(true)) {
          return false;
        }
      }
      return true;
    }
  } else {
    {
      if(SaveFile())
        return true;
    }
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return false;
    else {
      if (close == wxID_YES) {
        if (!SaveFile()) {
          if (!SaveFile(true))
            return false;
        }
      }
    }
  }
  return true;
}

void wxMaxima::OnClose(wxCloseEvent &event) {
  if ((!SaveOnClose() && (event.CanVeto()))) {
    event.Veto();
    return;
  }
  wxConfig::Get()->Write(wxS("lastPath"), m_lastPath);
  Destroy();
}

void wxMaxima::PopupMenu(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString selection = GetWorksheet()->GetString();
  if(event.GetId() == EventIDs::enable_unicodePane){
    wxMaximaFrame::ShowPane(EventIDs::menu_pane_unicode, true);
  }
  else if(event.GetId() == EventIDs::popid_fold){
      if (GetWorksheet()->GetActiveCell()) {
        // This "if" is pure paranoia. But - since the costs of an "if" are low...
        GroupCell *group = GetWorksheet()->GetActiveCell()->GetGroup();
        if (group->IsFoldable())
          group->Fold();
        else
          group->Hide(true);
        GetWorksheet()->UpdateTableOfContents();
      }
  }
  else if(event.GetId() == EventIDs::popid_maxsizechooser){
    if (GetWorksheet()->GetSelectionStart()) {
      Cell *output = GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if ((output->GetType() != MC_TYPE_IMAGE) &&
          (output->GetType() != MC_TYPE_SLIDE))
        return;

      MaxSizeChooser *chooser = new MaxSizeChooser(
                                                   this, -1, dynamic_cast<ImgCellBase *>(output)->GetMaxWidth(),
                                                   dynamic_cast<ImgCellBase *>(output)->GetHeightList());
      chooser->Centre(wxBOTH);
      if (chooser->ShowModal() == wxID_OK) {
        if (dynamic_cast<ImgCellBase *>(output)->GetMaxWidth() !=
            chooser->GetMaxImageWidth())
          GetWorksheet()->SetSaved(false);
        if (dynamic_cast<ImgCellBase *>(output)->GetHeightList() !=
            chooser->GetHeightList())
          GetWorksheet()->SetSaved(false);

        dynamic_cast<ImgCellBase *>(output)->SetMaxWidth(
                                                         chooser->GetMaxWidth());
        dynamic_cast<ImgCellBase *>(output)->SetMaxHeight(
                                                          chooser->GetHeightList());
      }
    }
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_resolutionchooser){
    if (GetWorksheet()->GetSelectionStart()) {
      Cell *output = GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if ((output->GetType() != MC_TYPE_IMAGE) &&
          (output->GetType() != MC_TYPE_SLIDE))
        return;

      ResolutionChooser *chooser = new ResolutionChooser(
                                                         this, -1, dynamic_cast<ImgCellBase *>(output)->GetPPI());
      chooser->Centre(wxBOTH);
      if (chooser->ShowModal() == wxID_OK) {
        if (dynamic_cast<ImgCellBase *>(output)->GetPPI() !=
            chooser->GetResolution())
          GetWorksheet()->SetSaved(false);

        dynamic_cast<ImgCellBase *>(output)->SetPPI(chooser->GetResolution());
      }
    }
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
  }
  else if(event.GetId() == EventIDs::popid_reloadimage){
    if (!GetWorksheet()->GetSelectionStart())
      return;

    {
      Cell *output = GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (output == NULL)
        return;
      if (output->GetType() != MC_TYPE_IMAGE)
        return;

      wxString imgFile = dynamic_cast<ImgCell *>(output)->GetOrigImageFile();

      if (!wxFileExists(imgFile)) {
        LoggingMessageDialog dialog(
                                    this,
                                    wxString::Format(_("The image file \"%s\" cannot be found."),
                                                     imgFile),
                                    "wxMaxima", wxCENTER | wxOK);
        dialog.SetOKLabel(_("OK"));

        dialog.ShowModal();

        return;
      }

      wxLogMessage(_("Reloading image file %s."), imgFile);
      dynamic_cast<ImgCell *>(output)->ReloadImage(imgFile,
                                                   wxEmptyString);

      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
      GetWorksheet()->SetSaved(false);

      UpdateMenus();
      UpdateToolBar();
      // ResetTitle(GetWorksheet()->IsSaved());
    }
  }
  else if(event.GetId() == EventIDs::popid_unfold){
      GroupCell *group = GetWorksheet()->GetActiveCell()->GetGroup();
      if (group->IsFoldable())
        group->Unfold();
      else
        group->Hide(false);
      GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_Fold){
    if (m_tableOfContents != NULL) {
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((GetWorksheet()->GetTree()) &&
          (GetWorksheet()->GetTree()->Contains(
                                            m_tableOfContents->RightClickedOn()))) {
        m_tableOfContents->RightClickedOn()->Fold();
        GetWorksheet()->Recalculate();
        GetWorksheet()->RequestRedraw();
        GetWorksheet()->UpdateTableOfContents();
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_Unfold){
    if (m_tableOfContents != NULL) {
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((GetWorksheet()->GetTree()) &&
          (GetWorksheet()->GetTree()->Contains(
                                            m_tableOfContents->RightClickedOn()))) {
        m_tableOfContents->RightClickedOn()->Unfold();
        GetWorksheet()->Recalculate();
        GetWorksheet()->RequestRedraw();
        GetWorksheet()->UpdateTableOfContents();
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_SelectTocChapter){
    if (m_tableOfContents != NULL) {
      if (m_tableOfContents->RightClickedOn()) {
        GroupCell *SelectionStart =
          m_tableOfContents->RightClickedOn();
        // We only update the table of contents when there is time => no
        // guarantee that the cell that was clicked at actually still is part of
        // the tree.
        if ((GetWorksheet()->GetTree()) &&
            (GetWorksheet()->GetTree()->Contains(SelectionStart))) {
          GroupCell *SelectionEnd = SelectionStart;
          while ((SelectionEnd->GetNext() != NULL) &&
                 (SelectionEnd->GetNext()->IsLesserGCType(
                                                          SelectionStart->GetGroupType())))
            SelectionEnd = SelectionEnd->GetNext();
          GetWorksheet()->SetActiveCell(NULL);
          GetWorksheet()->ScrolledAwayFromEvaluation(true);
          GetWorksheet()->SetHCaret(SelectionEnd);
          GetWorksheet()->SetSelection(SelectionStart, SelectionEnd);
          GetWorksheet()->RequestRedraw();
        }
      }
    }
  }
  else if(event.GetId() == EventIDs::popid_EvalTocChapter){
      GroupCell *SelectionStart =
        m_tableOfContents->RightClickedOn();
      // We only update the table of contents when there is time => no guarantee
      // that the cell that was clicked at actually still is part of the tree.
      if ((GetWorksheet()->GetTree()) &&
          (GetWorksheet()->GetTree()->Contains(SelectionStart))) {
        GetWorksheet()->AddSectionToEvaluationQueue(
                                                 m_tableOfContents->RightClickedOn());
        TriggerEvaluation();
      }
  }
  else if(event.GetId() == EventIDs::popid_ToggleTOCshowsSectionNumbers){
      m_configuration.TocShowsSectionNumbers(event.IsChecked());
      GetWorksheet()->UpdateTableOfContents();
  }
  else if((event.GetId() >= EventIDs::popid_tocLevel1) && (event.GetId() < EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels - 2)) {
    m_configuration.TocDepth(event.GetId() - EventIDs::popid_tocLevel1 + 1 );
    GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels -1){
      m_configuration.TocDepth(255);
      GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocdnd){
    GetWorksheet()->TOCdnd(m_tableOfContents->DNDStart(), m_tableOfContents->DNDEnd());
  }
  else if(event.GetId() == EventIDs::popid_tocMoveIn){
    GetWorksheet()->SectioningMoveIn(m_tableOfContents->RightClickedOn());
    GetWorksheet()->NumberSections();
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
    GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_tocMoveOut){
    GetWorksheet()->SectioningMoveOut(m_tableOfContents->RightClickedOn());
    GetWorksheet()->NumberSections();
    GetWorksheet()->Recalculate();
    GetWorksheet()->RequestRedraw();
    GetWorksheet()->UpdateTableOfContents();
  }
  else if(event.GetId() == EventIDs::popid_evaluate_section){
      GroupCell *group = NULL;
      if (GetWorksheet()->GetActiveCell()) {
        // This "if" is pure paranoia. But - since the costs of an "if" are low...
        if (GetWorksheet()->GetActiveCell()->GetGroup())
          group = GetWorksheet()->GetActiveCell()->GetGroup();
      } else if (GetWorksheet()->HCaretActive()) {
        if (GetWorksheet()->GetHCaret()) {
          group = GetWorksheet()->GetHCaret();
          if ((false))
            if (group->GetNext())
              group = group->GetNext();
        } else
          group = GetWorksheet()->GetTree();
      }
      if (group) {
        GetWorksheet()->AddSectionToEvaluationQueue(group);
        TriggerEvaluation();
      }
    }
  else if((event.GetId() == EventIDs::popid_evaluate) ||
          (event.GetId() == ToolBar::tb_eval)){
    wxCommandEvent *dummy = new wxCommandEvent;
    EvaluateEvent(*dummy);
  }
  else if(event.GetId() == ToolBar::tb_evaluate_rest){
    GetWorksheet()->AddRestToEvaluationQueue();
    EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                          GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
  }
  else if(event.GetId() == ToolBar::tb_evaltillhere){
    GetWorksheet()->m_evaluationQueue.Clear();
    GetWorksheet()->ResetInputPrompts();
    EvaluationQueueLength(0);
    if (m_configuration.RestartOnReEvaluation())
      StartMaxima();
    GetWorksheet()->AddDocumentTillHereToEvaluationQueue();
    // Inform the user about the length of the evaluation queue.
    EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                          GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
    TriggerEvaluation();
  }
  else if(event.GetId() == EventIDs::popid_copy_matlab){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyMatlab();
  }
  else if(event.GetId() == EventIDs::popid_copy_tex){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyTeX();
  }
  else if(event.GetId() == EventIDs::popid_copy_text){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyText();
  }
  else if(event.GetId() == EventIDs::popid_comment_selection){
    GetWorksheet()->CommentSelection();
  }
  else if(event.GetId() == EventIDs::popid_divide_cell){
    GetWorksheet()->DivideCell();
  }
  else if(event.GetId() == EventIDs::popid_copy_image){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyBitmap();
  }
  else if(event.GetId() == EventIDs::popid_copy_animation){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyAnimation();
  }
  else if(event.GetId() == EventIDs::popid_copy_svg){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopySVG();
  }
#if wxUSE_ENH_METAFILE
  else if(event.GetId() == EventIDs::popid_copy_emf){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyEMF();
  }
#endif
  else if(event.GetId() == EventIDs::popid_copy_rtf){
    if (GetWorksheet()->CanCopy())
      GetWorksheet()->CopyRTF();
  }
  else if(event.GetId() == EventIDs::popid_simplify){
    MenuCommand(wxS("ratsimp(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_expand){
    MenuCommand(wxS("expand(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_factor){
    MenuCommand(wxS("factor(") + selection + wxS(");"));
  }
  else if(event.GetId() == EventIDs::popid_solve){
    CommandWiz(
               _("Solve"),
               _("solve() will solve a list of equations only if for n "
                 "independent equations there are n variables to solve to.\n"
                 "If only one result variable is of interest the other result "
                 "variables can be used to to tell solve() which variables to "
                 "eliminate from the solution\n"
                 "solve() searches for a global solution. If a problem has different "
                 "solutions depending on the range its variables are in one way "
                 "to successfully use solve() is to use solve() to eliminate "
                 "variables "
                 "one by one and to manually choose which of the solutions solve() "
                 "found "
                 "matches the current problem."),
               wxEmptyString, wxS("solve([#1#],[#2#]);"), _("Data:"), selection,
               _("Comma-separated equations"), _("Result variables:"), wxS("x"),
               _("Comma-separated variables"));
  }
  else if(event.GetId() == EventIDs::popid_solve_num){
    CommandWiz(_("Find root (solve numerically)"),
               _("Tries to find a solution of the equation that lies between "
                 "the two bounds."),
               wxEmptyString, wxS("find_root(#1#,#2#,#3#,#4#);"),
               _("Equation:"), selection, wxEmptyString, _("Variable:"),
               wxS("x"), wxEmptyString, _("Lower bound:"), wxS("-1"),
               wxEmptyString, _("Upper bound:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_integrate){
      wxWindowPtr<IntegrateWiz> wiz(new IntegrateWiz(this, -1, &m_configuration, _("Integrate")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_diff){
    CommandWiz(_("Differentiate"), _("Differentiates the expression n times"),
               wxEmptyString, wxS("diff(#1#,#2#,#3#);"), _("Expression:"),
               selection, wxEmptyString, _("Variable(s):"), wxS("x"),
               wxEmptyString, _("Times:"), wxS("1"), wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_subst){
    CommandWiz(_("Substitute"),
               _("Introduces one or more assignments into an expression"),
               wxEmptyString, wxS("subst(#1#,#2#);"), _("Assignment(s):"),
               wxS("x=sqrt(u)"), _("Assignments of the format a=10,b=20"),
               _("Expression"), selection, wxEmptyString);
  }
  else if(event.GetId() == EventIDs::popid_plot2d){
      wxWindowPtr<Plot2DWiz> wiz(new Plot2DWiz(this, -1, &m_configuration, _("Plot 2D")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_plot3d){
      wxWindowPtr<Plot3DWiz> wiz(new Plot3DWiz(this, -1, &m_configuration, _("Plot 3D")));
      wiz->SetValue(selection);
      // wiz->Centre(wxBOTH);
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK) {
          wxString val = wiz->GetValue();
          MenuCommand(val);
        }
      });
    }
  else if(event.GetId() == EventIDs::popid_float){
    MenuCommand(wxS("float(") + selection + wxS("), numer;"));
  }
  else if(event.GetId() == EventIDs::popid_image){
      if ((GetWorksheet()->GetSelectionStart() == GetWorksheet()->GetSelectionEnd()) &&
          (GetWorksheet()->GetSelectionStart() != NULL))
        {
          bool canExportSVG = false;

          if ((GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_IMAGE) ||
              (GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            if (dynamic_cast<ImgCellBase *>(GetWorksheet()->GetSelectionStart())
                ->CanExportSVG())
              canExportSVG = true;

          wxString selectorString;

          if (canExportSVG)
            selectorString = _("Scalable Vector image (*.svg)|*.svg|"
                               "Compressed Scalable Vector Image (*.svgz)|*.svgz|"
                               "PNG image (*.png)|*.png|"
                               "JPEG image (*.jpg)|*.jpg|"
                               "GIF image (*.gif)|*.gif|"
                               "Windows bitmap (*.bmp)|*.bmp|") +
#ifdef wwxUSE_LIBWEB
                               _("WebP (*.webp)|*.webp|") +
#endif
                               _("Portable anymap (*.pnm)|*.pnm|"
                               "Tagged image file format (*.tif)|*.tif|"
                               "X pixmap (*.xpm)|*.xpm");
          else
            selectorString = _("PNG image (*.png)|*.png|"
                               "JPEG image (*.jpg)|*.jpg|"
                               "Windows bitmap (*.bmp)|*.bmp|") +
#ifdef wwxUSE_LIBWEB
                               _("WebP (*.webp)|*.webp|") +
#endif
                               _("GIF image (*.gif)|*.gif|"
                               "Portable anymap (*.pnm)|*.pnm|"
                               "Tagged image file format (*.tif)|*.tif|"
                               "X pixmap (*.xpm)|*.xpm");

          wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                         wxS("image.png"), wxS("png"), selectorString,
                                         wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
          if (file.Length()) {
            GetWorksheet()->CopyToFile(file);
            m_lastPath = wxPathOnly(file);
          }
        }
  }
  else if(event.GetId() == EventIDs::popid_change_image){
      if (!GetWorksheet()->GetSelectionStart())
        return;

      Cell *cell = GetWorksheet()->GetSelectionStart()->GetGroup()->GetLabel();
      if (cell == NULL)
        return;

      if (cell->GetType() != MC_TYPE_IMAGE)
        return;

      wxString newImg = wxFileSelector(
                                       _("Change Image"), m_lastPath, wxEmptyString, wxEmptyString,
                                       _("Image files") + " (*.png, *.jpg, "
#ifdef wwxUSE_LIBWEB
                                         "*.webp, "
#endif

                                         "*.bmp, *.xpm, *.gif, *.svg, "
                                         "*.svgz)|*.png;*.jpg;"
#ifdef wwxUSE_LIBWEB
                                         "*.webp;"
#endif
                                         "*.bmp;*.xpm;*.gif;*.svg;*.svgz",
                                       wxFD_OPEN);

      if (!newImg.Length()) {
        return;
      }

      if (!wxFileExists(newImg)) {
        LoggingMessageDialog dialog(
                                    this,
                                    wxString::Format(_("The image file \"%s\" cannot be found."),
                                                     newImg),
                                    "wxMaxima", wxCENTER | wxOK);
        dialog.SetOKLabel(_("OK"));

        dialog.ShowModal();

        return;
      }

      ImgCell *ic = dynamic_cast<ImgCell *>(cell);

      wxLogMessage(_("Changing image originally loaded from file %s to %s."),
                   ic->GetOrigImageFile(), newImg);
      ic->ReloadImage(newImg, wxEmptyString);
      ic->SetOrigImageFile(newImg);

      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
      GetWorksheet()->SetSaved(false);
      m_lastPath = wxPathOnly(newImg);

      UpdateMenus();
      UpdateToolBar();
    }
  else if(event.GetId() == EventIDs::popid_animation_save){
      wxString file = wxFileSelector(_("Save animation to file"), m_lastPath,
                                     wxS("animation.gif"), wxS("gif"),
                                     _("GIF image (*.gif)|*.gif"),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length()) {
        Cell *selectedCell = GetWorksheet()->GetSelectionStart();
        if (selectedCell != NULL && selectedCell->GetType() == MC_TYPE_SLIDE)
          {
            wxBusyCursor crs;
            dynamic_cast<AnimationCell *>(selectedCell)->ToGif(file);
          }
      }
    }
  else if(event.GetId() == EventIDs::popid_merge_cells){
    GetWorksheet()->MergeCells();
  }
}


void wxMaxima::OnRecentDocument(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString file = m_recentDocuments.Get(event.GetId() - EventIDs::menu_recent_document_0);

  if (SaveNecessary() && ((file.Lower().EndsWith(wxS(".wxmx"))) ||
                          (file.Lower().EndsWith(wxS(".wxm"))))) {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES) {
      if (!SaveFile())
        return;
    }
  }

  if (wxFileExists(file))
    OpenFile(file);
  else {
    LoggingMessageBox(_("File you tried to open does not exist."),
                      _("File not found"), wxOK);
  }
}

void wxMaxima::OnRecentPackage(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString file = m_recentPackages.Get(event.GetId() - EventIDs::menu_recent_package_0);
#ifdef __WXMSW__
  file.Replace(wxS("\\"), wxS("/"));
#endif
  MenuCommand(wxS("load(\"") + file + wxS("\")$"));
}

void wxMaxima::OnUnsavedDocument(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString file =
    GetWorksheet()->m_unsavedDocuments.Get(event.GetId() - EventIDs::menu_unsaved_document_0);

  if (file.IsEmpty())
    return;

  if (SaveNecessary() && ((file.Lower().EndsWith(wxS(".wxmx"))) ||
                          (file.Lower().EndsWith(wxS(".wxm"))))) {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES) {
      if (!SaveFile())
        return;
    }
  }

  if (wxFileExists(file)) {
    OpenWXMXFile(file, GetWorksheet(), true);
    m_tempfileName = file;
    GetWorksheet()->m_currentFile.Clear();
    GetWorksheet()->SetSaved(false);
  } else
    LoggingMessageBox(_("File you tried to open does not exist."),
                      _("File not found"), wxOK);
}

bool wxMaxima::SaveNecessary() {
  // No need to save an empty document
  if ((!GetWorksheet()) || (GetWorksheet()->GetTree() == NULL))
    return false;

  // No need to save a document only consisting of a prompt
  if (GetWorksheet()->GetTree()->Empty())
    return false;

  if (GetWorksheet()->m_currentFile.IsEmpty())
    return true;

  return !m_fileSaved;
}

void wxMaxima::EditInputMenu(wxCommandEvent &WXUNUSED(event)) {
  GetWorksheet()->CloseAutoCompletePopup();
  if (!GetWorksheet()->CanEdit())
    return;

  EditorCell *tmp =
    dynamic_cast<EditorCell *>(GetWorksheet()->GetSelectionStart());

  if (tmp == NULL)
    return;

  GetWorksheet()->SetActiveCell(tmp);
}

void wxMaxima::VarAddAllEvent(wxCommandEvent &WXUNUSED(event)) {
  wxString command = "\n:lisp-quiet (wx-add-all-variables)\n";
  if ((!GetWorksheet()->m_evaluationQueue.Empty()) || (m_maximaBusy) ||
      (GetWorksheet()->QuestionPending()))
    m_configCommands += command;
  else
    SendMaxima(command);
}

void wxMaxima::VarReadEvent(wxCommandEvent &WXUNUSED(event)) {
  if(m_variablesPane)
    m_varNamesToQuery = m_variablesPane->GetEscapedVarnames();
  QueryVariableValue();
}

//! Handle the evaluation event
//
// User tried to evaluate, find out what is the case
// Normally just add the respective groupcells to evaluationqueue
// If there is a special case - eg sending from output section
// of the working group, handle it carefully.
void wxMaxima::EvaluateEvent(wxCommandEvent &WXUNUSED(event)) {
  if (GetWorksheet() == NULL)
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  bool evaluating = !GetWorksheet()->m_evaluationQueue.Empty();
  if (!evaluating)
    GetWorksheet()->FollowEvaluation(true);

  EditorCell *editor = GetWorksheet()->GetActiveCell();

  if (editor == NULL) {
    GroupCell *group = NULL;
    if (GetWorksheet()->HasCellsSelected()) {
      // More than one cell is selected
      GetWorksheet()->AddSelectionToEvaluationQueue();
    }
    else
      {
        if (GetWorksheet()->HCaretActive()) {
          group = GetWorksheet()->GetHCaret();
          if (group == NULL)
            // If the cursor is before the 1st cell of the worksheet hcaret reads NULL.
            group = GetWorksheet()->GetTree();
          else
            // The HCaret points to the cell before the horizontal cursor.
            group = group->GetNext();

          // Now we search for the first cell below the cursor that actually contains code.
          while ((group != NULL) &&
                 (!((group->GetEditable() != NULL) &&
                    (group->GetEditable()->GetType() == MC_TYPE_INPUT)) &&
                  (!GetWorksheet()->m_evaluationQueue.IsLastInQueue(group))))
            group = group->GetNext();
        }
        if ((group != NULL) && (group->GetEditable() != NULL) &&
            (group->GetEditable()->GetType() == MC_TYPE_INPUT))
          editor = group->GetEditable();
      }
  }

  if (editor != NULL) // The cursor is in an active cell
    {
      if (editor->GetType() == MC_TYPE_INPUT && (!m_configuration.InLispMode()))
        editor->AddEnding();
      // if active cell is part of a working group, we have a special
      // case - answering a question. Manually send answer to Maxima.
      GroupCell *cell = editor->GetGroup();
      if (GetWorksheet()->GCContainsCurrentQuestion(cell)) {
        wxString answer = editor->ToString(true);
        // Add the answer to the current working cell or update the answer
        // that is stored within it.
        cell->SetAnswer(GetWorksheet()->GetLastQuestion(), answer);
        SendMaxima(answer, true);
        StatusMaximaBusy(StatusBar::MaximaStatus::calculating);
        GetWorksheet()->SetHCaret(cell);
        GetWorksheet()->ScrollToCaret();
      } else { // normally just add to queue (and mark the cell as no more
        // containing an error message)
        GetWorksheet()->GetErrorList().Remove(cell);
        GetWorksheet()->AddCellToEvaluationQueue(cell);
      }
    } else { // no evaluate has been called on no active cell?
    GetWorksheet()->AddSelectionToEvaluationQueue();
  }
  // Inform the user about the length of the evaluation queue.
  EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                        GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());
  TriggerEvaluation();
}

wxString wxMaxima::GetUnmatchedParenthesisState(wxString text, std::size_t &index) {
  text.Trim(false);
  if (text.EndsWith(wxS("\\")))
    return (_("Cell ends in a backslash"));
  text.Trim(true);
  if (text.IsEmpty())
    return (wxEmptyString);
  if (text.EndsWith(wxS("?")))
    return (_("Last non-whitespace is a question mark"));

  index = 0;
  bool endingNeeded = true;
  wxChar lastnonWhitespace;
  wxChar lastnonWhitespace_Next = wxS(' ');
  std::list<wxChar> delimiters;

  for (auto const &tok : MaximaTokenizer(text, &m_configuration).PopTokens()) {
    auto &itemText = tok.GetText();
    const TextStyle itemStyle = tok.GetTextStyle();
    index += itemText.Length();

    lastnonWhitespace = lastnonWhitespace_Next;

    // Handle comments
    if (itemStyle == TS_CODE_COMMENT) {
      if (!itemText.EndsWith("*/"))
        return (_("Unterminated comment."));
      continue;
    }

    wxChar firstC = itemText.at(0);
    wxChar lastC = itemText.Last();

    // Remember the last non-whitespace character that isn't part
    // of a comment.
    if ((firstC != ' ') && (firstC != '\t') && (firstC != '\r') &&
        (firstC != '\n'))
      lastnonWhitespace_Next = lastC;

    // Handle opening parenthesis
    if (itemText == "(") {
      delimiters.push_back(wxS(')'));
      continue;
    }
    if (itemText == "[") {
      delimiters.push_back(wxS(']'));
      continue;
    }
    if (itemText == "{") {
      delimiters.push_back(wxS('}'));
      continue;
    }

    // Handle closing parenthesis
    if ((itemText == ')') || (itemText == ']') || (itemText == '}')) {
      endingNeeded = true;
      if (delimiters.empty())
        return (_("Mismatched parenthesis"));
      if (firstC != delimiters.back())
        return (_("Mismatched parenthesis"));
      delimiters.pop_back();
      if (lastnonWhitespace == wxS(','))
        return (_("Comma directly followed by a closing parenthesis"));
      continue;
    }

    if (itemStyle == TS_CODE_STRING) {
      endingNeeded = true;
      if (!itemText.EndsWith("\""))
        return (_("Unterminated string."));
      continue;
    }

    if (itemStyle == TS_CODE_ENDOFLINE) {
      if (!delimiters.empty())
        return _("Un-closed parenthesis on encountering ; or $");
      endingNeeded = false;
      continue;
    }

    if (itemStyle == TS_CODE_LISP) {
      endingNeeded = false;
      continue;
    }
  }

  if (!delimiters.empty())
    return _("Un-closed parenthesis");

  if ((endingNeeded) && (!m_configuration.InLispMode()))
    return _("No dollar ($) or semicolon (;) at the end of command");
  else
    return wxEmptyString;
}

//! Tries to evaluate next group cell in queue
//
// Calling this function should not do anything dangerous
void wxMaxima::TriggerEvaluation() {
  if(!GetWorksheet())
    return;
  // If evaluation is already running we don't have anything to do
  if (m_maximaBusy)
    {
      wxLogMessage(_("Not triggering evaluation as maxima is still busy!"));
      return;
    }

  // While we wait for an answer we cannot send new commands.
  if (GetWorksheet()->QuestionPending())
    {
      wxLogMessage(_("Not triggering evaluation as maxima still asks a question!"));
      return;
    }

  // If we aren't connected yet this function will be triggered as soon as
  // maxima connects to wxMaxima
  if (!m_client || (!m_client->IsConnected()))
    {
      wxLogMessage(_("Not triggering evaluation as there is no working maxima process"));
      return;
    }

  // Maxima is connected. Let's test if the evaluation queue is empty.
  GroupCell *const tmp = GetWorksheet()->m_evaluationQueue.GetCell();
  if (!tmp) {
    wxLogMessage(_("Evaluation ended, since evaluation queue is empty."));
    // Maxima is no more busy.
    StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    // Inform the user that the evaluation queue length now is 0.
    EvaluationQueueLength(0);
    // Now we want to start to display things immediately again
    m_fastResponseTimer.Stop();
    // The cell from the last evaluation might still be shown in it's
    // "evaluating" state so let's refresh the console to update the display of
    // this.
    GetWorksheet()->RequestRedraw();

    // If the window isn't active we can inform the user that maxima in the
    // meantime has finished working.
    if ((m_configuration.NotifyIfIdle()) && (GetWorksheet()->GetTree() != NULL))
      GetWorksheet()->SetNotification(_("Maxima has finished calculating."));

    if (m_configCommands != wxEmptyString)
      SendMaxima(m_configCommands);
    m_configCommands.Clear();
    QueryVariableValue();
    return; // empty queue
  }

  // Add a semicolon at the end of the cell, if needed.
  if (tmp->AddEnding())
    GetWorksheet()->m_evaluationQueue.AddEnding();

  // Display the evaluation queue's status.
  EvaluationQueueLength(GetWorksheet()->m_evaluationQueue.Size(),
                        GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());


  // Maxima is connected, not asking a question and the queue contains an item.

  // From now on we look every second if we got some output from a crashing
  // maxima: Is maxima is working correctly the stdout and stderr descriptors we
  // poll don't offer any data.
  ReadStdErr();
  m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

  if (GetWorksheet()->m_evaluationQueue.m_workingGroupChanged) {
    // Clear the monitor that shows the xml representation of the output of the
    // current maxima command.
    if ((m_xmlInspector) && (IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_xmlInspector->Clear();

    // If the cell's output that we are about to remove contains the currently
    // selected cells we undo the selection.
    if (GetWorksheet()->GetSelectionStart()) {
      if (GetWorksheet()->GetSelectionStart()->GetGroup() == tmp)
        GetWorksheet()->ClearSelection();
    }
    if (GetWorksheet()->GetSelectionEnd()) {
      if (GetWorksheet()->GetSelectionEnd()->GetGroup() == tmp)
        GetWorksheet()->ClearSelection();
    }
    tmp->RemoveOutput();
    GetWorksheet()->Recalculate(tmp);
    GetWorksheet()->RequestRedraw();
  }
  wxString text = GetWorksheet()->m_evaluationQueue.GetCommand();
  m_commandIndex = GetWorksheet()->m_evaluationQueue.GetIndex();
  if ((text != wxEmptyString) && (text != wxS(";")) && (text != wxS("$"))) {
    std::size_t index;
    wxString parenthesisError =
      GetUnmatchedParenthesisState(tmp->GetEditable()->ToString(true), index);
    if (parenthesisError.IsEmpty()) {
      if (GetWorksheet()->FollowEvaluation()) {
        GetWorksheet()->SetSelection(tmp);
        if (!GetWorksheet()->GetWorkingGroup()) {
          GetWorksheet()->SetHCaret(tmp);
          GetWorksheet()->ScrollToCaret();
        }
      }

      GetWorksheet()->SetWorkingGroup(tmp);
      tmp->GetPrompt()->SetValue(m_lastPrompt);
      tmp->ResetSize();

      wxLogMessage(_("Sending a new command to Maxima."));
      SendMaxima(m_configCommands);
      SendMaxima(text, true);
      m_maximaBusy = true;
      // Now that we have sent a command we need to query all variable values
      // anew
      if(m_variablesPane)
        m_varNamesToQuery = m_variablesPane->GetEscapedVarnames();
      // And the gui is interested in a few variable names
      m_readMaximaVariables = true;
      m_configCommands.Clear();

      EvaluationQueueLength(
                            GetWorksheet()->m_evaluationQueue.Size(),
                            GetWorksheet()->m_evaluationQueue.CommandsLeftInCell());

      text.Trim(false);
      if (!m_hasEvaluatedCells) {
        if (text.StartsWith(wxS(":lisp")))
          StatusText(_("A \":lisp\" as the first command might fail to "
                       "send a \"finished\" signal."));
      }

      // Mark the current maxima process as "no more in its initial condition".
      m_hasEvaluatedCells = true;
    } else {
      // Manually mark the current cell as the one that has caused an error.
      GetWorksheet()->GetErrorList().Add(tmp);
      // Inform the status bar about the error
      StatusMaximaBusy(StatusBar::MaximaStatus::maximaerror);
      // Inform the user about the error (which automatically causes the
      // worksheet to the cell we marked as erroneous a few seconds ago.
      auto cell =
        std::make_unique<TextCell>(tmp, &m_configuration,
                                   _("Refusing to send cell to maxima: ") +
                                   parenthesisError + wxS("\n"));
      cell->SetType(MC_TYPE_ERROR);
      tmp->SetOutput(std::move(cell));
      GetWorksheet()->m_evaluationQueue.Clear();
      GetWorksheet()->SetWorkingGroup(nullptr);
      tmp->GetEditable()->SetCaretPosition(index);
      tmp->GetEditable()->SetErrorIndex((m_commandIndex = index) - 1);

      if (GetWorksheet()->FollowEvaluation())
        GetWorksheet()->SetSelection(NULL);

      GetWorksheet()->SetWorkingGroup(nullptr);
      GetWorksheet()->RequestRedraw();
      if (!AbortOnError()) {
        m_outputCellsFromCurrentCommand = 0;
        TriggerEvaluation();
      }
      GetWorksheet()->SetActiveCell(tmp->GetEditable());
    }
  } else {
    wxLogMessage(_("Empty command => re-triggering evaluation"));
    m_outputCellsFromCurrentCommand = 0;
    GetWorksheet()->m_evaluationQueue.RemoveFirst();
    TriggerEvaluation();
  }
}

void wxMaxima::ReplaceSuggestion(wxCommandEvent &event) {
  int index = event.GetId() - EventIDs::popid_suggestion1;

  EditorCell *editor = GetWorksheet()->GetActiveCell();
  if (editor == NULL)
    return;
  editor->SelectWordUnderCaret(false);
  editor->ReplaceSelection(editor->GetWordUnderCaret(),
                           GetWorksheet()->m_replacementsForCurrentWord.at(index));
}

void wxMaxima::InsertMenu(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  GroupType type = GC_TYPE_CODE;
  bool output = false;
  if(event.GetId() == EventIDs::popid_never_autoanswer){
    m_configuration.OfferKnownAnswers(!m_configuration.OfferKnownAnswers());
  }
  else if(event.GetId() == EventIDs::popid_auto_answer){
    if (GetWorksheet()->GetActiveCell() &&
        GetWorksheet()->GetActiveCell()->GetGroup()->GetGroupType() ==
        GC_TYPE_CODE)
      GetWorksheet()->GetActiveCell()->GetGroup()->SetAutoAnswer(
                                                              event.IsChecked());
    else if ((GetWorksheet()->GetSelectionStart() != NULL) &&
             (GetWorksheet()->GetSelectionStart()->GetType() == MC_TYPE_GROUP)) {
      GroupCell *gc =
        dynamic_cast<GroupCell *>(GetWorksheet()->GetSelectionStart());
      while (gc != NULL) {
        if (gc->GetGroupType() == GC_TYPE_CODE)
          gc->SetAutoAnswer(event.IsChecked());

        if (gc == GetWorksheet()->GetSelectionEnd())
          break;
        gc = gc->GetNext();
      }
    }
    m_fileSaved = false;
    GetWorksheet()->RequestRedraw();
    return;
  }
  else if(event.GetId() == EventIDs::popid_add_watch){
    wxString selectionString;
    if (GetWorksheet()->GetActiveCell()) {
      selectionString = GetWorksheet()->GetActiveCell()->GetSelectionString();
      if (selectionString.IsEmpty())
        selectionString = GetWorksheet()->GetActiveCell()->GetWordUnderCaret();
      if(m_variablesPane)
        m_variablesPane->AddWatchCode(selectionString);
      wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    if (selectionString.IsEmpty() && (GetWorksheet()->GetSelectionStart() != NULL))
      selectionString = GetWorksheet()->GetSelectionStart()->ToString();
    if (!selectionString.IsEmpty()) {
      if(m_variablesPane)
        m_variablesPane->AddWatchCode(selectionString);
      wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    return;
  }
  else if(event.GetId() == EventIDs::popid_add_watch_label){
    if (GetWorksheet()->IsSelected(MC_TYPE_LABEL)) {
      wxString selectionString = GetWorksheet()->GetSelectionStart()->ToString();
      selectionString.Trim(true);
      selectionString.Trim(false);
      if (selectionString.StartsWith("("))
        selectionString = selectionString.Right(selectionString.Length() - 1);
      if (selectionString.EndsWith(")"))
        selectionString = selectionString.Left(selectionString.Length() - 1);
      if(m_variablesPane)
        m_variablesPane->AddWatchCode(selectionString);
      wxMaximaFrame::ShowPane(EventIDs::menu_pane_variables, true);
    }
    return;
  }
  else if(event.GetId() == EventIDs::menu_insert_previous_output){
    output = true;
    type = GC_TYPE_CODE;
  }
  else if((event.GetId() == EventIDs::popid_insert_input) ||
          (event.GetId() == EventIDs::menu_insert_input) ||
          (event.GetId() == EventIDs::menu_insert_previous_input)){
    type = GC_TYPE_CODE;
  }
  else if(event.GetId() == EventIDs::menu_autocomplete){
    GetWorksheet()->Autocomplete();
    return;}
  else if(event.GetId() == EventIDs::menu_autocomplete_templates){
    GetWorksheet()->Autocomplete(AutoComplete::tmplte);
    return;}
  else if(event.GetId() == EventIDs::menu_convert_to_code){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_CODE);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if(event.GetId() == EventIDs::menu_convert_to_comment){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_TEXT);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if((event.GetId() == EventIDs::menu_add_comment) ||
          (event.GetId() == EventIDs::popid_add_comment) ||
          (event.GetId() == EventIDs::menu_format_text) ||
          (event.GetId() == EventIDs::popid_insert_text))
    {
      type = GC_TYPE_TEXT;
    }
  else if(event.GetId() == EventIDs::menu_convert_to_title){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_TITLE);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if((event.GetId() == EventIDs::menu_add_title) ||
          (event.GetId() == EventIDs::menu_format_title) ||
          (event.GetId() == EventIDs::popid_insert_title)){
    type = GC_TYPE_TITLE;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_section){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_SECTION);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if((event.GetId() == EventIDs::menu_add_section) ||
          (event.GetId() == EventIDs::menu_format_section) ||
          (event.GetId() == EventIDs::popid_insert_section)){
    type = GC_TYPE_SECTION;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_subsection){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(
                                                             GC_TYPE_SUBSECTION);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if((event.GetId() == EventIDs::menu_add_subsection) ||
          (event.GetId() == EventIDs::menu_format_subsection) ||
          (event.GetId() == EventIDs::popid_insert_subsection)){
    type = GC_TYPE_SUBSECTION;
  }
  else if(event.GetId() == EventIDs::menu_convert_to_subsubsection){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(
                                                             GC_TYPE_SUBSUBSECTION);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if(event.GetId() == EventIDs::menu_convert_to_heading5){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_HEADING5);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if(event.GetId() == EventIDs::menu_convert_to_heading6){
    if (GetWorksheet()->GetActiveCell()) {
      GetWorksheet()->GetActiveCell()->GetGroup()->SetGroupType(GC_TYPE_HEADING6);
      GetWorksheet()->Recalculate();
      GetWorksheet()->RequestRedraw();
    }
  }
  else if((event.GetId() == EventIDs::menu_add_subsubsection) ||
          (event.GetId() == EventIDs::menu_format_subsubsection) ||
          (event.GetId() == EventIDs::popid_insert_subsubsection)){
    type = GC_TYPE_SUBSUBSECTION;
  }
  else if((event.GetId() == EventIDs::menu_add_heading5) ||
          (event.GetId() == EventIDs::menu_format_heading5) ||
          (event.GetId() == EventIDs::popid_insert_heading5)){
    type = GC_TYPE_HEADING5;
  }
  else if((event.GetId() == EventIDs::menu_add_heading6) ||
          (event.GetId() == EventIDs::menu_format_heading6) ||
          (event.GetId() == EventIDs::popid_insert_heading6)){
    type = GC_TYPE_HEADING6;
  }
  else if((event.GetId() == EventIDs::menu_add_pagebreak) ||
          (event.GetId() == EventIDs::menu_format_pagebreak)) {
    GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_configuration, GC_TYPE_PAGEBREAK),
                                  GetWorksheet()->GetHCaret());
    GetWorksheet()->Recalculate();
    return;}
  else if((event.GetId() == EventIDs::menu_insert_image) ||
          (event.GetId() == EventIDs::menu_format_image)){
      wxString file = wxFileSelector(
                                     _("Insert Image"), m_lastPath, wxEmptyString, wxEmptyString,
                                     _("Image files") + " (*.png, *.jpg," +
#ifdef wwxUSE_LIBWEB
                                         "*.webp," +
#endif

                                       "*.bmp, *.xpm, *.gif, *.svg, "
                                       "*.svgz)|*.png;*.jpg;" +
#ifdef wwxUSE_LIBWEB
                                         "*.webp;" +
#endif
                                       "*.bmp;*.xpm;*.gif;*.svg;*.svgz",
                                     wxFD_OPEN);
      if (file != wxEmptyString)
        GetWorksheet()->OpenHCaret(file, GC_TYPE_IMAGE);
      return;
    }
  else if(event.GetId() == EventIDs::menu_fold_all_cells){
    GetWorksheet()->FoldAll();
    GetWorksheet()->Recalculate();
    // send cursor to the top
    GetWorksheet()->SetHCaret(NULL);
  }
  else if(event.GetId() == EventIDs::menu_unfold_all_cells){
    GetWorksheet()->UnfoldAll();
    GetWorksheet()->Recalculate();
    // refresh without moving cursor
    GetWorksheet()->SetHCaret(GetWorksheet()->GetHCaret());
  }

  CallAfter([this]{GetWorksheet()->SetFocus();});

  if (event.GetId() == EventIDs::menu_insert_previous_input ||
      event.GetId() == EventIDs::menu_insert_previous_output) {
    wxString input;

    if (output == true)
      input = GetWorksheet()->GetOutputAboveCaret();
    else
      input = GetWorksheet()->GetInputAboveCaret();
    if (input != wxEmptyString)
      GetWorksheet()->OpenHCaret(input, type);
  } else if ((event.GetId() == EventIDs::menu_unfold_all_cells) ||
             (event.GetId() == EventIDs::menu_fold_all_cells) ||
             (event.GetId() == EventIDs::menu_convert_to_heading6) ||
             (event.GetId() == EventIDs::menu_convert_to_heading5) ||
             (event.GetId() == EventIDs::menu_convert_to_subsubsection) ||
             (event.GetId() == EventIDs::menu_convert_to_subsection) ||
             (event.GetId() == EventIDs::menu_convert_to_section) ||
             (event.GetId() == EventIDs::menu_convert_to_comment) ||
             (event.GetId() == EventIDs::menu_convert_to_title) ||
             (event.GetId() == EventIDs::menu_convert_to_code)) {
    // don't do anything else
  } else
    GetWorksheet()->OpenHCaret(wxEmptyString, type);
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::ResetTitle(bool saved, bool force) {
  SetRepresentedFilename(GetWorksheet()->m_currentFile);
  OSXSetModified((saved != m_fileSaved) || (force));

  if ((saved != m_fileSaved) || (force)) {
    m_fileSaved = saved;
    if (GetWorksheet()->m_currentFile.Length() == 0) {
#ifndef __WXOSX__
      if (saved)
        SetTitle(wxString::Format(
                                  _("wxMaxima %s (%s) "), WXMAXIMA_VERSION,
                                  wxPlatformInfo::Get().GetOperatingSystemDescription()) +
                 _("[ unsaved ]"));
      else
        SetTitle(wxString::Format(
                                  _("wxMaxima %s (%s) "), WXMAXIMA_VERSION,
                                  wxPlatformInfo::Get().GetOperatingSystemDescription()) +
                 _("[ unsaved* ]"));
#endif
    } else {
      wxString name, ext;
      wxFileName::SplitPath(GetWorksheet()->m_currentFile, NULL, NULL, &name,
                            &ext);
#ifndef __WXOSX__
      if (m_fileSaved)
        SetTitle(wxString::Format(
                                  _("wxMaxima %s (%s) "), WXMAXIMA_VERSION,
                                  wxPlatformInfo::Get().GetOperatingSystemDescription()) +
                 wxS(" [ ") + name + wxS(".") + ext + wxS(" ]"));
      else
        SetTitle(wxString::Format(
                                  _("wxMaxima %s (%s) "), WXMAXIMA_VERSION,
                                  wxPlatformInfo::Get().GetOperatingSystemDescription()) +
                 wxS(" [ ") + name + wxS(".") + ext + wxS("* ]"));
#else
      SetTitle(name + wxS(".") + ext);
#endif
    }
#if defined __WXOSX__
#if defined __WXOSX_COCOA__
    OSXSetModified(!saved);
    if (GetWorksheet()->m_currentFile != wxEmptyString)
      SetRepresentedFilename(GetWorksheet()->m_currentFile);
#else
    WindowRef win = (WindowRef)MacGetTopLevelWindowRef();
    SetWindowModified(win, !saved);
    if (GetWorksheet()->m_currentFile != wxEmptyString) {
      FSRef fsref;
      wxMacPathToFSRef(GetWorksheet()->m_currentFile, &fsref);
      HIWindowSetProxyFSRef(win, &fsref);
    }
#endif
#endif
  }
}

///--------------------------------------------------------------------------------
///  Plot Slider
///--------------------------------------------------------------------------------

void wxMaxima::UpdateSlider() {
  if (GetWorksheet()->m_mainToolBar) {
    if (GetWorksheet()->m_mainToolBar->m_plotSlider) {
      if (GetWorksheet()->IsSelected(MC_TYPE_SLIDE)) {
        AnimationCell *cell =
          dynamic_cast<AnimationCell *>(GetWorksheet()->GetSelectionStart());

        GetWorksheet()->m_mainToolBar->UpdateSlider(cell);
      }
    }
  }
}

void wxMaxima::SliderEvent(wxScrollEvent &event) {
  AnimationCell *animation =
    dynamic_cast<AnimationCell *>(GetWorksheet()->GetSelectionStart());

  if (animation != NULL) {
    animation->AnimationRunning(false);
    animation->SetDisplayedIndex(event.GetPosition());

    wxRect rect = animation->GetRect();
    GetWorksheet()->RequestRedraw(rect);
    if (GetWorksheet()->m_mainToolBar)
      GetWorksheet()->m_mainToolBar->UpdateSlider(animation);
  }
}

void wxMaxima::ShowPane(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  int id = event.GetId();

  if (id == EventIDs::menu_pane_hideall)
    wxMaximaFrame::ShowPane(id, true);
  else {
    TogglePaneVisibility(id);

    if ((id == EventIDs::menu_pane_structure) &&
        (IsPaneDisplayed(id)))
      GetWorksheet()->UpdateTableOfContents();
  }
}

void wxMaxima::OnChar(wxKeyEvent &event) {
  GetWorksheet()->OnChar(event);
  event.Skip();
}

void wxMaxima::OnKeyDown(wxKeyEvent &event) {
  GetWorksheet()->OnKeyDown(event);
  event.Skip();
}

void wxMaxima::NetworkDClick(wxCommandEvent &WXUNUSED(event)) {
  ToggleXMLInspector();
}

void wxMaxima::MaximaDClick(wxCommandEvent &WXUNUSED(event)) {
  GetWorksheet()->ScrollToCaret();
}

void wxMaxima::StatusMsgDClick(wxCommandEvent &WXUNUSED(event)) {
  ToggleLogPane();
}

void wxMaxima::HistoryDClick(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();
  GetWorksheet()->OpenHCaret(event.GetString(), GC_TYPE_CODE);
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::TableOfContentsSelection(wxListEvent &event) {
  GroupCell *selection =
    m_tableOfContents->GetCell(event.GetIndex())->GetGroup();

  // We only update the table of contents when there is time => no guarantee
  // that the cell that was clicked at actually still is part of the tree.
  if ((GetWorksheet()->GetTree()) &&
      (GetWorksheet()->GetTree()->Contains(selection))) {
    GetWorksheet()->SetHCaret(selection);
    GetWorksheet()->ScrollToCaret();
    CallAfter([this]{GetWorksheet()->SetFocus();});
  }
}

void wxMaxima::OnFollow(wxCommandEvent &WXUNUSED(event)) {
  GetWorksheet()->CloseAutoCompletePopup();
  GetWorksheet()->OnFollow();
}

wxMaxima::VersionNumber::VersionNumber(const wxString &version)
  : m_major(0), m_minor(0), m_patchlevel(0) {
  wxStringTokenizer tokens(version, wxS("._-~$"));

  if (tokens.HasMoreTokens())
    {
      wxString token = tokens.GetNextToken();
      if(!token.ToLong(&m_major))
        wxLogMessage(_("Cannot interpret version number component %s"),
                     token);
    }
  if (tokens.HasMoreTokens()) //-V581
    {
      wxString token = tokens.GetNextToken();
      if(!token.ToLong(&m_minor))
        wxLogMessage(_("Cannot interpret version number component %s"),
                     token);
    }
  if (tokens.HasMoreTokens()) //-V581
    {
      wxString token = tokens.GetNextToken();
      if(!token.ToLong(&m_patchlevel))
        wxLogMessage(_("Cannot interpret version number component %s"),
                     token);
    }
}

bool operator<(const wxMaxima::VersionNumber &v1,
               const wxMaxima::VersionNumber &v2) {
  if (v1.Major() < v2.Major())
    return true;
  if (v1.Major() > v2.Major())
    return false;
  if (v1.Minor() < v2.Minor())
    return true;
  if (v1.Minor() > v2.Minor())
    return false;
  if (v1.Patchlevel() < v2.Patchlevel())
    return true;
  return false;
}

bool operator>(const wxMaxima::VersionNumber &v1,
               const wxMaxima::VersionNumber &v2) {
  if (v1.Major() > v2.Major())
    return true;
  if (v1.Major() < v2.Major())
    return false;
  if (v1.Minor() > v2.Minor())
    return true;
  if (v1.Minor() < v2.Minor())
    return false;
  if (v1.Patchlevel() > v2.Patchlevel())
    return true;
  return false;
}

/***
 * Checks the file http://wxMaxima-developers.github.io/wxmaxima/version.txt to
 * see if there is a newer version available.
 */
void wxMaxima::CheckForUpdates(bool reportUpToDate) {
  wxHTTP connection;
  connection.SetHeader(wxS("Content-type"), wxS("text/html; charset=utf-8"));
  connection.SetTimeout(2);

  if (!connection.Connect(wxS("wxMaxima-developers.github.io"))) {
    LoggingMessageBox(_("Can not connect to the web server."), _("Error"),
                      wxOK | wxICON_ERROR);
    return;
  }

  std::unique_ptr<wxInputStream> inputStream = std::unique_ptr<wxInputStream>(
                                                                              connection.GetInputStream(_T("/wxmaxima/version.txt")));

  if (connection.GetError() == wxPROTO_NOERR) {
    wxString version;
    wxStringOutputStream outputStream(&version);
    inputStream->Read(outputStream);

    if (version.StartsWith(wxS("wxmaxima ="))) {
      version = version.Mid(11, version.Length());
      version.Trim(true);
      version.Trim(false);
      VersionNumber myVersion(wxS(WXMAXIMA_VERSION));
      VersionNumber currVersion(version);

      if (myVersion < currVersion) {
        bool visit =
          LoggingMessageBox(wxString::Format(_("You have version %s. The newest version source code is available for is %s.\n\n"
                                               "Select OK to visit the wxMaxima webpage."),
                                             WXMAXIMA_VERSION, version),
                            _("Upgrade"), wxOK | wxCANCEL | wxICON_INFORMATION) == wxOK;

        if (visit)
          wxLaunchDefaultBrowser(
                                 wxS("https://wxMaxima-developers.github.io/wxmaxima"));
      } else if (reportUpToDate)
        LoggingMessageBox(_("Your version of wxMaxima is up to date."),
                          _("Upgrade"), wxOK | wxICON_INFORMATION);
    } else {
      LoggingMessageBox(
                        _("Unable to interpret the version info I got from "
                          "http://wxMaxima-developers.github.io/wxmaxima/version.txt: ") +
                        version,
                        _("Upgrade"), wxOK | wxICON_INFORMATION);
    }
  } else {
    LoggingMessageBox(_("Can not download version info."), _("Error"),
                      wxOK | wxICON_ERROR);
  }
  connection.Close();
}

int wxMaxima::SaveDocumentP() {
  wxString file, ext;
  if (GetWorksheet()->m_currentFile.IsEmpty()) {
    // Check if we want to save modified untitled documents on exit
    if (!m_configuration.SaveUntitled())
      return wxID_NO;
  }
  else {
    if (!m_configuration.AutoSaveAsTempFile())
      {
        if (SaveFile())
          return wxID_NO;
      }
  }

#if defined __WXOSX__
  file = GetTitle();
#else
  file = _("unsaved");
#endif
  wxFileName::SplitPath(GetWorksheet()->m_currentFile, NULL, NULL, &file, &ext);
  file += wxS(".") + ext;
  LoggingMessageDialog dialog(
                              this,
                              wxString::Format(
                                               _("Do you want to save the changes you made in the document \"%s\"?"),
                                               file),
                              "wxMaxima", wxCENTER | wxYES_NO | wxCANCEL);

  dialog.SetExtendedMessage(
                            _("Your changes will be lost if you don't save them."));
  dialog.SetYesNoCancelLabels(_("Save"), _("Don't save"), _("Cancel"));

  return dialog.ShowModal();
}

void wxMaxima::OnFocus(wxFocusEvent &event) {
  // We cannot change the focus during an focus event, but can tell the
  // event loop to do so.
  CallAfter(&wxMaxima::PassKeyboardFocus);
  event.Skip();
}
void wxMaxima::PassKeyboardFocus() {
  if (GetWorksheet()) {
    if (m_configuration.LastActiveTextCtrl()) {
      wxLogMessage(_("Forwarding the keyboard focus to a text control"));
      m_configuration.LastActiveTextCtrl()->SetFocus();
    } else {
      wxLogMessage(_("Forwarding the keyboard focus to the worksheet"));
      CallAfter([this]{GetWorksheet()->SetFocus();});
    }
  }
}

void wxMaxima::OnMinimize(wxIconizeEvent &event) {
  GetWorksheet()->WindowActive(!event.IsIconized());
  if (!event.IsIconized())
    CallAfter([this]{GetWorksheet()->SetFocus();});
  event.Skip();
}

void wxMaxima::ChangeCellStyle(wxCommandEvent &WXUNUSED(event)) {
  if ((GetWorksheet() == NULL) || (GetWorksheet()->m_mainToolBar == NULL))
    return;
  GetWorksheet()->CloseAutoCompletePopup();

  if (GetWorksheet()->GetActiveCell()) {
    GroupCell *group = GetWorksheet()->GetActiveCell()->GetGroup();
    switch (group->GetGroupType()) {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_TITLE:
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
    case GC_TYPE_SUBSUBSECTION:
    case GC_TYPE_HEADING5:
    case GC_TYPE_HEADING6:
      GetWorksheet()->SetCellStyle(group,
                                GetWorksheet()->m_mainToolBar->GetCellType());
      break;
    default:
      break;
    }
    GetWorksheet()->NumberSections();
  } else
    GetWorksheet()->m_mainToolBar->SetDefaultCellStyle();
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

wxString wxMaxima::EscapeFilenameForShell(wxString name)
{
#ifdef __WXMSW__
  name.Replace("\\", "/");
#endif
  name.Replace("\\", "\\\\");
  name.Replace("\"", "\\\"");
  return "\"" + name + "\"";
}

bool wxMaxima::m_exitOnError = false;
wxString wxMaxima::m_extraMaximaArgs;
int wxMaxima::m_exitCode = 0;
// wxRegEx  wxMaxima::m_outputPromptRegEx(wxS("<lbl>.*</lbl>"));
wxRegEx wxMaxima::m_funRegEx(
                             wxS("^ *([[:alnum:]%_]+) *\\(([[:alnum:]%_,[[.].] ]*)\\) *:="));
wxRegEx wxMaxima::m_varRegEx(wxS("^ *([[:alnum:]%_]+) *:"));
wxRegEx wxMaxima::m_blankStatementRegEx(
                                        wxS("(^;)|((^|;)(((\\/\\*.*\\*\\/)?([[:space:]]*))+;)+)"));
wxRegEx wxMaxima::m_sbclCompilationRegEx(wxS("; compiling (.* \\.*)"));
wxRegEx
wxMaxima::m_gnuplotErrorRegex(wxS("\".*\\.gnuplot\", line [0-9][0-9]*: "));
wxString wxMaxima::m_promptPrefix(wxS("<PROMPT>"));
const wxString wxMaxima::m_promptSuffix(wxS("</PROMPT>"));
wxString wxMaxima::m_mathPrefix1(wxS("<mth>"));
wxString wxMaxima::m_mathPrefix2(wxS("<math>"));
//wxString wxMaxima::m_mathSuffix1(wxS("</mth>"));
//wxString wxMaxima::m_mathSuffix2(wxS("</math>"));
wxString wxMaxima::m_firstPrompt(wxS("(%i1) "));
wxMaxima::VarReadFunctionHash wxMaxima::m_variableReadActions;
wxMaxima::VarUndefinedFunctionHash wxMaxima::m_variableUndefinedActions;
wxString wxMaxima::maxima_command_line_filename;
wxLogWindow *MyApp::m_logWindow;
int MyApp::m_windowcount = 0;
