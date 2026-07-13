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
#include "MaximaMenuSync.h"
#include "BTextCtrl.h"
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
#include "MaximaVariableUpdates.h"
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
#include "dialogs/DiffFrame.h"
#include "wxMaximaOSDescription.h"
#include "Version.h"
#include "BuildConfig.h"
#include "WXMformat.h"
#include "WXMXformat.h"
#include "wxMathml.h"
#include "wxMaxima.h"
#include <wx/app.h>
#include <wx/apptrait.h>
#include <wx/stdpaths.h>
#ifdef __WXMSW__
#include <wx/msw/registry.h> // wxRegKey, for RegisterWxmxDiffTool()
#endif
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
#if wxCHECK_VERSION(3, 1, 5)
#include <wx/webrequest.h>
#endif
#include "main.h"
#include <list>
#include <functional>
#include <memory>
#include <wx/sstream.h>
#include <wx/url.h>
#include "Configuration.h"

#ifndef __WXMSW__
#include <csignal>
#include <cstring>
#include <sys/types.h>
#include <unistd.h>
#endif

wxDECLARE_APP(MyApp);


#if defined __WXOSX__
#define MACPREFIX "wxMaxima.app/Contents/Resources/"
#endif

// Make the native chrome (menus, toolbars, sidebars, dialogs) follow the
// worksheet's light/dark appearance. Only wxWidgets 3.3+ can drive the OS
// appearance from inside the app; on older wx this is a no-op (the worksheet is
// themed via its style set, the chrome stays whatever the OS gives us).
static void ApplyAppearanceToApp(Configuration::Appearance appearance) {
#if wxCHECK_VERSION(3, 3, 0)
  if (!wxTheApp)
    return;
  wxApp::Appearance a = wxApp::Appearance::System;
  switch (appearance) {
  case Configuration::Appearance::light:
    a = wxApp::Appearance::Light;
    break;
  case Configuration::Appearance::dark:
    a = wxApp::Appearance::Dark;
    break;
  case Configuration::Appearance::followSystem:
    a = wxApp::Appearance::System;
    break;
  }
  wxTheApp->SetAppearance(a);
#else
  (void)appearance;
#endif
}

void wxMaxima::ConfigChanged() {

  ApplyAppearanceToApp(GetConfiguration().GetAppearance());

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
      GetWorksheet()->RequestRecalculation();
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

  if (GetWorksheet() && (GetWorksheet()->GetCurrentFile() != wxEmptyString)) {
    wxString filename(GetWorksheet()->GetCurrentFile());

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
    m_parser(&m_configuration),
    m_maximaError(false), m_menuCommands(*this), m_responseReader(*this),
    m_processManager(*this), m_evaluator(*this), m_fileIO(*this),
    m_outputAppender(*this) {
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT
  wxHandleFatalExceptions();
  wxLogMessage(_("Will try to generate a stack backtrace, if the program ever crashes"));
#endif
  m_processManager.GnuplotCommandName(wxS("gnuplot"));




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
    MyApp::m_logWindow->Show(logwindow_shown);
  }

  m_oldFindString.Clear();
  int findFlags = wxFR_DOWN | wxFR_MATCHCASE | FindReplacePane::wxFR_SEARCH_IN_INPUT | FindReplacePane::wxFR_SEARCH_IN_OUTPUT;
  if (wxConfig::Get()->Read(wxS("Find/Flags"), &findFlags))
    {
      if (!(findFlags & (FindReplacePane::wxFR_SEARCH_IN_INPUT | FindReplacePane::wxFR_SEARCH_IN_OUTPUT)))
        findFlags |= FindReplacePane::wxFR_SEARCH_IN_INPUT | FindReplacePane::wxFR_SEARCH_IN_OUTPUT;
    }
  m_findData.SetFlags(findFlags);
  bool findRegex = false;
  wxConfig::Get()->Read(wxS("Find/RegexSearch"), &findRegex);
  m_findData.SetRegexSearch(findRegex);
  if(GetWorksheet())
    GetWorksheet()->KeyboardInactiveTimer().SetOwner(this,
                                                     KEYBOARD_INACTIVITY_TIMER_ID);
  m_maximaStdoutPollTimer.SetOwner(this, MAXIMA_STDOUT_POLL_ID);

  m_autoSaveTimer.SetOwner(this, AUTO_SAVE_TIMER_ID);
  // Window size/position persistence is handled by wxPersistenceManager
  // (RegisterAndRestore in the constructor); the old EVT_SIZE/EVT_MOVE/
  // EVT_MAXIMIZE handlers only wrote "MainWindowPos/*" config keys nothing
  // ever read - on every resize/move event.

  Bind(wxEVT_TIMER, &wxMaxima::OnTimerEvent, this);

  if(m_wizard)
    {
      m_wizard->GetOKButton()->Bind(wxEVT_BUTTON, &wxMaxima::OnWizardOK, this);
      m_wizard->GetAbortButton()->Bind(wxEVT_BUTTON, &wxMaxima::OnWizardAbort, this);
      m_wizard->GetInsertButton()->Bind(wxEVT_BUTTON, &wxMaxima::OnWizardInsert, this);
      m_wizard->Bind(wxEVT_BUTTON, &wxMaxima::OnWizardHelpButton, this);
    }
#ifdef wxHAS_POWER_EVENTS
  Bind(wxEVT_POWER_SUSPENDED, &wxMaxima::OnPowerEvent, this);
#endif

#if wxUSE_DRAG_AND_DROP
  if(GetWorksheet())
    GetWorksheet()->SetDropTarget(new MyDropTarget(this));
#endif

  StatusMaximaBusy(StatusBar::MaximaStatus::disconnected);

  m_statusBar->GetNetworkStatusElement()->Bind(wxEVT_LEFT_DCLICK, &wxMaxima::NetworkDClick, this);
  m_statusBar->GetMaximaStatusElement()->Bind(wxEVT_LEFT_DCLICK, &wxMaxima::MaximaDClick, this);

  m_statusBar->GetStatusTextElement()->Bind(wxEVT_LEFT_DCLICK, &wxMaxima::StatusMsgDClick, this);

  m_fileToOpen = filename;
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

  if (!m_processManager.StartMaxima())
    StatusText(_("Starting Maxima process failed"));

  CallAfter([this] {
    if (GetWorksheet())
      GetWorksheet()->SetFocus();
  });

  Bind(wxEVT_SCROLL_CHANGED, &wxMaxima::SliderEvent, this);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, wxID_CLOSE);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_check_updates);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_image);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_animation);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_svg);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_emf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_rtf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_text);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_title);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_section);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_subsection);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_subsubsection);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_heading5);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_heading6);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_popup_gnuplot);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_show_cellbrackets);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_print_cellbrackets);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_delete);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_simplify);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_expand);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_solve);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::enable_unicodePane);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_subst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_plot2d);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_plot3d);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_diff);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_integrate);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_float);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_matlab);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_copy_tex);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::EventIDs::popid_copy_text);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_image);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_animation_save);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::popid_animation_start);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_integrate);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_diff);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::button_solve);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::button_solve_ode);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_sum);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_expand);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_factor);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_taylor);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_limit);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_ratsimp);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_trigexpand);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_trigreduce);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_trigsimp);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::button_product);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_radcan);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::button_subst);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::button_plot2);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::button_plot3);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::button_map);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_map);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_map_lambda);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_row);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_col);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_row_list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_col_list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_csv2mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_mat2csv);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_submatrix_rows);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_submatrix_columns);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_multiply);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_exponent);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_hadamard_product);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_hadamard_exponent);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_copymatrix);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_loadLapack);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dgeev_eigenvaluesOnly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dgeev);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zgeev_eigenvaluesOnly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zgeev);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dgeqrf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dgesv);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dgesvd);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::EventIDs::menu_matrix_dgesvd_valuesOnly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dlange_max);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dlange_one);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dlange_inf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_dlange_frobenius);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zlange_max);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zlange_one);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zlange_inf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zlange_frobenius);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_matrix_zheev);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_rectform);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::button_trigrat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_polarform);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, ToolBar::menu_restart_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, wxID_EXIT);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, wxID_ABOUT);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_license);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_changelog);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, wxID_SAVE);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, wxID_SAVEAS);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_load_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_functions);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_variables);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_arrays);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_macros);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_labels);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_myoptions);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_rules);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_aliases);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_structs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_dependencies);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_gradefs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_let_rule_packages);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_PREFERENCES);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_sconsole_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_export_html);
  if(GetWorksheet())
    GetWorksheet()->GetAutocomplete().Bind(NEW_DEMO_FILES_EVENT, &wxMaxima::OnNewDemoFiles, this);
  Bind(TOC_UPDATE_NEEDED_EVENT, &wxMaxima::OnUpdateTOCEvent, this);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, wxID_HELP);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_help_demo_for_command);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_help_maxima_homepage);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_help_tutorials);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_goto_url);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_bug_report);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_build_info);
#ifdef __WXMSW__
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_register_wxmx_difftool);
#endif
  Bind(wxEVT_MENU, &MaximaProcessManager::Interrupt, &m_processManager, EventIDs::menu_interrupt_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, wxID_OPEN);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_batch_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_compare_files);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_ratsimp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_radsimp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_expand);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_expandwrt);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::EventIDs::menu_expandwrt_denom);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_scsimp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_xthru);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_factor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_horner);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_collapse);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_optimize);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_mainvar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_scanmapfactor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_gfactor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_trigsimp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_trigexpand);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_trigreduce);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_rectform);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_demoivre);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_num_out);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_stringdisp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_num_domain);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_to_float);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_rat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_rationalize);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_guess_exact_value);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_to_bfloat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_to_numer);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::popid_special_constant_percent);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::popid_changeasterisk);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::popid_hideasterisk);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_exponentialize);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_invert_mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_determinant);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_rank);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_eigen);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_eigvect);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_adjoint_mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_transpose);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_set_precision);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_set_displayprecision);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_engineeringFormat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_engineeringFormatSetup);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qag);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qags);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qagi);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qawc);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qawf_sin);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qawf_cos);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qawo_sin);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qawo_cos);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qaws1);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qaws2);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qaws3);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qaws4);
  Bind(wxEVT_MENU, &MaximaCommandMenus::NumericalMenu, &m_menuCommands, EventIDs::menu_quad_qagp);

  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_talg);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_tellrat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_modulus);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_allroots);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_bfallroots);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_realroots);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_to_poly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_num);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_ode);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_map_mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_enter_mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_cpoly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_genmatrix);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_lin);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_algsys);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_eliminate);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_clear_var);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_clear_fun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_ivp_1);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_ivp_2);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_bvp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_bvp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_rk);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_fun_def);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_gensym);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_divide);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_gcd);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_lcm);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_continued_fraction);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_partfrac);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_risch);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_integrate);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_laplace);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_ilt);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_diff);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_taylor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_powerseries);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_fourier);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_limit);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_lbfgs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_gen_mat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_gen_mat_lambda);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_sum);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_maximahelp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_wxmaximahelp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_example);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_apropos);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_wxmaxima_uses_help_sidebar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_wxmaxima_uses_help_browser);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_maxima_uses_html_help);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_maxima_uses_internal_help);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_maxima_uses_wxmaxima_help);
  Bind(wxEVT_MENU, &MaximaCommandMenus::HelpMenu, &m_menuCommands, EventIDs::menu_show_tip);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_trigrat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_solve_de);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_atvalue);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_lhs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_rhs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::menu_construct_fraction);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_sum);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_product);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_change_var);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_time);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_factsimp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_factcomb);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_realpart);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_imagpart);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_nouns);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_simpsum);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_subst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_psubst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_ratsubst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_fullratsubst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_at);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_substinpart);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_opsubst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logcontract);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand_false);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand_true);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand_all);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_logexpand_super);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::gp_plot2);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::gp_plot3);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::menu_animationautostart);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::menu_animationframerate);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PlotMenu, &m_menuCommands, EventIDs::menu_plot_format);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_soft_restart);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_dependencies);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_values);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_functions);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_arrays);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_myoptions);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_rules);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_aliases);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_structures);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_labels);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_gradefs);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_props);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_macros);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_kill_let_rule_packages);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_garbage_collect);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_room);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_jumptoerror);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_display);
  Bind(wxEVT_MENU, &MaximaCommandMenus::CalculusMenu, &m_menuCommands, EventIDs::menu_pade);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_add_path);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_uuid);
  Bind(wxEVT_MENU, &MaximaCommandMenus::FileMenu, &m_menuCommands, EventIDs::menu_jump_to_uuid);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_COPY);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_text_from_worksheet);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_tex_from_worksheet);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_matlab_from_worksheet);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_copy_mathml);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_UNDO);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_UNDO);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_REDO);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_REDO);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_texform);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_grind);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_debugmode_lisp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_debugmode_all);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_debugmode_off);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_for);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_while);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_block);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_block_noLocal);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_local);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_return);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_trace);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_lambda);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_quotequote);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_quote);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_quoteblock);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_def_fun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_def_macro);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_def_variable);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_compile);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_paramType);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_structdef);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_structnew);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_structuse);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_saveLisp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_loadLisp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_maximatostring);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringtomaxima);

  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_setposition);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_getposition);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_flush_output);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_flength);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_close);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_opena);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_openr);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_openw);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_printf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_readline);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_readchar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_readbyte);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_writebyte);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_charp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_alphacharp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_alphanumericp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_digitcharp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_constituent);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_uppercasep);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_lowercasep);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_create_ascii);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_cequal);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_cequalignore);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_clessp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_clesspignore);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_cgreaterp);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_cgreaterpignore);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sequal);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sequalignore);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_ascii);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_cint);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_unicode);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_unicode_to_utf8);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_utf8_to_unicode);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_charat);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_charlist);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_simplode);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sinsert);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_eval_string);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_parse_string);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_scopy);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sdowncase);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_slength);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_smake);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_smismatch);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_split);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sposition);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sremove);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_sremovefirst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_tokens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_ssearch);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_ssort);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_ssubstfirst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_strim);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_striml);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_strimr);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_number_to_octets);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_octets_to_number);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_octets_to_string);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_stringproc_string_to_octets);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_load);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_regex_compile);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_regex_match_pos);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_regex_match);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_regex_split);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_subst_first);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_regex_subst);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_sregex_string_to_regex);

  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_load);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_chdir);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_mkdir);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_rmdir);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_getcurrentdirectory);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_copy_file);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_rename_file);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_delete_file);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_getenv);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_directory);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_pathname_directory);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_pathname_name);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_opsyst_pathname_type);

  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_load);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_lang_c);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_lang_fortran);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_lang_ratfor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_to_stdout);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::gentran_to_file);

  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_to_fact);
  Bind(wxEVT_MENU, &MaximaCommandMenus::SimplifyMenu, &m_menuCommands, EventIDs::menu_to_gamma);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PrintMenu, &m_menuCommands, wxID_PRINT);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_ZOOM_IN);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_ZOOM_OUT);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_zoom_80);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_ZOOM_100);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_zoom_120);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_zoom_150);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_zoom_200);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_zoom_300);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_labelwidth1, EventIDs::popid_labelwidth1 + LABELWIDTH_MAX - LABELWIDTH_MIN);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_digits_all);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_digits_all_linebreak);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_digits_20);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_digits_50);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_digits_100);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_labels_autogenerated);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_inputlabels_hide);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_labels_user);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_labels_useronly);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_labels_disable);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_math_as_1D_ASCII);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_math_as_2D_ASCII);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_math_as_2D_UNICODE);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_math_as_graphics);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::internalRepresentation);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::wxMathML);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_noAutosubscript);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_defaultAutosubscript);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_alwaysAutosubscript);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_declareAutosubscript);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_autosubscriptIndividual);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_noAutosubscriptIndividual);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_roundedMatrixParens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_straightMatrixParens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_angledMatrixParens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_squareMatrixParens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_noMatrixParens);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_fullscreen);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_show_logwindow);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, ToolBar::tb_hideCode);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_as_bitmap);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_as_svg);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_as_emf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_as_rtf);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_copy_to_file);
  Bind(wxEVT_TOOL, &MaximaProcessManager::Interrupt, &m_processManager, ToolBar::tb_interrupt);
  Bind(wxEVT_TOOL, &MaximaCommandMenus::FileMenu, &m_menuCommands, ToolBar::tb_animation_startStop);
  Bind(wxEVT_TOOL, &MaximaCommandMenus::FileMenu, &m_menuCommands, ToolBar::tb_animation_start);
  Bind(wxEVT_TOOL, &MaximaCommandMenus::FileMenu, &m_menuCommands, ToolBar::tb_animation_stop);
  Bind(wxEVT_TOOL, &wxMaxima::OnFollow, this, ToolBar::tb_follow);
  Bind(wxEVT_SOCKET, &MaximaProcessManager::ServerEvent, &m_processManager);
  Bind(wxEVT_CLOSE_WINDOW, &wxMaxima::OnClose, this);
  Bind(wxEVT_QUERY_END_SESSION, &wxMaxima::OnClose, this);
  Bind(wxEVT_END_SESSION, &wxMaxima::OnClose, this);
  Bind(wxEVT_END_PROCESS, &MaximaProcessManager::OnMaximaClose, &m_processManager, m_maxima_process_id);
  Bind(wxEVT_END_PROCESS, &MaximaProcessManager::OnGnuplotQueryTerminals, &m_processManager, EventIDs::gnuplot_query_terminals_id);
  Bind(wxEVT_END_PROCESS, &MaximaProcessManager::OnGnuplotClose, &m_processManager, m_gnuplot_process_id);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditInputMenu, &m_menuCommands, EventIDs::popid_edit);
  Bind(wxEVT_MENU, &MaximaEvaluator::EvaluateEvent, &m_evaluator, EventIDs::menu_evaluate);
  Bind(wxEVT_MENU, &wxMaxima::VarReadEvent, this, EventIDs::popid_var_newVar);
  Bind(wxEVT_MENU, &wxMaxima::VarAddAllEvent, this, EventIDs::popid_var_addAll);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_comment);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_section);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_subsection);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_subsubsection);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_heading5);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_heading6);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_title);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_add_pagebreak);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_fold_all_cells);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_unfold_all_cells);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_add_comment);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_add_watch);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_add_watch_label);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_insert_previous_input);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_insert_previous_output);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_autocomplete);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_autocomplete_templates);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_insert_input);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_insert_input);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_history_previous);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_history_next);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_PASTE);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_paste_input);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_CUT);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_SELECTALL);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_comment_selection);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_divide_cell);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_evaluate);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_evaluate_section);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, ToolBar::tb_eval);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, ToolBar::tb_eval_all);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, ToolBar::tb_evaluate_rest);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, ToolBar::tb_evaltillhere);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_merge_cells);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_maxsizechooser);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_resolutionchooser);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_reloadimage);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_change_image);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_Fold);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_Unfold);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_SelectTocChapter);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_EvalTocChapter);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_evaluate_section);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_ToggleTOCshowsSectionNumbers);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_TOCindentation);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_tocLevel1, EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_tocMoveIn);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_tocMoveOut);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_tocdnd);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_fold);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PopupMenu, &m_menuCommands, EventIDs::popid_unfold);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_evaluate_all_visible);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, EventIDs::menu_evaluate_all);
  Bind(wxEVT_MENU, &MaximaCommandMenus::MaximaMenu, &m_menuCommands, ToolBar::tb_evaltillhere);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_create_from_elements);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_create_from_rule);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_create_from_list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_actual_values_storage);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_sort);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_length);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_push);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_pop);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_reverse);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_first);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_last);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_rest);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_restN);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_lastn);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_nth);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_map);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_use_actual_values);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_as_function_arguments);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_extract_value);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_do_for_each_element);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_remove_duplicates);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_remove_element);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_append_item_start);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_append_item_end);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_append_list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_interleave);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_list2matrix);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_matrix2list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list_create_from_args);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_csv2list);
  Bind(wxEVT_MENU, &MaximaCommandMenus::ListMenu, &m_menuCommands, EventIDs::menu_list2csv);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_2d);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_2d);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_3d);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_3d);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_fgcolor);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_fgcolor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_fillcolor);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_fillcolor);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_title);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_title);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_key);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_key);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_explicit);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_explicit);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_implicit);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_implicit);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_parametric);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_parametric);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_points);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_points);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_axis);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_axis);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_contour);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_contour);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_accuracy);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_accuracy);
  Bind(wxEVT_MENU, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_grid);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::DrawMenu, &m_menuCommands, EventIDs::menu_draw_grid);
  Bind(wxEVT_IDLE, &wxMaxima::OnIdle, this);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_remove_output);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_hide_tooltipMarker);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::popid_hide_tooltipMarkerForThisMessage);
  Bind(wxEVT_MENU, &wxMaxima::OnRecentDocument, this, EventIDs::menu_recent_document_0, EventIDs::menu_recent_document_0 + EventIDs::NumberOfRecentFiles - 1);
  Bind(wxEVT_MENU, &wxMaxima::OnRecentPackage, this, EventIDs::menu_recent_package_0, EventIDs::menu_recent_package_0 + EventIDs::NumberOfRecentFiles - 1);
  Bind(wxEVT_MENU, &wxMaxima::OnUnsavedDocument, this, EventIDs::menu_unsaved_document_0, EventIDs::menu_unsaved_document_0 + EventIDs::NumberOfRecentFiles - 1);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_insert_image);
  for(const auto &[paneId, name]: GetSidebarNames())
    Bind(wxEVT_MENU, &wxMaxima::ShowPane, this, paneId);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, EventIDs::menu_pane_toolbar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_auto_answer);
  Bind(wxEVT_MENU, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::popid_never_autoanswer);
  Bind(wxEVT_LISTBOX_DCLICK, &wxMaxima::HistoryDClick, this, history_ctrl_id);
  Bind(wxEVT_LIST_ITEM_ACTIVATED, &wxMaxima::TableOfContentsSelection, this, structure_ctrl_id);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_histogram);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_piechart);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_scatterplot);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_barsplot);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_boxplot);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_mean);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_median);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_var);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_dev);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_tt1);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_tt2);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_tnorm);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_linreg);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_lsquares);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_readm);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::MatrixMenu, &m_menuCommands, EventIDs::menu_stats_enterm);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::StatsMenu, &m_menuCommands, EventIDs::menu_stats_subsample);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_title);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_text);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_heading6);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_heading5);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_subsubsection);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_subsection);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_section);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_pagebreak);
  Bind(wxEVT_BUTTON, &MaximaCommandMenus::InsertMenu, &m_menuCommands, EventIDs::menu_format_image);
  Bind(wxEVT_CHAR, &wxMaxima::OnChar, this);
  Bind(wxEVT_KEY_DOWN, &wxMaxima::OnKeyDown, this);
  Bind(wxEVT_CHOICE, &wxMaxima::ChangeCellStyle, this, ToolBar::tb_changeStyle);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EditMenu, &m_menuCommands, wxID_FIND);
  Bind(wxEVT_FIND, &wxMaxima::OnFind, this);
  Bind(wxEVT_FIND_NEXT, &wxMaxima::OnFind, this);
  Bind(wxEVT_FIND_REPLACE, &wxMaxima::OnReplace, this);
  Bind(wxEVT_FIND_REPLACE_ALL, &wxMaxima::OnReplaceAll, this);
  Bind(wxEVT_SET_FOCUS, &wxMaxima::OnFocus, this);
  Bind(wxEVT_ICONIZE, &wxMaxima::OnMinimize, this);
  Bind(SYMBOLADDEVENT, &wxMaxima::OnSymbolAdd, this);
  Bind(wxEVT_MENU, &wxMaxima::ReplaceSuggestion, this, EventIDs::popid_suggestion1, EventIDs::popid_suggestion1 + EventIDs::NumberOfSuggestions - 1);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_real);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_imaginary);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_complex);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_additive);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_alphabetic);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_bindtest);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_antisymmetric);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_commutative);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_symmetric);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_constant);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_even);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_odd);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_evenfun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::EquationsMenu, &m_menuCommands, EventIDs::popid_property_atvalue);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_oddfun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_increasing);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_decreasing);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_integer);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_noninteger);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_integervalued);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_lassociative);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_rassociative);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_linear);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_mainvar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_multiplicative);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_nary);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_nonarray);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_nonscalar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_scalar);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_noun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_outative);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_posfun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_rational);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_irrational);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_greaterThan);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_evfun);
  Bind(wxEVT_MENU, &MaximaCommandMenus::PropertiesMenu, &m_menuCommands, EventIDs::popid_property_evflag);
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
  AuiManagerUpdate();
}

#ifdef wxHAS_POWER_EVENTS
void wxMaxima::OnPowerEvent(wxPowerEvent &event) {
  m_fileIO.AutoSave();
  event.Skip();
}
#endif

void wxMaxima::OnNewDemoFiles(wxCommandEvent &WXUNUSED(event))
{
  if(!GetWorksheet())
    return;

  m_demoFilesIDs.clear();
  while(m_demo_sub->GetMenuItemCount() > 0)
      m_demo_sub->Delete(m_demo_sub->FindItemByPosition(0));

  auto filesList = GetWorksheet()->GetAutocomplete().GetDemoFilesList();
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
              for(const auto &demoFile : subMenuContents)
                {
                  wxWindowID id = wxWindow::NewControlId();
                  m_demoFilesIDs[id] = demoFile;
                  subMenu->Append(id, demoFile);
                }
              m_demo_sub->Append(wxWindow::NewControlId(),
                                 subMenuContents.front() + wxS("-") + subMenuContents.back(),
                                 subMenu);
              subMenu->Bind(wxEVT_MENU, &MaximaCommandMenus::OnDemoFileMenu, &m_menuCommands);
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


void wxMaxima::StartAutoSaveTimer() {
  m_autoSaveTimer.StartOnce(60000 * m_configuration.AutosaveMinutes());
}

// Kill a child process wxMaxima launched (a gnuplot process) if it is still
// running, then sever the wxProcess<->wxMaxima bond so a late termination event
// is not delivered to the destroyed wxMaxima.
//
// Plain Detach() (what this used to do) leaves the OS process running. That is a
// leak in its own right, and it bites hard in --batch/--pipe runs: the gnuplot
// terminal-capability query is launched asynchronously at startup, so on a fast
// batch shutdown it is often still alive - and, having inherited wxMaxima's
// stdout/stderr, a surviving gnuplot keeps that pipe open, so the parent reading
// it (e.g. the CI's ctest) never sees EOF and hangs until its timeout. So kill
// it first, the same way KillMaxima kills Maxima.
static void KillAndDetachProcess(wxProcess *&process) {
  if (!process)
    return;
  long pid = process->GetPid();
  // Detach FIRST, then kill by pid. Detach() severs the wxProcess<->wxMaxima
  // bond and hands the wxProcess its own lifetime (it self-deletes when the OS
  // process ends). Killing first and detaching afterwards races that
  // termination handling: the wxProcess may already have self-destructed by the
  // time we call Detach() on it -> use-after-free (this crashed batch/multiwindow
  // teardown). After Detach() we must not touch `process` again, so drop our
  // reference before issuing the kill.
  process->Detach();
  process = NULL;
  if ((pid > 0) && wxProcess::Exists(pid)) {
#ifdef __WINDOWS__
    // As in KillMaxima: taskkill /T for the whole tree, and wxEXEC_NOEVENTS so
    // the synchronous wait does not re-enter the event loop during teardown.
    wxArrayString out, err;
    wxExecute(wxString::Format("taskkill /PID %ld /F /T", pid), out, err,
              wxEXEC_SYNC | wxEXEC_NOEVENTS);
#else
    wxProcess::Kill(pid, wxSIGTERM, wxKILL_CHILDREN);
    if (wxProcess::Exists(pid))
      wxProcess::Kill(pid, wxSIGKILL, wxKILL_CHILDREN);
#endif
  }
}

wxMaxima::~wxMaxima() {
  // The gnuplot processes wxMaxima started must not outlive it: kill any that
  // are still running (and sever the bond so a late event is not delivered to
  // the destroyed wxMaxima). See KillAndDetachProcess above.
  KillAndDetachProcess(m_gnuplotTerminalQueryProcess);
  KillAndDetachProcess(m_gnuplotProcess);

  // Kill maxima
  m_processManager.KillMaxima(false);


  // In debug mode: Create a file describing what we know about maxima commands
  if(m_configuration.GetDebugmode() && (!Dirstructure::Get()->UserConfDir().IsEmpty()))
    {
      std::unordered_map<wxString, std::int_fast8_t, wxStringHash> knownWords;
      for(const auto &i : GetWorksheet()->GetAutocomplete().GetSymbolList())
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

      wxString knownSymbolsFile =
        Dirstructure::Get()->UserConfDir() + "knownSymbols.txt";
      wxFile fil(knownSymbolsFile, wxFile::write);
      // The known-symbols list is only an autocomplete convenience. If the config
      // dir isn't writable (e.g. a custom MAXIMA_USERDIR that doesn't exist) just
      // skip caching it -- writing to an unopened file would assert.
      if (!fil.IsOpened())
        wxLogMessage(_("Cannot cache the list of known symbols to %s"),
                     knownSymbolsFile);
      else {
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
    }

  // Allow the operating system to keep the clipboard's contents even after we
  // exit - if that option is supported by the OS.
  if (wxTheClipboard->Open()) {
    wxTheClipboard->Flush();
    wxTheClipboard->Close();
  }
  if (m_fileSaved)
    RemoveTempAutosavefile();

  // This window is still counted among the top-level windows while its close
  // event is being handled, so CountWindows() == 1 means it is the last
  // wxMaxima window.
  wxLogMessage("Window count (before closing the current window): %zu",
               wxMaximaFrame::CountWindows());
  if (wxMaximaFrame::CountWindows() == 1) {
    // Save the current state of the log window (shown/hidden) and, since the
    // last wxMaxima window is going away, dispose of the log window itself.
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
            m_wxmax->m_fileIO.OpenFile(file);
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


// The console output appenders (ConsoleAppend / DoConsoleAppend /
// DoRawConsoleAppend) were moved into MaximaOutputAppender (m_outputAppender).

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


///--------------------------------------------------------------------------------
///  Socket stuff
///--------------------------------------------------------------------------------


/*!
 * ServerEvent is triggered when maxima connects to the socket server.
 */



///--------------------------------------------------------------------------------
///  Maxima process stuff
///--------------------------------------------------------------------------------







///--------------------------------------------------------------------------------
///  Dealing with stuff read from the socket
///--------------------------------------------------------------------------------







/***
 * Checks if maxima displayed a new prompt.
 */

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
      if (GetWorksheet() && (GetWorksheet()->GetEvaluationQueue().Empty()))
        StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    }
    m_CWD = workingDirectory;
  }
}


// OpenWXMFile
// Clear document (if clearDocument == true), then insert file


wxRegEx wxMaxima::m_xmlOpeningTagName(wxS(".*<([a-zA-Z0-9_]*)[ >].*"));
wxRegEx wxMaxima::m_xmlOpeningTag(wxS("<[^/].*>"));




std::unique_ptr<GroupCell>
wxMaxima::CreateTreeFromXMLNode(wxXmlNode *xmlcells,
                                const wxString &wxmxfilename) {
  // Show a busy cursor as long as we export a .gif file (which might be a
  // lengthy action).
  wxBusyCursor crs;

  MathParser mp(&m_configuration, wxmxfilename);
  return mp.CreateTreeFromXMLNode(xmlcells);
}

wxString wxMaxima::EscapeForLisp(wxString str) {
  str.Replace(wxS("\\"), wxS("\\\\"));
  str.Replace(wxS("\""), wxS("\\\""));
  return (str);
}

void wxMaxima::ExitAfterEval(bool exitaftereval) {
  // We just left batch mode (an error dropped us into interactive use), so now
  // it's worth fetching the autocompletion symbols / variable list and compiling
  // the manual anchors -- interactive-only conveniences we skipped while batching.
  // (m_evaluator.AbortOnError() only calls this when we are NOT going to --exit-on-error, so
  // we really are becoming interactive here.)
  if (m_exitAfterEval && !exitaftereval) {
    // If we leave batch mode we want to have the autocompletion symbols
    // and the variable list.
    m_evaluator.SendMaxima(wxS(":lisp-quiet (setf *wx-defer-queries* nil) ")
               wxS("(wxPrint_autocompletesymbols) ")
               wxS("(wx-print-variables) ")
               wxS("(wx-print-gui-variables)\n"));
    // We are becoming interactive, so now the manual anchors (for help /
    // autocomplete) are worth compiling -- we skipped this in batch mode.
    if (GetWorksheet() && !GetWorksheet()->GetMaximaDocDir().IsEmpty())
      GetWorksheet()->LoadHelpFileAnchors(GetWorksheet()->GetMaximaDocDir(),
                                          m_configuration.GetMaximaVersion());
  }
  m_exitAfterEval = exitaftereval;
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
      // Deferred: GetCommand runs inside StartMaxima, which can itself run
      // under a wxProcess-termination event (restart after a crash); no
      // modal dialogs inside event handlers.
      CallAfter([]{
        LoggingMessageBox(
                          _("Can not start Maxima. The most probable cause is that Maxima "
                            "isn't installed (it can be downloaded from "
                            "https://maxima.sourceforge.io) or in wxMaxima's config dialogue "
                            "the setting for Maxima's location is wrong."),
                          _("Warning"), wxOK | wxICON_EXCLAMATION);
      });
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
            wxExecute(
                command,
                wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER);
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
        std::vector<char *>argv;
        wxCharBuffer commandnamebuffer = Configuration::FindProgram(m_configuration.HelpBrowserUserLocation()).mb_str();
        wxCharBuffer urlbuffer = uri.mb_str();
        argv.push_back(commandnamebuffer.data());
        argv.push_back(urlbuffer.data());
        argv.push_back(NULL);
        wxExecute(argv.data(),
                  wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER);
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
      if (!m_fileToOpen.IsEmpty()) {
        // Clear the pending filename *before* opening it. m_fileIO.OpenFile() can pump
        // the event loop (Maxima startup, recalculation, …); if m_fileToOpen
        // were still set then, a re-entrant OnIdle would open the same file a
        // second time and the worksheet would show its contents twice. This was
        // visible on Windows when opening a .wxmx by double-clicking it.
        const wxString fileToOpen = m_fileToOpen;
        m_fileToOpen = wxEmptyString;
        m_openInitialFileError = !m_fileIO.OpenFile(fileToOpen);
        // A batch run whose input file won't open has nothing to do and should
        // report failure. (m_openInitialFileError is cleared again a few idle
        // cycles later, so the exit code has to be latched here.)
        if (m_openInitialFileError && m_exitAfterEval)
          m_exitCode = 1;
        event.RequestMore();
        return;
      }
      else {
        if (m_evalOnStartup) {
          wxLogMessage(_("Starting evaluation of the document"));
          m_evalOnStartup = false;
          GetWorksheet()->AddDocumentToEvaluationQueue();
          EvaluationQueueLength(
                                GetWorksheet()->GetEvaluationQueue().Size(),
                                GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
          m_evaluator.TriggerEvaluation();
          event.RequestMore();
          return;
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
        }
        m_openInitialFileError = false;
      }
    }
    
    if (m_evalOnStartup && m_ready && (m_fileToOpen.IsEmpty())) {
      wxLogMessage(_("Starting evaluation of the document"));
      m_evalOnStartup = false;
      EvaluationQueueLength(
                            GetWorksheet()->GetEvaluationQueue().Size(),
                            GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
      m_evaluator.TriggerEvaluation();
      event.RequestMore();
      return;    
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
                                       !(m_findData.GetFlags() & wxFR_MATCHCASE),
                                       !!(m_findData.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                       !!(m_findData.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT));
        else
          GetWorksheet()->FindIncremental_RegEx(m_findData.GetFindString(),
                                             m_findData.GetFlags() & wxFR_DOWN,
                                             !!(m_findData.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                             !!(m_findData.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT));
        GetWorksheet()->RequestRedraw();
        m_oldFindFlags = GetWorksheet()->m_findDialog->GetData()->GetFlags();
        m_oldFindString = GetWorksheet()->m_findDialog->GetData()->GetFindString();
        event.RequestMore();
        return;
      }

    }
  }

  GetWorksheet()->AdjustSize();
    
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
      m_statusBar->SetStatusText(GetLeftStatusText());
  }

  if (HasPendingStatusText() && (!GetWorksheet()->StatusTextHas())) {
    m_statusBar->SetStatusText(GetLeftStatusText());

    ClearPendingStatusText();

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

  if (IsPerformanceMonitorShown())
    m_performanceSidebar->UpdateContents();

  if (m_exitAfterEval && GetWorksheet()->GetEvaluationQueue().Empty() &&
      m_fileToOpen.IsEmpty() && (!m_evalOnStartup))
    {
      // SaveFile is now a no-op when the session has no file name and we are
      // non-interactive (a failed initial load leaves exactly that state); the
      // error exit code for that case is set where the load fails.
      m_fileIO.SaveFile(false);
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
  m_evaluator.TriggerEvaluation();
  GetWorksheet()->RequestRedraw();
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------


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
      m_MenuBar->CheckItem(pane.first, IsPaneDisplayed(pane.first));

  bool hidecode = !(m_configuration.ShowCodeCells());
  m_MenuBar->CheckItem(ToolBar::tb_hideCode, hidecode);

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
  switch (StatusMaximaBusy()) {
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
              (!GetWorksheet()->GetEvaluationQueue().IsLastInQueue(group))))
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


void wxMaxima::OnUpdateTOCEvent(wxCommandEvent &WXUNUSED(event))
{
  m_scheduleUpdateToc = true;
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
  long long maximaJiffies = 0;
  bool foundAny = false;

  wxDir dir(wxS("/proc"));
  if (dir.IsOpened()) {
    wxString filename;
    bool cont = dir.GetFirst(&filename, wxEmptyString, wxDIR_DIRS);
    while (cont) {
      long pid;
      if (filename.ToLong(&pid)) {
        wxString statFileName = wxString::Format(wxS("/proc/%li/stat"), pid);
        wxFileInputStream input(statFileName);
        if (input.IsOk()) {
          wxTextInputStream text(input, wxS('\t'), wxConvAuto(wxFONTENCODING_UTF8));
          wxString line = text.ReadLine();
          
          int rparen = line.Find(wxS(')'), true);
          if (rparen != wxNOT_FOUND && rparen + 2 < (int)line.Length()) {
            wxString rest = line.Mid(rparen + 2);
            wxStringTokenizer tokens(rest, wxS(" "));
            
            if (tokens.CountTokens() >= 13) {
              long ppid = 0, pgrp = 0;
              tokens.GetNextToken().ToLong(&ppid);
              tokens.GetNextToken().ToLong(&pgrp);
              
              if (pid == m_pid || pgrp == m_pid || ppid == m_pid) {
                foundAny = true;
                for (int i = 3; i < 11; i++) {
                  tokens.GetNextToken();
                }
                long utime = 0, stime = 0;
                tokens.GetNextToken().ToLong(&utime);
                tokens.GetNextToken().ToLong(&stime);
                maximaJiffies += utime + stime;
              }
            }
          }
        }
      }
      cont = dir.GetNext(&filename);
    }
  }

  return foundAny ? maximaJiffies : -1;
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
    m_responseReader.ReadStdErr();

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
    if ((!GetWorksheet()->KeyboardInactiveTimer().IsRunning()) &&
        (!m_autoSaveTimer.IsRunning())) {
      m_fileIO.AutoSave();
      StartAutoSaveTimer();
    }
    break;
  }
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
                                     !(event.GetFlags() & wxFR_MATCHCASE),
                                     !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                     !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT)
                                     ))
            LoggingMessageBox(_("No matches found!"));
        }
      else
        {
          if (!GetWorksheet()->FindNext_Regex(event.GetFindString(),
                                           !!(event.GetFlags() & wxFR_DOWN),
                                           !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                           !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT)))
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
                                 !(event.GetFlags() & wxFR_MATCHCASE),
                                 !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                 !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT)))
        LoggingMessageBox(_("No matches found!"));
      else
        GetWorksheet()->UpdateTableOfContents();
    }
  else
    {
      GetWorksheet()->Replace_RegEx(event.GetFindString(), event.GetReplaceString());

      if (!GetWorksheet()->FindNext_Regex(event.GetFindString(),
                                       event.GetFlags() & wxFR_DOWN,
                                       !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                       !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT)))
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
                                !(event.GetFlags() & wxFR_MATCHCASE),
                                !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT));
    }
  else
    count =
      GetWorksheet()->ReplaceAll_RegEx(event.GetFindString(), event.GetReplaceString(),
                                   !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_INPUT),
                                   !!(event.GetFlags() & FindReplacePane::wxFR_SEARCH_IN_OUTPUT));
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
      GetWorksheet()->RequestRecalculation();
      GetWorksheet()->RequestRedraw();
    }
  }
  CallAfter([this]{GetWorksheet()->SetFocus();});
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
                          wxString defaultval9, wxString tooltip9, wxString label10,
                          wxString defaultval10, wxString tooltip10) {
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
                      std::move(tooltip9), std::move(label10), std::move(defaultval10),
                      std::move(tooltip10));
  ShowWizardPane(title);
}

void wxMaxima::OnWizardAbort(wxCommandEvent &WXUNUSED(event)) {
  HideWizardPane();
  BTextCtrl::ForgetLastActive();
}

void wxMaxima::OnWizardOK(wxCommandEvent &event) {
  OnWizardInsert(event);
  OnWizardAbort(event);
  BTextCtrl::ForgetLastActive();
}

void wxMaxima::OnWizardHelpButton(wxCommandEvent &event) {
  ShowMaximaHelp(m_wizard->GetHelpKeyword(event.GetId()));
}

void wxMaxima::OnWizardInsert(wxCommandEvent &WXUNUSED(event)) {
  MenuCommand(m_wizard->GetOutput());
  BTextCtrl::ForgetLastActive();
}


#ifdef __WXMSW__
void wxMaxima::RegisterWxmxDiffTool() {
  // The one program that reliably knows where wxMaxima.exe is -- no matter where
  // the user installed it, and Windows has no package manager to ask -- is
  // wxMaxima itself. So register our own path with the Tortoise clients rather
  // than making them discover it.
  const wxString exe = wxStandardPaths::Get().GetExecutablePath();
  // TortoiseSVN/TortoiseGit substitute %base and %mine with the two revisions of
  // the file (and %bname/%yname with the tab captions); --diff is wxMaxima's
  // side-by-side comparison mode.
  const wxString command =
    wxS("\"") + exe + wxS("\" --diff \"%base\" \"%mine\"");

  // Both clients keep their per-extension external diff tools as string VALUES
  // (named by the extension) under a "DiffTools" key in HKEY_CURRENT_USER, so no
  // administrator rights are needed.
  const wxString products[] = {wxS("TortoiseSVN"), wxS("TortoiseGit")};
  wxString done, failed;
  for (const wxString &product : products) {
    wxRegKey key(wxRegKey::HKCU,
                 wxS("Software\\") + product + wxS("\\DiffTools"));
    if (key.Create(/*bOkIfExists=*/true) && key.SetValue(wxS(".wxmx"), command))
      done += wxS("\n    • ") + product;
    else
      failed += wxS("\n    • ") + product;
  }

  wxString message;
  if (!done.IsEmpty())
    message += wxString::Format(
      _("wxMaxima is now the .wxmx diff tool for:%s\n\nOpen a .wxmx file's "
        "\"Diff\" in TortoiseSVN/TortoiseGit to compare it side by side."),
      done);
  if (!failed.IsEmpty())
    message += wxString::Format(
      _("%sCould not write the setting for:%s\n(Is the client installed?)"),
      done.IsEmpty() ? wxString() : wxString(wxS("\n\n")), failed);

  LoggingMessageBox(message, _("Tortoise diff tool"),
                    wxOK | (failed.IsEmpty() ? wxICON_INFORMATION : wxICON_WARNING));
}

void wxMaxima::RepairFileAssociations() {
  // After a wxMaxima update the file association frequently still points at the
  // *old* install: ".wxmx"/".wxm"/".mac" map to a stable ProgID ("wxmaxima.exe")
  // whose shell\open\command holds an absolute path the update did not refresh,
  // so Windows "forgets" the program. The running wxMaxima is the authority on
  // its own location, so it re-points that command at itself -- per-user
  // (HKCU\Software\Classes, no admin), which shadows the stale machine entry.
  const wxString exe = wxStandardPaths::Get().GetExecutablePath();
  if (exe.IsEmpty())
    return;
  const wxString progId = wxS("wxmaxima.exe"); // the long-standing wxMaxima ProgID
  const wxString wantCommand = wxS("\"") + exe + wxS("\" \"%1\"");

  // 1. Re-point our ProgID's open command at the running executable -- but only
  //    write when it is actually stale, to avoid needless registry churn.
  {
    wxRegKey cmd(wxRegKey::HKCU,
                 wxS("Software\\Classes\\") + progId + wxS("\\shell\\open\\command"));
    wxString current;
    const bool haveCurrent =
      cmd.Exists() && cmd.QueryValue(wxEmptyString, current);
    if ((!haveCurrent || (current != wantCommand)) && cmd.Create(true)) {
      cmd.SetValue(wxEmptyString, wantCommand);
      wxLogMessage(_("Re-pointed the .wxmx file association at %s"), exe);
    }
  }

  // 2. Make each extension resolve to our ProgID -- but never hijack one the
  //    user deliberately gave to another program: only claim it when it is unset
  //    or already ours. (A per-user "UserChoice" default, which we must not
  //    touch, still wins over this, so this is a safe fallback.)
  const wxString exts[] = {wxS(".wxmx"), wxS(".wxm"), wxS(".mac")};
  for (const wxString &ext : exts) {
    wxRegKey extKey(wxRegKey::HKCU, wxS("Software\\Classes\\") + ext);
    wxString current;
    const bool claimedByOther =
      extKey.Exists() && extKey.QueryValue(wxEmptyString, current) &&
      !current.IsEmpty() && (current != progId);
    if (!claimedByOther && extKey.Create(true))
      extKey.SetValue(wxEmptyString, progId);

    // Always offer wxMaxima in the "Open with" list, even when we did not claim
    // the default (value name = ProgID, empty data, as Windows expects here).
    wxRegKey openWith(wxRegKey::HKCU,
                      wxS("Software\\Classes\\") + ext + wxS("\\OpenWithProgids"));
    if (openWith.Create(true))
      openWith.SetValue(progId, wxEmptyString);
  }
}
#endif



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
        if (!m_fileIO.SaveFile(true)) {
          return false;
        }
      }
      return true;
    }
  } else {
    {
      if(m_fileIO.SaveFile())
        return true;
    }
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return false;
    else {
      if (close == wxID_YES) {
        if (!m_fileIO.SaveFile()) {
          if (!m_fileIO.SaveFile(true))
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



void wxMaxima::OnRecentDocument(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();

  wxString file = m_recentDocuments.Get(event.GetId() - EventIDs::menu_recent_document_0);

  if (SaveNecessary() && ((file.Lower().EndsWith(wxS(".wxmx"))) ||
                          (file.Lower().EndsWith(wxS(".wxm"))))) {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES) {
      if (!m_fileIO.SaveFile())
        return;
    }
  }

  if (wxFileExists(file))
    m_fileIO.OpenFile(file);
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
    GetWorksheet()->UnsavedDocuments().Get(event.GetId() - EventIDs::menu_unsaved_document_0);

  if (file.IsEmpty())
    return;

  if (SaveNecessary() && ((file.Lower().EndsWith(wxS(".wxmx"))) ||
                          (file.Lower().EndsWith(wxS(".wxm"))))) {
    int close = SaveDocumentP();

    if (close == wxID_CANCEL)
      return;

    if (close == wxID_YES) {
      if (!m_fileIO.SaveFile())
        return;
    }
  }

  if (wxFileExists(file)) {
    m_fileIO.OpenWXMXFile(file, GetWorksheet(), true);
    m_tempfileName = file;
    GetWorksheet()->SetCurrentFile(wxEmptyString);
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

  if (GetWorksheet()->GetCurrentFile().IsEmpty())
    return true;

  return !m_fileSaved;
}


void wxMaxima::VarAddAllEvent(wxCommandEvent &WXUNUSED(event)) {
  wxString command = "\n:lisp-quiet (wx-add-all-variables)\n";
  if ((!GetWorksheet()->GetEvaluationQueue().Empty()) || (m_maximaBusy) ||
      (GetWorksheet()->QuestionPending()))
    m_configCommands += command;
  else
    m_evaluator.SendMaxima(command);
}

void wxMaxima::VarReadEvent(wxCommandEvent &WXUNUSED(event)) {
  if(m_variablesPane)
    m_varNamesToQuery = m_variablesPane->GetEscapedVarnames();
  m_evaluator.QueryVariableValue();
}

//! Handle the evaluation event
//
// User tried to evaluate, find out what is the case
// Normally just add the respective groupcells to evaluationqueue
// If there is a special case - eg sending from output section
// of the working group, handle it carefully.

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

void wxMaxima::ReplaceSuggestion(wxCommandEvent &event) {
  int index = event.GetId() - EventIDs::popid_suggestion1;

  EditorCell *editor = GetWorksheet()->GetActiveCell();
  if (editor == NULL)
    return;
  editor->SelectWordUnderCaret(false);
  editor->ReplaceSelection(editor->GetWordUnderCaret(),
                           GetWorksheet()->m_replacementsForCurrentWord.at(index));
}


void wxMaxima::ResetTitle(bool saved, bool force) {
#ifndef __WXOSX__
  static wxString TitlePreambleSep = wxString::Format(
                    _("wxMaxima %s (%s) "), WXMAXIMA_VERSION,
                    wxMaximaOperatingSystemDescription());
#endif
  SetRepresentedFilename(GetWorksheet()->GetCurrentFile());
  OSXSetModified((saved != m_fileSaved) || (force));

  if ((saved != m_fileSaved) || (force)) {
    m_fileSaved = saved;
    if (GetWorksheet()->GetCurrentFile().Length() == 0) {
#ifndef __WXOSX__
      if (saved)
        SetTitle(TitlePreambleSep + _("[ unsaved ]"));
      else
        SetTitle(TitlePreambleSep + _("[ unsaved* ]"));
#endif
    } else {
      wxString name, ext;
      wxFileName::SplitPath(GetWorksheet()->GetCurrentFile(), NULL, NULL, &name,
                            &ext);
#ifndef __WXOSX__
      if (m_fileSaved)
        SetTitle(TitlePreambleSep +
                 wxS(" [ ") + name + wxS(".") + ext + wxS(" ]"));
      else
        SetTitle(TitlePreambleSep +
                 wxS(" [ ") + name + wxS(".") + ext + wxS("* ]"));
#else
      SetTitle(name + wxS(".") + ext);
#endif
    }
#if defined __WXOSX__
#if defined __WXOSX_COCOA__
    OSXSetModified(!saved);
    if (GetWorksheet()->GetCurrentFile() != wxEmptyString)
      SetRepresentedFilename(GetWorksheet()->GetCurrentFile());
#else
    WindowRef win = (WindowRef)MacGetTopLevelWindowRef();
    SetWindowModified(win, !saved);
    if (GetWorksheet()->GetCurrentFile() != wxEmptyString) {
      FSRef fsref;
      wxMacPathToFSRef(GetWorksheet()->GetCurrentFile(), &fsref);
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

void wxMaxima::NetworkDClick(wxMouseEvent &WXUNUSED(event)) {
  ToggleXMLInspector();
}

void wxMaxima::MaximaDClick(wxMouseEvent &WXUNUSED(event)) {
  GetWorksheet()->ScrollToCaret();
}

void wxMaxima::StatusMsgDClick(wxMouseEvent &WXUNUSED(event)) {
  ToggleLogPane();
}

void wxMaxima::HistoryDClick(wxCommandEvent &event) {
  GetWorksheet()->CloseAutoCompletePopup();
  GetWorksheet()->OpenHCaret(event.GetString(), GC_TYPE_CODE);
  CallAfter([this]{GetWorksheet()->SetFocus();});
}

void wxMaxima::TableOfContentsSelection(wxListEvent &event) {
  GroupCell *selection_base = m_tableOfContents->GetCell(event.GetIndex());
  if (!selection_base)
    return;
  GroupCell *selection = selection_base->GetGroup();

  // We only update the table of contents when there is time => no guarantee
  // that the cell that was clicked at actually still is part of the tree.
  if ((GetWorksheet()->GetTree()) &&
      (GetWorksheet()->GetTree()->Contains(selection))) {
    GetWorksheet()->ScrolledAwayFromEvaluation(true);
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

/***
 * Checks the file http://wxMaxima-developers.github.io/wxmaxima/version.txt to
 * see if there is a newer version available.
 *
 * Remark/TODO: Instead of our self created/updated version.txt file, it might
 * be possible to get the version info using the Github API as machine readable json file:
 * https://api.github.com/repos/wxMaxima-developers/wxmaxima/releases/latest
 * (only as HTTPS). There we could even parse more info about the latest release,
 * e.g. release date, changelog, available downloads, etc.
 */
void wxMaxima::CheckForUpdates(bool reportUpToDate) {
#if wxCHECK_VERSION(3, 1, 5)
  // wxWebRequest can handle https, not only http.
  // Create the request object

  wxWebRequest request = wxWebSession::GetDefault().CreateRequest(this, "https://wxMaxima-developers.github.io/wxmaxima/version.txt");

  if ( !request.IsOk() ) {
    LoggingMessageBox(_("Can not download version info."), _("Error"), wxOK | wxICON_ERROR);
    return;
  }

  // Bind state event
  Bind(wxEVT_WEBREQUEST_STATE, [reportUpToDate](wxWebRequestEvent& evt) {
    switch (evt.GetState()) {
      // Request completed
      case wxWebRequest::State_Completed:
        {
          wxString version = evt.GetResponse().AsString();
          if (version.StartsWith(wxS("wxmaxima ="))) {
            version = version.Mid(wxString("wmmaxima =").Length(), version.Length());
            version.Trim(true);
            version.Trim(false);
            VersionNumber myVersion(wxS(WXMAXIMA_VERSION));
            VersionNumber currVersion(version);

            if (myVersion < currVersion) {
              bool visit =
                LoggingMessageBox(wxString::Format(_("You have version %s. The newest wxMaxima release is %s.\n\n"
                                                  "Select OK to visit the wxMaxima webpage."),
                                                  WXMAXIMA_VERSION, version),
                                                  _("Upgrade"), wxOK | wxCANCEL | wxICON_INFORMATION) == wxOK;

              if (visit)
                wxLaunchDefaultBrowser(wxS("https://wxMaxima-developers.github.io/wxmaxima/"));
            } else if (reportUpToDate)
              LoggingMessageBox(_("Your version of wxMaxima is up to date."), _("Upgrade"), wxOK | wxICON_INFORMATION);
          } else {
            LoggingMessageBox(wxString::Format(_("Unable to interpret the version info I got: %s"), version), _("Upgrade"), wxOK | wxICON_ERROR);
          }
          break;
        }
        // Request failed
        case wxWebRequest::State_Failed:
            LoggingMessageBox(_("Can not download version info."), _("Error"), wxOK | wxICON_ERROR);
            break;
        default:
            break;
    }
  });

  // Start the request
  request.Start();
#else
  // Attention: wxHTTP does only http connections, not https!!
  wxURL version_url("http://wxMaxima-developers.github.io/wxmaxima/version.txt");
  wxASSERT(version_url.GetError() == wxURL_NOERR);
  wxHTTP connection;
  connection.SetHeader(wxS("Content-type"), wxS("text/html; charset=utf-8"));
  connection.SetTimeout(2);

  if (!connection.Connect(version_url.GetServer())) {
    LoggingMessageBox(_("Can not connect to the web server."), _("Error"), wxOK | wxICON_ERROR);
    return;
  }

  std::unique_ptr<wxInputStream> inputStream = std::unique_ptr<wxInputStream>(connection.GetInputStream(version_url.GetPath()));

  if (connection.GetError() == wxPROTO_NOERR) {
    wxString version;
    wxStringOutputStream outputStream(&version);
    inputStream->Read(outputStream);

    if (version.StartsWith(wxS("wxmaxima ="))) {
      version = version.Mid(wxString("wxmaxima =").Length(), version.Length());
      version.Trim(true);
      version.Trim(false);
      VersionNumber myVersion(wxS(WXMAXIMA_VERSION));
      VersionNumber currVersion(version);

      if (myVersion < currVersion) {
        bool visit =
          LoggingMessageBox(wxString::Format(_("You have version %s. The newest wxMaxima release is %s.\n\n"
                                               "Select OK to visit the wxMaxima webpage."),
                                             WXMAXIMA_VERSION, version),
                            _("Upgrade"), wxOK | wxCANCEL | wxICON_INFORMATION) == wxOK;

        if (visit)
          wxLaunchDefaultBrowser(wxS("https://wxMaxima-developers.github.io/wxmaxima/"));
      } else if (reportUpToDate)
        LoggingMessageBox(_("Your version of wxMaxima is up to date."), _("Upgrade"), wxOK | wxICON_INFORMATION);
    } else {
      LoggingMessageBox(wxString::Format(_("Unable to interpret the version info I got from %s: %s"), version_url.BuildURI(), version),
                        _("Upgrade"), wxOK | wxICON_INFORMATION);
    }
  } else {
    LoggingMessageBox(_("Can not download version info."), _("Error"), wxOK | wxICON_ERROR);
  }
  connection.Close();
#endif
}

int wxMaxima::SaveDocumentP() {
  wxString file, ext;
  if (GetWorksheet()->GetCurrentFile().IsEmpty()) {
    // Check if we want to save modified untitled documents on exit
    if (!m_configuration.SaveUntitled())
      return wxID_NO;
  }
  else {
    if (!m_configuration.AutoSaveAsTempFile())
      {
        if (m_fileIO.SaveFile())
          return wxID_NO;
      }
  }

#if defined __WXOSX__
  file = GetTitle();
#else
  file = _("unsaved");
#endif
  wxFileName::SplitPath(GetWorksheet()->GetCurrentFile(), NULL, NULL, &file, &ext);
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
    if (BTextCtrl::LastActive()) {
      wxLogMessage(_("Forwarding the keyboard focus to a text control"));
      BTextCtrl::LastActive()->SetFocus();
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
wxString wxMaxima::maxima_command_line_filename;
wxLogWindow *MyApp::m_logWindow;
