// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil; -*-
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
#include "wxMaxima.h"
#include "wxMathml.h"
#include "ImgCell.h"
#include "DrawWiz.h"
#include "LicenseDialog.h"
#include "SubstituteWiz.h"
#include "IntegrateWiz.h"
#include "LimitWiz.h"
#include "Plot2dWiz.h"
#include "SeriesWiz.h"
#include "SumWiz.h"
#include "Plot3dWiz.h"
#include "ConfigDialogue.h"
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
#include "wxMaximaIcon.h"
#include "ErrorRedirector.h"

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
#include <wx/utils.h>
#include <wx/mstream.h>

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/sckstrm.h>
#include <wx/fs_mem.h>
#include <wx/persist/toplevel.h>
#include <wx/wupdlock.h>

#include <wx/url.h>
#include <wx/sstream.h>
#include <list>

#if defined __WXOSX__
#define MACPREFIX "wxMaxima.app/Contents/Resources/"
#endif

wxDECLARE_APP (MyApp);

void wxMaxima::ConfigChanged()
{
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

#if defined (__WXOSX__)
  bool usepngCairo = false;
#else
  bool usepngCairo=true;
#endif
  wxLogMessage(_("Sending configuration data to maxima."));
  config->Read(wxT("usepngCairo"), &usepngCairo);
  if (usepngCairo)
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo t)\n");
  else
    m_configCommands += wxT(":lisp-quiet (setq $wxplot_pngcairo nil)\n");

  m_configCommands += wxT(":lisp-quiet (setq $wxsubscripts ") +
             m_worksheet->m_configuration->GetAutosubscript_string() +
             wxT(")\n");

  // A few variables for additional debug info in wxbuild_info();
  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq wxUserConfDir \"%s\")\n"),
                                       EscapeForLisp(Dirstructure::Get()->UserConfDir()));
  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq wxHelpDir \"%s\")\n"),
                              EscapeForLisp(Dirstructure::Get()->HelpDir()));

  int defaultPlotWidth = 600;
  config->Read(wxT("defaultPlotWidth"), &defaultPlotWidth);
  int defaultPlotHeight = 400;
  config->Read(wxT("defaultPlotHeight"), &defaultPlotHeight);
  m_configCommands += wxString::Format(wxT(":lisp-quiet (setq $wxplot_size '((mlist simp) %i %i))\n"),
                              defaultPlotWidth,
                              defaultPlotHeight);

  if (m_worksheet->m_currentFile != wxEmptyString)
  {
    wxString filename(m_worksheet->m_currentFile);

    SetCWD(filename);
  }
}

wxMaxima::wxMaxima(wxWindow *parent, int id, wxLocale *locale, const wxString title,
                   const wxPoint pos, const wxSize size) :
  wxMaximaFrame(parent, id, title, pos, size, wxDEFAULT_FRAME_STYLE,
                MyApp::m_topLevelWindows.empty())
{
  // Will be corrected by ConfigChanged()
  m_maxOutputCellsPerCommand = -1;
  m_locale = locale;
  m_isLogTarget = MyApp::m_topLevelWindows.empty();
  // Suppress window updates until this window has fully been created.
  // Not redrawing the window whilst constructing it hopefully speeds up
  // everything.
  wxWindowUpdateLocker noUpdates(this);
  m_rawBytesSent = 0;
  m_maximaBusy = true;
  m_evalOnStartup = false;
  m_dataFromMaximaIs = false;
  m_gnuplotProcess = NULL;
  m_openInitialFileError = false;
  m_maximaJiffies_old = 0;
  m_cpuTotalJiffies_old = 0;

  m_updateControls = true;
  m_commandIndex = -1;
  m_isActive = true;
  wxASSERT(m_outputPromptRegEx.Compile(wxT("<lbl>.*</lbl>")));
  wxConfigBase *config = wxConfig::Get();
  // If maxima fails to come up directly on startup of wxMaxima there is no need to retry.
  m_unsuccessfulConnectionAttempts = 11;
  m_outputCellsFromCurrentCommand = 0;
  m_CWD = wxEmptyString;
  m_pid = -1;
  wxASSERT(m_gnuplotErrorRegex.Compile(wxT("\".*\\.gnuplot\", line [0-9][0-9]*: ")));
  m_hasEvaluatedCells = false;
  m_process = NULL;
  m_maximaStdout = NULL;
  m_maximaStderr = NULL;
  m_ready = false;
  m_first = true;
  m_isRunning = false;
  m_dispReadOut = false;
  m_promptPrefix = wxT("<PROMPT-P/>");
  m_promptSuffix = wxT("<PROMPT-S/>");
  m_suppressOutputPrefix = wxT("<suppressOutput>");
  m_suppressOutputSuffix = wxT("</suppressOutput>");
  m_symbolsPrefix = wxT("<wxxml-symbols>");
  m_symbolsSuffix = wxT("</wxxml-symbols>");
  m_variablesPrefix = wxT("<variables>");
  m_variablesSuffix = wxT("</variables>");
  m_addVariablesPrefix = wxT("<watch_variables_add>");
  m_addVariablesSuffix = wxT("</watch_variables_add>");
  m_firstPrompt = wxT("(%i1) ");

  m_client = NULL;
  m_server = NULL;

  config->Read(wxT("lastPath"), &m_lastPath);
  m_lastPrompt = wxEmptyString;

  m_closing = false;
  m_openFile = wxEmptyString;
  m_fileSaved = true;
  m_printData = NULL;

  m_chmhelpFile = wxEmptyString;

  m_isConnected = false;
  m_isRunning = false;

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
  m_waitForStringEndTimer.SetOwner(this, WAITFORSTRING_ID);

  m_autoSaveTimer.SetOwner(this, AUTO_SAVE_TIMER_ID);

#if wxUSE_DRAG_AND_DROP
  m_worksheet->SetDropTarget(new MyDropTarget(this));
#endif

  StatusMaximaBusy(disconnected);

  /// RegEx for function definitions
  wxASSERT(m_funRegEx.Compile(wxT("^ *([[:alnum:]%_]+) *\\(([[:alnum:]%_,[[.].] ]*)\\) *:=")));
  // RegEx for variable definitions
  wxASSERT(m_varRegEx.Compile(wxT("^ *([[:alnum:]%_]+) *:")));
  // RegEx for blank statement removal
  wxASSERT(m_blankStatementRegEx.Compile(wxT("(^;)|((^|;)(((\\/\\*.*\\*\\/)?([[:space:]]*))+;)+)")));
  wxASSERT(m_sbclCompilationRegEx.Compile(wxT("; compiling (.* \\.*)")));

  m_statusBar->GetNetworkStatusElement()->Connect(wxEVT_LEFT_DCLICK,
                                                  wxCommandEventHandler(wxMaxima::NetworkDClick),
                                                  NULL, this);
  m_clientStream = NULL;
  m_clientTextStream = NULL;
  m_parser = new MathParser (&m_worksheet->m_configuration, &m_worksheet->m_cellPointers);
}

wxMaxima::~wxMaxima()
{
  KillMaxima(false);
  wxDELETE(m_printData);m_printData = NULL;
  delete(m_parser);
  m_parser = NULL;
  MyApp::m_topLevelWindows.remove(this);
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

  if (files[0].EndsWith(wxT(".wxm")) ||
      files[0].EndsWith(wxT(".wxmx")))
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

  if (files[0].EndsWith(wxT(".png")) ||
      files[0].EndsWith(wxT(".jpeg")) ||
      files[0].EndsWith(wxT(".jpg")))
  {
    m_wxmax->LoadImage(files[0]);
    return true;
  }

  m_wxmax->m_worksheet->InsertText(files[0]);
  return true;
}

#endif

//!--------------------------------------------------------------------------------
//  Startup
//--------------------------------------------------------------------------------
void wxMaxima::InitSession()
{
  bool server = false;
  m_port = m_worksheet->m_configuration->DefaultPort();
  while (!(server = StartServer()))
  {
    m_port++;
    if ((m_port > m_worksheet->m_configuration->DefaultPort() + 15000) || (m_port > 65535))
    {
      wxMessageBox(_("wxMaxima could not start the server.\n\n"
                             "Please check you have network support\n"
                             "enabled and try again!"),
                   _("Fatal error"),
                   wxOK | wxICON_ERROR);
      break;
    }
  }

  if (!server)
    LeftStatusText(_("Starting server failed"));
  else if (!StartMaxima())
    LeftStatusText(_("Starting Maxima process failed"));

  Refresh();
  m_worksheet->SetFocus();
  if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
    m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());
}

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
TextCell *wxMaxima::ConsoleAppend(wxString s, CellType type, wxString userLabel)
{
  TextCell *lastLine = NULL;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_worksheet->GetTree() == NULL)
    m_worksheet->InsertGroupCells(
            new GroupCell(&(m_worksheet->m_configuration), GC_TYPE_CODE, &m_worksheet->m_cellPointers, wxEmptyString));

  m_dispReadOut = false;
  s.Replace(m_promptSuffix, wxEmptyString);

  // If the string we have to append is empty we return immediately.
  wxString t(s);
  t.Trim();
  t.Trim(false);
  if (t.Length() == 0)
  {
    return NULL;
  }

  if (m_maxOutputCellsPerCommand > 0)
  {
    // If we already have output more lines than we are allowed to we a inform the user
    // about this and return.
    if (m_outputCellsFromCurrentCommand++ >= m_maxOutputCellsPerCommand)
    {
      DoRawConsoleAppend(
              _("... [suppressed additional lines since the output is longer than allowed in the configuration] "),
              MC_TYPE_ERROR);
      return NULL;
    };


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

    while (s.Length() > 0)
    {
      int start = s.Find(wxT("<mth"));

      if (start == wxNOT_FOUND)
      {
        t = s;
        t.Trim();
        t.Trim(false);
        if (t.Length())
          lastLine = DoRawConsoleAppend(s, MC_TYPE_DEFAULT);
        s = wxEmptyString;
      }
      else
      {

        // If the string does begin with a <mth> we add the
        // part of the string that precedes the <mth> to the console
        // first.
        wxString pre = s.SubString(0, start - 1);
        wxString pre1(pre);
        pre1.Trim();
        pre1.Trim(false);
        if (pre1.Length())
          DoRawConsoleAppend(pre, MC_TYPE_DEFAULT);

        // If the math tag ends inside this string we add the whole tag.
        int end = s.Find(wxT("</mth>"));
        if (end == wxNOT_FOUND)
          end = s.Length();
        else
          end += 5;
        wxString rest = s.SubString(start, end);

        DoConsoleAppend(wxT("<span>") + rest +
                        wxT("</span>"), type, false, true, userLabel);
        s = s.SubString(end + 1, s.Length());
      }
//      wxSafeYield();
    }
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

    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type, true, true, userLabel);
  }

  else if (type == MC_TYPE_ERROR)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ERROR);
    GroupCell *tmp = m_worksheet->GetWorkingGroup(true);

    if (tmp == NULL)
    {
    if (m_worksheet->GetActiveCell())
      tmp = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
    }

    if(tmp != NULL)
    {
      m_worksheet->m_cellPointers.m_errorList.Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_commandIndex - 1);
    }
  }
  else if (type == MC_TYPE_WARNING)
  {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_WARNING);
  }
  else
    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type, false);

  return lastLine;
}

void wxMaxima::DoConsoleAppend(wxString s, CellType type, bool newLine,
                               bool bigSkip, wxString userLabel)
{
  Cell *cell;

  if (s.IsEmpty())
    return;

  s.Replace(wxT("\n"), wxT(" "), true);

  m_parser->SetUserLabel(userLabel);
  cell = m_parser->ParseLine(s, type);

  wxASSERT_MSG(cell != NULL, _("There was an error in generated XML!\n\n"
                                       "Please report this as a bug."));
  if (cell == NULL)
  {
    return;
  }

  cell->SetSkip(bigSkip);
  m_worksheet->InsertLine(cell, newLine || cell->BreakLineHere());
}

TextCell *wxMaxima::DoRawConsoleAppend(wxString s, CellType type)
{
  TextCell *cell = NULL;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_worksheet->GetTree() == NULL)
    m_worksheet->InsertGroupCells(
            new GroupCell(&(m_worksheet->m_configuration), GC_TYPE_CODE, &m_worksheet->m_cellPointers, wxEmptyString));

  if (s.IsEmpty())
    return NULL;

  bool scrollToCaret = (!m_worksheet->FollowEvaluation() && m_worksheet->CaretVisibleIs());

  if (type == MC_TYPE_MAIN_PROMPT)
  {
    cell = new TextCell(m_worksheet->GetTree(), &(m_worksheet->m_configuration), &m_worksheet->m_cellPointers, s);
    cell->SetType(type);
    m_worksheet->InsertLine(cell, true);
  }

  else
  {

    TextCell *incompleteTextCell =
      dynamic_cast<TextCell *>(m_worksheet->m_cellPointers.m_currentTextCell);

    if(incompleteTextCell != NULL)
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
      if(s == wxEmptyString)
      {
        dynamic_cast<GroupCell *>(incompleteTextCell->GetGroup())->ResetSize();
        dynamic_cast<GroupCell *>(incompleteTextCell->GetGroup())->Recalculate();
        return incompleteTextCell;
      }
    }

    wxStringTokenizer tokens(s, wxT("\n"));
    int count = 0;
    Cell *tmp = NULL, *lst = NULL;
    while (tokens.HasMoreTokens())
    {
      wxString token = tokens.GetNextToken();
      // Move endles streams of compilation messages to the status bar...
      if(m_sbclCompilationRegEx.Matches(token))
      {
        wxString fileName = token;
        m_sbclCompilationRegEx.Replace(&fileName, wxT("\\1"));
        LeftStatusText(wxString::Format(_("Compiling %s"),fileName));
      }
      else
      {
        cell = new TextCell(m_worksheet->GetTree(), &(m_worksheet->m_configuration),
                            &m_worksheet->m_cellPointers,
                            token);

        cell->SetType(type);

        if (tokens.HasMoreTokens())
          cell->SetSkip(false);

        if (lst == NULL)
          tmp = lst = cell;
        else
        {
          lst->AppendCell(cell);
          cell->ForceBreakLine(true);
          lst = cell;
        }
      }
      count++;
    }
    m_worksheet->InsertLine(tmp, true);
  }

  if(cell)
  {
    m_worksheet->Recalculate(cell->GetGroup());
    if (scrollToCaret)
      m_worksheet->ScrollToCaret();
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
  if (parenthesisError == wxEmptyString)
  {
    s = m_worksheet->UnicodeToMaxima(s);

    if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
      m_xmlInspector->Add_ToMaxima(s);

    m_dispReadOut = false;

    /// Add this command to History
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

    if ((m_client) && (s.Length() >= 1) && (m_client->IsOk()))
    {
      // If there is no working group and we still are trying to send something
      // we are trying to change maxima's settings from the background and might never
      // get an answer that changes the status again.
      if (m_worksheet->GetWorkingGroup())
        StatusMaximaBusy(calculating);
      else
        StatusMaximaBusy(waiting);

      wxScopedCharBuffer const data_raw = s.utf8_str();
      #ifdef __WXMSW__
      // On MS Windows we don't get a signal that tells us if a write has
      // finishes. But it seems a write always succeeds
      m_client->Write(data_raw.data(), data_raw.length());
      #else
      // On Linux (and most probably all other non MS-Windows systems) we get a
      // signal that tells us a write command has finished - and tells us how many
      // bytes were sent. Which (at least on BSD) might be lower than we wanted.
      if(m_rawDataToSend.GetDataLen() > 0)
      {
        // We are already sending => append everything except the trailing NULL
        // char at the end of the string to the buffer containing the data we want
        // to send.
        m_rawDataToSend.AppendData(data_raw.data(),data_raw.length());
      }
      else
      {
        // Put everything except the NULL char at the end of the string into the
        // buffer containing the data we want to send
        m_rawDataToSend.AppendData(data_raw.data(),data_raw.length());
        // Now we have done this we attempt to send the data. If our try falls
        // short we'll find that out in the client event of the type wxSOCKET_OUTPUT
        // that will follow the write.
        m_client->Write((void *)m_rawDataToSend.GetData(), m_rawDataToSend.GetDataLen());
      }
      #endif
      if (m_client->Error()) {
        DoRawConsoleAppend(_("Error writing to Maxima"), MC_TYPE_ERROR);
        return;
      }
      m_statusBar->NetworkStatus(StatusBar::transmit);
    }
  }
  else
  {
    DoRawConsoleAppend(
      _("Refusing to send cell to maxima: ") +
      parenthesisError + wxT("\n"),              MC_TYPE_ERROR);
    m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
    m_worksheet->m_evaluationQueue.Clear();
  }
  if(!m_maximaStdoutPollTimer.IsRunning())
      m_statusBar->SetMaximaCPUPercentage(-1);
  m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
}

///--------------------------------------------------------------------------------
///  Socket stuff
///--------------------------------------------------------------------------------

void wxMaxima::ClientEvent(wxSocketEvent &event)
{
  switch (event.GetSocketEvent())
  {

  case wxSOCKET_INPUT:
  {
    // Read out stderr: We will do that in the background on a regular basis, anyway.
    // But if we do it manually now, too, the probability that things are presented
    // to the user in chronological order increases a bit.
    ReadStdErr();

    // It is theoretically possible that the client has exited after sending us
    // data and before we had been able to process it.
    if (m_client == NULL)
      return;

    if(!m_client->IsData())
      return;

    m_statusBar->NetworkStatus(StatusBar::receive);

    // Read all new lines of text we received.
    wxChar chr;

    while((m_client->IsData()) && (!m_clientStream->Eof()))
      {
        chr = m_clientTextStream->GetChar();
        if(chr == wxEOT)
          break;
        if(chr != '\0')
          m_newCharsFromMaxima += chr;
      }

    m_bytesFromMaxima += m_newCharsFromMaxima.Length();

    if(m_newCharsFromMaxima.EndsWith("\n") || m_newCharsFromMaxima.EndsWith(m_promptSuffix) || (m_first))
    {
      m_waitForStringEndTimer.Stop();
      InterpretDataFromMaxima();
    }
    else
      m_waitForStringEndTimer.StartOnce(5000);

    break;
    }
  case wxSOCKET_OUTPUT:
  {
    if(!m_client)
    {
      m_rawBytesSent = 0;
      m_rawDataToSend.Clear();
      return;
    }
    long int bytesWritten = m_client->LastWriteCount();
    m_rawBytesSent  += bytesWritten;
    if(m_rawDataToSend.GetDataLen() > m_rawBytesSent)
    {
      wxLogMessage(_("Continuing sending data to maxima."));
      m_client->Write(
        (void *)((char *)m_rawDataToSend.GetData() + m_rawBytesSent),
        m_rawDataToSend.GetDataLen() - m_rawBytesSent);
      if (m_client->Error()) {
        DoRawConsoleAppend(_("Error writing to Maxima"), MC_TYPE_ERROR);
        return;
      }
      m_statusBar->NetworkStatus(StatusBar::transmit);
    }
    else
    {
      m_rawBytesSent = 0;
      m_rawDataToSend.Clear();
    }
    break;
  }
  case wxSOCKET_LOST:
  {

//    wxLogMessage(_("Connection to Maxima lost."));
    //  KillMaxima();
    break;
  }
  default:
    break;
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
    {
      if (m_isConnected)
      {
        wxSocketBase *tmp = m_server->Accept(false);
        tmp->Close();
        wxLogMessage(_("New connection attempt when already connected."));
        return;
      }
      if(m_process == NULL)
        return;

      wxLogMessage(_("Connected."));
      m_rawDataToSend.Clear();
      m_rawBytesSent = 0;

      m_statusBar->NetworkStatus(StatusBar::idle);
      m_worksheet->QuestionAnswered();
      m_currentOutput = wxEmptyString;
      m_isConnected = true;
      m_client = m_server->Accept(false);
      m_clientStream = new wxSocketInputStream(*m_client);
      m_clientTextStream = new wxTextInputStream(*m_clientStream, wxT('\t'),
                                                 wxConvUTF8);
      m_client->SetEventHandler(*this, socket_client_id);
      m_client->SetNotify(wxSOCKET_INPUT_FLAG|wxSOCKET_OUTPUT_FLAG|wxSOCKET_LOST_FLAG);
      m_client->Notify(true);
      m_client->SetFlags(wxSOCKET_NOWAIT);
      m_client->SetTimeout(15);
      SetupVariables();
      wxUpdateUIEvent dummy;
      UpdateToolBar(dummy);
      UpdateMenus(dummy);
    }
    break;

  default:
      break;
  }
}

bool wxMaxima::StartServer()
{
  RightStatusText(wxString::Format(_("Starting server on port %d"), m_port));

  wxIPV4address addr;

#ifndef __WXOSX__
  addr.LocalHost();
#else
  addr.AnyAddress();
#endif

  addr.Service(m_port);

  m_server = new wxSocketServer(addr);
  if (!m_server->Ok())
  {
    m_server->Destroy();
    m_server = NULL;
    m_isRunning = false;
    m_isConnected = false;
    RightStatusText(_("Starting server failed"));
    m_statusBar->NetworkStatus(StatusBar::error);
    return false;
  }
  RightStatusText(_("Server started"));
  m_server->SetEventHandler(*this, socket_server_id);
  m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
  m_server->Notify(true);

  m_isConnected = false;
  m_isRunning = true;
  return m_isRunning;
}

///--------------------------------------------------------------------------------
///  Maxima process stuff
///--------------------------------------------------------------------------------

bool wxMaxima::StartMaxima(bool force)
{
  if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
    m_xmlInspector->Clear();

  // Maxima isn't in lisp mode
  m_worksheet->m_configuration->InLispMode(false);

  // If we have an open file tell maxima to start in the directory the file is in
  wxUnsetEnv(wxT("MAXIMA_INITIAL_FOLDER"));
  wxString filename = m_worksheet->m_currentFile;
  if(filename == wxEmptyString)
    filename = m_openFile;
  if(filename != wxEmptyString)
  {
    wxFileName dir(filename);
    dir.MakeAbsolute();
    wxString dirname = dir.GetPath();
    if(wxDirExists(dirname))
    {
      // Tell maxima to start in the directory the file is in
      wxSetEnv(wxT("MAXIMA_INITIAL_FOLDER"),dirname);
    }
    else
      wxLogWarning(wxString::Format(
                     wxT("Directory %s doesn't exist. Maxima might complain about that."),
                     dirname)
        );
  }

  // We only need to start or restart maxima if we aren't connected to a maxima
  // that till now never has done anything and therefore is in perfect working
  // order.
  if ((m_process == NULL) || (m_hasEvaluatedCells) || force)
  {
    if(m_process != NULL)
    {
      m_closing = true;
      KillMaxima();
    }
    m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

    wxString command = GetCommand();

    if (command.Length() > 0)
    {

      command.Append(wxString::Format(wxT(" -s %d "), m_port));

#if defined __WXOSX__
      wxSetEnv(wxT("DISPLAY"), wxT(":0.0"));
#endif

#if defined __WXMSW__
      // Tell maxima we want to be able to kill it on Ctrl+G by sending it a signal
      wxSetEnv(wxT("MAXIMA_SIGNALS_THREAD"), wxT("1"));
#endif
      m_process = new wxProcess(this, maxima_process_id);
      m_process->Redirect();
//      m_process->SetPriority(wxPRIORITY_MAX);
      m_first = true;
      m_pid = -1;
      wxLogMessage(wxString::Format(_("Running maxima as: %s"), command));
      if (wxExecute(command, wxEXEC_ASYNC, m_process) <= 0 )
      {
        StatusMaximaBusy(process_wont_start);
        RightStatusText(_("Cannot start the maxima binary"));
        m_process = NULL;
        m_maximaStdout = NULL;
        m_maximaStderr = NULL;
        m_statusBar->NetworkStatus(StatusBar::offline);
        wxMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."), _("Error"),
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
  }
  m_worksheet->m_cellPointers.m_errorList.Clear();

  // Initialize the performance counter.
  GetMaximaCPUPercentage();
  return true;
}


void wxMaxima::Interrupt(wxCommandEvent& WXUNUSED(event))
{
    if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  if (m_pid < 0)
  {
    m_MenuBar->Enable(menu_interrupt_id, false);
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
    LPVOID sharedMemoryAddress = 0;
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
      int *sharedMemoryContents = (int *)(sharedMemoryAddress);
      if (sharedMemoryAddress)
      {
        *sharedMemoryContents = *sharedMemoryContents | value;
        wxLogMessage(_("Sending an interrupt signal to Maxima."));
        UnmapViewOfFile(sharedMemoryAddress);
      }
      if (sharedMemoryHandle)
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
  m_worksheet->m_variablesPane->ResetValues();
  m_varNamesToQuery = m_worksheet->m_variablesPane->GetEscapedVarnames();
  if((m_pid > 0) && (m_client == NULL))
    return;

  m_isRunning = false;

  if(logMessage)
    wxLogMessage(_("Killing Maxima."));
  m_configCommands = wxEmptyString;
  // The new maxima process will be in its initial condition => mark it as such.
  m_hasEvaluatedCells = false;

  m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
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

  wxDELETE(m_clientTextStream);m_clientTextStream = NULL;
  //  wxDELETE(m_clientStream);
  m_clientStream = NULL;

  if(m_client)
  {
    // Make wxWidgets close the connection only after we have sent the close command.
    m_client->SetFlags(wxSOCKET_WAITALL);
    // Try to gracefully close maxima.
    if (m_worksheet->m_configuration->InLispMode())
      SendMaxima(wxT("($quit)"));
    else
      SendMaxima(wxT("quit();"));

    // The following command should close maxima, as well.
    m_client->Close(); m_client = NULL;
  }

  // Just to be absolutely sure: Additionally try to kill maxima
  if (m_pid > 0)
  {
    // wxProcess::kill will fail on MSW. Something with a console.
    SuppressErrorDialogs logNull;
    wxProcess::Kill(m_pid, wxSIGKILL, wxKILL_CHILDREN);
  }
  m_isConnected = false;
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
  wxUpdateUIEvent dummy;
  UpdateToolBar(dummy);
  UpdateMenus(dummy);
}

void wxMaxima::OnGnuplotClose(wxProcessEvent& WXUNUSED(event))
{
  m_gnuplotProcess = NULL;
  wxLogMessage(_("Gnuplot has closed."));
}

void wxMaxima::OnProcessEvent(wxProcessEvent& WXUNUSED(event))
{
  wxLogMessage(_("Maxima has terminated."));
  m_rawDataToSend.Clear();
  m_rawBytesSent = 0;
  m_statusBar->NetworkStatus(StatusBar::offline);
  std::cerr<<m_first<<" "<<m_unsuccessfulConnectionAttempts<<"\n";
  if (!m_closing)
  {
    RightStatusText(_("Maxima process terminated."));

    if(m_first)
    {
      wxMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."), _("Error"),
                   wxOK | wxICON_ERROR);
    }
    
    // Let's see if maxima has told us why this did happen.
    ReadStdErr();

    // if m_closing==true we might already have a new process
    // and therefore the following lines would probably mark
    // the wrong process as "deleted".
    m_process = NULL;
    m_maximaStdout = NULL;
    m_maximaStderr = NULL;
    m_maximaVersion = wxEmptyString;
    m_lispVersion = wxEmptyString;

    ConsoleAppend(wxT("\nMaxima exited...\n"),
                  MC_TYPE_ERROR);

    m_isConnected = false;
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
  wxUpdateUIEvent dummy;
  UpdateToolBar(dummy);
  UpdateMenus(dummy);
}

void wxMaxima::CleanUp()
{
  if (m_isConnected)
    KillMaxima();
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
    m_MenuBar->Enable(menu_interrupt_id, true);

  m_first = false;
  StatusMaximaBusy(waiting);
  m_closing = false; // when restarting maxima this is temporarily true

  wxString prompt_compact = data.Left(start + end + m_firstPrompt.Length() - 1);
  prompt_compact.Replace(wxT("\n"),wxT("\x21b2"));


  wxLogMessage(wxString::Format(_("Received maxima's first prompt: %s"),
                                prompt_compact));

  wxLogMessage(wxString::Format(_("Maxima's PID is %li"),(long)m_pid));
  // Remove the first prompt from Maxima's answer.
  data = data.Right(data.Length() - end - m_firstPrompt.Length());

  if (m_worksheet->m_evaluationQueue.Empty())
  {
    // Inform the user that the evaluation queue is empty.
    EvaluationQueueLength(0);
    if (m_evalOnStartup && m_isNamed)
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

int wxMaxima::GetMiscTextEnd(const wxString &data)
{
  // These tests are redundant with later tests. But they are faster.
  if(data.StartsWith("<mth>"))
    return 0;
  if(data.StartsWith("<lbl>"))
    return 0;
  if(data.StartsWith("<statusbar>"))
    return 0;
  if(data.StartsWith(m_promptPrefix))
    return 0;
  if(data.StartsWith(m_symbolsPrefix))
    return 0;
  if(data.StartsWith(m_variablesPrefix))
    return 0;
  if(data.StartsWith(m_addVariablesPrefix))
    return 0;
  if(data.StartsWith(m_suppressOutputPrefix))
    return 0;

  int mthpos = data.Find("<mth>");
  int lblpos = data.Find("<lbl>");
  int statpos = data.Find("<statusbar>");
  int prmptpos = data.Find(m_promptPrefix);
  int symbolspos = data.Find(m_symbolsPrefix);
  int suppressOutputPos = data.Find(m_suppressOutputPrefix);
  int variablespos = data.Find(m_variablesPrefix);
  int addvariablespos = data.Find(m_addVariablesPrefix);

  int tagPos = data.Length();
  if ((mthpos != wxNOT_FOUND) && (mthpos < tagPos))
    tagPos = mthpos;
  if ((tagPos == wxNOT_FOUND) || ((lblpos != wxNOT_FOUND) && (lblpos < tagPos)))
    tagPos = lblpos;
  if ((tagPos == wxNOT_FOUND) || ((statpos != wxNOT_FOUND) && (statpos < tagPos)))
    tagPos = statpos;
  if ((tagPos == wxNOT_FOUND) || ((prmptpos != wxNOT_FOUND) && (prmptpos < tagPos)))
    tagPos = prmptpos;
  if ((tagPos == wxNOT_FOUND) || ((prmptpos != wxNOT_FOUND) && (prmptpos < tagPos)))
    tagPos = prmptpos;
  if ((tagPos == wxNOT_FOUND) || ((symbolspos != wxNOT_FOUND) && (symbolspos < tagPos)))
    tagPos = symbolspos;
  if ((tagPos == wxNOT_FOUND) || ((variablespos != wxNOT_FOUND) && (variablespos < tagPos)))
    tagPos = variablespos;
  if ((tagPos == wxNOT_FOUND) || ((addvariablespos != wxNOT_FOUND) && (addvariablespos < tagPos)))
    tagPos = addvariablespos;
  if ((tagPos == wxNOT_FOUND) || ((suppressOutputPos != wxNOT_FOUND) && (suppressOutputPos < tagPos)))
    tagPos = suppressOutputPos;
  return tagPos;
}

void wxMaxima::ReadMiscText(wxString &data)
{
  if (data.IsEmpty())
    return;

  // Extract all text that isn't a xml tag known to us.
  int miscTextLen = GetMiscTextEnd(data);
  if(miscTextLen <= 0)
  {
    if(data != wxEmptyString)
      m_worksheet->m_cellPointers.m_currentTextCell = NULL;
    return;
  }

  wxString miscText = data.Left(miscTextLen);
  data = data.Right(data.Length() - miscTextLen);

  // Stupid DOS and MAC line endings. The first of these commands won't work
  // if the "\r" is the last char of a packet containing a part of a very long
  // string. But running a search-and-replace
  miscText.Replace("\r\n","\n");
  miscText.Replace("\r","\n");

  if(miscText.StartsWith("\n"))
    m_worksheet->m_cellPointers.m_currentTextCell = NULL;

  // A version of the text where each line begins with non-whitespace and whitespace
  // characters are merged.
  wxString mergedWhitespace = wxT("\n");
  bool whitespace = true;
  for ( wxString::iterator it = miscText.begin(); it!=miscText.end(); ++it)
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
  wxStringTokenizer lines(miscText, wxT("\n"));
  while (lines.HasMoreTokens())
  {
    // extract a string from the Data lines
    wxString textline = lines.GetNextToken();
    wxString trimmedLine = textline;

    trimmedLine.Trim(true);
    trimmedLine.Trim(false);

    if((textline != wxEmptyString)&&(textline != wxT("\n")))
    {
      if(error)
      {
        m_worksheet->m_cellPointers.m_currentTextCell = ConsoleAppend(textline, MC_TYPE_ERROR);
        AbortOnError();
      }
      else
      {
        if(warning)
          m_worksheet->m_cellPointers.m_currentTextCell = ConsoleAppend(textline, MC_TYPE_WARNING);
        else
          m_worksheet->m_cellPointers.m_currentTextCell = ConsoleAppend(textline, MC_TYPE_DEFAULT);
      }
    }
    if(lines.HasMoreTokens())
      m_worksheet->m_cellPointers.m_currentTextCell = NULL;
  }
  if(miscText.EndsWith("\n"))
    m_worksheet->m_cellPointers.m_currentTextCell = NULL;

  if(data != wxEmptyString)
    m_worksheet->m_cellPointers.m_currentTextCell = NULL;
}

int wxMaxima::FindTagEnd(wxString &data, const wxString &tag)
{
  if((m_currentOutputEnd == wxEmptyString) || (m_currentOutputEnd.Find(tag) != wxNOT_FOUND))
    return data.Find(tag);
  else
    return wxNOT_FOUND;
}

void wxMaxima::ReadStatusBar(wxString &data)
{
  wxString statusbarStart = wxT("<statusbar>");
  if (!data.StartsWith(statusbarStart))
    return;

  m_worksheet->m_cellPointers.m_currentTextCell = NULL;

  wxString sts = wxT("</statusbar>");
  int end;
  if ((end = FindTagEnd(data,sts)) != wxNOT_FOUND)
  {
    wxXmlDocument xmldoc;
    wxString xml = data.Left( end + sts.Length());
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
    data = data.Right(data.Length()-end-sts.Length());
  }
}

/***
 * Checks if maxima displayed a new chunk of math
 */
void wxMaxima::ReadMath(wxString &data)
{
  wxString mthstart = wxT("<mth>");
  if (!data.StartsWith(mthstart))
    return;

  m_worksheet->m_cellPointers.m_currentTextCell = NULL;

  // Append everything from the "beginning of math" to the "end of math" marker
  // to the console and remove it from the data we got.
  wxString mthend = wxT("</mth>");
  int end;
  if ((end = FindTagEnd(data,mthend)) != wxNOT_FOUND)
  {
    wxString o = data.Left(end + mthend.Length());
    data = data.Right(data.Length()-end-mthend.Length());
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

  m_worksheet->m_cellPointers.m_currentTextCell = NULL;

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
            if(name == "maxima_userdir")
            {
              Dirstructure::Get()->UserConfDir(value);
              wxLogMessage(wxString::Format("Maxima user configuration lies in directory %s",value));
            }
            if(name == "maxima_tempdir")
            {
              m_maximaTempDir = value;
              wxLogMessage(wxString::Format("Maxima uses temp directory %s",value));
              {
                // Sometimes people delete their temp dir
                // and gnuplot won't create a new one for them.
                wxLogNull logNull;
                wxMkDir(value, wxS_DIR_DEFAULT);
              }
            }
            if(name == "*autoconf-version*")
            {
              m_maximaVersion = value;
              wxLogMessage(wxString::Format("Maxima version: %s",value));
            }
            if(name == "*autoconf-host*")
            {
              m_maximaArch = value;
              wxLogMessage(wxString::Format("Maxima architecture: %s",value));
            }
            if(name == "*maxima-infodir*")
            {
              m_maximaDocDir = value;
              wxLogMessage(wxString::Format("Maxima's manual lies in directory %s",value));
            }
            if(name == "*maxima-sharedir*")
            {
              m_worksheet->m_configuration->MaximaShareDir(value);
              wxLogMessage(wxString::Format("Maxima's share files lie in directory %s",value));
              /// READ FUNCTIONS FOR AUTOCOMPLETION
              m_worksheet->LoadSymbols();
            }
            if(name == "*lisp-name*")
            {
              m_lispType = value;
              wxLogMessage(wxString::Format("Maxima was compiled using %s",value));
            }
            if(name == "*lisp-version*")
            {
              m_lispVersion = value;
              wxLogMessage(wxString::Format("Lisp version: %s",value));
            }
            if(name == "*wx-load-file-name*")
            {
              m_recentPackages.AddDocument(value);
              wxLogMessage(wxString::Format(_("Maxima has loaded the file %s."),value));
            }
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
      wxLogMessage(_("Maxima sends a new set of auto-completible symbols."));
    else
      wxLogMessage(_("Maxima has sent a new variable value."));

    // Remove the symbols from the data string
    data = data.Right(data.Length()-end-m_variablesSuffix.Length());
    TriggerEvaluation();
    QueryVariableValue();
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
    m_worksheet->m_variablesPane->AutoSize();
    m_worksheet->m_variablesPane->GetParent()->Layout();
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

  m_worksheet->m_cellPointers.m_currentTextCell = NULL;

  // Assume we don't have a question prompt
  m_worksheet->m_questionPrompt = false;
  m_ready = true;
  int end = FindTagEnd(data,m_promptSuffix);
  // Did we find a prompt?
  if (end == wxNOT_FOUND)
    return;

  m_maximaBusy = false;
  m_bytesFromMaxima = 0;

  wxString o = data.SubString(m_promptPrefix.Length(), end - 1);
  // Remove the prompt we will process from the string.
  data = data.Right(data.Length()-end-m_promptSuffix.Length());
  if(data == wxT(" "))
    data = wxEmptyString;

  // If we got a prompt our connection to maxima was successful.
  if(m_unsuccessfulConnectionAttempts > 0)
    m_unsuccessfulConnectionAttempts--;
  o.Trim(true);
  o.Trim(false);
  // Input prompts have a length > 0 and end in a number followed by a ")".
  // Depending on ibase the digits of the number might lie between 'A' and 'Z',
  // too. Input prompts also begin with a "(". Questions (hopefully)
  // don't do that; Lisp prompts look like question prompts.
  if (
    (
      (o.Length() > 2) &&
      o.StartsWith("(%") &&
      o.EndsWith(")") &&
      (((o[o.Length()-2] >= (wxT('0'))) &&
        (o[o.Length()-2] <= (wxT('9')))) ||
       ((o[o.Length()-2] >= (wxT('A'))) &&
        (o[o.Length()-2] <= (wxT('Z'))))
        )
      ) ||
      m_worksheet->m_configuration->InLispMode() ||
    (o.StartsWith(wxT("MAXIMA>"))) ||
    (o.StartsWith(wxT("\nMAXIMA>")))
    )
  {
    // Maxima displayed a new main prompt => We don't have a question
    m_worksheet->QuestionAnswered();
    // And we can remove one command from the evaluation queue.
    m_worksheet->m_evaluationQueue.RemoveFirst();

    //m_lastPrompt = o.Mid(1,o.Length()-1);
    //m_lastPrompt.Replace(wxT(")"), wxT(":"), false);
    m_lastPrompt = o;
    // remove the event maxima has just processed from the evaluation queue
    // if we remove a command from the evaluation queue the next output line will be the
    // first from the next command.
    m_outputCellsFromCurrentCommand = 0;
    if (m_worksheet->m_evaluationQueue.Empty())
    { // queue empty.
      StatusMaximaBusy(waiting);
      // If we have selected a cell in order to show we are evaluating it
      // we should now remove this marker.
      if (m_worksheet->FollowEvaluation())
      {
        if (m_worksheet->GetActiveCell())
          m_worksheet->GetActiveCell()->SelectNone();
        m_worksheet->SetSelection(NULL, NULL);
      }
      m_worksheet->FollowEvaluation(false);
      if (m_exitAfterEval)
      {
        SaveFile(false);
        Close();
      }
      // Inform the user that the evaluation queue is empty.
      EvaluationQueueLength(0);
      m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
      m_worksheet->m_evaluationQueue.RemoveFirst();
      m_worksheet->RequestRedraw();
      // Now that maxima is idle we can ask for the contents of its variables
      QueryVariableValue();
    }
    else
    { // we don't have an empty queue
      m_ready = false;
      m_worksheet->RequestRedraw();
      m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
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
    m_worksheet->SetLastQuestion(o);
    m_worksheet->QuestionAnswered();
    m_worksheet->QuestionPending(true);
    // If the user answers a question additional output might be required even
    // if the question has been preceded by many lines.
    m_outputCellsFromCurrentCommand = 0;
    if((m_worksheet->GetWorkingGroup() == NULL) ||
       ((m_worksheet->GetWorkingGroup()->m_knownAnswers.empty()) &&
        m_worksheet->GetWorkingGroup()->AutoAnswer()))
       m_worksheet->SetNotification(_("Maxima asks a question!"),wxICON_INFORMATION);
    if (!o.IsEmpty())
    {
      m_worksheet->m_configuration->SetDefaultCellToolTip(
        _("Most questions can be avoided using the assume() "
          "and the declare() command"));
      if (o.Find(wxT("<mth>")) > -1)
        DoConsoleAppend(o, MC_TYPE_PROMPT);
      else
        DoRawConsoleAppend(o, MC_TYPE_PROMPT);
      m_worksheet->m_configuration->SetDefaultCellToolTip(wxEmptyString);
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

  o.Trim(false);
  if (o.StartsWith(wxT("MAXIMA>")))
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
  if (!m_client)
    return;

  // Tell the math parser where to search for local files.
  m_worksheet->m_configuration->SetWorkingDirectory(wxFileName(file).GetPath());

#if defined __WXMSW__
  file.Replace(wxT("\\"), wxT("/"));
#endif

  wxFileName filename(file);

  if (filename.GetPath() == wxEmptyString)
    filename.AssignDir(wxGetCwd());

  // Escape all backslashes in the filename if needed by the OS.
  wxString filenamestring = filename.GetFullPath();
  wxString dirname = filename.GetPath();

#if defined (__WXMSW__)
  // On MSW filenames with a "\" are widely used - but only partially supported.
  filenamestring.Replace(wxT("\\"),wxT("/"));
  dirname.Replace(wxT("\\"),wxT("/"));
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

wxString wxMaxima::ReadMacContents(wxString file)
{
  bool xMaximaFile = file.Lower().EndsWith(wxT(".out"));

    // open mac file
  wxTextFile inputFile(file);

  if (!inputFile.Open())
  {
    wxMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return wxEmptyString;
  }

  bool input = true;
  wxString macContents;
  wxString line = inputFile.GetFirstLine();
  do
  {
    if(xMaximaFile)
    {
      // Detect output cells.
      if(line.StartsWith(wxT("(%o")))
        input = false;

      if(line.StartsWith(wxT("(%i")))
      {
        int end = line.Find(wxT(")"));
        if(end > 0)
        {
          line = line.Right(line.Length() - end - 2);
          input = true;
        }
      }

    }

    if(input)
      macContents += line + wxT("\n");

    if(!inputFile.Eof())
      line = inputFile.GetNextLine();

  } while (!inputFile.Eof());
  inputFile.Close();

  return macContents;
}

bool wxMaxima::OpenMACFile(wxString file, Worksheet *document, bool clearDocument)
{
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));
  wxWindowUpdateLocker noUpdates(document);

  wxString macContents = ReadMacContents(file);

  if(macContents == wxEmptyString)
    return false;

  if (clearDocument)
    document->ClearDocument();

  GroupCell *last = NULL;
  wxString line = wxEmptyString;
  wxChar lastChar = wxT(' ');
  wxString::iterator ch = macContents.begin();
  while (ch != macContents.end())
  {

    // Handle comments
    if((*ch == '*') && (lastChar == '/'))
    {
      // Does the current line contain nothing but a comment?
      bool isCommentLine = false;
      wxString test = line;
      test.Trim(false);
      if(test == wxT("/"))
      {
        isCommentLine = true;
        line.Trim(false);
      }

      // Skip to the end of the comment
      while (ch != macContents.end())
      {
        line += *ch;

        if ((lastChar == wxT('*')) && (*ch == wxT('/')))
        {
          lastChar = *ch;
          if(ch != macContents.end())
            ++ch;
          break;
        }

        lastChar = *ch;
        if(ch != macContents.end())
          ++ch;
      }

      if(isCommentLine)
      {
        line.Trim(true);
        line.Trim(false);

        // Is this a comment from wxMaxima?
        if(line.StartsWith(wxT("/* [wxMaxima: ")))
        {

          // Add the rest of this comment block to the "line".
          while(
            (
              !(
                  (line.EndsWith(" end   ] */")) ||
                  (line.EndsWith(" end   ] */\n"))
                  )
              ) &&
            (ch != macContents.end())
            )
          {
            while(ch != macContents.end())
            {
              wxChar c = *ch;
              line += *ch;
              ++ch;
              if(c == wxT('\n'))
                break;
            }
          }

          // If the last block was a caption block we need to read in the image
          // the caption was for, as well.
          if(line.StartsWith(wxT("/* [wxMaxima: caption start ]")))
          {
            if(ch != macContents.end())
            {
              line += *ch;
              ++ch;
            }
            while(ch != macContents.end())
            {
              wxChar c = *ch;
              line += *ch;
              ++ch;
              if(c == wxT('\n'))
                break;
            }
            while(
              (
                !(
                  (line.EndsWith(" end   ] */")) ||
                  (line.EndsWith(" end   ] */\n"))
                  )
              ) &&
              (ch != macContents.end())
              )
            {
              while(ch != macContents.end())
              {
                wxChar c = *ch;
                line += *ch;
                ++ch;
                if(c == wxT('\n'))
                  break;
              }
            }
          }

          //  Convert the comment block to an array of lines
          wxStringTokenizer tokenizer(line, "\n");
          wxArrayString commentLines;
          while ( tokenizer.HasMoreTokens() )
            commentLines.Add(tokenizer.GetNextToken());

          // Interpret this array of lines as wxm code.
          GroupCell *cell;
          document->InsertGroupCells(
            cell = m_worksheet->CreateTreeFromWXMCode(&commentLines),
            last);
          last = cell;

        }
          else
        {
          GroupCell *cell;

          if((line.StartsWith("/* ")) || (line.StartsWith("/*\n")))
            line = line.SubString(3,line.length()-1);
          else
            line = line.SubString(2,line.length()-1);

          if((line.EndsWith(" */")) || (line.EndsWith("\n*/")))
            line = line.SubString(0,line.length()-4);
          else
            line = line.SubString(0,line.length()-3);

          document->InsertGroupCells(
            cell = new GroupCell(&(document->m_configuration),
                                 GC_TYPE_TEXT, &document->m_cellPointers,
                                 line),
            last);
          last = cell;
        }

        line = wxEmptyString;
      }
    }
    // Handle strings
    else if((*ch == '\"') )
    {
      // Skip to the end of the string
      while (ch != macContents.end())
      {
        line += *ch;

        if ((*ch == wxT('\"')))
        {
          lastChar = *ch;
          if(ch != macContents.end())
            ++ch;
          break;
        }
      }
    }
    // Handle escaped chars
    else if((*ch == '\\') )
    {
      if(ch != macContents.end())
      {
        line += *ch;
        lastChar = *ch;
        ++ch;
      }
    }
    else
    {
      line += *ch;

      // A line ending followed by a new line means: We want to insert a new code cell.
      if(((lastChar == wxT('$')) || ((lastChar == wxT(';')))) && (*ch == wxT('\n')))
      {
        line.Trim(true);
        line.Trim(false);
        GroupCell *cell;
        document->InsertGroupCells(
          cell = new GroupCell(&(document->m_configuration),
                        GC_TYPE_CODE, &document->m_cellPointers, line),
          last);
        last = cell;
        line = wxEmptyString;
      }
      lastChar = *ch;
      if(ch != macContents.end())
        ++ch;
    }
  }

  line.Trim(true);
  line.Trim(false);
  if(line != wxEmptyString)
  {
    document->InsertGroupCells(
      new GroupCell(&(document->m_configuration),
                    GC_TYPE_CODE, &document->m_cellPointers, line),
      last);
  }

  if (clearDocument)
  {
//    m_worksheet->m_currentFile = file.SubString(0,file.Length()-4) + wxT("wxmx");
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
bool wxMaxima::OpenWXMFile(wxString file, Worksheet *document, bool clearDocument)
{
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  RightStatusText(_("Opening file"));
  wxWindowUpdateLocker noUpdates(document);

  // open wxm file
  wxTextFile inputFile(file);
  wxArrayString *wxmLines = NULL;

  if (!inputFile.Open())
  {
    wxMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  if (inputFile.GetFirstLine() !=
      wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"))
  {
    inputFile.Close();
    wxMessageBox(_("wxMaxima encountered an error loading ") + file, _("Error"), wxOK | wxICON_EXCLAMATION);
    return false;
  }
  wxmLines = new wxArrayString();
  wxString line;
  for (line = inputFile.GetFirstLine();
       !inputFile.Eof();
       line = inputFile.GetNextLine())
  {
    wxmLines->Add(line);
  }
  wxmLines->Add(line);

  inputFile.Close();

  GroupCell *tree = m_worksheet->CreateTreeFromWXMCode(wxmLines);

  wxDELETE(wxmLines);

  // from here on code is identical for wxm and wxmx
  if (clearDocument)
  {
    document->ClearDocument();
    StartMaxima();
  }

  document->InsertGroupCells(tree); // this also requests a recalculate

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
  RemoveTempAutosavefile();
  return true;
}

bool wxMaxima::OpenWXMXFile(wxString file, Worksheet *document, bool clearDocument)
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
    RemoveTempAutosavefile();
    return true;
  }

  // open wxmx file
  wxXmlDocument xmldoc;

  // We get only absolute paths so the path should start with a "/"
  //if(!file.StartsWith(wxT("/")))
  //  file = wxT("/") + file;

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

  uriCorector1.ReplaceFirst(&wxmxURI,wxT("file:///\\1:"));
  uriCorector2.ReplaceFirst(&wxmxURI,wxT("file:///\\1:"));
#endif
  // The URI of the wxm code contained within the .wxmx file
  wxString filename = wxmxURI + wxT("#zip:content.xml");

  // Open the file
  wxFSFile *fsfile = fs.OpenFile(filename);
  if (!fsfile)
  {
    filename = wxmxURI + wxT("#zip:/content.xml");
    fsfile = fs.OpenFile(filename);
  }

  // Did we succeed in opening the file?
  if (fsfile)
  {
    // Let's see if we can load the XML contained in this file.
    if (!xmldoc.Load(*(fsfile->GetStream()), wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES))
    {
      // If we cannot read the file a typical error in old wxMaxima versions was to include
      // a letter of ascii code 27 in content.xml. Let's filter this char out.

      // Re-open the file.
      wxDELETE(fsfile);
      fsfile = fs.OpenFile(filename);
      if (fsfile)
      {
        // Read the file into a string
        wxString s;
        wxTextInputStream istream1(*fsfile->GetStream(), wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
        while (!fsfile->GetStream()->Eof())
          s += istream1.ReadLine() + wxT("\n");

        // Remove the illegal character
        s.Replace(wxT('\x1b'), wxT("|"));

        {
          // Write the string into a memory buffer
          wxMemoryOutputStream ostream;
          wxTextOutputStream txtstrm(ostream);
          txtstrm.WriteString(s);
          wxMemoryInputStream istream(ostream);

          // Try to load the file from the memory buffer.
          xmldoc.Load(istream, wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
        }
      }
    }
  }
  else
  {
    if(m_worksheet)
    {
      m_worksheet->RecalculateForce();
      m_worksheet->RecalculateIfNeeded();
    }
    wxMessageBox(_("wxMaxima cannot open content.xml in the .wxmx zip archive ") + file +
                 wxT(", URI=") + filename, _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  wxDELETE(fsfile);

  if (!xmldoc.IsOk())
  {
    wxMessageBox(_("wxMaxima cannot read the xml contents of ") + file, _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    StatusMaximaBusy(waiting);
    RightStatusText(_("File could not be opened"));
    return false;
  }

  // start processing the XML file
  if (xmldoc.GetRoot()->GetName() != wxT("wxMaximaDocument"))
  {
    wxMessageBox(_("xml contained in the file claims not to be a wxMaxima worksheet. ") + file, _("Error"),
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
  GroupCell *tree = CreateTreeFromXMLNode(xmlcells, wxmxURI);

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

  document->InsertGroupCells(tree); // this also requests a recalculate
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
        pos = dynamic_cast<GroupCell *>(pos->m_next);

    if (pos)
      m_worksheet->SetHCaret(pos);
  }
  StatusMaximaBusy(waiting);
  RemoveTempAutosavefile();

  return true;
}

bool wxMaxima::CheckWXMXVersion(wxString docversion)
{
  double version = 1.0;
  if (docversion.ToDouble(&version))
  {
    int version_major = int(version);
    int version_minor = int(10 * (version - double(version_major)));

    if (version_major > DOCUMENT_VERSION_MAJOR)
    {
      wxMessageBox(_("Document was saved using a newer version of wxMaxima. Please update your wxMaxima."),
                   _("Error"), wxOK | wxICON_EXCLAMATION);
      RightStatusText(_("File could not be opened"));
      return false;
    }
    if (version_minor > DOCUMENT_VERSION_MINOR)
      wxMessageBox(
              _("Document was saved using a newer version of wxMaxima so it may not load correctly. Please update your wxMaxima."),
              _("Warning"), wxOK | wxICON_EXCLAMATION);
  }
  return true;
}

bool wxMaxima::OpenXML(wxString file, Worksheet *document)
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
    wxMessageBox(
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
    wxMessageBox(_("xml contained in the file claims not to be a wxMaxima worksheet. ") + file, _("Error"),
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
  GroupCell *tree = CreateTreeFromXMLNode(xmlcells, file);

  document->ClearDocument();
  StartMaxima();
  document->InsertGroupCells(tree); // this also requests a recalculate
  m_worksheet->m_currentFile = file;
  ResetTitle(true, true);
  document->RequestRedraw();
  m_worksheet->SetDefaultHCaret();
  m_worksheet->SetFocus();
  SetCWD(file);

  StatusMaximaBusy(waiting);
  return true;
}

GroupCell *wxMaxima::CreateTreeFromXMLNode(wxXmlNode *xmlcells, wxString wxmxfilename)
{
  MathParser mp(&m_worksheet->m_configuration, &m_worksheet->m_cellPointers, wxmxfilename);
  GroupCell *tree = NULL;
  GroupCell *last = NULL;

  bool warning = true;

  if (xmlcells)
    xmlcells = xmlcells->GetChildren();

  while (xmlcells != NULL)
  {
    if (xmlcells->GetType() != wxXML_TEXT_NODE)
    {
      Cell *mc = mp.ParseTag(xmlcells, false);
      if (mc != NULL)
      {
        GroupCell *cell = dynamic_cast<GroupCell *>(mc);

        if (last == NULL)
        {
          // first cell
          last = tree = cell;
        }
        else
        {
          // The rest of the cells
          last->m_next = last->m_nextToDraw = cell;
          last->m_next->m_previous = last->m_next->m_previousToDraw = last;

          last = dynamic_cast<GroupCell *>(last->m_next);
        }
      }
      else if (warning)
      {
        wxMessageBox(_("Parts of the document will not be loaded correctly!"), _("Warning"),
                     wxOK | wxICON_WARNING);
        warning = false;
      }
    }
    xmlcells = xmlcells->GetNext();
  }
  return tree;
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
  wxString gnuplotbin(wxT("/Applications/Gnuplot.app/Contents/Resources/bin/gnuplot"));
  if (wxFileExists(gnuplotbin))
    cmd += wxT("\n:lisp-quiet (setf $gnuplot_command \"") + gnuplotbin + wxT("\")\n");
#endif
  cmd.Replace(wxT("\\"),wxT("/"));
  SendMaxima(cmd);

  wxString wxmaximaversion_lisp(wxT(GITVERSION));
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
  wxString parameters, command = configuration->MaximaLocation();
  wxConfig::Get()->Read(wxT("parameters"), &parameters);


#if defined (__WXOSX__)
  if (command.EndsWith(wxT(".app"))) // if pointing to a Maxima.app
    command.Append(wxT("/Contents/Resources/maxima.sh"));
#endif
   // if 'maxima' is not searched in the path, check, if the file exists.
  if (command.Cmp("maxima")!=0) {
    if (!wxFileExists(command)) {
      wxMessageBox(_("Can not start maxima. The most probable cause is that maxima isn't installed (it can be downloaded from http://maxima.sourceforge.net) or in wxMaxima's config dialogue the setting for maxima's location is wrong."),
                   _("Warning"),
                   wxOK | wxICON_EXCLAMATION);
      LeftStatusText(_("Please configure wxMaxima with 'Edit->Configure'."));
    }
  }
  if (params) {
    // escape quotes
    command.Replace(wxT("\""), wxT("\\\""));
    // surround with quotes
    return wxT("\"") + command + wxT("\" ") + parameters;
  }
  else
  {
    command = wxT("\"") + command + wxT("\"");
    return command;
  }
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
    if (!ShowTips && !force)
      return;
  }

  TipOfTheDay *tip = new TipOfTheDay(this);
  tip->Show();
}

wxString wxMaxima::GetHelpFile()
{
  // Some operating systems don't like "//" or similar in paths.
  wxFileName helpFile(GetHelpFile2());
  helpFile.MakeAbsolute();
  return helpFile.GetFullPath();
}

wxString wxMaxima::GetHelpFile2()
{
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
    headerFile = wxEmptyString;

  wxString searchText = _("Searching for maxima help file %s");
  headerFile = m_maximaDocDir + wxT("/maxima.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/html/maxima.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  // Gentoo needs this one
  headerFile = wxString::Format("/usr/share/doc/maxima-%s/html/maxima.hhp",m_maximaVersion);
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/../html/maxima.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/../doc/html/maxima.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/doc/html/maxima.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/header.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/html/header.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/../html/header.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/../doc/html/header.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/doc/html/header.hhp");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/../html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/../doc/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_worksheet->m_configuration->MaximaShareDir() + wxT("/doc/html/maxima_singlepage.html");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  #ifdef __WXMSW__
  headerFile = m_maximaDocDir + wxT("/chm/maxima.chm");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;

  headerFile = m_maximaDocDir + wxT("/../chm/maxima.chm");
  wxLogMessage(wxString::Format(searchText, headerFile));
  if(wxFileExists(headerFile))
    return headerFile;
  #endif

  return wxEmptyString;
}

void wxMaxima::ShowHTMLHelp(wxString helpfile, wxString keyword)
{
  if(GetHelpFile().EndsWith("hhp"))
    m_htmlhelpCtrl.AddBook(GetHelpFile());
  else
    m_htmlhelpCtrl.AddBook(helpfile);

  if ((keyword == wxT("%")) ||
      (keyword == wxT(" << Graphics >> ")) ||
      (keyword == wxEmptyString))
    m_htmlhelpCtrl.DisplayContents();
  else
    m_htmlhelpCtrl.KeywordSearch(keyword, wxHELP_SEARCH_INDEX);
}

#if defined (__WXMSW__)
void wxMaxima::ShowCHMHelp(wxString helpfile,wxString keyword)
{
  if (m_chmhelpFile != helpfile)
    m_chmhelpCtrl.LoadFile(helpfile);

  if ((keyword == wxT("%")) ||
      (keyword == wxT(" << Graphics >> ")) ||
      (keyword.IsEmpty())
    )
    m_chmhelpCtrl.DisplayContents();
  else
    m_chmhelpCtrl.KeywordSearch(keyword, wxHELP_SEARCH_INDEX);
}
#endif

wxString wxMaxima::SearchwxMaximaHelp()
{
  wxString failmsg = _("No helpfile found at %s.");
  wxString helpfile;
  long language = m_locale->GetLanguage();
  if(language == wxLANGUAGE_DEFAULT)
    language = wxLocale::GetSystemLanguage();
  const wxLanguageInfo *langinfo = m_locale->GetLanguageInfo(language);

  wxASSERT(langinfo);
  
  if(langinfo)
  {
    wxString lang_long = langinfo->GetLocaleName();
    wxString lang_short = lang_long.Left(lang_long.Find('_'));
    helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima_") + lang_long + ".hhp";
#if defined (__WXMSW__)
    // Cygwin uses /c/something instead of c:/something and passes this path to the
    // web browser - which doesn't support cygwin paths => convert the path to a
    // native windows pathname if needed.
    if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
    if(wxFileExists(helpfile))
      return helpfile;
    wxLogMessage(wxString::Format(failmsg, helpfile));
    
    helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima_") + lang_short + ".hhp";
#if defined (__WXMSW__)
    // Cygwin uses /c/something instead of c:/something and passes this path to the
    // web browser - which doesn't support cygwin paths => convert the path to a
    // native windows pathname if needed.
    if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
    if(wxFileExists(helpfile))
    return helpfile;
    wxLogMessage(wxString::Format(failmsg, helpfile));

    helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.") + lang_long + ".hhp";
#if defined (__WXMSW__)
    // Cygwin uses /c/something instead of c:/something and passes this path to the
    // web browser - which doesn't support cygwin paths => convert the path to a
    // native windows pathname if needed.
    if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
    if(wxFileExists(helpfile))
      return helpfile;
    wxLogMessage(wxString::Format(failmsg, helpfile));
    
    helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.") + lang_short + ".Hhp";
#if defined (__WXMSW__)
    // Cygwin uses /c/something instead of c:/something and passes this path to the
    // web browser - which doesn't support cygwin paths => convert the path to a
    // native windows pathname if needed.
    if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
    if(wxFileExists(helpfile))
      return helpfile;
    wxLogMessage(wxString::Format(failmsg, helpfile));
  }
  
  helpfile = Dirstructure::Get()->HelpDir() + wxT("/wxmaxima.hhp");
#if defined (__WXMSW__)
  // Cygwin uses /c/something instead of c:/something and passes this path to the
  // web browser - which doesn't support cygwin paths => convert the path to a
  // native windows pathname if needed.
  if(helpfile.Length()>1 && helpfile[1]==wxT('/')){helpfile[1]=helpfile[2];helpfile[2]=wxT(':');}
#endif // __WXMSW__
  if(!wxFileExists(helpfile))
    wxLogMessage(wxString::Format(failmsg, helpfile));
  return helpfile;
}

void wxMaxima::ShowWxMaximaHelp()
{
  wxString helpfile = SearchwxMaximaHelp();
  
  wxLogMessage(wxString::Format(_("wxMaxima help should be at %s."),helpfile));
  wxString helpcontents = helpfile;
  helpcontents.Replace(".hhp",".html");
  if(GetHelpFile().EndsWith("hhp"))
    m_htmlhelpCtrl.AddBook(GetHelpFile());
  m_htmlhelpCtrl.AddBook(helpfile);

  m_htmlhelpCtrl.DisplaySection(helpcontents);
}

void wxMaxima::ShowMaximaHelp(wxString keyword)
{
  if(keyword == wxT("wxdraw"))
     keyword = wxT("draw");
  if(keyword == wxT("wxdraw2d"))
     keyword = wxT("draw2d");
  if(keyword == wxT("wxdraw3d"))
     keyword = wxT("draw3d");
  if(keyword == wxT("with_slider_draw"))
     keyword = wxT("draw");
  if(keyword == wxT("with_slider_draw2d"))
     keyword = wxT("draw2d");
  if(keyword == wxT("with_slider_draw3d"))
     keyword = wxT("draw3d");
  wxString MaximaHelpFile = GetHelpFile();
  if (MaximaHelpFile.Length() == 0)
  {
    wxMessageBox(_("wxMaxima could not find help files."
                           "\n\nPlease check your installation."),
                 _("Error"), wxICON_ERROR | wxOK);
    return;
  }

#if defined (__WXMSW__)
  if(MaximaHelpFile.EndsWith(wxT(".chm")))
    ShowCHMHelp(MaximaHelpFile,keyword);
  else
#endif
  {
    if(MaximaHelpFile.EndsWith(wxT(".html")))
      wxLaunchDefaultBrowser(wxURI("file://"+MaximaHelpFile+wxT("#Item: ")+keyword).BuildURI());
    else
      ShowHTMLHelp(MaximaHelpFile,keyword);
  }
}

bool wxMaxima::InterpretDataFromMaxima()
{
  if(m_newCharsFromMaxima.IsEmpty())
    return false;

  if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
    m_xmlInspector->Add_FromMaxima(m_newCharsFromMaxima);
  // This way we can avoid searching the whole string for a
  // ending tag if we have received only a few bytes of the
  // data between 2 tags
  m_currentOutputEnd = m_currentOutput.Right(30) + m_newCharsFromMaxima;

  m_currentOutput += m_newCharsFromMaxima;
  m_newCharsFromMaxima = wxEmptyString;
  if ((m_xmlInspector) && (IsPaneDisplayed(menu_pane_xmlInspector)))
    m_xmlInspector->Add_FromMaxima(m_newCharsFromMaxima);

  if (!m_dispReadOut &&
      (m_currentOutput != wxT("\n")) &&
      (m_currentOutput != wxT("<wxxml-symbols></wxxml-symbols>")))
  {
    StatusMaximaBusy(transferring);
    m_dispReadOut = true;
  }

  size_t length_old = -1;

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
      if(newActiveCell != oldActiveCell)
        m_worksheet->m_cellPointers.SetWorkingGroup(oldActiveCell);
      // Handle the <mth> tag that contains math output and sometimes text.
      ReadMath(m_currentOutput);

      // The following function calls each extract and remove one type of XML tag
      // information from the beginning of the data string we got - but only do so
      // after the closing tag has been transferred, as well.
      ReadLoadSymbols(m_currentOutput);

      // Discard startup warnings
      ReadSuppressedOutput(m_currentOutput);

      // Let's see if maxima informs us about the values of variables
      ReadVariables(m_currentOutput);

      // Let's see if maxima tells us to add new symbols to the watchlist
      ReadAddVariables(m_currentOutput);

      // Handle the XML tag that contains Status bar updates
      ReadStatusBar(m_currentOutput);

      // Handle text that isn't XML output: Mostly Error messages or warnings.
      ReadMiscText(m_currentOutput);
    }
    else
      // This function determines the port maxima is running on from  the text
      // maxima outputs at startup. This piece of text is afterwards discarded.
      ReadFirstPrompt(m_currentOutput);

    // Switch to the WorkingGroup the next bunch of data is for.
    if(newActiveCell != oldActiveCell)
      m_worksheet->m_cellPointers.SetWorkingGroup(newActiveCell);
  }
  return true;
}

///--------------------------------------------------------------------------------
///  Idle event
///--------------------------------------------------------------------------------

void wxMaxima::OnIdle(wxIdleEvent &event)
{
  // If wxMaxima has to open a file on startup we wait for that until we have
  // a valid draw context for size calculations.
  //
  // The draw context is created on displaying the worksheet for the 1st time
  // and after drawing the worksheet onIdle is called => we won't miss this
  // event when we wait for it here.
  if ((m_worksheet != NULL) && (m_worksheet->m_configuration->GetDC() != NULL) &&
      (m_openFile != wxEmptyString))
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

  // Update the info what maxima is currently doing
  UpdateStatusMaximaBusy();

  // Update the info how long the evaluation queue is
  if(m_updateEvaluationQueueLengthDisplay)
  {
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
    m_updateEvaluationQueueLengthDisplay = false;

    // We have shown that we are still alive => If maxima already offers new data
    // we process this data first and then continue with the idle task.
    if(
      (m_worksheet->m_scheduleUpdateToc) || (m_updateControls) || (m_worksheet->RedrawRequested()) ||
      (
         (m_worksheet->m_findDialog != NULL) &&
         (
           (m_oldFindString != m_worksheet->m_findDialog->GetData()->GetFindString()) ||
           (m_oldFindFlags != m_worksheet->m_findDialog->GetData()->GetFlags())
           )
        ) ||
      ((m_newLeftStatusText) || (m_newRightStatusText)) ||
      ((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded()))
      )
    {
      event.RequestMore();
      return;
    }
  }

  if(m_worksheet != NULL)
  {
    bool requestMore = m_worksheet->RecalculateIfNeeded();
    m_worksheet->ScrollToCellIfNeeded();
    if(requestMore)
    {
      event.RequestMore();
      return;
    }
  }

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

      bool incrementalSearch = true;
        wxConfig::Get()->Read("incrementalSearch", &incrementalSearch);
        if ((incrementalSearch) && (m_worksheet->m_findDialog != NULL))
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
    m_updateControls = true;
    event.RequestMore();
    return;
  }

  // If nothing which is visible has changed nothing that would cause us to need
  // update the menus and toolbars has.
  if (m_updateControls)
  {
    m_updateControls = false;
    wxUpdateUIEvent dummy;
    UpdateMenus(dummy);
    UpdateToolBar(dummy);
    UpdateSlider(dummy);
    if(m_isNamed)
      ResetTitle(m_worksheet->IsSaved());
    else
      ResetTitle(false);

    // This was a half-way lengthy task => Return from the idle task so we can give
    // maxima a chance to deliver new data.
    if((m_worksheet->m_scheduleUpdateToc) ||
       ((m_newLeftStatusText) || (m_newRightStatusText)) ||
       ((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded())))
    {
      event.RequestMore();
      return;
    }
  }

  if((m_newLeftStatusText) || (m_newRightStatusText))
  {
    if(m_newRightStatusText)
    {
      SetStatusText(m_rightStatusText, 1);
      m_newRightStatusText = false;
    }
    if(m_newLeftStatusText)
    {
      SetStatusText(m_leftStatusText,0);
      m_newLeftStatusText = false;
    }

    if((m_worksheet->m_scheduleUpdateToc) ||
       ((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded()))
      )
    {
      event.RequestMore();
      return;
    }
  }

  // If we have set the flag that tells us we should update the table of
  // contents sooner or later we should do so now that wxMaxima is idle.
  if (m_worksheet->m_scheduleUpdateToc)
  {
    if (m_worksheet->m_tableOfContents)
    {
      m_worksheet->m_scheduleUpdateToc = false;
      GroupCell *cursorPos;
      cursorPos = m_worksheet->GetHCaret();
      if ((!m_worksheet->HCaretActive()) && (cursorPos == m_worksheet->GetLastCell()))
      {
        if (m_worksheet->GetActiveCell() != NULL)
          cursorPos = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
        else
          cursorPos = m_worksheet->FirstVisibleGC();
      }
      m_worksheet->m_tableOfContents->UpdateTableOfContents(m_worksheet->GetTree(), cursorPos);
    }
    m_worksheet->m_scheduleUpdateToc = false;

    if((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded()))
    {
      event.RequestMore();
      return;
    }
  }
  if((m_xmlInspector != NULL) && (m_xmlInspector->UpdateNeeded()))
    m_xmlInspector->Update();

  UpdateDrawPane();

  // On MS Windows sometimes we don't get a wxSOCKET_INPUT event on input.
  // Let's trigger interpretation of new input if we don't have anything
  // else to do just to make sure that wxMaxima will eventually restart
  // receiving data.
  wxSocketEvent dummy(wxSOCKET_INPUT);
  ClientEvent(dummy);

  // Tell wxWidgets it can process its own idle commands, as well.
  event.Skip();
}

void wxMaxima::UpdateDrawPane()
{
  if(m_drawPane)
  {
    EditorCell *editor = m_worksheet->GetActiveCell();
    if(editor)
    {
      wxString command = m_worksheet->GetActiveCell()->GetFullCommandUnderCursor();
      int dimensions = 0;
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
      m_drawPane->SetDimensions(dimensions);
    }
    else
      m_drawPane->SetDimensions(0);
  }
  else
  {
    m_drawPane->SetDimensions(0);
  }
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::MenuCommand(wxString cmd)
{
  m_worksheet->SetFocus();
  m_worksheet->OpenHCaret(cmd);
  m_worksheet->AddCellToEvaluationQueue(dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup()));
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
    case ToolBar::tb_print:
    {
      wxPrintDialogData printDialogData;
      if (m_printData)
        printDialogData.SetPrintData(*m_printData);
      wxPrinter printer(&printDialogData);
      wxString title(_("wxMaxima document")), suffix;

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
        GroupCell *copy = m_worksheet->CopyTree();
        printout.SetData(copy);
        wxBusyCursor crs;
        if (printer.Print(this, &printout, true))
        {
          wxDELETE(m_printData);
          m_printData = new wxPrintData(printer.GetPrintDialogData().GetPrintData());
        }
      }
      m_worksheet->RecalculateForce();
      m_worksheet->RequestRedraw();
      break;
    }
  }
}

void wxMaxima::UpdateMenus(wxUpdateUIEvent &WXUNUSED(event))
{
  if (!m_worksheet)
    return;
  wxASSERT_MSG((!m_worksheet->HCaretActive()) || (m_worksheet->GetActiveCell() == NULL),
               _("Both horizontal and vertical cursor active at the same time"));

  m_MenuBar->Enable(menu_copy_from_worksheet, m_worksheet->CanCopy(true));
  m_MenuBar->Enable(menu_cut, m_worksheet->CanCut());
  m_MenuBar->Enable(menu_copy_tex_from_worksheet, m_worksheet->CanCopy());
  m_MenuBar->Enable(Worksheet::popid_copy_mathml, m_worksheet->CanCopy());
  m_MenuBar->Enable(menu_copy_as_bitmap, m_worksheet->CanCopy());
  m_MenuBar->Enable(menu_copy_as_svg, m_worksheet->CanCopy());
  #if wxUSE_ENH_METAFILE
  m_MenuBar->Enable(menu_copy_as_emf, m_worksheet->CanCopy());
  #endif
  m_MenuBar->Enable(menu_copy_as_rtf, m_worksheet->CanCopy());
  m_MenuBar->Enable(menu_copy_to_file, m_worksheet->CanCopy());
  m_MenuBar->Enable(menu_copy_text_from_worksheet, m_worksheet->CanCopy(true));
  m_MenuBar->Enable(menu_select_all, m_worksheet->GetTree() != NULL);
  m_MenuBar->Enable(menu_undo, m_worksheet->CanUndo());
  m_MenuBar->Enable(menu_redo, m_worksheet->CanRedo());
  m_MenuBar->Enable(menu_interrupt_id, m_pid > 0);
  m_MenuBar->Enable(Worksheet::popid_comment_selection,
                  (m_worksheet->GetActiveCell() != NULL) && (m_worksheet->GetActiveCell()->SelectionActive()));
  m_MenuBar->Enable(menu_evaluate, (
                    (m_worksheet->GetActiveCell() != NULL) ||
                          (m_worksheet->CellsSelected())
                    )
    );

  m_MenuBar->Enable(menu_evaluate_all_visible, m_worksheet->GetTree() != NULL);
  m_MenuBar->Enable(ToolBar::tb_evaltillhere,
                  (m_worksheet->GetTree() != NULL) &&
                  (m_worksheet->CanPaste()) &&
                  (m_worksheet->GetHCaret() != NULL)
  );

  m_MenuBar->Enable(menu_jumptoerror, !m_worksheet->m_cellPointers.m_errorList.Empty());
  m_MenuBar->Enable(menu_save_id, (!m_fileSaved));

  for (int id = menu_pane_math; id <= menu_pane_stats; id++)
    m_MenuBar->Check(id, IsPaneDisplayed(static_cast<Event>(id)));
  m_MenuBar->Check(menu_show_toolbar, ToolbarIsShown());

  bool hidecode = !(m_worksheet->m_configuration->ShowCodeCells());
  m_MenuBar->Check(ToolBar::tb_hideCode, hidecode);

  if (m_worksheet->GetTree() != NULL)
  {
    m_MenuBar->Enable(Worksheet::popid_divide_cell, m_worksheet->GetActiveCell() != NULL);
    m_MenuBar->Enable(Worksheet::popid_merge_cells, m_worksheet->CanMergeSelection());
    m_MenuBar->Enable(wxID_PRINT, true);
  }
  else
  {
    m_MenuBar->Enable(Worksheet::popid_divide_cell, false);
    m_MenuBar->Enable(Worksheet::popid_merge_cells, false);
    m_MenuBar->Enable(wxID_PRINT, false);
  }
  double zf = m_worksheet->m_configuration->GetZoomFactor();
  if (zf < Configuration::GetMaxZoomFactor())
    m_MenuBar->Enable(Worksheet::menu_zoom_in, true);
  else
    m_MenuBar->Enable(Worksheet::menu_zoom_in, false);
  if (zf > Configuration::GetMinZoomFactor())
    m_MenuBar->Enable(Worksheet::menu_zoom_out, true);
  else
    m_MenuBar->Enable(Worksheet::menu_zoom_out, false);

}

void wxMaxima::UpdateToolBar(wxUpdateUIEvent &WXUNUSED(event))
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
          (m_client != NULL)
  );

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
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      if (m_worksheet->GetWorkingGroup() == NULL)
      {
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      }
      break;
    case sending:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      if (m_worksheet->GetWorkingGroup() == NULL)
      {
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
        m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      }
      break;
    case calculating:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, follow);
      break;
    case transferring:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, follow);
      break;
    case parsing:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, true);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, follow);
      break;
    case wait_for_start:
    case disconnected:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      break;
    case process_wont_start:
      m_worksheet->m_mainToolBar->ShowFollowBitmap();
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_interrupt, false);
      m_worksheet->m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
      break;
  }
}

wxString wxMaxima::ExtractFirstExpression(wxString entry)
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
  if (m_worksheet->GetActiveCell() != NULL)
    return ExtractFirstExpression(m_worksheet->GetActiveCell()->ToString());
  return wxT("%");
}

bool wxMaxima::OpenFile(wxString file, wxString cmd)
{
  wxBusyCursor crs;
  bool retval = true;
  if (file == wxEmptyString)
  {
    wxLogError(_("Trying to open a file with an empty name!"));
    return false;
  }
  if(!(wxFileExists(file)))
  {
    wxLogError(_("Trying to open the non-existing file %s"), file);
    return false;
  }

  m_lastPath = wxPathOnly(file);
  wxString unixFilename(file);
#if defined __WXMSW__
  unixFilename.Replace(wxT("\\"), wxT("/"));
#endif

  if (cmd.Length() > 0)
  {
    MenuCommand(cmd + wxT("(\"") + unixFilename + wxT("\")$"));
    if(cmd == wxT("load"))
    {
      ReReadConfig();
      m_recentPackages.AddDocument(unixFilename);
      ReReadConfig();
    }
  }
  else if (file.EndsWith(wxT(".wxm")))
  {
    retval = OpenWXMFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }

  else if (file.EndsWith(wxT(".mac")))
  {
    retval = OpenMACFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
      m_recentDocuments.AddDocument(file);
      ReReadConfig();
    }
  }
  else if (file.EndsWith(wxT(".out")))
  {
    retval = OpenMACFile(file, m_worksheet);
    if(retval)
    {
      ReReadConfig();
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

  m_isNamed = true;

  UpdateRecentDocuments();

  if ((m_worksheet->m_configuration->AutoSaveMiliseconds() > 0) && (m_worksheet->m_currentFile.Length() > 0))
    m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());

  if (m_worksheet)m_worksheet->TreeUndo_ClearBuffers();
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
    LeftStatusText(wxString::Format("Errors trying to open the file %s.", file));

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
      if ((m_worksheet->m_configuration->AutoSaveMiliseconds() > 0) && (m_worksheet->m_currentFile.Length() > 0))
        m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());
      return false;
    }
  }

  if (file.Length())
  {
    if (!file.EndsWith(wxT(".wxm")) &&
        (!file.EndsWith(wxT(".wxmx")))
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

    m_worksheet->m_currentFile = file;
    m_lastPath = wxPathOnly(file);
    if (file.EndsWith(wxT(".wxmx")))
    {
      if (!m_worksheet->ExportToWXMX(file))
      {
        StatusSaveFailed();
        if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
          m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());
        return false;
      }
      else
        m_isNamed = true;

    }
    else
    {
      if (!m_worksheet->ExportToMAC(file))
      {
        config->Write(wxT("defaultExt"), wxT("wxm"));

        StatusSaveFailed();
        if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
          m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds() > 0);
        return false;
      }
      else
        m_isNamed = true;
    }

    m_recentDocuments.AddDocument(file);
    SetCWD(file);

    if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
      m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds() > 0);
    StatusSaveFinished();
    RemoveTempAutosavefile();
    UpdateRecentDocuments();
    return true;
  }

  if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
    m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds() > 0);

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
    int len = 0;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStdout->CanRead()))
    {
      o += ch;
      len++;
    }

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = _("Message from the stdout of Maxima: ") + o;
    if ((o_trimmed != wxEmptyString) && (!o.StartsWith("Connecting Maxima to server on port")) &&
        (!m_first))
      DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
  }
  if (m_process->IsErrorAvailable())
  {
    wxASSERT_MSG(m_maximaStderr != NULL, wxT("Bug: Trying to read from maxima but don't have a error input stream"));
    wxTextInputStream istrm(*m_maximaStderr, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    int len = 0;
    while (((ch = istrm.GetChar()) != wxT('\0')) && (m_maximaStderr->CanRead()))
    {
      o += ch;
      len++;
    }

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
      m_worksheet->m_cellPointers.m_errorList.Add(m_worksheet->GetWorkingGroup(true));
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

  if (m_worksheet->m_notificationMessage != NULL)
  {
    if (m_worksheet->GetWorkingGroup(true) !=
        m_worksheet->m_notificationMessage->m_errorNotificationCell)
      m_worksheet->SetNotification(_("Maxima has issued an error!"),wxICON_ERROR);
    m_worksheet->m_notificationMessage->m_errorNotificationCell = m_worksheet->GetWorkingGroup(true);
  }

  m_exitAfterEval = false;
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
      wxStringTokenizer tokens(line,wxT(" "));
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

      wxStringTokenizer tokens(line,wxT(" "));
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
    case WAITFORSTRING_ID:
      if(InterpretDataFromMaxima())
        wxLogMessage(_("String from maxima apparently didn't end in a newline"));
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
        if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
        {
          if(SaveNecessary())
          {
            if ((m_worksheet->m_currentFile.Length() > 0))
            {
              // Automatically safe the file for the user making it seem like the file
              // is always saved -
              SaveFile(false);
            }
            else
            {
              // The file hasn't been given a name yet.
              // Save the file and remember the file name.
              wxString name = GetTempAutosavefileName();
              m_worksheet->ExportToWXMX(name);
              RegisterAutoSaveFile();
              m_fileSaved = false;
            }
          }

          m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());
        }
      }
      break;
  }
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
  case mac_closeId:
    Close();
    break;

    case ToolBar::tb_open:
    case menu_open_id:
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
          OpenFile(file,wxEmptyString);
        else
          wxGetApp().NewWindow(file);
        #else
        OpenFile(file,wxEmptyString);
        #endif
      }
    }
      break;

    case menu_save_as_id:
      forceSave = true;
      m_fileSaved = false;
      SaveFile(forceSave);
      // Seems like resetting the title on "file/save as" is a little bit
      // sluggish, otherwise.
      ResetTitle(m_worksheet->IsSaved(), true);
      break;
    case ToolBar::tb_save:
    case menu_save_id:
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
                                        "pdfLaTeX file (*.tex)|*.tex"
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
          if ((!file.EndsWith(wxT(".html"))) &&
              (!file.EndsWith(wxT(".mac"))) &&
              (!file.EndsWith(wxT(".tex")))
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

          if (file.EndsWith(wxT(".tex")))
          {
            StatusExportStart();

            fileExt = wxT("tex");
            // Show a busy cursor as long as we export a file.
            wxBusyCursor crs;
            if (!m_worksheet->ExportToTeX(file))
            {
              wxMessageBox(_("Exporting to TeX failed!"), _("Error!"),
                           wxOK);
              StatusExportFailed();
            }
            else
              StatusExportFinished();
          }
          else if (file.EndsWith(wxT(".mac")))
          {
            StatusExportStart();

            // Show a busy cursor as long as we export a file.
            wxBusyCursor crs;
            fileExt = wxT("mac");
            if (!m_worksheet->ExportToMAC(file))
            {
              wxMessageBox(_("Exporting to maxima batch file failed!"), _("Error!"),
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
              wxMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                           wxOK);
              StatusExportFailed();
            }
            else
              StatusExportFinished();
          }
          if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
            m_autoSaveTimer.StartOnce(m_worksheet->m_configuration->AutoSaveMiliseconds());

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

    case wxID_EXIT:
      Close();
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
  case Worksheet::popid_popup_gnuplot:
  {
    if(m_worksheet->m_cellPointers.m_selectionStart == NULL)
      return;
    if(m_worksheet->m_cellPointers.m_selectionStart->GetType() != MC_TYPE_IMAGE)
      return;
    if(m_worksheet->m_cellPointers.m_selectionStart != m_worksheet->m_cellPointers.m_selectionEnd)
      return;

    wxString gnuplotSource =
      dynamic_cast<ImgCell *>(m_worksheet->m_cellPointers.m_selectionStart)->GnuplotSource();
    if(gnuplotSource == wxEmptyString)
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

    // Find gnuplot
    wxPathList pathlist;
    pathlist.AddEnvList(wxT("PATH"));
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath());
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot/bin");
    wxString gnuplot_binary = pathlist.FindAbsoluteValidPath(wxT("gnuplot"));
    if(gnuplot_binary == wxEmptyString)
      gnuplot_binary = pathlist.FindAbsoluteValidPath(wxT("gnuplot.exe"));
    if(gnuplot_binary == wxEmptyString)
      gnuplot_binary = pathlist.FindAbsoluteValidPath(wxT("gnuplot.app"));
    if(gnuplot_binary == wxEmptyString)
      gnuplot_binary = wxT("gnuplot");
    // Execute gnuplot
    wxString cmdline = gnuplot_binary + wxT(" " + gnuplotSource + wxT(".popout"));
    wxLogMessage(_("Running gnuplot as: " + cmdline));

    m_gnuplotProcess = new wxProcess(this, gnuplot_process_id);
    if (wxExecute(cmdline,
                  wxEXEC_ASYNC|wxEXEC_SHOW_CONSOLE,
                  m_gnuplotProcess) < 0)
      wxLogMessage(_("Cannot start gnuplot"));
    break;
  }
  case wxID_PREFERENCES:
  case ToolBar::tb_pref:
  {
    wxConfigBase *config = wxConfig::Get();
    // wxGTK uses wxFileConf. ...and wxFileConf loads the config file only once
    // on inintialisation => Let's reload the config file before entering the
    // config dialogue.
    ReReadConfig();
    config = wxConfig::Get();

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
  case ToolBar::tb_copy:
  case menu_copy_from_worksheet:
    if (m_worksheet->CanCopy(true))
      m_worksheet->Copy();
    break;
  case menu_copy_text_from_worksheet:
    if (m_worksheet->CanCopy(true))
      m_worksheet->Copy(true);
    break;
  case ToolBar::tb_cut:
  case menu_cut:
    if (m_worksheet->CanCut())
      m_worksheet->CutToClipboard();
    break;
  case menu_select_all:
  case ToolBar::tb_select_all:
    m_worksheet->SelectAll();
    break;
  case ToolBar::tb_paste:
  case menu_paste:
    if (m_worksheet->CanPaste())
      m_worksheet->PasteFromClipboard();
    break;
  case menu_undo:
    if (m_worksheet->CanUndo())
      m_worksheet->Undo();
    break;
  case menu_redo:
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
                                     "Windows bitmap (*.bmp)|*.bmp|"
                                     "Portable animap (*.pnm)|*.pnm|"
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
    case Worksheet::menu_zoom_in:
      m_worksheet->SetZoomFactor(m_worksheet->m_configuration->GetZoomFactor() + 0.1);
      break;
    case Worksheet::menu_zoom_out:
      m_worksheet->SetZoomFactor(m_worksheet->m_configuration->GetZoomFactor() - 0.1);
      break;
    case menu_zoom_80:
      m_worksheet->SetZoomFactor(0.8);
      break;
    case menu_zoom_100:
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
      MenuCommand(wxT("wxsubscripts: true"));
      break;
    case menu_alwaysAutosubscript:
      MenuCommand(wxT("wxsubscripts: 'always$"));
      break;
    case menu_roundedMatrixParensYes:
      MenuCommand(wxT("lmxchar:\"(\"$rmxchar:\")\"$"));
      break;
    case menu_roundedMatrixParensNo:
      MenuCommand(wxT("lmxchar:\"[\"$rmxchar:\"]\"$"));
      break;
    case menu_fullscreen:
      ShowFullScreen(!IsFullScreen());
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
    case menu_edit_find:
    case ToolBar::tb_find:
      if (m_worksheet->m_findDialog == NULL)
        m_worksheet->m_findDialog = new FindReplaceDialog(
                this,
                &m_findData,
                _("Find and Replace"));

      if (m_worksheet->GetActiveCell() != NULL)
      {
        // Start incremental search and highlighting of search results again.
        if (m_worksheet->m_findDialog != NULL)
          m_oldFindString = wxEmptyString;

        wxString selected = m_worksheet->GetActiveCell()->GetSelectionString();
        if (selected.Length() > 0)
        {
          if (m_worksheet->m_findDialog != NULL)
            m_worksheet->m_findDialog->SetFindString(selected);
        }
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
  }
  m_worksheet->RequestRedraw();
}

void wxMaxima::OnFind(wxFindDialogEvent &event)
{
  if (!m_worksheet->FindNext(event.GetFindString(),
                           event.GetFlags() & wxFR_DOWN,
                           !(event.GetFlags() & wxFR_MATCHCASE)))
    wxMessageBox(_("No matches found!"));
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
    wxMessageBox(_("No matches found!"));
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

  wxMessageBox(wxString::Format(_("Replaced %d occurrences."), count));
  if (count > 0)
    m_worksheet->UpdateTableOfContents();
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
      if(m_worksheet->m_cellPointers.m_errorList.FirstError())
      {
        m_worksheet->SetActiveCell(dynamic_cast<GroupCell *>(m_worksheet->m_cellPointers.m_errorList.FirstError())->GetEditable());
        dynamic_cast<GroupCell *>(m_worksheet->m_cellPointers.m_errorList.FirstError())->GetEditable()->CaretToEnd();
      }
      break;
    case ToolBar::menu_restart_id:
      m_closing = true;
      m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
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
      cmd = wxT("if showtime#false then showtime:false else showtime:all$");
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
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
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
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
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

void wxMaxima::AlgebraMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
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
        if(wiz->GetValue1() == wxEmptyString)
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
        if (!(wiz->GetValue2()).ToLong(&w) ||
            !(wiz->GetValue1()).ToLong(&h) ||
            w <= 0 || h <= 0)
        {
          wxMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                       wxOK | wxICON_ERROR);
          return;
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
    wiz->SetLabel1ToolTip(_("The rule that explains how to generate the value of an list item.\n"
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
    wiz->SetLabel1ToolTip(_("The rule that explains how to generate the value of an list item.\n"
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
      cmd = wxT("algebraic : not(algebraic);");
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
      MenuCommand(wxT("if wxanimate_autoplay#false then wxanimate_autoplay:false else wxanimate_autoplay:true;"));
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
      cmd = wxT("if numer#false then numer:false else numer:true;");
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
    MenuCommand(wxT("load(\"engineering-format\")$"));
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

      description = _("wxMaxima is a graphical user interface for the computer algebra system Maxima based on wxWidgets.");

#if defined(WXMAXIMA_GIT_VERSION)
      description += wxString::Format("\n(Build from Git version: " WXMAXIMA_GIT_VERSION ")");
#endif
      description += wxString::Format(
        _("\n\nwxWidgets: %d.%d.%d\nUnicode support: %s"),
        wxMAJOR_VERSION, wxMINOR_VERSION, wxRELEASE_NUMBER,
        _("yes")
        );

      if (m_maximaVersion != wxEmptyString)
        description += _("\nMaxima version: ") + m_maximaVersion + " ("+m_maximaArch+")";
      else
        description += _("\nNot connected.");
      if (m_lispVersion != wxEmptyString)
        description += _("\nLisp: ") + m_lispType + " " + m_lispVersion;

      info.SetIcon(wxMaximaIcon());
      info.SetDescription(description);
      info.SetName(_("wxMaxima"));
      info.SetVersion(wxT(GITVERSION));
      info.SetCopyright(wxT("(C) 2004-2018 Andrej Vodopivec"));
      info.SetWebSite(wxT("https://wxMaxima-developers.github.io/wxmaxima/"));

      info.AddDeveloper(wxT("Andrej Vodopivec <andrej.vodopivec@gmail.com>"));
      info.AddDeveloper(wxT("Ziga Lenarcic <ziga.lenarcic@gmail.com>"));
      info.AddDeveloper(wxT("Doug Ilijev <doug.ilijev@gmail.com>"));
      info.AddDeveloper(wxT("Gunter Königsmann <wxMaxima@physikbuch.de>"));
      info.AddDeveloper(wxT("Elias Mårtenson"));
      info.AddDeveloper(wxT("Steven McDonald"));
      info.AddDeveloper(wxT("Tomio Arisaka"));
      info.AddDeveloper(wxT("Rene Hansen"));
      info.AddDeveloper(wxT("Wolfgang Dautermann"));
      info.AddTranslator(wxT("Innocent De Marchi (ca)"));
      info.AddTranslator(wxT("Josef Barak (cs)"));
      info.AddTranslator(wxT("Robert Marik (cs)"));
      info.AddTranslator(wxT("Jens Thostrup (da)"));
      info.AddTranslator(wxT("Harald Geyer (de)"));
      info.AddTranslator(wxT("Dieter Kaiser (de)"));
      info.AddTranslator(wxT("Gunter Königsmann (de)"));
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
      info.AddArtist(wxT("Toolbar and config icons: The TANGO Project"));
      info.AddArtist(wxT("svg version of the icon: Gunter Königsmann"));

      wxAboutBox(info);
    }
    break;

    case menu_license:
    {
      LicenseDialog *dlg = new LicenseDialog(this);
      dlg->ShowModal();
      dlg->Destroy();
    }
    break;

    case ToolBar::tb_help:
      ShowWxMaximaHelp();
      break;

    case wxID_HELP:
      if((expr != "%") && (expr != wxEmptyString))
        ShowMaximaHelp(expr);
      else
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
      MenuCommand(wxT("wxbuild_info()$"));
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

void wxMaxima::StatsMenu(wxCommandEvent &ev)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString expr = GetDefaultEntry();

  switch (ev.GetId())
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
        if (file.EndsWith(wxT(".csv")))
          format = wxT("csv");
        else if (file.EndsWith(wxT(".tab")))
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

void wxMaxima::OnClose(wxCloseEvent &event)
{
  if(event.GetEventType() == wxEVT_END_SESSION)
    KillMaxima();;

  if (SaveNecessary())
  {
    // If autosave is on we automatically save the file on closing.
    if(m_isNamed && (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0))
    {
      if (!SaveFile())
      {
        if(!SaveFile(true))
        {
          event.Veto();
          return;
        }
      }
    }
    else
    {
      int close = SaveDocumentP();

      if (close == wxID_CANCEL)
      {
        event.Veto();
        return;
      }
      else
      {
        if (close == wxID_YES)
        {
          if (!SaveFile())
          {
            if(!SaveFile(true))
            {
              event.Veto();
              return;
            }
          }
        }
      }
    }
  }

  // Log events we generate now won't appear on the log panel any more.
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

  RemoveTempAutosavefile();
  CleanUp();
  MyApp::m_topLevelWindows.remove(this);
}

void wxMaxima::PopupMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString selection = m_worksheet->GetString();
  switch (event.GetId())
  {
    case Worksheet::popid_fold:
    {
      if (m_worksheet->GetActiveCell())
      {
        // This "if" is pure paranoia. But - since the costs of an "if" are low...
        GroupCell *group = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
        if (group->IsFoldable())
          group->Fold();
        else
          group->Hide(true);
        m_worksheet->UpdateTableOfContents();
      }
      break;
    }
    case Worksheet::popid_maxsizechooser:
      if(m_worksheet->m_cellPointers.m_selectionStart != NULL)
      {
        Cell *output = dynamic_cast<GroupCell *>(m_worksheet->m_cellPointers.m_selectionStart->GetGroup())->GetLabel();
        if (output == NULL)
          return;
        if(output->GetType() != MC_TYPE_IMAGE)
          return;

        MaxSizeChooser *chooser = new MaxSizeChooser(this, -1,
                                                     dynamic_cast<ImgCell *>(output)->GetMaxWidth(),
                                                     dynamic_cast<ImgCell *>(output)->GetMaxHeight()
        );
        chooser->Centre(wxBOTH);
        if (chooser->ShowModal() == wxID_OK)
        {
          if(dynamic_cast<ImgCell *>(output)->GetMaxWidth() != chooser->GetMaxWidth())
            m_fileSaved = false;
          if(dynamic_cast<ImgCell *>(output)->GetMaxHeight() != chooser->GetMaxHeight())
            m_fileSaved = false;

          dynamic_cast<ImgCell *>(output)->SetMaxWidth(chooser->GetMaxWidth());
          dynamic_cast<ImgCell *>(output)->SetMaxHeight(chooser->GetMaxHeight());
        }
      }
      m_worksheet->RecalculateForce();
      m_worksheet->RequestRedraw();
      break;
    case Worksheet::popid_unfold:
    {
      GroupCell *group = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
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
              (SelectionEnd->m_next != NULL)
              && (dynamic_cast<GroupCell *>(SelectionEnd->m_next)->IsLesserGCType(SelectionStart->GetGroupType()))
              )
              SelectionEnd = dynamic_cast<GroupCell *>(SelectionEnd->m_next);
            m_worksheet->SetActiveCell(NULL);
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
          group = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
      }
      else if (m_worksheet->HCaretActive())
      {
        if (m_worksheet->GetHCaret())
        {
          group = m_worksheet->GetHCaret();
/*        if(group->m_next)
          group = dynamic_cast<GroupCell*>(group->m_next);*/
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
    case Worksheet::popid_copy:
      if (m_worksheet->CanCopy(true))
        m_worksheet->Copy();
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
    case Worksheet::popid_cut:
      if (m_worksheet->CanCopy(true))
        m_worksheet->CutToClipboard();
      break;
    case Worksheet::popid_paste:
      m_worksheet->PasteFromClipboard();
      break;
    case Worksheet::popid_select_all:
    case ToolBar::tb_select_all:
      m_worksheet->SelectAll();
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
      wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                     wxT("image.png"), wxT("png"),
                                     _("PNG image (*.png)|*.png|"
                                               "JPEG image (*.jpg)|*.jpg|"
                                               "Windows bitmap (*.bmp)|*.bmp|"
                                               "Portable animap (*.pnm)|*.pnm|"
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
    case Worksheet::popid_animation_save:
    {
      wxString file = wxFileSelector(_("Save animation to file"), m_lastPath,
                                     wxT("animation.gif"), wxT("gif"),
                                     _("GIF image (*.gif)|*.gif"),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        Cell *selection = m_worksheet->GetSelectionStart();
        if (selection != NULL && selection->GetType() == MC_TYPE_SLIDE)
          dynamic_cast<SlideShow *>(selection)->ToGif(file);
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
              (file.EndsWith(wxT(".wxmx"))) ||
              (file.EndsWith(wxT(".wxm")))
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
    wxMessageBox(_("File you tried to open does not exist."), _("File not found"), wxOK);
  }
}

void wxMaxima::OnRecentPackage(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  wxString file = m_recentPackages.Get(event.GetId() - menu_recent_package_0);
  #ifdef __WXMSW__
  file.Replace(wxT("\\"),wxT("/"));
  #endif
  MenuCommand(wxT("load(\"") + file + wxT("\")$"));
}

void wxMaxima::OnUnsavedDocument(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();


  wxString file = m_unsavedDocuments.Get(event.GetId() - menu_unsaved_document_0);

  if(file == wxEmptyString)
    return;

  if (SaveNecessary() &&
      (
              (file.EndsWith(wxT(".wxmx"))) ||
              (file.EndsWith(wxT(".wxm")))
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
    OpenFile(file);
    m_fileSaved = false;
    m_isNamed   = false;
    m_tempfileName = file;
  }
  else
  {
    wxMessageBox(_("File you tried to open does not exist."), _("File not found"), wxOK);
  }
}

bool wxMaxima::SaveNecessary()
{
  // No need to save an empty document
  if(m_worksheet->GetTree() == NULL)
    return false;

  // No need to save a document only consisting of an prompt
  if(m_worksheet->GetTree()->Empty())
    return false;


  return ((!m_fileSaved) || (!m_isNamed));
}

void wxMaxima::EditInputMenu(wxCommandEvent &WXUNUSED(event))
{
  if(m_worksheet != NULL)
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
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  bool evaluating = !m_worksheet->m_evaluationQueue.Empty();
  if (!evaluating)
    m_worksheet->FollowEvaluation(true);
  EditorCell *tmp = m_worksheet->GetActiveCell();
  if (m_worksheet->QuestionPending())
    evaluating = true;

  if (tmp != NULL) // we have an active cell
  {
    if (tmp->GetType() == MC_TYPE_INPUT && (!m_worksheet->m_configuration->InLispMode()))
      tmp->AddEnding();
    // if active cell is part of a working group, we have a special
    // case - answering 1a question. Manually send answer to Maxima.
    GroupCell *cell = dynamic_cast<GroupCell *>(tmp->GetGroup());
    if (m_worksheet->GCContainsCurrentQuestion(cell))
    {
      wxString answer = tmp->ToString(true);
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
      m_worksheet->m_cellPointers.m_errorList.Remove(cell);
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
  if(text == wxEmptyString)
    return (wxEmptyString);
  if (text.EndsWith(wxT("\\")))
    return (_("Cell ends in a backslash"));

  MaximaTokenizer::TokenList tokens =
    MaximaTokenizer(text, m_worksheet->m_configuration).GetTokens();

  index = 0;
  bool endingNeeded = true;
  wxChar lastnonWhitespace;
  wxChar lastnonWhitespace_Next = wxT(' ');
  MaximaTokenizer::TokenList::iterator it;
  std::list<wxChar> delimiters;
  for (it = tokens.begin(); it != tokens.end(); ++it)
  {
    wxString itemText = (*it)->GetText();
    TextStyle itemStyle = (*it)->GetStyle();
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

    if((*it)->GetStyle() == TS_CODE_LISP)
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
  if (!m_isConnected)
    return;

  // Maxima is connected. Let's test if the evaluation queue is empty.
  GroupCell *tmp = dynamic_cast<GroupCell *>(m_worksheet->m_evaluationQueue.GetCell());
  if (tmp == NULL)
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
        m_worksheet->SetSelection(NULL, NULL);
    }
    if (m_worksheet->GetSelectionEnd())
    {
      if (m_worksheet->GetSelectionEnd()->GetGroup() == tmp)
        m_worksheet->SetSelection(NULL, NULL);
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
    if (parenthesisError == wxEmptyString)
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

      m_worksheet->m_cellPointers.SetWorkingGroup(tmp);
      tmp->GetPrompt()->SetValue(m_lastPrompt);

      SendMaxima(m_configCommands + text, true);
      m_maximaBusy = true;
      // Now that we have sent a command we need to query all variable values anew
      m_varNamesToQuery = m_worksheet->m_variablesPane->GetEscapedVarnames();
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
      m_worksheet->m_cellPointers.m_errorList.Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_commandIndex - 1);
      // Inform the user about the error (which automatically causes the worksheet
      // to the cell we marked as erroneous a few seconds ago.
      TextCell *cell = new TextCell(m_worksheet->GetTree(), &(m_worksheet->m_configuration),
                                    &m_worksheet->m_cellPointers,
                                    _("Refusing to send cell to maxima: ") +
                                    parenthesisError + wxT("\n"));
      cell->SetType(MC_TYPE_ERROR);
      cell->SetGroup(tmp);
      tmp->InEvaluationQueue(false);
      tmp->LastInEvaluationQueue(false);
      tmp->SetOutput(cell);
      tmp->ResetSize();
      tmp->Recalculate();
      m_worksheet->m_evaluationQueue.Clear();
      m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
      m_worksheet->Recalculate(cell->GetGroup());
      tmp->GetInput()->SetCaretPosition(index);
      tmp->GetInput()->SetErrorIndex((m_commandIndex = index) - 1);

      if (m_worksheet->FollowEvaluation())
        m_worksheet->SetSelection(NULL);

      m_worksheet->m_cellPointers.SetWorkingGroup(NULL);
      m_worksheet->RequestRedraw();
      if(!AbortOnError())
      {
        m_outputCellsFromCurrentCommand = 0;
        TriggerEvaluation();
      }
      if((tmp)&&(tmp->GetEditable()))
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

void wxMaxima::InsertMenu(wxCommandEvent &event)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  GroupType type = GC_TYPE_CODE;
  bool output = false;
  switch (event.GetId())
  {
    case Worksheet::popid_auto_answer:
      if((m_worksheet->GetActiveCell() != NULL) &&
         (dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->GetGroupType() == GC_TYPE_CODE))
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->AutoAnswer(event.IsChecked());
      else if((m_worksheet->GetSelectionStart() != NULL)&&
              (m_worksheet->GetSelectionStart()->GetType() == MC_TYPE_GROUP))
      {
        GroupCell *gc = dynamic_cast<GroupCell *>(m_worksheet->GetSelectionStart());
        while(gc != NULL)
        {
          if(gc->GetGroupType() == GC_TYPE_CODE)
            gc->AutoAnswer(event.IsChecked());

          if(gc == m_worksheet->GetSelectionEnd())
            break;
          gc = dynamic_cast<GroupCell *>(gc->m_next);
        }
      }
      m_fileSaved = false;
      m_worksheet->RequestRedraw();
      return;
      break;
    case Worksheet::popid_add_watch:
      if(m_worksheet->GetActiveCell())
      {
        wxString selectionString = m_worksheet->GetActiveCell()->GetSelectionString();
        if(selectionString.IsEmpty())
          selectionString = m_worksheet->GetActiveCell()->GetWordUnderCaret();
        m_worksheet->m_variablesPane->AddWatchCode(selectionString);
        wxMaximaFrame::ShowPane(menu_pane_variables,true);
      }
      return;
      break;
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
      break;
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
      break;
    case menu_autocomplete_templates:
      m_worksheet->Autocomplete(AutoComplete::tmplte);
      return;
      break;
    case menu_convert_to_code:
      if (m_worksheet->GetActiveCell())
      {
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_CODE);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_comment:
      if (m_worksheet->GetActiveCell())
      {
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_TEXT);
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
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_TITLE);
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
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_SECTION);
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
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_SUBSECTION);
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
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_SUBSUBSECTION);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_heading5:
      if (m_worksheet->GetActiveCell())
      {
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_HEADING5);
        m_worksheet->Recalculate(true);
        m_worksheet->RequestRedraw();
      }
      break;
    case menu_convert_to_heading6:
      if (m_worksheet->GetActiveCell())
      {
        dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup())->SetGroupType(GC_TYPE_HEADING6);
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
              new GroupCell(&(m_worksheet->m_configuration), GC_TYPE_PAGEBREAK,
                            &m_worksheet->m_cellPointers),
              m_worksheet->GetHCaret());
      m_worksheet->Recalculate();
      m_worksheet->SetFocus();
      return;
      break;
    case menu_insert_image:
    case menu_format_image:
    {
      wxString file = wxFileSelector(_("Insert Image"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Image files (*.png, *.jpg, *.bmp, *.xpm)|*.png;*.jpg;*.bmp;*.xpm"),
                                     wxFD_OPEN);
      if (file != wxEmptyString)
      {
        m_worksheet->OpenHCaret(file, GC_TYPE_IMAGE);
      }
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
  if(!m_isNamed)
  {
    SetRepresentedFilename(wxEmptyString);
    OSXSetModified(true);
    saved = false;
  }
  else
  {
    SetRepresentedFilename(m_worksheet->m_currentFile);
    OSXSetModified((saved != m_fileSaved) || (force));
  }

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

void wxMaxima::UpdateSlider(wxUpdateUIEvent &WXUNUSED(ev))
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

void wxMaxima::SliderEvent(wxScrollEvent &ev)
{
  SlideShow *slideShow = dynamic_cast<SlideShow *>(m_worksheet->GetSelectionStart());

  if (slideShow != NULL)
  {
    slideShow->AnimationRunning(false);
    slideShow->SetDisplayedIndex(ev.GetPosition());

    wxRect rect = slideShow->GetRect();
    m_worksheet->RequestRedraw(rect);
    if(m_worksheet->m_mainToolBar)
      m_worksheet->m_mainToolBar->UpdateSlider(slideShow);
  }
}

void wxMaxima::ShowPane(wxCommandEvent &ev)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  int id = ev.GetId();

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

void wxMaxima::NetworkDClick(wxCommandEvent &WXUNUSED(ev))
{
  m_manager.GetPane(wxT("XmlInspector")).Show(
          !m_manager.GetPane(wxT("XmlInspector")).IsShown()
  );
  m_manager.Update();
}

void wxMaxima::HistoryDClick(wxCommandEvent &ev)
{
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  m_worksheet->OpenHCaret(ev.GetString(), GC_TYPE_CODE);
  m_worksheet->SetFocus();
}

void wxMaxima::TableOfContentsSelection(wxListEvent &ev)
{
  GroupCell *selection = dynamic_cast<GroupCell *>(m_worksheet->m_tableOfContents->GetCell(ev.GetIndex())->GetGroup());

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
  if(m_worksheet != NULL)
    m_worksheet->CloseAutoCompletePopup();

  m_worksheet->OnFollow();
}

long *VersionToInt(wxString version)
{
  long *intV = new long[3];

  wxStringTokenizer tokens(version, wxT("."));

  for (int i = 0; i < 3 && tokens.HasMoreTokens(); i++)
    tokens.GetNextToken().ToLong(&intV[i]);

  return intV;
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
    wxMessageBox(_("Can not connect to the web server."), _("Error"),
                 wxOK | wxICON_ERROR);
    return;
  }

  wxInputStream *inputStream = connection.GetInputStream(_T("/wxmaxima/version.txt"));

  if (connection.GetError() == wxPROTO_NOERR)
  {
    wxString version;
    wxStringOutputStream outputStream(&version);
    inputStream->Read(outputStream);

    if (version.StartsWith(wxT("wxmaxima = ")))
    {
      version = version.Mid(11, version.Length()).Trim();
      long *myVersion = VersionToInt(wxT(GITVERSION));
      long *currVersion = VersionToInt(version);

      bool upgrade = myVersion[0] < currVersion[0] ||
                     (myVersion[0] == currVersion[0] && myVersion[1] < currVersion[1]) ||
                     (myVersion[0] == currVersion[0] &&
                      myVersion[1] == currVersion[1] &&
                      myVersion[2] < currVersion[2]);

      if (upgrade)
      {
        bool visit = wxMessageBox(wxString::Format(
                                          _("You have version %s. Current version is %s.\n\n"
                                                    "Select OK to visit the wxMaxima webpage."),
                                          wxT(GITVERSION), version.c_str()),
                                  _("Upgrade"),
                                  wxOK | wxCANCEL | wxICON_INFORMATION) == wxOK;

        if (visit)
          wxLaunchDefaultBrowser(wxT("https://wxMaxima-developers.github.io/wxmaxima"));
      }
      else if (reportUpToDate)
        wxMessageBox(_("Your version of wxMaxima is up to date."), _("Upgrade"),
                     wxOK | wxICON_INFORMATION);

      delete[] myVersion;
      delete[] currVersion;
    }
    else
    {
      wxMessageBox(
              _("Unable to interpret the version info I got from http://wxMaxima-developers.github.io//wxmaxima/version.txt: ") +
              version, _("Upgrade"),
              wxOK | wxICON_INFORMATION);

    }
  }
  else
  {
    wxMessageBox(_("Can not download version info."), _("Error"),
                 wxOK | wxICON_ERROR);
  }

  wxDELETE(inputStream);
  inputStream = NULL;
  connection.Close();
}

int wxMaxima::SaveDocumentP()
{
  wxString file, ext;
  if ((m_worksheet->m_currentFile == wxEmptyString) || (!m_isNamed))
  {
    // Check if we want to save modified untitled documents on exit
    bool save = true;
    wxConfig::Get()->Read(wxT("saveUntitled"), &save);
    if (!save)
      return wxID_NO;

#if defined __WXOSX__
    file = GetTitle();
#else
    file = _("unsaved");
#endif
  }
  else
  {
    if (m_worksheet->m_configuration->AutoSaveMiliseconds() > 0)
      if (SaveFile())
        return wxID_NO;

    wxString ext;
    wxFileName::SplitPath(m_worksheet->m_currentFile, NULL, NULL, &file, &ext);
    file += wxT(".") + ext;
  }

  wxMessageDialog dialog(this,
                         wxString::Format(_("Do you want to save the changes you made in the document \"%s\"?"),
                                          file),
                         "wxMaxima", wxCENTER | wxYES_NO | wxCANCEL);

  dialog.SetExtendedMessage(_("Your changes will be lost if you don't save them."));
  dialog.SetYesNoCancelLabels(_("Save"), _("Don't save"), _("Cancel"));

  return dialog.ShowModal();
}

void wxMaxima::OnActivate(wxActivateEvent &event)
{
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
    GroupCell *group = dynamic_cast<GroupCell *>(m_worksheet->GetActiveCell()->GetGroup());
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
    {}
    }
    m_worksheet->NumberSections();
    m_worksheet->SetFocus();
  }
  else
    m_worksheet->m_mainToolBar->SetDefaultCellStyle();
}

BEGIN_EVENT_TABLE(wxMaxima, wxFrame)

                EVT_MENU(mac_closeId, wxMaxima::FileMenu)
                EVT_MENU(menu_check_updates, wxMaxima::HelpMenu)
                EVT_TIMER(KEYBOARD_INACTIVITY_TIMER_ID, wxMaxima::OnTimerEvent)
                EVT_TIMER(MAXIMA_STDOUT_POLL_ID, wxMaxima::OnTimerEvent)
                EVT_TIMER(AUTO_SAVE_TIMER_ID, wxMaxima::OnTimerEvent)
                EVT_TIMER(wxID_ANY, wxMaxima::OnTimerEvent)
                EVT_COMMAND_SCROLL(ToolBar::plot_slider_id, wxMaxima::SliderEvent)
                EVT_MENU(Worksheet::popid_copy, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_image, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_animation, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_svg, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_emf, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_rtf, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_insert_text, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_title, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_section, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_subsection, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_subsubsection, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_heading5, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_heading6, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_popup_gnuplot, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::popid_delete, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::popid_simplify, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_factor, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_expand, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_solve, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_solve_num, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_subst, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_plot2d, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_plot3d, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_diff, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_integrate, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_float, wxMaxima::PopupMenu)
				EVT_MENU(Worksheet::popid_copy_matlab, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_tex, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_copy_text, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_image, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_animation_save, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_animation_start, wxMaxima::FileMenu)
                EVT_BUTTON(button_integrate, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_diff, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_solve, wxMaxima::EquationsMenu)
                EVT_BUTTON(button_solve_ode, wxMaxima::EquationsMenu)
                EVT_BUTTON(button_sum, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_expand, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_factor, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_taylor, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_limit, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_ratsimp, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_trigexpand, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_trigreduce, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_trigsimp, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_product, wxMaxima::CalculusMenu)
                EVT_BUTTON(button_radcan, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_subst, wxMaxima::MaximaMenu)
                EVT_BUTTON(button_plot2, wxMaxima::PlotMenu)
                EVT_BUTTON(button_plot3, wxMaxima::PlotMenu)
                EVT_BUTTON(button_map, wxMaxima::AlgebraMenu)
                EVT_BUTTON(button_rectform, wxMaxima::SimplifyMenu)
                EVT_BUTTON(button_trigrat, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_polarform, wxMaxima::SimplifyMenu)
                EVT_MENU(ToolBar::menu_restart_id, wxMaxima::MaximaMenu)
#ifndef __WXOSX__
                EVT_MENU(wxID_EXIT, wxMaxima::FileMenu)
#endif
                EVT_MENU(wxID_ABOUT, wxMaxima::HelpMenu)
                EVT_MENU(menu_license, wxMaxima::HelpMenu)
                EVT_MENU(menu_save_id, wxMaxima::FileMenu)
                EVT_MENU(menu_save_as_id, wxMaxima::FileMenu)
                EVT_MENU(menu_load_id, wxMaxima::FileMenu)
                EVT_MENU(menu_functions, wxMaxima::MaximaMenu)
                EVT_MENU(menu_variables, wxMaxima::MaximaMenu)
                EVT_MENU(wxID_PREFERENCES, wxMaxima::EditMenu)
                EVT_MENU(menu_sconsole_id, wxMaxima::FileMenu)
                EVT_MENU(menu_export_html, wxMaxima::FileMenu)
                EVT_MENU(wxID_HELP, wxMaxima::HelpMenu)
                EVT_MENU(menu_help_tutorials, wxMaxima::HelpMenu)
                EVT_MENU(menu_bug_report, wxMaxima::HelpMenu)
                EVT_MENU(menu_build_info, wxMaxima::HelpMenu)
                EVT_MENU(menu_interrupt_id, wxMaxima::Interrupt)
                EVT_MENU(menu_open_id, wxMaxima::FileMenu)
                EVT_MENU(menu_batch_id, wxMaxima::FileMenu)
                EVT_MENU(menu_ratsimp, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_radsimp, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_expand, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_factor, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_gfactor, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_trigsimp, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_trigexpand, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_trigreduce, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_rectform, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_demoivre, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_num_out, wxMaxima::NumericalMenu)
                EVT_MENU(menu_to_float, wxMaxima::NumericalMenu)
                EVT_MENU(menu_to_bfloat, wxMaxima::NumericalMenu)
                EVT_MENU(menu_to_numer, wxMaxima::NumericalMenu)
                EVT_MENU(menu_exponentialize, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_invert_mat, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_determinant, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_eigen, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_eigvect, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_adjoint_mat, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_transpose, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_set_precision, wxMaxima::NumericalMenu)
                EVT_MENU(menu_set_displayprecision, wxMaxima::NumericalMenu)
                EVT_MENU(menu_engineeringFormat, wxMaxima::NumericalMenu)
                EVT_MENU(menu_engineeringFormatSetup, wxMaxima::NumericalMenu)
                EVT_MENU(menu_talg, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_tellrat, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_modulus, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_allroots, wxMaxima::EquationsMenu)
                EVT_MENU(menu_bfallroots, wxMaxima::EquationsMenu)
                EVT_MENU(menu_realroots, wxMaxima::EquationsMenu)
                EVT_MENU(menu_solve, wxMaxima::EquationsMenu)
                EVT_MENU(menu_solve_to_poly, wxMaxima::EquationsMenu)
                EVT_MENU(menu_solve_num, wxMaxima::EquationsMenu)
                EVT_MENU(menu_solve_ode, wxMaxima::EquationsMenu)
                EVT_MENU(menu_map_mat, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_enter_mat, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_cpoly, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_solve_lin, wxMaxima::EquationsMenu)
                EVT_MENU(menu_solve_algsys, wxMaxima::EquationsMenu)
                EVT_MENU(menu_eliminate, wxMaxima::EquationsMenu)
                EVT_MENU(menu_clear_var, wxMaxima::MaximaMenu)
                EVT_MENU(menu_clear_fun, wxMaxima::MaximaMenu)
                EVT_MENU(menu_ivp_1, wxMaxima::EquationsMenu)
                EVT_MENU(menu_ivp_2, wxMaxima::EquationsMenu)
                EVT_MENU(menu_bvp, wxMaxima::EquationsMenu)
                EVT_MENU(menu_bvp, wxMaxima::EquationsMenu)
                EVT_MENU(menu_fun_def, wxMaxima::MaximaMenu)
                EVT_MENU(menu_divide, wxMaxima::CalculusMenu)
                EVT_MENU(menu_gcd, wxMaxima::CalculusMenu)
                EVT_MENU(menu_lcm, wxMaxima::CalculusMenu)
                EVT_MENU(menu_continued_fraction, wxMaxima::CalculusMenu)
                EVT_MENU(menu_partfrac, wxMaxima::CalculusMenu)
                EVT_MENU(menu_risch, wxMaxima::CalculusMenu)
                EVT_MENU(menu_integrate, wxMaxima::CalculusMenu)
                EVT_MENU(menu_laplace, wxMaxima::CalculusMenu)
                EVT_MENU(menu_ilt, wxMaxima::CalculusMenu)
                EVT_MENU(menu_diff, wxMaxima::CalculusMenu)
                EVT_MENU(menu_series, wxMaxima::CalculusMenu)
                EVT_MENU(menu_limit, wxMaxima::CalculusMenu)
                EVT_MENU(menu_lbfgs, wxMaxima::CalculusMenu)
                EVT_MENU(menu_gen_mat, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_gen_mat_lambda, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_map, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_sum, wxMaxima::CalculusMenu)
                EVT_MENU(menu_maximahelp, wxMaxima::HelpMenu)
                EVT_MENU(menu_example, wxMaxima::HelpMenu)
                EVT_MENU(menu_apropos, wxMaxima::HelpMenu)
                EVT_MENU(menu_show_tip, wxMaxima::HelpMenu)
                EVT_MENU(menu_trigrat, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_solve_de, wxMaxima::EquationsMenu)
                EVT_MENU(menu_atvalue, wxMaxima::EquationsMenu)
                EVT_MENU(menu_lhs, wxMaxima::EquationsMenu)
                EVT_MENU(menu_rhs, wxMaxima::EquationsMenu)
                EVT_MENU(menu_sum, wxMaxima::CalculusMenu)
                EVT_MENU(menu_product, wxMaxima::CalculusMenu)
                EVT_MENU(menu_change_var, wxMaxima::CalculusMenu)
                EVT_MENU(menu_make_list, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_apply, wxMaxima::AlgebraMenu)
                EVT_MENU(menu_time, wxMaxima::MaximaMenu)
                EVT_MENU(menu_factsimp, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_factcomb, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_realpart, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_imagpart, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_nouns, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_logcontract, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_logexpand, wxMaxima::SimplifyMenu)
                EVT_MENU(gp_plot2, wxMaxima::PlotMenu)
                EVT_MENU(gp_plot3, wxMaxima::PlotMenu)
                EVT_MENU(menu_animationautostart, wxMaxima::PlotMenu)
                EVT_MENU(menu_animationframerate, wxMaxima::PlotMenu)
                EVT_MENU(menu_plot_format, wxMaxima::PlotMenu)
                EVT_MENU(menu_soft_restart, wxMaxima::MaximaMenu)
                EVT_MENU(menu_jumptoerror, wxMaxima::MaximaMenu)
                EVT_MENU(menu_display, wxMaxima::MaximaMenu)
                EVT_MENU(menu_pade, wxMaxima::CalculusMenu)
                EVT_MENU(menu_add_path, wxMaxima::MaximaMenu)
                EVT_MENU(menu_copy_from_worksheet, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_text_from_worksheet, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_tex_from_worksheet, wxMaxima::EditMenu)
				EVT_MENU(menu_copy_matlab_from_worksheet, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::popid_copy_mathml, wxMaxima::EditMenu)
                EVT_MENU(menu_undo, wxMaxima::EditMenu)
                EVT_MENU(menu_redo, wxMaxima::EditMenu)
                EVT_MENU(menu_texform, wxMaxima::MaximaMenu)
                EVT_MENU(menu_to_fact, wxMaxima::SimplifyMenu)
                EVT_MENU(menu_to_gamma, wxMaxima::SimplifyMenu)
                EVT_MENU(wxID_PRINT, wxMaxima::PrintMenu)
                EVT_TOOL(ToolBar::tb_print, wxMaxima::PrintMenu)
                EVT_MENU(Worksheet::menu_zoom_in, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::menu_zoom_out, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_80, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_100, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_120, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_150, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_200, wxMaxima::EditMenu)
                EVT_MENU(menu_zoom_300, wxMaxima::EditMenu)
                EVT_MENU(menu_math_as_1D_ASCII, wxMaxima::EditMenu)
                EVT_MENU(menu_math_as_2D_ASCII, wxMaxima::EditMenu)
                EVT_MENU(menu_math_as_graphics, wxMaxima::EditMenu)
                EVT_MENU(menu_noAutosubscript, wxMaxima::EditMenu)
                EVT_MENU(menu_defaultAutosubscript, wxMaxima::EditMenu)
                EVT_MENU(menu_alwaysAutosubscript, wxMaxima::EditMenu)
                EVT_MENU(menu_roundedMatrixParensNo, wxMaxima::EditMenu)
                EVT_MENU(menu_roundedMatrixParensYes, wxMaxima::EditMenu)
                EVT_MENU(menu_fullscreen, wxMaxima::EditMenu)
                EVT_MENU(ToolBar::tb_hideCode, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_as_bitmap, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_as_svg, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_as_emf, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_as_rtf, wxMaxima::EditMenu)
                EVT_MENU(menu_copy_to_file, wxMaxima::EditMenu)
                EVT_MENU(menu_select_all, wxMaxima::EditMenu)
                EVT_MENU(menu_subst, wxMaxima::MaximaMenu)
                EVT_TOOL(ToolBar::tb_open, wxMaxima::FileMenu)
                EVT_TOOL(ToolBar::tb_save, wxMaxima::FileMenu)
                EVT_TOOL(ToolBar::tb_copy, wxMaxima::EditMenu)
                EVT_TOOL(ToolBar::tb_paste, wxMaxima::EditMenu)
                EVT_TOOL(ToolBar::tb_select_all, wxMaxima::PopupMenu)
                EVT_TOOL(ToolBar::tb_cut, wxMaxima::EditMenu)
                EVT_TOOL(ToolBar::tb_pref, wxMaxima::EditMenu)
                EVT_TOOL(ToolBar::tb_interrupt, wxMaxima::Interrupt)
                EVT_TOOL(ToolBar::tb_help, wxMaxima::HelpMenu)
                EVT_TOOL(ToolBar::tb_animation_startStop, wxMaxima::FileMenu)
                EVT_TOOL(ToolBar::tb_animation_start, wxMaxima::FileMenu)
                EVT_TOOL(ToolBar::tb_animation_stop, wxMaxima::FileMenu)
                EVT_TOOL(ToolBar::tb_find, wxMaxima::EditMenu)
                EVT_TOOL(ToolBar::tb_follow, wxMaxima::OnFollow)
                EVT_SOCKET(socket_server_id, wxMaxima::ServerEvent)
                EVT_SOCKET(socket_client_id, wxMaxima::ClientEvent)
/* These commands somehow caused the menu to be updated six times on every
   keypress and the tool bar to be updated six times on every menu update

   => Moved the update events to the idle loop.

EVT_UPDATE_UI(menu_interrupt_id, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(ToolBar::plot_slider_id, wxMaxima::UpdateSlider)
EVT_UPDATE_UI(menu_copy_from_worksheet, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_text_from_worksheet, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_tex_from_worksheet, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_mathml_from_worksheet, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(Worksheet::menu_zoom_in, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(Worksheet::menu_zoom_out, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(wxID_PRINT, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_as_bitmap, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_as_svg, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_copy_to_file, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_evaluate, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_evaluate_all, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(ToolBar::tb_evaltillhere, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_select_all, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_undo, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_hideall, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_math, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_stats, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_history, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_structure, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_pane_format, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_remove_output, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(ToolBar::tb_print, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_follow, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_copy, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_cut, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_interrupt, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_save, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_animation_startStop, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_animation_start, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(ToolBar::tb_animation_stop, wxMaxima::UpdateToolBar)
EVT_UPDATE_UI(menu_save_id, wxMaxima::UpdateMenus)
EVT_UPDATE_UI(menu_show_toolbar, wxMaxima::UpdateMenus)
*/
                EVT_CLOSE(wxMaxima::OnClose)
                EVT_QUERY_END_SESSION(wxMaxima::OnClose)
                EVT_END_SESSION(wxMaxima::OnClose)
                EVT_END_PROCESS(maxima_process_id, wxMaxima::OnProcessEvent)
                EVT_END_PROCESS(gnuplot_process_id, wxMaxima::OnGnuplotClose)
                EVT_MENU(Worksheet::popid_edit, wxMaxima::EditInputMenu)
                EVT_MENU(menu_evaluate, wxMaxima::EvaluateEvent)
                EVT_MENU(Variablespane::varID_newVar, wxMaxima::VarReadEvent)
                EVT_MENU(Variablespane::varID_add_all, wxMaxima::VarAddAllEvent)
                EVT_MENU(menu_add_comment, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_section, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_subsection, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_subsubsection, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_heading5, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_heading6, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_title, wxMaxima::InsertMenu)
                EVT_MENU(menu_add_pagebreak, wxMaxima::InsertMenu)
                EVT_MENU(menu_fold_all_cells, wxMaxima::InsertMenu)
                EVT_MENU(menu_unfold_all_cells, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_add_comment, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_add_watch, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_add_watch_label, wxMaxima::InsertMenu)
                EVT_MENU(menu_insert_previous_input, wxMaxima::InsertMenu)
                EVT_MENU(menu_insert_previous_output, wxMaxima::InsertMenu)
                EVT_MENU(menu_autocomplete, wxMaxima::InsertMenu)
                EVT_MENU(menu_autocomplete_templates, wxMaxima::InsertMenu)
                EVT_MENU(menu_insert_input, wxMaxima::InsertMenu)
                EVT_MENU(Worksheet::popid_insert_input, wxMaxima::InsertMenu)
                EVT_MENU(menu_history_previous, wxMaxima::EditMenu)
                EVT_MENU(menu_history_next, wxMaxima::EditMenu)
                EVT_MENU(menu_cut, wxMaxima::EditMenu)
                EVT_MENU(menu_paste, wxMaxima::EditMenu)
                EVT_MENU(menu_paste_input, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::popid_cut, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_paste, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_select_all, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_comment_selection, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_divide_cell, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_evaluate, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_evaluate_section, wxMaxima::PopupMenu)
                EVT_MENU(ToolBar::tb_evaluate_rest, wxMaxima::PopupMenu)
                EVT_MENU(ToolBar::tb_evaltillhere, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_merge_cells, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_maxsizechooser, wxMaxima::PopupMenu)
                EVT_MENU(TableOfContents::popid_Fold, wxMaxima::PopupMenu)
                EVT_MENU(TableOfContents::popid_Unfold, wxMaxima::PopupMenu)
                EVT_MENU(TableOfContents::popid_SelectTocChapter, wxMaxima::PopupMenu)
                EVT_MENU(TableOfContents::popid_EvalTocChapter, wxMaxima::PopupMenu)
                EVT_MENU(TableOfContents::popid_ToggleTOCshowsSectionNumbers, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_fold, wxMaxima::PopupMenu)
                EVT_MENU(Worksheet::popid_unfold, wxMaxima::PopupMenu)
                EVT_MENU(menu_evaluate_all_visible, wxMaxima::MaximaMenu)
                EVT_MENU(menu_evaluate_all, wxMaxima::MaximaMenu)
                EVT_MENU(ToolBar::tb_evaltillhere, wxMaxima::MaximaMenu)
                EVT_MENU(menu_list_create_from_elements,wxMaxima::ListMenu)
                EVT_MENU(menu_list_create_from_rule,wxMaxima::ListMenu)
                EVT_MENU(menu_list_create_from_list,wxMaxima::ListMenu)
                EVT_MENU(menu_list_actual_values_storage,wxMaxima::ListMenu)
                EVT_MENU(menu_list_sort,wxMaxima::ListMenu)
                EVT_MENU(menu_list_length,wxMaxima::ListMenu)
                EVT_MENU(menu_list_push,wxMaxima::ListMenu)
                EVT_MENU(menu_list_pop,wxMaxima::ListMenu)
                EVT_MENU(menu_list_reverse,wxMaxima::ListMenu)
                EVT_MENU(menu_list_first,wxMaxima::ListMenu)
                EVT_MENU(menu_list_last,wxMaxima::ListMenu)
                EVT_MENU(menu_list_rest,wxMaxima::ListMenu)
                EVT_MENU(menu_list_restN,wxMaxima::ListMenu)
                EVT_MENU(menu_list_lastn,wxMaxima::ListMenu)
                EVT_MENU(menu_list_nth,wxMaxima::ListMenu)
                EVT_MENU(menu_list_map,wxMaxima::ListMenu)
                EVT_MENU(menu_list_use_actual_values,wxMaxima::ListMenu)
                EVT_MENU(menu_list_as_function_arguments,wxMaxima::ListMenu)
                EVT_MENU(menu_list_extract_value,wxMaxima::ListMenu)
                EVT_MENU(menu_list_do_for_each_element,wxMaxima::ListMenu)
                EVT_MENU(menu_list_remove_duplicates,wxMaxima::ListMenu)
                EVT_MENU(menu_list_remove_element,wxMaxima::ListMenu)
                EVT_MENU(menu_list_append_item,wxMaxima::ListMenu)
                EVT_MENU(menu_list_append_list,wxMaxima::ListMenu)
                EVT_MENU(menu_list_interleave,wxMaxima::ListMenu)
                EVT_MENU(menu_list_list2matrix,wxMaxima::ListMenu)
                EVT_MENU(menu_list_matrix2list,wxMaxima::ListMenu)
                EVT_MENU(menu_list_create_from_args,wxMaxima::ListMenu)
                EVT_MENU(menu_draw_2d,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_2d,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_3d,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_3d,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_fgcolor,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_fgcolor,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_fillcolor,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_fillcolor,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_title,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_title,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_key,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_key,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_explicit,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_explicit,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_implicit,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_implicit,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_parametric,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_parametric,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_points,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_points,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_axis,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_axis,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_contour,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_contour,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_accuracy,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_accuracy,wxMaxima::DrawMenu)
                EVT_MENU(menu_draw_grid,wxMaxima::DrawMenu)
                EVT_BUTTON(menu_draw_grid,wxMaxima::DrawMenu)
                EVT_IDLE(wxMaxima::OnIdle)
                EVT_MENU(menu_remove_output, wxMaxima::EditMenu)
                EVT_MENU_RANGE(menu_recent_document_0, menu_recent_document_29, wxMaxima::OnRecentDocument)
                EVT_MENU_RANGE(menu_recent_package_0, menu_recent_package_29, wxMaxima::OnRecentPackage)
                EVT_MENU_RANGE(menu_unsaved_document_0, menu_unsaved_document_29, wxMaxima::OnUnsavedDocument)
                EVT_MENU(menu_insert_image, wxMaxima::InsertMenu)
                EVT_MENU_RANGE(menu_pane_hideall, menu_pane_stats, wxMaxima::ShowPane)
                EVT_MENU(menu_show_toolbar, wxMaxima::EditMenu)
                EVT_MENU(Worksheet::popid_auto_answer, wxMaxima::InsertMenu)
                EVT_LISTBOX_DCLICK(history_ctrl_id, wxMaxima::HistoryDClick)
                EVT_LIST_ITEM_ACTIVATED(structure_ctrl_id, wxMaxima::TableOfContentsSelection)
                EVT_BUTTON(menu_stats_histogram, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_piechart, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_scatterplot, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_barsplot, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_boxplot, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_mean, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_median, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_var, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_dev, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_tt1, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_tt2, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_tnorm, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_linreg, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_lsquares, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_readm, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_stats_enterm, wxMaxima::AlgebraMenu)
                EVT_BUTTON(menu_stats_subsample, wxMaxima::StatsMenu)
                EVT_BUTTON(menu_format_title, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_text, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_heading6, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_heading5, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_subsubsection, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_subsection, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_section, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_pagebreak, wxMaxima::InsertMenu)
                EVT_BUTTON(menu_format_image, wxMaxima::InsertMenu)
                EVT_CHAR(wxMaxima::OnChar)
                EVT_KEY_DOWN(wxMaxima::OnKeyDown)
                EVT_CHOICE(ToolBar::tb_changeStyle, wxMaxima::ChangeCellStyle)
                EVT_MENU(menu_edit_find, wxMaxima::EditMenu)
                EVT_FIND(wxID_ANY, wxMaxima::OnFind)
                EVT_FIND_NEXT(wxID_ANY, wxMaxima::OnFind)
                EVT_FIND_REPLACE(wxID_ANY, wxMaxima::OnReplace)
                EVT_FIND_REPLACE_ALL(wxID_ANY, wxMaxima::OnReplaceAll)
                EVT_FIND_CLOSE(wxID_ANY, wxMaxima::OnFindClose)
                EVT_ACTIVATE(wxMaxima::OnActivate)
                EVT_ICONIZE(wxMaxima::OnMinimize)
END_EVENT_TABLE()


/* Local Variables:       */
/* mode: text             */
/* c-file-style:  "linux" */
/* c-basic-offset: 2      */
/* indent-tabs-mode: nil  */
