// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Implements the evaluation-queue driver extracted from the wxMaxima god class.
*/

#include "MaximaEvaluator.h"
#include "wxMaxima.h"
#include "Maxima.h"
#include "cells/TextCell.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "wxMathml.h"
#include "MaximaProtocol.h"
#include "Version.h"
#include <wx/tokenzr.h>
#include <memory>

void MaximaEvaluator::FirstOutput() {
  m_wxMaxima.m_lastPrompt = wxS("(%i1) ");
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
}

void MaximaEvaluator::SendMaxima(wxString s, bool addToHistory, bool background) {
  // Normally we catch parenthesis errors before adding cells to the
  // evaluation queue. But if the error is introduced only after the
  // cell is placed in the evaluation queue we need to catch it here.
  std::size_t index;
  wxString parenthesisError = m_wxMaxima.GetUnmatchedParenthesisState(s, index);
  if (parenthesisError.IsEmpty()) {
    if(m_wxMaxima.GetWorksheet())
      s = m_wxMaxima.GetWorksheet()->UnicodeToMaxima(s);

    if ((m_wxMaxima.m_xmlInspector) && (m_wxMaxima.IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_wxMaxima.m_xmlInspector->Add_ToMaxima(s);

    m_wxMaxima.m_dispReadOut = false;

    if (addToHistory)
      m_wxMaxima.AddToHistory(s);

    if (s.StartsWith(wxS(":lisp ")) || s.StartsWith(wxS(":lisp\n")))
      s.Replace(wxS("\n"), wxS(" "));

    s.Trim(true);
    s.Append(wxS("\n"));
    /// Check for function/variable definitions
    wxStringTokenizer commands(s, wxS(";$"));
    while (commands.HasMoreTokens()) {
      wxString line = commands.GetNextToken();
      if(m_wxMaxima.GetWorksheet())
        {
          if (m_wxMaxima.m_varRegEx.Matches(line))
            m_wxMaxima.GetWorksheet()->AddSymbol(m_wxMaxima.m_varRegEx.GetMatch(line, 1));

          if (m_wxMaxima.m_funRegEx.Matches(line)) {
            wxString funName = m_wxMaxima.m_funRegEx.GetMatch(line, 1);
            m_wxMaxima.GetWorksheet()->AddSymbol(funName);
        /// Create a template from the input
        wxString args = m_wxMaxima.m_funRegEx.GetMatch(line, 2);
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
          m_wxMaxima.GetWorksheet()->AddSymbol(funName, AutoComplete::tmplte);
}
      }
    }

    if ((m_wxMaxima.m_client) && (m_wxMaxima.m_client->IsConnected()) && (s.Length() >= 1)) {
      // If there is no working group and we still are trying to send something
      // we are trying to change maxima's settings from the background and might
      // never get an answer that changes the status again.
      if (m_wxMaxima.GetWorksheet() && (m_wxMaxima.GetWorksheet()->GetWorkingGroup())) {
        if (!background)
          m_wxMaxima.m_maximaError = false;
        m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::calculating);
      } else {
        if (m_wxMaxima.m_maximaError && background)
          m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::maximaerror);
        else
          m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
      }

      wxScopedCharBuffer const data_raw = s.utf8_str();
      m_wxMaxima.m_client->Write(data_raw.data(), data_raw.length());
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::transmit);
    }
  } else {
    m_wxMaxima.m_outputAppender.DoRawConsoleAppend(_("Refusing to send cell to maxima: ") +
                       parenthesisError + wxS("\n"),
                       MC_TYPE_ERROR);
    if(m_wxMaxima.GetWorksheet())
      {
        m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
        m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
      }
  }
  if (!m_wxMaxima.m_maximaStdoutPollTimer.IsRunning())
    m_wxMaxima.m_statusBar->SetMaximaCPUPercentage(-1);
  m_wxMaxima.m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);
}

bool MaximaEvaluator::QueryVariableValue() {
  if (m_wxMaxima.GetWorksheet() && (!m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Empty()))
    return false;

  if (m_wxMaxima.m_maximaBusy)
    return false;

  if (m_wxMaxima.m_configuration.InLispMode())
    return false;

  if (m_wxMaxima.GetWorksheet() && (m_wxMaxima.GetWorksheet()->QuestionPending()))
    return false;

  if (m_wxMaxima.m_varNamesToQuery.size() > 0) {
    SendMaxima(wxS(":lisp-quiet (wx-query-variable \"") +
               m_wxMaxima.m_varNamesToQuery.back() + wxS("\")\n"), false, true);
    m_wxMaxima.m_varNamesToQuery.pop_back();
    return true;
  } else {
    if (m_wxMaxima.m_readMaximaVariables) {
      SendMaxima(wxS(":lisp-quiet (wx-print-gui-variables)\n"), false, true);
      m_wxMaxima.m_readMaximaVariables = false;
    } else
      {
        if(m_wxMaxima.m_updateAutocompletion)
          SendMaxima(wxS(":lisp-quiet (wxPrint_autocompletesymbols)\n"), false, true);
        m_wxMaxima.m_updateAutocompletion = false;
      }
    if (m_wxMaxima.GetWorksheet() && (m_wxMaxima.m_variablesPane) &&
        (m_wxMaxima.m_variablesPane->GetEscapedVarnames().size() != 0))
      m_wxMaxima.m_variablesPane->UpdateSize();

    return false;
  }
}

bool MaximaEvaluator::AbortOnError() {
  // Maxima encountered an error.
  // The question is now if we want to try to send it something new to evaluate.
  m_wxMaxima.m_maximaError = true;

  // An error normally drops a batch session into interactive use, so we leave
  // batch mode (m_wxMaxima.ExitAfterEval(false)) to re-enable the interactive-only work
  // (help anchors, autocomplete, ...). But with --exit-on-error we close instead
  // of becoming interactive (see below), so leaving batch mode would only flip
  // m_wxMaxima.m_exitAfterEval to false and wrongly re-enable all that work -- whose
  // background tasks then wedge the shutdown join (the multithreadtest CI hang).
  if (!m_wxMaxima.ExitOnErrorArmed())
    m_wxMaxima.ExitAfterEval(false);
  m_wxMaxima.EvalOnStartup(false);

  if (m_wxMaxima.GetWorksheet()->GetNotification()) {
    if (m_wxMaxima.GetWorksheet()->GetWorkingGroup(true) !=
        m_wxMaxima.GetWorksheet()->GetNotification()->m_errorNotificationCell)
      m_wxMaxima.GetWorksheet()->SetNotification(_("Maxima has issued an error!"),
                                   wxICON_ERROR);
    m_wxMaxima.GetWorksheet()->GetNotification()->m_errorNotificationCell =
      m_wxMaxima.GetWorksheet()->GetWorkingGroup(true);
  }

  if (m_wxMaxima.ExitOnErrorArmed()) {
    wxMaxima::m_exitCode = 1;
    m_wxMaxima.Close();
  }
  if (m_wxMaxima.m_configuration.GetAbortOnError()) {
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    // Inform the user that the evaluation queue is empty.
    m_wxMaxima.EvaluationQueueLength(0);
    m_wxMaxima.GetWorksheet()->ScrollToError();
    return true;
  } else
    return false;
}

void MaximaEvaluator::EvaluateEvent(wxCommandEvent &WXUNUSED(event)) {
  if (m_wxMaxima.GetWorksheet() == NULL)
    return;
  m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  // While sbcl is stopped in its low-level debugger the user's input is an LDB
  // command, not Maxima code: send it to the process's stdin (the channel LDB
  // reads) rather than the control socket, and do not queue it for evaluation.
  if (m_wxMaxima.m_inLDB) {
    EditorCell *ldbEditor = m_wxMaxima.GetWorksheet()->GetActiveCell();
    // The cursor may have wandered off the answer line; the LDB command is
    // then whatever the answer cell holds.
    if (!ldbEditor)
      ldbEditor =
        m_wxMaxima.GetWorksheet()->GetDocumentCellPointers().GetAnswerCell();
    if (ldbEditor) {
      const wxString cmd = ldbEditor->ToString(true);
      m_wxMaxima.m_processManager.WriteToMaximaStdin(cmd);
      // Consume the input caret; LDB's reply (and its next "ldb>" prompt) will
      // arrive on stderr and open a fresh input line.
      m_wxMaxima.GetWorksheet()->QuestionAnswered();
      m_wxMaxima.GetWorksheet()->SetHCaret(ldbEditor->GetGroup());
    }
    return;
  }

  bool evaluating = !m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Empty();
  if (!evaluating)
    m_wxMaxima.GetWorksheet()->FollowEvaluation(true);

  EditorCell *editor = m_wxMaxima.GetWorksheet()->GetActiveCell();
  if (m_wxMaxima.GetWorksheet()->QuestionPending() && m_wxMaxima.GetWorksheet()->GetDocumentCellPointers().GetAnswerCell())
    editor = m_wxMaxima.GetWorksheet()->GetDocumentCellPointers().GetAnswerCell();

  if (editor == NULL) {
    GroupCell *group = NULL;
    if (m_wxMaxima.GetWorksheet()->HasCellsSelected()) {
      // More than one cell is selected
      m_wxMaxima.GetWorksheet()->AddSelectionToEvaluationQueue();
    }
    else
      {
        if (m_wxMaxima.GetWorksheet()->HCaretActive()) {
          group = m_wxMaxima.GetWorksheet()->GetHCaret();
          if (group == NULL)
            // If the cursor is before the 1st cell of the worksheet hcaret reads NULL.
            group = m_wxMaxima.GetWorksheet()->GetTree();
          else
            // The HCaret points to the cell before the horizontal cursor.
            group = group->GetNext();

          // Now we search for the first cell below the cursor that actually contains code.
          while ((group != NULL) &&
                 (!((group->GetEditable() != NULL) &&
                    (group->GetEditable()->GetType() == MC_TYPE_INPUT)) &&
                  (!m_wxMaxima.GetWorksheet()->GetEvaluationQueue().IsLastInQueue(group))))
            group = group->GetNext();
        }
        if ((group != NULL) && (group->GetEditable() != NULL) &&
            (group->GetEditable()->GetType() == MC_TYPE_INPUT))
          editor = group->GetEditable();
      }
  }

  if (editor != NULL) // The cursor is in an active cell
    {
      if (editor->GetType() == MC_TYPE_INPUT && (!m_wxMaxima.m_configuration.InLispMode()))
        editor->AddEnding();
      // if active cell is part of a working group, we have a special
      // case - answering a question. Manually send answer to Maxima.
      GroupCell *cell = editor->GetGroup();
      if (m_wxMaxima.GetWorksheet()->GCContainsCurrentQuestion(cell)) {
        wxString answer = editor->ToString(true);
        // Add the answer to the current working cell or update the answer
        // that is stored within it.
        cell->SetAnswer(m_wxMaxima.GetWorksheet()->GetLastQuestion(), answer);
        SendMaxima(answer, true);
        m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::calculating);
        m_wxMaxima.GetWorksheet()->SetHCaret(cell);
        m_wxMaxima.GetWorksheet()->ScrollToCaret();
      } else { // normally just add to queue (and mark the cell as no more
        // containing an error message)
        m_wxMaxima.GetWorksheet()->GetErrorList().Remove(cell);
        m_wxMaxima.GetWorksheet()->AddCellToEvaluationQueue(cell);
      }
    } else { // no evaluate has been called on no active cell?
    m_wxMaxima.GetWorksheet()->AddSelectionToEvaluationQueue();
  }
  // Inform the user about the length of the evaluation queue.
  m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                        m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());
  TriggerEvaluation();
}

void MaximaEvaluator::TriggerEvaluation() {
  if(!m_wxMaxima.GetWorksheet())
    return;
  // If evaluation is already running we don't have anything to do
  if (m_wxMaxima.m_maximaBusy)
    {
      wxLogMessage(_("Not triggering evaluation as maxima is still busy!"));
      return;
    }

  // While we wait for an answer we cannot send new commands.
  if (m_wxMaxima.GetWorksheet()->QuestionPending())
    {
      wxLogMessage(_("Not triggering evaluation as maxima still asks a question!"));
      return;
    }

  // If we aren't connected yet this function will be triggered as soon as
  // maxima connects to wxMaxima
  if (!m_wxMaxima.m_client || (!m_wxMaxima.m_client->IsConnected()))
    {
      wxLogMessage(_("Not triggering evaluation as there is no working maxima process"));
      return;
    }

  // Maxima is connected. Let's test if the evaluation queue is empty.
  GroupCell *const tmp = m_wxMaxima.GetWorksheet()->GetEvaluationQueue().GetCell();
  if (!tmp) {
    wxLogMessage(_("Evaluation ended, since evaluation queue is empty."));
    // Maxima is no more busy.
    if (m_wxMaxima.m_maximaError)
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::maximaerror);
    else
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    // Inform the user that the evaluation queue length now is 0.
    m_wxMaxima.EvaluationQueueLength(0);
    // Now we want to start to display things immediately again
    m_wxMaxima.m_fastResponseTimer.Stop();
    // The cell from the last evaluation might still be shown in it's
    // "evaluating" state so let's refresh the console to update the display of
    // this.
    m_wxMaxima.GetWorksheet()->RequestRedraw();

    // If the window isn't active we can inform the user that maxima in the
    // meantime has finished working.
    if ((m_wxMaxima.m_configuration.NotifyIfIdle()) && (m_wxMaxima.GetWorksheet()->GetTree() != NULL))
      m_wxMaxima.GetWorksheet()->SetNotification(_("Maxima has finished calculating."));

    if (m_wxMaxima.m_configCommands != wxEmptyString)
      SendMaxima(m_wxMaxima.m_configCommands, false, true);
    m_wxMaxima.m_configCommands.Clear();
    QueryVariableValue();
    return; // empty queue
  }

  // Add a semicolon at the end of the cell, if needed.
  if (tmp->AddEnding())
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().AddEnding();

  // Display the evaluation queue's status.
  m_wxMaxima.EvaluationQueueLength(m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                        m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());


  // Maxima is connected, not asking a question and the queue contains an item.

  // From now on we look every second if we got some output from a crashing
  // maxima: Is maxima is working correctly the stdout and stderr descriptors we
  // poll don't offer any data.
  m_wxMaxima.m_responseReader.ReadStdErr();
  m_wxMaxima.m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

  if (m_wxMaxima.GetWorksheet()->GetEvaluationQueue().m_workingGroupChanged) {
    // Clear the monitor that shows the xml representation of the output of the
    // current maxima command.
    if ((m_wxMaxima.m_xmlInspector) && (m_wxMaxima.IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_wxMaxima.m_xmlInspector->Clear();

    // If the cell's output that we are about to remove contains the currently
    // selected cells we undo the selection.
    if (m_wxMaxima.GetWorksheet()->GetSelectionStart()) {
      if (m_wxMaxima.GetWorksheet()->GetSelectionStart()->GetGroup() == tmp)
        m_wxMaxima.GetWorksheet()->ClearSelection();
    }
    if (m_wxMaxima.GetWorksheet()->GetSelectionEnd()) {
      if (m_wxMaxima.GetWorksheet()->GetSelectionEnd()->GetGroup() == tmp)
        m_wxMaxima.GetWorksheet()->ClearSelection();
    }
    tmp->RemoveOutput();
    m_wxMaxima.GetWorksheet()->RequestRecalculation(tmp);
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  wxString text = m_wxMaxima.GetWorksheet()->GetEvaluationQueue().GetCommand();
  m_wxMaxima.m_commandIndex = m_wxMaxima.GetWorksheet()->GetEvaluationQueue().GetIndex();
  if ((text != wxEmptyString) && (text != wxS(";")) && (text != wxS("$"))) {
    std::size_t index;
    // Validate the SINGLE command we are about to send, in the lisp/maxima mode
    // that is current now - NOT the whole cell. A cell that switches to lisp mode
    // (to_lisp() ... (to-maxima)) contains lisp forms whose "$" would look like a
    // maxima statement terminator sitting inside an open "(" if the whole cell
    // were checked in maxima mode, giving a false "un-closed parenthesis" error.
    // Each command is already tokenized in its own mode by the evaluation queue.
    wxString parenthesisError =
      m_wxMaxima.GetUnmatchedParenthesisState(text, index);
    if (parenthesisError.IsEmpty()) {
      if (m_wxMaxima.GetWorksheet()->FollowEvaluation()) {
        m_wxMaxima.GetWorksheet()->SetSelection(tmp);
        if (!m_wxMaxima.GetWorksheet()->GetWorkingGroup()) {
          m_wxMaxima.GetWorksheet()->SetHCaret(tmp);
          m_wxMaxima.GetWorksheet()->ScrollToCaret();
        }
      }

      m_wxMaxima.GetWorksheet()->SetWorkingGroup(tmp);
      tmp->GetPrompt()->SetValue(m_wxMaxima.m_lastPrompt);
      tmp->ResetSize();

      wxLogMessage(_("Sending a new command to Maxima."));
      // Only send the config commands if they are not blank: an all-whitespace
      // (or empty) send results in a bare newline being transmitted, which a
      // normal maxima prompt ignores - but the maxima debugger prompt (dbm:N)
      // interprets an empty line as "repeat the last command", which e.g.
      // makes ":h" followed by ":continue" show the help text twice.
      if (!MaximaProtocol::CommandIsBlank(m_wxMaxima.m_configCommands))
        SendMaxima(m_wxMaxima.m_configCommands);
      SendMaxima(text, true);
      m_wxMaxima.m_maximaBusy = true;
      // Now that we have sent a command we need to query all variable values
      // anew
      if(m_wxMaxima.m_variablesPane)
        m_wxMaxima.m_varNamesToQuery = m_wxMaxima.m_variablesPane->GetEscapedVarnames();
      // And the gui is interested in a few variable names
      m_wxMaxima.m_readMaximaVariables = true;
      m_wxMaxima.m_configCommands.Clear();

      m_wxMaxima.EvaluationQueueLength(
                            m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Size(),
                            m_wxMaxima.GetWorksheet()->GetEvaluationQueue().CommandsLeftInCell());

      text.Trim(false);
      if (!m_wxMaxima.m_hasEvaluatedCells) {
        if (text.StartsWith(wxS(":lisp")))
          m_wxMaxima.StatusText(_("A \":lisp\" as the first command might fail to "
                       "send a \"finished\" signal."));
      }

      // Mark the current maxima process as "no more in its initial condition".
      m_wxMaxima.m_hasEvaluatedCells = true;
    } else {
      // Manually mark the current cell as the one that has caused an error.
      m_wxMaxima.GetWorksheet()->GetErrorList().Add(tmp);
      // Inform the status bar about the error
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::maximaerror);
      // Inform the user about the error (which automatically causes the
      // worksheet to the cell we marked as erroneous a few seconds ago.
      auto cell =
        std::make_unique<TextCell>(tmp, &m_wxMaxima.m_configuration,
                                   _("Refusing to send cell to maxima: ") +
                                   parenthesisError + wxS("\n"));
      cell->SetType(MC_TYPE_ERROR);
      tmp->SetOutput(std::move(cell));
      m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
      m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
      tmp->GetEditable()->SetCaretPosition(index);
      tmp->GetEditable()->SetErrorIndex((m_wxMaxima.m_commandIndex = index) - 1);

      if (m_wxMaxima.GetWorksheet()->FollowEvaluation())
        m_wxMaxima.GetWorksheet()->SetSelection(NULL);

      m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      if (!AbortOnError()) {
        m_wxMaxima.m_outputCellsFromCurrentCommand = 0;
        TriggerEvaluation();
      }
      m_wxMaxima.GetWorksheet()->SetActiveCell(tmp->GetEditable());
    }
  } else {
    wxLogMessage(_("Empty command => re-triggering evaluation"));
    m_wxMaxima.m_outputCellsFromCurrentCommand = 0;
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().RemoveFirst();
    TriggerEvaluation();
  }
}


void MaximaEvaluator::SetupVariables() {
  wxLogMessage(_("Sending maxima the info how to express 2d maths as XML"));
  if (m_wxMaxima.m_exitAfterEval) {
    SendMaxima(":lisp-quiet (defvar *wx-defer-queries* t)\n");
  }
  wxMathML wxmathml(&m_wxMaxima.m_configuration);
  SendMaxima(wxmathml.GetCmd());
  wxString cmd;

#if defined(__WXOSX__)
  wxString gnuplot_binary = m_wxMaxima.m_gnuplotcommand;

  gnuplot_binary.Replace("\\", "\\\\");
  gnuplot_binary.Replace("\"", "\\\"");
  if (wxFileExists(m_wxMaxima.m_gnuplotcommand))
    cmd += wxS("\n:lisp-quiet (setf $gnuplot_command \"") + m_wxMaxima.m_gnuplotcommand +
      wxS("\")\n");
  wxLogMessage(_("Setting gnuplot_binary to %s"),
               m_wxMaxima.m_gnuplotcommand);
#endif
  cmd.Replace(wxS("\\"), wxS("/"));
  SendMaxima(cmd);

  switch(m_wxMaxima.m_configuration.MaximaHelpFormat())
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
                 m_wxMaxima.m_promptPrefix + "(%i1)" + m_wxMaxima.m_promptSuffix + "\"))\n"));
  wxLogMessage(_("Setting prompt and help format"));
  SendMaxima(wxS(":lisp-quiet (setf *prompt-suffix* \"") +
             m_wxMaxima.m_promptSuffix + wxS("\") (setf *prompt-prefix* \"") +
             m_wxMaxima.m_promptPrefix +
             wxS("\") (setf $in_netmath nil) (setf $show_openplot t) ") +
             wxS("\n"));

  m_wxMaxima.ConfigChanged();
}
