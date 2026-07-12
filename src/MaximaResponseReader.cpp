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
  Implements the Maxima-response handlers extracted from the wxMaxima god class.
*/

#include "MaximaResponseReader.h"
#include "wxMaxima.h"
#include "Maxima.h"
#include "EventIDs.h"

void MaximaResponseReader::ReadStatusBar(const wxXmlDocument &xmldoc) {
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);
  if(!xmldoc.IsOk())
    {
      m_wxMaxima.DoRawConsoleAppend(_("There was an error in the XML that should describe the status bar message.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      m_wxMaxima.AbortOnError();
    }
  else
    {
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *contents = node->GetChildren();
        if (contents)
          m_wxMaxima.StatusText(contents->GetContent(), false);
      }
    }
}

void MaximaResponseReader::ReadManualTopicNames(const wxXmlDocument &xmldoc) {
  if(xmldoc.IsOk())
    {
      std::vector<wxString> topics;
      wxXmlNode *node = xmldoc.GetRoot();
      while ((node) && (node->GetName() != wxS("html-manual-keywords")))
        node = node->GetNext();

      if (node == NULL) {
        wxLogMessage(_("No topics found in topic tag"));
      } else {
        for (wxXmlNode *entry = node->GetChildren(); entry != NULL;
             entry = entry->GetNext())
          {
            if (entry->GetName() == wxS("keyword")) {
              wxXmlNode *topic = entry->GetChildren();
              if (topic) {
                wxLogMessage(_("Received manual topic request: %s"),
                             topic->GetContent().ToUTF8().data());
                topics.push_back(topic->GetContent());
              }
            }
          }
        if (topics.empty())
          wxLogMessage(_("No topics found in topic flag"));
        else
          {
#ifdef USE_WEBVIEW
            m_wxMaxima.m_helpPane->SelectKeywords(topics);
            m_wxMaxima.ShowPane(EventIDs::menu_pane_help);
#else
            m_wxMaxima.ShowMaximaHelp(topics.front());
#endif
          }
      }
    }
  else
    {
      m_wxMaxima.DoRawConsoleAppend(_("There was an error in the XML that should describe the manual topics.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      m_wxMaxima.AbortOnError();
    }
}


/***
 * Checks if maxima displayed a new chunk of math
 */
void MaximaResponseReader::ReadMath(const wxXmlDocument &xml) {
  if(!m_wxMaxima.GetWorksheet())
    return;

  m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);

  // Append everything from the "beginning of math" to the "end of math" marker
  // to the console
  if (m_wxMaxima.m_configuration.UseUserLabels()) {
    m_wxMaxima.ConsoleAppend(xml, MC_TYPE_DEFAULT,
                  m_wxMaxima.GetWorksheet()->GetEvaluationQueue().GetUserLabel());
  } else {
    m_wxMaxima.ConsoleAppend(xml, MC_TYPE_DEFAULT);
  }
}

void MaximaResponseReader::ReadLoadSymbols(const wxXmlDocument &data) {
  m_wxMaxima.GetWorksheet()->AddSymbols(data);
}

void MaximaResponseReader::ReadFirstPrompt(const wxString &data) {
  m_wxMaxima.m_firstPromptBuffer += data;
  auto end = m_wxMaxima.m_firstPromptBuffer.Find(m_wxMaxima.m_firstPrompt);
  if (end  == wxNOT_FOUND)
    return;

  m_wxMaxima.m_bytesFromMaxima = 0;

  int start = 0;
  start = m_wxMaxima.m_firstPromptBuffer.Find(wxS("Maxima "));
  if (start == wxNOT_FOUND)
    start = 0;
  m_wxMaxima.FirstOutput();

  m_wxMaxima.m_maximaBusy = false;

  // Wait for a line maxima informs us about it's process id in.
  int s = m_wxMaxima.m_firstPromptBuffer.Find(wxS("pid="));
  if (s != wxNOT_FOUND) {
    s += 4;
    int t =
        s + m_wxMaxima.m_firstPromptBuffer.SubString(s, m_wxMaxima.m_firstPromptBuffer.Length()).Find(wxS("\n")) - 1;

    // Read this pid
    if (s < t)
      if (!m_wxMaxima.m_firstPromptBuffer.SubString(s, t).ToLong(&m_wxMaxima.m_maximaPid))
        wxLogMessage(_("Cannot interpret the numeric value of pid %s"),
                     m_wxMaxima.m_firstPromptBuffer.SubString(s, t));
  }

  if (m_wxMaxima.m_pid > 0)
    m_wxMaxima.m_MenuBar->EnableItem(EventIDs::menu_interrupt_id, true);

  m_wxMaxima.m_client->ClearFirstPrompt();
  m_wxMaxima.m_first = false;
  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  m_wxMaxima.m_closing = false; // when restarting maxima this is temporarily true

  wxString prompt_compact = m_wxMaxima.m_firstPromptBuffer.Left(
      start + static_cast<std::size_t>(end) + m_wxMaxima.m_firstPrompt.Length() - 1);
  prompt_compact.Replace(wxS("\n"), wxS("\u21b2"));

  wxLogMessage(_("Received maxima's first prompt: %s"), prompt_compact);

  if (m_wxMaxima.m_maximaPid > 0)
    wxLogMessage(_("Maxima's PID is %li"), m_wxMaxima.m_maximaPid);
  else
    wxLogMessage(_("Maxima's PID is %li"), m_wxMaxima.m_pid);

  m_wxMaxima.m_firstPromptBuffer.Clear();

  if (m_wxMaxima.GetWorksheet() && (m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Empty())) {
    // Inform the user that the evaluation queue is empty.
    m_wxMaxima.EvaluationQueueLength(0);
    if (m_wxMaxima.GetWorksheet() && (m_wxMaxima.m_configuration.GetOpenHCaret()) &&
        (m_wxMaxima.GetWorksheet()->GetActiveCell() == NULL))
      m_wxMaxima.GetWorksheet()->OpenNextOrCreateCell();
  } else
    m_wxMaxima.TriggerEvaluation();
}

void MaximaResponseReader::ReadMiscText(const wxString &data) {
  if(!m_wxMaxima.m_maximaAuthenticated)
    return;

  auto style = MC_TYPE_ASCIIMATHS;

  if (data.StartsWith(wxS("(%")))
    style = MC_TYPE_TEXT;

  if (data.IsEmpty())
    return;

  if (data == "\r")
    return;

  if (m_wxMaxima.GetWorksheet() && (data.StartsWith("\n")))
    m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);

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
    if (m_wxMaxima.m_gnuplotErrorRegex.Matches(mergedWhitespace))
      style = MC_TYPE_ERROR;
  }

  // Add the text line to the console
  if (m_wxMaxima.GetWorksheet() && (!data.empty())) {
    m_wxMaxima.GetWorksheet()->SetCurrentTextCell(m_wxMaxima.ConsoleAppend(data, style));
    if (style == MC_TYPE_ERROR)
      m_wxMaxima.AbortOnError();
  }
  if (m_wxMaxima.GetWorksheet() && (data.EndsWith("\n")))
    m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);
}

void MaximaResponseReader::ReadSuppressedOutput(const wxString &data) {
  if(!m_wxMaxima.m_maximaAuthenticated)
    {
      if(data.Find("</wxxml-key>") != wxNOT_FOUND) {
        if(data.Find("<wxxml-key>" + m_wxMaxima.m_maximaAuthString + "</wxxml-key>")){
          wxLogMessage(_("Maxima has authenticated!"));
          m_wxMaxima.m_maximaAuthenticated = true;
        } else {
          wxLogMessage(_("Cannot authenticate Maxima!"));
          LoggingMessageBox(
                            _("Could not make sure that we talk to the maxima we started => "
                              "discarding all data it sends."),
                            _("Warning"), wxOK | wxICON_EXCLAMATION);
          m_wxMaxima.m_discardAllData = true;
        }
      }
    }

  if(!m_wxMaxima.m_maximaAuthenticated)
    {
      wxLogMessage(_("Maxima didn't attempt to authenticate!"));
      LoggingMessageBox(
                        _("Could not make sure that we talk to the maxima we started => "
                          "discarding all data it sends."),
                        _("Warning"), wxOK | wxICON_EXCLAMATION);
      m_wxMaxima.m_discardAllData = true;
    }
}

void MaximaResponseReader::ReadPrompt(const wxString &data) {
  m_wxMaxima.m_evalOnStartup = false;
  if(!m_wxMaxima.GetWorksheet())
    return;

  m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);

  // Assume we don't have a question prompt
  m_wxMaxima.GetWorksheet()->QuestionPending(false);
  m_wxMaxima.m_ready = true;

  wxLogMessage(_("Got a new input prompt!"));
  m_wxMaxima.m_maximaBusy = false;
  m_wxMaxima.m_bytesFromMaxima = 0;

  wxString label = data.SubString(m_wxMaxima.m_promptPrefix.Length(),
                                  data.Length() - m_wxMaxima.m_promptSuffix.Length() - 1);

  // If we got a prompt our connection to maxima was successful.
  if (m_wxMaxima.m_unsuccessfulConnectionAttempts > 0)
    m_wxMaxima.m_unsuccessfulConnectionAttempts--;
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
      m_wxMaxima.m_configuration.InLispMode() || (label.StartsWith(wxS("MAXIMA>"))) ||
      (label.StartsWith(wxS("\nMAXIMA>")))) {
    // Maxima displayed a new main prompt => We don't have a question
    m_wxMaxima.GetWorksheet()->QuestionAnswered();
    // And we can remove one command from the evaluation queue.
    m_wxMaxima.GetWorksheet()->GetEvaluationQueue().RemoveFirst();

    m_wxMaxima.m_lastPrompt = label;
    // remove the event maxima has just processed from the evaluation queue
    // if we remove a command from the evaluation queue the next output line
    // will be the first from the next command.
    m_wxMaxima.m_outputCellsFromCurrentCommand = 0;
    if (m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Empty()) { // queue empty.
      // This worksheet has drained its evaluation queue, so a clean batch run
      // is done and may exit normally. Disarm exit-on-error for THIS worksheet
      // only -- m_exitOnError is process-wide and shared, so clearing it here
      // would disable exit-on-error in every other window of a --single_process
      // run (the multithreadtest hang).
      m_wxMaxima.m_exitOnErrorArmed = false;
      if (m_wxMaxima.m_maximaError)
        m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::maximaerror);
      else
        m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
      // If we have selected a cell in order to show we are evaluating it
      // we should now remove this marker.
      if (m_wxMaxima.GetWorksheet()->FollowEvaluation()) {
        if (m_wxMaxima.GetWorksheet()->GetActiveCell())
          m_wxMaxima.GetWorksheet()->GetActiveCell()->SelectNone();
        m_wxMaxima.GetWorksheet()->ClearSelection();
      }
      m_wxMaxima.GetWorksheet()->FollowEvaluation(false);
      // Inform the user that the evaluation queue is empty.
      m_wxMaxima.EvaluationQueueLength(0);
      m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
      m_wxMaxima.GetWorksheet()->GetEvaluationQueue().RemoveFirst();
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      // Now that maxima is idle we can ask for the contents of its variables
      m_wxMaxima.QueryVariableValue();
    } else { // we don't have an empty queue
      m_wxMaxima.m_ready = false;
      m_wxMaxima.GetWorksheet()->RequestRedraw();
      m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::sending);
      m_wxMaxima.TriggerEvaluation();
    }

    if (m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Empty()) {
      if ((m_wxMaxima.m_configuration.GetOpenHCaret()) &&
          (m_wxMaxima.GetWorksheet()->GetActiveCell() == NULL))
        m_wxMaxima.GetWorksheet()->OpenNextOrCreateCell();
    }
  } else { // We have a question
    m_wxMaxima.GetWorksheet()->SetLastQuestion(label);
    m_wxMaxima.GetWorksheet()->QuestionAnswered();
    m_wxMaxima.GetWorksheet()->QuestionPending(true);
    // If the user answers a question additional output might be required even
    // if the question has been preceded by many lines.
    m_wxMaxima.m_outputCellsFromCurrentCommand = 0;

    bool autoAnswer = m_wxMaxima.GetWorksheet()->OpenQuestionCaret();

    if (m_wxMaxima.GetWorksheet()->ScrolledAwayFromEvaluation()) {
      if (m_wxMaxima.GetWorksheet()->m_mainToolBar)
        m_wxMaxima.GetWorksheet()->m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    }

    if (!label.IsEmpty()) {
      int options = wxMaxima::AppendOpt::NewLine | wxMaxima::AppendOpt::BigSkip;
      if (!autoAnswer)
        options |= wxMaxima::AppendOpt::PromptToolTip;

      if (std::max(label.Find(m_wxMaxima.m_mathPrefix1), label.Find(m_wxMaxima.m_mathPrefix2)) >= 0)
        m_wxMaxima.DoConsoleAppend(label, MC_TYPE_PROMPT, wxMaxima::AppendOpt(options));
      else
        m_wxMaxima.DoRawConsoleAppend(label, MC_TYPE_PROMPT, wxMaxima::AppendOpt(options));
    }

    if (!autoAnswer) {
      if ((m_wxMaxima.GetWorksheet()->GetWorkingGroup() == NULL) ||
          ((m_wxMaxima.GetWorksheet()->GetWorkingGroup()->m_knownAnswers.empty()) &&
           m_wxMaxima.GetWorksheet()->GetWorkingGroup()->AutoAnswer()))
        m_wxMaxima.GetWorksheet()->SetNotification(_("Maxima asks a question!"),
                                        wxICON_INFORMATION);
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::userinput);
    }
  }
  label.Trim(false);
  if (label.StartsWith(wxS("MAXIMA>")) || label.StartsWith("(dbm:")) {
    if (!m_wxMaxima.m_configuration.InLispMode()) {
      if (label.StartsWith("(dbm:"))
        wxLogMessage(_("Switched to lisp mode after receiving a lisp debug prompt!"));
      else
        wxLogMessage(_("Switched to lisp mode after receiving a lisp prompt!"));
    }
    m_wxMaxima.m_configuration.InLispMode(true);
  } else {
    if (m_wxMaxima.m_configuration.InLispMode())
      wxLogMessage(_("Ended lisp mode after receiving a maxima prompt!"));
    m_wxMaxima.m_configuration.InLispMode(false);
  }
}

void MaximaResponseReader::ReadStdErr() {
  // Maxima sends us its actual results over the network socket, not via its
  // stdout/stderr. But those streams are not silent after startup: in
  // particular Maxima forwards the stdout and stderr of the gnuplot process it
  // launches, so plotting errors and warnings (and the "End of animation
  // sequence" / Fontconfig / QSocketNotifier chatter filtered below) arrive
  // here. We also want to surface anything that turns up if something is
  // severely broken. Hence we keep reading and reporting both streams.

  if (m_wxMaxima.m_maximaProcess == NULL)
    return;

  if (m_wxMaxima.m_maximaProcess->IsInputAvailable()) {
    wxASSERT_MSG(
                 m_wxMaxima.m_maximaStdout != NULL,
                 wxS("Bug: Trying to read from maxima but don't have an input stream"));
    if(m_wxMaxima.m_maximaStdout == NULL)
      return;
    wxTextInputStream istrm(*m_wxMaxima.m_maximaStdout, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_wxMaxima.m_maximaStdout->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = _("Message from the stdout of Maxima: ") + o;
    if ((o_trimmed != wxEmptyString) &&
        (!o_trimmed.StartsWith("Connecting Maxima to server on port")) && (!m_wxMaxima.m_first)) {
      m_wxMaxima.DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
      if (Maxima::GetPipeToStdErr())
        std::cerr << o;
    }
  }
  if (m_wxMaxima.m_maximaProcess->IsErrorAvailable()) {
    wxASSERT_MSG(m_wxMaxima.m_maximaStderr != NULL,
                 wxS("Bug: Trying to read from maxima but don't have a error "
                     "input stream"));
    if(m_wxMaxima.m_maximaStderr == NULL)
      return;
    wxTextInputStream istrm(*m_wxMaxima.m_maximaStderr, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_wxMaxima.m_maximaStderr->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();

    o = wxS("Message from maxima's stderr stream: ") + o;

    if ((o != wxS("Message from maxima's stderr stream: End of animation sequence")) &&
        (o != wxS("Message from maxima's stderr stream: Fontconfig warning: using without calling FcInit()")) &&  // harmless warning, which may occur with Gnuplot 6 and fontconfig
        (o != wxS("Message from maxima's stderr stream: QSocketNotifier: Can only be used with threads started with QThread")) &&  // Maybe related to Gnuplot / Wayland?
        !o.Contains("frames in animation sequence") &&
        (o_trimmed != wxEmptyString) && (o.Length() > 1)) {
      m_wxMaxima.DoRawConsoleAppend(o, MC_TYPE_ERROR);
      m_wxMaxima.AbortOnError();
      m_wxMaxima.TriggerEvaluation();
      m_wxMaxima.GetWorksheet()->GetErrorList().Add(m_wxMaxima.GetWorksheet()->GetWorkingGroup(true));

      if (Maxima::GetPipeToStdErr())
        std::cerr << o;
    } else
      m_wxMaxima.DoRawConsoleAppend(o, MC_TYPE_DEFAULT);
  }
}

