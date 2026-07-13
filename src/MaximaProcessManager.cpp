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
  Implements the Maxima process/socket lifecycle extracted from the wxMaxima
  god class.
*/

#include "MaximaProcessManager.h"
#include "wxMaxima.h"
#include "Maxima.h"
#include "EventIDs.h"
#include "dialogs/MaximaNotStartingDialog.h"
#include <wx/sstream.h>

#ifndef __WXMSW__
#include <csignal>
#include <cstring>
#include <sys/types.h>
#endif

// ---------------------------------------------------------------------------
// If wxMaxima is terminated by a signal (or crashes) our destructor (and hence
// KillMaxima()) does not run, so a child Maxima would be orphaned. Worse, a
// Maxima busy in a computation only notices its closed control socket the next
// time it reads from it, so it keeps running and eats CPU/RAM. To avoid that we
// keep an async-signal-safe registry of running child Maxima group-leader PIDs
// and SIGKILL them from a SIGTERM/SIGINT/SIGHUP handler (and from
// OnFatalException()). On Windows Maxima runs under maxima.bat and the existing
// taskkill-based cleanup is used instead; everything here compiles to no-ops.
// ---------------------------------------------------------------------------
#ifndef __WXMSW__
namespace {
// One slot per wxMaxima window. We only register/unregister from the main (GUI)
// thread and each slot is a sig_atomic_t (atomic single writes), so the handler
// never sees a torn value and no locking is needed.
constexpr int kMaxChildMaximas = 256;
volatile sig_atomic_t s_childMaximaPids[kMaxChildMaximas] = {};

void wxmTerminationSignalHandler(int sig) {
  MaximaProcessManager::KillAllChildMaximas();
  // The disposition was reset to default by SA_RESETHAND before we were called,
  // so re-raising now performs the default action (terminate) with the right
  // status. raise() and the work above are all async-signal-safe.
  raise(sig);
}
} // namespace
#endif

void MaximaProcessManager::RegisterChildMaxima(long pid) {
#ifndef __WXMSW__
  if (pid <= 0)
    return;
  for (auto &slot : s_childMaximaPids) {
    if (slot == 0) {
      slot = static_cast<sig_atomic_t>(pid);
      return;
    }
  }
#else
  (void)pid;
#endif
}

void MaximaProcessManager::UnregisterChildMaxima(long pid) {
#ifndef __WXMSW__
  if (pid <= 0)
    return;
  for (auto &slot : s_childMaximaPids) {
    if (slot == static_cast<sig_atomic_t>(pid)) {
      slot = 0;
      return;
    }
  }
#else
  (void)pid;
#endif
}

void MaximaProcessManager::KillAllChildMaximas() {
#ifndef __WXMSW__
  for (sig_atomic_t pid : s_childMaximaPids) {
    if (pid > 0) {
      // Interactive Maxima is a process-group leader, so the negative pid kills
      // the whole group (the maxima wrapper and the Lisp it forks). In batch
      // mode it is not a group leader: the group kill is then a harmless ESRCH
      // no-op and the direct kill takes down the wrapper.
      kill(static_cast<pid_t>(-pid), SIGKILL);
      kill(static_cast<pid_t>(pid), SIGKILL);
    }
  }
#endif
}

void MaximaProcessManager::SetupTerminationHandlers() {
#ifndef __WXMSW__
  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = wxmTerminationSignalHandler;
  sigemptyset(&sa.sa_mask);
  // SA_RESETHAND: after we kill Maxima and re-raise, the default action runs and
  // a repeated signal is handled normally (no risk of looping in the handler).
  sa.sa_flags = SA_RESETHAND;
  sigaction(SIGTERM, &sa, nullptr);
  sigaction(SIGINT, &sa, nullptr);
  sigaction(SIGHUP, &sa, nullptr);
#endif
}

void MaximaProcessManager::ServerEvent(wxSocketEvent &event) {
  switch (event.GetSocketEvent()) {
  case wxSOCKET_CONNECTION:
    OnMaximaConnect();
    break;

  default:
    wxLogMessage(_("Encountered an unknown socket event."));
    break;
  }
}

void MaximaProcessManager::OnMaximaConnect() {
  if (m_wxMaxima.m_client && (m_wxMaxima.m_client->IsConnected())) {
    wxLogMessage(_("New connection attempt whilst already connected."));
    return;
  }
  if (m_wxMaxima.m_maximaProcess == NULL) {
    wxLogMessage(_("New connection attempt, but no currently running maxima process."));
    return;
  }
  if (m_wxMaxima.m_server == NULL) {
    wxLogMessage(_("New connection attempt, but no currently no socket maxima could connect to."));
    return;
  }

  m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::idle);
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.GetWorksheet()->QuestionAnswered();

  m_wxMaxima.m_client = std::make_unique<Maxima>(m_wxMaxima.m_server->Accept(false), &m_wxMaxima.m_configuration);
  if (m_wxMaxima.m_client->IsConnected()) {
    m_wxMaxima.m_client->Bind(EVT_MAXIMA, &MaximaProcessManager::MaximaEvent, this);
    m_wxMaxima.m_evaluator.SetupVariables();
  } else {
    wxLogMessage(_("Connection attempt, but connection failed."));
    m_wxMaxima.m_unsuccessfulConnectionAttempts++;
    if (m_wxMaxima.m_unsuccessfulConnectionAttempts < 12) {
      wxLogMessage(_("Trying to restart maxima."));
      StartMaxima(true);
      return;
    }
  }
}

bool MaximaProcessManager::StartServer() {
  if (m_wxMaxima.m_server) {
    if(m_wxMaxima.m_server->IsOk())
      m_wxMaxima.m_server->Close();
    m_wxMaxima.m_server.reset();
  }
  m_wxMaxima.m_port = m_wxMaxima.m_configuration.DefaultPort() + m_wxMaxima.m_unsuccessfulConnectionAttempts;

  do {
    wxLogMessage(_("Trying to start the socket a Maxima on the local "
                   "machine can connect to on port %i"), m_wxMaxima.m_port);

    // Currently, only wxIPV4address is implemented (current wxWidgets 3.2.6)
    // https://docs.wxwidgets.org/3.2.6/classwx_i_paddress.html
    wxIPV4address addr;
    if (!addr.LocalHost())
      wxLogMessage(_("Cannot set the communication address to localhost."));
    if (!addr.Service(m_wxMaxima.m_port))
      wxLogMessage(_("Cannot set the communication port to %i."), m_wxMaxima.m_port);
    m_wxMaxima.m_server = std::unique_ptr<wxSocketServer,
                               wxMaxima::ServerDeleter>(
                                              new wxSocketServer(addr, wxSOCKET_WAITALL_WRITE));
    if (!m_wxMaxima.m_server->IsOk()) {
      m_wxMaxima.m_port++;
      m_wxMaxima.m_server.reset();
    }
  } while (((m_wxMaxima.m_port < m_wxMaxima.m_configuration.DefaultPort() + 15000) &&
            (m_wxMaxima.m_port < 65535) && (!m_wxMaxima.m_server)));

  if (!m_wxMaxima.m_server) {
    m_wxMaxima.StatusText(_("Starting server failed"));
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::error);
    // Deferred: StartServer runs inside StartMaxima, which can itself run
    // under a wxProcess-termination event (restart after a crash); no modal
    // dialogs inside event handlers.
    m_wxMaxima.CallAfter([]{
      LoggingMessageBox(_("wxMaxima could not start a server.\n\n"
                          "Please check you have network support\n"
                          "enabled, that no internet safety appliance is configured "
                          "to intercept creation of servers even if said server only accepts "
                          "connections from local application (maxima insists on the graphical "
                          "frontend acting as a server it can connect on over a local socket) "
                          "and try again!"),
                        _("Fatal error"), wxOK | wxICON_ERROR);
    });

    return false;
  } else {
    m_wxMaxima.m_server->SetEventHandler(*m_wxMaxima.GetEventHandler());
    m_wxMaxima.m_server->Notify(true);
    m_wxMaxima.m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
    m_wxMaxima.m_server->SetTimeout(30);
    m_wxMaxima.StatusText(_("Server started"));
    return true;
  }
}

bool MaximaProcessManager::StartMaxima(bool force) {
  wxString dirname;
  {
    wxString filename;
    if(m_wxMaxima.GetWorksheet())
      filename = m_wxMaxima.GetWorksheet()->GetCurrentFile();
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

  if ((m_wxMaxima.m_maximaProcess == NULL) || (m_wxMaxima.m_hasEvaluatedCells) || force ||
      (dirname != dirname_Old)) {
    if (!StartServer())
      return false;

    if ((m_wxMaxima.m_maximaProcess != NULL) || (m_wxMaxima.m_pid >= 0) || (m_wxMaxima.m_client))
      {
        m_wxMaxima.m_unsuccessfulConnectionAttempts = 0;
        KillMaxima();
      }

    if ((m_wxMaxima.m_xmlInspector) && (m_wxMaxima.IsPaneDisplayed(EventIDs::menu_pane_xmlInspector)))
      m_wxMaxima.m_xmlInspector->Clear();

    // Maxima isn't in lisp mode
    m_wxMaxima.m_configuration.InLispMode(false);

    // Maxima isn't asking questions
    m_wxMaxima.QuestionAnswered();

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

    m_wxMaxima.m_maximaStdoutPollTimer.StartOnce(MAXIMAPOLLMSECS);

    wxString command = m_wxMaxima.GetCommand();
    if (!command.IsEmpty()) {
      command.Append(wxString::Format(wxS(" -s %d "), (int)m_wxMaxima.m_port));

      m_wxMaxima.m_maximaProcess = new wxProcess(&m_wxMaxima, m_wxMaxima.m_maxima_process_id);
      m_wxMaxima.m_maximaProcess->Redirect();
      //      m_wxMaxima.m_maximaProcess->SetPriority(wxPRIORITY_MAX);
      m_wxMaxima.m_first = true;
      m_wxMaxima.m_firstPromptBuffer.Clear();
      m_wxMaxima.m_pid = -1;
      m_wxMaxima.m_maximaPid = -1;
      wxLogMessage(_("Running maxima as: %s"), command);

      wxEnvVariableHashMap environment;
      environment = m_wxMaxima.m_configuration.MaximaEnvVars();
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
      m_wxMaxima.m_maximaAuthenticated = false;
      m_wxMaxima.m_discardAllData = false;
      // XOR OS entropy (RandomEntropy()) with PRNG output so the token is strong
      // when either source is good — entropy of the XOR is >= max(both sources).
      static constexpr size_t TOKEN_BYTES = 512;
      static_assert(TOKEN_BYTES % sizeof(uint32_t) == 0, "TOKEN_BYTES must be a multiple of 4");
      std::unique_ptr<wxExecuteEnv> env = std::unique_ptr<wxExecuteEnv>(new wxExecuteEnv);
      wxMemoryBuffer membuf(TOKEN_BYTES);
      std::uniform_int_distribution<unsigned int> byteUD(0, 255);
      for (size_t i = 0; i < TOKEN_BYTES / sizeof(uint32_t); i++) {
        auto rdVal = static_cast<uint32_t>(m_wxMaxima.m_configuration.RandomEntropy());
        for (size_t j = 0; j < sizeof(uint32_t); j++) {
          membuf.AppendByte(static_cast<char>(
            static_cast<unsigned char>(rdVal & 0xFFu) ^
            static_cast<unsigned char>(byteUD(m_wxMaxima.m_configuration.RandomEngine()))));
          rdVal >>= 8;
        }
      }
      m_wxMaxima.m_maximaAuthString = wxBase64Encode(membuf);
      environment["MAXIMA_AUTH_CODE"] = m_wxMaxima.m_maximaAuthString;

      env->env = std::move(environment);
      Configuration::g_stats.maximaProcessesSpawned++;
      m_wxMaxima.m_pid = wxExecute(
          command,
          wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER,
          m_wxMaxima.m_maximaProcess, env.get());
      if (m_wxMaxima.m_pid <= 0) {
        m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::process_wont_start);
        m_wxMaxima.StatusText(_("Cannot start the maxima binary"));
        m_wxMaxima.m_maximaProcess = NULL;
        m_wxMaxima.m_maximaStdout = NULL;
        m_wxMaxima.m_maximaStderr = NULL;
        m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::offline);
        // Deferred: StartMaxima can run under a wxProcess-termination event
        // (restart after a crash), and a modal dialog inside an event handler
        // pumps the event queue reentrantly. The stack-allocated dialog also
        // fixes a leak (the old new'd dialog was never destroyed).
        m_wxMaxima.CallAfter([this]{
          MaximaNotStartingDialog dlg(&m_wxMaxima, -1, &m_wxMaxima.m_configuration,
                                      _("Failed to start Maxima"));
          dlg.ShowModal();
        });
        return false;
      }
      // Track this Maxima so a termination signal / crash can kill it even if
      // our destructor never runs (see the registry above the class methods).
      MaximaProcessManager::RegisterChildMaxima(m_wxMaxima.m_pid);
      m_wxMaxima.m_maximaStdout = m_wxMaxima.m_maximaProcess->GetInputStream();
      m_wxMaxima.m_maximaStderr = m_wxMaxima.m_maximaProcess->GetErrorStream();
      m_wxMaxima.m_lastPrompt = wxS("(%i1) ");
      m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::wait_for_start);
    } else {
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::offline);
      wxLogMessage(_("Cannot find a maxima binary and no binary chosen in the "
                     "config dialogue."));
      return false;
    }
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->GetErrorList().Clear();

    // Initialize the performance counter.
    m_wxMaxima.GetMaximaCPUPercentage();
  }
  return true;
}

void MaximaProcessManager::KillMaxima(bool logMessage) {
  if (logMessage && (m_wxMaxima.m_closing || (m_wxMaxima.m_maximaProcess == NULL) || (m_wxMaxima.m_pid > 0))) {
    if (m_wxMaxima.m_maximaPid > 0)
      wxLogMessage("Killing Maxima. Wrapper PID=%ld, Maxima PID=%ld", m_wxMaxima.m_pid,
                   m_wxMaxima.m_maximaPid);
    else
      wxLogMessage("Killing Maxima. PID=%ld", m_wxMaxima.m_pid);
    if (m_wxMaxima.m_history)
      m_wxMaxima.m_history->MaximaSessionStart();
  }
  m_wxMaxima.m_discardAllData = true;
  m_wxMaxima.m_closing = true;
  if(m_wxMaxima.GetWorksheet() && (m_wxMaxima.m_variablesPane))
    {
      m_wxMaxima.m_variablesPane->ResetValues();
      m_wxMaxima.m_varNamesToQuery = m_wxMaxima.m_variablesPane->GetEscapedVarnames();
    }
  m_wxMaxima.m_configCommands.Clear();
  // The new maxima process will be in its initial condition => mark it as such.
  m_wxMaxima.m_hasEvaluatedCells = false;
  m_wxMaxima.m_maximaError = false;

  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetWorkingGroup(nullptr);
      m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
    }
  m_wxMaxima.EvaluationQueueLength(0);

  // We start checking for Maximas output again as soon as we send some data to
  // the program.
  m_wxMaxima.m_statusBar->SetMaximaCPUPercentage(0);
  m_wxMaxima.m_CWD.Clear();
  m_wxMaxima.QuestionAnswered();

  if(m_wxMaxima.m_maximaProcess) {
    // If Maxima no more has a stdout it should automatically close
    m_wxMaxima.m_maximaProcess->CloseOutput();
  }
  m_wxMaxima.m_maximaStdout = NULL;
  m_wxMaxima.m_maximaStderr = NULL;
  // This closes Maxima's network connection.
  m_wxMaxima.m_client.reset();

  // Finally found a long outstanding problem with leftover Lisp processes
  // (using debugging with command line Maxima and netcat):
  //
  // On Windows just closing the network (kill the netcat server) does NOT
  // terminate the Lisp, which is running in the background.
  // Don't know why. But do not try to do that, just kill the Maxima process (using "taskkill")!
#ifndef __WINDOWS__
  // If the process really exists after that, kill it with Signals.
  wxKillError killresult;
  if (wxProcess::Exists(m_wxMaxima.m_pid)) {
    killresult = wxProcess::Kill(m_wxMaxima.m_pid, wxSIGTERM, wxKILL_CHILDREN);
    wxLogMessage("Kill Maxima (SIGTERM). killresult=%d  [0=wxKILL_OK]", killresult);
  }
  if (wxProcess::Exists(m_wxMaxima.m_pid)) {
    killresult = wxProcess::Kill(m_wxMaxima.m_pid, wxSIGKILL, wxKILL_CHILDREN);
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
  // wxEXEC_NOEVENTS: wait for taskkill *without* dispatching events. The plain
  // wxEXEC_SYNC default spins a nested event loop here, which re-enters
  // ProcessPendingEvents() on the Maxima wxEvtHandler we are in the middle of
  // tearing down (m_wxMaxima.m_client was just reset above) -- tripping
  // "should have pending events if called" (event.cpp) and, with asserts
  // enabled, aborting; in release it could wedge. We do not need the event loop
  // while killing Maxima (the wxMilliSleep wait loop just below is already a
  // blocking, event-free wait), so suppress event dispatch for the kill.
  wxExecute(wxString::Format("taskkill /PID %d /F /T", m_wxMaxima.m_pid), taskkill_out, taskkill_err, wxEXEC_SYNC | wxEXEC_NOEVENTS);
  for (size_t i=0; i<taskkill_out.GetCount(); ++i)
    wxLogMessage("taskkill_out: %s", taskkill_out.Item(i));
  for (size_t i=0; i<taskkill_err.GetCount(); ++i)
    wxLogMessage("taskkill_err: %s", taskkill_err.Item(i));
#endif
  // Wait for Maxima to actually exit before we clean up its temporary files
  int count = 40;
  while (wxProcess::Exists(m_wxMaxima.m_pid) && (count > 0)) {
    wxMilliSleep(50);
    count--;
  }

  // As we might have killed maxima before it was able to clean up its
  // temp files we try to do so manually now:
  if (m_wxMaxima.m_maximaTempDir != wxEmptyString) {
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/maxout") +
                     wxString::Format("%li.gnuplot", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/maxout") +
                   wxString::Format("%li.gnuplot", m_wxMaxima.m_pid));
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/data") +
                     wxString::Format("%li.gnuplot", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/data") +
                   wxString::Format("%li.gnuplot", m_wxMaxima.m_pid));
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/maxout") +
                     wxString::Format("%li.xmaxima", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/maxout") +
                   wxString::Format("%li.xmaxima", m_wxMaxima.m_pid));
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/maxout_") +
                     wxString::Format("%li.gnuplot", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/maxout_") +
                   wxString::Format("%li.gnuplot", m_wxMaxima.m_pid));
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/data_") +
                     wxString::Format("%li.gnuplot", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/data_") +
                   wxString::Format("%li.gnuplot", m_wxMaxima.m_pid));
    if (wxFileExists(m_wxMaxima.m_maximaTempDir + wxS("/maxout_") +
                     wxString::Format("%li.xmaxima", m_wxMaxima.m_pid)))
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/maxout_") +
                   wxString::Format("%li.xmaxima", m_wxMaxima.m_pid));

    // Remove leftover png images from with_slider_draw, etc. (issue #2081).
    wxDir dir(m_wxMaxima.m_maximaTempDir);
    wxString file;
    bool cont = dir.GetFirst(&file, wxString::Format("maxout_%li_*.png", m_wxMaxima.m_pid), wxDIR_FILES);
    while (cont) {
      wxRemoveFile(m_wxMaxima.m_maximaTempDir + wxS("/") + file);
      cont = dir.GetNext(&file);
    }
  }
  // Set m_wxMaxima.m_pid to -1.The process really shouldn't exist any more.
  MaximaProcessManager::UnregisterChildMaxima(m_wxMaxima.m_pid);
  m_wxMaxima.m_pid = -1;
  m_wxMaxima.m_maximaPid = -1;

  // We don't need to be informed any more if the maxima process we just tried to
  // kill actually exits.
  if(m_wxMaxima.m_maximaProcess) {
    m_wxMaxima.m_maximaProcess->Detach();
    m_wxMaxima.m_maximaProcess = NULL;
  }
}

void MaximaProcessManager::OnMaximaClose(){
  if (wxProcess::Exists(m_wxMaxima.m_pid)) KillMaxima();
  m_wxMaxima.m_maximaProcess = NULL;
  MaximaProcessManager::UnregisterChildMaxima(m_wxMaxima.m_pid);
  m_wxMaxima.m_pid = -1;
  if (m_wxMaxima.m_maximaStdout) {
    wxTextInputStream istrm(*m_wxMaxima.m_maximaStdout, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_wxMaxima.m_maximaStdout->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();
    if (!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stdout: %s"), o);
  }
  if (m_wxMaxima.m_maximaStderr) {
    wxTextInputStream istrm(*m_wxMaxima.m_maximaStderr, wxS('\t'),
                            wxConvAuto(wxFONTENCODING_UTF8));
    wxString o;
    wxChar ch;
    while (((ch = istrm.GetChar()) != wxS('\0')) && (m_wxMaxima.m_maximaStderr->CanRead()))
      o += ch;

    wxString o_trimmed = o;
    o_trimmed.Trim();
    if (!o.IsEmpty())
      wxLogMessage(_("Last message from maxima's stderr: %s"), o);
  }
  m_wxMaxima.m_maximaStdout = NULL;
  m_wxMaxima.m_maximaStderr = NULL;
  m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::offline);
  if (!m_wxMaxima.m_closing) {
    m_wxMaxima.StatusText(_("Maxima process terminated unexpectedly."));

    if (m_wxMaxima.m_first) {
      // Deferred: we are inside a wxProcess-termination event handler; a
      // modal box here would pump the event queue reentrantly and would also
      // hold up the restart attempt below until the user dismisses it.
      m_wxMaxima.CallAfter([]{
        LoggingMessageBox(
                          _("Can not start Maxima. The most probable cause is that Maxima "
                            "isn't installed (it can be downloaded from "
                            "https://maxima.sourceforge.io) or in wxMaxima's config dialogue "
                            "the setting for Maxima's location is wrong."),
                          _("Error"), wxOK | wxICON_ERROR);
      });
    }

    // Let's see if maxima has told us why this did happen.
    m_wxMaxima.m_responseReader.ReadStdErr();
    m_wxMaxima.ConsoleAppend(wxS("\nMaxima exited...\n"), MC_TYPE_ERROR);

    if (m_wxMaxima.m_unsuccessfulConnectionAttempts > 10)
      m_wxMaxima.ConsoleAppend(wxS("Restart Maxima with 'Maxima->Restart Maxima'.\n"),
                    MC_TYPE_ERROR);
    else {
      m_wxMaxima.ConsoleAppend(wxS("Trying to restart Maxima.\n"), MC_TYPE_ERROR);
      // Perhaps we shouldn't restart maxima again if it outputs a prompt and
      // crashes immediately after => Each prompt is deemed as but one hint
      // for a working maxima while each crash counts twice.
      m_wxMaxima.m_unsuccessfulConnectionAttempts += 2;
      StartMaxima(true);
    }
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->GetEvaluationQueue().Clear();
  }
  // We did close Maxima on purpose (m_wxMaxima.m_closing==true) - do not restart it.
  //else
  //  StartMaxima(true);

  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::disconnected);
  m_wxMaxima.UpdateToolBar();
  m_wxMaxima.UpdateMenus();
}

void MaximaProcessManager::OnMaximaClose(wxProcessEvent &event) {
  if(event.GetPid() != m_wxMaxima.m_pid)
    return;
  OnMaximaClose();
}


void MaximaProcessManager::MaximaEvent(wxThreadEvent &event) {
  using std::swap;
  // Reentrancy tripwire: see m_inMaximaEvent's documentation. If this assert
  // fires, some code reachable from a Read* handler below pumped the event
  // queue (typically a modal dialog) - defer that dialog with CallAfter
  // instead.
  wxASSERT_MSG(!m_inMaximaEvent,
               wxS("MaximaEvent() re-entered: a nested event loop ran inside "
                   "a Maxima data handler"));
  struct FlagHolder {
    explicit FlagHolder(bool &flag) : m_flag(flag) { m_flag = true; }
    ~FlagHolder() { m_flag = false; }
    bool &m_flag;
  } inEventFlag(m_inMaximaEvent);
  // Don't interpret data from a Maxima process we don't trust: either its
  // authentication against MAXIMA_AUTH_CODE failed (so whatever connected to
  // our socket might not be the Maxima we started), or we are in the process
  // of killing it. This gate is what makes m_discardAllData actually discard;
  // it got lost when the chunk interpreter moved to the worker-thread
  // architecture. Lifecycle events stay live so shutdown still works.
  if (m_wxMaxima.m_discardAllData) {
    switch (event.GetInt()) {
    case Maxima::WRITE_ERROR:
    case Maxima::DISCONNECTED:
      break;
    default:
      return;
    }
  }
  switch (event.GetInt()) {
  case Maxima::READ_MISC_TEXT:
    // Read out stderr: We will do that in the background on a regular basis,
    // anyway. But if we do it manually now, too, the probability that things
    // are presented to the user in chronological order increases a bit.
    m_wxMaxima.m_responseReader.ReadStdErr();
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
    if(m_wxMaxima.m_first)
      m_wxMaxima.m_responseReader.ReadFirstPrompt(event.GetString());
    else
      m_wxMaxima.m_responseReader.ReadMiscText(event.GetString());
    break;
  case Maxima::STRING_FOR_XMLINSPECTOR:
    if(m_wxMaxima.m_xmlInspector)
      m_wxMaxima.m_xmlInspector->Add_FromMaxima(event.GetString());
    if (Maxima::GetPipeToStdErr())
      {
        std::cerr << event.GetString();
        std::cerr.flush();
      }
    break;
  case Maxima::XML_PROMPT:
    m_wxMaxima.m_responseReader.ReadStdErr();
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
    m_wxMaxima.m_responseReader.ReadPrompt(event.GetString());
    break;
  case Maxima::XML_SUPPRESSOUTPUT:
    m_wxMaxima.m_responseReader.ReadStdErr();
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
    m_wxMaxima.m_responseReader.ReadSuppressedOutput(event.GetString());
    break;
  case Maxima::XML_WXXMLSYMBOLS:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadLoadSymbols(xmldoc);
    }
    break;
  case Maxima::XML_VARIABLES:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadVariables(xmldoc);
    }
    break;
  case Maxima::XML_WATCH_VARIABLES_ADD:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadAddVariables(xmldoc);
    }
    break;
  case Maxima::XML_STATUSBAR:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadStatusBar(xmldoc);
    }
    break;
  case Maxima::XML_HTML_MANUAL_KEYWORDS:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadManualTopicNames(xmldoc);
    }
    break;
  case Maxima::XML_MATHS:
    {
      wxXmlDocument xmldoc;
      wxStringInputStream xmlStream(event.GetString());
      xmldoc.Load(xmlStream);
      m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
      m_wxMaxima.m_responseReader.ReadMath(xmldoc);
    }
    break;
  case Maxima::XML_TOOLONGMATHS:
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
    m_wxMaxima.DoRawConsoleAppend(_("(Config tells to suppress the output of long cells)"),
                       MC_TYPE_WARNING);
    break;
  case Maxima::XML_WXXML_KEY: // TODO: Should the key be outside the SuppressOutput?
    break;
  case Maxima::READ_PENDING:
    m_wxMaxima.m_responseReader.ReadStdErr();
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::receive);
    break;
  case Maxima::WRITE_PENDING:
    m_wxMaxima.m_statusBar->NetworkStatus(StatusBar::transmit);
    break;
  case Maxima::WRITE_ERROR:
    m_wxMaxima.DoRawConsoleAppend(_("Error writing to Maxima"), MC_TYPE_ERROR);
    break;
  case Maxima::DISCONNECTED: {
    wxLogMessage(_("Connection to Maxima lost."));
    //  KillMaxima();
    break;
  }
  }
}

void MaximaProcessManager::Interrupt(wxCommandEvent &WXUNUSED(event)) {
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.GetWorksheet()->CloseAutoCompletePopup();

  if (m_wxMaxima.m_pid < 0) {
    m_wxMaxima.m_MenuBar->EnableItem(EventIDs::menu_interrupt_id, false);
    return;
  }

#if defined(__WXMSW__)
  if (m_wxMaxima.m_pid > 0) {
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
    long signalPid = (m_wxMaxima.m_maximaPid > 0) ? m_wxMaxima.m_maximaPid : m_wxMaxima.m_pid;
    wxWCharBuffer sharedMemoryName(wxString::Format("gcl-%d", signalPid).wc_str());
    sharedMemoryHandle =
      OpenFileMapping(FILE_MAP_WRITE,    /*  Read/write permission.   */
                      FALSE,             /*  Do not inherit the name  */
                      sharedMemoryName.data()); /*  of the mapping object.   */

    /* If gcl is not running, send to maxima. */
    wxWCharBuffer sharedMemoryName2(wxString::Format("maxima-%d", signalPid).wc_str());
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

        m_wxMaxima.StatusText(errorMessage);
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
      return;
    }
  }

  if (m_wxMaxima.m_maximaProcess) {
    // We need to send the CTRL_BREAK_EVENT to the process group, not
    // to the lisp.
    auto pid = m_wxMaxima.m_maximaProcess->GetPid();
    if (!GenerateConsoleCtrlEvent(CTRL_C_EVENT, pid)) {
      wxLogMessage(_("Could not send an interrupt signal to maxima."));
      return;
    }
  }
#else
  wxLogMessage(_("Sending Maxima a SIGINT signal."));
  wxProcess::Kill(m_wxMaxima.m_pid, wxSIGINT);
#endif
  if (m_wxMaxima.m_maximaProcess) {
    wxProcess::Kill(m_wxMaxima.m_maximaProcess->GetPid(), wxSIGINT);
  }
}


void MaximaProcessManager::OnGnuplotQueryTerminals(wxProcessEvent &event) {
  if (!m_wxMaxima.m_gnuplotTerminalQueryProcess)
    return;
  wxString gnuplotMessage;
  {
    wxInputStream *istream = m_wxMaxima.m_gnuplotTerminalQueryProcess->GetInputStream();
    wxTextInputStream textin(*istream);
    while (!istream->Eof())
      gnuplotMessage += textin.ReadLine() + "\n";
  }
  {
    wxInputStream *istream = m_wxMaxima.m_gnuplotTerminalQueryProcess->GetErrorStream();
    wxTextInputStream textin(*istream);
    while (!istream->Eof())
      gnuplotMessage += textin.ReadLine() + "\n";
  }
  gnuplotMessage.Trim(true);
  gnuplotMessage.Trim(false);
  wxLogMessage("Terminals supported by gnuplot: %s", gnuplotMessage);
  if (gnuplotMessage.Contains(wxS("pngcairo"))) {
    wxLogMessage(_("Using gnuplot's pngcairo driver for embedded plots"));
    if (!m_wxMaxima.m_configuration.UsePngCairo())
      m_wxMaxima.m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo t)\n");
    m_wxMaxima.m_configuration.UsePngCairo(true);
  } else {
    wxLogMessage(_("Using gnuplot's antialiassing-less png driver for embedded "
                   "plots as pngcairo could not be found"));
    if (m_wxMaxima.m_configuration.UsePngCairo())
      m_wxMaxima.m_configCommands += wxS(":lisp-quiet (setq $wxplot_pngcairo nil)\n");
    m_wxMaxima.m_configuration.UsePngCairo(false);
  }
  m_wxMaxima.m_gnuplotTerminalQueryProcess->CloseOutput();
  m_wxMaxima.m_gnuplotTerminalQueryProcess = NULL;
  event.Skip();
}

void MaximaProcessManager::OnGnuplotClose(wxProcessEvent &event) {
  m_wxMaxima.m_gnuplotProcess = NULL;
  wxLogMessage(_("Gnuplot has closed."));
  event.Skip();
}

void MaximaProcessManager::GnuplotCommandName(wxString gnuplot) {
  m_wxMaxima.m_gnuplotcommand = gnuplot;
  if (!wxFileName(m_wxMaxima.m_gnuplotcommand).IsAbsolute()) {
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
    // One must specify the path to Maxima in that case, so use the m_wxMaxima.m_maximaUserLocation.
    wxString maximapath = wxFileName(m_wxMaxima.m_configuration.MaximaUserLocation()).GetPath();
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
    if(m_wxMaxima.m_configuration.UseWGnuplot())
      {
        wxLogMessage(_("Instructed to prefer wgnuplot over gnuplot"));
        long pos = gnuplot.rfind(wxS("gnuplot"));
        // if pos is not wxNOT_FOUND it is 6 or higher.
        if(pos != wxNOT_FOUND)
          {
            wgnuplot = gnuplot.Left(pos) + wxS("w") + gnuplot.Right(gnuplot.Length() - pos);
            if (!m_wxMaxima.m_gnuplotcommand.IsEmpty())
              m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wgnuplot);
          }
      }
    else
      {
        wxLogMessage(_("Instructed to prefer gnuplot over wgnuplot"));
      }
    if (m_wxMaxima.m_gnuplotcommand.IsEmpty())
      m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot);
    m_wxMaxima.m_gnuplotcommand_commandline = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));

    if(m_wxMaxima.m_configuration.UseWGnuplot())
      {
        // If not successful, Find executable "wgnuplot.exe" in our list of paths
        if (m_wxMaxima.m_gnuplotcommand.IsEmpty()) {
          m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("wgnuplot.exe"));
          m_wxMaxima.m_gnuplotcommand_commandline = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));
        }
      }
    // If not successful, Find executable "gnuplot.exe" in our list of paths
    if (m_wxMaxima.m_gnuplotcommand.IsEmpty())
      m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("gnuplot.exe"));
    // If not successful, Find executable "gnuplot.bat" in our list of paths
    if (m_wxMaxima.m_gnuplotcommand.IsEmpty())
      m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(wxS("gnuplot.bat"));
#endif
#ifdef __WXOSX__
    if (m_wxMaxima.m_gnuplotcommand.IsEmpty())
      m_wxMaxima.m_gnuplotcommand = pathlist.FindAbsoluteValidPath(gnuplot + wxS(".app"));
#endif
    // If not successful, use the original command (better than empty for error
    // messages)
    if (m_wxMaxima.m_gnuplotcommand.IsEmpty()) {
#ifdef __WXMSW__
      if(m_wxMaxima.m_configuration.UseWGnuplot())
        {
          m_wxMaxima.m_gnuplotcommand = wgnuplot;
          wxLogMessage(_("Gnuplot not found, using the default: %s"), wgnuplot);
        }
      else
        {
          m_wxMaxima.m_gnuplotcommand = gnuplot;
          wxLogMessage(_("Gnuplot not found, using the default: %s"), gnuplot);
        }
#endif
    } else {
      wxLogMessage(_("Gnuplot found at: %s"), m_wxMaxima.m_gnuplotcommand);
#ifdef __WINDOWS__
      wxLogMessage(_("Gnuplot (command line) found at: %s"), m_wxMaxima.m_gnuplotcommand_commandline);
#endif
    }
  }
  if (m_wxMaxima.m_gnuplotcommand.Contains(" ") && (!m_wxMaxima.m_gnuplotcommand.StartsWith("\"")) &&
      (!m_wxMaxima.m_gnuplotcommand.StartsWith("\'")))
    m_wxMaxima.m_gnuplotcommand = "\"" + m_wxMaxima.m_gnuplotcommand + "\"";
}

