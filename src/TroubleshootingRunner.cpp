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
  Implements TroubleshootingRunner.
*/

#include "TroubleshootingRunner.h"
#include "MaximaAuthLine.h"
#include <wx/base64.h>
#include <wx/file.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/stream.h>
#include <wx/time.h>
#include <array>
#include <algorithm>
#include <random>
#include <vector>

using wxm::TroubleshootingCheck;
using wxm::TroubleshootingStatus;

namespace {
//! How often the timer polls, in milliseconds
constexpr int POLL_MS = 50;
//! How much of a process' console output is kept
constexpr size_t MAX_CONSOLE = 64 * 1024;
//! How much of Maxima's output a result quotes
constexpr size_t TAIL_LENGTH = 1500;

//! The checks that depend on talking to a working Maxima, in the order they are run
const TroubleshootingCheck g_maximaChain[] = {
  TroubleshootingCheck::maximaFound, TroubleshootingCheck::server,
  TroubleshootingCheck::maximaStarts, TroubleshootingCheck::maximaConnects,
  TroubleshootingCheck::firstPrompt, TroubleshootingCheck::authentication,
  TroubleshootingCheck::wxMathML, TroubleshootingCheck::loadPackage,
  TroubleshootingCheck::loadDraw, TroubleshootingCheck::tempDir,
  TroubleshootingCheck::maximaHtmlManual, TroubleshootingCheck::maximaInfoManual};

//! A question to Maxima: prints the value of a Lisp expression as a wxprobe- line
wxString Query(const wxString &key, const wxString &lispExpression) {
  return wxS("(format t \"~%wxprobe-") + key + wxS("=~a~%\" ") + lispExpression + wxS(")");
}

//! A Lisp expression for a variable's value, or "" if this Maxima has no such variable
wxString IfBound(const wxString &symbol) {
  return wxS("(if (boundp '") + symbol + wxS(") ") + symbol + wxS(" \"\")");
}

//! A question whether Maxima can load a package
wxString LoadQuery(const wxString &key, const wxString &package) {
  // errcatch() catches what load() reports through merror(); ignore-errors
  // what goes wrong below that. errcatch returns [] on an error.
  return Query(key,
               wxS("(let ((r (ignore-errors (meval '(($errcatch) (($load) \"") +
               package + wxS("\")))))) (if (and (consp r) (cdr r)) \"ok\" \"failed\"))"));
}

//! Appends what a stream has to offer to \p text, keeping only its end
void Drain(wxInputStream *stream, wxString &text) {
  if (!stream)
    return;
  std::vector<char> buffer(4096);
  while (stream->CanRead()) {
    stream->Read(buffer.data(), buffer.size());
    const size_t count = stream->LastRead();
    if (count == 0)
      break;
    text += wxString::From8BitData(buffer.data(), count);
  }
  if (text.Length() > MAX_CONSOLE)
    text = text.Right(MAX_CONSOLE);
}

//! A strong random secret, as MaximaProcessManager makes one for its Maxima
wxString MakeSecret() {
  std::random_device entropy;
  std::mt19937 prng(entropy());
  std::uniform_int_distribution<unsigned int> byte(0, 255);
  wxMemoryBuffer buffer;
  for (int i = 0; i < 64; i++)
    buffer.AppendByte(static_cast<char>((entropy() ^ byte(prng)) & 0xFFu));
  return wxBase64Encode(buffer);
}

//! Looks for a program in the PATH, as MaximaProcessManager::GnuplotCommandName() does
wxString FindInPath(const wxString &program) {
  if (program.IsEmpty())
    return wxEmptyString;
  if (wxFileName(program).IsAbsolute())
    return wxFileExists(program) ? program : wxString();
  wxPathList pathlist;
  pathlist.AddEnvList(wxS("PATH"));
  wxString path = pathlist.FindAbsoluteValidPath(program);
#ifdef __WXMSW__
  if (path.IsEmpty())
    path = pathlist.FindAbsoluteValidPath(program + wxS(".exe"));
#endif
  return path;
}

//! Where in Maxima's HTML directory its manual is; empty if it isn't there
wxString FindMaximaHtmlManual(const wxString &htmlDir) {
  if (htmlDir.IsEmpty())
    return wxEmptyString;
  // The same places MaximaManual::FindMaximaHtmlDir() looks in.
  wxPathList dirs;
  dirs.Add(htmlDir);
  dirs.Add(htmlDir + wxS("/info"));
  dirs.Add(htmlDir + wxS("/info/html"));
  dirs.Add(htmlDir + wxS("/html"));
  dirs.Add(htmlDir + wxS("/../html"));
  wxString file = dirs.FindAbsoluteValidPath(wxS("maxima_singlepage.html"));
  if (file.IsEmpty())
    file = dirs.FindAbsoluteValidPath(wxS("maxima_toc.html"));
  return file;
}
} // namespace

//! A wxProcess that tells its runner when it ended, and outlives it if need be
class TroubleshootingRunner::Process : public wxProcess
{
public:
  explicit Process(TroubleshootingRunner *runner)
    : wxProcess(wxPROCESS_REDIRECT), m_runner(runner) {}
  //! The runner goes away while the process still runs: delete ourselves when it ends
  void Orphan() {
    m_runner = nullptr;
    Detach();
  }
  void OnTerminate(int WXUNUSED(pid), int status) override {
    if (m_runner)
      m_runner->ProcessEnded(this, status);
    else
      delete this;
  }

private:
  TroubleshootingRunner *m_runner;
};

const std::set<TroubleshootingCheck> &TroubleshootingRunner::Checks() {
  static const std::set<TroubleshootingCheck> checks = [] {
    std::set<TroubleshootingCheck> all(std::begin(g_maximaChain), std::end(g_maximaChain));
    all.insert(TroubleshootingCheck::maximaStops);
    all.insert(TroubleshootingCheck::gnuplotFound);
    all.insert(TroubleshootingCheck::gnuplotPng);
    all.insert(TroubleshootingCheck::gnuplotPngCairo);
    return all;
  }();
  return checks;
}

TroubleshootingRunner::TroubleshootingRunner(const Settings &settings,
                                             ResultCallback onResult,
                                             std::function<void ()> onFinished)
  : m_settings(settings), m_onResult(std::move(onResult)),
    m_onFinished(std::move(onFinished)), m_timer(this)
{
  Bind(wxEVT_TIMER, [this](wxTimerEvent &) { Poll(); });
}

TroubleshootingRunner::~TroubleshootingRunner() {
  m_timer.Stop();
  if (m_maxima) {
    if (!m_maximaEnded) {
      StopMaxima();
      m_maxima->Orphan();
    } else
      delete m_maxima;
  }
  if (m_gnuplot) {
    if (!m_gnuplotEnded) {
      wxProcess::Kill(static_cast<int>(m_gnuplot->GetPid()), wxSIGKILL, wxKILL_CHILDREN);
      m_gnuplot->Orphan();
    } else
      delete m_gnuplot;
  }
  if (m_socket)
    m_socket.release()->Destroy();
  if (m_server)
    m_server.release()->Destroy();
  if (!m_gnuplotScript.IsEmpty())
    wxRemoveFile(m_gnuplotScript);
  if (!m_gnuplotOutput.IsEmpty())
    wxRemoveFile(m_gnuplotOutput);
}

void TroubleshootingRunner::Start() {
  StartMaxima();
  if (!IsFinished())
    m_timer.Start(POLL_MS);
}

void TroubleshootingRunner::EnterStage(Stage stage) {
  m_stage = stage;
  m_stageStart = wxGetLocalTimeMillis();
  if (stage == Stage::finished) {
    m_timer.Stop();
    if (m_onFinished)
      m_onFinished();
  }
}

long TroubleshootingRunner::StageAge() const {
  return (wxGetLocalTimeMillis() - m_stageStart).ToLong();
}

void TroubleshootingRunner::Report(TroubleshootingCheck check, TroubleshootingStatus status,
                                   const wxString &details, const wxString &hint) {
  if (m_reported.count(check))
    return;
  m_reported.insert(check);
  wxm::TroubleshootingResult result;
  result.check = check;
  result.status = status;
  result.details = details;
  result.hint = hint;
  wxLogMessage(wxS("Troubleshooting: %s: %s %s"), wxm::TroubleshootingCheckTitle(check),
               wxm::TroubleshootingStatusName(status), details);
  if (m_onResult)
    m_onResult(result);
}

void TroubleshootingRunner::SkipRemainingMaximaChecks(const wxString &reason) {
  for (const auto check : g_maximaChain)
    Report(check, TroubleshootingStatus::skipped, reason);
}

wxString TroubleshootingRunner::MaximaOutputTail() const {
  wxString tail = m_fromMaxima;
  // The secret is nobody's business, not even in a bug report.
  if (!m_secret.IsEmpty())
    tail.Replace(m_secret, wxS("<secret>"));
  if (tail.Length() > TAIL_LENGTH)
    tail = wxS("...") + tail.Right(TAIL_LENGTH);
  return tail;
}

void TroubleshootingRunner::SendToMaxima(const wxString &text) {
  const wxScopedCharBuffer utf8 = text.utf8_str();
  m_toMaxima.append(utf8.data(), utf8.length());
}

void TroubleshootingRunner::StartMaxima() {
  const wxString exe = m_settings.maximaExecutable;
  if (exe.IsEmpty() || !wxFileExists(exe)) {
    Report(TroubleshootingCheck::maximaFound, TroubleshootingStatus::failed,
           exe.IsEmpty() ?
           wxString::Format(_("wxMaxima can't find the Maxima program \"%s\"."),
                            m_settings.maximaConfigured) :
           wxString::Format(_("The Maxima program %s doesn't exist."), exe),
           _("Install Maxima (https://maxima.sourceforge.io), or tell wxMaxima where "
             "it is in Edit > Configure > Maxima."));
    SkipRemainingMaximaChecks(_("wxMaxima can't find Maxima."));
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::skipped,
           _("Maxima wasn't started."));
    StartGnuplot(wxS("png"));
    return;
  }
  Report(TroubleshootingCheck::maximaFound, TroubleshootingStatus::ok, exe);

  // Port 0: whichever port is free. The worksheet's Maxima keeps its own.
  wxIPV4address addr;
  addr.LocalHost();
  addr.Service(0);
  m_server = std::make_unique<wxSocketServer>(addr, wxSOCKET_NOWAIT);
  wxIPV4address local;
  if (!m_server->IsOk() || !m_server->GetLocal(local) || (local.Service() == 0)) {
    m_server.release()->Destroy();
    Report(TroubleshootingCheck::server, TroubleshootingStatus::failed,
           _("wxMaxima couldn't open a server on 127.0.0.1."),
           _("Maxima insists on wxMaxima acting as a server it connects to. A "
             "firewall or other security software may forbid that, even for a "
             "server that only accepts connections from programs on this "
             "computer. Check that network support is enabled, too."));
    SkipRemainingMaximaChecks(_("wxMaxima can't offer Maxima a connection."));
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::skipped,
           _("Maxima wasn't started."));
    StartGnuplot(wxS("png"));
    return;
  }
  m_server->Notify(false);
  m_port = local.Service();
  Report(TroubleshootingCheck::server, TroubleshootingStatus::ok,
         wxString::Format(_("Listening on 127.0.0.1, port %i."), m_port));

  const wxString command = m_settings.maximaCommand + wxString::Format(wxS(" -s %i"), m_port);
  m_secret = MakeSecret();
  wxExecuteEnv env;
  env.env = m_settings.environment;
  env.env[wxS("MAXIMA_AUTH_CODE")] = m_secret;
  m_maxima = new Process(this);
  m_maximaPid = wxExecute(command, wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER,
                          m_maxima, &env);
  if (m_maximaPid <= 0) {
    // wxExecute() deletes the process object itself if it can't start one.
    m_maxima = nullptr;
    Report(TroubleshootingCheck::maximaStarts, TroubleshootingStatus::failed,
           wxString::Format(_("Running %s failed."), command),
           _("The program may not be executable, or security software may have "
             "kept it from running. Try running the command above in a terminal."));
    SkipRemainingMaximaChecks(_("Maxima didn't start."));
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::skipped,
           _("Maxima wasn't started."));
    StartGnuplot(wxS("png"));
    return;
  }
  EnterStage(Stage::waitConnect);
}

void TroubleshootingRunner::ProcessEnded(Process *process, int status) {
  if (process == m_maxima) {
    Drain(m_maxima->GetInputStream(), m_maximaConsole);
    Drain(m_maxima->GetErrorStream(), m_maximaConsole);
    m_maximaEnded = true;
    m_maximaExitCode = status;
  } else if (process == m_gnuplot) {
    Drain(m_gnuplot->GetInputStream(), m_gnuplotErrors);
    Drain(m_gnuplot->GetErrorStream(), m_gnuplotErrors);
    m_gnuplotEnded = true;
    m_gnuplotExitCode = status;
  }
}

void TroubleshootingRunner::PumpIO() {
  if (m_maxima && !m_maximaEnded) {
    // An undrained pipe fills up, and a Maxima that can't write any more stops.
    Drain(m_maxima->GetInputStream(), m_maximaConsole);
    Drain(m_maxima->GetErrorStream(), m_maximaConsole);
  }
  if (m_gnuplot && !m_gnuplotEnded) {
    Drain(m_gnuplot->GetInputStream(), m_gnuplotErrors);
    Drain(m_gnuplot->GetErrorStream(), m_gnuplotErrors);
  }
  if (!m_socket)
    return;
  // On the heap: a buffer this size is too much stack for some platforms.
  std::vector<char> buffer(16384);
  for (;;) {
    m_socket->Read(buffer.data(), buffer.size());
    const size_t count = m_socket->LastReadCount();
    if (count == 0)
      break;
    m_undecoded.append(buffer.data(), count);
    // Decode only up to the last complete UTF-8 sequence.
    size_t end = m_undecoded.size();
    size_t lead = end;
    while ((lead > 0) && (end - lead < 4) &&
           ((static_cast<unsigned char>(m_undecoded[lead - 1]) & 0xC0) == 0x80))
      lead--;
    if (lead > 0) {
      const unsigned char c = static_cast<unsigned char>(m_undecoded[lead - 1]);
      const size_t length = (c >= 0xF0) ? 4 : (c >= 0xE0) ? 3 : (c >= 0xC0) ? 2 : 1;
      if (end - (lead - 1) < length)
        end = lead - 1;
    }
    wxString text = wxString::FromUTF8(m_undecoded.data(), end);
    if (text.IsEmpty() && (end > 0))
      text = wxString::From8BitData(m_undecoded.data(), end);
    m_fromMaxima += text;
    m_undecoded.erase(0, end);
  }
  while (!m_toMaxima.empty()) {
    m_socket->Write(m_toMaxima.data(), std::min<size_t>(m_toMaxima.size(), 65536));
    const size_t count = m_socket->LastWriteCount();
    if (count == 0)
      break;
    m_toMaxima.erase(0, count);
  }
}

void TroubleshootingRunner::Poll() {
  PumpIO();
  switch (m_stage) {
  case Stage::waitConnect:
    PollConnect();
    break;
  case Stage::waitPrompt:
    PollPrompt();
    break;
  case Stage::waitSetup:
    PollSetup();
    break;
  case Stage::waitAnswers:
    PollAnswers();
    break;
  case Stage::waitStop:
    PollStop();
    break;
  case Stage::gnuplotPng:
  case Stage::gnuplotPngCairo:
    PollGnuplot();
    break;
  case Stage::notStarted:
  case Stage::finished:
    break;
  }
}

void TroubleshootingRunner::PollConnect() {
  if (m_server) {
    wxSocketBase *socket = m_server->Accept(false);
    if (socket) {
      m_socket.reset(socket);
      m_socket->SetFlags(wxSOCKET_NOWAIT);
      m_socket->Notify(false);
      // Nobody else gets a chance to connect in its place.
      m_server.release()->Destroy();
      Report(TroubleshootingCheck::maximaStarts, TroubleshootingStatus::ok,
             wxString::Format(_("Started %s -s %i (process %li)."),
                              m_settings.maximaCommand, m_port, m_maximaPid));
      Report(TroubleshootingCheck::maximaConnects, TroubleshootingStatus::ok,
             wxString::Format(_("Maxima connected to port %i."), m_port));
      EnterStage(Stage::waitPrompt);
      return;
    }
  }
  if (m_maximaEnded) {
    wxString details = wxString::Format(
      _("Maxima ended before it connected, with exit code %i."), m_maximaExitCode);
    if (!m_maximaConsole.IsEmpty())
      details += wxS("\n") + _("It printed:") + wxS("\n") + m_maximaConsole.Right(TAIL_LENGTH);
    Report(TroubleshootingCheck::maximaStarts, TroubleshootingStatus::failed, details,
           wxString::Format(_("Running \"%s -s %i\" in a terminal shows what goes "
                              "wrong. Lisp errors here often mean a broken Maxima "
                              "installation or a broken maxima-init.mac."),
                            m_settings.maximaCommand, m_port));
    SkipRemainingMaximaChecks(_("Maxima didn't start."));
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::skipped,
           _("Maxima had already ended."));
    StartGnuplot(wxS("png"));
    return;
  }
  if (StageAge() > m_settings.connectTimeoutMs) {
    Report(TroubleshootingCheck::maximaStarts, TroubleshootingStatus::ok,
           wxString::Format(_("Started %s -s %i (process %li); it is still running."),
                            m_settings.maximaCommand, m_port, m_maximaPid));
    wxString details = wxString::Format(
      _("Maxima didn't connect to port %i within %li seconds."), m_port,
      m_settings.connectTimeoutMs / 1000);
    if (!m_maximaConsole.IsEmpty())
      details += wxS("\n") + _("It printed:") + wxS("\n") + m_maximaConsole.Right(TAIL_LENGTH);
#ifdef __WXOSX__
    const wxString hint =
      _("macOS may be holding Maxima back as a program downloaded from the "
        "internet: a program wxMaxima starts in the background can't answer the "
        "security question macOS would ask. Run Maxima once from a Terminal, or "
        "remove the quarantine flag with xattr -d com.apple.quarantine.");
#else
    const wxString hint =
      _("wxMaxima and Maxima talk over a connection inside this computer. Firewalls "
        "and antivirus software sometimes block that, too. The program that "
        "connects is the Lisp Maxima runs on (sbcl, gcl, ccl, lisp.exe or similar), "
        "not necessarily one named maxima.");
#endif
    Report(TroubleshootingCheck::maximaConnects, TroubleshootingStatus::failed, details, hint);
    SkipRemainingMaximaChecks(_("Maxima didn't connect."));
    StopMaxima();
  }
}

void TroubleshootingRunner::PollPrompt() {
  static const wxString firstPrompt = wxS("(%i1) ");
  if (m_fromMaxima.Find(firstPrompt) == wxNOT_FOUND) {
    if (m_maximaEnded || (StageAge() > m_settings.promptTimeoutMs)) {
      wxString details = m_maximaEnded ?
        wxString::Format(_("Maxima ended with exit code %i before its first prompt."),
                         m_maximaExitCode) :
        wxString::Format(_("No prompt within %li seconds."),
                         m_settings.promptTimeoutMs / 1000);
      details += wxS("\n") + _("Maxima sent:") + wxS("\n") + MaximaOutputTail();
      Report(TroubleshootingCheck::firstPrompt, TroubleshootingStatus::failed, details,
             _("A Maxima that connects but never prompts may be stuck in its "
               "maxima-init.mac or in a Lisp error. The text it sent above may "
               "tell why."));
      SkipRemainingMaximaChecks(_("Maxima didn't send its first prompt."));
      StopMaxima();
    }
    return;
  }
  m_gotPrompt = true;
  Report(TroubleshootingCheck::firstPrompt, TroubleshootingStatus::ok, wxEmptyString);

  std::string text(m_fromMaxima.utf8_str());
  const MaximaAuthLine authLine = TakeMaximaAuthLine(text, std::string(m_secret.utf8_str()));
  m_fromMaxima = wxString::FromUTF8(text);
  if (authLine == MaximaAuthLine::accepted) {
    m_authenticated = true;
    Report(TroubleshootingCheck::authentication, TroubleshootingStatus::ok,
           _("Maxima sent the secret as its first line."));
  } else if (authLine == MaximaAuthLine::rejected) {
    Report(TroubleshootingCheck::authentication, TroubleshootingStatus::failed,
           _("What connected sent a wrong secret."),
           _("Some other program on this computer may have connected in Maxima's "
             "place."));
    SkipRemainingMaximaChecks(_("Maxima didn't authenticate."));
    StopMaxima();
    return;
  }
  // Without the auth= line (a Maxima older than 2026-10), wxMathML.lisp
  // prints the secret as <wxxml-key>; PollSetup() looks for that.
  SendToMaxima(m_settings.setupCommand + wxS("\n"));
  SendToMaxima(wxS(":lisp-quiet (progn ") +
               Query(wxS("wxmathml"), wxS("(if (fboundp '") + m_settings.setupCheckFunction +
                     wxS(") \"yes\" \"no\")")) +
               wxS(" (finish-output))\n"));
  EnterStage(Stage::waitSetup);
}

void TroubleshootingRunner::PollSetup() {
  bool answered = false;
  const wxString loaded = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("wxmathml"), &answered);
  if (!answered && !m_maximaEnded && (StageAge() <= m_settings.setupTimeoutMs))
    return;

  if (!m_reported.count(TroubleshootingCheck::authentication)) {
    if (m_fromMaxima.Find(wxS("<wxxml-key>") + m_secret + wxS("</wxxml-key>")) != wxNOT_FOUND) {
      m_authenticated = true;
      Report(TroubleshootingCheck::authentication, TroubleshootingStatus::ok,
             _("wxMathML.lisp sent the secret."));
    } else {
      Report(TroubleshootingCheck::authentication, TroubleshootingStatus::failed,
             (m_fromMaxima.Find(wxS("</wxxml-key>")) != wxNOT_FOUND) ?
             _("What connected sent a wrong secret.") :
             _("Maxima didn't send the secret: it has no auth= line, and "
               "wxMathML.lisp didn't print its key."),
             _("Without proof that the Maxima wxMaxima talks to is the one it "
               "started, wxMaxima ignores everything it says."));
    }
  }

  if (answered && (loaded == wxS("yes")))
    Report(TroubleshootingCheck::wxMathML, TroubleshootingStatus::ok, wxEmptyString);
  else {
    wxString details = answered ?
      _("Maxima read wxMathML.lisp, but not all of it worked.") :
      m_maximaEnded ?
      wxString::Format(_("Maxima ended with exit code %i while reading wxMathML.lisp."),
                       m_maximaExitCode) :
      wxString::Format(_("Maxima didn't answer within %li seconds."),
                       m_settings.setupTimeoutMs / 1000);
    details += wxS("\n") + _("Maxima sent:") + wxS("\n") + MaximaOutputTail();
    Report(TroubleshootingCheck::wxMathML, TroubleshootingStatus::failed, details,
           _("This Maxima, or the Lisp it runs on, may be too old or too new for "
             "this wxMaxima. The text Maxima sent may contain a Lisp error that "
             "tells more."));
  }

  // Only an authenticated Maxima is asked anything else, as the worksheet does.
  if (!m_authenticated || m_maximaEnded) {
    SkipRemainingMaximaChecks(m_maximaEnded ? _("Maxima has ended.") :
                              _("Maxima didn't authenticate."));
    StopMaxima();
    return;
  }
  SendQuestions();
}

void TroubleshootingRunner::SendQuestions() {
  SendToMaxima(wxS(":lisp-quiet (progn ") +
               Query(wxS("version"), IfBound(wxS("*autoconf-version*"))) +
               Query(wxS("lisp"), wxS("(lisp-implementation-type)")) +
               Query(wxS("tempdir"), IfBound(wxS("$maxima_tempdir"))) +
               Query(wxS("htmldir"), IfBound(wxS("*maxima-htmldir*"))) +
               Query(wxS("infodir"), IfBound(wxS("*maxima-infodir*"))) +
               Query(wxS("gnuplot"), IfBound(wxS("$gnuplot_command"))) +
               wxS(" (finish-output))\n"));
  SendToMaxima(wxS(":lisp-quiet (progn ") + LoadQuery(wxS("load-package"), wxS("stringproc")) +
               wxS(" (finish-output))\n"));
  SendToMaxima(wxS(":lisp-quiet (progn ") + LoadQuery(wxS("load-draw"), wxS("draw")) +
               wxS(" (finish-output))\n"));
  // Where the info manual is differs between installations (Debian, for
  // one, doesn't keep it in *maxima-infodir*), so ask describe() itself.
  // It returns true if it found an entry. Text, so that a Maxima told to
  // answer help questions with a browser doesn't open one.
  SendToMaxima(wxS(":lisp-quiet (progn ") +
               Query(wxS("describe"),
                     wxS("(let (($output_format_for_help '$text)) "
                         "(if (eq t (ignore-errors (meval '(($describe) \"integrate\" $exact)))) "
                         "\"ok\" \"failed\"))")) +
               wxS(" (finish-output))\n"));
  EnterStage(Stage::waitAnswers);
}

void TroubleshootingRunner::PollAnswers() {
  bool found = false;
  // The answer to the last question
  wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("describe"), &found);
  if (found)
    EvaluateAnswers(false);
  else if (m_maximaEnded || (StageAge() > m_settings.queryTimeoutMs))
    EvaluateAnswers(true);
}

void TroubleshootingRunner::EvaluateAnswers(bool timedOut) {
  const wxString why = m_maximaEnded ?
    wxString::Format(_("Maxima ended with exit code %i before it answered."),
                     m_maximaExitCode) :
    timedOut ? wxString::Format(_("Maxima didn't answer within %li seconds."),
                                m_settings.queryTimeoutMs / 1000) :
    wxString();

  bool found = false;
  const wxString loadPackage = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("load-package"), &found);
  if (found && (loadPackage == wxS("ok")))
    Report(TroubleshootingCheck::loadPackage, TroubleshootingStatus::ok,
           _("load(stringproc) worked."));
  else
    Report(TroubleshootingCheck::loadPackage, TroubleshootingStatus::failed,
           (found ? _("load(stringproc) failed.") : why) + wxS("\n") +
           _("Maxima sent:") + wxS("\n") + MaximaOutputTail(),
           _("Maxima's share directory may be missing or incomplete, or a "
             "maxima-init.mac may have changed where Maxima looks for packages."));

  const wxString loadDraw = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("load-draw"), &found);
  if (found && (loadDraw == wxS("ok")))
    Report(TroubleshootingCheck::loadDraw, TroubleshootingStatus::ok, _("load(draw) worked."));
  else
    Report(TroubleshootingCheck::loadDraw, TroubleshootingStatus::failed,
           (found ? _("load(draw) failed.") : why) + wxS("\n") +
           _("Maxima sent:") + wxS("\n") + MaximaOutputTail(),
           _("wxdraw2d(), wxdraw3d() and most of the plot wizards need draw. Its "
             "files may be missing from Maxima's share directory."));

  bool haveInfo = false;
  const wxString version = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("version"), &haveInfo);
  if (haveInfo) {
    bool dummy;
    wxLogMessage(wxS("Troubleshooting: Maxima %s on %s"), version,
                 wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("lisp"), &dummy));
  }

  // Maxima's temp directory, where the plots go
  m_tempDir = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("tempdir"), &found);
  if (!found || m_tempDir.IsEmpty()) {
    Report(TroubleshootingCheck::tempDir, TroubleshootingStatus::skipped,
           why.IsEmpty() ? _("Maxima didn't tell its temp directory.") : why);
    m_tempDir.Clear();
  } else {
    wxString details = m_tempDir;
    if (!wxDirExists(m_tempDir)) {
      // The worksheet creates it, too (MaximaResponseReader::VariableActionTempDir()).
      wxMkdir(m_tempDir, wxS_DIR_DEFAULT);
      details += wxS("\n") + _("It didn't exist, and wxMaxima created it.");
    }
    wxString probeFile;
    if (wxDirExists(m_tempDir)) {
      probeFile = wxFileName::CreateTempFileName(m_tempDir + wxFileName::GetPathSeparator() +
                                                 wxS("wxmprobe"));
      if (!probeFile.IsEmpty())
        wxRemoveFile(probeFile);
    }
    if (probeFile.IsEmpty()) {
      Report(TroubleshootingCheck::tempDir, TroubleshootingStatus::failed,
             details + wxS("\n") + _("wxMaxima can't create a file there."),
             _("Plots are written to this directory. Set the environment variable "
               "MAXIMA_TEMPDIR to a directory you can write to, in Edit > Configure > "
               "Maxima > Environment variables for maxima."));
      m_tempDir.Clear();
    } else if (wxm::PathHasQuote(m_tempDir))
      Report(TroubleshootingCheck::tempDir, TroubleshootingStatus::warning,
             details + wxS("\n") + _("The path contains a quote character."),
             _("Plots made with Maxima's draw package (wxdraw2d, wxdraw3d, "
               "wxdrawdf, with_slider_draw, ...) cannot be created in such a "
               "directory. Set the environment variable MAXIMA_TEMPDIR to a "
               "directory whose path contains no quotes, in Edit > Configure > "
               "Maxima > Environment variables for maxima."));
    else
      Report(TroubleshootingCheck::tempDir, TroubleshootingStatus::ok, details);
  }

  // Maxima's HTML manual, which F1 and "Help on" show
  const wxString htmlDir = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("htmldir"), &found);
  if (!found)
    Report(TroubleshootingCheck::maximaHtmlManual, TroubleshootingStatus::skipped,
           why.IsEmpty() ? _("Maxima didn't tell where its manual is.") : why);
  else {
    const wxString manual = FindMaximaHtmlManual(htmlDir);
    if (!manual.IsEmpty())
      Report(TroubleshootingCheck::maximaHtmlManual, TroubleshootingStatus::ok, manual);
    else
      Report(TroubleshootingCheck::maximaHtmlManual, TroubleshootingStatus::warning,
             wxString::Format(_("No maxima_singlepage.html or maxima_toc.html in or near %s."),
                              htmlDir),
             _("Maxima's HTML manual may not be installed: some Linux distributions "
               "ship it as a package of its own, for example maxima-doc. Without it "
               "wxMaxima shows Maxima's manual from the internet."));
  }

  // Maxima's info manual, which ?, ?? and describe() need
  const wxString infoDir = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("infodir"), &found);
  const wxString describe = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("describe"), &found);
  if (!found)
    Report(TroubleshootingCheck::maximaInfoManual, TroubleshootingStatus::skipped,
           why.IsEmpty() ? _("Maxima didn't answer.") : why);
  else if (describe == wxS("ok"))
    Report(TroubleshootingCheck::maximaInfoManual, TroubleshootingStatus::ok,
           _("describe(integrate) found its entry."));
  else {
    wxString details = wxString::Format(_("describe(integrate) found nothing. Maxima's "
                                          "info directory is %s."), infoDir);
    // Maxima warns about that on its console, not through the socket.
    if ((m_fromMaxima + m_maximaConsole).Find(wxS("unable to set up the help system")) !=
        wxNOT_FOUND)
      details += wxS("\n") + _("Maxima says it is unable to set up its help system.");
    Report(TroubleshootingCheck::maximaInfoManual, TroubleshootingStatus::warning, details,
           _("Without it ?, ?? and describe() find nothing, and Maxima warns on "
             "every start. Some Linux distributions ship the manual as a "
             "package of its own, for example maxima-doc; some installations "
             "leave out documentation altogether."));
  }

  m_gnuplotFromMaxima = wxm::TroubleshootingProbeValue(m_fromMaxima, wxS("gnuplot"), &found);
  StopMaxima();
}

void TroubleshootingRunner::StopMaxima() {
  if (!m_maxima || m_maximaEnded) {
    if (!m_reported.count(TroubleshootingCheck::maximaStops))
      Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::skipped,
             _("Maxima had already ended."));
    if (m_stage != Stage::notStarted && m_stage != Stage::finished &&
        m_stage < Stage::gnuplotPng)
      StartGnuplot(wxS("png"));
    return;
  }
  m_killedMaxima = true;
  if (m_socket)
    m_socket->Notify(false);
#ifdef __WXMSW__
  // As MaximaProcessManager::KillMaxima() does: the Lisp is a grandchild.
  wxArrayString out, err;
  wxExecute(wxString::Format(wxS("taskkill /PID %li /F /T"), m_maximaPid), out, err,
            wxEXEC_SYNC | wxEXEC_NOEVENTS);
#else
  // Maxima was started as a process group leader, so this reaches the Lisp, too.
  wxProcess::Kill(static_cast<int>(m_maximaPid), wxSIGKILL, wxKILL_CHILDREN);
#endif
  // Called from the destructor, too, which must not go on to start gnuplot.
  if (m_stage != Stage::notStarted && m_stage != Stage::finished)
    EnterStage(Stage::waitStop);
}

void TroubleshootingRunner::PollStop() {
  if (m_maximaEnded) {
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::ok,
           _("Maxima ended when wxMaxima stopped it."));
    StartGnuplot(wxS("png"));
  } else if (StageAge() > m_settings.stopTimeoutMs) {
    Report(TroubleshootingCheck::maximaStops, TroubleshootingStatus::failed,
           wxString::Format(_("Maxima (process %li) still runs %li seconds after "
                              "wxMaxima tried to kill it."),
                            m_maximaPid, m_settings.stopTimeoutMs / 1000),
           _("wxMaxima then can't interrupt or restart Maxima either. Security "
             "software may keep wxMaxima from stopping programs it started."));
    StartGnuplot(wxS("png"));
  }
}

wxString TroubleshootingRunner::FindGnuplot() {
  if (!m_settings.gnuplotCommand.IsEmpty() && wxFileExists(m_settings.gnuplotCommand))
    return m_settings.gnuplotCommand;
  wxString gnuplot = FindInPath(m_gnuplotFromMaxima);
  if (gnuplot.IsEmpty())
    gnuplot = FindInPath(wxS("gnuplot"));
  return gnuplot;
}

void TroubleshootingRunner::StartGnuplot(const wxString &terminal) {
  const TroubleshootingCheck check = (terminal == wxS("png")) ?
    TroubleshootingCheck::gnuplotPng : TroubleshootingCheck::gnuplotPngCairo;
  EnterStage((terminal == wxS("png")) ? Stage::gnuplotPng : Stage::gnuplotPngCairo);
  if (check == TroubleshootingCheck::gnuplotPng) {
    m_gnuplotBinary = FindGnuplot();
    if (m_gnuplotBinary.IsEmpty()) {
      Report(TroubleshootingCheck::gnuplotFound, TroubleshootingStatus::failed,
             m_gnuplotFromMaxima.IsEmpty() ? wxString(_("No gnuplot in the PATH.")) :
             wxString::Format(_("Maxima's gnuplot_command is %s, and wxMaxima "
                                "can't find that program."), m_gnuplotFromMaxima),
             _("Maxima plots with gnuplot. Install gnuplot, or tell Maxima where it "
               "is with gnuplot_command:\"/path/to/gnuplot\"$ in "
               "~/.maxima/wxmaxima-init.mac."));
      Report(TroubleshootingCheck::gnuplotPng, TroubleshootingStatus::skipped,
             _("There is no gnuplot."));
      Report(TroubleshootingCheck::gnuplotPngCairo, TroubleshootingStatus::skipped,
             _("There is no gnuplot."));
      EnterStage(Stage::finished);
      return;
    }
    Report(TroubleshootingCheck::gnuplotFound, TroubleshootingStatus::ok, m_gnuplotBinary);
  }

  if (!m_gnuplotScript.IsEmpty())
    wxRemoveFile(m_gnuplotScript);
  if (!m_gnuplotOutput.IsEmpty())
    wxRemoveFile(m_gnuplotOutput);
  if (m_gnuplot) {
    delete m_gnuplot;
    m_gnuplot = nullptr;
  }
  m_gnuplotEnded = false;
  m_gnuplotErrors.Clear();
  m_gnuplotTerminal = terminal;

  const wxString dir = m_tempDir.IsEmpty() ? wxFileName::GetTempDir() : m_tempDir;
  const wxString base = dir + wxFileName::GetPathSeparator() + wxS("wxmprobe");
  m_gnuplotScript = wxFileName::CreateTempFileName(base);
  m_gnuplotOutput = wxFileName::CreateTempFileName(base);
  wxFile script;
  if (m_gnuplotScript.IsEmpty() || m_gnuplotOutput.IsEmpty() ||
      !script.Open(m_gnuplotScript, wxFile::write) ||
      !script.Write(wxS("set terminal ") + terminal + wxS(" size 200,100\n") +
                    wxS("set output ") + wxm::GnuplotSingleQuoted(m_gnuplotOutput) + wxS("\n") +
                    wxS("plot sin(x)\n") + wxS("set output\n"))) {
    Report(check, TroubleshootingStatus::failed,
           wxString::Format(_("Can't write a gnuplot script to %s."), dir));
    if (check == TroubleshootingCheck::gnuplotPng)
      Report(TroubleshootingCheck::gnuplotPngCairo, TroubleshootingStatus::skipped,
             wxString::Format(_("Can't write a gnuplot script to %s."), dir));
    EnterStage(Stage::finished);
    return;
  }
  script.Close();
  // gnuplot must not find a plot of the last run there.
  wxRemoveFile(m_gnuplotOutput);

  m_gnuplot = new Process(this);
  const wxString command = wxS("\"") + m_gnuplotBinary + wxS("\" \"") + m_gnuplotScript + wxS("\"");
  if (wxExecute(command, wxEXEC_ASYNC | wxEXEC_HIDE_CONSOLE | wxEXEC_MAKE_GROUP_LEADER,
                m_gnuplot) <= 0) {
    m_gnuplot = nullptr;
    Report(check, TroubleshootingStatus::failed,
           wxString::Format(_("Running %s failed."), command));
    if (check == TroubleshootingCheck::gnuplotPng)
      Report(TroubleshootingCheck::gnuplotPngCairo, TroubleshootingStatus::skipped,
             _("gnuplot couldn't be started."));
    EnterStage(Stage::finished);
  }
}

void TroubleshootingRunner::PollGnuplot() {
  const TroubleshootingCheck check = (m_stage == Stage::gnuplotPng) ?
    TroubleshootingCheck::gnuplotPng : TroubleshootingCheck::gnuplotPngCairo;
  if (!m_gnuplotEnded) {
    if (StageAge() <= m_settings.gnuplotTimeoutMs)
      return;
    wxProcess::Kill(static_cast<int>(m_gnuplot->GetPid()), wxSIGKILL, wxKILL_CHILDREN);
    m_gnuplot->Orphan();
    m_gnuplot = nullptr;
    Report(check, TroubleshootingStatus::failed,
           wxString::Format(_("gnuplot didn't finish a tiny plot within %li seconds."),
                            m_settings.gnuplotTimeoutMs / 1000) +
           wxS("\n") + m_gnuplotErrors);
    if (check == TroubleshootingCheck::gnuplotPng)
      Report(TroubleshootingCheck::gnuplotPngCairo, TroubleshootingStatus::skipped,
             _("gnuplot hangs."));
    EnterStage(Stage::finished);
    return;
  }

  bool isPng = false;
  {
    wxFile output;
    std::array<unsigned char, 8> header{};
    if (wxFileExists(m_gnuplotOutput) && output.Open(m_gnuplotOutput)) {
      const ssize_t count = output.Read(header.data(), header.size());
      isPng = (count > 0) && wxm::IsPngData(header.data(), static_cast<std::size_t>(count));
    }
  }
  wxString details = m_gnuplotErrors.Strip(wxString::both);
  if (isPng)
    Report(check, TroubleshootingStatus::ok, details);
  else if ((check == TroubleshootingCheck::gnuplotPngCairo) &&
           wxm::GnuplotLacksTerminal(m_gnuplotErrors))
    Report(check, TroubleshootingStatus::warning, details,
           _("This gnuplot has no pngcairo terminal. wxMaxima then makes plots "
             "with the png terminal, which looks less smooth."));
  else
    Report(check, TroubleshootingStatus::failed,
           wxString::Format(_("gnuplot ended with exit code %i and drew no %s file."),
                            m_gnuplotExitCode, m_gnuplotTerminal) +
           (details.IsEmpty() ? wxString() : wxS("\n") + details),
           (check == TroubleshootingCheck::gnuplotPng) ?
           _("wxMaxima's plots are png files made by gnuplot. A gnuplot that can't "
             "make them may be broken, or may lack the libraries its png "
             "terminal needs.") : wxString());

  if (check == TroubleshootingCheck::gnuplotPng)
    StartGnuplot(wxS("pngcairo"));
  else
    EnterStage(Stage::finished);
}
