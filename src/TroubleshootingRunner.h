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
  Runs the troubleshooting checks that need Maxima or gnuplot (GH #2559).
*/

#ifndef TROUBLESHOOTINGRUNNER_H
#define TROUBLESHOOTINGRUNNER_H

#include "TroubleshootingChecks.h"
#include <wx/event.h>
#include <wx/process.h>
#include <wx/socket.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <functional>
#include <memory>
#include <set>
#include <string>

/*! Runs the troubleshooting checks that need a Maxima or a gnuplot process

  It starts a Maxima of its own -- with its own server, its own port and its
  own secret -- rather than looking at the one the worksheet talks to: that
  one may be busy, may never have started, or may be the very thing that
  went wrong, and the checks must neither disturb a running calculation nor
  depend on one. The Maxima is started the way MaximaProcessManager starts
  the worksheet's one (same command line, same environment), so a problem
  that keeps the worksheet's Maxima from working keeps this one from working
  too, and is reported at the step where it happens.

  The steps follow each other: find Maxima, open the server, start Maxima,
  wait for it to connect, wait for its first prompt, check that it
  authenticates, send wxMathML.lisp, ask it a few questions (where its temp
  directory and manuals are, which gnuplot it uses, whether it can load a
  package and draw), and stop it again. Then gnuplot draws a small png and
  pngcairo plot into Maxima's temp directory. A step that cannot be run
  since one before it failed is reported as skipped.

  Everything happens in Poll(), driven by a timer once Start() was called,
  so nothing here blocks the GUI. Each result is reported as soon as it is
  known, through the callback passed to the constructor.

  The answers to the questions are lines of the form "wxprobe-<key>=<value>"
  the Maxima prints (see wxm::TroubleshootingProbeValue()), which makes them
  easy to find among everything else Maxima prints, whether or not
  wxMathML.lisp has been loaded.
*/
class TroubleshootingRunner : public wxEvtHandler
{
public:
  //! What the runner needs to know to start Maxima and gnuplot
  struct Settings {
    //! The Maxima program, resolved to a path, as MaximaProcessManager would run it
    wxString maximaExecutable;
    //! The Maxima program as configured, before it was looked for
    wxString maximaConfigured;
    //! The whole command line Maxima is started with, minus the "-s <port>"
    wxString maximaCommand;
    //! The environment Maxima is started with, minus MAXIMA_AUTH_CODE
    wxEnvVariableHashMap environment;
    //! What wxMaxima sends Maxima once it is connected: wxMathML.lisp
    wxString setupCommand;
    /*! A Lisp function wxMathML.lisp defines near its end

      Whether Maxima knows it tells whether all of wxMathML.lisp was loaded:
      it reaches Maxima as one single form, and an error in it stops
      everything after that error.
    */
    wxString setupCheckFunction = wxS("wx-define-unicode-aliases");
    /*! The gnuplot the worksheet's Maxima session found

      Empty if that session never told: then the one this runner's Maxima
      names (gnuplot_command) is looked for in the PATH.
    */
    wxString gnuplotCommand;
    //! How long Maxima may take to connect to us
    long connectTimeoutMs = 30000;
    //! How long Maxima may take from connecting to its first prompt
    long promptTimeoutMs = 60000;
    //! How long Maxima may take to load wxMathML.lisp and authenticate
    long setupTimeoutMs = 60000;
    //! How long Maxima may take to answer all questions, loading draw included
    long queryTimeoutMs = 90000;
    //! How long a killed Maxima may take to end
    long stopTimeoutMs = 10000;
    //! How long gnuplot may take to draw one plot
    long gnuplotTimeoutMs = 30000;
  };

  using ResultCallback = std::function<void (const wxm::TroubleshootingResult &)>;

  TroubleshootingRunner(const Settings &settings, ResultCallback onResult,
                        std::function<void ()> onFinished = {});
  //! Stops Maxima and gnuplot if they still run
  ~TroubleshootingRunner() override;

  //! Starts the checks; from now on a timer calls Poll()
  void Start();
  /*! Does whatever can be done now, without waiting

    Called by the timer Start() starts. A test that has no event loop
    running calls it itself, yielding in between so wxProcess learns that a
    child has ended.
  */
  void Poll();
  //! True once every check this runner runs has been reported
  bool IsFinished() const { return m_stage == Stage::finished; }

  //! The checks this runner runs, and only these
  static const std::set<wxm::TroubleshootingCheck> &Checks();

private:
  //! A wxProcess that tells its runner when it ended, and outlives it if need be
  class Process;
  friend class Process;

  //! The steps Poll() goes through
  enum class Stage {
    notStarted,
    waitConnect,
    waitPrompt,
    waitSetup,
    waitAnswers,
    waitStop,
    gnuplotPng,
    gnuplotPngCairo,
    finished
  };

  //! Reports a result, unless that check already has one
  void Report(wxm::TroubleshootingCheck check, wxm::TroubleshootingStatus status,
              const wxString &details, const wxString &hint = wxEmptyString);
  //! Reports every Maxima check that has no result yet as skipped
  void SkipRemainingMaximaChecks(const wxString &reason);
  //! Finds Maxima, opens the server and starts Maxima
  void StartMaxima();
  //! Reads what Maxima and gnuplot sent, and sends what is waiting to be sent
  void PumpIO();
  //! Called by Process when the process it belongs to has ended
  void ProcessEnded(Process *process, int status);
  //! The step that waits for Maxima to connect
  void PollConnect();
  //! The step that waits for the first prompt and the auth= line
  void PollPrompt();
  //! The step that waits for wxMathML.lisp's key
  void PollSetup();
  //! The step that waits for Maxima's answers
  void PollAnswers();
  //! Sends the questions once Maxima is known to be ours
  void SendQuestions();
  //! Evaluates the answers that arrived (or didn't) and stops Maxima
  void EvaluateAnswers(bool timedOut);
  //! Kills Maxima and everything it started
  void StopMaxima();
  //! The step that waits for the killed Maxima to end
  void PollStop();
  //! Starts gnuplot drawing a plot with the given terminal into the temp dir
  void StartGnuplot(const wxString &terminal);
  //! The step that waits for gnuplot
  void PollGnuplot();
  //! Finds the gnuplot to check; empty if there is none
  wxString FindGnuplot();
  //! Moves to the next stage, remembering when it was entered
  void EnterStage(Stage stage);
  //! Milliseconds since the current stage was entered
  long StageAge() const;
  //! The last part of what Maxima sent, for showing it in a result
  wxString MaximaOutputTail() const;
  //! Queues text to be sent to Maxima
  void SendToMaxima(const wxString &text);

  Settings m_settings;
  ResultCallback m_onResult;
  std::function<void ()> m_onFinished;
  //! The checks that already have a result
  std::set<wxm::TroubleshootingCheck> m_reported;
  Stage m_stage = Stage::notStarted;
  //! wxGetLocalTimeMillis() when the current stage was entered
  wxLongLong m_stageStart;
  wxTimer m_timer;

  //! The server Maxima connects to
  std::unique_ptr<wxSocketServer> m_server;
  //! The port m_server listens on
  int m_port = 0;
  //! The connection to Maxima
  std::unique_ptr<wxSocketBase> m_socket;
  //! The secret Maxima has to prove it knows (MAXIMA_AUTH_CODE)
  wxString m_secret;
  //! Everything Maxima sent through the socket
  wxString m_fromMaxima;
  //! Bytes received but not yet decoded, since a UTF-8 sequence was split
  std::string m_undecoded;
  //! What is still to be sent to Maxima
  std::string m_toMaxima;
  //! True once the first prompt arrived and the auth= line was looked at
  bool m_gotPrompt = false;
  //! True once Maxima proved it knows m_secret
  bool m_authenticated = false;

  //! The Maxima process. Detached, not deleted, if it outlives the runner.
  Process *m_maxima = nullptr;
  long m_maximaPid = 0;
  bool m_maximaEnded = false;
  int m_maximaExitCode = 0;
  //! True once we killed Maxima ourselves, so its end is no surprise
  bool m_killedMaxima = false;
  //! What Maxima wrote to its stdout and stderr
  wxString m_maximaConsole;

  //! The gnuplot process currently running
  Process *m_gnuplot = nullptr;
  bool m_gnuplotEnded = false;
  int m_gnuplotExitCode = 0;
  wxString m_gnuplotErrors;
  wxString m_gnuplotTerminal;
  wxString m_gnuplotScript;
  wxString m_gnuplotOutput;
  //! The gnuplot found for the gnuplot checks
  wxString m_gnuplotBinary;

  //! What Maxima told about itself
  wxString m_tempDir;
  wxString m_gnuplotFromMaxima;
};

#endif // TROUBLESHOOTINGRUNNER_H
