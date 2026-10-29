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
  Tests the troubleshooting checks (GH #2559).

  The helpers that interpret what a check found are tested directly. The
  runner, which starts a Maxima and talks to it, is tested against this very
  executable playing Maxima's part: started with --fake-maxima=<how> it
  misbehaves the way a broken Maxima installation can (exits at once, never
  connects, never prompts, sends a wrong secret), or answers the runner's
  questions the way a working Maxima would. No Maxima needs to be installed
  for that, so these tests run everywhere unit tests do.

  The scenarios tagged [.maxima] run the checks against a real Maxima
  instead, named by the environment variable WXM_MAXIMA; test/CMakeLists.txt
  runs them as a test of their own wherever Maxima is installed. They
  are what notices if wxMathML.lisp or Maxima itself changes in a way the
  checks don't understand any more.
*/

#include "TroubleshootingChecks.h"
#include "TroubleshootingRunner.h"
#include "wxMathml.h"
#include <wx/app.h>
#include <wx/evtloop.h>
#include <wx/filename.h>
#include <wx/init.h>
#include <wx/log.h>
#include <wx/socket.h>
#include <wx/stdpaths.h>
#include <wx/time.h>
#include <wx/utils.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <set>
#include <string>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::TroubleshootingCheck;
using wxm::TroubleshootingStatus;

// ---------------------------------------------------------------------------
// The fake Maxima
// ---------------------------------------------------------------------------

namespace {
//! Plays a Maxima that connects to port \p port and behaves as \p mode says
int FakeMaxima(const std::string &mode, int port) {
  if (mode == "exit") {
    std::fputs("fake maxima: this installation is broken\n", stderr);
    return 3;
  }
  if (mode == "hang") {
    wxMilliSleep(60000);
    return 0;
  }
  wxSocketBase::Initialize();
  wxIPV4address addr;
  addr.LocalHost();
  addr.Service(port);
  wxSocketClient client(wxSOCKET_BLOCK);
  client.SetTimeout(60);
  if (!client.Connect(addr, true))
    return 4;
  auto send = [&client](const std::string &text) {
    client.Write(text.data(), text.size());
  };
  if (mode == "silent") {
    wxMilliSleep(60000);
    return 0;
  }
  wxString secret;
  wxGetEnv(wxS("MAXIMA_AUTH_CODE"), &secret);
  if (mode == "wrongauth") {
    send("auth=guessed\nMaxima 5.99\n(%i1) ");
    wxMilliSleep(60000);
    return 0;
  }
  if (mode == "old")
    // A Maxima from before the auth= line, whose wxMathML.lisp never prints its key
    send("pid=1\nMaxima 5.45\n(%i1) ");
  else
    send("auth=" + std::string(secret.utf8_str()) + "\nMaxima 5.99\n(%i1) ");

  // Answer the questions, line by line, the way the real one would.
  std::string received;
  std::vector<char> buffer(65536);
  for (;;) {
    client.Read(buffer.data(), buffer.size());
    if (client.LastReadCount() == 0) {
      if (!client.IsConnected() || client.Error())
        return 0;
      continue;
    }
    received.append(buffer.data(), client.LastReadCount());
    size_t end;
    while ((end = received.find('\n')) != std::string::npos) {
      const std::string line = received.substr(0, end);
      received.erase(0, end + 1);
      if (mode == "old")
        continue;
      if (line.find("wxprobe-wxmathml") != std::string::npos)
        send("<mth>some output</mth>\nwxprobe-wxmathml=yes\n");
      if (line.find("wxprobe-tempdir") != std::string::npos) {
        send("\nwxprobe-version=5.99\nwxprobe-lisp=FAKE\n");
        send("wxprobe-tempdir=" +
             std::string(wxFileName::GetTempDir().utf8_str()) + "\n");
        send("wxprobe-htmldir=/nonexistent/html\n");
        send("wxprobe-infodir=/nonexistent/info\n");
        send("wxprobe-gnuplot=no-such-gnuplot-program\n");
      }
      if (line.find("wxprobe-load-package") != std::string::npos)
        // The answer arrives in two pieces, as socket data often does.
        send("loading stringproc...\nwxprobe-load-pack");
      if (line.find("wxprobe-load-package") != std::string::npos)
        send("age=ok\n");
      if (line.find("wxprobe-load-draw") != std::string::npos)
        send("error: no draw here\nwxprobe-load-draw=failed\n");
      if (line.find("wxprobe-describe") != std::string::npos)
        send("Warning: Maxima is unable to set up the help system.\n"
             "wxprobe-describe=failed\n");
    }
  }
}

//! The results a runner reported, by check
using Results = std::map<TroubleshootingCheck, wxm::TroubleshootingResult>;

//! Runs \p runner to its end; returns false if that took more than a few minutes
bool RunToEnd(TroubleshootingRunner &runner) {
  runner.Start();
  const wxLongLong start = wxGetLocalTimeMillis();
  while (!runner.IsFinished()) {
    if ((wxGetLocalTimeMillis() - start).ToLong() > 300000)
      return false;
    // The timer that drives the runner and the end of its child processes
    // are both events.
    wxTheApp->Yield(true);
    wxMilliSleep(5);
  }
  return true;
}

//! Settings that start this executable as a fake Maxima
TroubleshootingRunner::Settings FakeSettings(const wxString &mode) {
  TroubleshootingRunner::Settings settings;
  settings.maximaExecutable = wxStandardPaths::Get().GetExecutablePath();
  settings.maximaCommand = wxS("\"") + settings.maximaExecutable +
    wxS("\" --fake-maxima=") + mode;
  wxGetEnvMap(&settings.environment);
  settings.setupCommand = wxS(":lisp-quiet (setup)");
  // Short, so the scenarios that wait for something that never comes are quick.
  settings.connectTimeoutMs = 3000;
  settings.promptTimeoutMs = 3000;
  settings.setupTimeoutMs = 3000;
  settings.queryTimeoutMs = 3000;
  // An unreachable gnuplot keeps these scenarios independent of gnuplot.
  settings.gnuplotCommand = wxS("/nonexistent/gnuplot");
  return settings;
}

//! Runs the checks with the given settings and collects what they reported
Results Run(const TroubleshootingRunner::Settings &settings) {
  Results results;
  TroubleshootingRunner runner(settings, [&results](const wxm::TroubleshootingResult &result) {
    // Every check is reported once, and only the runner's own ones.
    REQUIRE(results.count(result.check) == 0);
    REQUIRE(TroubleshootingRunner::Checks().count(result.check) == 1);
    results[result.check] = result;
  });
  REQUIRE(RunToEnd(runner));
  // Every check the runner owns got a result.
  for (const auto check : TroubleshootingRunner::Checks())
    REQUIRE(results.count(check) == 1);
  return results;
}

TroubleshootingStatus StatusOf(const Results &results, TroubleshootingCheck check) {
  return results.at(check).status;
}
} // namespace

// ---------------------------------------------------------------------------
// The pure helpers
// ---------------------------------------------------------------------------

SCENARIO("Every check has a title, and the report names them all") {
  std::vector<wxm::TroubleshootingResult> results;
  for (const auto check : wxm::AllTroubleshootingChecks()) {
    REQUIRE(!wxm::TroubleshootingCheckTitle(check).IsEmpty());
    wxm::TroubleshootingResult result;
    result.check = check;
    result.status = TroubleshootingStatus::failed;
    result.details = wxS("first line\nsecond line");
    result.hint = wxS("do this");
    results.push_back(result);
  }
  REQUIRE(results.size() == static_cast<size_t>(TroubleshootingCheck::count));
  const wxString report = wxm::TroubleshootingReport(results);
  for (const auto &result : results)
    REQUIRE(report.Contains(wxS("[FAILED] ") + wxm::TroubleshootingCheckTitle(result.check)));
  // Details and hints are indented below their check, line by line.
  REQUIRE(report.Contains(wxS("\n    first line\n    second line\n    do this\n")));
}

SCENARIO("The checks are grouped into sections, each summed up by its worst result") {
  using wxm::TroubleshootingSection;
  std::set<TroubleshootingSection> used;
  for (const auto check : wxm::AllTroubleshootingChecks())
    used.insert(wxm::TroubleshootingSectionOf(check));
  // No section is an empty page.
  REQUIRE(used.size() == wxm::AllTroubleshootingSections().size());
  for (const auto section : wxm::AllTroubleshootingSections())
    REQUIRE(!wxm::TroubleshootingSectionTitle(section).IsEmpty());

  // A check mark and a cross, as the dialog shows them
  REQUIRE(wxm::TroubleshootingStatusSymbol(TroubleshootingStatus::ok) == wxString(wxS("\u2714")));
  REQUIRE(wxm::TroubleshootingStatusSymbol(TroubleshootingStatus::failed) == wxString(wxS("\u2718")));
  std::set<wxString> symbols;
  for (const auto status : {TroubleshootingStatus::pending, TroubleshootingStatus::running,
                            TroubleshootingStatus::ok, TroubleshootingStatus::info,
                            TroubleshootingStatus::warning, TroubleshootingStatus::failed,
                            TroubleshootingStatus::skipped})
    symbols.insert(wxm::TroubleshootingStatusSymbol(status));
  REQUIRE(symbols.size() == 7);

  using S = TroubleshootingStatus;
  REQUIRE(wxm::TroubleshootingSummaryStatus({}) == S::ok);
  REQUIRE(wxm::TroubleshootingSummaryStatus({S::ok, S::info}) == S::info);
  REQUIRE(wxm::TroubleshootingSummaryStatus({S::ok, S::skipped, S::info}) == S::skipped);
  // A failure shows even while other checks of its section still run.
  REQUIRE(wxm::TroubleshootingSummaryStatus({S::running, S::failed, S::ok}) == S::failed);
  REQUIRE(wxm::TroubleshootingSummaryStatus({S::running, S::ok}) == S::running);
  REQUIRE(wxm::TroubleshootingSummaryStatus({S::warning, S::running}) == S::warning);
}

SCENARIO("A probe answer is only read from a complete line of its own") {
  bool found = false;
  GIVEN("an answer among other output") {
    const wxString text = wxS("loading...\nwxprobe-tempdir=/tmp/x\r\n(%i2) ");
    THEN("its value is found, without the line end") {
      REQUIRE(wxm::TroubleshootingProbeValue(text, wxS("tempdir"), &found) == wxS("/tmp/x"));
      REQUIRE(found);
    }
    THEN("another key isn't") {
      wxm::TroubleshootingProbeValue(text, wxS("infodir"), &found);
      REQUIRE(!found);
    }
  }
  GIVEN("an answer whose line hasn't ended yet") {
    wxm::TroubleshootingProbeValue(wxS("wxprobe-tempdir=/tm"), wxS("tempdir"), &found);
    THEN("it isn't there yet") { REQUIRE(!found); }
  }
  GIVEN("the key in the middle of a line, as an echo of the question would have it") {
    const wxString text = wxS("(format t \"wxprobe-load-draw=~a\")\nwxprobe-load-draw=ok\n");
    THEN("only the answer at the start of a line counts") {
      REQUIRE(wxm::TroubleshootingProbeValue(text, wxS("load-draw"), &found) == wxS("ok"));
      REQUIRE(found);
    }
  }
  GIVEN("an empty value") {
    THEN("it is found, and empty") {
      REQUIRE(wxm::TroubleshootingProbeValue(wxS("wxprobe-gnuplot=\n"), wxS("gnuplot"), &found)
              .IsEmpty());
      REQUIRE(found);
    }
  }
}

SCENARIO("The small checks on paths, files and gnuplot's messages") {
  REQUIRE(wxm::PathHasQuote(wxS("C:\\Users\\Jay's\\Temp")));
  REQUIRE(wxm::PathHasQuote(wxS("/tmp/\"x\"")));
  REQUIRE(!wxm::PathHasQuote(wxS("/tmp/plain dir")));

  const unsigned char png[] = {0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n', 0};
  REQUIRE(wxm::IsPngData(png, sizeof(png)));
  REQUIRE(!wxm::IsPngData(png, 4));
  const unsigned char gif[] = {'G', 'I', 'F', '8', '9', 'a', 0, 0};
  REQUIRE(!wxm::IsPngData(gif, sizeof(gif)));

  REQUIRE(wxm::GnuplotSingleQuoted(wxS("/tmp/Jay's/a.png")) == wxS("'/tmp/Jay''s/a.png'"));
  REQUIRE(wxm::GnuplotLacksTerminal(
            wxS("\"t.gp\" line 1: warning: unknown or ambiguous terminal type; "
                "type 'set terminal' for a list")));
  REQUIRE(!wxm::GnuplotLacksTerminal(wxS("")));
}

// ---------------------------------------------------------------------------
// The runner against a fake Maxima
// ---------------------------------------------------------------------------

SCENARIO("A Maxima program that doesn't exist") {
  auto settings = FakeSettings(wxS("exit"));
  settings.maximaExecutable = wxS("/nonexistent/maxima");
  const Results results = Run(settings);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaFound) == TroubleshootingStatus::failed);
  REQUIRE(!results.at(TroubleshootingCheck::maximaFound).hint.IsEmpty());
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStarts) == TroubleshootingStatus::skipped);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStops) == TroubleshootingStatus::skipped);
  // gnuplot doesn't need Maxima: it is checked anyway.
  REQUIRE(StatusOf(results, TroubleshootingCheck::gnuplotFound) != TroubleshootingStatus::skipped);
}

SCENARIO("A Maxima that ends at once") {
  const Results results = Run(FakeSettings(wxS("exit")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaFound) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::server) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStarts) == TroubleshootingStatus::failed);
  // What it printed is what tells the user why.
  REQUIRE(results.at(TroubleshootingCheck::maximaStarts).details.Contains(wxS("installation is broken")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaConnects) == TroubleshootingStatus::skipped);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStops) == TroubleshootingStatus::skipped);
}

SCENARIO("A Maxima that never connects") {
  const Results results = Run(FakeSettings(wxS("hang")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStarts) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaConnects) == TroubleshootingStatus::failed);
  REQUIRE(StatusOf(results, TroubleshootingCheck::firstPrompt) == TroubleshootingStatus::skipped);
  // Killing it works, and is checked even though nothing else could be.
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStops) == TroubleshootingStatus::ok);
}

SCENARIO("A Maxima that connects but never prompts") {
  const Results results = Run(FakeSettings(wxS("silent")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaConnects) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::firstPrompt) == TroubleshootingStatus::failed);
  REQUIRE(StatusOf(results, TroubleshootingCheck::authentication) == TroubleshootingStatus::skipped);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStops) == TroubleshootingStatus::ok);
}

SCENARIO("Something that connects with a wrong secret") {
  const Results results = Run(FakeSettings(wxS("wrongauth")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::firstPrompt) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::authentication) == TroubleshootingStatus::failed);
  // Nothing it says is believed.
  REQUIRE(StatusOf(results, TroubleshootingCheck::wxMathML) == TroubleshootingStatus::skipped);
  REQUIRE(StatusOf(results, TroubleshootingCheck::tempDir) == TroubleshootingStatus::skipped);
}

SCENARIO("A Maxima with no auth= line whose wxMathML.lisp doesn't load") {
  const Results results = Run(FakeSettings(wxS("old")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::firstPrompt) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::authentication) == TroubleshootingStatus::failed);
  REQUIRE(StatusOf(results, TroubleshootingCheck::wxMathML) == TroubleshootingStatus::failed);
  REQUIRE(StatusOf(results, TroubleshootingCheck::loadPackage) == TroubleshootingStatus::skipped);
}

SCENARIO("A Maxima that answers every question") {
  const Results results = Run(FakeSettings(wxS("good")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::authentication) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::wxMathML) == TroubleshootingStatus::ok);
  // An answer that arrived in two pieces.
  REQUIRE(StatusOf(results, TroubleshootingCheck::loadPackage) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::loadDraw) == TroubleshootingStatus::failed);
  REQUIRE(results.at(TroubleshootingCheck::loadDraw).details.Contains(wxS("no draw here")));
  REQUIRE(StatusOf(results, TroubleshootingCheck::tempDir) == TroubleshootingStatus::ok);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaHtmlManual) == TroubleshootingStatus::warning);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaInfoManual) == TroubleshootingStatus::warning);
  REQUIRE(StatusOf(results, TroubleshootingCheck::maximaStops) == TroubleshootingStatus::ok);
  // The gnuplot Maxima named doesn't exist, and neither does the configured one.
  REQUIRE(results.at(TroubleshootingCheck::gnuplotFound).status != TroubleshootingStatus::skipped);
}

// ---------------------------------------------------------------------------
// The runner against a real Maxima
// ---------------------------------------------------------------------------

SCENARIO("The checks pass with a working Maxima", "[.maxima]") {
  wxString maxima;
  REQUIRE(wxGetEnv(wxS("WXM_MAXIMA"), &maxima));
  TroubleshootingRunner::Settings settings;
  settings.maximaExecutable = maxima;
  settings.maximaCommand = wxS("\"") + maxima + wxS("\"");
  wxGetEnvMap(&settings.environment);
  wxMathML mathml(nullptr);
  settings.setupCommand = mathml.GetCmd();
  const Results results = Run(settings);
  for (const auto &[check, result] : results)
    WARN(wxm::TroubleshootingReport({result}).utf8_str().data());
  for (const auto check : {TroubleshootingCheck::maximaFound, TroubleshootingCheck::server,
                           TroubleshootingCheck::maximaStarts,
                           TroubleshootingCheck::maximaConnects,
                           TroubleshootingCheck::firstPrompt,
                           TroubleshootingCheck::authentication,
                           TroubleshootingCheck::wxMathML, TroubleshootingCheck::loadPackage,
                           TroubleshootingCheck::loadDraw, TroubleshootingCheck::maximaStops})
    REQUIRE(StatusOf(results, check) == TroubleshootingStatus::ok);
  // These depend on how Maxima, its manual and gnuplot were installed, which
  // the test can't know -- but each of them must have been checked.
  for (const auto check : {TroubleshootingCheck::tempDir, TroubleshootingCheck::maximaHtmlManual,
                           TroubleshootingCheck::maximaInfoManual,
                           TroubleshootingCheck::gnuplotFound})
    REQUIRE(StatusOf(results, check) != TroubleshootingStatus::skipped);
}

// ---------------------------------------------------------------------------

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};

int main(int argc, char **argv) {
  // Started by a scenario above as a fake Maxima: "--fake-maxima=<how> -s <port>"
  for (int i = 1; i < argc; i++)
    if (std::strncmp(argv[i], "--fake-maxima=", 14) == 0) {
      int port = 0;
      for (int j = 1; j + 1 < argc; j++)
        if (std::strcmp(argv[j], "-s") == 0)
          port = static_cast<int>(std::strtol(argv[j + 1], nullptr, 10));
      wxInitializer initializer;
      return FakeMaxima(argv[i] + 14, port);
    }

  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  int result;
  {
    // wxYield() needs an active event loop on MS Windows; see test_InterruptChannel.
    wxEventLoop loop;
    wxEventLoopActivator activate(&loop);
    result = Catch::Session().run(argc, argv);
  }
  wxEntryCleanup();
  return result;
}
