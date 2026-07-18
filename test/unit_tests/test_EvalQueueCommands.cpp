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
  Command-level behavior of the EvaluationQueue: how a GroupCell's input is
  broken into the individual commands that are sent to maxima one at a time, and
  how the queue advances from cell to cell.

  This is the GUI-free safety net for the "lazy, purely prompt-driven" queue:
  each command is tokenized in the lisp/maxima mode current when its turn comes;
  only the last command of a cell is terminated, and only in maxima mode; and in
  lisp mode each top-level form is sent as its own command (maxima's lisp REPL
  prints a prompt per form). Scenarios tagged "[redesign]" pin the behaviors that
  motivated that rewrite (a lone to_lisp() terminated by the queue itself; a
  to_lisp() followed by lisp forms split into per-command sends).

  The queue is driven exactly the way the live code drives it: read the current
  command with GetCommand(), let a "prompt" possibly flip lisp mode, then
  RemoveFirst() to advance - so a future prompt-driven queue can be exercised
  here without a Worksheet, a socket, or maxima.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include <functional>
#include <vector>

#include "Configuration.h"
#include "EvaluationQueue.h"
#include "Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
} // namespace

static void EnsureDisplay() {
#ifndef _WIN32
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
}

//! Owns the code cells a scenario builds (kept alive for the queue's CellPtrs).
static GroupCell *MakeCodeCell(const wxString &code) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, code);
  return g_ws->InsertGroupCells(std::move(group),
                                g_ws->GetLastCellInWorksheet());
}

//! A "prompt" reaction: given the command just read, update lisp mode the way
//! maxima's prompt would (MAXIMA> after to_lisp(), %-prompt after (to-maxima)).
using PromptReaction = std::function<void(const wxString &)>;

//! Drive the queue to exhaustion, returning every command it hands out in
//! order. After reading each command, \p onPrompt may flip g_cfg's lisp mode
//! (mimicking the prompt maxima prints for that command) before the queue
//! advances - which is what a prompt-driven queue must react to.
static std::vector<wxString> DriveQueue(EvaluationQueue &q,
                                        const PromptReaction &onPrompt = {}) {
  std::vector<wxString> out;
  int guard = 0;
  while (!q.Empty() && (guard++ < 1000)) {
    wxString cmd = q.GetCommand();
    if (!cmd.IsEmpty())
      out.push_back(cmd);
    if (onPrompt)
      onPrompt(cmd);
    q.RemoveFirst();
  }
  return out;
}

//! Reset for a fresh scenario.
static void Reset() {
  g_ws->ClearDocument();
  g_cfg->InLispMode(false);
}

// ---------------------------------------------------------------------------
// Current behavior - these must stay green through the rewrite.
// ---------------------------------------------------------------------------

SCENARIO("A single maxima statement is one command") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("1+1;")));
  auto cmds = DriveQueue(q);
  THEN("it is handed out unchanged as one command") {
    REQUIRE(cmds.size() == 1);
    REQUIRE(cmds[0] == wxS("1+1;"));
  }
}

SCENARIO("Several ;/$-terminated statements in one cell are separate commands") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("a:1$ b:2; c:3$")));
  auto cmds = DriveQueue(q);
  THEN("each statement is its own command, terminator kept") {
    REQUIRE(cmds.size() == 3);
    REQUIRE(cmds[0] == wxS("a:1$"));
    REQUIRE(cmds[1] == wxS("b:2;"));
    REQUIRE(cmds[2] == wxS("c:3$"));
  }
}

SCENARIO("Commands are drawn from the cells in queue order") {
  Reset();
  EvaluationQueue q;
  GroupCell *c0 = MakeCodeCell(wxS("first;"));
  GroupCell *c1 = MakeCodeCell(wxS("second;"));
  q.AddToQueue(c0);
  q.AddToQueue(c1);
  THEN("the queue size counts cells and GetCell tracks the current one") {
    REQUIRE(q.Size() == 2);
    REQUIRE(q.GetCell() == c0);
  }
  auto cmds = DriveQueue(q);
  THEN("both cells' commands come out in order") {
    REQUIRE(cmds.size() == 2);
    REQUIRE(cmds[0] == wxS("first;"));
    REQUIRE(cmds[1] == wxS("second;"));
  }
}

SCENARIO("A :lisp reader-macro line is a single lisp command") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS(":lisp (setq $x 7)")));
  auto cmds = DriveQueue(q);
  THEN("the whole :lisp line is one command (its $ is not a terminator)") {
    REQUIRE(cmds.size() == 1);
    REQUIRE(cmds[0].StartsWith(wxS(":lisp")));
    REQUIRE(cmds[0].Contains(wxS("$x")));
  }
}

SCENARIO("While maxima is already in lisp mode, a form stays one command") {
  Reset();
  g_cfg->InLispMode(true);
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("(setq $x 3)")));
  auto cmds = DriveQueue(q);
  THEN("the $ inside the lisp form is not split off") {
    REQUIRE(cmds.size() == 1);
    REQUIRE(cmds[0].Contains(wxS("$x")));
  }
}

SCENARIO("CommandsLeftInCell counts down as commands are consumed") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("a:1$ b:2$ c:3$")));
  THEN("all three are pending at the start") {
    REQUIRE(q.CommandsLeftInCell() == 3);
  }
  q.RemoveFirst();
  THEN("one fewer after the first is removed") {
    REQUIRE(q.CommandsLeftInCell() == 2);
  }
}

// ---------------------------------------------------------------------------
// The behaviors that motivated the prompt-driven rewrite (tagged [redesign]).
// ---------------------------------------------------------------------------

SCENARIO("A lone to_lisp() is terminated and sent by itself", "[redesign]") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("to_lisp()")));
  auto cmds = DriveQueue(q);
  THEN("it becomes exactly one terminated maxima command") {
    REQUIRE(cmds.size() == 1);
    REQUIRE(cmds[0] == wxS("to_lisp();"));
  }
}

SCENARIO("to_lisp() then lisp forms in one cell is driven per-command by the "
         "prompt",
         "[redesign]") {
  Reset();
  EvaluationQueue q;
  q.AddToQueue(MakeCodeCell(wxS("to_lisp();\n(setq $x 3)\n(to-maxima)")));
  // Mimic maxima's prompts: to_lisp() switches to lisp; (to-maxima) switches back.
  PromptReaction prompt = [](const wxString &cmd) {
    if (cmd.Contains(wxS("to_lisp")))
      g_cfg->InLispMode(true);
    if (cmd.Contains(wxS("to-maxima")))
      g_cfg->InLispMode(false);
  };
  auto cmds = DriveQueue(q, prompt);
  THEN("to_lisp() is its own maxima command, the lisp forms follow as lisp") {
    REQUIRE(cmds.size() >= 2);
    REQUIRE(cmds[0] == wxS("to_lisp();"));
    REQUIRE(cmds[1].Contains(wxS("(setq $x 3)")));
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
