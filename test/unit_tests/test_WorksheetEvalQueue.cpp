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
  Pins the worksheet's "add cells to the evaluation queue" behavior before the
  Add*ToEvaluationQueue family is extracted into WorksheetEvalQueue.

  Each Worksheet::Add*ToEvaluationQueue method decides *which* group cells to
  enqueue (only visible code cells are eligible) and *where* the h-caret should
  land afterwards. These scenarios pin both - including the quirks in how the
  "rest of the document" start point is derived from the active cell, a
  selection or the h-caret - so the extraction cannot change them.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "EvaluationQueue.h"
#include "worksheet/Worksheet.h"
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

//! Append a group of the given type (with a text output) after \p after.
static GroupCell *AppendGroup(GroupType type, const wxString &code,
                              GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, type, code);
  // The group must be set at construction: m_group is only ever assigned in the
  // Cell constructor; SetOutput does not re-parent cells.
  group->SetOutput(std::make_unique<TextCell>(group.get(), g_cfg, wxS("out")));
  return g_ws->InsertGroupCells(std::move(group), after);
}

//! A fresh four-code-cell document with a cleared evaluation queue.
static GroupCell *BuildCodeDocument() {
  g_ws->ClearDocument();
  g_ws->GetEvaluationQueue().Clear();
  GroupCell *c0 = AppendGroup(GC_TYPE_CODE, wxS("c0 : 0;"), nullptr);
  GroupCell *c1 = AppendGroup(GC_TYPE_CODE, wxS("c1 : 1;"), c0);
  GroupCell *c2 = AppendGroup(GC_TYPE_CODE, wxS("c2 : 2;"), c1);
  AppendGroup(GC_TYPE_CODE, wxS("c3 : 3;"), c2);
  g_ws->SetActiveCell(nullptr);
  g_ws->ClearSelection();
  g_ws->RecalculateIfNeeded();
  return c0;
}

SCENARIO("AddDocumentToEvaluationQueue enqueues every code cell and parks the "
         "caret at the end") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *last = g_ws->GetLastCellInWorksheet();

  g_ws->AddDocumentToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("all four cells are queued and the h-caret sits on the last cell") {
    REQUIRE(q.Size() == 4);
    for (GroupCell *g = c0; g; g = g->GetNext())
      REQUIRE(q.IsInQueue(g));
    REQUIRE(g_ws->GetHCaret() == last);
  }
}

SCENARIO("AddEntireDocumentToEvaluationQueue enqueues every code cell") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *last = g_ws->GetLastCellInWorksheet();

  g_ws->AddEntireDocumentToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("all cells are queued and the caret parks at the end") {
    REQUIRE(q.Size() == 4);
    for (GroupCell *g = c0; g; g = g->GetNext())
      REQUIRE(q.IsInQueue(g));
    REQUIRE(g_ws->GetHCaret() == last);
  }
}

SCENARIO("AddToEvaluationQueue skips a non-code cell but takes a code cell") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *text = AppendGroup(GC_TYPE_TEXT, wxS("just prose"), c0);

  g_ws->AddToEvaluationQueue(text);
  THEN("the text cell is not enqueued") {
    REQUIRE_FALSE(g_ws->GetEvaluationQueue().IsInQueue(text));
    REQUIRE(g_ws->GetEvaluationQueue().Size() == 0);
  }

  g_ws->AddToEvaluationQueue(c0);
  THEN("a code cell is enqueued") {
    REQUIRE(g_ws->GetEvaluationQueue().IsInQueue(c0));
    REQUIRE(g_ws->GetEvaluationQueue().Size() == 1);
  }
}

SCENARIO("AddSelectionToEvaluationQueue(start,end) enqueues only the range and "
         "parks the caret at the range end") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  GroupCell *c3 = c2->GetNext();

  g_ws->AddSelectionToEvaluationQueue(c1, c2);

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("only c1 and c2 are queued and the caret sits on c2") {
    REQUIRE(q.Size() == 2);
    REQUIRE(q.IsInQueue(c1));
    REQUIRE(q.IsInQueue(c2));
    REQUIRE_FALSE(q.IsInQueue(c0));
    REQUIRE_FALSE(q.IsInQueue(c3));
    REQUIRE(g_ws->GetHCaret() == c2);
  }
}

SCENARIO("AddSelectionToEvaluationQueue() uses the current selection") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  g_ws->SetSelection(c1, c2);

  g_ws->AddSelectionToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("the selected range is queued and the caret sits on its end") {
    REQUIRE(q.Size() == 2);
    REQUIRE(q.IsInQueue(c1));
    REQUIRE(q.IsInQueue(c2));
    REQUIRE(g_ws->GetHCaret() == c2);
  }
}

SCENARIO("AddRestToEvaluationQueue from an active cell enqueues from that cell "
         "onwards") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  GroupCell *c3 = c2->GetNext();
  g_ws->SetActiveCell(c1->GetEditable());

  g_ws->AddRestToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("c1..c3 are queued (the cell holding the cursor is included)") {
    REQUIRE(q.Size() == 3);
    REQUIRE(q.IsInQueue(c1));
    REQUIRE(q.IsInQueue(c2));
    REQUIRE(q.IsInQueue(c3));
    REQUIRE_FALSE(q.IsInQueue(c0));
    REQUIRE(g_ws->GetHCaret() == c3);
  }
}

SCENARIO("AddRestToEvaluationQueue from the h-caret enqueues the cells below "
         "it") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  GroupCell *c3 = c2->GetNext();
  g_ws->SetHCaret(c1);

  g_ws->AddRestToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("only the cells after the caret (c2, c3) are queued") {
    REQUIRE(q.Size() == 2);
    REQUIRE(q.IsInQueue(c2));
    REQUIRE(q.IsInQueue(c3));
    REQUIRE_FALSE(q.IsInQueue(c1));
    REQUIRE(g_ws->GetHCaret() == c3);
  }
}

SCENARIO("AddRestToEvaluationQueue from a selection starts after the "
         "selection's group") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  GroupCell *c3 = c2->GetNext();
  g_ws->SetSelection(c1, c1);

  g_ws->AddRestToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("the queue starts at the cell after the selected group") {
    REQUIRE(q.Size() == 2);
    REQUIRE(q.IsInQueue(c2));
    REQUIRE(q.IsInQueue(c3));
    REQUIRE_FALSE(q.IsInQueue(c1));
  }
}

SCENARIO("AddDocumentTillHereToEvaluationQueue enqueues up to the caret and "
         "does not move it") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c1 = c0->GetNext();
  GroupCell *c2 = c1->GetNext();
  GroupCell *c3 = c2->GetNext();
  g_ws->SetHCaret(c2);

  g_ws->AddDocumentTillHereToEvaluationQueue();

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("c0..c2 are queued, c3 is not, and the caret stays on c2") {
    REQUIRE(q.Size() == 3);
    REQUIRE(q.IsInQueue(c0));
    REQUIRE(q.IsInQueue(c1));
    REQUIRE(q.IsInQueue(c2));
    REQUIRE_FALSE(q.IsInQueue(c3));
    REQUIRE(g_ws->GetHCaret() == c2);
  }
}

SCENARIO("AddCellToEvaluationQueue enqueues one cell and parks the caret on "
         "it") {
  GroupCell *c0 = BuildCodeDocument();
  GroupCell *c2 = c0->GetNext()->GetNext();

  g_ws->AddCellToEvaluationQueue(c2);

  EvaluationQueue &q = g_ws->GetEvaluationQueue();
  THEN("exactly that cell is queued and the caret sits on it") {
    REQUIRE(q.Size() == 1);
    REQUIRE(q.IsInQueue(c2));
    REQUIRE(g_ws->GetHCaret() == c2);
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
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
