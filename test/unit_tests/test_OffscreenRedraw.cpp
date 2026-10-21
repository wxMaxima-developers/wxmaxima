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
  Output that changes outside the window doesn't show a wrong frame.

  When output above the view grows, the layout pass scrolls by the same
  amount so that what is on screen stays put. A paint that ran that pass
  itself used to have its DC set up for the scroll position from before it,
  and drew the new layout shifted for one frame. And a change that starts
  below the window used to repaint the whole window for nothing.

  Needs a real, shown window (run under xvfb); no Maxima.
*/

#include <wx/app.h>
#include <wx/config.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/sizer.h>
#include <wx/utils.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
constexpr int g_cells = 80;

void ProcessEvents() {
  for (int i = 0; i < 20; i++) {
    wxTheApp->ProcessPendingEvents();
    wxYield();
    wxMilliSleep(5);
  }
}

std::unique_ptr<Cell> OutputLine(int n) {
  MathParser parser(g_cfg);
  return parser.ParseLine(
    wxString::Format(wxS("<mth><lbl altCopy=\"%%o%i\">(%%o%i) </lbl><mn>%i</mn></mth>"),
                     n, n, n));
}

// A fresh worksheet of g_cells code cells with one line of output each.
void FillWorksheet() {
  g_ws->ClearDocument();
  std::unique_ptr<GroupCell> tree;
  GroupCell *last = nullptr;
  for (int i = 0; i < g_cells; i++) {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("x;"));
    group->AppendOutput(OutputLine(i + 1));
    GroupCell *raw = group.get();
    if (!tree)
      tree = std::move(group);
    else
      CellList::AppendCell(last, std::move(group));
    last = raw;
  }
  g_ws->InsertGroupCells(std::move(tree));
  g_ws->RequestRecalculation();
  g_ws->RecalculateIfNeeded();
  g_ws->RedrawIfRequested();
  ProcessEvents();
}

GroupCell *Nth(int n) {
  GroupCell *cell = g_ws->GetTree();
  for (int i = 0; i < n && cell; i++)
    cell = cell->GetNext();
  return cell;
}

int ScrollUnit() {
  int x, y;
  g_ws->GetScrollPixelsPerUnit(&x, &y);
  return y;
}

int ViewTopPx() {
  int x, y;
  g_ws->GetViewStart(&x, &y);
  return y * ScrollUnit();
}

// Scrolls so that the given cell is roughly at the top of the window.
void ScrollTo(GroupCell *cell) {
  g_ws->Scroll(-1, cell->GetRect().GetTop() / ScrollUnit());
  ProcessEvents();
  g_ws->RedrawIfRequested();
  ProcessEvents();
}
} // namespace

SCENARIO("A paint that runs the pending layout pass draws at the scroll "
         "position that pass leaves") {
  FillWorksheet();
  REQUIRE(ScrollUnit() > 0);
  GIVEN("a view scrolled to the middle of the worksheet") {
    ScrollTo(Nth(g_cells / 2));
    const int viewTopBefore = ViewTopPx();
    REQUIRE(viewTopBefore > 0);
    WHEN("output is added to a cell above the view and only a paint runs "
         "the layout pass") {
      GroupCell *above = Nth(3);
      REQUIRE(above->GetRect().GetBottom() < viewTopBefore);
      // The user scrolled away from the evaluation to read something else:
      // what is in view is what has to stay still.
      g_ws->FollowEvaluation(false);
      g_ws->KeepCursorStillOnScreen();
      above->AppendOutput(OutputLine(1000));
      above->AppendOutput(OutputLine(1001));
      above->AppendOutput(OutputLine(1002));
      g_ws->RequestRecalculation(above);
      const long paintsBefore = Configuration::g_stats.worksheetRepaints;
      const long stalePaintsBefore = g_ws->GetStalePaintCount();
      g_ws->Refresh();
      g_ws->Update();
      ProcessEvents();
      REQUIRE(Configuration::g_stats.worksheetRepaints > paintsBefore);
      THEN("the view scrolled to keep its contents still") {
        CHECK(ViewTopPx() > viewTopBefore);
      }
      THEN("no paint was drawn for the old position") {
        CHECK(g_ws->GetStalePaintCount() == stalePaintsBefore);
      }
    }
  }
}

SCENARIO("A change below the window doesn't repaint it") {
  FillWorksheet();
  GIVEN("a view showing the top of the worksheet") {
    g_ws->Scroll(-1, 0);
    ProcessEvents();
    REQUIRE(ViewTopPx() == 0);
    int width, height;
    g_ws->GetClientSize(&width, &height);
    GroupCell *below = Nth(g_cells - 5);
    REQUIRE(below->GetRect().GetTop() > height);
    // Nothing else may be pending for the result to mean something.
    g_ws->RedrawIfRequested();
    WHEN("output is appended to a cell below the window") {
      below->AppendOutput(OutputLine(2000));
      g_ws->RequestRecalculation(below);
      g_ws->RequestRedraw(below);
      THEN("no redraw is issued") {
        CHECK_FALSE(g_ws->RedrawIfRequested());
      }
      THEN("the layout and the scroll range still follow the change") {
        const int oldBottom = Nth(g_cells - 1)->GetRect().GetBottom();
        g_ws->RedrawIfRequested();
        CHECK(Nth(g_cells - 1)->GetRect().GetBottom() > oldBottom);
      }
    }
    WHEN("output is appended to a cell in the window") {
      GroupCell *visible = Nth(1);
      visible->AppendOutput(OutputLine(3000));
      g_ws->RequestRecalculation(visible);
      g_ws->RequestRedraw(visible);
      THEN("the window is redrawn") {
        CHECK(g_ws->RedrawIfRequested());
      }
    }
    WHEN("a redraw of everything is requested") {
      g_ws->RequestRedraw();
      THEN("the window is redrawn") {
        CHECK(g_ws->RedrawIfRequested());
      }
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"), wxDefaultPosition,
                        wxSize(600, 400));
  wxClientDC dc(g_frame);
  g_cfg = new Configuration(&dc);
  g_cfg->SetZoomFactor(1.0);
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  auto *sizer = new wxBoxSizer(wxVERTICAL);
  sizer->Add(g_ws, 1, wxEXPAND);
  g_frame->SetSizer(sizer);
  g_frame->Show();
  g_frame->Layout();
  ProcessEvents();
  g_ws->UpdateConfigurationClientSize();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
