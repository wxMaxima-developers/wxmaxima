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
  Headless tests for the full WorksheetLayout pipeline.

  The point of the WorksheetLayout extraction is that the complete
  request -> recalculate -> resize pipeline can run without any Worksheet
  window: real GroupCells are laid out against a Configuration whose drawing
  context is an off-screen wxMemoryDC, and everything the engine tells the
  window goes through a MockView that just records it. Unlike
  test_GroupCellLayout there is no wxFrame and no Worksheet instance here at
  all - which is exactly what these tests pin down.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "worksheet/WorksheetLayout.h"
#include "cells/CellList.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#include <memory>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// The cell layer needs a Configuration with a valid recalc DC (font metrics)
// and a CellPointers registry - but, since the WorksheetLayout split, no
// Worksheet. Owned for the process lifetime.
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
} // namespace

// wxGTK routes font/DC work through GTK, which needs an X display. When run via
// ctest the headless wrapper provides one; when run directly we start our own.
static void EnsureDisplay() {
#ifndef _WIN32
  // Windows runners have a real desktop session, so this is a no-op there
  // (and Xvfb/setenv/sleep are POSIX-only anyway).
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
}

namespace {
//! Records everything the engine pushes to the window; injects a client size.
class MockView : public WorksheetView {
public:
  int clientW = 500, clientH = 800;
  int scrollUnitY = 0;

  int setVirtualCalls = 0;
  int lastSetW = -1, lastSetH = -1;
  int setScrollRateCalls = 0;
  int lastScrollRate = -1;

  void GetViewClientSize(int *w, int *h) const override {
    *w = clientW;
    *h = clientH;
  }
  int GetViewScrollUnitY() const override { return scrollUnitY; }
  void SetViewVirtualSize(int w, int h) override {
    ++setVirtualCalls;
    lastSetW = w;
    lastSetH = h;
  }
  void SetViewScrollRate(int rate) override {
    ++setScrollRateCalls;
    lastScrollRate = rate;
  }
  void GetViewPosition(int *x, int *y) const override {
    *x = 0;
    *y = 0;
  }
};

GroupCell *LastGroup(GroupCell *tree) {
  GroupCell *last = tree;
  while (last && last->GetNext())
    last = last->GetNext();
  return last;
}

/*! A worksheet-less layout setup: a locally owned cell tree, a MockView and a
  WorksheetLayout bound to both, with the two Configuration callbacks (the ones
  Worksheet's ctor normally registers) wired straight to the engine - proving
  the cell layer's height-change notifications are engine-complete. */
struct EngineFixture {
  MockView view;
  std::unique_ptr<GroupCell> tree;
  WorksheetLayout layout;

  EngineFixture()
    : layout(g_cfg, view, [this] { return tree.get(); },
             [this] { return LastGroup(tree.get()); }) {
    g_cfg->SetRecalculateRequestCallback(
      [this](GroupCell *group) { layout.RequestRecalculation(group); });
    g_cfg->SetRecalculateAllRequestCallback(
      [this] { layout.RequestFullRecalculation(); });
    g_cfg->SetAdjustWorksheetSizeRequestCallback(
      [this] { layout.RequestAdjustSize(); });
    g_cfg->SetCanvasSize(wxSize(1000, 800));
  }
  ~EngineFixture() {
    g_cfg->SetRecalculateRequestCallback({});
    g_cfg->SetRecalculateAllRequestCallback({});
    g_cfg->SetAdjustWorksheetSizeRequestCallback({});
  }

  //! Append a dirty code cell holding \p text to the tree.
  GroupCell *AddCell(const wxString &text) {
    auto cell = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, text);
    GroupCell *raw = cell.get();
    raw->MarkNeedsRecalculate();
    if (!tree)
      tree = std::move(cell);
    else
      CellList::AppendCell(LastGroup(tree.get()), std::move(cell));
    return raw;
  }
};
} // namespace

SCENARIO("The full pipeline lays out cells and pushes a virtual size - windowless") {
  GIVEN("three dirty code cells, an engine and a mock view") {
    EngineFixture f;
    GroupCell *A = f.AddCell(wxS("a:1$"));
    GroupCell *B = f.AddCell(wxS("b:2$"));
    GroupCell *C = f.AddCell(wxS("c:3$"));
    f.layout.RequestRecalculation(A);

    WHEN("a one-shot recalculation runs") {
      REQUIRE(f.layout.RecalculateIfNeeded());

      THEN("every cell is laid out") {
        REQUIRE_FALSE(A->NeedsRecalculation());
        REQUIRE_FALSE(B->NeedsRecalculation());
        REQUIRE_FALSE(C->NeedsRecalculation());
        REQUIRE(C->GetCurrentPoint().y > A->GetCurrentPoint().y);
      }
      THEN("the mock view received the resulting scroll geometry") {
        REQUIRE(f.view.setVirtualCalls >= 1);
        // Short document: the scrollbar-always-active invariant pins the
        // height to clientH + 10.
        REQUIRE(f.view.lastSetH >= f.view.clientH + 10);
        REQUIRE(f.view.lastSetW >= g_cfg->GetBaseIndent());
        REQUIRE(f.view.setScrollRateCalls >= 1);
        REQUIRE(f.layout.GetScrollUnit() >= 10);
        REQUIRE(f.view.lastScrollRate == f.layout.GetScrollUnit());
      }
    }
  }
}

SCENARIO("A time-sliced pass resumes across yields - windowless") {
  // Windowless port of test_GroupCellLayout's resume-point regression test: a
  // negative time budget forces a yield after every cell, so only an engine
  // that keeps its resume point reaches all three cells.
  GIVEN("three dirty code cells") {
    EngineFixture f;
    GroupCell *A = f.AddCell(wxS("a:1$"));
    GroupCell *B = f.AddCell(wxS("b:2$"));
    GroupCell *C = f.AddCell(wxS("c:3$"));
    f.layout.RequestRecalculation(A);

    WHEN("the time-sliced pass is driven one cell per slice") {
      int ticks = 0;
      while (f.layout.RecalculateIfNeeded(/*timeout=*/true, /*timeSliceMs=*/-1) &&
             ++ticks < 100)
        ;

      THEN("every cell was reached, across several resuming slices") {
        REQUIRE_FALSE(A->NeedsRecalculation());
        REQUIRE_FALSE(B->NeedsRecalculation());
        REQUIRE_FALSE(C->NeedsRecalculation());
        REQUIRE(ticks >= 3);
        REQUIRE(f.view.setVirtualCalls >= 1);
      }
    }
  }
}

SCENARIO("An unchanged virtual size is not re-applied to the view") {
  GIVEN("a laid-out document") {
    EngineFixture f;
    f.layout.RequestRecalculation(f.AddCell(wxS("a:1$")));
    REQUIRE(f.layout.RecalculateIfNeeded());
    const int callsAfterLayout = f.view.setVirtualCalls;
    REQUIRE(callsAfterLayout >= 1);

    WHEN("AdjustSize() runs again with nothing changed") {
      f.layout.AdjustSize();

      THEN("the view is not touched a second time") {
        REQUIRE(f.view.setVirtualCalls == callsAfterLayout);
      }
    }
  }
}

SCENARIO("A cell growing taller reaches the view through the callbacks") {
  GIVEN("a laid-out document on a small client area") {
    EngineFixture f;
    // A small client height makes the virtual height track the document
    // instead of being pinned to the clientH + 10 minimum, so growth shows.
    f.view.clientH = 100;
    GroupCell *A = f.AddCell(wxS("a:1$"));
    f.AddCell(wxS("b:2$"));
    f.layout.RequestRecalculation(A);
    REQUIRE(f.layout.RecalculateIfNeeded());
    const int heightBefore = f.view.lastSetH;
    REQUIRE(heightBefore > 0);

    WHEN("the first cell's input gains a line (the worksheet keypress path)") {
      EditorCell *editor = A->GetEditable();
      REQUIRE(editor != nullptr);
      wxKeyEvent ev(wxEVT_CHAR);
      ev.m_keyCode = WXK_RETURN;
      ev.m_uniChar = WXK_RETURN;
      editor->ProcessEvent(ev);
      editor->Recalculate(g_cfg->GetDefaultFontSize());
      // InputHeightChanged() notifies the view solely through the
      // Configuration callbacks - which the fixture wired to the engine.
      A->InputHeightChanged();
      f.layout.RecalculateIfNeeded();
      f.layout.AdjustSize();

      THEN("the view received a taller virtual size") {
        REQUIRE(f.view.lastSetH > heightBefore);
      }
    }
  }
}

SCENARIO("AdjustSize() defers while a recalculation is pending") {
  GIVEN("a scheduled but not yet executed recalculation") {
    EngineFixture f;
    f.layout.RequestRecalculation(f.AddCell(wxS("a:1$")));

    WHEN("AdjustSize() is called with the cell positions still stale") {
      f.layout.AdjustSize();

      THEN("nothing is pushed to the view yet") {
        REQUIRE(f.view.setVirtualCalls == 0);
      }
      AND_WHEN("the recalculation actually runs") {
        REQUIRE(f.layout.RecalculateIfNeeded());

        THEN("the deferred size adjustment is applied") {
          REQUIRE(f.view.setVirtualCalls >= 1);
        }
      }
    }
  }
}

SCENARIO("A zero-sized canvas blocks the layout walk") {
  GIVEN("a dirty cell but no canvas size") {
    EngineFixture f;
    f.layout.RequestRecalculation(f.AddCell(wxS("a:1$")));
    g_cfg->SetCanvasSize(wxSize(0, 0));

    WHEN("a recalculation is attempted") {
      const bool worked = f.layout.RecalculateIfNeeded();

      THEN("it reports no work done and leaves the view untouched") {
        REQUIRE_FALSE(worked);
        REQUIRE(f.view.setVirtualCalls == 0);
      }
    }
  }
}

// The recalculation walk must stay proportional to what actually changed: a
// localized edit should re-lay-out only the cell(s) that changed, never the
// whole worksheet. This has regressed before (an operation marking the entire
// tree dirty, or a stray full RequestRecalculation()), and on a large document
// a full re-layout on every keystroke / output line is a very visible lag.
// GetLastCellsRecalculated() reports how many cells the last pass expensively
// re-laid-out, which is what these tests pin.
SCENARIO("A localized change recalculates only the cell that changed") {
  GIVEN("a fully laid-out worksheet of many cells") {
    EngineFixture f;
    std::vector<GroupCell *> cells;
    for (int i = 0; i < 30; i++)
      cells.push_back(f.AddCell(wxString::Format(wxS("x%d:%d$"), i, i)));
    f.layout.RequestRecalculation(cells.front());
    // Drive the pass(es) to completion so every cell is clean.
    while (f.layout.RecalculateIfNeeded())
      ;

    THEN("the initial full layout recalculated every cell") {
      REQUIRE(f.layout.GetLastCellsRecalculated() == 30);
    }

    WHEN("a single cell in the middle is marked dirty and laid out again") {
      f.layout.RequestRecalculation(cells[15]);
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("only that one cell is expensively recalculated") {
        REQUIRE(f.layout.GetLastCellsRecalculated() == 1);
      }
      AND_THEN("cells above the change are not even visited") {
        // The walk starts at the changed cell, so it never touches the 15
        // cells above it - only the change and the cells below get visited.
        REQUIRE(f.layout.GetLastCellsVisited() <= 30 - 15);
      }
    }
  }
}

SCENARIO("The layout pass stops early once the dirty range is covered") {
  // The engine tracks the dirty range's END as well as its start (every
  // dirty-marking path reports through RequestRecalculation()), so a pass
  // that has processed the last dirty cell - and has no reposition
  // propagation pending - must stop instead of scanning to the end of the
  // document. On a long document this is the difference between an edit
  // costing O(cells below the edit) and O(1).
  GIVEN("a fully laid-out worksheet of 30 cells") {
    EngineFixture f;
    std::vector<GroupCell *> cells;
    for (int i = 0; i < 30; i++)
      cells.push_back(f.AddCell(wxString::Format(wxS("z%d:%d$"), i, i)));
    f.layout.RequestRecalculation(cells.front());
    while (f.layout.RecalculateIfNeeded())
      ;

    WHEN("one mid-document cell is re-laid-out without a height change") {
      f.layout.RequestRecalculation(cells[15]);
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("the pass stops right after the dirty range instead of scanning "
           "to the end of the document") {
        REQUIRE(f.layout.GetLastCellsRecalculated() == 1);
        // The changed cell plus at most one cell below it (to see that the
        // reposition propagation has settled).
        REQUIRE(f.layout.GetLastCellsVisited() <= 2);
      }
    }

    WHEN("two disjoint cells are marked dirty") {
      f.layout.RequestRecalculation(cells[10]);
      f.layout.RequestRecalculation(cells[20]);
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("one pass covers the range between them and stops there") {
        REQUIRE(f.layout.GetLastCellsRecalculated() == 2);
        // Cells 10..20 plus at most one settling cell below.
        REQUIRE(f.layout.GetLastCellsVisited() <= 12);
      }
    }

    WHEN("a whole-document recalculation is requested") {
      f.layout.RequestFullRecalculation();
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("the pass visits every cell (the range is open-ended)") {
        REQUIRE(f.layout.GetLastCellsVisited() == 30);
      }
    }
  }
}

SCENARIO("A global invalidation recalculates everything (the costly path)") {
  GIVEN("a fully laid-out worksheet") {
    EngineFixture f;
    std::vector<GroupCell *> cells;
    for (int i = 0; i < 12; i++)
      cells.push_back(f.AddCell(wxString::Format(wxS("y%d:%d$"), i, i)));
    f.layout.RequestRecalculation(cells.front());
    while (f.layout.RecalculateIfNeeded())
      ;

    WHEN("every cell is marked dirty (as a font / zoom change does via "
         "ResetSizeList) and the head is scheduled") {
      for (GroupCell *c : cells)
        c->MarkNeedsRecalculate();
      f.layout.RequestRecalculation(cells.front());
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("every cell is recalculated - which is why a full invalidation is "
           "reserved for genuinely global changes, not per-cell edits") {
        REQUIRE(f.layout.GetLastCellsRecalculated() == 12);
      }
    }
  }
}

SCENARIO("A config-counter bump while a bounded range is pending is absorbed fully") {
  // RecalculateForce() (Configuration::RecalculateForce, called by every
  // config setter - zoom, label width, ...) bumps a GLOBAL counter that flips
  // every cell's ConfigChanged() to true at once, with no per-cell
  // notification to the layout engine. If a bounded dirty range is pending
  // when that happens, the early stop must NOT leave the cells outside the
  // range reporting dirty forever - that is the exact invariant the
  // RecalculateIfNeeded debug tripwire caught firing in CI's --debug
  // integration tests.
  GIVEN("a fully laid-out worksheet of 20 cells") {
    EngineFixture f;
    std::vector<GroupCell *> cells;
    for (int i = 0; i < 20; i++)
      cells.push_back(f.AddCell(wxString::Format(wxS("w%d:%d$"), i, i)));
    f.layout.RequestRecalculation(cells.front());
    while (f.layout.RecalculateIfNeeded())
      ;
    // Every cell is clean now.
    for (GroupCell *c : cells)
      REQUIRE(!c->NeedsRecalculation());

    WHEN("a mid-document cell is scheduled and then a global config bump "
         "happens before the pass runs") {
      f.layout.RequestRecalculation(cells[10]);
      g_cfg->RecalculateForce(); // bumps CellCfgCnt: all cells now ConfigChanged
      REQUIRE(cells[19]->NeedsRecalculation()); // sanity: the bump took effect
      while (f.layout.RecalculateIfNeeded())
        ;

      THEN("the pass absorbs the global invalidation - no cell is left dirty") {
        for (GroupCell *c : cells)
          REQUIRE(!c->NeedsRecalculation());
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
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  // The cell layer dereferences the CellPointers registry unconditionally; a
  // null canvas is fine (it is only used by AnimationCell's timer). What we
  // deliberately do NOT create here: a Worksheet or any other window.
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
