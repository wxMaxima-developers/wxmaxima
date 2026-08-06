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
  Regression tests for the table of contents sidebar's drag-and-drop reorder
  (GH #1524).

  The feature was wired up (drag image, scroll-while-dragging, reordered-
  preview rendering) well before it was ever exercised end-to-end, and two
  independent bugs in Worksheet::TOCdnd() -- the function that performs the
  actual reorder -- made it either a no-op or silently data-corrupting:

  - TOCdnd() used to require a pre-existing worksheet selection before doing
    anything, even though it builds its own selection from dndStart a few
    lines later. A TOC-driven drag normally starts with no such selection
    active, so every drop was silently swallowed.
  - TOCdnd()'s selection-extension loop used to compare each next cell's
    heading rank against whatever cell the selection currently ended on (a
    moving target) instead of the originally dragged heading's rank. Once
    the selection had absorbed the dragged heading's own content cell, a
    sub-heading right after it was compared against that content cell's
    rank instead and always judged "not part of this chapter", leaving it
    (and its own content) behind during the move.

  A third bug lived in TableOfContents' own mouse handlers: dropping at or
  near the end of the list clamped to a position near the drag's own start
  instead of the end. That logic is pinned here too, extracted into the
  pure ClampDropIndex() helper so it needs no simulated mouse events at all.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"
#include "sidebars/TableOfContents.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#include <vector>

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

//! Appends a heading with one code cell of content after *after, and
//! advances *after past that content cell.
static void AppendHeading(GroupType type, const wxString &title,
                          const wxString &code, GroupCell **after) {
  auto heading = std::make_unique<GroupCell>(g_cfg, type, title);
  GroupCell *headingCell = g_ws->InsertGroupCells(std::move(heading), *after);
  *after = g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, code), headingCell);
}

//! The first group cell whose editable text is exactly title.
static GroupCell *FindHeading(const wxString &title) {
  for (GroupCell *g = g_ws->GetTree(); g; g = g->GetNext())
    if (g->GetEditable() && g->GetEditable()->ToString(true) == title)
      return g;
  return nullptr;
}

//! A fresh document: Title, SectionA (+content), SubsectionA1 (+content),
//! SectionB (+content), SectionC (+content) -- the same shape used to find
//! and confirm the fix for GH #1524 in a live session.
static void BuildDocument() {
  g_ws->ClearDocument();
  GroupCell *pos = g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_TITLE, wxS("Test Document")),
    nullptr);
  AppendHeading(GC_TYPE_SECTION, wxS("SectionA"), wxS("a: 1;"), &pos);
  AppendHeading(GC_TYPE_SUBSECTION, wxS("SubsectionA1"), wxS("a1: 11;"), &pos);
  AppendHeading(GC_TYPE_SECTION, wxS("SectionB"), wxS("b: 2;"), &pos);
  AppendHeading(GC_TYPE_SECTION, wxS("SectionC"), wxS("c: 3;"), &pos);
  g_ws->SetActiveCell(nullptr);
  g_ws->ClearSelection();
  g_ws->RecalculateIfNeeded();
}

//! The document's group cells, in order, by their editable text -- compact
//! enough to compare the whole tree shape in one assertion.
static std::vector<wxString> DocumentOrder() {
  std::vector<wxString> result;
  for (GroupCell *g = g_ws->GetTree(); g; g = g->GetNext())
    result.push_back(g->GetEditable() ? g->GetEditable()->ToString(true)
                                      : wxString());
  return result;
}

SCENARIO("TOCdnd moves a chapter with no pre-existing worksheet selection") {
  BuildDocument();
  REQUIRE(g_ws->GetSelectionStart() == nullptr);
  REQUIRE(g_ws->GetSelectionEnd() == nullptr);

  GroupCell *sectionB = FindHeading(wxS("SectionB"));
  GroupCell *sectionC = FindHeading(wxS("SectionC"));
  REQUIRE(sectionB != nullptr);
  REQUIRE(sectionC != nullptr);

  // Move SectionB (+ its own content) to after SectionC's content, i.e. to
  // the very end of the document.
  g_ws->TOCdnd(sectionB, sectionC->GetNext());
  g_ws->RecalculateIfNeeded();

  THEN("the section actually moved") {
    REQUIRE(DocumentOrder() == std::vector<wxString>{
      wxS("Test Document"), wxS("SectionA"), wxS("a: 1;"),
      wxS("SubsectionA1"), wxS("a1: 11;"),
      wxS("SectionC"), wxS("c: 3;"),
      wxS("SectionB"), wxS("b: 2;")});
  }
}

SCENARIO("TOCdnd keeps a nested sub-heading attached to its parent when moving") {
  BuildDocument();
  GroupCell *sectionA = FindHeading(wxS("SectionA"));
  GroupCell *sectionC = FindHeading(wxS("SectionC"));
  REQUIRE(sectionA != nullptr);
  REQUIRE(sectionC != nullptr);

  // Move SectionA to the very end. It should take its own content cell and
  // its nested SubsectionA1 (+ that subsection's own content) along with it.
  g_ws->TOCdnd(sectionA, sectionC->GetNext());
  g_ws->RecalculateIfNeeded();

  THEN("SectionA, its content and its subsection all move together, in order") {
    REQUIRE(DocumentOrder() == std::vector<wxString>{
      wxS("Test Document"),
      wxS("SectionB"), wxS("b: 2;"),
      wxS("SectionC"), wxS("c: 3;"),
      wxS("SectionA"), wxS("a: 1;"),
      wxS("SubsectionA1"), wxS("a1: 11;")});
  }
}

SCENARIO("TOCdnd moves a chapter to the very top of the document") {
  BuildDocument();
  GroupCell *sectionC = FindHeading(wxS("SectionC"));
  REQUIRE(sectionC != nullptr);

  // dndEnd == nullptr means "insert at the top of the document" (see
  // Worksheet::InsertGroupCells()'s own doc comment).
  g_ws->TOCdnd(sectionC, nullptr);
  g_ws->RecalculateIfNeeded();

  THEN("SectionC (+ its content) now leads the document, ahead of the title") {
    REQUIRE(DocumentOrder() == std::vector<wxString>{
      wxS("SectionC"), wxS("c: 3;"),
      wxS("Test Document"),
      wxS("SectionA"), wxS("a: 1;"),
      wxS("SubsectionA1"), wxS("a1: 11;"),
      wxS("SectionB"), wxS("b: 2;")});
  }
}

SCENARIO("ClampDropIndex clamps an out-of-range drop to the end of the list, not the start") {
  // 5 displayed items, dragging 2 of them (a chapter heading + one nested
  // sub-heading) -- 3 "other" items are left once the dragged block is
  // removed, so slot 3 is the last valid drop target (append at the end).
  CHECK(TableOfContents::ClampDropIndex(0, 5, 2) == 0);
  CHECK(TableOfContents::ClampDropIndex(2, 5, 2) == 2);
  CHECK(TableOfContents::ClampDropIndex(3, 5, 2) == 3);
  // At or past the last valid slot clamps to that slot itself, not to
  // numberOfCaptionsDragged - 1 (a position near the *start* of the list --
  // the bug that made "drop near the end" silently no-op, GH #1524).
  CHECK(TableOfContents::ClampDropIndex(4, 5, 2) == 3);
  CHECK(TableOfContents::ClampDropIndex(100, 5, 2) == 3);
  // A "no item hit" result (HitTest() returning a negative flag) passes
  // through unchanged.
  CHECK(TableOfContents::ClampDropIndex(-1, 5, 2) == -1);
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
  wxInitAllImageHandlers();

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
