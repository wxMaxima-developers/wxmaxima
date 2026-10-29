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
  Pins "Forget known answers" (GH #1018): wxMaxima remembers the answers
  given to Maxima's questions and saves them with the worksheet, so a user
  who wants to start over needs a way to drop them -- for the cell under
  the cursor, or for every cell in the selection, and for no other cell.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;

//! A code group that remembers the answer \p answer to \p question.
GroupCell *AppendAnsweredGroup(const wxString &question, const wxString &answer,
                               GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("read(a);"));
  group->SetAnswer(question, answer);
  return g_ws->InsertGroupCells(std::move(group), after);
}

//! Start every scenario from an empty worksheet with no cursor or selection.
void Reset() {
  g_ws->ClearDocument();
  g_ws->ClearSelection();
  g_ws->SetActiveCell(nullptr);
}
} // namespace

SCENARIO("A GroupCell forgets its answers") {
  GroupCell group(g_cfg, GC_TYPE_CODE, wxS("read(a); read(b);"));
  group.SetAnswer(wxS("Enter a:"), wxS("1"));
  group.SetAnswer(wxS("Enter b:"), wxS("2"));
  group.SetAutoAnswer(true);
  REQUIRE(group.ContainsSavedAnswers());
  REQUIRE(group.ToXML().Contains(wxS("answer1=")));

  group.ForgetAnswers();

  THEN("no answer is left to offer, send or save") {
    REQUIRE_FALSE(group.ContainsSavedAnswers());
    REQUIRE(group.GetAnswer(wxS("Enter a:")).empty());
    REQUIRE(group.GetAnswer(wxS("Enter b:")).empty());
    REQUIRE_FALSE(group.ToXML().Contains(wxS("question1=")));
    REQUIRE_FALSE(group.ToXML().Contains(wxS("answer1=")));
  }
  THEN("the user's choice to send known answers automatically is kept") {
    REQUIRE(group.AutoAnswer());
  }
  THEN("answers given afterwards are remembered again") {
    group.SetAnswer(wxS("Enter a:"), wxS("3"));
    REQUIRE(group.GetAnswer(wxS("Enter a:")) == wxS("3"));
  }
}

SCENARIO("Forget known answers acts on the selected cells only") {
  Reset();
  GroupCell *before = AppendAnsweredGroup(wxS("q1"), wxS("a1"), nullptr);
  GroupCell *first = AppendAnsweredGroup(wxS("q2"), wxS("a2"), before);
  auto plain = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, wxS("no answers"));
  GroupCell *text = g_ws->InsertGroupCells(std::move(plain), first);
  GroupCell *last = AppendAnsweredGroup(wxS("q3"), wxS("a3"), text);
  GroupCell *after = AppendAnsweredGroup(wxS("q4"), wxS("a4"), last);
  g_ws->SetSelection(first, last);

  const auto forgotten = g_ws->ForgetKnownAnswers();

  THEN("the cells in the selection that knew answers forget them") {
    REQUIRE(forgotten == 2);
    REQUIRE_FALSE(first->ContainsSavedAnswers());
    REQUIRE_FALSE(last->ContainsSavedAnswers());
  }
  THEN("the cells around the selection keep theirs") {
    REQUIRE(before->ContainsSavedAnswers());
    REQUIRE(after->ContainsSavedAnswers());
  }
}

SCENARIO("Forget known answers acts on the cell the cursor is in") {
  Reset();
  GroupCell *other = AppendAnsweredGroup(wxS("q1"), wxS("a1"), nullptr);
  GroupCell *current = AppendAnsweredGroup(wxS("q2"), wxS("a2"), other);
  g_ws->SetActiveCell(current->GetEditable());

  REQUIRE(g_ws->ForgetKnownAnswers() == 1);
  REQUIRE_FALSE(current->ContainsSavedAnswers());
  REQUIRE(other->ContainsSavedAnswers());

  WHEN("asked again") {
    THEN("there is nothing left to forget") {
      REQUIRE(g_ws->ForgetKnownAnswers() == 0);
    }
  }
}

SCENARIO("Forget known answers without a cursor or a selection does nothing") {
  Reset();
  GroupCell *group = AppendAnsweredGroup(wxS("q1"), wxS("a1"), nullptr);
  g_ws->ClearSelection();
  g_ws->SetActiveCell(nullptr);

  REQUIRE(g_ws->ForgetKnownAnswers() == 0);
  REQUIRE(group->ContainsSavedAnswers());
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
