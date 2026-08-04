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
  Pins Worksheet::AnonymizeCodeCells() (GH #1339): replacing every
  non-builtin variable/function name in the selected code cells with a
  random, collision-free, reproducible-within-the-run replacement, leaving
  Maxima builtins, the tokenizer's hardcoded keywords (for/then/do/...),
  string/number literals and non-code cells untouched, all as one undo step.

  These scenarios always select the cells to anonymize, deliberately never
  exercising the "nothing selected -> ask the user" confirmation dialog: a
  real modal dialog can't be driven headlessly here, so that path is verified
  manually instead (see AGENTS.md).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MaximaTokenizer.h"
#include "worksheet/Worksheet.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;

//! A code group with the given input and no output.
GroupCell *AppendCodeGroup(const wxString &code, GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, code);
  return g_ws->InsertGroupCells(std::move(group), after);
}

//! A text/heading group -- must never be touched by AnonymizeCodeCells().
GroupCell *AppendTextGroup(const wxString &text, GroupCell *after) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, text);
  return g_ws->InsertGroupCells(std::move(group), after);
}

/*! Does any token in \p code have exactly this text?

  Unlike a naive substring search, this can't produce a false positive from a
  randomly generated "anon_..." replacement that happens to end in the same
  letter(s) as \p exactText followed by whatever real character comes next in
  the source (e.g. a replacement for some other name ending in "f" right
  before a "(" would make a plain code.Contains("f(") check falsely believe
  the original "f" survived).
*/
bool HasExactToken(const wxString &code, const wxString &exactText) {
  for (auto const &tok : MaximaTokenizer(code, g_cfg).PopTokens())
    if (tok.GetText() == exactText)
      return true;
  return false;
}
} // namespace

SCENARIO("AnonymizeCodeCells renames non-builtin names consistently") {
  g_ws->ClearDocument();
  GroupCell *group = AppendCodeGroup(
    wxS("myvar: 5; f(myvar):=myvar^2+myvar;"), nullptr);
  g_ws->SetSelection(group, group);
  g_ws->RecalculateIfNeeded();

  const wxString before = group->GetEditable()->GetValue();
  g_ws->AnonymizeCodeCells();
  const wxString after = group->GetEditable()->GetValue();

  THEN("the text actually changed") { REQUIRE(after != before); }

  THEN("the user-defined names are gone") {
    REQUIRE_FALSE(HasExactToken(after, wxS("myvar")));
    REQUIRE_FALSE(HasExactToken(after, wxS("f")));
  }

  THEN("every occurrence of the same original name got the same replacement") {
    // "myvar" occurs 4 times in the source ("myvar: 5;", the "f(myvar)"
    // parameter, "myvar^2" and the trailing "+myvar"); its replacement must
    // therefore also occur exactly 4 times.
    int myvarReplacementCount = 0;
    // Whatever "myvar" became, it's a single anon_-prefixed token repeated
    // four times -- find it and count its occurrences.
    int pos = after.Find(wxS("anon_"));
    REQUIRE(pos != wxNOT_FOUND);
    wxString token = after.Mid(pos).BeforeFirst('^').BeforeFirst('+').BeforeFirst(':');
    // token now holds one full anon_ identifier ("anon_" plus 8 chars).
    REQUIRE(token.StartsWith(wxS("anon_")));
    wxString remaining = after;
    for (;;) {
      int idx = remaining.Find(token);
      if (idx == wxNOT_FOUND)
        break;
      ++myvarReplacementCount;
      remaining = remaining.Mid(idx + token.Length());
    }
    REQUIRE(myvarReplacementCount == 4);
  }
}

SCENARIO("AnonymizeCodeCells leaves Maxima builtins and syntax keywords alone") {
  g_ws->ClearDocument();
  GroupCell *group = AppendCodeGroup(
    wxS("for myindex:1 thru 3 do myresult: sin(myindex)+sqrt(2)+integrate(x,x);"),
    nullptr);
  g_ws->SetSelection(group, group);
  g_ws->RecalculateIfNeeded();

  g_ws->AnonymizeCodeCells();
  const wxString after = group->GetEditable()->GetValue();

  THEN("the loop keywords survive untouched") {
    REQUIRE(after.Contains(wxS("for ")));
    REQUIRE(after.Contains(wxS("thru")));
    REQUIRE(after.Contains(wxS("do ")));
  }
  THEN("builtin functions survive untouched") {
    REQUIRE(after.Contains(wxS("sin(")));
    REQUIRE(after.Contains(wxS("sqrt(")));
    REQUIRE(after.Contains(wxS("integrate(")));
  }
  THEN("the user-defined loop variable and result name are gone") {
    REQUIRE_FALSE(after.Contains(wxS("myindex")));
    REQUIRE_FALSE(after.Contains(wxS("myresult")));
  }
}

SCENARIO("AnonymizeCodeCells uses the same replacement across multiple cells") {
  g_ws->ClearDocument();
  GroupCell *first = AppendCodeGroup(wxS("shared_name: 1;"), nullptr);
  GroupCell *second = AppendCodeGroup(wxS("print(shared_name);"), first);
  g_ws->SetSelection(first, second);
  g_ws->RecalculateIfNeeded();

  g_ws->AnonymizeCodeCells();

  const wxString firstAfter = first->GetEditable()->GetValue();
  const wxString secondAfter = second->GetEditable()->GetValue();
  REQUIRE_FALSE(firstAfter.Contains(wxS("shared_name")));
  REQUIRE_FALSE(secondAfter.Contains(wxS("shared_name")));

  THEN("both cells got the very same replacement name") {
    int pos = firstAfter.Find(wxS("anon_"));
    REQUIRE(pos != wxNOT_FOUND);
    wxString replacement = firstAfter.Mid(pos, 13); // "anon_" + 1 letter + 7 alnum
    REQUIRE(secondAfter.Contains(replacement));
  }
}

SCENARIO("AnonymizeCodeCells only touches code cells, and only within the selection") {
  g_ws->ClearDocument();
  GroupCell *text = AppendTextGroup(wxS("please anonymize secret_topic"), nullptr);
  GroupCell *inSelection = AppendCodeGroup(wxS("secret_topic: 1;"), text);
  GroupCell *outsideSelection = AppendCodeGroup(wxS("secret_topic: 2;"), inSelection);
  // Select only the text cell and the first code cell -- not the second.
  g_ws->SetSelection(text, inSelection);
  g_ws->RecalculateIfNeeded();

  g_ws->AnonymizeCodeCells();

  THEN("the text cell is untouched even though it contains the same word") {
    REQUIRE(text->GetEditable()->GetValue().Contains(wxS("secret_topic")));
  }
  THEN("the selected code cell was anonymized") {
    REQUIRE_FALSE(inSelection->GetEditable()->GetValue().Contains(wxS("secret_topic")));
  }
  THEN("the code cell outside the selection was left alone") {
    REQUIRE(outsideSelection->GetEditable()->GetValue().Contains(wxS("secret_topic")));
  }
}

SCENARIO("AnonymizeCodeCells groups all its edits into a single undo step") {
  g_ws->ClearDocument();
  GroupCell *first = AppendCodeGroup(wxS("shared_name: 1;"), nullptr);
  GroupCell *second = AppendCodeGroup(wxS("print(shared_name);"), first);
  g_ws->SetSelection(first, second);
  g_ws->RecalculateIfNeeded();

  const wxString firstBefore = first->GetEditable()->GetValue();
  const wxString secondBefore = second->GetEditable()->GetValue();

  g_ws->AnonymizeCodeCells();
  REQUIRE(first->GetEditable()->GetValue() != firstBefore);
  REQUIRE(second->GetEditable()->GetValue() != secondBefore);

  g_ws->ClearSelection();
  g_ws->SetActiveCell(nullptr);
  g_ws->Undo();

  THEN("a single Undo restores both cells at once") {
    REQUIRE(first->GetEditable()->GetValue() == firstBefore);
    REQUIRE(second->GetEditable()->GetValue() == secondBefore);
  }
}

SCENARIO("AnonymizeCodeCells does nothing when nothing needs renaming") {
  g_ws->ClearDocument();
  GroupCell *group = AppendCodeGroup(wxS("sin(1)+sqrt(2);"), nullptr);
  g_ws->SetSelection(group, group);
  g_ws->RecalculateIfNeeded();

  const wxString before = group->GetEditable()->GetValue();
  g_ws->AnonymizeCodeCells();

  THEN("the text is unchanged") {
    REQUIRE(group->GetEditable()->GetValue() == before);
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

  // AnonymizeCodeCells() needs a real builtin-symbol list to tell user names
  // apart from Maxima's own. Worksheet::LoadSymbols() would do that too, but
  // it also kicks off directory scans for load/demo files (looking for a
  // real Maxima installation's share/demo directories) that this headless
  // test has no use for and that can stall for a long time here -- call the
  // narrower, synchronous, filesystem-free AutoComplete::LoadBuiltinSymbols()
  // directly instead.
  g_ws->GetAutocomplete().LoadBuiltinSymbols();

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
