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
  Copying from and searching in the diff viewer.

  - Copy: in the main window Ctrl+C and the context menu's "Copy" are handled
    by the menu bar's handler. The diff viewer has no menu bar, so the command
    reached nothing and the clipboard stayed as it was.
  - Search: the only way to get to the next match was pressing Enter in the
    search field again, which nothing hinted at. DiffFrame::FindNext() is what
    the new "Find next"/"Find previous" buttons, Enter and F3 now call.

  Uses the real clipboard, so it needs a display (CI runs it under xvfb).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/dcmemory.h>
#include <wx/ffile.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/srchctrl.h>

#include "Configuration.h"
#include "EventIDs.h"
#include "cells/EditorCell.h"
#include "dialogs/DiffFrame.h"

#define CATCH_CONFIG_RUNNER
#define CATCH_CONFIG_ENABLE_PAIR_STRINGMAKER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;

//! Writes a .wxm file with one code cell per entry of @p cells
wxString WriteWxm(const std::vector<wxString> &cells) {
  const wxString name = wxFileName::CreateTempFileName(wxS("diffedit")) + wxS(".wxm");
  wxFFile file(name, wxS("w"));
  file.Write(wxS("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/\n"));
  for (const auto &code : cells)
    file.Write(wxS("/* [wxMaxima: input   start ] */\n") + code +
               wxS("\n/* [wxMaxima: input   end   ] */\n"));
  file.Write(wxS("\"Created with wxMaxima\"$\n"));
  return name;
}

wxString ClipboardText() {
  wxString text;
  if (wxTheClipboard->Open()) {
    wxTextDataObject data;
    if (wxTheClipboard->GetData(data))
      text = data.GetText();
    wxTheClipboard->Close();
  }
  return text;
}

void SetClipboardText(const wxString &text) {
  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(text));
    wxTheClipboard->Close();
  }
}

//! The first line of the editor the search left the cursor in, and where the
//! match starts (as std::string, which Catch can print). Only the first line:
//! the "Created with wxMaxima" trailer ends up in the last cell.
std::pair<std::string, size_t> CurrentMatch(const DiffFrame &diff) {
  const EditorCell *editor = diff.GetPane(0)->GetActiveCell();
  if (!editor)
    return {std::string(), 0};
  return {editor->GetValue().BeforeFirst(wxS('\n')).utf8_string(),
          editor->SelectionLeft()};
}

using Match = std::pair<std::string, size_t>;
} // namespace

SCENARIO("Copy in the diff viewer reaches the clipboard") {
  wxArrayString files;
  files.Add(WriteWxm({wxS("a: 1$"), wxS("copyme: 42$")}));
  files.Add(WriteWxm({wxS("a: 2$"), wxS("copyme: 42$")}));
  auto *diff = new DiffFrame(nullptr, files, g_cfg);

  GIVEN("a selection in the left pane") {
    SetClipboardText(wxS("stale"));
    diff->GetPane(0)->SelectAll();
    REQUIRE(diff->GetPane(0)->CanCopy());

    WHEN("Copy is chosen (Ctrl+C or the context menu)") {
      wxCommandEvent copy(wxEVT_MENU, wxID_COPY);
      diff->ProcessWindowEvent(copy);
      THEN("the selected code is on the clipboard") {
        CHECK(ClipboardText().Contains(wxS("copyme: 42$")));
      }
    }
    WHEN("the context menu's \"Copy as plain text\" is chosen") {
      wxCommandEvent copy(wxEVT_MENU, EventIDs::popid_copy_text);
      diff->ProcessWindowEvent(copy);
      THEN("the selected code is on the clipboard") {
        CHECK(ClipboardText().Contains(wxS("copyme: 42$")));
      }
    }
  }

  delete diff;
  for (const auto &file : files)
    wxRemoveFile(file);
}

SCENARIO("The diff viewer's search can move on to the next match") {
  wxArrayString files;
  files.Add(WriteWxm({wxS("foo: 1$"), wxS("bar: foo + foobar$")}));
  files.Add(WriteWxm({wxS("foo: 2$"), wxS("bar: foo + foobar$")}));
  auto *diff = new DiffFrame(nullptr, files, g_cfg);

  GIVEN("a search for a word that occurs several times") {
    diff->GetSearchCtrl()->ChangeValue(wxS("foo"));

    WHEN("Find next is used repeatedly") {
      REQUIRE(diff->FindNext(true));
      const auto first = CurrentMatch(*diff);
      REQUIRE(diff->FindNext(true));
      const auto second = CurrentMatch(*diff);
      REQUIRE(diff->FindNext(true));
      const auto third = CurrentMatch(*diff);
      THEN("each use moves to the next occurrence") {
        CHECK(first == Match("foo: 1$", 0));
        CHECK(second == Match("bar: foo + foobar$", 5));
        CHECK(third == Match("bar: foo + foobar$", 11));
      }
      AND_WHEN("Find previous is used") {
        REQUIRE(diff->FindNext(false));
        THEN("it goes back to the occurrence before") {
          CHECK(CurrentMatch(*diff) == Match("bar: foo + foobar$", 5));
        }
      }
    }
  }

  GIVEN("an empty search field") {
    diff->GetSearchCtrl()->ChangeValue(wxEmptyString);
    THEN("Find next finds nothing") { CHECK_FALSE(diff->FindNext(true)); }
  }

  delete diff;
  for (const auto &file : files)
    wxRemoveFile(file);
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

  // An in-memory config: nothing this test does reaches the disk.
  delete wxConfig::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                        wxEmptyString, wxEmptyString, 0));

  wxBitmap bmp(400, 400);
  wxMemoryDC dc;
  dc.SelectObject(bmp);
  g_cfg = new Configuration(&dc);
  g_cfg->MakeTemporary();

  const int result = Catch::Session().run(argc, argv);

  delete g_cfg;
  wxEntryCleanup();
  return result;
}
