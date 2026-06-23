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
  Hardening tests for EditorCell's text-editing core.

  EditorCell is large and its mutation paths (character input, Backspace/Delete,
  word/bracket motion, Tab, undo/redo) all compute indices into m_text by hand
  and then access it with at()/Mid()/SubString(). An off-by-one there is an
  out-of-bounds access that wxASSERTs in a Debug build and is undefined behaviour
  in Release.

  Two kinds of test live here:

   1. Targeted scenarios with concrete expected results (typing, deletion,
      selection-replace, undo/redo round-trips, and the previously-unguarded
      "redo with an empty history" case).

   2. A randomized stress driver that fires long pseudo-random sequences of real
      key events at a real EditorCell and, after every single operation, checks
      the cursor/selection invariants. A wxWidgets assert handler that throws
      turns any internal out-of-bounds access into a named test failure (with the
      seed) instead of a headless hang, so the fuzz run is a genuine OOB probe.

  These exercise the real EditorCell/GroupCell/Worksheet/Configuration code
  (linked via the wxmTestApp object library).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "Worksheet.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/EditorCell.h"

#include <cstdlib>
#include <memory>
#include <random>
#include <stdexcept>
#include <unistd.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// As in test_GroupCellLayout: the recalculation/styling pipeline reaches
// Cell::GetWorksheet(), which asserts a worksheet is registered, so a single
// off-screen, event-less one is shared by every scenario for the process
// lifetime.
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
} // namespace

// wxGTK routes font/DC work through GTK, which needs an X display. When run via
// ctest the headless wrapper provides one; when run directly we start our own.
static void EnsureDisplay() {
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
}

// A fresh, active code/text GroupCell whose editor we drive. The GroupCell is
// owned by the returned unique_ptr; the editor it lends out lives as long as it.
static EditorCell *ActiveEditor(std::unique_ptr<GroupCell> &owner,
                                GroupType type, const wxString &text) {
  owner = std::make_unique<GroupCell>(g_cfg, type, text);
  EditorCell *editor = owner->GetEditable();
  REQUIRE(editor != nullptr);
  g_ws->SetActiveCell(editor);
  editor->ActivateCursor();
  return editor;
}

// Feed one printable character through the genuine key path
// (ProcessEvent -> HandleOrdinaryKey).
static void SendChar(EditorCell *e, wxChar c) {
  wxKeyEvent ev(wxEVT_CHAR);
  ev.m_uniChar = c;
  ev.m_keyCode = c;
  e->ProcessEvent(ev);
}

// Feed one non-printable key (cursor motion, Backspace, ...) with the given
// modifier state, exercising HandleSpecialKey.
static void SendKey(EditorCell *e, int keyCode, bool shift = false,
                    bool ctrl = false, bool alt = false) {
  wxKeyEvent ev(wxEVT_CHAR);
  ev.m_keyCode = keyCode;
  ev.m_uniChar = WXK_NONE;
  ev.m_shiftDown = shift;
  ev.m_controlDown = ctrl;
  ev.m_altDown = alt;
  e->ProcessEvent(ev);
}

// The one universal invariant: the caret never points past the end of the text,
// and the selection bounds are ordered and in range.
static void RequireConsistent(EditorCell *e) {
  const size_t len = e->GetValue().Length();
  REQUIRE(e->CursorPosition() <= len);
  REQUIRE(e->SelectionLeft() <= e->SelectionRight());
  REQUIRE(e->SelectionRight() <= len);
}

SCENARIO("Typing characters inserts them in order") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxEmptyString);
  WHEN("the user types 'hello'") {
    for (wxChar c : {'h', 'e', 'l', 'l', 'o'})
      SendChar(e, c);
    THEN("the text reads 'hello' and the caret is at the end") {
      REQUIRE(e->GetValue() == wxS("hello"));
      REQUIRE(e->CursorPosition() == 5);
      RequireConsistent(e);
    }
  }
}

SCENARIO("Backspace at the end of the text removes the last character") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("abc"));
  e->CaretToEnd();
  WHEN("Backspace is pressed") {
    SendKey(e, WXK_BACK);
    THEN("the last character is gone") {
      REQUIRE(e->GetValue() == wxS("ab"));
      RequireConsistent(e);
    }
  }
}

SCENARIO("Backspace at the very start of the text is a harmless no-op") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("abc"));
  e->CaretToStart();
  WHEN("Backspace is pressed with the caret at index 0") {
    SendKey(e, WXK_BACK);
    THEN("nothing is deleted and the state stays consistent") {
      REQUIRE(e->GetValue() == wxS("abc"));
      RequireConsistent(e);
    }
  }
}

// Guards the clamp-in-the-setter invariant (EditorCell.h ClampToText): the
// selection/cursor setters clamp to [0, text length]. The distinguishing case is
// CursorMove() past the start: it must land at 0, not wrap a size_t to a huge
// value that the getter would then clamp to the *end* (cursor jumping to the wrong
// end of the text).
SCENARIO("Cursor and selection positions are clamped to the text") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("abcdef")); // length 6

  GIVEN("the cursor near the start") {
    e->CursorPosition(2);
    WHEN("it is moved far past the start") {
      e->CursorMove(-100);
      THEN("it lands at the start, not the end") {
        REQUIRE(e->CursorPosition() == 0u);
      }
    }
  }
  GIVEN("a cursor position past the end") {
    e->CursorPosition(1000);
    THEN("it is clamped to the text length")
      REQUIRE(e->CursorPosition() == 6u);
  }
  GIVEN("a selection past the end") {
    e->SetSelection(1000, 2000);
    THEN("both ends clamp to the text length") {
      REQUIRE(e->SelectionStart() == 6u);
      REQUIRE(e->SelectionEnd() == 6u);
    }
  }
}

SCENARIO("Typing over a selection replaces it") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("abcdef"));
  WHEN("'cd' is selected and 'X' is typed") {
    e->SetSelection(2, 4);
    SendChar(e, 'X');
    THEN("the selection is replaced by the typed character") {
      REQUIRE(e->GetValue() == wxS("abXef"));
      RequireConsistent(e);
    }
  }
}

SCENARIO("Undo reverts the last edit and Redo re-applies it") {
  std::unique_ptr<GroupCell> owner;
  EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("start"));
  WHEN("a word is appended, then undone, then redone") {
    e->CaretToEnd();
    for (wxChar c : {'_', 'e', 'n', 'd'})
      SendChar(e, c);
    const wxString typed = e->GetValue();
    REQUIRE(typed == wxS("start_end"));

    e->Undo();
    THEN("Undo restores the earlier text") {
      REQUIRE(e->GetValue() != typed);
      RequireConsistent(e);

      AND_WHEN("Redo is issued") {
        e->Redo();
        THEN("the typed text is back") {
          REQUIRE(e->GetValue() == typed);
          RequireConsistent(e);
        }
      }
    }
  }
}

SCENARIO("Redo with nothing to redo does not crash") {
  // History::GetState() does m_history.at(m_historyPosition); after a fresh start
  // (or right after typing) m_historyPosition == m_history.size(), so an
  // unguarded Redo() that still calls GetState() would index one past the end and
  // throw std::out_of_range -> terminate. Redo() must be a safe no-op when there
  // is nothing to redo.
  GIVEN("a brand-new editor with an empty undo history") {
    std::unique_ptr<GroupCell> owner;
    EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxS("x"));
    THEN("Redo() is a harmless no-op") {
      REQUIRE_NOTHROW(e->Redo());
      RequireConsistent(e);
    }
  }
  GIVEN("an editor that was just typed into (no Undo yet)") {
    std::unique_ptr<GroupCell> owner;
    EditorCell *e = ActiveEditor(owner, GC_TYPE_CODE, wxEmptyString);
    SendChar(e, 'a');
    SendChar(e, 'b');
    THEN("Redo() with no pending redo does not throw") {
      REQUIRE_NOTHROW(e->Redo());
      REQUIRE(e->GetValue() == wxS("ab"));
      RequireConsistent(e);
    }
  }
}

// One pseudo-random edit operation. Returns nothing; the point is that none of
// these may walk off the end of m_text (which the throwing assert handler would
// turn into an exception).
static void ApplyRandomOp(EditorCell *e, std::mt19937 &rng) {
  static const wxString chars =
    wxS("ab cd()[]{}\"\\_+-*/^=:;,.123\n\t");
  std::uniform_int_distribution<int> opDist(0, 17);
  std::uniform_int_distribution<int> boolDist(0, 1);
  auto flip = [&] { return boolDist(rng) != 0; };
  // A short random string (possibly with newlines/brackets) for the bulk-edit
  // operations.
  auto randomSnippet = [&] {
    std::uniform_int_distribution<size_t> cDist(0, chars.Length() - 1);
    std::uniform_int_distribution<int> lenDist(0, 6);
    wxString s;
    int n = lenDist(rng);
    for (int i = 0; i < n; ++i)
      s += chars[cDist(rng)];
    return s;
  };

  switch (opDist(rng)) {
  case 0:
  case 1:
  case 2: {
    std::uniform_int_distribution<size_t> cDist(0, chars.Length() - 1);
    SendChar(e, chars[cDist(rng)]);
    break;
  }
  case 3:
    SendKey(e, WXK_RETURN);
    break;
  case 4:
    SendKey(e, WXK_BACK);
    break;
  case 5:
    SendKey(e, WXK_DELETE);
    break;
  case 6:
    SendKey(e, WXK_LEFT, flip(), flip(), flip());
    break;
  case 7:
    SendKey(e, WXK_RIGHT, flip(), flip(), flip());
    break;
  case 8:
    SendKey(e, flip() ? WXK_HOME : WXK_END, flip(), flip());
    break;
  case 9:
    e->Undo();
    break;
  case 10:
    e->Redo();
    break;
  case 11: {
    // Set a random (possibly empty, possibly reversed) selection. Stays within
    // the current length so this models a legitimate UI selection.
    const size_t len = e->GetValue().Length();
    std::uniform_int_distribution<size_t> pDist(0, len);
    e->SetSelection(pDist(rng), pDist(rng));
    break;
  }
  case 12:
    SendKey(e, WXK_TAB, flip());
    break;
  case 13:
    // Paste-like bulk insertion at the caret (PasteFromClipboard funnels through
    // InsertText, minus the clipboard which is unreliable headless).
    e->InsertText(randomSnippet());
    break;
  case 14: {
    // Replace every occurrence of a short fragment. oldString comes from the
    // alphabet so it sometimes actually matches.
    std::uniform_int_distribution<size_t> cDist(0, chars.Length() - 1);
    e->ReplaceAll(chars[cDist(rng)], randomSnippet(), flip());
    break;
  }
  case 15:
    // Replace the current selection (only acts if oldStr matches the selection).
    e->ReplaceSelection(e->GetSelectionString(), randomSnippet());
    break;
  case 16:
    // Splits the text at the caret and returns the tail; we discard it (the cell
    // keeps the head). Exercises the caret/substring math.
    (void)e->DivideAtCaret();
    break;
  case 17:
    // Comment / uncomment the selection (code path inserts/removes /* */).
    e->CommentSelection();
    break;
  }
}

SCENARIO("A long random edit sequence never corrupts the cursor or overruns the text") {
  // Two cell types because code and text cells use different styling/indent
  // paths (StyleTextCode vs StyleTextTexts, code-only auto-indent and bracket
  // matching).
  for (GroupType type : {GC_TYPE_CODE, GC_TYPE_TEXT}) {
    std::unique_ptr<GroupCell> owner;
    EditorCell *e = ActiveEditor(owner, type, wxS("f(x) := x^2 + 1;\n/* a comment */"));

    // A handful of independent seeds so a one-in-a-while index path is more
    // likely to be hit, while the test stays fast and deterministic.
    for (unsigned seed = 1; seed <= 8; ++seed) {
      std::mt19937 rng(seed * 7919u + static_cast<unsigned>(type));
      for (int step = 0; step < 1500; ++step) {
        INFO("cellType=" << static_cast<int>(type) << " seed=" << seed
                         << " step=" << step
                         << " text-len=" << e->GetValue().Length());
        REQUIRE_NOTHROW(ApplyRandomOp(e, rng));
        RequireConsistent(e);
        // Periodically run the real recalculation, exercising the styling /
        // soft-line-break / layout geometry on the mutated text.
        if (step % 64 == 0)
          REQUIRE_NOTHROW(e->GetGroup()->Recalculate());
      }
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};

// Turn wxWidgets asserts (e.g. an out-of-range wxString::at()) into C++
// exceptions so REQUIRE_NOTHROW reports them as failures with the offending
// seed/step instead of popping a dialog or aborting under the headless runner.
static void ThrowingAssertHandler(const wxString &file, int line,
                                  const wxString &func, const wxString &cond,
                                  const wxString &msg) {
  throw std::runtime_error(
    (file + wxS(":") + wxString::Format(wxS("%d"), line) + wxS(" in ") + func +
     wxS(": ") + cond + wxS(" ") + msg)
      .ToStdString());
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  wxSetAssertHandler(ThrowingAssertHandler);

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
