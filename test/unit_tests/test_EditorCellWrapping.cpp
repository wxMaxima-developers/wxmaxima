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
  Tests for automatic line-wrapping inside EditorCells.

  A soft line break is the character '\r' written in place over a space in the
  cell's text (a hard break is '\n'); StyleText() scrubs and re-derives the
  soft breaks on every restyle, so wrapping is transient layout state that
  must never change the cell's content or leak into serialization, the
  clipboard or the selection string. These tests pin exactly that, for the
  prose path (which always worked) and the code path (formerly disabled
  prototype code).

  Windowless: real GroupCell/EditorCell against a memory-DC Configuration,
  no Worksheet, no wxFrame - the test_WorksheetLayout pattern.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#include <memory>
#include <vector>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
// The cell layer needs a Configuration with a valid recalc DC (font metrics)
// and a CellPointers registry, but no Worksheet. Owned for the process
// lifetime.
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

//! Saves the auto-wrap mode and canvas size; restores both on scope exit.
struct WrapSetupGuard {
  long savedAutoWrap;
  wxSize savedCanvas;
  WrapSetupGuard()
    : savedAutoWrap(g_cfg->GetAutoWrap() ? (g_cfg->GetAutoWrapCode() ? 2 : 1) : 0),
      savedCanvas(g_cfg->GetCanvasSize()) {}
  ~WrapSetupGuard() {
    g_cfg->SetAutoWrap(savedAutoWrap);
    g_cfg->SetCanvasSize(savedCanvas);
  }
};

//! A canvas narrow enough that a sentence must wrap several times.
void Narrow() { g_cfg->SetCanvasSize(wxSize(300, 800)); }
/*! A canvas for the code scenarios: narrow enough that long code lines must
  wrap, but wide enough that a single identifier plus the (clamped)
  continuation indentation always fits - so the assertions test the wrap
  logic, not the font metrics. */
void CodeNarrow() { g_cfg->SetCanvasSize(wxSize(500, 800)); }
//! A canvas wide enough that ordinary lines never wrap.
void Wide() { g_cfg->SetCanvasSize(wxSize(4000, 800)); }

//! The indices of the soft line breaks ('\r') in the editor's text.
std::vector<size_t> Breaks(const EditorCell *editor) {
  std::vector<size_t> breaks;
  const wxString &value = editor->GetValue();
  for (size_t i = 0; i < value.Length(); i++)
    if (value[i] == wxS('\r'))
      breaks.push_back(i);
  return breaks;
}

//! Force a full restyle at the current canvas width.
void Restyle(EditorCell *editor) {
  editor->ResetSize();
  editor->Recalculate(g_cfg->GetDefaultFontSize());
}

//! A longish code line with plenty of soft-break candidates (spaces).
wxString LongCodeLine() {
  wxString text = wxS("result: function_with_a_name(");
  for (int i = 0; i < 30; i++)
    text += wxString::Format(wxS("argument_%d + "), i);
  text += wxS("0);");
  return text;
}

//! A longish prose sentence.
wxString LongProseLine() {
  wxString text;
  for (int i = 0; i < 30; i++)
    text += wxS("several words of ordinary prose ");
  return text;
}

} // namespace

SCENARIO("Prose cells wrap when auto-wrap is on and stay intact") {
  WrapSetupGuard guard;
  GIVEN("a long one-line text cell on a narrow canvas") {
    const wxString original = LongProseLine();

    WHEN("auto-wrap is set to text-only") {
      g_cfg->SetAutoWrap(1);
      Narrow();
      GroupCell group(g_cfg, GC_TYPE_TEXT, original);
      EditorCell *editor = group.GetEditable();
      REQUIRE(editor != nullptr);
      group.Recalculate();

      THEN("soft breaks appear, each replacing a space in place") {
        const std::vector<size_t> breaks = Breaks(editor);
        REQUIRE(breaks.size() > 0);
        wxString value = editor->GetValue();
        REQUIRE(value.Length() == original.Length());
        for (size_t breakPos : breaks)
          REQUIRE(original[breakPos] == wxS(' '));
        value.Replace(wxS("\r"), wxS(" "));
        REQUIRE(value == original);
      }
    }

    WHEN("auto-wrap is off") {
      g_cfg->SetAutoWrap(0);
      Narrow();
      GroupCell group(g_cfg, GC_TYPE_TEXT, original);
      EditorCell *editor = group.GetEditable();
      REQUIRE(editor != nullptr);
      group.Recalculate();

      THEN("no soft break is inserted") {
        REQUIRE(Breaks(editor).empty());
        REQUIRE(editor->GetValue() == original);
      }
    }
  }
}

SCENARIO("Code cells do not wrap while auto-wrap covers text only") {
  // Code wrapping is opt-in (auto-wrap mode 2); the text-only mode must
  // leave code cells strictly alone.
  WrapSetupGuard guard;
  GIVEN("a long one-line code cell on a narrow canvas, auto-wrap text-only") {
    g_cfg->SetAutoWrap(1);
    Narrow();
    const wxString original = LongCodeLine();
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    EditorCell *editor = group.GetEditable();
    REQUIRE(editor != nullptr);
    group.Recalculate();

    THEN("the code cell keeps its single line") {
      REQUIRE(Breaks(editor).empty());
      REQUIRE(editor->GetValue() == original);
    }
  }
}

SCENARIO("Runs of consecutive spaces survive styling at any width") {
  // Regression guard for the historical crash (a29349a94) and for soft
  // breaks being written past the last space of a run: space runs near the
  // wrap column and as the very last characters of the cell.
  WrapSetupGuard guard;
  GIVEN("cells whose text contains space runs of various lengths") {
    Narrow();
    wxString spaceRuns;
    for (int run = 2; run <= 10; run++) {
      spaceRuns += wxS("word");
      spaceRuns += wxString(wxS(' '), run);
    }
    spaceRuns += wxString(wxS(' '), 5); // space run as the final token

    WHEN("a text cell is styled with wrapping on") {
      g_cfg->SetAutoWrap(1);
      GroupCell group(g_cfg, GC_TYPE_TEXT, spaceRuns);
      REQUIRE_NOTHROW(group.Recalculate());

      THEN("the content is preserved modulo soft breaks") {
        wxString value = group.GetEditable()->GetValue();
        REQUIRE(value.Length() == spaceRuns.Length());
        value.Replace(wxS("\r"), wxS(" "));
        REQUIRE(value == spaceRuns);
      }
    }

    WHEN("a code cell is styled") {
      g_cfg->SetAutoWrap(2);
      GroupCell group(g_cfg, GC_TYPE_CODE, spaceRuns);
      REQUIRE_NOTHROW(group.Recalculate());

      THEN("the content is preserved modulo soft breaks") {
        wxString value = group.GetEditable()->GetValue();
        REQUIRE(value.Length() == spaceRuns.Length());
        value.Replace(wxS("\r"), wxS(" "));
        REQUIRE(value == spaceRuns);
      }
    }
  }
}

SCENARIO("Soft breaks never leak out of a wrapped cell") {
  WrapSetupGuard guard;
  GIVEN("a wrapped prose cell") {
    g_cfg->SetAutoWrap(1);
    Narrow();
    const wxString original = LongProseLine();
    GroupCell group(g_cfg, GC_TYPE_TEXT, original);
    EditorCell *editor = group.GetEditable();
    group.Recalculate();
    REQUIRE(Breaks(editor).size() > 0); // the premise: it did wrap

    THEN("ToString restores the original text") {
      wxString str = editor->ToString();
      REQUIRE(str.Find(wxS('\r')) == wxNOT_FOUND);
      REQUIRE(str == original);
    }
    THEN("ToXML contains no soft break") {
      REQUIRE(editor->ToXML().Find(wxS('\r')) == wxNOT_FOUND);
    }
    THEN("the selection string sees spaces, not soft breaks") {
      editor->SetSelection(0, editor->GetValue().Length());
      wxString sel = editor->GetSelectionString();
      REQUIRE(sel.Find(wxS('\r')) == wxNOT_FOUND);
      REQUIRE(sel == original);
    }
  }
}

SCENARIO("Caret positions round-trip across soft breaks") {
  WrapSetupGuard guard;
  GIVEN("a wrapped prose cell") {
    g_cfg->SetAutoWrap(1);
    Narrow();
    GroupCell group(g_cfg, GC_TYPE_TEXT, LongProseLine());
    EditorCell *editor = group.GetEditable();
    group.Recalculate();
    const std::vector<size_t> breaks = Breaks(editor);
    REQUIRE(breaks.size() > 0);

    THEN("XYToPosition(PositionToXY(p)) == p for every position") {
      const size_t len = editor->GetValue().Length();
      for (size_t p = 0; p <= len; p++) {
        size_t x = 0, y = 0;
        editor->PositionToXY(p, &x, &y);
        REQUIRE(editor->XYToPosition(x, y) == p);
      }
    }
    THEN("the line index increases across each soft break") {
      for (size_t breakPos : breaks) {
        size_t xBefore = 0, yBefore = 0, xAfter = 0, yAfter = 0;
        editor->PositionToXY(breakPos, &xBefore, &yBefore);
        editor->PositionToXY(breakPos + 1, &xAfter, &yAfter);
        REQUIRE(yAfter == yBefore + 1);
      }
    }
  }
}

SCENARIO("Re-styling at a different width re-derives the wrapping") {
  WrapSetupGuard guard;
  GIVEN("a wrapped prose cell") {
    g_cfg->SetAutoWrap(1);
    Narrow();
    const wxString original = LongProseLine();
    GroupCell group(g_cfg, GC_TYPE_TEXT, original);
    EditorCell *editor = group.GetEditable();
    group.Recalculate();
    const std::vector<size_t> narrowBreaks = Breaks(editor);
    REQUIRE(narrowBreaks.size() > 0);

    WHEN("the canvas becomes wide") {
      // Note: GetLineWidth() is capped at LineWidth_em for readability, so a
      // long enough line wraps even on an arbitrarily wide canvas - what a
      // width change must guarantee is that the wrapping is re-derived as a
      // pure function of (text, width), not that it disappears.
      Wide();
      Restyle(editor);

      THEN("the wrapping equals a from-scratch styling at that width") {
        GroupCell fresh(g_cfg, GC_TYPE_TEXT, original);
        fresh.Recalculate();
        REQUIRE(Breaks(editor) == Breaks(fresh.GetEditable()));
        REQUIRE(Breaks(editor).size() < narrowBreaks.size());
        wxString value = editor->GetValue();
        value.Replace(wxS("\r"), wxS(" "));
        REQUIRE(value == original);
      }

      AND_WHEN("the canvas becomes narrow again") {
        Narrow();
        Restyle(editor);

        THEN("the same break set as before is derived") {
          REQUIRE(Breaks(editor) == narrowBreaks);
        }
      }
    }
  }
}

SCENARIO("Code cells wrap when 'Text & Code' is selected, preserving content") {
  WrapSetupGuard guard;
  GIVEN("a long one-line code cell on a narrow canvas, auto-wrap text+code") {
    g_cfg->SetAutoWrap(2);
    CodeNarrow();
    const wxString original = LongCodeLine();
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    EditorCell *editor = group.GetEditable();
    REQUIRE(editor != nullptr);
    group.Recalculate();

    THEN("soft breaks appear, each replacing a space in place") {
      const std::vector<size_t> breaks = Breaks(editor);
      REQUIRE(breaks.size() > 0);
      wxString value = editor->GetValue();
      REQUIRE(value.Length() == original.Length());
      for (size_t breakPos : breaks)
        REQUIRE(original[breakPos] == wxS(' '));
      value.Replace(wxS("\r"), wxS(" "));
      REQUIRE(value == original);
    }
    THEN("the cell's width stays within the line width (plus overshoot slack)") {
      // A wrap triggers after the token that crossed the limit, so a display
      // line may overshoot by roughly one token; anything beyond that means
      // the wrapping failed to make the cell narrower.
      REQUIRE(editor->GetWidth() <=
              static_cast<wxCoord>(g_cfg->GetLineWidth()) * 3 / 2);
    }
    THEN("styling again at the same width derives the same breaks") {
      const std::vector<size_t> first = Breaks(editor);
      for (int i = 0; i < 3; i++) {
        Restyle(editor);
        REQUIRE(Breaks(editor) == first);
      }
    }
  }
}

SCENARIO("Deeply nested code wraps with clamped indentation instead of degenerating") {
  // The failure that got code wrapping disabled: the continuation-line
  // indentation (4 chars per open bracket) could reach or exceed the line
  // width, so wrapping made lines longer instead of shorter.
  WrapSetupGuard guard;
  GIVEN("a code line inside 15 nested function calls") {
    g_cfg->SetAutoWrap(2);
    CodeNarrow();
    wxString original;
    for (int i = 0; i < 15; i++)
      original += wxS("f(");
    for (int i = 0; i < 30; i++)
      original += wxString::Format(wxS("arg_%d, "), i);
    original += wxS("0");
    for (int i = 0; i < 15; i++)
      original += wxS(")");
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    EditorCell *editor = group.GetEditable();
    group.Recalculate();

    THEN("the line wraps, but not at every space") {
      const size_t spaceCount = original.Freq(wxS(' '));
      const std::vector<size_t> breaks = Breaks(editor);
      REQUIRE(breaks.size() >= 2);
      REQUIRE(breaks.size() < spaceCount);
    }
    THEN("the wrapped cell is genuinely narrower than the unwrapped line") {
      REQUIRE(editor->GetWidth() <=
              static_cast<wxCoord>(g_cfg->GetLineWidth()) * 3 / 2);
    }
    THEN("the content survives") {
      wxString value = editor->GetValue();
      value.Replace(wxS("\r"), wxS(" "));
      REQUIRE(value == original);
    }
  }
}

SCENARIO("Hard newlines reset the wrap accounting") {
  WrapSetupGuard guard;
  g_cfg->SetAutoWrap(2);
  CodeNarrow();

  GIVEN("many hard lines that are each far below the limit") {
    wxString original;
    for (int i = 0; i < 20; i++)
      original += wxString::Format(wxS("x%d: %d$\n"), i, i);
    original.RemoveLast(); // no trailing newline
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    group.Recalculate();

    THEN("their summed width does not trigger any soft break") {
      // Fails if the per-line width accumulates across hard newlines.
      REQUIRE(Breaks(group.GetEditable()).empty());
    }
  }

  GIVEN("two long hard lines") {
    const wxString line = LongCodeLine();
    const wxString original = line + wxS("\n") + line;
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    group.Recalculate();

    THEN("each hard line wraps on its own; no break crosses the newline") {
      const std::vector<size_t> breaks = Breaks(group.GetEditable());
      const size_t newlinePos = line.Length();
      size_t before = 0, after = 0;
      for (size_t breakPos : breaks) {
        REQUIRE(breakPos != newlinePos);
        if (breakPos < newlinePos)
          before++;
        else
          after++;
      }
      REQUIRE(before > 0);
      REQUIRE(after > 0);
    }
  }
}

SCENARIO("Undo is unaffected by wrapping") {
  WrapSetupGuard guard;
  GIVEN("a code cell that wraps after being edited") {
    g_cfg->SetAutoWrap(2);
    CodeNarrow();
    const wxString original = wxS("short: 1$");
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    EditorCell *editor = group.GetEditable();
    group.Recalculate();

    WHEN("a long value replaces the short one and gets wrapped") {
      editor->SaveValue();
      editor->SetValue(LongCodeLine());
      REQUIRE_NOTHROW(group.Recalculate());

      AND_WHEN("the edit is undone") {
        REQUIRE(editor->CanUndo());
        editor->Undo();

        THEN("the original text is restored, without any soft break") {
          wxString value = editor->GetValue();
          value.Replace(wxS("\r"), wxS(" "));
          REQUIRE(value == original);
        }
      }
    }
  }
}

SCENARIO("The auto-wrap mode is clamped so code wrapping stays opt-in") {
  WrapSetupGuard guard;
  THEN("the legacy stored value 3 (and anything out of range) means text-only") {
    // Old installations persist autoWrapMode=3 (the pre-tri-state default);
    // ReadConfig() funnels the stored value through SetAutoWrap's clamp.
    g_cfg->SetAutoWrap(3);
    REQUIRE(g_cfg->GetAutoWrap());
    REQUIRE_FALSE(g_cfg->GetAutoWrapCode());
    g_cfg->SetAutoWrap(7);
    REQUIRE(g_cfg->GetAutoWrap());
    REQUIRE_FALSE(g_cfg->GetAutoWrapCode());
    g_cfg->SetAutoWrap(-1);
    REQUIRE_FALSE(g_cfg->GetAutoWrap());
    REQUIRE_FALSE(g_cfg->GetAutoWrapCode());
  }
  THEN("mode 2 enables both, mode 1 only text") {
    g_cfg->SetAutoWrap(2);
    REQUIRE(g_cfg->GetAutoWrap());
    REQUIRE(g_cfg->GetAutoWrapCode());
    g_cfg->SetAutoWrap(1);
    REQUIRE(g_cfg->GetAutoWrap());
    REQUIRE_FALSE(g_cfg->GetAutoWrapCode());
  }
}

SCENARIO("Wrapped code leaks no soft break into serialization") {
  WrapSetupGuard guard;
  GIVEN("a wrapped code cell") {
    g_cfg->SetAutoWrap(2);
    CodeNarrow();
    const wxString original = LongCodeLine();
    GroupCell group(g_cfg, GC_TYPE_CODE, original);
    EditorCell *editor = group.GetEditable();
    group.Recalculate();
    REQUIRE(Breaks(editor).size() > 0);

    THEN("ToString restores the original text") {
      wxString str = editor->ToString();
      REQUIRE(str.Find(wxS('\r')) == wxNOT_FOUND);
      REQUIRE(str == original);
    }
    THEN("ToXML contains no soft break") {
      REQUIRE(editor->ToXML().Find(wxS('\r')) == wxNOT_FOUND);
    }
    THEN("the selection string sees spaces, not soft breaks") {
      editor->SetSelection(0, editor->GetValue().Length());
      wxString sel = editor->GetSelectionString();
      REQUIRE(sel.Find(wxS('\r')) == wxNOT_FOUND);
      REQUIRE(sel == original);
    }
    THEN("caret positions round-trip across the soft breaks") {
      const size_t len = editor->GetValue().Length();
      for (size_t p = 0; p <= len; p++) {
        size_t x = 0, y = 0;
        editor->PositionToXY(p, &x, &y);
        REQUIRE(editor->XYToPosition(x, y) == p);
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
  static CellPointers cellPointers(nullptr);
  g_cfg->SetCellPointers(&cellPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
