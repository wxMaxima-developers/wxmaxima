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
  Tests for SymbolShortcuts: what a sidebar symbol is, and the keyboard
  shortcuts that type symbols (GH #2578, GH #2296). GUI-free.
*/

#include "SymbolShortcuts.h"
#include <wx/accel.h>
#include <wx/event.h>

// Not CATCH_CONFIG_MAIN, see test_SyntaxErrorLocation.cpp
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
const wxString tMacron = wxString(wxS("t")) + wxUniChar(0x0304);

std::vector<wxString> Symbols(std::initializer_list<wxString> list) {
  return std::vector<wxString>(list);
}

//! A key-down event as the worksheet gets one
wxKeyEvent KeyDown(int keyCode, bool ctrl, bool alt, bool shift = false) {
  wxKeyEvent event(wxEVT_KEY_DOWN);
  event.m_keyCode = keyCode;
  event.SetControlDown(ctrl);
  event.SetAltDown(alt);
  event.SetShiftDown(shift);
#ifdef __WXOSX__
  event.SetRawControlDown(false);
#endif
  return event;
}
} // namespace

SCENARIO("A letter with a combining accent is one symbol (GH #2296)") {
  THEN("t followed by a combining macron stays together") {
    CHECK(wxm::SplitIntoSymbols(wxS("Ø") + tMacron + wxS("§")) ==
          Symbols({wxS("Ø"), tMacron, wxS("§")}));
  }
  THEN("several accents stay with their letter") {
    const wxString twoAccents = wxString(wxS("a")) + wxUniChar(0x0301) + wxUniChar(0x0323);
    CHECK(wxm::SplitIntoSymbols(twoAccents + wxS("b")) ==
          Symbols({twoAccents, wxS("b")}));
  }
  THEN("a vector arrow from the symbol marks joins its letter") {
    const wxString vec = wxString(wxS("v")) + wxUniChar(0x20D7);
    CHECK(wxm::SplitIntoSymbols(vec) == Symbols({vec}));
  }
  THEN("spaces only separate, and let a combining mark be a symbol of its own") {
    const wxString macron(wxUniChar(0x0304));
    CHECK(wxm::SplitIntoSymbols(wxS("a  b")) == Symbols({wxS("a"), wxS("b")}));
    CHECK(wxm::SplitIntoSymbols(wxS("a ") + macron) == Symbols({wxS("a"), macron}));
    CHECK(wxm::SplitIntoSymbols(macron + wxS("a")) == Symbols({macron, wxS("a")}));
  }
  THEN("an old list of single characters splits into them as before") {
    CHECK(wxm::SplitIntoSymbols(wxS("Øü§")) ==
          Symbols({wxS("Ø"), wxS("ü"), wxS("§")}));
  }
}

SCENARIO("Joining symbols and splitting them again gives the same symbols") {
  const wxString macron(wxUniChar(0x0304));
  for (const auto &symbols :
         {Symbols({wxS("Ø"), tMacron, wxS("§")}),
          Symbols({wxS("a"), macron, wxS("b")}),
          Symbols({macron}),
          Symbols({})}) {
    CHECK(wxm::SplitIntoSymbols(wxm::JoinSymbols(symbols)) == symbols);
  }
  THEN("there are no separators unless needed, so old versions read it") {
    CHECK(wxm::JoinSymbols(Symbols({wxS("Ø"), tMacron, wxS("§")})) ==
          wxS("Ø") + tMacron + wxS("§"));
    CHECK(wxm::JoinSymbols(Symbols({wxS("a"), macron})) == wxS("a ") + macron);
  }
}

SCENARIO("Shortcuts are stored in the untranslated form") {
  THEN("what the user typed is normalized") {
    const wxString normalized = wxm::NormalizeShortcut(wxS("ctrl+alt+t"));
    CHECK(!normalized.IsEmpty());
    CHECK(wxm::SameShortcut(normalized, wxS("Alt+Ctrl+T")));
  }
  THEN("a key needed for typing can't be a symbol's shortcut") {
    CHECK(wxm::NormalizeShortcut(wxS("T")).IsEmpty());
    CHECK(wxm::NormalizeShortcut(wxS("Shift+T")).IsEmpty());
    CHECK(wxm::NormalizeShortcut(wxS("Return")).IsEmpty());
    CHECK(!wxm::NormalizeShortcut(wxS("F7")).IsEmpty());
    CHECK(!wxm::NormalizeShortcut(wxS("Alt+Shift+T")).IsEmpty());
  }
  THEN("nonsense is refused") {
    CHECK(wxm::NormalizeShortcut(wxS("Ctrl+")).IsEmpty());
    CHECK(wxm::NormalizeShortcut(wxEmptyString).IsEmpty());
  }
  THEN("the same keys are the same shortcut, others are not") {
    CHECK(wxm::SameShortcut(wxS("Ctrl+Alt+T"), wxS("Alt+Ctrl+t")));
    CHECK(!wxm::SameShortcut(wxS("Ctrl+Alt+T"), wxS("Ctrl+T")));
    CHECK(!wxm::SameShortcut(wxS("Ctrl+Alt+T"), wxS("Ctrl+Alt+Shift+T")));
  }
}

SCENARIO("The shortcuts list survives storing and reading it") {
  const std::vector<wxm::SymbolShortcut> shortcuts{
    {tMacron, wxm::NormalizeShortcut(wxS("Ctrl+Alt+T"))},
    {wxS("α"), wxm::NormalizeShortcut(wxS("Alt+A"))},
  };
  CHECK(wxm::ParseSymbolShortcuts(wxm::SerializeSymbolShortcuts(shortcuts)) == shortcuts);
  THEN("broken lines are skipped, not misread") {
    CHECK(wxm::ParseSymbolShortcuts(wxS("x\n\tCtrl+Alt+X\ny\tnonsense+\nz\tAlt+Z\n")) ==
          std::vector<wxm::SymbolShortcut>{{wxS("z"), wxm::NormalizeShortcut(wxS("Alt+Z"))}});
  }
}

SCENARIO("A key press finds the symbol whose shortcut it is") {
  const std::vector<wxm::SymbolShortcut> shortcuts{
    {tMacron, wxm::NormalizeShortcut(wxS("Ctrl+Alt+T"))},
    {wxS("β"), wxm::NormalizeShortcut(wxS("F7"))},
  };
  // On the Mac, wxACCEL_CTRL and ControlDown() both mean Cmd.
  CHECK(wxm::SymbolForKeyEvent(shortcuts, KeyDown('T', true, true)) == tMacron);
  CHECK(wxm::SymbolForKeyEvent(shortcuts, KeyDown(WXK_F7, false, false)) == wxS("β"));
  THEN("other keys and plain typing type nothing extra") {
    CHECK(wxm::SymbolForKeyEvent(shortcuts, KeyDown('T', false, false)).IsEmpty());
    CHECK(wxm::SymbolForKeyEvent(shortcuts, KeyDown('T', true, false)).IsEmpty());
    CHECK(wxm::SymbolForKeyEvent(shortcuts, KeyDown('T', true, true, true)).IsEmpty());
  }
  THEN("holding down a modifier is not a shortcut yet") {
    CHECK(wxm::ShortcutFromKeyEvent(KeyDown(WXK_CONTROL, true, false)).IsEmpty());
    CHECK(wxm::ShortcutFromKeyEvent(KeyDown(WXK_ALT, true, true)).IsEmpty());
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
