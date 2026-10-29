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
  Keyboard shortcuts that type a symbol into the worksheet (GH #2578).
*/

#include "SymbolShortcuts.h"
#include <wx/accel.h>
#include <wx/event.h>
#include <wx/log.h>
#include <wx/tokenzr.h>

namespace {
//! Does this code unit belong to the character in front of it?
bool JoinsPrevious(wxUniChar ch) {
  const auto c = ch.GetValue();
  return
    (c >= 0x0300 && c <= 0x036F) || // Combining Diacritical Marks
    (c >= 0x1AB0 && c <= 0x1AFF) || // ... Extended
    (c >= 0x1DC0 && c <= 0x1DFF) || // ... Supplement
    (c >= 0x20D0 && c <= 0x20FF) || // ... for Symbols (vector arrows)
    (c >= 0xFE20 && c <= 0xFE2F) || // Combining Half Marks
    (c >= 0xFE00 && c <= 0xFE0F) || // Variation selectors
    (c >= 0xDC00 && c <= 0xDFFF) || // Low surrogate (UTF-16 wxString)
    (c == 0x200D);                  // Zero-width joiner
}

const wxUniChar zeroWidthJoiner(0x200D);

/*! The flags and key code a raw shortcut string stands for.

  Letters are compared in upper case: that is what a key-down event reports,
  whatever the user typed into the configuration.
*/
bool ParseEntry(const wxString &shortcut, int &flags, int &keyCode) {
  wxAcceleratorEntry entry;
  // A broken entry in the config file is skipped, not reported every time.
  wxLogNull suppressor;
  if (shortcut.IsEmpty() || !entry.FromString(shortcut))
    return false;
  flags = entry.GetFlags();
  keyCode = entry.GetKeyCode();
  if ((keyCode >= 'a') && (keyCode <= 'z'))
    keyCode += 'A' - 'a';
  return true;
}

//! Our key event's modifiers, in the terms of a wxAcceleratorEntry.
int FlagsOf(const wxKeyEvent &event) {
  int flags = 0;
  // ControlDown() is Cmd on the Mac, which is what wxACCEL_CTRL means there.
  if (event.ControlDown())
    flags |= wxACCEL_CTRL;
  if (event.AltDown())
    flags |= wxACCEL_ALT;
  if (event.ShiftDown())
    flags |= wxACCEL_SHIFT;
#ifdef __WXOSX__
  if (event.RawControlDown())
    flags |= wxACCEL_RAW_CTRL;
#endif
  return flags;
}
} // namespace

namespace wxm {

std::vector<wxString> SplitIntoSymbols(const wxString &text) {
  std::vector<wxString> symbols;
  bool joinNext = false;
  // After a space nothing joins the symbol before it: that is how to get a
  // button for a combining mark of its own (" ̄").
  bool afterSpace = true;
  for (const auto &ch : text) {
    if ((ch == ' ') || (ch == '\t') || (ch == '\n') || (ch == '\r')) {
      joinNext = false;
      afterSpace = true;
      continue;
    }
    if (!afterSpace && (joinNext || JoinsPrevious(ch)))
      symbols.back() += ch;
    else
      symbols.emplace_back(ch);
    joinNext = (ch == zeroWidthJoiner);
    afterSpace = false;
  }
  return symbols;
}

wxString JoinSymbols(const std::vector<wxString> &symbols) {
  wxString text;
  for (const auto &symbol : symbols) {
    if (symbol.IsEmpty())
      continue;
    if (!text.IsEmpty() &&
        (JoinsPrevious(symbol[0]) || (text.Last() == zeroWidthJoiner)))
      text += wxS(" ");
    text += symbol;
  }
  return text;
}

std::vector<SymbolShortcut> ParseSymbolShortcuts(const wxString &stored) {
  std::vector<SymbolShortcut> shortcuts;
  wxStringTokenizer lines(stored, wxS("\n"), wxTOKEN_STRTOK);
  while (lines.HasMoreTokens()) {
    const wxString line = lines.GetNextToken();
    const wxString symbol = line.BeforeFirst('\t');
    const wxString shortcut = NormalizeShortcut(line.AfterFirst('\t'));
    if (symbol.IsEmpty() || shortcut.IsEmpty() || !line.Contains(wxS("\t")))
      continue;
    shortcuts.push_back({symbol, shortcut});
  }
  return shortcuts;
}

wxString SerializeSymbolShortcuts(const std::vector<SymbolShortcut> &shortcuts) {
  wxString stored;
  for (const auto &entry : shortcuts) {
    if (entry.symbol.IsEmpty() || entry.shortcut.IsEmpty())
      continue;
    stored += entry.symbol + wxS("\t") + entry.shortcut + wxS("\n");
  }
  return stored;
}

bool IsUsableShortcut(int flags, int keyCode) {
  if ((keyCode >= WXK_F1) && (keyCode <= WXK_F24))
    return true;
  return (flags & (wxACCEL_CTRL | wxACCEL_ALT | wxACCEL_RAW_CTRL)) != 0;
}

wxString NormalizeShortcut(const wxString &shortcut) {
  int flags, keyCode;
  if (!ParseEntry(shortcut.Strip(wxString::both), flags, keyCode))
    return {};
  if (!IsUsableShortcut(flags, keyCode))
    return {};
  return wxAcceleratorEntry(flags, keyCode).ToRawString();
}

wxString ShortcutFromKeyEvent(const wxKeyEvent &event) {
  const int keyCode = event.GetKeyCode();
  switch (keyCode) {
  case WXK_NONE:
  case WXK_SHIFT:
  case WXK_ALT:
  case WXK_CONTROL:
#ifdef __WXOSX__
  case WXK_RAW_CONTROL:
#endif
  case WXK_WINDOWS_LEFT:
  case WXK_WINDOWS_RIGHT:
  case WXK_WINDOWS_MENU:
  case WXK_CAPITAL:
  case WXK_NUMLOCK:
  case WXK_SCROLL:
    return {};
  default:
    break;
  }
  const int flags = FlagsOf(event);
  if (!IsUsableShortcut(flags, keyCode))
    return {};
  return wxAcceleratorEntry(flags, keyCode).ToRawString();
}

bool SameShortcut(const wxString &a, const wxString &b) {
  int flagsA, keyA, flagsB, keyB;
  return ParseEntry(a, flagsA, keyA) && ParseEntry(b, flagsB, keyB) &&
    (flagsA == flagsB) && (keyA == keyB);
}

wxString ShortcutForDisplay(const wxString &rawShortcut) {
  int flags, keyCode;
  if (!ParseEntry(rawShortcut, flags, keyCode))
    return rawShortcut;
  return wxAcceleratorEntry(flags, keyCode).ToString();
}

wxString SymbolForKeyEvent(const std::vector<SymbolShortcut> &shortcuts,
                           const wxKeyEvent &event) {
  if (shortcuts.empty())
    return {};
  const wxString pressed = ShortcutFromKeyEvent(event);
  if (pressed.IsEmpty())
    return {};
  for (const auto &entry : shortcuts)
    if (SameShortcut(entry.shortcut, pressed))
      return entry.symbol;
  return {};
}

} // namespace wxm
