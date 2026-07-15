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
  Implements the pure Maxima-protocol helpers. See MaximaProtocol.h.
*/

#include "MaximaProtocol.h"

namespace MaximaProtocol {

bool IsDebuggerPrompt(const wxString &label) {
  return label.StartsWith(wxS("(dbm:"));
}

bool IsLispReplPrompt(const wxString &label) {
  return label.StartsWith(wxS("MAXIMA>")) || label.StartsWith(wxS("\nMAXIMA>"));
}

bool IsNumberedInputPrompt(const wxString &label) {
  // Input prompts have a length > 0 and end in a number followed by a ")".
  // Depending on ibase the digits of the number might be between 'A' and 'Z',
  // too. Input prompts also begin with a "(". The debugger prompt "(dbm:N)" is
  // excluded explicitly (it also parenthesizes but is not an input prompt).
  //
  // Maxima sends the prompt with a trailing space ("(%i1) "); trim it here so
  // the function is correct on the raw label, not only the pre-trimmed one
  // ReadPrompt happens to pass.
  wxString prompt = label;
  prompt.Trim(true);
  if (prompt.Length() <= 2)
    return false;
  if (!prompt.StartsWith(wxS("(%")))
    return false;
  if (IsDebuggerPrompt(prompt))
    return false;
  if (!prompt.EndsWith(wxS(")")))
    return false;
  const wxUniChar c = prompt[prompt.Length() - 2];
  return ((c >= wxS('0')) && (c <= wxS('9'))) ||
         ((c >= wxS('A')) && (c <= wxS('Z')));
}

bool IsMainInputPrompt(const wxString &label, bool inLispMode) {
  return IsNumberedInputPrompt(label) || inLispMode || IsLispReplPrompt(label);
}

bool CommandIsBlank(const wxString &command) {
  wxString trimmed = command;
  trimmed.Trim(true); // strip trailing whitespace only - exactly as SendMaxima does
  return trimmed.IsEmpty();
}

} // namespace MaximaProtocol
