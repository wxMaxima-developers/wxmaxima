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
  Finds where in a code cell a Maxima syntax error is; see SyntaxErrorLocation.h.
*/

#include "SyntaxErrorLocation.h"
#include <wx/tokenzr.h>
#include <wx/wxcrt.h>
#include <vector>

namespace wxm {

namespace {

//! Maxima's parser never echoes more than this many characters of a line.
constexpr size_t maxEchoLength = 20;

//! True for the characters a Maxima name or number is made of.
bool IsTokenChar(wxUniChar c) {
  return wxIsalnum(c) || (c == wxS('_')) || (c == wxS('%')) || (c > 127);
}

//! True if line consists of blanks followed by a single caret
bool IsCaretLine(const wxString &line) {
  wxString trimmed = line;
  trimmed.Trim(true);
  if (!trimmed.EndsWith(wxS("^")))
    return false;
  for (size_t i = 0; i + 1 < trimmed.Length(); i++)
    if (trimmed[i] != wxS(' '))
      return false;
  return true;
}

/*! The ways the echoed line may have to be read

  Literally first: in a modern Maxima a "Space" in the echo really is the
  word, part of a name. Then the way Maxima 5.46 spells out blanks.
*/
std::vector<wxString> EchoCandidates(const wxString &echo) {
  std::vector<wxString> candidates{echo};
  if (echo.Contains(wxS("Space")) || echo.Contains(wxS("Tab"))) {
    wxString spelledOut = echo;
    spelledOut.Replace(wxS("Space"), wxS(" "));
    spelledOut.Replace(wxS("Tab"), wxS("\t"));
    candidates.push_back(spelledOut);
  }
  return candidates;
}

/*! Where in text[from, to) the echoed characters are, if anywhere

  Maxima stops at the first error, so the first occurrence is the most likely
  one. A short echo is the whole beginning of its line, though, which rules
  out any occurrence that starts in the middle of a line: those are skipped
  when one that starts a line exists.
*/
std::optional<size_t> FindEcho(const wxString &text, size_t from, size_t to,
                               const wxString &echo) {
  if (echo.IsEmpty() || (to < from) || (echo.Length() > to - from))
    return std::nullopt;
  const bool startsALine = echo.Length() < maxEchoLength;
  std::optional<size_t> first;
  for (size_t pos = from; pos + echo.Length() <= to; pos++) {
    if (text.compare(pos, echo.Length(), echo) != 0)
      continue;
    if (!startsALine)
      return pos;
    // Where the statement starts is a line start for Maxima's parser, too:
    // wxMaxima sends it on its own.
    if ((pos == from) || (text[pos - 1] == wxS('\n')))
      return pos;
    if (!first)
      first = pos;
  }
  return first;
}

} // namespace

std::optional<wxString> SyntaxErrorEcho(const wxString &maximaOutput) {
  const auto messageStart = maximaOutput.Find(wxS("incorrect syntax:"));
  if (messageStart == wxNOT_FOUND)
    return std::nullopt;

  std::vector<wxString> lines;
  wxStringTokenizer tokenizer(maximaOutput.Mid(messageStart), wxS("\n"),
                              wxTOKEN_RET_EMPTY_ALL);
  while (tokenizer.HasMoreTokens()) {
    wxString line = tokenizer.GetNextToken();
    line.Replace(wxS("\r"), wxEmptyString);
    lines.push_back(line);
  }

  // lines[0] is the message. The echo directly precedes the caret, which
  // tells us the echo is complete, too: the output may arrive in pieces.
  // Only look a few lines further: a caret that far down belongs to
  // something else.
  for (size_t i = 2; (i < lines.size()) && (i < 5); i++)
    if (IsCaretLine(lines[i]))
      return lines[i - 1];
  return std::nullopt;
}

std::optional<TextRange> LocateSyntaxError(const wxString &maximaOutput,
                                           const wxString &cellText,
                                           TextRange statement) {
  const auto echo = SyntaxErrorEcho(maximaOutput);
  if (!echo)
    return std::nullopt;
  if (statement.end > cellText.Length())
    statement.end = cellText.Length();

  for (const auto &candidate : EchoCandidates(*echo)) {
    const auto found = FindEcho(cellText, statement.start, statement.end, candidate);
    if (!found)
      continue;
    const size_t echoStart = *found;

    // Maxima stopped reading where the echo ends. Trailing blanks it read are
    // not what's wrong, though.
    size_t end = echoStart + candidate.Length();
    while ((end > echoStart + 1) && wxIsspace(cellText[end - 1]))
      end--;
    // The parser may have read one character beyond the offending token to
    // see where the token ends ("x: 2 34;" stops after the ";"). So the
    // mark starts one character early, and extends to all of the token that
    // character is part of.
    size_t start = (end - echoStart >= 2) ? end - 2 : echoStart;
    while ((start > echoStart) && IsTokenChar(cellText[start]) &&
           IsTokenChar(cellText[start - 1]))
      start--;
    return TextRange{start, end};
  }
  return std::nullopt;
}

} // namespace wxm
