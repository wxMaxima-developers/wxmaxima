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
  Splits plain text output into the pieces it may be broken into lines
  between. See TextBreaks.h.
*/

#include "TextBreaks.h"
#include "UrlDetection.h"

namespace wxm {

bool IsTextBreakChar(wxUniChar ch) {
  switch (static_cast<wxChar>(ch)) {
  case wxS(' '):
  case wxS('\t'):
  case wxS('+'):
  case wxS('-'):
  case wxS(','):
  case wxS(';'):
  case wxS('.'):
  case wxS('*'):
  case wxS('/'):
    return true;
  default:
    return false;
  }
}

namespace {
bool IsDigit(wxUniChar ch) { return (ch >= wxS('0')) && (ch <= wxS('9')); }
}

std::vector<wxString> SplitAtTextBreaks(const wxString &line) {
  std::vector<wxString> pieces;
  // A link is shown as one, and only if it is in one cell: never break it.
  const std::vector<UrlSpan> urls = FindUrls(line);
  const auto insideUrl = [&urls](size_t pos) {
    for (const auto &url : urls)
      if ((pos >= url.start) && (pos + 1 < url.start + url.length))
        return true;
    return false;
  };
  wxString piece;
  size_t pos = 0;
  // Has the current piece got a character a break isn't allowed after?
  // Leading indentation is no reason to break.
  bool pieceHasText = false;
  for (auto it = line.begin(); it != line.end(); ++it, ++pos) {
    const wxUniChar ch = *it;
    piece += ch;
    if (!IsTextBreakChar(ch)) {
      pieceHasText = true;
      continue;
    }
    auto next = it;
    ++next;
    if (next == line.end())
      break;
    // A run of break characters stays together: break after its last one.
    if (IsTextBreakChar(*next))
      continue;
    // Don't tear a number apart at its decimal point or thousands separator.
    if (((ch == wxS('.')) || (ch == wxS(','))) && IsDigit(*next))
      continue;
    if (!pieceHasText || insideUrl(pos))
      continue;
    pieces.push_back(piece);
    piece.Clear();
    pieceHasText = false;
  }
  if (!piece.IsEmpty() || pieces.empty())
    pieces.push_back(std::move(piece));
  return pieces;
}

} // namespace wxm
