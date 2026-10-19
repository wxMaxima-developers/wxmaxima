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
  Splits a line of plain text Maxima sends into the pieces the worksheet may
  break it into lines between.

  Text Maxima sends outside of its XML -- warnings, errors, status messages,
  printf() output -- used to become one cell per line. The worksheet only
  breaks output between cells, so a long message ran off the right edge of the
  window. MaximaOutputAppender::DoRawConsoleAppend() therefore turns such a
  line into one TextCell per piece returned here, the pieces after the first
  marked as continuing the one before (TextCell::IsTextContinuation()), so
  that, as long as the line fits, it looks exactly as it did before.

  Text Maxima marks as 2D ASCII-art maths (<wxxml-asciimath>) is never split:
  its columns only line up if every line stays in one piece.

  GUI-free, so test_TextBreaks can pin it directly.
*/

#ifndef TEXTBREAKS_H
#define TEXTBREAKS_H

#include <wx/string.h>
#include <vector>

namespace wxm {

/*! Is a line break allowed after this character?

  True for a space, a tab, "+", "-", ",", ";", ".", "*" and "/".
*/
bool IsTextBreakChar(wxUniChar ch);

/*! Split one line of text (no newlines) after the characters a line may be
  broken after.

  Concatenating the pieces always gives back \p line unchanged. The character
  that allows the break stays at the end of the piece before the break. A run
  of such characters stays together (", " breaks after the space, "--" after
  the second "-"), and the line isn't broken
  - before its first character that isn't a break character, so indentation
    stays with the word it indents, or
  - after a "." or a "," that is followed by a digit, so a number like 3.14 or
    1,000 is never torn apart, or
  - inside a link (see wxm::FindUrls()): a link is only shown as one if all of
    it is in the same cell.
*/
std::vector<wxString> SplitAtTextBreaks(const wxString &line);

} // namespace wxm

#endif // TEXTBREAKS_H
