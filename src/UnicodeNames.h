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
  Unicode code points typed as hex numbers, and the names Unicode gives them.

  ESC 61 ESC enters an "a": the ESC popup offers the code point it has been
  typed so far next to the named ESC codes, and needs to know whether the
  typed text is a code point and what to call it. The names come from the
  UnicodeData.txt table the Unicode sidebar uses, which is embedded in the
  binary (data/UnicodeData.h).
*/

#ifndef UNICODENAMES_H
#define UNICODENAMES_H

#include <functional>
#include <optional>
#include <wx/string.h>

namespace wxm {
/*! The code point a hexadecimal number names, if it names one.

  \param text A hex number, optionally preceded by "u+" or "U+"
              ("61", "00be", "U+1D49C").
  \return The code point, or nothing if the text isn't a hex number or the
          number lies outside Unicode's range or among the surrogates, which
          aren't characters on their own.
*/
std::optional<long> HexCodePoint(const wxString &text);

/*! The name UnicodeData.txt gives a code point.

  \return The name ("LATIN SMALL LETTER A"), or an empty string if the table
          has none. Code points the table only covers by a range (the CJK
          ideographs, Hangul syllables, ...) and control characters have none.

  Decompresses and scans the table, stopping at the code point; the result of
  the last call is cached, as the popup asks for the same code point while
  the list is redrawn.
*/
wxString UnicodeCharName(long codePoint);

/*! Calls \p func(codePoint, name) for each named character in UnicodeData.txt.

  In the table's order, which is ascending code points. Control characters
  and the private-use planes are skipped; the "<..., First>"/"<..., Last>"
  markers of the other ranges are not. Stops early if \p func returns false.
*/
void ForEachUnicodeChar(const std::function<bool(long, const wxString &)> &func);
} // namespace wxm

#endif // UNICODENAMES_H
