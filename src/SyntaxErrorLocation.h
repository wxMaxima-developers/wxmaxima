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
  Finds where in a code cell a Maxima syntax error is (GH #289).

  wxMaxima sends a cell to Maxima one statement at a time, so it always knows
  which statement an error belongs to. For a syntax error Maxima also tells
  where in that statement it gave up -- but only as text, for a human: its
  parser (mread-synerr in Maxima's nparse.lisp) prints

      incorrect syntax: , is not a prefix operator
      f(a,,
         ^

  i.e. the message, then the last (up to 20) characters of the current line
  it has read, then a caret. The second line ends where the parser stopped
  reading, which is at, or one character after, the offending token.

  This file reads that text and finds the same characters in the statement.
  It deliberately changes nothing in Maxima and hooks none of its functions:
  it only reads what Maxima prints anyway, the same way wxMaxima already
  recognises an error at all by the words "incorrect syntax". If a future
  Maxima prints something else, LocateSyntaxError() finds nothing, and the
  worksheet falls back to marking the whole statement.

  Two quirks of real Maxima versions are handled:
  - Maxima 5.46 prints a space in that line as the word "Space" (and a tab as
    "Tab"); newer versions print the characters themselves.
  - The line stops at a newline, so for a statement spanning several lines it
    is the tail of the line the error is in.

  GUI-free, so test_SyntaxErrorLocation can pin it directly.
*/

#ifndef SYNTAXERRORLOCATION_H
#define SYNTAXERRORLOCATION_H

#include <wx/string.h>
#include <optional>

namespace wxm {

//! A range of characters in a cell's text: [start, end)
struct TextRange {
  size_t start;
  size_t end;
  bool operator==(const TextRange &o) const {
    return start == o.start && end == o.end;
  }
};

/*! The line Maxima echoes under its first "incorrect syntax:" message

  \param maximaOutput Everything Maxima printed in reply to one statement.
  \return The echoed input line, exactly as Maxima printed it, or nothing if
          the output contains no complete syntax error report (the caret line
          is what tells that the echo is complete).
*/
std::optional<wxString> SyntaxErrorEcho(const wxString &maximaOutput);

/*! Where in a cell the syntax error Maxima reported is

  \param maximaOutput Everything Maxima printed in reply to one statement.
  \param cellText     The text of the code cell the statement came from.
  \param statement    The statement's characters in cellText.
  \return The characters to mark: the token Maxima stopped at, plus the one
          character after it it may have read ahead. Nothing if Maxima did not
          report a syntax error, or the echoed text can't be found in the
          statement (for example because wxMaxima stripped a comment from it
          before sending it).
*/
std::optional<TextRange> LocateSyntaxError(const wxString &maximaOutput,
                                           const wxString &cellText,
                                           TextRange statement);

} // namespace wxm

#endif // SYNTAXERRORLOCATION_H
