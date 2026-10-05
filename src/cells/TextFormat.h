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
  Character formatting (bold, italic, ...) of the text in a text cell (GH #492).

  Everything here is a pure function of strings and vectors, needing neither a
  cell nor a GUI, so that it can be tested on its own.

  <b>The model</b>: one Format per character of the cell's text, stored in a
  vector indexed exactly like the wxString it describes. An empty vector means
  "no formatting at all", which is what every cell that never had any carries,
  so plain cells pay nothing.

  <b>How it is saved</b>: .wxmx stores a text cell as one \c \<line\> element
  per line. Formatting goes into attributes of those elements, one per
  format, each a list of half-open ranges counted in Unicode code points from
  the start of the line:

      <line bold="0-4,10-12" italic="6-9">Bold, italic and bold</line>

  This is not a pretty format, but it is the one older wxMaxima versions can
  read: they take the element's text and never look at its attributes, so they
  show the text unformatted instead of losing any of it. Inline tags
  (\c \<b\>, \c \<i\>, ...) would have been nicer, but an older version reading
  \c \<line\>a \<b\>b\</b\> c\</line\> keeps only the "a ".

  ReadLine() nevertheless understands such inline tags too, so a future
  version can switch to them: \c \<b\>/\c \<strong\>, \c \<i\>/\c \<em\>,
  \c \<u\> and \c \<s\>/\c \<strike\>/\c \<del\> set the matching format, and any
  other tag is read as if it weren't there - its text is kept, only the
  formatting it stands for is lost.
*/

#ifndef TEXTFORMAT_H
#define TEXTFORMAT_H

#include <cstdint>
#include <cstddef>
#include <vector>
#include <wx/string.h>

class wxXmlNode;

namespace TextFormat {

//! The formatting of one character: a combination of the flags below.
using Format = std::uint8_t;

enum : Format {
  None = 0,
  Bold = 1,
  Italic = 2,
  Underline = 4,
  Strikethrough = 8
};

//! The flags that change a character's width, and therefore the font it is
//! measured and drawn with. Underline and strikethrough are lines drawn on top.
constexpr Format WidthAffecting = Bold | Italic;

//! One Format per character, indexed like the text it belongs to; empty = plain.
using Formats = std::vector<Format>;

//! Does this formatting vector describe plain text?
bool IsPlain(const Formats &formats);

//! The format of character pos, None past the end (or for plain text).
inline Format At(const Formats &formats, std::size_t pos) {
  return (pos < formats.size()) ? formats[pos] : Format(None);
}

/*! The format a character typed at pos gets.

  The one of the character before it, as in any word processor (typing at the
  end of a bold word continues it in bold) - unless there is none on the same
  line, in which case the character after it decides.
*/
Format FormatForInsertionAt(const wxString &text, const Formats &formats,
                            std::size_t pos);

/*! The formatting of newText, given that it came from oldText by editing.

  Finds which characters of newText already existed in oldText and lets them
  keep their format; characters that are new take the format of the
  character(s) they replaced, or else of the one before them (as in any word
  processor, typing at the end of a bold word continues it in bold).

  One contiguous edit - typing, deleting, pasting, replacing a selection -
  is mapped exactly via the common prefix and suffix. Several separate edits
  at once (a "replace all", indenting several lines) are mapped by a
  longest-common-subsequence diff of the part that changed, which is
  bounded in size; past that bound the changed part keeps its formats by
  position.

  \param pendingPos, pendingFormat If hasPending and the edit was a pure
         insertion at pendingPos, the inserted characters get pendingFormat:
         that is what pressing "bold" with nothing selected and then typing
         does.
  \return the new formatting, empty if it is plain.
*/
Formats Reconcile(const wxString &oldText, const Formats &oldFormats,
                  const wxString &newText, bool hasPending = false,
                  std::size_t pendingPos = 0, Format pendingFormat = None);

/*! The attributes that store the formatting of [lineStart, lineEnd) of text.

  \return a string like <code> bold="0-4" italic="2-3"</code> (with a leading
          space), or an empty one if that line is plain.
*/
wxString LineAttributes(const wxString &text, const Formats &formats,
                        std::size_t lineStart, std::size_t lineEnd);

/*! Append the text and formatting of a \c \<line\> element.

  \param line    The \c \<line\> element.
  \param text    The line's text is appended to it.
  \param formats Receives one format per appended character. Resized to
                 match text first, so it may be passed empty.
  \param unknownAttributes If not null, receives every attribute of \c line
                 this version doesn't understand, serialized as
                 <code> name="value"</code>, so a newer version's formatting
                 survives being opened and saved by this one.
*/
void ReadLine(const wxXmlNode *line, wxString &text, Formats &formats,
              wxString *unknownAttributes = nullptr);

/*! Parse a range list like "0-4,10-12" (half-open, in code points).

  Malformed entries are skipped. Exposed for the tests.
*/
std::vector<std::pair<std::size_t, std::size_t>> ParseRanges(const wxString &ranges);

//! How many Unicode code points text[0, index) holds - which differs from
//! index where wxString stores UTF-16 (MS Windows) and the text contains a
//! character outside the Basic Multilingual Plane.
std::size_t CodePointsBefore(const wxString &text, std::size_t start, std::size_t index);

//! The wxString index of the codePoints'th code point after start.
std::size_t IndexOfCodePoint(const wxString &text, std::size_t start,
                             std::size_t codePoints);
} // namespace TextFormat

#endif // TEXTFORMAT_H
