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
  Finds the web links in a piece of plain text (GH #2396).

  Text cells show a bare "https://..." as a clickable link, and the HTML and
  LaTeX exports turn it into a real link. All three need to agree on where a
  link starts and ends, so the rule lives here, in one GUI-free place a unit
  test can pin directly.

  Links are derived from the text every time rather than stored anywhere: the
  file formats don't change, and a worksheet written by this version reads
  exactly the same in an older one.
*/

#ifndef URLDETECTION_H
#define URLDETECTION_H

#include <wx/string.h>
#include <functional>
#include <vector>

namespace wxm {

//! Where a link sits in a string: the index of its first character and its length.
struct UrlSpan {
  size_t start;
  size_t length;
  bool operator==(const UrlSpan &o) const {
    return start == o.start && length == o.length;
  }
};

/*! Finds every http://, https:// and mailto: link in a piece of text.

  A link has to begin a word (so "xhttp://" is not one) and runs until the
  first character a URL may not contain unescaped: whitespace, or one of
  <>"{}|\^`. Punctuation that ends a sentence rather than the link (".,;:!?'")
  is left out, and so is a closing parenthesis or bracket that has no
  opening partner inside the link -- "(see https://example.org)" links to
  "https://example.org", while a Wikipedia address ending in "_(physics)"
  keeps its parenthesis.

  Since whitespace ends a link, a link never spans two lines: soft line
  breaks, which the text cells only ever put at spaces, can't split one.

  Cheap enough to run on every restyle: a text without a ':' is rejected
  before anything else is looked at, and otherwise it is one linear pass.
*/
std::vector<UrlSpan> FindUrls(const wxString &text);

/*! Is this a link wxMaxima may hand to the web browser?

  Only http, https and mailto are allowed. A worksheet is a document from
  somewhere else, and letting a click in it launch a "file:" link, or one
  of the scheme handlers an operating system registers for its own
  programs, would let that document start things on the user's machine.
*/
bool IsLaunchableUrl(const wxString &url);

/*! Replaces every link in a text by a placeholder an exporter won't touch.

  The exporters escape a text cell's characters one kind at a time (LaTeX
  turns "_" into "\_", "%" into "\%", HTML "&" into "&amp;", ...) and then
  run the result through the Markdown converter. A link must come out of that
  unchanged, so it is parked in `urls` and represented by a run of private-use
  characters and digits, which none of those steps change. RestoreUrls() puts
  it back, formatted as the exporter wants it.
*/
wxString ProtectUrls(const wxString &text, std::vector<wxString> &urls);

//! Undoes ProtectUrls(), writing each link as format(url) returns it.
wxString RestoreUrls(const wxString &text, const std::vector<wxString> &urls,
                     const std::function<wxString(const wxString &)> &format);

} // namespace wxm

#endif // URLDETECTION_H
