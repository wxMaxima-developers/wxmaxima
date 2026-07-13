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
  Implements the pure LDB-recognition helpers. See LdbSupport.h.
*/

#include "LdbSupport.h"

namespace LdbSupport {

bool ContainsLdbBanner(const wxString &streamText) {
  // The banner sbcl prints on entering its low-level debugger. Match a stable
  // substring rather than the whole line so trailing runtime details (version,
  // fault address) do not defeat it.
  return streamText.Contains(wxS("LDB, a low-level debugger for the Lisp runtime"));
}

bool EndsWithLdbPrompt(const wxString &streamText) {
  // LDB's input prompt is "ldb> ". It is the last thing on the stream when LDB
  // is waiting for a command; tolerate trailing whitespace/newlines that the
  // stream reader may append.
  wxString trimmed = streamText;
  trimmed.Trim(true); // strip trailing whitespace only
  return trimmed.EndsWith(wxS("ldb>"));
}

bool LooksLikeLdb(const wxString &streamText) {
  return ContainsLdbBanner(streamText) || EndsWithLdbPrompt(streamText);
}

} // namespace LdbSupport
