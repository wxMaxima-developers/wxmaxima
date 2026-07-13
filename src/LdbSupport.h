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
  Pure helpers for recognizing sbcl's low-level debugger (LDB).

  When the Lisp runtime under Maxima faults badly enough (a memory fault, an
  unhandled runtime signal, corruption) sbcl drops into LDB. Unlike Maxima's
  own `(dbm:N)` / `MAXIMA>` Lisp debugger - which talks to us as <PROMPT> XML
  over the control socket - LDB runs *below* Maxima: it writes to the Maxima
  process's stdout/stderr and reads commands from its stdin, entirely outside
  the socket protocol. wxMaxima therefore has to recognize LDB in the raw
  stdout/stderr stream (that is what these helpers do) and, once in LDB mode,
  send the user's input to the process's stdin rather than the socket.

  These functions are deliberately free of any GUI/state so they can be unit
  tested against captured LDB output.
*/

#ifndef LDBSUPPORT_H
#define LDBSUPPORT_H

#include <wx/string.h>

namespace LdbSupport {

//! Does \p streamText (a chunk read from Maxima's stdout/stderr) contain the
//! banner sbcl prints when it enters LDB? This marks the transition INTO LDB.
bool ContainsLdbBanner(const wxString &streamText);

//! Does \p streamText end with an LDB input prompt ("ldb> ")? LDB is then
//! waiting for a command on stdin.
bool EndsWithLdbPrompt(const wxString &streamText);

//! Does \p streamText look like LDB is active at all (banner or prompt)? A
//! convenience for the stdout/stderr reader.
bool LooksLikeLdb(const wxString &streamText);

} // namespace LdbSupport

#endif // LDBSUPPORT_H
