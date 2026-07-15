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
  Pure helpers for the text protocol wxMaxima speaks with Maxima over the
  control socket: classifying the <PROMPT> labels Maxima sends, and deciding
  whether a command is empty enough that sending it would put nothing but a
  bare newline on the wire.

  Everything here is deliberately free of any GUI/state (only wxString) so it
  can be unit tested against captured prompt/command strings - see
  test/unit_tests/test_MaximaProtocol.cpp. The classification logic used to
  live inline in MaximaResponseReader::ReadPrompt.
*/

#ifndef MAXIMAPROTOCOL_H
#define MAXIMAPROTOCOL_H

#include <wx/string.h>

namespace MaximaProtocol {

//! A Maxima high-level debugger prompt, e.g. "(dbm:1)". Maxima drops into this
//! REPL on an error while debugmode is on, or at a breakpoint.
bool IsDebuggerPrompt(const wxString &label);

//! A Maxima Lisp-REPL prompt ("MAXIMA> "), as seen after to_lisp(). The prompt
//! may be preceded by a newline in the stream.
bool IsLispReplPrompt(const wxString &label);

//! A normal numbered Maxima input/label prompt, e.g. "(%i3) " or "(%o12) ".
//! The character before the closing ")" is a digit (any ibase) or A-Z. Excludes
//! the debugger prompt (which also parenthesizes but is not an input prompt).
bool IsNumberedInputPrompt(const wxString &label);

//! Does \p label mark a new *main* prompt - one where wxMaxima should advance
//! its evaluation queue - as opposed to a question it must stop and answer?
//!
//! This mirrors the historical ReadPrompt condition exactly: a numbered input
//! prompt, OR anything at all while wxMaxima already believes it is in Lisp
//! mode, OR a Lisp-REPL prompt.
//!
//! \warning Note the second clause: while \p inLispMode is true, EVERY prompt -
//! including the debugger's "(dbm:N)" - is reported as a main prompt. Because
//! ReadPrompt turns Lisp mode ON as soon as it has seen one "(dbm:N)" prompt,
//! the FIRST debugger prompt is classified as a question (answer caret) but
//! every SUBSEQUENT one is classified as a main prompt (queue-advancing input).
//! That asymmetry is a known bug (debugger commands after the first get
//! recorded as ordinary worksheet input); this function reproduces the current
//! behavior so the bug is pinned rather than fixed here.
bool IsMainInputPrompt(const wxString &label, bool inLispMode);

//! Would transmitting \p command send nothing but a bare newline? SendMaxima
//! trims trailing whitespace and appends "\n", so an empty or all-whitespace
//! string becomes a lone blank line on the wire. At a normal Maxima prompt that
//! is a harmless no-op, but at the "(dbm:N)" debugger prompt a blank line means
//! "repeat the last command" - so a caller that has nothing to send must not
//! call SendMaxima at all. Uses the same trim SendMaxima does.
bool CommandIsBlank(const wxString &command);

} // namespace MaximaProtocol

#endif // MAXIMAPROTOCOL_H
