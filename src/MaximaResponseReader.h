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
  The handlers that turn a chunk of data Maxima sent into a change to
  wxMaxima's state and worksheet.

  Maxima answers wxMaxima's commands with XML wrapped in known tags plus the
  occasional bit of plain text (prompts, stderr, misc output). wxMaxima's
  socket-event handler (wxMaxima::MaximaEvent) classifies each chunk and hands
  it to one of the Read* handlers collected here. They used to be members of
  the wxMaxima god class; they are being peeled off into this class to shrink
  wxMaxima.cpp.

  The pure XML -> data-structure parsing keeps moving into GUI-free,
  unit-testable free functions (see MaximaVariableUpdates.h); the handlers here
  drive the GUI (the worksheet, the sidebars, the status bar, the
  configuration) through the m_wxMaxima reference, so MaximaResponseReader is a
  friend of wxMaxima.
*/

#ifndef MAXIMARESPONSEREADER_H
#define MAXIMARESPONSEREADER_H

#include <wx/string.h>
#include <wx/xml/xml.h>

class wxMaxima;

/*! The Maxima-response handlers extracted from the wxMaxima god class.

  Owned by value by the wxMaxima frame; wxMaxima::MaximaEvent dispatches each
  incoming chunk to the matching handler here. Holds a reference back to that
  frame for the services the handlers need (the worksheet, the sidebars, the
  status bar, the configuration).
*/
class MaximaResponseReader
{
public:
  explicit MaximaResponseReader(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Handles a <statusbar> document: shows its text in wxMaxima's status bar.
  void ReadStatusBar(const wxXmlDocument &xmldoc);

  //! Handles a <html-manual-keywords> document: opens the manual on the
  //! keyword(s) Maxima resolved a help request to.
  void ReadManualTopicNames(const wxXmlDocument &xmldoc);

  //! Appends a new chunk of typeset math Maxima sent to the worksheet.
  void ReadMath(const wxXmlDocument &xml);

  //! Hands a <wxxml-symbols> document (a new batch of auto-completable symbols)
  //! to the worksheet's autocompleter.
  void ReadLoadSymbols(const wxXmlDocument &data);

  //! Handles Maxima's very first prompt after startup (banner, PID, readiness).
  void ReadFirstPrompt(const wxString &data);

  //! Handles a chunk of plain (non-XML) text Maxima sent: classifies it as
  //! output/warning/error and appends it to the worksheet.
  void ReadMiscText(const wxString &data);

  //! Handles output Maxima marked as suppressed; also the startup
  //! authentication handshake (the <wxxml-key> exchange).
  void ReadSuppressedOutput(const wxString &data);

  //! Handles a new input/question prompt: advances the evaluation queue or
  //! surfaces the question, and tracks Maxima's lisp/maxima mode.
  void ReadPrompt(const wxString &data);

  //! Reads and reports whatever Maxima forwarded on its stdout/stderr streams
  //! (most prominently the gnuplot subprocess's diagnostics).
  void ReadStdErr();

private:
  //! The wxMaxima frame whose services the handlers drive. Not owned; the
  //! frame owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMARESPONSEREADER_H
