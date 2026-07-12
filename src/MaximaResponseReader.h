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
  socket-event handler (MaximaProcessManager::MaximaEvent) classifies each chunk and hands
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
#include <wx/hashmap.h>
#include <unordered_map>

class wxMaxima;

/*! The Maxima-response handlers extracted from the wxMaxima god class.

  Owned by value by the wxMaxima frame; MaximaProcessManager::MaximaEvent dispatches each
  incoming chunk to the matching handler here. Holds a reference back to that
  frame for the services the handlers need (the worksheet, the sidebars, the
  status bar, the configuration).
*/
class MaximaResponseReader
{
public:
  explicit MaximaResponseReader(wxMaxima &wxm) : m_wxMaxima(wxm) {
    RegisterVariableActions();
  }

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

  //! Handles a <variables> document: updates the variables sidebar, the
  //! table-synced menus and runs the per-variable action for each value
  //! Maxima advertised.
  void ReadVariables(const wxXmlDocument &xmldoc);

  //! Handles a <watch_variables_add> document: adds the named variables to the
  //! variables-sidebar watch list.
  void ReadAddVariables(const wxXmlDocument &xmldoc);

private:
  //! The wxMaxima frame whose services the handlers drive. Not owned; the
  //! frame owns this object.
  wxMaxima &m_wxMaxima;

  // --- Per-variable actions -------------------------------------------------
  // Run by ReadVariables() when Maxima advertises the value (or undefined-ness)
  // of one of the variables wxMaxima watches. Registered in the dispatch
  // tables below by RegisterVariableActions().

  //! Called if maxima tells us the value of the maxima variable display2d_unicode.
  void VariableActionDisplay2d_Unicode(const wxString &value);
  //! Called if maxima tells us the value of the maxima variable output_format_for_help.
  void VariableActionHtmlHelp(const wxString &value);
  //! Called if maxima tells us the value of the maxima variable sinnpiflag.
  void VariableActionSinnpiflag(const wxString &value);
  //! Called if maxima tells us that the maxima variable sinnpiflag is undefined.
  void VariableActionSinnpiflagUndefined();
  //! Called if maxima tells us where the user files are located.
  void VariableActionUserDir(const wxString &value);
  //! Called if maxima tells us where the temp files are located.
  void VariableActionTempDir(const wxString &value);
  //! Called if maxima tells us the maxima version as defined by autoconf.
  void VariableActionAutoconfVersion(const wxString &value);
  //! Called if maxima tells us the maxima build host as defined by autoconf.
  void VariableActionAutoconfHost(const wxString &value);
  //! Called if maxima tells us the maxima info dir.
  void VariableActionMaximaInfodir(const wxString &value);
  //! Called if maxima tells us the maxima html dir.
  void VariableActionMaximaHtmldir(const wxString &value);
  //! Called if maxima tells us the value of the maxima variable <code>gnuplot</code>
  void VariableActionGnuplotCommand(const wxString &value);
  //! Called if maxima tells us the maxima share dir.
  void VariableActionMaximaSharedir(const wxString &value);
  //! Called if maxima tells us the maxima demo dir.
  void VariableActionMaximaDemodir(const wxString &value);
  //! Called if maxima tells us the lisp name.
  void VariableActionLispName(const wxString &value);
  //! Called if maxima tells us the lisp version.
  void VariableActionLispVersion(const wxString &value);
  //! Called if maxima tells us the name of a package that was loaded
  void VariableActionWxLoadFileName(const wxString &value);
  //! Called if maxima tells us the value of the maxima variable <code>display2d</code>
  void VariableActionDisplay2D(const wxString &value);
  //! Called if maxima tells us if it currently outputs XML
  void VariableActionAltDisplay2D(const wxString &value);
  //! Called if maxima tells us the value of the maxima variable <code>engineering_format_floats</code>
  void VariableActionEngineeringFormat(const wxString &value);
  //! Called if maxima sends us the list of known operators
  void VariableActionOperators(const wxString &value);
  /*! Derive the display mode (1D/2D ASCII/unicode/graphical) from the
    display2d and *alt-display2d* variable values Maxima sent us and
    apply it to the configuration and the display-math-as menu.
  */
  void UpdateDisplayMode();

  //! Populates the dispatch tables below (once; they are static).
  void RegisterVariableActions();

  typedef void (MaximaResponseReader::*VarReadFunction)(const wxString &value);
  typedef void (MaximaResponseReader::*VarUndefinedFunction)();
  typedef std::unordered_map <wxString, VarReadFunction, wxStringHash> VarReadFunctionHash;
  typedef std::unordered_map <wxString, VarUndefinedFunction,
                              wxStringHash> VarUndefinedFunctionHash;
  //! Maps a variable name to the action run when Maxima advertises its value.
  static VarReadFunctionHash m_variableReadActions;
  //! Maps a variable name to the action run when Maxima reports it undefined.
  static VarUndefinedFunctionHash m_variableUndefinedActions;
};

#endif // MAXIMARESPONSEREADER_H
