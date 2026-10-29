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
  The checks the troubleshooting dialog runs (GH #2559), their results, and
  the pure helpers that interpret what the checks found.

  Nothing here starts a process or opens a window, so all of it can be
  tested directly; the checks themselves are run by TroubleshootingRunner
  (the ones that need Maxima or gnuplot) and by TroubleshootingDialog (the
  ones that only ask wxWidgets).
*/

#ifndef TROUBLESHOOTINGCHECKS_H
#define TROUBLESHOOTINGCHECKS_H

#include <wx/string.h>
#include <cstddef>
#include <vector>

namespace wxm {

//! One thing the troubleshooting dialog checks. The order is the display order.
enum class TroubleshootingCheck {
  maximaFound,        //!< The configured Maxima program exists
  server,             //!< We can open the local server Maxima connects to
  maximaStarts,       //!< The Maxima program can be run and stays alive
  maximaConnects,     //!< Maxima connects to our server
  firstPrompt,        //!< Maxima sends its first prompt
  authentication,     //!< Maxima proves it is the Maxima we started
  wxMathML,           //!< Maxima accepts wxMaxima's Lisp code (wxMathML.lisp)
  loadPackage,        //!< Maxima can load a package from its share directory
  loadDraw,           //!< Maxima can load draw, which most wxMaxima plots use
  tempDir,            //!< Maxima's temp directory is usable for plots
  gnuplotFound,       //!< The gnuplot program exists
  gnuplotPng,         //!< gnuplot can draw a png file
  gnuplotPngCairo,    //!< gnuplot can draw a pngcairo file (nicer plots)
  maximaStops,        //!< Maxima can be stopped from wxMaxima
  maximaHtmlManual,   //!< Maxima's HTML manual exists
  maximaInfoManual,   //!< Maxima's info manual (used by ? and describe())
  wxMaximaHtmlManual, //!< wxMaxima's own HTML manual exists
  helpKnowsManual,    //!< wxMaxima's help finds Maxima's manual
  helpSidebar,        //!< The integrated help browser can be offered
  externalBrowser,    //!< Something can open the manual in a web browser
  translations,       //!< The translation of wxMaxima's texts was found
  bidiText,           //!< libfribidi handles lines mixing both text directions
  svgText,            //!< wxWidgets' SVG renderer draws text (scalable plots)
  aiSidebar,          //!< The AI chat sidebar can be offered
  count               //!< Not a check: the number of checks
};

//! What a check found
enum class TroubleshootingStatus {
  pending, //!< Not run yet
  running, //!< Being run now
  ok,      //!< Works
  info,    //!< Not a problem, but worth knowing (e.g. "not needed here")
  warning, //!< Works, but not fully: some feature won't
  failed,  //!< Doesn't work
  skipped  //!< Couldn't be run, since a check it depends on failed
};

/*! The groups the dialog shows the checks in, one page each

  Like the configuration dialog's pages, these are what the user thinks
  about rather than the order the checks run in.
*/
enum class TroubleshootingSection {
  maxima,    //!< Starting and talking to Maxima
  plots,     //!< Packages, the temp directory, gnuplot and SVG
  help,      //!< The manuals and where help is shown
  userInterface, //!< Translations and the AI sidebar (not "interface": a macro on MSW)
  count      //!< Not a section: the number of sections
};

//! The result of one check
struct TroubleshootingResult {
  TroubleshootingCheck check = TroubleshootingCheck::maximaFound;
  TroubleshootingStatus status = TroubleshootingStatus::pending;
  //! What was found: paths, versions, error messages, ...
  wxString details;
  //! What the user can do about a problem. Empty if there is none.
  wxString hint;
};

//! All checks, in the order they are shown
std::vector<TroubleshootingCheck> AllTroubleshootingChecks();

//! The (translated) one-line name of a check
wxString TroubleshootingCheckTitle(TroubleshootingCheck check);

//! The (translated) word the dialog shows for a status
wxString TroubleshootingStatusName(TroubleshootingStatus status);

/*! The Unicode symbol the dialog shows in front of a status

  A check mark for ok and a cross for failed, so that the result can be
  seen at a glance; the dialog colours them.
*/
wxString TroubleshootingStatusSymbol(TroubleshootingStatus status);

//! All sections, in the order they are shown
std::vector<TroubleshootingSection> AllTroubleshootingSections();

//! The section a check is shown in
TroubleshootingSection TroubleshootingSectionOf(TroubleshootingCheck check);

//! The (translated) name of a section
wxString TroubleshootingSectionTitle(TroubleshootingSection section);

/*! The status that sums up a section

  The most important of its checks' statuses: a failure before a warning,
  before a check that still runs, ... before ok. So a section is only ok
  once all its checks are, and a failure shows on its page even while other
  checks still run.
*/
TroubleshootingStatus TroubleshootingSummaryStatus(const std::vector<TroubleshootingStatus> &statuses);

/*! The report the dialog copies to the clipboard, to be pasted into a bug report

  One block per check: its status, its title, and the details and hint
  indented below it. The status words are not translated, so that whoever
  reads the bug report can tell what failed whatever language it was made in.
*/
wxString TroubleshootingReport(const std::vector<TroubleshootingResult> &results);

/*! The value a probe query printed, as "wxprobe-<key>=<value>" on a line of its own

  The Maxima the troubleshooting dialog starts answers each query by
  printing such a line among whatever else it prints (load() messages, the
  output of wxMathML.lisp, ...). Only a complete line counts: a value whose
  line end hasn't arrived yet may still be incomplete.

  \param text  Everything Maxima has sent so far
  \param key   The key the query printed
  \param found Set to whether a complete line with that key was there
*/
wxString TroubleshootingProbeValue(const wxString &text, const wxString &key, bool *found);

//! True if a path contains a character draw can't cope with in its temp dir (GH #1106)
bool PathHasQuote(const wxString &path);

//! True if \p data starts with the signature every PNG file starts with
bool IsPngData(const unsigned char *data, std::size_t size);

//! A path as a single-quoted gnuplot string: gnuplot doubles a quote inside one
wxString GnuplotSingleQuoted(const wxString &path);

//! True if gnuplot's error output says that it doesn't know a terminal
bool GnuplotLacksTerminal(const wxString &gnuplotErrors);

} // namespace wxm

#endif // TROUBLESHOOTINGCHECKS_H
