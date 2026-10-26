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
  Implements the troubleshooting checks' names and helpers; see
  TroubleshootingChecks.h.
*/

#include "TroubleshootingChecks.h"
#include <wx/intl.h>
#include <wx/tokenzr.h>

std::vector<wxm::TroubleshootingCheck> wxm::AllTroubleshootingChecks() {
  std::vector<TroubleshootingCheck> checks;
  for (int i = 0; i < static_cast<int>(TroubleshootingCheck::count); i++)
    checks.push_back(static_cast<TroubleshootingCheck>(i));
  return checks;
}

wxString wxm::TroubleshootingCheckTitle(TroubleshootingCheck check) {
  switch (check) {
  case TroubleshootingCheck::maximaFound:
    return _("Can wxMaxima find Maxima?");
  case TroubleshootingCheck::server:
    return _("Can wxMaxima offer Maxima a network connection?");
  case TroubleshootingCheck::maximaStarts:
    return _("Can Maxima be started?");
  case TroubleshootingCheck::maximaConnects:
    return _("Does Maxima connect to wxMaxima?");
  case TroubleshootingCheck::firstPrompt:
    return _("Does Maxima send its first prompt?");
  case TroubleshootingCheck::authentication:
    return _("Does Maxima authenticate?");
  case TroubleshootingCheck::wxMathML:
    return _("Does Maxima accept wxMaxima's Lisp code?");
  case TroubleshootingCheck::loadPackage:
    return _("Can Maxima load packages?");
  case TroubleshootingCheck::loadDraw:
    return _("Can Maxima load draw?");
  case TroubleshootingCheck::tempDir:
    return _("Is Maxima's temp directory usable?");
  case TroubleshootingCheck::gnuplotFound:
    return _("Can wxMaxima find gnuplot?");
  case TroubleshootingCheck::gnuplotPng:
    return _("Can gnuplot draw png plots?");
  case TroubleshootingCheck::gnuplotPngCairo:
    return _("Can gnuplot draw pngcairo plots?");
  case TroubleshootingCheck::maximaStops:
    return _("Can wxMaxima stop Maxima?");
  case TroubleshootingCheck::maximaHtmlManual:
    return _("Is there an HTML manual of Maxima?");
  case TroubleshootingCheck::maximaInfoManual:
    return _("Is there an info manual of Maxima (for ? and describe())?");
  case TroubleshootingCheck::wxMaximaHtmlManual:
    return _("Is there an HTML manual of wxMaxima?");
  case TroubleshootingCheck::helpKnowsManual:
    return _("Does wxMaxima's help find Maxima's manual?");
  case TroubleshootingCheck::helpSidebar:
    return _("Can wxMaxima show help in a sidebar?");
  case TroubleshootingCheck::externalBrowser:
    return _("Can wxMaxima show help in a web browser?");
  case TroubleshootingCheck::translations:
    return _("Did wxMaxima find its translations?");
  case TroubleshootingCheck::svgText:
    return _("Can wxWidgets draw the text in SVG images?");
  case TroubleshootingCheck::aiSidebar:
    return _("Can wxMaxima offer the AI chat sidebar?");
  case TroubleshootingCheck::count:
    break;
  }
  return wxEmptyString;
}

wxString wxm::TroubleshootingStatusName(TroubleshootingStatus status) {
  switch (status) {
  case TroubleshootingStatus::pending:
    return _("Waiting");
  case TroubleshootingStatus::running:
    return _("Checking...");
  case TroubleshootingStatus::ok:
    return _("OK");
  case TroubleshootingStatus::info:
    return _("Note");
  case TroubleshootingStatus::warning:
    return _("Warning");
  case TroubleshootingStatus::failed:
    return _("Failed");
  case TroubleshootingStatus::skipped:
    return _("Skipped");
  }
  return wxEmptyString;
}

namespace {
//! The untranslated status word a bug report shows
wxString ReportStatusName(wxm::TroubleshootingStatus status) {
  switch (status) {
  case wxm::TroubleshootingStatus::pending:
    return wxS("NOT RUN");
  case wxm::TroubleshootingStatus::running:
    return wxS("RUNNING");
  case wxm::TroubleshootingStatus::ok:
    return wxS("OK");
  case wxm::TroubleshootingStatus::info:
    return wxS("NOTE");
  case wxm::TroubleshootingStatus::warning:
    return wxS("WARNING");
  case wxm::TroubleshootingStatus::failed:
    return wxS("FAILED");
  case wxm::TroubleshootingStatus::skipped:
    return wxS("SKIPPED");
  }
  return wxEmptyString;
}

//! Appends \p text to \p report, every line of it indented
void AppendIndented(wxString &report, const wxString &text) {
  wxStringTokenizer lines(text, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (lines.HasMoreTokens()) {
    const wxString line = lines.GetNextToken();
    if (!line.IsEmpty())
      report += wxS("    ") + line + wxS("\n");
  }
}
} // namespace

wxString wxm::TroubleshootingReport(const std::vector<TroubleshootingResult> &results) {
  wxString report;
  for (const auto &result : results) {
    report += wxS("[") + ReportStatusName(result.status) + wxS("] ") +
      TroubleshootingCheckTitle(result.check) + wxS("\n");
    AppendIndented(report, result.details);
    AppendIndented(report, result.hint);
  }
  return report;
}

wxString wxm::TroubleshootingProbeValue(const wxString &text, const wxString &key,
                                        bool *found) {
  *found = false;
  const wxString marker = wxS("wxprobe-") + key + wxS("=");
  size_t pos = 0;
  while ((pos = text.find(marker, pos)) != wxString::npos) {
    // Only at the start of a line: a load() message could quote the query.
    if ((pos == 0) || (text[pos - 1] == wxS('\n'))) {
      const size_t start = pos + marker.Length();
      const size_t end = text.find(wxS('\n'), start);
      if (end == wxString::npos)
        return wxEmptyString;
      *found = true;
      wxString value = text.SubString(start, end - 1);
      if (value.EndsWith(wxS("\r")))
        value.RemoveLast();
      return value;
    }
    pos += marker.Length();
  }
  return wxEmptyString;
}

bool wxm::PathHasQuote(const wxString &path) {
  return (path.Find(wxS('\'')) != wxNOT_FOUND) || (path.Find(wxS('"')) != wxNOT_FOUND);
}

bool wxm::IsPngData(const unsigned char *data, std::size_t size) {
  static const unsigned char signature[] = {0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n'};
  if (size < sizeof(signature))
    return false;
  for (std::size_t i = 0; i < sizeof(signature); i++)
    if (data[i] != signature[i])
      return false;
  return true;
}

wxString wxm::GnuplotSingleQuoted(const wxString &path) {
  wxString quoted = path;
  quoted.Replace(wxS("'"), wxS("''"));
  return wxS("'") + quoted + wxS("'");
}

bool wxm::GnuplotLacksTerminal(const wxString &gnuplotErrors) {
  return gnuplotErrors.Find(wxS("unknown or ambiguous terminal")) != wxNOT_FOUND;
}
