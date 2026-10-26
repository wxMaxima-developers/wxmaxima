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
  The Help > Troubleshooting dialog (GH #2559).
*/

#ifndef TROUBLESHOOTINGDIALOG_H
#define TROUBLESHOOTINGDIALOG_H

#include "precomp.h"
#include "TroubleshootingChecks.h"
#include "TroubleshootingRunner.h"
#include <wx/dataview.h>
#include <wx/dialog.h>
#include <wx/textctrl.h>
#include <memory>
#include <vector>

/*! Checks the things that commonly keep wxMaxima from working, and tells what it found

  Each check is a row: whether wxMaxima finds Maxima, whether Maxima starts,
  connects, prompts and authenticates, whether it loads packages and draw,
  whether gnuplot plots, whether the manuals, the help sidebar, a browser,
  the translations, SVG text and the AI sidebar are there. Selecting a row
  shows what was found and, for a problem, what can be done about it; "Copy
  report" puts all of it on the clipboard, for a bug report.

  The checks that need Maxima or gnuplot are run by a TroubleshootingRunner,
  with a Maxima of its own, so the worksheet's Maxima is left alone; the
  dialog stays usable while they run. The others only ask wxWidgets and
  wxMaxima's own state, and are done at once.
*/
class TroubleshootingDialog : public wxDialog
{
public:
  //! What the checks need to know about the running wxMaxima
  struct Inputs {
    //! How to start Maxima and where gnuplot is, as the worksheet does it
    TroubleshootingRunner::Settings runner;
    //! Where wxMaxima's own HTML manual is expected
    wxString wxMaximaManual;
    //! Where the worksheet's help would show the entry for integrate(); empty if nowhere
    wxString helpUrlForIntegrate;
    //! Whether the integrated help browser can be offered
    bool offerHelpSidebar = false;
    //! The language configured, a wxLanguage
    long language = 0;
  };

  TroubleshootingDialog(wxWindow *parent, const Inputs &inputs);

  //! The checks that need no Maxima: what wxWidgets and wxMaxima's own state say
  static std::vector<wxm::TroubleshootingResult> LocalChecks(const Inputs &inputs);

private:
  //! Starts (or restarts) all checks
  void RunChecks();
  //! Shows a result in its row
  void ShowResult(const wxm::TroubleshootingResult &result);
  //! Shows the selected row's details below the list
  void ShowDetails();
  void OnSelectionChanged(wxDataViewEvent &event);
  void OnRunAgain(wxCommandEvent &event);
  void OnCopyReport(wxCommandEvent &event);
  //! Called once the runner has reported everything
  void OnFinished();

  Inputs m_inputs;
  //! Each check's latest result, indexed by TroubleshootingCheck
  std::vector<wxm::TroubleshootingResult> m_results;
  std::unique_ptr<TroubleshootingRunner> m_runner;
  wxDataViewListCtrl *m_list = nullptr;
  wxTextCtrl *m_details = nullptr;
  wxButton *m_runAgain = nullptr;
};

#endif // TROUBLESHOOTINGDIALOG_H
