// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#include "LogPane.h"

LogPane::LogPane(wxWindow *parent, wxWindowID id) : wxPanel(parent, id)
{
  wxBoxSizer *vbox  = new wxBoxSizer(wxVERTICAL);

  wxTextCtrl *textCtrl = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
					wxDefaultSize,
					wxTE_MULTILINE | wxTE_READONLY | wxHSCROLL);

  vbox->Add(textCtrl, wxSizerFlags().Expand().Proportion(10));

  SetSizerAndFit(vbox);
  wxLog::SetActiveTarget(m_logPanelTarget = new wxLogTextCtrl(textCtrl));
  m_errorRedirector = new ErrorRedirector(new wxLogGui());

  // m_logPanelTarget->SetRepetitionCounting();
  // m_logPanelTarget->DisableTimestamp();
  SetMinSize(wxSize(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )/10,
                    wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )/10));
}

LogPane::~LogPane()
{
  // m_logPanelTarget is automatically destroyed in this step..
  wxDELETE(m_errorRedirector);
  wxLog::SetActiveTarget(new wxLogGui());
  m_errorRedirector = NULL;
  m_logPanelTarget = NULL;
}
