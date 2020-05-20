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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#include "LogPane.h"
LogPane::LogPane(wxWindow *parent, wxWindowID id, bool becomeLogTarget) : wxPanel(parent, id)
{
  m_isLogTarget = false;
  wxBoxSizer *vbox  = new wxBoxSizer(wxVERTICAL);

  m_textCtrl = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition,
					wxDefaultSize,
					wxTE_MULTILINE | wxTE_READONLY | wxHSCROLL);

  m_textCtrl->SetMinSize(wxSize(wxSystemSettings::GetMetric( wxSYS_SCREEN_X )/10,
                                wxSystemSettings::GetMetric( wxSYS_SCREEN_Y )/10));
  vbox->Add(m_textCtrl, wxSizerFlags().Expand().Proportion(1));
    
  if(becomeLogTarget)
    BecomeLogTarget();    

  // m_logPanelTarget->SetRepetitionCounting();
  // m_logPanelTarget->DisableTimestamp();
  SetSizerAndFit(vbox);
}

void LogPane::DropLogTarget()
{
  // m_logPanelTarget is automatically destroyed in this step.
  if(m_isLogTarget)
  {
    m_errorRedirector = NULL;
    wxLog::SetActiveTarget(NULL);
  }
  m_logPanelTarget = NULL;
  m_isLogTarget = false;
}

void LogPane::BecomeLogTarget()
{
  m_isLogTarget = true;
  m_logPanelTarget = std::unique_ptr<wxLog>(new wxLogTextCtrl(m_textCtrl));
  wxLog::SetActiveTarget(m_logPanelTarget.get());
  m_errorRedirector = std::unique_ptr<ErrorRedirector>(new ErrorRedirector(new wxLogGui()));
  #ifdef wxUSE_STD_IOSTREAM
  if(!ErrorRedirector::LoggingToStdErr())
    m_textRedirector = std::unique_ptr<wxStreamToTextRedirector>(new wxStreamToTextRedirector(m_textCtrl));
  #endif
}

LogPane::~LogPane()
{
  DropLogTarget();
  #ifdef wxUSE_STD_IOSTREAM
  m_textRedirector.reset();
  #endif
}

