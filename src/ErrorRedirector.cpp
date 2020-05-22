// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C)      1998 Vadim Zeitlin <zeitlin@dptmaths.ens-cachan.fr>
//  Copyright (C)      2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C)      2020 Kuba Ober <kuba@bertec.com>
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
  
  This file contains the class ErrorRedirector that redirects wx Errors to a dialogue

  It is a customized copy of a portion of wxWidget's log.cpp.
 */

#include "ErrorRedirector.h"
#include <wx/datetime.h>
#include <cassert>
#include <iostream>

ErrorRedirector::ErrorRedirector(std::unique_ptr<wxLog> &&newLog) :
    m_logNew(newLog.get()),
    m_logOld(wxLog::GetActiveTarget()),
    m_logOwned(std::move(newLog))
{
  // Notice that we used GetActiveTarget() here instead of directly calling
  // SetActiveTarget() to trigger wxLog auto-creation: if we're created as
  // the first logger, we should still chain with the standard, implicit and
  // possibly still not created standard logger instead of disabling normal
  // logging entirely.
  wxLog::SetActiveTarget(this);
}

ErrorRedirector::~ErrorRedirector()
{
  assert(!m_logNew || m_logOwned.get() == m_logNew || (!m_logOwned && m_logNew == this));
  wxLog::SetActiveTarget(m_logOld);
}

void ErrorRedirector::SetLog(std::unique_ptr<wxLog> &&logger)
{
  m_logOwned = std::move(logger);
  m_logNew = m_logOwned.get();
  assert(!m_logNew || m_logOwned.get() == m_logNew || (!m_logOwned && m_logNew == this));
}

void ErrorRedirector::SetLogThis()
{
  m_logOwned.reset();
  m_logNew = this;
  assert(!m_logNew || m_logOwned.get() == m_logNew || (!m_logOwned && m_logNew == this));
}

void ErrorRedirector::DoLogRecord(wxLogLevel level,
                                  const wxString& msg,
                                  const wxLogRecordInfo& info)
{
  assert(!m_logNew || m_logOwned.get() == m_logNew || (!m_logOwned && m_logNew == this));
  // let the previous logger show it
  if (m_logOld)
    m_logOld->LogRecord(level, msg, info);

  // and also send it to the new one
  if (m_logNew && (m_messages_logPaneOnly <= 0))
  {
    if (m_logOwned)
    {
      // The owned log is not us and this ensures no infinite recursion
      assert(m_logOwned.get() != this);
      if((level == wxLOG_FatalError) || (level == wxLOG_Error))
      {
        m_logOwned->LogRecord(level, msg, info);
        m_logOwned->Flush();
      }
    }
    else
      wxLog::DoLogRecord(level, msg, info);
  }

  if (m_logToStdErr)
  {
    wxDateTime now;
    now.SetToCurrent();
    std::cerr << now.FormatTime() << ": " << msg << "\n";
  }
}

void ErrorRedirector::Flush()
{
  assert(!m_logNew || m_logOwned.get() == m_logNew || (!m_logOwned && m_logNew == this));
  if (m_logOld)
    m_logOld->Flush();

  if (m_logOwned)
  {
    // The owned log is not us and this ensures no infinite recursion
    assert(m_logOwned.get() != this);
    m_logOwned->Flush();
  }
}

int ErrorRedirector::m_messages_logPaneOnly = 0;

bool ErrorRedirector::m_logToStdErr = false;
