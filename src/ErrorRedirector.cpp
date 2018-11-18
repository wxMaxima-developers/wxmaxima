// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C)      1998 Vadim Zeitlin <zeitlin@dptmaths.ens-cachan.fr>
//  Copyright (C)      2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  
  This file contains the class ErrorRedirector that redirects wx Errors to a dialogue

  It is a customized copy of a portion of wxWidget's log.cpp.
 */

#include "ErrorRedirector.h"
#include <iostream>

ErrorRedirector::ErrorRedirector(wxLog *logger) : wxLog()
{
    m_logNew = logger;

    // Notice that we use GetActiveTarget() here instead of directly calling
    // SetActiveTarget() to trigger wxLog auto-creation: if we're created as
    // the first logger, we should still chain with the standard, implicit and
    // possibly still not created standard logger instead of disabling normal
    // logging entirely.
    m_logOld = wxLog::GetActiveTarget();
    wxLog::SetActiveTarget(this);
}

ErrorRedirector::~ErrorRedirector()
{
    wxLog::SetActiveTarget(m_logOld);

    if ( m_logNew != this )
        delete m_logNew;
}

void ErrorRedirector::SetLog(wxLog *logger)
{
    if ( m_logNew != this )
        delete m_logNew;

    m_logNew = logger;
}

void ErrorRedirector::DoLogRecord(wxLogLevel level,
                             const wxString& msg,
                             const wxLogRecordInfo& info)
{
    // let the previous logger show it
    if ( m_logOld )
        m_logOld->LogRecord(level, msg, info);

    // and also send it to the new one
    if ( m_logNew )
    {
        // don't call m_logNew->LogRecord() to avoid infinite recursion when
        // m_logNew is this object itself
      if ( m_logNew != this )
      {
        if((level == wxLOG_FatalError) || (level == wxLOG_Error))
        {
          std::cerr<<msg<<"\n";
          m_logNew->LogRecord(level, msg, info);
          m_logNew->Flush();
        }
      }
      else
        wxLog::DoLogRecord(level, msg, info);
    }
}

void ErrorRedirector::Flush()
{
    if ( m_logOld )
        m_logOld->Flush();

    // be careful to avoid infinite recursion
    if ( m_logNew && m_logNew != this )
        m_logNew->Flush();
}
