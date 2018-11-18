// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  
  This file defines the class ErrorRedirector that redirects wx Errors to a dialogue

  It is a customized copy of a portion of wxWidget's log.cpp.
 */

#include <wx/log.h>

class ErrorRedirector : public wxLog
{
public:
    /**
        Sets the specified @c logger (which may be @NULL) as the default log
        target but the log messages are also passed to the previous log target if any.
    */
    ErrorRedirector(wxLog* logger);

    /**
        Destroys the previous log target.
    */
    virtual ~ErrorRedirector();

  virtual void Flush();

  virtual void DoLogRecord(wxLogLevel level,
                           const wxString& msg,
                           const wxLogRecordInfo& info);
 
    /**
        Detaches the old log target so it won't be destroyed when the wxLogChain object
        is destroyed.
    */
    void DetachOldLog();

    /**
        Returns the pointer to the previously active log target (which may be @NULL).
    */
    wxLog* GetOldLog() const;

    /**
        Sets another log target to use (may be @NULL).

        The log target specified in the wxLogChain(wxLog*) constructor or in a
        previous call to this function is deleted.
        This doesn't change the old log target value (the one the messages are
        forwarded to) which still remains the same as was active when wxLogChain
        object was created.
    */
    void SetLog(wxLog* logger);
protected:
  //! the current log target
  wxLog *m_logNew;
  
  //! the previous log target
  wxLog *m_logOld;
};
