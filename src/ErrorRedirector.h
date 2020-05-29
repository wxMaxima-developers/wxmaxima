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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  
  This file defines the class ErrorRedirector that redirects wx Errors to a dialogue

  It is a customized copy of a portion of wxWidget's log.cpp.
 */

#ifndef ERRORREDIRECTOR_H
#define ERRORREDIRECTOR_H

#include <wx/log.h>
#include <memory>

//! Redirect error messages (but not warnings) to a second target.
class ErrorRedirector : public wxLog
{
public:
  /*! A variable used by the SuppressErrorDialogs class

    >=0 means: Messages should appear in the log pane only.
   */
  static int m_messages_logPaneOnly;
  /**
     Sets the specified @c logger (which may be NULL) as the default log
     target but the log messages are also passed to the previous log target if any.
  */
  explicit ErrorRedirector(std::unique_ptr<wxLog> &&newLog);

  //! Restores the previous log target
  ~ErrorRedirector() override;

  /*! This method is called from the idle loop.

    All log targets collect log messages between calls to Flush.
  */
  void Flush() override;

  void DoLogRecord(wxLogLevel level,
                   const wxString& msg,
                   const wxLogRecordInfo& info) override;

  /**
     Detaches the old log target so it won't be destroyed when the wxLogChain object
     is destroyed.
  */
  void DetachOldLog();

  //! Sets the "batch mode" flag that causes error messages to be output to stderr, as well.
  void SetBatchMode(){m_batchMode = true;}
  /**
     Returns the pointer to the previously active log target (which may be NULL).
  */
  wxLog* GetOldLog() const;

  /**
     Sets and takes ownership of another log target to use (may be nullptr).

     The previously owned logger - if any - is destroyed.
     This doesn't change the old log target value (the one the messages are
     forwarded to) which still remains the same as was active when wxLogChain
     object was created.
  */
  void SetLog(std::unique_ptr<wxLog> &&logger);
  //! Sets ourseves as the log target
  void SetLogThis();

  //! Output all log messages to stderr, too.
  static void LogToStdErr(){m_logToStdErr = true;}

  //! Output all log messages to stderr, too.
  static bool LoggingToStdErr(){return m_logToStdErr;}

protected:
  //! the current log target - equal to m_logOwned or this
  wxLog *m_logNew = {};  
  //! the previous log target
  wxLog *const m_logOld = {};
  //! the owned new log, of null if no log is owned
  std::unique_ptr<wxLog> m_logOwned;

  bool m_batchMode = false;

private:
  //! Output all log messages to stderr, too?
  static  bool m_logToStdErr;
};

//! If a variable of this class is alive errors won't create popup dialogues
class SuppressErrorDialogs 
{
public:
  SuppressErrorDialogs(){ErrorRedirector::m_messages_logPaneOnly++;}
  ~SuppressErrorDialogs(){ErrorRedirector::m_messages_logPaneOnly--;}
};

#endif // ERRORREDIRECTOR_H
