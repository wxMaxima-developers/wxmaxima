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

#ifndef NULLLOG_H
#define NULLLOG_H

#include "precomp.h"
#include <wx/log.h>
#include <memory>

//! Redirect error messages (but not warnings) to a second target.
class NullLog : public wxLog
{
public:
  /**
     Sets the specified @c logger (which may be NULL) as the default log
     target but the log messages are also passed to the previous log target if any.
  */
  explicit NullLog();

  //! Restores the previous log target
  virtual ~NullLog() override {}

  /*! This method is called from the idle loop.

    All log targets collect log messages between calls to Flush.
  */
  void Flush() override {}

  void DoLogRecord(wxLogLevel level,
                   const wxString& msg,
                   const wxLogRecordInfo& info) override {}

};

#endif // NULLLOG_H
