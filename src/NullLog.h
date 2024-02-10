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

  This file defines the class NullLog that just ignores log messages it is sent
*/

#ifndef NULLLOG_H
#define NULLLOG_H

#include <wx/log.h>

//! Redirect error messages to /dev/null
class NullLog : public wxLog
{
public:
  explicit NullLog();

  virtual ~NullLog() override {}

  /*! This method is called from the idle loop.

    All log targets collect log messages between calls to Flush.
  */
  void Flush() override {}

  void DoLogRecord(wxLogLevel WXUNUSED(level),
                   const wxString& WXUNUSED(msg),
                   const wxLogRecordInfo& WXUNUSED(info)) override {}

};

#endif // NULLLOG_H
