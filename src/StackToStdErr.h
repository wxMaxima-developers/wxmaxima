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

  This file defines the class StackToStdErr that prints a stack backtrace to stderr.
*/

#ifndef STACKTOSTDERR_H
#define STACKTOSTDERR_H
#include "precomp.h"
#include <wx/log.h>
#include <memory>
#include <wx/stackwalk.h>
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT

//! Redirect error messages (but not warnings) to a second target.
class StackToStdErr : public wxStackWalker
{
public:
  StackToStdErr(const char *argv0 = NULL): wxStackWalker(argv0){}
  void OnStackFrame(const wxStackFrame &frame) override;
};

#endif // STACKTOSTDERR_H

#endif
