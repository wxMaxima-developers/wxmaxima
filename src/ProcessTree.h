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
  Declares DescendantPids(), which finds every process started below a given one.
*/

#ifndef PROCESSTREE_H
#define PROCESSTREE_H

#include <vector>

//! One process of a process list: its own id and the id of its parent.
struct ProcessEntry
{
  long pid;
  long parentPid;
};

/*! Returns \p root and all its descendants in \p processes, root first.

  On MS Windows wxMaxima does not start Lisp directly: it starts maxima.bat
  (or whatever the user configured as Maxima), which in turn starts the Lisp.
  To interrupt a computation we need to find the named shared-memory segment
  the Lisp created, and that name contains the Lisp's own process id -- which
  is only known if Maxima's first prompt told us about it. Walking the process
  tree below the process we started gives us every candidate instead.

  The walk is breadth-first, so the closest descendants come first. It is
  deliberately robust against what a real Windows process snapshot can
  contain: process ids are reused, so a parent id may name a process that
  died and whose id was handed to an unrelated process since. That can make
  an unrelated process look like a child, or even create a cycle; every pid
  is therefore reported at most once. A false positive is harmless for the
  caller, which only looks for a shared-memory segment of that name.

  Pure and portable, so it can be tested on every platform.
*/
std::vector<long> DescendantPids(long root, const std::vector<ProcessEntry> &processes);

#endif // PROCESSTREE_H
