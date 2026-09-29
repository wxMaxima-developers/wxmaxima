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
  Implements DescendantPids().
*/

#include "ProcessTree.h"
#include <cstddef>
#include <unordered_set>

std::vector<long> DescendantPids(long root, const std::vector<ProcessEntry> &processes)
{
  std::vector<long> result;
  if (root <= 0)
    return result;
  std::unordered_set<long> seen{root};
  result.push_back(root);
  // result doubles as the breadth-first queue: everything past `next` still
  // has to have its children looked up.
  for (std::size_t next = 0; next < result.size(); ++next) {
    const long parent = result[next];
    for (const auto &process : processes) {
      if ((process.parentPid == parent) && (process.pid > 0) &&
          seen.insert(process.pid).second)
        result.push_back(process.pid);
    }
  }
  return result;
}
