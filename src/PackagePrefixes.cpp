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
  Defines wxm::PackagePrefixes(), where on macOS to look for Maxima and gnuplot.
*/

#include "PackagePrefixes.h"
#include <algorithm>

std::vector<wxString> wxm::PackagePrefixes(const wxString &installPrefix) {
  std::vector<wxString> prefixes;
  auto add = [&prefixes](wxString prefix) {
    // "/" itself is a legitimate (if odd) prefix and keeps its slash.
    while ((prefix.Length() > 1) && prefix.EndsWith(wxS("/")))
      prefix.RemoveLast();
    if (prefix.IsEmpty())
      return;
    if (std::find(prefixes.begin(), prefixes.end(), prefix) == prefixes.end())
      prefixes.push_back(prefix);
  };
  add(installPrefix);
  add(wxS("/opt/homebrew"));
  add(wxS("/usr/local"));
  add(wxS("/opt/local"));
  return prefixes;
}
