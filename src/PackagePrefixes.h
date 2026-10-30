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
  Declares wxm::PackagePrefixes(), where on macOS to look for Maxima and gnuplot.
*/

#ifndef PACKAGEPREFIXES_H
#define PACKAGEPREFIXES_H

#include <vector>
#include <wx/string.h>

namespace wxm {
/*! The installation prefixes to search for Maxima and gnuplot on macOS, best first

  A GUI program started from the Finder gets a minimal PATH that contains none
  of the places package managers install to, so wxMaxima has to know them.

  The first entry is the prefix wxMaxima itself was configured with
  (CMAKE_INSTALL_PREFIX): a package manager that builds wxMaxima -- MacPorts
  passes its own prefix, /opt/local by default -- installs Maxima and gnuplot
  there as well. That replaces the OSX_MACPORTS_PREFIX/OSX_MACPORTS_PREFER
  macros the MacPorts port used to patch into the source (GH #1347). After
  that come Homebrew (/opt/homebrew on Apple silicon, /usr/local on Intel) and
  MacPorts' default prefix, for a wxMaxima that came from somewhere else.

  \param installPrefix The prefix wxMaxima was configured with. Empty entries
         and trailing slashes are ignored; a prefix that is already in the
         list is not repeated.
  \return The prefixes, without trailing slash; each has a bin/ subdirectory
          the programs are searched in.
*/
std::vector<wxString> PackagePrefixes(const wxString &installPrefix);
} // namespace wxm

#endif // PACKAGEPREFIXES_H
