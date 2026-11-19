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
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  Finding an option on the command line before it is parsed.

  wxMaxima sets up its UI language before wxCmdLineParser runs: the parser's
  own messages (an unknown option, the "Usage:" of --help) are translated
  through the catalogues that need the language first. An option that
  decides the language itself (--language) therefore has to be found by a
  simpler look at the arguments first. The real parser still sees it later,
  and checks it like any other option.
*/
#ifndef WXM_COMMANDLINEPRESCAN_H
#define WXM_COMMANDLINEPRESCAN_H

#include <wx/string.h>
#include <optional>
#include <vector>

namespace wxm::cmdline {

/*! The value of the long option --name, as wxCmdLineParser would read it

  Understands "--name=value" and "--name value". The last occurrence wins,
  as it does for wxCmdLineParser. Stops at a bare "--", after which
  everything is a file name, and skips the program name in args[0].

  \return the value, or no value if the option isn't there or is the last
          argument with nothing following it
*/
std::optional<wxString> PrescanLongOption(const std::vector<wxString> &args,
                                          const wxString &name);

} // namespace wxm::cmdline

#endif // WXM_COMMANDLINEPRESCAN_H
