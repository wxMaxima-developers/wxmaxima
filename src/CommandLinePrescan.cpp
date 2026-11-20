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
*/

#include "CommandLinePrescan.h"

std::optional<wxString>
wxm::cmdline::PrescanLongOption(const std::vector<wxString> &args,
                                const wxString &name) {
  const wxString option = wxS("--") + name;
  const wxString optionIs = option + wxS("=");
  std::optional<wxString> value;
  for (std::size_t i = 1; i < args.size(); ++i) {
    const wxString &arg = args[i];
    if (arg == wxS("--"))
      break;
    if (arg.StartsWith(optionIs))
      value = arg.Mid(optionIs.Length());
    else if ((arg == option) && (i + 1 < args.size()))
      value = args[++i];
  }
  return value;
}
