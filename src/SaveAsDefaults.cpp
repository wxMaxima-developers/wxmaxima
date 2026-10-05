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
  Defines SaveAsDefaults(), what the "Save As" dialog starts out with.
*/

#include "SaveAsDefaults.h"
#include <wx/filename.h>

SaveAsDefault SaveAsDefaults(const wxString &currentFile,
                             const wxString &defaultExt,
                             const wxString &untitled) {
  wxString name = untitled;
  wxString ext = defaultExt;
  if (!currentFile.IsEmpty())
    wxFileName::SplitPath(currentFile, nullptr, &name, &ext);

  SaveAsDefault result;
  // Only .wxmx and .wxm can be saved. Anything else (a .mac or .out that was
  // opened, say) is offered as a .wxmx under its old name.
  if (ext.Lower() == wxS("wxm")) {
    result.extension = wxS("wxm");
    result.filterIndex = 1;
  } else {
    result.extension = wxS("wxmx");
    result.filterIndex = 0;
  }
  result.name = name + wxS(".") + result.extension;
  return result;
}
