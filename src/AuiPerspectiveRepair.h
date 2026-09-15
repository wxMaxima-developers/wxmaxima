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
  Declares RepairAuiPerspective(), which makes a stored wxAUI layout safe to
  load.
*/

#ifndef AUIPERSPECTIVEREPAIR_H
#define AUIPERSPECTIVEREPAIR_H

#include <wx/string.h>

/*! Returns \p perspective with any invalid centre-pane geometry corrected.

  wxAUI requires the centre pane to sit at dock layer, row and position 0;
  wxAuiPaneInfo::IsValid() asserts otherwise. wxMaxima used to declare the
  worksheet pane with both .Center() and .Row(2), which was fixed in the
  code -- but every perspective saved before that fix still says row=2, and
  those live in users' config files indefinitely.

  Loading one is not harmless. wxAuiManager::LoadPerspective() hands each
  stored pane to wxAuiPaneInfo::SafeSet(), which validates it and *discards
  it entirely* if it is invalid -- so the worksheet's stored geometry is
  silently thrown away in a release build, and the assert aborts wxMaxima
  outright in a debug one. Neither is something the user can act on, and
  neither is fixed by correcting the pane afterwards: by then LoadPerspective()
  has already rejected it.

  Repairing the string before it is ever handed to wxAUI is what makes an old
  config load cleanly instead. Anything this function does not recognise is
  passed through untouched -- a perspective is opaque, versioned, wxAUI-owned
  data, and the goal is only to correct the one field wxMaxima itself is
  known to have written wrong.

  \param perspective A perspective string as stored by
                     wxAuiManager::SavePerspective().
  \return The same string with the centre pane's layer/row/pos forced to 0.
*/
wxString RepairAuiPerspective(const wxString &perspective);

#endif // AUIPERSPECTIVEREPAIR_H
