// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  The worksheet's right-click context menu.

  Building the menu is a pure function of the worksheet's current state (what
  is selected, which cell is active, where the click landed): it appends items
  to a wxMenu and does not show it. That makes it testable headlessly and
  keeps the ~800 lines of menu construction out of the Worksheet class; the
  mouse handling around it (hit testing, mouse capture, actually popping up
  the menu) stays in Worksheet::OnMouseRightDown.
*/

#ifndef WORKSHEETCONTEXTMENU_H
#define WORKSHEETCONTEXTMENU_H

class Worksheet;
class wxMenu;

/*! Appends the context-menu items matching the worksheet's current state.

  \param worksheet The worksheet whose state (selection, active cell, ...)
  decides which items appear. Its m_replacementsForCurrentWord is refilled
  with the spelling suggestions the menu offers.
  \param popupMenu The menu to append to; the caller shows it if any items
  were appended.
  \param downx,downy The click position in unscrolled worksheet coordinates.
  \param clickInSelection Whether the click landed inside the selection.
*/
void PopulateWorksheetContextMenu(Worksheet &worksheet, wxMenu &popupMenu,
                                  int downx, int downy, bool clickInSelection);

#endif // WORKSHEETCONTEXTMENU_H
