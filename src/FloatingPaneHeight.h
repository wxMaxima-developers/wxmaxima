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
  Declares LimitFloatingPaneHeight(), which keeps a floating sidebar from
  being made higher than its contents.
*/

#ifndef FLOATINGPANEHEIGHT_H
#define FLOATINGPANEHEIGHT_H

class wxWindow;

/*! Don't let the window a floating sidebar floats in grow higher than needed

  A sidebar that wxAUI floats lives in a wxAuiFloatingFrame of its own, which
  the user can resize freely. For a sidebar whose contents have a natural
  height (the find/replace pane, the wizard) any height beyond that only
  adds empty space, so this caps the frame's height at what shows
  \p contentHeight pixels of \p paneWindow, and shrinks the frame if it is
  higher than that already. The width stays free.

  Does nothing while \p paneWindow is docked, i.e. is not the child of a
  floating frame. Call it whenever the pane may have been floated or its
  contents may have changed height; calling it again with the same height
  changes nothing, so it is safe to call from the pane's size event handler.
*/
void LimitFloatingPaneHeight(wxWindow *paneWindow, int contentHeight);

#endif // FLOATINGPANEHEIGHT_H
