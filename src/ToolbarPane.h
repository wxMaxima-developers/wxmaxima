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
  Declares SetToolbarPaneProperties(), which gives the main toolbar's wxAUI
  pane its properties -- both when the pane is first added and again after a
  stored perspective has been loaded over it.
*/

#ifndef TOOLBARPANE_H
#define TOOLBARPANE_H

#include <wx/aui/framemanager.h>
#include <wx/aui/auibar.h>

/*! Sets the main toolbar pane's properties, its size included.

  The toolbar's height follows entirely from its contents -- icon size, which
  depends on the display's resolution, and the height of the controls in it,
  which depends on the font and the platform's theme. It is nothing the user
  can set, so it must be taken from the toolbar every time and never from a
  stored layout.

  wxAuiManager::SavePerspective() stores it anyway (as the pane's best size,
  and as the size of the dock the pane sits in), and LoadPerspective()
  restores the stored value over the one wxAuiManager::AddPane() had just
  taken from the toolbar. From then on nothing in wxAUI recomputes it: the
  toolbar is sized to whatever the previous session saved, which that
  session in turn had from the one before. So a size that once came out
  wrong is kept forever, and on the platforms without DPI-independent pixels
  (MS Windows) wxAUI additionally rescales the stored best size whenever the
  window changes to a display with a different DPI -- rounding every time,
  so the value can drift from one session to the next.

  Calling this after LoadPerspective() takes the size from the toolbar again.
  It also re-applies the pane's flags, which LoadPerspective() equally
  replaces by whatever an older wxMaxima stored: without DockFixed() the dock
  keeps its stored size instead of being sized from its contents on the next
  layout.

  The dock direction is left alone, as the user may have moved the toolbar to
  the bottom.

  \param pane    The toolbar's pane.
  \param toolbar The toolbar itself; Realize() must already have been called
                 on it so that its hint size is up to date.
  \return \p pane, so that further settings can be chained.
*/
wxAuiPaneInfo &SetToolbarPaneProperties(wxAuiPaneInfo &pane,
                                        const wxAuiToolBar *toolbar);

#endif // TOOLBARPANE_H
