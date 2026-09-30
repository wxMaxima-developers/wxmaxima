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
  Defines SetToolbarPaneProperties(), see ToolbarPane.h.
*/

#include "ToolbarPane.h"

wxAuiPaneInfo &SetToolbarPaneProperties(wxAuiPaneInfo &pane,
                                        const wxAuiToolBar *toolbar) {
  pane.TopDockable(true)
    .BottomDockable(true)
    .LeftDockable(false)
    .RightDockable(false)
    .CaptionVisible(false)
    .CloseButton(false)
    .DockFixed()
    .Floatable(false)
    .Gripper(false)
    // A stored minimum would override the contents' size just like a stored
    // best size does; AddPane() never set one.
    .MinSize(wxDefaultSize);

  if (toolbar != nullptr) {
    // The toolbar is horizontal-only (wxAUI_TB_HORIZONTAL), so the hint for
    // a top dock is the right one for the bottom dock, too.
    pane.BestSize(toolbar->GetHintSize(wxAUI_DOCK_TOP));
  }
  return pane;
}
