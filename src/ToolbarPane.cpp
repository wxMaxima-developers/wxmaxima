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
  Defines AddToolbarPane() and SetToolbarPaneProperties(), see ToolbarPane.h.
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
#if wxCHECK_VERSION(3, 3, 4)
  // No dock size of its own: the dock is then sized from the best size set
  // above, plus the pane's border. (wxAuiPaneInfo only has a dock size since
  // wxWidgets 3.3, and only 3.3.4 started to set it from the best size.)
  pane.dock_size = 0;
#endif
  return pane;
}

void AddToolbarPane(wxAuiManager &manager, wxAuiToolBar *toolbar,
                    wxAuiPaneInfo pane) {
  manager.AddPane(toolbar, SetToolbarPaneProperties(pane, toolbar));
#if wxCHECK_VERSION(3, 3, 4)
  // AddPane() has just set the dock size to the pane's best size, which
  // leaves no room for the pane's border.
  manager.GetPane(toolbar).dock_size = 0;
#endif
}
