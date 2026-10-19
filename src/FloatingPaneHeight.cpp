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
  Implements LimitFloatingPaneHeight(); see FloatingPaneHeight.h.
*/

#include "FloatingPaneHeight.h"
#include <wx/aui/floatpane.h>
#include <algorithm>

void LimitFloatingPaneHeight(wxWindow *paneWindow, int contentHeight) {
  if ((paneWindow == nullptr) || (contentHeight <= 0))
    return;
  // wxAUI reparents a pane it floats into a frame of its own, and back into
  // the managed frame when it is docked again.
  wxAuiFloatingFrame *frame =
    wxDynamicCast(paneWindow->GetParent(), wxAuiFloatingFrame);
  if (frame == nullptr)
    return;
  // wxGTK sets a hidden frame's minimum size from its sizer only when it is
  // first shown, and asserts if that exceeds a maximum set before, with
  // contents that may have grown since. Wait for the frame to be shown.
  if (!frame->IsShown())
    return;
  // The frame's caption and borders, plus whatever wxAUI puts around the
  // pane inside it, come on top of the pane's own height.
  const int decoration = frame->GetSize().y - paneWindow->GetSize().y;
  const int maxHeight = contentHeight + std::max(decoration, 0);
  // Set both at once: the minimum height wxAUI carried over from the pane
  // must not exceed the maximum, or the toolkit refuses the size hints.
  if (frame->GetMaxSize().y != maxHeight) {
    const wxSize minSize = frame->GetMinSize();
    frame->SetSizeHints(wxSize(minSize.x, std::min(minSize.y, maxHeight)),
                        wxSize(wxDefaultCoord, maxHeight));
  }
  if (frame->GetSize().y > maxHeight)
    frame->SetSize(wxSize(frame->GetSize().x, maxHeight));
}
