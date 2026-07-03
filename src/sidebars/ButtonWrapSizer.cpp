// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class CharacterButton

  This file contains the definition of the class CharacterButton that allows to
  select arbitrary unicode symbols.
*/

#include "ButtonWrapSizer.h"
#include <wx/settings.h>
#include <wx/sizer.h>

Buttonwrapsizer::Buttonwrapsizer(int orient) : wxWrapSizer(orient) {}

int Buttonwrapsizer::HeightForWidth(int width) const {
  if (width <= 0)
    return 0;
  // Match CalcMin()'s model: every shown button is laid out at the same (largest)
  // size, so as many as fit in the width share a row.
  int maxWidth = 1, maxHeight = 1, shown = 0;
  for (auto node = GetChildren().GetFirst(); node; node = node->GetNext()) {
    const wxSizerItem *item = node->GetData();
    if (!item->IsShown())
      continue;
    const wxWindow *win = item->GetWindow();
    if (win)
      win->CacheBestSize(wxDefaultSize); // drop any uniform width cached by CalcMin
    const wxSize best = win ? win->GetBestSize() : item->GetMinSize();
    maxWidth = std::max(maxWidth, best.x);
    maxHeight = std::max(maxHeight, best.y);
    ++shown;
  }
  if (shown == 0)
    return 0;
  const int perRow = std::max(1, width / maxWidth);
  const int rows = (shown + perRow - 1) / perRow;
  return rows * maxHeight;
}

wxSize Buttonwrapsizer::CalcMin() {
  wxSizerItemList children = GetChildren();
  wxCoord width = -1;
  wxCoord height = 20;
  for (auto node = children.GetFirst(); node; node = node->GetNext()) {
    const wxSizerItem *current = node->GetData();
    const wxWindow *item = current->GetWindow();
    // Clear the value we have written into the item's best size cache
    item->CacheBestSize(wxDefaultSize);
    width = std::max(width, item->GetBestSize().x);
    height = std::max(height, item->GetBestSize().y);
  }
  // Only impose the grid-aligning uniform width when both inputs are valid:
  //  - m_availSize (wxWrapSizer's available size in the wrap direction) is -1
  //    until the first RepositionChildren() (see wx/wrapsizer.h); using it while
  //    invalid just yields a width that GetBestSize() ignores anyway.
  //  - width is 0 when every button is hidden (a font that renders no Greek
  //    letter hides them all): "m_availSize / width" would then divide by zero.
  // Otherwise leave the buttons at their natural size for wxWrapSizer to wrap.
  if ((m_availSize > 0) && (width > 0)) {
    int items = m_availSize / width;
    if (items < 1)
      items = 1;
    width = m_availSize / items;

    wxSize bestSize(width, height);
    for (auto node = children.GetFirst(); node; node = node->GetNext()) {
      const wxSizerItem *current = node->GetData();
      const wxWindow *item = current->GetWindow();
      item->CacheBestSize(bestSize);
    }
  }
  return wxWrapSizer::CalcMin();
}
