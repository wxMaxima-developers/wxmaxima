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

wxSize Buttonwrapsizer::CalcMin() {
  wxSizerItemList children = GetChildren();
  wxCoord width = -1;
  wxCoord height = 20;
  for (auto node = children.GetFirst(); node; node = node->GetNext()) {
    wxSizerItem *current = node->GetData();
    wxWindow *item = current->GetWindow();
    // Clear the value we have written into the item's best size cache
    item->CacheBestSize(wxDefaultSize);
    width = std::max(width, item->GetBestSize().x);
    height = std::max(height, item->GetBestSize().y);
  }
  int items = m_availSize / width;
  if(items < 1)
    items = 1;
  width = m_availSize / items;
  
  //  if(width < m_availSize)
  //    width = m_availSize / (m_availSize / width);

  wxSize bestSize(width, height);
  for (auto node = children.GetFirst(); node; node = node->GetNext()) {
    wxSizerItem *current = node->GetData();
    wxWindow *item = current->GetWindow();
    item->CacheBestSize(bestSize);
  }
  return wxWrapSizer::CalcMin();
}
