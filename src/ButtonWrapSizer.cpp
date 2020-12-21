// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
#include "UnicodeSidebar.h"
#include "wx/sizer.h"
#include <wx/settings.h>
Buttonwrapsizer::Buttonwrapsizer(int orient) :
  wxWrapSizer(orient)
{
}

void Buttonwrapsizer::RecalcSizes()
{
  wxSizerItemList children = GetChildren();
  long width = 50;
  long height = 20;
  for (wxSizerItemList::Node *node = children.GetFirst(); node; node = node->GetNext())
  {
    wxSizerItem* current =  node->GetData();
    wxWindow *item = current->GetWindow();
    if(item->GetBestSize().x > width)
      width = item->GetBestSize().x;
    if(item->GetBestSize().y > height)
      width = item->GetBestSize().y;
  }
  
  children = GetChildren();
  for (wxSizerItemList::Node *node = children.GetFirst(); node; node = node->GetNext())
  {
    wxSizerItem* current =  node->GetData();
    wxWindow *item = current->GetWindow();
    wxSize bestSize(width, height);
    item->SetInitialSize(bestSize);
    item->SetMinSize(bestSize);
    item->SetSize(bestSize);
  }
  wxWrapSizer::RecalcSizes();
}

