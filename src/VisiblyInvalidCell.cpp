// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class VisiblyInvalidCell

  VisiblyInvalidCell is the Cell that is used as a placeholder for items that
  should be overridden, before they are displayed.
 */

#include "VisiblyInvalidCell.h"
#include "CellImpl.h"
#include "StringUtils.h"

static wxString cellContents(wxT("?"));

VisiblyInvalidCell::VisiblyInvalidCell(GroupCell *parent,
                                       Configuration **config)
    : TextCell(parent, config, cellContents, TS_ERROR)
{
  InitBitFields();
  // We cannot do this at the startup of the program as we first need to wait
  // for the language selection to take place.
  // NOTE: static variables are initialized exactly 0 or 1 times, so the below
  // is not wasteful.
  SetToolTip(&T_("Missing contents. Bug?"));
}

VisiblyInvalidCell::VisiblyInvalidCell(GroupCell *parent,
                                       Configuration **config, wxString &&toolTip)
    : TextCell(parent, config, cellContents, TS_ERROR)
{
  InitBitFields();
  SetToolTip(std::move(toolTip));
}

VisiblyInvalidCell::VisiblyInvalidCell(GroupCell *parent,
                                       Configuration **config, const wxString *toolTip)
    : TextCell(parent, config, cellContents, TS_ERROR)
{
  InitBitFields();
  SetToolTip(toolTip);
}

DEFINE_CELL_TYPEINFO(VisiblyInvalidCell)
