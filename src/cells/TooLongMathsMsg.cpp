// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class TooLongMathsMsg

  TooLongMathsMsg is the Cell that is used as a placeholder for items that
  should be overridden, before they are displayed.
*/

#include "TooLongMathsMsg.h"
#include "CellImpl.h"
#include "StringUtils.h"


TooLongMathsMsg::TooLongMathsMsg(GroupCell *group, Configuration *config)
  : TextCell(group, config,
             _("(wxMaxima is configured not to show long expressions - which would be slow)"),
             TS_ERROR) {
  InitBitFields_TooLongMathsMsg();
  InitToolTip();
  SetToolTip(&m_cellTooltip);
}

TooLongMathsMsg::TooLongMathsMsg(GroupCell *group, Configuration *config,
                                       wxString &&toolTip)
  : TextCell(group, config,
             _("(wxMaxima is configured not to show long expressions - which would be slow)"),
             TS_ERROR) {
  InitBitFields_TooLongMathsMsg();
  InitToolTip();
  SetToolTip(&m_cellTooltip);
}

TooLongMathsMsg::TooLongMathsMsg(GroupCell *group, Configuration *config,
                                       const wxString *toolTip)
  : TextCell(group, config,
             _("(wxMaxima is configured not to show long expressions - which would be slow)"),
             TS_ERROR) {
  InitBitFields_TooLongMathsMsg();
  InitToolTip();
  SetToolTip(&m_cellTooltip);
}

TooLongMathsMsg::TooLongMathsMsg(GroupCell *group,
                                       const TooLongMathsMsg &cell)
  : TooLongMathsMsg(group, cell.m_configuration) {}

DEFINE_CELL_TYPEINFO(TooLongMathsMsg)

void TooLongMathsMsg::InitToolTip()
{
  if(m_cellTooltip.IsEmpty())
    m_cellTooltip = _(
                      "The maximum size of the expressions wxMaxima is allowed to display "
                      "can be changed in the configuration dialogue.");
}

wxString TooLongMathsMsg::m_cellTooltip;
