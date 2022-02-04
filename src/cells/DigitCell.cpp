// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
  This file defines the class DigitCell

  DigitCells display one digit each of a number that is too long to fit into one
  line.
 */

#include "DigitCell.h"
#include "CellImpl.h"
#include "CellList.h"
#include "StringUtils.h"

DigitCell::DigitCell(GroupCell *group, Configuration **config, const wxString &text, TextStyle style):
  TextCell(group,config,text,style)
{}

DigitCell::DigitCell(GroupCell *group, const DigitCell &cell):
  DigitCell(group, cell.m_configuration, cell.m_text, cell.GetStyle())
{
}


DEFINE_CELL(DigitCell)

void DigitCell::Recalculate(AFontSize fontsize)
{
  if(NeedsRecalculation(fontsize))
  {      
    Cell::Recalculate(fontsize);
    SetFont(m_fontSize_Scaled);
    wxSize sz = CalculateTextSize((*m_configuration)->GetDC(), m_displayedText, cellText);
    m_width = sz.GetWidth();
    m_height = sz.GetHeight();
    m_height += 2 * MC_TEXT_PADDING;
    m_center = m_height / 2;
  }
}

void DigitCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  Configuration *configuration = (*m_configuration);
  if (DrawThisCell(point))
  {
    wxDC *dc = configuration->GetDC();
    SetForeground();
    SetFont(m_fontSize_Scaled);
    dc->DrawText(m_text,
                 point.x,
                 point.y - m_center + MC_TEXT_PADDING);
  }
}
