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
  This file defines the class SetCell

  SetCell is the Cell type that represents a list of math elements.
*/

#include "SetCell.h"
#include "CellImpl.h"

SetCell::SetCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&inner)
  : ListCell(group, config, std::move(inner)) {
  m_open = std::make_unique<TextCell>(group, config, wxS("{"));
  m_close = std::make_unique<TextCell>(group, config, wxS("}"));
}

SetCell::SetCell(GroupCell *group, const SetCell &cell)
  : SetCell(group, cell.m_configuration,
	    CopyList(group, cell.m_innerCell.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(SetCell)

void SetCell::Draw(wxPoint point) {
  Cell::Draw(point);
  if (DrawThisCell(point)) {
    wxPoint innerCellPos(point);

    if (m_drawAsAscii) {
      innerCellPos.x += m_open->GetWidth();
      m_open->DrawList(point);
      m_close->DrawList(wxPoint(
				point.x + m_open->GetWidth() + m_innerCell->GetFullWidth(), point.y));
    } else {
      wxDC *adc = m_configuration->GetAntialiassingDC();
      innerCellPos.y +=
	(m_innerCell->GetCenterList() - m_innerCell->GetHeightList() / 2);
      SetPen(1.5);

      int signWidth = m_signWidth - Scale_Px(2);
      innerCellPos.x = point.x + m_signWidth;

      // Left curly brace
      const wxPoint pointsL[8] = {
	{point.x + signWidth, point.y - m_center + Scale_Px(4)},
	{point.x + signWidth / 2,
	 point.y - m_center + Scale_Px(4) + signWidth / 2},
	{point.x + signWidth / 2, point.y - signWidth / 2},
	{point.x, point.y},
	{point.x, point.y},
	{point.x + signWidth / 2, point.y + signWidth / 2},
	{point.x + signWidth / 2,
	 point.y + m_center - Scale_Px(4) - signWidth / 2},
	{point.x + signWidth, point.y + m_center - Scale_Px(4)}};
      adc->DrawSpline(8, pointsL);

      // Right curly brace
      const wxPoint pointsR[8] = {
	{point.x + m_width - signWidth, point.y - m_center + Scale_Px(4)},
	{point.x - signWidth / 2 + m_width,
	 point.y - m_center + Scale_Px(4) + signWidth / 2},
	{point.x - signWidth / 2 + m_width, point.y - signWidth / 2},
	{point.x + m_width, point.y},
	{point.x + m_width, point.y},
	{point.x - signWidth / 2 + m_width, point.y + signWidth / 2},
	{point.x - signWidth / 2 + m_width,
	 point.y + m_center - Scale_Px(4) - signWidth / 2},
	{point.x + m_width - signWidth, point.y + m_center - Scale_Px(4)}};
      adc->DrawSpline(8, pointsR);
    }

    if (!IsBrokenIntoLines())
      m_innerCell->DrawList(innerCellPos);
  }
}

wxString SetCell::ToString() const {
  wxString s;
  if (!m_innerCell)
    return "[]";

  if (!IsBrokenIntoLines())
    s = wxS("{") + m_innerCell->ListToString() + wxS("}");
  return s;
}

wxString SetCell::ToMatlab() const {
  wxString s;
  if (!IsBrokenIntoLines())
    s = wxS("{") + m_innerCell->ListToMatlab() + wxS("}");
  return s;
}

wxString SetCell::ToTeX() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    wxString innerCell = m_innerCell->ListToTeX();

    // Let's see if the cell contains anything potentially higher than a normal
    // character.
    bool needsLeftRight = false;
    for (size_t i = 0; i < innerCell.Length(); i++)
      if (!wxIsalnum(innerCell[i])) {
        needsLeftRight = true;
        break;
      }

    if (needsLeftRight)
      s = wxS("\\left{ ") + m_innerCell->ListToTeX() + wxS("\\right} ");
    else
      s = wxS("{") + m_innerCell->ListToTeX() + wxS("}");
  }
  return s;
}

wxString SetCell::ToXML() const {
  wxString s = m_innerCell->ListToXML();
  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");
  return (wxS("<r set=\"true\"") + flags + wxS("><t listdelim=\"true\">{</t>") +
          s + wxS("<t listdelim=\"true\">}</t></r>"));
}
