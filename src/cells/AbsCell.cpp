// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class AbsCell

  AbsCell is the Cell type that represents the field that represents the
  <code>abs()</code> and <code>cabs()</code> commands.
*/

#include "AbsCell.h"
#include "CellImpl.h"
#include "TextCell.h"
#include <utility>
#include <memory>

AbsCell::AbsCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&inner)
  : Cell(group, config), m_innerCell(std::move(inner)) {
  InitBitFields_AbsCell();
  SetStyle(TS_VARIABLE);
  MakeBreakupCells();
}

AbsCell::AbsCell(GroupCell *group, const AbsCell &cell)
  : AbsCell(group, cell.m_configuration,
            CopyList(group, cell.m_innerCell.get())) {
  CopyCommonData(cell);
  MakeBreakupCells();
}

DEFINE_CELL(AbsCell)

void AbsCell::MakeBreakupCells() {
  if (m_open)
    return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, wxS("abs("));
  static_cast<TextCell &>(*m_open).DontEscapeOpeningParenthesis();
  m_open->SetStyle(TS_FUNCTION);
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxS(")"));
}

void AbsCell::Recalculate(AFontSize fontsize) const {
  if (NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);
    m_innerCell->RecalculateList(fontsize);
    m_open->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);

    if (IsBrokenIntoLines()) {
      m_width = 0;
      m_height = 0;
      m_center = 0;
    } else {
      m_width = m_innerCell->SumOfWidths() + Scale_Px(8) +
        2 * m_configuration->GetDefaultLineWidth();
      m_height = m_innerCell->GetHeightList() + Scale_Px(4);
      m_center = m_innerCell->GetCenterList() + Scale_Px(2);
    }
  }
}

/**
 * @brief Pass 2 (Arrange): Positions the inner content within absolute value bars.
 */
void AbsCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
  
  // Linearized cells (broken into lines) behave as zero-size containers.
  // Their children are positioned by the flattened GroupCell loop.
  if (IsBrokenIntoLines())
    return;

  wxPoint in;
  // Offset content to account for the left bar and padding
  in.x = point.x + Scale_Px(4) + m_configuration->GetDefaultLineWidth();
  in.y = point.y;
  m_innerCell->SetCurrentPointList(in);
}

void AbsCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  if (DrawThisCell()) {
    wxPoint point = GetCurrentPoint();
    SetPen(dc);
    m_innerCell->DrawList(dc, antialiassingDC);

    dc->DrawLine(
        point.x + Scale_Px(2) + m_configuration->GetDefaultLineWidth() / 2,
        point.y - m_center + Scale_Px(2),
        point.x + Scale_Px(2) + m_configuration->GetDefaultLineWidth() / 2,
        point.y - m_center + m_height - Scale_Px(2));
    dc->DrawLine(point.x + m_width - Scale_Px(2) - 1 -
                     m_configuration->GetDefaultLineWidth() / 2,
                 point.y - m_center + Scale_Px(2),
                 point.x + m_width - Scale_Px(2) - 1 -
                     m_configuration->GetDefaultLineWidth() / 2,
                 point.y - m_center + m_height - Scale_Px(2));
  }
}

wxString AbsCell::ToString() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  wxString s;
  s = wxS("abs(") + m_innerCell->ListToString() + wxS(")");
  return s;
}

wxString AbsCell::ToMatlab() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  wxString s;
  s = wxS("abs(") + m_innerCell->ListToMatlab() + wxS(")");
  return s;
}

wxString AbsCell::ToTeX() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  return wxS("\\left| ") + m_innerCell->ListToTeX() + wxS("\\right| ");
}

wxString AbsCell::ToMathML() const {
  return wxS("<mrow><mo>|</mo>") + m_innerCell->ListToMathML() +
    wxS("<mo>|</mo></mrow>\n");
  //  return wxS("<apply><abs/><ci>") + m_innerCell->ListToMathML() +
  //  wxS("</ci></apply>");
}

wxString AbsCell::ToOMML() const {
  return wxS("<m:d><m:dPr m:begChr=\"|\" m:endChr=\"|\"></m:dPr><m:e>") +
    m_innerCell->ListToOMML() + wxS("</m:e></m:d>");
}

wxString AbsCell::ToXML() const {
  return wxS("<a") + GetXMLFlags() + wxS(">") + m_innerCell->ListToXML() + wxS("</a>");
}

bool AbsCell::BreakUp() const {
  if (IsBrokenIntoLines())
    return false;

  Cell::BreakUpAndMark();
  m_open->SetNextToDraw(m_innerCell);
  m_innerCell->last()->SetNextToDraw(m_close);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  return true;
}

void AbsCell::SetNextToDraw(Cell *next) const {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
