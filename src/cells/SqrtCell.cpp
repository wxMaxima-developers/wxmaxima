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
  This file defines the class SqrtCell

  SqrtCell is the Cell type that represents a square root.
*/

#include "SqrtCell.h"
#include "CellImpl.h"

#define SIGN_FONT_SCALE 2.0

SqrtCell::SqrtCell(GroupCell *group, Configuration *config,
                   std::unique_ptr<Cell> &&inner)
  : Cell(group, config), m_innerCell(std::move(inner)) {
  InitBitFields();
  SetStyle(TS_VARIABLE);
}

// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_comma
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signSize
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signTop
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signType
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signFontScale
SqrtCell::SqrtCell(GroupCell *group, const SqrtCell &cell)
  : SqrtCell(group, cell.m_configuration,
	     CopyList(group, cell.m_innerCell.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(SqrtCell)

void SqrtCell::MakeBreakUpCells() {
  if (m_open)
    return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, "sqrt(");
  m_open->SetStyle(TS_FUNCTION);
  static_cast<TextCell &>(*m_open).DontEscapeOpeningParenthesis();
  m_close = std::make_unique<TextCell>(m_group, m_configuration, ")");
}

void SqrtCell::Recalculate(AFontSize fontsize) {
  m_innerCell->RecalculateList(fontsize);

  m_width = m_innerCell->GetFullWidth() + Scale_Px(13) + 1;
  if (!IsBrokenIntoLines()) {
    auto openHeight = 0; // m_open->GetHeightList();
    auto openCenter = 0; // m_open->GetCenterList();
    m_height = wxMax(m_innerCell->GetHeightList(), openHeight) + Scale_Px(3);
    m_center = wxMax(m_innerCell->GetCenterList(), openCenter) + Scale_Px(3);
  } else {
    m_height = m_center = m_width = 0;
    m_open->Recalculate(fontsize);
    m_close->Recalculate(fontsize);
  }
  Cell::Recalculate(fontsize);
}

void SqrtCell::Draw(wxPoint point) {
  Cell::Draw(point);
  if (DrawThisCell(point)) {
    wxDC *dc = m_configuration->GetDC();

    wxPoint in(point);


    {
      wxDC *adc = m_configuration->GetAntialiassingDC();
      in.x += Scale_Px(11) + 1;
      SetPen(1.2);

      const wxPoint points[12] = {
	{0, 0},
	{Scale_Px(3), -Scale_Px(1)},
	//  A wider line
	{Scale_Px(3), -Scale_Px(1)},
	{Scale_Px(7), m_height - m_center - Scale_Px(4)},
	{Scale_Px(7.5), m_height - m_center - Scale_Px(4)},
	{Scale_Px(3.5), -Scale_Px(1)},
	{Scale_Px(3.5), -Scale_Px(1)},
	{Scale_Px(3), -Scale_Px(1)},
	{Scale_Px(8), m_height - m_center - Scale_Px(4)},
	// The upwards line
	{Scale_Px(10), -m_center + Scale_Px(2)},
	// The horizontal line
	{m_width - Scale_Px(1), -m_center + Scale_Px(2)},
	// The serif at the end of the root
	{m_width - Scale_Px(1), -m_center + Scale_Px(6)}};
      adc->DrawLines(12, points, point.x, point.y);
    }

    m_innerCell->DrawList(in);
  }
}

wxString SqrtCell::ToString() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxT("sqrt(") + m_innerCell->ListToString() + wxT(")");
}

wxString SqrtCell::ToMatlab() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxT("sqrt(") + m_innerCell->ListToMatlab() + wxT(")");
}

wxString SqrtCell::ToTeX() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxT("\\sqrt{") + m_innerCell->ListToTeX() + wxT("}");
}

wxString SqrtCell::ToMathML() const {
  return wxT("<msqrt>") + m_innerCell->ListToMathML() + wxT("</msqrt>\n");
}

wxString SqrtCell::ToOMML() const {
  return wxT("<m:rad><m:radPr m:degHide=\"1\"></m:radPr><m:deg></m:deg><m:e>") +
    m_innerCell->ListToOMML() + wxT("</m:e></m:rad>\n");
}

wxString SqrtCell::ToXML() const {
  //  if (IsBrokenIntoLines())
  //    return wxEmptyString;
  wxString flags;
  if (HasHardLineBreak())
    flags += wxT(" breakline=\"true\"");

  return wxT("<q") + flags + wxT(">") + m_innerCell->ListToXML() + wxT("</q>");
}

bool SqrtCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  MakeBreakUpCells();
  Cell::BreakUpAndMark();
  m_open->SetNextToDraw(m_innerCell);
  m_innerCell->last()->SetNextToDraw(m_close);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;

  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void SqrtCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
