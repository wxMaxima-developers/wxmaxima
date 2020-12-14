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
  This file defines the class AbsCell

  AbsCell is the Cell type that represents the field that represents the 
  <code>abs()</code> and <code>cabs()</code> commands.
*/

#include "AbsCell.h"
#include "CellImpl.h"
#include "TextCell.h"

AbsCell::AbsCell(GroupCell *parent, Configuration **config, std::unique_ptr<Cell> &&inner) :
    Cell(parent, config),
    m_innerCell(std::move(inner))
{
  InitBitFields();
  SetStyle(TS_VARIABLE);
}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=AbsCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=AbsCell::m_close
AbsCell::AbsCell(const AbsCell &cell) :
  AbsCell(cell.m_group, cell.m_configuration, CopyList(cell.m_innerCell.get()))
{
  CopyCommonData(cell);
}

DEFINE_CELL(AbsCell)

void AbsCell::MakeBreakupCells()
{
  if (m_open) return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, wxT("abs("));
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
  m_open->SetStyle(TS_FUNCTION);
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxT(")"));
}

void AbsCell::Recalculate(AFontSize fontsize)
{
  m_innerCell->RecalculateList(fontsize);
  if (IsBrokenIntoLines())
  {
    m_width = 0;
    m_height = 0;
    m_center = 0;
    m_open->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  }
  else
  {
    m_width = m_innerCell->GetFullWidth() + Scale_Px(8) + 2 * (*m_configuration)->GetDefaultLineWidth();
    m_height = m_innerCell->GetHeightList() + Scale_Px(4);
    m_center = m_innerCell->GetCenterList() + Scale_Px(2);
  }
  Cell::Recalculate(fontsize);
}

void AbsCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {    
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    SetPen();
    wxPoint in;
    in.x = point.x + Scale_Px(4) + (*m_configuration)->GetDefaultLineWidth();
    in.y = point.y;
    m_innerCell->DrawList(in);

    dc->DrawLine(point.x + Scale_Px(2) + (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + Scale_Px(2),
                point.x + Scale_Px(2) + (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + m_height - Scale_Px(2));
    dc->DrawLine(point.x + m_width - Scale_Px(2) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + Scale_Px(2),
                point.x + m_width - Scale_Px(2) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + m_height - Scale_Px(2));
  }
}

wxString AbsCell::ToString() const
{
  if (IsBrokenIntoLines())
    return wxEmptyString;
  wxString s;
  s = wxT("abs(") + m_innerCell->ListToString() + wxT(")");
  return s;
}

wxString AbsCell::ToMatlab() const
{
  if (IsBrokenIntoLines())
	return wxEmptyString;
  wxString s;
  s = wxT("abs(") + m_innerCell->ListToMatlab() + wxT(")");
  return s;
}

wxString AbsCell::ToTeX() const
{
  if (IsBrokenIntoLines())
    return wxEmptyString;
  return wxT("\\left| ") + m_innerCell->ListToTeX() + wxT("\\right| ");
}

wxString AbsCell::ToMathML() const
{
  return wxT("<row><mo>|</mo>") +
         m_innerCell->ListToMathML() +
         wxT("<mo>|</mo></row>\n");
//  return wxT("<apply><abs/><ci>") + m_innerCell->ListToMathML() + wxT("</ci></apply>");
}

wxString AbsCell::ToOMML() const
{
  return wxT("<m:d><m:dPr m:begChr=\"|\" m:endChr=\"|\"></m:dPr><m:e>") +
         m_innerCell->ListToOMML() + wxT("</m:e></m:d>");
}

wxString AbsCell::ToXML() const
{
  wxString flags;
  if (HasHardLineBreak())
    flags += wxT(" breakline=\"true\"");
  
  return wxT("<a") +flags + wxT(">") + m_innerCell->ListToXML() + wxT("</a>");
}

bool AbsCell::BreakUp()
{
  if (IsBrokenIntoLines())
    return false;

  MakeBreakupCells();
  Cell::BreakUpAndMark();
  wxASSERT(!m_open->GetNext());
  wxASSERT(!m_close->GetNext());
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}
