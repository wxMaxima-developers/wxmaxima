// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class NamedBoxCell

  NamedBoxCell is the Cell type that represents the field that represents the
  box() command with two arguments.
*/

#include <utility>
#include <memory>
#include "NamedBoxCell.h"
#include "CellImpl.h"

NamedBoxCell::NamedBoxCell(GroupCell *group, Configuration *config,
                           std::unique_ptr<Cell> &&inner,
                           wxString name)
  : Cell(group, config),
    m_innerCell(std::move(inner)),
    m_boxname(std::make_unique<TextCell>(m_group, m_configuration, name, TS_STRING))
{
  InitBitFields_NamedBoxCell();
  SetStyle(TS_VARIABLE);
}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=NamedBoxCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=NamedBoxCell::m_close
NamedBoxCell::NamedBoxCell(GroupCell *group, const NamedBoxCell &cell)
  : NamedBoxCell(group, cell.m_configuration,
                 CopyList(group, cell.m_innerCell.get()),
                 cell.m_boxname->GetValue()) {
  CopyCommonData(cell);
}

DEFINE_CELL(NamedBoxCell)

void NamedBoxCell::MakeBreakupCells() {
  if (m_open)
    return;
  m_open =
    std::make_unique<TextCell>(m_group, m_configuration, wxS("box("));
  static_cast<TextCell &>(*m_open).DontEscapeOpeningParenthesis();
  m_open->SetStyle(TS_FUNCTION);
  m_comma = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxS(")"));
}

void NamedBoxCell::Recalculate(AFontSize fontsize) {
  if (NeedsRecalculation(fontsize)) {
    m_innerCell->RecalculateList(fontsize);
    m_boxname->RecalculateList(fontsize);

    m_innerCellWidth = m_innerCell->GetFullWidth();
    m_innerCellHeight = m_innerCell->GetHeightList();
    m_nameWidth = m_boxname->GetFullWidth();
    m_nameHeight = m_boxname->GetHeightList();

    if (!IsBrokenIntoLines()) {
      m_width = std::max(m_innerCellWidth, m_nameWidth) + Scale_Px(8);
      m_height = m_innerCellHeight + m_nameHeight + Scale_Px(16);
      m_center = m_innerCell->GetCenterList() + m_nameHeight + Scale_Px(8);
    } else {
      // The NamedBoxCell itself isn't displayed if it is broken into lines.
      // instead m_open, m_innerCell and m_close are => We can set our size to 0
      // in this case.
      m_width = 0;
      m_height = 0;
      m_center = 0;
      m_open->RecalculateList(fontsize);
      m_comma->RecalculateList(fontsize);
      m_close->RecalculateList(fontsize);
    }
    Cell::Recalculate(fontsize);
  }
}

void NamedBoxCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    SetPen(dc);
    wxPoint in;
    in.x = point.x + Scale_Px(4) + ((m_width - Scale_Px(8)) - m_innerCellWidth) / 2;
    in.y = point.y;
    m_innerCell->DrawList(in, dc, antialiassingDC);

    wxPoint namepos(point);
    namepos.x += ((m_width - Scale_Px(8)) - m_nameWidth) / 2 + Scale_Px(4);
    namepos.y -= m_center - m_boxname->GetCenterList();
    m_boxname->DrawList(namepos, dc, antialiassingDC);

    // dc->DrawLine(
    //               point.x + Scale_Px(2),
    //               point.y - m_center + innerCellHeight + Scale_Px(2),
    //              point.x + m_width - Scale_Px(2) - 1,
    //              point.y - m_center + innerCellHeight + Scale_Px(2)
    //               );

    // The top left line of the box
    dc->DrawLine(
                 point.x + Scale_Px(2),
                 point.y - m_center + Scale_Px(2) + m_boxname->GetCenterList(),
                 point.x + (m_width - m_nameWidth) / 2,
                 point.y - m_center + Scale_Px(2) + m_boxname->GetCenterList()
                 );
    // The top right line of the box
    dc->DrawLine(
                 point.x + m_width - Scale_Px(2),
                 point.y - m_center + Scale_Px(2) + m_boxname->GetCenterList(),
                 point.x + m_width - ((m_width - m_nameWidth) / 2),
                 point.y - m_center + Scale_Px(2) + m_boxname->GetCenterList()
                 );
    // The left line of the box
    dc->DrawLine(
                 point.x + Scale_Px(2),
                 point.y - m_center + m_height - Scale_Px(2),
                 point.x + Scale_Px(2),
                 point.y - m_center + Scale_Px(2) + m_nameHeight / 2
                 );
    // The right line of the box
    dc->DrawLine(
                 point.x + m_width - Scale_Px(2),
                 point.y - m_center + Scale_Px(2) + m_nameHeight / 2,
                 point.x + m_width - Scale_Px(2),
                 point.y - m_center + m_height - Scale_Px(2)
                 );
    // The bottom line of the box
    dc->DrawLine(
                 point.x + Scale_Px(2),
                 point.y - m_center + m_height - Scale_Px(2),
                 point.x + m_width - Scale_Px(2),
                 point.y - m_center + m_height - Scale_Px(2)
                 );
  }
}

wxString NamedBoxCell::ToString() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxS("box(") + m_innerCell->ListToString() +
      wxS(",") + m_boxname->ListToString() + wxS(")");
}

wxString NamedBoxCell::ToMatlab() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxS("box(") + m_innerCell->ListToMatlab() +
      wxS(",") + m_boxname->ListToMatlab() + wxS(")");
}

wxString NamedBoxCell::ToTeX() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  else
    return wxS("\\fbox{") + m_innerCell->ListToTeX() + wxS("}");
}

wxString NamedBoxCell::ToMathML() const {
  return wxS("<apply><box/><ci>") + m_innerCell->ListToMathML() +
    wxS("</ci><ci>") +
    m_boxname->ListToMathML() +
    wxS("</apply>");
}

wxString NamedBoxCell::ToOMML() const {
  return _T("<m:func><m:fName><m:r>box</m:r></m:fName><m:e>") +
    m_innerCell->ListToOMML() + wxS("</m:e><m:e>") +
    m_boxname->ListToOMML() +
    _T("</m:e></m:func>");
}

wxString NamedBoxCell::ToXML() const {
  wxString flags = " boxname =\"" + XMLescape(m_boxname->GetValue()) + "\"";
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  return wxS("<hl") + flags + wxS(">") + m_innerCell->ListToXML() +
    wxS("</hl>");
}

bool NamedBoxCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  MakeBreakupCells();
  Cell::BreakUpAndMark();
  m_open->SetNextToDraw(m_innerCell);
  m_innerCell->last()->SetNextToDraw(m_comma);
  m_comma->last()->SetNextToDraw(m_boxname);
  m_boxname->last()->SetNextToDraw(m_close);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  return true;
}

void NamedBoxCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
