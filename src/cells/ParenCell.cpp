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
  This file defines the class ParenCell

  ParenCell is the Cell type that represents a math element that is kept
  between parenthesis.
*/

#include "ParenCell.h"
#include "CellImpl.h"
#include "VisiblyInvalidCell.h"

ParenCell::ParenCell(GroupCell *group, Configuration *config,
                     std::unique_ptr<Cell> &&inner)
  : Cell(group, config),
    m_open(std::make_unique<TextCell>(group, config, wxS("("))),
    m_innerCell(std::move(inner)),
    m_close(std::make_unique<TextCell>(group, config, wxS(")"))) {
  InitBitFields_ParenCell();
  // If there is a contents it doesn't need to start with a multiplication
  // dot.
  if(m_innerCell)
    m_innerCell->SetSuppressMultiplicationDot(true);
  m_open->SetStyle(TS_FUNCTION);
  m_close->SetStyle(TS_FUNCTION);
}

// These false-positive warnings only appear in old versions of cppcheck
// that don't fully understand constructor delegation, still.
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_last1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_print
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_numberOfExtensions
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charWidth1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charHeight1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signTopHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signBotHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_extendHeight
ParenCell::ParenCell(GroupCell *group, const ParenCell &cell)
  : ParenCell(group, cell.m_configuration,
              CopyList(group, cell.m_innerCell.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(ParenCell)

void ParenCell::SetInner(std::unique_ptr<Cell> inner, CellType type) {
  if (!inner)
    return;
  m_innerCell = std::move(inner);

  m_type = type;
  // Tell the first of our inner cells not to begin with a multiplication dot.
  m_innerCell->SetSuppressMultiplicationDot(true);
  ResetSize();
}

void ParenCell::Recalculate(AFontSize fontsize) {
  if(m_innerCell)
    m_innerCell->RecalculateList(fontsize);
  m_open->RecalculateList(fontsize);
  m_close->RecalculateList(fontsize);

  wxDC *dc = m_configuration->GetRecalcDC();
  auto fontsize1 = Scale_Px(fontsize);
  int size = 0;
  if(m_innerCell)
    size = m_innerCell->GetHeightList();
  // If our font provides all the unicode chars we need we don't need
  // to bother which exotic method we need to use for drawing nice parenthesis.
  if (fontsize1 * 3 > size) {
    m_bigParenType = Configuration::ascii;
    m_signHeight = m_open->GetHeightList();
    m_signWidth = m_open->GetWidth();
  } else {
    m_bigParenType = Configuration::handdrawn;
    m_signWidth = Scale_Px(6) + m_configuration->GetDefaultLineWidth();
    if (m_signWidth < size / 15)
      m_signWidth = size / 15;
  }
  int innerCellWidth = 0;
  if(m_innerCell)
    innerCellWidth = m_innerCell->GetFullWidth();
  m_width = innerCellWidth + m_signWidth * 2;
  if (IsBrokenIntoLines())
    m_width = 0;

  int innerCellHeight = 0;
  if(m_innerCell)
    innerCellHeight = m_innerCell->GetHeightList();
  m_height = std::max(m_signHeight, innerCellHeight) + Scale_Px(2);
  m_center = m_height / 2;

  dc->GetTextExtent(wxS("("), &m_charWidth1, &m_charHeight1);
  if (m_charHeight1 < 2)
    m_charHeight1 = 2;

  if (IsBrokenIntoLines()) {
    int innerCellCenter = 0;
    if(m_innerCell)
      innerCellCenter = m_innerCell->GetCenterList();
    m_height = std::max(innerCellHeight, m_open->GetHeightList());
    m_center = std::max(innerCellCenter, m_open->GetCenterList());
  } else {
    if (m_innerCell) {
      switch (m_bigParenType) {
      case Configuration::ascii:
        m_signHeight = m_charHeight1;
        break;
      default: {
      }
      }
      m_innerCell->SetCurrentPoint(
                                   wxPoint(m_currentPoint.x + m_signWidth, m_currentPoint.y));

      // Center the argument of all big parenthesis vertically
      if (m_bigParenType != Configuration::ascii)
        m_innerCell->SetCurrentPoint(
                                     wxPoint(m_currentPoint.x + m_signWidth,
                                             m_currentPoint.y + (m_innerCell->GetCenterList() -
                                                                 m_innerCell->GetHeightList() / 2)));
      else
        m_innerCell->SetCurrentPoint(
                                     wxPoint(m_currentPoint.x + m_signWidth, m_currentPoint.y));

      m_height =
        std::max(m_signHeight, m_innerCell->GetHeightList()) + Scale_Px(4);
      m_center = m_height / 2;
    }
  }
  Cell::Recalculate(fontsize);
}

void ParenCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    wxPoint innerCellPos(point);

    switch (m_bigParenType) {
    case Configuration::ascii:
      {
        innerCellPos.x += m_open->GetWidth();
        m_open->DrawList(point, dc, antialiassingDC);
        int innerCellWidth = 0;
        if(m_innerCell)
          innerCellWidth = m_innerCell->GetFullWidth();

        m_close->DrawList(wxPoint(point.x + m_open->GetWidth() + innerCellWidth,
                                  point.y), dc, antialiassingDC);
        break;
      }
    default: {
      if(m_innerCell)
        {
          innerCellPos.y +=
            (m_innerCell->GetCenterList() - m_innerCell->GetHeightList() / 2);
        }
      SetPen(antialiassingDC, 1.0);
      SetBrush(antialiassingDC);

      int signWidth = m_signWidth - Scale_Px(2);
      innerCellPos.x = point.x + m_signWidth;

      // Left bracket
      const wxPoint pointsL[10] = {
        {point.x + Scale_Px(1) + signWidth, point.y - m_center + Scale_Px(4)},
        {point.x + Scale_Px(1) + 3 * signWidth / 4,
         point.y - m_center + 3 * signWidth / 4 + Scale_Px(4)},
        {point.x + Scale_Px(1), point.y},
        {point.x + Scale_Px(1) + 3 * signWidth / 4,
         point.y + m_center - 3 * signWidth / 4 - Scale_Px(4)},
        {point.x + Scale_Px(1) + signWidth, point.y + m_center - Scale_Px(4)},
        // Appending the last point twice should allow for an abrupt 180° turn
        {point.x + Scale_Px(1) + signWidth, point.y + m_center - Scale_Px(4)},
        {point.x + Scale_Px(1) + 3 * signWidth / 4,
         point.y + m_center - 3 * signWidth / 4 - Scale_Px(4)},
        // The middle point of the 2nd run of the parenthesis is at a
        // different place making the parenthesis wider here
        {point.x + Scale_Px(2), point.y},
        {point.x + Scale_Px(1) + 3 * signWidth / 4,
         point.y - m_center + 3 * signWidth / 4 + Scale_Px(4)},
        {point.x + Scale_Px(1) + signWidth,
         point.y - m_center + Scale_Px(4)}};
      antialiassingDC->DrawSpline(10, pointsL);

      // Right bracket
      const wxPoint pointsR[10] = {
        {point.x + m_width - Scale_Px(1) - signWidth,
         point.y - m_center + Scale_Px(4)},
        {point.x + m_width - Scale_Px(1) - signWidth / 2,
         point.y - m_center + signWidth / 2 + Scale_Px(4)},
        {point.x + m_width - Scale_Px(1), point.y},
        {point.x + m_width - Scale_Px(1) - signWidth / 2,
         point.y + m_center - signWidth / 2 - Scale_Px(4)},
        {point.x + m_width - Scale_Px(1) - signWidth,
         point.y + m_center - Scale_Px(4)},
        {point.x + m_width - Scale_Px(1) - signWidth,
         point.y + m_center - Scale_Px(4)},
        {point.x + m_width - Scale_Px(1) - signWidth / 2,
         point.y + m_center - signWidth / 2 - Scale_Px(4)},
        {point.x + m_width - Scale_Px(2), point.y},
        {point.x + m_width - Scale_Px(1) - signWidth / 2,
         point.y - m_center + signWidth / 2 + Scale_Px(4)},
        {point.x + m_width - Scale_Px(1) - signWidth,
         point.y - m_center + Scale_Px(4)}};
      antialiassingDC->DrawSpline(10, pointsR);
    } break;
    }

    if (!IsBrokenIntoLines())
      {
        if(m_innerCell)
          m_innerCell->DrawList(innerCellPos, dc, antialiassingDC);
      }
  }
}

wxString ParenCell::ToString() const {
  wxString s;
  if (!m_innerCell)
    return wxS("()");

  if (!IsBrokenIntoLines()) {
    if (m_print)
      s = wxS("(") + m_innerCell->ListToString() + wxS(")");
    else
      s = m_innerCell->ListToString();
  }
  return s;
}

wxString ParenCell::ToMatlab() const {
  if (!m_innerCell)
    return "()";
  wxString s;
  if (!IsBrokenIntoLines()) {
    if (m_print)
      s = wxS("(") + m_innerCell->ListToMatlab() + wxS(")");
    else
      s = m_innerCell->ListToMatlab();
  }
  return s;
}

wxString ParenCell::ToTeX() const {
  if (!m_innerCell)
    return "()";
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

    if (m_print) {
      if (needsLeftRight)
        s = wxS("\\left( ") + m_innerCell->ListToTeX() + wxS("\\right) ");
      else
        s = wxS("(") + m_innerCell->ListToTeX() + wxS(")");
    } else
      s = m_innerCell->ListToTeX();
  }
  return s;
}

wxString ParenCell::ToOMML() const {
  if(m_innerCell)
    return wxS("<m:d><m:dPr m:begChr=\"") + XMLescape(m_open->ToString()) +
      wxS("\" m:endChr=\"") + XMLescape(m_close->ToString()) +
      wxS("\" m:grow=\"1\"></m:dPr><m:e>") + m_innerCell->ListToOMML() +
      wxS("</m:e></m:d>");
  else
    return wxS("<m:d><m:dPr m:begChr=\"") + XMLescape(m_open->ToString()) +
      wxS("\" m:endChr=\"") + XMLescape(m_close->ToString()) +
      wxS("\" m:grow=\"1\"></m:dPr><m:e>") +
      wxS("</m:e></m:d>");
}

wxString ParenCell::ToMathML() const {
  if (!m_print)
    return m_innerCell->ListToMathML();

  wxString open = m_open->ToString();
  wxString close = m_close->ToString();
  wxString innerCellContents;
  if(m_innerCell)
    innerCellContents = m_innerCell->ListToMathML();

  return (wxS("<mrow><mo>") + XMLescape(open) + wxS("</mo>") +
          innerCellContents + wxS("<mo>") + XMLescape(close) +
          wxS("</mo></mrow>\n"));
}

wxString ParenCell::ToXML() const {
  //  if(IsBrokenIntoLines())
  //    return wxEmptyString;
  wxString s;
  if(m_innerCell)
    s = m_innerCell->ListToXML();
  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");
  return ((m_print) ? wxS("<r><p") + flags + wxS(">") + s + wxS("</p></r>") : s);
}

bool ParenCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  Cell::BreakUpAndMark();
  if(m_innerCell)
    {
      m_open->SetNextToDraw(m_innerCell);
      m_innerCell->last()->SetNextToDraw(m_close);
    }
  else
    m_open->SetNextToDraw(m_close);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;

  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void ParenCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
