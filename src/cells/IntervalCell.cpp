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
  This file defines the class IntervalCell

  IntervalCell is the Cell type that represents a list of math elements.
*/

#include "IntervalCell.h"
#include "CellImpl.h"

IntervalCell::IntervalCell(GroupCell *group, Configuration *config,
                           std::unique_ptr<Cell> &&start,
                           std::unique_ptr<Cell> &&end)
  : Cell(group, config),
    m_leftBracketOpensLeft(start->ListToString() == wxS("-inf")),
    m_rightBracketOpensRight(end->ListToString() == wxS("inf")),
    m_open(std::make_unique<TextCell>(group, config, wxS("interval("))),
    m_openBracket(std::make_unique<TextCell>(
                                             group, config, m_leftBracketOpensLeft ? wxS("]") : wxS("["))),
    m_start(std::move(start)),
    m_comma(std::make_unique<TextCell>(group, config, wxS(","))),
    m_ellipsis(std::make_unique<TextCell>(group, config, wxS("\u2026"))),
    m_stop(std::move(end)),
    m_closeBracket(std::make_unique<TextCell>(
                                              group, config, m_rightBracketOpensRight ? wxS("[") : wxS("]"))),
    m_close(std::make_unique<TextCell>(group, config, wxS(")"))) {
  InitBitFields_IntervalCell();
  SetStyle(TS_VARIABLE);

  // Tell the first of our inner cells not to begin with a multiplication dot.
  m_start->SetSuppressMultiplicationDot(true);
  m_ellipsis->SetAltCopyText(wxS(","));
  m_open->SetStyle(TS_FUNCTION);
  m_close->SetStyle(TS_FUNCTION);
}

// These false-positive warnings only appear in old versions of cppcheck
// that don't fully understand constructor delegation, still.
// cppcheck-suppress uninitMemberVar symbolName=IntervalCell::m_last1
// cppcheck-suppress uninitMemberVar symbolName=IntervalCell::m_print
// cppcheck-suppress uninitMemberVar
// symbolName=IntervalCell::m_numberOfExtensions cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_charWidth cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_charHeight cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_charWidth1 cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_charHeight1 cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_signWidth cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_signHeight cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_signTopHeight cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_signBotHeight cppcheck-suppress
// uninitMemberVar symbolName=IntervalCell::m_extendHeight
IntervalCell::IntervalCell(GroupCell *group, const IntervalCell &cell)
  : IntervalCell(group, cell.m_configuration,
                 CopyList(group, cell.m_start.get()),
                 CopyList(group, cell.m_stop.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(IntervalCell)

void IntervalCell::Recalculate(AFontSize fontsize) {
  if (NeedsRecalculation(fontsize)) {
    if (IsBrokenIntoLines()) {
      m_comma->RecalculateList(fontsize);
      m_open->RecalculateList(fontsize);
    } else {
      m_openBracket->RecalculateList(fontsize);
      m_closeBracket->RecalculateList(fontsize);
      m_ellipsis->RecalculateList(fontsize);
    }
    m_close->RecalculateList(fontsize);
    m_start->RecalculateList(fontsize);
    m_stop->RecalculateList(fontsize);
    m_signWidth = m_close->GetWidth();

    // If our font provides all the unicode chars we need we don't need
    // to bother which exotic method we need to use for drawing nice parenthesis.
    if (1.2 * m_open->GetHeight() >=
        std::max(m_start->GetHeightList(), m_stop->GetHeightList())) {
      m_drawAsAscii = true;
      m_signHeight = m_open->GetHeight();
    } else {
      m_drawAsAscii = false;
      m_signHeight =
        std::max(std::max(std::max(m_start->GetHeightList(), m_stop->GetHeightList()),
                          m_open->GetHeight()),
                 m_ellipsis->GetHeight());
    }

    if (IsBrokenIntoLines()) {
      m_width = 0;
      m_height = 0;
      m_center = 0;
    } else {
      m_width = m_signWidth + m_start->GetFullWidth() + m_ellipsis->GetWidth() +
        m_stop->GetFullWidth() + m_signWidth;

      m_height = std::max(std::max(m_signHeight, m_start->GetHeightList()),
                          m_stop->GetHeightList()) +
        Scale_Px(4);
      m_center = m_height / 2;
    }
    Cell::Recalculate(fontsize);
  }
}

void IntervalCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    wxPoint innerCellPos(point);

    if (m_drawAsAscii) {
      m_openBracket->DrawList(point, dc, antialiassingDC);
      point.x += m_openBracket->GetFullWidth();
      m_start->DrawList(point, dc, antialiassingDC);
      point.x += m_start->GetFullWidth();
      m_ellipsis->DrawList(point, dc, antialiassingDC);
      point.x += m_ellipsis->GetFullWidth();
      m_stop->DrawList(point, dc, antialiassingDC);
      point.x += m_stop->GetFullWidth();
      m_closeBracket->DrawList(point, dc, antialiassingDC);
    } else {
      SetPen(dc, 1.5);

      innerCellPos.x = point.x + m_signWidth;

      // Left bracket
      if (m_leftBracketOpensLeft)
        DrawBigRightOpenBracket(antialiassingDC, point);
      else
        DrawBigLeftOpenBracket(antialiassingDC, point);

      wxPoint rightBracketPos(point);
      rightBracketPos.x += m_width - m_signWidth;
      // Right bracket
      if (m_rightBracketOpensRight)
        DrawBigLeftOpenBracket(antialiassingDC, rightBracketPos);
      else
        DrawBigRightOpenBracket(antialiassingDC, rightBracketPos);

      wxPoint startCellPos(innerCellPos);
      startCellPos.y +=
        (m_start->GetCenterList() - m_start->GetHeightList() / 2);
      m_start->DrawList(startCellPos, dc, antialiassingDC);
      innerCellPos.x += m_start->GetFullWidth();
      m_ellipsis->DrawList(innerCellPos, dc, antialiassingDC);
      innerCellPos.x += m_ellipsis->GetWidth();
      wxPoint stopCellPos(innerCellPos);
      stopCellPos.y += (m_stop->GetCenterList() - m_stop->GetHeightList() / 2);
      m_stop->DrawList(stopCellPos, dc, antialiassingDC);
    }
  }
}

void IntervalCell::DrawBigLeftOpenBracket(wxDC *dc, wxPoint point) const {
  const wxPoint pointsL[4] = {
    {point.x - Scale_Px(1) + m_signWidth, point.y - m_center + Scale_Px(4)},
    {point.x + Scale_Px(1), point.y - m_center + Scale_Px(4)},
    {point.x + Scale_Px(1), point.y + m_center - Scale_Px(4)},
    {point.x - Scale_Px(1) + m_signWidth, point.y + m_center - Scale_Px(4)}};
  dc->DrawLines(4, pointsL);
}
void IntervalCell::DrawBigRightOpenBracket(wxDC *dc, wxPoint point) const {
  const wxPoint pointsR[4] = {
    {point.x + Scale_Px(1), point.y - m_center + Scale_Px(4)},
    {point.x + Scale_Px(1) + m_signWidth, point.y - m_center + Scale_Px(4)},
    {point.x + Scale_Px(1) + m_signWidth, point.y + m_center - Scale_Px(4)},
    {point.x + Scale_Px(1), point.y + m_center - Scale_Px(4)}};
  dc->DrawLines(4, pointsR);
}

wxString IntervalCell::ToString() const {
  wxString s;

  if (!IsBrokenIntoLines())
    s = wxS("interval(") + m_start->ListToString() + wxS(",") +
      m_stop->ListToString() + wxS(")");
  return s;
}

wxString IntervalCell::ToMatlab() const {
  wxString s;
  if (!IsBrokenIntoLines())
    s = wxS("interval(") + m_start->ListToMatlab() + wxS(",") +
      m_stop->ListToMatlab() + wxS(")");
  return s;
}

wxString IntervalCell::ToTeX() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    s = wxS("\\left[ ") + m_start->ListToTeX() + "\\ldots " +
      m_stop->ListToTeX() + wxS("\\right] ");
  }
  return s;
}

wxString IntervalCell::ToOMML() const {
  wxString open = m_openBracket->ToString();
  wxString close = m_closeBracket->ToString();
  wxString retval = wxString(wxS("<m:d>")) + wxS("<m:dPr m:begChr=\"") + open +
    wxS("\" m:endChr=\"") + close +
    wxS("]\" m:grow=\"1\"></m:dPr>") + wxS("<m:e>") +
    m_start->ListToOMML() + wxS("\u2026") +
    m_stop->ListToOMML() + wxS("</m:e>") + wxS("</m:d>");
  return retval;
}

wxString IntervalCell::ToMathML() const {
  wxString open = m_openBracket->ToString();
  wxString close = m_closeBracket->ToString();
  wxString retval = wxString(wxS("<mrow>")) + wxS("<mo>") + open +
    wxS("</mo>") + m_start->ListToMathML() + wxS(",") +
    m_stop->ListToMathML() + wxS("<mo>") + close +
    wxS("</mo>") + wxS("</mrow>\n");

  return (retval);
}

wxString IntervalCell::ToXML() const {
  wxString flags = wxS(" interval=\"true\"");
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");
  if (m_leftBracketOpensLeft)
    flags += wxS(" leftBracketOpensLeft=\"true\"");
  else
    flags += wxS(" leftBracketOpensLeft=\"false\"");
  if (m_rightBracketOpensRight)
    flags += wxS(" rightBracketOpensRight=\"true\"");
  else
    flags += wxS(" rightBracketOpensRight=\"false\"");

  return wxS("<fn") + flags + wxS("><fnm>interval</fnm>") + wxS("<r><p><r>") +
    m_start->ListToXML() + wxS("</r><r>,</r><r>") +
    m_stop->ListToXML() + wxS("</r></p></r>") + wxS("</fn>");
}

bool IntervalCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  Cell::BreakUpAndMark();
  m_stop->last()->SetNextToDraw(m_close);
  m_comma->SetNextToDraw(m_stop);
  m_start->last()->SetNextToDraw(m_comma);
  m_open->SetNextToDraw(m_start);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  return true;
}

void IntervalCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
