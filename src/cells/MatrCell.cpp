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
  This file defines the class MatrCell

  MatrCell is the Cell type that represents matrices and matrix-like
  elements like the table_form command.
*/

#include "MatrCell.h"
#include "CellImpl.h"

MatrCell::MatrCell(GroupCell *group, Configuration *config)
  : Cell(group, config) {
  InitBitFields_MatrCell();
}

MatrCell::MatrCell(GroupCell *group, const MatrCell &cell)
  : MatrCell(group, cell.m_configuration) {
  CopyCommonData(cell);
  m_specialMatrix = cell.m_specialMatrix;
  m_inferenceMatrix = cell.m_inferenceMatrix;
  m_parenType = cell.m_parenType;
  m_rowNames = cell.m_rowNames;
  m_colNames = cell.m_colNames;
  m_matWidth = cell.m_matWidth;
  m_matHeight = cell.m_matHeight;
  for (size_t i = 0; i < cell.m_matWidth * cell.m_matHeight; i++)
    if (i < cell.m_cells.size())
      m_cells.emplace_back(cell.m_cells[i]->CopyList(group));

  for (size_t i = 0; i < m_matHeight; i++)
    m_dropCenters.emplace_back(-1, -1);

  for (size_t i = 0; i < m_matWidth; i++)
    m_widths.emplace_back(-1);
}

DEFINE_CELL(MatrCell)

void MatrCell::Recalculate(AFontSize const fontsize) {
  AFontSize const fontsize_entry{MC_MIN_SIZE, fontsize - 2};
  for (size_t i = 0; i < m_cells.size(); i++)
    m_cells[i]->RecalculateList(fontsize_entry);

  m_width = 0;
  m_widths.clear();
  for (size_t i = 0; i < m_matWidth; i++) {
    wxCoord width = 0;
    for (size_t j = 0; j < m_matHeight; j++) {
      if ((m_matWidth * j + i) < m_cells.size())
        width = std::max(width, GetInnerCell(j, i)->GetFullWidth());
    }
    m_widths.emplace_back(width);
    m_width += (width + Scale_Px(10));
  }
  if (m_width < Scale_Px(14))
    m_width = Scale_Px(14);

  m_height = 0;
  m_dropCenters.clear();
  for (size_t i = 0; i < m_matHeight; i++) {
    wxCoord center = 0, drop = 0;
    for (size_t j = 0; j < m_matWidth; j++)
      if (m_matWidth * i + j < m_cells.size()) {
        center = std::max(center, GetInnerCell(i, j)->GetCenterList());
        drop = std::max(drop, GetInnerCell(i, j)->GetMaxDrop());
      }
    m_dropCenters.emplace_back(drop, center);
    m_height += (center + drop + Scale_Px(10));
  }
  if (m_height == 0)
    m_height = fontsize + Scale_Px(10);
  m_center = m_height / 2;

  Cell::Recalculate(fontsize);
}

void MatrCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  SetBrush(dc);
  if (DrawThisCell(point)) {
    wxPoint mp;
    mp.x = point.x + Scale_Px(5);
    mp.y = point.y - m_center;
    for (size_t i = 0; i < m_matWidth; i++) {
      mp.y = point.y - m_center + Scale_Px(5);
      for (size_t j = 0; j < m_matHeight; j++) {
        if ((j * m_matWidth + i) < m_cells.size()) {
          mp.y += m_dropCenters[j].center;
          wxPoint mp1(mp);
          mp1.x =
            mp.x +
            (m_widths[i] - GetInnerCell(j, i)->GetFullWidth()) / 2;
          GetInnerCell(j, i)->DrawList(mp1, dc, antialiassingDC);
          mp.y += (m_dropCenters[j].drop + Scale_Px(10));
        }
      }
      mp.x += (m_widths[i] + Scale_Px(10));
    }
    SetPen(antialiassingDC, 1.5);
    if (m_specialMatrix) {
      if (m_inferenceMatrix)
        antialiassingDC->DrawLine(point.x + Scale_Px(1), point.y - m_center + Scale_Px(2),
                                  point.x + Scale_Px(1), point.y + m_center - Scale_Px(2));
      else {
        if (m_rowNames)
          antialiassingDC->DrawLine(point.x + m_widths[0] + 2 * Scale_Px(5),
                                    point.y - m_center + Scale_Px(2),
                                    point.x + m_widths[0] + 2 * Scale_Px(5),
                                    point.y + m_center - Scale_Px(2));
        if (m_colNames)
          antialiassingDC->DrawLine(
                                    point.x + Scale_Px(1),
                                    point.y - m_center + m_dropCenters[0].Sum() + 2 * Scale_Px(5),
                                    point.x + Scale_Px(1) + m_width,
                                    point.y - m_center + m_dropCenters[0].Sum() + 2 * Scale_Px(5));
      }
    } else {
      switch (m_parenType) {
      case paren_rounded: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (m_height <= signWidth / 3)
          signWidth = m_height / 3;

        // Left bracket
        wxPoint pointsL[5] = {
          {point.x + Scale_Px(1) + signWidth, point.y - m_center},
          {point.x + Scale_Px(1) + signWidth / 2,
           point.y - m_center + signWidth / 2},
          {point.x + Scale_Px(1), point.y},
          {point.x + Scale_Px(1) + signWidth / 2,
           point.y + m_center - signWidth / 2},
          {point.x + Scale_Px(1) + signWidth, point.y + m_center}};
        antialiassingDC->DrawSpline(5, pointsL);
        pointsL[2] = {point.x + Scale_Px(1.5), point.y};
        antialiassingDC->DrawSpline(5, pointsL);

        // Right bracket
        wxPoint pointsR[5] = {
          {point.x + m_width - Scale_Px(1) - signWidth, point.y - m_center},
          {point.x + m_width - Scale_Px(1) - signWidth / 2,
           point.y - m_center + signWidth / 2},
          {point.x + m_width - Scale_Px(1.5), point.y},
          {point.x + m_width - Scale_Px(1) - signWidth / 2,
           point.y + m_center - signWidth / 2},
          {point.x + m_width - Scale_Px(1) - signWidth, point.y + m_center}};
        antialiassingDC->DrawSpline(5, pointsR);
        pointsR[2] = {point.x + m_width - Scale_Px(1), point.y};
        antialiassingDC->DrawSpline(5, pointsR);
        break;
      }
      case paren_angled: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (m_height <= signWidth / 3)
          signWidth = m_height / 3;

        // Left bracket
        wxPoint pointsL[3] = {
          {point.x + Scale_Px(1) + signWidth, point.y - m_center},
          {point.x + Scale_Px(1), point.y},
          {point.x + Scale_Px(1) + signWidth, point.y + m_center}};
        antialiassingDC->DrawLines(3, pointsL);

        // Right bracket
        wxPoint pointsR[3] = {
          {point.x + m_width - Scale_Px(1) - signWidth, point.y - m_center},
          {point.x + m_width - Scale_Px(1.5), point.y},
          {point.x + m_width - Scale_Px(1) - signWidth, point.y + m_center}};
        antialiassingDC->DrawLines(3, pointsR);
        break;
      }
      case paren_straight: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (m_height <= signWidth / 3)
          signWidth = m_height / 3;

        // Left bracket
        wxPoint pointsL[2] = {
          {point.x + Scale_Px(1) + signWidth / 2, point.y - m_center},
          {point.x + Scale_Px(1) + signWidth / 2, point.y + m_center}};
        antialiassingDC->DrawLines(2, pointsL);

        // Right bracket
        wxPoint pointsR[2] = {{point.x + m_width - Scale_Px(1) - signWidth / 2,
                                 point.y - m_center},
                              {point.x + m_width - Scale_Px(1) - signWidth / 2,
                               point.y + m_center}};
        antialiassingDC->DrawLines(2, pointsR);
        break;
      }
      case paren_brackets: {
        SetPen(dc, 1.5);
        // left bracket
        const wxPoint pointsL[4] = {{Scale_Px(5), -m_center + Scale_Px(2)},
                                    {Scale_Px(1), -m_center + Scale_Px(2)},
                                    {Scale_Px(1), m_center - Scale_Px(2)},
                                    {Scale_Px(5), m_center - Scale_Px(2)}};
        antialiassingDC->DrawLines(4, pointsL, point.x, point.y);

        // right bracket
        const wxPoint pointsR[4] = {{-Scale_Px(5), -m_center + Scale_Px(2)},
                                    {-Scale_Px(1), -m_center + Scale_Px(2)},
                                    {-Scale_Px(1), m_center - Scale_Px(2)},
                                    {-Scale_Px(5), m_center - Scale_Px(2)}};
        antialiassingDC->DrawLines(4, pointsR, point.x + m_width - 1, point.y);
        break;
      }
      case paren_none:
        break;
      }
    }
  }
}

void MatrCell::AddNewCell(std::unique_ptr<Cell> &&cell) {
  m_cells.emplace_back(std::move(cell));
}

wxString MatrCell::ToString() const {
  wxString s = wxS("matrix(\n");
  for (size_t i = 0; i < m_matHeight; i++) {
    s += wxS("\t\t[");
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToString();
      if (j < m_matWidth - 1)
        s += wxS(",\t");
    }
    s += wxS("]");
    if (i < m_matHeight - 1)
      s += wxS(",");
    s += wxS("\n");
  }
  s += wxS("\t)");
  return s;
}

wxString MatrCell::ToMatlab() const {
  // ToDo: We ignore colNames and rowNames here. Are they currently in use?
  wxString s;

  s = wxS("[");
  for (size_t i = 0; i < m_matHeight; i++) {
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToMatlab();
      if (j < m_matWidth - 1)
        s += wxS(", ");
    }
    if (i < m_matHeight - 1)
      s += wxS(";\n");
  }

  s += wxS("];");

  return s;
}

wxString MatrCell::ToTeX() const {
  // ToDo: We ignore colNames and rowNames here. Are they currently in use?
  wxString s;

  if (!m_specialMatrix) {
    switch (m_parenType) {
    case paren_rounded:
      s = wxS("\\begin{pmatrix}");
      break;
    case paren_brackets:
    case paren_angled:
    case paren_straight:
    case paren_none:
      s = wxS("\\begin{bmatrix}");
      break;
    }
  } else {
    s = wxS("\\begin{array}{");
    for (size_t j = 0; j < m_matWidth; j++)
      s += wxS("c");
    s += wxS("}");
  }
  for (size_t i = 0; i < m_matHeight; i++) {
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToTeX();
      if (j < m_matWidth - 1)
        s += wxS(" & ");
    }
    if (i < m_matHeight - 1)
      s += wxS("\\\\\n");
  }
  if (!m_specialMatrix) {
    switch (m_parenType) {
    case paren_rounded:
      s += wxS("\\end{pmatrix}");
      break;
    case paren_brackets:
    case paren_angled:
    case paren_straight:
    case paren_none:
      s += wxS("\\end{bmatrix}");
      break;
    }
  } else
    s += wxS("\\end{array}");
  return s;
}

wxString MatrCell::ToMathML() const {
  wxString retval;
  if (!m_specialMatrix)
    retval = wxS("<mrow><mo>(</mo><mrow>");
  retval += wxS("<mtable>");

  for (size_t i = 0; i < m_matHeight; i++) {
    retval += wxS("<mtr>");
    for (size_t j = 0; j < m_matWidth; j++)
      retval += wxS("<mtd>") + GetInnerCell(i, j)->ListToMathML() +
        wxS("</mtd>");
    retval += wxS("</mtr>");
  }
  retval += wxS("</mtable>\n");
  if (!m_specialMatrix)
    retval += wxS("</mrow><mo>)</mo></mrow>\n");
  return retval;
}

wxString MatrCell::ToOMML() const {
  wxString retval;

  retval = wxS("<m:d>");
  if (!m_specialMatrix) {
    switch (m_parenType) {
    case paren_rounded:
      retval += wxS("<m:dPr><m:begChr>(</m:begChr><m:endChr>)</m:endChr> "
                    "<m:grow>\"1\"</m:grow></m:dPr>");
      break;
    case paren_brackets:
      retval += wxS("<m:dPr><m:begChr>[</m:begChr><m:endChr>]</m:endChr> "
                    "<m:grow>\"1\"</m:grow></m:dPr>");
      break;
    case paren_angled:
      retval += wxS("<m:dPr><m:begChr>&lt;</m:begChr><m:endChr>&gt;</m:endChr> "
                    "<m:grow>\"1\"</m:grow></m:dPr>");
      break;
    case paren_straight:
      retval += wxS("<m:dPr><m:begChr>|</m:begChr><m:endChr>|</m:endChr> "
                    "<m:grow>\"1\"</m:grow></m:dPr>");
      break;
    case paren_none:
      retval += wxS("<m:dPr><m:begChr> </m:begChr><m:endChr> </m:endChr> "
                    "<m:grow>\"1\"</m:grow></m:dPr>");
      break;
    }
  }

  retval += wxS("<m:e><m:m>");

  for (size_t i = 0; i < m_matHeight; i++) {
    retval += wxS("<m:mr>");
    for (size_t j = 0; j < m_matWidth; j++)
      retval += wxS("<m:e>") + GetInnerCell(i, j)->ListToOMML() +
        wxS("</m:e>");
    retval += wxS("</m:mr>");
  }

  retval += wxS("</m:m></m:e></m:d>");
  return retval;
}

wxString MatrCell::ToXML() const {
  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");
  switch (m_parenType) {
  case paren_rounded:
    flags += wxS(" roundedParens=\"true\"");
    break;
  case paren_brackets:
    flags += wxS(" roundedParens=\"false\"");
    flags += wxS(" bracketParens=\"true\"");
    break;
  case paren_angled:
    flags += wxS(" roundedParens=\"false\"");
    flags += wxS(" angledParens=\"true\"");
    break;
  case paren_straight:
    flags += wxS(" roundedParens=\"false\"");
    flags += wxS(" straightParens=\"true\"");
    break;
  case paren_none:
    flags += wxS(" roundedParens=\"false\"");
    break;
  }

  wxString s = wxEmptyString;
  if (m_specialMatrix)
    s = wxString::Format(wxS("<tb") + flags +
                         wxS(" special=\"true\" inference=\"%s\" "
                             "rownames=\"%s\" colnames=\"%s\">"),
                         m_inferenceMatrix ? "true" : "false",
                         m_rowNames ? "true" : "false",
                         m_colNames ? "true" : "false");
  else
    s = wxS("<tb") + flags + wxS(">");

  for (size_t i = 0; i < m_matHeight; i++) {
    s += wxS("<mtr>");
    for (size_t j = 0; j < m_matWidth; j++)
      s += wxS("<mtd>") + GetInnerCell(i, j)->ListToXML() +
        wxS("</mtd>");
    s += wxS("</mtr>");
  }
  s += wxS("</tb>");

  return s;
}

void MatrCell::SetDimension() {
  if (m_matHeight != 0)
    m_matWidth = m_matWidth / m_matHeight;
}
