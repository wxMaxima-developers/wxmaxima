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
  This file defines the class FracCell

  FracCell is the Cell type that represents fractions.
*/

#include "FracCell.h"
#include "CellImpl.h"

#define FRAC_DEC 1

FracCell::FracCell(GroupCell *group, Configuration *config,
                   std::unique_ptr<Cell> &&num, std::unique_ptr<Cell> &&denom)
    : Cell(group, config), m_numParenthesis(std::make_unique<ParenCell>(
                               group, m_configuration, std::move(num))),
      m_denomParenthesis(std::make_unique<ParenCell>(group, m_configuration,
                                                     std::move(denom))) {
  InitBitFields();
  SetStyle(TS_VARIABLE);
  SetupBreakUps();
}

FracCell::FracCell(GroupCell *group, const FracCell &cell)
    : FracCell(group, cell.m_configuration, CopyList(group, cell.Num()),
               CopyList(group, cell.Denom())) {
  CopyCommonData(cell);
  m_fracStyle = cell.m_fracStyle;
  SetupBreakUps();
  if (cell.m_inExponent)
    SetExponentFlag();
}

DEFINE_CELL(FracCell)

void FracCell::MakeDivideCell() {
  if (m_divideOwner)
    return;
  m_divideOwner = std::make_unique<TextCell>(m_group, m_configuration, "/");
  m_divideOwner->SetStyle(TS_VARIABLE);
  m_divide = m_divideOwner.get();
}

void FracCell::Recalculate(AFontSize fontsize) {
  if (m_inExponent || IsBrokenIntoLines()) {
    m_displayedNum->RecalculateList(fontsize);
    m_displayedDenom->RecalculateList(fontsize);
    m_divide->RecalculateList(fontsize);
  } else {
    m_displayedNum->RecalculateList({MC_MIN_SIZE, fontsize - FRAC_DEC});
    m_displayedDenom->RecalculateList({MC_MIN_SIZE, fontsize - FRAC_DEC});
  }

  if (IsBrokenIntoLines()) {
    m_height = 0;
    m_center = 0;
    m_width = 0;
  } else {
    if (m_inExponent) {
      m_protrusion = m_horizontalGapLeft = m_horizontalGapRight = 0;
      m_width = Num()->GetWidth() + Denom()->GetWidth() + m_divide->GetWidth();
      m_height = wxMax(Num()->GetHeightList(), Denom()->GetHeightList()) +
                 Scale_Px(6.5);
      m_center =
          wxMax(Num()->GetCenterList(), Denom()->GetCenterList()) + Scale_Px(3);
    } else {
      m_protrusion = Scale_Px(m_configuration->GetMathFontSize() / 2);

      // We want half a space's widh of blank space to separate us from the
      // next minus.

      if (GetPrevious() && GetPrevious()->ToString().EndsWith(wxT("-")))
        m_horizontalGapLeft = m_protrusion;
      else
        m_horizontalGapLeft = 0;

      if (GetNext() && GetNext()->ToString().StartsWith(wxT("-")))
        m_horizontalGapRight = m_protrusion;
      else
        m_horizontalGapRight = 0;

      m_width = wxMax(m_displayedNum->GetFullWidth(),
                      m_displayedDenom->GetFullWidth()) +
                2 * m_protrusion + m_horizontalGapLeft + m_horizontalGapRight;
      m_height =
          Num()->GetHeightList() + Denom()->GetHeightList() + Scale_Px(6.5);
      m_center = Num()->GetHeightList() + Scale_Px(3);
    }
  }

  Cell::Recalculate(fontsize);
}

void FracCell::Draw(wxPoint point) {
  Cell::Draw(point);
  if (DrawThisCell(point)) {
    wxDC *dc = m_configuration->GetDC();
    wxPoint num, denom;

    if (IsBrokenIntoLines())
      return;

    if (m_inExponent) {
      num = point;
      wxPoint divide(point);
      divide.x += m_displayedNum->GetFullWidth();
      denom = divide;
      denom.x += m_divide->GetFullWidth();

      m_displayedNum->DrawList(num);
      m_divide->Draw(divide);
      m_displayedDenom->DrawList(denom);
    } else {
      num.x = point.x + m_horizontalGapLeft +
              (m_width - m_horizontalGapLeft - m_horizontalGapRight -
               m_displayedNum->GetFullWidth()) /
                  2;
      num.y = point.y - m_displayedNum->GetHeightList() +
              m_displayedNum->GetCenterList();
      m_displayedNum->DrawList(num);

      denom.x = point.x + m_horizontalGapLeft +
                (m_width - m_horizontalGapLeft - m_horizontalGapRight -
                 m_displayedDenom->GetFullWidth()) /
                    2;
      denom.y = point.y + m_displayedDenom->GetCenterList() + Scale_Px(4);
      m_displayedDenom->DrawList(denom);
      SetPen(1.2);
      if (m_fracStyle != FC_CHOOSE)
        dc->DrawLine(point.x + m_horizontalGapLeft +
                         m_configuration->GetDefaultLineWidth() / 2,
                     point.y + Scale_Px(2),
                     point.x + m_width - m_horizontalGapRight -
                         m_configuration->GetDefaultLineWidth() / 2,
                     point.y + Scale_Px(2));
    }
  }
}

wxString FracCell::ToString() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    if (m_fracStyle == FC_NORMAL) {
      if (Num()->IsCompound())
        s += wxT("(") + Num()->ListToString() + wxT(")/");
      else
        s += Num()->ListToString() + wxT("/");
      if (Denom()->IsCompound())
        s += wxT("(") + Denom()->ListToString() + wxT(")");
      else
        s += Denom()->ListToString();
    } else if (m_fracStyle == FC_CHOOSE) {
      s = wxT("binomial(") + Num()->ListToString() + wxT(",") +
          Denom()->ListToString() + wxT(")");
    } else {
      Cell *tmp = Denom();
      while (tmp != NULL) {
        tmp = tmp->GetNext(); // Skip the d
        if (tmp == NULL)
          break;
        tmp = tmp->GetNext(); // Skip the *
        if (tmp == NULL)
          break;
        s += tmp->GetDiffPart();
        tmp = tmp->GetNext(); // Skip the *
        if (tmp == NULL)
          break;
        tmp = tmp->GetNext();
      }
    }
  }
  return s;
}

wxString FracCell::ToMatlab() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    if (m_fracStyle == FC_NORMAL) {
      if (Num()->IsCompound())
        s += wxT("(") + Num()->ListToMatlab() + wxT(")/");
      else
        s += Num()->ListToMatlab() + wxT("/");
      if (Denom()->IsCompound())
        s += wxT("(") + Denom()->ListToMatlab() + wxT(")");
      else
        s += Denom()->ListToMatlab();
    } else if (m_fracStyle == FC_CHOOSE) {
      s = wxT("binomial(") + Num()->ListToMatlab() + wxT(",") +
          Denom()->ListToMatlab() + wxT(")");
    } else {
      for (Cell *tmp = Denom(); tmp; tmp = tmp->GetNext()) {
        tmp = tmp->GetNext(); // Skip the d
        if (!tmp)
          break;
        tmp = tmp->GetNext(); // Skip the *
        if (!tmp)
          break;
        s += tmp->GetDiffPart();
        tmp = tmp->GetNext(); // Skip the *
        if (!tmp)
          break;
      }
    }
  }
  return s;
}

wxString FracCell::ToTeX() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    if (m_fracStyle == FC_CHOOSE) {
      s = wxT("\\begin{pmatrix}") + Num()->ListToTeX() + wxT("\\\\\n") +
          Denom()->ListToTeX() + wxT("\\end{pmatrix}");
    } else {
      s = wxT("\\frac{") + Num()->ListToTeX() + wxT("}{") +
          Denom()->ListToTeX() + wxT("}");
    }
  }
  return s;
}

wxString FracCell::ToMathML() const {
  return wxT("<mfrac>") + Num()->ListToMathML() + Denom()->ListToMathML() +
         wxT("</mfrac>\n");
}

wxString FracCell::ToOMML() const {
  return wxT("<m:f><m:num>") + Num()->ListToOMML() + wxT("</m:num><m:den>") +
         Denom()->ListToOMML() + wxT("</m:den></m:f>\n");
}

wxString FracCell::ToXML() const {
  wxString s = (m_fracStyle == FC_NORMAL || m_fracStyle == FC_DIFF)
                   ? _T("f")
                   : _T("f line = \"no\"");
  wxString diffStyle;
  if (m_fracStyle == FC_DIFF)
    diffStyle = wxT(" diffstyle=\"yes\"");
  if (HasHardLineBreak())
    diffStyle += wxT(" breakline=\"true\"");

  return _T("<") + s + diffStyle + _T("><r>") + Num()->ListToXML() +
         _T("</r><r>") + Denom()->ListToXML() + _T("</r></f>");
}

void FracCell::SetExponentFlag() {
  if (Num()->IsShortNum() && Denom()->IsShortNum()) {
    m_inExponent = true;
    MakeDivideCell();
  }
}

void FracCell::SetupBreakUps() {
  m_displayedNum = m_numParenthesis.get();
  m_displayedDenom = m_denomParenthesis.get();
  if (m_fracStyle == FC_CHOOSE) {
    if (Num() && !Num()->IsCompound())
      m_displayedNum = Num();
    if (Denom() && !Denom()->IsCompound())
      m_displayedDenom = Denom();
  } else {
    m_displayedNum = Num();
    m_displayedDenom = Denom();
  }
}

bool FracCell::BreakUp() {
  if (m_fracStyle == FC_DIFF || IsBrokenIntoLines())
    return false;

  MakeDivideCell();
  Cell::BreakUpAndMark();
  if (Num() && Num()->GetNext())
    m_displayedNum = m_numParenthesis.get();
  if (Denom() && Denom()->GetNext())
    m_displayedDenom = m_denomParenthesis.get();
  // Note: Yes, we don't want m_displayedNum->last() here.
  m_displayedNum->SetNextToDraw(m_divide);
  m_divide->SetNextToDraw(m_displayedDenom);
  m_displayedDenom->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_displayedNum;
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void FracCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_displayedDenom->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
