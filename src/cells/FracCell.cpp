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
#include <utility>
#include <memory>

#define FRAC_DEC 1

FracCell::FracCell(GroupCell *group, Configuration *config,
                   std::unique_ptr<Cell> &&num, std::unique_ptr<Cell> &&denom)
  : Cell(group, config), m_numParenthesis(std::make_unique<ParenCell>(
                                                                      group, m_configuration, std::move(num))),
    m_denomParenthesis(std::make_unique<ParenCell>(group, m_configuration,
                                                   std::move(denom))) {
  InitBitFields_FracCell();
  SetStyle(TS_VARIABLE);
  SetupBreakUps();
  MakeDivideCell();
}

FracCell::FracCell(GroupCell *group, const FracCell &cell)
  : FracCell(group, cell.m_configuration, CopyList(group, cell.Num()),
             CopyList(group, cell.Denom())) {
  CopyCommonData(cell);
  m_fracStyle = cell.m_fracStyle;
  SetupBreakUps();
  if (cell.m_inExponent)
    SetIsExponent();
  MakeDivideCell();
}

DEFINE_CELL(FracCell)

void FracCell::SetFracStyle(FracType style)
{ m_fracStyle = style;
  if(style == FC_DIFF)
    {
      Cell *multsign = Denom();
      if(multsign)
        multsign = multsign->GetNext();
      if(multsign)
        {
          if ((multsign->GetValue() == wxS("\u00B7")) || (multsign->GetValue() == wxS("*")))
            multsign->Hide(true);
          else
            wxLogMessage(_("diff(): Cannot find the multiplication sign I should hide"));
        }
    }
}


void FracCell::MakeDivideCell() {
  if (m_divideOwner)
    return;
  m_divideOwner = std::make_unique<TextCell>(m_group, m_configuration, wxS("/"));
  m_divideOwner->SetStyle(TS_VARIABLE);
  m_divide = m_divideOwner.get();
}

void FracCell::Recalculate(AFontSize fontsize) const {
  if (NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);
    if (m_inExponent || IsBrokenIntoLines()) {
      m_displayedNum->RecalculateList(fontsize);
      m_displayedDenom->RecalculateList(fontsize);
    } else {
      m_displayedNum->RecalculateList({MC_MIN_SIZE, fontsize - FRAC_DEC});
      m_displayedDenom->RecalculateList({MC_MIN_SIZE, fontsize - FRAC_DEC});
    }
    m_divide->RecalculateList(fontsize);

    if (IsBrokenIntoLines()) {
      m_height = 0;
      m_center = 0;
      m_width = 0;
    } else {
      if (m_inExponent) {
        m_protrusion = m_horizontalGapLeft = m_horizontalGapRight = 0;
        // Numerator/denominator can be multi-cell lists (e.g. (a+b)/c), and the
        // linear a/b layout below positions the pieces with SumOfWidths(). Use
        // the same list-wide widths here - GetWidth() would only count the first
        // cell and make the fraction report a width narrower than it draws,
        // overlapping whatever follows. (Matches the GetHeightList()/
        // GetCenterList() list-wide calls just below.)
        m_width = m_displayedNum->SumOfWidths() + m_displayedDenom->SumOfWidths() +
          m_divide->SumOfWidths();
        m_height = std::max(m_displayedNum->GetHeightList(), m_displayedDenom->GetHeightList()) +
          Scale_Px(6.5);
        m_center =
          std::max(m_displayedNum->GetCenterList(), m_displayedDenom->GetCenterList()) + Scale_Px(3);
      } else {
        m_protrusion = Scale_Px(m_configuration->GetMathFontSize() / 2);

        // We want half a space's widh of blank space to separate us from the
        // next minus.

        if (GetPrevious() && GetPrevious()->ToString().EndsWith(wxS("-")))
          m_horizontalGapLeft = m_protrusion;
        else
          m_horizontalGapLeft = 0;

        if (GetNext() && GetNext()->ToString().StartsWith(wxS("-")))
          m_horizontalGapRight = m_protrusion;
        else
          m_horizontalGapRight = 0;

        m_width = std::max(m_displayedNum->SumOfWidths(),
                           m_displayedDenom->SumOfWidths()) +
          2 * m_protrusion + m_horizontalGapLeft + m_horizontalGapRight;
        m_height =
          m_displayedNum->GetHeightList() + m_displayedDenom->GetHeightList() + Scale_Px(6.5);
        m_center = m_displayedNum->GetHeightList() + Scale_Px(3);
      }
    }
  }
}

/**
 * @brief Pass 2 (Arrange): Positions numerator, denominator, and division sign.
 */
void FracCell::SetCurrentPoint(wxPoint point) {
  Cell::SetCurrentPoint(point);
  
  // Linearized cells (broken into lines) behave as zero-size containers.
  // Their children are positioned by the flattened GroupCell loop.
  if (IsBrokenIntoLines())
    return;

  wxPoint num, denom;

  if (m_inExponent) {
    // Linear 1D layout for exponents (a/b)
    num = point;
    wxPoint divide(point);
    divide.x += m_displayedNum->SumOfWidths();
    denom = divide;
    denom.x += m_divide->SumOfWidths();

    m_displayedNum->SetCurrentPointList(num);
    m_divide->SetCurrentPoint(divide);
    m_displayedDenom->SetCurrentPointList(denom);
  } else {
    // 2D Stacked layout
    // Numerator is centered above the baseline
    num.x = point.x + m_horizontalGapLeft +
      (m_width - m_horizontalGapLeft - m_horizontalGapRight -
       m_displayedNum->SumOfWidths()) /
      2;
    num.y = point.y - m_displayedNum->GetMaxDrop();
    m_displayedNum->SetCurrentPointList(num);

    // Denominator is centered below the baseline
    denom.x = point.x + m_horizontalGapLeft +
      (m_width - m_horizontalGapLeft - m_horizontalGapRight -
       m_displayedDenom->SumOfWidths()) /
      2;
    denom.y = point.y + m_displayedDenom->GetCenterList() + Scale_Px(4);
    m_displayedDenom->SetCurrentPointList(denom);
  }
}

void FracCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  if (DrawThisCell()) {
    if (IsBrokenIntoLines())
      return;

    wxPoint point = GetCurrentPoint();
    if (m_inExponent) {
      m_displayedNum->DrawList(dc, antialiassingDC);
      m_divide->Draw(dc, antialiassingDC);
      m_displayedDenom->DrawList(dc, antialiassingDC);
    } else {
      m_displayedNum->DrawList(dc, antialiassingDC);
      m_displayedDenom->DrawList(dc, antialiassingDC);
      if (m_fracStyle != FC_CHOOSE) {
        SetPen(antialiassingDC, 1.2);
        antialiassingDC->DrawLine(
            point.x + m_horizontalGapLeft +
                m_configuration->GetDefaultLineWidth() / 2,
            point.y + Scale_Px(2),
            point.x + m_width - m_horizontalGapRight -
                m_configuration->GetDefaultLineWidth() / 2,
            point.y + Scale_Px(2));
      }
    }
  }
}

wxString FracCell::ToString() const {
  wxString s;
  if (!IsBrokenIntoLines()) {
    if (m_fracStyle == FC_NORMAL) {
      if (Num()->IsCompound())
        s += wxS("(") + Num()->ListToString() + wxS(")/");
      else
        s += Num()->ListToString() + wxS("/");
      if (Denom()->IsCompound())
        s += wxS("(") + Denom()->ListToString() + wxS(")");
      else
        s += Denom()->ListToString();
    } else if (m_fracStyle == FC_CHOOSE) {
      s = wxS("binomial(") + Num()->ListToString() + wxS(",") +
        Denom()->ListToString() + wxS(")");
    } else {
      const Cell *tmp = Denom();
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
        s += wxS("(") + Num()->ListToMatlab() + wxS(")/");
      else
        s += Num()->ListToMatlab() + wxS("/");
      if (Denom()->IsCompound())
        s += wxS("(") + Denom()->ListToMatlab() + wxS(")");
      else
        s += Denom()->ListToMatlab();
    } else if (m_fracStyle == FC_CHOOSE) {
      s = wxS("binomial(") + Num()->ListToMatlab() + wxS(",") +
        Denom()->ListToMatlab() + wxS(")");
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
      s = wxS("\\begin{pmatrix}") + Num()->ListToTeX() + wxS("\\\\\n") +
        Denom()->ListToTeX() + wxS("\\end{pmatrix}");
    } else {
      s = wxS("\\frac{") + Num()->ListToTeX() + wxS("}{") +
        Denom()->ListToTeX() + wxS("}");
    }
  }
  return s;
}

wxString FracCell::ToMathML() const {
  wxString flags;
  if (m_fracStyle == FC_CHOOSE)
    flags = wxS(" linethickness=\"0\"");
  if (m_inExponent)
    flags += wxS(" bevelled=\"true\"");
  return wxS("<mfrac") + flags + wxS(">") + Num()->ListToMathML() +
    Denom()->ListToMathML() + wxS("</mfrac>\n");
}

wxString FracCell::ToOMML() const {
  return wxS("<m:f><m:num>") + Num()->ListToOMML() + wxS("</m:num><m:den>") +
    Denom()->ListToOMML() + wxS("</m:den></m:f>\n");
}

wxString FracCell::ToXML() const {
  wxString s = (m_fracStyle == FC_NORMAL || m_fracStyle == FC_DIFF)
    ? wxS("")
    : wxS(" line=\"no\"");
  wxString diffStyle;
  if (m_fracStyle == FC_DIFF)
    diffStyle = wxS(" diffstyle=\"yes\"");

  return wxS("<f") + s + diffStyle + GetXMLFlags() + wxS("><r>") + Num()->ListToXML() +
    wxS("</r><r>") + Denom()->ListToXML() + wxS("</r></f>");
}

void FracCell::SetIsExponent() {
  if (Num()->IsShortNum() && Denom()->IsShortNum()) {
    m_inExponent = true;
    MakeDivideCell();
    Num()->SetIsExponentList();
    Denom()->SetIsExponentList();
  }
}

void FracCell::SetupBreakUps() const {
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

bool FracCell::BreakUp() const {
  if (m_fracStyle == FC_DIFF || IsBrokenIntoLines())
    return false;

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
  return true;
}

void FracCell::SetNextToDraw(Cell *next) const {
  if (IsBrokenIntoLines())
    m_displayedDenom->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
