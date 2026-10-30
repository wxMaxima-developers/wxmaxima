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
  This file defines the class ExptCell

  ExptCell is the Cell type that represents exponents.
*/

#include "ExptCell.h"
#include "CellImpl.h"
#include "ParenCell.h"
#include <utility>
#include <memory>

#define EXPT_DEC 2

ExptCell::ExptCell(GroupCell *group, Configuration *config,
                   std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&expt)
  : Cell(group, config), m_baseCell(std::move(base)),
    m_exptCell(std::move(expt)) {
  SetStyle(TS_VARIABLE);
  MakeBreakupCells();
}

ExptCell::ExptCell(GroupCell *group, const ExptCell &cell)
  : ExptCell(group, cell.m_configuration,
             CopyList(group, cell.m_baseCell.get()),
             CopyList(group, cell.m_exptCell.get())) {
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
  MakeBreakupCells();
  IsMatrix(cell.m_isMatrix);
}

DEFINE_CELL(ExptCell)

void ExptCell::MakeBreakupCells() {
  if (m_open)
    return;
  m_exp = std::make_unique<TextCell>(m_group, m_configuration, wxS("^"));
  m_exp->SetStyle(TS_FUNCTION);
  m_open = std::make_unique<TextCell>(m_group, m_configuration, wxS("("));
  m_open->SetStyle(TS_FUNCTION);
  static_cast<TextCell &>(*m_open).DontEscapeOpeningParenthesis();
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxS(")"));
  m_close->SetStyle(TS_FUNCTION);
  m_matrixOpen =
    std::make_unique<TextCell>(m_group, m_configuration, wxS("\u27E8"));
  m_matrixOpen->SetStyle(TS_FUNCTION);
  m_matrixClose =
    std::make_unique<TextCell>(m_group, m_configuration, wxS("\u27E9"));
  m_matrixClose->SetStyle(TS_FUNCTION);

  if (!m_exptCell->IsCompound()) {
    m_open->Hide();
    m_close->Hide();
  }
}

void ExptCell::IsMatrix(bool isMatrix) {
  m_isMatrix = isMatrix;
  m_exp->SetValue(isMatrix ? wxS("^^") : wxS("^"));
  if (isMatrix)
    SetToolTip(MatrixPowerToolTip());
}

wxString ExptCell::MatrixPowerToolTip() {
  return _("A matrix power: A^^n is A.A. ... .A (n times), the "
           "non-commutative product. A^n would instead raise every "
           "element of A to the n-th power.");
}

wxCoord ExptCell::ExponentWidth() const {
  wxCoord width = m_exptCell->SumOfWidths();
  if (m_isMatrix)
    width += m_matrixOpen->GetWidth() + m_matrixClose->GetWidth();
  return width;
}

/**
 * @brief Pass 2 (Arrange): Positions the base and the raised exponent.
 */
void ExptCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
  
  // Linearized cells (broken into lines) behave as zero-size containers.
  // Their children are positioned by the flattened GroupCell loop.
  if (IsBrokenIntoLines())
    return;

  // Position the base
  m_baseCell->SetCurrentPointList(point);

  // Position the exponent (raised by m_expt_yoffset and shifted right)
  point.x += m_baseCell->SumOfWidths() - MC_TEXT_PADDING;
  point.y -= m_expt_yoffset;
  if (m_isMatrix) {
    m_matrixOpen->SetCurrentPoint(point);
    point.x += m_matrixOpen->GetWidth();
  }
  m_exptCell->SetCurrentPointList(point);
  if (m_isMatrix) {
    point.x += m_exptCell->SumOfWidths();
    m_matrixClose->SetCurrentPoint(point);
  }
}

void ExptCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  if (DrawThisCell()) {
    m_baseCell->DrawList(dc, antialiassingDC);
    m_exptCell->DrawList(dc, antialiassingDC);
    if (m_isMatrix) {
      m_matrixOpen->Draw(dc, antialiassingDC);
      m_matrixClose->Draw(dc, antialiassingDC);
    }
  }
}

void ExptCell::Recalculate(AFontSize fontsize) const {
  bool changed = false;
  changed |= m_baseCell->RecalculateList(fontsize);

  changed |= m_exp->RecalculateList(fontsize);
  changed |= m_open->RecalculateList(fontsize);
  changed |= m_close->RecalculateList(fontsize);

  if (IsBrokenIntoLines())
    changed |= m_exptCell->RecalculateList(fontsize);
  else
    changed |= m_exptCell->RecalculateList({MC_MIN_SIZE, fontsize - EXPT_DEC});
  // The angle brackets of a matrix power belong to the exponent, so they
  // share its font size.
  changed |= m_matrixOpen->RecalculateList({MC_MIN_SIZE, fontsize - EXPT_DEC});
  changed |= m_matrixClose->RecalculateList({MC_MIN_SIZE, fontsize - EXPT_DEC});

  if (changed || NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);

    if (IsBrokenIntoLines()) {
      m_height = m_width = m_center = 0;
    } else {
      m_width = m_baseCell->SumOfWidths() + ExponentWidth() - MC_TEXT_PADDING;
      m_expt_yoffset = m_exptCell->GetMaxDrop() + .3 * m_fontSize_Scaled;

      m_height = m_baseCell->GetHeightList();
      m_center = m_baseCell->GetCenterList();

      int baseHeight = m_baseCell->GetHeightList() - m_baseCell->GetMaxDrop();
      int exptHeight =
        m_exptCell->GetHeightList() - m_exptCell->GetMaxDrop() + m_expt_yoffset;

      if (baseHeight < exptHeight) {
        m_height += exptHeight - baseHeight;
        m_center += exptHeight - baseHeight;
      } else
        m_expt_yoffset += baseHeight - exptHeight;
    }
  }
}

wxString ExptCell::ToString() const {
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;
  if (IsBrokenIntoLines())
    return wxEmptyString;
  wxString s = m_baseCell->ListToString() + wxS("^");
  if (m_isMatrix)
    s += wxS("^");
  if (m_exptCell->IsCompound())
    s += wxS("(") + m_exptCell->ListToString() + wxS(")");
  else
    s += m_exptCell->ListToString();
  return s;
}

wxString ExptCell::ToMatlab() const {
  // m_altCopyText is Maxima syntax (A^^n), which Matlab doesn't understand.
  if ((m_altCopyText != wxEmptyString) && !m_isMatrix)
    return m_altCopyText;
  if (IsBrokenIntoLines())
    return wxEmptyString;
  // In Matlab ^ already is the matrix power (.^ is the elementwise one), so
  // Maxima's A^^n becomes A^n.
  wxString s = m_baseCell->ListToMatlab() + wxS("^");
  if (m_exptCell->IsCompound())
    s += wxS("(") + m_exptCell->ListToMatlab() + wxS(")");
  else
    s += m_exptCell->ListToMatlab();
  return s;
}

wxString ExptCell::ToTeX() const {
  const wxString base = m_baseCell->ListToTeX();
  // A superscript on the closing parenthesis looks exactly like one on the
  // whole parenthesized base, so (x+1)^2 needs no braces around the base.
  const bool parenthesized =
    !m_baseCell->GetNext() && (dynamic_cast<const ParenCell *>(m_baseCell.get())) &&
    (base.StartsWith(wxS("(")) || base.StartsWith(wxS("\\left(")));
  // A matrix power is written the way Maxima's own tex() writes it.
  const wxString exponent =
    m_isMatrix ? wxS("{\\langle ") + m_exptCell->ListToTeX() + wxS("\\rangle}")
               : TeXGroup(m_exptCell->ListToTeX());
  return (parenthesized ? base : TeXGroup(base)) + wxS("^") + exponent;
}

wxString ExptCell::ExponentToMathML() const {
  if (!m_isMatrix)
    return m_exptCell->ListToMathML();
  return wxS("<mrow>") + m_matrixOpen->ToMathML() +
    m_exptCell->ListToMathML() + m_matrixClose->ToMathML() + wxS("</mrow>");
}

wxString ExptCell::GetDiffPart() const {
  wxString s(wxS(","));
  if (m_baseCell != nullptr)
    s += m_baseCell->ListToString();
  s += wxS(",");
  if (m_exptCell != nullptr)
    s += m_exptCell->ListToString();
  return s;
}

wxString ExptCell::ToMathML() const {
  return wxS("<msup>") + m_baseCell->ListToMathML() + ExponentToMathML() +
    wxS("</msup>\n");
  //  return wxS("<apply><power/>") + m_baseCell->ListToMathML() +
  //  m_exptCell->ListToMathML() + wxS("</apply>");
}

wxString ExptCell::ToOMML() const {
  return wxS("<m:sSup><m:e>") + m_baseCell->ListToOMML() +
    wxS("</m:e><m:sup>") +
    (m_isMatrix ? m_matrixOpen->ToOMML() : wxString()) +
    m_exptCell->ListToOMML() +
    (m_isMatrix ? m_matrixClose->ToOMML() : wxString()) +
    wxS("</m:sup></m:sSup>\n");
}

wxString ExptCell::ToXML() const {
  return wxS("<e") + GetXMLFlags() +
    (m_isMatrix ? wxString(wxS(" mat=\"true\"")) : wxString()) + wxS("><r>") + m_baseCell->ListToXML() +
    wxS("</r><r>") + m_exptCell->ListToXML() + wxS("</r></e>");
}

bool ExptCell::BreakUp() const {
  if (IsBrokenIntoLines())
    return false;

  Cell::BreakUpAndMark();
  return true;
}
