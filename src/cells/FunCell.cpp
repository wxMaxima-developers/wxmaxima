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
  This file defines the class FunCell

  FunCell is the Cell type that represents functions that don't require special
  handling.
*/

#include "FunCell.h"
#include "CellImpl.h"
#include "TextCell.h"
#include <unordered_map>
#include <utility>
#include <wx/hashmap.h>
#include <memory>

FunCell::FunCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&name, std::unique_ptr<Cell> &&arg)
  : Cell(group, config), m_nameCell(std::move(name)),
    m_argCell(std::move(arg)) {
  SetStyle(TS_FUNCTION);
  m_nameCell->SetStyle(TS_FUNCTION);
}

FunCell::FunCell(GroupCell *group, const FunCell &cell)
  : FunCell(group, cell.m_configuration,
            CopyList(group, cell.m_nameCell.get()),
            CopyList(group, cell.m_argCell.get())) {
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
}

DEFINE_CELL(FunCell)

void FunCell::Recalculate(AFontSize fontsize) const {
  bool changed = false;
  changed |= m_argCell->RecalculateList(fontsize);
  changed |= m_nameCell->RecalculateList(fontsize);

  if (changed || NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);

    if (IsBrokenIntoLines()) {
      m_width = m_center = m_height = 0;
    } else {
      m_width =
        m_nameCell->SumOfWidths() + m_argCell->SumOfWidths() - Scale_Px(1);
      m_center = std::max(m_nameCell->GetCenterList(), m_argCell->GetCenterList());
      m_height =
        m_center + std::max(m_nameCell->GetMaxDrop(), m_argCell->GetMaxDrop());
    }
  }
}

void FunCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
  if (IsBrokenIntoLines())
    return;
  wxPoint name(point), arg(point);
  m_nameCell->SetCurrentPointDrawList(name);

  arg.x += m_nameCell->SumOfWidths();
  m_argCell->SetCurrentPointDrawList(arg);
}

void FunCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  if (DrawThisCell()) {
    m_nameCell->DrawList(dc, antialiassingDC);
    m_argCell->DrawList(dc, antialiassingDC);
  }
}

wxString FunCell::ToString() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;
  return m_nameCell->ListToString() + m_argCell->ListToString();
}

wxString FunCell::ToMatlab() const {
  if (IsBrokenIntoLines())
    return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText + Cell::ListToMatlab();
  wxString s = m_nameCell->ListToMatlab() + m_argCell->ListToMatlab();
  return s;
}

wxString FunCell::ToTeX() const {
  // Functions LaTeX typesets upright, with an operator's spacing. Braces
  // after the command would make the argument an ordinary atom, which only
  // adds a thin space between the name and the parenthesis.
  static const std::unordered_map<wxString, wxString, wxStringHash> texNames = {
    {wxS("sin"), wxS("\\sin")}, {wxS("cos"), wxS("\\cos")},
    {wxS("tan"), wxS("\\tan")}, {wxS("cot"), wxS("\\cot")},
    {wxS("sec"), wxS("\\sec")}, {wxS("csc"), wxS("\\csc")},
    {wxS("sinh"), wxS("\\sinh")}, {wxS("cosh"), wxS("\\cosh")},
    {wxS("tanh"), wxS("\\tanh")}, {wxS("coth"), wxS("\\coth")},
    {wxS("sech"), wxS("\\operatorname{sech}")},
    {wxS("csch"), wxS("\\operatorname{csch}")},
    {wxS("asin"), wxS("\\operatorname{asin}")},
    {wxS("acos"), wxS("\\operatorname{acos}")},
    {wxS("atan"), wxS("\\operatorname{atan}")},
    {wxS("acot"), wxS("\\operatorname{acot}")},
    {wxS("asec"), wxS("\\operatorname{asec}")},
    {wxS("acsc"), wxS("\\operatorname{acsc}")},
    {wxS("asinh"), wxS("\\operatorname{asinh}")},
    {wxS("acosh"), wxS("\\operatorname{acosh}")},
    {wxS("atanh"), wxS("\\operatorname{atanh}")},
    {wxS("acoth"), wxS("\\operatorname{acoth}")},
    {wxS("asech"), wxS("\\operatorname{asech}")},
    {wxS("acsch"), wxS("\\operatorname{acsch}")},
    {wxS("log"), wxS("\\log")}};

  const wxString arg = m_argCell->ListToTeX();
  const auto texName = texNames.find(m_nameCell->ToString());
  if (texName == texNames.end())
    return m_nameCell->ListToTeX() + arg;
  // \sin x needs the space, \sin(x) doesn't.
  if (!arg.IsEmpty() && wxIsalpha(arg[0]))
    return texName->second + wxS(" ") + arg;
  return texName->second + arg;
}

wxString FunCell::ToXML() const {
  return wxS("<fn") + GetXMLFlags() + wxS("><r>") + m_nameCell->ListToXML() +
    wxS("</r>") + m_argCell->ListToXML() + wxS("</fn>");
}

wxString FunCell::ToMathML() const {
  //  if (IsBrokenIntoLines())
  //    return wxEmptyString;
  return wxS("<mrow>") + m_nameCell->ListToMathML() + wxS("<mo>&#x2061;</mo>") +
    m_argCell->ListToMathML() + wxS("</mrow>\n");
}

wxString FunCell::ToOMML() const {
  return m_nameCell->ListToOMML() + m_argCell->ListToOMML();
}

bool FunCell::BreakUp() const {
  if (IsBrokenIntoLines())
    return false;

  Cell::BreakUpAndMark();
  return true;
}
