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
#include <utility>
#include <memory>

FunCell::FunCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&name, std::unique_ptr<Cell> &&arg)
    : Cell(group, config), m_nameCell(std::move(name)),
      m_argCell(std::move(arg)) {
    InitBitFields();
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

void FunCell::Recalculate(AFontSize fontsize) {
    m_argCell->RecalculateList(fontsize);
    m_nameCell->RecalculateList(fontsize);

    if (IsBrokenIntoLines()) {
        m_width = m_center = m_height = 0;
    } else {
        m_width =
            m_nameCell->GetFullWidth() + m_argCell->GetFullWidth() - Scale_Px(1);
        m_center = wxMax(m_nameCell->GetCenterList(), m_argCell->GetCenterList());
        m_height =
            m_center + wxMax(m_nameCell->GetMaxDrop(), m_argCell->GetMaxDrop());
    }
    Cell::Recalculate(fontsize);
}

void FunCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
    Cell::Draw(point, dc, antialiassingDC);
    if (DrawThisCell(point)) {
        wxPoint name(point), arg(point);
        m_nameCell->DrawList(name, dc, antialiassingDC);

        arg.x += m_nameCell->GetFullWidth();
        m_argCell->DrawList(arg, dc, antialiassingDC);
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
    if (IsBrokenIntoLines())
        return wxEmptyString;

    wxString s;

    if ((m_nameCell->ToString() == wxS("sin")) ||
        (m_nameCell->ToString() == wxS("cos")) ||
        (m_nameCell->ToString() == wxS("cosh")) ||
        (m_nameCell->ToString() == wxS("sinh")) ||
        (m_nameCell->ToString() == wxS("log")) ||
        (m_nameCell->ToString() == wxS("cot")) ||
        (m_nameCell->ToString() == wxS("sec")) ||
        (m_nameCell->ToString() == wxS("csc")) ||
        (m_nameCell->ToString() == wxS("tan")))
        s = wxS("\\") + m_nameCell->ToString() + wxS("{") + m_argCell->ListToTeX() +
            wxS("}");
    else
        s = m_nameCell->ListToTeX() + m_argCell->ListToTeX();

    return s;
}

wxString FunCell::ToXML() const {
    //  if (IsBrokenIntoLines())
    //    return wxEmptyString;
    wxString flags;
    if (HasHardLineBreak())
        flags += wxS(" breakline=\"true\"");
    return wxS("<fn") + flags + wxS("><r>") + m_nameCell->ListToXML() +
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

bool FunCell::BreakUp() {
    if (IsBrokenIntoLines())
        return false;

    Cell::BreakUpAndMark();
    m_nameCell->last()->SetNextToDraw(m_argCell);
    m_argCell->last()->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_nameCell;
    ResetCellListSizes();
    m_height = 0;
    m_center = 0;
    return true;
}

void FunCell::SetNextToDraw(Cell *next) {
    if (IsBrokenIntoLines())
        m_argCell->last()->SetNextToDraw(next);
    else
        m_nextToDraw = next;
}
