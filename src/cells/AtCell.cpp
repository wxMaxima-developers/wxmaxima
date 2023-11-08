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
  This file defines the class AtCell

  AtCell is the Cell type that represents maxima's at() command.
*/

#include <memory>
#include <utility>
#include "AtCell.h"
#include "CellImpl.h"

AtCell::AtCell(GroupCell *group, Configuration *config,
               std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&index)
    : Cell(group, config), m_baseCell(std::move(base)),
      m_indexCell(std::move(index)) {
    SetStyle(TS_VARIABLE);
}

AtCell::AtCell(GroupCell *group, const AtCell &cell)
    : AtCell(group, cell.m_configuration,
             CopyList(group, cell.m_baseCell.get()),
             CopyList(group, cell.m_indexCell.get())) {
    CopyCommonData(cell);
}

DEFINE_CELL(AtCell)

void AtCell::Recalculate(AFontSize fontsize) {
    m_baseCell->RecalculateList(fontsize);
    m_indexCell->RecalculateList({MC_MIN_SIZE, fontsize - 3});
    m_width =
        m_baseCell->GetFullWidth() + m_indexCell->GetFullWidth() + Scale_Px(4);
    m_height =
        m_baseCell->GetHeightList() + m_indexCell->GetHeightList() - Scale_Px(7);
    m_center = m_baseCell->GetCenter();
    Cell::Recalculate(fontsize);
}

void AtCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
    Cell::Draw(point, dc, antialiassingDC);
    if (DrawThisCell(point)) {
        wxPoint bs, in;

        bs.x = point.x;
        bs.y = point.y;
        m_baseCell->DrawList(bs, dc, antialiassingDC);

        in.x = point.x + m_baseCell->GetFullWidth() + Scale_Px(4);
        in.y = point.y + m_baseCell->GetMaxDrop() + +m_indexCell->GetCenterList() -
            Scale_Px(7);
        m_indexCell->DrawList(in, dc, antialiassingDC);
        SetPen(dc);
        dc->DrawLine(in.x - Scale_Px(2), bs.y - m_baseCell->GetCenterList(),
                     in.x - Scale_Px(2), in.y);
    }
}

wxString AtCell::ToString() const {
    wxString s = wxS("at(");
    s += m_baseCell->ListToString();
    s += wxS(",") + m_indexCell->ListToString() + wxS(")");
    return s;
}

wxString AtCell::ToMatlab() const {
    wxString s = wxS("at(");
    s += m_baseCell->ListToMatlab();
    s += wxS(",") + m_indexCell->ListToMatlab() + wxS(")");
    return s;
}

wxString AtCell::ToTeX() const {
    wxString s = wxS("\\left. ");
    s += m_baseCell->ListToTeX();
    s += wxS("\\right|_{") + m_indexCell->ListToTeX() + wxS("}");
    return s;
}

wxString AtCell::ToMathML() const {
    return wxS("<msub>") + m_baseCell->ListToMathML() +
        m_indexCell->ListToMathML() + wxS("</msub>\n");
}

wxString AtCell::ToOMML() const {
    return wxS("<m:sSub><m:e>") + m_baseCell->ListToOMML() +
        wxS("</m:e><m:sub>") + m_indexCell->ListToOMML() +
        wxS("</m:sub></m:sSub>\n");
}

wxString AtCell::ToXML() const {
    wxString flags;
    if (HasHardLineBreak())
        flags += wxS(" breakline=\"true\"");

    return wxS("<at") + flags + wxS("><r>") + m_baseCell->ListToXML() +
        wxS("</r><r>") + m_indexCell->ListToXML() + wxS("</r></at>");
}
