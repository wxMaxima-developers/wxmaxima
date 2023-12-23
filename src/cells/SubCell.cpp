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
  This file defines the class SubCell

  SubCell is the Cell type that represents a math element with subscript.
*/

#include "SubCell.h"
#include "CellImpl.h"

#define SUB_DEC 2

SubCell::SubCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&index)
  : Cell(group, config), m_baseCell(std::move(base)),
    m_indexCell(std::move(index)) {
  InitBitFields_SubCell();
  SetStyle(TS_VARIABLE);
}

SubCell::SubCell(GroupCell *group, const SubCell &cell)
  : SubCell(group, cell.m_configuration,
            CopyList(group, cell.m_baseCell.get()),
            CopyList(group, cell.m_indexCell.get())) {
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
}

DEFINE_CELL(SubCell)

void SubCell::Recalculate(AFontSize fontsize) {
  m_baseCell->RecalculateList(fontsize);
  m_indexCell->RecalculateList({MC_MIN_SIZE, fontsize - SUB_DEC});
  m_width =
    m_baseCell->GetFullWidth() + m_indexCell->GetFullWidth() - Scale_Px(2);
  m_height = m_baseCell->GetHeightList() + m_indexCell->GetHeightList() -
    Scale_Px(.8 * fontsize + MC_EXP_INDENT);
  m_center = m_baseCell->GetCenter();
  Cell::Recalculate(fontsize);
}

void SubCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs, dc, antialiassingDC);

    in.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(2);
    in.y = point.y + m_baseCell->GetMaxDrop() + m_indexCell->GetCenterList() -
      .8 * m_fontSize_Scaled + MC_EXP_INDENT;
    m_indexCell->DrawList(in, dc, antialiassingDC);
  }
}

wxString SubCell::ToString() const {
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s;
  if (m_baseCell->IsCompound())
    s += wxS("(") + m_baseCell->ListToString() + wxS(")");
  else
    s += m_baseCell->ListToString();
  s += wxS("[") + m_indexCell->ListToString() + wxS("]");
  return s;
}

wxString SubCell::ToMatlab() const {
  if (m_altCopyText != wxEmptyString) {
    return m_altCopyText;
  }

  wxString s;
  if (m_baseCell->IsCompound())
    s += wxS("(") + m_baseCell->ListToMatlab() + wxS(")");
  else
    s += m_baseCell->ListToMatlab();
  s += wxS("[") + m_indexCell->ListToMatlab() + wxS("]");
  return s;
}

wxString SubCell::ToTeX() const {
  wxString s;
  wxString base = m_baseCell->ListToTeX();
  wxString index = m_indexCell->ListToTeX();
  if (base.Length() > 1)
    s = wxS("{{") + base + wxS("}_");
  else
    s = wxS("{") + base + wxS("_");
  if (index.Length() > 1)
    s += wxS("{") + index + wxS("}}");
  else
    s += index + wxS("}");
  return s;
}

wxString SubCell::ToMathML() const {
  return wxS("<msub>") + m_baseCell->ListToMathML() +
    m_indexCell->ListToMathML() + wxS("</msub>\n");
}

wxString SubCell::ToOMML() const {
  return wxS("<m:sSub><m:e>") + m_baseCell->ListToOMML() +
    wxS("</m:e><m:sub>") + m_indexCell->ListToOMML() +
    wxS("</m:sub></m:sSub>\n");
}

wxString SubCell::ToXML() const {
  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  if (m_altCopyText != wxEmptyString)
    flags += wxS(" altCopy=\"") + XMLescape(m_altCopyText) + wxS("\"");

  return wxS("<i") + flags + wxS("><r>") + m_baseCell->ListToXML() +
    wxS("</r><r>") + m_indexCell->ListToXML() + wxS("</r></i>");
}
