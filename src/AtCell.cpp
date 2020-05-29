// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#include "AtCell.h"
#include "TextCell.h"

AtCell::AtCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
  Cell(parent, config, cellPointers),
  m_baseCell (std::make_shared<TextCell>(parent, config, cellPointers)),
  m_indexCell(std::make_shared<TextCell>(parent, config, cellPointers))
{
  m_nextToDraw = NULL;
}

AtCell::AtCell(const AtCell &cell):
 AtCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
  if(cell.m_indexCell)
    SetIndex(cell.m_indexCell->CopyList());
}

AtCell::~AtCell()
{
  MarkAsDeleted();
}

void AtCell::SetIndex(Cell *index)
{
  if (!index)
    return;
  m_indexCell = std::shared_ptr<Cell>(index);
}

void AtCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_baseCell = std::shared_ptr<Cell>(base);
}

void AtCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateWidthsList(fontsize);
  m_indexCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - 3));
  m_width = m_baseCell->GetFullWidth() + m_indexCell->GetFullWidth() +
            Scale_Px(4);
  Cell::RecalculateWidths(fontsize);
}

void AtCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateHeightList(fontsize);
  m_indexCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - 3));
  m_height = m_baseCell->GetHeightList() + m_indexCell->GetHeightList() -
             Scale_Px(7);
  m_center = m_baseCell->GetCenter();
  Cell::RecalculateHeight(fontsize);
}

void AtCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {
    
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs);

    in.x = point.x + m_baseCell->GetFullWidth() + Scale_Px(4);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           +m_indexCell->GetCenterList() - Scale_Px(7);
    m_indexCell->DrawList(in);
    SetPen();
    dc->DrawLine(in.x - Scale_Px(2),
                bs.y - m_baseCell->GetCenterList(),
                in.x - Scale_Px(2),
                in.y);
    UnsetPen();
  }
}

wxString AtCell::ToString()
{
  wxString s = wxT("at(");
  s += m_baseCell->ListToString();
  s += wxT(",") + m_indexCell->ListToString() + wxT(")");
  return s;
}

wxString AtCell::ToMatlab()
{
  wxString s = wxT("at(");
  s += m_baseCell->ListToMatlab();
  s += wxT(",") + m_indexCell->ListToMatlab() + wxT(")");
  return s;
}

wxString AtCell::ToTeX()
{
  wxString s = wxT("\\left. ");
  s += m_baseCell->ListToTeX();
  s += wxT("\\right|_{") + m_indexCell->ListToTeX() + wxT("}");
  return s;
}

wxString AtCell::ToMathML()
{
  return wxT("<msub>") + m_baseCell->ListToMathML() +
         m_indexCell->ListToMathML() + wxT("</msub>\n");
}

wxString AtCell::ToOMML()
{
  return wxT("<m:sSub><m:e>") + m_baseCell->ListToOMML() + wxT("</m:e><m:sub>") +
         m_indexCell->ListToOMML() + wxT("</m:sub></m:sSub>\n");
}


wxString AtCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  
  return wxT("<at") + flags + wxT("><r>") + m_baseCell->ListToXML() + wxT("</r><r>") +
         m_indexCell->ListToXML() + wxT("</r></at>");
}

