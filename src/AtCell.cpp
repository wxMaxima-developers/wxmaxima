// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file
  This file defines the class AtCell

  AtCell is the MathCell type that represents maxima's at() command.
*/

#include "AtCell.h"

AtCell::AtCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_cellPointers = cellPointers;
  m_baseCell = NULL;
  m_indexCell = NULL;
}

void AtCell::SetGroup(MathCell *parent)
{
  m_group = parent;
  if (m_baseCell != NULL)
    m_baseCell->SetGroupList(parent);
  if (m_indexCell != NULL)
    m_indexCell->SetGroupList(parent);
}

MathCell *AtCell::Copy()
{
  AtCell *tmp = new AtCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetIndex(m_indexCell->CopyList());

  return tmp;
}

AtCell::~AtCell()
{
  wxDELETE(m_baseCell);
  wxDELETE(m_indexCell);
  m_baseCell = m_indexCell = NULL;
  MarkAsDeleted();
}

std::list<MathCell *> AtCell::GetInnerCells()
{
  std::list<MathCell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_indexCell)
    innerCells.push_back(m_indexCell);
  return innerCells;
}

void AtCell::SetIndex(MathCell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_indexCell);
  m_indexCell = index;
}

void AtCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
}

void AtCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_baseCell->RecalculateWidthsList(fontsize);
  m_indexCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - 4));
  m_width = m_baseCell->GetFullWidth(scale) + m_indexCell->GetFullWidth(scale) +
            Scale_Px(4, scale);
  ResetData();
}

void AtCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_baseCell->RecalculateHeightList(fontsize);
  m_indexCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - 3));
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             Scale_Px(7, scale);
  m_center = m_baseCell->GetCenter();
}

void AtCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    MathCell::Draw(point, fontsize);
    
    Configuration *configuration = (*m_configuration);
    double scale = configuration->GetScale();
    wxDC *dc = configuration->GetDC();
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs, fontsize);

    in.x = point.x + m_baseCell->GetFullWidth(scale) + Scale_Px(4, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           +m_indexCell->GetMaxCenter() - Scale_Px(7, scale);
    m_indexCell->DrawList(in, MAX(MC_MIN_SIZE, fontsize - 3));
    SetPen();
    dc->DrawLine(in.x - Scale_Px(2, scale),
                bs.y - m_baseCell->GetMaxCenter(),
                in.x - Scale_Px(2, scale),
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
