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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class SubCell

  SubCell is the Cell type that represents a math element with subscript.
 */

#include "SubCell.h"

#define SUB_DEC 2

SubCell::SubCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_baseCell = NULL;
  m_indexCell = NULL;
}

Cell *SubCell::Copy()
{
  SubCell *tmp = new SubCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetIndex(m_indexCell->CopyList());

  return tmp;
}

SubCell::~SubCell()
{
  wxDELETE(m_baseCell);
  wxDELETE(m_indexCell);
  m_baseCell = NULL;
  m_indexCell = NULL;
  MarkAsDeleted();
}

std::list<Cell *> SubCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_indexCell)
    innerCells.push_back(m_indexCell);
  return innerCells;
}


void SubCell::SetIndex(Cell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_indexCell);
  m_indexCell = index;
}

void SubCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
}

void SubCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  m_baseCell->RecalculateWidthsList(fontsize);
  m_indexCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_width = m_baseCell->GetFullWidth() + m_indexCell->GetFullWidth() -
            Scale_Px(2);
  ResetData();
}

void SubCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_baseCell->RecalculateHeightList(fontsize);
  m_indexCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             Scale_Px((8 * fontsize) / 10 + MC_EXP_INDENT);
  m_center = m_baseCell->GetCenter();
}

void SubCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs);

    in.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(2);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           m_indexCell->GetMaxCenter() -
           Scale_Px((8 * m_fontSize) / 10 + MC_EXP_INDENT);
    m_indexCell->DrawList(in);
  }
}

wxString SubCell::ToString()
{
  if (m_altCopyText != wxEmptyString)
  {
    return m_altCopyText;
  }

  wxString s;
  if (m_baseCell->IsCompound())
    s += wxT("(") + m_baseCell->ListToString() + wxT(")");
  else
    s += m_baseCell->ListToString();
  s += wxT("[") + m_indexCell->ListToString() + wxT("]");
  return s;
}

wxString SubCell::ToMatlab()
{
  if (m_altCopyText != wxEmptyString)
  {
	return m_altCopyText;
  }

  wxString s;
  if (m_baseCell->IsCompound())
	s += wxT("(") + m_baseCell->ListToMatlab() + wxT(")");
  else
	s += m_baseCell->ListToMatlab();
  s += wxT("[") + m_indexCell->ListToMatlab() + wxT("]");
  return s;
}

wxString SubCell::ToTeX()
{
  wxString s;
  wxString base = m_baseCell->ListToTeX();
  wxString index = m_indexCell->ListToTeX();
  if (base.Length() > 1)
    s = wxT("{{") + base + wxT("}_");
  else
    s = wxT("{") + base + wxT("_");
  if (index.Length() > 1)
    s += wxT("{") + index + wxT("}}");
  else
    s += index + wxT("}");
  return s;
}

wxString SubCell::ToMathML()
{
  return wxT("<msub>") +
         m_baseCell->ListToMathML() +
         m_indexCell->ListToMathML() +
         wxT("</msub>\n");
}

wxString SubCell::ToOMML()
{
  return wxT("<m:sSub><m:e>") + m_baseCell->ListToOMML() + wxT("</m:e><m:sub>") +
         m_indexCell->ListToOMML() + wxT("</m:sub></m:sSub>\n");
}

wxString SubCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  if (m_altCopyText != wxEmptyString)
    flags += wxT(" altCopy=\"") + XMLescape(m_altCopyText) + wxT("\"");
  
  return wxT("<i") + flags + wxT("><r>") + m_baseCell->ListToXML() + wxT("</r><r>") +
           m_indexCell->ListToXML() + wxT("</r></i>");
}
