// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class SubCell

  SubCell is the MathCell type that represents a math element with subscript.
 */

#include "SubCell.h"

#define SUB_DEC 2

SubCell::SubCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_cellPointers = cellPointers;
  m_baseCell = NULL;
  m_indexCell = NULL;
}

void SubCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_baseCell != NULL)
    m_baseCell->SetParentList(parent);
  if (m_indexCell != NULL)
    m_indexCell->SetParentList(parent);
}

MathCell *SubCell::Copy()
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

void SubCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_baseCell, m_indexCell);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}


void SubCell::SetIndex(MathCell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_indexCell);
  m_indexCell = index;
}

void SubCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
}

void SubCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_baseCell->RecalculateWidthsList(fontsize);
  m_indexCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_width = m_baseCell->GetFullWidth(scale) + m_indexCell->GetFullWidth(scale) -
            SCALE_PX(2, configuration->GetScale());
  ResetData();
}

void SubCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  m_baseCell->RecalculateHeightList(fontsize);
  m_indexCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, configuration->GetScale());
  m_center = m_baseCell->GetCenter();
}

void SubCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    MathCell::Draw(point, fontsize);
    Configuration *configuration = (*m_configuration);
    double scale = configuration->GetScale();
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs, fontsize);

    in.x = point.x + m_baseCell->GetFullWidth(scale) - SCALE_PX(2, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           m_indexCell->GetMaxCenter() -
           SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, scale);
    m_indexCell->DrawList(in, MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
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
  if (m_altCopyText == wxEmptyString)
  {
    return _T("<i><r>") + m_baseCell->ListToXML() + _T("</r><r>") +
           m_indexCell->ListToXML() + _T("</r></i>");
  }
  return _T("<i altCopy=\"" + m_altCopyText + "\"><r>") + m_baseCell->ListToXML() + _T("</r><r>") +
         m_indexCell->ListToXML() + _T("</r></i>");
}

void SubCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_indexCell->ContainsRect(rect))
    m_indexCell->SelectRect(rect, first, last);
  else if (m_baseCell->ContainsRect(rect))
    m_baseCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
