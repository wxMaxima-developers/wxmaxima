// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "AtCell.h"

AtCell::AtCell() : MathCell()
{
  m_baseCell = NULL;
  m_indexCell = NULL;
}

AtCell::~AtCell()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_indexCell != NULL)
    delete m_indexCell;
  if (m_next != NULL)
    delete m_next;
}

void AtCell::SetParent(MathCell *parent)
{
  m_group=parent;
  if (m_baseCell != NULL)
    m_baseCell->SetParentList(parent);
  if (m_indexCell != NULL)
    m_indexCell->SetParentList(parent);
}

MathCell* AtCell::Copy()
{
  AtCell* tmp = new AtCell;
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetIndex(m_indexCell->CopyList());

  return tmp;
}

void AtCell::Destroy()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_indexCell != NULL)
    delete m_indexCell;
  m_baseCell = NULL;
  m_indexCell = NULL;
  m_next = NULL;
}


void AtCell::SetIndex(MathCell *index)
{
  if (index == NULL)
    return ;
  if (m_indexCell != NULL)
    delete m_indexCell;
  m_indexCell = index;
}

void AtCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return ;
  if (m_baseCell != NULL)
    delete m_baseCell;
  m_baseCell = base;
}

void AtCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateWidthsList(parser, fontsize);
  m_indexCell->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - 4));
  m_width = m_baseCell->GetFullWidth(scale) + m_indexCell->GetFullWidth(scale) +
            SCALE_PX(4, scale);
  ResetData();
}

void AtCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateSizeList(parser, fontsize);
  m_indexCell->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - 3));
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             SCALE_PX(7, scale);
  m_center = m_baseCell->GetCenter();
}

void AtCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point))
  {
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(parser, bs, fontsize);

    in.x = point.x + m_baseCell->GetFullWidth(scale) + SCALE_PX(4, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           + m_indexCell->GetMaxCenter() - SCALE_PX(7, scale);
    m_indexCell->DrawList(parser, in, MAX(MC_MIN_SIZE, fontsize - 3));
    SetPen(parser);
    dc.DrawLine(in.x - SCALE_PX(2, scale),
                bs.y - m_baseCell->GetMaxCenter(),
                in.x - SCALE_PX(2, scale),
                in.y);
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize);
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

wxString AtCell::ToXML()
{
  return wxT("<at><r>") + m_baseCell->ListToXML() + wxT("</r><r>") +
    m_indexCell->ListToXML() + wxT("</r></at>");
}

void AtCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  if (m_baseCell->ContainsRect(rect))
    m_baseCell->SelectRect(rect, first, last);
  else if (m_indexCell->ContainsRect(rect))
    m_indexCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
