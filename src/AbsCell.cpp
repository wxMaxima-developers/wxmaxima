//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "AbsCell.h"
#include "TextCell.h"

AbsCell::AbsCell() : MathCell()
{
  m_innerCell = NULL;
  m_open = new TextCell(wxT("abs("));
  m_close = new TextCell(wxT(")"));
}

AbsCell::~AbsCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
  delete m_open;
  delete m_close;
}

void AbsCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_innerCell != NULL)
    m_innerCell->SetParentList(parent);
  if (m_open != NULL)
    m_open->SetParentList(parent);
  if (m_close != NULL)
    m_close->SetParentList(parent);

  MathCell::SetParentList(parent);
}

MathCell* AbsCell::Copy()
{
  AbsCell* tmp = new AbsCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList());

  return tmp;
}

void AbsCell::Destroy()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = NULL;
  m_next = NULL;
}

void AbsCell::SetInner(MathCell *inner)
{
  if (inner == NULL)
    return ;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;

  m_last = m_innerCell;
  while (m_last->m_next != NULL)
    m_last = m_last->m_next;
}

void AbsCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateWidthsList(parser, fontsize);
  m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(8, scale);
  m_open->RecalculateWidthsList(parser, fontsize);
  m_close->RecalculateWidthsList(parser, fontsize);
  ResetData();
}

void AbsCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSizeList(parser, fontsize);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(4, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(2, scale);
  m_open->RecalculateSizeList(parser, fontsize);
  m_close->RecalculateSizeList(parser, fontsize);
}

void AbsCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point))
  {
    SetPen(parser);
    wxPoint in;
    in.x = point.x + SCALE_PX(4, scale);
    in.y = point.y;
    m_innerCell->DrawList(parser, in, fontsize);

    dc.DrawLine(point.x + SCALE_PX(2, scale),
                point.y - m_center + SCALE_PX(2, scale),
                point.x + SCALE_PX(2, scale),
                point.y - m_center + m_height - SCALE_PX(2, scale));
    dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y - m_center + SCALE_PX(2, scale),
                point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y - m_center + m_height - SCALE_PX(2, scale));
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize);
}

wxString AbsCell::ToString()
{
  return wxT("abs(") + m_innerCell->ListToString() + wxT(")");
}

wxString AbsCell::ToTeX()
{
  return wxT("\\left| ") + m_innerCell->ListToTeX() + wxT("\\right| ");
}

wxString AbsCell::ToXML()
{
  return wxT("<a>") + m_innerCell->ListToXML() + wxT("</a>");
}

void AbsCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_innerCell->ContainsRect(rect))
    m_innerCell->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool AbsCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    m_last->m_nextToDraw = m_close;
    m_close->m_previousToDraw = m_last;
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    return true;
  }
  return false;
}

void AbsCell::Unbreak(bool all)
{
  if (m_isBroken)
    m_innerCell->Unbreak(true);
  MathCell::Unbreak(all);
}
