/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "AbsCell.h"

AbsCell::AbsCell() : MathCell()
{
  m_innerCell = NULL;
}

AbsCell::~AbsCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
}

MathCell* AbsCell::Copy(bool all)
{
  AbsCell* tmp = new AbsCell;
  tmp->SetInner(m_innerCell->Copy(true));
  tmp->m_style = m_style;
  if (all && m_nextToDraw!=NULL)
    tmp->AppendCell(m_nextToDraw->Copy(all));
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
    return;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;
}

void AbsCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateWidths(parser, fontsize, true);
  m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(8, scale);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void AbsCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSize(parser, fontsize, true);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(4, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(2, scale);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void AbsCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point)) {
    SetPen(parser);
    wxPoint in;
    in.x = point.x + SCALE_PX(4, scale);
    in.y = point.y;
    m_innerCell->Draw(parser, in, fontsize, true);
  
    dc.DrawLine(point.x + SCALE_PX(2, scale),
                point.y - m_center + SCALE_PX(2, scale),
                point.x + SCALE_PX(2, scale),
                point.y - m_center + m_height - SCALE_PX(2, scale));
    dc.DrawLine(point.x + m_width - SCALE_PX(2, scale),
                point.y - m_center + SCALE_PX(2, scale),
                point.x + m_width - SCALE_PX(2, scale),
                point.y - m_center + m_height - SCALE_PX(2, scale));
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString AbsCell::ToString(bool all)
{
  return wxT("abs(") + m_innerCell->ToString(true) + wxT(")") +
         MathCell::ToString(all);
}

void AbsCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  
  if (m_innerCell->ContainsRect(rect))
    m_innerCell->SelectRect(rect, first, last);
  
  if (*first == NULL || *last == NULL) {
    *first = this;
    *last = this;
  }
}
