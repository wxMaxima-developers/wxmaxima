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

#include "ParenCell.h"
#include "TextCell.h"

ParenCell::ParenCell() : MathCell()
{
  m_innerCell = NULL;
  m_print = true;
  m_open = new TextCell(wxT("("));
  m_close = new TextCell(wxT(")"));
}


ParenCell::~ParenCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
  delete m_open;
  delete m_close;
}

MathCell* ParenCell::Copy(bool all)
{
  ParenCell *tmp = new ParenCell;
  tmp->SetInner(m_innerCell->Copy(true), m_style);
  tmp->m_style = m_style;
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void ParenCell::Destroy()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = NULL;
  m_next = NULL;
}

void ParenCell::SetInner(MathCell *inner, int style)
{
  if (inner == NULL)
    return;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;
  m_style = style;
  
  
  m_open->m_nextToDraw = m_innerCell;
  m_open->m_previousToDraw = this;
  while (inner->m_next != NULL)
    inner = inner->m_next;
  m_last1 = inner;
}

void ParenCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  if (m_innerCell == NULL)
    m_innerCell = new TextCell;
  
  m_innerCell->RecalculateWidths(parser, fontsize, true);
  m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(12, parser.GetScale());
  
  m_open->RecalculateWidths(parser, fontsize, false);
  m_close->RecalculateWidths(parser, fontsize, false);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void ParenCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSize(parser, fontsize, true);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(2, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(1, scale);
  
  m_open->RecalculateSize(parser, fontsize, false);
  m_close->RecalculateSize(parser, fontsize, false);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void ParenCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point)) {
    double scale = parser.GetScale();
    wxDC& dc = parser.GetDC();
    wxPoint in;
    in.x = point.x + SCALE_PX(6, scale);
    in.y = point.y;
    m_innerCell->Draw(parser, in, fontsize, true);
  
    SetPen(parser);
    // left
    dc.DrawLine(point.x + SCALE_PX(5, scale),
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                point.x + SCALE_PX(2, scale),
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(5, scale));
    dc.DrawLine(point.x + SCALE_PX(2, scale),
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(5, scale),
                point.x + SCALE_PX(2, scale),
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(5, scale));
    dc.DrawLine(point.x + SCALE_PX(2, scale),
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(5, scale),
                point.x + SCALE_PX(5, scale),
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
    // right
    dc.DrawLine(point.x + m_width - SCALE_PX(5, scale) - 1,
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(5, scale));
    dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y - m_innerCell->GetMaxCenter() + SCALE_PX(5, scale),
                point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(5, scale));
    dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(5, scale),
                point.x + m_width - SCALE_PX(5, scale) - 1,
                point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString ParenCell::ToString(bool all)
{
  wxString s;
  if (!m_isBroken) {
    if (m_print)
      s = wxT("(") + m_innerCell->ToString(true) + wxT(")");
    else
      s = m_innerCell->ToString(true);
  }
  s +=  MathCell::ToString(all);
  return s;
}

void ParenCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
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

bool ParenCell::BreakUp(bool br)
{
  if (!m_isBroken) {
    m_isBroken = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    m_last1->m_nextToDraw = m_close;
    m_close->m_previousToDraw = m_last1;
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    return true;
  }
  return false;
}

void ParenCell::Unbreak(bool all)
{
  if (m_isBroken)
    m_innerCell->Unbreak(true);
  MathCell::Unbreak(all);
}
