///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

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

void AbsCell::SetParent(MathCell *parent, bool all)
{
  if (m_innerCell != NULL)
    m_innerCell->SetParent(parent, true);
  if (m_open != NULL)
    m_open->SetParent(parent, true);
  if (m_close != NULL)
    m_close->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* AbsCell::Copy(bool all)
{
  AbsCell* tmp = new AbsCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
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

void AbsCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateWidths(parser, fontsize, true);
  m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(8, scale);
  m_open->RecalculateWidths(parser, fontsize, true);
  m_close->RecalculateWidths(parser, fontsize, true);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void AbsCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSize(parser, fontsize, true);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(4, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(2, scale);
  m_open->RecalculateSize(parser, fontsize, true);
  m_close->RecalculateSize(parser, fontsize, true);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void AbsCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point))
  {
    SetPen(parser);
    wxPoint in;
    in.x = point.x + SCALE_PX(4, scale);
    in.y = point.y;
    m_innerCell->Draw(parser, in, fontsize, true);

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
  MathCell::Draw(parser, point, fontsize, all);
}

wxString AbsCell::ToString(bool all)
{
  return wxT("abs(") + m_innerCell->ToString(true) + wxT(")") +
         MathCell::ToString(all);
}

wxString AbsCell::ToTeX(bool all)
{
  return wxT("\\left| ") + m_innerCell->ToTeX(true) + wxT("\\right| ") +
    MathCell::ToTeX(all);
}

wxString AbsCell::ToXML(bool all)
{
  return wxT("<a>") + m_innerCell->ToXML(true) + wxT("</a>") + 
    MathCell::ToXML(all);
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
