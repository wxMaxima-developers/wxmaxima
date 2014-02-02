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

void AtCell::SetParent(MathCell *parent, bool all)
{
  if (m_baseCell != NULL)
    m_baseCell->SetParent(parent, true);
  if (m_indexCell != NULL)
    m_indexCell->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* AtCell::Copy(bool all)
{
  AtCell* tmp = new AtCell;
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->Copy(true));
  tmp->SetIndex(m_indexCell->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
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

void AtCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateWidths(parser, fontsize, true);
  m_indexCell->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 4), true);
  m_width = m_baseCell->GetFullWidth(scale) + m_indexCell->GetFullWidth(scale) +
            SCALE_PX(4, scale);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void AtCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateSize(parser, fontsize, true);
  m_indexCell->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 3), true);
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             SCALE_PX(7, scale);
  m_center = m_baseCell->GetCenter();
  MathCell::RecalculateSize(parser, fontsize, all);
}

void AtCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point))
  {
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->Draw(parser, bs, fontsize, true);

    in.x = point.x + m_baseCell->GetFullWidth(scale) + SCALE_PX(4, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           + m_indexCell->GetMaxCenter() - SCALE_PX(7, scale);
    m_indexCell->Draw(parser, in, MAX(MC_MIN_SIZE, fontsize - 3), true);
    SetPen(parser);
    dc.DrawLine(in.x - SCALE_PX(2, scale),
                bs.y - m_baseCell->GetMaxCenter(),
                in.x - SCALE_PX(2, scale),
                in.y);
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString AtCell::ToString(bool all)
{
  wxString s = wxT("at(");
  s += m_baseCell->ToString(true);
  s += wxT(",") + m_indexCell->ToString(true) + wxT(")");
  s += MathCell::ToString(all);
  return s;
}

wxString AtCell::ToTeX(bool all)
{
  wxString s = wxT("\\left. ");
  s += m_baseCell->ToTeX(true);
  s += wxT("\\right|_{") + m_indexCell->ToTeX(true) + wxT("}");
  s += MathCell::ToTeX(all);
  return s;
}

wxString AtCell::ToXML(bool all)
{
  return wxT("<at><r>") + m_baseCell->ToXML(true) + wxT("</r><r>") +
    m_indexCell->ToXML(true) + wxT("</r></at>") +
    MathCell::ToXML(all);
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
