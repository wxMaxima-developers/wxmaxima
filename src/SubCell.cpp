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

#include "SubCell.h"

#define SUB_DEC 2

SubCell::SubCell() : MathCell()
{
  m_baseCell = NULL;
  m_indexCell = NULL;
}

SubCell::~SubCell()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_indexCell != NULL)
    delete m_indexCell;
  if (m_next != NULL)
    delete m_next;
}

void SubCell::SetParent(MathCell *parent)
{
  m_group=parent;
  if (m_baseCell != NULL)
    m_baseCell->SetParentList(parent);
  if (m_indexCell != NULL)
    m_indexCell->SetParentList(parent);
}

MathCell* SubCell::Copy()
{
  SubCell* tmp = new SubCell;
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetIndex(m_indexCell->CopyList());

  return tmp;
}

void SubCell::Destroy()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_indexCell != NULL)
    delete m_indexCell;
  m_baseCell = NULL;
  m_indexCell = NULL;
  m_next = NULL;
}

void SubCell::SetIndex(MathCell *index)
{
  if (index == NULL)
    return ;
  if (m_indexCell != NULL)
    delete m_indexCell;
  m_indexCell = index;
}

void SubCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return ;
  if (m_baseCell != NULL)
    delete m_baseCell;
  m_baseCell = base;
}

void SubCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateWidthsList(parser, fontsize);
  m_indexCell->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_width = m_baseCell->GetFullWidth(scale) + m_indexCell->GetFullWidth(scale) -
            SCALE_PX(2, parser.GetScale());
  ResetData();
}

void SubCell::RecalculateSize(CellParser& parser, int fontsize)
{
  m_baseCell->RecalculateSizeList(parser, fontsize);
  m_indexCell->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - SUB_DEC));
  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() -
             SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, parser.GetScale());
  m_center = m_baseCell->GetCenter();
}

void SubCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->Draw(parser, bs, fontsize, true);

    in.x = point.x + m_baseCell->GetFullWidth(scale) - SCALE_PX(2, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           m_indexCell->GetMaxCenter() -
           SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, scale);
    m_indexCell->Draw(parser, in, MAX(MC_MIN_SIZE, fontsize - SUB_DEC), true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString SubCell::ToString()
{
  if (m_altCopyText != wxEmptyString) {
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
  wxString s = wxT("{") + m_baseCell->ListToTeX() + wxT("}_{") +
               m_indexCell->ListToTeX() + wxT("}");
  return s;
}

wxString SubCell::ToXML()
{
  return _T("<i><r>") + m_baseCell->ListToXML() + _T("</r><r>") +
    m_indexCell->ListToXML() + _T("</r></i>");
}

void SubCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
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
