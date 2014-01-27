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

#include "DiffCell.h"

DiffCell::DiffCell() : MathCell()
{
  m_baseCell = NULL;
  m_diffCell = NULL;
}

DiffCell::~DiffCell()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_diffCell != NULL)
    delete m_diffCell;
  if (m_next != NULL)
    delete m_next;
}

void DiffCell::SetParent(MathCell *parent, bool all)
{
  if (m_baseCell != NULL)
    m_baseCell->SetParent(parent, true);
  if (m_diffCell != NULL)
    m_diffCell->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* DiffCell::Copy(bool all)
{
  DiffCell* tmp = new DiffCell;
  CopyData(this, tmp);
  tmp->SetDiff(m_diffCell->Copy(true));
  tmp->SetBase(m_baseCell->Copy(true));
  if (all && m_next!= NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void DiffCell::Destroy()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_diffCell != NULL)
    delete m_diffCell;
  m_baseCell = NULL;
  m_diffCell = NULL;
  m_next = NULL;
}

void DiffCell::SetDiff(MathCell *diff)
{
  if (diff == NULL)
    return;
  if (m_diffCell != NULL)
    delete m_diffCell;
  m_diffCell = diff;
}

void DiffCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  if (m_baseCell != NULL)
    delete m_baseCell;
  m_baseCell = base;
}

void DiffCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateWidths(parser, fontsize, true);
  m_diffCell->RecalculateWidths(parser, fontsize, true);
  m_width = m_baseCell->GetFullWidth(scale) + m_diffCell->GetFullWidth(scale) + 2*MC_CELL_SKIP;
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void DiffCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  m_baseCell->RecalculateSize(parser, fontsize, true);
  m_diffCell->RecalculateSize(parser, fontsize, true);
  m_center = MAX(m_diffCell->GetMaxCenter(), m_baseCell->GetMaxCenter());
  m_height = m_center + MAX(m_diffCell->GetMaxDrop(), m_baseCell->GetMaxDrop());
  MathCell::RecalculateSize(parser, fontsize, all);
}

void DiffCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point)) {
    wxPoint bs, df;
    df.x = point.x;
    df.y = point.y;
    m_diffCell->Draw(parser, df, fontsize, true);

    bs.x = point.x + m_diffCell->GetFullWidth(parser.GetScale()) + 2*MC_CELL_SKIP;
    bs.y = point.y;
    m_baseCell->Draw(parser, bs, fontsize, true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString DiffCell::ToString(bool all)
{
  MathCell* tmp = m_baseCell->m_next;
  wxString s = wxT("'diff(");
  if (tmp != NULL)
    s += tmp->ToString(true);
  s += m_diffCell->ToString(true);
  s += wxT(")");
  s += MathCell::ToString(all);
  return s;
}

wxString DiffCell::ToTeX(bool all)
{
  wxString s = m_diffCell->ToTeX(true) + m_baseCell->ToTeX(true);
  s += MathCell::ToTeX(all);
  return s;
}

wxString DiffCell::ToXML(bool all)
{
  return _T("<d>") + m_baseCell->ToXML(true) +
    m_diffCell->ToXML(true) + _T("</d>") +
    MathCell::ToXML(all);
}

void DiffCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  if (m_baseCell->ContainsRect(rect))
    m_baseCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL) {
    *first = this;
    *last = this;
  }
}
