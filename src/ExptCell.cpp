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

#include "ExptCell.h"

ExptCell::ExptCell() : MathCell()
{
  m_baseCell = NULL;
  m_powCell = NULL;
  m_isMatrix = false;
}

MathCell* ExptCell::Copy(bool all)
{
  ExptCell* tmp = new ExptCell;
  tmp->SetBase(m_baseCell->Copy(true));
  tmp->SetPower(m_powCell->Copy(true));
  if (all && m_nextToDraw!=NULL)
    tmp->AppendCell(m_nextToDraw->Copy(all));
  return tmp;
}

ExptCell::~ExptCell()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_powCell != NULL)
    delete m_powCell;
  if (m_next != NULL)
    delete m_next;
}

void ExptCell::Destroy()
{
  if (m_baseCell != NULL)
    delete m_baseCell;
  if (m_powCell != NULL)
    delete m_powCell;
  m_baseCell = NULL;
  m_powCell = NULL;
  m_next = NULL;
}

void ExptCell::SetPower(MathCell *power)
{
  if (power == NULL)
    return;
  if (m_powCell != NULL)
    delete m_powCell;
  m_powCell = power;
}

void ExptCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  if (m_baseCell != NULL)
    delete m_baseCell;
  m_baseCell = base;
}

void ExptCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateWidths(parser, fontsize, true);
  m_powCell->RecalculateWidths(parser, MAX(8, fontsize-3), true);
  m_width = m_baseCell->GetFullWidth(scale) + m_powCell->GetFullWidth(scale) -
            SCALE_PX(2, scale);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void ExptCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_baseCell->RecalculateSize(parser, fontsize, true);
  m_powCell->RecalculateSize(parser, MAX(8, fontsize-3), true);
  m_height = m_baseCell->GetMaxHeight() + m_powCell->GetMaxHeight() -
             SCALE_PX((13*fontsize)/10, scale);
  m_center = m_powCell->GetMaxHeight() + m_baseCell->GetMaxCenter() -
             SCALE_PX((13*fontsize)/10, scale);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void ExptCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();
    wxPoint bs, pw;
    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->Draw(parser, bs, fontsize, true);
  
    pw.x = point.x + m_baseCell->GetFullWidth(scale) - SCALE_PX(2, scale);
    pw.y = point.y - m_baseCell->GetMaxCenter() - m_powCell->GetMaxHeight()
                   + m_powCell->GetMaxCenter() +  
                     SCALE_PX((13*fontsize)/10, scale);
    m_powCell->Draw(parser, pw, MAX(8, fontsize-3), true);
  }
  
  MathCell::Draw(parser, point, fontsize, all);
}

wxString ExptCell::ToString(bool all) {
  wxString s = m_baseCell->ToString(true) + wxT("^");
  if (m_isMatrix) s += wxT("^");
  if (m_powCell->IsCompound())
    s += wxT("(") + m_powCell->ToString(true) + wxT(")");
  else
    s += m_powCell->ToString(true);
  s += MathCell::ToString(all);
  return s;
}

wxString ExptCell::GetDiffPart()
{
  wxString s(wxT(","));
  if (m_baseCell != NULL)
    s += m_baseCell->ToString(true);
  s += wxT(",");
  if (m_powCell != NULL)
    s += m_powCell->ToString(true);
  return s;
}

void ExptCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_powCell->ContainsRect(rect))
    m_powCell->SelectRect(rect, first, last);
  else if (m_baseCell->ContainsRect(rect))
    m_baseCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL) {
    *first = this;
    *last = this;
  }
}
