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

#include "FracCell.h"

FracCell::FracCell() : MathCell()
{
  m_num = NULL;
  m_denom = NULL;
  m_fracStyle = FC_NORMAL;
}

MathCell* FracCell::Copy(bool all)
{
  FracCell* tmp = new FracCell;
  tmp->SetNum(m_num->Copy(true));
  tmp->SetDenom(m_denom->Copy(true));
  tmp->m_fracStyle = m_fracStyle;
  if (all && m_nextToDraw!=NULL)
    tmp->AppendCell(m_nextToDraw->Copy(all));
  return tmp;
}

FracCell::~FracCell()
{
  if (m_num != NULL)
    delete m_num;
  if (m_denom != NULL)
    delete m_denom;
  if (m_next != NULL)
    delete m_next;
}

void FracCell::Destroy()
{
  if (m_num != NULL)
    delete m_num;
  if (m_denom != NULL)
    delete m_denom;
  m_num = NULL;
  m_denom = NULL;
  m_next = NULL;
}

void FracCell::SetNum(MathCell *num)
{
  if (num == NULL)
    return;
  if (m_num != NULL)
    delete m_num;
  m_num = num;
}

void FracCell::SetDenom(MathCell *denom)
{
  if (denom == NULL)
    return;
  if (m_denom != NULL)
    delete m_denom;
  m_denom = denom;
}

void FracCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_num->RecalculateWidths(parser, MAX(8, fontsize-2), true);
  m_denom->RecalculateWidths(parser, MAX(8, fontsize-2), true);
  m_width = MAX(m_num->GetFullWidth(scale), m_denom->GetFullWidth(scale));
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void FracCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_num->RecalculateSize(parser, MAX(8, fontsize-2), true);
  m_denom->RecalculateSize(parser, MAX(8, fontsize-2), true);
  m_height = m_num->GetMaxHeight() + m_denom->GetMaxHeight() +
             SCALE_PX(4, scale);
  m_center = m_num->GetMaxHeight() + SCALE_PX(2, scale);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void FracCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point)) {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    wxPoint num, denom;
    SetPen(parser);
    if (m_fracStyle != FC_CHOOSE)
      dc.DrawLine(point.x, point.y, point.x + m_width, point.y);
    UnsetPen(parser);
      
    num.x = point.x + (m_width - m_num->GetFullWidth(scale))/2;
    num.y = point.y - m_num->GetMaxHeight() + m_num->GetMaxCenter() - 
            SCALE_PX(2, scale);
    m_num->Draw(parser, num, MAX(8, fontsize-2), true);
  
    denom.x = point.x + (m_width - m_denom->GetFullWidth(scale))/2;
    denom.y = point.y + m_denom->GetMaxCenter() + SCALE_PX(2, scale);
    m_denom->Draw(parser, denom, MAX(8, fontsize-2), true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString FracCell::ToString(bool all)
{
  wxString s;
  if (m_fracStyle == FC_NORMAL) {
    if (m_num->IsCompound())
      s += wxT("(") + m_num->ToString(true) + wxT(")/");
    else
      s += m_num->ToString(true) + wxT("/");
    if (m_denom->IsCompound())
      s += wxT("(") + m_denom->ToString(true) + wxT(")");
    else
      s += m_denom->ToString(true);
    s += MathCell::ToString(all);
  }
  else if (m_fracStyle == FC_CHOOSE) {
    s = wxT("binom(") + m_num->ToString(true) + wxT(",") +
        m_denom->ToString(true) + wxT(")");
    s += MathCell::ToString(all);
  }
  else {
    MathCell* tmp = m_denom;
    while (tmp != NULL) {
      tmp = tmp->m_next;   // Skip the d
      if (tmp == NULL)
        break;
      tmp = tmp->m_next;   // Skip the *
      if (tmp == NULL)
        break;
      s += tmp->GetDiffPart();
      tmp = tmp->m_next;   // Skip the *
      if (tmp == NULL)
        break;
      tmp = tmp->m_next;
    }
  }
  return s;
}

void FracCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_num->ContainsRect(rect))
    m_num->SelectRect(rect, first, last);
  else if (m_denom->ContainsRect(rect))
    m_denom->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL) {
    *first = this;
    *last = this;
  }
}
