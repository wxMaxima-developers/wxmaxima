///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "SumCell.h"
#include "TextCell.h"

SumCell::SumCell() : MathCell()
{
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_signSize = 50;
  m_signWidth = 30;
  m_signCenter = 15;
  m_sumStyle = SM_SUM;
}

SumCell::~SumCell()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  if (m_next != NULL)
    delete m_next;
}

void SumCell::SetParent(MathCell *parent, bool all)
{
  if (m_base != NULL)
    m_base->SetParent(parent, true);
  if (m_under != NULL)
    m_under->SetParent(parent, true);
  if (m_over != NULL)
    m_over->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* SumCell::Copy(bool all)
{
  SumCell *tmp = new SumCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->Copy(true));
  tmp->SetUnder(m_under->Copy(true));
  tmp->SetOver(m_over->Copy(true));
  tmp->m_sumStyle = m_sumStyle;
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void SumCell::Destroy()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  m_next = NULL;
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
}

void SumCell::SetOver(MathCell* over)
{
  if (over == NULL)
    return ;
  if (m_over != NULL)
    delete m_over;
  m_over = over;
}

void SumCell::SetBase(MathCell* base)
{
  if (base == NULL)
    return ;
  if (m_base != NULL)
    delete m_base;
  m_base = base;
}

void SumCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return ;
  if (m_under != NULL)
    delete m_under;
  m_under = under;
}

void SumCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_signSize = SCALE_PX(50, scale);
  m_signWidth = SCALE_PX(30, scale);
  m_signCenter = SCALE_PX(15, scale);

  m_base->RecalculateWidths(parser, fontsize, true);
  m_under->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  if (m_over == NULL)
    m_over = new TextCell;
  m_over->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);

  m_signCenter = MAX(m_signCenter, m_under->GetFullWidth(scale) / 2);
  m_signCenter = MAX(m_signCenter, m_over->GetFullWidth(scale) / 2);
  m_width = 2 * m_signCenter + m_base->GetFullWidth(scale) + SCALE_PX(4, scale);

  MathCell::RecalculateWidths(parser, fontsize, all);
}

void SumCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_under->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  m_over->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  m_base->RecalculateSize(parser, fontsize, true);

  m_center = MAX(m_over->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2,
                 m_base->GetMaxCenter());
  m_height = m_center +
             MAX(m_under->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2,
                 m_base->GetMaxDrop());

  MathCell::RecalculateSize(parser, fontsize, all);
}

void SumCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();

    wxPoint base(point), under(point), over(point);

    under.x += m_signCenter - m_under->GetFullWidth(scale) / 2;
    under.y = point.y + m_signSize / 2 + m_under->GetMaxCenter() + SCALE_PX(2, scale);
    m_under->Draw(parser, under, MAX(MC_MIN_SIZE, fontsize - 5), true);

    over.x += m_signCenter - m_over->GetFullWidth(scale) / 2;
    over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - SCALE_PX(2, scale);
    m_over->Draw(parser, over, MAX(MC_MIN_SIZE, fontsize - 5), true);

    SetPen(parser);
    if (m_sumStyle == SM_SUM)
    {
      //DRAW SUM SIGN
      // Upper part
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 6,
                  point.y,
                  point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2 + 1);
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2);
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2 + 1,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2 + 1);
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2 + SCALE_PX(5, scale));
      // Lower part
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 6,
                  point.y,
                  point.x + m_signCenter - m_signWidth / 2,
                  point.y + m_signSize / 2 - 1);
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y + m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y + m_signSize / 2);
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y + m_signSize / 2 - 1,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y + m_signSize / 2 - 1);
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 2,
                  point.y + m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y + m_signSize / 2 - SCALE_PX(5, scale));
    }
    else
    {
      // DRAW PRODUCT SIGN
      // Vertical lines
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 6,
                  point.y + m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 6,
                  point.y - m_signSize / 2 + SCALE_PX(4, scale));
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 6,
                  point.y + m_signSize / 2,
                  point.x + m_signCenter - m_signWidth / 6,
                  point.y - m_signSize / 2 + SCALE_PX(4, scale));
      // Horizonral line (double)
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2);
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2 + 1,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2 + 1);
      // Ticks on horizontal line
      dc.DrawLine(point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2,
                  point.x + m_signCenter - m_signWidth / 2,
                  point.y - m_signSize / 2 + SCALE_PX(5, scale));
      dc.DrawLine(point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2,
                  point.x + m_signCenter + m_signWidth / 2,
                  point.y - m_signSize / 2 + SCALE_PX(5, scale));
    }
    UnsetPen(parser);
    base.x += (2 * m_signCenter + SCALE_PX(4, scale));
    m_base->Draw(parser, base, fontsize, true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString SumCell::ToString(bool all)
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("sum(");
  else
    s = wxT("product(");
  s += m_base->ToString(true);

  MathCell* tmp = m_under;
  wxString var = tmp->ToString(false);
  wxString from;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
    tmp = tmp->m_next;
    if (tmp != NULL)
      from = tmp->ToString(true);
  }
  wxString to = m_over->ToString(true);
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
    s += wxT(",") + to + wxT(")");
  else
    s = wxT("l") + s + wxT(")"),
        s += MathCell::ToString(all);
  return s;
}

wxString SumCell::ToTeX(bool all)
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("\\sum");
  else
    s = wxT("\\prod");


  s += wxT("_{") + m_under->ToTeX(true) + wxT("}");
  wxString to = m_over->ToTeX(true);
  if (to.Length())
    s += wxT("^{") + to + wxT("}");
  s += m_base->ToTeX(true);

  s += MathCell::ToTeX(all);
  return s;
}

wxString SumCell::ToXml(bool all)
{
	return _T("<sm><r>") + m_under->ToXml(true) + _T("</r><r>") +
			m_over->ToXml(true) + _T("</r><r>") +
			m_base->ToXml(true) + _T("</r></sm>") +
			MathCell::ToXml(all);
}

void SumCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  if (m_over->ContainsRect(rect))
    m_over->SelectRect(rect, first, last);
  else if (m_under->ContainsRect(rect))
    m_under->SelectRect(rect, first, last);
  else if (m_base->ContainsRect(rect))
    m_base->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
