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

#include "LimitCell.h"

#define MIN_LIMIT_FONT_SIZE 8
#define LIMIT_FONT_SIZE_DECREASE 1

LimitCell::LimitCell() : MathCell()
{
  m_base = NULL;
  m_under = NULL;
  m_name = NULL;
}

LimitCell::~LimitCell()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_name != NULL)
    delete m_name;
  if (m_next != NULL)
    delete m_next;
}

void LimitCell::SetParent(MathCell *parent, bool all)
{
  if (m_base != NULL)
    m_base->SetParent(parent, true);
  if (m_under != NULL)
    m_under->SetParent(parent, true);
  if (m_name != NULL)
    m_name->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* LimitCell::Copy(bool all)
{
  LimitCell* tmp = new LimitCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->Copy(true));
  tmp->SetUnder(m_under->Copy(true));
  tmp->SetName(m_name->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(true));
  return tmp;
}

void LimitCell::Destroy()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_name != NULL)
    delete m_name;
  m_base = NULL;
  m_under = NULL;
  m_name = NULL;
  m_next = NULL;
}

void LimitCell::SetName(MathCell* name)
{
  if (name == NULL)
    return ;
  if (m_name != NULL)
    delete m_name;
  m_name = name;
}

void LimitCell::SetBase(MathCell* base)
{
  if (base == NULL)
    return ;
  if (m_base != NULL)
    delete m_base;
  m_base = base;
}

void LimitCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return ;
  if (m_under != NULL)
    delete m_under;
  m_under = under;
}

void LimitCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_base->RecalculateWidths(parser, fontsize, true);
  m_under->RecalculateWidths(parser, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE), true);
  m_name->RecalculateWidths(parser, fontsize, true);

  m_width = MAX(m_name->GetFullWidth(scale), m_under->GetFullWidth(scale))
            + m_base->GetFullWidth(scale);

  MathCell::RecalculateWidths(parser, fontsize, all);
}

void LimitCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  m_under->RecalculateSize(parser, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE), true);
  m_name->RecalculateSize(parser, fontsize, true);
  m_base->RecalculateSize(parser, fontsize, true);

  m_center = MAX(m_base->GetMaxCenter(), m_name->GetMaxCenter());
  m_height = m_center + MAX(m_name->GetMaxDrop() + m_under->GetMaxHeight(),
                            m_base->GetMaxDrop());

  MathCell::RecalculateSize(parser, fontsize, all);
}

void LimitCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();
    wxPoint base(point), under(point), name(point);

    name.x = point.x + MAX(m_name->GetFullWidth(scale),
                           m_under->GetFullWidth(scale)) / 2 -
             m_name->GetFullWidth(scale) / 2;
    m_name->Draw(parser, name, fontsize, true);

    under.x = point.x + MAX(m_name->GetFullWidth(scale),
                            m_under->GetFullWidth(scale)) / 2 -
              m_under->GetFullWidth(scale) / 2;
    under.y = point.y + m_name->GetMaxDrop() + m_under->GetMaxCenter();
    m_under->Draw(parser, under, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE), true);

    base.x += MAX(m_name->GetFullWidth(scale),
                  m_under->GetFullWidth(scale));
    m_base->Draw(parser, base, fontsize, true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString LimitCell::ToString(bool all)
{
  wxString s(wxT("limit"));
  wxString under = m_under->ToString(true);
  wxString base = m_base->ToString(true);
  wxString var = under.SubString(0, under.Find(wxT("->")) - 1);
  wxString to = under.SubString(under.Find(wxT("->")) + 2,
                                under.Length() - 1);
  if (to.Right(1) == wxT("+"))
    to = to.Left(to.Length() - 1) + wxT(",plus");
  if (to.Right(1) == wxT("-"))
    to = to.Left(to.Length() - 1) + wxT(",minus");

  s += wxT("(") + base + wxT(",") + var + wxT(",") + to + wxT(")");
  s += MathCell::ToString(all);
  return s;
}

wxString LimitCell::ToTeX(bool all)
{
  wxString s = wxT("\\lim");
  wxString under = m_under->ToTeX(true);
  wxString base = m_base->ToTeX(true);
  wxString var = under.SubString(0, under.Find(wxT("->")) - 1);
  wxString to = under.SubString(under.Find(wxT("->")) + 2,
                                under.Length() - 1);
  s += wxT("_{") + var + wxT("\\to ") + to + wxT("}") + base;
  s += MathCell::ToTeX(all);
  return s;
}

wxString LimitCell::ToXML(bool all)
{
  return _T("<lm><r>") + m_name->ToXML(true) + _T("</r><r>") +
    m_base->ToXML(true) + _T("</r><r>") +
    m_under->ToXML(true) + _T("</r></lm>") + 
    MathCell::ToXML(all);
}

void LimitCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  if (m_base->ContainsRect(rect))
    m_base->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
