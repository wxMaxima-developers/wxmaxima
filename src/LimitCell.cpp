// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

void LimitCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_base != NULL)
    m_base->SetParentList(parent);
  if (m_under != NULL)
    m_under->SetParentList(parent);
  if (m_name != NULL)
    m_name->SetParentList(parent);
}

MathCell* LimitCell::Copy()
{
  LimitCell* tmp = new LimitCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetName(m_name->CopyList());
    
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

void LimitCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();

  m_base->RecalculateWidthsList(parser, fontsize);
  m_under->RecalculateWidthsList(parser, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  m_name->RecalculateWidthsList(parser, fontsize);

  m_width = MAX(m_name->GetFullWidth(scale), m_under->GetFullWidth(scale))
            + m_base->GetFullWidth(scale);
  ResetData();
}

void LimitCell::RecalculateSize(CellParser& parser, int fontsize)
{
  m_under->RecalculateSizeList(parser, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  m_name->RecalculateSizeList(parser, fontsize);
  m_base->RecalculateSizeList(parser, fontsize);

  m_center = MAX(m_base->GetMaxCenter(), m_name->GetMaxCenter());
  m_height = m_center + MAX(m_name->GetMaxDrop() + m_under->GetMaxHeight(),
                            m_base->GetMaxDrop());
}

void LimitCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();
    wxPoint base(point), under(point), name(point);

    name.x = point.x + MAX(m_name->GetFullWidth(scale),
                           m_under->GetFullWidth(scale)) / 2 -
             m_name->GetFullWidth(scale) / 2;
    m_name->DrawList(parser, name, fontsize);

    under.x = point.x + MAX(m_name->GetFullWidth(scale),
                            m_under->GetFullWidth(scale)) / 2 -
              m_under->GetFullWidth(scale) / 2;
    under.y = point.y + m_name->GetMaxDrop() + m_under->GetMaxCenter();
    m_under->DrawList(parser, under, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));

    base.x += MAX(m_name->GetFullWidth(scale),
                  m_under->GetFullWidth(scale));
    m_base->DrawList(parser, base, fontsize);
  }

  MathCell::Draw(parser, point, fontsize);
}

wxString LimitCell::ToString()
{
  wxString s(wxT("limit"));
  wxString under = m_under->ListToString();
  wxString base = m_base->ListToString();
  wxString var = under.SubString(0, under.Find(wxT("->")) - 1);
  wxString to = under.SubString(under.Find(wxT("->")) + 2,
                                under.Length() - 1);
  if (to.Right(1) == wxT("+"))
    to = to.Left(to.Length() - 1) + wxT(",plus");
  if (to.Right(1) == wxT("-"))
    to = to.Left(to.Length() - 1) + wxT(",minus");

  s += wxT("(") + base + wxT(",") + var + wxT(",") + to + wxT(")");
  return s;
}

wxString LimitCell::ToTeX()
{
  wxString under = m_under->ListToTeX();
  wxString base  = m_base ->ListToTeX();
  wxString var = under.SubString(0, under.Find(wxT("->")) - 1);
  wxString to = under.SubString(under.Find(wxT("->")) + 2,
                                under.Length() - 1);
  wxString s = wxT("\\lim_{")+var+wxT("\\to ")+to+wxT("}{")+base+wxT("}");
  return s;
}

wxString LimitCell::ToMathML()
{
  wxString base = m_base->ListToMathML();

  wxString from;
  if(m_under) from = m_under->ListToMathML();
  
  wxString retval;
  if(from.IsEmpty())
    retval = wxT("<mo>lim</mo>") + base;
  else
    retval = wxT("<munder><mo>lim</mo>") + from + wxT("</munder>");
  return(retval);
}

wxString LimitCell::ToXML()
{
  return _T("<lm><r>") + m_name->ListToXML() + _T("</r><r>") +
    m_under->ListToXML() + _T("</r><r>") +
    m_base->ListToXML() + _T("</r></lm>");
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
