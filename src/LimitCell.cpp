// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class LimitCell

  LimitCell is the MathCell type that represents maxima's <code>limit()</code> command.
*/

#include "LimitCell.h"

#define MIN_LIMIT_FONT_SIZE 8
#define LIMIT_FONT_SIZE_DECREASE 1

LimitCell::LimitCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_base = NULL;
  m_under = NULL;
  m_name = NULL;
  m_cellPointers = cellPointers;
}

void LimitCell::SetGroup(MathCell *parent)
{
  m_group = parent;
  if (m_base != NULL)
    m_base->SetGroupList(parent);
  if (m_under != NULL)
    m_under->SetGroupList(parent);
  if (m_name != NULL)
    m_name->SetGroupList(parent);
}

MathCell *LimitCell::Copy()
{
  LimitCell *tmp = new LimitCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetName(m_name->CopyList());

  return tmp;
}

LimitCell::~LimitCell()
{
  wxDELETE(m_base);
  wxDELETE(m_under);
  wxDELETE(m_name);
  m_base = m_under = m_name = NULL;
  MarkAsDeleted();
}

std::list<MathCell *> LimitCell::GetInnerCells()
{
  std::list<MathCell *> innerCells;
  if(m_base)
    innerCells.push_back(m_base);
  if(m_under)
    innerCells.push_back(m_under);
  if(m_name)
    innerCells.push_back(m_name);
  return innerCells;
}

void LimitCell::SetName(MathCell *name)
{
  if (name == NULL)
    return;
  wxDELETE(m_name);
  m_name = name;
}

void LimitCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_base);
  m_base = base;
}

void LimitCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return;
  wxDELETE(m_under);
  m_under = under;
}

void LimitCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();

  m_base->RecalculateWidthsList(fontsize);
  m_under->RecalculateWidthsList(MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  m_name->RecalculateWidthsList(fontsize);

  m_width = MAX(m_name->GetFullWidth(scale), m_under->GetFullWidth(scale))
            + m_base->GetFullWidth(scale);
  ResetData();
}

void LimitCell::RecalculateHeight(int fontsize)
{
  m_under->RecalculateHeightList(MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  m_name->RecalculateHeightList(fontsize);
  m_base->RecalculateHeightList(fontsize);

  m_center = MAX(m_base->GetMaxCenter(), m_name->GetMaxCenter());
  m_height = m_center + MAX(m_name->GetMaxDrop() + m_under->GetMaxHeight(),
                            m_base->GetMaxDrop());
}

void LimitCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {   
    MathCell::Draw(point, fontsize);
    Configuration *configuration = (*m_configuration);
    double scale = configuration->GetScale();
    wxPoint base(point), under(point), name(point);

    name.x = point.x + MAX(m_name->GetFullWidth(scale),
                           m_under->GetFullWidth(scale)) / 2 -
             m_name->GetFullWidth(scale) / 2;
    m_name->DrawList(name, fontsize);

    under.x = point.x + MAX(m_name->GetFullWidth(scale),
                            m_under->GetFullWidth(scale)) / 2 -
              m_under->GetFullWidth(scale) / 2;
    under.y = point.y + m_name->GetMaxDrop() + m_under->GetMaxCenter();
    m_under->DrawList(under, MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));

    base.x += MAX(m_name->GetFullWidth(scale),
                  m_under->GetFullWidth(scale));
    m_base->DrawList(base, fontsize);
  }
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
  std::cerr<<under<<"\n";
  wxString base = m_base->ListToTeX();
  int varEnd = under.Find(wxT("->"));
  int toStart = 0;
  if(varEnd == wxNOT_FOUND)
  {
    varEnd = under.Find(wxT("\\mbox{\\rightarrow }"));
    if(varEnd != wxNOT_FOUND)
    {
      toStart = varEnd + 19;
      varEnd -= 1;
    }
  }
  else
  {
    toStart = varEnd + 2;
    varEnd -= 1;
  }
                          
  wxString var = under.SubString(0, varEnd);
  wxString to = under.SubString(toStart,
                                under.Length() - 1);
  wxString s = wxT("\\lim_{") + var + wxT("\\to ") + to + wxT("}{") + base + wxT("}");
  return s;
}

wxString LimitCell::ToMathML()
{
  wxString base = m_base->ListToMathML();

  wxString from;
  if (m_under) from = m_under->ListToMathML();

  wxString retval;
  if (from.IsEmpty())
    retval = wxT("<mo>lim</mo>") + base;
  else
    retval = wxT("<munder><mo>lim</mo>") + from + wxT("</munder>\n");
  return (retval);
}

wxString LimitCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return _T("<lm") + flags + wxT("><r>") + m_name->ListToXML() + _T("</r><r>") +
         m_under->ListToXML() + _T("</r><r>") +
         m_base->ListToXML() + _T("</r></lm>");
}

wxString LimitCell::ToOMML()
{
  wxString under = m_under->ListToOMML();
  under.Replace(wxT("->"), wxT("\x2192"));

  return _T("<m:func><m:fName><m:limLow><m:e><m:r>lim</m:r></m:e><m:lim>") +
         under + _T("</m:lim></m:limLow></m:fName><m:e>") +
         m_base->ListToOMML() + _T("</m:e></m:func>");
}

void LimitCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
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
