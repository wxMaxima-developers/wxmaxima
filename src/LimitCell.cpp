// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class LimitCell

  LimitCell is the Cell type that represents maxima's <code>limit()</code> command.
*/

#include "LimitCell.h"

#define MIN_LIMIT_FONT_SIZE 8
#define LIMIT_FONT_SIZE_DECREASE 1

LimitCell::LimitCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_base = NULL;
  m_under = NULL;
  m_name = NULL;
  m_open = new TextCell(parent, config, cellPointers, "(");
  m_comma = new TextCell(parent, config, cellPointers, ",");
  m_close = new TextCell(parent, config, cellPointers, ")");

  m_cellPointers = cellPointers;
}

Cell *LimitCell::Copy()
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
  wxDELETE(m_open);
  wxDELETE(m_comma);
  wxDELETE(m_close);
  MarkAsDeleted();
  m_base = m_under = m_name = m_open = m_comma = m_close = NULL;
}

std::list<Cell *> LimitCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_base)
    innerCells.push_back(m_base);
  if(m_under)
    innerCells.push_back(m_under);
  if(m_name)
    innerCells.push_back(m_name);
  if(m_open)
    innerCells.push_back(m_open);
  if(m_comma)
    innerCells.push_back(m_comma);
  if(m_close)
    innerCells.push_back(m_close);
  return innerCells;
}

void LimitCell::SetName(Cell *name)
{
  if (name == NULL)
    return;
  wxDELETE(m_name);
  m_name_last = m_name = name;
  while(m_name_last->m_next != NULL)
    m_name_last = m_name_last->m_next;
}

void LimitCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_base);
  m_base_last = m_base = base;
  while(m_base_last->m_next != NULL)
    m_base_last = m_base_last->m_next;
}

void LimitCell::SetUnder(Cell *under)
{
  if (under == NULL)
    return;
  wxDELETE(m_under);
  m_under_last = m_under = under;
  while(m_under_last->m_next != NULL)
    m_under_last = m_under_last->m_next;
}

void LimitCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  if(m_base)
    m_base->RecalculateWidthsList(fontsize);
  if(m_under)
    m_under->RecalculateWidthsList(MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  if(m_name)
    m_name->RecalculateWidthsList(fontsize);
  if(m_open)
    m_open->RecalculateWidthsList(fontsize);
  if(m_comma)
    m_comma->RecalculateWidthsList(fontsize);
  if(m_close)
    m_close->RecalculateWidthsList(fontsize);

  if (!m_isBrokenIntoLines)
    m_width = MAX(m_name->GetFullWidth(), m_under->GetFullWidth())
      + m_base->GetFullWidth();
  else
    m_width = 0;
  
  ResetData();
}

void LimitCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  if(m_under)
    m_under->RecalculateHeightList(MAX(MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE));
  if(m_name)
    m_name->RecalculateHeightList(fontsize);
  if(m_base)
    m_base->RecalculateHeightList(fontsize);
  if(m_open)
    m_open->RecalculateHeightList(fontsize);
  if(m_comma)
    m_comma->RecalculateHeightList(fontsize);
  if(m_close)
    m_close->RecalculateHeightList(fontsize);

  if (!m_isBrokenIntoLines)
  {
    m_center = MAX(m_base->GetMaxCenter(), m_name->GetMaxCenter());
    m_height = m_center + MAX(m_name->GetMaxDrop() + m_under->GetMaxHeight(),
                              m_base->GetMaxDrop());
  }
  else
  {
    m_height = m_name->GetMaxHeight();
    m_center = m_name->GetMaxCenter();
  }
}

void LimitCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {   
    wxPoint base(point), under(point), name(point);

    name.x = point.x + MAX(m_name->GetFullWidth(),
                           m_under->GetFullWidth()) / 2 -
             m_name->GetFullWidth() / 2;
    m_name->DrawList(name);

    under.x = point.x + MAX(m_name->GetFullWidth(),
                            m_under->GetFullWidth()) / 2 -
              m_under->GetFullWidth() / 2;
    under.y = point.y + m_name->GetMaxDrop() + m_under->GetMaxCenter();
    m_under->DrawList(under);

    base.x += MAX(m_name->GetFullWidth(),
                  m_under->GetFullWidth());
    m_base->DrawList(base);
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

wxString LimitCell::ToMatlab()
{
  wxString s(wxT("limit"));
  wxString under = m_under->ListToMatlab();
  wxString base = m_base->ListToMatlab();
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


bool LimitCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_name_last->m_nextToDraw = m_open;
    m_open->m_nextToDraw = m_base;
    m_base_last->m_nextToDraw = m_comma;
    m_comma->m_nextToDraw = m_under;
    m_under_last->m_nextToDraw = m_close;
    m_close->m_nextToDraw = m_nextToDraw;
    if(m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_name;
    m_name->m_previousToDraw = this;
    m_open->m_previousToDraw = m_name_last;
    m_base->m_previousToDraw = m_open;
    m_comma->m_previousToDraw = m_base_last;
    m_under->m_previousToDraw = m_comma;
    m_close->m_previousToDraw = m_under_last;
    return true;
  }
  return false;
}

void LimitCell::Unbreak()
{
  if (m_isBrokenIntoLines)
  {
    m_name->UnbreakList();
    m_base->UnbreakList();
    m_under->UnbreakList();
  }
  Cell::Unbreak();
}
