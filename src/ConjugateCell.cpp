// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ConjugateCell

  ConjugateCell is the MathCell type that represents the field that represents the 
  conjugate() command.
 */

#include "ConjugateCell.h"
#include "TextCell.h"

ConjugateCell::ConjugateCell() : MathCell()
{
  m_innerCell = NULL;
  m_last = NULL;
  m_open = new TextCell(wxT("conjugate("));
  dynamic_cast<TextCell*>(m_open) -> DontEscapeOpeningParenthesis();
  m_close = new TextCell(wxT(")"));
}

ConjugateCell::~ConjugateCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
  delete m_open;
  delete m_close;
}

void ConjugateCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_innerCell != NULL)
    m_innerCell->SetParentList(parent);
  if (m_open != NULL)
    m_open->SetParentList(parent);
  if (m_close != NULL)
    m_close->SetParentList(parent);
}

MathCell* ConjugateCell::Copy()
{
  ConjugateCell* tmp = new ConjugateCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList());
  tmp->m_isBroken = m_isBroken;
  
  return tmp;
}

void ConjugateCell::Destroy()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = NULL;
  m_next = NULL;
}

void ConjugateCell::SetInner(MathCell *inner)
{
  if (inner == NULL)
    return ;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;

  m_last = m_innerCell;
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void ConjugateCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateWidthsList(parser, fontsize);
  m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(8, scale);
  m_open->RecalculateWidthsList(parser, fontsize);
  m_close->RecalculateWidthsList(parser, fontsize);
  ResetData();
}

void ConjugateCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSizeList(parser, fontsize);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(4, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(2, scale);
  m_open->RecalculateSizeList(parser, fontsize);
  m_close->RecalculateSizeList(parser, fontsize);
}

void ConjugateCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  MathCell::Draw(parser, point, fontsize);

  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point) && InUpdateRegion())
  {
    SetPen(parser);
    wxPoint in;
    in.x = point.x + SCALE_PX(4, scale);
    in.y = point.y;
    m_innerCell->DrawList(parser, in, fontsize);

    dc.DrawLine(point.x + SCALE_PX(2, scale),
                point.y - m_center + SCALE_PX(2, scale),
                point.x + m_width - SCALE_PX(2, scale) - 1,
                point.y - m_center + SCALE_PX(2, scale)
		);
		//                point.y - m_center + m_height - SCALE_PX(2, scale));
    UnsetPen(parser);
  }
}

wxString ConjugateCell::ToString()
{
  if (m_isBroken)
    return wxEmptyString;
  else
    return wxT("conjugate(") + m_innerCell->ListToString() + wxT(")");
}

wxString ConjugateCell::ToTeX()
{
  if (m_isBroken)
    return wxEmptyString;
  else
    return wxT("\\overline{") + m_innerCell->ListToTeX() + wxT("}");
}

wxString ConjugateCell::ToMathML()
{
//  return wxT("<apply><conjugate/><ci>") + m_innerCell->ListToMathML() + wxT("</ci></apply>");
  return wxT("<mover accent=\"true\">") + m_innerCell->ListToMathML() +
    wxT("<mo>&#xaf;</mo></mover>\n");
}

wxString ConjugateCell::ToOMML()
{
  return wxT("<m:bar><m:barPr><m:pos m:val=\"top\"/> </m:barPr><m:e>") +
    m_innerCell->ListToOMML() + wxT("</m:e></m:bar>");
}

wxString ConjugateCell::ToXML()
{
  return wxT("<cj>") + m_innerCell->ListToXML() + wxT("</cj>");
}

void ConjugateCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_innerCell->ContainsRect(rect))
    m_innerCell->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool ConjugateCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    wxASSERT_MSG(m_last != NULL,_("Bug: No last cell in an conjugateCell!"));
    if(m_last != NULL)
    {
      m_last->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last;
    }
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    return true;
  }
  return false;
}

void ConjugateCell::Unbreak()
{
  if (m_isBroken)
    m_innerCell->UnbreakList();
  MathCell::Unbreak();
}
