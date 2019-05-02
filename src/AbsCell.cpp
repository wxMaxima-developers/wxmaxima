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
  This file defines the class AbsCell

  AbsCell is the Cell type that represents the field that represents the 
  <code>abs()</code> and <code>cabs()</code> commands.
*/


#include "AbsCell.h"
#include "TextCell.h"

AbsCell::AbsCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_innerCell = NULL;
  m_open = new TextCell(parent, config, cellPointers, wxT("abs("));
  m_open->DontEscapeOpeningParenthesis();
  m_close = new TextCell(parent, config, cellPointers, wxT(")"));
  m_last = NULL;
}

Cell *AbsCell::Copy()
{
  AbsCell *tmp = new AbsCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList());
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;
  tmp->m_open->DontEscapeOpeningParenthesis();

  return tmp;
}

AbsCell::~AbsCell()
{
  wxDELETE(m_innerCell);
  wxDELETE(m_open);
  wxDELETE(m_close);
  m_innerCell = NULL;
  m_open = NULL;
  m_close = NULL;
  MarkAsDeleted();
}

std::list<Cell *> AbsCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_innerCell)
    innerCells.push_back(m_innerCell);
  return innerCells;
}

void AbsCell::SetInner(Cell *inner)
{
  if (inner == NULL)
    return;
  wxDELETE(m_innerCell);
  m_innerCell = inner;

  m_last = m_innerCell;
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void AbsCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  m_innerCell->RecalculateWidthsList(fontsize);
  m_width = m_innerCell->GetFullWidth() + Scale_Px(8) + 2 * (*m_configuration)->GetDefaultLineWidth();
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  ResetData();
  if(m_isBrokenIntoLines)
  {
    m_width = 0;
    m_height = 0;
  }
}

void AbsCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_innerCell->RecalculateHeightList(fontsize);
  m_height = m_innerCell->GetMaxHeight() + Scale_Px(4);
  m_center = m_innerCell->GetMaxCenter() + Scale_Px(2);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);

  if (m_isBrokenIntoLines)
  {
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
}

void AbsCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {    
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    SetPen();
    wxPoint in;
    in.x = point.x + Scale_Px(4) + (*m_configuration)->GetDefaultLineWidth();
    in.y = point.y;
    m_innerCell->DrawList(in);

    dc->DrawLine(point.x + Scale_Px(2) + (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + Scale_Px(2),
                point.x + Scale_Px(2) + (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + m_height - Scale_Px(2));
    dc->DrawLine(point.x + m_width - Scale_Px(2) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + Scale_Px(2),
                point.x + m_width - Scale_Px(2) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                point.y - m_center + m_height - Scale_Px(2));
    UnsetPen();
  }
}

wxString AbsCell::ToString()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  wxString s;
  s = wxT("abs(") + m_innerCell->ListToString() + wxT(")");
  return s;
}

wxString AbsCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  wxString s;
  s = wxT("abs(") + m_innerCell->ListToMatlab() + wxT(")");
  return s;
}

wxString AbsCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  return wxT("\\left| ") + m_innerCell->ListToTeX() + wxT("\\right| ");
}

wxString AbsCell::ToMathML()
{
  return wxT("<row><mo>|</mo>") +
         m_innerCell->ListToMathML() +
         wxT("<mo>|</mo></row>\n");
//  return wxT("<apply><abs/><ci>") + m_innerCell->ListToMathML() + wxT("</ci></apply>");
}

wxString AbsCell::ToOMML()
{
  return wxT("<m:d><m:dPr m:begChr=\"|\" m:endChr=\"|\"></m:dPr><m:e>") +
         m_innerCell->ListToOMML() + wxT("</m:e></m:d>");
}

wxString AbsCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  
  return wxT("<a") +flags + wxT(">") + m_innerCell->ListToXML() + wxT("</a>");
}

bool AbsCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    wxASSERT_MSG(m_last != NULL, _("Bug: No last cell in an absCell!"));
    if (m_last != NULL)
    {
      m_last->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last;
    }
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;

    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());

    return true;
  }
  return false;
}

void AbsCell::Unbreak()
{
  if (m_isBrokenIntoLines)
    m_innerCell->UnbreakList();
  Cell::Unbreak();
}
