// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class ConjugateCell

  ConjugateCell is the Cell type that represents the field that represents the 
  conjugate() command.
 */

#include "ConjugateCell.h"

ConjugateCell::ConjugateCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_innerCell(new TextCell(parent, config, cellPointers, wxString{})),
    m_open(new TextCell(parent, config, cellPointers, "conjugate(")),
    m_close(new TextCell(parent, config, cellPointers, ")"))
{
  m_nextToDraw = NULL;
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
  m_last = NULL;
}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=ConjugateCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=ConjugateCell::m_close
ConjugateCell::ConjugateCell(const ConjugateCell &cell):
 ConjugateCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if (cell.m_innerCell)
    SetInner(cell.m_innerCell->CopyList());
}

ConjugateCell::~ConjugateCell()
{
  if(this == m_cellPointers->m_selectionStart)
    m_cellPointers->m_selectionStart = NULL;
  if(this == m_cellPointers->m_selectionEnd)
    m_cellPointers->m_selectionEnd = NULL;
  MarkAsDeleted();
}

void ConjugateCell::SetInner(Cell *inner)
{
  if (!inner)
    return;
  m_innerCell.reset(inner);

  m_last = m_innerCell.get();
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void ConjugateCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_innerCell->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  if(!m_isBrokenIntoLines)
    m_width = m_innerCell->GetFullWidth() + Scale_Px(8);
  else
    m_width = 0;
  Cell::RecalculateWidths(fontsize);
}

void ConjugateCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_innerCell->RecalculateHeightList(fontsize);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  if(!m_isBrokenIntoLines)
  {
    m_height = m_innerCell->GetHeightList() + Scale_Px(4);
    m_center = m_innerCell->GetCenterList() + Scale_Px(2);
  }
  else
  {
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
  }
  Cell::RecalculateHeight(fontsize);
}

void ConjugateCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {
    Configuration *configuration = (*m_configuration);
    
    wxDC *dc = configuration->GetDC();
    SetPen();
    wxPoint in;
    in.x = point.x + Scale_Px(4);
    in.y = point.y;
    m_innerCell->DrawList(in);

    dc->DrawLine(point.x + Scale_Px(2),
                 point.y - m_center + Scale_Px(2),
                 point.x + m_width - Scale_Px(2) - 1,
                 point.y - m_center + Scale_Px(2)
      );
    //                point.y - m_center + m_height - Scale_Px(2));
    UnsetPen();
  }
}

wxString ConjugateCell::ToString()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  else
    return wxT("conjugate(") + m_innerCell->ListToString() + wxT(")");
}

wxString ConjugateCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  else
	return wxT("conjugate(") + m_innerCell->ListToMatlab() + wxT(")");
}

wxString ConjugateCell::ToTeX()
{
  if (m_isBrokenIntoLines)
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
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<cj") + flags + wxT(">") + m_innerCell->ListToXML() + wxT("</cj>");
}

bool ConjugateCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open->SetNextToDraw(m_innerCell.get());
    wxASSERT_MSG(m_last != NULL, _("Bug: No last cell in a conjugateCell!"));
    if (m_last != NULL)
      m_last->SetNextToDraw(m_close.get());
    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_open.get();
    ResetData();        
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
    return true;
  }
  return false;
}

void ConjugateCell::SetNextToDraw(Cell *next)
{
  if(m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
