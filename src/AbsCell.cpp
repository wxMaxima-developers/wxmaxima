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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class AbsCell

  AbsCell is the Cell type that represents the field that represents the 
  <code>abs()</code> and <code>cabs()</code> commands.
*/


#include "AbsCell.h"

static const wxString absText{wxT("abs(")};
static const wxString absParen{wxT(")")};

AbsCell::AbsCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
  Cell(parent, config, cellPointers),
  m_open(new TextCell(parent, config, cellPointers, absText)),
  m_close(new TextCell(parent, config, cellPointers, absParen))
{
  m_nextToDraw = NULL;
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
  m_open->SetStyle(TS_FUNCTION);
}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=AbsCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=AbsCell::m_close
AbsCell::AbsCell(const AbsCell &cell):
  AbsCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_innerCell)
    SetInner(cell.m_innerCell->CopyList());
}

AbsCell::~AbsCell()
{
  m_innerCell.reset();
  m_open.reset();
  m_close.reset();
  MarkAsDeleted();
}

Cell::InnerCells AbsCell::GetInnerCells() const
{
  InnerCells innerCells;
  if(m_innerCell)
    innerCells.push_back(m_innerCell);
  if(m_open)
    innerCells.push_back(m_open);
  if(m_close)
    innerCells.push_back(m_close);
  return innerCells;
}

void AbsCell::SetInner(Cell *inner)
{
  if (inner == NULL)
    return;
  m_innerCell = std::shared_ptr<Cell>(inner);

  m_last = m_innerCell.get();
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void AbsCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_innerCell->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  if(m_isBrokenIntoLines)
  {
    m_width = 0;
    m_height = 0;
  }
  else
  {
    m_width = m_innerCell->GetFullWidth() + Scale_Px(8) + 2 * (*m_configuration)->GetDefaultLineWidth();
  }
  Cell::RecalculateWidths(fontsize);
}

void AbsCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_innerCell->RecalculateHeightList(fontsize);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  if (!m_isBrokenIntoLines)
  {
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
    m_height = m_innerCell->GetHeightList() + Scale_Px(4);
    m_center = m_innerCell->GetCenterList() + Scale_Px(2);
    m_open->RecalculateHeightList(fontsize);
    m_close->RecalculateHeightList(fontsize);
  }
  Cell::RecalculateHeight(fontsize);
}

void AbsCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
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
    return {};
  static const wxString format{wxT("abs(%s)")};
  return wxString::Format(format, m_innerCell->ListToString());
}

wxString AbsCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
    return {};
  static const wxString format{wxT("abs(%s)")};
  return wxString::Format(format, m_innerCell->ListToMatlab());
}

wxString AbsCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return {};
  static const wxString format{wxT("\\left| %s\\right| ")};
  return wxString::Format(format, m_innerCell->ListToTeX());
}

wxString AbsCell::ToMathML()
{
#if 0
  static const wxString format{wxT("<apply><abs/><ci>%s</ci></apply>")};
#endif
  static const wxString format{wxT("<row><mo>|</mo>%s<mo>|</mo></row>\n")};
  return wxString::Format(format, m_innerCell->ListToMathML());
}

wxString AbsCell::ToOMML()
{
  static const wxString format{
    wxT("<m:d><m:dPr m:begChr=\"|\" m:endChr=\"|\"></m:dPr><m:e>%s</m:e></m:d>")
  };
  return wxString::Format(format, m_innerCell->ListToOMML());
}

wxString AbsCell::ToXML()
{
  static const wxString breaklineFlags{wxT(" breakline=\"true\"")};
  static const wxString format{wxT("<a%s>%s</a>")};
  return wxString::Format(format,
                          m_forceBreakLine ? breaklineFlags : wxString{},
                          m_innerCell->ListToXML());
}

bool AbsCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open->SetNextToDraw(m_innerCell.get());
    wxASSERT_MSG(m_last != NULL, _("Bug: No last cell in an absCell!"));
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

void AbsCell::SetNextToDraw(Cell *next)
{
  if(m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
