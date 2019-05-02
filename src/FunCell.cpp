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
  This file defines the class FunCell

  FunCell is the Cell type that represents functions that don't require special handling.
 */

#include "FunCell.h"

FunCell::FunCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_nameCell = NULL;
  m_argCell = NULL;
}

Cell *FunCell::Copy()
{
  FunCell *tmp = new FunCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetName(m_nameCell->CopyList());
  tmp->SetArg(m_argCell->CopyList());
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;

  return tmp;
}

FunCell::~FunCell()
{
  wxDELETE(m_nameCell);
  wxDELETE(m_argCell);
  m_nameCell = m_argCell = NULL;
  MarkAsDeleted();
}

std::list<Cell *> FunCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_nameCell)
    innerCells.push_back(m_nameCell);
  if(m_argCell)
    innerCells.push_back(m_argCell);
  return innerCells;
}

void FunCell::SetName(Cell *name)
{
  if (name == NULL)
    return;
  wxDELETE(m_nameCell);
  m_nameCell = name;
  name->SetStyle(TS_FUNCTION);
}

void FunCell::SetArg(Cell *arg)
{
  if (arg == NULL)
    return;
  wxDELETE(m_argCell);
  m_argCell = arg;
}

void FunCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  m_argCell->RecalculateWidthsList(fontsize);
  m_nameCell->RecalculateWidthsList(fontsize);
  if(!m_isBrokenIntoLines)
    m_width = m_nameCell->GetFullWidth() + m_argCell->GetFullWidth() -
      Scale_Px(1);
  else
    m_width = 0;
  ResetData();
}

void FunCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_nameCell->RecalculateHeightList(fontsize);
  m_argCell->RecalculateHeightList(fontsize);
  m_center = MAX(m_nameCell->GetMaxCenter(), m_argCell->GetMaxCenter());
  if(!m_isBrokenIntoLines)
    m_height = m_center + MAX(m_nameCell->GetMaxDrop(), m_argCell->GetMaxDrop());
  else
    m_height = 0;
}

void FunCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {

    wxPoint name(point), arg(point);
    m_nameCell->DrawList(name);

    arg.x += m_nameCell->GetFullWidth();
    m_argCell->DrawList(arg);
  }
}

wxString FunCell::ToString()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText + Cell::ListToString();
  wxString s = m_nameCell->ListToString() + m_argCell->ListToString();
  return s;
}

wxString FunCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
	return m_altCopyText + Cell::ListToMatlab();
  wxString s = m_nameCell->ListToMatlab() + m_argCell->ListToMatlab();
  return s;
}

wxString FunCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;

  wxString s;

  if (
          (m_nameCell->ToString() == wxT("sin")) ||
          (m_nameCell->ToString() == wxT("cos")) ||
          (m_nameCell->ToString() == wxT("cosh")) ||
          (m_nameCell->ToString() == wxT("cos")) ||
          (m_nameCell->ToString() == wxT("log")) ||
          (m_nameCell->ToString() == wxT("cot")) ||
          (m_nameCell->ToString() == wxT("sec")) ||
          (m_nameCell->ToString() == wxT("csc")) ||
          (m_nameCell->ToString() == wxT("tan"))
          )
    s = wxT("\\") + m_nameCell->ToString() + wxT("{") + m_argCell->ListToTeX() + wxT("}");
  else
    s = m_nameCell->ListToTeX() + m_argCell->ListToTeX();

  return s;
}

wxString FunCell::ToXML()
{
//  if (m_isBrokenIntoLines)
//    return wxEmptyString;
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  return wxT("<fn") + flags + wxT("><r>") + m_nameCell->ListToXML() + wxT("</r>") +
         m_argCell->ListToXML() + wxT("</fn>");
}

wxString FunCell::ToMathML()
{
//  if (m_isBrokenIntoLines)
//    return wxEmptyString;
  return wxT("<mrow>") + m_nameCell->ListToMathML() +
         wxT("<mo>&#x2061;</mo>") + m_argCell->ListToMathML() + wxT("</mrow>\n");
}

wxString FunCell::ToOMML()
{
  return m_nameCell->ListToOMML() +
         m_argCell->ListToOMML();
}

bool FunCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_nameCell->m_previousToDraw = this;
    m_nameCell->m_nextToDraw = m_argCell;
    m_argCell->m_previousToDraw = m_nameCell;
    m_argCell->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_argCell;
    m_nextToDraw = m_nameCell;
    m_width = 0;
    return true;
  }
  return false;
}

void FunCell::Unbreak()
{
  if (m_isBrokenIntoLines)
  {
    m_nameCell->UnbreakList();
    m_argCell->UnbreakList();
  }
  Cell::Unbreak();
}
