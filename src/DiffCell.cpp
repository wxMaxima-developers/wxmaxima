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
  This file defines the class DiffCell

  DiffCell is the Cell type that represents the field that represents the diff() command.
 */

#include "DiffCell.h"
#include "wx/config.h"

DiffCell::DiffCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_baseCell = NULL;
  m_diffCell = NULL;
  m_cellPointers = cellPointers;
}

Cell *DiffCell::Copy()
{
  DiffCell *tmp = new DiffCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetDiff(m_diffCell->CopyList());
  tmp->SetBase(m_baseCell->CopyList());
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;

  return tmp;
}

DiffCell::~DiffCell()
{
  wxDELETE(m_baseCell);
  wxDELETE(m_diffCell);
  m_baseCell = m_diffCell = NULL;
  MarkAsDeleted();
}

std::list<Cell *> DiffCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_diffCell)
    innerCells.push_back(m_diffCell);
  return innerCells;
}


void DiffCell::SetDiff(Cell *diff)
{
  if (diff == NULL)
    return;
  wxDELETE(m_diffCell);
  m_diffCell = diff;

  m_diffCell->m_SuppressMultiplicationDot = true;
}

void DiffCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
}

void DiffCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  m_baseCell->RecalculateWidthsList(fontsize);
  m_diffCell->RecalculateWidthsList(fontsize);
  m_width = m_baseCell->GetFullWidth() + m_diffCell->GetFullWidth();
  ResetData();
}

void DiffCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_baseCell->RecalculateHeightList(fontsize);
  m_diffCell->RecalculateHeightList(fontsize);
  m_center = MAX(m_diffCell->GetMaxCenter(), m_baseCell->GetMaxCenter());
  m_height = m_center + MAX(m_diffCell->GetMaxDrop(), m_baseCell->GetMaxDrop());
}

void DiffCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  { 
    wxPoint bs, df;
    df.x = point.x;
    df.y = point.y;
    m_diffCell->DrawList(df);

    bs.x = point.x + m_diffCell->GetFullWidth();
    bs.y = point.y;
    m_baseCell->DrawList(bs);
  }
}

wxString DiffCell::ToString()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  Cell *tmp = m_baseCell->m_next;
  wxString s = wxT("'diff(");
  if (tmp != NULL)
    s += tmp->ListToString();
  s += m_diffCell->ListToString();
  s += wxT(")");
  return s;
}

wxString DiffCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  Cell *tmp = m_baseCell->m_next;
  wxString s = wxT("'diff(");
  if (tmp != NULL)
	s += tmp->ListToMatlab();
  s += m_diffCell->ListToMatlab();
  s += wxT(")");
  return s;
}

wxString DiffCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  wxString diff = m_diffCell->ListToTeX();
  wxString function = m_baseCell->ListToTeX();

  bool usePartialForDiff = false;
  wxConfig::Get()->Read(wxT("usePartialForDiff"), &usePartialForDiff);
  if (usePartialForDiff)
    diff.Replace(wxT("\\frac{d}{d"), wxT("\\frac{\\partial}{\\partial"));

  wxString s = diff + function;
  return s;
}

wxString DiffCell::ToMathML()
{
  wxString retval;

  retval = wxT("<mrow>") + m_diffCell->ListToMathML();
  if (m_baseCell)
    retval += m_baseCell->ListToMathML();
  retval += wxT("</mrow>\n");
  // retval = wxT("<apply><diff/><ci>") + m_diffCell->ListToMathML() + wxT("</ci>");
  // if(m_baseCell)
  //   retval += wxT("<ci>") + m_baseCell->ListToMathML() + wxT("</ci>") ;
  // retval += wxT("</apply>");
  return retval;
}

wxString DiffCell::ToOMML()
{
  wxString retval;

  retval = m_diffCell->ListToOMML();
  if (m_baseCell)
    retval += m_baseCell->ListToOMML();

  return retval;
}

wxString DiffCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<d") + flags + wxT(">") + m_diffCell->ListToXML() + m_baseCell->ListToXML() + _T("</d>");
}
