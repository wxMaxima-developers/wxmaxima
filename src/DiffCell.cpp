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
  This file defines the class DiffCell

  DiffCell is the Cell type that represents the field that represents the diff() command.
 */

#include "DiffCell.h"
#include "TextCell.h"
#include "wx/config.h"

DiffCell::DiffCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
  Cell(parent, config, cellPointers),
  m_baseCell(std::make_shared<TextCell>(parent, config, cellPointers)),
  m_diffCell(std::make_shared<TextCell>(parent, config, cellPointers))
{
  m_nextToDraw = NULL;
}

DiffCell::DiffCell(const DiffCell &cell):
 DiffCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_diffCell)
    SetDiff(cell.m_diffCell->CopyList());
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
}

DiffCell::~DiffCell()
{
  MarkAsDeleted();
}

Cell::InnerCells DiffCell::GetInnerCells() const
{
  InnerCells innerCells;
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
  m_diffCell = std::shared_ptr<Cell>(diff);

  m_diffCell->m_SuppressMultiplicationDot = true;
}

void DiffCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  m_baseCell = std::shared_ptr<Cell>(base);
}

void DiffCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  Cell::RecalculateWidths(fontsize);
    m_baseCell->RecalculateWidthsList(fontsize);
    m_diffCell->RecalculateWidthsList(fontsize);
  if(!m_isBrokenIntoLines)
    m_width = m_baseCell->GetFullWidth() + m_diffCell->GetFullWidth();
  else
    m_width = 0;
  Cell::RecalculateWidths(fontsize);
}

void DiffCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateHeightList(fontsize);
  m_diffCell->RecalculateHeightList(fontsize);
  if(!m_isBrokenIntoLines)
  {
    m_center = wxMax(m_diffCell->GetCenterList(), m_baseCell->GetCenterList());
    m_height = m_center + wxMax(m_diffCell->GetMaxDrop(), m_baseCell->GetMaxDrop());
  }
  else
    m_center = m_height = 0;
  Cell::RecalculateHeight(fontsize);
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
    return {};
  static const wxString format{wxT("'diff(%s%s)")};
  Cell *tmp = m_baseCell->m_next;
  return wxString::Format(format,
                          tmp ? tmp->ListToString() : wxString{},
                          m_diffCell->ListToString());
}

wxString DiffCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return {};
  static const wxString format{wxT("'diff(%s%s)")};
  Cell *tmp = m_baseCell->m_next;
  return wxString::Format(format,
                          tmp ? tmp->ListToMatlab() : wxString{},
                          m_diffCell->ListToMatlab());
}

wxString DiffCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return {};

  // TODO: This is slow. The configuration should be cached.
  bool usePartialForDiff = false;
  wxConfig::Get()->Read(wxT("usePartialForDiff"), &usePartialForDiff);

  wxString diff = m_diffCell->ListToTeX();
  if (usePartialForDiff)
    diff.Replace(wxT("\\frac{d}{d"), wxT("\\frac{\\partial}{\\partial"));

  static const wxString format{wxT("%s%s")};
  return wxString::Format(format, diff, m_diffCell->ListToTeX());
}

wxString DiffCell::ToMathML()
{
#if 0
  static const wxString format{wxT("<apply><diff/><ci>%s</ci>%s%s%s</apply>")};
  static const wxString left{wxT("<ci>")};
  static const wxString right{wxT("</ci>")};
  return wxString::Format(format,
                          m_diffCell->ListToMathML(),
                          m_baseCell ? left : wxString{},
                          m_baseCell ? m_baseCell->ListToMathML() : wxString{},
                          m_baseCell ? right : wxString{});
#endif
  static const wxString format{wxT("<mrow>%s%s</mrow>\n")};
  return wxString::Format(format,
                          m_diffCell->ListToMathML(),
                          m_baseCell ? m_baseCell->ListToMathML() : wxString{});
}

wxString DiffCell::ToOMML()
{
  static const wxString format(wxT("%s%s"));
  return m_baseCell
           ? wxString::Format(format, m_diffCell->ListToOMML(), m_baseCell->ListToOMML())
           : m_diffCell->ListToOMML();
}

wxString DiffCell::ToXML()
{
  static const wxString breaklineFlags{wxT(" breakline=\"true\"")};
  static const wxString format{wxT("<d%s>%s%s</d>")};
  return wxString::Format(format,
                          m_forceBreakLine ? breaklineFlags : wxString{},
                          m_diffCell->ListToXML(),
                          m_baseCell->ListToXML());
}

void DiffCell::SetNextToDraw(Cell *next)
{
  m_nextToDraw = next;
}
