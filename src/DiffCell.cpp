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
#include "GroupCell.h"
#include "TextCell.h"
#include "wx/config.h"
#include "VisiblyInvalidCell.h"


DiffCell::DiffCell(GroupCell *parent, Configuration **config) :
  Cell(parent, config),
  m_baseCell(std::make_unique<VisiblyInvalidCell>(parent,config)),
  m_diffCell(std::make_unique<VisiblyInvalidCell>(parent,config)),
  m_open(std::make_unique<TextCell>(parent, config, "diff(")),
  m_comma(std::make_unique<TextCell>(parent, config, ",")),
  m_close(std::make_unique<TextCell>(parent, config, ")"))

{
  InitBitFields();
}

DiffCell::DiffCell(const DiffCell &cell):
 DiffCell(cell.m_group, cell.m_configuration)
{
  CopyCommonData(cell);
  if(cell.m_diffCell)
    SetDiff(cell.m_diffCell->CopyList());
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
}

std::unique_ptr<Cell> DiffCell::Copy() const
{
  return std::make_unique<DiffCell>(*this);
}

void DiffCell::SetDiff(std::unique_ptr<Cell> &&diff)
{
  if (!diff)
    return;
  m_diffCell = std::move(diff);
  m_diffCell->SetSuppressMultiplicationDot(true);
}

void DiffCell::SetBase(std::unique_ptr<Cell> &&base)
{
  if (!base)
    return;
  m_baseCell = std::move(base);
}

void DiffCell::Recalculate(AFontSize fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateList(fontsize);
  m_diffCell->RecalculateList(fontsize);
  m_open->RecalculateList(fontsize);
  m_comma->RecalculateList(fontsize);
  m_close->RecalculateList(fontsize);
  if(!m_isBrokenIntoLines)
  {
    m_width = m_baseCell->GetFullWidth() + m_diffCell->GetFullWidth();
    m_center = wxMax(m_diffCell->GetCenterList(), m_baseCell->GetCenterList());
    m_height = m_center + wxMax(m_diffCell->GetMaxDrop(), m_baseCell->GetMaxDrop());
  }
  else
    m_width = m_center = m_height = 0;
  Cell::Recalculate(fontsize);
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

wxString DiffCell::ToString() const
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

wxString DiffCell::ToMatlab() const
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

wxString DiffCell::ToTeX() const
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

wxString DiffCell::ToMathML() const
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

wxString DiffCell::ToOMML() const
{
  wxString retval;

  retval = m_diffCell->ListToOMML();
  if (m_baseCell)
    retval += m_baseCell->ListToOMML();

  return retval;
}

wxString DiffCell::ToXML() const
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<d") + flags + wxT(">") + m_diffCell->ListToXML() + m_baseCell->ListToXML() + _T("</d>");
}

bool DiffCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    Cell::BreakUp();
    m_isBrokenIntoLines = true;
    m_open->SetNextToDraw(m_baseCell);
    m_baseCell->last()->SetNextToDraw(m_comma);
    m_comma->SetNextToDraw(m_diffCell);
    m_diffCell->last()->SetNextToDraw(m_close);
    m_nextToDraw = m_open;
    ResetCellListSizes();
    m_height = 0;
    m_center = 0;
    m_width = 0;
    return true;
  }
  return false;
}

void DiffCell::SetNextToDraw(Cell *next)
{
  if (m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
