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
  This file defines the class ExptCell

  ExptCell is the Cell type that represents exponents.
 */

#include "ExptCell.h"
#include "VisiblyInvalidCell.h"

#define EXPT_DEC 2

ExptCell::ExptCell(GroupCell *parent, Configuration **config) :
    Cell(parent, config),
    m_baseCell(std::make_unique<VisiblyInvalidCell>(parent,config)),
    m_exptCell(std::make_unique<VisiblyInvalidCell>(parent,config)),
    m_exp(std::make_unique<TextCell>(parent, config, "^")),
    m_open(std::make_unique<TextCell>(parent, config, "(")),
    m_close(std::make_unique<TextCell>(parent, config, ")"))
{
  InitBitFields();
  m_open->SetStyle(TS_FUNCTION);
  m_close->SetStyle(TS_FUNCTION);
  m_exp->SetStyle(TS_FUNCTION);
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
}

ExptCell::ExptCell(const ExptCell &cell):
    ExptCell(cell.m_group, cell.m_configuration)
{
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
  if(cell.m_exptCell)
    SetPower(cell.m_exptCell->CopyList());
}

std::unique_ptr<Cell> ExptCell::Copy() const
{
  return std::make_unique<ExptCell>(*this);
}

void ExptCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {    
    wxPoint bs, pw;
    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs);

    point.x += m_baseCell->GetFullWidth() - MC_TEXT_PADDING;
    point.y -= m_expt_yoffset;
    m_exptCell->DrawList(point);
  }
}

void ExptCell::SetPower(std::unique_ptr<Cell> &&power)
{
  if (!power)
    return;
  m_exptCell = std::move(power);

  if (!m_exptCell->IsCompound())
  {
    m_open->Hide();
    m_close->Hide();
  }
}

void ExptCell::SetBase(std::unique_ptr<Cell> &&base)
{
  if (!base)
    return;
  m_baseCell = std::move(base);
}

void ExptCell::Recalculate(AFontSize fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateList(fontsize);
  if (m_isBrokenIntoLines)
    m_exptCell->RecalculateList(fontsize);
  else
    m_exptCell->RecalculateList({ MC_MIN_SIZE, fontsize - EXPT_DEC });
  m_exp->RecalculateList(fontsize);
  m_open->RecalculateList(fontsize);
  m_close->RecalculateList(fontsize);
  
  
  if (m_isBrokenIntoLines)
  {
    m_height = m_width = m_center = 0;
  }
  else
  {
    m_width = m_baseCell->GetFullWidth() + m_exptCell->GetFullWidth() -
      MC_TEXT_PADDING;
    m_expt_yoffset = m_exptCell->GetMaxDrop() + PowRise();
    
    m_height = m_baseCell->GetHeightList();
    m_center = m_baseCell->GetCenterList();
    
    int baseHeight = m_baseCell->GetHeightList() - m_baseCell->GetMaxDrop();
    int exptHeight = m_exptCell->GetHeightList() - m_exptCell->GetMaxDrop() + m_expt_yoffset;
    
    if(baseHeight < exptHeight)
    {
      m_height += exptHeight - baseHeight;
      m_center += exptHeight - baseHeight;
    }
    else
      m_expt_yoffset += baseHeight - exptHeight;
  }
  Cell::Recalculate(fontsize);
}

wxString ExptCell::ToString() const
{
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  wxString s = m_baseCell->ListToString() + wxT("^");
  if (m_isMatrix)
    s += wxT("^");
  if (m_exptCell->IsCompound())
    s += wxT("(") + m_exptCell->ListToString() + wxT(")");
  else
    s += m_exptCell->ListToString();
  return s;
}

wxString ExptCell::ToMatlab() const
{
  if (m_altCopyText != wxEmptyString)
	return m_altCopyText;
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  wxString s = m_baseCell->ListToMatlab() + wxT("^");
  if (m_isMatrix)
	s += wxT("^");
  if (m_exptCell->IsCompound())
	s += wxT("(") + m_exptCell->ListToMatlab() + wxT(")");
  else
	s += m_exptCell->ListToMatlab();
  return s;
}

wxString ExptCell::ToTeX() const
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  wxString s = wxT("{{") + m_baseCell->ListToTeX() + wxT("}^{") +
               m_exptCell->ListToTeX() + wxT("}}");
  return s;
}

wxString ExptCell::GetDiffPart() const
{
  wxString s(wxT(","));
  if (m_baseCell != NULL)
    s += m_baseCell->ListToString();
  s += wxT(",");
  if (m_exptCell != NULL)
    s += m_exptCell->ListToString();
  return s;
}

wxString ExptCell::ToMathML() const
{
  return wxT("<msup>") +
         m_baseCell->ListToMathML() +
         m_exptCell->ListToMathML() +
         wxT("</msup>\n");
//  return wxT("<apply><power/>") + m_baseCell->ListToMathML() + m_exptCell->ListToMathML() + wxT("</apply>");
}

wxString ExptCell::ToOMML() const
{
  return wxT("<m:sSup><m:e>") + m_baseCell->ListToOMML() + wxT("</m:e><m:sup>") +
         m_exptCell->ListToOMML() + wxT("</m:sup></m:sSup>\n");
}

wxString ExptCell::ToXML() const
{
//  if (m_isBrokenIntoLines)
//    return wxEmptyString;
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<e") + flags + wxT("><r>") + m_baseCell->ListToXML() + _T("</r><r>") +
         m_exptCell->ListToXML() + _T("</r></e>");
}

bool ExptCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    Cell::BreakUp();
    m_isBrokenIntoLines = true;
    m_baseCell->last()->SetNextToDraw(m_exp);
    m_exp->SetNextToDraw(m_open);
    m_open->SetNextToDraw(m_exptCell);
    m_exptCell->last()->SetNextToDraw(m_close);
    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_baseCell;
    ResetCellListSizes();
    m_height = 0;
    m_center = 0;
    return true;
  }
  return false;
}
