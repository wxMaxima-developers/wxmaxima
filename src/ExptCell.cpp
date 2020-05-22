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

#define EXPT_DEC 2

ExptCell::ExptCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_baseCell(new TextCell(parent, config, cellPointers)),
    m_exptCell(new TextCell(parent, config, cellPointers)),
    m_exp(new TextCell(parent, config, cellPointers, "^")),
    m_open(new TextCell(parent, config, cellPointers, "(")),
    m_close(new TextCell(parent, config, cellPointers, ")"))
{
  m_nextToDraw = NULL;
  m_open->SetStyle(TS_FUNCTION);
  m_close->SetStyle(TS_FUNCTION);
  m_exp->SetStyle(TS_FUNCTION);
  m_expt_yoffset = 0;
  m_base_last = m_baseCell.get();
  m_expt_last = m_exptCell.get();
  m_isMatrix = false;
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
}

ExptCell::ExptCell(const ExptCell &cell):
    ExptCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
  if(cell.m_exptCell)
    SetPower(cell.m_exptCell->CopyList());
}

ExptCell::~ExptCell()
{
  MarkAsDeleted();
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

void ExptCell::SetPower(Cell *power)
{
  if (!power)
    return;
  m_exptCell.reset(power);

  if (!m_exptCell->IsCompound())
  {
    m_open->m_isHidden = true;
    m_close->m_isHidden = true;
  }

  m_expt_last = power;
  if (m_expt_last != NULL)
    while (m_expt_last->m_next != NULL)
      m_expt_last = m_expt_last->m_next;
}

void ExptCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_baseCell.reset(base);

  m_base_last = base;
  if (m_base_last != NULL)
    while (m_base_last->m_next != NULL)
      m_base_last = m_base_last->m_next;
}

void ExptCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateWidthsList(fontsize);
  if (m_isBrokenIntoLines)
    m_exptCell->RecalculateWidthsList(fontsize);
  else
    m_exptCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - EXPT_DEC));
  m_width = m_baseCell->GetFullWidth() + m_exptCell->GetFullWidth() -
            MC_TEXT_PADDING;
  m_exp->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  if(m_isBrokenIntoLines)
    m_width = 0;
  Cell::RecalculateWidths(fontsize);
}

void ExptCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_baseCell->RecalculateHeightList(fontsize);
  if (m_isBrokenIntoLines)
    m_exptCell->RecalculateHeightList(fontsize);
  else
    m_exptCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - EXPT_DEC));

  m_exp->RecalculateHeightList(fontsize);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  
  if (m_isBrokenIntoLines)
  {
    m_height = wxMax(m_baseCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_baseCell->GetCenterList(), m_open->GetCenterList());
  }
  else
  {
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
  Cell::RecalculateHeight(fontsize);
}

wxString ExptCell::ToString()
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

wxString ExptCell::ToMatlab()
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

wxString ExptCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  wxString s = wxT("{{") + m_baseCell->ListToTeX() + wxT("}^{") +
               m_exptCell->ListToTeX() + wxT("}}");
  return s;
}

wxString ExptCell::GetDiffPart()
{
  wxString s(wxT(","));
  if (m_baseCell != NULL)
    s += m_baseCell->ListToString();
  s += wxT(",");
  if (m_exptCell != NULL)
    s += m_exptCell->ListToString();
  return s;
}

wxString ExptCell::ToMathML()
{
  return wxT("<msup>") +
         m_baseCell->ListToMathML() +
         m_exptCell->ListToMathML() +
         wxT("</msup>\n");
//  return wxT("<apply><power/>") + m_baseCell->ListToMathML() + m_exptCell->ListToMathML() + wxT("</apply>");
}

wxString ExptCell::ToOMML()
{
  return wxT("<m:sSup><m:e>") + m_baseCell->ListToOMML() + wxT("</m:e><m:sup>") +
         m_exptCell->ListToOMML() + wxT("</m:sup></m:sSup>\n");
}

wxString ExptCell::ToXML()
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
    m_isBrokenIntoLines = true;
    wxASSERT_MSG(m_base_last != NULL, _("Bug: No last cell in the base of an exptCell!"));
    if (m_base_last != NULL)
      m_base_last->SetNextToDraw(m_exp.get());
    m_exp->SetNextToDraw(m_open.get());
    m_open->SetNextToDraw(m_exptCell.get());
    wxASSERT_MSG(m_expt_last != NULL, _("Bug: No last cell in an exponent of an exptCell!"));
    if (m_expt_last != NULL)
      m_expt_last->SetNextToDraw(m_close.get());
    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_baseCell.get();
    m_height = 1;
    m_center = 1;
    ResetData();    
    return true;
  }
  return false;
}
