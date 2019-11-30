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
  This file defines the class ExptCell

  ExptCell is the Cell type that represents exponents.
 */

#include "ExptCell.h"
#include "TextCell.h"

#define EXPT_DEC 2

ExptCell::ExptCell(Cell *parent, Configuration **config, CellPointers *cellpointers) : Cell(parent, config, cellpointers)
{
  m_last1 = NULL;
  m_last2 = NULL;
  m_baseCell = NULL;
  m_exptCell = NULL;
  m_isMatrix = false;
  m_exp = new TextCell(parent, config, cellpointers, wxT("^"));
  m_open = new TextCell(parent, config, cellpointers, wxT("("));
  m_open->DontEscapeOpeningParenthesis();
  m_close = new TextCell(parent, config, cellpointers, wxT(")"));
}

ExptCell::ExptCell(const ExptCell &cell):
  ExptCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  CopyCommonData(cell);
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
  if(cell.m_exptCell)
    SetPower(cell.m_exptCell->CopyList());
}

ExptCell::~ExptCell()
{
  wxDELETE(m_baseCell);
  wxDELETE(m_exptCell);
  wxDELETE(m_exp);
  wxDELETE(m_open);
  wxDELETE(m_close);
  m_baseCell = m_exptCell = m_exp = m_open = m_close = NULL;
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
    // Raise the exponent to share the upper border with the base cell
    if(m_exptCell->GetMaxHeight() < m_baseCell->GetMaxHeight())
      point.y -= m_baseCell->GetMaxHeight() - m_exptCell->GetMaxHeight();
    // Raise the exponent a little bit more so it looks like an exponent
    point.y -= PowRise();
    m_exptCell->DrawList(point);
  }
}

std::list<Cell *> ExptCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_exptCell)
    innerCells.push_back(m_exptCell);
  if(m_exp)
    innerCells.push_back(m_exp);
  if(m_open)
    innerCells.push_back(m_open);
  if(m_close)
    innerCells.push_back(m_close );
  return innerCells;
}


void ExptCell::SetPower(Cell *power)
{
  if (power == NULL)
    return;
  wxDELETE(m_exptCell);
  m_exptCell = power;

  if (!m_exptCell->IsCompound())
  {
    m_open->m_isHidden = true;
    m_close->m_isHidden = true;
  }

  m_last2 = power;
  if (m_last2 != NULL)
    while (m_last2->m_next != NULL)
      m_last2 = m_last2->m_next;
}

void ExptCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;

  m_last1 = base;
  if (m_last1 != NULL)
    while (m_last1->m_next != NULL)
      m_last1 = m_last1->m_next;
}

void ExptCell::RecalculateWidths(int fontsize)
{
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
  Cell::RecalculateHeight(fontsize);
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
    m_height = wxMax(m_baseCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = wxMax(m_baseCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
  else
  {
    int expt_yoffset = 0;

    if(m_exptCell->GetMaxCenter() < m_baseCell->GetMaxCenter())
      expt_yoffset += m_baseCell->GetMaxHeight() - m_exptCell->GetMaxHeight();
    // Raise the exponent a little bit more so it looks like an exponent
    expt_yoffset += PowRise();

    m_height = m_baseCell->GetMaxHeight();
    m_center = m_baseCell->GetMaxCenter();

    if(expt_yoffset + m_exptCell->GetMaxCenter() - m_baseCell->GetMaxCenter() > 0)
    m_height += expt_yoffset + m_exptCell->GetMaxCenter() - m_baseCell->GetMaxCenter();
    m_center += expt_yoffset + m_exptCell->GetMaxCenter() - m_baseCell->GetMaxCenter();
  }
  
  
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
    m_baseCell->m_previousToDraw = this;
    wxASSERT_MSG(m_last1 != NULL, _("Bug: No last cell in the base of an exptCell!"));
    if (m_last1 != NULL)
    {
      m_last1->m_nextToDraw = m_exp;
      m_exp->m_previousToDraw = m_last1;
    }
    m_exp->m_nextToDraw = m_open;
    m_open->m_previousToDraw = m_exp;
    m_open->m_nextToDraw = m_exptCell;
    m_exptCell->m_previousToDraw = m_open;
    wxASSERT_MSG(m_last2 != NULL, _("Bug: No last cell in an exponent of an exptCell!"));
    if (m_last2 != NULL)
    {
      m_last2->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last2;
    }
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_baseCell;
    ResetData();    
    m_height = wxMax(m_baseCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = wxMax(m_baseCell->GetMaxCenter(), m_open->GetMaxCenter());
    return true;
  }
  return false;
}

void ExptCell::Unbreak()
{
  if (m_isBrokenIntoLines)
  {
    m_baseCell->UnbreakList();
    m_exptCell->UnbreakList();
  }
  Cell::Unbreak();
}
