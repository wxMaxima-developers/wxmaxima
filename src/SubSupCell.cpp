// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
  This file defines the class SubSupCell

  SubSubCell is the Cell type that represents a math element with subscript and
  superscript.
 */

#include "SubSupCell.h"
#include <wx/config.h>
#include "wx/config.h"

#define SUBSUP_DEC 3

SubSupCell::SubSupCell(Cell *parent, Configuration **config,CellPointers *cellPointers) : Cell(parent, config, cellPointers)
{
  m_baseCell = NULL;
  m_postSubCell = NULL;
  m_postSupCell = NULL;
  m_preSubCell = NULL;
  m_preSupCell = NULL;
}

SubSupCell& SubSupCell::operator=(const SubSupCell &other)
{
  if(&other == this)
    return *this;
  Cell::operator=(other);
  if(other.m_baseCell)
    SetBase(other.m_baseCell->CopyList());
  if(other.m_postSubCell)
    SetIndex(other.m_postSubCell->CopyList());
  if(other.m_postSupCell)
    SetExponent(other.m_postSupCell->CopyList());
  if(other.m_preSubCell)
    SetPreSub(other.m_preSubCell->CopyList());
  if(other.m_preSupCell)
    SetPreSup(other.m_preSupCell->CopyList());
  return *this;
}

SubSupCell::SubSupCell(const SubSupCell &cell):
 SubSupCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  *this = cell;
}

SubSupCell::~SubSupCell()
{
  wxDELETE(m_preSubCell);
  m_preSubCell = NULL;
  wxDELETE(m_preSupCell);
  m_preSupCell = NULL;
  wxDELETE(m_baseCell);
  m_baseCell = NULL;
  wxDELETE(m_postSubCell);
  m_postSubCell = NULL;
  wxDELETE(m_postSupCell);
  m_postSupCell = NULL;
  MarkAsDeleted();
}

std::list<Cell *> SubSupCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_postSubCell)
    innerCells.push_back(m_postSubCell);
  if(m_postSupCell)
    innerCells.push_back(m_postSupCell);
  if(m_preSubCell)
    innerCells.push_back(m_preSubCell);
  if(m_preSupCell)
    innerCells.push_back(m_preSupCell);
  return innerCells;
}

void SubSupCell::SetPreSup(Cell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_preSupCell);
  m_preSupCell = index;
  m_innerCellList.push_back(index);
}

void SubSupCell::SetPreSub(Cell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_preSubCell);
  m_preSubCell = index;
  m_innerCellList.push_back(index);
}

void SubSupCell::SetIndex(Cell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_postSubCell);
  m_postSubCell = index;
  m_innerCellList.push_back(index);
}

void SubSupCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
  m_innerCellList.push_back(base);
}

void SubSupCell::SetExponent(Cell *expt)
{
  if (expt == NULL)
    return;
  wxDELETE(m_postSupCell);
  m_postSupCell = expt;
}

void SubSupCell::RecalculateWidths(int fontsize)
{
  m_baseCell->RecalculateWidthsList(fontsize);
  if(m_postSubCell)    
    m_postSubCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_postSupCell)    
    m_postSupCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_preSubCell)    
    m_preSubCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_preSupCell)    
    m_preSupCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));

  int preWidth = 0;
  int postWidth = 0;

  if(m_postSubCell)
    postWidth = m_postSubCell->GetFullWidth();
  if(m_postSupCell)
    postWidth = wxMax(postWidth, m_postSupCell->GetFullWidth());

  if(m_preSubCell)
    preWidth = m_preSubCell->GetFullWidth();
  if(m_preSupCell)
    preWidth = wxMax(preWidth, m_preSupCell->GetFullWidth());

  m_width = preWidth + m_baseCell->GetFullWidth() + postWidth;
  Cell::RecalculateWidths(fontsize);
}

void SubSupCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_baseCell->RecalculateHeightList(fontsize);
  if(m_postSubCell)
    m_postSubCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_postSupCell)
    m_postSupCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_preSubCell)
    m_preSubCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  if(m_preSupCell)
    m_preSupCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));

  int subHeight = 0;
  if(m_preSubCell)
    subHeight = m_preSubCell->GetMaxHeight();
  if(m_postSubCell)
    subHeight = wxMax(subHeight, m_postSubCell->GetMaxHeight());

  int supHeight = 0;
  if(m_preSupCell)
    supHeight = m_preSupCell->GetMaxHeight();
  if(m_postSupCell)
    supHeight = wxMax(supHeight, m_postSupCell->GetMaxHeight());

  m_height = m_baseCell->GetMaxHeight() + subHeight + supHeight -
             2 * Scale_Px(.8 * fontsize + MC_EXP_INDENT);

  m_center = supHeight +
    m_baseCell->GetMaxCenter() -
    Scale_Px(.8 * fontsize + MC_EXP_INDENT);
}

void SubSupCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    wxPoint in;

    int preWidth = 0;
    
    if(m_preSubCell)
      preWidth = m_preSubCell->GetFullWidth();
    if(m_preSupCell)
      preWidth = wxMax(preWidth, m_preSupCell->GetFullWidth());

    point.x += preWidth;

    if(m_preSubCell)
    {
      wxPoint presub = point;
      presub.x -= m_preSubCell->GetFullWidth();
      presub.y += m_baseCell->GetMaxDrop() +
        m_preSubCell->GetMaxCenter() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_preSubCell->DrawList(presub);
    }

    if(m_preSupCell)
    {
      wxPoint presup = point;
      presup.x -= m_preSupCell->GetFullWidth();
      presup.y -= m_baseCell->GetMaxCenter() - m_preSupCell->GetMaxHeight()
        + m_preSupCell->GetMaxCenter() +
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);;
      m_preSupCell->DrawList(presup);
    }
    
    m_baseCell->DrawList(point);

    if(m_postSubCell)
    {
      in.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(2);
      in.y = point.y + m_baseCell->GetMaxDrop() +
        m_postSubCell->GetMaxCenter() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_postSubCell->DrawList(in);
    }
    
    if(m_postSupCell)
    {
      in.y = point.y - m_baseCell->GetMaxCenter() - m_postSupCell->GetMaxHeight()
        + m_postSupCell->GetMaxCenter() +
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_postSupCell->DrawList(in);
    }
  }
}

wxString SubSupCell::ToString()
{
  wxString s;
  if (m_baseCell->IsCompound())
    s += wxT("(") + m_baseCell->ListToString() + wxT(")");
  else
    s += m_baseCell->ListToString();
  s += wxT("[") + m_postSubCell->ListToString() + wxT("]");
  s += wxT("^");
  if (m_postSupCell->IsCompound())
    s += wxT("(");
  s += m_postSupCell->ListToString();
  if (m_postSupCell->IsCompound())
    s += wxT(")");
  return s;
}

wxString SubSupCell::ToMatlab()
{
  wxString s;
  if (m_baseCell->IsCompound())
	s += wxT("(") + m_baseCell->ListToMatlab() + wxT(")");
  else
	s += m_baseCell->ListToMatlab();
  s += wxT("[") + m_postSubCell->ListToMatlab() + wxT("]");
  s += wxT("^");
  if (m_postSupCell->IsCompound())
	s += wxT("(");
  s += m_postSupCell->ListToMatlab();
  if (m_postSupCell->IsCompound())
	s += wxT(")");
  return s;
}

wxString SubSupCell::ToTeX()
{
  wxConfigBase *config = wxConfig::Get();

  bool TeXExponentsAfterSubscript = false;

  config->Read(wxT("TeXExponentsAfterSubscript"), &TeXExponentsAfterSubscript);

  wxString s;

  if (TeXExponentsAfterSubscript)
    s = wxT("{{{") + m_baseCell->ListToTeX() + wxT("}_{") +
        m_postSubCell->ListToTeX() + wxT("}}^{") +
        m_postSupCell->ListToTeX() + wxT("}}");
  else
    s = wxT("{{") + m_baseCell->ListToTeX() + wxT("}_{") +
        m_postSubCell->ListToTeX() + wxT("}^{") +
        m_postSupCell->ListToTeX() + wxT("}}");

  return s;
}

wxString SubSupCell::ToMathML()
{
  return wxT("<msubsup>") +
         m_baseCell->ListToMathML() +
         m_postSubCell->ListToMathML() +
         m_postSupCell->ListToMathML() +
         wxT("</msubsup>\n");
}

wxString SubSupCell::ToOMML()
{
  return wxT("<m:sSubSup><m:e>") +
         m_baseCell->ListToOMML() + wxT("</m:e><m:sup>") +
         m_postSubCell->ListToOMML() + wxT("</m:sup><m:sub>") +
         m_postSupCell->ListToOMML() +
         wxT("</m:sub></m:sSubSup>\n");
}

wxString SubSupCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return _T("<ie") + flags +wxT("><r>") + m_baseCell->ListToXML()
         + _T("</r><r>") + m_postSubCell->ListToXML()
         + _T("</r><r>") + m_postSupCell->ListToXML()
         + _T("</r></ie>");
}
