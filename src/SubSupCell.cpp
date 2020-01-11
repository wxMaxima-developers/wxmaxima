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
}

SubSupCell::SubSupCell(const SubSupCell &cell):
 SubSupCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  CopyCommonData(cell);
  if(cell.m_baseCell)
    SetBase(cell.m_baseCell->CopyList());
  if(cell.m_postSubCell)
    SetIndex(cell.m_postSubCell->CopyList());
  if(cell.m_postSupCell)
    SetExponent(cell.m_postSupCell->CopyList());
  if(cell.m_preSubCell)
    SetPreSub(cell.m_preSubCell->CopyList());
  if(cell.m_preSupCell)
    SetPreSup(cell.m_preSupCell->CopyList());
}

SubSupCell::~SubSupCell()
{
  MarkAsDeleted();
}

std::list<std::shared_ptr<Cell>> SubSupCell::GetInnerCells()
{
  std::list<std::shared_ptr<Cell>> innerCells;
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
  m_preSupCell = std::shared_ptr<Cell>(index);
  m_innerCellList.push_back(index);
}

void SubSupCell::SetPreSub(Cell *index)
{
  if (index == NULL)
    return;
  m_preSubCell = std::shared_ptr<Cell>(index);
  m_innerCellList.push_back(index);
}

void SubSupCell::SetIndex(Cell *index)
{
  if (index == NULL)
    return;
  m_postSubCell = std::shared_ptr<Cell>(index);
}

void SubSupCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  m_baseCell = std::shared_ptr<Cell>(base);
}

void SubSupCell::SetExponent(Cell *expt)
{
  if (expt == NULL)
    return;
  m_postSupCell = std::shared_ptr<Cell>(expt);
}

void SubSupCell::RecalculateWidths(int fontsize)
{
  m_baseCell->RecalculateWidthsList(fontsize);

  int preWidth = 0;
  int postWidth = 0;

  if(m_postSubCell)
  {
    postWidth = m_postSubCell->GetFullWidth();
    m_postSubCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  }
  if(m_postSupCell)
  {
    m_postSupCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    postWidth = wxMax(postWidth, m_postSupCell->GetFullWidth());
  }
  if(m_preSubCell)
  {
    m_preSubCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    preWidth = m_preSubCell->GetFullWidth();
  }
  if(m_preSupCell)
  {
    m_preSupCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    preWidth = wxMax(preWidth, m_preSupCell->GetFullWidth());
  }

  m_width = preWidth + m_baseCell->GetFullWidth() + postWidth;
  Cell::RecalculateWidths(fontsize);
}

void SubSupCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_baseCell->RecalculateHeightList(fontsize);

  int subHeight = 0;
  if(m_preSubCell)
  {
    m_preSubCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    subHeight = m_preSubCell->GetHeightList();
  }
  if(m_postSubCell)
  {
    m_postSubCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    subHeight = wxMax(subHeight, m_postSubCell->GetHeightList());
  }
  
  int supHeight = 0;
  if(m_preSupCell)
  {
    m_preSupCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    supHeight = m_preSupCell->GetHeightList();
  }
  if(m_postSupCell)
  {
    m_postSupCell->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    supHeight = wxMax(supHeight, m_postSupCell->GetHeightList());
  }
  
  m_height = m_baseCell->GetHeightList() + subHeight + supHeight -
             2 * Scale_Px(.8 * fontsize + MC_EXP_INDENT);

  m_center = supHeight +
    m_baseCell->GetCenterList() -
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
        m_preSubCell->GetCenterList() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_preSubCell->DrawList(presub);
    }

    if(m_preSupCell)
    {
      wxPoint presup = point;
      presup.x -= m_preSupCell->GetFullWidth();
      presup.y -= m_baseCell->GetCenterList() - m_preSupCell->GetHeightList()
        + m_preSupCell->GetCenterList() +
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);;
      m_preSupCell->DrawList(presup);
    }
    
    m_baseCell->DrawList(point);

    if(m_postSubCell)
    {
      in.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(2);
      in.y = point.y + m_baseCell->GetMaxDrop() +
        m_postSubCell->GetCenterList() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_postSubCell->DrawList(in);
    }
    
    if(m_postSupCell)
    {
      in.y = point.y - m_baseCell->GetCenterList() - m_postSupCell->GetHeightList()
        + m_postSupCell->GetCenterList() +
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_postSupCell->DrawList(in);
    }
  }
}

wxString SubSupCell::ToString()
{
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s;
  if (m_baseCell->IsCompound())
    s += wxT("(") + m_baseCell->ListToString() + wxT(")");
  else
    s += m_baseCell->ListToString();
  if(m_innerCellList.empty())
  {
    s += wxT("[") + m_postSubCell->ListToString() + wxT("]");
    s += wxT("^");
    if (m_postSupCell->IsCompound())
      s += wxT("(");
    s += m_postSupCell->ListToString();
    if (m_postSupCell->IsCompound())
      s += wxT(")");
  }
  else
  {
    std::list<Cell *> innerCells = m_innerCellList;
    while(!innerCells .empty())
    {
      s += "[" + innerCells.front()->ListToString() + "]";
      innerCells.pop_front();
    }
  }
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

  if (m_altCopyText != wxEmptyString)
    flags += wxT(" altCopy=\"") + XMLescape(m_altCopyText) + wxT("\"");
  
  return _T("<ie") + flags +wxT("><r>") + m_baseCell->ListToXML()
         + _T("</r><r>") + m_postSubCell->ListToXML()
         + _T("</r><r>") + m_postSupCell->ListToXML()
         + _T("</r></ie>");
}
