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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class SubSupCell

  SubSubCell is the Cell type that represents a math element with subscript and
  superscript.
 */

#include "SubSupCell.h"
#include "TextCell.h"
#include <wx/config.h>
#include "wx/config.h"

#define SUBSUP_DEC 3

SubSupCell::SubSupCell(Cell *parent, Configuration **config,CellPointers *cellPointers) :
    Cell(parent, config, cellPointers)
{
  m_nextToDraw = NULL;
}

SubSupCell::SubSupCell(const SubSupCell &cell):
    SubSupCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
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

void SubSupCell::SetPreSup(Cell *index)
{
  if (!index)
    return;
  wxASSERT(!m_preSupCell);
  m_preSupCell.reset(index);
  m_scriptCells.push_back(m_preSupCell);
}

void SubSupCell::SetPreSub(Cell *index)
{
  if (!index)
    return;
  wxASSERT(!m_preSubCell);
  m_preSubCell.reset(index);
  m_scriptCells.push_back(m_preSubCell);
}

void SubSupCell::SetPostSup(Cell *index)
{
  if (!index)
    return;
  wxASSERT(!m_postSupCell);
  m_postSupCell.reset(index);
  m_scriptCells.push_back(m_postSupCell);
}

void SubSupCell::SetPostSub(Cell *index)
{
  if (!index)
    return;
  wxASSERT(!m_postSubCell);
  m_postSubCell.reset(index);
  m_scriptCells.push_back(m_postSubCell);
}

void SubSupCell::SetIndex(Cell *index)
{
  if (!index)
    return;
  m_postSubCell.reset(index);
}

void SubSupCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_baseCell.reset(base);
}

void SubSupCell::SetExponent(Cell *expt)
{
  if (!expt)
    return;
  m_postSupCell.reset(expt);
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
  {
    m_postSubCell->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
    postWidth = m_postSubCell->GetFullWidth();
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
  if(!NeedsRecalculation(fontsize))
    return;

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
  Cell::RecalculateHeight(fontsize);
}

void SubSupCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {
    wxPoint in;

    int preWidth = 0;
    
    if(m_preSubCell)
      preWidth = m_preSubCell->GetFullWidth();
    if(m_preSupCell)
      preWidth = wxMax(preWidth, m_preSupCell->GetFullWidth());

    if(m_preSubCell)
    {
      wxPoint presub = point;
      presub.x += preWidth - m_preSubCell->GetFullWidth();
      presub.y += m_baseCell->GetMaxDrop() +
        m_preSubCell->GetCenterList() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_preSubCell->DrawList(presub);
    }

    if(m_preSupCell)
    {
      wxPoint presup = point;
      presup.x += preWidth - m_preSupCell->GetFullWidth();
      presup.y -= m_baseCell->GetCenterList() + m_preSupCell->GetHeightList()
        - m_preSupCell->GetCenterList() -
        Scale_Px(.8 * m_fontSize + MC_EXP_INDENT);
      m_preSupCell->DrawList(presup);
    }

    point.x += preWidth;
    m_baseCell->DrawList(point);

    in.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(2);
    if(m_postSubCell)
    {
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
    s += "(" + m_baseCell->ListToString() + ")";
  else
    s += m_baseCell->ListToString();
  if (m_scriptCells.empty())
  {
    s += "[" + m_postSubCell->ListToString() + "]";
    s += "^";
    if (m_postSupCell->IsCompound())
      s += "(";
    s += m_postSupCell->ListToString();
    if (m_postSupCell->IsCompound())
      s += ")";
  }
  else
  {
    for (auto &cell : m_scriptCells)
      s += "[" + cell->ListToString() + "]";
  }
  return s;
}

wxString SubSupCell::ToMatlab()
{
  wxString s;
  if (m_baseCell->IsCompound())
	s += "(" + m_baseCell->ListToMatlab() + ")";
  else
	s += m_baseCell->ListToMatlab();
  if (m_scriptCells.empty())
  {
    s += "[" + m_postSubCell->ListToMatlab() + "]";
    s += "^";
    if (m_postSupCell->IsCompound())
      s += "(";
    s += m_postSupCell->ListToMatlab();
    if (m_postSupCell->IsCompound())
      s += ")";
  }
  else
  {
    s += "[";
    bool first = false;
    
    for (auto &cell : m_scriptCells)
    {
      if(!first)
        s += ";";
      first = true;
      s += cell->ListToMatlab();
    }
    s += "]";
  }
  return s;
}

wxString SubSupCell::ToTeX()
{
  wxConfigBase *config = wxConfig::Get();

  bool TeXExponentsAfterSubscript = false;

  config->Read("TeXExponentsAfterSubscript", &TeXExponentsAfterSubscript);

  wxString s;

  if (m_scriptCells.empty())
  {
    if (TeXExponentsAfterSubscript)
    {
      s = "{{{" + m_baseCell->ListToTeX() + "}";
      if(m_postSubCell)
        s += "_{" + m_postSubCell->ListToTeX() + "}";
      s += "}";
      if(m_postSupCell)
        s += "^{" + m_postSupCell->ListToTeX() + "}";
      s += "}";
    }
    else
    {
      s = "{{" + m_baseCell->ListToTeX() + "}";
      if(m_postSubCell)
        s +="_{" + m_postSubCell->ListToTeX() + "}";
      if(m_postSupCell)
        s += "^{" + m_postSupCell->ListToTeX() + "}";
      s += "}";
    }
  }
  else
  {
    if(m_preSupCell || m_preSubCell)
    {
      s = "{}";
      if(m_preSupCell)
        s += "^{" + m_preSupCell->ListToTeX() + "}";
      if(m_preSubCell)
        s += "^{" + m_preSubCell->ListToTeX() + "}";
    }
    s += "{" + m_baseCell->ListToTeX() + "}";
    if(m_postSupCell)
      s += "^{" + m_postSupCell->ListToTeX() + "}";
    if(m_postSubCell)
      s += "^{" + m_postSubCell->ListToTeX() + "}";
  }
  return s;
}

wxString SubSupCell::ToMathML()
{
  wxString retval;
  if (m_scriptCells.empty())
  {
    retval = "<msubsup>" +
      m_baseCell->ListToMathML();
    if(m_postSubCell)
      retval += m_postSubCell->ListToMathML();
    else
      retval += "<mrow/>";
    if(m_postSupCell)
      m_postSupCell->ListToMathML();
    else
      retval += "<mrow/>";
    retval += "</msubsup>\n";
  }
  else
  {
    retval = "<mmultiscripts>" + m_baseCell->ListToMathML();
    if(m_postSupCell || m_postSubCell)
    {
      if(m_postSubCell)
        retval += "<mrow>" + m_postSubCell->ListToMathML() + "</mrow>";
      else
        retval += "<none/>";
      if(m_postSupCell)
        retval += "<mrow>" + m_postSupCell->ListToMathML() + "</mrow>";
      else
        retval += "<none/>";
    }
    if(m_preSupCell || m_preSubCell)
    {
      retval += "<mprescripts/>";
      if(m_preSubCell)
        retval += "<mrow>" + m_preSubCell->ListToMathML() + "</mrow>";
      else
        retval += "<none/>";
      if(m_preSupCell)
        retval += "<mrow>" + m_preSupCell->ListToMathML() + "</mrow>";
      else
        retval += "<none/>";
    }
    retval += "</mmultiscripts>\n";
  }
  return retval;
}
wxString SubSupCell::ToOMML()
{
  wxString retval;
  if(m_preSupCell || m_preSubCell)
  {
    retval += "<m:sSubSup><m:e><m:r></m:r></m:e><m:sub>";
    if(m_preSubCell)
      retval += m_preSubCell->ListToOMML();
    else
      retval += "<m:r></m:r>";
    retval += "</m:sub><m:sup>";
    if(m_preSupCell)
      retval += m_preSupCell->ListToOMML();
    else
      retval += "<m:r></m:r>";
    retval += "</m:sup></m:sSubSup>\n";
  }
  retval += "<m:sSubSup><m:e>" + m_baseCell->ListToOMML() + "</m:e><m:sub>";
  if(m_postSubCell)
    retval += m_postSubCell->ListToOMML();
  else
    retval += "<m:r></m:r>";
  retval += "</m:sub><m:sup>";
  if(m_postSupCell)
    retval += m_postSupCell->ListToOMML();
  else
    retval += "<m:r></m:r>";
  retval += "</m:sup></m:sSubSup>\n";
  return retval;
}

wxString SubSupCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += " breakline=\"true\"";

  if (m_altCopyText != wxEmptyString)
    flags += " altCopy=\"" + XMLescape(m_altCopyText) + "\"";

  wxString retval;
  if (m_scriptCells.empty())
  {
    retval = "<ie" + flags + "><r>" + m_baseCell->ListToXML()
      + "</r><r>";
    if(m_postSubCell)
      retval += m_postSubCell->ListToXML();
    retval += "</r><r>";
    if(m_postSupCell)
      retval += m_postSupCell->ListToXML();
    retval += "</r></ie>";
  }
  else
  {
    retval = "<ie" + flags + "><r>" + m_baseCell->ListToXML() + "</r>";
    if(m_preSupCell)
      retval += "<r pos=\"presup\">" + m_preSupCell->ListToXML() + "</r>";
    if(m_preSubCell)
      retval += "<r pos=\"presub\">" + m_preSubCell->ListToXML() + "</r>";
    if(m_postSupCell)
      retval += "<r pos=\"postsup\">" + m_postSupCell->ListToXML() + "</r>";
    if(m_postSubCell)
      retval += "<r pos=\"postsub\">" + m_postSubCell->ListToXML() + "</r>";
    retval += "</ie>";
  }
  return retval;
}
