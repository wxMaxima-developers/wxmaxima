// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class SubSupCell

  SubSubCell is the MathCell type that represents a math element with subscript and
  superscript.
 */

#include "SubSupCell.h"
#include <wx/config.h>
#include "wx/config.h"

#define SUBSUP_DEC 3

SubSupCell::SubSupCell(MathCell *parent, Configuration **config,CellPointers *cellPointers) : MathCell(parent, config)
{
  m_cellPointers = cellPointers;
  m_baseCell = NULL;
  m_indexCell = NULL;
  m_exptCell = NULL;
}

void SubSupCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_baseCell != NULL)
    m_baseCell->SetParentList(parent);
  if (m_indexCell != NULL)
    m_indexCell->SetParentList(parent);
  if (m_exptCell != NULL)
    m_exptCell->SetParentList(parent);
}

MathCell *SubSupCell::Copy()
{
  SubSupCell *tmp = new SubSupCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetIndex(m_indexCell->CopyList());
  tmp->SetExponent(m_exptCell->CopyList());

  return tmp;
}

SubSupCell::~SubSupCell()
{
  wxDELETE(m_baseCell);
  m_baseCell = NULL;
  wxDELETE(m_indexCell);
  m_indexCell = NULL;
  wxDELETE(m_exptCell);
  m_exptCell = NULL;
  MarkAsDeleted();
}

void SubSupCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_baseCell, m_indexCell, m_exptCell);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}


void SubSupCell::SetIndex(MathCell *index)
{
  if (index == NULL)
    return;
  wxDELETE(m_indexCell);
  m_indexCell = index;
}

void SubSupCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_baseCell);
  m_baseCell = base;
}

void SubSupCell::SetExponent(MathCell *exp)
{
  if (exp == NULL)
    return;
  wxDELETE(m_exptCell);
  m_exptCell = exp;
}

void SubSupCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_baseCell->RecalculateWidthsList(fontsize);
  m_indexCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  m_exptCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  m_width = m_baseCell->GetFullWidth(scale) +
            MAX(m_indexCell->GetFullWidth(scale), m_exptCell->GetFullWidth(scale)) -
            SCALE_PX(2, configuration->GetScale());
  ResetData();
}

void SubSupCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();

  m_baseCell->RecalculateHeightList(fontsize);
  m_indexCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  m_exptCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));

  m_height = m_baseCell->GetMaxHeight() + m_indexCell->GetMaxHeight() +
             m_exptCell->GetMaxHeight() -
             2 * SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, configuration->GetScale());

  m_center = m_exptCell->GetMaxHeight() + m_baseCell->GetMaxCenter() -
             SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, scale);
}

void SubSupCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    Configuration *configuration = (*m_configuration);
    MathCell::Draw(point, fontsize);
    double scale = configuration->GetScale();
    wxPoint bs, in;

    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs, fontsize);

    in.x = point.x + m_baseCell->GetFullWidth(scale) - SCALE_PX(2, scale);
    in.y = point.y + m_baseCell->GetMaxDrop() +
           m_indexCell->GetMaxCenter() -
           SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, scale);
    m_indexCell->DrawList(in, MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));

    in.y = point.y - m_baseCell->GetMaxCenter() - m_exptCell->GetMaxHeight()
           + m_exptCell->GetMaxCenter() +
           SCALE_PX((8 * fontsize) / 10 + MC_EXP_INDENT, scale);
    m_exptCell->DrawList(in, MAX(MC_MIN_SIZE, fontsize - SUBSUP_DEC));
  }
}

wxString SubSupCell::ToString()
{
  wxString s;
  if (m_baseCell->IsCompound())
    s += wxT("(") + m_baseCell->ListToString() + wxT(")");
  else
    s += m_baseCell->ListToString();
  s += wxT("[") + m_indexCell->ListToString() + wxT("]");
  s += wxT("^");
  if (m_exptCell->IsCompound())
    s += wxT("(");
  s += m_exptCell->ListToString();
  if (m_exptCell->IsCompound())
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
        m_indexCell->ListToTeX() + wxT("}}^{") +
        m_exptCell->ListToTeX() + wxT("}}");
  else
    s = wxT("{{") + m_baseCell->ListToTeX() + wxT("}_{") +
        m_indexCell->ListToTeX() + wxT("}^{") +
        m_exptCell->ListToTeX() + wxT("}}");

  return s;
}

wxString SubSupCell::ToMathML()
{
  return wxT("<msubsup>") +
         m_baseCell->ListToMathML() +
         m_exptCell->ListToMathML() +
         m_indexCell->ListToMathML() +
         wxT("</msubsup>\n");
}

wxString SubSupCell::ToOMML()
{
  return wxT("<m:sSubSup><m:e>") +
         m_baseCell->ListToOMML() + wxT("</m:e><m:sup>") +
         m_indexCell->ListToOMML() + wxT("</m:sup><m:sub>") +
         m_exptCell->ListToOMML() +
         wxT("</m:sub></m:sSubSup>\n");
}

wxString SubSupCell::ToXML()
{
  return _T("<ie><r>") + m_baseCell->ListToXML()
         + _T("</r><r>") + m_exptCell->ListToXML()
         + _T("</r><r>") + m_indexCell->ListToXML()
         + _T("</r></ie>");
}

void SubSupCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_indexCell->ContainsRect(rect))
    m_indexCell->SelectRect(rect, first, last);
  else if (m_baseCell->ContainsRect(rect))
    m_baseCell->SelectRect(rect, first, last);
  else if (m_exptCell->ContainsRect(rect))
    m_exptCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
