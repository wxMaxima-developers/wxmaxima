// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ExptCell

  ExptCell is the MathCell type that represents exponents.
 */

#include "ExptCell.h"
#include "TextCell.h"

#define EXPT_DEC 2

ExptCell::ExptCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_cellPointers = cellPointers;
  m_last1 = NULL;
  m_last2 = NULL;
  m_baseCell = NULL;
  m_powCell = NULL;
  m_isMatrix = false;
  m_exp = new TextCell(parent, config, cellPointers, wxT("^"));
  m_open = new TextCell(parent, config, cellPointers, wxT("("));
  m_open->DontEscapeOpeningParenthesis();
  m_close = new TextCell(parent, config, cellPointers, wxT(")"));
}

void ExptCell::SetGroup(MathCell *parent)
{
  m_group = parent;
  if (m_baseCell != NULL)
    m_baseCell->SetGroupList(parent);
  if (m_powCell != NULL)
    m_powCell->SetGroupList(parent);
  if (m_open != NULL)
    m_open->SetGroupList(parent);
  if (m_close != NULL)
    m_close->SetGroupList(parent);
}

MathCell *ExptCell::Copy()
{
  ExptCell *tmp = new ExptCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_baseCell->CopyList());
  tmp->SetPower(m_powCell->CopyList());
  tmp->m_isBroken = m_isBroken;
  tmp->m_open->DontEscapeOpeningParenthesis();

  return tmp;
}

ExptCell::~ExptCell()
{
  wxDELETE(m_baseCell);
  wxDELETE(m_powCell);
  wxDELETE(m_exp);
  wxDELETE(m_open);
  wxDELETE(m_close);
  m_baseCell = m_powCell = m_exp = m_open = m_close = NULL;
  MarkAsDeleted();
}

std::list<MathCell *> ExptCell::GetInnerCells()
{
  std::list<MathCell *> innerCells;
  if(m_baseCell)
    innerCells.push_back(m_baseCell);
  if(m_powCell)
    innerCells.push_back(m_powCell);
  if(m_exp)
    innerCells.push_back(m_exp);
  if(m_open)
    innerCells.push_back(m_open);
  if(m_close)
    innerCells.push_back(m_close );
  return innerCells;
}


void ExptCell::SetPower(MathCell *power)
{
  if (power == NULL)
    return;
  wxDELETE(m_powCell);
  m_powCell = power;

  if (!m_powCell->IsCompound())
  {
    m_open->m_isHidden = true;
    m_close->m_isHidden = true;
  }

  m_last2 = power;
  if (m_last2 != NULL)
    while (m_last2->m_next != NULL)
      m_last2 = m_last2->m_next;
}

void ExptCell::SetBase(MathCell *base)
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
  if (m_isBroken)
    m_powCell->RecalculateWidthsList(fontsize);
  else
    m_powCell->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - EXPT_DEC));
  m_width = m_baseCell->GetFullWidth() + m_powCell->GetFullWidth() -
            Scale_Px(MC_TEXT_PADDING);
  m_exp->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  ResetData();
}

void ExptCell::RecalculateHeight(int fontsize)
{
  m_baseCell->RecalculateHeightList(fontsize);
  if (m_isBroken)
    m_powCell->RecalculateHeightList(fontsize);
  else
    m_powCell->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - EXPT_DEC));
  m_height = m_baseCell->GetMaxHeight() + m_powCell->GetMaxHeight() -
             Scale_Px((8 * fontsize) / 10 + MC_EXP_INDENT);
  m_center = m_powCell->GetMaxHeight() + m_baseCell->GetMaxCenter() -
             Scale_Px((8 * fontsize) / 10 + MC_EXP_INDENT);
  m_exp->RecalculateHeightList(fontsize);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  if (m_isBroken)
  {
    m_height = MAX(m_baseCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_baseCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
}

void ExptCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    
    MathCell::Draw(point, fontsize);
    wxPoint bs, pw;
    bs.x = point.x;
    bs.y = point.y;
    m_baseCell->DrawList(bs, fontsize);

    pw.x = point.x + m_baseCell->GetFullWidth() - Scale_Px(MC_TEXT_PADDING);
    pw.y = point.y - m_baseCell->GetMaxCenter() - m_powCell->GetMaxHeight()
           + m_powCell->GetMaxCenter() +
           Scale_Px((8 * fontsize) / 10 + MC_EXP_INDENT);
    m_powCell->DrawList(pw, MAX(MC_MIN_SIZE, fontsize - EXPT_DEC));
  }
}

wxString ExptCell::ToString()
{
  if (m_isBroken)
    return wxEmptyString;
  wxString s = m_baseCell->ListToString() + wxT("^");
  if (m_isMatrix)
    s += wxT("^");
  if (m_powCell->IsCompound())
    s += wxT("(") + m_powCell->ListToString() + wxT(")");
  else
    s += m_powCell->ListToString();
  return s;
}

wxString ExptCell::ToTeX()
{
  if (m_isBroken)
    return wxEmptyString;
  wxString s = wxT("{{") + m_baseCell->ListToTeX() + wxT("}^{") +
               m_powCell->ListToTeX() + wxT("}}");
  return s;
}

wxString ExptCell::GetDiffPart()
{
  wxString s(wxT(","));
  if (m_baseCell != NULL)
    s += m_baseCell->ListToString();
  s += wxT(",");
  if (m_powCell != NULL)
    s += m_powCell->ListToString();
  return s;
}

wxString ExptCell::ToMathML()
{
  return wxT("<msup>") +
         m_baseCell->ListToMathML() +
         m_powCell->ListToMathML() +
         wxT("</msup>\n");
//  return wxT("<apply><power/>") + m_baseCell->ListToMathML() + m_powCell->ListToMathML() + wxT("</apply>");
}

wxString ExptCell::ToOMML()
{
  return wxT("<m:sSup><m:e>") + m_baseCell->ListToOMML() + wxT("</m:e><m:sup>") +
         m_powCell->ListToOMML() + wxT("</m:sup></m:sSup>\n");
}

wxString ExptCell::ToXML()
{
//  if (m_isBroken)
//    return wxEmptyString;
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<e") + flags + wxT("><r>") + m_baseCell->ListToXML() + _T("</r><r>") +
         m_powCell->ListToXML() + _T("</r></e>");
}

bool ExptCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
    m_baseCell->m_previousToDraw = this;
    wxASSERT_MSG(m_last1 != NULL, _("Bug: No last cell in the base of an exptCell!"));
    if (m_last1 != NULL)
    {
      m_last1->m_nextToDraw = m_exp;
      m_exp->m_previousToDraw = m_last1;
    }
    m_exp->m_nextToDraw = m_open;
    m_open->m_previousToDraw = m_exp;
    m_open->m_nextToDraw = m_powCell;
    m_powCell->m_previousToDraw = m_open;
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
    m_height = MAX(m_baseCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_baseCell->GetMaxCenter(), m_open->GetMaxCenter());
    return true;
  }
  return false;
}

void ExptCell::Unbreak()
{
  if (m_isBroken)
  {
    m_baseCell->UnbreakList();
    m_powCell->UnbreakList();
  }
  MathCell::Unbreak();
}
