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
  This file defines the class FracCell

  FracCell is the Cell type that represents fractions.
*/

#include "FracCell.h"
#include "TextCell.h"

#define FRAC_DEC 1

FracCell::FracCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
  Cell(parent, config, cellPointers),
  m_num(new TextCell(parent, config, cellPointers)),
  m_denom(new TextCell(parent, config, cellPointers)),
  m_numParenthesis(new ParenCell(m_group, m_configuration, m_cellPointers)),
  m_denomParenthesis(new ParenCell(m_group, m_configuration, m_cellPointers)),
  m_divide(new TextCell(parent, config, cellPointers, "/"))
{
  m_displayedNum = NULL;
  m_displayedDenom = NULL;
  m_divide->SetStyle(TS_VARIABLE);
  m_num_Last = NULL;
  m_denom_Last = NULL;
  m_fracStyle = FC_NORMAL;
  m_exponent = false;
  m_horizontalGapLeft = 0;
  m_horizontalGapRight = 0;
  m_protrusion = 0;
}

FracCell::FracCell(const FracCell &cell):
 FracCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  CopyCommonData(cell);
  if(cell.m_num)
    SetNum(cell.m_num->CopyList());
  if(cell.m_denom)
    SetDenom(cell.m_denom->CopyList());
  m_fracStyle = cell.m_fracStyle;
  m_exponent = cell.m_exponent;
  SetupBreakUps();
}

FracCell::~FracCell()
{
  MarkAsDeleted();
}

std::list<std::shared_ptr<Cell>> FracCell::GetInnerCells()
{
  std::list<std::shared_ptr<Cell>> innerCells;
  if(m_divide)
    innerCells.push_back(m_divide);
  // Contains m_num
  if(m_numParenthesis)
    innerCells.push_back(m_numParenthesis);
  // Contains m_denom
  if(m_denomParenthesis)
    innerCells.push_back(m_denomParenthesis);
  return innerCells;
}

void FracCell::SetNum(Cell *num)
{
  if (num == NULL)
    return;
  m_num = std::shared_ptr<Cell>(num);
  m_numParenthesis->SetInner(m_num);
  m_num_Last = num;
  SetupBreakUps();
}

void FracCell::SetDenom(Cell *denom)
{
  if (denom == NULL)
    return;
  m_denom = std::shared_ptr<Cell>(denom);
  m_denomParenthesis->SetInner(m_denom);
  SetupBreakUps();
}

void FracCell::RecalculateWidths(int fontsize)
{
  if(m_exponent || m_isBrokenIntoLines)
  {
    m_numParenthesis->RecalculateWidthsList(fontsize);
    m_denomParenthesis->RecalculateWidthsList(fontsize);
  }
  else
  {
    m_numParenthesis->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denomParenthesis->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  m_divide->RecalculateWidths(fontsize);
  
  if (m_exponent)
  {
    m_protrusion = m_horizontalGapLeft = m_horizontalGapRight = 0;
    m_width = m_num->GetWidth() + m_denom->GetWidth() + m_divide->GetWidth();
  }
  else
  {
    if(m_isBrokenIntoLines)
      m_width = 0;
    else
    {
      m_protrusion = Scale_Px((*m_configuration)->GetMathFontSize() / 3);
      
      // We want half a space's widh of blank space to separate us from the
      // next minus.
      
      if (((m_previous != NULL) && (m_previous->ToString().EndsWith(wxT("-")))))
        m_horizontalGapLeft = m_protrusion;
      else
        m_horizontalGapLeft = 0;
      
      if (((m_next != NULL) && (m_next->ToString().StartsWith(wxT("-")))))
        m_horizontalGapRight = m_protrusion;
      else
        m_horizontalGapRight = 0;
      
      m_width = wxMax(m_num->GetFullWidth(), m_denom->GetFullWidth()) +
        2 * m_protrusion + m_horizontalGapLeft + m_horizontalGapRight;
    }
  }
  Cell::RecalculateWidths(fontsize);
}

void FracCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  if(m_exponent || m_isBrokenIntoLines)
  {
    m_numParenthesis->RecalculateHeightList(fontsize);
    m_denomParenthesis->RecalculateHeightList(fontsize);
  }
  else
  {
    m_numParenthesis->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denomParenthesis->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  m_divide->RecalculateHeight(fontsize);

  if(m_isBrokenIntoLines)
  {
    m_height = m_num->GetMaxHeight();
    m_center = m_num->GetMaxCenter();
  }
  else
  {
    if (!m_exponent)
    {
      m_height = m_num->GetMaxHeight() + m_denom->GetMaxHeight() +
        Scale_Px(4);
      m_center = m_num->GetMaxHeight() + Scale_Px(2);
    }
    else
    {
      m_height = wxMax(m_num->GetMaxHeight(), m_denom->GetMaxHeight());
      m_center = wxMax(m_num->GetMaxCenter(), m_denom->GetMaxCenter());
    }
  }
}

void FracCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    Configuration *configuration = (*m_configuration);
    
    wxDC *dc = configuration->GetDC();
    wxPoint num, denom;

    if(m_isBrokenIntoLines)
      return;
    
    if (m_exponent)
    {
      num = point;
      wxPoint divide(point);
      divide.x += m_num->GetFullWidth();
      denom = divide;
      denom.x += m_divide->GetFullWidth();
      
      m_num->DrawList(num);
      m_divide->Draw(divide);
      m_denom->DrawList(denom);
    }
    else
    {
      num.x = point.x + m_horizontalGapLeft +
              (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_num->GetFullWidth()) / 2;
      num.y = point.y - m_num->GetMaxHeight() + m_num->GetMaxCenter() -
              Scale_Px(2);
      m_num->DrawList(num);

      denom.x = point.x + m_horizontalGapLeft +
                (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_denom->GetFullWidth()) / 2;
      denom.y = point.y + m_denom->GetMaxCenter() + Scale_Px(2);
      m_denom->DrawList(denom);
      SetPen(1.2);
      if (m_fracStyle != FC_CHOOSE)
        dc->DrawLine(point.x + m_horizontalGapLeft + (*m_configuration)->GetDefaultLineWidth() / 2,
                    point.y,
                    point.x + m_width - m_horizontalGapRight - (*m_configuration)->GetDefaultLineWidth() / 2,
                    point.y
        );
      UnsetPen();
    }
  }
}

wxString FracCell::ToString()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
    if (m_fracStyle == FC_NORMAL)
    {
      if (m_num->IsCompound())
        s += wxT("(") + m_num->ListToString() + wxT(")/");
      else
        s += m_num->ListToString() + wxT("/");
      if (m_denom->IsCompound())
        s += wxT("(") + m_denom->ListToString() + wxT(")");
      else
        s += m_denom->ListToString();
    }
    else if (m_fracStyle == FC_CHOOSE)
    {
      s = wxT("binomial(") + m_num->ListToString() + wxT(",") +
          m_denom->ListToString() + wxT(")");
    }
    else
    {
      Cell *tmp = m_denom.get();
      while (tmp != NULL)
      {
        tmp = tmp->m_next;   // Skip the d
        if (tmp == NULL)
          break;
        tmp = tmp->m_next;   // Skip the *
        if (tmp == NULL)
          break;
        s += tmp->GetDiffPart();
        tmp = tmp->m_next;   // Skip the *
        if (tmp == NULL)
          break;
        tmp = tmp->m_next;
      }
    }
  }
  return s;
}

wxString FracCell::ToMatlab()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
	if (m_fracStyle == FC_NORMAL)
	{
	  if (m_num->IsCompound())
		s += wxT("(") + m_num->ListToMatlab() + wxT(")/");
	  else
		s += m_num->ListToMatlab() + wxT("/");
	  if (m_denom->IsCompound())
		s += wxT("(") + m_denom->ListToMatlab() + wxT(")");
	  else
		s += m_denom->ListToMatlab();
	}
	else if (m_fracStyle == FC_CHOOSE)
	{
	  s = wxT("binomial(") + m_num->ListToMatlab() + wxT(",") +
		  m_denom->ListToMatlab() + wxT(")");
	}
	else
	{
	  Cell *tmp = m_denom.get();
	  while (tmp != NULL)
	  {
		tmp = tmp->m_next;   // Skip the d
		if (tmp == NULL)
		  break;
		tmp = tmp->m_next;   // Skip the *
		if (tmp == NULL)
		  break;
		s += tmp->GetDiffPart();
		tmp = tmp->m_next;   // Skip the *
		if (tmp == NULL)
		  break;
		tmp = tmp->m_next;
	  }
	}
  }
  return s;
}

wxString FracCell::ToTeX()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
    if (m_fracStyle == FC_CHOOSE)
    {
      s = wxT("\\begin{pmatrix}") + m_num->ListToTeX() + wxT("\\\\\n") +
          m_denom->ListToTeX() + wxT("\\end{pmatrix}");
    }
    else
    {
      s = wxT("\\frac{") + m_num->ListToTeX() + wxT("}{") +
          m_denom->ListToTeX() + wxT("}");
    }
  }
  return s;
}

wxString FracCell::ToMathML()
{
  return wxT("<mfrac>") +
         m_num->ListToMathML() +
         m_denom->ListToMathML() + wxT("</mfrac>\n");
}


wxString FracCell::ToOMML()
{
  return wxT("<m:f><m:num>") +
         m_num->ListToOMML() + wxT("</m:num><m:den>") +
         m_denom->ListToOMML() + wxT("</m:den></m:f>\n");
}

wxString FracCell::ToXML()
{
  wxString s = (m_fracStyle == FC_NORMAL || m_fracStyle == FC_DIFF) ?
               _T("f") : _T("f line = \"no\"");
  wxString diffStyle;
  if (m_fracStyle == FC_DIFF)
    diffStyle = wxT(" diffstyle=\"yes\"");
  if (m_forceBreakLine)
    diffStyle += wxT(" breakline=\"true\"");

  return _T("<") + s + diffStyle + _T("><r>") +
         m_num->ListToXML() + _T("</r><r>") +
         m_denom->ListToXML() + _T("</r></f>");
}

void FracCell::SetExponentFlag()
{
  if (m_num->IsShortNum() && m_denom->IsShortNum())
    m_exponent = true;
}

void FracCell::SetupBreakUps()
{
  m_displayedNum = m_numParenthesis.get();
  m_displayedDenom = m_denomParenthesis.get();
  if (m_fracStyle == FC_NORMAL)
  {
    if (m_num && (!m_num->IsCompound()))
      m_displayedNum = m_num.get();
    if (m_denom && (!m_denom->IsCompound()))
    {
      m_displayedDenom = m_denom.get();
    }
  }
  else
  {
    m_displayedNum = m_num.get();
    m_displayedDenom = m_denom.get();
  }
  m_num_Last = m_displayedNum;
  if (m_num_Last != NULL)
  {
    while (m_num_Last->m_next != NULL)
      m_num_Last = m_num_Last->m_next;
  }
  m_denom_Last = m_displayedDenom;
  if (m_denom_Last != NULL)
  {
    while (m_denom_Last->m_next != NULL)
      m_denom_Last = m_denom_Last->m_next;
  }
}

bool FracCell::BreakUp()
{
  if (m_fracStyle == FC_DIFF)
    return false;

  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    wxASSERT_MSG(m_num_Last != NULL, _("Bug: No last cell in an numerator!"));
    if (m_num_Last != NULL)
      m_num_Last->m_nextToDraw = m_divide.get();
    m_divide->m_nextToDraw = m_displayedDenom;
    m_displayedDenom->m_nextToDraw = m_nextToDraw;
    wxASSERT_MSG(m_denom_Last != NULL, _("Bug: No last cell in an denominator!"));
    m_nextToDraw = m_displayedNum;
    ResetData();    
    return true;
  }
  return false;
}

void FracCell::Unbreak()
{
  if (m_isBrokenIntoLines)
  {
    m_num->UnbreakList();
    m_denom->UnbreakList();
  }
  Cell::Unbreak();
}
