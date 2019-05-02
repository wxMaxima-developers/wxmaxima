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

FracCell::FracCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_num = NULL;
  m_denom = NULL;
  m_last1 = NULL;
  m_last2 = NULL;
  m_expDivideWidth = 12;
  m_fracStyle = FC_NORMAL;
  m_exponent = false;
  m_horizontalGapLeft = 0;
  m_horizontalGapRight = 0;
  m_protrusion = 0;
  
  m_open1 = NULL;
  m_close1 = NULL;
  m_open2 = NULL;
  m_close2 = NULL;
  m_divide = NULL;
}

Cell *FracCell::Copy()
{
  FracCell *tmp = new FracCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetNum(m_num->CopyList());
  tmp->SetDenom(m_denom->CopyList());
  tmp->m_fracStyle = m_fracStyle;
  tmp->m_exponent = m_exponent;
  tmp->SetupBreakUps();
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;

  return tmp;
}

FracCell::~FracCell()
{
  wxDELETE(m_open1);
  wxDELETE(m_open2);
  wxDELETE(m_close1);
  wxDELETE(m_close2);
  wxDELETE(m_num);
  wxDELETE(m_denom);
  wxDELETE(m_divide);
  m_open1 = m_open2 = m_close1 = m_close2 = m_num = m_denom = m_divide = NULL;
  MarkAsDeleted();
}

std::list<Cell *> FracCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_divide)
    innerCells.push_back(m_divide);
  if(m_denom)
    innerCells.push_back(m_denom);
  if(m_num)
    innerCells.push_back(m_num);
  if(m_open1)
    innerCells.push_back(m_open1);
  if(m_close1)
    innerCells.push_back(m_close1);
  if(m_open2)
    innerCells.push_back(m_open2);
  if(m_close2)
    innerCells.push_back(m_close2);
  return innerCells;
}

void FracCell::SetNum(Cell *num)
{
  if (num == NULL)
    return;
  wxDELETE(m_num);
  m_num = num;
}

void FracCell::SetDenom(Cell *denom)
{
  if (denom == NULL)
    return;
  wxDELETE(m_denom);
  m_denom = denom;
}

void FracCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  wxASSERT(fontsize >= 1);
  Configuration *configuration = (*m_configuration);
  if (m_isBrokenIntoLines || m_exponent)
  {
    m_num->RecalculateWidthsList(fontsize);
    m_denom->RecalculateWidthsList(fontsize);
  }
  else
  {
    m_num->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denom->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  wxDC *dc = configuration->GetDC();
  dc->SetFont(configuration->GetFont(TS_VARIABLE,fontsize));
  if (m_exponent && !m_isBrokenIntoLines)
  {
    m_protrusion = 0;
    int height;
    dc->GetTextExtent(wxT("/"), &m_expDivideWidth, &height);
    m_width = m_num->GetFullWidth() + m_denom->GetFullWidth() + m_expDivideWidth;
  }
  else
  {
    int dummy;

    dc->GetTextExtent(wxT("X"), &m_protrusion, &dummy);
    m_protrusion /= 3;
    
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
    
    m_width = MAX(m_num->GetFullWidth(), m_denom->GetFullWidth()) +
              2 * m_protrusion + m_horizontalGapLeft + m_horizontalGapRight;
  }
  m_open1->RecalculateWidths(fontsize);
  m_close1->RecalculateWidths(fontsize);
  m_open2->RecalculateWidths(fontsize);
  m_close2->RecalculateWidths(fontsize);
  m_divide->RecalculateWidths(fontsize);
  ResetData();
  if(m_isBrokenIntoLines)
    m_width = 0;
}

void FracCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  if (m_isBrokenIntoLines || m_exponent)
  {
    m_num->RecalculateHeightList(fontsize);
    m_denom->RecalculateHeightList(fontsize);
  }
  else
  {
    m_num->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denom->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
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
      m_height = m_num->GetMaxHeight();
      m_center = m_height / 2;
    }
  }

  m_open1->RecalculateHeight(fontsize);
  m_close1->RecalculateHeight(fontsize);
  m_open2->RecalculateHeight(fontsize);
  m_close2->RecalculateHeight(fontsize);
  m_divide->RecalculateHeight(fontsize);
}

void FracCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    Configuration *configuration = (*m_configuration);
    
    wxDC *dc = configuration->GetDC();
    wxPoint num, denom;

    if (m_exponent)
    {
      num.x = point.x;
      num.y = point.y;
      denom.x = point.x + m_num->GetFullWidth() + m_expDivideWidth;
      denom.y = num.y;

      m_num->DrawList(num);
      m_denom->DrawList(denom);

      int fontsize1 = Scale_Px(m_fontSize);
      wxASSERT(fontsize1 > 0);
      dc->SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                        wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        configuration->GetFontName(TS_VARIABLE)));
      dc->DrawText(wxT("/"),
                  point.x + m_num->GetFullWidth(),
                  point.y - m_num->GetMaxCenter() + MC_TEXT_PADDING);
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
      Cell *tmp = m_denom;
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
	  Cell *tmp = m_denom;
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
  if (m_fracStyle == FC_NORMAL)
  {
    m_open1 = new TextCell(m_group, m_configuration, m_cellPointers, wxT("("));
    m_close1 = new TextCell(m_group, m_configuration, m_cellPointers, wxT(")"));
    m_open2 = new TextCell(m_group, m_configuration, m_cellPointers, wxT("("));
    m_close2 = new TextCell(m_group, m_configuration, m_cellPointers, wxT(")"));
    if (m_num)
    {
      if (!m_num->IsCompound())
      {
        m_open1->m_isHidden = true;
        m_close1->m_isHidden = true;
      }
    }
    if (m_denom)
    {
      if (!m_denom->IsCompound())
      {
        m_open2->m_isHidden = true;
        m_close2->m_isHidden = true;
      }
    }
    m_divide = new TextCell(m_group, m_configuration, m_cellPointers, wxT("/"));
  }
  else
  {
    m_open1 = new TextCell(m_group, m_configuration, m_cellPointers, wxT("binomial("));
    m_close1 = new TextCell(m_group, m_configuration, m_cellPointers, wxT("x"));
    m_open2 = new TextCell(m_group, m_configuration, m_cellPointers, wxT("x"));
    m_close2 = new TextCell(m_group, m_configuration, m_cellPointers, wxT(")"));
    m_divide = new TextCell(m_group, m_configuration, m_cellPointers, wxT(","));
    m_close1->m_isHidden = true;
    m_open2->m_isHidden = true;
  }

  m_last1 = m_num;
  if (m_last1 != NULL)
  {
    while (m_last1->m_next != NULL)
      m_last1 = m_last1->m_next;
  }

  m_last2 = m_denom;
  if (m_last2 != NULL)
  {
    while (m_last2->m_next != NULL)
      m_last2 = m_last2->m_next;
  }
}

bool FracCell::BreakUp()
{
  if (m_fracStyle == FC_DIFF)
    return false;

  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open1->m_previousToDraw = this;
    m_open1->m_nextToDraw = m_num;
    m_num->m_previousToDraw = m_open1;
    wxASSERT_MSG(m_last1 != NULL, _("Bug: No last cell in an numerator!"));
    if (m_last1 != NULL)
    {
      m_last1->m_nextToDraw = m_close1;
      m_close1->m_previousToDraw = m_last1;
    }
    m_close1->m_nextToDraw = m_divide;
    m_divide->m_previousToDraw = m_close1;
    m_divide->m_nextToDraw = m_open2;
    m_open2->m_previousToDraw = m_divide;
    m_open2->m_nextToDraw = m_denom;
    m_denom->m_previousToDraw = m_open2;
    wxASSERT_MSG(m_last2 != NULL, _("Bug: No last cell in an denominator!"));
    if (m_last2 != NULL)
    {
      m_last2->m_nextToDraw = m_close2;
      m_close2->m_previousToDraw = m_last2;
    }
    m_close2->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close2;
    m_nextToDraw = m_open1;
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
