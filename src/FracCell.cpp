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
  This file defines the class FracCell

  FracCell is the Cell type that represents fractions.
*/

#include "FracCell.h"
#include "ParenCell.h"
#include "VisiblyInvalidCell.h"

#define FRAC_DEC 1

FracCell::FracCell(GroupCell *parent, Configuration **config) :
    Cell(parent, config),
    m_numParenthesis(new ParenCell(m_group, m_configuration)),
    m_denomParenthesis(new ParenCell(m_group, m_configuration)),
    m_divideOwner(new TextCell(parent, config, "/"))
{
  SetNum(new VisiblyInvalidCell(parent,config));
  SetDenom(new VisiblyInvalidCell(parent,config));
  m_divide->SetStyle(TS_VARIABLE);
  m_fracStyle = FC_NORMAL;
  m_exponent = false;
  m_horizontalGapLeft = 0;
  m_horizontalGapRight = 0;
  m_protrusion = 0;
}

FracCell::FracCell(const FracCell &cell):
    FracCell(cell.m_group, cell.m_configuration)
{
  CopyCommonData(cell);
  if (cell.Num())
    SetNum(cell.Num()->CopyList());
  if (cell.Denom())
    SetDenom(cell.Denom()->CopyList());
  m_fracStyle = cell.m_fracStyle;
  m_exponent = cell.m_exponent;
  SetupBreakUps();
}

void FracCell::SetNum(Cell *num)
{
  if (!num)
    return;
  m_numParenthesis->SetInner(num);
  SetupBreakUps();
}

void FracCell::SetDenom(Cell *denom)
{
  if (!denom)
    return;
  m_denomParenthesis->SetInner(denom);
  SetupBreakUps();
}

void FracCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;
  if(m_exponent || m_isBrokenIntoLines)
  {
    m_displayedNum->RecalculateWidthsList(fontsize);
    m_displayedDenom->RecalculateWidthsList(fontsize);
  }
  else
  {
    m_displayedNum->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_displayedDenom->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  m_divide->RecalculateWidths(fontsize);
  
  if (m_exponent)
  {
    m_protrusion = m_horizontalGapLeft = m_horizontalGapRight = 0;
    m_width = Num()->GetWidth() + Denom()->GetWidth() + m_divide->GetWidth();
  }
  else
  {
    if(m_isBrokenIntoLines)
      m_width = 0;
    else
    {
      m_protrusion = Scale_Px((*m_configuration)->GetMathFontSize() / 2);
      
      // We want half a space's widh of blank space to separate us from the
      // next minus.
      
      if (m_previous && m_previous->ToString().EndsWith(wxT("-")))
        m_horizontalGapLeft = m_protrusion;
      else
        m_horizontalGapLeft = 0;
      
      if (m_next && m_next->ToString().StartsWith(wxT("-")))
        m_horizontalGapRight = m_protrusion;
      else
        m_horizontalGapRight = 0;
      
      m_width = wxMax(m_displayedNum->GetFullWidth(), m_displayedDenom->GetFullWidth()) +
        2 * m_protrusion + m_horizontalGapLeft + m_horizontalGapRight;
    }
  }
  Cell::RecalculateWidths(fontsize);
}

void FracCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;
  if(m_exponent || m_isBrokenIntoLines)
  {
    m_displayedNum->RecalculateHeightList(fontsize);
    m_displayedDenom->RecalculateHeightList(fontsize);
  }
  else
  {
    m_displayedNum->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_displayedDenom->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  m_divide->RecalculateHeight(fontsize);

  if(m_isBrokenIntoLines)
  {
    m_height = 2;
    m_center = 1;
  }
  else
  {
    if (!m_exponent)
    {
      m_height = Num()->GetHeightList() + Denom()->GetHeightList() +
        Scale_Px(4);
      m_center = Num()->GetHeightList() + Scale_Px(2);
    }
    else
    {
      m_height = wxMax(Num()->GetHeightList(), Denom()->GetHeightList());
      m_center = wxMax(Num()->GetCenterList(), Denom()->GetCenterList());
    }
  }
  Cell::RecalculateHeight(fontsize);
}

void FracCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
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
      divide.x += m_displayedNum->GetFullWidth();
      denom = divide;
      denom.x += m_divide->GetFullWidth();
      
      m_displayedNum->DrawList(num);
      m_divide->Draw(divide);
      m_displayedDenom->DrawList(denom);
    }
    else
    {
      num.x = point.x + m_horizontalGapLeft +
              (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_displayedNum->GetFullWidth()) / 2;
      num.y = point.y - m_displayedNum->GetHeightList() + m_displayedNum->GetCenterList() -
              Scale_Px(2);
      m_displayedNum->DrawList(num);

      denom.x = point.x + m_horizontalGapLeft +
                (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_displayedDenom->GetFullWidth()) / 2;
      denom.y = point.y + m_displayedDenom->GetCenterList() + Scale_Px(2);
      m_displayedDenom->DrawList(denom);
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
      if (Num()->IsCompound())
        s += wxT("(") + Num()->ListToString() + wxT(")/");
      else
        s += Num()->ListToString() + wxT("/");
      if (Denom()->IsCompound())
        s += wxT("(") + Denom()->ListToString() + wxT(")");
      else
        s += Denom()->ListToString();
    }
    else if (m_fracStyle == FC_CHOOSE)
    {
      s = wxT("binomial(") + Num()->ListToString() + wxT(",") +
          Denom()->ListToString() + wxT(")");
    }
    else
    {
      Cell *tmp = Denom();
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
      if (Num()->IsCompound())
        s += wxT("(") + Num()->ListToMatlab() + wxT(")/");
	  else
        s += Num()->ListToMatlab() + wxT("/");
      if (Denom()->IsCompound())
        s += wxT("(") + Denom()->ListToMatlab() + wxT(")");
	  else
        s += Denom()->ListToMatlab();
	}
	else if (m_fracStyle == FC_CHOOSE)
	{
      s = wxT("binomial(") + Num()->ListToMatlab() + wxT(",") +
          Denom()->ListToMatlab() + wxT(")");
	}
	else
    {
      for (Cell *tmp = Denom(); tmp; tmp = tmp->m_next)
	  {
		tmp = tmp->m_next;   // Skip the d
        if (!tmp)
		  break;
		tmp = tmp->m_next;   // Skip the *
        if (!tmp)
		  break;
		s += tmp->GetDiffPart();
		tmp = tmp->m_next;   // Skip the *
        if (!tmp)
		  break;
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
      s = wxT("\\begin{pmatrix}") + Num()->ListToTeX() + wxT("\\\\\n") +
          Denom()->ListToTeX() + wxT("\\end{pmatrix}");
    }
    else
    {
      s = wxT("\\frac{") + Num()->ListToTeX() + wxT("}{") +
          Denom()->ListToTeX() + wxT("}");
    }
  }
  return s;
}

wxString FracCell::ToMathML()
{
  return wxT("<mfrac>") +
         Num()->ListToMathML() +
         Denom()->ListToMathML() + wxT("</mfrac>\n");
}


wxString FracCell::ToOMML()
{
  return wxT("<m:f><m:num>") +
         Num()->ListToOMML() + wxT("</m:num><m:den>") +
         Denom()->ListToOMML() + wxT("</m:den></m:f>\n");
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
         Num()->ListToXML() + _T("</r><r>") +
         Denom()->ListToXML() + _T("</r></f>");
}

void FracCell::SetExponentFlag()
{
  if (Num()->IsShortNum() && Denom()->IsShortNum())
    m_exponent = true;
}

void FracCell::SetupBreakUps()
{
  m_displayedNum = m_numParenthesis.get();
  m_displayedDenom = m_denomParenthesis.get();
  if (m_fracStyle == FC_CHOOSE)
  {
    if (Num() && !Num()->IsCompound())
      m_displayedNum = Num();
    if (Denom() && !Denom()->IsCompound())
      m_displayedDenom = Denom();
  }
  else
  {
    m_displayedNum = Num();
    m_displayedDenom = Denom();
  }
}

bool FracCell::BreakUp()
{
  if (m_fracStyle == FC_DIFF)
    return false;

  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    if(Num() && Num()->m_next)
      m_displayedNum = m_numParenthesis.get();
    if(Denom() && Denom()->m_next)
      m_displayedDenom = m_denomParenthesis.get();
    // Note: Yes, we don't want m_displayedNum->last() here.
    m_displayedNum->SetNextToDraw(m_divide);
    m_divide->SetNextToDraw(m_displayedDenom);
    m_displayedDenom->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_displayedNum;
    ResetData();    
    return true;
  }
  return false;
}

void FracCell::SetNextToDraw(Cell *next)
{
  if (m_isBrokenIntoLines)
    m_denomParenthesis->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
