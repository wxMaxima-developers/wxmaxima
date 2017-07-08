// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
  This file defines the class FracCell

  FracCell is the MathCell type that represents fractions.
*/

#include "FracCell.h"
#include "TextCell.h"

#define FRAC_DEC 1

FracCell::FracCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
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

  m_open1 = NULL;
  m_close1 = NULL;
  m_open2 = NULL;
  m_close2 = NULL;
  m_divide = NULL;
}

void FracCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_num != NULL)
    m_num->SetParentList(parent);
  if (m_denom != NULL)
    m_denom->SetParentList(parent);
  if (m_open1 != NULL)
    m_open1->SetParentList(parent);
  if (m_close1 != NULL)
    m_close2->SetParentList(parent);
  if (m_open2 != NULL)
    m_open2->SetParentList(parent);
  if (m_close2 != NULL)
    m_close2->SetParentList(parent);
  if (m_divide != NULL)
    m_divide->SetParentList(parent);
}

MathCell *FracCell::Copy()
{
  FracCell *tmp = new FracCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetNum(m_num->CopyList());
  tmp->SetDenom(m_denom->CopyList());
  tmp->m_fracStyle = m_fracStyle;
  tmp->m_exponent = m_exponent;
  tmp->SetupBreakUps();
  tmp->m_isBroken = m_isBroken;

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

void FracCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_open1,m_open2,m_close1,m_close2,m_num,m_denom,m_divide);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}

void FracCell::SetNum(MathCell *num)
{
  if (num == NULL)
    return;
  wxDELETE(m_num);
  m_num = num;
}

void FracCell::SetDenom(MathCell *denom)
{
  if (denom == NULL)
    return;
  wxDELETE(m_denom);
  m_denom = denom;
}

void FracCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  if (m_isBroken || m_exponent)
  {
    m_num->RecalculateWidthsList(fontsize);
    m_denom->RecalculateWidthsList(fontsize);
  }
  else
  {
    m_num->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denom->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  wxDC &dc = configuration->GetDC();
  dc.SetFont(configuration->GetFont(TS_VARIABLE,fontsize));
  if (m_exponent && !m_isBroken)
  {

    int height;
    dc.GetTextExtent(wxT("/"), &m_expDivideWidth, &height);
    m_width = m_num->GetFullWidth(scale) + m_denom->GetFullWidth(scale) + m_expDivideWidth;
  }
  else
  {
    // We want half a space's widh of blank space to separate us from the
    // next minus.
    int dummy;
    m_horizontalGapLeft = 0;
    if (((m_previous != NULL) && (m_previous->ToString().EndsWith(wxT("-")))))
      dc.GetTextExtent(wxT("X"), &m_horizontalGapLeft, &dummy);
    m_horizontalGapLeft /= 2;
    m_horizontalGapRight = 0;
    if (((m_next != NULL) && (m_next->ToString().StartsWith(wxT("-")))))
      dc.GetTextExtent(wxT("X"), &m_horizontalGapRight, &dummy);

    m_width = MAX(m_num->GetFullWidth(scale), m_denom->GetFullWidth(scale)) +
              m_horizontalGapLeft + m_horizontalGapRight;
  }
  m_open1->RecalculateWidths(fontsize);
  m_close1->RecalculateWidths(fontsize);
  m_open2->RecalculateWidths(fontsize);
  m_close2->RecalculateWidths(fontsize);
  m_divide->RecalculateWidths(fontsize);
  ResetData();
}

void FracCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  if (m_isBroken || m_exponent)
  {
    m_num->RecalculateHeightList(fontsize);
    m_denom->RecalculateHeightList(fontsize);
  }
  else
  {
    m_num->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
    m_denom->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
  }
  if (!m_exponent)
  {
    m_height = m_num->GetMaxHeight() + m_denom->GetMaxHeight() +
               SCALE_PX(4, scale);
    m_center = m_num->GetMaxHeight() + SCALE_PX(2, scale);
  }
  else
  {
    m_height = m_num->GetMaxHeight();
    m_center = m_height / 2;
  }

  m_open1->RecalculateHeight(fontsize);
  m_close1->RecalculateHeight(fontsize);
  m_open2->RecalculateHeight(fontsize);
  m_close2->RecalculateHeight(fontsize);
  m_divide->RecalculateHeight(fontsize);
  MathCell::RecalculateHeight(fontsize);
}

void FracCell::Draw(wxPoint point, int fontsize)
{
  MathCell::Draw(point, fontsize);
  Configuration *configuration = (*m_configuration);

  if (DrawThisCell(point) && InUpdateRegion())
  {
    wxDC &dc = configuration->GetDC();
    double scale = configuration->GetScale();
    wxPoint num, denom;

    if (m_exponent && !m_isBroken)
    {
      double scale = configuration->GetScale();

      num.x = point.x;
      num.y = point.y;
      denom.x = point.x + m_num->GetFullWidth(scale) + m_expDivideWidth;
      denom.y = num.y;

      m_num->DrawList(num, fontsize);
      m_denom->DrawList(denom, fontsize);

      int fontsize1 = (int) ((double) (fontsize) * scale + 0.5);
      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                        wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        configuration->GetFontName(TS_VARIABLE)));
      dc.DrawText(wxT("/"),
                  point.x + m_num->GetFullWidth(scale),
                  point.y - m_num->GetMaxCenter() + SCALE_PX(MC_TEXT_PADDING, scale));
    }
    else
    {
      num.x = point.x + m_horizontalGapLeft +
              (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_num->GetFullWidth(scale)) / 2;
      num.y = point.y - m_num->GetMaxHeight() + m_num->GetMaxCenter() -
              SCALE_PX(2, scale);
      m_num->DrawList(num, MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));

      denom.x = point.x + m_horizontalGapLeft +
                (m_width - m_horizontalGapLeft - m_horizontalGapRight - m_denom->GetFullWidth(scale)) / 2;
      denom.y = point.y + m_denom->GetMaxCenter() + SCALE_PX(2, scale);
      m_denom->DrawList(denom, MAX(MC_MIN_SIZE, fontsize - FRAC_DEC));
      SetPen();
      if (m_fracStyle != FC_CHOOSE)
        dc.DrawLine(point.x + m_horizontalGapLeft + (*m_configuration)->GetDefaultLineWidth() / 2,
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
  if (!m_isBroken)
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
      MathCell *tmp = m_denom;
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
  if (!m_isBroken)
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
  return _T("<") + s + diffStyle + _T("><r>") +
         m_num->ListToXML() + _T("</r><r>") +
         m_denom->ListToXML() + _T("</r></f>");
}

void FracCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_num->ContainsRect(rect))
    m_num->SelectRect(rect, first, last);
  else if (m_denom->ContainsRect(rect))
    m_denom->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
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

  if (!m_isBroken)
  {
    m_isBroken = true;
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
  if (m_isBroken)
  {
    m_num->UnbreakList();
    m_denom->UnbreakList();
  }
  MathCell::Unbreak();
}
