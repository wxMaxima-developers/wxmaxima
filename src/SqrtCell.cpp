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
  This file defines the class SqrtCell

  SqrtCell is the Cell type that represents a square root.
 */

#include "SqrtCell.h"
#include "TextCell.h"

#define SIGN_FONT_SCALE 2.0

SqrtCell::SqrtCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_signSize = 50;
  m_signWidth = 18;
  m_signTop = m_signSize / 2;
  m_last = NULL;
  m_signType = 0;
  m_signFontScale = 0;
  m_innerCell = NULL;
  m_open = new TextCell(parent, config, cellPointers, wxT("sqrt("));
  m_open->DontEscapeOpeningParenthesis();
  m_close = new TextCell(parent, config, cellPointers, wxT(")"));
}


Cell *SqrtCell::Copy()
{
  SqrtCell *tmp = new SqrtCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList());
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;
  tmp->m_open->DontEscapeOpeningParenthesis();

  return tmp;
}

SqrtCell::~SqrtCell()
{
  wxDELETE(m_innerCell);
  wxDELETE(m_open);
  wxDELETE(m_close);
  m_innerCell = m_open = m_close = NULL;
  MarkAsDeleted();
}

std::list<Cell *> SqrtCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_innerCell)
    innerCells.push_back(m_innerCell);
  if(m_open)
    innerCells.push_back(m_open);
  if(m_close)
    innerCells.push_back(m_close);
  return innerCells;
}

void SqrtCell::SetInner(Cell *inner)
{
  if (inner == NULL)
    return;
  wxDELETE(m_innerCell);
  m_innerCell = inner;

  m_last = inner;
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void SqrtCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  Configuration *configuration = (*m_configuration);
  m_innerCell->RecalculateWidthsList(fontsize);
  if (configuration->CheckTeXFonts())
  {
    wxDC *dc = configuration->GetDC();
    m_innerCell->RecalculateHeightList(fontsize);

    m_signFontScale = 1.0;
    double fontsize1 = Scale_Px(SIGN_FONT_SCALE * fontsize * m_signFontScale);
    wxASSERT(fontsize1 > 0);
    wxFont font(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                configuration->GetTeXCMEX());
#if wxCHECK_VERSION(3, 1, 2)
    font.SetFractionalPointSize(fontsize1);
#else
    font.SetPointSize(fontsize1);
#endif
    dc->SetFont(font);
    dc->GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth() + m_signWidth;
    
    int size = m_innerCell->GetMaxHeight();

    if (size <= (m_signSize) / 5)
    {
      m_signType = 1;
      m_signFontScale = (5.0 * size) / (1.5 * m_signSize);
    }
    else if (size <= (2 * m_signSize) / 5)
    {
      m_signType = 2;
      m_signFontScale = (5.0 * size) / (2.2 * m_signSize);
    }
    else if (size <= (3 * m_signSize) / 5)
    {
      m_signType = 3;
      m_signFontScale = (5.0 * size) / (3.0 * m_signSize);
    }
    else if (size <= (4 * m_signSize) / 5)
    {
      m_signType = 4;
      m_signFontScale = (5.0 * size) / (3.8 * m_signSize);
    }
    else
    {
      m_signType = 5;
      m_signFontScale = 1.0;
    }

    fontsize1 = Scale_Px(SIGN_FONT_SCALE * fontsize * m_signFontScale);
    font = wxFont(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
    wxASSERT(fontsize1 > 0);
#if wxCHECK_VERSION(3, 1, 2)
    font.SetFractionalPointSize(fontsize1);
#else
    font.SetPointSize(fontsize1);
#endif
    dc->SetFont(font);
    dc->GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth() + m_signWidth;
  }
  else
    m_width = m_innerCell->GetFullWidth() + Scale_Px(13) + 1;
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  ResetData();
  if(m_isBrokenIntoLines)
    m_width = 0;
}

void SqrtCell::RecalculateHeight(int fontsize)
{
  m_innerCell->RecalculateHeightList(fontsize);
  m_height = m_innerCell->GetMaxHeight() + Scale_Px(3);
  m_center = m_innerCell->GetMaxCenter() + Scale_Px(3);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  if (m_isBrokenIntoLines)
  {
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
}

void SqrtCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();

    wxPoint in(point);

    if (configuration->CheckTeXFonts())
    {
      SetPen();

      in.x += m_signWidth;

      double fontsize1 = Scale_Px(SIGN_FONT_SCALE * m_fontSize * m_signFontScale);

      wxFont font(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
      wxASSERT(fontsize1 > 0);
#if wxCHECK_VERSION(3, 1, 2)
      font.SetFractionalPointSize(fontsize1);
#else
      font.SetPointSize(fontsize1);
#endif
      dc->SetFont(font);
      SetForeground();
      if (m_signType < 4)
      {
        dc->DrawText(
                m_signType == 1 ? wxT("p") :
                m_signType == 2 ? wxT("q") :
                m_signType == 3 ? wxT("r") : wxT("s"),
                point.x,
                point.y - m_innerCell->GetMaxCenter() - m_signTop);
      }
      else
      {
        int yBottom = point.y + m_innerCell->GetMaxDrop() - 3.2 * m_signTop;
        int yTop = point.y - m_innerCell->GetMaxCenter() - m_signTop;
        int dy = m_signSize / 10;
        wxASSERT_MSG((yTop != 0) || (yBottom != 0), _("Font issue? The contents of a sqrt() has the size 0."));
        wxASSERT_MSG(dy > 0,
                     _("Font issue: The sqrt() sign has the size 0! Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should be a workaround."));
        if (dy <= 0)
          dy = 1;
        dc->DrawText(wxT("t"),
                    point.x,
                    yBottom);
        dc->DrawText(wxT("v"),
                    point.x,
                    yTop);
        while (yTop < yBottom)
        {
          yTop += dy;
          dc->DrawText(wxT("u"),
                      point.x,
                      yTop);
        }
      }

      wxDC *adc = configuration->GetAntialiassingDC();
      adc->DrawLine(point.x + m_signWidth,
                  point.y - m_innerCell->GetMaxCenter(),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(),
                  point.y - m_innerCell->GetMaxCenter());

      UnsetPen();
    }
    else
    {
      wxDC *adc = configuration->GetAntialiassingDC();
      in.x += Scale_Px(11) + 1;
      SetPen(1.2);
      adc->DrawLine(point.x,
                  point.y,
                  point.x + Scale_Px(3),
                  point.y - Scale_Px(1));
      //  A wider line
      adc->DrawLine(point.x + Scale_Px(3),
                  point.y - Scale_Px(1),
                  point.x + Scale_Px(7),
                  point.y + m_height - m_center - Scale_Px(4));
      adc->DrawLine(point.x + Scale_Px(3+1),
                  point.y - Scale_Px(1),
                  point.x + Scale_Px(7+1),
                  point.y + m_height - m_center - Scale_Px(4));
      adc->DrawLine(point.x + Scale_Px(3+.5),
                  point.y - Scale_Px(1),
                  point.x + Scale_Px(7+.5),
                  point.y + m_height - m_center - Scale_Px(4));
      adc->DrawLine(point.x + Scale_Px(3),
                  point.y - Scale_Px(1),
                  point.x + Scale_Px(3+1),
                  point.y - Scale_Px(1));
      adc->DrawLine(point.x + Scale_Px(7),
                    point.y + m_height - m_center - Scale_Px(4),
                    point.x + Scale_Px(7+1),
                    point.y + m_height - m_center - Scale_Px(4));

      adc->DrawLine(point.x + Scale_Px(7+1),
                  point.y + m_height - m_center - Scale_Px(4),
                  point.x + Scale_Px(10),
                  point.y - m_center + Scale_Px(2));
      adc->DrawLine(point.x + Scale_Px(10),
                  point.y - m_center + Scale_Px(2),
                  point.x + m_width - Scale_Px(1),
                  point.y - m_center + Scale_Px(2));
      adc->DrawLine(point.x + m_width - Scale_Px(1),
                  point.y - m_center + Scale_Px(2),
                  point.x + m_width - Scale_Px(1),
                  point.y - m_center + Scale_Px(6));
      UnsetPen();
    }

    m_innerCell->DrawList(in);
  }
}

wxString SqrtCell::ToString()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  else
    return wxT("sqrt(") + m_innerCell->ListToString() + wxT(")");
}

wxString SqrtCell::ToMatlab()
{
  if (m_isBrokenIntoLines)
	return wxEmptyString;
  else
	return wxT("sqrt(") + m_innerCell->ListToMatlab() + wxT(")");
}

wxString SqrtCell::ToTeX()
{
  if (m_isBrokenIntoLines)
    return wxEmptyString;
  else
    return wxT("\\sqrt{") + m_innerCell->ListToTeX() + wxT("}");
}

wxString SqrtCell::ToMathML()
{
  return wxT("<msqrt>") + m_innerCell->ListToMathML() + wxT("</msqrt>\n");
}

wxString SqrtCell::ToOMML()
{
  return wxT("<m:rad><m:radPr m:degHide=\"1\"></m:radPr><m:deg></m:deg><m:e>") + m_innerCell->ListToOMML() +
         wxT("</m:e></m:rad>\n");
}

wxString SqrtCell::ToXML()
{
//  if (m_isBrokenIntoLines)
//    return wxEmptyString;
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  return wxT("<q") + flags + wxT(">") + m_innerCell->ListToXML() + wxT("</q>");
}

bool SqrtCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    wxASSERT_MSG(m_last != NULL, _("Bug: No last cell inside a square root!"));
    if (m_last != NULL)
    {
      m_last->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last;
    }
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
    return true;
  }
  return false;
}

void SqrtCell::Unbreak()
{
  if (m_isBrokenIntoLines)
    m_innerCell->UnbreakList();
  Cell::Unbreak();
}
