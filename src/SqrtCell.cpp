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
  This file defines the class SqrtCell

  SqrtCell is the MathCell type that represents a square root.
 */

#include "SqrtCell.h"
#include "TextCell.h"

#define SIGN_FONT_SCALE 2.0

SqrtCell::SqrtCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
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


void SqrtCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_innerCell != NULL)
    m_innerCell->SetParentList(parent);
  if (m_open != NULL)
    m_open->SetParentList(parent);
  if (m_close != NULL)
    m_close->SetParentList(parent);
}

MathCell *SqrtCell::Copy()
{
  SqrtCell *tmp = new SqrtCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList());
  tmp->m_isBroken = m_isBroken;
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

void SqrtCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_innerCell, m_open, m_close);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}


void SqrtCell::SetInner(MathCell *inner)
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
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_innerCell->RecalculateWidthsList(fontsize);
  if (configuration->CheckTeXFonts())
  {
    wxDC &dc = configuration->GetDC();
    double scale = configuration->GetScale();
    m_innerCell->RecalculateHeightList(fontsize);

    m_signFontScale = 1.0;
    int fontsize1 = (int) (SIGN_FONT_SCALE * scale * fontsize * m_signFontScale + 0.5);

    wxFont font(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                configuration->GetTeXCMEX());
    font.SetPointSize(fontsize1);
    dc.SetFont(font);
    dc.GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth(scale) + m_signWidth;

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

    fontsize1 = (int) (SIGN_FONT_SCALE * scale * fontsize * m_signFontScale + 0.5);
    font = wxFont(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
    font.SetPointSize(fontsize1);
    dc.SetFont(font);
    dc.GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth(scale) + m_signWidth;
  }
  else
    m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(10, scale) +
              3 * SCALE_PX(1, scale) + 1;
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  ResetData();
}

void SqrtCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_innerCell->RecalculateHeightList(fontsize);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(3, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(3, scale);
  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);
  if (m_isBroken)
  {
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
  }

}

void SqrtCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    MathCell::Draw(point, fontsize);
    Configuration *configuration = (*m_configuration);
    wxDC &dc = configuration->GetDC();
    double scale = configuration->GetScale();

    wxPoint in(point);

    if (configuration->CheckTeXFonts())
    {
      SetPen();

      in.x += m_signWidth;
      double scale = configuration->GetScale();

      int fontsize1 = (int) (SIGN_FONT_SCALE * scale * fontsize * m_signFontScale + 0.5);

      wxFont font(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
      font.SetPointSize(fontsize1);
      dc.SetFont(font);
      SetForeground();
      if (m_signType < 4)
      {
        dc.DrawText(
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
        dc.DrawText(wxT("t"),
                    point.x,
                    yBottom);
        dc.DrawText(wxT("v"),
                    point.x,
                    yTop);
        while (yTop < yBottom)
        {
          yTop += dy;
          dc.DrawText(wxT("u"),
                      point.x,
                      yTop);
        }
      }

      dc.DrawLine(point.x + m_signWidth,
                  point.y - m_innerCell->GetMaxCenter(),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                  point.y - m_innerCell->GetMaxCenter());

      UnsetPen();
    }
    else
    {
      in.x += SCALE_PX(10, scale) + SCALE_PX(1, scale) + 1;

      SetPen();
      dc.DrawLine(point.x,
                  point.y,
                  point.x + SCALE_PX(3, scale),
                  point.y - SCALE_PX(1, scale));
      dc.DrawLine(point.x + SCALE_PX(3, scale),
                  point.y - SCALE_PX(1, scale),
                  point.x + SCALE_PX(7, scale),
                  point.y + m_height - m_center - SCALE_PX(4, scale));
      dc.DrawLine(point.x + SCALE_PX(3, scale) + 1,
                  point.y - SCALE_PX(1, scale),
                  point.x + SCALE_PX(7, scale) + 1,
                  point.y + m_height - m_center - SCALE_PX(4, scale));
      dc.DrawLine(point.x + SCALE_PX(7, scale) + 1,
                  point.y + m_height - m_center - SCALE_PX(4, scale),
                  point.x + SCALE_PX(10, scale),
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + SCALE_PX(10, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(6, scale));
      UnsetPen();
    }

    m_innerCell->DrawList(in, fontsize);
  }
}

wxString SqrtCell::ToString()
{
  if (m_isBroken)
    return wxEmptyString;
  else
    return wxT("sqrt(") + m_innerCell->ListToString() + wxT(")");
}

wxString SqrtCell::ToTeX()
{
  if (m_isBroken)
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
//  if (m_isBroken)
//    return wxEmptyString;
  return _T("<q>") + m_innerCell->ListToXML() + _T("</q>");
}

void SqrtCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_innerCell->ContainsRect(rect))
    m_innerCell->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool SqrtCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
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
  if (m_isBroken)
    m_innerCell->UnbreakList();
  MathCell::Unbreak();
}
