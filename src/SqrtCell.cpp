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
  This file defines the class SqrtCell

  SqrtCell is the Cell type that represents a square root.
 */

#include "SqrtCell.h"
#include "FontCache.h"

#define SIGN_FONT_SCALE 2.0

SqrtCell::SqrtCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_innerCell(new TextCell(parent, config, cellPointers)),
    m_open(new TextCell(parent, config, cellPointers, "sqrt(")),
    m_close(new TextCell(parent, config, cellPointers, ")"))
{
  m_nextToDraw = NULL;
  m_open->SetStyle(TS_FUNCTION);
  m_signSize = 50;
  m_signWidth = 18;
  m_signTop = m_signSize / 2;
  m_last = NULL;
  m_signType = 0;
  m_signFontScale = 0;
  static_cast<TextCell&>(*m_open).DontEscapeOpeningParenthesis();
}

// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_comma
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signSize
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signTop
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signType
// cppcheck-suppress uninitMemberVar symbolName=SqrtCell::m_signFontScale
SqrtCell::SqrtCell(const SqrtCell &cell):
    SqrtCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_innerCell)
    SetInner(cell.m_innerCell->CopyList());
  m_isBrokenIntoLines = cell.m_isBrokenIntoLines;
}

SqrtCell::~SqrtCell()
{
  MarkAsDeleted();
}

void SqrtCell::SetInner(Cell *inner)
{
  if (!inner)
    return;
  m_innerCell.reset(inner);

  m_last = inner;
  if (m_last != NULL)
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
}

void SqrtCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  Configuration *configuration = (*m_configuration);
  m_innerCell->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidths(fontsize);
  m_close->RecalculateWidths(fontsize);

  if (configuration->CheckTeXFonts())
  {
    wxDC *dc = configuration->GetDC();
    m_innerCell->RecalculateHeightList(fontsize);

    m_signFontScale = 1.0;
    double fontsize1 = Scale_Px(SIGN_FONT_SCALE * fontsize * m_signFontScale);
    wxASSERT(fontsize1 > 0);

    wxFont font =
      FontCache::GetAFont(wxFontInfo(fontsize1)
                            .Family(wxFONTFAMILY_MODERN)
                            .Italic(false)
                            .Bold(false)
                            .Underlined(false)
                            .FaceName(configuration->GetTeXCMEX()));

    dc->SetFont(font);
    dc->GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth() + m_signWidth;
    
    int size = m_innerCell->GetHeightList();

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
    wxASSERT(fontsize1 > 0);

    font =
      FontCache::GetAFont(wxFontInfo(fontsize1)
                            .Family(wxFONTFAMILY_MODERN)
                            .Italic(false)
                            .Bold(false)
                            .Underlined(false)
                            .FaceName(configuration->GetTeXCMEX()));

    dc->SetFont(font);
    dc->GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth() + m_signWidth;
  }
  else
    m_width = m_innerCell->GetFullWidth() + Scale_Px(13) + 1;
  if(m_isBrokenIntoLines)
    m_width = 0;
  Cell::RecalculateWidths(fontsize);
}

void SqrtCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  if(!m_isBrokenIntoLines)
  {
    m_innerCell->RecalculateHeightList(fontsize);
    m_height = m_innerCell->GetHeightList() + Scale_Px(3);
    m_center = m_innerCell->GetCenterList() + Scale_Px(3);
    m_open->RecalculateHeightList(fontsize);
    m_close->RecalculateHeightList(fontsize);
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
  }
  Cell::RecalculateHeight(fontsize);
}

void SqrtCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();

    wxPoint in(point);

    if (configuration->CheckTeXFonts())
    {
      SetPen();

      in.x += m_signWidth;

      double fontsize1 = Scale_Px(SIGN_FONT_SCALE * m_fontSize * m_signFontScale);
      wxASSERT(fontsize1 > 0);

      wxFont font =
        FontCache::GetAFont(wxFontInfo(fontsize1)
                              .Family(wxFONTFAMILY_MODERN)
                              .Italic(false)
                              .Bold(false)
                              .Underlined(false)
                              .FaceName(configuration->GetTeXCMEX()));

      dc->SetFont(font);
      SetForeground();
      if (m_signType < 4)
      {
        dc->DrawText(
                m_signType == 1 ? wxT("p") :
                m_signType == 2 ? wxT("q") :
                m_signType == 3 ? wxT("r") : wxT("s"),
                point.x,
                point.y - m_innerCell->GetCenterList() - m_signTop);
      }
      else
      {
        int yBottom = point.y + m_innerCell->GetMaxDrop() - 3.2 * m_signTop;
        int yTop = point.y - m_innerCell->GetCenterList() - m_signTop;
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
                  point.y - m_innerCell->GetCenterList(),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(),
                  point.y - m_innerCell->GetCenterList());

      UnsetPen();
    }
    else
    {
      wxDC *adc = configuration->GetAntialiassingDC();
      in.x += Scale_Px(11) + 1;
      SetPen(1.2);
      wxPointList points;
      // The "serif" at the start of the sign
      points.Append(new wxPoint(point));
      points.Append(new wxPoint(point.x + Scale_Px(3),
                                point.y - Scale_Px(1)));
      //  A wider line
      points.Append(new wxPoint(point.x + Scale_Px(3),
                                point.y - Scale_Px(1)));
      points.Append(new wxPoint(point.x + Scale_Px(7),
                                point.y + m_height - m_center - Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(7.5),
                                point.y + m_height - m_center - Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(3.5),
                                point.y - Scale_Px(1)));
      points.Append(new wxPoint(point.x + Scale_Px(3.5),
                                point.y - Scale_Px(1)));
      points.Append(new wxPoint(point.x + Scale_Px(3),
                                point.y - Scale_Px(1)));
      points.Append(new wxPoint(point.x + Scale_Px(8),
                                point.y + m_height - m_center - Scale_Px(4)));
      // The upwards line
      points.Append(new wxPoint(point.x + Scale_Px(10),
                                point.y - m_center + Scale_Px(2)));
      // The horizontal line
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1),
                                point.y - m_center + Scale_Px(2)));
      // The serif at the end of the root
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1),
                                point.y - m_center + Scale_Px(6)));
      adc->DrawLines(&points);
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
    m_open->SetNextToDraw(m_innerCell.get());
    wxASSERT_MSG(m_last != NULL, _("Bug: No last cell inside a square root!"));
    if (m_last != NULL)
      m_last->SetNextToDraw(m_close.get());
    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_open.get();

    ResetData();
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
    return true;
  }
  return false;
}

void SqrtCell::SetNextToDraw(Cell *next)
{
  if(m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
