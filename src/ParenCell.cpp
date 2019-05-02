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
  This file defines the class ParenCell

  ParenCell is the Cell type that represents a math element that is kept
  between parenthesis.
 */

#include "ParenCell.h"
#include "TextCell.h"

ParenCell::ParenCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_numberOfExtensions = 0;
  m_extendHeight = 12;
  m_charWidth = 12;
  m_charWidth1 = 12;
  m_charHeight = 12;
  m_charHeight1 = 12;
  m_fontSize = 10;
  m_last1 = NULL;
  m_signTopHeight = 12;
  m_signHeight = 50;
  m_signBotHeight = 12;
  m_signWidth = 12;
  m_bigParenType = Configuration::ascii;
  m_innerCell = NULL;
  m_print = true;
  m_open = new TextCell(parent, config, cellPointers, wxT("("));
  m_close = new TextCell(parent, config, cellPointers, wxT(")"));
}

Cell *ParenCell::Copy()
{
  ParenCell *tmp = new ParenCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList(), m_type);
  tmp->m_isBrokenIntoLines = m_isBrokenIntoLines;

  return tmp;
}

ParenCell::~ParenCell()
{
  wxDELETE(m_innerCell);
  wxDELETE(m_open);
  wxDELETE(m_close);
  m_innerCell = m_open = m_close = NULL;
  MarkAsDeleted();
}

std::list<Cell *> ParenCell::GetInnerCells()
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

void ParenCell::SetInner(Cell *inner, CellType type)
{
  if (inner == NULL)
    return;
  wxDELETE(m_innerCell);
  m_innerCell = inner;
  m_type = type;

  // Tell the first of our inter cell not to begin with a multiplication dot.
  m_innerCell->m_SuppressMultiplicationDot = true;

  // Search for the last of the inner cells
  while (inner->m_next != NULL)
    inner = inner->m_next;
  m_last1 = inner;
  ResetSize();
}

void ParenCell::SetFont(int fontsize)
{
  wxASSERT(fontsize >= 1);

  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();

  wxFont font;
  if(m_bigParenType == Configuration::ascii)
    font = configuration->GetFont(TS_FUNCTION, fontsize);
  else
    font = configuration->GetFont(TS_FUNCTION, configuration->GetMathFontSize());

  wxASSERT(font.GetPointSize() > 0);

  switch(m_bigParenType)
  {
  case Configuration::ascii:
  case Configuration::assembled_unicode:
    break;

  case Configuration::assembled_unicode_fallbackfont:
    font.SetFaceName(wxT("Linux Libertine"));
    break;

  case Configuration::assembled_unicode_fallbackfont2:
    font.SetFaceName(wxT("Linux Libertine O"));
    break;

  default:
    break;
  }

  font.SetStyle(wxFONTSTYLE_NORMAL);
  font.SetUnderlined(false);
  if (!font.IsOk())
  {
    font.SetFamily(wxFONTFAMILY_MODERN);
    font.SetStyle(wxFONTSTYLE_NORMAL);
    font.SetFaceName(wxEmptyString);
    font.SetUnderlined(false);
  }

  if (!font.IsOk())
    font = *wxNORMAL_FONT;

  // A fallback if we have been completely unable to set a working font
  if (!dc->GetFont().IsOk())
    m_bigParenType = Configuration::handdrawn;

  if(m_bigParenType != Configuration::handdrawn)
    dc->SetFont(font);

  SetForeground();
}

void ParenCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  Configuration *configuration = (*m_configuration);

  // Add a dummy contents to empty parenthesis
  if (m_innerCell == NULL)
    m_innerCell = new TextCell(m_group, m_configuration, m_cellPointers);
  
  m_innerCell->RecalculateWidthsList(fontsize);
  m_innerCell->RecalculateHeightList(fontsize);
  
  wxDC *dc = configuration->GetDC();
  int size = m_innerCell->GetMaxHeight();
  if (fontsize < 4) fontsize = 4;
  int fontsize1 = Scale_Px(fontsize);
  // If our font provides all the unicode chars we need we don't need
  // to bother which exotic method we need to use for drawing nice parenthesis.
  if (fontsize1*3 > size)
  {
    if(configuration->GetGrouphesisDrawMode() != Configuration::handdrawn)
      m_bigParenType = Configuration::ascii;
    m_open->RecalculateWidthsList(fontsize);
    m_close->RecalculateWidthsList(fontsize);
    m_signWidth = m_open->GetWidth();
    m_signHeight= m_open->GetHeight();
  }
  else
  {
    m_bigParenType = configuration->GetGrouphesisDrawMode();
    if(m_bigParenType != Configuration::handdrawn)
    {
      SetFont(fontsize);
      int signWidth1,signWidth2,signWidth3,descent,leading;
      dc->GetTextExtent(wxT(PAREN_OPEN_TOP_UNICODE),    &signWidth1, &m_signTopHeight, &descent, &leading);
      m_signTopHeight -= 2*descent + Scale_Px(1);
      dc->GetTextExtent(wxT(PAREN_OPEN_EXTEND_UNICODE), &signWidth2, &m_extendHeight, &descent, &leading);
      m_extendHeight -= 2*descent + Scale_Px(1);
      dc->GetTextExtent(wxT(PAREN_OPEN_BOTTOM_UNICODE), &signWidth3, &m_signBotHeight, &descent, &leading);
      m_signBotHeight -= descent + Scale_Px(1);

      m_signWidth = signWidth1;
      if(m_signWidth < signWidth2)
        m_signWidth = signWidth2;
      if(m_signWidth < signWidth3)
        m_signWidth = signWidth3;

      if(m_extendHeight < 1)
        m_extendHeight = 1;
      
      m_numberOfExtensions = ((size - m_signTopHeight - m_signBotHeight + m_extendHeight/ 2 - 1) / m_extendHeight);
      if(m_numberOfExtensions < 0)
        m_numberOfExtensions = 0;
      m_signHeight = m_signTopHeight + m_signBotHeight + m_extendHeight * m_numberOfExtensions;
    }
    else
    {
      m_signWidth = Scale_Px(6) + (*m_configuration)->GetDefaultLineWidth();
      if(m_signWidth < size / 15)
        m_signWidth = size / 15;
    }
  }
  m_width = m_innerCell->GetFullWidth() + m_signWidth * 2;
  if(m_isBrokenIntoLines)
    m_width = 0;
  ResetData();
}

void ParenCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  Configuration *configuration = (*m_configuration);
  m_height = MAX(m_signHeight,m_innerCell->GetMaxHeight()) + Scale_Px(2);
  m_center = m_height / 2;

  SetFont(fontsize);
  wxDC *dc = configuration->GetDC();
  dc->GetTextExtent(wxT("("), &m_charWidth1, &m_charHeight1);
  if(m_charHeight1 < 2)
    m_charHeight1 = 2;

  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);

  if (m_isBrokenIntoLines)
  {
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
  else
  {
    if(m_innerCell)
    {
      switch(m_bigParenType)
      {
      case Configuration::ascii:
        m_signHeight = m_charHeight1;
        break;
      case Configuration::assembled_unicode:
      case Configuration::assembled_unicode_fallbackfont:
      case Configuration::assembled_unicode_fallbackfont2:
        // Center the contents of the parenthesis vertically.
        //  m_innerCell->m_currentPoint.y += m_center - m_signHeight / 2;
        break;
      default:{}
      }
      m_innerCell->SetCurrentPoint(
        wxPoint(m_currentPoint.x + m_signWidth,
                m_currentPoint.y));

      // Center the argument of all big parenthesis vertically
      if(m_bigParenType != Configuration::ascii)
        m_innerCell->SetCurrentPoint(
          wxPoint(m_currentPoint.x + m_signWidth,
                  m_currentPoint.y + (m_innerCell->GetMaxCenter() - m_innerCell->GetMaxHeight() /2)));
      else
        m_innerCell->SetCurrentPoint(
          wxPoint(m_currentPoint.x + m_signWidth,
                  m_currentPoint.y));
      
      m_height = MAX(m_signHeight,m_innerCell->GetMaxHeight()) + Scale_Px(2);      
      m_center = m_height / 2;   
    }
  }
}

void ParenCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && (InUpdateRegion()))
  { 
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    wxPoint innerCellPos(point);

    SetFont(configuration->GetMathFontSize());
    
    switch(m_bigParenType)
    {            
    case Configuration::ascii:
      innerCellPos.x += m_open->GetWidth();
      m_open->DrawList(point);
      m_close->DrawList(wxPoint(point.x + m_signWidth + m_innerCell->GetFullWidth(),point.y));
      break;
    case Configuration::assembled_unicode:
    case Configuration::assembled_unicode_fallbackfont:
    case Configuration::assembled_unicode_fallbackfont2:
    {
      innerCellPos.x += m_signWidth;
      // Center the contents of the parenthesis vertically.
      innerCellPos.y += (m_innerCell->GetMaxCenter() - m_innerCell->GetMaxHeight() /2);

      int top = point.y - m_center + Scale_Px (1);
      int bottom = top + m_signHeight - m_signBotHeight - Scale_Px (2);
      dc->DrawText(wxT(PAREN_OPEN_TOP_UNICODE),
                    point.x,
                  top);
      dc->DrawText(wxT(PAREN_CLOSE_TOP_UNICODE),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(),
                  top);
      dc->DrawText(wxT(PAREN_OPEN_BOTTOM_UNICODE),
                  point.x,
                  bottom);
      dc->DrawText(wxT(PAREN_CLOSE_BOTTOM_UNICODE),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(),
                  bottom);
      
      for (int i = 0;i < m_numberOfExtensions;i++)
      {
        dc->DrawText(wxT(PAREN_OPEN_EXTEND_UNICODE),
                    point.x,
                    top + m_signTopHeight + i*m_extendHeight);
        dc->DrawText(wxT(PAREN_CLOSE_EXTEND_UNICODE),
                    point.x + m_signWidth + m_innerCell->GetFullWidth(),
                    top + m_signTopHeight + i*m_extendHeight);
      }
    }
    break;
    default:
    {
      wxDC *adc = configuration->GetAntialiassingDC();
      innerCellPos.x = point.x + Scale_Px(6) + (*m_configuration)->GetDefaultLineWidth();
      innerCellPos.y += (m_innerCell->GetMaxCenter() - m_innerCell->GetMaxHeight() /2);
      SetPen(1.0);

      int signWidth = m_signWidth - Scale_Px(2);
      
      wxPoint pointList[5];
      // Left bracket
      pointList[0] = wxPoint(point.x + Scale_Px(1) + signWidth,
                             point.y - m_center);
      pointList[1] = wxPoint(point.x + Scale_Px(1) + signWidth / 2,
                             point.y - m_center + signWidth / 2);
      pointList[2] = wxPoint(point.x + Scale_Px(1),
                             point.y);
      pointList[3] = wxPoint(point.x + Scale_Px(1) + signWidth / 2,
                             point.y + m_center - signWidth / 2);
      pointList[4] = wxPoint(point.x + Scale_Px(1) + signWidth,
                             point.y + m_center);
      configuration->GetAntialiassingDC()->DrawSpline(5,pointList);
      pointList[2] = wxPoint(point.x + Scale_Px(1.5),
                             point.y);
      adc->DrawSpline(5,pointList);
      
      // Right bracket
      pointList[0] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                             point.y - m_center);
      pointList[1] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                             point.y - m_center + signWidth / 2);
      pointList[2] = wxPoint(point.x + m_width - Scale_Px(1.5),
                             point.y);
      pointList[3] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                             point.y + m_center - signWidth / 2);
      pointList[4] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                             point.y + m_center);
      configuration->GetAntialiassingDC()->DrawSpline(5,pointList);
      pointList[2] = wxPoint(point.x + m_width - Scale_Px(1),
                             point.y);
      adc->DrawSpline(5,pointList);      
    }
      break;
    }
    
    UnsetPen();
    if(!m_isBrokenIntoLines)
      m_innerCell->DrawList(innerCellPos);
  }
}

wxString ParenCell::ToString()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
    if (m_print)
      s = wxT("(") + m_innerCell->ListToString() + wxT(")");
    else
      s = m_innerCell->ListToString();
  }
  return s;
}

wxString ParenCell::ToMatlab()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
	if (m_print)
	  s = wxT("(") + m_innerCell->ListToMatlab() + wxT(")");
	else
	  s = m_innerCell->ListToMatlab();
  }
  return s;
}

wxString ParenCell::ToTeX()
{
  wxString s;
  if (!m_isBrokenIntoLines)
  {
    wxString innerCell = m_innerCell->ListToTeX();

    // Let's see if the cell contains anything potentially higher than a normal
    // character.
    bool needsLeftRight = false;
    for (size_t i = 0; i < innerCell.Length(); i++)
      if (!wxIsalnum(innerCell[i]))
      {
        needsLeftRight = true;
        break;
      }

    if (m_print)
    {
      if (needsLeftRight)
        s = wxT("\\left( ") + m_innerCell->ListToTeX() + wxT("\\right) ");
      else
        s = wxT("(") + m_innerCell->ListToTeX() + wxT(")");
    }
    else
      s = m_innerCell->ListToTeX();
  }
  return s;
}

wxString ParenCell::ToOMML()
{
  return wxT("<m:d><m:dPr m:begChr=\"") + XMLescape(m_open->ToString()) + wxT("\" m:endChr=\"") +
         XMLescape(m_close->ToString()) + wxT("\" m:grow=\"1\"></m:dPr><m:e>") +
         m_innerCell->ListToOMML() + wxT("</m:e></m:d>");
}

wxString ParenCell::ToMathML()
{
  if (!m_print) return m_innerCell->ListToMathML();

  wxString open = m_open->ToString();
  wxString close = m_close->ToString();
  return (
          wxT("<mrow><mo>") + XMLescape(open) + wxT("</mo>") +
          m_innerCell->ListToMathML() +
          wxT("<mo>") + XMLescape(close) + wxT("</mo></mrow>\n")
  );
}

wxString ParenCell::ToXML()
{
//  if( m_isBrokenIntoLines )
//    return wxEmptyString;
  wxString s = m_innerCell->ListToXML();
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  return ((m_print) ? _T("<r><p") + flags + wxT(">") + s + _T("</p></r>") : s);
}

bool ParenCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    m_isBrokenIntoLines = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    wxASSERT_MSG(m_last1 != NULL, _("Bug: No last cell inside a parenthesis!"));
    if (m_last1 != NULL)
    {
      m_last1->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last1;
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

void ParenCell::Unbreak()
{
  if (m_isBrokenIntoLines)
    m_innerCell->UnbreakList();
  Cell::Unbreak();
}
