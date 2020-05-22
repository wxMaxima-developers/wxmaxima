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
  This file defines the class ParenCell

  ParenCell is the Cell type that represents a math element that is kept
  between parenthesis.
 */

#include "ParenCell.h"
#include "FontCache.h"

ParenCell::ParenCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_innerCell(new TextCell(parent, config, cellPointers)),
    m_open(new TextCell(parent, config, cellPointers, wxT("("))),
    m_close(new TextCell(parent, config, cellPointers, wxT(")")))
{
  m_open->SetStyle(TS_FUNCTION);
  m_close->SetStyle(TS_FUNCTION);
  m_numberOfExtensions = 0;
  m_extendHeight = 12;
  m_charWidth = 12;
  m_charWidth1 = 12;
  m_charHeight = 12;
  m_charHeight1 = 12;
  m_fontSize = 10;
  m_signTopHeight = 12;
  m_signHeight = 50;
  m_signBotHeight = 12;
  m_signWidth = 12;
  m_bigParenType = Configuration::ascii;
  m_print = true;
}

// These false-positive warnings only appear in old versions of cppcheck
// that don't fully understand constructor delegation, still.
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_last1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_print
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_numberOfExtensions
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charWidth
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charWidth1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_charHeight1
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signTopHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_signBotHeight
// cppcheck-suppress uninitMemberVar symbolName=ParenCell::m_extendHeight
ParenCell::ParenCell(const ParenCell &cell):
    ParenCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  CopyCommonData(cell);
  if (cell.m_innerCell)
    SetInner(cell.m_innerCell->CopyList(), cell.m_type);
  m_isBrokenIntoLines = cell.m_isBrokenIntoLines;
}

ParenCell::~ParenCell()
{
  MarkAsDeleted();
}

void ParenCell::SetInner(Cell *inner, CellType type)
{
  if (inner)
    SetInner(std::shared_ptr<Cell>(inner), type);
}

void ParenCell::SetInner(std::shared_ptr<Cell> inner, CellType type)
{
  if (!inner)
    return;
  m_innerCell = inner;

  m_type = type;
  // Tell the first of our inner cells not to begin with a multiplication dot.
  m_innerCell->m_SuppressMultiplicationDot = true;

  // Search for the last of the inner cells
  Cell *last1 = inner.get();
  while (last1->m_next != NULL)
    last1 = last1->m_next;
  m_last1 = last1;
  ResetSize();
}

void ParenCell::SetFont(int fontsize)
{
  wxASSERT(fontsize >= 1);

  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();

  wxFontInfo req;
  wxFont font;
  if(m_bigParenType == Configuration::ascii)
    req = FontInfo::GetFor(configuration->GetFont(TS_FUNCTION, fontsize));
  else
    req = FontInfo::GetFor(configuration->GetFont(TS_FUNCTION, configuration->GetMathFontSize()));

  wxASSERT(req.GetPointSize() > 0);

  switch(m_bigParenType)
  {
  case Configuration::ascii:
  case Configuration::assembled_unicode:
    break;

  case Configuration::assembled_unicode_fallbackfont:
    req.FaceName(wxT("Linux Libertine"));
    break;

  case Configuration::assembled_unicode_fallbackfont2:
    req.FaceName(wxT("Linux Libertine O"));
    break;

  default:
    break;
  }

  req.Italic(false).Underlined(false);
  font = FontCache::GetAFont(req);
  if (!font.IsOk())
  {
    req.Family(wxFONTFAMILY_MODERN)
      .Italic(false)
      .FaceName(wxEmptyString)
      .Underlined(false);
    font = FontCache::GetAFont(req);
  }

  if (!font.IsOk())
    font = FontCache::GetAFont(*wxNORMAL_FONT);

  // A fallback if we have been completely unable to set a working font
  if (!dc->GetFont().IsOk())
    m_bigParenType = Configuration::handdrawn;

  if(m_bigParenType != Configuration::handdrawn)
    dc->SetFont(font);

  SetForeground();
}

void ParenCell::RecalculateWidths(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  Configuration *configuration = (*m_configuration);
  
  m_innerCell->RecalculateWidthsList(fontsize);
  m_innerCell->RecalculateHeightList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  
  wxDC *dc = configuration->GetDC();
  int size = m_innerCell->GetHeightList();
  if (fontsize < 4) fontsize = 4;
  int fontsize1 = Scale_Px(fontsize);
  // If our font provides all the unicode chars we need we don't need
  // to bother which exotic method we need to use for drawing nice parenthesis.
  if (fontsize1*3 > size)
  {
    if(configuration->GetParenthesisDrawMode() != Configuration::handdrawn)
      m_bigParenType = Configuration::ascii;
    m_signHeight = m_open->GetHeightList();
    m_signWidth  = m_open->GetWidth();
  }
  else
  {
    m_bigParenType = configuration->GetParenthesisDrawMode();
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
  Cell::RecalculateWidths(fontsize);
}

void ParenCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  Configuration *configuration = (*m_configuration);
  m_height = wxMax(m_signHeight,m_innerCell->GetHeightList()) + Scale_Px(2);
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
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
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
                  m_currentPoint.y + (m_innerCell->GetCenterList() - m_innerCell->GetHeightList() /2)));
      else
        m_innerCell->SetCurrentPoint(
          wxPoint(m_currentPoint.x + m_signWidth,
                  m_currentPoint.y));
      
      m_height = wxMax(m_signHeight,m_innerCell->GetHeightList()) + Scale_Px(4);
      m_center = m_height / 2;   
    }
  }
  Cell::RecalculateHeight(fontsize);
}

void ParenCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
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
      m_close->DrawList(wxPoint(point.x + m_open->GetWidth() + m_innerCell->GetFullWidth(),point.y));
      break;
    case Configuration::assembled_unicode:
    case Configuration::assembled_unicode_fallbackfont:
    case Configuration::assembled_unicode_fallbackfont2:
    {
      innerCellPos.x += m_signWidth;
      // Center the contents of the parenthesis vertically.
      innerCellPos.y += (m_innerCell->GetCenterList() - m_innerCell->GetHeightList() /2);

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
      innerCellPos.y += (m_innerCell->GetCenterList() - m_innerCell->GetHeightList() /2);
      SetPen(1.0);

      int signWidth = m_signWidth - Scale_Px(2);
      innerCellPos.x = point.x + m_signWidth;

      wxPointList points;
      // Left bracket
      points.Append(new wxPoint(point.x + Scale_Px(1) + signWidth,
                             point.y - m_center + Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(1) + 3 * signWidth / 4,
                                point.y - m_center + 3 * signWidth / 4 + Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(1),
                                point.y));
      points.Append(new wxPoint(point.x + Scale_Px(1) + 3 * signWidth / 4,
                                point.y + m_center - 3 * signWidth / 4 - Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(1) + signWidth,
                                point.y + m_center - Scale_Px(4)));
      // Appending the last point twice should allow for an abrupt 180° turn
      points.Append(new wxPoint(point.x + Scale_Px(1) + signWidth,
                                point.y + m_center - Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(1) + 3 * signWidth / 4,
                                point.y + m_center - 3 * signWidth / 4 - Scale_Px(4)));
      // The middle point of the 2nd run of the parenthesis is at a different place
      // making the parenthesis wider here
      points.Append(new wxPoint(point.x + Scale_Px(2),
                                point.y));
      points.Append(new wxPoint(point.x + Scale_Px(1) + 3 * signWidth / 4,
                                point.y - m_center + 3 * signWidth / 4 + Scale_Px(4)));
      points.Append(new wxPoint(point.x + Scale_Px(1) + signWidth,
                             point.y - m_center + Scale_Px(4)));
      adc->DrawSpline(&points);

      points.Clear();
      // Right bracket
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                                point.y - m_center + Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                                point.y - m_center + signWidth / 2 + Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1),
                                point.y));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                                point.y + m_center - signWidth / 2 - Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                                point.y + m_center - Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                                point.y + m_center - Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                                point.y + m_center - signWidth / 2 - Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(2),
                                point.y));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                                point.y - m_center + signWidth / 2 + Scale_Px(4)));
      points.Append(new wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                                point.y - m_center + Scale_Px(4)));
      adc->DrawSpline(&points);
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
  if(!m_innerCell)
    return "()";
  
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
    m_open->SetNextToDraw(m_innerCell.get());
    wxASSERT_MSG(m_last1 != NULL, _("Bug: No last cell inside a parenthesis!"));
    if (m_last1 != NULL)
      m_last1->SetNextToDraw(m_close.get());
    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_open.get();

    ResetData();
    m_height = wxMax(m_innerCell->GetHeightList(), m_open->GetHeightList());
    m_center = wxMax(m_innerCell->GetCenterList(), m_open->GetCenterList());
    return true;
  }
  return false;
}

void ParenCell::SetNextToDraw(Cell *next)
{
  if(m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
