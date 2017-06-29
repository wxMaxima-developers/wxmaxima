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
  This file defines the class ParenCell

  ParenCell is the MathCell type that represents a math element that is kept
  between parenthesis.
 */

#include "ParenCell.h"
#include "TextCell.h"

#define PAREN_OPEN_TOP_UNICODE     "\x239b"
#define PAREN_OPEN_EXTEND_UNICODE  "\x239c"
#define PAREN_OPEN_BOTTOM_UNICODE  "\x239d"
#define PAREN_CLOSE_TOP_UNICODE    "\x239e"
#define PAREN_CLOSE_EXTEND_UNICODE "\x239f"
#define PAREN_CLOSE_BOTTOM_UNICODE "\x23a0"

ParenCell::ParenCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_cellPointers = cellPointers;
  m_numberOfExtensions = 0;
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
  m_parenFontSize = 12;
  m_bigParenType = ascii;
  m_innerCell = NULL;
  m_print = true;
  m_open = new TextCell(parent, config, cellPointers, wxT("("));
  m_close = new TextCell(parent, config, cellPointers, wxT(")"));
}

void ParenCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_innerCell != NULL)
    m_innerCell->SetParentList(parent);
  if (m_open != NULL)
    m_open->SetParentList(parent);
  if (m_close != NULL)
    m_close->SetParentList(parent);
}

MathCell *ParenCell::Copy()
{
  ParenCell *tmp = new ParenCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList(), m_type);
  tmp->m_isBroken = m_isBroken;

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

void ParenCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_innerCell, m_open, m_close);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}

void ParenCell::SetInner(MathCell *inner, int type)
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
  Configuration *configuration = (*m_configuration);
  wxDC &dc = configuration->GetDC();
  double scale = configuration->GetScale();

  wxString fontName;
  wxFontStyle fontStyle;
  wxFontWeight fontWeight;
  wxFontEncoding fontEncoding;
  bool underlined = configuration->IsUnderlined(TS_DEFAULT);

  // Ensure a sane minimum font size
  if (fontsize < 4)
    fontsize = 4;
  m_parenFontSize = fontsize;
  
  // The font size scales with the worksheet
  int fontsize1 = (int) (((double) fontsize) * scale + 0.5);

  fontEncoding = configuration->GetFontEncoding();

  switch(m_bigParenType)
  {
  case ascii:
  case assembled_unicode:
    fontName = configuration->GetFontName(TS_DEFAULT);
    break;

  case assembled_unicode_fallbackfont:
    fontName = wxT("Linux Libertine");
    break;

  case assembled_unicode_fallbackfont2:
    fontName = wxT("Linux Libertine O");
    break;

  default:
    fontName = configuration->GetFontName(TS_DEFAULT);
  }
  fontStyle = configuration->IsItalic(TS_DEFAULT);
  fontWeight = configuration->IsBold(TS_DEFAULT);
  fontName = configuration->GetFontName(TS_DEFAULT);

  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(fontName);
  if((m_bigParenType == assembled_unicode_fallbackfont) ||
     (m_bigParenType == assembled_unicode_fallbackfont2))
    font.SetEncoding(wxFONTENCODING_UTF8);
  else
    font.SetEncoding(fontEncoding);
  font.SetStyle(fontStyle);
  font.SetWeight(fontWeight);
  font.SetUnderlined(underlined);
  if (!font.IsOk())
  {
    font.SetFamily(wxFONTFAMILY_MODERN);
    font.SetEncoding(fontEncoding);
    font.SetStyle(fontStyle);
    font.SetWeight(fontWeight);
    font.SetUnderlined(underlined);
  }

  if (!font.IsOk())
    font = *wxNORMAL_FONT;

  font.SetPointSize(fontsize1);

  // A fallback if we have been completely unable to set a working font
  if (!dc.GetFont().IsOk())
    m_bigParenType = handdrawn;

  if(m_bigParenType != handdrawn)
    dc.SetFont(font);
}

void ParenCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  
  // Add a dummy contents to empty parenthesis
  if (m_innerCell == NULL)
    m_innerCell = new TextCell(m_group, m_configuration, m_cellPointers);
  
  m_innerCell->RecalculateWidthsList(fontsize);
  m_innerCell->RecalculateHeightList(fontsize);
  
  wxDC &dc = configuration->GetDC();
  int size = m_innerCell->GetMaxHeight();
  if (fontsize < 4) fontsize = 4;
  int fontsize1 = (int) ((fontsize * scale + 0.5));
  // If our font provides all the unicode chars we need we don't need
  // to bother which exotic method we need to use for drawing nice parenthesis.
  if (fontsize1*3 > size)
  {
    m_bigParenType = ascii;
    m_open->RecalculateWidthsList(fontsize);
    m_close->RecalculateWidthsList(fontsize);
    m_signWidth = m_open->GetWidth();
    m_signHeight= m_open->GetHeight();
  }
  else
  {
    m_bigParenType = assembled_unicode;
    SetFont(fontsize);
    int signWidth1,signWidth2,signWidth3,descent,leading;
    dc.GetTextExtent(wxT(PAREN_OPEN_TOP_UNICODE),    &signWidth1, &m_signTopHeight, &descent, &leading);
    m_signTopHeight -= descent + 1;
    dc.GetTextExtent(wxT(PAREN_OPEN_EXTEND_UNICODE), &signWidth2, &m_extendHeight, &descent, &leading);
    m_extendHeight -= descent + 1;
    dc.GetTextExtent(wxT(PAREN_OPEN_BOTTOM_UNICODE), &signWidth3, &m_signBotHeight, &descent, &leading);
    m_signBotHeight -= descent + 1;
    
    if(
      (signWidth1 < 1 ) ||
      (signWidth2 < 1 ) ||
      (signWidth3 < 1 ) ||
      (m_signTopHeight < 1) ||
      (m_extendHeight < 1) ||
      (m_signBotHeight < 1)
      )
    {
      m_bigParenType = assembled_unicode_fallbackfont;
      SetFont(fontsize);
      dc.GetTextExtent(wxT(PAREN_OPEN_TOP_UNICODE),    &signWidth1, &m_signTopHeight, &descent);
      dc.GetTextExtent(wxT(PAREN_OPEN_EXTEND_UNICODE), &signWidth2, &m_extendHeight, &descent);
      dc.GetTextExtent(wxT(PAREN_OPEN_BOTTOM_UNICODE),    &signWidth3, &m_signBotHeight, &descent);
      
      if(
        (signWidth1 < 1 ) ||
        (signWidth2 < 1 ) ||
        (signWidth3 < 1 ) ||
        (m_signTopHeight < 1) ||
        (m_extendHeight < 1) ||
        (m_signBotHeight < 1)
        )
      {
        m_bigParenType = assembled_unicode_fallbackfont2;
        SetFont(fontsize);
        dc.GetTextExtent(wxT(PAREN_OPEN_TOP_UNICODE),    &signWidth1, &m_signTopHeight, &descent);
        dc.GetTextExtent(wxT(PAREN_OPEN_EXTEND_UNICODE), &signWidth2, &m_extendHeight, &descent);
        dc.GetTextExtent(wxT(PAREN_OPEN_BOTTOM_UNICODE),    &signWidth3, &m_signBotHeight, &descent);
        
        if(
          (signWidth1 < 1 ) ||
          (signWidth2 < 1 ) ||
          (signWidth3 < 1 ) ||
          (m_signTopHeight < 1) ||
          (m_extendHeight < 1) ||
          (m_signBotHeight < 1)
          )
          m_bigParenType = handdrawn;
      }
    }
    
    if(m_bigParenType != handdrawn)
    {
      m_signWidth = signWidth1;
      if(m_signWidth < signWidth2)
        m_signWidth = signWidth2;
      if(m_signWidth < signWidth3)
        m_signWidth = signWidth3;
      m_numberOfExtensions = ((size - m_signTopHeight - m_signBotHeight + m_extendHeight/ 2 - 1) / m_extendHeight);
      if(m_numberOfExtensions < 0)
        m_numberOfExtensions = 0;
      m_signHeight = m_signTopHeight + m_signBotHeight + m_extendHeight * m_numberOfExtensions;
      m_height = MAX(m_signHeight,m_innerCell->GetMaxHeight()) + SCALE_PX(2, scale);
      m_center = m_signHeight / 2;
    }
    else
      m_signWidth = SCALE_PX(6, configuration->GetScale()) + (*m_configuration)->GetDefaultLineWidth();
  }
  m_width = m_innerCell->GetFullWidth(scale) + m_signWidth * 2;
  ResetData();
}

void ParenCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();
  m_innerCell->RecalculateHeightList(fontsize);
  m_height = MAX(m_signHeight,m_innerCell->GetMaxHeight()) + SCALE_PX(2, scale);
  m_center = m_height / 2;

  SetFont(fontsize);
  wxDC& dc = configuration->GetDC();
  dc.GetTextExtent(wxT("("), &m_charWidth1, &m_charHeight1);
  if(m_charHeight1 < 2)
    m_charHeight1 = 2;

  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);

  if (m_isBroken)
  {
    m_height = MAX(m_innerCell->GetMaxHeight(), m_open->GetMaxHeight());
    m_center = MAX(m_innerCell->GetMaxCenter(), m_open->GetMaxCenter());
  }
}

void ParenCell::Draw(wxPoint point, int fontsize)
{
  Configuration *configuration = (*m_configuration);
  MathCell::Draw(point, fontsize);
  if (DrawThisCell(point) && (InUpdateRegion()))
  {
    double scale = configuration->GetScale();
    wxDC &dc = configuration->GetDC();
    wxPoint in(point);

    in.x = point.x;
    SetForeground();
    SetFont(m_parenFontSize);
    
    switch(m_bigParenType)
    {            
    case ascii:
      m_open->DrawList(point, fontsize);
      m_close->DrawList(wxPoint(point.x + m_signWidth + m_innerCell->GetFullWidth(scale),point.y), fontsize);
      in.x += m_open->GetWidth();
      break;
    case assembled_unicode:
    case assembled_unicode_fallbackfont:
    {
      int top = point.y - m_center + SCALE_PX (1,scale);
      int bottom = top + m_signHeight - m_signBotHeight - SCALE_PX (2,scale);
      dc.DrawText(wxT(PAREN_OPEN_TOP_UNICODE),
                    point.x,
                  top);
      dc.DrawText(wxT(PAREN_CLOSE_TOP_UNICODE),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                  top);
      dc.DrawText(wxT(PAREN_OPEN_BOTTOM_UNICODE),
                  point.x,
                  bottom);
      dc.DrawText(wxT(PAREN_CLOSE_BOTTOM_UNICODE),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                  bottom);
      
      for (int i = 0;i < m_numberOfExtensions;i++)
      {
        dc.DrawText(wxT(PAREN_OPEN_EXTEND_UNICODE),
                    point.x,
                    top + m_signTopHeight + i*m_extendHeight);
        dc.DrawText(wxT(PAREN_CLOSE_EXTEND_UNICODE),
                    point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                    top + m_signTopHeight + i*m_extendHeight);
      }
      
      in.x += m_signWidth;
      // Center the contents of the parenthesis vertically.
      in.y += (m_innerCell->GetCenter() - m_innerCell->GetMaxHeight() /2);
    }
    break;
    case handdrawn:
      in.x = point.x + SCALE_PX(6, scale) + (*m_configuration)->GetDefaultLineWidth();
      SetPen();
      // left
      dc.DrawLine(point.x + SCALE_PX(5, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                  point.x + SCALE_PX(2, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale));
      dc.DrawLine(point.x + SCALE_PX(2, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale),
                  point.x + SCALE_PX(2, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale));
      dc.DrawLine(point.x + SCALE_PX(2, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale),
                  point.x + SCALE_PX(5, scale) + (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
      // right
      dc.DrawLine(point.x + m_width - SCALE_PX(5, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                  point.x + m_width - SCALE_PX(2, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale),
                  point.x + m_width - SCALE_PX(2, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale),
                  point.x + m_width - SCALE_PX(5, scale) - 1 - (*m_configuration)->GetDefaultLineWidth() / 2,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
      break;
    }
    
    UnsetPen();
    m_innerCell->DrawList(in, fontsize);
  }
}

wxString ParenCell::ToString()
{
  wxString s;
  if (!m_isBroken)
  {
    if (m_print)
      s = wxT("(") + m_innerCell->ListToString() + wxT(")");
    else
      s = m_innerCell->ListToString();
  }
  return s;
}

wxString ParenCell::ToTeX()
{
  wxString s;
  if (!m_isBroken)
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
//  if( m_isBroken )
//    return wxEmptyString;
  wxString s = m_innerCell->ListToXML();
  return ((m_print) ? _T("<r><p>") + s + _T("</p></r>") : s);
}

void ParenCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
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

bool ParenCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
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
  if (m_isBroken)
    m_innerCell->UnbreakList();
  MathCell::Unbreak();
}
