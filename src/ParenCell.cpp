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

ParenCell::ParenCell() : MathCell()
{
  m_last1 = NULL;
  m_innerCell = NULL;
  m_print = true;
  m_open = new TextCell(wxT("("));
  m_close = new TextCell(wxT(")"));
}


ParenCell::~ParenCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
  delete m_open;
  delete m_close;
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

MathCell* ParenCell::Copy()
{
  ParenCell *tmp = new ParenCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->CopyList(), m_type);
  tmp->m_isBroken = m_isBroken;

  return tmp;
}

void ParenCell::SetFont(int fontsize)
{
  Configuration *configuration = Configuration::Get();
  wxDC& dc = configuration->GetDC();
  double scale = configuration->GetScale();
  
  int fontSize1 = (int) (((double)fontsize) * scale + 0.5);
  fontSize1 = MAX(fontSize1, 1);
  
  wxFont font(fontSize1, wxFONTFAMILY_MODERN,
              wxFONTSTYLE_NORMAL,
              configuration->IsBold(m_textStyle),
              false, // parenthesis aren't underlined
              configuration->GetFontName(m_textStyle),
              configuration->GetFontEncoding());
  wxASSERT_MSG(font.IsOk(),_("Seems like something is broken with a font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));
  dc.SetFont(font);
  
  // A fallback if the font we selected is no more installed or isn't working at all.
  if(!dc.GetFont().IsOk())
  {
    dc.SetFont(wxFontInfo(10));
  }
}

void ParenCell::Destroy()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = NULL;
  m_next = NULL;
}

void ParenCell::SetInner(MathCell *inner, int type)
{
  if (inner == NULL)
    return ;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;
  m_type = type;

  // Tell the first of our inter cell not to begin with a multiplication dot.
  m_innerCell->m_SuppressMultiplicationDot=true;

  // Search for the last of the inner cells
  while (inner->m_next != NULL)
    inner = inner->m_next;
  m_last1 = inner;
}

void ParenCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = Configuration::Get();
  double scale = configuration->GetScale();

  m_bigParenType = normal;
  
  if (m_innerCell == NULL)
    m_innerCell = new TextCell;

  m_innerCell->RecalculateWidthsList(fontsize);

  wxDC& dc = configuration->GetDC();
  m_innerCell->RecalculateHeightList(fontsize);
  int height = m_innerCell->GetMaxHeight();
  /// BUG 2897415: Exporting equations to HTML locks up on Mac
  ///  there is something wrong with what dc.GetTextExtent returns,
  ///  make sure there is no infinite loop!
  // Avoid a possible infinite loop.
  if(height < 1) height = 1;
  SetFont(fontsize);
  
  dc.GetTextExtent( wxT("("),&m_parenWidth, &m_parenHeight);

  if(height >= m_parenHeight)
    m_bigParenType = small;
    
  
  if (height >= 1.4*m_parenHeight)
  {
    // An x^2 is slightly higher than an ordinary parenthesis. But it still more or
    // less fits into one. The current parenthesis contents is larger than that
    // => We need to combine the parenthesis out of more than one unicode character.
    m_bigParenType = assembled;
    dc.GetTextExtent(Paren_Open_Top(),
                       &m_parenWidth, &m_parenTopHeight);
    dc.GetTextExtent(Paren_Open_Mid(),
                     &m_parenWidth, &m_parenMidHeight);
    dc.GetTextExtent(Paren_Open_Bottom(),
                     &m_parenWidth, &m_parenBottomHeight);

    if((m_parenBottomHeight<1)||(m_parenTopHeight<1)||(m_parenMidHeight<1))
    {
      // Obviously the font we are using lacks the possibility to assemble big parenthesis.
      // Therefore we will draw them by hand.
      m_bigParenType = handdrawn;
      m_parenHeight  = height;
    }
    else
    {
      // Calculate how many middle pieces we need in order to reach the parenthesis height we want.
      m_parenMidNum = (height - m_parenTopHeight - m_parenBottomHeight + m_parenMidHeight / 2 - 1) / m_parenMidHeight;

      m_parenHeight  = m_parenTopHeight + m_parenBottomHeight + m_parenMidNum * m_parenMidHeight;
    }
  }

  m_width = m_innerCell->GetFullWidth(scale) + 2*m_parenWidth;

  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);
  ResetData();
}

void ParenCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = Configuration::Get();
  double scale = configuration->GetScale();

  m_open->RecalculateHeightList(fontsize);
  m_close->RecalculateHeightList(fontsize);

  if(m_isBroken)
  {
    m_center = MAX(m_open->GetMaxCenter(),m_innerCell->GetMaxCenter());
    m_height = MAX(m_open->GetMaxHeight(),m_innerCell->GetMaxHeight());
  }
  else
  {
    // The "fontsize * scale" part makes sure we leave some vertical space.
    m_height = m_parenHeight + fontsize * scale / 3;
    m_center = m_parenHeight/2;
  }
}

void ParenCell::Draw(wxPoint point, int fontsize)
{
  Configuration *configuration = Configuration::Get();
  MathCell::Draw(point, fontsize);
  if (DrawThisCell(point)&&(InUpdateRegion()))
  {
    double scale = configuration->GetScale();
    wxDC& dc = configuration->GetDC();
    SetForeground();
    SetFont(fontsize);
    switch(m_bigParenType)
    {
    case small:
    case normal:
      dc.DrawText(Paren_Open(),
                  point.x,
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale)
        );
      dc.DrawText(Paren_Close(),
                  point.x + m_parenWidth + m_innerCell->GetFullWidth(scale),
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale)
        );
      break;
      
    case assembled:
      // Draw the left parenthesis
      dc.DrawText(Paren_Open_Top(),
                  point.x,
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale)
        );
      for(int i=0;i<m_parenMidNum;i++)
      {
        dc.DrawText(Paren_Open_Mid(),
                    point.x,
                    point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) + m_parenTopHeight +
                    i * m_parenMidHeight
          );
      }
      dc.DrawText(Paren_Open_Bottom(),
                  point.x,
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) + m_parenTopHeight +
                  m_parenMidNum * m_parenMidHeight
        );

      // Draw the right parenthesis
      dc.DrawText(Paren_Close_Top(),
                  point.x + m_parenWidth + m_innerCell->GetFullWidth(scale),
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale)
        );
      for(int i=0;i<m_parenMidNum;i++)
      {
        dc.DrawText(Paren_Close_Mid(),
                    point.x + m_parenWidth + m_innerCell->GetFullWidth(scale),
                    point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) + m_parenTopHeight +
                    i * m_parenMidHeight
          );
      }
      dc.DrawText(Paren_Close_Bottom(),
                  point.x + m_parenWidth + m_innerCell->GetFullWidth(scale),
                  point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) + m_parenTopHeight +
                  m_parenMidNum * m_parenMidHeight
        );
      break;
      
    case handdrawn:
    default:
      SetPen();
      // left
      dc.DrawLine(point.x + SCALE_PX(5, scale),
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                  point.x + SCALE_PX(2, scale),
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale));
      dc.DrawLine(point.x + SCALE_PX(2, scale),
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale),
                  point.x + SCALE_PX(2, scale),
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale));
      dc.DrawLine(point.x + SCALE_PX(2, scale),
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale),
                  point.x + SCALE_PX(5, scale),
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
      // right
      dc.DrawLine(point.x + m_width - SCALE_PX(5, scale) - 1,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(1, scale),
                  point.x + m_width - SCALE_PX(2, scale) - 1,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1,
                  point.y - m_innerCell->GetMaxCenter() + SCALE_PX(7, scale),
                  point.x + m_width - SCALE_PX(2, scale) - 1,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(2, scale) - 1,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(7, scale),
                  point.x + m_width - SCALE_PX(5, scale) - 1,
                  point.y + m_innerCell->GetMaxDrop() - SCALE_PX(1, scale));
      UnsetPen();

    }

    wxPoint in(point);
    in.x += m_parenWidth;
    if(m_bigParenType != small)
    {
      in.y += m_innerCell->GetMaxCenter();
      in.y -= (m_innerCell->GetMaxHeight() + 1) / 2;
    }
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
    for(size_t i=0;i<innerCell.Length();i++)
      if(!wxIsalnum(innerCell[i]))
      {
        needsLeftRight = true;
        break;
      }
    
    if (m_print)
    {
      if(needsLeftRight)
        s = wxT("\\left( ") + m_innerCell->ListToTeX()  + wxT("\\right) ");
      else
        s = wxT("(") + m_innerCell->ListToTeX()  + wxT(")");
    }
    else
      s = m_innerCell->ListToTeX();
  }
  return s;
}

wxString ParenCell::ToOMML()
{
  return wxT("<m:d><m:dPr m:begChr=\"") + XMLescape(m_open->ToString()) + wxT("\" m:endChr=\"") +
    XMLescape(m_close->ToString()) + wxT("\"></m:dPr><m:e>") +
    m_innerCell->ListToOMML()+wxT("</m:e></m:d>");
}

wxString ParenCell::ToMathML()
{
  if(!m_print) return m_innerCell->ListToMathML();

  wxString open   = m_open->ToString();
  wxString close  = m_close->ToString();
  return(
    wxT("<mrow><mo>") + XMLescape(open) + wxT("</mo>") +
    m_innerCell->ListToMathML() +
    wxT("<mo>") + XMLescape(close) +  wxT("</mo></mrow>\n")
    );
}

wxString ParenCell::ToXML()
{
//  if( m_isBroken )
//    return wxEmptyString;
  wxString s = m_innerCell->ListToXML();
  return ( ( m_print )? _T("<r><p>") + s + _T("</p></r>") : s );
}

void ParenCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
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
    wxASSERT_MSG(m_last1 != NULL,_("Bug: No last cell inside a parenthesis!"));
    if(m_last1 != NULL)
    {
      m_last1->m_nextToDraw = m_close;
      m_close->m_previousToDraw = m_last1;
    }
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    m_center = MAX(m_open->GetMaxCenter(),m_innerCell->GetMaxCenter());
    m_height = MAX(m_open->GetMaxHeight(),m_innerCell->GetMaxHeight());
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

