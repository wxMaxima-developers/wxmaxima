///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "ParenCell.h"
#include "TextCell.h"

#if defined __WXMSW__
 #define PAREN_LEFT_TOP "\xE6"
 #define PAREN_LEFT_BOTTOM "\xE8"
 #define PAREN_RIGHT_TOP "\xF6"
 #define PAREN_RIGHT_BOTTOM "\xF8"
 #define PAREN_LEFT_EXTEND "\xE7"
 #define PAREN_RIGHT_EXTEND "\xF7"
 #define PAREN_FONT_SIZE 12
#endif

#define PAREN_OPEN "\xB0"
#define PAREN_CLOSE "\xD1"
#define PAREN_OPEN_TOP "\x30"
#define PAREN_OPEN_EXTEND "\x42"
#define PAREN_OPEN_BOTTOM "\x40"
#define PAREN_CLOSE_TOP "\x31"
#define PAREN_CLOSE_EXTEND "\x43"
#define PAREN_CLOSE_BOTTOM "\x41"

#define TRANSFORM_SIZE(type, size) \
  (type == 0 ? size:                \
  type == 1 ? 2*size:              \
      (3*size)/2)

ParenCell::ParenCell() : MathCell()
{
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

void ParenCell::SetParent(MathCell *parent, bool all)
{
  if (m_innerCell != NULL)
    m_innerCell->SetParent(parent, true);
  if (m_open != NULL)
    m_open->SetParent(parent, true);
  if (m_close != NULL)
    m_close->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* ParenCell::Copy(bool all)
{
  ParenCell *tmp = new ParenCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->Copy(true), m_type);
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
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

  while (inner->m_next != NULL)
    inner = inner->m_next;
  m_last1 = inner;
}

void ParenCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  if (m_innerCell == NULL)
    m_innerCell = new TextCell;

  m_innerCell->RecalculateWidths(parser, fontsize, true);

  if (parser.CheckTeXFonts())
  {
    wxDC& dc = parser.GetDC();
    m_innerCell->RecalculateSize(parser, fontsize, true);
    int size = m_innerCell->GetMaxHeight();

    int fontsize1 = (int) ((fontsize * scale + 0.5));

    if (size < 2*fontsize1)
      m_bigParenType = 0;
    else if (size < 4*fontsize1)
      m_bigParenType = 1;
    else
      m_bigParenType = 2;

    if (m_bigParenType < 2)
    {
      m_parenFontSize = fontsize;
      fontsize1 = (int) ((m_parenFontSize * scale + 0.5));

      dc.SetFont( wxFont(fontsize1, wxFONTFAMILY_MODERN,
                        wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        m_bigParenType == 0 ?
                          parser.GetTeXCMRI() :
                            parser.GetTeXCMEX()));
      dc.GetTextExtent(m_bigParenType == 0 ? wxT("(") :
                       m_bigParenType == 1 ? wxT(PAREN_OPEN) :
                                             wxT(PAREN_OPEN_TOP),
                       &m_signWidth, &m_signSize);

      /// BUG 2897415: Exporting equations to HTML locks up on Mac
      ///  there is something wrong with what dc.GetTextExtent returns,
      ///  make sure there is no infinite loop!
      int i=0;
      while (m_signSize < TRANSFORM_SIZE(m_bigParenType, size) && i<20)
      {
        int fontsize1 = (int) ((m_parenFontSize++ * scale + 0.5));
        dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                          wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                          m_bigParenType == 0 ?
                             parser.GetTeXCMRI() :
                               parser.GetTeXCMEX()));
        dc.GetTextExtent(m_bigParenType == 0 ? wxT("(") :
                         m_bigParenType == 1 ? wxT(PAREN_OPEN) :
                                               wxT(PAREN_OPEN_TOP),
                         &m_signWidth, &m_signSize);
        i++;
      }
    }
    else
    {
      m_parenFontSize = fontsize;
      fontsize1 = (int) ((m_parenFontSize * scale + 0.5));
      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
    		  	  	  	wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        m_bigParenType < 1 ?
                          parser.GetTeXCMRI() :
                            parser.GetTeXCMEX()));
      dc.GetTextExtent(wxT(PAREN_OPEN), &m_signWidth, &m_signSize);
    }

    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth(scale) + 2*m_signWidth;
  }
  else
  {
#if defined __WXMSW__
    wxDC& dc = parser.GetDC();
    int fontsize1 = (int) ((PAREN_FONT_SIZE * scale + 0.5));
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      parser.IsItalic(TS_DEFAULT),
                      parser.IsBold(TS_DEFAULT),
                      parser.IsUnderlined(TS_DEFAULT),
                      parser.GetSymbolFontName()));
    dc.GetTextExtent(wxT(PAREN_LEFT_TOP), &m_charWidth, &m_charHeight);
    m_width = m_innerCell->GetFullWidth(scale) + 2*m_charWidth;
#else
    m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(12, parser.GetScale());
#endif
  }

  m_open->RecalculateWidths(parser, fontsize, false);
  m_close->RecalculateWidths(parser, fontsize, false);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void ParenCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSize(parser, fontsize, true);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(2, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(1, scale);

#if defined __WXMSW__
  if (!parser.CheckTeXFonts())
  {
    wxDC& dc = parser.GetDC();
    int fontsize1 = (int) ((fontsize * scale + 0.5));
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      false,
                      false,
                      false,
                      parser.GetFontName()));
    dc.GetTextExtent(wxT("("), &m_charWidth1, &m_charHeight1);
  }
#endif

  m_open->RecalculateSize(parser, fontsize, false);
  m_close->RecalculateSize(parser, fontsize, false);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void ParenCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();
    wxDC& dc = parser.GetDC();
    wxPoint in(point);

    if (parser.CheckTeXFonts())
    {
      in.x = point.x + m_signWidth;
      SetForeground(parser);
      int fontsize1 = (int) ((m_parenFontSize * scale + 0.5));
      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
    		            wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        m_bigParenType < 1 ?
                          parser.GetTeXCMRI() :
                            parser.GetTeXCMEX()));
      if (m_bigParenType < 2)
      {
        dc.DrawText(m_bigParenType == 0 ? wxT("(") :
                                          wxT(PAREN_OPEN),
                    point.x,
                    point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) -
                    (m_bigParenType > 0 ? m_signTop : 0));
        dc.DrawText(m_bigParenType == 0 ? wxT(")") :
                                          wxT(PAREN_CLOSE),
                    point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                    point.y - m_center + SCALE_PX(MC_TEXT_PADDING, scale) -
                    (m_bigParenType > 0 ? m_signTop : 0));
      }
      else {
        int top = point.y - m_center - m_signTop;
        int bottom = point.y + m_height - m_center - m_signTop - m_signSize / 2;
        dc.DrawText(wxT(PAREN_OPEN_TOP),
                    point.x,
                    top);
        dc.DrawText(wxT(PAREN_CLOSE_TOP),
                    point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                    top);
        dc.DrawText(wxT(PAREN_OPEN_BOTTOM),
                    point.x,
                    bottom);
        dc.DrawText(wxT(PAREN_CLOSE_BOTTOM),
                    point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                    bottom);
        top = top + m_signSize / 2;
        if (top <= bottom)
        {
          while (top < bottom)
          {
            dc.DrawText(wxT(PAREN_OPEN_EXTEND),
                          point.x,
                          top-1);
            dc.DrawText(wxT(PAREN_CLOSE_EXTEND),
                          point.x + m_width - m_signWidth,
                          top-1);
            top += m_signSize / 10;
          }
        }
      }
    }
    else
    {
#if defined __WXMSW__
      in.x += m_charWidth;
      int fontsize1 = (int) ((PAREN_FONT_SIZE * scale + 0.5));
      SetForeground(parser);
      if (m_height < (3*m_charHeight)/2)
      {
        fontsize1 = (int) ((fontsize * scale + 0.5));
        dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                          false,
                          false,
                          false,
                          parser.GetFontName()));
        dc.DrawText(wxT("("),
                    point.x + m_charWidth - m_charWidth1,
                    point.y - m_charHeight1 / 2);
        dc.DrawText(wxT(")"),
                    point.x + m_width - m_charWidth,
                    point.y - m_charHeight1 / 2);
      }
      else
      {
        dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                          false,
                          false,
                          false,
                          parser.GetSymbolFontName()));
        dc.DrawText(wxT(PAREN_LEFT_TOP),
                    point.x,
                    point.y - m_center);
        dc.DrawText(wxT(PAREN_LEFT_BOTTOM),
                    point.x,
                    point.y + m_height - m_center - m_charHeight);
        dc.DrawText(wxT(PAREN_RIGHT_TOP),
                    point.x + m_width - m_charWidth,
                    point.y - m_center);
        dc.DrawText(wxT(PAREN_RIGHT_BOTTOM),
                    point.x + m_width - m_charWidth,
                    point.y + m_height - m_center - m_charHeight);
        int top, bottom;
        top = point.y - m_center + m_charHeight/2;
        bottom = point.y + m_height - m_center - (4*m_charHeight)/3;
        if (top <= bottom)
        {
          while (top < bottom)
          {
            dc.DrawText(wxT(PAREN_LEFT_EXTEND),
                          point.x,
                          top);
            dc.DrawText(wxT(PAREN_RIGHT_EXTEND),
                          point.x + m_width - m_charWidth,
                          top);
            top += (2*m_charHeight)/3;
          }
          dc.DrawText(wxT(PAREN_LEFT_EXTEND),
                          point.x,
                          point.y + m_height - m_center - (3*m_charHeight)/2);
          dc.DrawText(wxT(PAREN_RIGHT_EXTEND),
                        point.x + m_width - m_charWidth,
                        point.y + m_height - m_center - (3*m_charHeight)/2);
        }
      }
#else
      in.x = point.x + SCALE_PX(6, scale);
      SetPen(parser);
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
      UnsetPen(parser);
#endif
    }
    m_innerCell->Draw(parser, in, fontsize, true);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString ParenCell::ToString(bool all)
{
  wxString s;
  if (!m_isBroken)
  {
    if (m_print)
      s = wxT("(") + m_innerCell->ToString(true) + wxT(")");
    else
      s = m_innerCell->ToString(true);
  }
  s += MathCell::ToString(all);
  return s;
}

wxString ParenCell::ToTeX(bool all)
{
  wxString s;
  if (!m_isBroken)
  {
    if (m_print)
      s = wxT("\\left( ") + m_innerCell->ToTeX(true) + wxT("\\right) ");
    else
      s = m_innerCell->ToTeX(true);
  }
  s += MathCell::ToTeX(all);
  return s;
}

wxString ParenCell::ToXML(bool all)
{
//  if( m_isBroken )
//    return wxEmptyString;
  wxString s = m_innerCell->ToXML(true);
  return ( ( m_print )? _T("<p>") + s + _T("</p>") : s ) +
    MathCell::ToXML(all);
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
    m_last1->m_nextToDraw = m_close;
    m_close->m_previousToDraw = m_last1;
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    return true;
  }
  return false;
}

void ParenCell::Unbreak(bool all)
{
  if (m_isBroken)
    m_innerCell->Unbreak(true);
  MathCell::Unbreak(all);
}
