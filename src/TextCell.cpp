/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "TextCell.h"

TextCell::TextCell() : MathCell()
{
  m_text = wxT("");
  m_symbol = false;
}


TextCell::~TextCell()
{
  if (m_next != NULL)
    delete m_next;
}

MathCell* TextCell::Copy(bool all)
{
  TextCell *tmp = new TextCell;
  tmp->m_text = wxString(m_text);
  tmp->m_symbol = m_symbol;
  tmp->m_style = m_style;
  tmp->m_forceBreakLine = m_forceBreakLine;
  tmp->m_nextToDrawIsNext = m_nextToDrawIsNext;
  tmp->m_bigSkip = m_bigSkip;
  if (all && m_nextToDraw!=NULL)
    tmp->AppendCell(m_nextToDraw->Copy(all));
  return tmp;
}

void TextCell::Destroy()
{
  m_next = NULL;
}

void TextCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();
  SetFont(parser, fontsize);
  dc.GetTextExtent(m_text, &m_width, &m_height);
  m_width = m_width + SCALE_PX(4, scale);
  m_height = m_height + SCALE_PX(4, scale);
  if (m_text == wxT("*"))
    m_width = 0;
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void TextCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();
  
  SetFont(parser, fontsize);
  dc.GetTextExtent(m_text, &m_width, &m_height);
  m_height = MAX(m_height, fontsize);
  m_height += SCALE_PX(4, scale);
  m_width += SCALE_PX(4, scale);
  if (m_text == wxT("*"))
    m_width = 0;
  m_center = m_height/2;
  MathCell::RecalculateSize(parser, fontsize, all);
}

void TextCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  wxString fontname = parser.GetFontName();
  
  
  if (DrawThisCell(parser, point) && m_text != wxT("*")) {
    SetFont(parser, fontsize);
    switch(m_style) {
      case TC_PROMPT:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_OTHER_PROMPT)));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_OTHER_PROMPT))));
#endif
        break;
      case TC_MAIN_PROMPT:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_MAIN_PROMPT)));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_MAIN_PROMPT))));
#endif
        break;
      case TC_ERROR:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(wxT("red")));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(wxT("red"))));
#endif
        break;
      case TC_INPUT:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_INPUT)));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_INPUT))));
#endif
        break;
      case TC_LABEL:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_LABEL)));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_LABEL))));
#endif
        break;
      case TC_VARIABLE:
      case TC_OPERATOR:
      case TC_NUMBER:
      default:
#if wxCHECK_VERSION(2, 5, 3)
        dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_NORMAL_TEXT)));
#else
        dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_NORMAL_TEXT))));
#endif
        break;
    }
    // This only happend in labels - labels are always on the beginnig
    // of a line so we can add some more text.
    if (!m_nextToDrawIsNext)
      dc.DrawText(m_text + _T(" << Hidden expression. >>"),
                  point.x + SCALE_PX(2, scale),
                  point.y - m_center + SCALE_PX(2, scale));
    else
      dc.DrawText(m_text,
                  point.x + SCALE_PX(2, scale),
                  point.y - m_center + SCALE_PX(2, scale));
  }
  MathCell::Draw(parser, point, fontsize, all);
}

void TextCell::SetFont(CellParser& parser, int fontsize)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();
  
  int fontsize1 = (int) (((double)fontsize)*scale + 0.5);
  fontsize1 = MAX(fontsize1, 1);
  
  if (!m_nextToDrawIsNext)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_HIDDEN_GROUP),
                      parser.IsBold(TS_HIDDEN_GROUP),
                      parser.IsUnderlined(TS_HIDDEN_GROUP),
                      parser.GetFontName()));
  else if (m_symbol)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_SPECIAL_CONSTANT),
                      parser.IsBold(TS_SPECIAL_CONSTANT),
                      parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                      parser.GetFontName()));
  else if (m_style == TC_MAIN_PROMPT)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_MAIN_PROMPT),
                      parser.IsBold(TS_MAIN_PROMPT),
                      parser.IsUnderlined(TS_MAIN_PROMPT),
                      parser.GetFontName()));
  else if (m_style == TC_PROMPT)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_OTHER_PROMPT),
                      parser.IsBold(TS_OTHER_PROMPT),
                      parser.IsUnderlined(TS_OTHER_PROMPT),
                      parser.GetFontName()));
  else if (m_style == TC_LABEL)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_LABEL),
                      parser.IsBold(TS_LABEL),
                      parser.IsUnderlined(TS_LABEL),
                      parser.GetFontName()));
  else if (m_style == TC_INPUT)
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_INPUT),
                      parser.IsBold(TS_INPUT),
                      parser.IsUnderlined(TS_INPUT),
                      parser.GetFontName()));
  else
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(TS_NORMAL_TEXT),
                      parser.IsBold(TS_NORMAL_TEXT),
                      parser.IsUnderlined(TS_NORMAL_TEXT),
                      parser.GetFontName()));
}

bool TextCell::IsOperator()
{
  if (wxString(wxT("+*/-")).Find(m_text)>=0)
    return true;
  return false;
}

wxString TextCell::ToString(bool all)
{
  wxString text = m_text;
  return text + MathCell::ToString(all);
}

void TextCell::SetSymbol(bool symbol)
{
  m_symbol = symbol;
}

wxString TextCell::GetDiffPart()
{
  return wxT(",") + m_text + wxT(",1");
}

bool TextCell::IsShortNum()
{
  if (m_next != NULL)
    return false;
  else if (m_text.Length() < 4)
    return true;
  return false;
}
