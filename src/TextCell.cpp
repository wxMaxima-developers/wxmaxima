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
  m_greek = false;
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
  tmp->m_hidden = m_hidden;
  tmp->m_greek = m_greek;
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
  if (m_symbol && parser.HaveSymbolFont() && m_text == wxT("%pi"))
    dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);
  else if (m_greek && parser.HaveSymbolFont())
    dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);
  else
    dc.GetTextExtent(m_text, &m_width, &m_height);
  m_width = m_width + 2*SCALE_PX(2, scale);
  m_height = m_height + 2*SCALE_PX(2, scale);
  if (m_hidden)
    m_width = 0;
  m_center = m_height/2;
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void TextCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  MathCell::RecalculateSize(parser, fontsize, all);
}

void TextCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  wxString fontname = parser.GetFontName();
  
  if (m_width == 0)
    RecalculateWidths(parser, fontsize, false);
  
  if (DrawThisCell(parser, point) && !m_hidden) {
    SetFont(parser, fontsize);
    SetForeground(parser);
    
    if (m_symbol && parser.HaveSymbolFont() && m_text == wxT("%pi"))
        dc.DrawText(GetGreekString(parser),
                    point.x + SCALE_PX(2, scale),
                    point.y - m_center + SCALE_PX(2, scale));
    else if (m_greek && parser.HaveSymbolFont())
      dc.DrawText(GetGreekString(parser),
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
  else if (m_symbol) {
    if (parser.HaveSymbolFont() && m_text == wxT("%pi"))
      dc.SetFont(wxFont(fontsize1 + parser.GetSymbolFontAdj(),
                        wxMODERN,
                        parser.IsItalic(TS_NORMAL_TEXT),
                        parser.IsBold(TS_NORMAL_TEXT),
                        parser.IsUnderlined(TS_NORMAL_TEXT),
                        parser.GetSymbolFontName(),
                        parser.GetSymbolFontEncoding()));
    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_SPECIAL_CONSTANT),
                        parser.IsBold(TS_SPECIAL_CONSTANT),
                        parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                        parser.GetFontName()));
  }
  else if (m_greek) {
    if (parser.HaveSymbolFont())
      dc.SetFont(wxFont(fontsize1 + parser.GetSymbolFontAdj(),
                        wxMODERN,
                        parser.IsItalic(TS_NORMAL_TEXT),
                        parser.IsBold(TS_NORMAL_TEXT),
                        parser.IsUnderlined(TS_NORMAL_TEXT),
                        parser.GetSymbolFontName(),
                        parser.GetSymbolFontEncoding()));
    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_SPECIAL_CONSTANT),
                        parser.IsBold(TS_SPECIAL_CONSTANT),
                        parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                        parser.GetFontName()));
  }
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

void TextCell::SetForeground(CellParser& parser)
{
  wxDC& dc = parser.GetDC();
#if wxCHECK_VERSION(2, 5, 3)
  switch(m_style) {
    case TC_PROMPT:
      dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_OTHER_PROMPT)));
      break;
    case TC_MAIN_PROMPT:
      dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_MAIN_PROMPT)));
      break;
    case TC_ERROR:
      dc.SetTextForeground(wxTheColourDatabase->Find(wxT("red")));
      break;
    case TC_INPUT:
      dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_INPUT)));
      break;
    case TC_LABEL:
      dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_LABEL)));
      break;
    default:
     dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_NORMAL_TEXT)));
      break;
  }
#else
  switch(m_style) {
    case TC_PROMPT:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_OTHER_PROMPT))));
      break;
    case TC_MAIN_PROMPT:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_MAIN_PROMPT))));
      break;
    case TC_ERROR:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(wxT("red"))));
      break;
    case TC_INPUT:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_INPUT))));
      break;
    case TC_LABEL:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_LABEL))));
      break;
    default:
      dc.SetTextForeground(*(wxTheColourDatabase->FindColour(parser.GetColor(TS_NORMAL_TEXT))));
      break;
  }
#endif
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
  if (m_style == TC_STRING)
    text += wxT(" ");
  return text + MathCell::ToString(all);
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

void TextCell::Hide(bool hide)
{
  m_width = 0;
  if (hide)
    m_text = m_text + wxT(" << Hidden expression. >>");
  else
    m_text = m_text.Left(m_text.Length() - 25);
}

wxString TextCell::GetGreekString(CellParser& parser)
{
  if (parser.SymbolFontIso())
    return GetGreekStringIso();
  return GetGreekStringSym();
}

wxString TextCell::GetGreekStringSym()
{
  if (m_text == wxT("gamma"))
    return wxT("G");
  else if (m_text == wxT("zeta"))
    return wxT("z");

  else if (m_text == wxT("%alpha"))
    return wxT("a");
  else if (m_text == wxT("%beta"))
    return wxT("b");
  else if (m_text == wxT("%gamma"))
    return wxT("g");
  else if (m_text == wxT("%delta"))
    return wxT("d");
  else if (m_text == wxT("%epsilon"))
    return wxT("e");
  else if (m_text == wxT("%zeta"))
    return wxT("z");
  else if (m_text == wxT("%eta"))
    return wxT("h");
  else if (m_text == wxT("%theta"))
    return wxT("q");
  else if (m_text == wxT("%iota"))
    return wxT("i");
  else if (m_text == wxT("%kappa"))
    return wxT("k");
  else if (m_text == wxT("%lambda"))
    return wxT("l");
  else if (m_text == wxT("%mu"))
    return wxT("m");
  else if (m_text == wxT("%nu"))
    return wxT("n");
  else if (m_text == wxT("%xi"))
    return wxT("x");
  else if (m_text == wxT("%omicron"))
    return wxT("o");
  else if (m_text == wxT("%pi"))
    return wxT("p");
  else if (m_text == wxT("%rho"))
    return wxT("r");
  else if (m_text == wxT("%sigma"))
    return wxT("s");
  else if (m_text == wxT("%tau"))
    return wxT("t");
  else if (m_text == wxT("%upsilon"))
    return wxT("u");
  else if (m_text == wxT("%phi"))
    return wxT("f");
  else if (m_text == wxT("%chi"))
    return wxT("c");
  else if (m_text == wxT("%psi"))
    return wxT("y");
  else if (m_text == wxT("%omega"))
    return wxT("w");
  else if (m_text == wxT("%Alpha"))
    return wxT("A");
  else if (m_text == wxT("%Beta"))
    return wxT("B");
  else if (m_text == wxT("%Gamma"))
    return wxT("G");
  else if (m_text == wxT("%Delta"))
    return wxT("D");
  else if (m_text == wxT("%Epsilon"))
    return wxT("E");
  else if (m_text == wxT("%Zeta"))
    return wxT("Z");
  else if (m_text == wxT("%Eta"))
    return wxT("H");
  else if (m_text == wxT("%Theta"))
    return wxT("Q");
  else if (m_text == wxT("%Iota"))
    return wxT("I");
  else if (m_text == wxT("%Kappa"))
    return wxT("K");
  else if (m_text == wxT("%Lambda"))
    return wxT("L");
  else if (m_text == wxT("%Mu"))
    return wxT("M");
  else if (m_text == wxT("%Nu"))
    return wxT("N");
  else if (m_text == wxT("%Xi"))
    return wxT("X");
  else if (m_text == wxT("%Omicron"))
    return wxT("O");
  else if (m_text == wxT("%Rho"))
    return wxT("R");
  else if (m_text == wxT("%Sigma"))
    return wxT("S");
  else if (m_text == wxT("%Tau"))
    return wxT("T");
  else if (m_text == wxT("%Upsilon"))
    return wxT("U");
  else if (m_text == wxT("%Phi"))
    return wxT("F");
  else if (m_text == wxT("%Chi"))
    return wxT("C");
  else if (m_text == wxT("%Psi"))
    return wxT("Y");
  else if (m_text == wxT("%Omega"))
    return wxT("W");
  else if (m_text == wxT("%Pi"))
    return wxT("P");

  return m_text;
}

wxString TextCell::GetGreekStringIso()
{
  if (m_text == wxT("gamma"))
    return wxString::Format("%c", 195);
  else if (m_text == wxT("zeta"))
    return wxString::Format("%c", 230);

  else if (m_text == wxT("%alpha"))
    return wxString::Format("%c", 225);
  else if (m_text == wxT("%beta"))
    return wxString::Format("%c", 226);
  else if (m_text == wxT("%gamma"))
    return wxString::Format("%c", 227);
  else if (m_text == wxT("%delta"))
    return wxString::Format("%c", 228);
  else if (m_text == wxT("%epsilon"))
    return wxString::Format("%c", 229);
  else if (m_text == wxT("%zeta"))
    return wxString::Format("%c", 230);
  else if (m_text == wxT("%eta"))
    return wxString::Format("%c", 231);
  else if (m_text == wxT("%theta"))
    return wxString::Format("%c", 232);
  else if (m_text == wxT("%iota"))
    return wxString::Format("%c", 233);
  else if (m_text == wxT("%kappa"))
    return wxString::Format("%c", 234);
  else if (m_text == wxT("%lambda"))
    return wxString::Format("%c", 235);
  else if (m_text == wxT("%mu"))
    return wxString::Format("%c", 236);
  else if (m_text == wxT("%nu"))
    return wxString::Format("%c", 237);
  else if (m_text == wxT("%xi"))
    return wxString::Format("%c", 238);
  else if (m_text == wxT("%omicron"))
    return wxString::Format("%c", 239);
  else if (m_text == wxT("%rho"))
    return wxString::Format("%c", 241);
  else if (m_text == wxT("%pi"))
    return wxString::Format("%c", 240);
  else if (m_text == wxT("%sigma"))
    return wxString::Format("%c", 243);
  else if (m_text == wxT("%tau"))
    return wxString::Format("%c", 244);
  else if (m_text == wxT("%upsilon"))
    return wxString::Format("%c", 245);
  else if (m_text == wxT("%phi"))
    return wxString::Format("%c", 246);
  else if (m_text == wxT("%chi"))
    return wxString::Format("%c", 247);
  else if (m_text == wxT("%psi"))
    return wxString::Format("%c", 248);
  else if (m_text == wxT("%omega"))
    return wxString::Format("%c", 249);
  else if (m_text == wxT("%Alpha"))
    return wxString::Format("%c", 193);
  else if (m_text == wxT("%Beta"))
    return wxString::Format("%c", 194);
  else if (m_text == wxT("%Gamma"))
    return wxString::Format("%c", 195);
  else if (m_text == wxT("%Delta"))
    return wxString::Format("%c", 196);
  else if (m_text == wxT("%Epsilon"))
    return wxString::Format("%c", 197);
  else if (m_text == wxT("%Zeta"))
    return wxString::Format("%c", 198);
  else if (m_text == wxT("%Eta"))
    return wxString::Format("%c", 199);
  else if (m_text == wxT("%Theta"))
    return wxString::Format("%c", 200);
  else if (m_text == wxT("%Iota"))
    return wxString::Format("%c", 201);
  else if (m_text == wxT("%Kappa"))
    return wxString::Format("%c", 202);
  else if (m_text == wxT("%Lambda"))
    return wxString::Format("%c", 203);
  else if (m_text == wxT("%Mu"))
    return wxString::Format("%c", 204);
  else if (m_text == wxT("%Nu"))
    return wxString::Format("%c", 205);
  else if (m_text == wxT("%Xi"))
    return wxString::Format("%c", 206);
  else if (m_text == wxT("%Omicron"))
    return wxString::Format("%c", 207);
  else if (m_text == wxT("%Pi"))
    return wxString::Format("%c", 208);
  else if (m_text == wxT("%Rho"))
    return wxString::Format("%c", 209);
  else if (m_text == wxT("%Sigma"))
    return wxString::Format("%c", 211);
  else if (m_text == wxT("%Tau"))
    return wxString::Format("%c", 212);
  else if (m_text == wxT("%Upsilon"))
    return wxString::Format("%c", 213);
  else if (m_text == wxT("%Phi"))
    return wxString::Format("%c", 214);
  else if (m_text == wxT("%Chi"))
    return wxString::Format("%c", 215);
  else if (m_text == wxT("%Psi"))
    return wxString::Format("%c", 216);
  else if (m_text == wxT("%Omega"))
    return wxString::Format("%c", 217);

  return m_text;
}
