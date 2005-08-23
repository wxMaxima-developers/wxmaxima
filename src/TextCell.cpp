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
  m_text = wxEmptyString;
  m_symbol = false;
  m_greek = false;
  m_fontSize = -1;
}

TextCell::TextCell(wxString text) : MathCell()
{
  m_text = text;
  m_symbol = false;
  m_greek = false;
  m_text.Replace(wxT("\n"), wxEmptyString);
}

TextCell::~TextCell()
{
  if (m_next != NULL)
    delete m_next;
}

void TextCell::SetValue(wxString text)
{
  m_text = text;
  m_text.Replace(wxT("\n"), wxEmptyString);
}

MathCell* TextCell::Copy(bool all)
{
  TextCell *tmp = new TextCell(wxEmptyString);
  tmp->m_text = wxString(m_text);
  tmp->m_symbol = m_symbol;
  tmp->m_style = m_style;
  tmp->m_forceBreakLine = m_forceBreakLine;
  tmp->m_isFolded = m_isFolded;
  tmp->m_bigSkip = m_bigSkip;
  tmp->m_isHidden = m_isHidden;
  tmp->m_greek = m_greek;
  if (all && m_next!=NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void TextCell::Destroy()
{
  m_next = NULL;
}

void TextCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  if (m_height == -1 || m_width == -1 || fontsize != m_fontSize || parser.ForceUpdate()) {
    m_fontSize = fontsize;
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    SetFont(parser, fontsize);
    if (m_symbol && parser.HaveSymbolFont() && m_text == wxT("%pi"))
      dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);
    else if (m_greek && parser.HaveSymbolFont())
      dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);
    else if (m_text == wxEmptyString) {
      dc.GetTextExtent(wxT("X"), &m_width, &m_height);
      m_width = 0;
    }
    else
      dc.GetTextExtent(m_text, &m_width, &m_height);
    m_width = m_width + 2*SCALE_PX(2, scale);
    m_height = m_height + 2*SCALE_PX(2, scale);
    if (m_isHidden) {
      m_height = 0;
      m_width = -2*MC_CELL_SKIP;
    }
    m_center = m_height/2;
  }
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

  if (m_width == -1 || m_height == -1)
    RecalculateWidths(parser, fontsize, false);

  if (DrawThisCell(parser, point) && !m_isHidden) {
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

  if (m_isFolded)
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

void TextCell::Fold(bool fold)
{
  m_isFolded = fold;
  m_width = -1;
  if (fold)
    m_text = m_text + wxT(" << Hidden expression. >>");
  else
    m_text = m_text.Left(m_text.Length() - 25);
}

wxString TextCell::GetGreekString(CellParser& parser)
{
#if defined __WXGTK20__
  return wxConvLocal.cWC2WX(GetGreekStringUnicode());
#else
  return GetGreekStringIso();
#endif
}

#if defined __WXGTK20__

wchar_t* TextCell::GetGreekStringUnicode()
{
  if (m_text == wxT("gamma"))
    return L"\x0393";
  else if (m_text == wxT("zeta"))
    return L"\x03B6";

  else if (m_text == wxT("%alpha"))
    return L"\x03B1";
  else if (m_text == wxT("%beta"))
    return L"\x03B2";
  else if (m_text == wxT("%gamma"))
    return L"\x03B3";
  else if (m_text == wxT("%delta"))
    return L"\x03B4";
  else if (m_text == wxT("%epsilon"))
    return L"\x03B5";
  else if (m_text == wxT("%zeta"))
    return L"\x03B6";
  else if (m_text == wxT("%eta"))
    return L"\x03B7";
  else if (m_text == wxT("%theta"))
    return L"\x03B8";
  else if (m_text == wxT("%iota"))
    return L"\x03B9";
  else if (m_text == wxT("%kappa"))
    return L"\x03BA";
  else if (m_text == wxT("%lambda"))
    return L"\x03BB";
  else if (m_text == wxT("%mu"))
    return L"\x03BC";
  else if (m_text == wxT("%nu"))
    return L"\x03BD";
  else if (m_text == wxT("%xi"))
    return L"\x03BE";
  else if (m_text == wxT("%omicron"))
    return L"\x03BF";
  else if (m_text == wxT("%pi"))
    return L"\x03C0";
  else if (m_text == wxT("%rho"))
    return L"\x03C1";
  else if (m_text == wxT("%sigma"))
    return L"\x03C3";
  else if (m_text == wxT("%tau"))
    return L"\x03C4";
  else if (m_text == wxT("%upsilon"))
    return L"\x03C5";
  else if (m_text == wxT("%phi"))
    return L"\x03C6";
  else if (m_text == wxT("%chi"))
    return L"\x03C7";
  else if (m_text == wxT("%psi"))
    return L"\x03C8";
  else if (m_text == wxT("%omega"))
    return L"\x03C9";
  else if (m_text == wxT("%Alpha"))
    return L"\x0391";
  else if (m_text == wxT("%Beta"))
    return L"\x0392";
  else if (m_text == wxT("%Gamma"))
    return L"\x0393";
  else if (m_text == wxT("%Delta"))
    return L"\x0394";
  else if (m_text == wxT("%Epsilon"))
    return L"\x0395";
  else if (m_text == wxT("%Zeta"))
    return L"\x0396";
  else if (m_text == wxT("%Eta"))
    return L"\x0397";
  else if (m_text == wxT("%Theta"))
    return L"\x0398";
  else if (m_text == wxT("%Iota"))
    return L"\x0399";
  else if (m_text == wxT("%Kappa"))
    return L"\x039A";
  else if (m_text == wxT("%Lambda"))
    return L"\x039B";
  else if (m_text == wxT("%Mu"))
    return L"\x039C";
  else if (m_text == wxT("%Nu"))
    return L"\x039D";
  else if (m_text == wxT("%Xi"))
    return L"\x039E";
  else if (m_text == wxT("%Omicron"))
    return L"\x039F";
  else if (m_text == wxT("%Pi"))
    return L"\x03A0";
  else if (m_text == wxT("%Rho"))
    return L"\x03A1";
  else if (m_text == wxT("%Sigma"))
    return L"\x03A3";
  else if (m_text == wxT("%Tau"))
    return L"\x03A4";
  else if (m_text == wxT("%Upsilon"))
    return L"\x03A5";
  else if (m_text == wxT("%Phi"))
    return L"\x03A6";
  else if (m_text == wxT("%Chi"))
    return L"\x03A7";
  else if (m_text == wxT("%Psi"))
    return L"\x03A8";
  else if (m_text == wxT("%Omega"))
    return L"\x03A9";

  return L"";
}

#else

wxString TextCell::GetGreekStringIso()
{
  if (m_text == wxT("gamma"))
    return wxT("\xC3");
  else if (m_text == wxT("zeta"))
    return wxT("\xE6");

  else if (m_text == wxT("%alpha"))
    return wxT("\xE1");
  else if (m_text == wxT("%beta"))
    return wxT("\xE2");
  else if (m_text == wxT("%gamma"))
    return wxT("\xE3");
  else if (m_text == wxT("%delta"))
    return wxT("\xE4");
  else if (m_text == wxT("%epsilon"))
    return wxT("\xE5");
  else if (m_text == wxT("%zeta"))
    return wxT("\xE6");
  else if (m_text == wxT("%eta"))
    return wxT("\xE7");
  else if (m_text == wxT("%theta"))
    return wxT("\xE8");
  else if (m_text == wxT("%iota"))
    return wxT("\xE9");
  else if (m_text == wxT("%kappa"))
    return wxT("\xEA");
  else if (m_text == wxT("%lambda"))
    return wxT("\xEB");
  else if (m_text == wxT("%mu"))
    return wxT("\xEC");
  else if (m_text == wxT("%nu"))
    return wxT("\xED");
  else if (m_text == wxT("%xi"))
    return wxT("\xEE");
  else if (m_text == wxT("%omicron"))
    return wxT("\xEF");
  else if (m_text == wxT("%pi"))
    return wxT("\xF0");
  else if (m_text == wxT("%rho"))
    return wxT("\xF1");
  else if (m_text == wxT("%sigma"))
    return wxT("\xF3");
  else if (m_text == wxT("%tau"))
    return wxT("\xF4");
  else if (m_text == wxT("%upsilon"))
    return wxT("\xF5");
  else if (m_text == wxT("%phi"))
    return wxT("\xF6");
  else if (m_text == wxT("%chi"))
    return wxT("\xF7");
  else if (m_text == wxT("%psi"))
    return wxT("\xF8");
  else if (m_text == wxT("%omega"))
    return wxT("\xF9");
  else if (m_text == wxT("%Alpha"))
    return wxT("\xC1");
  else if (m_text == wxT("%Beta"))
    return wxT("\xC2");
  else if (m_text == wxT("%Gamma"))
    return wxT("\xC3");
  else if (m_text == wxT("%Delta"))
    return wxT("\xC4");
  else if (m_text == wxT("%Epsilon"))
    return wxT("\xC5");
  else if (m_text == wxT("%Zeta"))
    return wxT("\xC6");
  else if (m_text == wxT("%Eta"))
    return wxT("\xC7");
  else if (m_text == wxT("%Theta"))
    return wxT("\xC8");
  else if (m_text == wxT("%Iota"))
    return wxT("\xC9");
  else if (m_text == wxT("%Kappa"))
    return wxT("\xCA");
  else if (m_text == wxT("%Lambda"))
    return wxT("\xCB");
  else if (m_text == wxT("%Mu"))
    return wxT("\xCC");
  else if (m_text == wxT("%Nu"))
    return wxT("\xCD");
  else if (m_text == wxT("%Xi"))
    return wxT("\xCE");
  else if (m_text == wxT("%Omicron"))
    return wxT("\xCF");
  else if (m_text == wxT("%Pi"))
    return wxT("\xD0");
  else if (m_text == wxT("%Rho"))
    return wxT("\xD1");
  else if (m_text == wxT("%Sigma"))
    return wxT("\xD3");
  else if (m_text == wxT("%Tau"))
    return wxT("\xD4");
  else if (m_text == wxT("%Upsilon"))
    return wxT("\xD5");
  else if (m_text == wxT("%Phi"))
    return wxT("\xD6");
  else if (m_text == wxT("%Chi"))
    return wxT("\xD7");
  else if (m_text == wxT("%Psi"))
    return wxT("\xD8");
  else if (m_text == wxT("%Omega"))
    return wxT("\xD9");

  return m_text;
}

#endif
