///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "TextCell.h"
#include "Setup.h"

TextCell::TextCell() : MathCell()
{
  m_text = wxEmptyString;
  m_fontSize = -1;
  m_highlight = false;
}

TextCell::TextCell(wxString text) : MathCell()
{
  m_text = text;
  m_text.Replace(wxT("\n"), wxEmptyString);
  m_highlight = false;
}

TextCell::~TextCell()
{
  if (m_next != NULL)
    delete m_next;
}

void TextCell::SetValue(wxString text)
{
  m_text = text;
  m_width = -1;
  m_text.Replace(wxT("\n"), wxEmptyString);
}

MathCell* TextCell::Copy(bool all)
{
  TextCell *tmp = new TextCell(wxEmptyString);
  CopyData(this, tmp);
  tmp->m_text = wxString(m_text);
  tmp->m_forceBreakLine = m_forceBreakLine;
  tmp->m_bigSkip = m_bigSkip;
  tmp->m_isHidden = m_isHidden;
  tmp->m_textStyle = m_textStyle;
  tmp->m_highlight = m_highlight;
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void TextCell::Destroy()
{
  m_next = NULL;
}

void TextCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  if (m_height == -1 || m_width == -1 || fontsize != m_fontSize || parser.ForceUpdate())
  {
    m_fontSize = m_fontSizeTeX = fontsize;

    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    SetFont(parser, fontsize);

    if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_MAIN_PROMPT)) {
      dc.GetTextExtent(wxT("(\%oXXX)"), &m_width, &m_height);
      m_fontSizeLabel = m_fontSize;
      dc.GetTextExtent(m_text, &m_labelWidth, &m_labelHeight);
      while (m_labelWidth >= m_width) {
        int fontsize1 = (int) (((double) --m_fontSizeLabel) * scale + 0.5);
        dc.SetFont(wxFont(fontsize1, wxMODERN,
              parser.IsItalic(m_textStyle),
              parser.IsBold(m_textStyle),
              false, //parser.IsUnderlined(m_textStyle),
              parser.GetFontName(m_textStyle),
              parser.GetFontEncoding()));
        dc.GetTextExtent(m_text, &m_labelWidth, &m_labelHeight);
      }
    }

    else if (parser.CheckTeXFonts() && (m_text == wxT("inf") || m_text == wxT("->") ||
             m_text == wxT(">=") || m_text == wxT("<=")))
    {
      dc.GetTextExtent(GetSymbolString(parser), &m_width, &m_height);
      m_height = m_height / 2;
    }


    else if (parser.CheckTeXFonts() && m_text == wxT("-"))
      dc.GetTextExtent(wxT("\x7B"), &m_width, &m_height);

#if defined __WXMSW__
    else if (m_text == wxT("inf") || m_text == wxT("->") ||
             m_text == wxT(">=") || m_text == wxT("<="))
      dc.GetTextExtent(GetSymbolString(parser), &m_width, &m_height);
#endif

    else if (m_textStyle == TS_GREEK_CONSTANT && parser.CheckTeXFonts())
      dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);

#if defined __WXMSW__
    else if (m_textStyle == TS_GREEK_CONSTANT)
      dc.GetTextExtent(GetGreekString(parser), &m_width, &m_height);
#endif

    else if (m_text == wxEmptyString)
    {
      dc.GetTextExtent(wxT("X"), &m_width, &m_height);
      m_width = 0;
    }

    else
      dc.GetTextExtent(m_text, &m_width, &m_height);

    m_width = m_width + 2 * SCALE_PX(MC_TEXT_PADDING, scale);
    m_height = m_height + 2 * SCALE_PX(MC_TEXT_PADDING, scale);

    if (m_isHidden)
    {
      m_height = 0;
      m_width = m_width / 4;
    }

    m_realCenter = m_center = m_height / 2;
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

  if (m_width == -1 || m_height == -1)
    RecalculateWidths(parser, fontsize, false);

  if (DrawThisCell(parser, point) && !m_isHidden)
  {
    SetFont(parser, fontsize);
    SetForeground(parser);

    if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_MAIN_PROMPT)) {
      SetFont(parser, m_fontSizeLabel);
      dc.DrawText(m_text,
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale) + (m_width - m_labelWidth),
                  point.y - m_realCenter + (m_height - m_labelHeight)/2);
    }

    else if (parser.CheckTeXFonts() && (m_text == wxT("inf") || m_text == wxT("->") ||
                 m_text == wxT(">=") || m_text == wxT("<=")))
      dc.DrawText(GetSymbolString(parser),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

#if defined __WXMSW__
    else if (m_text == wxT("inf") || m_text == wxT("->") ||
             m_text == wxT(">=") || m_text == wxT("<="))
      dc.DrawText(GetSymbolString(parser),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));
#endif


    else if (parser.CheckTeXFonts() && m_text == wxT("-"))
      dc.DrawText(wxT("\x7B"),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

    else if (m_textStyle == TS_GREEK_CONSTANT && parser.CheckTeXFonts())
      dc.DrawText(GetGreekString(parser),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

#if defined __WXMSW__
    else if (m_textStyle == TS_GREEK_CONSTANT)
      dc.DrawText(GetGreekString(parser),
          point.x + SCALE_PX(MC_TEXT_PADDING, scale),
          point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));
#endif

#if defined __WXMSW__ || wxUSE_UNICODE
    else if (parser.GetChangeAsterisk() &&  m_text == wxT("*"))
    dc.DrawText(wxT("\xB7"),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));
#endif

    else
      dc.DrawText(m_text,
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));
  }
  MathCell::Draw(parser, point, fontsize, all);
}

void TextCell::SetFont(CellParser& parser, int fontsize)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();

  int fontsize1 = (int) (((double)fontsize) * scale + 0.5);
  int fontsize1TeX = (int) (((double)m_fontSizeTeX) * scale + 0.5);

  if ((m_textStyle == TS_TITLE) ||
      (m_textStyle == TS_SECTION) ||
      (m_textStyle == TS_SUBSECTION)) {
    fontsize1 = parser.GetFontSize(m_textStyle);
    fontsize1 = (int) (((double)fontsize1) * scale + 0.5);
  }

  fontsize1 = MAX(fontsize1, 1);

  switch(m_textStyle)
  {
  case TS_SPECIAL_CONSTANT:
    if (m_text == wxT("inf") && parser.CheckTeXFonts())
      dc.SetFont(wxFont(fontsize1TeX, wxMODERN,
                        parser.IsItalic(TS_SPECIAL_CONSTANT),
                        parser.IsBold(TS_SPECIAL_CONSTANT),
                        parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                        parser.GetTeXCMSY()));
#if defined __WXMSW__
    else if (m_text == wxT("inf"))
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_DEFAULT),
                        parser.IsBold(TS_DEFAULT),
                        parser.IsUnderlined(TS_DEFAULT),
                        parser.GetSymbolFontName()));
#endif
    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_SPECIAL_CONSTANT),
                        parser.IsBold(TS_SPECIAL_CONSTANT),
                        parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                        parser.GetFontName(m_textStyle),
                        parser.GetFontEncoding()));
    break;

  case TS_GREEK_CONSTANT:
    if (parser.CheckTeXFonts())
      dc.SetFont(wxFont(fontsize1TeX,
                        wxMODERN,
                        false, false, false,
                        parser.GetTeXCMMI()));
#if defined __WXMSW__
    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                              parser.IsItalic(TS_SPECIAL_CONSTANT),
                              parser.IsBold(TS_SPECIAL_CONSTANT),
                              parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                              parser.GetSymbolFontName()));
#else
    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_SPECIAL_CONSTANT),
                        parser.IsBold(TS_SPECIAL_CONSTANT),
                        parser.IsUnderlined(TS_SPECIAL_CONSTANT),
                        parser.GetFontName(),
                        parser.GetFontEncoding()));
#endif
    break;

  case TS_SECTION:
  case TS_SUBSECTION:
  case TS_TITLE:
    dc.SetFont(wxFont(fontsize1, wxMODERN,
                      parser.IsItalic(m_textStyle),
                      parser.IsBold(m_textStyle),
                      false,
                      parser.GetFontName(m_textStyle),
                      parser.GetFontEncoding()));
    break;

  default:
    if (parser.CheckTeXFonts() &&
        (m_text == wxT("->") ||
         m_text == wxT(">=") || m_text == wxT("<=")))
      dc.SetFont(wxFont(fontsize1TeX, wxMODERN,
                 parser.IsItalic(TS_DEFAULT),
                 parser.IsBold(TS_DEFAULT),
                 parser.IsUnderlined(TS_DEFAULT),
                 parser.GetTeXCMSY()));

#if defined __WXMSW__
    else if (m_text == wxT("->") ||
        m_text == wxT(">=") || m_text == wxT("<="))
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(TS_DEFAULT),
                        parser.IsBold(TS_DEFAULT),
                        parser.IsUnderlined(TS_DEFAULT),
                        parser.GetSymbolFontName()));
#endif

    else if (parser.CheckTeXFonts() &&
        (m_text == wxT("-") || m_text == wxT("+") || m_text == wxT("=")))
      dc.SetFont(wxFont(fontsize1TeX, wxMODERN,
                 parser.IsItalic(TS_DEFAULT),
                 parser.IsBold(TS_DEFAULT),
                 parser.IsUnderlined(TS_DEFAULT),
                 parser.GetTeXCMRI()));

    else
      dc.SetFont(wxFont(fontsize1, wxMODERN,
                        parser.IsItalic(m_textStyle),
                        parser.IsBold(m_textStyle),
                        false,
                        parser.GetFontName(m_textStyle),
                        parser.GetFontEncoding()));
  }
}

bool TextCell::IsOperator()
{
  if (wxString(wxT("+*/-")).Find(m_text) >= 0)
    return true;
  return false;
}

wxString TextCell::ToString(bool all)
{
  wxString text = m_text;
  return text + MathCell::ToString(all);
}

wxString TextCell::ToTeX(bool all)
{
  wxString text;
  if (m_isHidden)
    text = wxT("\\,");
  else if (m_textStyle == TS_GREEK_CONSTANT)
  {
    if (m_text[0] != '%')
      text = wxT("\\") + m_text;
    else
    {
      text = m_text;
      text.Replace(wxT("%"), wxT("\\"));
    }
  }
  else if (m_textStyle == TS_SPECIAL_CONSTANT)
  {
    if (m_text == wxT("inf"))
      text = wxT("\\infty ");
    else if (m_text == wxT("%e"))
      text = wxT("e");
    else if (m_text == wxT("%i"))
      text = wxT("i");
    else if (m_text == wxT("%pi"))
      text = wxT("\\pi ");
    else
      text = m_text;
  }
  else if (m_type == MC_TYPE_LABEL)
  {
    text = wxT("\\leqno{\\tt ") + m_text + wxT("}");
    text.Replace(wxT("%"), wxT("\\%"));
  }
  else
  {
    text = m_text;
    text.Replace(wxT("\\"), wxT("\\\\"));
    text.Replace(wxT("^"), wxT("\\^"));
    text.Replace(wxT("_"), wxT("\\_"));
    text.Replace(wxT("%"), wxT("\\%"));
  }

  return text + MathCell::ToTeX(all);
}

wxString TextCell::ToXML(bool all)
{
	wxString tag = ( m_isHidden )? _T("h") :
					( m_textStyle == TS_GREEK_CONSTANT )? _T("g") :
					( m_textStyle == TS_SPECIAL_CONSTANT )? _T("s") :
					( m_textStyle == TS_VARIABLE )? _T("v") :
					( m_textStyle == TS_FUNCTION )? _T("fnm") :
					( m_textStyle == TS_NUMBER )? _T("n") :
					( m_textStyle == TS_STRING )? _T("st") :
					( m_textStyle == TS_LABEL)? _T("lbl") : _T("t");
  wxString xmlstring = m_text;
  // convert it, so that the XML parser doesn't fail
  xmlstring.Replace(wxT("&"),  wxT("&amp;"));
  xmlstring.Replace(wxT("<"),  wxT("&lt;"));
  xmlstring.Replace(wxT(">"),  wxT("&gt;"));
  xmlstring.Replace(wxT("'"),  wxT("&apos;"));
  xmlstring.Replace(wxT("\""), wxT("&quot;"));

	return _T("<") + tag + _T(">") + xmlstring + _T("</") + tag + _T(">") +
				MathCell::ToXML(all);
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

wxString TextCell::GetSymbolString(CellParser& parser)
{
  if (parser.CheckTeXFonts())
  {
    if (m_text == wxT("inf"))
      return wxT("\x31");
    else if (m_text == wxT("->"))
      return wxT("\x21");
    else if (m_text == wxT(">="))
      return wxT("\xD5");
    else if (m_text == wxT("<="))
      return wxT("\xD4");
    else
      return m_text;
  }
#if defined __WXMSW__
  else if (m_text == wxT("inf"))
    return wxT("\xA5");
  else if (m_text == wxT("->"))
    return wxT("\xAE");
  else if (m_text == wxT(">="))
    return wxT("\xB3");
  else if (m_text == wxT("<="))
    return wxT("\xA3");
#endif
  return m_text;
}

wxString TextCell::GetGreekString(CellParser& parser)
{
  if (parser.CheckTeXFonts())
    return GetGreekStringUnicode();
  return GetGreekStringSymbol();
}

wxString TextCell::GetGreekStringUnicode()
{
  if (m_text == wxT("gamma"))
    return wxT("\xC0");
  else if (m_text == wxT("zeta"))
    return wxT("\xB0");
  else if (m_text == wxT("psi"))
    return wxT("\xC9");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxT("\xCB");
  else if (txt == wxT("%beta"))
    return wxT("\xCC");
  else if (txt == wxT("%gamma"))
    return wxT("\xCD");
  else if (txt == wxT("%delta"))
    return wxT("\xCE");
  else if (txt == wxT("%epsilon"))
    return wxT("\xCF");
  else if (txt == wxT("%zeta"))
    return wxT("\xB0");
  else if (txt == wxT("%eta"))
    return wxT("\xD1");
  else if (txt == wxT("%theta"))
    return wxT("\xD2");
  else if (txt == wxT("%iota"))
    return wxT("\xD3");
  else if (txt == wxT("%kappa"))
    return wxT("\xD4");
  else if (txt == wxT("%lambda"))
    return wxT("\xD5");
  else if (txt == wxT("%mu"))
    return wxT("\xD6");
  else if (txt == wxT("%nu"))
    return wxT("\xB7");
  else if (txt == wxT("%xi"))
    return wxT("\xD8");
  else if (txt == wxT("%omicron"))
    return wxT("o");
  else if (txt == wxT("%pi"))
    return wxT("\xD9");
  else if (txt == wxT("%rho"))
    return wxT("\xDA");
  else if (txt == wxT("%sigma"))
    return wxT("\xDB");
  else if (txt == wxT("%tau"))
    return wxT("\xDC");
  else if (txt == wxT("%upsilon"))
    return wxT("\xB5");
  else if (txt == wxT("%chi"))
    return wxT("\xDF");
  else if (txt == wxT("%psi"))
    return wxT("\xEF");
  else if (txt == wxT("%phi"))
    return wxT("\x27");
  else if (txt == wxT("%omega"))
    return wxT("\x21");
  else if (txt == wxT("%Alpha"))
    return wxT("A");
  else if (txt == wxT("%Beta"))
    return wxT("B");
  else if (txt == wxT("%Gamma"))
    return wxT("\xC0");
  else if (txt == wxT("%Delta"))
    return wxT("\xC1");
  else if (txt == wxT("%Epsilon"))
    return wxT("E");
  else if (txt == wxT("%Zeta"))
    return wxT("Z");
  else if (txt == wxT("%Eta"))
    return wxT("H");
  else if (txt == wxT("%Theta"))
    return wxT("\xC2");
  else if (txt == wxT("%Iota"))
    return wxT("I");
  else if (txt == wxT("%Kappa"))
    return wxT("K");
  else if (txt == wxT("%Lambda"))
    return wxT("\xC3");
  else if (txt == wxT("%Mu"))
    return wxT("M");
  else if (txt == wxT("%Nu"))
    return wxT("N");
  else if (txt == wxT("%Xi"))
    return wxT("\xC4");
  else if (txt == wxT("%Omicron"))
    return wxT("O");
  else if (txt == wxT("%Pi"))
    return wxT("\xC5");
  else if (txt == wxT("%Rho"))
    return wxT("P");
  else if (txt == wxT("%Sigma"))
    return wxT("\xC6");
  else if (txt == wxT("%Tau"))
    return wxT("T");
  else if (txt == wxT("%Upsilon"))
    return wxT("Y");
  else if (txt == wxT("%Phi"))
    return wxT("\xC8");
  else if (txt == wxT("%Chi"))
    return wxT("X");
  else if (txt == wxT("%Psi"))
    return wxT("\xC9");
  else if (txt == wxT("%Omega"))
    return wxT("\xCA");

  return wxEmptyString;
}

wxString TextCell::GetGreekStringSymbol()
{
  if (m_text == wxT("gamma"))
    return wxT("\x47");
  else if (m_text == wxT("zeta"))
    return wxT("\x7A");
  else if (m_text == wxT("psi"))
    return wxT("\x59");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxT("\x61");
  else if (txt == wxT("%beta"))
    return wxT("\x62");
  else if (txt == wxT("%gamma"))
    return wxT("\x67");
  else if (txt == wxT("%delta"))
    return wxT("\x64");
  else if (txt == wxT("%epsilon"))
    return wxT("\x65");
  else if (txt == wxT("%zeta"))
    return wxT("\x7A");
  else if (txt == wxT("%eta"))
    return wxT("\x68");
  else if (txt == wxT("%theta"))
    return wxT("\x71");
  else if (txt == wxT("%iota"))
    return wxT("\x69");
  else if (txt == wxT("%kappa"))
    return wxT("\x6B");
  else if (txt == wxT("%lambda"))
    return wxT("\x6C");
  else if (txt == wxT("%mu"))
    return wxT("\x6D");
  else if (txt == wxT("%nu"))
    return wxT("\x6E");
  else if (txt == wxT("%xi"))
    return wxT("\x78");
  else if (txt == wxT("%omicron"))
    return wxT("\x6F");
  else if (txt == wxT("%pi"))
    return wxT("\x70");
  else if (txt == wxT("%rho"))
    return wxT("\x72");
  else if (txt == wxT("%sigma"))
    return wxT("\x73");
  else if (txt == wxT("%tau"))
    return wxT("\x74");
  else if (txt == wxT("%upsilon"))
    return wxT("\x75");
  else if (txt == wxT("%phi"))
    return wxT("\x66");
  else if (txt == wxT("%chi"))
    return wxT("\x63");
  else if (txt == wxT("%psi"))
    return wxT("\x79");
  else if (txt == wxT("%omega"))
    return wxT("\x77");
  else if (txt == wxT("%Alpha"))
    return wxT("\x41");
  else if (txt == wxT("%Beta"))
    return wxT("\x42");
  else if (txt == wxT("%Gamma"))
    return wxT("\x47");
  else if (txt == wxT("%Delta"))
    return wxT("\x44");
  else if (txt == wxT("%Epsilon"))
    return wxT("\x45");
  else if (txt == wxT("%Zeta"))
    return wxT("\x5A");
  else if (txt == wxT("%Eta"))
    return wxT("\x48");
  else if (txt == wxT("%Theta"))
    return wxT("\x51");
  else if (txt == wxT("%Iota"))
    return wxT("\x49");
  else if (txt == wxT("%Kappa"))
    return wxT("\x4B");
  else if (txt == wxT("%Lambda"))
    return wxT("\x4C");
  else if (txt == wxT("%Mu"))
    return wxT("\x4D");
  else if (txt == wxT("%Nu"))
    return wxT("\x4E");
  else if (txt == wxT("%Xi"))
    return wxT("\x58");
  else if (txt == wxT("%Omicron"))
    return wxT("\x4F");
  else if (txt == wxT("%Pi"))
    return wxT("\x50");
  else if (txt == wxT("%Rho"))
    return wxT("\x52");
  else if (txt == wxT("%Sigma"))
    return wxT("\x53");
  else if (txt == wxT("%Tau"))
    return wxT("\x54");
  else if (txt == wxT("%Upsilon"))
    return wxT("\x55");
  else if (txt == wxT("%Phi"))
    return wxT("\x46");
  else if (txt == wxT("%Chi"))
    return wxT("\x43");
  else if (txt == wxT("%Psi"))
    return wxT("\x59");
  else if (txt == wxT("%Omega"))
    return wxT("\x57");

  return m_text;
}
