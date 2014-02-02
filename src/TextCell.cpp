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

#include "TextCell.h"
#include "Setup.h"

TextCell::TextCell() : MathCell()
{
  m_text = wxEmptyString;
  m_fontSize = -1;
  m_highlight = false;
  m_altJs = m_alt = false;
}

TextCell::TextCell(wxString text) : MathCell()
{
  m_text = text;
  m_text.Replace(wxT("\n"), wxEmptyString);
  m_highlight = false;
  m_altJs = m_alt = false;
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
  m_alt = m_altJs = false;
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
  SetAltText(parser);

  if (m_height == -1 || m_width == -1 || fontsize != m_fontSize || parser.ForceUpdate())
  {
    m_fontSize = fontsize;

    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    SetFont(parser, fontsize);

    /// Labels and prompts are fixed width - adjust font size so that
    /// they fit in
    if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_MAIN_PROMPT)) {
	  // Check for output annotations (/R/ for CRE and /T/ for Taylor expressions)
      if (m_text.Right(2) != wxT("/ "))
        dc.GetTextExtent(wxT("(\%oXXX)"), &m_width, &m_height);
      else
        dc.GetTextExtent(wxT("(\%oXXX)/R/"), &m_width, &m_height);
      m_fontSizeLabel = m_fontSize;
      dc.GetTextExtent(m_text, &m_labelWidth, &m_labelHeight);
      while (m_labelWidth >= m_width) {
        int fontsize1 = (int) (((double) --m_fontSizeLabel) * scale + 0.5);
        dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
              parser.IsItalic(m_textStyle),
              parser.IsBold(m_textStyle),
              false, //parser.IsUnderlined(m_textStyle),
              parser.GetFontName(m_textStyle),
              parser.GetFontEncoding()));
        dc.GetTextExtent(m_text, &m_labelWidth, &m_labelHeight);
      }
    }

    /// Check if we are using jsMath and have jsMath character
    else if (m_altJs && parser.CheckTeXFonts())
    {
      dc.GetTextExtent(m_altJsText, &m_width, &m_height);

      if (m_texFontname == wxT("jsMath-cmsy10"))
        m_height = m_height / 2;
    }

    /// We are using a special symbol
    else if (m_alt)
    {
      dc.GetTextExtent(m_altText, &m_width, &m_height);
    }

    /// Empty string has height of X
    else if (m_text == wxEmptyString)
    {
      dc.GetTextExtent(wxT("X"), &m_width, &m_height);
      m_width = 0;
    }

    /// This is the default.
    else
      dc.GetTextExtent(m_text, &m_width, &m_height);

    m_width = m_width + 2 * SCALE_PX(MC_TEXT_PADDING, scale);
    m_height = m_height + 2 * SCALE_PX(MC_TEXT_PADDING, scale);

    /// Hidden cells (multiplication * is not displayed)
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

    /// Labels and prompts have special fontsize
    if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_MAIN_PROMPT))
    {
      SetFont(parser, m_fontSizeLabel);
      dc.DrawText(m_text,
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale) + (m_width - m_labelWidth),
                  point.y - m_realCenter + (m_height - m_labelHeight)/2);
    }

    /// Check if we are using jsMath and have jsMath character
    else if (m_altJs && parser.CheckTeXFonts())
      dc.DrawText(m_altJsText,
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

    /// We are using a special symbol
    else if (m_alt)
      dc.DrawText(m_altText,
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

    /// Change asterisk
    else if (parser.GetChangeAsterisk() &&  m_text == wxT("*"))
      dc.DrawText(wxT("\xB7"),
                  point.x + SCALE_PX(MC_TEXT_PADDING, scale),
                  point.y - m_realCenter + SCALE_PX(MC_TEXT_PADDING, scale));

    /// This is the default.
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

  if ((m_textStyle == TS_TITLE) ||
      (m_textStyle == TS_SECTION) ||
      (m_textStyle == TS_SUBSECTION)) {
    fontsize1 = parser.GetFontSize(m_textStyle);
    fontsize1 = (int) (((double)fontsize1) * scale + 0.5);
  }

  fontsize1 = MAX(fontsize1, 1);

  // Use jsMath
  if (m_altJs && parser.CheckTeXFonts())
  {
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      wxFONTSTYLE_NORMAL,
                      parser.IsBold(m_textStyle),
                      parser.IsUnderlined(m_textStyle),
                      m_texFontname));
  }

  // We have an alternative symbol
  else if (m_alt)
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      wxFONTSTYLE_NORMAL,
                      parser.IsBold(m_textStyle),
                      false,
                      m_fontname != wxEmptyString ?
                          m_fontname : parser.GetFontName(m_textStyle),
                      parser.GetFontEncoding()));

  // Titles, sections, subsections - don't underline
  else if ((m_textStyle == TS_TITLE) ||
           (m_textStyle == TS_SECTION) ||
           (m_textStyle == TS_SUBSECTION))
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      parser.IsItalic(m_textStyle),
                      parser.IsBold(m_textStyle),
                      false,
                      parser.GetFontName(m_textStyle),
                      parser.GetFontEncoding()));

  // Default
  else
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                      parser.IsItalic(m_textStyle),
                      parser.IsBold(m_textStyle),
                      parser.IsUnderlined(m_textStyle),
                      parser.GetFontName(m_textStyle),
                      parser.GetFontEncoding()));
}

bool TextCell::IsOperator()
{
  if (wxString(wxT("+*/-")).Find(m_text) >= 0)
    return true;
  return false;
}

wxString TextCell::ToString(bool all)
{
  wxString text;
  if (m_altCopyText != wxEmptyString)
    text = m_altCopyText;
  else
    text = m_text;
#if wxUSE_UNICODE
    text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
#endif
  if (m_textStyle == TS_STRING)
    text = wxT("\"") + text + wxT("\"");
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
      text = wxT("%") + m_text;
    else
      text = m_text;

    if (text == wxT("%Alpha"))
      text = wxT("A");
    else if (text == wxT("%Beta"))
      text = wxT("B");
    else if (text == wxT("%Epsilon"))
      text = wxT("E");
    else if (text == wxT("%Zeta"))
      text = wxT("Z");
    else if (text == wxT("%Eta"))
      text = wxT("H");
    else if (text == wxT("%Iota"))
      text = wxT("I");
    else if (text == wxT("%Kappa"))
      text = wxT("K");
    else if (text == wxT("%Mu"))
      text = wxT("M");
    else if (text == wxT("%Nu"))
      text = wxT("N");
    else if (text == wxT("%Omicron"))
      text = wxT("O");
    else if (text == wxT("%Rho"))
      text = wxT("P");
    else if (text == wxT("%Tau"))
      text = wxT("T");
    else if (text == wxT("%Chi"))
      text = wxT("X");
    else
      text = wxT("\\") + text.Mid(1);
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
    if (m_textStyle == TS_FUNCTION)
      text = wxT("\\mathrm{") + m_text + wxT("}");
    else
      text = m_text;
    text.Replace(wxT("^"), wxT("\\^"));
    text.Replace(wxT("_"), wxT("\\_"));
    text.Replace(wxT("%"), wxT("\\%"));
#if wxUSE_UNICODE
    text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
#endif
  }

  return text + MathCell::ToTeX(all);
}

wxString TextCell::ToXML(bool all)
{
  wxString tag = ( m_isHidden ) ? _T("h") :
    ( m_textStyle == TS_GREEK_CONSTANT ) ? _T("g") :
    ( m_textStyle == TS_SPECIAL_CONSTANT ) ? _T("s") :
    ( m_textStyle == TS_VARIABLE ) ? _T("v") :
    ( m_textStyle == TS_FUNCTION ) ? _T("fnm") :
    ( m_textStyle == TS_NUMBER ) ? _T("n") :
    ( m_textStyle == TS_STRING ) ? _T("st") :
    ( m_textStyle == TS_LABEL) ? _T("lbl") : _T("t");
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

void TextCell::SetAltText(CellParser& parser)
{
  m_altJs = m_alt = false;
  if (m_textStyle == TS_DEFAULT)
    return ;

  /// Greek characters are defined in jsMath, Windows and Unicode
  if (m_textStyle == TS_GREEK_CONSTANT)
  {
    m_altJs = true;
    m_altJsText = GetGreekStringTeX();
    m_texFontname = wxT("jsMath-cmmi10");

#if wxUSE_UNICODE
    m_alt = true;
    m_altText = GetGreekStringUnicode();
#elif defined __WXMSW__
    m_alt = true;
    m_altText = GetGreekStringSymbol();
    m_fontname = wxT("Symbol");
#endif
  }

  /// Check for other symbols
  else {
    m_altJsText = GetSymbolTeX();
    if (m_altJsText != wxEmptyString)
    {
      if (m_text == wxT("+") || m_text == wxT("="))
        m_texFontname = wxT("jsMath-cmr10");
      else if (m_text == wxT("%pi"))
        m_texFontname = wxT("jsMath-cmmi10");
      else
        m_texFontname = wxT("jsMath-cmsy10");
      m_altJs = true;
    }
#if wxUSE_UNICODE
    m_altText = GetSymbolUnicode(parser.CheckKeepPercent());
    if (m_altText != wxEmptyString)
      m_alt = true;
#elif defined __WXMSW__
    m_altText = GetSymbolSymbol(parser.CheckKeepPercent());
    if (m_altText != wxEmptyString)
    {
      m_alt = true;
      m_fontname = wxT("Symbol");
    }
#endif
  }
}

#if wxUSE_UNICODE

wxString TextCell::GetGreekStringUnicode()
{
  wxString txt(m_text);

  if (txt == wxT("gamma"))
    return wxString(L"\x0393");
  else if (txt == wxT("psi"))
    return wxString(L"\x03A8");

  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxString(L"\x03B1");
  else if (txt == wxT("%beta"))
    return wxString(L"\x03B2");
  else if (txt == wxT("%gamma"))
    return wxString(L"\x03B3");
  else if (txt == wxT("%delta"))
    return wxString(L"\x03B4");
  else if (txt == wxT("%epsilon"))
    return wxString(L"\x03B5");
  else if (txt == wxT("%zeta"))
    return wxString(L"\x03B6");
  else if (txt == wxT("%eta"))
    return wxString(L"\x03B7");
  else if (txt == wxT("%theta"))
    return wxString(L"\x03B8");
  else if (txt == wxT("%iota"))
    return wxString(L"\x03B9");
  else if (txt == wxT("%kappa"))
    return wxString(L"\x03BA");
  else if (txt == wxT("%lambda"))
    return wxString(L"\x03BB");
  else if (txt == wxT("%mu"))
    return wxString(L"\x03BC");
  else if (txt == wxT("%nu"))
    return wxString(L"\x03BD");
  else if (txt == wxT("%xi"))
    return wxString(L"\x03BE");
  else if (txt == wxT("%omicron"))
    return wxString(L"\x03BF");
  else if (txt == wxT("%pi"))
    return wxString(L"\x03C0");
  else if (txt == wxT("%rho"))
    return wxString(L"\x03C1");
  else if (txt == wxT("%sigma"))
    return wxString(L"\x03C3");
  else if (txt == wxT("%tau"))
    return wxString(L"\x03C4");
  else if (txt == wxT("%upsilon"))
    return wxString(L"\x03C5");
  else if (txt == wxT("%phi"))
    return wxString(L"\x03C6");
  else if (txt == wxT("%chi"))
    return wxString(L"\x03C7");
  else if (txt == wxT("%psi"))
    return wxString(L"\x03C8");
  else if (txt == wxT("%omega"))
    return wxString(L"\x03C9");
  else if (txt == wxT("%Alpha"))
    return wxString(L"\x0391");
  else if (txt == wxT("%Beta"))
    return wxString(L"\x0392");
  else if (txt == wxT("%Gamma"))
    return wxString(L"\x0393");
  else if (txt == wxT("%Delta"))
    return wxString(L"\x0394");
  else if (txt == wxT("%Epsilon"))
    return wxString(L"\x0395");
  else if (txt == wxT("%Zeta"))
    return wxString(L"\x0396");
  else if (txt == wxT("%Eta"))
    return wxString(L"\x0397");
  else if (txt == wxT("%Theta"))
    return wxString(L"\x0398");
  else if (txt == wxT("%Iota"))
    return wxString(L"\x0399");
  else if (txt == wxT("%Kappa"))
    return wxString(L"\x039A");
  else if (txt == wxT("%Lambda"))
    return wxString(L"\x039B");
  else if (txt == wxT("%Mu"))
    return wxString(L"\x039C");
  else if (txt == wxT("%Nu"))
    return wxString(L"\x039D");
  else if (txt == wxT("%Xi"))
    return wxString(L"\x039E");
  else if (txt == wxT("%Omicron"))
    return wxString(L"\x039F");
  else if (txt == wxT("%Pi"))
    return wxString(L"\x03A0");
  else if (txt == wxT("%Rho"))
    return wxString(L"\x03A1");
  else if (txt == wxT("%Sigma"))
    return wxString(L"\x03A3");
  else if (txt == wxT("%Tau"))
    return wxString(L"\x03A4");
  else if (txt == wxT("%Upsilon"))
    return wxString(L"\x03A5");
  else if (txt == wxT("%Phi"))
    return wxString(L"\x03A6");
  else if (txt == wxT("%Chi"))
    return wxString(L"\x03A7");
  else if (txt == wxT("%Psi"))
    return wxString(L"\x03A8");
  else if (txt == wxT("%Omega"))
    return wxString(L"\x03A9");

  return wxEmptyString;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent)
{
  if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("inf"))
    return wxString(L"\x221E");
  else if (m_text == wxT("%pi"))
    return wxString(L"\x03C0");
  else if (m_text == wxT("<="))
    return wxString(L"\x2264");
  else if (m_text == wxT(">="))
    return wxString(L"\x2265");
  else if (m_text == wxT(" and "))
    return wxString(L" \x22C0 ");
  else if (m_text == wxT(" or "))
    return wxString(L" \x22C1 ");
  else if (m_text == wxT(" xor "))
    return wxString(L" \x22BB ");
  else if (m_text == wxT(" nand "))
    return wxString(L" \x22BC ");
  else if (m_text == wxT(" nor "))
    return wxString(L" \x22BD ");
  else if (m_text == wxT(" implies "))
    return wxString(L" \x21D2 ");
  else if (m_text == wxT(" equiv "))
    return wxString(L" \x21D4 ");
  else if (m_text == wxT("not"))
    return wxString(L"\x00AC");
  else if (m_text == wxT("->"))
    return wxString(L"\x2192");
 /*
  else if (m_textStyle == TS_SPECIAL_CONSTANT && m_text == wxT("d"))
    return wxString(L"\x2202");
  */

  if (!keepPercent) {
    if (m_text == wxT("%e"))
      return wxString(L"e");
    else if (m_text == wxT("%i"))
      return wxString(L"i");
  }

  return wxEmptyString;
}

#elif defined __WXMSW__

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

  return wxEmptyString;
}

wxString TextCell::GetSymbolSymbol(bool keepPercent)
{
  if (m_text == wxT("inf"))
    return wxT("\xA5");
  else if (m_text == wxT("%pi"))
    return wxT("\x70");
  else if (m_text == wxT("->"))
    return wxT("\xAE");
  else if (m_text == wxT(">="))
    return wxT("\xB3");
  else if (m_text == wxT("<="))
    return wxT("\xA3");
  else if (m_text == wxT(" and "))
    return wxT("\xD9");
  else if (m_text == wxT(" or "))
    return wxT("\xDA");
  else if (m_text == wxT("not"))
    return wxT("\xD8");
  else if (m_text == wxT(" nand "))
    return wxT("\xAD");
  else if (m_text == wxT(" nor "))
    return wxT("\xAF");
  else if (m_text == wxT(" implies "))
    return wxT("\xDE");
  else if (m_text == wxT(" equiv "))
    return wxT("\xDB");
  else if (m_text == wxT(" xor "))
    return wxT("\xC5");

  if (!keepPercent) {
    if (m_text == wxT("%e"))
      return wxString(L"e");
    else if (m_text == wxT("%i"))
      return wxString(L"i");
  }

  return wxEmptyString;
}

#endif

wxString TextCell::GetGreekStringTeX()
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

wxString TextCell::GetSymbolTeX()
{
  if (m_text == wxT("inf"))
    return wxT("\x31");
  else if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("%pi"))
    return wxT("\xD9");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("->"))
    return wxT("\x21");
  else if (m_text == wxT(">="))
    return wxT("\xD5");
  else if (m_text == wxT("<="))
    return wxT("\xD4");
/*
  else if (m_text == wxT(" and "))
    return wxT(" \x5E ");
  else if (m_text == wxT(" or "))
    return wxT(" \x5F ");
  else if (m_text == wxT(" nand "))
    return wxT(" \x22 ");
  else if (m_text == wxT(" nor "))
    return wxT(" \x23 ");
  else if (m_text == wxT(" eq "))
    return wxT(" \x2C ");
  else if (m_text == wxT(" implies "))
    return wxT(" \x29 ");
  else if (m_text == wxT("not"))
    return wxT("\x3A");
  else if (m_text == wxT(" xor "))
    return wxT("\xC8");
*/

  return wxEmptyString;
}
