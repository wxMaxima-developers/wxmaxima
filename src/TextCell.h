///
///  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef TEXTCELL_H
#define TEXTCELL_H

#include "MathCell.h"

class TextCell : public MathCell
{
public:
  TextCell();
  TextCell(wxString text);
  ~TextCell();
  MathCell* Copy(bool all);
  void Destroy();
  void SetValue(wxString text);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  void SetFont(CellParser& parser, int fontsize);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
	wxString ToXML(bool all);	// new!
  wxString GetDiffPart();
  bool IsOperator();
  wxString GetValue() { return m_text; }
  wxString GetGreekStringTeX();
  wxString GetSymbolTeX();
#if wxUSE_UNICODE
  wxString GetGreekStringUnicode();
  wxString GetSymbolUnicode(bool keepPercent);
#elif defined __WXMSW__
  wxString GetGreekStringSymbol();
  wxString GetSymbolSymbol(bool keepPercent);
#endif
  bool IsShortNum();
protected:
  void SetAltText(CellParser& parser);
  wxString m_text;
  wxString m_altText, m_altJsText;
  wxString m_fontname, m_texFontname;
  bool m_alt, m_altJs;
  int m_realCenter;
  int m_fontSize;
  int m_fontSizeTeX;
  int m_fontSizeLabel;
  int m_labelWidth, m_labelHeight;
};

#endif // TEXTCELL_H
