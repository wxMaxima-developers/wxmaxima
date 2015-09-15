// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef TEXTCELL_H
#define TEXTCELL_H

#include "MathCell.h"

class TextCell : public MathCell
{
public:
  TextCell();
  TextCell(wxString text);
  ~TextCell();
  MathCell* Copy();
  void Destroy();
  void SetValue(wxString text);
  void RecalculateWidths(CellParser& parser, int fontsize);
  void Draw(CellParser& parser, wxPoint point, int fontsize);
  void SetFont(CellParser& parser, int fontsize);
  wxString ToString();
  wxString ToTeX();
  wxString ToXML();
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
private:
  //! Produces a text sample that determines the label width
  wxString LabelWidthText();

};

#endif // TEXTCELL_H
