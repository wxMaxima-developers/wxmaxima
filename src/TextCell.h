///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _TEXTCELL_H_
#define _TEXTCELL_H_

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
	wxString ToXml(bool all);	// new!
  wxString GetDiffPart();
  bool IsOperator();
  wxString GetValue() { return m_text; }
  void SetStyle(int style) { m_textStyle = style; }
  int GetStyle() { return m_textStyle; }
  wxString GetSymbolString(CellParser& parser);
  wxString GetGreekString(CellParser& parser);
#if defined (__WXGTK20__) || defined (__WXMAC__)
  wchar_t* GetGreekStringUnicode();
#else
  wxString GetGreekStringIso();
#endif
  bool IsShortNum();
protected:
  wxString m_text;
  int m_realCenter;
  int m_fontSize;
};

#endif //_TEXTCELL_H_
