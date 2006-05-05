///
///  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _CELLPARSER_H
#define _CELLPARSER_H

#include <wx/wx.h>
#include "TextStyle.h"

class CellParser
{
public:
  CellParser(wxDC& dc);
  CellParser(wxDC& dc, double scale);
  ~CellParser();
  void SetScale(double scale)
  {
    m_scale = scale;
  }
  double GetScale()
  {
    return m_scale;
  }
  wxDC& GetDC()
  {
    return m_dc;
  }
  void SetBouns(int top, int bottom)
  {
    m_top = top;
    m_bottom = bottom;
  }
  int GetTop()
  {
    return m_top;
  }
  int GetBottom()
  {
    return m_bottom;
  }
  wxString GetFontName()
  {
    return m_fontName;
  }
  wxString GetSymbolFontName();
  wxString GetGreekFontName()
  {
    return m_symbolFontName;
  }
  wxFontEncoding GetGreekFontEncoding();
  int GetGreekFontAdj()
  {
    return m_symbolFontAdj;
  }
  bool HaveSymbolFont()
  {
    return m_haveSymbolFont;
  }
  wxString GetColor(int st)
  {
    return m_styles[st].color;
  }
  int IsBold(int st);
  int IsItalic(int st);
  int IsUnderlined(int st);
  void ReadStyle();
  void SetForceUpdate(bool force)
  {
    m_forceUpdate = force;
  }
  bool ForceUpdate()
  {
    return m_forceUpdate;
  }
  wxFontEncoding GetFontEncoding()
  {
    return m_fontEncoding;
  }
private:
  double m_scale;
  wxDC& m_dc;
  int m_top, m_bottom;
  wxString m_fontName;
  wxString m_symbolFontName;
  int m_symbolFontAdj;
  bool m_haveSymbolFont;
  bool m_forceUpdate;
  wxFontEncoding m_fontEncoding;
  style m_styles[12];
};

#endif
