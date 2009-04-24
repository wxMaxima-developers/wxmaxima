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

#ifndef _CELLPARSER_H
#define _CELLPARSER_H

#include <wx/wx.h>
#include "TextStyle.h"

#include "Setup.h"

class CellParser
{
public:
  CellParser(wxDC& dc);
  CellParser(wxDC& dc, double scale);
  ~CellParser();
  void SetZoomFactor(double newzoom) { m_zoomFactor = newzoom; }
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
  wxString GetFontName(int type = TS_DEFAULT)
  {
    if (type == TS_TITLE || type == TS_SUBSECTION || type == TS_SECTION || type == TS_TEXT)
      return m_styles[type].font;
    return m_fontName;
  }
  wxString GetSymbolFontName();
  wxString GetGreekFontName()
  {
    return m_greekFontName;
  }
  wxFontEncoding GetGreekFontEncoding();
  int GetGreekFontAdj()
  {
    return m_greekFontAdj;
  }
  bool HaveGreekFont()
  {
    return m_haveGreekFont;
  }
  wxColour GetColor(int st);
  wxFontWeight IsBold(int st);
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
  bool GetChangeAsterisk()
  {
    return m_changeAsterisk;
  }
  void SetChangeAsterisk(bool changeAsterisk)
  {
    m_changeAsterisk = changeAsterisk;
  }
  int GetIndent() { return m_indent; }
  void SetIndent(int indent) { m_indent = indent; }
  void SetClientWidth(int width) { m_clientWidth = width; }
  int GetClientWidth() { return m_clientWidth; }
  int GetDefaultFontSize() { return int(m_zoomFactor * double(m_defaultFontSize)); }
  int GetFontSize(int st)
  {
    if (st == TS_TEXT || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return int(m_zoomFactor * double(m_styles[st].fontSize));
    return 0;
  }
  void Outdated(bool outdated) { m_outdated = outdated; }
private:
  int m_indent;
  double m_scale;
  double m_zoomFactor;
  wxDC& m_dc;
  int m_top, m_bottom;
  wxString m_fontName;
  int m_defaultFontSize;
  wxString m_greekFontName;
#if (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  wxString m_unicodeSymbolsFont;
#endif
  int m_greekFontAdj;
  bool m_haveGreekFont;
  bool m_forceUpdate;
  bool m_changeAsterisk;
  bool m_outdated;
  int m_clientWidth;
  wxFontEncoding m_fontEncoding;
  style m_styles[STYLE_NUM];
};

#endif
