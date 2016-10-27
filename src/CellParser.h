// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef CELLPARSER_H
#define CELLPARSER_H

#include <wx/wx.h>
#include <wx/fontenum.h>

#include "TextStyle.h"

#include "Setup.h"

/*! The configuration storage for the current worksheet.

  Caching the information here means we don't need to search for the configuration
  value's name every time we need the information: Reading configuration values from
  the system's configuration storage can be quite time consuming, especially on a 
  MSW with a long registry.

  In order to make all cells temporary listen to a different set of configuration 
  than the default one all that has to be done is to create a new configuration 
  object that contains hold the right settings for printing/export as bitmap or 
  similar: CellParser::Get() will always return the last CellParser that was 
  created and therefore as long as the new configuration object exist will return
  a pointer to this object if configuration is needed.
 */
class CellParser
{
public:
  void ReadConfig();
  CellParser(wxDC& dc);
  //! Set the drawing context that is currently active
  void SetContext(wxDC &dc){m_dc = &dc;}
  CellParser(wxDC& dc, double scale);
  ~CellParser();
  void SetZoomFactor(double newzoom) { m_zoomFactor = newzoom; }
  void SetScale(double scale) { m_scale = scale; }
  const double GetScale() { return m_scale; }
  //! Get a drawing context suitable for size calculations
  wxDC& GetDC() { return *m_dc; }
  void SetBounds(int top, int bottom) {
    m_top = top;
    m_bottom = bottom;
  }
  const int GetTop()
  {
    return m_top;
  }
  const int GetBottom()
  {
    return m_bottom;
  }
  wxString GetFontName(int type = TS_DEFAULT);
  wxString GetSymbolFontName();
  wxColour GetColor(int st);
  wxFontWeight IsBold(int st);
  wxFontStyle IsItalic(int st);
  bool IsUnderlined(int st);
  void ReadStyle();
  void SetForceUpdate(bool force)
  {
    m_forceUpdate = force;
  }
  const bool ForceUpdate()
  {
    return m_forceUpdate;
  }
  const wxFontEncoding GetFontEncoding()
  {
    return m_fontEncoding;
  }
  const bool GetChangeAsterisk()
  {
    return m_changeAsterisk;
  }
  void SetChangeAsterisk(bool changeAsterisk)
  {
    m_changeAsterisk = changeAsterisk;
  }
  const int GetLabelWidth(){return m_labelWidth;}
  const int GetIndent() { return m_indent; }
  void SetIndent(int indent) { m_indent = indent; }
  void SetClientWidth(int width) { m_clientWidth = width; }
  //! Returns the width of the visible portion of the worksheet
  const int GetClientWidth() { return m_clientWidth; }
  //! Returns the maximum sensible width for a text line: On big 16:9 screens
  //  text tends to get \b very wide before it hits the right margin. But text
  //  blocks that are 1 meter wide and 2 cm high feel - weird.
  const int GetLineWidth() {
    if(m_clientWidth<=m_zoomFactor * double(m_defaultFontSize)*88.0*m_zoomFactor)
      return m_clientWidth;
    else
      return double(m_defaultFontSize)*88.0*m_zoomFactor;
  }
  const int GetDefaultFontSize() { return int(m_zoomFactor * double(m_defaultFontSize)); }
  const int GetMathFontSize() { return int(m_zoomFactor * double(m_mathFontSize)); }
  const bool GetAutoWrap() { return m_autoWrap;}
  const int GetFontSize(int st)
  {
    if (st == TS_TEXT || st == TS_SUBSUBSECTION || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return int(m_zoomFactor * double(m_styles[st].fontSize));
    return 0;
  }
  void Outdated(bool outdated) { m_outdated = outdated; }
  const bool CheckTeXFonts() { return m_TeXFonts; }
  const bool CheckKeepPercent() { return m_keepPercent; }
  const wxString GetTeXCMRI() { return m_fontCMRI; }
  const wxString GetTeXCMSY() { return m_fontCMSY; }
  const wxString GetTeXCMEX() { return m_fontCMEX; }
  const wxString GetTeXCMMI() { return m_fontCMMI; }
  const wxString GetTeXCMTI() { return m_fontCMTI; }
  void SetPrinter(bool printer) { m_printer = printer; }
  const bool GetPrinter() { return m_printer; }
  const bool GetMatchParens() { return m_matchParens; }
  const bool GetInsertAns() { return m_insertAns; }

  //! Returns a pointer to the instance of CellParser that exists
  static CellParser *Get() {return m_cellParser;}
private:
  //! Automatically wrap long lines?
  bool m_autoWrap;
  //! Do we want to automatically close parenthesis?
  bool m_matchParens;
  //! Do we want to automatically insert new cells conaining a "%" at the end of every command?
  bool m_insertAns;
  //! The CellParser that was active before this one
  CellParser *m_last;
  //! The width of input and output labels [in chars]
  int m_labelWidth;
  int m_indent;
  double m_scale;
  double m_zoomFactor;
  wxDC *m_dc;
  int m_top, m_bottom;
  wxString m_fontName;
  int m_defaultFontSize, m_mathFontSize;
  wxString m_mathFontName;
  bool m_forceUpdate;
  bool m_changeAsterisk;
  bool m_outdated;
  bool m_TeXFonts;
  bool m_keepPercent;
  wxString m_fontCMRI, m_fontCMSY, m_fontCMEX, m_fontCMMI, m_fontCMTI;
  int m_clientWidth;
  wxFontEncoding m_fontEncoding;
  style m_styles[STYLE_NUM];
  bool m_printer;
  static CellParser *m_cellParser;
};

#endif // CELLPARSER_H
