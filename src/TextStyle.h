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
//  SPDX-License-Identifier: GPL-2.0+

/*!\file 
This file declares everything needed in order to style all the elements
shown on the work sheet.
*/


#ifndef TEXTSTYLE_H
#define TEXTSTYLE_H

#include <wx/config.h>
#include <wx/colour.h>
#include <wx/font.h>

//! A text style for the work sheet
class Style
{
public:
  Style() : m_bold(false), m_italic(false), m_underlined(false), m_fontSize(10)
  {
  };
  void Read(wxConfigBase *config, wxString where)
    {
      wxString tmp;
      if (config->Read(where + wxT("color"), &tmp))
        Color(wxColor((tmp)));
      config->Read(where + wxT("bold"), &m_bold);
      config->Read(where + wxT("italic"), &m_italic);
      config->Read(where + wxT("underlined"), &m_underlined);
      int size = 12;
      config->Read(wxT("Style/Text/fontsize"),
                   &m_fontSize);
      config->Read(wxT("Style/Text/fontname"),
                   &m_fontName);
#ifdef __WXOSX_MAC__
      if(m_fontName = wxEmptyString) m_fontName = "Monaco";
#endif
      wxFont font;
      font.SetFamily(wxFONTFAMILY_MODERN);
      font.SetFaceName(m_fontName);
      if (!font.IsOk())
      {
        font = wxFontInfo(10);
        m_fontName = font.GetFaceName();
      }
    }
  void Write(wxConfigBase *config, wxString where)
    {
      config->Write(where + wxT("color"), Color().GetAsString());
      config->Write(where + wxT("bold"), m_bold);
      config->Write(where + wxT("italic"), m_italic);
      config->Write(where + wxT("underlined"), m_underlined);
      config->Write(wxT("Style/Text/fontsize"),
               m_fontSize);
      config->Write(wxT("Style/Text/fontname"),
               m_fontName);
    }
  void Set(wxColor color,
           bool bold = false, bool italic = false, bool underlined = false,
           int fontSize=10)
    {
      m_color = color;
      m_bold = bold;
      m_italic = italic;
      m_underlined = underlined;
      m_fontSize = fontSize;
    }
  bool Italic(){return m_italic;}
  void Italic(bool italic){m_italic = italic;}
  bool Bold(){return m_bold;}
  void Bold(bool bold){m_bold = bold;}
  bool Underlined(){return m_underlined;}
  void Underlined(bool underlined){m_underlined = underlined;}
  int FontSize(){return m_fontSize;}
  void FontSize(int size){m_fontSize = size;}
  wxString FontName(){return m_fontName;}
  void FontName(wxString name){m_fontName = name;}
  wxColor GetColor(){return m_color;}
  void Color(wxColor color){m_color = color;}
  void Color(int r, int g, int b){m_color = wxColor(r,g,b);}
  wxColor Color(){return m_color;}
private:
  wxColor m_color;
  wxString m_fontName;
  int m_fontSize;
  bool m_bold;
  bool m_italic;
  bool m_underlined;
};

/*! All text styles known to wxMaxima

  \attention If an additional style is added here STYLE_NUM has to be incremented 
             accordingly. 
 */
enum TextStyle
{
  TS_DEFAULT = 0,
  TS_VARIABLE,
  TS_FUNCTION,
  TS_NUMBER,
  TS_GREEK_CONSTANT,
  TS_SPECIAL_CONSTANT,
  TS_STRING,
  TS_MAIN_PROMPT,
  TS_OTHER_PROMPT,
  TS_LABEL,
  TS_USERLABEL,
  TS_INPUT,
  TS_HIGHLIGHT,
  TS_TEXT_BACKGROUND,
  TS_TEXT,
  TS_HEADING6,
  TS_HEADING5,
  TS_SUBSUBSECTION,
  TS_SUBSECTION,
  TS_SECTION,
  TS_TITLE,
  TS_WARNING,
  TS_ERROR,
  TS_CELL_BRACKET,
  TS_ACTIVE_CELL_BRACKET,
  TS_CURSOR,
  TS_SELECTION,
  TS_EQUALSSELECTION,
  TS_OUTDATED,
  TS_CODE_COMMENT,
  TS_CODE_VARIABLE,
  TS_CODE_FUNCTION,
  TS_CODE_NUMBER,
  TS_CODE_STRING,
  TS_CODE_OPERATOR,
  TS_CODE_ENDOFLINE
};

//! The number of entries TextStyle is long
#define STYLE_NUM (TS_CODE_ENDOFLINE-TS_DEFAULT+1)

#endif // TEXTSTYLE_H
