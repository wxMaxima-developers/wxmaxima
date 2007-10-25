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

#include "CellParser.h"

#include <wx/font.h>
#include <wx/config.h>

CellParser::CellParser(wxDC& dc) : m_dc(dc)
{
  m_scale = 1.0;
  m_top = -1;
  m_bottom = -1;
  m_forceUpdate = false;

  ReadStyle();
}

CellParser::CellParser(wxDC& dc, double scale) : m_dc(dc)
{
  m_scale = scale;
  m_top = -1;
  m_bottom = -1;
  m_haveGreekFont = false;
  m_greekFontAdj = 0;
  ReadStyle();
}

CellParser::~CellParser()
{}

void CellParser::ReadStyle()
{
  wxConfigBase* config = wxConfig::Get();

  // Font
  config->Read(wxT("Style/fontname"), &m_fontName);

  // Encogind - used only for comments
  m_fontEncoding = wxFONTENCODING_DEFAULT;
  int encoding = m_fontEncoding;
  config->Read(wxT("fontEncoding"), &encoding);
  m_fontEncoding = (wxFontEncoding)encoding;

  // Greek font
  m_haveGreekFont = false;
  m_greekFontAdj = 0;
  m_greekFontName = wxEmptyString;
  config->Read(wxT("Style/GreekFont/fontname"), &m_greekFontName);
  config->Read(wxT("Style/GreekFont/adj"), &m_greekFontAdj);
  config->Read(wxT("Style/GreekFont/ok"), &m_haveGreekFont);

#if wxUSE_UNICODE
  m_unicodeSymbolsFont = wxT("Sans");
  config->Read(wxT("Style/Unicode/fontname"), &m_unicodeSymbolsFont);
#endif

  // Normal text
  m_styles[TS_NORMAL_TEXT].color = wxT("black");
  m_styles[TS_NORMAL_TEXT].bold = false;
  m_styles[TS_NORMAL_TEXT].italic = true;
  m_styles[TS_NORMAL_TEXT].underlined = false;
  config->Read(wxT("Style/NormalText/color"),
               &m_styles[TS_NORMAL_TEXT].color);
  config->Read(wxT("Style/NormalText/bold"),
               &m_styles[TS_NORMAL_TEXT].bold);
  config->Read(wxT("Style/NormalText/italic"),
               &m_styles[TS_NORMAL_TEXT].italic);
  config->Read(wxT("Style/NormalText/underlined"),
               &m_styles[TS_NORMAL_TEXT].underlined);

  // Hidden groups
  m_styles[TS_HIDDEN_GROUP].bold = false;
  m_styles[TS_HIDDEN_GROUP].italic = true;
  m_styles[TS_HIDDEN_GROUP].underlined = true;
  config->Read(wxT("Style/HiddenText/color"),
               &m_styles[TS_HIDDEN_GROUP].color);
  config->Read(wxT("Style/HiddenText/bold"),
               &m_styles[TS_HIDDEN_GROUP].bold);
  config->Read(wxT("Style/HiddenText/italic"),
               &m_styles[TS_HIDDEN_GROUP].italic);
  config->Read(wxT("Style/HiddenText/underlined"),
               &m_styles[TS_HIDDEN_GROUP].underlined);

  // Main prompt
  m_styles[TS_MAIN_PROMPT].color = wxT("red");
  m_styles[TS_MAIN_PROMPT].bold = false;
  m_styles[TS_MAIN_PROMPT].italic = false;
  m_styles[TS_MAIN_PROMPT].underlined = false;
  config->Read(wxT("Style/MainPrompt/color"),
               &m_styles[TS_MAIN_PROMPT].color);
  config->Read(wxT("Style/MainPrompt/bold"),
               &m_styles[TS_MAIN_PROMPT].bold);
  config->Read(wxT("Style/MainPrompt/italic"),
               &m_styles[TS_MAIN_PROMPT].italic);
  config->Read(wxT("Style/MainPrompt/underlined"),
               &m_styles[TS_MAIN_PROMPT].underlined);

  // Other prompt
  m_styles[TS_OTHER_PROMPT].color = wxT("red");
  m_styles[TS_OTHER_PROMPT].bold = false;
  m_styles[TS_OTHER_PROMPT].italic = true;
  m_styles[TS_OTHER_PROMPT].underlined = false;
  config->Read(wxT("Style/OtherPrompt/color"),
               &m_styles[TS_OTHER_PROMPT].color);
  config->Read(wxT("Style/OtherPrompt/bold"),
               &m_styles[TS_OTHER_PROMPT].bold);
  config->Read(wxT("Style/OtherPrompt/italic"),
               &m_styles[TS_OTHER_PROMPT].italic);
  config->Read(wxT("Style/OtherPrompt/underlined"),
               &m_styles[TS_OTHER_PROMPT].underlined);

  // Labels
  m_styles[TS_LABEL].color = wxT("brown");
  m_styles[TS_LABEL].bold = false;
  m_styles[TS_LABEL].italic = false;
  m_styles[TS_LABEL].underlined = false;
  config->Read(wxT("Style/Label/color"),
               &m_styles[TS_LABEL].color);
  config->Read(wxT("Style/Label/bold"),
               &m_styles[TS_LABEL].bold);
  config->Read(wxT("Style/Label/italic"),
               &m_styles[TS_LABEL].italic);
  config->Read(wxT("Style/Label/underlined"),
               &m_styles[TS_LABEL].underlined);

  // Special
  m_styles[TS_SPECIAL_CONSTANT].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_SPECIAL_CONSTANT].bold = false;
  m_styles[TS_SPECIAL_CONSTANT].italic = false;
  m_styles[TS_SPECIAL_CONSTANT].underlined = false;
  config->Read(wxT("Style/Special/color"),
               &m_styles[TS_SPECIAL_CONSTANT].color);
  config->Read(wxT("Style/Special/bold"),
               &m_styles[TS_SPECIAL_CONSTANT].bold);
  config->Read(wxT("Style/Special/italic"),
               &m_styles[TS_SPECIAL_CONSTANT].italic);
  config->Read(wxT("Style/Special/underlined"),
               &m_styles[TS_SPECIAL_CONSTANT].underlined);

  // Input
  m_styles[TS_INPUT].color = wxT("blue");
  m_styles[TS_INPUT].bold = false;
  m_styles[TS_INPUT].italic = false;
  m_styles[TS_INPUT].underlined = false;
  config->Read(wxT("Style/Input/color"),
               &m_styles[TS_INPUT].color);
  config->Read(wxT("Style/Input/bold"),
               &m_styles[TS_INPUT].bold);
  config->Read(wxT("Style/Input/italic"),
               &m_styles[TS_INPUT].italic);
  config->Read(wxT("Style/Input/underlined"),
               &m_styles[TS_INPUT].underlined);

  // Number
  m_styles[TS_NUMBER].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_NUMBER].bold = false;
  m_styles[TS_NUMBER].italic = false;
  m_styles[TS_NUMBER].underlined = false;
  config->Read(wxT("Style/Number/color"),
               &m_styles[TS_NUMBER].color);
  config->Read(wxT("Style/Number/bold"),
               &m_styles[TS_NUMBER].bold);
  config->Read(wxT("Style/Number/italic"),
               &m_styles[TS_NUMBER].italic);
  config->Read(wxT("Style/Number/underlined"),
               &m_styles[TS_NUMBER].underlined);

  // String
  m_styles[TS_STRING].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_STRING].bold = false;
  m_styles[TS_STRING].italic = true;
  m_styles[TS_STRING].underlined = false;
  config->Read(wxT("Style/String/color"),
               &m_styles[TS_STRING].color);
  config->Read(wxT("Style/String/bold"),
               &m_styles[TS_STRING].bold);
  config->Read(wxT("Style/String/italic"),
               &m_styles[TS_STRING].italic);
  config->Read(wxT("Style/String/underlined"),
               &m_styles[TS_STRING].underlined);

  // Greek
  m_styles[TS_GREEK_CONSTANT].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_GREEK_CONSTANT].bold = false;
  m_styles[TS_GREEK_CONSTANT].italic = false;
  m_styles[TS_GREEK_CONSTANT].underlined = false;
  config->Read(wxT("Style/Greek/color"),
               &m_styles[TS_GREEK_CONSTANT].color);
  config->Read(wxT("Style/Greek/bold"),
               &m_styles[TS_GREEK_CONSTANT].bold);
  config->Read(wxT("Style/Greek/italic"),
               &m_styles[TS_GREEK_CONSTANT].italic);
  config->Read(wxT("Style/Greek/underlined"),
               &m_styles[TS_GREEK_CONSTANT].underlined);

  // Variables
  m_styles[TS_VARIABLE].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_VARIABLE].bold = false;
  m_styles[TS_VARIABLE].italic = true;
  m_styles[TS_VARIABLE].underlined = false;
  config->Read(wxT("Style/Variable/color"),
               &m_styles[TS_VARIABLE].color);
  config->Read(wxT("Style/Variable/bold"),
               &m_styles[TS_VARIABLE].bold);
  config->Read(wxT("Style/Variable/italic"),
               &m_styles[TS_VARIABLE].italic);
  config->Read(wxT("Style/Variable/underlined"),
               &m_styles[TS_VARIABLE].underlined);

  // FUNCTIONS
  m_styles[TS_FUNCTION].color = m_styles[TS_NORMAL_TEXT].color;
  m_styles[TS_FUNCTION].bold = false;
  m_styles[TS_FUNCTION].italic = false;
  m_styles[TS_FUNCTION].underlined = false;
  config->Read(wxT("Style/Function/color"),
               &m_styles[TS_FUNCTION].color);
  config->Read(wxT("Style/Function/bold"),
               &m_styles[TS_FUNCTION].bold);
  config->Read(wxT("Style/Function/italic"),
               &m_styles[TS_FUNCTION].italic);
  config->Read(wxT("Style/Function/underlined"),
               &m_styles[TS_FUNCTION].underlined);

  // Highlight
  m_styles[TS_HIGHLIGHT].color = m_styles[TS_NORMAL_TEXT].color;
  config->Read(wxT("Style/Highlight/color"),
               &m_styles[TS_HIGHLIGHT].color);

  // Text background
  m_styles[TS_TEXT_BACKGROUND].color = wxT("white");
  config->Read(wxT("Style/Background/color"),
               &m_styles[TS_TEXT_BACKGROUND].color);
  config->Read(wxT("Style/TextBackground/color"),
               &m_styles[TS_TEXT_BACKGROUND].color);

  m_dc.SetPen(*(wxThePenList->FindOrCreatePen(m_styles[TS_NORMAL_TEXT].color, 1, wxSOLID)));
}

wxFontWeight CellParser::IsBold(int st)
{
  if (m_styles[st].bold)
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

int CellParser::IsItalic(int st)
{
  if (m_styles[st].italic)
    return wxSLANT;
  return wxNORMAL;
}

int CellParser::IsUnderlined(int st)
{
  if (m_styles[st].underlined)
    return 1;
  return 0;
}

wxString CellParser::GetSymbolFontName()
{
#if defined __WXMSW__
  return wxT("Symbol");
#elif wxUSE_UNICODE
  return m_unicodeSymbolsFont;
#else
  return m_fontName;
#endif
}

wxFontEncoding CellParser::GetGreekFontEncoding()
{
#if defined (__WXGTK20__) || defined (__WXMAC__)
  return wxFONTENCODING_DEFAULT;
#else
 #if defined __WXMSW__
  return wxFONTENCODING_CP1253;
 #else
  return wxFONTENCODING_ISO8859_7;
 #endif
#endif
}
