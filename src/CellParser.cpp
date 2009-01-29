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
#include "MathCell.h"

CellParser::CellParser(wxDC& dc) : m_dc(dc)
{
  m_scale = 1.0;
  m_top = -1;
  m_bottom = -1;
  m_forceUpdate = false;
  m_indent = MC_GROUP_LEFT_INDENT;
  m_changeAsterisk = false;
  ReadStyle();
}

CellParser::CellParser(wxDC& dc, double scale) : m_dc(dc)
{
  m_scale = scale;
  m_top = -1;
  m_bottom = -1;
  m_haveGreekFont = false;
  m_greekFontAdj = 0;
  m_changeAsterisk = false;
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

  wxString tmpCol;

#define READ_STYLES(type, where)                                    \
  config->Read(wxT(where "color"), &m_styles[type].color);          \
  config->Read(wxT(where "bold"), &m_styles[type].bold);            \
  config->Read(wxT(where "italic"), &m_styles[type].italic);        \
  config->Read(wxT(where "underlined"), &m_styles[type].underlined);

  // Normal text
  m_styles[TS_DEFAULT].color = wxT("black");
  m_styles[TS_DEFAULT].bold = false;
  m_styles[TS_DEFAULT].italic = true;
  m_styles[TS_DEFAULT].underlined = false;
  READ_STYLES(TS_DEFAULT, "Style/NormalText/")

  // Text
  m_styles[TS_TEXT].color = wxT("black");
  m_styles[TS_TEXT].bold = false;
  m_styles[TS_TEXT].italic = false;
  m_styles[TS_TEXT].underlined = false;
  m_styles[TS_TEXT].fontSize = 0;
  config->Read(wxT("Style/Text/fontsize"),
               &m_styles[TS_TEXT].fontSize);
  config->Read(wxT("Style/Text/fontname"),
               &m_styles[TS_TEXT].font);
  READ_STYLES(TS_TEXT, "Style/Text/")

  // Section
  m_styles[TS_SECTION].color = wxT("black");
  m_styles[TS_SECTION].bold = true;
  m_styles[TS_SECTION].italic = true;
  m_styles[TS_SECTION].underlined = false;
  m_styles[TS_SECTION].fontSize = 18;
  config->Read(wxT("Style/Section/fontsize"),
               &m_styles[TS_SECTION].fontSize);
  config->Read(wxT("Style/Section/fontname"),
               &m_styles[TS_SECTION].font);
  READ_STYLES(TS_SECTION, "Style/Section/")

  // Title
  m_styles[TS_TITLE].color = wxT("black");
  m_styles[TS_TITLE].bold = true;
  m_styles[TS_TITLE].italic = false;
  m_styles[TS_TITLE].underlined = true;
  m_styles[TS_TITLE].fontSize = 24;
  config->Read(wxT("Style/Title/fontsize"),
               &m_styles[TS_TITLE].fontSize);
  config->Read(wxT("Style/Title/fontname"),
               &m_styles[TS_TITLE].font);
  READ_STYLES(TS_TITLE, "Style/Title/")

  // Main prompt
  m_styles[TS_MAIN_PROMPT].color = wxT("red");
  m_styles[TS_MAIN_PROMPT].bold = false;
  m_styles[TS_MAIN_PROMPT].italic = false;
  m_styles[TS_MAIN_PROMPT].underlined = false;
  READ_STYLES(TS_MAIN_PROMPT, "Style/MainPrompt/")

  // Other prompt
  m_styles[TS_OTHER_PROMPT].color = wxT("red");
  m_styles[TS_OTHER_PROMPT].bold = false;
  m_styles[TS_OTHER_PROMPT].italic = true;
  m_styles[TS_OTHER_PROMPT].underlined = false;
  READ_STYLES(TS_OTHER_PROMPT, "Style/OtherPrompt/");

  // Labels
  m_styles[TS_LABEL].color = wxT("brown");
  m_styles[TS_LABEL].bold = false;
  m_styles[TS_LABEL].italic = false;
  m_styles[TS_LABEL].underlined = false;
  READ_STYLES(TS_LABEL, "Style/Label/")

  // Special
  m_styles[TS_SPECIAL_CONSTANT].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_SPECIAL_CONSTANT].bold = false;
  m_styles[TS_SPECIAL_CONSTANT].italic = false;
  m_styles[TS_SPECIAL_CONSTANT].underlined = false;
  READ_STYLES(TS_SPECIAL_CONSTANT, "Style/Special/")

  // Input
  m_styles[TS_INPUT].color = wxT("blue");
  m_styles[TS_INPUT].bold = false;
  m_styles[TS_INPUT].italic = false;
  m_styles[TS_INPUT].underlined = false;
  READ_STYLES(TS_INPUT, "Style/Input/")

  // Number
  m_styles[TS_NUMBER].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_NUMBER].bold = false;
  m_styles[TS_NUMBER].italic = false;
  m_styles[TS_NUMBER].underlined = false;
  READ_STYLES(TS_NUMBER, "Style/Number/")

  // String
  m_styles[TS_STRING].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_STRING].bold = false;
  m_styles[TS_STRING].italic = true;
  m_styles[TS_STRING].underlined = false;
  READ_STYLES(TS_STRING, "Style/String/")

  // Greek
  m_styles[TS_GREEK_CONSTANT].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_GREEK_CONSTANT].bold = false;
  m_styles[TS_GREEK_CONSTANT].italic = false;
  m_styles[TS_GREEK_CONSTANT].underlined = false;
  READ_STYLES(TS_GREEK_CONSTANT, "Style/Greek/")

  // Variables
  m_styles[TS_VARIABLE].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_VARIABLE].bold = false;
  m_styles[TS_VARIABLE].italic = true;
  m_styles[TS_VARIABLE].underlined = false;
  READ_STYLES(TS_VARIABLE, "Style/Variable/")

  // FUNCTIONS
  m_styles[TS_FUNCTION].color = m_styles[TS_DEFAULT].color;
  m_styles[TS_FUNCTION].bold = false;
  m_styles[TS_FUNCTION].italic = false;
  m_styles[TS_FUNCTION].underlined = false;
  READ_STYLES(TS_FUNCTION, "Style/Function/")

  // Highlight
  m_styles[TS_HIGHLIGHT].color = m_styles[TS_DEFAULT].color;
  config->Read(wxT("Style/Highlight/color"),
               &m_styles[TS_HIGHLIGHT].color);

  // Text background
  m_styles[TS_TEXT_BACKGROUND].color = wxT("white");
  config->Read(wxT("Style/Background/color"),
               &m_styles[TS_TEXT_BACKGROUND].color);
  config->Read(wxT("Style/TextBackground/color"),
               &m_styles[TS_TEXT_BACKGROUND].color);

  // Cell bracket colors
  m_styles[TS_CELL_BRACKET].color = wxT("rgb(0,0,0)");
  config->Read(wxT("Style/CellBracket/color"),
               &m_styles[TS_CELL_BRACKET].color);
  m_styles[TS_ACTIVE_CELL_BRACKET].color = wxT("rgb(255,0,0)");
  config->Read(wxT("Style/ActiveCellBracket/color"),
               &m_styles[TS_ACTIVE_CELL_BRACKET].color);

  // Cursor (hcaret in MathCtrl and caret in EditorCell)
  m_styles[TS_CURSOR].color = wxT("rgb(0,0,0)");
  config->Read(wxT("Style/Cursor/color"),
               &m_styles[TS_CURSOR].color);

  // Selection color
  m_styles[TS_SELECTION].color = wxT("light grey"); // on mac should default to system selection color
  config->Read(wxT("Style/Selection/color"),
               &m_styles[TS_SELECTION].color);

#undef READ_STYLES

  m_dc.SetPen(*(wxThePenList->FindOrCreatePen(m_styles[TS_DEFAULT].color, 1, wxSOLID)));
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
