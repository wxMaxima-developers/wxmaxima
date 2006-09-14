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
  m_haveSymbolFont = false;
  m_symbolFontAdj = 0;
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

  // Symbol font
  m_haveSymbolFont = false;
  m_symbolFontAdj = 0;
  m_symbolFontName = wxEmptyString;
  config->Read(wxT("Style/Symbol/fontname"), &m_symbolFontName);
  config->Read(wxT("Style/Symbol/adj"), &m_symbolFontAdj);
  config->Read(wxT("Style/Symbol/ok"), &m_haveSymbolFont);

  // Normal text
  m_styles[0].color = wxT("black");
  m_styles[0].bold = false;
  m_styles[0].italic = true;
  m_styles[0].underlined = false;
  config->Read(wxT("Style/NormalText/color"),
               &m_styles[0].color);
  config->Read(wxT("Style/NormalText/bold"),
               &m_styles[0].bold);
  config->Read(wxT("Style/NormalText/italic"),
               &m_styles[0].italic);
  config->Read(wxT("Style/NormalText/underlined"),
               &m_styles[0].underlined);

  // Hidden groups
  m_styles[1].bold = false;
  m_styles[1].italic = true;
  m_styles[1].underlined = true;
  config->Read(wxT("Style/HiddenText/color"),
               &m_styles[1].color);
  config->Read(wxT("Style/HiddenText/bold"),
               &m_styles[1].bold);
  config->Read(wxT("Style/HiddenText/italic"),
               &m_styles[1].italic);
  config->Read(wxT("Style/HiddenText/underlined"),
               &m_styles[1].underlined);

  // Main prompt
  m_styles[2].color = wxT("red");
  m_styles[2].bold = false;
  m_styles[2].italic = false;
  m_styles[2].underlined = false;
  config->Read(wxT("Style/MainPrompt/color"),
               &m_styles[2].color);
  config->Read(wxT("Style/MainPrompt/bold"),
               &m_styles[2].bold);
  config->Read(wxT("Style/MainPrompt/italic"),
               &m_styles[2].italic);
  config->Read(wxT("Style/MainPrompt/underlined"),
               &m_styles[2].underlined);

  // Other prompt
  m_styles[3].color = wxT("red");
  m_styles[3].bold = false;
  m_styles[3].italic = true;
  m_styles[3].underlined = false;
  config->Read(wxT("Style/OtherPrompt/color"),
               &m_styles[3].color);
  config->Read(wxT("Style/OtherPrompt/bold"),
               &m_styles[3].bold);
  config->Read(wxT("Style/OtherPrompt/italic"),
               &m_styles[3].italic);
  config->Read(wxT("Style/OtherPrompt/underlined"),
               &m_styles[3].underlined);

  // Labels
  m_styles[4].color = wxT("brown");
  m_styles[4].bold = false;
  m_styles[4].italic = false;
  m_styles[4].underlined = false;
  config->Read(wxT("Style/Label/color"),
               &m_styles[4].color);
  config->Read(wxT("Style/Label/bold"),
               &m_styles[4].bold);
  config->Read(wxT("Style/Label/italic"),
               &m_styles[4].italic);
  config->Read(wxT("Style/Label/underlined"),
               &m_styles[4].underlined);

  // Special
  m_styles[5].color = m_styles[0].color;
  m_styles[5].bold = false;
  m_styles[5].italic = false;
  m_styles[5].underlined = false;
  config->Read(wxT("Style/Special/color"),
               &m_styles[5].color);
  config->Read(wxT("Style/Special/bold"),
               &m_styles[5].bold);
  config->Read(wxT("Style/Special/italic"),
               &m_styles[5].italic);
  config->Read(wxT("Style/Special/underlined"),
               &m_styles[5].underlined);

  // Input
  m_styles[6].color = wxT("blue");
  m_styles[6].bold = false;
  m_styles[6].italic = false;
  m_styles[6].underlined = false;
  config->Read(wxT("Style/Input/color"),
               &m_styles[6].color);
  config->Read(wxT("Style/Input/bold"),
               &m_styles[6].bold);
  config->Read(wxT("Style/Input/italic"),
               &m_styles[6].italic);
  config->Read(wxT("Style/Input/underlined"),
               &m_styles[6].underlined);

  // Number
  m_styles[7].color = m_styles[0].color;
  m_styles[7].bold = false;
  m_styles[7].italic = false;
  m_styles[7].underlined = false;
  config->Read(wxT("Style/Number/color"),
               &m_styles[7].color);
  config->Read(wxT("Style/Number/bold"),
               &m_styles[7].bold);
  config->Read(wxT("Style/Number/italic"),
               &m_styles[7].italic);
  config->Read(wxT("Style/Number/underlined"),
               &m_styles[7].underlined);

  // String
  m_styles[8].color = m_styles[0].color;
  m_styles[8].bold = false;
  m_styles[8].italic = true;
  m_styles[8].underlined = false;
  config->Read(wxT("Style/String/color"),
               &m_styles[8].color);
  config->Read(wxT("Style/String/bold"),
               &m_styles[8].bold);
  config->Read(wxT("Style/String/italic"),
               &m_styles[8].italic);
  config->Read(wxT("Style/String/underlined"),
               &m_styles[8].underlined);

  // Greek
  m_styles[9].color = m_styles[0].color;
  m_styles[9].bold = false;
  m_styles[9].italic = false;
  m_styles[9].underlined = false;
  config->Read(wxT("Style/Greek/color"),
               &m_styles[9].color);
  config->Read(wxT("Style/Greek/bold"),
               &m_styles[9].bold);
  config->Read(wxT("Style/Greek/italic"),
               &m_styles[9].italic);
  config->Read(wxT("Style/Greek/underlined"),
               &m_styles[9].underlined);

  // Variables
  m_styles[10].color = m_styles[0].color;
  m_styles[10].bold = false;
  m_styles[10].italic = true;
  m_styles[10].underlined = false;
  config->Read(wxT("Style/Variable/color"),
               &m_styles[10].color);
  config->Read(wxT("Style/Variable/bold"),
               &m_styles[10].bold);
  config->Read(wxT("Style/Variable/italic"),
               &m_styles[10].italic);
  config->Read(wxT("Style/Variable/underlined"),
               &m_styles[10].underlined);

  // Highlight
  m_styles[11].color = m_styles[0].color;
  config->Read(wxT("Style/Highlight/color"),
               &m_styles[11].color);

  m_dc.SetPen(*(wxThePenList->FindOrCreatePen(m_styles[0].color, 1, wxSOLID)));
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
#else
  return wxT("Standard Symbols L");
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
