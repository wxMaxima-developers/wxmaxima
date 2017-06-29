// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class Configuration which serves as a fast configuration storage.
 */

#include "Configuration.h"

#include <wx/font.h>
#include <wx/config.h>
#include "MathCell.h"

Configuration::Configuration(wxDC &dc, bool isTopLevel) : m_dc(&dc)
{
  m_mathJaxURL = wxT("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML");
  m_scale = 1.0;
  m_zoomFactor = 1.0; // affects returned fontsizes
  m_top = -1;
  m_bottom = -1;
  m_changeAsterisk = true;
  m_forceUpdate = false;
  m_outdated = false;
  m_printer = false;
  m_TeXFonts = false;
  m_printer = false;
  m_notifyIfIdle = true;
  m_fixReorderedIndices = true;
  m_showBrackets = true;
  m_printBrackets = false;
  m_hideBrackets = true;
  m_lineWidth_em = 88;
  m_showLabelChoice = 1;
  m_clientWidth = 1024;
  m_clientHeight = 768;
  Dirstructure dirstruct;
  m_maximaLocation = dirstruct.MaximaDefaultLocation();
  m_indent = -1;
  ReadConfig();
  m_showCodeCells = true;

}

void Configuration::ShowCodeCells(bool show)
{
  m_showCodeCells = show;
}

bool Configuration::MaximaFound(wxString location)
{
  if (location == wxEmptyString)
    location = m_maximaLocation;
  bool maximaFound = false;
  if (wxFileExists(location))
    maximaFound = true;

  // Find a maxima within an application package.
  if (wxFileExists(location + wxT("/Contents/Resources/maxima.sh")))
    maximaFound = true;

  wxPathList pathlist;
  pathlist.AddEnvList(wxT("PATH"));
  wxString path = pathlist.FindAbsoluteValidPath(location);
  if (!path.empty())
    maximaFound = true;
  return maximaFound;
}

void Configuration::ReadConfig()
{
  Dirstructure dirstruct;

  wxConfig *config = (wxConfig *) wxConfig::Get();
  m_autoWrap = 3;
  config->Read(wxT("autoWrapMode"), &m_autoWrap);

  config->Read(wxT("mathJaxURL"), &m_mathJaxURL);

  config->Read(wxT("fixReorderedIndices"), &m_fixReorderedIndices);

  config->Read(wxT("maxima"), &m_maximaLocation);
  //Fix wrong" maxima=1" paraneter in ~/.wxMaxima if upgrading from 0.7.0a
  if (m_maximaLocation.IsSameAs(wxT("1")))
    m_maximaLocation = dirstruct.MaximaDefaultLocation();

  m_autoIndent = true;
  config->Read(wxT("autoIndent"), &m_autoIndent);

  config->Read(wxT("showLabelChoice"), &m_showLabelChoice);

  config->Read(wxT("changeAsterisk"), &m_changeAsterisk);

  config->Read(wxT("notifyIfIdle"), &m_notifyIfIdle);

  config->Read(wxT("hideBrackets"), &m_hideBrackets);

  m_displayedDigits = 100;
  config->Read(wxT("displayedDigits"), &m_displayedDigits);
  if (m_displayedDigits <= 20)
    m_displayedDigits = 20;

  m_restartOnReEvaluation = true;
  config->Read(wxT("restartOnReEvaluation"), &m_restartOnReEvaluation);

  m_matchParens = true;
  config->Read(wxT("matchParens"), &m_matchParens);

  m_insertAns = true;
  config->Read(wxT("insertAns"), &m_insertAns);

  m_openHCaret = false;
  config->Read(wxT("openHCaret"), &m_openHCaret);


  m_labelWidth = 4;
  config->Read(wxT("labelWidth"), &m_labelWidth);

  config->Read(wxT("printBrackets"), &m_printBrackets);

  m_zoomFactor = 1.0;
  config->Read(wxT("ZoomFactor"), &m_zoomFactor);

  if (wxFontEnumerator::IsValidFacename(m_fontCMEX = CMEX10) &&
      wxFontEnumerator::IsValidFacename(m_fontCMSY = CMSY10) &&
      wxFontEnumerator::IsValidFacename(m_fontCMRI = CMR10) &&
      wxFontEnumerator::IsValidFacename(m_fontCMMI = CMMI10) &&
      wxFontEnumerator::IsValidFacename(m_fontCMTI = CMTI10))
  {
    m_TeXFonts = true;
    config->Read(wxT("usejsmath"), &m_TeXFonts);
  }

  m_keepPercent = true;
  wxConfig::Get()->Read(wxT("keepPercent"), &m_keepPercent);

  ReadStyle();
}

void Configuration::SetZoomFactor(double newzoom)
{
  if (newzoom > GetMaxZoomFactor())
    newzoom = GetMaxZoomFactor();
  if (newzoom < GetMinZoomFactor())
    newzoom = GetMinZoomFactor();

  m_zoomFactor = newzoom;
  wxConfig::Get()->Write(wxT("ZoomFactor"), m_zoomFactor);
}

Configuration::~Configuration()
{
}

wxString Configuration::GetFontName(int type)
{
  if (type == TS_TITLE || type == TS_SUBSECTION || type == TS_SUBSUBSECTION || type == TS_SECTION || type == TS_TEXT)
    return m_styles[type].font;
  else if (type == TS_NUMBER || type == TS_VARIABLE || type == TS_FUNCTION ||
           type == TS_SPECIAL_CONSTANT || type == TS_STRING)
    return m_mathFontName;
  return m_fontName;
}

void Configuration::ReadStyle()
{
  wxConfigBase *config = wxConfig::Get();


  #ifdef __WXMSW__
  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(wxT("Linux Libertine O"));
  font.SetEncoding(wxFONTENCODING_UTF8);
  font.SetStyle(wxFONTSTYLE_NORMAL );
  if(font.IsOk())
    m_fontName = wxT("Linux Libertine O");
  #endif
  
  // Font
  config->Read(wxT("Style/fontname"), &m_fontName);
#ifdef __WXOSX_MAC__
  if (m_fontName.IsEmpty())
  {
    m_fontName = "Monaco";
  }
#endif

  // Default fontsize
  m_defaultFontSize = 12;
  config->Read(wxT("fontSize"), &m_defaultFontSize);
  m_mathFontSize = m_defaultFontSize;
  config->Read(wxT("mathfontsize"), &m_mathFontSize);

  // Encoding - used only for comments
  m_fontEncoding = wxFONTENCODING_DEFAULT;
  int encoding = m_fontEncoding;
  config->Read(wxT("fontEncoding"), &encoding);
  m_fontEncoding = (wxFontEncoding) encoding;

  // Math font
  #ifdef __WXMSW__
  if(font.IsOk())
    m_mathFontName = wxT("Linux Libertine O");
  else
    m_mathFontName = wxEmptyString;
  #endif
  config->Read(wxT("Style/Math/fontname"), &m_mathFontName);
#ifdef __WXOSX_MAC__
  if (m_mathFontName.IsEmpty())
  {
    m_mathFontName = "Monaco";
  }
#endif

  wxString tmp;

#define READ_STYLES(type, where)                                    \
  if (config->Read(wxT(where "color"), &tmp)) m_styles[type].color.Set(tmp);          \
  config->Read(wxT(where "bold"), &m_styles[type].bold);            \
  config->Read(wxT(where "italic"), &m_styles[type].italic);        \
  config->Read(wxT(where "underlined"), &m_styles[type].underlined);

  // Normal text
  m_styles[TS_DEFAULT].color = wxT("black");
  m_styles[TS_DEFAULT].bold = true;
  m_styles[TS_DEFAULT].italic = true;
  m_styles[TS_DEFAULT].underlined = false;
  READ_STYLES(TS_DEFAULT, "Style/NormalText/")

  // Text
  m_styles[TS_TEXT].color = wxT("black");
  m_styles[TS_TEXT].bold = false;
  m_styles[TS_TEXT].italic = false;
  m_styles[TS_TEXT].underlined = false;
  m_styles[TS_TEXT].fontSize = 12;
#ifdef __WXOSX_MAC__
  m_styles[TS_TEXT].font = "Monaco";
#endif
  config->Read(wxT("Style/Text/fontsize"),
               &m_styles[TS_TEXT].fontSize);
  config->Read(wxT("Style/Text/fontname"),
               &m_styles[TS_TEXT].font);
  READ_STYLES(TS_TEXT, "Style/Text/")

  // Variables in highlighted code
  m_styles[TS_CODE_VARIABLE].color = wxT("rgb(0,128,0)");
  m_styles[TS_CODE_VARIABLE].bold = false;
  m_styles[TS_CODE_VARIABLE].italic = true;
  m_styles[TS_CODE_VARIABLE].underlined = false;
  READ_STYLES(TS_CODE_VARIABLE, "Style/CodeHighlighting/Variable/")

  // Keywords in highlighted code
  m_styles[TS_CODE_FUNCTION].color = wxT("rgb(128,0,0)");
  m_styles[TS_CODE_FUNCTION].bold = false;
  m_styles[TS_CODE_FUNCTION].italic = true;
  m_styles[TS_CODE_FUNCTION].underlined = false;
  READ_STYLES(TS_CODE_FUNCTION, "Style/CodeHighlighting/Function/")

  // Comments in highlighted code
  m_styles[TS_CODE_COMMENT].color = wxT("rgb(64,64,64)");
  m_styles[TS_CODE_COMMENT].bold = false;
  m_styles[TS_CODE_COMMENT].italic = true;
  m_styles[TS_CODE_COMMENT].underlined = false;
  READ_STYLES(TS_CODE_COMMENT, "Style/CodeHighlighting/Comment/")

  // Numbers in highlighted code
  m_styles[TS_CODE_NUMBER].color = wxT("rgb(128,64,0)");
  m_styles[TS_CODE_NUMBER].bold = false;
  m_styles[TS_CODE_NUMBER].italic = true;
  m_styles[TS_CODE_NUMBER].underlined = false;
  READ_STYLES(TS_CODE_NUMBER, "Style/CodeHighlighting/Number/")

  // Strings in highlighted code
  m_styles[TS_CODE_STRING].color = wxT("rgb(0,0,128)");
  m_styles[TS_CODE_STRING].bold = false;
  m_styles[TS_CODE_STRING].italic = true;
  m_styles[TS_CODE_STRING].underlined = false;
  READ_STYLES(TS_CODE_STRING, "Style/CodeHighlighting/String/")

  // Operators in highlighted code
  m_styles[TS_CODE_OPERATOR].color = wxT("rgb(0,0,0)");
  m_styles[TS_CODE_OPERATOR].bold = false;
  m_styles[TS_CODE_OPERATOR].italic = true;
  m_styles[TS_CODE_OPERATOR].underlined = false;
  READ_STYLES(TS_CODE_OPERATOR, "Style/CodeHighlighting/Operator/")

  // Line endings in highlighted code
  m_styles[TS_CODE_ENDOFLINE].color = wxT("rgb(128,128,128)");
  m_styles[TS_CODE_ENDOFLINE].bold = false;
  m_styles[TS_CODE_ENDOFLINE].italic = true;
  m_styles[TS_CODE_ENDOFLINE].underlined = false;
  READ_STYLES(TS_CODE_ENDOFLINE, "Style/CodeHighlighting/EndOfLine/")

  // Subsubsection
  m_styles[TS_SUBSUBSECTION].color = wxT("black");
  m_styles[TS_SUBSUBSECTION].bold = true;
  m_styles[TS_SUBSUBSECTION].italic = false;
  m_styles[TS_SUBSUBSECTION].underlined = false;
  m_styles[TS_SUBSUBSECTION].fontSize = 14;
#ifdef __WXOSX_MAC__
  m_styles[TS_SUBSUBSECTION].font = "Monaco";
#endif
  config->Read(wxT("Style/Subsubsection/fontsize"),
               &m_styles[TS_SUBSUBSECTION].fontSize);
  config->Read(wxT("Style/Subsubsection/fontname"),
               &m_styles[TS_SUBSUBSECTION].font);
  READ_STYLES(TS_SUBSUBSECTION, "Style/Subsubsection/")

  // Subsection
  m_styles[TS_SUBSECTION].color = wxT("black");
  m_styles[TS_SUBSECTION].bold = true;
  m_styles[TS_SUBSECTION].italic = false;
  m_styles[TS_SUBSECTION].underlined = false;
  m_styles[TS_SUBSECTION].fontSize = 16;
#ifdef __WXOSX_MAC__
  m_styles[TS_SUBSECTION].font = "Monaco";
#endif
  config->Read(wxT("Style/Subsection/fontsize"),
               &m_styles[TS_SUBSECTION].fontSize);
  config->Read(wxT("Style/Subsection/fontname"),
               &m_styles[TS_SUBSECTION].font);
  READ_STYLES(TS_SUBSECTION, "Style/Subsection/")

  // Section
  m_styles[TS_SECTION].color = wxT("black");
  m_styles[TS_SECTION].bold = true;
  m_styles[TS_SECTION].italic = true;
  m_styles[TS_SECTION].underlined = false;
  m_styles[TS_SECTION].fontSize = 18;
#ifdef __WXOSX_MAC__
  m_styles[TS_SECTION].font = "Monaco";
#endif
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
#ifdef __WXOSX_MAC__
  m_styles[TS_TITLE].font = "Monaco";
#endif
  config->Read(wxT("Style/Title/fontsize"),
               &m_styles[TS_TITLE].fontSize);
  config->Read(wxT("Style/Title/fontname"),
               &m_styles[TS_TITLE].font);
  READ_STYLES(TS_TITLE, "Style/Title/")

  // Warning
  m_styles[TS_WARNING].color = wxT("orange");
  m_styles[TS_WARNING].bold = true;
  m_styles[TS_WARNING].italic = false;
  m_styles[TS_WARNING].underlined = false;
  m_styles[TS_WARNING].fontSize = 24;
  config->Read(wxT("Style/Warning/fontname"),
               &m_styles[TS_WARNING].font);
  READ_STYLES(TS_WARNING, "Style/Warning/")
  
  // Main prompt
  m_styles[TS_MAIN_PROMPT].color = wxT("rgb(255,128,128)");
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
  m_styles[TS_LABEL].color = wxT("rgb(255,192,128)");
  m_styles[TS_LABEL].bold = false;
  m_styles[TS_LABEL].italic = false;
  m_styles[TS_LABEL].underlined = false;
  READ_STYLES(TS_LABEL, "Style/Label/")

  // User-defined Labels
  m_styles[TS_USERLABEL].color = wxT("rgb(255,64,0)");
  m_styles[TS_USERLABEL].bold = false;
  m_styles[TS_USERLABEL].italic = false;
  m_styles[TS_USERLABEL].underlined = false;
  READ_STYLES(TS_USERLABEL, "Style/UserDefinedLabel/")

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
  if (config->Read(wxT("Style/Highlight/color"),
                   &tmp))
    m_styles[TS_HIGHLIGHT].color.Set(tmp);

  // Text background
  m_styles[TS_TEXT_BACKGROUND].color = wxColour(wxT("white"));
  if (config->Read(wxT("Style/TextBackground/color"),
                   &tmp))
    m_styles[TS_TEXT_BACKGROUND].color.Set(tmp);

  // Cell bracket colors
  m_styles[TS_CELL_BRACKET].color = wxColour(wxT("rgb(0,0,0)"));
  if (config->Read(wxT("Style/CellBracket/color"),
                   &tmp))
    m_styles[TS_CELL_BRACKET].color.Set(tmp);

  m_styles[TS_ACTIVE_CELL_BRACKET].color = wxT("rgb(255,0,0)");
  if (config->Read(wxT("Style/ActiveCellBracket/color"),
                   &tmp))
    m_styles[TS_ACTIVE_CELL_BRACKET].color.Set(tmp);

  // Cursor (hcaret in MathCtrl and caret in EditorCell)
  m_styles[TS_CURSOR].color = wxT("rgb(0,0,0)");
  if (config->Read(wxT("Style/Cursor/color"),
                   &tmp))
    m_styles[TS_CURSOR].color.Set(tmp);

  // Selection color defaults to light grey on windows
#if defined __WXMSW__
  m_styles[TS_SELECTION].color = wxColour(wxT("light grey"));
#else
  m_styles[TS_SELECTION].color = wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT);
#endif
  if (config->Read(wxT("Style/Selection/color"),
                   &tmp))
    m_styles[TS_SELECTION].color.Set(tmp);
  m_styles[TS_EQUALSSELECTION].color = wxT("rgb(192,255,192)");
  if (config->Read(wxT("Style/EqualsSelection/color"),
                   &tmp))
    m_styles[TS_EQUALSSELECTION].color.Set(tmp);

  // Outdated cells
  m_styles[TS_OUTDATED].color = wxT("rgb(153,153,153)");
  if (config->Read(wxT("Style/Outdated/color"),
                   &tmp))
    m_styles[TS_OUTDATED].color.Set(tmp);


#undef READ_STYLES

  m_dc->SetPen(*(wxThePenList->FindOrCreatePen(m_styles[TS_DEFAULT].color, 1, wxPENSTYLE_SOLID)));
}

wxFontWeight Configuration::IsBold(int st)
{
  if (m_styles[st].bold)
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(int st)
{
  if (m_styles[st].italic)
    return wxFONTSTYLE_SLANT;
  return wxFONTSTYLE_NORMAL;
}

bool Configuration::IsUnderlined(int st)
{
  return m_styles[st].underlined;
}

wxString Configuration::GetSymbolFontName()
{
#if defined __WXMSW__
  return wxT("Symbol");
#endif
  return m_fontName;
}

wxColour Configuration::GetColor(int st)
{
  if (m_outdated)
    return m_styles[TS_OUTDATED].color;
  return m_styles[st].color;
}

/*
wxFontEncoding Configuration::GetGreekFontEncoding()
{
#if wxUSE_UNICODE || defined (__WXGTK20__) || defined (__WXMAC__)
  return wxFONTENCODING_DEFAULT;
#elif defined __WXMSW__
  return wxFONTENCODING_CP1253;
#else
  return wxFONTENCODING_ISO8859_7;
#endif
}
*/

