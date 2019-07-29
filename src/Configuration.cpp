// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class Configuration which serves as a fast configuration storage.
 */

#include "Configuration.h"
#include "Dirstructure.h"
#include "ErrorRedirector.h"
#include <wx/wx.h>
#include <wx/string.h>
#include <wx/font.h>
#include <wx/config.h>
#include <wx/wfstream.h>
#include <wx/fileconf.h>
#include "Cell.h"

Configuration::Configuration(wxDC *dc) : m_dc(dc) 
{
  m_htmlEquationFormat = mathJaX_TeX;
  m_autodetectMaxima = true;
  m_BackgroundBrush = *wxWHITE_BRUSH;
  m_clipToDrawRegion = true;
  m_fontChanged = true;
  m_mathJaxURL_UseUser = false;
  m_TOCshowsSectionNumbers = false;
  m_antialiassingDC = NULL;
  m_parenthesisDrawMode = unknown;
  m_mathJaxURL = wxT("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML");
  m_zoomFactor = 1.0; // affects returned fontsizes
  m_top = -1;
  m_bottom = -1;
  m_changeAsterisk = true;
  m_workSheet = NULL;
  m_printScale = 1.0;
  m_forceUpdate = false;
  m_outdated = false;
  m_printing = false;
  m_TeXFonts = false;
  m_printing = false;
  m_notifyIfIdle = true;
  m_fixReorderedIndices = true;
  m_showBrackets = true;
  m_printBrackets = false;
  m_hideBrackets = true;
  m_lineWidth_em = 88;
  m_adjustWorksheetSizeNeeded = false;
  m_showLabelChoice = 1;
  m_abortOnError = true;
  m_autoSaveMinutes = 3;
  m_clientWidth = 1024;
  m_clientHeight = 768;
  m_indentMaths=true;
  if(m_maximaLocation_override != wxEmptyString)
    m_maximaUserLocation = m_maximaLocation_override;
  else
    m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();
  m_indent = -1;
  m_autoSubscript = 1;
  m_antiAliasLines = true;
  ReadConfig();
  m_showCodeCells = true;
  m_defaultToolTip = wxEmptyString;
  m_copyBitmap = false; // Otherwise MS Office, OpenOffice and LibreOffice prefer the bitmap
  // to Mathml and RTF. Also mail programs prefer bitmaps to text - which is counter-productive
  // for maxima-discuss.
  m_copyMathML = true;
  m_copyMathMLHTML = false;
  m_copyRTF = true;
  m_copySVG = true;
  m_copyEMF = false;
  m_showLength = 2;
  m_useUnicodeMaths = true;
  m_escCodes["pm"]    = wxT("\x00B1");
  m_escCodes["+/-"]   = wxT("\x00B1");
  m_escCodes["alpha"] = wxT("\x03B1");
  m_escCodes["beta"]  = wxT("\x03B2");
  m_escCodes["gamma"] = wxT("\x03B3");
  m_escCodes["delta"] = wxT("\x03B4");
  m_escCodes["epsilon"] = wxT("\x03B5");
  m_escCodes["zeta"] = wxT("\x03B6");
  m_escCodes["eta"] = wxT("\x03B7");
  m_escCodes["theta"] = wxT("\x03B8");
  m_escCodes["iota"] = wxT("\x03B9");
  m_escCodes["kappa"] = wxT("\x03BA");
  m_escCodes["lambda"] = wxT("\x03BB");
  m_escCodes["mu"] = wxT("\x03BC");
  m_escCodes["nu"] = wxT("\x03BD");
  m_escCodes["xi"] = wxT("\x03BE");
  m_escCodes["om"] = wxT("\x03BF");
  m_escCodes["omicron"] = wxT("\x03BF");
  m_escCodes["pi"] = wxT("\x03C0");
  m_escCodes["rho"] = wxT("\x03C1");
  m_escCodes["sigma"] = wxT("\x03C3");
  m_escCodes["tau"] = wxT("\x03C4");
  m_escCodes["upsilon"] = wxT("\x03C5");
  m_escCodes["phi"] = wxT("\x03C6");
  m_escCodes["chi"] = wxT("\x03C7");
  m_escCodes["psi"] = wxT("\x03C8");
  m_escCodes["omega"] = wxT("\x03C9");
  m_escCodes["Alpha"] = wxT("\x0391");
  m_escCodes["Beta"] = wxT("\x0392");
  m_escCodes["Gamma"] = wxT("\x0393");
  m_escCodes["Delta"] = wxT("\x0394");
  m_escCodes["Epsilon"] = wxT("\x0395");
  m_escCodes["Zeta"] = wxT("\x0396");
  m_escCodes["Eta"] = wxT("\x0397");
  m_escCodes["Theta"] = wxT("\x0398");
  m_escCodes["Iota"] = wxT("\x0399");
  m_escCodes["Kappa"] = wxT("\x039A");
  m_escCodes["Lambda"] = wxT("\x039B");
  m_escCodes["Mu"] = wxT("\x039C");
  m_escCodes["Nu"] = wxT("\x039D");
  m_escCodes["Xi"] = wxT("\x039E");
  m_escCodes["Omicron"] = wxT("\x039F");
  m_escCodes["Pi"] = wxT("\x03A0");
  m_escCodes["Rho"] = wxT("\x03A1");
  m_escCodes["Sigma"] = wxT("\x03A3");
  m_escCodes["Tau"] = wxT("\x03A4");
  m_escCodes["Upsilon"] = wxT("\x03A5");
  m_escCodes["Phi"] = wxT("\x03A6");
  m_escCodes["Chi"] = wxT("\x03A7");
  m_escCodes["Psi"] = wxT("\x03A8");
  m_escCodes["Omega"] = wxT("\x03A9");
  m_escCodes["Ohm"] = wxT("\x03A9");
  //////////////////////////
  m_escCodes["^2"] = wxT("\x00B2");
  m_escCodes["^3"] = wxT("\x00B3");
  m_escCodes["/2"] = wxT("\x00BD");
  m_escCodes["sq"] = wxT("\x221A");
  m_escCodes["ii"] = wxT("\x2148");
  m_escCodes["ee"] = wxT("\x2147");
  m_escCodes["hb"] = wxT("\x210F");
  m_escCodes["in"] = wxT("\x2208");
  m_escCodes["impl"] = wxT("\x21D2");
  m_escCodes["inf"] = wxT("\x221e");
  m_escCodes["empty"] = wxT("\x2205");
  m_escCodes["TB"] = wxT("\x25b6");
  m_escCodes["tb"] = wxT("\x25b8");
  m_escCodes["and"] = wxT("\x22C0");
  m_escCodes["or"] = wxT("\x22C1");
  m_escCodes["xor"] = wxT("\x22BB");
  m_escCodes["nand"] = wxT("\x22BC");
  m_escCodes["nor"] = wxT("\x22BD");
  m_escCodes["implies"] = wxT("\x21D2");
  m_escCodes["=>"] = wxT("\x21D2");
  m_escCodes["equiv"] = wxT("\x21D4");
  m_escCodes["<=>"] = wxT("\x21D4");
  m_escCodes["not"] = wxT("\x00AC");
  m_escCodes["union"] = wxT("\x22C3");
  m_escCodes["inter"] = wxT("\x22C2");
  m_escCodes["subseteq"] = wxT("\x2286");
  m_escCodes["subset"] = wxT("\x2282");
  m_escCodes["notsubseteq"] = wxT("\x2288");
  m_escCodes["notsubset"] = wxT("\x2284");
  m_escCodes["hbar"] = wxT("\x0127");
  m_escCodes["Hbar"] = wxT("\x0126");
  m_escCodes["partial"] = wxT("\x2202");
  m_escCodes["integral"] = wxT("\x222b");
  m_escCodes["approx"] = wxT("\x2245");
  m_escCodes["prop"] = wxT("\x221d");
  m_escCodes["propto"] = wxT("\x221d");
  m_escCodes["neq"] = wxT("\x2260");
  m_escCodes["!="] = wxT("\x2260");
  m_escCodes["/="] = wxT("\x2260");
  m_escCodes["#"] = wxT("\x2260");
  m_escCodes["<="] = wxT("\x2264");
  m_escCodes["leq"] = wxT("\x2264");
  m_escCodes[">="] = wxT("\x2265");
  m_escCodes["geq"] = wxT("\x2265");
  m_escCodes["ll"] = wxT("\x226A");
  m_escCodes["<<"] = wxT("\x226A");
  m_escCodes["gg"] = wxT("\x226B");
  m_escCodes[">>"] = wxT("\x226B");
  m_escCodes["qed"] = wxT("\x220E");
  m_escCodes["equiv"] = wxT("\x2263");
  m_escCodes["sum"] = wxT("\x2211");
  m_escCodes["prod"] = wxT("\x220F");
  m_escCodes["product"] = wxT("\x220F");
  m_escCodes["exists"] = wxT("\x2203");
  m_escCodes["nexists"] = wxT("\x2204");
  m_escCodes["parallel"] = wxT("\x2225");
  m_escCodes["perp"] = wxT("\x27C2");
  m_escCodes["perpendicular"] = wxT("\x27C2");
  m_escCodes["bot"] = wxT("\x27C2");
  m_escCodes["leadsto"] = wxT("\x219D");
  m_escCodes["->"] = wxT("\x2192");
  m_escCodes["-->"] = wxT("\x27F6");
  m_escCodes[" --> "] = wxT("\x27F6");

  m_parenthesisDrawMode = unknown;

  #ifdef __WXMSW__
  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(wxT("Linux Libertine O"));
  font.SetStyle(wxFONTSTYLE_NORMAL );
  if(font.IsOk())
    m_fontName = wxT("Linux Libertine O");
  if(font.IsOk())
    m_mathFontName = wxT("Linux Libertine O");
  else
    m_mathFontName = wxEmptyString;
  #endif
  m_defaultFontSize = 12;
  m_mathFontSize = m_defaultFontSize;
  m_fontEncoding = wxFONTENCODING_DEFAULT;
  m_styles[TS_DEFAULT].Set(_("Default"),*wxBLACK, true, true, false, 12);
  m_styles[TS_TEXT].Set(_("Text cell"),*wxBLACK, false, false, false, 12);
  m_styles[TS_CODE_VARIABLE].Set(_("Code highlighting: Variables"),wxColor(0,128,0), false, true, false);
  m_styles[TS_CODE_FUNCTION].Set(_("Code highlighting: Functions"),wxColor(128,0,0), false, true, false);
  m_styles[TS_CODE_COMMENT].Set(_("Code highlighting: Comments"),wxColor(64,64,64), false, true, false);
  m_styles[TS_CODE_NUMBER].Set(_("Code highlighting: Numbers"),wxColor(128,64,0), false, true, false);
  m_styles[TS_CODE_STRING].Set(_("Code highlighting: Strings"),wxColor(0,0,128), false, true, false);
  m_styles[TS_CODE_OPERATOR].Set(_("Code highlighting: Operators"),*wxBLACK, false, true, false);
  m_styles[TS_CODE_LISP].Set(_("Code highlighting: Lisp"),wxColor(255,0,128), false, true, false);
  m_styles[TS_CODE_ENDOFLINE].Set(_("Code highlighting: End of line"),wxColor(128,128,128), false, true, false);
  m_styles[TS_GREEK_CONSTANT].Set(_("Greek constants"),*wxBLACK, false, true, false);
  m_styles[TS_HEADING6].Set(_("Heading 6"),*wxBLACK, true, false, false, 14);
  m_styles[TS_HEADING5].Set(_("Heading 5"),*wxBLACK, true, false, false, 15);
  m_styles[TS_SUBSUBSECTION].Set(_("Subsubsection cell"),*wxBLACK, true, false, false, 16);
  m_styles[TS_SUBSECTION].Set(_("Subsection cell"),*wxBLACK, true, false, false, 16);
  m_styles[TS_SECTION].Set(_("Section cell"),*wxBLACK, true, true, false, 18);
  m_styles[TS_TITLE].Set(_("Title cell"),*wxBLACK, true, false, true, 24);
  m_styles[TS_WARNING].Set(_("Maxima warnings"),wxColor(wxT("orange")), true, false, false, 12);
  m_styles[TS_ERROR].Set(_("Maxima errors"),*wxRED, false, false, false, 12);
  m_styles[TS_MAIN_PROMPT].Set(_("Input labels"),wxColor(wxT("rgb(255,128,128)")), false, false, false);
  m_styles[TS_OTHER_PROMPT].Set(_("Maxima questions"),*wxRED, false, true, false);
  m_styles[TS_LABEL].Set(_("Output labels"),wxColor(wxT("rgb(255,192,128)")), false, false, false);
  m_styles[TS_USERLABEL].Set(_("User-defined labels"),wxColor(wxT("rgb(255,64,0)")), false, false, false);
  m_styles[TS_SPECIAL_CONSTANT].Set(_("Special constants"),*wxBLACK, false, false, false);
  m_styles[TS_INPUT].Set(_("Maxima input"),*wxBLUE, false, false, false);
  m_styles[TS_NUMBER].Set(_("Numbers"),*wxBLACK, false, false, false);
  m_styles[TS_STRING].Set(_("Strings"),*wxBLACK, false, true, false);
  m_styles[TS_GREEK_CONSTANT].Set(_("Greek Constants"),*wxBLACK, false, false, false);
  m_styles[TS_VARIABLE].Set(_("Variables"),*wxBLACK, false, true, false);
  m_styles[TS_FUNCTION].Set(_("Function names"),*wxBLACK);
  m_styles[TS_HIGHLIGHT].Set(_("Highlight (dpart)"),*wxRED);
  m_styles[TS_TEXT_BACKGROUND].Set(_("Text cell background"),*wxWHITE);
  m_styles[TS_DOCUMENT_BACKGROUND].Set(_("Document background"),*wxWHITE);
  m_styles[TS_CELL_BRACKET].Set(_("Cell bracket"),wxColour(wxT("rgb(0,0,0)")));
  m_styles[TS_ACTIVE_CELL_BRACKET].Set(_("Active cell bracket"),wxT("rgb(255,0,0)"));
  m_styles[TS_CURSOR].Set(_("Cursor"),wxT("rgb(0,0,0)"));
  m_styles[TS_SELECTION].Set(_("Selection"),wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT));
  m_styles[TS_EQUALSSELECTION].Set(_("Text equal to selection"),wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT).ChangeLightness(150));
  m_styles[TS_OUTDATED].Set(_("Outdated cells"),wxColor(wxT("rgb(153,153,153)")));
}

wxSize Configuration::GetPPI(wxWindow *win)
{
  if(win == NULL)
    return wxSize(96,96);
  
  wxSize ppi(-1,-1);
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  
  int display_idx = wxDisplay::GetFromWindow(win);
  if (display_idx < 0)
    ppi = wxSize(72,72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#endif

  if((ppi.x < 10) || (ppi.y < 10))
    ppi = wxGetDisplayPPI();
  if((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72,72);

  return ppi;
}

wxString Configuration::GetAutosubscript_string(){
  switch (m_autoSubscript)
  {
  case 0:
    return "nil";
    break;
  case 1:
    return "t";
    break;
  default:
    return "'all";
    break;
  }
}

void Configuration::ShowCodeCells(bool show)
{
  m_showCodeCells = show;
}

bool Configuration::MaximaFound(wxString location)
{
  if(location == wxEmptyString)
    return false;
  
  bool maximaFound = false;
  if (wxFileExists(location))
    maximaFound = true;

  // Find a maxima within an application package.
  if (wxFileExists(location + wxT("/Contents/Resources/maxima.sh")))
    maximaFound = true;

  // Don't complain if PATH doesn't yield a result.
  SuppressErrorDialogs logNull;

  if(!(location.EndsWith("/") || location.EndsWith("\\")))
  {
    wxPathList pathlist;
    pathlist.AddEnvList(wxT("PATH"));
    wxString path = pathlist.FindAbsoluteValidPath(location);
    if (!path.empty())
      maximaFound = true;
  }
  return maximaFound;
}

void Configuration::ReadConfig()
{
  wxConfigBase *config = wxConfig::Get();
  m_autoWrap = 3;

  config->Read(wxT("autoSaveMinutes"),&m_autoSaveMinutes);
  if(m_autoSaveMinutes < 0)
    m_autoSaveMinutes = 0;

  {
    int tmp;
    config->Read("HTMLequationFormat", &tmp);
    m_htmlEquationFormat = (Configuration::htmlExportFormat)tmp;
  }
  
config->Read(wxT("TOCshowsSectionNumbers"), &m_TOCshowsSectionNumbers);
  config->Read(wxT("autoWrapMode"), &m_autoWrap);
  config->Read(wxT("mathJaxURL_UseUser"), &m_mathJaxURL_UseUser);
  config->Read(wxT("useUnicodeMaths"), &m_useUnicodeMaths);
  config->Read(wxT("mathJaxURL"), &m_mathJaxURL);
  config->Read(wxT("autosubscript"), &m_autoSubscript);
  config->Read(wxT("antiAliasLines"), &m_antiAliasLines);
  config->Read(wxT("indentMaths"), &m_indentMaths);
  config->Read(wxT("abortOnError"),&m_abortOnError);

  config->Read(wxT("fixReorderedIndices"), &m_fixReorderedIndices);

  config->Read(wxT("showLength"), &m_showLength);
  config->Read(wxT("printScale"), &m_printScale);

  config->Read(wxT("copyBitmap"), &m_copyBitmap);
  config->Read(wxT("copyMathML"), &m_copyMathML);
  config->Read(wxT("copyMathMLHTML"), &m_copyMathMLHTML);
  config->Read(wxT("copyRTF"), &m_copyRTF);
  config->Read(wxT("copySVG"), &m_copySVG );
  config->Read(wxT("copyEMF"), &m_copyEMF );

  config->Read(wxT("autodetectMaxima"), &m_autodetectMaxima);

  config->Read(wxT("maxima"), &m_maximaUserLocation);
  // Fix wrong" maxima=1" paraneter in ~/.wxMaxima if upgrading from 0.7.0a
  if (m_maximaUserLocation.IsSameAs(wxT("1")))
    m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();

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

  m_insertAns = false;
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

  ReadStyles();
}

wxFont Configuration::GetFont(TextStyle textStyle, int fontSize)
{
  wxString fontName;
  wxFontStyle fontStyle;
  wxFontWeight fontWeight;
  wxFontEncoding fontEncoding;
  bool underlined = IsUnderlined(textStyle);
  
  if ((textStyle == TS_TITLE) ||
      (textStyle == TS_SECTION) ||
      (textStyle == TS_SUBSECTION) ||
      (textStyle == TS_SUBSUBSECTION) ||
      (textStyle == TS_HEADING5) ||
      (textStyle == TS_HEADING6))
  {
    // While titles and section names may be underlined the section number
    // isn't. Else the space between section number and section title
    // would look weird.
    underlined = false;

    // Besides that these items have a fixed font size.
    fontSize = GetFontSize(textStyle);
  }  
  if (fontSize < 4)
    fontSize = 4;

  // The font size scales with the worksheet
  int fontSize1 = Scale_Px(fontSize);

  // Ensure a sane minimum font size
  if (fontSize1 < 4)
    fontSize1 = 4;


  fontName = GetFontName(textStyle);
  fontStyle = IsItalic(textStyle);
  fontWeight = IsBold(textStyle);
  
  fontEncoding = GetFontEncoding();
  
  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(fontName);
  font.SetEncoding(fontEncoding);
  font.SetStyle(fontStyle);
  font.SetWeight(fontWeight);
  font.SetUnderlined(underlined);
  font.SetEncoding(fontEncoding);
  if (!font.IsOk())
  {
    font.SetFamily(wxFONTFAMILY_MODERN);
    font.SetEncoding(fontEncoding);
    font.SetStyle(fontStyle);
    font.SetWeight(fontWeight);
    font.SetUnderlined(underlined);
  }
  
  if (!font.IsOk())
    font = *wxNORMAL_FONT;
  
  font.SetPointSize(fontSize1);

  return font;
}

Configuration::drawMode Configuration::GetGrouphesisDrawMode()
{
  if(m_parenthesisDrawMode == unknown)
  {
    m_parenthesisDrawMode = handdrawn;
    wxFont font = GetFont(TS_FUNCTION,20);
    if (CharsExistInFont(font,
                         wxT(PAREN_OPEN_TOP_UNICODE),
                         wxT(PAREN_OPEN_EXTEND_UNICODE),
                         wxT(PAREN_OPEN_BOTTOM_UNICODE))
      )
    {
      m_parenthesisDrawMode = assembled_unicode;
      return m_parenthesisDrawMode;
    }
    font.SetFaceName(wxT("Linux Libertine"));
    if (CharsExistInFont(font,
                         wxT(PAREN_OPEN_TOP_UNICODE),
                         wxT(PAREN_OPEN_EXTEND_UNICODE),
                         wxT(PAREN_OPEN_BOTTOM_UNICODE))
      )
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont;
      return m_parenthesisDrawMode;
    }
      
    font.SetFaceName(wxT("Linux Libertine O"));
    if (CharsExistInFont(font,
                         wxT(PAREN_OPEN_TOP_UNICODE),
                         wxT(PAREN_OPEN_EXTEND_UNICODE),
                         wxT(PAREN_OPEN_BOTTOM_UNICODE))
      )
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont2;
      return m_parenthesisDrawMode;
    }
  }
  return m_parenthesisDrawMode;
}

bool Configuration::IsEqual(wxBitmap bitmap1, wxBitmap bitmap2)
{
  if (bitmap1.GetSize() != bitmap2.GetSize())
    return false;

  wxImage img1=bitmap1.ConvertToImage();
  wxImage img2=bitmap2.ConvertToImage();
  int bytes = img1.GetWidth()*img1.GetHeight()*3;

  if(bytes < 0)
    return false;

  bool equal = (memcmp(img1.GetData(),img2.GetData(),bytes) == 0);
  return equal;
}

void Configuration::SetZoomFactor(double newzoom)
{
  if (newzoom > GetMaxZoomFactor())
    newzoom = GetMaxZoomFactor();
  if (newzoom < GetMinZoomFactor())
    newzoom = GetMinZoomFactor();

  m_zoomFactor = newzoom;
  wxConfig::Get()->Write(wxT("ZoomFactor"), m_zoomFactor);
  RecalculationForce(true);
}

Configuration::~Configuration()
{
  WriteStyles();
}

bool Configuration::CharsExistInFont(wxFont font, wxString char1,wxString char2, wxString char3)
{
  wxString name = char1 + char2 + char3;
  CharsInFontMap::iterator it = m_charsInFontMap.find(name);
  if(it != m_charsInFontMap.end())
    return it->second;

  if(!font.IsOk())
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  // Seems like Apple didn't hold to their high standards as the maths part of this font
  // don't form nice big mathematical symbols => Blacklisting this font.
  if (font.GetFaceName() == wxT("Monaco"))
  {
    m_charsInFontMap[name] = false;
    return false;
  }

  if(!m_useUnicodeMaths)
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  
  // Letters with width or height = 0 don't exist in the current font
  int width1,height1,descent1;
  GetDC()->SetFont(font);
  GetDC()->GetTextExtent(char1,&width1,&height1,&descent1);
  if((width1 < 1) || (height1-descent1 < 1))
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  int width2,height2,descent2;
  GetDC()->GetTextExtent(char2,&width2,&height2,&descent2);
  if((width2 < 1) || (height2-descent2 < 1))
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  int width3,height3,descent3;
  GetDC()->GetTextExtent(char3,&width3,&height3,&descent3);
  if((width3 < 1) || (height3-descent3 < 1))
  {
    m_charsInFontMap[name] = false;
    return false;
  }

  if(((width1 != width2) &&
      (width1 != width3) &&
      (width2 != width3))||
     ((height1 != height2) &&
      (height1 != height3) &&
      (height2 != height3)))
  {
    m_charsInFontMap[name] = true;
    return true;
  }
  
  wxBitmap bmp1(width1,height1);
  wxMemoryDC dc1(bmp1);
  dc1.SetFont(font);
  dc1.SelectObject(bmp1);
  dc1.Clear();
  dc1.DrawText(char1,wxPoint(0,0));
  
  wxBitmap bmp2(width2,height2);
  wxMemoryDC dc2(bmp2);
  dc2.SetFont(font);
  dc2.SelectObject(bmp2);
  dc2.Clear();
  dc2.DrawText(char2,wxPoint(0,0));
  
  wxBitmap bmp3(width3,height3);
  wxMemoryDC dc3(bmp3);
  dc3.SetFont(font);
  dc3.SelectObject(bmp3);
  dc3.Clear();
  dc3.DrawText(char3,wxPoint(0,0));

  if(IsEqual(bmp1,bmp2) || IsEqual(bmp2,bmp3) || IsEqual(bmp1,bmp3))
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  else
  {
    m_charsInFontMap[name] = false;
    return true;
  }
}

wxString Configuration::GetFontName(int type)
{
  wxString retval = m_fontName;
  if (type == TS_TITLE || type == TS_SUBSECTION || type == TS_SUBSUBSECTION ||
      type == TS_HEADING5 || type == TS_HEADING6 || type == TS_SECTION || type == TS_TEXT)
    retval = m_styles[type].FontName();
  if(retval == wxEmptyString)
    retval = m_fontName;
  
  if (type == TS_NUMBER || type == TS_VARIABLE || type == TS_FUNCTION ||
      type == TS_SPECIAL_CONSTANT || type == TS_STRING)
    retval = m_mathFontName;
  return retval;
}

wxString Configuration::MaximaLocation()
{
  if(m_autodetectMaxima)
    return MaximaDefaultLocation();
  else
    return m_maximaUserLocation;
}

wxString Configuration::MaximaDefaultLocation()
{ 
  return Dirstructure::Get()->MaximaDefaultLocation();
}

void Configuration::ReadStyles(wxString file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
  {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }
  
  // Font
  config->Read(wxT("Style/fontname"), &m_fontName);
#ifdef __WXOSX_MAC__
  if (m_fontName.IsEmpty())
  {
    m_fontName = "Monaco";
  }
#endif

  config->Read(wxT("fontSize"), &m_defaultFontSize);
  config->Read(wxT("mathfontsize"), &m_mathFontSize);
  int encoding = m_fontEncoding;
  config->Read(wxT("fontEncoding"), &encoding);
  m_fontEncoding = (wxFontEncoding) encoding;

  config->Read(wxT("Style/Math/fontname"), &m_mathFontName);
#ifdef __WXOSX_MAC__
  if (m_mathFontName.IsEmpty())
  {
    m_mathFontName = "Monaco";
  }
#endif
  
  m_styles[TS_DEFAULT].Read(config, "Style/Default/");
  m_styles[TS_TEXT].Read(config, "Style/Text/");
  m_styles[TS_CODE_VARIABLE].Read(config, "Style/CodeHighlighting/Variable/");
  m_styles[TS_CODE_FUNCTION].Read(config, "Style/CodeHighlighting/Function/");
  m_styles[TS_CODE_COMMENT].Read(config, "Style/CodeHighlighting/Comment/");
  m_styles[TS_CODE_NUMBER].Read(config, "Style/CodeHighlighting/Number/");
  m_styles[TS_CODE_STRING].Read(config, "Style/CodeHighlighting/String/");
  m_styles[TS_CODE_OPERATOR].Read(config, "Style/CodeHighlighting/Operator/");
  m_styles[TS_CODE_LISP].Read(config, "Style/CodeHighlighting/Lisp/");
  m_styles[TS_CODE_ENDOFLINE].Read(config, "Style/CodeHighlighting/EndOfLine/");
  m_styles[TS_HEADING6].Read(config, "Style/Heading6/");
  m_styles[TS_HEADING5].Read(config, "Style/Heading5/");
  m_styles[TS_SUBSUBSECTION].Read(config, "Style/Subsubsection/");
  m_styles[TS_SUBSECTION].Read(config, "Style/Subsection/");
  m_styles[TS_SECTION].Read(config, "Style/Section/");
  m_styles[TS_TITLE].Read(config, "Style/Title/");
  m_styles[TS_WARNING].Read(config, "Style/Warning/");
  m_styles[TS_MAIN_PROMPT].Read(config, "Style/MainPrompt/");
  m_styles[TS_OTHER_PROMPT].Read(config, "Style/OtherPrompt/");
  m_styles[TS_LABEL].Read(config, "Style/Label/");  
  m_styles[TS_USERLABEL].Read(config, "Style/UserDefinedLabel/");
  m_styles[TS_SPECIAL_CONSTANT].Read(config, "Style/Special/");
  m_styles[TS_GREEK_CONSTANT].Read(config, "Style/Greek/");
  m_styles[TS_INPUT].Read(config, "Style/Input/");
  m_styles[TS_NUMBER].Read(config, "Style/Number/");
  m_styles[TS_STRING].Read(config, "Style/String/");
  m_styles[TS_GREEK_CONSTANT].Read(config, "Style/Greek/");
  m_styles[TS_VARIABLE].Read(config, "Style/Variable/");
  m_styles[TS_FUNCTION].Read(config, "Style/Function/");
  m_styles[TS_HIGHLIGHT].Read(config, "Style/Highlight/");  
  m_styles[TS_TEXT_BACKGROUND].Read(config, "Style/Background/");    
  m_styles[TS_DOCUMENT_BACKGROUND].Read(config, "Style/DocumentBackground/");
  m_styles[TS_ERROR].Read(config, "Style/Error/");
  m_styles[TS_CELL_BRACKET].Read(config, "Style/CellBracket/");
  m_styles[TS_ACTIVE_CELL_BRACKET].Read(config,wxT("Style/ActiveCellBracket/"));
  m_styles[TS_CURSOR].Read(config,wxT("Style/ActiveCellBracket/"));
  m_styles[TS_SELECTION].Read(config,wxT("Style/Selection/"));
  m_styles[TS_EQUALSSELECTION].Read(config,wxT("Style/EqualsSelection/"));
  m_styles[TS_OUTDATED].Read(config,wxT("Style/Outdated/"));
  m_BackgroundBrush = *wxTheBrushList->FindOrCreateBrush(m_styles[TS_DOCUMENT_BACKGROUND].GetColor(), wxBRUSHSTYLE_SOLID);

}

//! Saves the style settings to a file.
void Configuration::WriteStyles(wxString file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxT("wxMaxima"),wxEmptyString,file);

  // Font
  config->Write(wxT("Style/fontname"), m_fontName);
  config->Write(wxT("fontSize"), m_defaultFontSize);
  config->Write(wxT("mathfontsize"), m_mathFontSize);
  config->Write(wxT("fontEncoding"), static_cast<int>(m_fontEncoding));
  config->Write(wxT("Style/Math/fontname"), m_mathFontName);
  
  m_styles[TS_DEFAULT].Write(config, "Style/Default/");
  m_styles[TS_TEXT].Write(config, "Style/Text/");
  m_styles[TS_CODE_VARIABLE].Write(config, "Style/CodeHighlighting/Variable/");
  m_styles[TS_CODE_FUNCTION].Write(config, "Style/CodeHighlighting/Function/");
  m_styles[TS_CODE_COMMENT].Write(config, "Style/CodeHighlighting/Comment/");
  m_styles[TS_CODE_NUMBER].Write(config, "Style/CodeHighlighting/Number/");
  m_styles[TS_CODE_STRING].Write(config, "Style/CodeHighlighting/String/");
  m_styles[TS_CODE_OPERATOR].Write(config, "Style/CodeHighlighting/Operator/");
  m_styles[TS_CODE_LISP].Write(config, "Style/CodeHighlighting/Lisp/");
  m_styles[TS_CODE_ENDOFLINE].Write(config, "Style/CodeHighlighting/EndOfLine/");
  m_styles[TS_HEADING6].Write(config, "Style/Heading6/");
  m_styles[TS_HEADING5].Write(config, "Style/Heading5/");
  m_styles[TS_SUBSUBSECTION].Write(config, "Style/Subsubsection/");
  m_styles[TS_SUBSECTION].Write(config, "Style/Subsection/");
  m_styles[TS_SECTION].Write(config, "Style/Section/");
  m_styles[TS_TITLE].Write(config, "Style/Title/");
  m_styles[TS_WARNING].Write(config, "Style/Warning/");
  m_styles[TS_MAIN_PROMPT].Write(config, "Style/MainPrompt/");
  m_styles[TS_OTHER_PROMPT].Write(config, "Style/OtherPrompt/");
  m_styles[TS_LABEL].Write(config, "Style/Label/");  
  m_styles[TS_USERLABEL].Write(config, "Style/UserDefinedLabel/");
  m_styles[TS_SPECIAL_CONSTANT].Write(config, "Style/Special/");
  m_styles[TS_GREEK_CONSTANT].Write(config, "Style/Greek/");
  m_styles[TS_INPUT].Write(config, "Style/Input/");
  m_styles[TS_NUMBER].Write(config, "Style/Number/");
  m_styles[TS_STRING].Write(config, "Style/String/");
  m_styles[TS_GREEK_CONSTANT].Write(config, "Style/Greek/");
  m_styles[TS_VARIABLE].Write(config, "Style/Variable/");
  m_styles[TS_FUNCTION].Write(config, "Style/Function/");
  m_styles[TS_HIGHLIGHT].Write(config, "Style/Highlight/");  
  m_styles[TS_TEXT_BACKGROUND].Write(config, "Style/Background/");    
  m_styles[TS_DOCUMENT_BACKGROUND].Write(config, "Style/DocumentBackground/");
  m_styles[TS_ERROR].Write(config, "Style/Error/");
  m_styles[TS_CELL_BRACKET].Write(config, "Style/CellBracket/");
  m_styles[TS_ACTIVE_CELL_BRACKET].Write(config,wxT("Style/ActiveCellBracket/"));
  m_styles[TS_CURSOR].Write(config,wxT("Style/ActiveCellBracket/"));
  m_styles[TS_SELECTION].Write(config,wxT("Style/Selection/"));
  m_styles[TS_EQUALSSELECTION].Write(config,wxT("Style/EqualsSelection/"));
  m_styles[TS_OUTDATED].Write(config,wxT("Style/Outdated/"));
  if(file != wxEmptyString)
  {
    config->Flush();
    delete config;
  }
}

wxFontWeight Configuration::IsBold(int st)
{
  if (m_styles[st].Bold())
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(int st)
{
  if (m_styles[st].Italic())
    return wxFONTSTYLE_SLANT;
  return wxFONTSTYLE_NORMAL;
}

bool Configuration::IsUnderlined(int st)
{
  return m_styles[st].Underlined();
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
    return m_styles[TS_OUTDATED].Color();
  return m_styles[st].Color();
}

int Configuration::Scale_Px(double px)
{
  int retval = round(px * GetZoomFactor());
  if (retval < 1)
    retval = 1;
  return retval;
}


wxString Configuration::m_maximaLocation_override;
wxString Configuration::m_configfileLocation_override;

