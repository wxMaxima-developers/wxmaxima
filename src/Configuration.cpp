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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Configuration which serves as a fast configuration storage.
 */

#include "Configuration.h"
#include "Cell.h"
#include "Dirstructure.h"
#include "ErrorRedirector.h"
#include "FontCache.h"
#include "StringUtils.h"
#include <wx/wx.h>
#include <wx/string.h>
#include <wx/font.h>
#include <wx/config.h>
#include <wx/wfstream.h>
#include <wx/fileconf.h>

Configuration::Configuration(wxDC *dc) :
  m_dc(dc),
  m_mathJaxURL(MathJaXURL_Auto()),
  m_documentclass("article"),
  m_documentclassOptions("fleqn"),
  m_symbolPaneAdditionalChars("Øü§")
{
  SetBackgroundBrush(*wxWHITE_BRUSH);
  m_hidemultiplicationsign = true;
  m_autoSaveAsTempFile = false;
  m_inLispMode = false;
  m_htmlEquationFormat = mathJaX_TeX;
  m_autodetectMaxima = true;
  m_clipToDrawRegion = true;
  m_fontChanged = true;
  m_mathJaxURL_UseUser = false;
  m_TOCshowsSectionNumbers = false;
  m_invertBackground = false;
  m_antialiassingDC = NULL;
  m_parenthesisDrawMode = unknown;
  m_zoomFactor = 1.0; // affects returned fontsizes
  m_useSVG = false;
  m_changeAsterisk = true;
  m_workSheet = NULL;
  m_latin2greek = false;
  m_enterEvaluates = false;
  m_printScale = 1.0;
  m_forceUpdate = false;
  m_outdated = false;
  m_printing = false;
  m_TeXFonts = false;
  m_notifyIfIdle = true;
  m_fixReorderedIndices = true;
  m_showBrackets = true;
  m_printBrackets = false;
  m_hideBrackets = true;
  m_language = wxLANGUAGE_DEFAULT;
  m_lineWidth_em = 88;
  m_adjustWorksheetSizeNeeded = false;
  m_showLabelChoice = labels_prefer_user;
  m_abortOnError = true;
  m_defaultPort = 49152;
  m_maxGnuplotMegabytes = 12;
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
  m_showCodeCells = true;
  m_greekSidebar_ShowLatinLookalikes = false;
  m_greekSidebar_Show_mu = false;
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
  m_offerKnownAnswers = true;
  m_escCodes["pm"]    = wxT("\u00B1");
  m_escCodes["+/-"]   = wxT("\u00B1");
  m_escCodes["alpha"] = wxT("\u03B1");
  m_escCodes["beta"]  = wxT("\u03B2");
  m_escCodes["gamma"] = wxT("\u03B3");
  m_escCodes["delta"] = wxT("\u03B4");
  m_escCodes["epsilon"] = wxT("\u03B5");
  m_escCodes["zeta"] = wxT("\u03B6");
  m_escCodes["eta"] = wxT("\u03B7");
  m_escCodes["theta"] = wxT("\u03B8");
  m_escCodes["iota"] = wxT("\u03B9");
  m_escCodes["kappa"] = wxT("\u03BA");
  m_escCodes["lambda"] = wxT("\u03BB");
  m_escCodes["mu"] = wxT("\u03BC");
  m_escCodes["nu"] = wxT("\u03BD");
  m_escCodes["xi"] = wxT("\u03BE");
  m_escCodes["om"] = wxT("\u03BF");
  m_escCodes["omicron"] = wxT("\u03BF");
  m_escCodes["nabla"] = wxT("\u2207");
  m_escCodes["pi"] = wxT("\u03C0");
  m_escCodes["rho"] = wxT("\u03C1");
  m_escCodes["sigma"] = wxT("\u03C3");
  m_escCodes["tau"] = wxT("\u03C4");
  m_escCodes["upsilon"] = wxT("\u03C5");
  m_escCodes["phi"] = wxT("\u03C6");
  m_escCodes["chi"] = wxT("\u03C7");
  m_escCodes["psi"] = wxT("\u03C8");
  m_escCodes["omega"] = wxT("\u03C9");
  m_escCodes["Alpha"] = wxT("\u0391");
  m_escCodes["Beta"] = wxT("\u0392");
  m_escCodes["Gamma"] = wxT("\u0393");
  m_escCodes["Delta"] = wxT("\u0394");
  m_escCodes["Epsilon"] = wxT("\u0395");
  m_escCodes["Zeta"] = wxT("\u0396");
  m_escCodes["Eta"] = wxT("\u0397");
  m_escCodes["Theta"] = wxT("\u0398");
  m_escCodes["Iota"] = wxT("\u0399");
  m_escCodes["Kappa"] = wxT("\u039A");
  m_escCodes["Lambda"] = wxT("\u039B");
  m_escCodes["Mu"] = wxT("\u039C");
  m_escCodes["Nu"] = wxT("\u039D");
  m_escCodes["Xi"] = wxT("\u039E");
  m_escCodes["Omicron"] = wxT("\u039F");
  m_escCodes["Pi"] = wxT("\u03A0");
  m_escCodes["Rho"] = wxT("\u03A1");
  m_escCodes["Sigma"] = wxT("\u03A3");
  m_escCodes["Tau"] = wxT("\u03A4");
  m_escCodes["Upsilon"] = wxT("\u03A5");
  m_escCodes["Phi"] = wxT("\u03A6");
  m_escCodes["Chi"] = wxT("\u03A7");
  m_escCodes["Psi"] = wxT("\u03A8");
  m_escCodes["Omega"] = wxT("\u03A9");
  m_escCodes["Ohm"] = wxT("\u03A9");
  //////////////////////////
  m_escCodes["^2"] = wxT("\u00B2");
  m_escCodes["^3"] = wxT("\u00B3");
  m_escCodes["/2"] = wxT("\u00BD");
  m_escCodes["sq"] = wxT("\u221A");
  m_escCodes["ii"] = wxT("\u2148");
  m_escCodes["ee"] = wxT("\u2147");
  m_escCodes["hb"] = wxT("\u210F");
  m_escCodes["in"] = wxT("\u2208");
  m_escCodes["impl"] = wxT("\u21D2");
  m_escCodes["inf"] = wxT("\u221e");
  m_escCodes["empty"] = wxT("\u2205");
  m_escCodes["TB"] = wxT("\u25b6");
  m_escCodes["tb"] = wxT("\u25b8");
  m_escCodes["and"] = wxT("\u22C0");
  m_escCodes["or"] = wxT("\u22C1");
  m_escCodes["xor"] = wxT("\u22BB");
  m_escCodes["nand"] = wxT("\u22BC");
  m_escCodes["nor"] = wxT("\u22BD");
  m_escCodes["implies"] = wxT("\u21D2");
  m_escCodes["=>"] = wxT("\u21D2");
  m_escCodes["equiv"] = wxT("\u21D4");
  m_escCodes["<=>"] = wxT("\u21D4");
  m_escCodes["not"] = wxT("\u00AC");
  m_escCodes["union"] = wxT("\u22C3");
  m_escCodes["inter"] = wxT("\u22C2");
  m_escCodes["subseteq"] = wxT("\u2286");
  m_escCodes["subset"] = wxT("\u2282");
  m_escCodes["notsubseteq"] = wxT("\u2288");
  m_escCodes["notsubset"] = wxT("\u2284");
  m_escCodes["hbar"] = wxT("\u0127");
  m_escCodes["Hbar"] = wxT("\u0126");
  m_escCodes["partial"] = wxT("\u2202");
  m_escCodes["integral"] = wxT("\u222b");
  m_escCodes["approx"] = wxT("\u2245");
  m_escCodes["prop"] = wxT("\u221d");
  m_escCodes["propto"] = wxT("\u221d");
  m_escCodes["neq"] = wxT("\u2260");
  m_escCodes["!="] = wxT("\u2260");
  m_escCodes["/="] = wxT("\u2260");
  m_escCodes["#"] = wxT("\u2260");
  m_escCodes["<="] = wxT("\u2264");
  m_escCodes["leq"] = wxT("\u2264");
  m_escCodes[">="] = wxT("\u2265");
  m_escCodes["geq"] = wxT("\u2265");
  m_escCodes["ll"] = wxT("\u226A");
  m_escCodes["<<"] = wxT("\u226A");
  m_escCodes["gg"] = wxT("\u226B");
  m_escCodes[">>"] = wxT("\u226B");
  m_escCodes["qed"] = wxT("\u220E");
  m_escCodes["equiv"] = wxT("\u2263");
  m_escCodes["sum"] = wxT("\u2211");
  m_escCodes["prod"] = wxT("\u220F");
  m_escCodes["product"] = wxT("\u220F");
  m_escCodes["exists"] = wxT("\u2203");
  m_escCodes["nexists"] = wxT("\u2204");
  m_escCodes["parallel"] = wxT("\u2225");
  m_escCodes["perp"] = wxT("\u27C2");
  m_escCodes["perpendicular"] = wxT("\u27C2");
  m_escCodes["bot"] = wxT("\u27C2");
  m_escCodes["leadsto"] = wxT("\u219D");
  m_escCodes["->"] = wxT("\u2192");
  m_escCodes["-->"] = wxT("\u27F6");
  m_escCodes[" --> "] = wxT("\u27F6");

  m_parenthesisDrawMode = unknown;

  #ifdef __WXMSW__
  auto req = wxFontInfo()
               .Family(wxFONTFAMILY_MODERN)
               .FaceName(wxT("Linux Libertine O"))
               .Style(wxFONTSTYLE_NORMAL);

  wxFont font = FontCache::GetAFont(req);
  if (font.IsOk())
  {
    FontName(req.GetFaceName(), false);
    MathFontName(req.GetFaceName(), false);
  }
  else
    MathFontName({}, false);
  #endif
  MathFontSize(12);
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
  m_styles[TS_SUBSUBSECTION].Set(_("Subsubsection cell (Heading 4)"),*wxBLACK, true, false, false, 16);
  m_styles[TS_SUBSECTION].Set(_("Subsection cell (Heading 3)"),*wxBLACK, true, false, false, 16);
  m_styles[TS_SECTION].Set(_("Section cell (Heading 2)"),*wxBLACK, true, true, false, 18);
  m_styles[TS_TITLE].Set(_("Title cell (Heading 1)"),*wxBLACK, true, false, true, 24);
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
  ReadConfig();
}

wxSize Configuration::GetPPI(wxWindow *win) const
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

const wxString &Configuration::GetAutosubscript_string() const
{
  switch (m_autoSubscript)
  {
  case 0:
    return stR("nil");
  case 1:
    return stR("t");
  default:
    return stR("'all");
  }
}

void Configuration::ShowCodeCells(bool show)
{
  m_showCodeCells = show;
}

void Configuration::SetBackgroundBrush(wxBrush brush)
{
  m_BackgroundBrush = brush;
  m_tooltipBrush = brush;
  m_tooltipBrush.SetColour(wxColour(255, 255, 192, 128));
}

bool Configuration::MaximaFound(const wxString &location)
{
  if(location.IsEmpty())
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

  if(!config->Read(wxT("AutoSaveAsTempFile"), &m_autoSaveAsTempFile))
  {
    long autoSaveMinutes = 0;
    config->Read(wxT("autoSaveMinutes"), &autoSaveMinutes);
    m_autoSaveAsTempFile = (autoSaveMinutes == 0);
  }
  config->Read("language", &m_language);
  if (m_language == wxLANGUAGE_UNKNOWN)
    m_language = wxLANGUAGE_DEFAULT;

  config->Read("invertBackground", &m_invertBackground);
  config->Read("maxGnuplotMegabytes", &m_maxGnuplotMegabytes);
  config->Read("offerKnownAnswers", &m_offerKnownAnswers);
  config->Read(wxT("documentclass"), &m_documentclass);
  config->Read(wxT("documentclassoptions"), &m_documentclassOptions);
  config->Read(wxT("latin2greek"), &m_latin2greek);
  config->Read(wxT("enterEvaluates"), &m_enterEvaluates);
  config->Read(wxT("hidemultiplicationsign"), &m_hidemultiplicationsign);
  config->Read("greekSidebar_ShowLatinLookalikes", &m_greekSidebar_ShowLatinLookalikes);
  config->Read("greekSidebar_Show_mu", &m_greekSidebar_Show_mu);
  config->Read("symbolPaneAdditionalChars", &m_symbolPaneAdditionalChars);
  config->Read("parameters", &m_maximaParameters);
  
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
  config->Read("defaultPort",&m_defaultPort);
  config->Read(wxT("fixReorderedIndices"), &m_fixReorderedIndices);
  config->Read(wxT("showLength"), &m_showLength);
  config->Read(wxT("printScale"), &m_printScale);
  config->Read(wxT("useSVG"), &m_useSVG);
  config->Read(wxT("copyBitmap"), &m_copyBitmap);
  config->Read(wxT("copyMathML"), &m_copyMathML);
  config->Read(wxT("copyMathMLHTML"), &m_copyMathMLHTML);
  config->Read(wxT("copyRTF"), &m_copyRTF);
  config->Read(wxT("copySVG"), &m_copySVG );
  config->Read(wxT("copyEMF"), &m_copyEMF );
  config->Read(wxT("autodetectMaxima"), &m_autodetectMaxima);
  config->Read(wxT("maxima"), &m_maximaUserLocation);
  // Fix wrong" maxima=1" parameter in ~/.wxMaxima if upgrading from 0.7.0a
  if (m_maximaUserLocation.IsSameAs(wxT("1")))
    m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();

  m_autoIndent = true;
  config->Read(wxT("autoIndent"), &m_autoIndent);

  int showLabelChoice;
  config->Read(wxT("showLabelChoice"), &showLabelChoice);
  m_showLabelChoice = (showLabels) showLabelChoice; 

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

void Configuration::FontName(const wxString &name, bool save)
{
  if (save)
    wxConfig::Get()->Write("Style/Default/Style/Text/fontname", name);

  m_styles[TS_DEFAULT].FontName(name);
}

void Configuration::FontSize(long fontSize, bool save)
{
  (void)save;
  m_styles[TS_DEFAULT].FontSize(fontSize);

  /*
  {
    if (st == TS_TEXT || st == TS_HEADING5 || st == TS_HEADING6 || st == TS_SUBSUBSECTION || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return m_styles[st].FontSize();
    return 0;
  }
*/
}

void Configuration::MathFontName(const wxString &name, bool save)
{
  if (save)
    wxConfig::Get()->Write("Style/Math/fontname", name);

  m_styles[TS_NUMBER].FontName(name);
  m_styles[TS_VARIABLE].FontName(name);
  m_styles[TS_FUNCTION].FontName(name);
  m_styles[TS_SPECIAL_CONSTANT].FontName(name);
  m_styles[TS_STRING].FontName(name);
}

void Configuration::MathFontSize(double size)
{
  m_styles[TS_NUMBER].FontSize(size);
}

/*
 *   TS_DEFAULT             = 0,
  TS_VARIABLE            = 1,
  TS_NUMBER              = 2,
  TS_FUNCTION            = 3,
  TS_SPECIAL_CONSTANT    = 4,
  TS_GREEK_CONSTANT      = 5,
  TS_STRING              = 6,
  TS_INPUT               = 7,
  TS_MAIN_PROMPT         = 8,
  TS_OTHER_PROMPT        = 9,
  TS_LABEL               = 10,
  TS_USERLABEL           = 11,
  TS_HIGHLIGHT           = 12,
  TS_WARNING             = 13,
  TS_ERROR               = 14,
  TS_TEXT                = 15,
  TS_HEADING6            = 16,
  TS_HEADING5            = 17,
  TS_SUBSUBSECTION       = 18,
  TS_SUBSECTION          = 19,
  TS_SECTION             = 20,
  TS_TITLE               = 21,
  TS_TEXT_BACKGROUND     = 22,
  TS_DOCUMENT_BACKGROUND = 23,
  TS_CELL_BRACKET        = 24,
  TS_ACTIVE_CELL_BRACKET = 25,
  TS_CURSOR              = 26,
  TS_SELECTION           = 27,
  TS_EQUALSSELECTION     = 28,
  TS_OUTDATED            = 29,
  TS_CODE_VARIABLE       = 30,
  TS_CODE_FUNCTION       = 31,
  TS_CODE_COMMENT        = 32,
  TS_CODE_NUMBER         = 33,
  TS_CODE_STRING         = 34,
  TS_CODE_OPERATOR       = 35,
  TS_CODE_LISP           = 36,
  TS_CODE_ENDOFLINE      = 37,
*/





void Configuration::UpdateWorksheetFonts()
{
  for(int i = (TextStyle)0; i<NUMBEROFSTYLES; i++)
  {
    TextStyle style = (TextStyle) i;
    wxFont font;
    switch(style)
    {
    case TS_DEFAULT            :
    case TS_VARIABLE           : //
    case TS_NUMBER             : //
    case TS_FUNCTION           : //
    case TS_SPECIAL_CONSTANT   : //
    case TS_GREEK_CONSTANT     :
    case TS_STRING             : //
    case TS_MAIN_PROMPT        :
    case TS_OTHER_PROMPT       :
    case TS_LABEL              :
    case TS_USERLABEL          :
    case TS_HIGHLIGHT          :
    case TS_WARNING            :
    case TS_ERROR              :
    case TS_TEXT               :
    case TS_TEXT_BACKGROUND    :
    case TS_DOCUMENT_BACKGROUND:
    case TS_CELL_BRACKET       :
    case TS_ACTIVE_CELL_BRACKET:
    case TS_CURSOR             :
    case TS_SELECTION          :
    case TS_EQUALSSELECTION    :
    case TS_OUTDATED           :
      font.SetFaceName(MathFontName());
      break;
//    case TS_INPUT              :
//    case TS_HEADING6           :
//    case TS_HEADING5           :
//    case TS_SUBSUBSECTION      :
//    case TS_SUBSECTION         :
//    case TS_SECTION            :
//    case TS_TITLE              :
//    case TS_CODE_VARIABLE      :
//    case TS_CODE_FUNCTION      :
//    case TS_CODE_COMMENT       :
//    case TS_CODE_NUMBER        :
//    case TS_CODE_STRING        :
//    case TS_CODE_OPERATOR      :
//    case TS_CODE_LISP          :
//    case TS_CODE_ENDOFLINE     :
    default:
      font.SetFaceName(FontName());
      break;
    }
    if(!font.IsOk())
    {
      font.SetFaceName(wxEmptyString);
      font.SetFamily(wxFONTFAMILY_MODERN);
    }
    if ((style == TS_TITLE) ||
        (style == TS_SECTION) ||
        (style == TS_SUBSECTION) ||
        (style == TS_SUBSUBSECTION) ||
        (style == TS_HEADING5) || 
        (style == TS_HEADING6))
    {
      // Titles have a fixed font size 
#if wxCHECK_VERSION(3, 1, 2)
      font.SetFractionalPointSize(GetFontSize(style));
#else
      font.SetPointSize(GetFontSize(style));
#endif
    }
    else
    {
      // Font with maths has a dynamic font size that might be reduced for example
      // within fractions, subscripts or superscripts.
      if (
        (style != TS_MAIN_PROMPT) &&
        (style != TS_OTHER_PROMPT) &&
        (style != TS_ERROR) &&
        (style != TS_WARNING)
        )
      {
#if wxCHECK_VERSION(3, 1, 2)
        font.SetFractionalPointSize(FontSize());
#else
        font.SetPointSize(FontSize());
#endif
      }
    }
    if(IsItalic(style) != wxFONTSTYLE_NORMAL)
      font.SetStyle(wxFONTSTYLE_ITALIC);
    if(IsBold(style) != wxFONTWEIGHT_NORMAL)
      font.MakeBold();
    if(IsUnderlined(style))
      font.MakeUnderlined();
  }
}

wxFont Configuration::GetFont(TextStyle textStyle, long fontSize) const
{
  wxString fontName;
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
  long fontSize1 = Scale_Px(fontSize);

  // Ensure a sane minimum font size
  if (fontSize1 < 4)
    fontSize1 = 4;


  fontName = GetFontName(textStyle);
  
  wxFont font =
    FontCache::GetAFont(wxFontInfo(fontSize1)
                          .Family(wxFONTFAMILY_MODERN)
                          .FaceName(fontName)
                          .Italic(IsItalic(textStyle))
                          .Bold(IsBold(textStyle))
                          .Underlined(underlined));

  if (!font.IsOk())
  {
    font =
      FontCache::GetAFont(wxFontInfo(fontSize1)
                            .Family(wxFONTFAMILY_MODERN)
                            .Italic(IsItalic(textStyle))
                            .Bold(IsBold(textStyle))
                            .Underlined(underlined));
  }
  
  if (!font.IsOk())
  {
    auto req = wxFontInfo(fontSize1);
    FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
    font = FontCache::GetAFont(req);
  }

  return font;
}

wxColor Configuration::DefaultBackgroundColor()
{
  if(InvertBackground())
    return InvertColour(m_styles[TS_DOCUMENT_BACKGROUND].GetColor());
  else
    return m_styles[TS_DOCUMENT_BACKGROUND].GetColor();
}

wxColor Configuration::EditorBackgroundColor()
{
  if(InvertBackground())
    return InvertColour(m_styles[TS_TEXT_BACKGROUND].GetColor());
  else
    return m_styles[TS_TEXT_BACKGROUND].GetColor();
}

void Configuration::SetPrinting(bool printing)
{
  m_printing = printing;
  if(printing)
    m_invertBackground = false;
  else
    wxConfig::Get()->Read("invertBackground", m_invertBackground);
  if(printing)
    ClipToDrawRegion(false);
}

wxColour Configuration::InvertColour(wxColour col)
{
  return wxColour(
    255 - col.Red(),
    255 - col.Green(),
    255 - col.Blue(),
    col.Alpha());
}

long Configuration::GetLineWidth() const
{
  // The default line width is the width of the viewport minus the indentation minus
  // roughly one char
  long lineWidth = m_clientWidth - Scale_Px(GetLabelWidth() +
                                           GetCellBracketWidth() + FontSize());

  // If that was suspiciously wide we reduce the default line width again.
  if((lineWidth >= Scale_Px(double(FontSize())) * LineWidth_em()) &&
     (!m_printing))
    lineWidth = Scale_Px(double(FontSize())) * LineWidth_em();
  return lineWidth;
}

Configuration::drawMode Configuration::GetParenthesisDrawMode()
{
  if(m_parenthesisDrawMode == unknown)
  {
    m_parenthesisDrawMode = handdrawn;
    wxFont font = GetFont(TS_FUNCTION,20);
    if (CharsExistInFont(font,
                         PAREN_OPEN_TOP_UNICODE,
                         PAREN_OPEN_EXTEND_UNICODE,
                         PAREN_OPEN_BOTTOM_UNICODE)
      )
    {
      m_parenthesisDrawMode = assembled_unicode;
      return m_parenthesisDrawMode;
    }
    auto req = FontInfo::GetFor(font)
               .FaceName(wxT("Linux Libertine"));
    font = FontCache::GetAFont(req);
    if (CharsExistInFont(font,
                         PAREN_OPEN_TOP_UNICODE,
                         PAREN_OPEN_EXTEND_UNICODE,
                         PAREN_OPEN_BOTTOM_UNICODE)
      )
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont;
      return m_parenthesisDrawMode;
    }
    req.FaceName(wxT("Linux Libertine O"));
    font = FontCache::GetAFont(req);
    if (CharsExistInFont(font,
                         PAREN_OPEN_TOP_UNICODE,
                         PAREN_OPEN_EXTEND_UNICODE,
                         PAREN_OPEN_BOTTOM_UNICODE)
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
  long bytes = img1.GetWidth()*img1.GetHeight()*3;

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

bool Configuration::CharsExistInFont(wxFont font, wchar_t char1, wchar_t char2, wchar_t char3)
{
  wxString name;
  name.reserve(12);
  name << char1 << char2 << char3;
  CharsInFontMap::const_iterator it = m_charsInFontMap.find(name);
  if(it != m_charsInFontMap.end())
    return it->second;

  if(!font.IsOk())
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  // Seems like Apple didn't hold to their high standards as the maths part of this font
  // don't form nice big mathematical symbols => Blacklisting this font.
  if (font.GetFaceName() == stR("Monaco"))
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
  wxCoord width1,height1,descent1;
  GetDC()->SetFont(font);
  GetDC()->GetTextExtent(char1,&width1,&height1,&descent1);
  if((width1 < 1) || (height1-descent1 < 1))
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  wxCoord width2,height2,descent2;
  GetDC()->GetTextExtent(char2,&width2,&height2,&descent2);
  if((width2 < 1) || (height2-descent2 < 1))
  {
    m_charsInFontMap[name] = false;
    return false;
  }
  wxCoord width3,height3,descent3;
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

const wxString &Configuration::GetFontName(long type) const
{
  if (type == TS_NUMBER || type == TS_VARIABLE || type == TS_FUNCTION ||
      type == TS_SPECIAL_CONSTANT || type == TS_STRING ||
      type == TS_TITLE || type == TS_SUBSECTION || type == TS_SUBSUBSECTION ||
      type == TS_HEADING5 || type == TS_HEADING6 || type == TS_SECTION || type == TS_TEXT)
  {
    auto &retval = m_styles[type].FontName();
    if (!retval.IsEmpty())
      return retval;
  }
  return FontName();
}

const wxString &Configuration::MaximaLocation() const
{
  if(m_autodetectMaxima)
    return MaximaDefaultLocation();
  else
    return m_maximaUserLocation;
}

const wxString &Configuration::MaximaDefaultLocation()
{
  static wxString defaultLocation;
  defaultLocation = Dirstructure::Get()->MaximaDefaultLocation();
  return defaultLocation;
}

const wxString &Configuration::MathJaXURL_Auto() {
  return stR("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.6/MathJax.js?config=TeX-AMS_HTML");
}

void Configuration::ReadStyles(const wxString &file)
{
  wxString tmpStr;
  long tmpLong;

  wxConfigBase *config = NULL;
  if (file.IsEmpty())
    config = wxConfig::Get();
  else
  {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }
  
  // Font
  config->Read(wxT("Style/Default/Style/Text/fontname"), &tmpStr);
#ifdef __WXOSX_MAC__
  if (tmpStr.IsEmpty())
  {
    tmpStr = wxT("Monaco");
  }
#endif
  FontName(tmpStr, false);

  config->Read(wxT("mathfontsize"), &tmpLong);
  config->Read(wxT("Style/Math/fontname"), &tmpStr);
#ifdef __WXOSX_MAC__
  if (tmpStr.IsEmpty())
    tmpStr = wxT("Monaco");
#endif
  MathFontSize(tmpLong);
  MathFontName(tmpStr, false);
  
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
void Configuration::WriteStyles(const wxString &file)
{
  wxConfigBase *config = NULL;
  if (file.IsEmpty())
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxT("wxMaxima"), wxEmptyString, file);

  // Font
  config->Write("Style/Default/Style/Text/fontname", FontName());
  config->Write(wxT("mathfontsize"), MathFontSize());
  config->Write("Style/Math/fontname", MathFontName());
  
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

wxFontWeight Configuration::IsBold(long st) const
{
  if (m_styles[st].Bold())
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(long st) const
{
  if (m_styles[st].Italic())
    return wxFONTSTYLE_ITALIC;
  return wxFONTSTYLE_NORMAL;
}

const wxString &Configuration::GetSymbolFontName() const
{
#if defined __WXMSW__
  return stR("Symbol");
#else
  return m_fontName;
#endif
}

wxColour Configuration::GetColor(TextStyle style)
{
  wxColour col = m_styles[style].GetColor();
  if (m_outdated)
    col = m_styles[TS_OUTDATED].Color();

  if(InvertBackground() &&
     (style != TS_TEXT_BACKGROUND) &&
     (style != TS_DOCUMENT_BACKGROUND))
    col = MakeColorDifferFromBackground(col);
  return col;
}

long Configuration::Scale_Px(double px) const
{
  long retval = round(px * GetZoomFactor());
  if (retval < 1)
    retval = 1;
  return retval;
}

wxColor Configuration::MakeColorDifferFromBackground(wxColor color)
{
  int newBrightness = 255 - (color.Red() + color.Green() + color.Blue()) / 3;
  if(color == DefaultBackgroundColor())
  {
    return InvertColour(color);
  }
  else
  {
    int maxOldCol = wxMax(wxMax(color.Red(), color.Green()), color.Blue());
    return wxColour(
      newBrightness * color.Red() / maxOldCol,
      newBrightness * color.Green() / maxOldCol,
      newBrightness * color.Blue() / maxOldCol
      );
  }
}

wxString Configuration::m_maximaLocation_override;
wxString Configuration::m_configfileLocation_override;
