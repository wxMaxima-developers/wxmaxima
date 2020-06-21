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
#include "Dirstructure.h"
#include "ErrorRedirector.h"
#include "FontCache.h"
#include <wx/wx.h>
#include <wx/mimetype.h>
#include <wx/string.h>
#include <wx/font.h>
#include <wx/config.h>
#include <wx/wfstream.h>
#include <wx/fileconf.h>
#include "Cell.h"

Configuration::Configuration(wxDC *dc) :
  m_dc(dc),
  m_mathJaxURL("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-AMS_HTML"),
  m_documentclass("article"),
  m_documentclassOptions("fleqn"),
  m_symbolPaneAdditionalChars("Øü§")
{
  SetBackgroundBrush(*wxWHITE_BRUSH);
  m_hidemultiplicationsign = true;
  m_autodetectHelpBrowser = true;
  #ifdef __WXGTK__
  m_helpBrowserUserLocation = wxT("xdg-open");
  #else
  wxMimeTypesManager manager;
  wxFileType * filetype = manager.GetFileTypeFromExtension("html");
  wxString m_helpBrowserUserLocation = filetype->GetOpenCommand(wxEmptyString);
  #endif

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
  m_parenthesisDrawMode = unknown;
  
  InitStyles();
}

static const Configuration::EscCodeContainer &EscCodes()
{
  static const Configuration::EscCodeContainer escCodes{
    {wxT("pm"), wxT("\u00B1")},
    {wxT("+/-"), wxT("\u00B1")},
    {wxT("alpha"), wxT("\u03B1")},
    {wxT("beta"), wxT("\u03B2")},
    {wxT("gamma"), wxT("\u03B3")},
    {wxT("delta"), wxT("\u03B4")},
    {wxT("epsilon"), wxT("\u03B5")},
    {wxT("zeta"), wxT("\u03B6")},
    {wxT("eta"), wxT("\u03B7")},
    {wxT("theta"), wxT("\u03B8")},
    {wxT("iota"), wxT("\u03B9")},
    {wxT("kappa"), wxT("\u03BA")},
    {wxT("lambda"), wxT("\u03BB")},
    {wxT("mu"), wxT("\u03BC")},
    {wxT("nu"), wxT("\u03BD")},
    {wxT("xi"), wxT("\u03BE")},
    {wxT("om"), wxT("\u03BF")},
    {wxT("omicron"), wxT("\u03BF")},
    {wxT("nabla"), wxT("\u2207")},
    {wxT("pi"), wxT("\u03C0")},
    {wxT("rho"), wxT("\u03C1")},
    {wxT("sigma"), wxT("\u03C3")},
    {wxT("tau"), wxT("\u03C4")},
    {wxT("upsilon"), wxT("\u03C5")},
    {wxT("phi"), wxT("\u03C6")},
    {wxT("chi"), wxT("\u03C7")},
    {wxT("psi"), wxT("\u03C8")},
    {wxT("omega"), wxT("\u03C9")},
    {wxT("Alpha"), wxT("\u0391")},
    {wxT("Beta"), wxT("\u0392")},
    {wxT("Gamma"), wxT("\u0393")},
    {wxT("Delta"), wxT("\u0394")},
    {wxT("Epsilon"), wxT("\u0395")},
    {wxT("Zeta"), wxT("\u0396")},
    {wxT("Eta"), wxT("\u0397")},
    {wxT("Theta"), wxT("\u0398")},
    {wxT("Iota"), wxT("\u0399")},
    {wxT("Kappa"), wxT("\u039A")},
    {wxT("Lambda"), wxT("\u039B")},
    {wxT("Mu"), wxT("\u039C")},
    {wxT("Nu"), wxT("\u039D")},
    {wxT("Xi"), wxT("\u039E")},
    {wxT("Omicron"), wxT("\u039F")},
    {wxT("Pi"), wxT("\u03A0")},
    {wxT("Rho"), wxT("\u03A1")},
    {wxT("Sigma"), wxT("\u03A3")},
    {wxT("Tau"), wxT("\u03A4")},
    {wxT("Upsilon"), wxT("\u03A5")},
    {wxT("Phi"), wxT("\u03A6")},
    {wxT("Chi"), wxT("\u03A7")},
    {wxT("Psi"), wxT("\u03A8")},
    {wxT("Omega"), wxT("\u03A9")},
    {wxT("Ohm"), wxT("\u03A9")},
    //////////////////////////
    {wxT("^2"), wxT("\u00B2")},
    {wxT("^3"), wxT("\u00B3")},
    {wxT("/2"), wxT("\u00BD")},
    {wxT("sq"), wxT("\u221A")},
    {wxT("ii"), wxT("\u2148")},
    {wxT("ee"), wxT("\u2147")},
    {wxT("hb"), wxT("\u210F")},
    {wxT("in"), wxT("\u2208")},
    {wxT("impl"), wxT("\u21D2")},
    {wxT("inf"), wxT("\u221e")},
    {wxT("empty"), wxT("\u2205")},
    {wxT("TB"), wxT("\u25b6")},
    {wxT("tb"), wxT("\u25b8")},
    {wxT("and"), wxT("\u22C0")},
    {wxT("or"), wxT("\u22C1")},
    {wxT("xor"), wxT("\u22BB")},
    {wxT("nand"), wxT("\u22BC")},
    {wxT("nor"), wxT("\u22BD")},
    {wxT("implies"), wxT("\u21D2")},
    {wxT("=>"), wxT("\u21D2")},
    {wxT("equiv"), wxT("\u21D4")},
    {wxT("<=>"), wxT("\u21D4")},
    {wxT("not"), wxT("\u00AC")},
    {wxT("union"), wxT("\u22C3")},
    {wxT("inter"), wxT("\u22C2")},
    {wxT("subseteq"), wxT("\u2286")},
    {wxT("subset"), wxT("\u2282")},
    {wxT("notsubseteq"), wxT("\u2288")},
    {wxT("notsubset"), wxT("\u2284")},
    {wxT("hbar"), wxT("\u0127")},
    {wxT("Hbar"), wxT("\u0126")},
    {wxT("partial"), wxT("\u2202")},
    {wxT("integral"), wxT("\u222b")},
    {wxT("approx"), wxT("\u2245")},
    {wxT("prop"), wxT("\u221d")},
    {wxT("propto"), wxT("\u221d")},
    {wxT("neq"), wxT("\u2260")},
    {wxT("!="), wxT("\u2260")},
    {wxT("/="), wxT("\u2260")},
    {wxT("#"), wxT("\u2260")},
    {wxT("<="), wxT("\u2264")},
    {wxT("leq"), wxT("\u2264")},
    {wxT(">="), wxT("\u2265")},
    {wxT("geq"), wxT("\u2265")},
    {wxT("ll"), wxT("\u226A")},
    {wxT("<<"), wxT("\u226A")},
    {wxT("gg"), wxT("\u226B")},
    {wxT(">>"), wxT("\u226B")},
    {wxT("qed"), wxT("\u220E")},
    {wxT("equiv"), wxT("\u2263")},
    {wxT("sum"), wxT("\u2211")},
    {wxT("prod"), wxT("\u220F")},
    {wxT("product"), wxT("\u220F")},
    {wxT("exists"), wxT("\u2203")},
    {wxT("nexists"), wxT("\u2204")},
    {wxT("parallel"), wxT("\u2225")},
    {wxT("perp"), wxT("\u27C2")},
    {wxT("perpendicular"), wxT("\u27C2")},
    {wxT("bot"), wxT("\u27C2")},
    {wxT("leadsto"), wxT("\u219D")},
    {wxT("->"), wxT("\u2192")},
    {wxT("-->"), wxT("\u27F6")},
    {wxT(" --> "), wxT("\u27F6")},
    };
  return escCodes;
}

void Configuration::InitStyles()
{
  #ifdef __WXMSW__
  auto req = wxFontInfo()
               .Family(wxFONTFAMILY_MODERN)
               .FaceName(wxT("Linux Libertine O"))
               .Style(wxFONTSTYLE_NORMAL);

  wxFont font = FontCache::GetAFont(req);
  if (font.IsOk())
  {
    m_fontName = req.GetFaceName();
    m_mathFontName = req.GetFaceName();
  }
  else
    m_mathFontName = wxEmptyString;
  #endif
  m_mathFontSize = 12;
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

const wxString &Configuration::GetEscCode(const wxString &key)
{
  static wxString empty;
  auto &escCodes = EscCodes();
  auto it = escCodes.find(key);
  if (it != escCodes.end())
    return it->second;
  return empty;
}

Configuration::EscCodeIterator Configuration::EscCodesBegin()
{ return EscCodes().cbegin(); }
Configuration::EscCodeIterator Configuration::EscCodesEnd()
{ return EscCodes().cend(); }

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

wxString Configuration::GetAutosubscript_string() const
{
  switch (m_autoSubscript)
  {
  case 0:
    return "nil";
  case 1:
    return "t";
  default:
    return "'all";
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
  config->Read("autodetectHelpBrowser", &m_autodetectHelpBrowser);
  config->Read("helpBrowser", &m_helpBrowserUserLocation);
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

void Configuration::UpdateWorksheetFonts()
{
  for(int i = (TextStyle)0; i<NUMBEROFSTYLES; i++)
  {
    TextStyle style = (TextStyle) i;
    wxFont font;
    switch(style)
    {
    case TS_DEFAULT            :
    case TS_VARIABLE           :
    case TS_NUMBER             :
    case TS_FUNCTION           :
    case TS_SPECIAL_CONSTANT   :
    case TS_GREEK_CONSTANT     :
    case TS_STRING             :
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
        font.SetFractionalPointSize(GetDefaultFontSize());
#else
        font.SetPointSize(GetDefaultFontSize());
#endif
      }
    }
    if(IsItalic(style) != wxFONTSTYLE_NORMAL)
      font.SetStyle(wxFONTSTYLE_ITALIC);
    if(IsBold(style) != wxFONTWEIGHT_NORMAL)
      font.MakeBold();
    if(IsUnderlined(style))
      font.MakeUnderlined();
    m_worksheetFonts[style] = font;
  }
}

wxFont Configuration::GetWorksheetFont(TextStyle style) const
{
  wxASSERT((style > 0) && (style < NUMBEROFSTYLES));
  wxFont font = m_worksheetFonts[style];
  
#if wxCHECK_VERSION(3, 1, 2)
  font.SetFractionalPointSize(Scale_Px(font.GetFractionalPointSize()));
#else
  font.SetPointSize(Scale_Px(font.GetPointSize()));
#endif
  return font;
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
                                           GetCellBracketWidth() + GetDefaultFontSize());

  // If that was suspiciously wide we reduce the default line width again.
  if((lineWidth >= Scale_Px(double(GetDefaultFontSize())) * LineWidth_em()) &&
     (!m_printing))
    lineWidth = Scale_Px(double(GetDefaultFontSize())) * LineWidth_em();
  return lineWidth;
}

Configuration::drawMode Configuration::GetParenthesisDrawMode()
{
  if (m_parenthesisDrawMode == unknown)
  {
    static const wxString parens{
      (wxT(PAREN_OPEN_TOP_UNICODE)
         wxT(PAREN_OPEN_EXTEND_UNICODE)
           wxT(PAREN_OPEN_BOTTOM_UNICODE))};

    m_parenthesisDrawMode = handdrawn;
    wxFont font = GetFont(TS_FUNCTION, 20);
    if (CharsExistInFont(font, parens))
    {
      m_parenthesisDrawMode = assembled_unicode;
      return m_parenthesisDrawMode;
    }
    auto req = FontInfo::GetFor(font)
               .FaceName(wxT("Linux Libertine"));
    font = FontCache::GetAFont(req);
    if (CharsExistInFont(font, parens))
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont;
      return m_parenthesisDrawMode;
    }
    req.FaceName(wxT("Linux Libertine O"));
    font = FontCache::GetAFont(req);
    if (CharsExistInFont(font, parens))
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont2;
      return m_parenthesisDrawMode;
    }
  }
  return m_parenthesisDrawMode;
}

//! A comparison operator for wxImage
static bool operator==(const wxImage &a, const wxImage &b)
{
  if (a.GetSize() != b.GetSize())
    return false;

  long bytes = a.GetWidth() * b.GetHeight() * 3;
  if (bytes < 0)
    return false;

  return memcmp(a.GetData(), b.GetData(), bytes) == 0;
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

bool Configuration::CharsExistInFont(const wxFont &font, const wxString &chars)
{
  wxASSERT(!chars.empty());
  for (auto const &ex : m_charsInFont)
    // cppcheck-suppress useStlAlgorithm
    if (ex.chars == chars)
      return ex.exist;

  auto const cache = [this, &chars](bool result)
  {
    m_charsInFont.emplace_back(chars, result);
    return result;
  };

  if (!font.IsOk())
    return cache(false);

  // Seems like Apple didn't hold to their high standards as the maths part of this font
  // don't form nice big mathematical symbols => Blacklisting this font.
  if (font.GetFaceName() == wxT("Monaco"))
    return cache(false);

  if (!m_useUnicodeMaths)
    return cache(false);
  
  struct Params {
    wxUniChar ch;
    wxSize size;
    wxImage image;
    explicit Params(wxUniChar ch) : ch(ch) {}
  };
  std::vector<Params> P(chars.begin(), chars.end());

  // Letters with width or height = 0 don't exist in the current font
  GetDC()->SetFont(font);
  for (auto &p : P)
  {
    wxCoord descent;
    GetDC()->GetTextExtent(p.ch, &p.size.x, &p.size.y, &descent);
    if ((p.size.x < 1) || ((p.size.y-descent) < 1))
      return cache(false);
  }

  bool allDifferentSizes = true;
  for (auto i = P.begin(); allDifferentSizes && i != P.end(); ++i)
    for (auto j = i+1; allDifferentSizes && j != P.end(); ++j)
      allDifferentSizes &= i->size != j->size;

  if (allDifferentSizes)
    return cache(true);

  for (auto &p : P)
  {
    wxBitmap bmp(p.size);
    wxMemoryDC dc(bmp);
    dc.SetFont(font);
    dc.Clear();
    dc.DrawText(p.ch, wxPoint(0,0));
    p.image = bmp.ConvertToImage();
  }

  for (auto i = P.begin(); i != P.end(); ++i)
    for (auto j = i+1; j != P.end(); ++j)
      if (i->image == j->image)
        return cache(false);

  return cache(true);
}

wxString Configuration::GetFontName(long type) const
{
  wxString retval = FontName();
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

wxString Configuration::MaximaLocation() const
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
  config->Read(wxT("Style/Default/Style/Text/fontname"), &m_fontName);
#ifdef __WXOSX_MAC__
  if (m_fontName.IsEmpty())
  {
    m_fontName = "Monaco";
  }
#endif

  config->Read(wxT("mathfontsize"), &m_mathFontSize);
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
    config = new wxFileConfig(wxT("wxMaxima"), wxEmptyString, file);

  // Font
  config->Write("Style/Default/Style/Text/fontname", m_fontName);
  config->Write(wxT("mathfontsize"), m_mathFontSize);
  config->Write("Style/Math/fontname", m_mathFontName);
  
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

wxString Configuration::GetSymbolFontName() const
{
#if defined __WXMSW__
  return wxT("Symbol");
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
