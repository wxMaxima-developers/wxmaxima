// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class Configuration which serves as a fast configuration
  storage.
*/

#include "Configuration.h"

#include "Cell.h"
#include "TextStyle.h"
#include "Dirstructure.h"
#include "ErrorRedirector.h"
#include "StringUtils.h"
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/font.h>
#include <wx/mimetype.h>
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/txtstrm.h>
#ifdef USE_WEBVIEW
#include <wx/webview.h>
#ifdef __WXMSW__
#include <wx/msw/webview_ie.h>
#endif
#endif
#include <wx/wfstream.h>
#include <wx/wx.h>
#include <wx/xml/xml.h>
#include <algorithm>
#include <limits>

Configuration::Configuration(wxDC *dc, InitOpt options) :
  m_eng{m_rd()},
  m_dc(dc)
{

  wxConfigBase *config = wxConfig::Get();
  std::uniform_int_distribution<long> urd(std::numeric_limits<long>::min(), std::numeric_limits<long>::max());
  m_configId = urd(m_eng);
  if(!config->Read(wxS("configID"), &m_configId))
    config->Write(wxS("configID"), m_configId);

  // We want to read the zoom factor, but don't want it to be updated on each ReadConfig()
  config->Read(wxS("ZoomFactor"), &m_zoomFactor);

  if(m_styleNames.empty())
    {
      m_styleNames[TS_VARIABLE            ] = _("Output: Variable names");
      m_styleNames[TS_OPERATOR            ] = _("Output: Operators");
      m_styleNames[TS_NUMBER              ] = _("Output: Numbers and Digits");
      m_styleNames[TS_FUNCTION            ] = _("Output: Function names");
      m_styleNames[TS_SPECIAL_CONSTANT    ] = _("Output: Special constants (%e,%i)");
      m_styleNames[TS_GREEK_CONSTANT      ] = _("Output: Latin names as greek symbols");
      m_styleNames[TS_STRING              ] = _("Output: Strings");
      m_styleNames[TS_CODE_DEFAULT        ] = _("Code Default (Maxima input)");
      m_styleNames[TS_MAIN_PROMPT         ] = _("Input labels");
      m_styleNames[TS_OTHER_PROMPT        ] = _("Maxima question labels");
      m_styleNames[TS_LABEL               ] = _("Output labels");
      m_styleNames[TS_USERLABEL           ] = _("User-defined labels");
      m_styleNames[TS_HIGHLIGHT           ] = _("Highlight (box)");
      m_styleNames[TS_WARNING             ] = _("Maxima warnings");
      m_styleNames[TS_ERROR               ] = _("Maxima errors");
      m_styleNames[TS_ASCIIMATHS          ] = _("ASCII maths");
      m_styleNames[TS_TEXT                ] = _("Standard Text");
      m_styleNames[TS_HEADING6            ] = _("Heading 6");
      m_styleNames[TS_HEADING5            ] = _("Heading 5");
      m_styleNames[TS_SUBSUBSECTION       ] = _("Subsubsection cell (Heading 4)");
      m_styleNames[TS_SUBSECTION          ] = _("Subsection cell (Heading 3)");
      m_styleNames[TS_SECTION             ] = _("Section cell (Heading 2)");
      m_styleNames[TS_TITLE               ] = _("Title cell (Heading 1)");
      m_styleNames[TS_TEXT_BACKGROUND     ] = _("Text cell background");
      m_styleNames[TS_DOCUMENT_BACKGROUND ] = _("Document background");
      m_styleNames[TS_CELL_BRACKET        ] = _("Cell bracket fill, Inactive");
      m_styleNames[TS_ACTIVE_CELL_BRACKET ] = _("Cell bracket fill, Active");
      m_styleNames[TS_CURSOR              ] = _("Cursor color");
      m_styleNames[TS_SELECTION           ] = _("Selection color");
      m_styleNames[TS_EQUALSSELECTION     ] = _("Color of text equal to selection");
      m_styleNames[TS_OUTDATED            ] = _("Color of Outdated cells");
      m_styleNames[TS_CODE_VARIABLE       ] = _("Code highlighting: Variables");
      m_styleNames[TS_CODE_FUNCTION       ] = _("Code highlighting: Functions");
      m_styleNames[TS_CODE_COMMENT        ] = _("Code highlighting: Comments");
      m_styleNames[TS_CODE_NUMBER         ] = _("Code highlighting: Numbers");
      m_styleNames[TS_CODE_STRING         ] = _("Code highlighting: Strings");
      m_styleNames[TS_CODE_OPERATOR       ] = _("Code highlighting: Operators");
      m_styleNames[TS_CODE_LISP           ] = _("Code highlighting: Lisp");
      m_styleNames[TS_CODE_ENDOFLINE      ] = _("Code highlighting: End of line markers");
      m_styleNames[TS_MATH                ] = _("Math Default");
    }
  if(m_codeStyles.empty())
    {
      m_codeStyles.push_back(TS_CODE_VARIABLE);
      m_codeStyles.push_back(TS_CODE_FUNCTION);
      m_codeStyles.push_back(TS_CODE_COMMENT);
      m_codeStyles.push_back(TS_CODE_NUMBER);
      m_codeStyles.push_back(TS_CODE_STRING);
      m_codeStyles.push_back(TS_CODE_OPERATOR);
      m_codeStyles.push_back(TS_CODE_LISP);
      m_codeStyles.push_back(TS_CODE_ENDOFLINE);
      m_codeStyles.push_back(TS_EQUALSSELECTION);
    }
  if(m_2dMathStyles.empty())
    {
      m_2dMathStyles.push_back(TS_VARIABLE);
      m_2dMathStyles.push_back(TS_OPERATOR);
      m_2dMathStyles.push_back(TS_NUMBER);
      m_2dMathStyles.push_back(TS_FUNCTION);
      m_2dMathStyles.push_back(TS_SPECIAL_CONSTANT);
      m_2dMathStyles.push_back(TS_GREEK_CONSTANT);
      m_2dMathStyles.push_back(TS_STRING);
      m_2dMathStyles.push_back(TS_MAIN_PROMPT);
      m_2dMathStyles.push_back(TS_OTHER_PROMPT);
      m_2dMathStyles.push_back(TS_LABEL);
      m_2dMathStyles.push_back(TS_USERLABEL);
      m_2dMathStyles.push_back(TS_HIGHLIGHT);
      m_2dMathStyles.push_back(TS_WARNING);
      m_2dMathStyles.push_back(TS_ERROR);
      m_2dMathStyles.push_back(TS_ASCIIMATHS);
      m_2dMathStyles.push_back(TS_TEXT);
    }
  if(m_colorOnlyStyles.empty())
    {
      m_colorOnlyStyles.push_back(TS_TEXT_BACKGROUND);
      m_colorOnlyStyles.push_back(TS_DOCUMENT_BACKGROUND);
      m_colorOnlyStyles.push_back(TS_CELL_BRACKET);
      m_colorOnlyStyles.push_back(TS_ACTIVE_CELL_BRACKET);
      m_colorOnlyStyles.push_back(TS_CURSOR);
      m_colorOnlyStyles.push_back(TS_SELECTION);
      m_colorOnlyStyles.push_back(TS_EQUALSSELECTION);
      m_colorOnlyStyles.push_back(TS_OUTDATED);
    }
  m_maximaOperators[wxS("(")] = 1;
  m_maximaOperators[wxS("/")] = 1;
  m_maximaOperators[wxS("{")] = 1;
  m_maximaOperators[wxS("-")] = 1;
  m_maximaOperators[wxS("^")] = 1;
  m_maximaOperators[wxS("#")] = 1;
  m_maximaOperators[wxS("=")] = 1;
  m_maximaOperators[wxS(":")] = 1;
  m_maximaOperators[wxS("[")] = 1;
  m_maximaOperators[wxS("'")] = 1;
  m_maximaOperators[wxS("!")] = 1;
  m_maximaOperators[wxS("+")] = 1;
  m_maximaOperators[wxS("*")] = 1;
  m_maximaOperators[wxS("or")] = 1;
  m_maximaOperators[wxS("and")] = 1;
  m_maximaOperators[wxS("do_in")] = 1;
  m_maximaOperators[wxS(">")] = 1;
  m_maximaOperators[wxS("$SUBVAR")] = 1;
  m_maximaOperators[wxS("<")] = 1;
  m_maximaOperators[wxS("if")] = 1;
  m_maximaOperators[wxS("::=")] = 1;
  m_maximaOperators[wxS("::")] = 1;
  m_maximaOperators[wxS("@")] = 1;
  m_maximaOperators[wxS(".")] = 1;
  m_maximaOperators[wxS("-->")] = 1;
  m_maximaOperators[wxS("^^")] = 1;
  m_maximaOperators[wxS("not")] = 1;
  m_maximaOperators[wxS("<=")] = 1;
  m_maximaOperators[wxS(":=")] = 1;
  m_maximaOperators[wxS(">=")] = 1;
  m_maximaOperators[wxS("$BFLOAT")] = 1;
  m_maximaOperators[wxS("do")] = 1;
  m_printing = false;
  m_clipToDrawRegion = true;
  m_inLispMode = false;
  m_forceUpdate = false;
  m_outdated = false;
  m_lineWidth_em = 88;
  m_workSheet = NULL;
  SetBackgroundBrush(*wxWHITE_BRUSH);
  ResetAllToDefaults(options);
  ReadConfig();
  wxString operators(wxS("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b"
                         "\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}"));
  for (wxString::const_iterator it = operators.begin(); it != operators.end();
       ++it)
    m_maximaOperators[wxString(*it)] = 1;
}

void Configuration::SetWorkSheet(wxWindow *workSheet)
{
  m_workSheet = workSheet;
  if(workSheet)
    {
      m_worksheetDC = std::unique_ptr<wxClientDC>(new wxClientDC(workSheet));
      m_dc = m_worksheetDC.get();
    }
  else
    {
      if(m_dc == m_worksheetDC.get())
	m_dc = NULL;
      m_worksheetDC.reset();
    }
}

wxSize Configuration::GetPPI() const {
  wxSize ppi;
  if(GetRecalcDC())
    {
      if(GetRecalcDC()->IsOk())
	ppi = GetRecalcDC()->GetPPI();
    }
#if wxCHECK_VERSION(3, 1, 1)
  if((ppi.x < 10 ) || (ppi.y < 10 ))
    {
      if (GetWorkSheet()) {
	int display_idx = wxDisplay::GetFromWindow(GetWorkSheet());
	if (display_idx >= 0)
	  ppi = wxDisplay(display_idx).GetPPI();
      }
    }
#endif
  if((ppi.x < 10 ) || (ppi.y < 10 ))
    ppi = wxSize(96, 96);
  return ppi;
}

void Configuration::ResetAllToDefaults(InitOpt options) {
  m_printMargin_Top = 10;
  m_printMargin_Bot = 10;
  m_printMargin_Left = 10;
  m_printMargin_Right = 10;

  m_wizardTab = 0;
  for (const auto &i : m_renderableChars)
    m_renderableChars[i.first] = wxEmptyString;
  for (const auto &i : m_nonRenderableChars)
    m_nonRenderableChars[i.first] = wxEmptyString;
  m_showAllDigits = false;
  m_lineBreaksInLongNums = false;
  m_autoSaveMinutes = 3;
  m_numpadEnterEvaluates = true;
  m_saveImgFileName = false;
  m_maximaEnvVars.clear();
  // Tell gnuplot not to wait for <enter> every few lines
#ifndef __WXMSW__
  m_maximaEnvVars[wxS("PAGER")] = wxS("cat");
#endif
  m_wrapLatexMath = true;
  m_allowNetworkHelp = false;
  m_exportContainsWXMX = true;
  m_maximaUsesHhtmlBrowser = true;
  m_maximaUsesWxmaximaBrowser = OfferInternalHelpBrowser();
  m_bitmapScale = 3;
  m_maxClipbrd_BitmapMegabytes = 4;
  m_defaultFramerate = 12;
  m_tocDepth = 255;
  m_fixedFontTC = false;
  m_hideMarkerForThisMessage.clear();
#ifdef __WXOSX__
  m_usepngCairo = false;
#else
  m_usepngCairo = true;
#endif
  m_wxMathML_Filename = wxEmptyString;

  m_mathJaxURL =
    wxS("https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js");
  m_usePartialForDiff = false, m_documentclass = wxS("article");
  m_documentclassOptions = wxS("fleqn");
  m_incrementalSearch = true;
  m_symbolPaneAdditionalChars = wxS("Øü§");
  m_hidemultiplicationsign = true;
  m_autodetectHelpBrowser = true;
  m_singlePageManual = false;
  m_useInternalHelpBrowser = OfferInternalHelpBrowser();
#ifdef __WXGTK__
  m_helpBrowserUserLocation = wxS("xdg-open");
#else
  // see https://docs.wxwidgets.org/3.0/classwx_mime_types_manager.html
  auto *manager = wxTheMimeTypesManager;
  wxFileType *filetype = manager->GetFileTypeFromExtension("html");
  m_helpBrowserUserLocation = filetype->GetOpenCommand({});
#endif

  m_TeXExponentsAfterSubscript = false;
  m_saveUntitled = true;
  m_cursorJump = true;
  m_autoSaveAsTempFile = false;
  m_htmlEquationFormat = mathJaX_TeX;
  m_autodetectMaxima = true;
  m_mathJaxURL_UseUser = false;
  m_TOCshowsSectionNumbers = false;
  m_invertBackground = false;
  m_undoLimit = 0;
  m_recentItems = 10;
  m_parenthesisDrawMode = ascii;
  m_zoomFactor = 1.0; // affects returned fontsizes
  m_useSVG = false;
  m_changeAsterisk = true;
  m_latin2greek = false;
  m_enterEvaluates = false;
  m_printScale = 1.0;
  m_notifyIfIdle = true;
  m_fixReorderedIndices = true;
  m_showBrackets = true;
  m_printBrackets = false;
  m_hideBrackets = true;
  m_defaultPlotWidth = 1200;
  m_defaultPlotHeight = 900;
  SetLanguage(wxLANGUAGE_DEFAULT);
  m_showLabelChoice = labels_prefer_user;
  m_abortOnError = true;
  m_defaultPort = 49152;
  m_maxGnuplotMegabytes = 12;
  m_indentMaths = true;
  m_indent = -1;
  m_autoSubscript = 2;
  m_showCodeCells = true;
  m_greekSidebar_ShowLatinLookalikes = false;
  m_greekSidebar_Show_mu = false;
  m_copyBitmap = false; // Otherwise MS Office, OpenOffice and LibreOffice
                        // prefer the bitmap
  // to Mathml and RTF. Also mail programs prefer bitmaps to text - which is
  // counter-productive for maxima-discuss.
  m_copyMathML = true;
  m_copyMathMLHTML = false;
  m_copyRTF = true;
  m_copySVG = true;
  m_copyEMF = false;
  m_showLength = 2;
  m_useUnicodeMaths = true;
  m_offerKnownAnswers = true;
  m_parenthesisDrawMode = ascii;
  m_autoWrap = 3;
  m_displayedDigits = 100;
  m_autoIndent = true;
  m_restartOnReEvaluation = true;
  m_matchParens = true;
  m_showMatchingParens = true;
  m_insertAns = false;
  m_openHCaret = false;
  m_labelWidth = 4;
  m_zoomFactor = 1.0;
  m_keepPercent = true;
  InitStyles();
}

static const Configuration::EscCodeContainer &EscCodes() {
  static const Configuration::EscCodeContainer escCodes{
    {wxS("pm"), wxS("\u00B1")},
    {wxS("+/-"), wxS("\u00B1")},
    {wxS("alpha"), wxS("\u03B1")},
    {wxS("beta"), wxS("\u03B2")},
    {wxS("gamma"), wxS("\u03B3")},
    {wxS("delta"), wxS("\u03B4")},
    {wxS("epsilon"), wxS("\u03B5")},
    {wxS("zeta"), wxS("\u03B6")},
    {wxS("eta"), wxS("\u03B7")},
    {wxS("theta"), wxS("\u03B8")},
    {wxS("iota"), wxS("\u03B9")},
    {wxS("kappa"), wxS("\u03BA")},
    {wxS("lambda"), wxS("\u03BB")},
    {wxS("mu"), wxS("\u03BC")},
    {wxS("nu"), wxS("\u03BD")},
    {wxS("xi"), wxS("\u03BE")},
    {wxS("om"), wxS("\u03BF")},
    {wxS("omicron"), wxS("\u03BF")},
    {wxS("nabla"), wxS("\u2207")},
    {wxS("pi"), wxS("\u03C0")},
    {wxS("rho"), wxS("\u03C1")},
    {wxS("sigma"), wxS("\u03C3")},
    {wxS("tau"), wxS("\u03C4")},
    {wxS("upsilon"), wxS("\u03C5")},
    {wxS("phi"), wxS("\u03C6")},
    {wxS("chi"), wxS("\u03C7")},
    {wxS("psi"), wxS("\u03C8")},
    {wxS("omega"), wxS("\u03C9")},
    {wxS("Alpha"), wxS("\u0391")},
    {wxS("Beta"), wxS("\u0392")},
    {wxS("Gamma"), wxS("\u0393")},
    {wxS("Delta"), wxS("\u0394")},
    {wxS("Epsilon"), wxS("\u0395")},
    {wxS("Zeta"), wxS("\u0396")},
    {wxS("Eta"), wxS("\u0397")},
    {wxS("Theta"), wxS("\u0398")},
    {wxS("Iota"), wxS("\u0399")},
    {wxS("Kappa"), wxS("\u039A")},
    {wxS("Lambda"), wxS("\u039B")},
    {wxS("Mu"), wxS("\u039C")},
    {wxS("Nu"), wxS("\u039D")},
    {wxS("Xi"), wxS("\u039E")},
    {wxS("Omicron"), wxS("\u039F")},
    {wxS("Pi"), wxS("\u03A0")},
    {wxS("Rho"), wxS("\u03A1")},
    {wxS("Sigma"), wxS("\u03A3")},
    {wxS("Tau"), wxS("\u03A4")},
    {wxS("Upsilon"), wxS("\u03A5")},
    {wxS("Phi"), wxS("\u03A6")},
    {wxS("Chi"), wxS("\u03A7")},
    {wxS("Psi"), wxS("\u03A8")},
    {wxS("Omega"), wxS("\u03A9")},
    {wxS("Ohm"), wxS("\u03A9")},
    //////////////////////////
    {wxS("^2"), wxS("\u00B2")},
    {wxS("^3"), wxS("\u00B3")},
    {wxS("/2"), wxS("\u00BD")},
    {wxS("sq"), wxS("\u221A")},
    {wxS("ii"), wxS("\u2148")},
    {wxS("ee"), wxS("\u2147")},
    {wxS("hb"), wxS("\u210F")},
    {wxS("in"), wxS("\u2208")},
    {wxS("impl"), wxS("\u21D2")},
    {wxS("inf"), wxS("\u221e")},
    {wxS("empty"), wxS("\u2205")},
    {wxS("TB"), wxS("\u25b6")},
    {wxS("tb"), wxS("\u25b8")},
    {wxS("and"), wxS("\u22C0")},
    {wxS("or"), wxS("\u22C1")},
    {wxS("xor"), wxS("\u22BB")},
    {wxS("nand"), wxS("\u22BC")},
    {wxS("nor"), wxS("\u22BD")},
    {wxS("implies"), wxS("\u21D2")},
    {wxS("=>"), wxS("\u21D2")},
    {wxS("<=>"), wxS("\u21D4")},
    {wxS("not"), wxS("\u00AC")},
    {wxS("union"), wxS("\u22C3")},
    {wxS("inter"), wxS("\u22C2")},
    {wxS("subseteq"), wxS("\u2286")},
    {wxS("subset"), wxS("\u2282")},
    {wxS("notsubseteq"), wxS("\u2288")},
    {wxS("notsubset"), wxS("\u2284")},
    {wxS("hbar"), wxS("\u0127")},
    {wxS("Hbar"), wxS("\u0126")},
    {wxS("partial"), wxS("\u2202")},
    {wxS("integral"), wxS("\u222b")},
    {wxS("approx"), wxS("\u2245")},
    {wxS("prop"), wxS("\u221d")},
    {wxS("propto"), wxS("\u221d")},
    {wxS("neq"), wxS("\u2260")},
    {wxS("!="), wxS("\u2260")},
    {wxS("/="), wxS("\u2260")},
    {wxS("#"), wxS("\u2260")},
    {wxS("<="), wxS("\u2264")},
    {wxS("leq"), wxS("\u2264")},
    {wxS(">="), wxS("\u2265")},
    {wxS("geq"), wxS("\u2265")},
    {wxS("ll"), wxS("\u226A")},
    {wxS("<<"), wxS("\u226A")},
    {wxS("gg"), wxS("\u226B")},
    {wxS(">>"), wxS("\u226B")},
    {wxS("qed"), wxS("\u220E")},
    {wxS("equiv"), wxS("\u2263")},
    {wxS("sum"), wxS("\u2211")},
    {wxS("prod"), wxS("\u220F")},
    {wxS("product"), wxS("\u220F")},
    {wxS("exists"), wxS("\u2203")},
    {wxS("nexists"), wxS("\u2204")},
    {wxS("parallel"), wxS("\u2225")},
    {wxS("perp"), wxS("\u27C2")},
    {wxS("perpendicular"), wxS("\u27C2")},
    {wxS("bot"), wxS("\u27C2")},
    {wxS("leadsto"), wxS("\u219D")},
    {wxS("->"), wxS("\u2192")},
    {wxS("-->"), wxS("\u27F6")},
    {wxS(" --> "), wxS("\u27F6")},
  };
  return escCodes;
}

void Configuration::InitStyles() {
  m_showInputLabels = true;
  std::fill(std::begin(m_styles), std::end(m_styles), Style{});

  Style defaultStyle;

  // TODO It's a fat chance that this font actually will be monospace.
  wxFont monospace(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL,
                   wxFONTWEIGHT_NORMAL);
  m_styles[TS_ASCIIMATHS].SetFontName(monospace.GetFaceName());
  m_styles[TS_ASCIIMATHS].FontSize(12.0);
  m_styles[TS_TEXT].SetFontName(monospace.GetFaceName());
  m_styles[TS_TEXT].FontSize(12.0);
  m_styles[TS_MATH].FontSize(12.0);

  m_styles[TS_TEXT].FontSize(12);
  m_styles[TS_CODE_VARIABLE].Color(0, 128, 0).Slant();
  m_styles[TS_CODE_FUNCTION].Color(128, 0, 0).Slant();
  m_styles[TS_CODE_COMMENT].Color(64, 64, 64).Slant();
  m_styles[TS_CODE_NUMBER].Color(128, 64, 0).Slant();
  m_styles[TS_CODE_STRING].Color(0, 0, 128).Slant();
  m_styles[TS_CODE_OPERATOR].Slant();
  m_styles[TS_CODE_LISP].Color(255, 0, 128).Slant();
  m_styles[TS_CODE_ENDOFLINE].Color(128, 128, 128).Slant();
  m_styles[TS_GREEK_CONSTANT].Slant();
  m_styles[TS_HEADING6].Bold().FontSize(14);
  m_styles[TS_HEADING5].Bold().FontSize(15);
  m_styles[TS_SUBSUBSECTION].Bold().FontSize(16);
  m_styles[TS_SUBSECTION].Bold().FontSize(16);
  m_styles[TS_SECTION].Bold().Slant().FontSize(18);
  m_styles[TS_TITLE].Bold().Underlined().FontSize(24);
  m_styles[TS_WARNING].Color(wxS("orange")).Bold().FontSize(12);
  m_styles[TS_ERROR].Color(*wxRED).FontSize(12);
  m_styles[TS_MAIN_PROMPT].Color(255, 128, 128);
  m_styles[TS_OTHER_PROMPT].Color(*wxRED).Slant();
  m_styles[TS_LABEL].Color(255, 192, 128);
  m_styles[TS_USERLABEL].Color(255, 64, 0);
  // m_styles[TS_SPECIAL_CONSTANT];
  m_styles[TS_CODE_DEFAULT].Bold().Slant().FontSize(12);
  // m_styles[TS_NUMBER];
  m_styles[TS_STRING].Slant();
  // m_styles[TS_GREEK_CONSTANT];
  m_styles[TS_VARIABLE].Slant();
  // m_styles[TS_FUNCTION];
  m_styles[TS_HIGHLIGHT].Color(*wxRED);
  m_styles[TS_TEXT_BACKGROUND].Color(*wxWHITE);
  m_styles[TS_DOCUMENT_BACKGROUND].Color(*wxWHITE);
  // m_styles[TS_CELL_BRACKET];
  m_styles[TS_ACTIVE_CELL_BRACKET].Color(*wxRED);
  // m_styles[TS_CURSOR];
  m_styles[TS_SELECTION].Color(wxSYS_COLOUR_HIGHLIGHT);
  m_styles[TS_EQUALSSELECTION]
    .Color(wxSYS_COLOUR_HIGHLIGHT)
    .ChangeLightness(150);
  m_styles[TS_OUTDATED].Color(153, 153, 153);
}

const wxString &Configuration::GetEscCode(const wxString &key) {
  auto &escCodes = EscCodes();
  auto it = escCodes.find(key);
  if (it != escCodes.end())
    return it->second;
  return wxm::emptyString;
}

Configuration::EscCodeIterator Configuration::EscCodesBegin() {
  return EscCodes().cbegin();
}
Configuration::EscCodeIterator Configuration::EscCodesEnd() {
  return EscCodes().cend();
}

wxString Configuration::GetAutosubscript_string() const {
  switch (m_autoSubscript) {
  case 0:
    return "nil";
  case 1:
    return "t";
  default:
    return "'all";
  }
}

void Configuration::ShowCodeCells(bool show) { m_showCodeCells = show; }

void Configuration::SetBackgroundBrush(wxBrush brush) {
  m_BackgroundBrush = brush;
  m_tooltipBrush = brush;
  m_tooltipBrush.SetColour(wxColour(255, 255, 192, 128));
}

bool Configuration::MaximaFound(wxString location) {
  if (location == wxEmptyString)
    return false;

  bool maximaFound = false;
  if (wxFileExists(location))
    maximaFound = true;

  // Find a maxima within an application package.
  if (wxFileExists(location + wxS("/Contents/Resources/maxima.sh")))
    maximaFound = true;

  // Don't complain if PATH doesn't yield a result.
  SuppressErrorDialogs logNull;

  if (!(location.EndsWith("/") || location.EndsWith("\\"))) {
    wxPathList pathlist;
    pathlist.AddEnvList(wxS("PATH"));
    wxString path = pathlist.FindAbsoluteValidPath(location);
    if (!path.empty())
      maximaFound = true;
  }
  return maximaFound;
}

void Configuration::ReadConfig() {
  wxConfigBase *config = wxConfig::Get();
  
  config->Read(wxS("configID"), &m_configId);

  wxString str;
  long dummy;
  config->SetPath("/renderability/good");
  bool bCont = config->GetFirstEntry(str, dummy);
  while (bCont) {
    wxString chars;
    config->Read(str, &chars);
    m_renderableChars[str] = chars;
    bCont = config->GetNextEntry(str, dummy);
  }
  config->SetPath("/renderability/bad");
  bCont = config->GetFirstEntry(str, dummy);
  while (bCont) {
    wxString chars;
    config->Read(str, &chars);
    m_nonRenderableChars[str] = chars;
    bCont = config->GetNextEntry(str, dummy);
  }
  config->SetPath("/");

  {
    // If this preference cannot be loaded we don't want an error message about
    // it
    SuppressErrorDialogs suppressor;
    wxString hideMessagesConfigString;
    config->Read(wxS("Print/Margin/Top"), &m_printMargin_Top);
    config->Read(wxS("Print/Margin/Bot"), &m_printMargin_Bot);
    config->Read(wxS("Print/Margin/Left"), &m_printMargin_Left);
    config->Read(wxS("Print/Margin/Right"), &m_printMargin_Right);
    config->Read(wxS("showAllDigits"), &m_showAllDigits);
    config->Read(wxS("lineBreaksInLongNums"), &m_lineBreaksInLongNums);
    config->Read(wxS("autoSaveMinutes"), &m_autoSaveMinutes);
    config->Read(wxS("wrapLatexMath"), &m_wrapLatexMath);
    config->Read(wxS("allowNetworkHelp"), &m_allowNetworkHelp);
    config->Read(wxS("exportContainsWXMX"), &m_exportContainsWXMX);
    config->Read(wxS("maximaUsesHhtmlBrowser"), &m_maximaUsesHhtmlBrowser);
    config->Read(wxS("maximaUsesWxmaximaBrowser"),
                 &m_maximaUsesWxmaximaBrowser);
    config->Read(wxS("texPreamble"), &m_texPreamble);
    {
      config->Read(wxS("suppressYellowMarkerMessages"),
                   &hideMessagesConfigString);
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(hideMessagesConfigString);
      wxMemoryInputStream istream(
				  ostream.GetOutputStreamBuffer()->GetBufferStart(),
				  ostream.GetOutputStreamBuffer()->GetBufferSize());
      wxXmlDocument xmlDocument;
      if ((!hideMessagesConfigString.IsEmpty()) &&
          (xmlDocument.Load(istream))) {
        wxXmlNode *headNode = xmlDocument.GetDocumentNode();
        if (headNode) {
          headNode = headNode->GetChildren();
          while ((headNode) && (headNode->GetName() != wxS("markers")))
            headNode = headNode->GetNext();
	  if(headNode)
	    {
	      wxXmlNode *entry = headNode->GetChildren();
	      while (entry) {
		if (entry->GetName() == wxS("hide")) {
		  wxXmlNode *node = entry->GetChildren();
		  if (node) {
		    HideMarkerForThisMessage(node->GetContent(), true);
		  }
		}
		entry = entry->GetNext();
	      }
	    }
        }
      }
    }
    {
      wxString maximaEnvironmentString;
      config->Read(wxS("maximaEnvironment"), &maximaEnvironmentString);
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(maximaEnvironmentString);
      wxMemoryInputStream istream(
				  ostream.GetOutputStreamBuffer()->GetBufferStart(),
				  ostream.GetOutputStreamBuffer()->GetBufferSize());
      wxXmlDocument xmlDocument;
      if ((!maximaEnvironmentString.IsEmpty()) && (xmlDocument.Load(istream))) {
        wxXmlNode *headNode = xmlDocument.GetDocumentNode();
        if (headNode) {
          headNode = headNode->GetChildren();
          while (headNode) {
            if (headNode->GetName() == wxS("entries")) {
              m_maximaEnvVars.clear();
              wxXmlNode *entryNode = headNode->GetChildren();
              while (entryNode) {
                if (entryNode->GetName() == wxS("entry")) {
                  wxXmlNode *entry = entryNode->GetChildren();
                  wxString var;
                  wxString value;
                  while (entry) {
                    if (entry->GetName() == wxS("var")) {
                      wxXmlNode *node = entry->GetChildren();
                      while (node) {
                        if (node->GetType() == wxXML_TEXT_NODE)
                          var = node->GetContent();
                        node = node->GetNext();
                      }
                    }
                    if (entry->GetName() == wxS("value")) {
                      wxXmlNode *node = entry->GetChildren();
                      while (node) {
                        if (node->GetType() == wxXML_TEXT_NODE)
                          value = node->GetContent();
                        node = node->GetNext();
                      }
                    }
                    entry = entry->GetNext();
                  }
                  if (!var.IsEmpty())
                    m_maximaEnvVars[var] = value;
                }
                entryNode = entryNode->GetNext();
              }
            }
            headNode = headNode->GetNext();
          }
        }
      }
    }
  }
  config->Read(wxS("maxClipbrd_BitmapMegabytes"),
               &m_maxClipbrd_BitmapMegabytes);
  if (m_maxClipbrd_BitmapMegabytes < 0)
    m_maxClipbrd_BitmapMegabytes = 1;
  config->Read(wxS("wizardTab"), &m_wizardTab);
  config->Read(wxS("numpadEnterEvaluates"), &m_numpadEnterEvaluates);
  config->Read(wxS("saveImgFileName"), &m_saveImgFileName);
  config->Read(wxS("usePartialForDiff"), &m_usePartialForDiff);
  config->Read(wxS("TeXExponentsAfterSubscript"),
               &m_TeXExponentsAfterSubscript);
  config->Read(wxS("defaultPlotWidth"), &m_defaultPlotWidth);
  config->Read(wxS("defaultPlotHeight"), &m_defaultPlotHeight);
  config->Read(wxS("fixedFontTC"), &m_fixedFontTC);
  config->Read(wxS("usepngCairo"), &m_usepngCairo);

  if (!config->Read(wxS("AutoSaveAsTempFile"), &m_autoSaveAsTempFile)) {
    long autoSaveMinutes = 3;
    config->Read(wxS("autoSaveMinutes"), &autoSaveMinutes);
    m_autoSaveAsTempFile = (autoSaveMinutes == 0);
  }
  config->Read("language", &m_language);
  config->Read("incrementalSearch", &m_incrementalSearch);
  if (m_language == wxLANGUAGE_UNKNOWN)
    m_language = wxLANGUAGE_DEFAULT;

  config->Read("invertBackground", &m_invertBackground);
  config->Read("undoLimit", &m_undoLimit);
  config->Read("recentItems", &m_recentItems);
  config->Read("maxGnuplotMegabytes", &m_maxGnuplotMegabytes);
  config->Read("offerKnownAnswers", &m_offerKnownAnswers);
  config->Read(wxS("documentclass"), &m_documentclass);
  config->Read(wxS("documentclassoptions"), &m_documentclassOptions);
  config->Read(wxS("latin2greek"), &m_latin2greek);
  config->Read(wxS("enterEvaluates"), &m_enterEvaluates);
  config->Read(wxS("hidemultiplicationsign"), &m_hidemultiplicationsign);
  config->Read("greekSidebar_ShowLatinLookalikes",
               &m_greekSidebar_ShowLatinLookalikes);
  config->Read("greekSidebar_Show_mu", &m_greekSidebar_Show_mu);
  config->Read("symbolPaneAdditionalChars", &m_symbolPaneAdditionalChars);
  config->Read("parameters", &m_maximaParameters);
  config->Read("autodetectHelpBrowser", &m_autodetectHelpBrowser);
  config->Read(wxS("useInternalHelpBrowser"), &m_useInternalHelpBrowser);
  config->Read(wxS("singlePageManual"), &m_singlePageManual);
  config->Read("helpBrowser", &m_helpBrowserUserLocation);
  {
    int tmp = static_cast<int>(m_htmlEquationFormat);
    config->Read("HTMLequationFormat", &tmp);
    if(tmp < 0)
      tmp = mathJaX_TeX;
    if(tmp > html_export_invalidChoice)
      tmp = svg;
    m_htmlEquationFormat = static_cast<Configuration::htmlExportFormat>(tmp);
  }

  config->Read(wxS("TOCshowsSectionNumbers"), &m_TOCshowsSectionNumbers);
  config->Read(wxS("autoWrapMode"), &m_autoWrap);
  config->Read(wxS("mathJaxURL_UseUser"), &m_mathJaxURL_UseUser);
  config->Read(wxS("useUnicodeMaths"), &m_useUnicodeMaths);
  config->Read(wxS("mathJaxURL"), &m_mathJaxURL);
  config->Read(wxS("autosubscript"), &m_autoSubscript);
  if(m_autoSubscript < 0)
    m_autoSubscript = 0;
  if(m_autoSubscript > 2)
    m_autoSubscript = 2;
  config->Read(wxS("indentMaths"), &m_indentMaths);
  config->Read(wxS("abortOnError"), &m_abortOnError);
  config->Read("defaultPort", &m_defaultPort);
  config->Read(wxS("fixReorderedIndices"), &m_fixReorderedIndices);
  config->Read(wxS("showLength"), &m_showLength);
  if(m_showLength < 0)
    m_showLength = 0;
  if(m_showLength > 3)
    m_showLength = 3;
  config->Read(wxS("printScale"), &m_printScale);
  config->Read(wxS("useSVG"), &m_useSVG);
  config->Read(wxS("copyBitmap"), &m_copyBitmap);
  config->Read(wxS("bitmapScale"), &m_bitmapScale);
  config->Read(wxS("DefaultFramerate"), &m_defaultFramerate);
  config->Read(wxS("tocDepth"), &m_tocDepth);
  if (m_tocDepth < 1)
    m_tocDepth = 1;
  config->Read(wxS("copyBitmap"), &m_copyBitmap);
  config->Read(wxS("copyMathML"), &m_copyMathML);
  config->Read(wxS("copyMathMLHTML"), &m_copyMathMLHTML);
  config->Read(wxS("copyRTF"), &m_copyRTF);
  config->Read(wxS("copySVG"), &m_copySVG);
  config->Read(wxS("copyEMF"), &m_copyEMF);
  config->Read(wxS("autodetectMaxima"), &m_autodetectMaxima);
  config->Read(wxS("maxima"), &m_maximaUserLocation);
  // Fix wrong" maxima=1" parameter in ~/.wxMaxima if upgrading from 0.7.0a
  if (m_maximaUserLocation.IsSameAs(wxS("1")))
    m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();

  config->Read(wxS("autoIndent"), &m_autoIndent);

  long showLabelChoice = static_cast<long>(m_showLabelChoice);
  if(static_cast<long>(m_showLabelChoice) < 0)
    m_showLabelChoice = labels_automatic;
  if(m_showLabelChoice >= labels_invalidSelection)
    m_showLabelChoice = labels_none;
  config->Read(wxS("showLabelChoice"), &showLabelChoice);
  m_showLabelChoice = (showLabels)showLabelChoice;

  config->Read(wxS("changeAsterisk"), &m_changeAsterisk);

  config->Read(wxS("notifyIfIdle"), &m_notifyIfIdle);

  config->Read(wxS("hideBrackets"), &m_hideBrackets);

  config->Read(wxS("displayedDigits"), &m_displayedDigits);
  if (m_displayedDigits <= 20)
    m_displayedDigits = 20;

  config->Read(wxS("restartOnReEvaluation"), &m_restartOnReEvaluation);

  config->Read(wxS("matchParens"), &m_matchParens);
  config->Read(wxS("showMatchingParens"), &m_showMatchingParens);

  config->Read(wxS("insertAns"), &m_insertAns);

  config->Read(wxS("openHCaret"), &m_openHCaret);

  config->Read(wxS("labelWidth"), &m_labelWidth);

  config->Read(wxS("printBrackets"), &m_printBrackets);
  config->Read(wxS("keepPercent"), &m_keepPercent);
  config->Read(wxS("saveUntitled"), &m_saveUntitled);
  config->Read(wxS("cursorJump"), &m_cursorJump);

  ReadStyles();
}

void Configuration::LastActiveTextCtrl(wxTextCtrl *last) {
  m_lastActiveTextCtrl = last;
}

bool Configuration::HideMarkerForThisMessage(wxString message) {
  auto it = m_hideMarkerForThisMessage.find(message);
  if (it == m_hideMarkerForThisMessage.end())
    return false;
  else
    return it->second;
}

//TODO: Don't underline the section number of titles
void Configuration::MakeStylesConsistent()
{
  for(const auto &style : GetCodeStylesList())
    {
      m_styles[style].SetFamily(GetStyle(TS_CODE_DEFAULT)->GetFamily());
      m_styles[style].SetEncoding(GetStyle(TS_CODE_DEFAULT)->GetEncoding());
      m_styles[style].SetFontSize(GetStyle(TS_CODE_DEFAULT)->GetFontSize());
      m_styles[style].SetFontName(GetStyle(TS_CODE_DEFAULT)->GetFontName());
      m_styles[style].SetBold(GetStyle(TS_CODE_DEFAULT)->IsBold());
      m_styles[style].SetItalic(GetStyle(TS_CODE_DEFAULT)->IsItalic());
      m_styles[style].SetSlant(GetStyle(TS_CODE_DEFAULT)->IsSlant());
      m_styles[style].SetStrikethrough(GetStyle(TS_CODE_DEFAULT)->IsStrikethrough());
      m_styles[style].SetUnderlined(GetStyle(TS_CODE_DEFAULT)->IsUnderlined());
      m_styles[style].CantChangeFontName(true);
      m_styles[style].CantChangeFontVariant(true);
    }

  for(const auto &style : GetMathStylesList())
    {
      if((style != TS_ASCIIMATHS) && (style != TS_TEXT))
	{
	  m_styles[style].SetFontSize(GetStyle(TS_MATH)->GetFontSize());
	  m_styles[style].SetFamily(GetStyle(TS_MATH)->GetFamily());
	  m_styles[style].SetEncoding(GetStyle(TS_MATH)->GetEncoding());
	  m_styles[style].SetFontName(GetStyle(TS_MATH)->GetFontName());
	  m_styles[style].CantChangeFontName(true);
	}
    }

  for(const auto &style : GetColorOnlyStylesList())
    {
      m_styles[style].SetFamily(GetStyle(TS_CODE_DEFAULT)->GetFamily());
      m_styles[style].SetEncoding(GetStyle(TS_CODE_DEFAULT)->GetEncoding());
      m_styles[style].SetFontSize(GetStyle(TS_CODE_DEFAULT)->GetFontSize());
      m_styles[style].SetFontName(GetStyle(TS_CODE_DEFAULT)->GetFontName());
      m_styles[style].SetBold(GetStyle(TS_CODE_DEFAULT)->IsBold());
      m_styles[style].SetItalic(GetStyle(TS_CODE_DEFAULT)->IsItalic());
      m_styles[style].SetUnderlined(GetStyle(TS_CODE_DEFAULT)->IsUnderlined());
      m_styles[style].SetSlant(GetStyle(TS_CODE_DEFAULT)->IsSlant());
      m_styles[style].SetStrikethrough(GetStyle(TS_CODE_DEFAULT)->IsStrikethrough());
      m_styles[style].CantChangeFontName(true);
      m_styles[style].CantChangeFontVariant(true);
    }
}

bool Configuration::StyleAffectsCode(TextStyle style) const
{
  bool retval = false;
  for(const auto &i : GetCodeStylesList())
    if(style == i)
      retval = true;
  return retval;
}

bool Configuration::StyleAffectsMathOut(TextStyle style) const
{
  bool retval = false;
  for(const auto &i : GetMathStylesList())
    if(style == i)
      retval = true;
  return retval;
}

bool Configuration::StyleAffectsColorOnly(TextStyle style) const
{
  bool retval = false;
  for(const auto &i : GetColorOnlyStylesList())
    if(style == i)
      retval = true;
  return retval;
}

wxColor Configuration::DefaultBackgroundColor() {
  if (InvertBackground())
    return InvertColour(m_styles[TS_DOCUMENT_BACKGROUND].GetColor());
  else
    return m_styles[TS_DOCUMENT_BACKGROUND].GetColor();
}

wxColor Configuration::EditorBackgroundColor() {
  if (InvertBackground())
    return InvertColour(m_styles[TS_TEXT_BACKGROUND].GetColor());
  else
    return m_styles[TS_TEXT_BACKGROUND].GetColor();
}

void Configuration::NotifyOfCellRedraw(const Cell *cell) {
  if(!GetDebugmode())
    return;
  if (!m_cellRedrawTrace || !cell)
    return;
  // This operation is fast and doesn't allocate after the configuration
  // was used for a few screen redraws.
  m_cellRedrawTrace->push_back(cell);
}

void Configuration::ClearAndEnableRedrawTracing() {
  if(!GetDebugmode())
    return;
  if (!m_cellRedrawTrace)
    m_cellRedrawTrace.reset(new CellRedrawTrace);
  else
    m_cellRedrawTrace->clear();
}

void Configuration::ReportMultipleRedraws() {
  if(!GetDebugmode())
    return;

  if (!m_cellRedrawTrace)
    return;

  // This sort is over two orders of magnitude faster,
  // per-cell, than having counters in a map or hash.
  std::sort(m_cellRedrawTrace->begin(), m_cellRedrawTrace->end());
  int counter = 0;
  const Cell *prev = {};
  for (auto *cell : *m_cellRedrawTrace) {
    if (prev != cell) {
      wxASSERT(counter <= 1);
      if (counter > 1)
        wxLogMessage(
		     "Bug: %i redraws in one screen refresh for a cell reading \"%s\"",
		     counter, prev->ToString().mb_str());
      prev = cell;
      counter = 1;
    } else
      ++counter;
  }
}

void Configuration::SetPrinting(bool printing) {
  m_printing = printing;
  if (printing)
    m_invertBackground = false;
  else
    wxConfig::Get()->Read("invertBackground", m_invertBackground);
  if (printing)
    ClipToDrawRegion(!printing);
}

wxColour Configuration::InvertColour(wxColour col) {
  return wxColour(255 - col.Red(), 255 - col.Green(), 255 - col.Blue(),
                  col.Alpha());
}

long Configuration::GetLineWidth() const {
  // The default line width is the width of the viewport minus the indentation
  // minus roughly one char
  long lineWidth =
    m_clientWidth -
    Scale_Px(GetLabelWidth() + GetCellBracketWidth() + GetDefaultFontSize());

  // If that was suspiciously wide we reduce the default line width again.
  if ((lineWidth >= Scale_Px(GetDefaultFontSize()) * LineWidth_em()) &&
      (!m_printing))
    lineWidth = Scale_Px(GetDefaultFontSize()) * LineWidth_em();
  return lineWidth;
}

//! A comparison operator for wxImage
static bool operator==(const wxImage &a, const wxImage &b) {
  if (a.GetSize() != b.GetSize())
    return false;

  long bytes = (long)a.GetWidth() * b.GetHeight() * 3;
  if (bytes < 0)
    return false;

  return memcmp(a.GetData(), b.GetData(), bytes) == 0;
}

void Configuration::SetZoomFactor(double newzoom) {
  if(m_zoomFactor == newzoom)
    return;

  for (auto &i: m_styles)
    i.ClearCache();
  if (newzoom > GetMaxZoomFactor())
    newzoom = GetMaxZoomFactor();
  if (newzoom < GetMinZoomFactor())
    newzoom = GetMinZoomFactor();

  m_zoomFactor = newzoom;
}

Configuration::~Configuration() {
  WriteStyles();
  WriteSettings();
}

bool Configuration::CharsExistInFont(const wxFont &font,
                                     const wxString &chars) {
  wxASSERT(!chars.empty());
  for (auto const &ex : m_charsInFont)
    // cppcheck-suppress useStlAlgorithm
    if (ex.chars == chars)
      return ex.exist;

  auto const cache = [this, &chars](bool result) {
    m_charsInFont.emplace_back(chars, result);
    return result;
  };

  if (!font.IsOk())
    return cache(false);

  // Seems like Apple didn't hold to their high standards as the maths part of
  // this font don't form nice big mathematical symbols => Blacklisting this
  // font.
  if (font.GetFaceName() == wxS("Monaco"))
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
  GetRecalcDC()->SetFont(font);
  for (auto &p : P) {
    wxCoord descent;
    GetRecalcDC()->GetTextExtent(p.ch, &p.size.x, &p.size.y, &descent);
    if ((p.size.x < 1) || ((p.size.y - descent) < 1))
      return cache(false);
  }

  bool allDifferentSizes = true;
  for (auto i = P.begin(); allDifferentSizes && i != P.end(); ++i)
    for (auto j = i + 1; allDifferentSizes && j != P.end(); ++j)
      allDifferentSizes &= i->size != j->size;

  if (allDifferentSizes)
    return cache(true);

  for (auto &p : P) {
    wxBitmap bmp(p.size);
    wxMemoryDC dc(bmp);
    dc.SetFont(font);
    dc.Clear();
    dc.DrawText(p.ch, wxPoint(0, 0));
    p.image = bmp.ConvertToImage();
  }

  for (auto i = P.begin(); i != P.end(); ++i)
    for (auto j = i + 1; j != P.end(); ++j)
      if (i->image == j->image)
        return cache(false);

  return cache(true);
}

wxString Configuration::GetFontName(TextStyle const ts) const {
  wxString retval;
  retval = m_styles[ts].GetFontName();
  return retval;
}

wxString Configuration::MaximaLocation() const {
  if (m_autodetectMaxima)
    return MaximaDefaultLocation();
  else
    return m_maximaUserLocation;
}

wxString Configuration::MaximaDefaultLocation() {
  return Dirstructure::Get()->MaximaDefaultLocation();
}

void Configuration::ReadStyles(const wxString &file) {
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }


  // Read legacy defaults for the math font name and size
  long tmpLong;
  if (config->Read(wxS("mathfontsize"), &tmpLong) && tmpLong > 1)
    m_styles[TS_MATH].SetFontSize(AFontSize(tmpLong));
  config->Read(wxS("showInputLabels"), &m_showInputLabels);
  wxString tmpString;
  if (config->Read(wxS("Style/Math/fontname"), &tmpString) &&
      tmpString.size() > 1)
    m_styles[TS_MATH].SetFontName(tmpString);

  m_styles[TS_MATH].Read(config, "Style/Math/");
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
  m_styles[TS_CODE_DEFAULT].Read(config, "Style/Default/");
  m_styles[TS_NUMBER].Read(config, "Style/Number/");
  m_styles[TS_STRING].Read(config, "Style/String/");
  m_styles[TS_ASCIIMATHS].Read(config, "Style/ASCIImaths/");
  m_styles[TS_VARIABLE].Read(config, "Style/Variable/");
  m_styles[TS_OPERATOR].Read(config, "Style/Operator/");
  m_styles[TS_FUNCTION].Read(config, "Style/Function/");
  m_styles[TS_HIGHLIGHT].Read(config, "Style/Highlight/");
  m_styles[TS_TEXT_BACKGROUND].Read(config, "Style/Background/");
  m_styles[TS_DOCUMENT_BACKGROUND].Read(config, "Style/DocumentBackground/");
  m_styles[TS_ERROR].Read(config, "Style/Error/");
  m_styles[TS_CELL_BRACKET].Read(config, "Style/CellBracket/");
  m_styles[TS_ACTIVE_CELL_BRACKET].Read(config,
                                        wxS("Style/ActiveCellBracket/"));
  m_styles[TS_CURSOR].Read(config, wxS("Style/ActiveCellBracket/"));
  m_styles[TS_SELECTION].Read(config, wxS("Style/Selection/"));
  m_styles[TS_EQUALSSELECTION].Read(config, wxS("Style/EqualsSelection/"));
  m_styles[TS_OUTDATED].Read(config, wxS("Style/Outdated/"));
  m_BackgroundBrush = *wxTheBrushList->FindOrCreateBrush(
							 m_styles[TS_DOCUMENT_BACKGROUND].GetColor(), wxBRUSHSTYLE_SOLID);
  MakeStylesConsistent();
}

//! Saves the settings to a file.
void Configuration::WriteSettings(const wxString &file) {
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxS("wxMaxima"), wxEmptyString, file);

  {
    wxXmlNode *topNode =
      new wxXmlNode(NULL, wxXML_DOCUMENT_NODE, wxEmptyString, wxEmptyString);
    wxXmlNode *entriesNode =
      new wxXmlNode(topNode, wxXML_ELEMENT_NODE, "entries");
    wxEnvVariableHashMap::const_iterator it;
    for (it = m_maximaEnvVars.begin(); it != m_maximaEnvVars.end(); ++it) {
      if (!it->first.IsEmpty()) {
        wxXmlNode *entryNode =
	  new wxXmlNode(entriesNode, wxXML_ELEMENT_NODE, "entry");
        wxXmlNode *varNode =
	  new wxXmlNode(entryNode, wxXML_ELEMENT_NODE, "var");
        wxXmlNode *valueNode =
	  new wxXmlNode(entryNode, wxXML_ELEMENT_NODE, "value");
        new wxXmlNode(varNode, wxXML_TEXT_NODE, wxEmptyString, it->first);
        new wxXmlNode(valueNode, wxXML_TEXT_NODE, wxEmptyString, it->second);
      }
    }
    wxXmlDocument xmlDoc;
    xmlDoc.SetDocumentNode(topNode);
    // Write the string into a memory buffer
    wxMemoryOutputStream ostream;
    xmlDoc.Save(ostream);
    wxMemoryInputStream istream(
				ostream.GetOutputStreamBuffer()->GetBufferStart(),
				ostream.GetOutputStreamBuffer()->GetBufferSize());
    wxTextInputStream text(istream);
    wxString maximaEnvConfigString;
    while (!istream.Eof())
      maximaEnvConfigString += text.ReadLine() + wxS("\n");
    config->Write(wxS("maximaEnvironment"), maximaEnvConfigString);
  }
  {
    wxXmlNode *topNode =
      new wxXmlNode(NULL, wxXML_DOCUMENT_NODE, wxEmptyString, wxEmptyString);
    wxXmlNode *headNode = new wxXmlNode(topNode, wxXML_ELEMENT_NODE,
                                        wxS("markers"), wxEmptyString);
    StringBoolHash::const_iterator it;
    for (it = m_hideMarkerForThisMessage.begin();
         it != m_hideMarkerForThisMessage.end(); ++it) {
      if (it->second) {
        wxXmlNode *hideNode =
	  new wxXmlNode(headNode, wxXML_ELEMENT_NODE, "hide");
        new wxXmlNode(hideNode, wxXML_TEXT_NODE, wxEmptyString, it->first);
      }
    }
    wxXmlDocument xmlDoc;
    xmlDoc.SetDocumentNode(topNode);
    // Write the string into a memory buffer
    wxMemoryOutputStream ostream;
    xmlDoc.Save(ostream);
    wxMemoryInputStream istream(
				ostream.GetOutputStreamBuffer()->GetBufferStart(),
				ostream.GetOutputStreamBuffer()->GetBufferSize());
    wxTextInputStream text(istream);
    wxString hideMessagesConfigString;
    while (!istream.Eof())
      hideMessagesConfigString += text.ReadLine() + wxS("\n");
    config->Write(wxS("suppressYellowMarkerMessages"),
                  hideMessagesConfigString);
  }

  config->Write(wxS("Print/Margin/Top"), m_printMargin_Top);
  config->Write(wxS("Print/Margin/Bot"), m_printMargin_Bot);
  config->Write(wxS("Print/Margin/Left"), m_printMargin_Left);
  config->Write(wxS("Print/Margin/Right"), m_printMargin_Right);
  config->Write(wxS("showAllDigits"), m_showAllDigits);
  config->Write(wxS("lineBreaksInLongNums"), m_lineBreaksInLongNums);
  config->Write(wxS("keepPercent"), m_keepPercent);
  config->Write(wxS("labelWidth"), m_labelWidth);
  config->Write(wxS("saveUntitled"), m_saveUntitled);
  config->Write(wxS("cursorJump"), m_cursorJump);
  config->Write(wxS("autoSaveMinutes"), m_autoSaveMinutes);

  config->Write(wxS("maxClipbrd_BitmapMegabytes"),
                m_maxClipbrd_BitmapMegabytes);

  WriteStyles(config);
  for (const auto &i : m_renderableChars)
    config->Write(wxS("renderability/good/") + i.first, i.second);
  for (const auto &i : m_nonRenderableChars)
    config->Write(wxS("renderability/bad/") + i.first, i.second);
  if (file != wxEmptyString) {
    config->Flush();
    delete config;
  }
}

wxFontWeight Configuration::IsBold(long st) const {
  if (m_styles[st].IsBold())
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(long st) const {
  if (m_styles[st].IsItalic())
    return wxFONTSTYLE_ITALIC;
  return wxFONTSTYLE_NORMAL;
}

wxString Configuration::GetSymbolFontName() const {
  return m_styles[TS_CODE_DEFAULT].GetFontName();
}

wxColour Configuration::GetColor(TextStyle style) {
  wxColour col = m_styles[style].GetColor();
  if (m_outdated)
    col = m_styles[TS_OUTDATED].GetColor();

  if (InvertBackground() && (style != TS_TEXT_BACKGROUND) &&
      (style != TS_DOCUMENT_BACKGROUND))
    col = MakeColorDifferFromBackground(col);
  return col;
}

long Configuration::Scale_Px(double px) const {
  long retval = lround(px * GetZoomFactor());
  return std::max(retval, 1l);
}

AFontSize Configuration::Scale_Px(AFontSize size) const {
  auto retval = size.Get() * GetZoomFactor();
  return AFontSize(retval);
}

wxColor Configuration::MakeColorDifferFromBackground(wxColor color) {
  int newBrightness = 255 - (color.Red() + color.Green() + color.Blue()) / 3;
  if (color == DefaultBackgroundColor()) {
    return InvertColour(color);
  } else {
    int maxOldCol = wxMax(wxMax(color.Red(), color.Green()), color.Blue());
    return wxColour(newBrightness * color.Red() / maxOldCol,
                    newBrightness * color.Green() / maxOldCol,
                    newBrightness * color.Blue() / maxOldCol);
  }
}

Configuration::FileToSave Configuration::PopFileToSave()
{
  FileToSave retval(m_filesToSave.back());
  m_filesToSave.pop_back();
  return retval;
}

bool Configuration::InUpdateRegion(wxRect const rect) const {
  if (!ClipToDrawRegion())
    return true;

  wxRect const updateRegion = GetUpdateRegion();

  return updateRegion.Intersects(rect);
}

bool Configuration::FontRendersChar(wxChar ch, const wxFont &font) {
  wxString fontName = font.GetNativeFontInfoDesc();
  fontName.Replace("/", "_");
  if (m_renderableChars[fontName].Contains(ch))
    return true;
  if (m_nonRenderableChars[fontName].Contains(ch))
    return false;

  bool retval = FontDisplaysChar(ch, font) &&
    CharVisiblyDifferent(ch, wxS('\1'), font) &&
    CharVisiblyDifferent(ch, L'\uF299', font) &&
    CharVisiblyDifferent(ch, L'\uF000', font);

  if (retval)
    m_renderableChars[fontName] += wxString(ch);
  else
    m_nonRenderableChars[fontName] += wxString(ch);

  return retval;
}

bool Configuration::FontDisplaysChar(wxChar ch, const wxFont &font) {
  int width = 200;
  int height = 200;

  // Prepare two identical device contexts that create identical bitmaps
  wxBitmap characterBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxBitmap referenceBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxMemoryDC characterDC;
  wxMemoryDC referenceDC;
  characterDC.SetFont(font);
  referenceDC.SetFont(font);
  characterDC.SelectObject(characterBitmap);
  referenceDC.SelectObject(referenceBitmap);
  characterDC.SetBrush(*wxWHITE_BRUSH);
  referenceDC.SetBrush(*wxWHITE_BRUSH);
  characterDC.DrawRectangle(0, 0, 200, 200);
  referenceDC.DrawRectangle(0, 0, 200, 200);
  characterDC.SetPen(*wxBLACK_PEN);
  referenceDC.SetPen(*wxBLACK_PEN);

  // Now draw the character our button shows into one of these bitmaps and see
  // if that changed any aspect of the bitmap
  characterDC.DrawText(wxString(ch), 100, 100);
  wxImage characterImage = characterBitmap.ConvertToImage();
  wxImage referenceImage = referenceBitmap.ConvertToImage();
  for (int x = 0; x < width; x++)
    for (int y = 0; y < height; y++) {
      if (characterImage.GetRed(x, y) != referenceImage.GetRed(x, y))
        return true;
      if (characterImage.GetGreen(x, y) != referenceImage.GetGreen(x, y))
        return true;
      if (characterImage.GetBlue(x, y) != referenceImage.GetBlue(x, y))
        return true;
    }
  wxLogMessage(wxS("Char '%s' seems not to be displayed."), wxString(ch).mb_str());

  // characterImage.SaveFile(wxString(m_char)+".png");

  return false;
}

bool Configuration::CharVisiblyDifferent(wxChar ch, wxChar otherChar,
                                         const wxFont &font) {
  int width = 200;
  int height = 200;

  // Prepare two identical device contexts that create identical bitmaps
  wxBitmap characterBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxBitmap referenceBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxMemoryDC characterDC;
  wxMemoryDC referenceDC;
  characterDC.SetFont(font);
  referenceDC.SetFont(font);
  characterDC.SelectObject(characterBitmap);
  referenceDC.SelectObject(referenceBitmap);
  characterDC.SetBrush(*wxWHITE_BRUSH);
  referenceDC.SetBrush(*wxWHITE_BRUSH);
  characterDC.DrawRectangle(0, 0, 200, 200);
  referenceDC.DrawRectangle(0, 0, 200, 200);
  characterDC.SetPen(*wxBLACK_PEN);
  referenceDC.SetPen(*wxBLACK_PEN);
  characterDC.DrawText(wxString(ch), 100, 100);
  referenceDC.DrawText(wxString(otherChar), 100, 100);
  wxImage characterImage = characterBitmap.ConvertToImage();
  wxImage referenceImage = referenceBitmap.ConvertToImage();
  for (int x = 0; x < width; x++)
    for (int y = 0; y < height; y++) {
      if (characterImage.GetRed(x, y) != referenceImage.GetRed(x, y))
        return true;
      if (characterImage.GetGreen(x, y) != referenceImage.GetGreen(x, y))
        return true;
      if (characterImage.GetBlue(x, y) != referenceImage.GetBlue(x, y))
        return true;
    }
  wxLogMessage(wxS("Char '%s' looks identical to '%s'."),
	       wxString(ch).mb_str(),
	       wxString(otherChar).mb_str());
  return false;
}

bool Configuration::OfferInternalHelpBrowser() const {
#ifdef USE_WEBVIEW
#ifdef __WINDOWS__
#if wxCHECK_VERSION(3, 1, 5)
  return wxWebView::IsBackendAvailable(wxWebViewBackendEdge);
#else
  return false;
#endif
#else
  return true;
#endif
#else
  return false;
#endif
}

bool Configuration::UpdateNeeded() const
{
  long configId;
  wxConfig::Get()->Read(wxS("configID"), &configId);

  return m_configId != configId;
}
  
void Configuration::WriteStyles(wxConfigBase *config) {
  std::uniform_int_distribution<long> urd(std::numeric_limits<long>::min(), std::numeric_limits<long>::max());
  m_configId = urd(m_eng);
  config->Write(wxS("configID"), m_configId);
  config->Write(wxS("showInputLabels"), m_showInputLabels);
  config->Write(wxS("wrapLatexMath"), m_wrapLatexMath);
  config->Write(wxS("allowNetworkHelp"), m_allowNetworkHelp);
  config->Write(wxS("exportContainsWXMX"), m_exportContainsWXMX);
  config->Write(wxS("maximaUsesHhtmlBrowser"), m_maximaUsesHhtmlBrowser);
  if (OfferInternalHelpBrowser())
    config->Write(wxS("maximaUsesWxmaximaBrowser"),
                  m_maximaUsesWxmaximaBrowser);
  config->Write(wxS("texPreamble"), m_texPreamble);
  config->Write(wxS("wizardTab"), m_wizardTab);
  config->Write(wxS("numpadEnterEvaluates"), m_numpadEnterEvaluates);
  config->Write(wxS("saveImgFileName"), m_saveImgFileName);
  config->Write(wxS("usePartialForDiff"), m_usePartialForDiff);
  config->Write(wxS("TeXExponentsAfterSubscript"),
                m_TeXExponentsAfterSubscript);
  config->Write(wxS("defaultPlotWidth"), m_defaultPlotWidth);
  config->Write(wxS("defaultPlotHeight"), m_defaultPlotHeight);
  config->Write(wxS("fixedFontTC"), m_fixedFontTC);
  config->Write(wxS("bitmapScale"), m_bitmapScale);
  config->Write(wxS("DefaultFramerate"), m_defaultFramerate);
  config->Write(wxS("tocDepth"), m_tocDepth);
  config->Write(wxS("usepngCairo"), m_usepngCairo);
  config->Write("incrementalSearch", m_incrementalSearch);
  config->Write(wxS("hideBrackets"), m_hideBrackets);
  config->Write(wxS("printScale"), m_printScale);
  config->Write(wxS("AutoSaveAsTempFile"), m_autoSaveAsTempFile);
  config->Write(wxS("autoWrapMode"), m_autoWrap);
  config->Write(wxS("autoIndent"), m_autoIndent);
  config->Write(wxS("indentMaths"), m_indentMaths);
  config->Write(wxS("matchParens"), m_matchParens);
  config->Write(wxS("showMatchingParens"), m_showMatchingParens);
  config->Write(wxS("changeAsterisk"), m_changeAsterisk);
  config->Write(wxS("hidemultiplicationsign"), m_hidemultiplicationsign);
  config->Write(wxS("latin2greek"), m_latin2greek);
  config->Write(wxS("greekSidebar_ShowLatinLookalikes"),
                m_greekSidebar_ShowLatinLookalikes);
  config->Write(wxS("greekSidebar_Show_mu"), m_greekSidebar_Show_mu);
  config->Write(wxS("symbolPaneAdditionalChars"), m_symbolPaneAdditionalChars);
  config->Write(wxS("notifyIfIdle"), m_notifyIfIdle);
  config->Write(wxS("displayedDigits"), m_displayedDigits);
  config->Write(wxS("insertAns"), m_insertAns);
  config->Write(wxS("openHCaret"), m_openHCaret);
  config->Write(wxS("restartOnReEvaluation"), m_restartOnReEvaluation);
  config->Write(wxS("invertBackground"), m_invertBackground);
  config->Write("recentItems", m_recentItems);
  config->Write(wxS("undoLimit"), m_undoLimit);
  config->Write(wxS("showLabelChoice"), static_cast<int>(m_showLabelChoice));
  config->Write(wxS("printBrackets"), m_printBrackets);
  config->Write(wxS("autodetectMaxima"), m_autodetectMaxima);
  config->Write(wxS("parameters"), m_maximaParameters);
  config->Write(wxS("maxima"), m_maximaUserLocation);
  config->Write(wxS("autodetectHelpBrowser"), m_autodetectHelpBrowser);
  if (OfferInternalHelpBrowser())
    config->Write(wxS("useInternalHelpBrowser"), m_useInternalHelpBrowser);
  config->Write(wxS("singlePageManual"), m_singlePageManual);
  config->Write(wxS("helpBrowser"), m_helpBrowserUserLocation);
  config->Write(wxS("fixReorderedIndices"), m_fixReorderedIndices);
  config->Write(wxS("mathJaxURL_UseUser"), m_mathJaxURL_UseUser);
  config->Write(wxS("enterEvaluates"), m_enterEvaluates);
  config->Write(wxS("mathJaxURL"), m_mathJaxURL);
  config->Write(wxS("copyBitmap"), m_copyBitmap);
  config->Write(wxS("copyMathML"), m_copyMathML);
  config->Write(wxS("copyMathMLHTML"), m_copyMathMLHTML);
  config->Write(wxS("copyRTF"), m_copyRTF);
  config->Write(wxS("copySVG"), m_copySVG);
  config->Write(wxS("copyEMF"), m_copyEMF);
  config->Write(wxS("useSVG"), m_useSVG);
  config->Write(wxS("showLength"), m_showLength);
  config->Write(wxS("TOCshowsSectionNumbers"), m_TOCshowsSectionNumbers);
  config->Write(wxS("useUnicodeMaths"), m_useUnicodeMaths);
  config->Write("defaultPort", m_defaultPort);
  config->Write("abortOnError", m_abortOnError);
  config->Write("language", m_language);
  config->Write("maxGnuplotMegabytes", m_maxGnuplotMegabytes);
  config->Write("offerKnownAnswers", m_offerKnownAnswers);
  config->Write("documentclass", m_documentclass);
  config->Write("documentclassoptions", m_documentclassOptions);
  config->Write("HTMLequationFormat", static_cast<int>(m_htmlEquationFormat));
  config->Write("autosubscript", m_autoSubscript);
  config->Write(wxS("ZoomFactor"), m_zoomFactor);
  // Fonts
  m_styles[TS_MATH].Write(config, "Style/Math/");
  m_styles[TS_TEXT].Write(config, "Style/Text/");
  m_styles[TS_CODE_VARIABLE].Write(config, "Style/CodeHighlighting/Variable/");
  m_styles[TS_CODE_FUNCTION].Write(config, "Style/CodeHighlighting/Function/");
  m_styles[TS_CODE_COMMENT].Write(config, "Style/CodeHighlighting/Comment/");
  m_styles[TS_CODE_NUMBER].Write(config, "Style/CodeHighlighting/Number/");
  m_styles[TS_CODE_STRING].Write(config, "Style/CodeHighlighting/String/");
  m_styles[TS_CODE_OPERATOR].Write(config, "Style/CodeHighlighting/Operator/");
  m_styles[TS_CODE_LISP].Write(config, "Style/CodeHighlighting/Lisp/");
  m_styles[TS_CODE_ENDOFLINE].Write(config,
                                    "Style/CodeHighlighting/EndOfLine/");
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
  m_styles[TS_CODE_DEFAULT].Write(config, "Style/Default/");
  m_styles[TS_NUMBER].Write(config, "Style/Number/");
  m_styles[TS_STRING].Write(config, "Style/String/");
  m_styles[TS_ASCIIMATHS].Write(config, "Style/ASCIImaths/");
  m_styles[TS_VARIABLE].Write(config, "Style/Variable/");
  m_styles[TS_OPERATOR].Write(config, "Style/Operator/");
  m_styles[TS_FUNCTION].Write(config, "Style/Function/");
  m_styles[TS_HIGHLIGHT].Write(config, "Style/Highlight/");
  m_styles[TS_TEXT_BACKGROUND].Write(config, "Style/Background/");
  m_styles[TS_DOCUMENT_BACKGROUND].Write(config, "Style/DocumentBackground/");
  m_styles[TS_ERROR].Write(config, "Style/Error/");
  m_styles[TS_CELL_BRACKET].Write(config, "Style/CellBracket/");
  m_styles[TS_ACTIVE_CELL_BRACKET].Write(config,
                                         wxS("Style/ActiveCellBracket/"));
  m_styles[TS_CURSOR].Write(config, wxS("Style/ActiveCellBracket/"));
  m_styles[TS_SELECTION].Write(config, wxS("Style/Selection/"));
  m_styles[TS_EQUALSSELECTION].Write(config, wxS("Style/EqualsSelection/"));
  m_styles[TS_OUTDATED].Write(config, wxS("Style/Outdated/"));
}

//! Saves the style settings to a file.
void Configuration::WriteStyles(const wxString &file) {
  MakeStylesConsistent();
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxS("wxMaxima"), wxEmptyString, file);
  
  WriteStyles(config);
  if (file != wxEmptyString) {
    config->Flush();
    delete config;
  }
}
const wxString &Configuration::GetStyleName(TextStyle textStyle) {
  if (textStyle >= 0 && textStyle < NUMBEROFSTYLES)
    return m_styleNames[textStyle];
  else
    return wxm::emptyString;
}

wxString Configuration::m_configfileLocation_override;
std::unordered_map<TextStyle, wxString> Configuration::m_styleNames;
bool Configuration::m_debugMode = false;
