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

#include "BuildConfig.h"
#include "Configuration.h"

#include "cells/Cell.h"
#include "cells/TextStyle.h"
#include "Dirstructure.h"
#include "StringUtils.h"
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/font.h>
#include <wx/settings.h>
#include <wx/mimetype.h>
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/wx.h>
#include <wx/xml/xml.h>
#include <algorithm>
#include <limits>
#include <wx/webview.h>

Configuration::Configuration(wxDC *dc, InitOpt options) :
  m_initOpts(options),
  m_eng{m_rd()}
{
  m_renderContext.SetRecalcDC(dc);
  wxConfigBase *config = wxConfig::Get();
  std::uniform_int_distribution<long> urd(std::numeric_limits<long>::min(), std::numeric_limits<long>::max());
  m_configId = urd(m_eng);
  if(!config->Read(wxS("configID"), &m_configId))
    config->Write(wxS("configID"), m_configId);

  // We want to read the zoom factor, but don't want it to be updated on each ReadConfig()
  config->Read(wxS("ZoomFactor"), &m_zoomFactor);

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
  m_maximaHelpFormat = frontend;
  m_inLispMode = false;
  m_outdated = false;
  m_lineWidth_em = 88;
  m_workSheet = NULL;
  SetBackgroundBrush(*wxWHITE_BRUSH);
  ResetAllToDefaults();
  ReadConfig();
  wxString operators(wxS("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b"
                         "\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}"));
  for (wxString::const_iterator it = operators.begin(); it != operators.end();
       ++it)
    m_maximaOperators[wxString(*it)] = 1;
}

Configuration::Configuration(const Configuration &o) :
  m_maximaArch(o.m_maximaArch),
  m_lispVersion(o.m_lispVersion),
  m_lispType(o.m_lispType),
  m_maximaVersion(o.m_maximaVersion),
  m_initOpts(o.m_initOpts),
  m_configId(o.m_configId),
  m_rd(),
  m_eng(m_rd()),
  m_maximaOperators(o.m_maximaOperators),
  m_maximaEnvVars(o.m_maximaEnvVars),
  m_filesToSave(o.m_filesToSave),
  m_renderableChars(o.m_renderableChars),
  m_nonRenderableChars(o.m_nonRenderableChars),
  m_displayMode(o.m_displayMode),
  m_showInputLabels(o.m_showInputLabels),
  m_wizardTab(o.m_wizardTab),
  m_display2d_Unicode(o.m_display2d_Unicode),
  m_usePartialForDiff(o.m_usePartialForDiff),
  m_maximaUsesHhtmlBrowser(o.m_maximaUsesHhtmlBrowser),
  m_maximaUsesWxmaximaBrowser(o.m_maximaUsesWxmaximaBrowser),
  m_autoSaveAsTempFile(o.m_autoSaveAsTempFile),
  m_language(o.m_language),
  m_autodetectMaxima(o.m_autodetectMaxima),
  m_autodetectHelpBrowser(o.m_autodetectHelpBrowser),
  m_useInternalHelpBrowser(o.m_useInternalHelpBrowser),
  m_singlePageManual(o.m_singlePageManual),
  m_incrementalSearch(o.m_incrementalSearch),
  m_autoSubscript(o.m_autoSubscript),
  m_workSheet(nullptr),
  m_renderContext(o.m_renderContext),
  m_wrapLatexMath(o.m_wrapLatexMath),
  m_allowNetworkHelp(o.m_allowNetworkHelp),
  m_exportContainsWXMX(o.m_exportContainsWXMX),
  m_texPreamble(o.m_texPreamble),
  m_parenthesisDrawMode(o.m_parenthesisDrawMode),
  m_workingdir(o.m_workingdir),
  m_TeXExponentsAfterSubscript(o.m_TeXExponentsAfterSubscript),
  m_helpBrowserUserLocation(o.m_helpBrowserUserLocation),
  m_maximaUserLocation(o.m_maximaUserLocation),
  m_hideBrackets(o.m_hideBrackets),
  m_printScale(o.m_printScale),
  m_printMargin_Top(o.m_printMargin_Top),
  m_printMargin_Bot(o.m_printMargin_Bot),
  m_printMargin_Left(o.m_printMargin_Left),
  m_printMargin_Right(o.m_printMargin_Right),
  m_showBrackets(o.m_showBrackets),
  m_printBrackets(o.m_printBrackets),
  m_changeAsterisk(o.m_changeAsterisk),
  m_notifyIfIdle(o.m_notifyIfIdle),
  m_displayedDigits(o.m_displayedDigits),
  m_autoWrap(o.m_autoWrap),
  m_autoIndent(o.m_autoIndent),
  m_showAllDigits(o.m_showAllDigits),
  m_lineBreaksInLongNums(o.m_lineBreaksInLongNums),
  m_matchParens(o.m_matchParens),
  m_insertAns(o.m_insertAns),
  m_openHCaret(o.m_openHCaret),
  m_labelWidth(o.m_labelWidth),
  m_indent(o.m_indent),
  m_latin2greek(o.m_latin2greek),
  m_zoomFactor(o.m_zoomFactor),
  m_maximaShareDir(o.m_maximaShareDir),
  m_maximaDemoDir(o.m_maximaDemoDir),
  m_outdated(o.m_outdated),
  m_maximaParameters(o.m_maximaParameters),
  m_keepPercent(o.m_keepPercent),
  m_restartOnReEvaluation(o.m_restartOnReEvaluation),
  m_fontCMRI(o.m_fontCMRI),
  m_fontCMSY(o.m_fontCMSY),
  m_fontCMEX(o.m_fontCMEX),
  m_fontCMMI(o.m_fontCMMI),
  m_fontCMTI(o.m_fontCMTI),
  m_lineWidth_em(o.m_lineWidth_em),
  m_showLabelChoice(o.m_showLabelChoice),
  m_fixReorderedIndices(o.m_fixReorderedIndices),
  m_mathJaxURL(o.m_mathJaxURL),
  m_mathJaxURL_UseUser(o.m_mathJaxURL_UseUser),
  m_showCodeCells(o.m_showCodeCells),
  m_copyBitmap(o.m_copyBitmap),
  m_copyMathML(o.m_copyMathML),
  m_copyMathMLHTML(o.m_copyMathMLHTML),
  m_showLength(o.m_showLength),
  m_inLispMode(o.m_inLispMode),
  m_usepngCairo(o.m_usepngCairo),
  m_enterEvaluates(o.m_enterEvaluates),
  m_useSVG(o.m_useSVG),
  m_fixedFontTC(o.m_fixedFontTC),
  m_copyRTF(o.m_copyRTF),
  m_copySVG(o.m_copySVG),
  m_copyEMF(o.m_copyEMF),
  m_TOCshowsSectionNumbers(o.m_TOCshowsSectionNumbers),
  m_useUnicodeMaths(o.m_useUnicodeMaths),
  m_indentMaths(o.m_indentMaths),
  m_abortOnError(o.m_abortOnError),
  m_showMatchingParens(o.m_showMatchingParens),
  m_hidemultiplicationsign(o.m_hidemultiplicationsign),
  m_offerKnownAnswers(o.m_offerKnownAnswers),
  m_defaultPort(o.m_defaultPort),
  m_maxGnuplotMegabytes(o.m_maxGnuplotMegabytes),
  m_defaultPlotHeight(o.m_defaultPlotHeight),
  m_defaultPlotWidth(o.m_defaultPlotWidth),
  m_saveUntitled(o.m_saveUntitled),
  m_cursorJump(o.m_cursorJump),
  m_numpadEnterEvaluates(o.m_numpadEnterEvaluates),
  m_saveImgFileName(o.m_saveImgFileName),
  m_documentclass(o.m_documentclass),
  m_documentclassOptions(o.m_documentclassOptions),
  m_htmlEquationFormat(o.m_htmlEquationFormat),
  m_defaultBackgroundColor(o.m_defaultBackgroundColor),
  m_tooltipBrush(o.m_tooltipBrush),
  m_greekSidebar_ShowLatinLookalikes(o.m_greekSidebar_ShowLatinLookalikes),
  m_greekSidebar_Show_mu(o.m_greekSidebar_Show_mu),
  m_symbolPaneAdditionalChars(o.m_symbolPaneAdditionalChars),
  m_appearance(o.m_appearance),
  m_undoLimit(o.m_undoLimit),
  m_recentItems(o.m_recentItems),
  m_bitmapScale(o.m_bitmapScale),
  m_defaultFramerate(o.m_defaultFramerate),
  m_tocDepth(o.m_tocDepth),
  m_maxClipbrd_BitmapMegabytes(o.m_maxClipbrd_BitmapMegabytes),
  m_autoSaveMinutes(o.m_autoSaveMinutes),
  m_maxLayoutTime(o.m_maxLayoutTime),
  m_layoutStrategy(o.m_layoutStrategy),
  m_wxMathML_Filename(o.m_wxMathML_Filename),
  m_maximaHelpFormat(o.m_maximaHelpFormat),
  m_lastActiveTextCtrl(o.m_lastActiveTextCtrl),
  m_cellCfgCnt(o.m_cellCfgCnt.load())
{
#ifdef __WXMSW__
  m_useWgnuplot = o.m_useWgnuplot;
#endif
  m_styleStore = o.m_styleStore;
}


void Configuration::SetWorkSheet(wxWindow *workSheet)
{
  m_workSheet = workSheet;
  if(workSheet)
    m_renderContext.AttachWorksheetDC(workSheet);
  else
    m_renderContext.DetachWorksheetDC();
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

void Configuration::ResetAllToDefaults() {
  RecalculateForce();
  m_display2d_Unicode = true;
  m_printMargin_Top = 10;
  m_printMargin_Bot = 10;
  m_printMargin_Left = 10;
  m_printMargin_Right = 10;

  m_wizardTab = 0;
  for (const auto &i : m_renderableChars)
    m_renderableChars[i.first].Clear();
  for (const auto &i : m_nonRenderableChars)
    m_nonRenderableChars[i.first].Clear();
  m_showAllDigits = false;
  m_lineBreaksInLongNums = false;
  m_autoSaveMinutes = 3;
  m_maxLayoutTime = 5;
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
  m_wxMathML_Filename.Clear();

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
  m_appearance = Appearance::followSystem;
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
  m_layoutStrategy = LayoutStrategy::layout2DIfFits;
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
  m_styleStore.SetDefaults();
  m_styleStore.SetDarkDefaults();
  m_styleStore.SetUseDark(UseDarkMode());
}

bool Configuration::SystemIsDark() {
#if wxCHECK_VERSION(3, 1, 3)
  return wxSystemSettings::GetAppearance().IsDark();
#else
  return false; // no reliable way to ask the OS on this wx version
#endif
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

void Configuration::ShowCodeCells(bool show) {
  if(m_showCodeCells != show)
    RecalculateForce();
  m_showCodeCells = show;
}

void Configuration::UpdateBackgroundBrush() {
  m_renderContext.SetBackgroundBrush(*wxTheBrushList->FindOrCreateBrush(
    m_styleStore[TS_DOCUMENT_BACKGROUND].GetColor(), wxBRUSHSTYLE_SOLID));
}

void Configuration::SetBackgroundBrush(const wxBrush &brush) {
  m_renderContext.SetBackgroundBrush(brush);
  m_tooltipBrush = brush;
  m_tooltipBrush.SetColour(wxColour(255, 255, 192, 128));
}

wxString Configuration::FindProgram(const wxString &location) {
  if (location.IsEmpty())
    return location;

  if (wxFileExists(location))
    return location;

  wxString exePath;
  #if defined __WXOSX__
  // Find the program within our application bundle.
  exePath = wxStandardPaths::Get().GetExecutablePath() + wxS("/Contents/Resources/") + location;
  if (wxFileExists(exePath))
    return exePath;
  #endif

  exePath = wxStandardPaths::Get().GetExecutablePath() + location;
  if (wxFileExists(exePath))
    return exePath;


  if (!(location.EndsWith("/") || location.EndsWith("\\"))) {
    wxPathList pathlist;
    pathlist.AddEnvList(wxS("PATH"));
    wxString path = pathlist.FindAbsoluteValidPath(location);
    if (!path.empty())
      return path;
    #if defined __WXMSW__
    // NB: the result must be assigned back to `path` -- otherwise these
    // extension-qualified PATH lookups are no-ops and e.g. maxima.bat on PATH is
    // never found (which left wxMaxima unable to locate Maxima on Windows when it
    // was not installed next to wxmaxima.exe).
    path = pathlist.FindAbsoluteValidPath(location + wxS(".exe"));
    if (!path.empty())
      return path;
    path = pathlist.FindAbsoluteValidPath(location + wxS(".bat"));
    if (!path.empty())
      return path;
    #endif
    #if defined __WXOSX__
    path = pathlist.FindAbsoluteValidPath(location + wxS(".app"));
    if (!path.empty())
      return path;
    #endif
  }
  return wxEmptyString;
}

void Configuration::ReadConfig() {
  RecalculateForce();
  wxConfigBase *config = wxConfig::Get();

  // All mechanical scalar settings share one key<->member table with
  // WriteStyles(), so the read and the write side cannot disagree on a key.
  // Keys that need extra logic (migrations, clamping, randomization) are
  // handled individually below.
  for (const auto &setting : ScalarConfigSettings())
    std::visit([&](auto member) { config->Read(setting.key, &(this->*member)); },
               setting.member);

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
    wxString hideMessagesConfigString;
    config->Read(wxS("Print/Margin/Top"), &m_printMargin_Top);
    config->Read(wxS("Print/Margin/Bot"), &m_printMargin_Bot);
    config->Read(wxS("Print/Margin/Left"), &m_printMargin_Left);
    config->Read(wxS("Print/Margin/Right"), &m_printMargin_Right);
    config->Read(wxS("showAllDigits"), &m_showAllDigits);
    config->Read(wxS("lineBreaksInLongNums"), &m_lineBreaksInLongNums);
    config->Read(wxS("autoSaveMinutes"), &m_autoSaveMinutes);
    if (m_autoSaveMinutes <= 0)
      m_autoSaveMinutes = 3;
    config->Read(wxS("MaxLayoutTime"), &m_maxLayoutTime);
    if (m_maxLayoutTime <= 0)
      m_maxLayoutTime = 5;
    config->Read(wxS("maximaUsesWxmaximaBrowser"),
                 &m_maximaUsesWxmaximaBrowser);
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
  #ifdef __WXMSW__
  config->Read("usewgnuplot", &m_useWgnuplot);
  #endif
  config->Read(wxS("TeXExponentsAfterSubscript"),
               &m_TeXExponentsAfterSubscript);

  if (!config->Read(wxS("AutoSaveAsTempFile"), &m_autoSaveAsTempFile)) {
    long autoSaveMinutes = 3;
    config->Read(wxS("autoSaveMinutes"), &autoSaveMinutes);
    m_autoSaveAsTempFile = (autoSaveMinutes == 0);
  }
  config->Read("language", &m_language);
  config->Read("incrementalSearch", &m_incrementalSearch);
  if (m_language == wxLANGUAGE_UNKNOWN)
    m_language = wxLANGUAGE_DEFAULT;
  {
    long format;
    if(config->Read("maximaHelpFormat", &format))
      MaximaHelpFormat(static_cast<maximaHelpFormat>(format));
  }
  // Appearance: read the explicit setting if present; otherwise this is a config
  // from before the dark/light style sets existed -> migrate the old "invert
  // brightness" toggle once the styles are loaded (see after ReadStyles()).
  const bool haveAppearanceSetting = config->HasEntry(wxS("appearance"));
  bool legacyInvertBackground = false;
  if (haveAppearanceSetting) {
    long appearance = static_cast<long>(Appearance::followSystem);
    config->Read(wxS("appearance"), &appearance);
    if (appearance < 0 || appearance > static_cast<long>(Appearance::followSystem))
      appearance = static_cast<long>(Appearance::followSystem);
    m_appearance = static_cast<Appearance>(appearance);
  } else {
    config->Read(wxS("invertBackground"), &legacyInvertBackground);
    m_appearance = Appearance::followSystem;
  }
  config->Read("undoLimit", &m_undoLimit);
  config->Read("recentItems", &m_recentItems);
  config->Read("maxGnuplotMegabytes", &m_maxGnuplotMegabytes);
  config->Read("offerKnownAnswers", &m_offerKnownAnswers);
  config->Read(wxS("documentclass"), &m_documentclass);
  config->Read(wxS("documentclassoptions"), &m_documentclassOptions);
  config->Read("greekSidebar_ShowLatinLookalikes",
               &m_greekSidebar_ShowLatinLookalikes);
  config->Read("greekSidebar_Show_mu", &m_greekSidebar_Show_mu);
  config->Read("symbolPaneAdditionalChars", &m_symbolPaneAdditionalChars);
  config->Read("parameters", &m_maximaParameters);
  config->Read("autodetectHelpBrowser", &m_autodetectHelpBrowser);
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

  config->Read(wxS("autosubscript"), &m_autoSubscript);
  if(m_autoSubscript < 0)
    m_autoSubscript = 0;
  if(m_autoSubscript > 2)
    m_autoSubscript = 2;
  config->Read(wxS("abortOnError"), &m_abortOnError);
  config->Read("defaultPort", &m_defaultPort);
  {
    int ls = static_cast<int>(m_layoutStrategy);
    config->Read(wxS("layoutStrategy"), &ls);
    if (ls < 0 || ls > 2) ls = 1;
    m_layoutStrategy = static_cast<LayoutStrategy>(ls);
  }
  if(m_showLength < 0)
    m_showLength = 0;
  if(m_showLength > 3)
    m_showLength = 3;
  if (m_tocDepth < 1)
    m_tocDepth = 1;
  // Fix wrong" maxima=1" parameter in ~/.wxMaxima if upgrading from 0.7.0a
  if (m_maximaUserLocation.IsSameAs(wxS("1")))
    m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();


  long showLabelChoice = static_cast<long>(m_showLabelChoice);
  if(static_cast<long>(m_showLabelChoice) < 0)
    m_showLabelChoice = labels_automatic;
  if(m_showLabelChoice >= labels_invalidSelection)
    m_showLabelChoice = labels_none;
  config->Read(wxS("showLabelChoice"), &showLabelChoice);
  m_showLabelChoice = (showLabels)showLabelChoice;




  if (m_displayedDigits <= 20)
    m_displayedDigits = 20;





  config->Read(wxS("labelWidth"), &m_labelWidth);

  config->Read(wxS("keepPercent"), &m_keepPercent);
  config->Read(wxS("saveUntitled"), &m_saveUntitled);
  config->Read(wxS("cursorJump"), &m_cursorJump);

  ReadStyles();
  if (!haveAppearanceSetting) {
    // Migrate a pre-dark-mode config. The appearance the user actually saw is the
    // XOR of whether their own style set is dark (document background darker than
    // its text) and whether the old "invert brightness" toggle was on -- inverting
    // a dark set yields a bright look and vice versa. Preserve that look.
    m_styleStore.SetUseDark(false); // inspect the user's (light-slot) loaded set
    const auto brightness = [](const wxColour &c) {
      return static_cast<int>(c.Red()) + c.Green() + c.Blue();
    };
    const bool styleSetIsDark =
      brightness(GetStyle(TS_DOCUMENT_BACKGROUND)->GetColor()) <
      brightness(GetStyle(TS_TEXT)->GetColor());
    m_appearance = (styleSetIsDark != legacyInvertBackground) ? Appearance::dark
                                                              : Appearance::light;
  }
  // Both the appearance setting and the style sets are loaded now; point the
  // active set at the one the appearance selects and refresh the cached
  // background brush (ReadStyles() set it from whatever set was active before).
  m_styleStore.SetUseDark(UseDarkMode());
  UpdateBackgroundBrush();
}

Configuration::maximaHelpFormat Configuration::MaximaHelpFormat() const
{
  if(
     (m_maximaHelpFormat == frontend) ||
     (m_maximaHelpFormat == browser) ||
     (m_maximaHelpFormat == maxima))
    return m_maximaHelpFormat;
  else
    return frontend;
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
void Configuration::MakeStylesConsistent() { m_styleStore.MakeConsistent(); }

bool Configuration::StyleAffectsCode(TextStyle style) const {
  return m_styleStore.AffectsCode(style);
}

bool Configuration::StyleAffectsMathOut(TextStyle style) const {
  return m_styleStore.AffectsMathOut(style);
}

bool Configuration::StyleAffectsColorOnly(TextStyle style) const {
  return m_styleStore.AffectsColorOnly(style);
}

wxColor Configuration::DefaultBackgroundColor() {
  return m_styleStore[TS_DOCUMENT_BACKGROUND].GetColor();
}

wxColor Configuration::EditorBackgroundColor() {
  return m_styleStore[TS_TEXT_BACKGROUND].GetColor();
}

void Configuration::NotifyOfCellRedraw(const Cell *cell) {
  if(!GetDebugmode())
    return;
  m_renderContext.NotifyOfCellRedraw(cell);
}

void Configuration::ClearAndEnableRedrawTracing() {
  if(!GetDebugmode())
    return;
  m_renderContext.ClearAndEnableRedrawTracing();
}

void Configuration::ReportMultipleRedraws() {
  if(!GetDebugmode())
    return;

  m_renderContext.ReportMultipleRedraws([](size_t counter, const Cell *cell) {
    wxASSERT(counter <= 1);
    wxLogMessage(
                 "Bug: %li redraws in one screen refresh for a cell reading \"%s\"",
                 static_cast<long>(counter), cell->ToString().mb_str());
  });
}

void Configuration::SetPrinting(bool printing) {
  m_renderContext.SetPrinting(printing);
  // Always print against the light style set (white paper), regardless of the
  // on-screen appearance.
  m_styleStore.SetUseDark(printing ? false : UseDarkMode());
  if (printing)
    ClipToDrawRegion(!printing);
}

long Configuration::GetLineWidth() const {
  // The default line width is the width of the viewport minus the indentation
  // minus roughly one char
  wxCoord lineWidth =
    GetCanvasSize().x -
    Scale_Px(GetLabelWidth()) + Scale_Px(GetCellBracketWidth()) +
    Scale_Px(GetDefaultFontSize().Get());

  // If that was suspiciously wide we reduce the default line width again.
  if ((lineWidth >= Scale_Px(GetDefaultFontSize().Get()) * LineWidth_em()) &&
      (!GetPrinting()))
    lineWidth = Scale_Px(GetDefaultFontSize().Get()) * LineWidth_em();
  return lineWidth;
}

//! A comparison operator for wxImage
// static bool operator==(const wxImage &a, const wxImage &b) {
//   if (a.GetSize() != b.GetSize())
//     return false;

//   long bytes = (long)a.GetWidth() * b.GetHeight() * 3;
//   if (bytes < 0)
//     return false;

//   return memcmp(a.GetData(), b.GetData(), bytes) == 0;
// }

void Configuration::SetZoomFactor(double newzoom) {
  if(m_zoomFactor == newzoom)
    return;

  RecalculateForce();

  m_styleStore.ClearCaches();
  if (newzoom > GetMaxZoomFactor())
    newzoom = GetMaxZoomFactor();
  if (newzoom < GetMinZoomFactor())
    newzoom = GetMinZoomFactor();

  m_zoomFactor = newzoom;
}

Configuration::~Configuration() {
  if(m_initOpts != temporary)
    {
      WriteStyles();
      WriteSettings();
    }
}

// bool Configuration::CharsExistInFont(const wxFont &font,
//                                      const wxString &chars) {
//   wxASSERT(!chars.empty());
//   for (auto const &ex : m_charsInFont)
//     // cppcheck-suppress useStlAlgorithm
//     if (ex.chars == chars)
//       return ex.exist;

//   auto const cache = [this, &chars](bool result) {
//     m_charsInFont.emplace_back(chars, result);
//     return result;
//   };

//   if (!font.IsOk())
//     return cache(false);

//   // Seems like Apple didn't hold to their high standards as the maths part of
//   // this font don't form nice big mathematical symbols => Blacklisting this
//   // font.
//   if (font.GetFaceName() == wxS("Monaco"))
//     return cache(false);

//   if (!m_useUnicodeMaths)
//     return cache(false);

//   struct Params {
//     wxUniChar ch;
//     wxSize size;
//     wxImage image;
//     explicit Params(wxUniChar ch) : ch(ch) {}
//   };
//   std::vector<Params> P(chars.begin(), chars.end());

//   // Letters with width or height = 0 don't exist in the current font
//   GetRecalcDC()->SetFont(font);
//   for (auto &p : P) {
//     wxCoord descent;
//     GetRecalcDC()->GetTextExtent(p.ch, &p.size.x, &p.size.y, &descent);
//     if ((p.size.x < 1) || ((p.size.y - descent) < 1))
//       return cache(false);
//   }

//   bool allDifferentSizes = true;
//   for (auto i = P.begin(); allDifferentSizes && i != P.end(); ++i)
//     for (auto j = i + 1; allDifferentSizes && j != P.end(); ++j)
//       allDifferentSizes &= i->size != j->size;

//   if (allDifferentSizes)
//     return cache(true);

//   for (auto &p : P) {
//     wxBitmap bmp(p.size);
//     wxMemoryDC dc(bmp);
//     dc.SetFont(font);
//     dc.Clear();
//     dc.DrawText(p.ch, wxPoint(0, 0));
//     p.image = bmp.ConvertToImage();
//   }

//   for (auto i = P.begin(); i != P.end(); ++i)
//     for (auto j = i + 1; j != P.end(); ++j)
//       if (i->image == j->image)
//         return cache(false);

//   return cache(true);
// }

wxString Configuration::GetFontName(TextStyle const ts) const {
  wxString retval;
  retval = m_styleStore[ts].GetFontName();
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
  RecalculateForce();
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
    m_styleStore[TS_MATH].SetFontSize(AFontSize(tmpLong));
  config->Read(wxS("showInputLabels"), &m_showInputLabels);
  wxString tmpString;
  if (config->Read(wxS("Style/Math/fontname"), &tmpString) &&
      tmpString.size() > 1)
    m_styleStore[TS_MATH].SetFontName(tmpString);

  m_styleStore.Read(config);
  m_renderContext.SetBackgroundBrush(*wxTheBrushList->FindOrCreateBrush(
                                                         m_styleStore[TS_DOCUMENT_BACKGROUND].GetColor(), wxBRUSHSTYLE_SOLID));
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
#ifdef __WXMSW__
  config->Write("usewgnuplot", m_useWgnuplot);
#endif
  config->Write("maximaHelpFormat", static_cast<long>(m_maximaHelpFormat));
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
  config->Write(wxS("MaxLayoutTime"), m_maxLayoutTime);

  config->Write(wxS("maxClipbrd_BitmapMegabytes"),
                m_maxClipbrd_BitmapMegabytes);

  WriteStyles(config);
  for (const auto &[fontName, chars] : m_renderableChars)
    config->Write(wxS("renderability/good/") + fontName, chars);
  for (const auto &[fontName, chars] : m_nonRenderableChars)
    config->Write(wxS("renderability/bad/") + fontName, chars);
  if (file != wxEmptyString) {
    config->Flush();
    delete config;
  }
}

wxFontWeight Configuration::IsBold(long st) const {
  if (m_styleStore[st].IsBold())
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(long st) const {
  if (m_styleStore[st].IsItalic())
    return wxFONTSTYLE_ITALIC;
  return wxFONTSTYLE_NORMAL;
}

wxString Configuration::GetSymbolFontName() const {
  return m_styleStore[TS_CODE_DEFAULT].GetFontName();
}

wxColour Configuration::GetColor(TextStyle style) {
  wxColour col = m_styleStore[style].GetColor();
  if (m_outdated)
    col = m_styleStore[TS_OUTDATED].GetColor();
  return col;
}

wxCoord Configuration::Scale_Px(double px) const {
  wxCoord retval = lround(px * GetZoomFactor());
  if(retval < 1)
    retval = 1;
  return retval;
}

AFontSize Configuration::Scale_Px(AFontSize size) const {
  auto retval = size.Get() * GetZoomFactor();
  return AFontSize(retval);
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

bool Configuration::FontRendersChar(wxUniChar ch, const wxFont &font) {
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
    m_renderableChars[fontName] += ch;
  else
    m_nonRenderableChars[fontName] += ch;

  return retval;
}

bool Configuration::FontDisplaysChar(wxUniChar ch, const wxFont &font) {
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
  characterDC.DrawText(ch, 100, 100);
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
  long configId = m_configId;
  wxConfig::Get()->Read(wxS("configID"), &configId);

  return m_configId != configId;
}

void Configuration::WriteStyles(wxConfigBase *config) {
  // The mechanical scalar settings come from the same key<->member table
  // ReadConfig() reads them from, so both sides always agree on the keys.
  for (const auto &setting : ScalarConfigSettings())
    std::visit([&](auto member) { config->Write(setting.key, this->*member); },
               setting.member);

  std::uniform_int_distribution<long> urd(std::numeric_limits<long>::min(), std::numeric_limits<long>::max());
  m_configId = urd(m_eng);
  config->Write(wxS("configID"), m_configId);
  config->Write(wxS("showInputLabels"), m_showInputLabels);
  if (OfferInternalHelpBrowser())
    config->Write(wxS("maximaUsesWxmaximaBrowser"),
                  m_maximaUsesWxmaximaBrowser);
  config->Write(wxS("TeXExponentsAfterSubscript"),
                m_TeXExponentsAfterSubscript);
  config->Write("incrementalSearch", m_incrementalSearch);
  config->Write(wxS("AutoSaveAsTempFile"), m_autoSaveAsTempFile);
  config->Write(wxS("greekSidebar_ShowLatinLookalikes"),
                m_greekSidebar_ShowLatinLookalikes);
  config->Write(wxS("greekSidebar_Show_mu"), m_greekSidebar_Show_mu);
  config->Write(wxS("symbolPaneAdditionalChars"), m_symbolPaneAdditionalChars);
  config->Write(wxS("appearance"), static_cast<long>(m_appearance));
  config->Write("recentItems", m_recentItems);
  config->Write(wxS("undoLimit"), m_undoLimit);
  config->Write(wxS("showLabelChoice"), static_cast<int>(m_showLabelChoice));
  config->Write(wxS("parameters"), m_maximaParameters);
  config->Write(wxS("autodetectHelpBrowser"), m_autodetectHelpBrowser);
  if (OfferInternalHelpBrowser())
  config->Write(wxS("helpBrowser"), m_helpBrowserUserLocation);
  config->Write(wxS("layoutStrategy"), static_cast<int>(m_layoutStrategy));
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
  m_styleStore.Write(config);
}

const std::vector<Configuration::ScalarSetting> &
Configuration::ScalarConfigSettings() {
  static const std::vector<ScalarSetting> settings = {
    {wxS("allowNetworkHelp"), &Configuration::m_allowNetworkHelp},
    {wxS("autodetectMaxima"), &Configuration::m_autodetectMaxima},
    {wxS("autoIndent"), &Configuration::m_autoIndent},
    {wxS("autoWrapMode"), &Configuration::m_autoWrap},
    {wxS("bitmapScale"), &Configuration::m_bitmapScale},
    {wxS("changeAsterisk"), &Configuration::m_changeAsterisk},
    {wxS("copyBitmap"), &Configuration::m_copyBitmap},
    {wxS("copyEMF"), &Configuration::m_copyEMF},
    {wxS("copyMathMLHTML"), &Configuration::m_copyMathMLHTML},
    {wxS("copyMathML"), &Configuration::m_copyMathML},
    {wxS("copyRTF"), &Configuration::m_copyRTF},
    {wxS("copySVG"), &Configuration::m_copySVG},
    {wxS("DefaultFramerate"), &Configuration::m_defaultFramerate},
    {wxS("defaultPlotHeight"), &Configuration::m_defaultPlotHeight},
    {wxS("defaultPlotWidth"), &Configuration::m_defaultPlotWidth},
    {wxS("displayedDigits"), &Configuration::m_displayedDigits},
    {wxS("enterEvaluates"), &Configuration::m_enterEvaluates},
    {wxS("exportContainsWXMX"), &Configuration::m_exportContainsWXMX},
    {wxS("fixedFontTC"), &Configuration::m_fixedFontTC},
    {wxS("fixReorderedIndices"), &Configuration::m_fixReorderedIndices},
    {wxS("hideBrackets"), &Configuration::m_hideBrackets},
    {wxS("hidemultiplicationsign"), &Configuration::m_hidemultiplicationsign},
    {wxS("indentMaths"), &Configuration::m_indentMaths},
    {wxS("insertAns"), &Configuration::m_insertAns},
    {wxS("latin2greek"), &Configuration::m_latin2greek},
    {wxS("matchParens"), &Configuration::m_matchParens},
    {wxS("mathJaxURL"), &Configuration::m_mathJaxURL},
    {wxS("mathJaxURL_UseUser"), &Configuration::m_mathJaxURL_UseUser},
    {wxS("maxima"), &Configuration::m_maximaUserLocation},
    {wxS("maximaUsesHhtmlBrowser"), &Configuration::m_maximaUsesHhtmlBrowser},
    {wxS("notifyIfIdle"), &Configuration::m_notifyIfIdle},
    {wxS("numpadEnterEvaluates"), &Configuration::m_numpadEnterEvaluates},
    {wxS("openHCaret"), &Configuration::m_openHCaret},
    {wxS("printBrackets"), &Configuration::m_printBrackets},
    {wxS("printScale"), &Configuration::m_printScale},
    {wxS("restartOnReEvaluation"), &Configuration::m_restartOnReEvaluation},
    {wxS("saveImgFileName"), &Configuration::m_saveImgFileName},
    {wxS("showLength"), &Configuration::m_showLength},
    {wxS("showMatchingParens"), &Configuration::m_showMatchingParens},
    {wxS("singlePageManual"), &Configuration::m_singlePageManual},
    {wxS("texPreamble"), &Configuration::m_texPreamble},
    {wxS("tocDepth"), &Configuration::m_tocDepth},
    {wxS("TOCshowsSectionNumbers"), &Configuration::m_TOCshowsSectionNumbers},
    {wxS("useInternalHelpBrowser"), &Configuration::m_useInternalHelpBrowser},
    {wxS("usePartialForDiff"), &Configuration::m_usePartialForDiff},
    {wxS("usepngCairo"), &Configuration::m_usepngCairo},
    {wxS("useSVG"), &Configuration::m_useSVG},
    {wxS("useUnicodeMaths"), &Configuration::m_useUnicodeMaths},
    {wxS("wizardTab"), &Configuration::m_wizardTab},
    {wxS("wrapLatexMath"), &Configuration::m_wrapLatexMath},
  };
  return settings;
}

const std::vector<std::pair<TextStyle, wxString>> &
Configuration::StyleConfigKeys() {
  // The table now lives in Styles; keep this forwarding wrapper so existing
  // callers (and the round-trip test) need not change.
  return Styles::ConfigKeys();
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
  return Styles::Name(textStyle);
}

std::size_t Configuration::ShowLength_Bytes() const
{
  std::size_t showLength;
  switch (ShowLength()) {
  case 0:
    showLength = 6000;
    break;
  case 1:
    showLength = 20000;
    break;
  case 2:
    showLength = 100000;
    break;
  case 3:
    showLength = 0;
    break;
  default:
    showLength = 6000;
  }
  return showLength;
}
wxString Configuration::m_configfileLocation_override;
bool Configuration::m_debugMode = false;
bool Configuration::m_use_threads = true;
Configuration::PerformanceStats Configuration::g_stats;
void Configuration::PerformanceStats::Report() const {
  static bool reported = false;
  if (reported)
    return;
  reported = true;

  wxLog::EnableLogging(true);
  
  // Ensure we have a log target that doesn't use GUI dialogs.
  // We don't restore the old target as we are shutting down.
  wxLog::SetActiveTarget(new wxLogStderr);

  wxLogMessage(_("Performance Statistics:"));
  wxLogMessage(_("  Manual anchors from built-in: %ld"), manualAnchorsFromBuiltin.load());
  wxLogMessage(_("  Manual anchors from cache: %ld"), manualAnchorsFromCache.load());
  wxLogMessage(_("  Manual anchors self-compiled: %ld"), manualAnchorsCompiled.load());
  wxLogMessage(_("  Maxima processes spawned: %ld"), maximaProcessesSpawned.load());
  wxLogMessage(_("  Font cache hits: %ld"), fontCacheHits.load());
  wxLogMessage(_("  Font cache misses: %ld"), fontCacheMisses.load());
  wxLogMessage(_("  Recalculation needed - Font invalid: %ld"), recalculationNeeded_FontInvalid.load());
  wxLogMessage(_("  Recalculation needed - Size invalid: %ld"), recalculationNeeded_SizeInvalid.load());
  wxLogMessage(_("  Recalculation needed - Font mismatch: %ld"), recalculationNeeded_FontMismatch.load());
  wxLogMessage(_("  Recalculation needed - Config changed: %ld"), recalculationNeeded_ConfigChanged.load());
  wxLogMessage(_("  Recalculation needed - Cells appended: %ld"), recalculationNeeded_CellsAppended.load());
  wxLogMessage(_("  Recalculation needed - Editor dirty: %ld"), recalculationNeeded_EditorDirty.load());
  wxLogMessage(_("  Cells converted to linear: %ld"), cellsConvertedToLinear.load());
  wxLogMessage(_("  Cells converted to 2D: %ld"), cellsConvertedTo2D.load());

  wxLog::FlushActive();
}

wxString Configuration::m_maxima_LANG;
