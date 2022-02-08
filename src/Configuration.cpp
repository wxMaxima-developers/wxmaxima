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
#include "StringUtils.h"
#include <wx/wx.h>
#include <wx/mimetype.h>
#include <wx/string.h>
#include <wx/font.h>
#include <wx/config.h>
#include <wx/wfstream.h>
#include <wx/fileconf.h>
#include <wx/txtstrm.h>
#include <wx/mstream.h>
#include <wx/xml/xml.h>

Configuration::Configuration(wxDC *dc, InitOpt options) :
  m_dc(dc)
{
  m_maximaOperators["("] = 1;
  m_maximaOperators["/"] = 1;
  m_maximaOperators["{"] = 1;
  m_maximaOperators["-"] = 1;
  m_maximaOperators["^"] = 1;
  m_maximaOperators["#"] = 1;
  m_maximaOperators["="] = 1;
  m_maximaOperators[":"] = 1;
  m_maximaOperators["["] = 1;
  m_maximaOperators["'"] = 1;
  m_maximaOperators["!"] = 1;
  m_maximaOperators["+"] = 1;
  m_maximaOperators["*"] = 1;
  m_maximaOperators["or"] = 1;
  m_maximaOperators["and"] = 1;
  m_maximaOperators["do_in"] = 1;
  m_maximaOperators[">"] = 1;
  m_maximaOperators["$SUBVAR"] = 1;
  m_maximaOperators["<"] = 1;
  m_maximaOperators["if"] = 1;
  m_maximaOperators["::="] = 1;
  m_maximaOperators["::"] = 1;
  m_maximaOperators["@"] = 1;
  m_maximaOperators["."] = 1;
  m_maximaOperators["-->"] = 1;
  m_maximaOperators["^^"] = 1;
  m_maximaOperators["not"] = 1;
  m_maximaOperators["<="] = 1;
  m_maximaOperators[":="] = 1;
  m_maximaOperators[">="] = 1;
  m_maximaOperators["$BFLOAT"] = 1;
  m_maximaOperators["do"] = 1;
  m_printing = false;
  m_clipToDrawRegion = true;
  m_inLispMode = false;
  m_forceUpdate = false;
  m_outdated = false;
  m_lineWidth_em = 88;
  m_antialiassingDC = NULL;
  m_workSheet = NULL;
  SetBackgroundBrush(*wxWHITE_BRUSH);
  ResetAllToDefaults(options);
  ReadConfig();
  wxString operators(
    wxT("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}")
    );
  for (wxString::const_iterator it = operators.begin(); it != operators.end(); ++it)
    m_maximaOperators[wxString(*it)] = 1;
}

wxSize Configuration::GetPPI() const
{
  if(GetWorkSheet())
  {
    wxSize ppi;
#if wxCHECK_VERSION(3, 1, 1)
    wxDisplay display;
    
    int display_idx = wxDisplay::GetFromWindow(GetWorkSheet());
    if(display_idx < 0)
      ppi = wxSize(72, 72);
    else
      ppi = wxDisplay(display_idx).GetPPI();
#else
    ppi = wxGetDisplayPPI();
#endif
    if((ppi.x < 10) || (ppi.y < 10))
      ppi = wxGetDisplayPPI();
    if((ppi.x <= 10) || (ppi.y < 10))
      ppi = wxSize(72, 72);
    
    return ppi;;
  }
  else
    return m_ppi;
}

void Configuration::ResetAllToDefaults(InitOpt options)
{
  m_showAllDigits = false;
  m_lineBreaksInLongNums = false;
  m_autoSaveMinutes = 3;
  m_numpadEnterEvaluates = true;
  m_saveImgFileName = false;
  m_maximaEnvVars.clear();
  // Tell gnuplot not to wait for <enter> every few lines
  #ifndef __WXMSW__
  m_maximaEnvVars[wxT("PAGER")] = wxT("cat");
  #endif
  m_wrapLatexMath = true;
  m_exportContainsWXMX = true;
  m_bitmapScale = 3;
  m_maxClipbrd_BitmapMegabytes = 4;
  m_defaultFramerate = 12;
  m_tocDepth = 6;
  m_fixedFontTC = false;
  m_hideMarkerForThisMessage.clear();
  #ifdef __WXOSX__
  m_usepngCairo = false;
  #else
  m_usepngCairo = true;
  #endif
  m_wxMathML_Filename = wxEmptyString;
  m_wxMathML_UseFile = false;

  m_mathJaxURL = wxT("https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js");
  m_usePartialForDiff = false,
  m_documentclass = wxT("article");
  m_documentclassOptions = wxT("fleqn");
  m_incrementalSearch = true;
  m_symbolPaneAdditionalChars = wxT("Øü§");
  m_hidemultiplicationsign = true;
  m_autodetectHelpBrowser = true;
  #ifdef __WXGTK__
  m_helpBrowserUserLocation = wxT("xdg-open");
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
  m_parenthesisDrawMode = unknown;
  m_zoomFactor = 1.0; // affects returned fontsizes
  m_useSVG = false;
  m_changeAsterisk = true;
  m_latin2greek = false;
  m_enterEvaluates = false;
  m_printScale = 1.0;
  m_TeXFonts = false;
  m_notifyIfIdle = true;
  m_fixReorderedIndices = true;
  m_showBrackets = true;
  m_printBrackets = false;
  m_hideBrackets = true;
  m_defaultPlotWidth = 1200;
  m_defaultPlotHeight = 900;
  SetLanguage(wxLANGUAGE_DEFAULT);
  m_adjustWorksheetSizeNeeded = false;
  m_showLabelChoice = labels_prefer_user;
  m_abortOnError = true;
  m_defaultPort = 49152;
  m_maxGnuplotMegabytes = 12;
  m_clientWidth = 1024;
  m_clientHeight = 768;
  m_indentMaths=true;
  if (!(options & InitOpt::temporary))
  {
    if(m_maximaLocation_override != wxEmptyString)
      m_maximaUserLocation = m_maximaLocation_override;
    else
      m_maximaUserLocation = Dirstructure::Get()->MaximaDefaultLocation();
  }
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
  m_TeXFonts = false;
  m_keepPercent = true;  
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
  std::fill(std::begin(m_styles), std::end(m_styles), Style{});

  Style defaultStyle;

  #ifdef __WINDOWS__
  // Font defaulting for Windows
  m_styles[TS_DEFAULT].FontName(AFontName::Arial());

  for (auto fontName :
       {AFontName::Linux_Libertine_G(), AFontName::Linux_Libertine_O(),
        AFontName::Linux_Libertine(), AFontName::Times_New_Roman()})
  {
    auto style = Style().FontName(fontName);
    style.ResolveToFont();
    if (style.IsFontOk() && style.GetFontName() == fontName)
    {
      m_styles[TS_MATH].FontName(style.GetFontName());
      break;
    }
  }
  #endif

  // TODO It's a fat chance that this font actually will be monospace.
  wxFont monospace(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
  m_styles[TS_ASCIIMATHS].SetFontName(AFontName(monospace.GetFaceName()));
  m_styles[TS_DEFAULT].Bold().Italic().FontSize(12);
  m_styles[TS_MATH].FontSize(12.0);

  m_styles[TS_TEXT].FontSize(12);
  m_styles[TS_CODE_VARIABLE].Color(0,128,0).Italic();
  m_styles[TS_CODE_FUNCTION].Color(128,0,0).Italic();
  m_styles[TS_CODE_COMMENT].Color(64,64,64).Italic();
  m_styles[TS_CODE_NUMBER].Color(128,64,0).Italic();
  m_styles[TS_CODE_STRING].Color(0,0,128).Italic();
  m_styles[TS_CODE_OPERATOR].Italic();
  m_styles[TS_CODE_LISP].Color(255,0,128).Italic();
  m_styles[TS_CODE_ENDOFLINE].Color(128,128,128).Italic();
  m_styles[TS_GREEK_CONSTANT].Italic();
  m_styles[TS_HEADING6].Bold().FontSize(14);
  m_styles[TS_HEADING5].Bold().FontSize(15);
  m_styles[TS_SUBSUBSECTION].Bold().FontSize(16);
  m_styles[TS_SUBSECTION].Bold().FontSize(16);
  m_styles[TS_SECTION].Bold().Italic().FontSize(18);
  m_styles[TS_TITLE].Bold().Underlined().FontSize(24);
  m_styles[TS_WARNING].Color(wxT("orange")).Bold().FontSize(12);
  m_styles[TS_ERROR].Color(*wxRED).FontSize(12);
  m_styles[TS_MAIN_PROMPT].Color(255,128,128);
  m_styles[TS_OTHER_PROMPT].Color(*wxRED).Italic();
  m_styles[TS_LABEL].Color(255,192,128);
  m_styles[TS_USERLABEL].Color(255,64,0);
  //m_styles[TS_SPECIAL_CONSTANT];
  m_styles[TS_INPUT].Color(*wxBLUE);
  //m_styles[TS_NUMBER];
  m_styles[TS_STRING].Italic();
  //m_styles[TS_GREEK_CONSTANT];
  m_styles[TS_VARIABLE].Italic();
  //m_styles[TS_FUNCTION];
  m_styles[TS_HIGHLIGHT].Color(*wxRED);
  m_styles[TS_TEXT_BACKGROUND].Color(*wxWHITE);
  m_styles[TS_DOCUMENT_BACKGROUND].Color(*wxWHITE);
  //m_styles[TS_CELL_BRACKET];
  m_styles[TS_ACTIVE_CELL_BRACKET].Color(*wxRED);
  //m_styles[TS_CURSOR];
  m_styles[TS_SELECTION].Color(wxSYS_COLOUR_HIGHLIGHT);
  m_styles[TS_EQUALSSELECTION].Color(wxSYS_COLOUR_HIGHLIGHT).ChangeLightness(150);
  m_styles[TS_OUTDATED].Color(153,153,153);
}

const wxString &Configuration::GetEscCode(const wxString &key)
{
  auto &escCodes = EscCodes();
  auto it = escCodes.find(key);
  if (it != escCodes.end())
    return it->second;
  return wxm::emptyString;
}

Configuration::EscCodeIterator Configuration::EscCodesBegin()
{ return EscCodes().cbegin(); }
Configuration::EscCodeIterator Configuration::EscCodesEnd()
{ return EscCodes().cend(); }


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

  {
    // If this preference cannot be loaded we don't want an error message about it
    SuppressErrorDialogs suppressor;
    wxString hideMessagesConfigString;
    config->Read(wxT("showAllDigits"), &m_showAllDigits);
    config->Read(wxT("lineBreaksInLongNums"), &m_lineBreaksInLongNums);
    config->Read(wxT("autoSaveMinutes"), &m_autoSaveMinutes);
    config->Read(wxT("wrapLatexMath"), &m_wrapLatexMath);
    config->Read(wxT("exportContainsWXMX"), &m_exportContainsWXMX);
    config->Read(wxT("texPreamble"), &m_texPreamble);
    {
      config->Read(wxT("suppressYellowMarkerMessages"), &hideMessagesConfigString);
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(hideMessagesConfigString);
      wxMemoryInputStream istream(ostream.GetOutputStreamBuffer()->GetBufferStart(),
                                  ostream.GetOutputStreamBuffer()->GetBufferSize());
      wxXmlDocument xmlDocument;
      if(xmlDocument.Load(istream))
      {
        wxXmlNode *headNode = xmlDocument.GetDocumentNode();
        if(headNode)
        {
          headNode = headNode->GetChildren();
          while((headNode) && (headNode->GetName() != wxT("markers")))
            headNode = headNode->GetNext();
          wxXmlNode *entry = headNode->GetChildren();
          while(entry)
          {

            if(entry->GetName() == wxT("hide"))
            {
              wxXmlNode *node = entry->GetChildren();
              if(node)
              {
                HideMarkerForThisMessage(node->GetContent(), true);
              }
            }
            entry = entry->GetNext();
          }
        }
      }
    }
    {
      wxString maximaEnvironmentString;
      config->Read(wxT("maximaEnvironment"), &maximaEnvironmentString);
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(maximaEnvironmentString);
      wxMemoryInputStream istream(ostream.GetOutputStreamBuffer()->GetBufferStart(),
                                  ostream.GetOutputStreamBuffer()->GetBufferSize());
      wxXmlDocument xmlDocument;
      if(xmlDocument.Load(istream))
      {
        wxXmlNode *headNode = xmlDocument.GetDocumentNode();
        if(headNode)
        {
          headNode = headNode->GetChildren();
          while(headNode)
          {
            if(headNode->GetName() == wxT("entries"))
            {
              m_maximaEnvVars.clear();
              wxXmlNode *entryNode = headNode->GetChildren();
              while(entryNode)
              {
                if(entryNode->GetName() == wxT("entry"))
                {
                  wxXmlNode *entry = entryNode->GetChildren();
                  wxString var;
                  wxString value;
                  while(entry)
                  {
                    if(entry->GetName() == wxT("var"))
                    {
                      wxXmlNode *node = entry->GetChildren();
                      while(node)
                      {
                        if(node->GetType() == wxXML_TEXT_NODE)
                          var = node->GetContent();
                        node = node->GetNext();
                      }
                    }
                    if(entry->GetName() == wxT("value"))
                    {
                      wxXmlNode *node = entry->GetChildren();
                      while(node)
                      {
                        if(node->GetType() == wxXML_TEXT_NODE)
                          value = node->GetContent();
                        node = node->GetNext();
                      }
                    }
                    entry = entry->GetNext();
                  }
                  if(!var.IsEmpty())
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
  config->Read(wxT("maxClipbrd_BitmapMegabytes"), &m_maxClipbrd_BitmapMegabytes);
  if(m_maxClipbrd_BitmapMegabytes<0)
    m_maxClipbrd_BitmapMegabytes = 1;
  config->Read(wxT("numpadEnterEvaluates"), &m_numpadEnterEvaluates);
  config->Read(wxT("saveImgFileName"), &m_saveImgFileName);
  config->Read(wxT("usePartialForDiff"), &m_usePartialForDiff);
  config->Read(wxT("TeXExponentsAfterSubscript"), &m_TeXExponentsAfterSubscript);
  config->Read(wxT("defaultPlotWidth"), &m_defaultPlotWidth);
  config->Read(wxT("defaultPlotHeight"), &m_defaultPlotHeight);
  config->Read(wxT("fixedFontTC"), &m_fixedFontTC);
  config->Read(wxT("usepngCairo"), &m_usepngCairo);
  if(!config->Read(wxT("AutoSaveAsTempFile"), &m_autoSaveAsTempFile))
  {
    long autoSaveMinutes = 0;
    config->Read(wxT("autoSaveMinutes"), &autoSaveMinutes);
    m_autoSaveAsTempFile = (autoSaveMinutes == 0);
  }
  config->Read("language", &m_language);
  config->Read("incrementalSearch", &m_incrementalSearch);
  if (m_language == wxLANGUAGE_UNKNOWN)
    m_language = wxLANGUAGE_DEFAULT;

  config->Read("wxMathML_Filename", &m_wxMathML_Filename);
  config->Read("wxMathML_UseFile", &m_wxMathML_UseFile);
  config->Read("invertBackground", &m_invertBackground);
  config->Read("undoLimit", &m_undoLimit);
  config->Read("recentItems", &m_recentItems);
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
  config->Read(wxT("bitmapScale"), &m_bitmapScale);
  config->Read(wxT("DefaultFramerate"), &m_defaultFramerate);
  config->Read(wxT("tocDepth"), &m_tocDepth);
  if(m_tocDepth < 1)
    m_tocDepth = 1;
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

  config->Read(wxT("autoIndent"), &m_autoIndent);

  int showLabelChoice = 0;
  config->Read(wxT("showLabelChoice"), &showLabelChoice);
  m_showLabelChoice = (showLabels) showLabelChoice; 

  config->Read(wxT("changeAsterisk"), &m_changeAsterisk);

  config->Read(wxT("notifyIfIdle"), &m_notifyIfIdle);

  config->Read(wxT("hideBrackets"), &m_hideBrackets);

  config->Read(wxT("displayedDigits"), &m_displayedDigits);
  if (m_displayedDigits <= 20)
    m_displayedDigits = 20;

  config->Read(wxT("restartOnReEvaluation"), &m_restartOnReEvaluation);

  config->Read(wxT("matchParens"), &m_matchParens);
  config->Read(wxT("showMatchingParens"), &m_showMatchingParens);

  config->Read(wxT("insertAns"), &m_insertAns);

  config->Read(wxT("openHCaret"), &m_openHCaret);
  
  config->Read(wxT("labelWidth"), &m_labelWidth);

  config->Read(wxT("printBrackets"), &m_printBrackets);

  config->Read(wxT("ZoomFactor"), &m_zoomFactor);

  if (wxFontEnumerator::IsValidFacename((m_fontCMEX = AFontName::CMEX10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMSY = AFontName::CMSY10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMRI = AFontName::CMR10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMMI = AFontName::CMMI10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMTI = AFontName::CMTI10()).GetAsString()))
  {
    m_TeXFonts = true;
    config->Read(wxT("usejsmath"), &m_TeXFonts);
  }

  config->Read(wxT("keepPercent"), &m_keepPercent);
  config->Read(wxT("saveUntitled"), &m_saveUntitled);
  config->Read(wxT("cursorJump"), &m_cursorJump);

  ReadStyles();
}

bool Configuration::HideMarkerForThisMessage(wxString message)
{
  auto it = m_hideMarkerForThisMessage.find(message);
  if (it == m_hideMarkerForThisMessage.end())
    return false;
  else
    return it->second;
}

Style Configuration::GetStyle(TextStyle ts, AFontSize fontSize) const
{
  Style style = m_styles[ts];

  if ((ts == TS_TITLE) || (ts == TS_SECTION) || (ts == TS_SUBSECTION) ||
      (ts == TS_SUBSUBSECTION) || (ts == TS_HEADING5) || (ts == TS_HEADING6))
  {
    // While titles and section names may be underlined the section number
    // isn't. Else the space between section number and section title
    // would look weird.
    style.SetUnderlined(false);

    // Besides that these items have a fixed font size.
  } else
    style.SetFontSize(fontSize);

  style.SetFontName(GetFontName(ts));

  if (!style.IsFontOk())
    style.SetFontName({});

  // cppcheck-suppress duplicateCondition
  if (!style.IsFontOk())
  {
    auto size = style.GetFontSize();
    style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);
    style.SetFontSize(size);
  }

  wxASSERT_MSG(style.IsFontOk(),
               _("Seems like something is broken with a font."));
  
  return style;
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

void Configuration::NotifyOfCellRedraw(const Cell *cell)
{
  if (!m_cellRedrawTrace || !cell)
    return;
  // This operation is fast and doesn't allocate after the configuration
  // was used for a few screen redraws.
  m_cellRedrawTrace->push_back(cell);
}

void Configuration::ClearAndEnableRedrawTracing()
{
  if (!m_cellRedrawTrace)
    m_cellRedrawTrace.reset(new CellRedrawTrace);
  else
    m_cellRedrawTrace->clear();
}

void Configuration::ReportMultipleRedraws()
{
  if (!m_cellRedrawTrace)
    return;

  // This sort is over two orders of magnitude faster,
  // per-cell, than having counters in a map or hash.
  std::sort(m_cellRedrawTrace->begin(), m_cellRedrawTrace->end());
  int counter = 0;
  const Cell *prev = {};
  for (auto *cell : *m_cellRedrawTrace)
  {
    if (prev != cell)
    {
      if (counter > 1)
        wxLogMessage("Bug: %i redraws in one screen refresh for a cell reading \"%s\"",
                     counter, prev->ToString());
      prev = cell;
      counter = 1;
    }
    else
      ++counter;
  }
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
  if((lineWidth >= Scale_Px(GetDefaultFontSize()) * LineWidth_em()) &&
     (!m_printing))
    lineWidth = Scale_Px(GetDefaultFontSize()) * LineWidth_em();
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
    auto style = GetStyle(TS_FUNCTION, AFontSize(20.0f));

    if (CharsExistInFont(style.GetFont(), parens))
    {
      m_parenthesisDrawMode = assembled_unicode;
      return m_parenthesisDrawMode;
    }
    style.SetFontName(AFontName::Linux_Libertine());
    if (CharsExistInFont(style.GetFont(), parens))
    {
      m_parenthesisDrawMode = assembled_unicode_fallbackfont;
      return m_parenthesisDrawMode;
    }
    style.SetFontName(AFontName::Linux_Libertine_O());
    if (CharsExistInFont(style.GetFont(), parens))
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

  long bytes = (long)a.GetWidth() * b.GetHeight() * 3;
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
}

Configuration::~Configuration()
{
  WriteStyles();
  WriteSettings();
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

AFontName Configuration::GetFontName(TextStyle const ts) const
{
  AFontName retval;

  if (ts == TS_TITLE || ts == TS_SUBSECTION || ts == TS_SUBSUBSECTION ||
      ts == TS_HEADING5 || ts == TS_HEADING6 || ts == TS_SECTION ||
      ts == TS_TEXT || ts == TS_ASCIIMATHS)
    retval = m_styles[ts].GetFontName();

  else if (ts == TS_NUMBER || ts == TS_VARIABLE || ts == TS_FUNCTION ||
           ts == TS_SPECIAL_CONSTANT || ts == TS_STRING)
    retval = m_styles[TS_MATH].GetFontName();

  if (retval.empty())
    retval = m_styles[TS_DEFAULT].GetFontName();

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

void Configuration::ReadStyles(const wxString &file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
  {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }

  m_styles[TS_DEFAULT].Read(config, "Style/Default/");

  // Read legacy defaults for the math font name and size
  long tmpLong;
  if (config->Read(wxT("mathfontsize"), &tmpLong) && tmpLong > 1)
    m_styles[TS_MATH].SetFontSize(AFontSize(tmpLong));
  wxString tmpString;
  if (config->Read(wxT("Style/Math/fontname"), &tmpString) && tmpString.size() > 1)
    m_styles[TS_MATH].SetFontName(AFontName(tmpString));

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
  m_styles[TS_INPUT].Read(config, "Style/Input/");
  m_styles[TS_NUMBER].Read(config, "Style/Number/");
  m_styles[TS_STRING].Read(config, "Style/String/");
  m_styles[TS_ASCIIMATHS].Read(config, "Style/ASCIImaths/");
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
void Configuration::WriteSettings(const wxString &file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxT("wxMaxima"), wxEmptyString, file);

  {
    wxXmlNode *topNode = new wxXmlNode(
      NULL,
      wxXML_DOCUMENT_NODE, wxEmptyString,
      wxEmptyString
      );
    wxXmlNode *entriesNode =
      new wxXmlNode(
        topNode,
        wxXML_ELEMENT_NODE,
        "entries");
    wxEnvVariableHashMap::const_iterator it;
    for (it = m_maximaEnvVars.begin();
         it != m_maximaEnvVars.end();
         ++it)
    {
      if(!it->first.IsEmpty())
      {
        wxXmlNode *entryNode =
          new wxXmlNode(
            entriesNode,
            wxXML_ELEMENT_NODE,
            "entry");
        wxXmlNode *varNode =
          new wxXmlNode(
            entryNode,
            wxXML_ELEMENT_NODE,
            "var");
        wxXmlNode *valueNode =
          new wxXmlNode(
            entryNode,
            wxXML_ELEMENT_NODE,
            "value");
        new wxXmlNode(
          varNode,
          wxXML_TEXT_NODE,
          wxEmptyString,
          it->first);
        new wxXmlNode(
          valueNode,
          wxXML_TEXT_NODE,
          wxEmptyString,
          it->second);
      }
    }
    wxXmlDocument xmlDoc;
    xmlDoc.SetDocumentNode(topNode);
    // Write the string into a memory buffer
    wxMemoryOutputStream ostream;
    xmlDoc.Save(ostream);
    wxMemoryInputStream istream(ostream.GetOutputStreamBuffer()->GetBufferStart(),
                                ostream.GetOutputStreamBuffer()->GetBufferSize());
    wxTextInputStream text(istream);
    wxString maximaEnvConfigString;
    while(!istream.Eof())
      maximaEnvConfigString += text.ReadLine() + wxT("\n");
    config->Write(wxT("maximaEnvironment"), maximaEnvConfigString);
  }
  {
    wxXmlNode *topNode = new wxXmlNode(
      NULL,
      wxXML_DOCUMENT_NODE, wxEmptyString,
      wxEmptyString
      );
    wxXmlNode *headNode = new wxXmlNode(
      topNode,
      wxXML_ELEMENT_NODE, wxT("markers"),
      wxEmptyString
      );
    StringBoolHash::const_iterator it;
    for (it = m_hideMarkerForThisMessage.begin();
         it != m_hideMarkerForThisMessage.end();
         ++it)
    {
      if(it->second)
      {
        wxXmlNode *hideNode =
          new wxXmlNode(
            headNode,
            wxXML_ELEMENT_NODE,
            "hide");
        new wxXmlNode(
          hideNode,
          wxXML_TEXT_NODE,
          wxEmptyString,
          it->first);
      }
    }
    wxXmlDocument xmlDoc;
    xmlDoc.SetDocumentNode(topNode);
    // Write the string into a memory buffer
    wxMemoryOutputStream ostream;
    xmlDoc.Save(ostream);
    wxMemoryInputStream istream(ostream.GetOutputStreamBuffer()->GetBufferStart(),
                                ostream.GetOutputStreamBuffer()->GetBufferSize());
    wxTextInputStream text(istream);
    wxString hideMessagesConfigString;
    while(!istream.Eof())
      hideMessagesConfigString += text.ReadLine() + wxT("\n");
    config->Write(wxT("suppressYellowMarkerMessages"), hideMessagesConfigString);
  }
  
  config->Write(wxT("showAllDigits"), m_showAllDigits);
  config->Write(wxT("lineBreaksInLongNums"), m_lineBreaksInLongNums);
  config->Write(wxT("keepPercent"), m_keepPercent);
  config->Write(wxT("labelWidth"), m_labelWidth);
  config->Write(wxT("saveUntitled"), m_saveUntitled);
  config->Write(wxT("cursorJump"), m_cursorJump);
  config->Write(wxT("autoSaveMinutes"), m_autoSaveMinutes);
  config->Write(wxT("wxMathML_Filename"), m_wxMathML_Filename);
  config->Write(wxT("wxMathML_UseFile"), m_wxMathML_UseFile);

  config->Write(wxT("maxClipbrd_BitmapMegabytes"), m_maxClipbrd_BitmapMegabytes);

  WriteStyles(config);
  if(file != wxEmptyString)
  {
    config->Flush();
    delete config;
  }
}

wxFontWeight Configuration::IsBold(long st) const
{
  if (m_styles[st].IsBold())
    return wxFONTWEIGHT_BOLD;
  return wxFONTWEIGHT_NORMAL;
}

wxFontStyle Configuration::IsItalic(long st) const
{
  if (m_styles[st].IsItalic())
    return wxFONTSTYLE_ITALIC;
  return wxFONTSTYLE_NORMAL;
}

class AFontName Configuration::GetSymbolFontName() const
{
#ifdef __WINDOWS__
  return AFontName::Symbol();
#else
  return m_styles[TS_DEFAULT].GetFontName();
#endif
}

wxColour Configuration::GetColor(TextStyle style)
{
  wxColour col = m_styles[style].GetColor();
  if (m_outdated)
    col = m_styles[TS_OUTDATED].GetColor();

  if(InvertBackground() &&
     (style != TS_TEXT_BACKGROUND) &&
     (style != TS_DOCUMENT_BACKGROUND))
    col = MakeColorDifferFromBackground(col);
  return col;
}

long Configuration::Scale_Px(double px) const
{
  long retval = lround(px * GetZoomFactor());
  return std::max(retval, 1l);
}

AFontSize Configuration::Scale_Px(AFontSize size) const
{
  auto retval = size.Get() * GetZoomFactor();
  return AFontSize(retval);
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

bool Configuration::InUpdateRegion(wxRect const rect) const
{
  if (!ClipToDrawRegion())
    return true;

  wxRect const updateRegion = GetUpdateRegion();

  return updateRegion.Intersects(rect) ||
         updateRegion.Contains(rect) ||
         (updateRegion == rect) ||
         rect.Contains(updateRegion);
}

void Configuration::WriteStyles(wxConfigBase *config)
{
  config->Write(wxT("wrapLatexMath"), m_wrapLatexMath);
  config->Write(wxT("exportContainsWXMX"), m_exportContainsWXMX);
  config->Write(wxT("texPreamble"), m_texPreamble);
  config->Write(wxT("numpadEnterEvaluates"), m_numpadEnterEvaluates);
  config->Write(wxT("saveImgFileName"), m_saveImgFileName);
  config->Write(wxT("usePartialForDiff"), m_usePartialForDiff);
  config->Write(wxT("TeXExponentsAfterSubscript"), m_TeXExponentsAfterSubscript);
  config->Write(wxT("defaultPlotWidth"), m_defaultPlotWidth);
  config->Write(wxT("defaultPlotHeight"), m_defaultPlotHeight);
  config->Write(wxT("fixedFontTC"), m_fixedFontTC);
  config->Write(wxT("bitmapScale"), m_bitmapScale);
  config->Write(wxT("DefaultFramerate"), m_defaultFramerate);
  config->Write(wxT("tocDepth"), m_tocDepth);
  config->Write(wxT("usepngCairo"), m_usepngCairo);
  config->Write("incrementalSearch", m_incrementalSearch);
  config->Write(wxT("hideBrackets"), m_hideBrackets);
  config->Write(wxT("printScale"), m_printScale);
  config->Write(wxT("AutoSaveAsTempFile"), m_autoSaveAsTempFile);
  config->Write(wxT("autoWrapMode"), m_autoWrap);
  config->Write(wxT("autoIndent"), m_autoIndent);
  config->Write(wxT("indentMaths"), m_indentMaths);
  config->Write(wxT("matchParens"), m_matchParens);
  config->Write(wxT("showMatchingParens"), m_showMatchingParens);
  config->Write(wxT("changeAsterisk"), m_changeAsterisk);
  config->Write(wxT("hidemultiplicationsign"), m_hidemultiplicationsign);
  config->Write(wxT("latin2greek"), m_latin2greek);
  config->Write(wxT("greekSidebar_ShowLatinLookalikes"), m_greekSidebar_ShowLatinLookalikes);
  config->Write(wxT("greekSidebar_Show_mu"), m_greekSidebar_Show_mu);
  config->Write(wxT("symbolPaneAdditionalChars"),m_symbolPaneAdditionalChars);
  config->Write(wxT("notifyIfIdle"), m_notifyIfIdle);
  config->Write(wxT("displayedDigits"), m_displayedDigits);
  config->Write(wxT("insertAns"), m_insertAns);
  config->Write(wxT("openHCaret"), m_openHCaret);
  config->Write(wxT("restartOnReEvaluation"), m_restartOnReEvaluation);
  config->Write(wxT("invertBackground"), m_invertBackground);
  config->Write("recentItems", m_recentItems);
  config->Write(wxT("undoLimit"), m_undoLimit);
  config->Write(wxT("showLabelChoice"), (int) (m_showLabelChoice));
  config->Write(wxT("printBrackets"), m_printBrackets);
  config->Write(wxT("autodetectMaxima"), m_autodetectMaxima);
  config->Write(wxT("parameters"),m_maximaParameters);
  config->Write(wxT("maxima"), m_maximaUserLocation);
  config->Write(wxT("autodetectHelpBrowser"), m_autodetectHelpBrowser);
  config->Write(wxT("helpBrowser"), m_helpBrowserUserLocation);
  config->Write(wxT("fixReorderedIndices"), m_fixReorderedIndices);
  config->Write(wxT("mathJaxURL_UseUser"), m_mathJaxURL_UseUser);
  config->Write(wxT("enterEvaluates"), m_enterEvaluates);
  config->Write(wxT("mathJaxURL"), m_mathJaxURL);
  config->Write(wxT("antiAliasLines"), m_antiAliasLines);
  config->Write(wxT("copyBitmap"), m_copyBitmap);
  config->Write(wxT("copyMathML"), m_copyMathML);
  config->Write(wxT("copyMathMLHTML"), m_copyMathMLHTML);
  config->Write(wxT("copyRTF"), m_copyRTF);
  config->Write(wxT("copySVG"), m_copySVG);
  config->Write(wxT("copyEMF"), m_copyEMF);
  config->Write(wxT("useSVG"), m_useSVG);
  config->Write(wxT("showLength"), m_showLength);
  config->Write(wxT("TOCshowsSectionNumbers"), m_TOCshowsSectionNumbers);
  config->Write(wxT("useUnicodeMaths"), m_useUnicodeMaths);
  config->Write("defaultPort",m_defaultPort);
  config->Write("abortOnError",m_abortOnError);
  config->Write("language",m_language);
  config->Write("maxGnuplotMegabytes",m_maxGnuplotMegabytes);
  config->Write("offerKnownAnswers",m_offerKnownAnswers);
  config->Write("documentclass",m_documentclass);
  config->Write("documentclassoptions",m_documentclassOptions);
  config->Write("HTMLequationFormat", (int) (m_htmlEquationFormat));
  config->Write("autosubscript", m_autoSubscript);
  config->Write(wxT("ZoomFactor"), m_zoomFactor);
  // Fonts
  m_styles[TS_DEFAULT].Write(config, "Style/Default/");
  m_styles[TS_MATH].Write(config, "Style/Math/");
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
  m_styles[TS_ASCIIMATHS].Write(config, "Style/ASCIImaths/");
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
}

//! Saves the style settings to a file.
void Configuration::WriteStyles(const wxString &file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxT("wxMaxima"), wxEmptyString, file);

  WriteStyles(config);
  if(file != wxEmptyString)
  {
    config->Flush();
    delete config;
  }
}
const wxString &Configuration::GetStyleName(TextStyle style) const
{
  static const wxString *names[NUMBEROFSTYLES] = {
    &_("Default"),
    &_("Variables"),
    &_("Numbers"),
    &_("Function names"),
    &_("Special constants"),
    &_("Greek Constants"),
    &_("Strings"),
    &_("Maxima input"),
    &_("Input labels"),
    &_("Maxima questions"),
    &_("Output labels"),
    &_("User-defined labels"),
    &_("Highlight (dpart)"),
    &_("Maxima warnings"),
    &_("Maxima errors"),
    &_("ASCII maths"),
    &_("Text cell"),
    &_("Heading 6"),
    &_("Heading 5"),
    &_("Subsubsection cell (Heading 4)"),
    &_("Subsection cell (Heading 3)"),
    &_("Section cell (Heading 2)"),
    &_("Title cell (Heading 1)"),
    &_("Text cell background"),
    &_("Document background"),
    &_("Cell bracket"),
    &_("Active cell bracket"),
    &_("Cursor"),
    &_("Selection"),
    &_("Text equal to selection"),
    &_("Outdated cells"),
    &_("Code highlighting: Variables"),
    &_("Code highlighting: Functions"),
    &_("Code highlighting: Comments"),
    &_("Code highlighting: Numbers"),
    &_("Code highlighting: Strings"),
    &_("Code highlighting: Operators"),
    &_("Code highlighting: Lisp"),
    &_("Code highlighting: End of line"),
    &_("Math Default"),
  };
  if (style >= 0 && style < NUMBEROFSTYLES)
    return *names[style];

  return wxm::emptyString;
}

wxString Configuration::m_maximaLocation_override;
wxString Configuration::m_configfileLocation_override;
