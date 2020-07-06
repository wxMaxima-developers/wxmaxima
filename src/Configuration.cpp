// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
#include <wx/wx.h>
#include <wx/mimetype.h>
#include <wx/string.h>
#include <wx/font.h>
#include <wx/config.h>
#include <wx/wfstream.h>
#include <wx/fileconf.h>
#include <map>

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
  // see https://docs.wxwidgets.org/3.0/classwx_mime_types_manager.html
  auto *manager = wxTheMimeTypesManager;
  wxFileType *filetype = manager->GetFileTypeFromExtension("html");
  m_helpBrowserUserLocation = filetype->GetOpenCommand({});
  #endif

  m_worksheetRedrawCounter = 0;
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
  auto &Default = m_styles[TS_DEFAULT];
  SetStyle(TS_DEFAULT, Style::FactoryDefault().Bold().Italic());
  SetStyle(TS_MATH, Style::FactoryMath());

  SetStyle(TS_TEXT, Style(Default).FontSize(12));
  SetStyle(TS_CODE_VARIABLE, Style().Color(0,128,0).Italic());
  SetStyle(TS_CODE_FUNCTION, Style().Color(128,0,0).Italic());
  SetStyle(TS_CODE_COMMENT, Style().Color(64,64,64).Italic());
  SetStyle(TS_CODE_NUMBER, Style().Color(128,64,0).Italic());
  SetStyle(TS_CODE_STRING, Style().Color(0,0,128).Italic());
  SetStyle(TS_CODE_OPERATOR, Style().Italic());
  SetStyle(TS_CODE_LISP, Style().Color(255,0,128).Italic());
  SetStyle(TS_CODE_ENDOFLINE, Style().Color(128,128,128).Italic());
  SetStyle(TS_GREEK_CONSTANT, Style().Italic());
  SetStyle(TS_HEADING6, Style(Default).Bold().FontSize(14));
  SetStyle(TS_HEADING5, Style(Default).Bold().FontSize(15));
  SetStyle(TS_SUBSUBSECTION, Style(Default).Bold().FontSize(16));
  SetStyle(TS_SUBSECTION, Style(Default).Bold().FontSize(16));
  SetStyle(TS_SECTION, Style(Default).Bold().Italic().FontSize(18));
  SetStyle(TS_TITLE, Style(Default).Bold().Underlined().FontSize(24));
  SetStyle(TS_WARNING, Style().Color(wxT("orange")).Bold().FontSize(12));
  SetStyle(TS_ERROR, Style().Color(*wxRED).FontSize(12));
  SetStyle(TS_MAIN_PROMPT, Style().Color(255,128,128));
  SetStyle(TS_OTHER_PROMPT, Style().Color(*wxRED).Italic());
  SetStyle(TS_LABEL, Style().Color(255,192,128));
  SetStyle(TS_USERLABEL, Style().Color(255,64,0));
  SetStyle(TS_SPECIAL_CONSTANT, Style());
  SetStyle(TS_INPUT, Style().Color(*wxBLUE));
  SetStyle(TS_NUMBER, Style());
  SetStyle(TS_STRING, Style().Italic());
  SetStyle(TS_GREEK_CONSTANT, Style());
  SetStyle(TS_VARIABLE, Style().Italic());
  SetStyle(TS_FUNCTION, Style());
  SetStyle(TS_HIGHLIGHT, Style().Color(*wxRED));
  SetStyle(TS_TEXT_BACKGROUND, Style().Color(*wxWHITE));
  SetStyle(TS_DOCUMENT_BACKGROUND, Style().Color(*wxWHITE));
  SetStyle(TS_CELL_BRACKET, Style());
  SetStyle(TS_ACTIVE_CELL_BRACKET, Style().Color(*wxRED));
  SetStyle(TS_CURSOR, Style());
  SetStyle(TS_SELECTION, Style().Color(wxSYS_COLOUR_HIGHLIGHT));
  SetStyle(TS_EQUALSSELECTION, Style().Color(wxSYS_COLOUR_HIGHLIGHT).ChangeLightness(150));
  SetStyle(TS_OUTDATED, Style().Color(153,153,153));

  wxLogDebug("** After Style Init");
  auto i = 0;
  for (auto &s : m_styles)
    wxLogDebug("%s %s", GetStyleName(TextStyle(i++)), s.GetDump());

  ReadConfig();

  wxLogDebug("** After Style Read");
  i = 0;
  for (auto &s : m_styles)
    wxLogDebug("%s %s", GetStyleName(TextStyle(i++)), s.GetDump());
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

  if (wxFontEnumerator::IsValidFacename((m_fontCMEX = AFontName::CMEX10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMSY = AFontName::CMSY10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMRI = AFontName::CMR10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMMI = AFontName::CMMI10()).GetAsString()) &&
      wxFontEnumerator::IsValidFacename((m_fontCMTI = AFontName::CMTI10()).GetAsString()))
  {
    m_TeXFonts = true;
    config->Read(wxT("usejsmath"), &m_TeXFonts);
  }

  m_keepPercent = true;
  wxConfig::Get()->Read(wxT("keepPercent"), &m_keepPercent);

  ReadStyles();
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

static const std::map<TextStyle, wxString> StylePaths =
  {
    { TS_TEXT, "Style/Text/" },
    { TS_CODE_VARIABLE, "Style/CodeHighlighting/Variable/" },
    { TS_CODE_FUNCTION, "Style/CodeHighlighting/Function/" },
    { TS_CODE_COMMENT, "Style/CodeHighlighting/Comment/" },
    { TS_CODE_NUMBER, "Style/CodeHighlighting/Number/" },
    { TS_CODE_STRING, "Style/CodeHighlighting/String/" },
    { TS_CODE_OPERATOR, "Style/CodeHighlighting/Operator/" },
    { TS_CODE_LISP, "Style/CodeHighlighting/Lisp/" },
    { TS_CODE_ENDOFLINE, "Style/CodeHighlighting/EndOfLine/" },
    { TS_HEADING6, "Style/Heading6/" },
    { TS_HEADING5, "Style/Heading5/" },
    { TS_SUBSUBSECTION, "Style/Subsubsection/" },
    { TS_SUBSECTION, "Style/Subsection/" },
    { TS_SECTION, "Style/Section/" },
    { TS_TITLE, "Style/Title/" },
    { TS_WARNING, "Style/Warning/" },
    { TS_MAIN_PROMPT, "Style/MainPrompt/" },
    { TS_OTHER_PROMPT, "Style/OtherPrompt/" },
    { TS_LABEL, "Style/Label/" },
    { TS_USERLABEL, "Style/UserDefinedLabel/" },
    { TS_SPECIAL_CONSTANT, "Style/Special/" },
    { TS_GREEK_CONSTANT, "Style/Greek/" },
    { TS_INPUT, "Style/Input/" },
    { TS_NUMBER, "Style/Number/" },
    { TS_STRING, "Style/String/" },
    { TS_GREEK_CONSTANT, "Style/Greek/" },
    { TS_VARIABLE, "Style/Variable/" },
    { TS_FUNCTION, "Style/Function/" },
    { TS_HIGHLIGHT, "Style/Highlight/" },
    { TS_TEXT_BACKGROUND, "Style/Background/" },
    { TS_DOCUMENT_BACKGROUND, "Style/DocumentBackground/" },
    { TS_ERROR, "Style/Error/" },
    { TS_CELL_BRACKET, "Style/CellBracket/" },
    { TS_ACTIVE_CELL_BRACKET, "Style/ActiveCellBracket/" },
    { TS_CURSOR, "Style/ActiveCellBracket/" },
    { TS_SELECTION, "Style/Selection/" },
    { TS_EQUALSSELECTION, "Style/EqualsSelection/" },
    { TS_OUTDATED, "Style/Outdated/" },
};

void Configuration::ReadStyles(const wxString &file)
{
  wxConfigBase *config = nullptr;
  if (file.empty())
    config = wxConfig::Get();
  else
  {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }

  SetStyle(TS_DEFAULT, m_styles[TS_DEFAULT].Read(config, "Style/Default/"));

  wxString mathFontName;
  long mathFontSize;
  if (config->Read(wxT("Style/Math/fontname"), &mathFontName) && !mathFontName.empty())
    m_styles[TS_MATH].SetFontName(AFontName(mathFontName));
  if (config->Read(wxT("mathfontsize"), &mathFontSize) && mathFontSize > 0)
    m_styles[TS_MATH].SetFontSize(AFontSize(mathFontSize));
  SetStyle(TS_MATH, m_styles[TS_MATH].Read(config, "Style/Math/"));

  for (auto &stylePath : StylePaths)
    SetStyle(stylePath.first, m_styles[stylePath.first].Read(config, stylePath.second));

  m_BackgroundBrush = *wxTheBrushList->FindOrCreateBrush(m_styles[TS_DOCUMENT_BACKGROUND].GetColor(), wxBRUSHSTYLE_SOLID);
}

//! Saves the style settings to a file.
void Configuration::WriteStyles(const wxString &file)
{
  wxConfigBase *config = nullptr;
  if (file.empty())
    config = wxConfig::Get();
  else
    config = new wxFileConfig(wxT("wxMaxima"), wxEmptyString, file);

  // Font
  m_styles[TS_DEFAULT].Write(config, "Style/Default/");
  config->Write(wxT("mathfontsize"), m_styles[TS_MATH].GetFontSize().GetAsLong());
  config->Write("Style/Math/fontname", m_styles[TS_MATH].GetNameStr());

  for (auto &stylePath : StylePaths)
    m_styles[stylePath.first].Write(config, stylePath.second);

  if (!file.empty())
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
#if defined __WXMSW__
  static const AFontName name(wxT("Symbol"));
  return name;
#else
  return m_fontName;
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
    &_("Math Default")
    };
  if (style >= 0 && style < NUMBEROFSTYLES)
    return *names[style];
  static wxString empty;
  return empty;
}

//! Styles that only carry colors
static const TextStyle ColorOnlyStyles[] = {
  TS_TEXT_BACKGROUND,
  TS_DOCUMENT_BACKGROUND,
  TS_CELL_BRACKET,
  TS_ACTIVE_CELL_BRACKET,
  TS_CURSOR,
  TS_SELECTION,
  TS_EQUALSSELECTION,
  TS_OUTDATED,
  TS_CODE_VARIABLE,
  TS_CODE_FUNCTION,
  TS_CODE_COMMENT,
  TS_CODE_NUMBER,
  TS_CODE_STRING,
  TS_CODE_OPERATOR,
  TS_CODE_LISP,
  TS_CODE_ENDOFLINE
};

//! Styles with individual fonts - all other styles inherit the TS_DEFAULT or TS_MATH
static const TextStyle CustomFontStyles[] = {
  TS_DEFAULT, TS_MATH,
  TS_TEXT, TS_HEADING5, TS_HEADING6, TS_SUBSECTION, TS_SUBSUBSECTION, TS_SECTION, TS_TITLE
};

  //! Styles rendered in math font
  static const TextStyle MathFontStyles[] = {
    TS_VARIABLE, TS_NUMBER, TS_FUNCTION, TS_SPECIAL_CONSTANT, TS_GREEK_CONSTANT,
    TS_STRING,
    TS_HIGHLIGHT,
    };

//! Styles rendered in default font size (but not default font)
static const TextStyle DefaultSizeMathFontStyles[] = {
  TS_MAIN_PROMPT, TS_OTHER_PROMPT,
  TS_WARNING, TS_ERROR
};

  // NOTE: TS_LABEL, TS_USERLABEL, TS_INPUT were originally rejected by Cell::IsMath
  // and they were sized using the default font size (if not font). Thus we leave those
  // as not math styles - they'll use the default font.

  //! Styles rendered in default font
  static const TextStyle DefaultFontStyles[] = {
    TS_INPUT,
    TS_LABEL,
    TS_USERLABEL,
    };

template <typename Array>
static bool IsIn(TextStyle st, Array const &array)
{
  for (auto s : array)
    if (s == st)
      return true;
  return false;
};

Style Configuration::GetDefaultStyleAt(AFontSize fontSize) const
{
  auto style = m_styles[TS_DEFAULT];
  style.SetFontSize({ MC_MIN_SIZE, fontSize });
  return style;
}

Style Configuration::GetStyle(TextStyle textStyle, GetStyleOpt options, AFontSize fontSize) const
{
  Style style = m_styles[textStyle];
  if (fontSize.IsNull() || IsIn(textStyle, CustomFontStyles))
    fontSize = style.GetFontSize();
  if (!(options & UnscaledStyle))
    fontSize = Scale_Px(fontSize);
  style.SetFontSize({ MC_MIN_SIZE, fontSize });
  return style;
}

void Configuration::SetStyle(TextStyle st, const Style *newStyle)
{
  Style style = newStyle ? *newStyle : m_styles[st];

  // A sanity check is needed for all styles that have custom fonts
  if (IsIn(st, CustomFontStyles))
  {
    if (style.GetFontSize() < MC_MIN_SIZE || style.GetFontSize() > MC_MAX_SIZE)
    {
      if (st != TS_DEFAULT && st != TS_MATH)
        style.SetFontSize(m_styles[TS_DEFAULT].GetFontSize());
      else
        style.SetFontSize({ MC_MIN_SIZE, style.GetFontSize(), MC_MAX_SIZE });
    }
    // Font Fallbacks
    if (!style.IsFontOk())
    {
      if (st == TS_DEFAULT)
        style.SetFontFaceFrom(Style::FactoryDefault());
      else if (st == TS_MATH)
        style.SetFontFaceFrom(Style::FactoryMath());
      else
        style.SetFontFaceFrom(m_styles[TS_DEFAULT]);
      if (!style.IsFontOk() && st != TS_DEFAULT)
      {
        auto size = style.GetFontSize();
        style = m_styles[TS_DEFAULT];
        style.SetFontSize(size);
        if (!style.IsFontOk())
          style = m_styles[TS_DEFAULT];
      }
    }
  }

  if (!IsIn(st, ColorOnlyStyles))
    style.ResolveToFont(); // Resolve the style to reflect the attributes of a font

  // Default Font propagation
  if (st == TS_DEFAULT)
  {
    for (auto ps : DefaultFontStyles)
      if (m_styles[ps].SetFontFaceAndSizeFrom(style)) m_styles[ps].ResolveToFont();
    for (auto ps : DefaultSizeMathFontStyles)
      m_styles[ps].SetFontSize(style.GetFontSize());
  }
  else
    if (IsIn(st, DefaultFontStyles))
    style.SetFontFaceAndSizeFrom(m_styles[TS_DEFAULT]);
  // Default Font Size + Math Font Face Propagation
  else
    if (IsIn(st, DefaultSizeMathFontStyles))
  {
    style.SetFontFaceFrom(m_styles[TS_MATH]);
    style.SetFontSize(m_styles[TS_DEFAULT].GetFontSize());
  }
  // Math Font Propagation
  else
    if (st == TS_MATH)
  {
    for (auto ps : MathFontStyles)
      m_styles[ps].SetFontFaceAndSizeFrom(style);
    for (auto ps : DefaultSizeMathFontStyles)
      m_styles[ps].SetFontFaceFrom(style);
  }
  else
    if (IsIn(st, MathFontStyles))
    style.SetFontFaceAndSizeFrom(m_styles[TS_MATH]);
  // Color-Only Styles
  else
    if (IsIn(st, ColorOnlyStyles))
    style = Style().Color(style.GetColor());

  m_styles[st] = style;
}


wxString Configuration::m_maximaLocation_override;
wxString Configuration::m_configfileLocation_override;
