// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <wx/wx.h>
#include <wx/config.h>
#include <wx/display.h>
#include <wx/fontenum.h>
#include "LoggingMessageDialog.h"
#include "TextStyle.h"
#include <vector>

#define MC_LINE_SKIP Scale_Px(2)
#define MC_TEXT_PADDING Scale_Px(1)

#define PAREN_OPEN_TOP_UNICODE     "\u239b"
#define PAREN_OPEN_EXTEND_UNICODE  "\u239c"
#define PAREN_OPEN_BOTTOM_UNICODE  "\u239d"
#define PAREN_CLOSE_TOP_UNICODE    "\u239e"
#define PAREN_CLOSE_EXTEND_UNICODE "\u239f"
#define PAREN_CLOSE_BOTTOM_UNICODE "\u23a0"
#define SUM_SIGN "\u2211"
#define PROD_SIGN "\u220F"
#define SUM_DEC 2

//! The width of the horizontally-drawn cursor
#define MC_HCARET_WIDTH 25

#define MC_EXP_INDENT 2
#define MC_MIN_SIZE 6
#define MC_MAX_SIZE 48

#define CMEX10 "jsMath-cmex10"
#define CMSY10 "jsMath-cmsy10"
#define CMR10  "jsMath-cmr10"
#define CMMI10 "jsMath-cmmi10"
#define CMTI10 "jsMath-cmti10"

#define LIBERTINE1 "LinLibertine_DRah.ttf"
#define LIBERTINE2 "LinLibertine_I.ttf"
#define LIBERTINE3 "LinLibertine_Mah.ttf"
#define LIBERTINE4 "LinLibertine_Rah.ttf"
#define LIBERTINE5 "LinLibertine_RBah.ttf"
#define LIBERTINE6 "LinLibertine_RBIah.ttf"
#define LIBERTINE7 "LinLibertine_RIah.ttf"
#define LIBERTINE8 "LinLibertine_RZah.ttf"
#define LIBERTINE9 "LinLibertine_RZIah.ttf"

/*! The configuration storage for the current worksheet.

  Caching the information here means we don't need to search for the configuration
  value's name every time we need the information: Reading configuration values from
  the system's configuration storage can be quite time consuming, especially on a 
  MSW with a long registry.

  In order to make all cells temporary listen to a different set of configuration 
  than the default one all that has to be done is to create a new configuration 
  object that contains hold the right settings for printing/export as bitmap or 
  similar: Configuration::Get() will always return the last Configuration that was 
  created and therefore as long as the new configuration object exist will return
  a pointer to this object if configuration is needed.
 */
class Configuration
{
public:
  //! The export formats we support for HTML equations
  enum htmlExportFormat
  {
    mathJaX_TeX = 0,
    bitmap = 1,
    mathML_mathJaX = 2,
    svg = 3
  };

  enum showLabels
  {
    labels_automatic = 0,
    labels_prefer_user = 1,
    labels_useronly = 2,
    labels_none = 3
  };

  enum drawMode
  {
    ascii,              //!< Use ascii characters only
    assembled_unicode_fallbackfont,  //!< Unicode, fallbackfont 1
    assembled_unicode,  //!< Unicode, current font
    assembled_unicode_fallbackfont2,  //!< Unicode, fallbackfont 2
    handdrawn,           //!< A  parenthesis sign that was created using draw commands
    unknown
  };

  WX_DECLARE_STRING_HASH_MAP(wxString, StringHash);
  //! A list of all symbols that can be entered using Esc-Codes
  StringHash m_escCodes;

  //! Set maxima's working directory
  void SetWorkingDirectory(wxString dir)
  { m_workingdir = dir; }

  wxString GetWorkingDirectory() const
  { return m_workingdir; }

  void ReadConfig();

  /*! The constructor
    
    \param dc The drawing context that is to be used for drawing objects
   */
  explicit Configuration(wxDC *dc = NULL);

  //! Set the drawing context that is currently active
  void SetContext(wxDC &dc)
  {
    m_dc = &dc;
    m_antialiassingDC = NULL;
  }

  void SetBackgroundBrush(wxBrush brush);
  wxBrush GetBackgroundBrush() const {return m_BackgroundBrush;}
  wxBrush GetTooltipBrush() const {return m_tooltipBrush;}
  void SetAntialiassingDC(wxDC &antialiassingDC)
    {m_antialiassingDC = &antialiassingDC;}

  void UnsetAntialiassingDC()
    {m_antialiassingDC = NULL;}

  ~Configuration();

  static wxString m_maximaLocation_override;
  static wxString m_configfileLocation_override;
  
  static double GetMinZoomFactor()
  { return 0.1; }

  static double GetMaxZoomFactor()
  { return 32.0; }

  /*! Extra space to leave between two equations in output cells.

    Extra space between equations is useful if we don't display labels that show
    which line begins a new equation and which line merely continues a multi-line
    equation.
   */
  double GetInterEquationSkip() const 
  {
    if (ShowAutomaticLabels())
      return 0;
    else
      return GetZoomFactor() * m_mathFontSize / 2;
  }

  long GetCellBracketWidth() const
  {
    return (int) (GetZoomFactor() * 16);
  }

  //! Hide brackets that are not under the pointer?
  bool HideBrackets() const
  { return m_hideBrackets; }

  //! Define if we want to hide brackets that are not under the pointer.
  void HideBrackets(bool hide)
  {
    wxConfig::Get()->Write(wxT("hideBrackets"), m_hideBrackets = hide);
  }

  //! Hide brackets that are not under the pointer?
  double PrintScale() const
  { return m_printScale; }

  //! Define if we want to hide brackets that are not under the pointer.
  void PrintScale(double scale)
  {
    wxConfig::Get()->Write(wxT("printScale"), m_printScale = scale);
  }

  //! Sets the zoom factor the worksheet is displayed at
  void SetZoomFactor(double newzoom);

  //! Sets the zoom factor without storing the new value in the config file/registry.
  void SetZoomFactor_temporarily(double newzoom){
    if(m_zoomFactor != newzoom)
    {
      RecalculationForce(true);
      FontChanged(true);
    }
    m_zoomFactor = newzoom;
  }

  /*! Scales a distance [in pixels] according to the zoom factor

    Is used for displaying/printing/exporting of text/maths.
   */
  long Scale_Px(double px) const;

  //! Determines the zoom factor the worksheet is displayed at
  double GetZoomFactor() const
  { return m_zoomFactor; }

  //! Get a drawing context suitable for size calculations
  wxDC *GetDC()
  { return m_dc; }

  //! Get a drawing context suitable for size calculations
  wxDC *GetAntialiassingDC()
    {
      if ((m_antialiassingDC != NULL) && m_antiAliasLines)
        return m_antialiassingDC;
      else
        return m_dc;
    }
  
  wxString GetFontName(long type = TS_DEFAULT) const;

  // cppcheck-suppress functionStatic
  // cppcheck-suppress functionConst
  wxString GetSymbolFontName() const;

  wxFontWeight IsBold(long st) const;

  wxFontStyle IsItalic(long st) const;

  bool IsUnderlined(long st) const {return m_styles[st].Underlined();}

  //! Force a full recalculation?
  void RecalculationForce(bool force)
  {
    m_forceUpdate = force;
  }

  //! Force a full recalculation?
  bool RecalculationForce() const 
  {
    return m_forceUpdate;
  }

  long GetLabelWidth() const
  { return m_labelWidth * 14; }

  //! Get the indentation of GroupCells.
  long GetIndent() const
  {
    if (m_indent < 0)
      return 3 * GetCellBracketWidth() / 2;
    else
      return m_indent;
  }

  //! Get the resolution of the display showing the worksheet
  wxSize GetPPI() const {return GetPPI(GetWorkSheet());}

  // cppcheck-suppress functionStatic
  // cppcheck-suppress functionConst
  //! Get the resolution of an arbitrary display
  wxSize GetPPI(wxWindow *win) const;
  
  //! How much vertical space is to be left between two group cells?
  long GetCursorWidth() const
  {
    long ppi;

    if(!m_printing)
      ppi = GetPPI().x;
    else
      ppi = 96;

    if (ppi / 45 < 1)
      return 1;
    else
      return ppi / 45;
  }
  
  //! The y position the worksheet starts at
  long GetBaseIndent() const
  {
    if (GetCursorWidth() < 12)
      return 12;
    else
      return 4 + GetCursorWidth();
  }

  //! The vertical space between GroupCells
  long GetGroupSkip() const
  {
    if (GetCursorWidth() < 10)
      return 20;
    else
      return 10 + GetCursorWidth();
  }

  /*! Set the indentation of GroupCells

    Normally this parameter is automatically calculated
   */
  void SetIndent(long indent)
  {
    if(m_indent != indent)
      RecalculationForce(true);
    m_indent = indent;
  }

  //! Set the width of the visible window for GetClientWidth()
  void SetClientWidth(long width)
  {
    if(m_clientWidth != width)
      RecalculationForce(true);
    m_clientWidth = width;
  }
  //! Has a font changed?
  bool FontChanged() const {return m_fontChanged;}

  struct CharsExist {
    wxString chars;
    bool exist;
    CharsExist(const wxString &chars, bool exist) : chars(chars), exist(exist) {}
  };
  std::vector<CharsExist> m_charsInFont;

  //! Has a font changed?
  void FontChanged(bool fontChanged)
    {
      m_fontChanged = fontChanged;
      if(fontChanged)
        RecalculationForce(true);
      m_charsInFont.clear();
    }
  
  //! Set the height of the visible window for GetClientHeight()
  void SetClientHeight(long height)
  { m_clientHeight = height; }

  //! Returns the width of the visible portion of the worksheet
  long GetClientWidth() const
  { return m_clientWidth; }

  //! Returns the height of the visible portion of the worksheet
  long GetClientHeight() const
  { return m_clientHeight; }

  //! Calculates the default line width for the worksheet
  double GetDefaultLineWidth() const
  {
    if (GetZoomFactor() < 1.0)
      return 1.0;
    else
      return GetZoomFactor();
  }

  //! The minimum sensible line width in withs of a letter.
  long LineWidth_em() const 
  {
    if(!m_printing)
      return m_lineWidth_em;
    else
      return 10000;
  }

  bool AutoSaveAsTempFile() const {return m_autoSaveAsTempFile;}
  void AutoSaveAsTempFile(bool asTempFile){wxConfig::Get()->Write(wxT("AutoSaveAsTempFile"), m_autoSaveAsTempFile = asTempFile);}

  //! Set the minimum sensible line width in widths of a letter.
  void LineWidth_em(long width)
  { m_lineWidth_em = width; }

  //! Returns the maximum sensible width for a text line [in characters]:
  // On big 16:9 screens text tends to get \b very wide before it hits the right margin.
  // But text blocks that are 1 meter wide and 2 cm high feel - weird.
  long GetLineWidth() const;

  long GetDefaultFontSize() const
  { return m_styles[TS_DEFAULT].FontSize(); }

  void SetDefaultFontSize(long fontSize)
  {
    m_styles[TS_DEFAULT].FontSize(fontSize);
  }

  long GetMathFontSize() const
  { return m_mathFontSize; }

  void SetMathFontSize(double size)
  { m_mathFontSize = size; }

  //! Do we want to have automatic line breaks for text cells?
  bool GetAutoWrap() const
  { return m_autoWrap > 0; }

  // cppcheck-suppress functionStatic
  //! Do we want to have automatic line breaks for code cells?
  bool GetAutoWrapCode() const
  { return false; }

  /*! Sets the auto wrap mode
    \param autoWrap 
     - 0: No automatic line breaks
     - 1: Automatic line breaks only for text cells
     - 2: Automatic line breaks for text and code cells.
  */
  void SetAutoWrap(long autoWrap)
  {
    wxConfig::Get()->Write(wxT("autoWrapMode"), m_autoWrap = autoWrap);
  }

  //! Do we want automatic indentation?
  bool GetAutoIndent() const
  { return m_autoIndent; }

  void SetAutoIndent(bool autoIndent)
  {
    wxConfig::Get()->Write(wxT("autoIndent"), m_autoIndent = autoIndent);
  }

  //! Do we want to indent all maths?
  bool IndentMaths() const {return m_indentMaths;}
  void IndentMaths(bool indent){wxConfig::Get()->Write(wxT("indentMaths"), m_indentMaths=indent);}
  long GetFontSize(TextStyle st) const
  {
    if (st == TS_TEXT || st == TS_HEADING5 || st == TS_HEADING6 || st == TS_SUBSUBSECTION || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return m_styles[st].FontSize();
    return 0;
  }

  /*! Reads the style settings 

    If a file name is given the settings are read from a file.
  */
  
  void ReadStyles(wxString file = wxEmptyString);
  
  /*! Saves the style settings 

    If a file name is given the settings are written to a file.
  */
  void WriteStyles(wxString file = wxEmptyString);
  
  void Outdated(bool outdated)
  { m_outdated = outdated; }

  bool CheckTeXFonts() const
  { return m_TeXFonts; }

  void CheckTeXFonts(bool check)
  { m_TeXFonts = check; }

  bool CheckKeepPercent() const
  { return m_keepPercent; }

  wxString GetTeXCMRI() const
  { return m_fontCMRI; }

  wxString GetTeXCMSY() const
  { return m_fontCMSY; }

  wxString GetTeXCMEX() const
  { return m_fontCMEX; }

  wxString GetTeXCMMI() const
  { return m_fontCMMI; }

  wxString GetTeXCMTI() const
  { return m_fontCMTI; }

  bool ShowCodeCells() const
  { return m_showCodeCells; }

  void ShowCodeCells(bool show);

  /*! Are we currently printing?

    This affects the bitmap scale as well as the fact if we want
    to output objects that are outside the region that currently is
    redrawn.
  */
  void SetPrinting(bool printing);
  
  /*! Are we currently printing?

    This affects the bitmap scale as well as the fact if we want
    to output objects that are outside the region that currently is
    redrawn.
  */
  bool GetPrinting() const
    { return m_printing; }

  //! Gets the color for a text style
  wxColour GetColor(TextStyle style);
  
  //! Inverts a color: In 2020 wxColor still lacks this functionality
  wxColour InvertColour(wxColour col);
  
  /*! Make this color differ from the background by a noticeable amount
    
    Useful for black/white background theme changes
  */
  wxColor MakeColorDifferFromBackground(wxColor color);
  
  bool GetMatchParens() const
    { return m_matchParens; }
  void SetMatchParens(bool matchParens)
    { wxConfig::Get()->Write(wxT("matchParens"), m_matchParens = matchParens); }

  bool GetChangeAsterisk() const
    { return m_changeAsterisk; }
  
  void SetChangeAsterisk(bool changeAsterisk)
    {
      wxConfig::Get()->Write(wxT("changeAsterisk"), m_changeAsterisk = changeAsterisk);
    }
  
  bool HidemultiplicationSign() const
    {
      return m_hidemultiplicationsign;
    }

  void HidemultiplicationSign(bool show)
    {
      wxConfig::Get()->Write(wxT("hidemultiplicationsign"), m_hidemultiplicationsign = show);
    }
  
  bool Latin2Greek() const
    {return m_latin2greek;}

  void Latin2Greek(bool latin2greek)
    {
      wxConfig::Get()->Write(wxT("latin2greek"), m_latin2greek = latin2greek);
    }

  bool GreekSidebar_ShowLatinLookalikes() const
    {return m_greekSidebar_ShowLatinLookalikes;}
  void GreekSidebar_ShowLatinLookalikes(bool show)
    {wxConfig::Get()->Write(wxT("greekSidebar_ShowLatinLookalikes"),
                            m_greekSidebar_ShowLatinLookalikes = show);}

  bool GreekSidebar_Show_mu() const
    {return m_greekSidebar_Show_mu;}
  void GreekSidebar_Show_mu(bool show)
    {wxConfig::Get()->Write(wxT("greekSidebar_Show_mu"),
                            m_greekSidebar_Show_mu = show);}

  wxString SymbolPaneAdditionalChars() const
    {return m_symbolPaneAdditionalChars;}
  void SymbolPaneAdditionalChars(wxString symbols)
    {wxConfig::Get()->Write(wxT("symbolPaneAdditionalChars"),
                            m_symbolPaneAdditionalChars = symbols);}

  
  //! Notify the user if maxima is idle?
  bool NotifyIfIdle() const
  { return m_notifyIfIdle; }

  void NotifyIfIdle(bool notify)
  {
    wxConfig::Get()->Write(wxT("notifyIfIdle"), m_notifyIfIdle = notify);
  }

  /*! Returns the maximum number of displayed digits

    m_displayedDigits is always >= 20, so we can guarantee the number we return to be unsigned.
   */
  long GetDisplayedDigits() const
  { return m_displayedDigits; }

  void SetDisplayedDigits(long displayedDigits)
  {
    wxASSERT_MSG(displayedDigits >= 0, _("Bug: Maximum number of digits that is to be displayed is too low!"));
    wxConfig::Get()->Write(wxT("displayedDigits"), m_displayedDigits = displayedDigits);
  }
  
  wxRect GetUpdateRegion() const {return m_updateRegion;}
  void SetUpdateRegion(wxRect rect){m_updateRegion = rect;}
  bool GetInsertAns() const
  { return m_insertAns; }

  void SetInsertAns(bool insertAns)
  {
    wxConfig::Get()->Write(wxT("insertAns"), m_insertAns = insertAns);
  }

  bool GetOpenHCaret() const
  { return m_openHCaret; }

  void SetOpenHCaret(bool openHCaret)
  {
    wxConfig::Get()->Write(wxT("openHCaret"), m_openHCaret = openHCaret);
  }

  bool RestartOnReEvaluation() const
  { return m_restartOnReEvaluation; }

  void RestartOnReEvaluation(bool arg)
  {
    wxConfig::Get()->Write(wxT("restartOnReEvaluation"), m_restartOnReEvaluation = arg);
  }

  //! Reads the size of the current worksheet's visible window. See SetCanvasSize
  wxSize GetCanvasSize() const
  { return m_canvasSize; }

  //! Sets the size of the current worksheet's visible window.
  void SetCanvasSize(wxSize siz)
  { m_canvasSize = siz; }

  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool ShowBrackets() const
  { return m_showBrackets; }

  bool ShowBrackets(bool show)
  { return m_showBrackets = show; }

  //! Prlong the cell brackets [displayed left to each group cell showing its extend]?
  bool PrintBrackets() const
  { return m_printBrackets; }

  showLabels GetLabelChoice() const
  { return m_showLabelChoice; }

  bool InvertBackground(){return m_invertBackground;}
  void InvertBackground(bool invert)
    {
    wxConfig::Get()->Write("invertBackground", m_invertBackground = invert);
    }
  
  //! Do we want to show maxima's automatic labels (%o1, %t1, %i1,...)?
  bool ShowAutomaticLabels() const
  { return (m_showLabelChoice < labels_useronly); }

  //! Do we want at all to show labels?
  bool UseUserLabels() const
  { return m_showLabelChoice > labels_automatic; }
  
  //! Do we want at all to show labels?
  bool ShowLabels() const
  { return m_showLabelChoice < labels_none; }

  //! Sets the value of the Configuration ChoiceBox that treads displaying labels
  void SetLabelChoice(showLabels choice)
  {
    wxConfig::Get()->Write(wxT("showLabelChoice"), (int) (m_showLabelChoice = choice));
  }

  bool PrintBrackets(bool print)
  {
    wxConfig::Get()->Write(wxT("printBrackets"), m_printBrackets = print);
    return print;
  }

  //! Autodetect maxima's location? (If false the user-specified location is used)
  bool AutodetectMaxima() const {return m_autodetectMaxima;}
  //! Autodetect maxima's location?
  void AutodetectMaxima(bool autodetectmaxima){wxConfig::Get()->Write(
      wxT("autodetectMaxima"),
      m_autodetectMaxima = autodetectmaxima);
  }

  //! Parameters to the maxima binary
  wxString MaximaParameters() const {return m_maximaParameters;}
  //! The parameters we pass to the maxima binary
  void MaximaParameters(wxString parameters){wxConfig::Get()->Write(
      "parameters",
      m_maximaParameters = parameters);
  }

  //! The auto-detected maxima location
  static wxString MaximaDefaultLocation();

  //! Returns the location of the maxima binary.
  wxString MaximaLocation() const;

  //! Returns the location of the maxima binary the user has selected.
  wxString MaximaUserLocation() const {return m_maximaUserLocation;}

  //! Sets the location of the maxima binary.
  void MaximaUserLocation(wxString maxima)
  {
    wxConfig::Get()->Write(wxT("maxima"), m_maximaUserLocation = maxima);
  }

  /*! Could a maxima binary be found in the path we expect it to be in?

    \param location The location to search for maxima in. 
    If location == wxEmptyString the default location from the configuration 
    is taken.
   */
  static bool MaximaFound(wxString location = wxEmptyString);

  //! Renumber out-of-order cell labels on saving.
  bool FixReorderedIndices() const
  { return m_fixReorderedIndices; }

  void FixReorderedIndices(bool fix)
  {
    wxConfig::Get()->Write(wxT("fixReorderedIndices"), m_fixReorderedIndices = fix);
  }

  //! Returns the URL MathJaX can be found at.
  wxString MathJaXURL() const {if(m_mathJaxURL_UseUser) return m_mathJaxURL; else return MathJaXURL_Auto();}
  wxString MathJaXURL_User() const { return m_mathJaxURL;}
  bool MathJaXURL_UseUser() const { return m_mathJaxURL_UseUser;}
  void MathJaXURL_UseUser(bool useUser){wxConfig::Get()->Write(wxT("mathJaxURL_UseUser"),
                                                               m_mathJaxURL_UseUser = useUser);}

  bool EnterEvaluates() const {return m_enterEvaluates;}
  void EnterEvaluates(bool enterEvaluates) {wxConfig::Get()->Write(wxT("enterEvaluates"),
                                                                m_enterEvaluates = enterEvaluates);}
  static wxString MathJaXURL_Auto() { return wxT("https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.6/MathJax.js?config=TeX-AMS_HTML");}
  //! Returns the URL MathJaX can be found at.
  void MathJaXURL(wxString url){wxConfig::Get()->Write(wxT("mathJaxURL"), m_mathJaxURL = url);}
  bool AntiAliasLines() const {return m_antiAliasLines;}
  void AntiAliasLines(bool antiAlias)
    {
      wxConfig::Get()->Write(wxT("antiAliasLines"), m_antiAliasLines = antiAlias );
    }

  bool CopyBitmap() const {return m_copyBitmap;}
  void CopyBitmap(bool copyBitmap)
    {
      wxConfig::Get()->Write(wxT("copyBitmap"), m_copyBitmap = copyBitmap );
    }
  
  bool CopyMathML() const {return m_copyMathML;}
  void CopyMathML(bool copyMathML)
    {
      wxConfig::Get()->Write(wxT("copyMathML"), m_copyMathML = copyMathML );
    }
  bool CopyMathMLHTML() const {return m_copyMathMLHTML;}
  void CopyMathMLHTML(bool copyMathMLHTML)
    {
      wxConfig::Get()->Write(wxT("copyMathMLHTML"), m_copyMathMLHTML = copyMathMLHTML );
    }
  bool CopyRTF() const {return m_copyRTF;}
  void CopyRTF(bool copyRTF)
    {
      wxConfig::Get()->Write(wxT("copyRTF"), m_copyRTF = copyRTF );
    }
  bool CopySVG() const {return m_copySVG;}
  void CopySVG(bool copySVG)
    {
      wxConfig::Get()->Write(wxT("copySVG"), m_copySVG = copySVG );
    }
  bool CopyEMF() const {return m_copyEMF;}
  void CopyEMF(bool copyEMF)
    {
      wxConfig::Get()->Write(wxT("copyEMF"), m_copyEMF = copyEMF );
    }
  bool UseSVG(){return m_useSVG;}
  void UseSVG(bool useSVG)
    {
      wxConfig::Get()->Write(wxT("useSVG"), m_useSVG = useSVG );
    }
  void ShowLength(long length)
    {
      wxConfig::Get()->Write(wxT("showLength"), m_showLength = length);
    }
  long ShowLength() const {return m_showLength;}

  //! Sets the default toolTip for new cells
  void SetDefaultCellToolTip(wxString defaultToolTip){m_defaultToolTip = defaultToolTip;}
  //! Gets the default toolTip for new cells
  wxString GetDefaultCellToolTip() const {return m_defaultToolTip;}
  //! Which way do we want to draw parenthesis?
  void SetGrouphesisDrawMode(drawMode mode){m_parenthesisDrawMode = mode;}

  void TocShowsSectionNumbers(bool showSectionNumbers)
    {
      wxConfig::Get()->Write(wxT("TOCshowsSectionNumbers"), (m_TOCshowsSectionNumbers = showSectionNumbers));
    }

  bool TocShowsSectionNumbers() const {return m_TOCshowsSectionNumbers;}

  void UseUnicodeMaths(bool useunicodemaths)
    {
      wxConfig::Get()->Write(wxT("useUnicodeMaths"), (m_useUnicodeMaths = useunicodemaths));
    }
  bool UseUnicodeMaths() const {return m_useUnicodeMaths;}

  drawMode GetParenthesisDrawMode();
  /*! Get the font for a given text style

    \param textStyle The text style to get the font for
    \param fontSize Only relevant for math cells: Super- and subscripts can have different
    font styles than the rest.
   */
  wxFont GetFont(TextStyle textStyle, long fontSize) const;

  //! Get the worksheet this configuration storage is valid for
  wxWindow *GetWorkSheet() const {return m_workSheet;}
  //! Set the worksheet this configuration storage is valid for
  void SetWorkSheet(wxWindow *workSheet){m_workSheet = workSheet;}

  long DefaultPort() const {return m_defaultPort;}
  void DefaultPort(long port){wxConfig::Get()->Write("defaultPort",m_defaultPort = port);}
  bool GetAbortOnError() const {return m_abortOnError;}
  void SetAbortOnError(bool abortOnError)
    {wxConfig::Get()->Write("abortOnError",m_abortOnError = abortOnError);}

  int GetLanguage() const {return m_language;}
  void SetLanguage(int language)
    {wxConfig::Get()->Write("language",m_language = language);}

  //! The maximum number of Megabytes of gnuplot sources we should store
  long MaxGnuplotMegabytes() const {return m_maxGnuplotMegabytes;}
  void MaxGnuplotMegabytes(long megaBytes)
    {wxConfig::Get()->Write("maxGnuplotMegabytes",m_maxGnuplotMegabytes = megaBytes);}

  bool OfferKnownAnswers() const {return m_offerKnownAnswers;}
  void OfferKnownAnswers(bool offerKnownAnswers)
    {wxConfig::Get()->Write("offerKnownAnswers",m_offerKnownAnswers = offerKnownAnswers);}
  
  wxString Documentclass() const {return m_documentclass;}
  void Documentclass(wxString clss){wxConfig::Get()->Write("documentclass",m_documentclass = clss);}
  wxString DocumentclassOptions() const {return m_documentclassOptions;}
  void DocumentclassOptions(wxString classOptions){wxConfig::Get()->Write("documentclassoptions",m_documentclassOptions = classOptions);}

  
  htmlExportFormat HTMLequationFormat() const {return m_htmlEquationFormat;}
  void HTMLequationFormat(htmlExportFormat HTMLequationFormat)
    {wxConfig::Get()->Write("HTMLequationFormat", (int) (m_htmlEquationFormat = HTMLequationFormat));}

  wxString FontName()const {return m_fontName;}
  void FontName(wxString name){wxConfig::Get()->Write("Style/Default/Style/Text/fontname",m_fontName = name);}
  void MathFontName(wxString name){wxConfig::Get()->Write("Style/Math/fontname",m_mathFontName = name);}
  wxString MathFontName()const {return m_mathFontName;}

  //! Update the list of fonts associated to the worksheet styles
  void UpdateWorksheetFonts();
  //! Get the font for the given worksheet style
  wxFont GetWorksheetFont(TextStyle style) const;
  //! Get the worksheet this configuration storage is valid for
  long GetAutosubscript_Num() const {return m_autoSubscript;}
  void SetAutosubscript_Num(long autosubscriptnum)
    {wxConfig::Get()->Write("autosubscript",m_autoSubscript = autosubscriptnum);}
  wxString GetAutosubscript_string() const;
  //! Determine the default background color of the worksheet
  wxColor DefaultBackgroundColor();
  //! Determine the default background color of editorcells
  wxColor EditorBackgroundColor();
  //! Do we want to save time by only redrawing the area currently shown on the screen?
  bool ClipToDrawRegion() const {return m_clipToDrawRegion;}
  //! Do we want to save time by only redrawing the area currently shown on the screen?
  void ClipToDrawRegion(bool clipToDrawRegion){m_clipToDrawRegion = clipToDrawRegion; m_forceUpdate = true;}
  //! Request adjusting the worksheet size?
  void AdjustWorksheetSize(bool adjust)
    { m_adjustWorksheetSizeNeeded = adjust; }
  bool AdjustWorksheetSize() const
    { return m_adjustWorksheetSizeNeeded; }
  void SetVisibleRegion(wxRect visibleRegion){m_visibleRegion = visibleRegion;}
  wxRect GetVisibleRegion() const {return m_visibleRegion;}
  void SetWorksheetPosition(wxPoint worksheetPosition){m_worksheetPosition = worksheetPosition;}
  wxPoint GetWorksheetPosition() const {return m_worksheetPosition;}
  wxString MaximaShareDir() const {return m_maximaShareDir;}
  void MaximaShareDir(wxString dir){m_maximaShareDir = dir;}
  void InLispMode(bool lisp){m_inLispMode = lisp;}
  bool InLispMode() const {return m_inLispMode;}
  Style m_styles[NUMBEROFSTYLES];
private:
  //! true = Autosave doesn't save into the current file.
  bool m_autoSaveAsTempFile;
  //! The number of the language wxMaxima uses.
  long m_language;
  //! Autodetect maxima's location?
  bool m_autodetectMaxima;
  //! The worksheet all cells are drawn on
  wxRect m_updateRegion;
  //! Has the font changed?
  bool m_fontChanged;
  //! Which objects do we want to convert into subscripts if they occur after an underscore?
  long m_autoSubscript;
  //! The worksheet this configuration storage is valid for
  wxWindow *m_workSheet;
  /*! Do these chars exist in the given font?

    wxWidgets currently doesn't define such a function. But we can do the following:
      - Test if any of these characters has the width or height 0 (or even less)
        which clearly indicates that this char doesn't exist.
      - Test if any two of the characters are equal when rendered as bitmaps: 
        If they are we most probably didn't get render real characters but rather
        render placeholders for characters.

      As these might be costly operations it is important to cache the result
      of this function.
   */
  bool CharsExistInFont(const wxFont &font, const wxString& chars);
  //! Caches the information on how to draw big parenthesis for GetParenthesisDrawMode().
  drawMode m_parenthesisDrawMode;
  wxString m_workingdir;

  wxString m_maximaUserLocation;
  //! Hide brackets that are not under the pointer
  bool m_hideBrackets;
  //! The scale for printing
  double m_printScale;
  //! The size of the canvas our cells have to be drawn on
  wxSize m_canvasSize;
  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool m_showBrackets;
  //! Prlong the cell brackets [displayed left to each group cell showing its extend]?
  bool m_printBrackets;
  /*! Replace a "*" by a centered dot?
    
    Normally we ask the parser for this piece of information. But during recalculation
    of widths while selecting text we don't know our parser.
   */
  bool m_changeAsterisk;
  //! Notify the user if maxima is idle
  bool m_notifyIfIdle;
  //! How many digits of a number we show by default?
  long m_displayedDigits;
  //! Automatically wrap long lines?
  long m_autoWrap;
  //! Automatically indent long lines?
  bool m_autoIndent;
  //! Do we want to automatically close parenthesis?
  bool m_matchParens;
  //! Do we want to automatically insert new cells containing a "%" at the end of every command?
  bool m_insertAns;
  //! Do we want to automatically open a new cell if maxima has finished evaluating its input?
  bool m_openHCaret;
  //! The width of input and output labels [in chars]
  long m_labelWidth;
  long m_indent;
  bool m_latin2greek;
  bool m_antiAliasLines;
  double m_zoomFactor;
  wxDC *m_dc;
  wxDC *m_antialiassingDC;
  wxString m_fontName;
  long m_mathFontSize;
  wxString m_mathFontName;
  wxString m_maximaShareDir;
  bool m_forceUpdate;
  bool m_clipToDrawRegion;
  bool m_outdated;
  wxString m_maximaParameters;
  wxString m_defaultToolTip;
  bool m_TeXFonts;
  bool m_keepPercent;
  bool m_restartOnReEvaluation;
  wxString m_fontCMRI, m_fontCMSY, m_fontCMEX, m_fontCMMI, m_fontCMTI;
  long m_clientWidth;
  long m_clientHeight;
  bool m_printing;
  long m_lineWidth_em;
  showLabels m_showLabelChoice;
  bool m_fixReorderedIndices;
  wxString m_mathJaxURL;
  bool m_mathJaxURL_UseUser;
  bool m_showCodeCells;
  bool m_copyBitmap;
  bool m_copyMathML;
  bool m_copyMathMLHTML;
  long m_showLength;
  //!< don't add ; in lisp mode
  bool m_inLispMode;
  bool m_enterEvaluates;
  bool m_useSVG;
  bool m_copyRTF;
  bool m_copySVG;
  bool m_copyEMF;
  bool m_TOCshowsSectionNumbers;
  bool m_useUnicodeMaths;
  bool m_indentMaths;
  bool m_abortOnError;
  bool m_hidemultiplicationsign;
  bool m_offerKnownAnswers;
  long m_defaultPort;
  long m_maxGnuplotMegabytes;
  wxString m_documentclass;
  wxString m_documentclassOptions;
  htmlExportFormat m_htmlEquationFormat;
  bool m_adjustWorksheetSizeNeeded;
  //! The rectangle of the worksheet that is currently visible.
  wxRect m_visibleRegion;
  //! The position of the worksheet in the wxMaxima window
  wxPoint m_worksheetPosition;

  wxColour m_defaultBackgroundColor;
  //! The brush the normal cell background is painted with
  wxBrush m_BackgroundBrush;
  wxBrush m_tooltipBrush;
  bool m_greekSidebar_ShowLatinLookalikes;
  bool m_greekSidebar_Show_mu;
  wxString m_symbolPaneAdditionalChars;
  bool m_invertBackground;
  wxFont m_worksheetFonts[NUMBEROFSTYLES];
};

//! Sets the configuration's "printing" flag until this class is left.
class Printing
{
public:
  explicit Printing(Configuration *configuration)
    {
      m_configuration = configuration;
      m_configuration->SetPrinting(true);
      m_configuration->ClipToDrawRegion(false);
    }
  ~Printing()
    {
      m_configuration->SetPrinting(false);
      m_configuration->ClipToDrawRegion(true);
    }
private:
  Configuration * m_configuration;
};

//! Clears the configuration's "Clip to draw region" flag until this class is left.
class NoClipToDrawRegion
{
public:
  explicit NoClipToDrawRegion(Configuration *configuration)
    {
      m_configuration = configuration;
      m_configuration->ClipToDrawRegion(false);
    }
  ~NoClipToDrawRegion()
    {
      m_configuration->ClipToDrawRegion(true);
    }
private:
  Configuration * m_configuration;
};

#endif // CONFIGURATION_H
