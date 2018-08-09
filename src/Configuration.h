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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <wx/wx.h>
#include <wx/config.h>
#include <wx/fontenum.h>

#include "TextStyle.h"
#include "Dirstructure.h"
#include "Setup.h"


#define MC_CELL_SKIP 0
#define MC_LINE_SKIP 2
#define MC_TEXT_PADDING 1

#define PAREN_OPEN_TOP_UNICODE     "\x239b"
#define PAREN_OPEN_EXTEND_UNICODE  "\x239c"
#define PAREN_OPEN_BOTTOM_UNICODE  "\x239d"
#define PAREN_CLOSE_TOP_UNICODE    "\x239e"
#define PAREN_CLOSE_EXTEND_UNICODE "\x239f"
#define PAREN_CLOSE_BOTTOM_UNICODE "\x23a0"

//! The width of the horizontally-drawn cursor
#define MC_HCARET_WIDTH 25

#if defined __WXMAC__
#define MC_EXP_INDENT 2
#define MC_MIN_SIZE 10
#define MC_MAX_SIZE 36
#else
#define MC_EXP_INDENT 4
#define MC_MIN_SIZE 8
#define MC_MAX_SIZE 36
#endif

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
  Dirstructure m_dirStructure;
  enum drawMode
  {
    ascii,              //!< Use ascii characters only
    assembled_unicode_fallbackfont,  //!< Unicode, fallbackfont 1
    assembled_unicode,  //!< Unicode, current font
    assembled_unicode_fallbackfont2,  //!< Unicode, fallbackfont 2
    handdrawn,           //!< A  parenthesis sign that was created using draw commands
    unknown
  };

  //! Set maxima's working directory
  void SetWorkingDirectory(wxString dir)
  { m_workingdir = dir; }

  wxString GetWorkingDirectory()
  { return m_workingdir; }

  void ReadConfig();

  /*! The constructor
    
    \param dc The drawing context that is to be used for drawing objects
   */
  Configuration(wxDC &dc);

  //! Set the drawing context that is currently active
  void SetContext(wxDC &dc)
  {
    m_dc = &dc;
    m_antialiassingDC = NULL;
  }

  void SetAntialiassingDC(wxDC &antialiassingDC)
    {m_antialiassingDC = &antialiassingDC;}

  void UnsetAntialiassingDC()
    {m_antialiassingDC = NULL;}

  ~Configuration();

  static double GetMinZoomFactor()
  { return 0.1; }

  static double GetMaxZoomFactor()
  { return 32.0; }

  /*! Extra space to leave between two equations in output cells.

    Extra space between equations is useful if we don't display labels that show
    which line begins a new equation and which line merely continues a multi-line
    equation.
   */
  double GetInterEquationSkip()
  {
    if (ShowAutomaticLabels())
      return 0;
    else
      return GetZoomFactor() * m_mathFontSize / 2;
  }

  int GetCellBracketWidth()
  {
    return (int) (GetZoomFactor() * 16);
  }

  //! Hide brackets that are not under the pointer?
  bool HideBrackets()
  { return m_hideBrackets; }

  //! Define if we want to hide brackets that are not under the pointer.
  void HideBrackets(bool hide)
  {
    wxConfig::Get()->Write(wxT("hideBrackets"), m_hideBrackets = hide);
  }

  //! Hide brackets that are not under the pointer?
  double PrintScale()
  { return m_printScale; }

  //! Define if we want to hide brackets that are not under the pointer.
  void PrintScale(double scale)
  {
    wxConfig::Get()->Write(wxT("printScale"), m_printScale = scale);
  }

  //! Sets the zoom factor the worksheet is displayed at
  void SetZoomFactor(double newzoom);

  //! Sets the zoom factor without storing the new value in the config file/registry.
  void SetZoomFactor_temporarily(double newzoom){m_zoomFactor = newzoom;}

  /*! Scales a distance [in pixels] according to the zoom factor

    Is used for displaying/printing/exporting of text/maths.
   */
  int Scale_Px(double px);

  //! Determines the zoom factor the worksheet is displayed at
  double GetZoomFactor()
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

  void SetBounds(int top, int bottom)
  {
    m_top = top;
    m_bottom = bottom;
  }

  int GetTop()
  {
    return m_top;
  }

  int GetBottom()
  {
    return m_bottom;
  }

  wxString GetFontName(int type = TS_DEFAULT);

  wxString GetSymbolFontName();

  wxColour GetColor(int st);

  wxFontWeight IsBold(int st);

  wxFontStyle IsItalic(int st);

  bool IsUnderlined(int st);

  void ReadStyle();

  void SetForceUpdate(bool force)
  {
    m_forceUpdate = force;
  }

  bool ForceUpdate()
  {
    return m_forceUpdate;
  }

  wxFontEncoding GetFontEncoding()
  {
    return m_fontEncoding;
  }

  int GetLabelWidth()
  { return m_labelWidth; }

  //! Get the indentation of GroupCells.
  int GetIndent()
  {
    if (m_indent < 0)
      return 3 * GetCellBracketWidth() / 2;
    else
      return m_indent;
  }

  //! How much vertical space is to be left between two group cells?
  int GetCursorWidth()
  {
    if (wxGetDisplayPPI().x / 45 < 1)
      return 1;
    else
      return wxGetDisplayPPI().x / 45;
  }

  //! The y position the worksheet starts at
  int GetBaseIndent()
  {
    if (GetCursorWidth() < 12)
      return 12;
    else
      return 4 + GetCursorWidth();
  }

  //! The vertical space between GroupCells
  int GetGroupSkip()
  {
    if (GetCursorWidth() < 10)
      return 20;
    else
      return 10 + GetCursorWidth();
  }

  /*! Set the indentation of GroupCells

    Normallly this parameter is automatically calculated
   */
  void SetIndent(int indent)
  { m_indent = indent; }

  //! Set the width of the visible window for GetClientWidth()
  void SetClientWidth(int width)
  { m_clientWidth = width; }

  //! Set the height of the visible window for GetClientHeight()
  void SetClientHeight(int height)
  { m_clientHeight = height; }

  //! Returns the width of the visible portion of the worksheet
  int GetClientWidth()
  { return m_clientWidth; }

  //! Returns the height of the visible portion of the worksheet
  int GetClientHeight()
  { return m_clientHeight; }

  //! Calculates the default line width for the worksheet
  double GetDefaultLineWidth()
  {
    if (GetZoomFactor() < 1.0)
      return 1.0;
    else
      return GetZoomFactor();
  }

  //! The minimum sensible line width in withs of a letter.
  int LineWidth_em()
  { return m_lineWidth_em; }

  //! Set the minimum sensible line width in widths of a lletter.
  void LineWidth_em(int width)
  { m_lineWidth_em = width; }

  //! Returns the maximum sensible width for a text line [in characters]:
  // On big 16:9 screens text tends to get \b very wide before it hits the right margin.
  // But text blocks that are 1 meter wide and 2 cm high feel - weird.
  int GetLineWidth()
  {
    if (m_clientWidth <= m_zoomFactor * double(m_defaultFontSize) * LineWidth_em() * m_zoomFactor)
      return m_clientWidth;
    else
      return (const int) (double(m_defaultFontSize) * LineWidth_em() * m_zoomFactor);
  }

  int GetDefaultFontSize()
  { return m_defaultFontSize; }

  int GetMathFontSize()
  { return m_mathFontSize; }

  //! Do we want to have automatic line breaks for text cells?
  bool GetAutoWrap()
  { return m_autoWrap > 0; }

  //! Do we want to have automatic line breaks for code cells?
  bool GetAutoWrapCode()
  { return false; }

  /*! Sets the auto wrap mode
    \param autoWrap 
     - 0: No automatic line breaks
     - 1: Automatic line breaks only for text cells
     - 2: Automatic line breaks for text and code cells.
  */
  void SetAutoWrap(int autoWrap)
  {
    wxConfig::Get()->Write(wxT("autoWrapMode"), m_autoWrap = autoWrap);
  }

  //! Do we want automatic indentation?
  bool GetAutoIndent()
  { return m_autoIndent; }

  void SetAutoIndent(bool autoIndent)
  {
    wxConfig::Get()->Write(wxT("autoIndent"), m_autoIndent = autoIndent);
  }

  int GetFontSize(int st)
  {
    if (st == TS_TEXT || st == TS_SUBSUBSECTION || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return m_styles[st].fontSize;
    return 0;
  }

  void Outdated(bool outdated)
  { m_outdated = outdated; }

  bool CheckTeXFonts()
  { return m_TeXFonts; }

  bool CheckKeepPercent()
  { return m_keepPercent; }

  const wxString GetTeXCMRI()
  { return m_fontCMRI; }

  const wxString GetTeXCMSY()
  { return m_fontCMSY; }

  const wxString GetTeXCMEX()
  { return m_fontCMEX; }

  const wxString GetTeXCMMI()
  { return m_fontCMMI; }

  const wxString GetTeXCMTI()
  { return m_fontCMTI; }

  bool ShowCodeCells()
  { return m_showCodeCells; }

  void ShowCodeCells(bool show);

  void SetPrinter(bool printer)
  { m_printer = printer; }

  bool GetPrinter()
  { return m_printer; }

  bool GetMatchParens()
  { return m_matchParens; }

  bool GetChangeAsterisk()
  { return m_changeAsterisk; }

  void SetChangeAsterisk(bool changeAsterisk)
  {
    wxConfig::Get()->Write(wxT("changeAsterisk"), m_changeAsterisk = changeAsterisk);
  }
  
  //! Notify the user if maxima is idle?
  bool NotifyIfIdle()
  { return m_notifyIfIdle; }

  void NotifyIfIdle(bool notify)
  {
    wxConfig::Get()->Write(wxT("notifyIfIdle"), m_notifyIfIdle = notify);
  }

  /*! Returns the maximum number of displayed digits

    m_displayedDigits is always >= 20, so we can guarantee the number we return to be unsigned.
   */
  int GetDisplayedDigits()
  { return m_displayedDigits; }

  void SetDisplayedDigits(int displayedDigits)
  {
    wxASSERT_MSG(displayedDigits >= 20, _("Bug: Maximum number of digits that is to be displayed is too low!"));
    wxConfig::Get()->Write(wxT("displayedDigits"), m_displayedDigits = displayedDigits);
  }

  bool GetInsertAns()
  { return m_insertAns; }

  void SetInsertAns(bool insertAns)
  {
    wxConfig::Get()->Write(wxT("insertAns"), m_insertAns = insertAns);
  }

  bool GetOpenHCaret()
  { return m_openHCaret; }

  void SetOpenHCaret(bool openHCaret)
  {
    wxConfig::Get()->Write(wxT("openHCaret"), m_openHCaret = openHCaret);
  }

  bool RestartOnReEvaluation()
  { return m_restartOnReEvaluation; }

  void RestartOnReEvaluation(bool arg)
  {
    wxConfig::Get()->Write(wxT("restartOnReEvaluation"), m_restartOnReEvaluation = arg);
  }

  //! Reads the size of the current worksheet's visible window. See SetCanvasSize
  wxSize GetCanvasSize()
  { return m_canvasSize; }

  //! Sets the size of the current worksheet's visible window.
  void SetCanvasSize(wxSize siz)
  { m_canvasSize = siz; }

  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool ShowBrackets()
  { return m_showBrackets; }

  bool ShowBrackets(bool show)
  { return m_showBrackets = show; }

  //! Print the cell brackets [displayed left to each group cell showing its extend]?
  bool PrintBrackets()
  { return m_printBrackets; }

  int GetLabelChoice()
  { return m_showLabelChoice; }

  //! Do we want to show maxima's automatic labels (%o1, %t1, %i1,...)?
  bool ShowAutomaticLabels()
  { return (m_showLabelChoice < 2); }

  //! Do we want at all to show labels?
  bool UseUserLabels()
  { return m_showLabelChoice > 0; }

  //! Do we want at all to show labels?
  bool ShowLabels()
  { return m_showLabelChoice < 3; }

  //! Sets the value of the Configuration ChoiceBox that treads displaying labels
  void SetLabelChoice(int choice)
  {
    wxConfig::Get()->Write(wxT("showLabelChoice"), m_showLabelChoice = choice);
  }

  bool PrintBrackets(bool print)
  {
    wxConfig::Get()->Write(wxT("printBrackets"), m_printBrackets = print);
    return print;
  }

  //! Returns the location of the maxima binary.
  wxString MaximaLocation()
  { return m_maximaLocation; }

  //! Sets the location of the maxima binary.
  void MaximaLocation(wxString maxima)
  {
    wxConfig::Get()->Write(wxT("maxima"), m_maximaLocation = maxima);
  }

  /*! Could a maxima binary be found in the path we expect it to be in?

    \param location The location to search for maxima in. 
    If location == wxEmptyString the default location from the configuration 
    is taken.
   */
  bool MaximaFound(wxString location = wxEmptyString);

  //! Renumber out-of-order cell labels on saving.
  bool FixReorderedIndices()
  { return m_fixReorderedIndices; }

  void FixReorderedIndices(bool fix)
  {
    wxConfig::Get()->Write(wxT("fixReorderedIndices"), m_fixReorderedIndices = fix);
  }

  //! Returns the URL MathJaX can be found at.
  wxString MathJaXURL(){ return m_mathJaxURL;}
  //! Returns the URL MathJaX can be found at.
  void MathJaXURL(wxString url){wxConfig::Get()->Write(wxT("mathJaxURL"), m_mathJaxURL = url);}
  bool AntiAliasLines(){return m_antiAliasLines;}
  void AntiAliasLines(bool antiAlias)
    {
      wxConfig::Get()->Write(wxT("antiAliasLines"), m_antiAliasLines = antiAlias );
    }

  bool CopyBitmap(){return m_copyBitmap;}
  void CopyBitmap(bool copyBitmap)
    {
      wxConfig::Get()->Write(wxT("copyBitmap"), m_copyBitmap = copyBitmap );
    }
  bool CopyMathML(){return m_copyMathML;}
  void CopyMathML(bool copyMathML)
    {
      wxConfig::Get()->Write(wxT("copyMathML"), m_copyMathML = copyMathML );
    }
  bool CopyMathMLHTML(){return m_copyMathMLHTML;}
  void CopyMathMLHTML(bool copyMathMLHTML)
    {
      wxConfig::Get()->Write(wxT("copyMathMLHTML"), m_copyMathMLHTML = copyMathMLHTML );
    }
  bool CopyRTF(){return m_copyRTF;}
  void CopyRTF(bool copyRTF)
    {
      wxConfig::Get()->Write(wxT("copyRTF"), m_copyRTF = copyRTF );
    }
  bool CopySVG(){return m_copySVG;}
  void CopySVG(bool copySVG)
    {
      wxConfig::Get()->Write(wxT("copySVG"), m_copySVG = copySVG );
    }
  bool CopyEMF(){return m_copyEMF;}
  void CopyEMF(bool copyEMF)
    {
      wxConfig::Get()->Write(wxT("copyEMF"), m_copyEMF = copyEMF );
    }
  void ShowLength(int length)
    {
      wxConfig::Get()->Write(wxT("showLength"), m_showLength = length );
    }
  int ShowLength(){return m_showLength;}

  //! Sets the default toolTip for new cells
  void SetDefaultMathCellToolTip(wxString defaultToolTip){m_defaultToolTip = defaultToolTip;}
  //! Gets the default toolTip for new cells
  wxString GetDefaultMathCellToolTip(){return m_defaultToolTip;}
  //! Which way do we want to draw parenthesis?
  void SetGrouphesisDrawMode(drawMode mode){m_parenthesisDrawMode = mode;}

  void TocShowsSectionNumbers(bool showSectionNumbers)
    {
      wxConfig::Get()->Write(wxT("TOCshowsSectionNumbers"), (m_TOCshowsSectionNumbers = showSectionNumbers));
    }

  bool TocShowsSectionNumbers(){return m_TOCshowsSectionNumbers;}

  void UseUnicodeMaths(bool useunicodemaths)
    {
      wxConfig::Get()->Write(wxT("useUnicodeMaths"), (m_useUnicodeMaths = useunicodemaths));
    }
  bool UseUnicodeMaths(){return m_useUnicodeMaths;}

  drawMode GetGrouphesisDrawMode();
  /*! Get the font for a given text style

    \param textStyle The text style to get the font for
    \param fontSize Only relevant for math cells: Super- and subscripts can have different
    font styles than the rest.
   */
  wxFont GetFont(int textStyle, int fontSize);

  //! Get the worksheet this configuration storage is valid for
  wxWindow *GetWorkSheet(){return m_workSheet;}
  //! Set the worksheet this configuration storage is valid for
  void SetWorkSheet(wxWindow *workSheet){m_workSheet = workSheet;}

  //! Get the autosave interval [in milliseconds]; 0 = no autosave
  int AutoSaveInterval(){if (m_autoSaveInterval > 1) return m_autoSaveInterval; else return 0;}
  //! Set the autosave interval [in milliseconds]; 0 = noautosave
  void AutoSaveInterval(int miliseconds){wxConfig::Get()->Write(wxT("autoSaveInterval"),
                                                  (m_autoSaveInterval = miliseconds / 1000 / 60)
      );
  }
  
  //! Get the worksheet this configuration storage is valid for
  int GetAutosubscript_Num(){return m_autoSubscript;}
  void SetAutosubscript_Num(int autosubscriptnum)
    {wxConfig::Get()->Write("autosubscript",m_autoSubscript = autosubscriptnum);}
  wxString GetAutosubscript_string();
  //! Determine the default background color of the worksheet
  wxColor DefaultBackgroundColor(){return m_defaultBackgroundColor;}
private:
  /*! The interval between auto-saves (in milliseconds). 

    Values <10000 mean: Auto-save is off.
  */
  int m_autoSaveInterval;
  //! Which objects do we want to convert into subscripts if they occur after an underscore?
  int m_autoSubscript;
  //! The worksheet this configuration storage is valid for
  wxWindow *m_workSheet;
  //! A replacement for the non-existing "==" operator for wxBitmaps.
  bool IsEqual(wxBitmap bitmap1, wxBitmap bitmap2);
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
  bool CharsExistInFont(wxFont font, wxString char1, wxString char2, wxString char3);
  //! Caches the information on how to draw big parenthesis for GetGrouphesisDrawMode().
  drawMode m_parenthesisDrawMode;
  wxString m_workingdir;
  wxString m_maximaLocation;
  //! Hide brackets that are not under the pointer
  bool m_hideBrackets;
  //! The scale for printing
  double m_printScale;
  //! The size of the canvas our cells have to be drawn on
  wxSize m_canvasSize;
  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool m_showBrackets;
  //! Print the cell brackets [displayed left to each group cell showing its extend]?
  bool m_printBrackets;
  /*! Replace a "*" by a centered dot?
    
    Normally we ask the parser for this piece of information. But during recalculation
    of widths while selecting text we don't know our parser.
   */
  bool m_changeAsterisk;
  //! Notify the user if maxima is idle
  bool m_notifyIfIdle;
  //! How many digits of a number we show by default?
  int m_displayedDigits;
  //! Automatically wrap long lines?
  int m_autoWrap;
  //! Automatically indent long lines?
  bool m_autoIndent;
  //! Do we want to automatically close parenthesis?
  bool m_matchParens;
  //! Do we want to automatically insert new cells conaining a "%" at the end of every command?
  bool m_insertAns;
  //! Do we want to automatically open a new cell if maxima has finished evaluating its input?
  bool m_openHCaret;
  //! The width of input and output labels [in chars]
  int m_labelWidth;
  int m_indent;
  bool m_antiAliasLines;
  double m_zoomFactor;
  wxDC *m_dc;
  wxDC *m_antialiassingDC;
  int m_top, m_bottom;
  wxString m_fontName;
  int m_defaultFontSize, m_mathFontSize;
  wxString m_mathFontName;
  bool m_forceUpdate;
  bool m_outdated;
  wxString m_defaultToolTip;
  bool m_TeXFonts;
  bool m_keepPercent;
  bool m_restartOnReEvaluation;
  wxString m_fontCMRI, m_fontCMSY, m_fontCMEX, m_fontCMMI, m_fontCMTI;
  int m_clientWidth;
  int m_clientHeight;
  wxFontEncoding m_fontEncoding;
  style m_styles[STYLE_NUM];
  bool m_printer;
  int m_lineWidth_em;
  int m_showLabelChoice;
  bool m_fixReorderedIndices;
  wxString m_mathJaxURL;
  bool m_showCodeCells;
  bool m_copyBitmap;
  bool m_copyMathML;
  bool m_copyMathMLHTML;
  int m_showLength;
  bool m_copyRTF;
  bool m_copySVG;
  bool m_copyEMF;
  bool m_TOCshowsSectionNumbers;
  bool m_useUnicodeMaths;
  wxColour m_defaultBackgroundColor;
};

#endif // CONFIGURATION_H
