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

#include "precomp.h"
#include <wx/wx.h>
#include <wx/config.h>
#include <wx/display.h>
#include <wx/fontenum.h>
#include <wx/hashmap.h>
#include "dialogs/LoggingMessageDialog.h"
#include "cells/TextStyle.h"
#include <cstdint>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <random>
#include <list>
#include <vector>
#include <algorithm>
#include <wx/wupdlock.h>
#include <wx/webview.h>
#ifdef __WXMSW__
#include <wx/msw/webview_ie.h>
#endif

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
//! How much to make the font smaller for the sum index and limit?
#define SUM_DEC 2

//! The width of the horizontally-drawn cursor
#define MC_HCARET_WIDTH 25

#define MC_EXP_INDENT static_cast<wxCoord>(Scale_Px(2))
static constexpr AFontSize MC_MIN_SIZE{ 6.0f };
static constexpr AFontSize MC_MAX_SIZE{ 48.0f };

// The minimal and maximal label width
#define LABELWIDTH_MIN 3
#define LABELWIDTH_MAX 10
class Cell;

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
  enum maximaHelpFormat{
    maxima = 0,
    frontend = 1,
    browser = 2
  };
  //! The export formats we support for HTML equations
  enum htmlExportFormat
  {
    mathJaX_TeX = 0,
    bitmap = 1,
    mathML_mathJaX = 2,
    svg = 3,
    html_export_invalidChoice
  };

  enum showLabels : int8_t
  {
    labels_automatic = 0,
    labels_prefer_user = 1,
    labels_useronly = 2,
    labels_none = 3,
    labels_invalidSelection
  };

  enum drawMode
  {
    ascii,              //!< Use ascii characters only
    handdrawn           //!< A  parenthesis sign that was created using draw commands
  };

  enum InitOpt
  {
    none,
    temporary          //!< This configuration is temporary and shouldn't redetect Maxima etc.
  };

  enum mathDisplayMode
  {
    display_2d,
    display_2dASCII,
    display_2dUNICODE,
    display_1dASCII
  };

  typedef std::unordered_map <wxString, bool, wxStringHash> StringBoolHash;
  typedef std::unordered_map <wxString, wxString, wxStringHash> RenderablecharsHash;
  typedef std::unordered_map <wxString, int, wxStringHash> StringHash;
  /*! All maxima operator names we know
   */
  StringHash m_maximaOperators;
  //! Coincides name with a operator known to maxima?
  bool IsOperator(wxString name){return !(m_maximaOperators.find(name) == m_maximaOperators.end());}
  const wxEnvVariableHashMap& MaximaEnvVars() const {return m_maximaEnvVars;}
  wxEnvVariableHashMap m_maximaEnvVars;

  //! Does maxima output 1D, 2D or 2D ASCII equations?
  mathDisplayMode DisplayMode() const {return m_displayMode;}
  //! Tell the config if maxima outputs 1D, 2D or 2D ASCII equations, currently
  void DisplayMode(mathDisplayMode mode ) {m_displayMode = mode;}

  //! Set maxima's working directory
  void SetWorkingDirectory(wxString dir)
    { m_workingdir = std::move(dir); }

  //! Get maxima's working directory
  wxString GetWorkingDirectory() const
    { return m_workingdir; }

  //! Read the config from the wxConfig object
  void ReadConfig();

  /*! The constructor

    \param dc The drawing context that is to be used for drawing objects
  */
  explicit Configuration(wxDC *dc = {}, InitOpt options = none);

  //! Reset the whole configuration to its default values
  void ResetAllToDefaults();

  //! Set the drawing context that is currently active
  void SetRecalcContext(wxDC &dc)
    {
      m_dc = &dc;
    }
  void UnsetContext() {m_dc = NULL;}

  //! Set the brush to be used for the worksheet background
  void SetBackgroundBrush(const wxBrush &brush);
  //! Use a typewriter font in wizard entry fields that can contain maxima commands?
  bool FixedFontInTextControls() const {return m_fixedFontTC;}
  //! Use a typewriter font in wizard entry fields that can contain maxima commands?
  void FixedFontInTextControls(bool fixed) {m_fixedFontTC = fixed;}
  //! Get the brush to be used for the worksheet background
  wxBrush GetBackgroundBrush() const {return m_BackgroundBrush;}
  //! Get the brush to be used for worksheet objects that provide a mouse-over tooltip
  wxBrush GetTooltipBrush() const {return m_tooltipBrush;}

  virtual ~Configuration();

  /*! Where to store the configuration.

    Empty means: "Use the default location", which on MS Windows is somewhere in the registry.
  */
  static wxString m_configfileLocation_override;

  //! The list of names for the worksheet's text styles
  static std::unordered_map<TextStyle, wxString> m_styleNames;
  using EscCodeContainer = std::unordered_map<wxString, wxString, wxStringHash>;
  using EscCodeIterator = EscCodeContainer::const_iterator;

  //! Retrieve a symbol for the escape codes that can be typed after hitting the Escape key in the worksheet.
  static const wxString &GetEscCode(const wxString &key);
  //! Iterators over the escape code list
  static EscCodeIterator EscCodesBegin();
  //! Iterators over the escape code list
  static EscCodeIterator EscCodesEnd();

  //! The lower limit we allow for the zoom factor
  static double GetMinZoomFactor()
    { return 0.1; }

  //! The upper limit we allow for the zoom factor
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
        return GetZoomFactor() * m_styles[TS_MATH].GetFontSize() / 2;
    }

  //! The width we allocate for our cell brackets
  wxCoord GetCellBracketWidth() const
    {
      return static_cast<wxCoord>(GetZoomFactor() * 16);
    }

  //! Hide brackets that are not under the pointer?
  bool HideBrackets() const
    { return m_hideBrackets; }

  //! Define if we want to hide brackets that are not under the pointer.
  void HideBrackets(bool hide){m_hideBrackets = hide;}

  //! Hide brackets that are not under the pointer?
  double PrintScale() const
    { return m_printScale; }

  //! Define if we want to hide brackets that are not under the pointer.
  void PrintScale(double scale){m_printScale = scale;}

  void PrintMargin_Top(double margin){m_printMargin_Top = margin;}
  void PrintMargin_Bot(double margin){m_printMargin_Bot = margin;}
  void PrintMargin_Left(double margin){m_printMargin_Left = margin;}
  void PrintMargin_Right(double margin){m_printMargin_Right = margin;}
  double PrintMargin_Top() const {return m_printMargin_Top;}
  double PrintMargin_Bot() const {return m_printMargin_Bot;}
  double PrintMargin_Left() const {return m_printMargin_Left;}
  double PrintMargin_Right() const {return m_printMargin_Right;}

  //! Sets the zoom factor the worksheet is displayed at
  void SetZoomFactor(double newzoom);

  //! Sets the zoom factor without storing the new value in the config file/registry.
  void SetZoomFactor_temporarily(double newzoom){
    if(m_zoomFactor != newzoom)
      RecalculateForce();
    m_zoomFactor = newzoom; ;
  }

  /*! Scales a distance [in pixels] according to the zoom factor

    Is used for displaying/printing/exporting of text/maths.
  */
  wxCoord Scale_Px(double px) const;
  AFontSize Scale_Px(AFontSize size) const;

  //! Determines the zoom factor the worksheet is displayed at
  double GetZoomFactor() const
    { return m_zoomFactor; }

  //! Get a drawing context suitable for size calculations
  wxDC *GetRecalcDC() const
    { return m_dc; }

  void SetRecalcDC(wxDC *dc)
    { m_dc = dc; }

  wxString GetFontName(TextStyle ts = TS_CODE_DEFAULT) const;

  // cppcheck-suppress functionStatic
  // cppcheck-suppress functionConst
  wxString GetSymbolFontName() const;

  wxFontWeight IsBold(long st) const;

  wxFontStyle IsItalic(long st) const;

  bool IsUnderlined(long st) const {return m_styles[st].IsUnderlined();}

  /*! Get the width of worksheet labels [in unscaled pixels]

    \todo Should the font size be regarded here?
  */
  wxCoord GetLabelWidth() const
    { return m_labelWidth * 14; }

  //! Get the width of worksheet labels [in chars]
  long LabelWidth() const
    { return m_labelWidth; }
  //! Set the width of worksheet labels [in chars]
  void LabelWidth(long labelWidth)
    {
      if(m_labelWidth != labelWidth)
        RecalculateForce();
      m_labelWidth = labelWidth;
    }

  //! Get the indentation of GroupCells.
  wxCoord GetIndent() const
    {
      if (m_indent < 0)
        return 3 * GetCellBracketWidth() / 2;
      else
        return m_indent;
    }

  /*! Get the resolution

    During recalculation the drawing context doesn't know the display's resolution
    so we need to do some additional tricks here.
  */
  wxSize GetPPI() const;
  int AutosaveMinutes() const {return m_autoSaveMinutes;}
  void AutosaveMinutes(int minutes){m_autoSaveMinutes = minutes;}

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
        RecalculateForce();
      m_indent = indent;
    }

  bool IncrementalSearch() const {return m_incrementalSearch;}


  void IncrementalSearch(bool incrementalSearch) { m_incrementalSearch = incrementalSearch; }

  struct CharsExist {
    wxString chars;
    bool exist;
    CharsExist(const wxString &chars, bool exist) : chars(chars), exist(exist) {}
  };
  std::vector<CharsExist> m_charsInFont;

  //! To be called if a font has changed
  void FontChanged()
    {
      m_charsInFont.clear();
      RecalculateForce();
    }

  //! Calculates the default line width for the worksheet
  double GetDefaultLineWidth() const
    {
      if (GetZoomFactor() < 1.0)
        return 1.0;
      else
        return GetZoomFactor();
    }

  //! The minimum sensible line width in widths of a letter.
  long LineWidth_em() const
    {
      if(!m_printing)
        return m_lineWidth_em;
      else
        return 10000;
    }

  bool AutoSaveAsTempFile() const {return m_autoSaveAsTempFile;}
  void AutoSaveAsTempFile(bool asTempFile){m_autoSaveAsTempFile = asTempFile;}

  //! Set the minimum sensible line width in widths of a letter.
  void LineWidth_em(long width)
    { m_lineWidth_em = width; }

  //! Returns the maximum sensible width for a text line [in characters]:
  // On big 16:9 screens text tends to get \b very wide before it hits the right margin.
  // But text blocks that are 1 meter wide and 2 cm high feel - weird.
  long GetLineWidth() const;

  bool SaveUntitled() const { return m_saveUntitled;}
  void SaveUntitled(bool save) {m_saveUntitled = save;}

  bool CursorJump() const { return m_cursorJump;}
  void CursorJump(bool save){m_cursorJump = save;}

  bool NumpadEnterEvaluates() const { return m_numpadEnterEvaluates;}
  void NumpadEnterEvaluates(bool eval){m_numpadEnterEvaluates = eval;}

  bool SaveImgFileName() const { return m_saveImgFileName;}
  void SaveImgFileName(bool save) { m_saveImgFileName = save;}

  //! Do we want to have automatic line breaks for text cells?
  bool GetAutoWrap() const
    { return m_autoWrap > 0; }

  // cppcheck-suppress functionStatic
  //! Do we want to have automatic line breaks for code cells?
  static bool GetAutoWrapCode()
    { return false; }

  /*! Sets the auto wrap mode
    \param autoWrap
    - 0: No automatic line breaks
    - 1: Automatic line breaks only for text cells
    - 2: Automatic line breaks for text and code cells.
  */
  void SetAutoWrap(long autoWrap){m_autoWrap = autoWrap;}

  //! Do we want automatic indentation?
  bool GetAutoIndent() const
    { return m_autoIndent; }

  void SetAutoIndent(bool autoIndent){m_autoIndent = autoIndent;}

  //! Do we want to indent all maths?
  bool IndentMaths() const {return m_indentMaths;}
  void IndentMaths(bool indent){
    if(m_indentMaths != indent)
      RecalculateForce();
    m_indentMaths = indent;}
  AFontSize GetFontSize(TextStyle st) const { return m_styles[st].GetFontSize(); }

  static const wxString &GetStyleName(TextStyle textStyle);

  /*! Reads the style settings

    If a file name is given the settings are read from a file.
  */

  void ReadStyles(const wxString &file = {});

  /*! Saves the style settings

    If a file name is given the settings are written to a file.
  */
  void WriteStyles(const wxString &file = {});
  void WriteStyles(wxConfigBase *config);
  void WriteSettings(const wxString &file = {});
  void MakeStylesConsistent();
  void Outdated(bool outdated)
    { m_outdated = outdated; }

  //! Do we want to display "%e" as "%e" and "%pi" as "%pi"?
  bool CheckKeepPercent() const
    { return m_keepPercent; }

  //! Do we want to display "%e" as "%e" and "%pi" as "%pi"?
  void SetKeepPercent(bool keepPercent)
    {
      if(m_keepPercent != keepPercent)
        RecalculateForce();
      m_keepPercent = keepPercent;
    }

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
  static wxColour InvertColour(wxColour col);

  /*! Make this color differ from the background by a noticeable amount

    Useful for black/white background theme changes
  */
  wxColor MakeColorDifferFromBackground(wxColor color);

  bool UsePngCairo() const { return m_usepngCairo;}
  void UsePngCairo(bool usepngCairo) { m_usepngCairo = usepngCairo;}

  bool GetMatchParens() const { return m_matchParens; }
  void SetMatchParens(bool matchParens) { m_matchParens = matchParens; }
  bool ShowMatchingParens() const { return m_showMatchingParens; }
  void ShowMatchingParens(bool show) { m_showMatchingParens = show; }

  //! Use Unicode centered dots for multiplication signs?
  bool GetChangeAsterisk() const{ return m_changeAsterisk; }

  //! Use Unicode centered dots for multiplication signs?
  void SetChangeAsterisk(bool changeAsterisk) {
    if(m_changeAsterisk != changeAsterisk)
      RecalculateForce();
    m_changeAsterisk = changeAsterisk;
  }

  bool HidemultiplicationSign() const {return m_hidemultiplicationsign;}

  void HidemultiplicationSign(bool show) {
    if(m_hidemultiplicationsign != show)
      RecalculateForce();
    m_hidemultiplicationsign = show;
  }

  bool Latin2Greek() const
    {return m_latin2greek;}

  void Latin2Greek(bool latin2greek) {
    if(m_latin2greek != latin2greek)
      RecalculateForce();
    m_latin2greek = latin2greek;
  }

  bool GreekSidebar_ShowLatinLookalikes() const {return m_greekSidebar_ShowLatinLookalikes;}
  void GreekSidebar_ShowLatinLookalikes(bool show){m_greekSidebar_ShowLatinLookalikes = show;}

  bool GreekSidebar_Show_mu() const {return m_greekSidebar_Show_mu;}
  void GreekSidebar_Show_mu(bool show) {m_greekSidebar_Show_mu = show;}

  wxString SymbolPaneAdditionalChars() const
    {return m_symbolPaneAdditionalChars;}
  void SymbolPaneAdditionalChars(wxString symbols) {m_symbolPaneAdditionalChars = std::move(symbols);}

  //! Notify the user if maxima is idle?
  bool NotifyIfIdle() const
    { return m_notifyIfIdle; }

  void NotifyIfIdle(bool notify) {m_notifyIfIdle = notify;}

  /*! Returns the maximum number of displayed digits

    m_displayedDigits is always >= 20, so we can guarantee the number we return to be unsigned.
  */
  long GetDisplayedDigits() const
    { return m_displayedDigits; }

  void SetDisplayedDigits(long displayedDigits)
    {
      wxASSERT_MSG(displayedDigits >= 0, _("Bug: Maximum number of digits that is to be displayed is too low!"));
      if(m_displayedDigits != displayedDigits)
        RecalculateForce();
      m_displayedDigits = displayedDigits;
    }

  //! Stores the information about a file we need to write during the save process
  class FileToSave
  {
  public:
    FileToSave(const wxString &filename, const wxMemoryBuffer &data):
      m_data(data),
      m_filename(filename)
      {
      }
    const wxString FileName() const{return m_filename;}
    const wxMemoryBuffer Data() const{return m_data;}
  private:
    const wxMemoryBuffer m_data;
    const wxString m_filename;
  };

  //! Stores the information about a file we need to write during the save process
  class TextsnippetToDraw
  {
  public:
    TextsnippetToDraw(const wxPoint &pos, const wxString &text, const wxColor &color):
      m_pos(pos),
      m_text(text),
      m_color(color)
      {
      }
    const wxPoint  Pos() const{return m_pos;}
    const wxString Text() const{return m_text;}
    const wxColor  Color() const{return m_color;}
  private:
    const wxPoint  m_pos;
    const wxString m_text;
    const wxColor  m_color;
  };

  FileToSave PopFileToSave();
  void PushFileToSave(const wxString &filename, const wxMemoryBuffer &data)
    { m_filesToSave.emplace_front(FileToSave(filename, data)); }

  wxRect GetUpdateRegion() const {return m_updateRegion;}
  const std::list<FileToSave> &GetFilesToSave() const {return m_filesToSave;}
  void ClearFilesToSave () { m_filesToSave.clear();}
  void SetUpdateRegion(wxRect rect){m_updateRegion = rect;}

  //! Whether any part of the given rectangle is within the current update region,
  //! or true if drawing is not clipped to update region.
  bool InUpdateRegion(wxRect rect) const;

  bool GetInsertAns() const
    { return m_insertAns; }

  void SetInsertAns(bool insertAns){ m_insertAns = insertAns; }

  bool GetOpenHCaret() const
    { return m_openHCaret; }

  void SetOpenHCaret(bool openHCaret){ m_openHCaret = openHCaret; }

  bool RestartOnReEvaluation() const
    { return m_restartOnReEvaluation; }

  void RestartOnReEvaluation(bool arg){ m_restartOnReEvaluation = arg; }

  //! Reads the size of the current worksheet's visible window. See SetCanvasSize
  wxSize GetCanvasSize() const
    { return m_canvasSize; }

  //! Sets the size of the current worksheet's visible window.
  void SetCanvasSize(wxSize siz)
    {
      if(m_canvasSize.GetWidth() != siz.GetWidth())
        RecalculateForce();
      m_canvasSize = siz;
    }

  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool ShowBrackets() const
    { return m_showBrackets; }

  bool ShowBrackets(bool show)
    {
      if(m_showBrackets != show)
        RecalculateForce();
      return m_showBrackets = show;
    }

  //! Print the cell brackets [displayed left to each group cell showing its extend]?
  bool PrintBrackets() const
    { return m_printBrackets; }

  showLabels GetLabelChoice() const
    { return m_showLabelChoice; }

  bool ShowInputLabels() const {return m_showInputLabels;}
  void ShowInputLabels(bool show) {
    if(m_showInputLabels != show)
      RecalculateForce();
    m_showInputLabels = show;
  }

  bool InvertBackground() const {return m_invertBackground;}
  void InvertBackground(bool invert){ m_invertBackground = invert; }

  long UndoLimit(){return std::max(m_undoLimit, static_cast<long>(0));}
  void UndoLimit(long limit){ m_undoLimit = limit; }

  long RecentItems(){return std::max(m_recentItems, static_cast<long>(0));}
  void RecentItems(long items){ m_recentItems = items; }

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
  void SetLabelChoice(showLabels choice) {
    if(m_showLabelChoice != choice)
      RecalculateForce();
    m_showLabelChoice = choice;
  }

  bool PrintBrackets(bool print)
    {
      m_printBrackets = print;
      return print;
    }

  //! Autodetect maxima's location? (If false the user-specified location is used)
  bool AutodetectMaxima() const {return m_autodetectMaxima;}
  //! Autodetect maxima's location?
  void AutodetectMaxima(bool autodetectmaxima){m_autodetectMaxima = autodetectmaxima;}

  //! Parameters to the maxima binary
  wxString MaximaParameters() const {return m_maximaParameters;}
  //! The parameters we pass to the maxima binary
  void MaximaParameters(wxString parameters){m_maximaParameters = std::move(parameters);}

  //! Dynamic memory parameter to maxima binary
  bool MaximaEnableCustomDynamicMemory() const {return m_maximaEnableCustomDynamicMemory;}
  //! The dynamic memory parameter for the maxima binary
  void MaximaEnableCustomDynamicMemory(bool enable) {m_maximaEnableCustomDynamicMemory = enable;}
  //! Dynamic memory parameter to maxima binary if MaximaEnableCustomDynamicMemory() is true. Otherwise this parameter is ignored
  int MaximaCustomDynamicMemory() const {return m_maximaCustomDynamicMemory;}
  //! The dynamic memory parameter for the maxima binary if MaximaEnableCustomDynamicMemory() is true. Otherwise this parameter is ignored
  void MaximaCustomDynamicMemory(int memory) {m_maximaCustomDynamicMemory = memory;}

  //! The auto-detected maxima location
  static wxString MaximaDefaultLocation();

  //! Returns the location of the maxima binary.
  wxString MaximaLocation() const;

  //! Returns the location of the maxima binary the user has selected.
  wxString MaximaUserLocation() const {return m_maximaUserLocation;}

  //! Sets the location of the maxima binary.
  void MaximaUserLocation(wxString maxima) { m_maximaUserLocation = std::move(maxima); }

  //! Autodetect the web browser? (If false the user-specified location is used)
  bool AutodetectHelpBrowser() const {return m_autodetectHelpBrowser;}
  //! Autodetect the web browser?
  void AutodetectHelpBrowser(bool autodetect){m_autodetectHelpBrowser = autodetect;}

  //! Use the internal help browser? If not a external web browser is used.
  bool InternalHelpBrowser() const {return m_useInternalHelpBrowser && OfferInternalHelpBrowser();}
  //! Use the internal help browser? If not a external web browser is used.
  void InternalHelpBrowser(bool useInternalHelpBrowser)
    {m_useInternalHelpBrowser = useInternalHelpBrowser;}

  //! Prefer the single-page manual?
  bool SinglePageManual() const {return m_singlePageManual;}
  //! Prefer the single-page manual?
  void SinglePageManual(bool singlePageManual)
    {m_singlePageManual = singlePageManual;}

  //! Returns the location of the web browser the user has selected.
  wxString HelpBrowserUserLocation() const {return m_helpBrowserUserLocation;}

  //! Sets the location of the web browser the user has detected.
  void HelpBrowserUserLocation(wxString helpBrowser) { m_helpBrowserUserLocation = std::move(helpBrowser);}

  /*! Could a maxima binary be found in the path we expect it to be in?

    \param location The location to search for maxima in.
    If location == wxEmptyString the default location from the configuration
    is taken.
  */
  static wxString FindProgram(const wxString &location);

  //! Renumber out-of-order cell labels on saving.
  bool FixReorderedIndices() const
    { return m_fixReorderedIndices; }

  void FixReorderedIndices(bool fix) { m_fixReorderedIndices = fix;}

  //! Returns the URL MathJaX can be found at.
  wxString MathJaXURL() const {
    if(m_mathJaxURL_UseUser)
      return m_mathJaxURL;
    else
      return MathJaXURL_Auto();}
  wxString MathJaXURL_User() const { return m_mathJaxURL;}
  bool MathJaXURL_UseUser() const { return m_mathJaxURL_UseUser;}
  void MathJaXURL_UseUser(bool useUser){m_mathJaxURL_UseUser = useUser;}

  bool EnterEvaluates() const {return m_enterEvaluates;}
  void EnterEvaluates(bool enterEvaluates) {m_enterEvaluates = enterEvaluates;}
  static wxString MathJaXURL_Auto() { return wxS("https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js");}
  //! Returns the URL MathJaX can be found at.
  void MathJaXURL(wxString url){m_mathJaxURL = std::move(url);}

  bool CopyBitmap() const {return m_copyBitmap;}
  void CopyBitmap(bool copyBitmap){ m_copyBitmap = copyBitmap; }

  bool CopyMathML() const {return m_copyMathML;}
  void CopyMathML(bool copyMathML){ m_copyMathML = copyMathML;}
  bool CopyMathMLHTML() const {return m_copyMathMLHTML;}
  void CopyMathMLHTML(bool copyMathMLHTML){ m_copyMathMLHTML = copyMathMLHTML; }
  bool HideMarkerForThisMessage(wxString message);
  void HideMarkerForThisMessage(const wxString &message, bool hide)
    {m_hideMarkerForThisMessage[message] = hide;}
  bool CopyRTF() const {return m_copyRTF;}
  void CopyRTF(bool copyRTF) { m_copyRTF = copyRTF; }
  bool CopySVG() const {return m_copySVG;}
  void CopySVG(bool copySVG) { m_copySVG = copySVG; }
  bool CopyEMF() const {return m_copyEMF;}
  void CopyEMF(bool copyEMF) { m_copyEMF = copyEMF; }
  bool UseSVG() const {return m_useSVG;}
  void UseSVG(bool useSVG) { m_useSVG = useSVG ;}
  //! The choice for the "maximum output length to display" setting
  void ShowLength(long length) {
    m_showLength = length;
  }
  //! The choice for the "maximum output length to display" setting
  long ShowLength() const {return m_showLength;}
  //! The "maximum output length to display" setting in cells
  std::size_t ShowLength_Bytes() const;

  //! Which way do we want to draw parenthesis?
  void SetParenthesisDrawMode(drawMode mode) {
    if(m_parenthesisDrawMode != mode)
      RecalculateForce();
    m_parenthesisDrawMode = mode;
  }

  void TocShowsSectionNumbers(bool showSectionNumbers) { m_TOCshowsSectionNumbers = showSectionNumbers; }

  bool TocShowsSectionNumbers() const {return m_TOCshowsSectionNumbers;}

  void UseUnicodeMaths(bool useunicodemaths)
    { m_useUnicodeMaths = useunicodemaths; }
  bool UseUnicodeMaths() const {return m_useUnicodeMaths;}

  StringBoolHash m_hideMarkerForThisMessage;

  /*! Get the text Style for a given text style identifier.

    \param textStyle The text style to resolve the style for.
  */
  const Style *GetStyle(TextStyle textStyle) const { return &m_styles[textStyle]; }
  /*! Get the text Style for a given text style identifier.

    Theoretically GetStyle and GetWritableStyle wouldn't collide if they had the
    same name. But I am afraid if all developers know how to deal with the situation
    that performance degrades if a const is missing while the rest works fine.
    \param textStyle The text style to resolve the style for.
  */
  Style *GetWritableStyle(TextStyle textStyle) { return &m_styles[textStyle]; }

  //! Get the worksheet this configuration storage is valid for
  wxWindow *GetWorkSheet() const {return m_workSheet;}
  //! Set the worksheet this configuration storage is valid for
  void SetWorkSheet(wxWindow *workSheet);

  long DefaultPort() const {return m_defaultPort;}
  void DefaultPort(long port){m_defaultPort = port;}
  bool GetAbortOnError() const {return m_abortOnError;}
  void SetAbortOnError(bool abortOnError) {m_abortOnError = abortOnError;}

  long GetLanguage() const {return m_language;}
  void SetLanguage(long language) {m_language = language;}

  //! The maximum number of Megabytes of gnuplot sources we should store
  long MaxGnuplotMegabytes() const {return m_maxGnuplotMegabytes;}
  void MaxGnuplotMegabytes(long megaBytes)
    {m_maxGnuplotMegabytes = megaBytes;}

  bool OfferKnownAnswers() const {return m_offerKnownAnswers;}
  void OfferKnownAnswers(bool offerKnownAnswers)
    {m_offerKnownAnswers = offerKnownAnswers;}

  wxString Documentclass() const {return m_documentclass;}
  void Documentclass(wxString clss){m_documentclass = std::move(clss);}
  wxString DocumentclassOptions() const {return m_documentclassOptions;}
  void DocumentclassOptions(wxString classOptions){m_documentclassOptions = std::move(classOptions);}


  htmlExportFormat HTMLequationFormat() const {return m_htmlEquationFormat;}
  void HTMLequationFormat(htmlExportFormat HTMLequationFormat)
    {m_htmlEquationFormat = HTMLequationFormat;}

  AFontSize GetDefaultFontSize() const        { return m_styles[TS_CODE_DEFAULT].GetFontSize(); }
  AFontSize GetMathFontSize() const           { return m_styles[TS_MATH].GetFontSize(); }

  //! Get the worksheet this configuration storage is valid for
  long GetAutosubscript_Num() const {return m_autoSubscript;}
  void SetAutosubscript_Num(long autosubscriptnum) {m_autoSubscript = autosubscriptnum;}
  wxString GetAutosubscript_string() const;
  //! Determine the default background color of the worksheet
  wxColor DefaultBackgroundColor();
  //! Determine the default background color of editorcells
  wxColor EditorBackgroundColor();
  //! Do we want to save time by only redrawing the area currently shown on the screen?
  bool ClipToDrawRegion() const {return m_clipToDrawRegion;}
  //! Do we want to save time by only redrawing the area currently shown on the screen?
  void ClipToDrawRegion(bool clipToDrawRegion){m_clipToDrawRegion = clipToDrawRegion; m_forceUpdate = true;}
  void SetVisibleRegion(wxRect visibleRegion){
    if(m_visibleRegion.GetWidth() != visibleRegion.GetWidth())
      RecalculateForce();
    m_visibleRegion = visibleRegion;
  }
  wxRect GetVisibleRegion() const {return m_visibleRegion;}
  void SetWorksheetPosition(wxPoint worksheetPosition){m_worksheetPosition = worksheetPosition;}
  wxPoint GetWorksheetPosition() const {return m_worksheetPosition;}
  wxString MaximaShareDir() const {return m_maximaShareDir;}
  void MaximaShareDir(wxString dir){m_maximaShareDir = std::move(dir);}
  wxString MaximaDemoDir() const {return m_maximaDemoDir;}
  void MaximaDemoDir(wxString dir){m_maximaDemoDir = std::move(dir);}
  void InLispMode(bool lisp){m_inLispMode = lisp;}
  bool InLispMode() const {return m_inLispMode;}
  void BitmapScale(int factor){m_bitmapScale = factor;}
  int BitmapScale() const {return m_bitmapScale;}
  void DefaultPlotHeight(int px){m_defaultPlotHeight = px;}
  int DefaultPlotHeight() const {return m_defaultPlotHeight;}
  void DefaultPlotWidth(int px){m_defaultPlotWidth = px;}
  int DefaultPlotWidth() const {return m_defaultPlotWidth;}
  void DefaultFramerate(int fps){m_defaultFramerate = fps;}
  int DefaultFramerate() const {return m_defaultFramerate;}
  void TocDepth(int depth){m_tocDepth = depth;}
  int TocDepth() const {return m_tocDepth;}
  bool TeXExponentsAfterSubscript() const {return m_TeXExponentsAfterSubscript;}
  void TeXExponentsAfterSubscript(bool ExponentsAfterSubscript)
    {m_TeXExponentsAfterSubscript = ExponentsAfterSubscript;}
  bool UsePartialForDiff() const {return m_usePartialForDiff;}
  void UsePartialForDiff(bool usePartialForDiff)
    {m_usePartialForDiff = usePartialForDiff;}
  //! Record that this cell has been drawn to ReportMultipleRedraws()
  void NotifyOfCellRedraw(const Cell *cell);
  //! Clear the memory of ReportMultipleRedraws()
  void ClearAndEnableRedrawTracing();
  /*! Report if a cell has been redrawn 2 or more times during a simple Draw() command

    Only used in debug mode; If a cell is drawn and then re-drawn in the same draw
    command this most likely means that a 2D-displayed cell draws its sub-cells,
    but didn't remove them from the Cell::NextToDraw()-list of cells to draw
    after this cell has been drawn.
  */
  void ReportMultipleRedraws();
  //! If we decide that the HTML browser in the sidebar doesn't work for every platform...
  bool OfferInternalHelpBrowser() const;
  bool WrapLatexMath() const {return m_wrapLatexMath;}
  void WrapLatexMath(bool wrapLatexMath){m_wrapLatexMath = wrapLatexMath;}
  bool AllowNetworkHelp() const {return m_allowNetworkHelp;}
  void AllowNetworkHelp(bool allowNetworkHelp){m_allowNetworkHelp = allowNetworkHelp;}
  bool ShowAllDigits() const {return m_showAllDigits;}
  void ShowAllDigits(bool shw){
    if(m_showAllDigits != shw)
      RecalculateForce();
    m_showAllDigits = shw;
  }
  bool LineBreaksInLongNums() const {return m_lineBreaksInLongNums;}
  void LineBreaksInLongNums(bool brk){
    if(m_lineBreaksInLongNums != brk)
      RecalculateForce();
    m_lineBreaksInLongNums = brk;
  }
  int  MaxClipbrdBitmapMegabytes() const {return m_maxClipbrd_BitmapMegabytes;}
  void MaxClipbrdBitmapMegabytes(int maxClipbrd_BitmapMegabytes)
    {m_maxClipbrd_BitmapMegabytes = maxClipbrd_BitmapMegabytes;}

  void MaximaUsesHtmlBrowser(bool maximaUsesHhtmlBrowser){m_maximaUsesHhtmlBrowser = maximaUsesHhtmlBrowser;}
  bool MaximaUsesHtmlBrowser() const {return m_maximaUsesHhtmlBrowser;}
  void MaximaUsesWxmaximaBrowser(bool maximaUsesWxmaximaBrowser){m_maximaUsesWxmaximaBrowser = maximaUsesWxmaximaBrowser;}
  bool MaximaUsesWxmaximaBrowser() const {return m_maximaUsesWxmaximaBrowser && OfferInternalHelpBrowser();}
  void ExportContainsWXMX(bool exportContainsWXMX){m_exportContainsWXMX = exportContainsWXMX;}
  bool ExportContainsWXMX() const {return m_exportContainsWXMX;}
  void WizardTab(long tab){m_wizardTab = tab;}
  long WizardTab() const {return m_wizardTab;}

  void Display2d_Unicode(bool unicode){m_display2d_Unicode = unicode;}
  bool Display2d_Unicode() const {return m_display2d_Unicode;}

  #ifdef __WXMSW__
  bool UseWGnuplot() const {return m_useWgnuplot;}
  void UseWGnuplot(bool usewgnuplot) {m_useWgnuplot = usewgnuplot;}
  #endif
  wxString TexPreamble() const {return m_texPreamble;}
  void TexPreamble(wxString texPreamble) {m_texPreamble = std::move(texPreamble);}

  void SetMaximaVersion(const wxString &version){m_maximaVersion = version;}
  wxString GetMaximaVersion() const {return m_maximaVersion;}
  void SetMaximaArch(const wxString &arch){m_maximaArch = arch;}
  wxString GetMaximaArch() const {return m_maximaArch;}
  void SetLispVersion(const wxString &version){m_lispVersion = version;}
  wxString GetLispVersion() const {return m_lispVersion;}
  void SetLispType(const wxString &type){m_lispType = type;}
  wxString GetLispType() const {return m_lispType;}

  Style m_styles[NUMBEROFSTYLES];
  //! Initialize the text styles on construction.
  void InitStyles();
  //! True if we are confident that the font renders this char
  bool FontRendersChar(wxUniChar ch, const wxFont &font = *wxNORMAL_FONT);
  wxTextCtrl *LastActiveTextCtrl() const { return m_lastActiveTextCtrl; }
  void LastActiveTextCtrl(wxTextCtrl *last);

  //! Which styles affect how code is displayed?
  const std::vector<TextStyle> &GetCodeStylesList() const {return m_codeStyles;}
  //! Which styles affect how math output is displayed?
  const std::vector<TextStyle> &GetMathStylesList() const {return m_2dMathStyles;}
  //! Which styles affect only colors?
  const std::vector<TextStyle> &GetColorOnlyStylesList() const {return m_colorOnlyStyles;}
  bool StyleAffectsCode(TextStyle style) const;
  bool StyleAffectsMathOut(TextStyle style) const;
  bool StyleAffectsColorOnly(TextStyle style) const;
  //! true means: The system's config storage has changed since the configuration has been read
  bool UpdateNeeded() const;
  //! Enable costly checks
  static void SetDebugmode(){m_debugMode = true;}
  //! Enable costly checks?
  static bool GetDebugmode(){return m_debugMode;}
  maximaHelpFormat MaximaHelpFormat() const;
  void MaximaHelpFormat(maximaHelpFormat format) {m_maximaHelpFormat = format;}

private:
  wxString m_maximaArch;
  wxString m_lispVersion;
  wxString m_lispType;
  wxString m_maximaVersion;
  //! The options this class got at initialization time
  InitOpt m_initOpts;
  /*! The id of the current configuration

    If the ID in the operating system's config storage differs from this one someone
    has changed the configuration.
  */
  long m_configId;
public:
  //! Our random device
  std::random_device m_rd;
  //! Our random engine
  std::default_random_engine m_eng;
  /*! A counter that increases every time we need to recalculate all worksheet cells

    If that counter eventually wraps that should be fine: The cells will have been
    recalculated before that and if someone trying to do a speed run with wxMaxima
    manages to accumulate billions of config changes before the worksheet manages
    to recalculate itself the worst thing that can happen is a visual glitch.
  */
  std::int_fast32_t CellCfgCnt() const {return m_cellCfgCnt;}
  void RecalculateForce() { m_cellCfgCnt++; }
  static bool UseThreads(){return m_use_threads;}
  static void UseThreads(bool use){m_use_threads = use;}
  static void SetMaximaLang(const wxString &LANG){m_maxima_LANG = LANG;}
  static wxString GetMaximaLang(){return m_maxima_LANG;}
private:
  //! Which LANG environment variable to communicate to maxima?
  static wxString m_maxima_LANG;
  //! Which styles affect how code is displayed?
  std::vector<TextStyle> m_codeStyles;
  //! Which styles affect how math output is displayed?
  std::vector<TextStyle> m_2dMathStyles;
  //! Which styles affect only colors?
  std::vector<TextStyle> m_colorOnlyStyles;
  std::list<FileToSave> m_filesToSave;
  RenderablecharsHash m_renderableChars;
  RenderablecharsHash m_nonRenderableChars;
  //! True if drawing the char this button displays alters at least one pixel
  static bool FontDisplaysChar(wxUniChar ch, const wxFont &font = *wxNORMAL_FONT);
  //! True if drawing the char this button displays differs visibly from otherChar
  static bool CharVisiblyDifferent(wxChar ch, wxChar otherChar, const wxFont &font = *wxNORMAL_FONT);
  mathDisplayMode m_displayMode = display_2d;
  using CellRedrawTrace = std::vector<const Cell*>;
  static bool m_debugMode;
  bool m_showInputLabels;
  long m_wizardTab;
  bool m_display2d_Unicode;
  bool m_usePartialForDiff;
  bool m_maximaUsesHhtmlBrowser;
  bool m_maximaUsesWxmaximaBrowser;
  //! true = Autosave doesn't save into the current file.
  bool m_autoSaveAsTempFile;
  //! The number of the language wxMaxima uses.
  long m_language;
  //! Autodetect maxima's location?
  bool m_autodetectMaxima;
  //! Autodetect the help browser?
  bool m_autodetectHelpBrowser;
  //! Use the internal help browser?
  bool m_useInternalHelpBrowser;
  //! Prefer the single-page manual?
  bool m_singlePageManual;
  //! The worksheet all cells are drawn on
  wxRect m_updateRegion;
  //! Do we want to use incremental search?
  bool m_incrementalSearch;
  //! Which objects do we want to convert into subscripts if they occur after an underscore?
  long m_autoSubscript;
  //! The worksheet this configuration storage is valid for
  wxWindow *m_workSheet = NULL;
  //! A drawing context that knows the text sizes for the worksheet
  std::unique_ptr<wxClientDC> m_worksheetDC;
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
//  bool CharsExistInFont(const wxFont &font, const wxString& chars);
  //! Caches the information on how to draw big parenthesis for GetParenthesisDrawMode().
  bool m_wrapLatexMath;
  bool m_allowNetworkHelp;
  bool m_exportContainsWXMX;
  wxString m_texPreamble;

  drawMode m_parenthesisDrawMode;
  wxString m_workingdir;
  bool m_TeXExponentsAfterSubscript;
  wxString m_helpBrowserUserLocation;
  wxString m_maximaUserLocation;
  //! Hide brackets that are not under the pointer
  bool m_hideBrackets;
  //! The scale for printing
  double m_printScale;
  double m_printMargin_Top;
  double m_printMargin_Bot;
  double m_printMargin_Left;
  double m_printMargin_Right;
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
  //! Always show all digits of all numbers?
  bool m_showAllDigits;
  //! Allow linebreaks in numbers that are longer than a line?
  bool m_lineBreaksInLongNums;
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
  double m_zoomFactor;
  wxDC *m_dc;
  wxString m_maximaShareDir;
  wxString m_maximaDemoDir;
  bool m_forceUpdate;
  bool m_clipToDrawRegion = true;
  bool m_outdated;
  wxString m_maximaParameters;
  bool m_maximaEnableCustomDynamicMemory{false};
  int m_maximaCustomDynamicMemory{1000};
  bool m_keepPercent;
  bool m_restartOnReEvaluation;
  wxString m_fontCMRI, m_fontCMSY, m_fontCMEX, m_fontCMMI, m_fontCMTI;
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
  bool m_usepngCairo;
  bool m_enterEvaluates;
  bool m_useSVG;
  bool m_fixedFontTC;
  bool m_copyRTF;
  bool m_copySVG;
  bool m_copyEMF;
  bool m_TOCshowsSectionNumbers;
  bool m_useUnicodeMaths;
  bool m_indentMaths;
  bool m_abortOnError;
  bool m_showMatchingParens;
  bool m_hidemultiplicationsign;
  bool m_offerKnownAnswers;
  long m_defaultPort;
  long m_maxGnuplotMegabytes;
  long m_defaultPlotHeight;
  long m_defaultPlotWidth;
  bool m_saveUntitled;
  bool m_cursorJump;
  bool m_numpadEnterEvaluates;
  bool m_saveImgFileName;
  /*! A vector containing pointers to all cells the current draw command hit

    Only used in debug mode. There it is used in order to determine if any
    cell is drawn twice - which most likely means that a 2D-displayed cell
    draws its sub-cells, but didn't remove them from the list of cells to draw
    after this cell has been drawn.
  */
  std::unique_ptr<CellRedrawTrace> m_cellRedrawTrace;
  wxString m_documentclass;
  wxString m_documentclassOptions;
  htmlExportFormat m_htmlEquationFormat;
  //! The rectangle of the worksheet that is currently visible.
  wxRect m_visibleRegion;
  //! The position of the worksheet in the wxMaxima window
  wxPoint m_worksheetPosition;

  wxColour m_defaultBackgroundColor;
  //! The brush the normal cell background is painted with
  wxBrush m_BackgroundBrush;
  wxBrush m_tooltipBrush;
  bool m_greekSidebar_ShowLatinLookalikes;
  #ifdef __WXMSW__
  bool m_useWgnuplot = false;
  #endif
  bool m_greekSidebar_Show_mu;
  wxString m_symbolPaneAdditionalChars;
  bool m_invertBackground;
  long m_undoLimit;
  long m_recentItems;
  int m_bitmapScale;
  int m_defaultFramerate;
  int m_tocDepth;
  int m_maxClipbrd_BitmapMegabytes;
  int m_autoSaveMinutes;
  wxString m_wxMathML_Filename;
  maximaHelpFormat m_maximaHelpFormat;
  wxTextCtrl *m_lastActiveTextCtrl = NULL;
  std::int_fast32_t m_cellCfgCnt = 0;
  static bool m_use_threads;
};

//! Sets the configuration's "printing" flag until this class is left.
class Printing
{
public:
  explicit Printing(Configuration *configuration)
    {
      m_configuration = configuration;
      m_configuration->SetPrinting(true);
    }
  virtual ~Printing()
    {
      m_configuration->SetPrinting(false);
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
  virtual ~NoClipToDrawRegion()
    {
      m_configuration->ClipToDrawRegion(true);
    }
private:
  Configuration * m_configuration;
};

#endif // CONFIGURATION_H
