// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2020 Kuba Ober <kuba@bertec.com>
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
#include <wx/fdrepdlg.h>
#include <wx/fontenum.h>
#include <wx/hashmap.h>
#include "LoggingMessageDialog.h"
#include "TextStyle.h"
#include <unordered_map>
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

  In order to make all cells temporarily listen to a different set of configuration
  than the default one all that has to be done is to create a new configuration 
  object that contains the right settings for printing/export as bitmap or
  similar.
 */
class Configuration final
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
    handdrawn,          //!< A  parenthesis sign that was created using draw commands
    unknown
  };

  void SetWorkingDirectory(const wxString &dir) {m_workingdir = dir;}
  const wxString &GetWorkingDirectory() const   {return m_workingdir;}

  //! Update the configuration cache from the configuration file / registry
  void ReadConfig();

  /*! Reads the style settings
   * If a file name is given the settings are read from a file.
   */
  void ReadStyles(const wxString &file = {});

  /*! Saves the style settings
   * If a file name is given the settings are written to a file.
   */
  void WriteStyles(const wxString &file = {});

  /*! The constructor
    
    \param dc The drawing context that is to be used for drawing objects
   */
  explicit Configuration(wxDC *dc = {});
  ~Configuration();

  //
  // Stored and Cached Configuration Settings
  // Those settings are retained in the configuration file or registry.
  //
  // These entries should be kept in alphabetic order of the m_.... cache members.

  bool GetAbortOnError() const            {return m_abortOnError;}
  void SetAbortOnError(bool abortOnError) {CWrite(m_abortOnError, abortOnError);}
  bool AntiAliasLines() const         {return m_antiAliasLines;}
  void AntiAliasLines(bool antiAlias) {CWrite(m_antiAliasLines, antiAlias);}
  //! Autodetect maxima's location? (If false the user-specified location is used)
  bool AutodetectMaxima() const                {return m_autodetectMaxima;}
  //! Autodetect maxima's location?
  void AutodetectMaxima(bool autodetectmaxima) {CWrite(m_autodetectMaxima, autodetectmaxima);}
  //! Do we want automatic indentation?
  bool GetAutoIndent() const          {return m_autoIndent;}
  void SetAutoIndent(bool autoIndent) {CWrite(m_autoIndent, autoIndent);}
  bool AutoSaveAsTempFile() const          {return m_autoSaveAsTempFile;}
  void AutoSaveAsTempFile(bool asTempFile) {CWrite(m_autoSaveAsTempFile, asTempFile);}
  //! Get the worksheet this configuration storage is valid for
  long GetAutosubscript_Num() const                {return m_autoSubscript;}
  void SetAutosubscript_Num(long autosubscriptnum) {CWrite(m_autoSubscript, autosubscriptnum);}
  const wxString &GetAutosubscript_string() const;
  //! Do we want to have automatic line breaks for text cells?
  bool GetAutoWrap() const     {return m_autoWrap > 0;}
  // cppcheck-suppress functionStatic
  //! Do we want to have automatic line breaks for code cells?
  bool GetAutoWrapCode() const {return false;}
  /*! Sets the auto wrap mode TODO this should be an enum
    \param autoWrap
     - 0: No automatic line breaks
     - 1: Automatic line breaks only for text cells
     - 2: Automatic line breaks for text and code cells.
  */
  void SetAutoWrap(long autoWrap) {CWrite(m_autoWrap, autoWrap);};
  bool GetChangeAsterisk() const              {return m_changeAsterisk;}
  void SetChangeAsterisk(bool changeAsterisk) {CWrite(m_changeAsterisk, changeAsterisk);}
  bool CopyBitmap() const          {return m_copyBitmap;}
  void CopyBitmap(bool copyBitmap) {CWrite(m_copyBitmap, copyBitmap);}
  bool CopyEMF() const       {return m_copyEMF;}
  void CopyEMF(bool copyEMF) {CWrite(m_copyEMF, copyEMF);}
  bool CopyMathML() const          {return m_copyMathML;}
  void CopyMathML(bool copyMathML) {CWrite(m_copyMathML, copyMathML);}
  bool CopyMathMLHTML() const              {return m_copyMathMLHTML;}
  void CopyMathMLHTML(bool copyMathMLHTML) {CWrite(m_copyMathMLHTML, copyMathMLHTML);}
  bool CopyRTF() const       {return m_copyRTF;}
  void CopyRTF(bool copyRTF) {CWrite(m_copyRTF, copyRTF);}
  bool CopySVG() const       {return m_copySVG;}
  void CopySVG(bool copySVG) {CWrite(m_copySVG, copySVG);}
  long DefaultPort() const   {return m_defaultPort;}
  void DefaultPort(long port){CWrite(m_defaultPort, port);}
  /*! Returns the maximum number of displayed digits

    m_displayedDigits is always >= 20, so we can guarantee the number we return to be unsigned.
   */
  long GetDisplayedDigits() const               {return m_displayedDigits;}
  void SetDisplayedDigits(long displayedDigits) {CWrite(m_displayedDigits, displayedDigits);}
  const wxString &Documentclass() const    {return m_documentclass;}
  void Documentclass(const wxString &clss) {CWrite(m_documentclass, clss);}
  const wxString &DocumentclassOptions() const           {return m_documentclassOptions;}
  void DocumentclassOptions(const wxString &classOptions){CWrite(m_documentclassOptions, classOptions);}
  bool EnterEvaluates() const              {return m_enterEvaluates;}
  void EnterEvaluates(bool enterEvaluates) {CWrite(m_enterEvaluates, enterEvaluates);}
  wxFindReplaceFlags FindFlags() const     {return m_findFlags;}
  void FindFlags(wxFindReplaceFlags flags) {CWrite(m_findFlags, flags);}
  //! Renumber out-of-order cell labels on saving.
  bool FixReorderedIndices() const   { return m_fixReorderedIndices; }
  void FixReorderedIndices(bool fix) {CWrite(m_fixReorderedIndices, fix);}
  int GetDefaultFontSize() const        {return m_styles[TS_DEFAULT].FontSize();}
  void SetDefaultFontSize(int fontSize) {CWrite(m_styles[TS_DEFAULT].m_fontSize, fontSize);}
  bool GreekSidebar_ShowLatinLookalikes() const    {return m_greekSidebar_ShowLatinLookalikes;}
  void GreekSidebar_ShowLatinLookalikes(bool show) {CWrite(m_greekSidebar_ShowLatinLookalikes, show);}
  bool GreekSidebar_Show_mu() const    {return m_greekSidebar_Show_mu;}
  void GreekSidebar_Show_mu(bool show) {CWrite(m_greekSidebar_Show_mu, show);}
  //! Hide brackets that are not under the pointer?
  bool HideBrackets() const    {return m_hideBrackets;}
  //! Define if we want to hide brackets that are not under the pointer.
  void HideBrackets(bool hide) {CWrite(m_hideBrackets, hide);}
  bool HidemultiplicationSign() const    {return m_hidemultiplicationsign;}
  void HidemultiplicationSign(bool show) {CWrite(m_hidemultiplicationsign, show);}
  htmlExportFormat HTMLequationFormat() const                  {return m_htmlEquationFormat;}
  void HTMLequationFormat(htmlExportFormat HTMLequationFormat) {CWrite(m_htmlEquationFormat, HTMLequationFormat);}
  //! Do we want to indent all maths?
  bool IndentMaths() const      {return m_indentMaths;}
  void IndentMaths(bool indent) {CWrite(m_indentMaths, indent);}
  bool GetInsertAns() const         {return m_insertAns;}
  void SetInsertAns(bool insertAns) {CWrite(m_insertAns, insertAns);}
  bool InvertBackground()            {return m_invertBackground;}
  void InvertBackground(bool invert) {CWrite(m_invertBackground, invert);}
  bool CheckKeepPercent() const {return m_keepPercent;}
  void KeepPercent(bool val)    {CWrite(m_keepPercent, val);}
  long GetLabelWidth() const {return m_labelWidth * 14;}
  int  GetLanguage() const        {return m_language;}
  void SetLanguage(long language) {CWrite(m_language, language);}
  bool Latin2Greek() const           {return m_latin2greek;}
  void Latin2Greek(bool latin2greek) {CWrite(m_latin2greek, latin2greek);}
  bool GetMatchParens() const           {return m_matchParens;}
  void SetMatchParens(bool matchParens) {CWrite(m_matchParens, matchParens);}
  const wxString &MathJaXURL_User() const   {return m_mathJaxURL;}
  void MathJaXURL_User(const wxString &url) {CWrite(m_mathJaxURL, url);}
  bool MathJaXURL_UseUser() const      {return m_mathJaxURL_UseUser;}
  void MathJaXURL_UseUser(bool useUser){CWrite(m_mathJaxURL_UseUser, useUser);}
  //! The maximum number of Megabytes of gnuplot sources we should store
  long MaxGnuplotMegabytes() const         {return m_maxGnuplotMegabytes;}
  void MaxGnuplotMegabytes(long megaBytes) {CWrite(m_maxGnuplotMegabytes, megaBytes);}
  //! Returns the location of the maxima binary the user has selected.
  const wxString &MaximaUserLocation() const      {return m_maximaUserLocation;}
  //! Sets the location of the maxima binary.
  void MaximaUserLocation(const wxString &maxima) {CWrite(m_maximaUserLocation, maxima);}
  //! Notify the user if maxima is idle?
  bool NotifyIfIdle() const      {return m_notifyIfIdle; }
  void NotifyIfIdle(bool notify) {CWrite(m_notifyIfIdle, notify);}
  bool OfferKnownAnswers() const                 {return m_offerKnownAnswers;}
  void OfferKnownAnswers(bool offerKnownAnswers) {CWrite(m_offerKnownAnswers, offerKnownAnswers);}
  bool GetOpenHCaret() const          {return m_openHCaret;}
  void SetOpenHCaret(bool openHCaret) {CWrite(m_openHCaret, openHCaret);}
  //! Parameters to the maxima binary
  const wxString &MaximaParameters() const {return m_maximaParameters;}
  //! The parameters we pass to the maxima binary
  void MaximaParameters(const wxString &parameters) {CWrite(m_maximaParameters, parameters);}
  //! Print the cell brackets [displayed left to each group cell showing its extend]?
  bool PrintBrackets() const     {return m_printBrackets;}
  void PrintBrackets(bool print) {CWrite(m_printBrackets, print);}
  double PrintScale() const     {return m_printScale;}
  void PrintScale(double scale) {CWrite(m_printScale, scale);}
  bool RestartOnReEvaluation() const   {return m_restartOnReEvaluation;}
  void RestartOnReEvaluation(bool arg) {CWrite(m_restartOnReEvaluation, arg);}
  //! Do we want to show maxima's automatic labels (%o1, %t1, %i1,...)?
  bool ShowAutomaticLabels() const       {return m_showLabelChoice < labels_useronly;}
  //! Do we want at all to show labels?
  bool UseUserLabels() const             {return m_showLabelChoice > labels_automatic;}
    //! Do we want at all to show labels?
  bool ShowLabels() const                {return m_showLabelChoice < labels_none;}
  showLabels GetLabelChoice() const      {return m_showLabelChoice;}
  //! Sets the value of the Configuration ChoiceBox that treads displaying labels
  void SetLabelChoice(showLabels choice) {CWrite(m_showLabelChoice, choice);}
  long ShowLength() const      {return m_showLength;}
  void ShowLength(long length) {CWrite(m_showLength, length);}
  const wxString &FontName() const    {return m_fontName;}
  void FontName(const wxString &name) {CWrite(m_fontName, name);}
  const wxString &MathFontName()const    {return m_mathFontName;}
  void MathFontName(const wxString &name){CWrite(m_mathFontName, name);}
  long GetMathFontSize() const      {return m_mathFontSize;}
  void SetMathFontSize(double size) {CWrite(m_mathFontSize, long(size));}
  const wxString &SymbolPaneAdditionalChars() const       {return m_symbolPaneAdditionalChars;}
  void SymbolPaneAdditionalChars(const wxString &symbols) {CWrite(m_symbolPaneAdditionalChars, symbols);}
  bool CheckTeXFonts() const     {return m_TeXFonts;}
  void CheckTeXFonts(bool check) {CWrite(m_TeXFonts, check);}

  bool TocShowsSectionNumbers() const    {return m_TOCshowsSectionNumbers;}
  void TocShowsSectionNumbers(bool show) {CWrite(m_TOCshowsSectionNumbers, show);}
  bool UseSVG() const      {return m_useSVG;}
  void UseSVG(bool useSVG) {CWrite(m_useSVG, useSVG);}
  bool UseUnicodeMaths() const   {return m_useUnicodeMaths;}
  void UseUnicodeMaths(bool use) {CWrite(m_useUnicodeMaths, use);}
  //! Determines the zoom factor the worksheet is displayed at
  double GetZoomFactor() const {return m_zoomFactor;}
  //! Sets the zoom factor the worksheet is displayed at
  void SetZoomFactor(double newzoom);

  // End of Stored Configuration Settings
  //

  //! Get a drawing context suitable for size calculations
  wxDC *GetDC() const        {return m_dc;}
  //! Set the drawing context that is currently active
  void SetContext(wxDC &dc)  {(m_dc = &dc), (m_antialiassingDC = {});}
  void UnsetContext()        {m_dc = {};}

  //! Get a drawing context suitable for size calculations
  wxDC *GetAntialiassingDC() const
  { return (m_antialiassingDC && m_antiAliasLines) ? m_antialiassingDC : m_dc; }
  void SetAntialiassingDC(wxDC &antialiassingDC)  {m_antialiassingDC = &antialiassingDC;}
  void UnsetAntialiassingDC()                     {m_antialiassingDC = {};}

  static wxString m_maximaLocation_override;
  static wxString m_configfileLocation_override;

  void SetBackgroundBrush(const wxBrush &brush);
  const wxBrush &GetBackgroundBrush() const  {return m_BackgroundBrush;}
  const wxBrush &GetTooltipBrush() const     {return m_tooltipBrush;}

  using EscCodeContainer = std::unordered_map<wxString, wxString, wxStringHash>;
  using EscCodeIterator = EscCodeContainer::const_iterator;

  //! Retrieve a symbol for the escape code typed after the Escape key.
  static const wxString &GetEscCode(const wxString &key);
  //! Iterators over the escape code list
  static EscCodeIterator EscCodesBegin();
  static EscCodeIterator EscCodesEnd();

  //! Returns the URL MathJaX can be found at.
  wxString MathJaXURL() const
  { return m_mathJaxURL_UseUser ? m_mathJaxURL : wxString(MathJaXURL_Auto()); }
  
  static constexpr double GetMinZoomFactor()  {return 0.1;}
  static constexpr double GetMaxZoomFactor()  {return 32.0;}

  /*! Extra space to leave between two equations in output cells.

    Extra space between equations is useful if we don't display labels that show
    which line begins a new equation and which line merely continues a multi-line
    equation.
   */
  double GetInterEquationSkip() const 
  { return ShowAutomaticLabels() ? 0 : GetZoomFactor() * m_mathFontSize / 2; }

  long GetCellBracketWidth() const
  { return long(GetZoomFactor() * 16); }

  //! Sets the zoom factor without storing the new value in the config file/registry.
  void SetZoomFactor_temporarily(double newzoom)
  {
    if (m_zoomFactor == newzoom)
      return;
    RecalculationForce(true);
    FontChanged(true);
    m_zoomFactor = newzoom;
  }

  /*! Scales a distance [in pixels] according to the zoom factor

    Is used for displaying/printing/exporting of text/maths.
   */
  long Scale_Px(double px) const;

  wxString GetFontName(long type = TS_DEFAULT) const;

  // cppcheck-suppress functionStatic
  // cppcheck-suppress functionConst
  wxString GetSymbolFontName() const;

  wxFontWeight IsBold(long st) const;

  wxFontStyle IsItalic(long st) const;

  bool IsUnderlined(long st) const  {return m_styles[st].Underlined();}

  //! Force a full recalculation?
  bool RecalculationForce() const      {return m_forceUpdate;}
  void RecalculationForce(bool force)  {m_forceUpdate = force;}

  //! Get the indentation of GroupCells.
  long GetIndent() const
  { return (m_indent >= 0) ? m_indent : 3 * GetCellBracketWidth() / 2; }
  /*! Set the indentation of GroupCells

    Normally this parameter is automatically calculated
   */
  void SetIndent(long indent)
  {
    if (m_indent == indent)
      return;
    RecalculationForce(true);
    m_indent = indent;
  }

  //! Get the resolution of the display showing the worksheet
  wxSize GetPPI() const   {return GetPPI(GetWorkSheet());}

  // cppcheck-suppress functionStatic
  // cppcheck-suppress functionConst
  //! Get the resolution of an arbitrary display
  wxSize GetPPI(wxWindow *win) const;
  
  //! How much vertical space is to be left between two group cells?
  long GetCursorWidth() const
  {
    long ppi = m_printing ? 96 : GetPPI().x;
    return (ppi / 45 < 1) ? 1 : ppi / 45;
  }
  
  //! The y position the worksheet starts at
  long GetBaseIndent() const  {return std::max(4 + GetCursorWidth(), 16l);}

  //! The vertical space between GroupCells
  long GetGroupSkip() const   {return std::max(10 + GetCursorWidth(), 20l);}

  //! Has a font changed?
  bool FontChanged() const    {return m_fontChanged;}
  void FontChanged(bool fontChanged)
  {
    m_fontChanged = fontChanged;
    if (fontChanged)
      RecalculationForce(true);
    m_charsInFont.clear();
  }
  
  //! Returns the width of the visible portion of the worksheet
  long GetClientWidth() const      {return m_clientWidth;}
  //! Set the width of the visible window for GetClientWidth()
  void SetClientWidth(long width)
  {
    if (m_clientWidth == width)
      return;
    RecalculationForce(true);
    m_clientWidth = width;
  }

  //! Returns the height of the visible portion of the worksheet
  long GetClientHeight() const       {return m_clientHeight;}
  //! Set the height of the visible window for GetClientHeight()
  void SetClientHeight(long height)  {m_clientHeight = height;}

  //! Calculates the default line width for the worksheet
  double GetDefaultLineWidth() const
  {
    if (GetZoomFactor() < 1.0)
      return 1.0;
    else
      return GetZoomFactor();
  }

  //! The minimum sensible line width in withs of a letter.
  long LineWidth_em() const      {return (!m_printing) ? m_lineWidth_em : 10000;}
  //! Set the minimum sensible line width in widths of a letter.
  void LineWidth_em(long width)  {m_lineWidth_em = width;}

  //! Returns the maximum sensible width for a text line [in characters]:
  // On big 16:9 screens text tends to get \b very wide before it hits the right margin.
  // But text blocks that are 1 meter wide and 2 cm high feel - weird.
  long GetLineWidth() const;

  long GetFontSize(TextStyle st) const
  {
    if (st == TS_TEXT || st == TS_HEADING5 || st == TS_HEADING6 || st == TS_SUBSUBSECTION || st == TS_SUBSECTION || st == TS_SECTION || st == TS_TITLE)
      return m_styles[st].FontSize();
    return 0;
  }

  void Outdated(bool outdated)   {m_outdated = outdated;}

  const wxString &GetTeXCMRI() const    {return m_fontCMRI;}
  const wxString &GetTeXCMSY() const    {return m_fontCMSY;}
  const wxString &GetTeXCMEX() const    {return m_fontCMEX;}
  const wxString &GetTeXCMMI() const    {return m_fontCMMI;}
  const wxString &GetTeXCMTI() const    {return m_fontCMTI;}

  bool ShowCodeCells() const     {return m_showCodeCells;}
  void ShowCodeCells(bool show);
  
  /*! Are we currently printing?

    This affects the bitmap scale as well as the fact if we want
    to output objects that are outside the region that currently is
    redrawn.
  */
  bool GetPrinting() const         {return m_printing;}
  void SetPrinting(bool printing);

  //! Gets the color for a text style
  wxColour GetColor(TextStyle style);
  
  //! Inverts a color: In 2020 wxColor still lacks this functionality
  wxColour InvertColour(wxColour col);
  
  /*! Make this color differ from the background by a noticeable amount
    
    Useful for black/white background theme changes
  */
  wxColor MakeColorDifferFromBackground(wxColor color);
  
  wxRect GetUpdateRegion() const     {return m_updateRegion;}
  void SetUpdateRegion(wxRect rect)  {m_updateRegion = rect;}

  //! Reads the size of the current worksheet's visible window. See SetCanvasSize
  wxSize GetCanvasSize() const    {return m_canvasSize;}
  //! Sets the size of the current worksheet's visible window.
  void SetCanvasSize(wxSize siz)  {m_canvasSize = siz;}

  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool ShowBrackets() const       {return m_showBrackets;}
  bool ShowBrackets(bool show)    {return m_showBrackets = show;}

  //! The auto-detected maxima location
  static wxString MaximaDefaultLocation();

  //! Returns the location of the maxima binary.
  wxString MaximaLocation() const;

  /*! Could a maxima binary be found in the path we expect it to be in?

    \param location The location to search for maxima in. 
    If location == wxEmptyString the default location from the configuration 
    is taken.
   */
  static bool MaximaFound(const wxString &location = {});

  static const wchar_t *MathJaXURL_Auto() { return L"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.6/MathJax.js?config=TeX-AMS_HTML";}

  //! Sets the default toolTip for new cells
  void SetDefaultCellToolTip(const wxString &defaultToolTip)  {m_defaultToolTip = defaultToolTip;}
  //! Gets the default toolTip for new cells
  const wxString &GetDefaultCellToolTip() const               {return m_defaultToolTip;}

  drawMode GetParenthesisDrawMode();
  //! Which way do we want to draw parenthesis?
  void SetGrouphesisDrawMode(drawMode mode)  {m_parenthesisDrawMode = mode;}

  /*! Get the font for a given text style

    \param textStyle The text style to get the font for
    \param fontSize Only relevant for math cells: Super- and subscripts can have different
    font styles than the rest.
   */
  wxFont GetFont(TextStyle textStyle, long fontSize) const;

  //! Get the worksheet this configuration storage is valid for
  wxWindow *GetWorkSheet() const          {return m_workSheet;}
  //! Set the worksheet this configuration storage is valid for
  void SetWorkSheet(wxWindow *workSheet)  {m_workSheet = workSheet;}

  //! Update the list of fonts associated to the worksheet styles
  void UpdateWorksheetFonts();
  //! Get the font for the given worksheet style
  wxFont GetWorksheetFont(TextStyle style) const;
  //! Determine the default background color of the worksheet

  wxColor DefaultBackgroundColor();
  //! Determine the default background color of editorcells
  wxColor EditorBackgroundColor();

  //! Do we want to save time by only redrawing the area currently shown on the screen?
  bool ClipToDrawRegion() const                {return m_clipToDrawRegion;}
  //! Do we want to save time by only redrawing the area currently shown on the screen?
  void ClipToDrawRegion(bool clipToDrawRegion) {m_clipToDrawRegion = clipToDrawRegion; m_forceUpdate = true;}

  bool AdjustWorksheetSize() const       {return m_adjustWorksheetSizeNeeded;}
  //! Request adjusting the worksheet size?
  void AdjustWorksheetSize(bool adjust)  {m_adjustWorksheetSizeNeeded = adjust;}

  wxRect GetVisibleRegion() const              {return m_visibleRegion;}
  void SetVisibleRegion(wxRect visibleRegion)  {m_visibleRegion = visibleRegion;}

  wxPoint GetWorksheetPosition() const                  {return m_worksheetPosition;}
  void SetWorksheetPosition(wxPoint worksheetPosition)  {m_worksheetPosition = worksheetPosition;}

  const wxString & MaximaShareDir() const   {return m_maximaShareDir;}
  void MaximaShareDir(const wxString &dir)  {m_maximaShareDir = dir;}

  void InLispMode(bool lisp)  {m_inLispMode = lisp;}
  bool InLispMode() const     {return m_inLispMode;}

  Style m_styles[NUMBEROFSTYLES];

private:  
  //
  // Configuration Settings Cache
  // These values are cached from the configuration file/registry. They should not be
  // accessed directly from outside - only via the setters/getters.
  // All the values are initialized by the constructor per the configuration entry
  // definitions - do not set any default values here, they'll be overwritten.
  //
  bool m_abortOnError;
  bool m_antiAliasLines;
  //! Autodetect maxima's location?
  bool m_autodetectMaxima;
  //! Automatically indent long lines?
  bool m_autoIndent;
  //! Should autosave into a temp file instead of open file?
  bool m_autoSaveAsTempFile;
  //! Which objects do we want to convert into subscripts if they occur after an underscore?
  long m_autoSubscript;
  //! Automatically wrap long lines?
  long m_autoWrap;
  /*! Replace a "*" by a centered dot?

    Normally we ask the parser for this piece of information. But during recalculation
    of widths while selecting text we don't know our parser.
   */
  bool m_changeAsterisk;
  bool m_copyBitmap;
  bool m_copyEMF;
  bool m_copyMathML;
  bool m_copyMathMLHTML;
  bool m_copyRTF;
  bool m_copySVG;
  long m_defaultPort;
  //! How many digits of a number we show by default?
  long m_displayedDigits;
  wxString m_documentclass;
  wxString m_documentclassOptions;
  bool m_enterEvaluates;
  wxFindReplaceFlags m_findFlags;
  bool m_fixReorderedIndices;
  bool m_greekSidebar_ShowLatinLookalikes;
  bool m_greekSidebar_Show_mu;
  //! Hide brackets that are not under the pointer
  bool m_hideBrackets;
  bool m_hidemultiplicationsign;
  htmlExportFormat m_htmlEquationFormat;
  bool m_indentMaths;
  //! Do we want to automatically insert new cells containing a "%" at the end of every command?
  bool m_insertAns;
  bool m_invertBackground;
  bool m_keepPercent;
  //! The width of input and output labels [in chars]
  long m_labelWidth;
  //! The number of the language wxMaxima uses.
  long m_language;
  bool m_latin2greek;
  //! Do we want to automatically close parenthesis?
  bool m_matchParens;
  wxString m_mathJaxURL;
  bool m_mathJaxURL_UseUser;
  long m_maxGnuplotMegabytes;
  wxString m_maximaUserLocation;
  //! Notify the user if maxima is idle
  bool m_notifyIfIdle;
  bool m_offerKnownAnswers;
  //! Do we want to automatically open a new cell if maxima has finished evaluating its input?
  bool m_openHCaret;
  wxString m_maximaParameters;
  //! Print the cell brackets [displayed left to each group cell showing its extend]?
  bool m_printBrackets;
  //! The scale for printing
  double m_printScale;
  bool m_restartOnReEvaluation;
  showLabels m_showLabelChoice;
  long m_showLength;
  wxString m_fontName;
  wxString m_mathFontName;
  long m_mathFontSize;
  wxString m_symbolPaneAdditionalChars;
  bool m_TOCshowsSectionNumbers;
  bool m_TeXFonts;
  bool m_useSVG;
  bool m_useUnicodeMaths;
  double m_zoomFactor;

  //
  // End of cached entries
  // Start of entries used in definition initialization

  // Used by to initialize m_texFonts via HasTeXFonts()
  wxString m_fontCMRI, m_fontCMSY, m_fontCMEX, m_fontCMMI, m_fontCMTI;

  //
  // End of entries used in definition initalization

  //! A definition of a configuration setting
  struct SettingDefinition;

  /*! Configuration Setting Definitions
   * Do not move these definitions - they must be placed immediately after
   * after all configuration cache entries.
   */
  std::vector<SettingDefinition> m_defs;

  //! Returns the setting definition for a given cache entry, or null if not found.
  SettingDefinition *LookupDef(const void *cache);

  //! Writes a new value of a given cache entry
  template <typename T>
  void CWrite(T &field, T value) { ConfigWrite(&field, &value); }
  void CWrite(wxString &field, const wxString &value) { ConfigWrite(&field, &value); }

  //! Implementation of a write of a value of a cache entry, optionally first set to a new value.
  bool ConfigWrite(void *cache, const void *newValue = {});
  bool ConfigWrite(wxConfigBase &, void *cache, const void *newValue = {});
  // wxString converts to a pointer - we don't want that
  bool ConfigWrite(const wxString &) = delete;
  bool ConfigWrite(wxString &) = delete;

  //! Implementation of a read of a value into the cache entry
  bool ConfigRead(void *cache);
  bool ConfigRead(wxConfigBase &, void *cache);

  //
  // End of Configuration Settings Cache
  //

  // All POD (plain old data) members below must be initialized - they are not
  // cached, and there's no other code that might initialize them. For consistency, do
  // not initialize them in the constructor initializer list unless there's a specific
  // reason.

  wxDC *m_dc = {};
  wxDC *m_antialiassingDC = {};

  //! The worksheet all cells are drawn on
  wxRect m_updateRegion;
  //! Has the font changed?
  bool m_fontChanged = true;
  //! The worksheet this configuration storage is valid for
  wxWindow *m_workSheet = {};
  //! A replacement for the non-existing "==" operator for wxBitmaps.

  //! Caches the information on how to draw big parenthesis for GetParenthesisDrawMode().
  drawMode m_parenthesisDrawMode = unknown;

  wxString m_workingdir;

  //! The size of the canvas our cells have to be drawn on
  wxSize m_canvasSize;
  //! Show the cell brackets [displayed left to each group cell showing its extend]?
  bool m_showBrackets = true;

  long m_indent = -1;

  wxString m_maximaShareDir;

  bool m_forceUpdate = false;
  bool m_clipToDrawRegion = true;
  bool m_outdated = false;

  wxString m_defaultToolTip;

  long m_clientWidth = 1024;
  long m_clientHeight = 768;
  bool m_printing = false;
  long m_lineWidth_em = 88;

  bool m_showCodeCells = true;

  //!< don't add ; in lisp mode
  bool m_inLispMode = false;

  bool m_adjustWorksheetSizeNeeded = false;
  //! The rectangle of the worksheet that is currently visible.
  wxRect m_visibleRegion;
  //! The position of the worksheet in the wxMaxima window
  wxPoint m_worksheetPosition;

  wxColour m_defaultBackgroundColor;
  //! The brush the normal cell background is painted with
  wxBrush m_BackgroundBrush;
  wxBrush m_tooltipBrush;

  wxFont m_worksheetFonts[NUMBEROFSTYLES];

  bool HasTeXFonts();

  struct CharsExist {
    wxString chars;
    bool exist;
    CharsExist(const wxString &chars, bool exist) : chars(chars), exist(exist) {}
  };
  std::vector<CharsExist> m_charsInFont;

  //! Do these chars exist in the given font?
  bool CharsExistInFont(const wxFont &font, const wxString &chars);

  //! Initialize the text styles on construction.
  void InitStyles();
};

//! Sets the configuration's "printing" flag until this class is left.
class Printing final
{
public:
  explicit Printing(Configuration *configuration) :
    m_configuration(configuration)
  {
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
class NoClipToDrawRegion final
{
public:
  explicit NoClipToDrawRegion(Configuration *configuration) :
    m_configuration(configuration)
  {
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
