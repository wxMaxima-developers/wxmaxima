// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
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

/*!
\file
The configuration dialog.

This file contains the code for ConfigDialogue, the class that handles the preferences
dialog. The preferences themself will be read directly using
<code> config->Read </code>, instead, where needed or from Configuration.
*/

#include <wx/wx.h>
#include <wx/image.h>

#include <wx/propdlg.h>
#include <wx/generic/propdlg.h>
#include <wx/spinctrl.h>
#include <wx/notebook.h>

#include <wx/imaglist.h>
#include <wx/bookctrl.h>
#include <wx/artprov.h>

#ifndef CONFIGDIALOGUE_H
#define CONFIGDIALOGUE_H

#include "TextStyle.h"
#include "Configuration.h"

enum
{
  color_id,
  listbox_styleFor,
  checkbox_bold,
  checkbox_italic,
  checkbox_underlined,
  button_mathFont,
  font_family,
  style_font_family,
  language_id,
  save_id,
  load_id
};


/*! The configuration dialog

This class draws and handles the configuration dialog.

Code that needs to know the value of the preferences that are set here reads
them directly using <code> config->Read </code>, instead.
 */
class ConfigDialogue : public wxPropertySheetDialog
{
public:
  //! The constructor
  ConfigDialogue(wxWindow *parent, Configuration *config);

  //! The destructor
  ~ConfigDialogue();

  //! The export formats we support for HTML equations
  enum htmlExportFormats
  {
    mathJaX_TeX = 0,
    bitmap = 1,
    mathML_mathJaX = 2,
    svg = 3
  };

  /*! Called if the color of an item has been changed

    called from class ColorPanel
  */
  void OnChangeColor();

  /*! Stores the settings from the configuration dialog.

    wxWidgets knows how to store the settings to gconf, the registry or wherever the current
    system expects settings to be saved to.
  */
  void WriteSettings();

private:
  Configuration *m_configuration;
  /*! TheSample text that is shown by the style selector.

    This is a piece of text that shows the user how the selected style will look.
  */
  class ExamplePanel : public wxPanel
  {
  public:
    //! The constructor
    ExamplePanel(wxWindow *parent, int id, wxPoint pos, wxSize size) : wxPanel(parent, id, pos, size)
      {
#if defined (__WXMAC__)
        m_size = 12;
#else
        m_size = 10;
#endif
        m_italic = false;
        m_bold = false;
        m_underlined = false;
      };

    //! Sets all user-changable elements of style of the example at once.
    void SetStyle(wxColour fg_color, bool italic, bool bold, bool underlined, wxString font)
      {
        m_fgColor = fg_color;
        m_italic = italic;
        m_bold = bold;
        m_underlined = underlined;
        m_font = font;
      }

    //! Sets the font size of the example
    void SetFontSize(int size)
      { m_size = size; }

  private:
    /*! Actually updates the formatting example

      This function is called after ConfigDialogue::UpdateExample() changes the example's style.
    */
    void OnPaint(wxPaintEvent &event);

    //! The foreground color of the currently selected item type
    wxColour m_fgColor;
    //! Is the currently selected item type displayed in italic?
    bool m_italic;
    //! Is the currently selected item type displayed in bold?
    bool m_bold;
    //! Is the currently selected item type displayed underlined?
    bool m_underlined;
    //! The font the currently selected item type is displayed with
    wxString m_font;
    //! The size of the characters of the currently selected item type
    int m_size;
    DECLARE_EVENT_TABLE()
  };

  /*! A rectangle showing the color of an item

    If the color contains transparency the rectangle is checkered accordingly.
   */
  class ColorPanel : public wxPanel
  {
  public:
    ColorPanel(ConfigDialogue *conf, wxWindow *parent,
               int id, wxPoint pos, wxSize size, long style);

    void OnPaint(wxPaintEvent &WXUNUSED(event));

    void OnClick(wxMouseEvent& WXUNUSED(event));

    void SetColor(wxColor color)
      {
        m_color = color;
        Refresh();
      };
  private:
    ConfigDialogue *m_configDialogue;
    wxColor m_color;
    DECLARE_EVENT_TABLE()
  };


  /*! begin wxGlade: ConfigDialogue::methods

    This method sets the window title, the tool tips etc.
   */
  void SetProperties();

  //! Calculates the size of the images for a configuration tab
  int GetImageSize();

  //! Loads the image for a configuration tab
  wxBitmap GetImage(wxString name,
                   unsigned char *data_128, size_t len_128,
                   unsigned char *data_192, size_t len_192);

  //! The panel that allows to choose which formats to put on the clipboard
  wxPanel *CreateClipboardPanel();

  wxCheckBox *m_copyBitmap, *m_copyMathML, *m_copyMathMLHTML, *m_copyRTF, *m_copySVG;
  #if wxUSE_ENH_METAFILE
  wxCheckBox *m_copyEMF;
  #endif

  //! The panel that allows to set the editing options
  wxPanel *CreateWorksheetPanel();

  //! A panel that allows to set general options
  wxPanel *CreateOptionsPanel();

  //! The panel that allows to set options affecting the export functionality
  wxPanel *CreateExportPanel();

  //! The panel that allows to change styles
  wxPanel *CreateStylePanel();

  //! The panel that allows to change maxima-specific configurations.
  wxPanel *CreateMaximaPanel();

  //! The panel that allows to specify startup commands
  wxPanel *CreateStartupPanel();

  // end wxGlade
protected:
  //! The name of maxima's startup file.
  wxString m_startupFileName;
  //! The name of wxMaxima's startup file.
  wxString m_wxStartupFileName;
  //! The text "Maxima Program" that can change color.
  wxStaticText *m_mp;

  //! Called if the currently active dialogue sheet is changed.
  void OnTabChange(wxBookCtrlEvent &event);

  //! Called if the user changes the style that is to be edited.
  void OnStyleToEditChanged(wxCommandEvent &event);

  // begin wxGlade: ConfigDialogue::attributes
  //! A textbox containing maxima's startup commands
  wxTextCtrl *m_startupCommands;
  //! A textbox containing wxMaxima's startup commands
  wxTextCtrl *m_wxStartupCommands;
  wxTextCtrl *m_maximaProgram;
  wxTextCtrl *m_documentclass;
  wxTextCtrl *m_texPreamble;
  wxSpinCtrl *m_autoSaveInterval;
  wxButton *m_mpBrowse;
  wxTextCtrl *m_additionalParameters;
  wxTextCtrl *m_mathJaxURL;
  wxChoice *m_language;
  wxTextCtrl *m_symbolPaneAdditionalChars;
  wxCheckBox *m_saveSize;
  wxCheckBox *m_abortOnError;
  wxCheckBox *m_restartOnReEvaluation;
  wxCheckBox *m_wrapLatexMath;
  wxCheckBox *m_savePanes;
  wxCheckBox *m_usepngCairo;
  wxCheckBox *m_antialiasLines;
  wxSpinCtrl *m_defaultFramerate;
  wxSpinCtrl *m_defaultPlotWidth;
  wxSpinCtrl *m_defaultPlotHeight;
  wxSpinCtrl *m_displayedDigits;
  //! A checkbox that allows to select if the LaTeX file should contain animations.
  wxCheckBox *m_AnimateLaTeX;
  //! A checkbox that asks if TeX should put the exponents above or after the subscripts.
  wxCheckBox *m_TeXExponentsAfterSubscript;
  //! A checkbox that asks if TeX should use the \\partial symbol for representing diff()
  wxCheckBox *m_usePartialForDiff;
  //! A checkbox that asks if all newlines in text cells have to be passed to HTML.
  wxCheckBox *m_exportContainsWXMX;
  wxCheckBox *m_printBrackets;
  wxChoice *m_exportWithMathJAX;
  wxCheckBox *m_matchParens;
  wxChoice *m_showLength;
  wxChoice *m_autosubscript;
  wxCheckBox *m_enterEvaluates;
  wxCheckBox *m_saveUntitled;
  wxCheckBox *m_openHCaret;
  wxCheckBox *m_insertAns;
  wxCheckBox *m_autoIndent;
  wxCheckBox *m_cursorJump;
  wxCheckBox *m_hideBrackets;
  wxCheckBox *m_indentMaths;
  wxChoice *m_autoWrap;
  wxSpinCtrl *m_labelWidth;
  wxSpinCtrl *m_undoLimit;
  wxSpinCtrl *m_recentItems;
  wxSpinCtrl *m_bitmapScale;
  wxSpinCtrlDouble *m_printScale;
  wxCheckBox *m_fixReorderedIndices;
  wxCheckBox *m_incrementalSearch;
  wxCheckBox *m_notifyIfIdle;
  wxChoice *m_showUserDefinedLabels;
  wxButton *m_getFont;
  wxButton *m_getStyleFont;
  wxFontEncoding m_fontEncoding;
  wxListBox *m_styleFor;
  //! An example rectangle with the font color
  ColorPanel *m_styleColor;
  wxCheckBox *m_boldCB;
  wxCheckBox *m_italicCB;
  wxCheckBox *m_underlinedCB;
  wxCheckBox *m_fixedFontInTC;
  wxCheckBox *m_unixCopy;
  wxCheckBox *m_changeAsterisk;
  wxCheckBox *m_useJSMath;
  wxCheckBox *m_useUnicodeMaths;
  wxCheckBox *m_keepPercentWithSpecials;
  wxBookCtrlBase *m_notebook;
  wxStaticText *m_mathFont;
  wxButton *m_getMathFont;
  wxString m_mathFontName;
  wxButton *m_saveStyle, *m_loadStyle;
  wxSpinCtrl *m_defaultPort;
  ExamplePanel *m_examplePanel;
  // end wxGlade
  style m_styleDefault,
          m_styleVariable,
          m_styleFunction,
          m_styleNumber,
          m_styleSpecial,
          m_styleGreek,
          m_styleString,
          m_styleInput,
          m_styleMainPrompt,
          m_styleOtherPrompt,
          m_styleLabel,
          m_styleUserDefinedLabel,
          m_styleHighlight,
          m_styleWarning,
          m_styleText,
          m_styleHeading6,
          m_styleHeading5,
          m_styleSubsubsection,
          m_styleSubsection,
          m_styleSection,
          m_styleTitle,
          m_styleTextBackground,
          m_styleBackground,
          m_styleCellBracket,
          m_styleActiveCellBracket,
          m_styleCursor,
          m_styleSelection,
          m_styleEqualsSelection,
          m_styleOutdated,
          m_styleCodeHighlightingVariable,
          m_styleCodeHighlightingFunction,
          m_styleCodeHighlightingComment,
          m_styleCodeHighlightingNumber,
          m_styleCodeHighlightingString,
          m_styleCodeHighlightingOperator,
          m_styleCodeHighlightingEndOfLine;

  //! Is called when the path to the maxima binary was changed.
  void MaximaLocationChanged(wxCommandEvent &unused);

  //! Is called when the path to the maxima binary was changed.
  void UsepngcairoChanged(wxCommandEvent &event);

  //! Is called when the configuration dialog is closed.
  void OnClose(wxCloseEvent &event);

  //! Starts the file chooser that allows selecting where the maxima binary lies
  void OnMpBrowse(wxCommandEvent &event);

  void OnIdle(wxIdleEvent &event);

  //! Starts the font selector dialog for the math font
  void OnMathBrowse(wxCommandEvent &event);

  //! Called if a new item type that is to be styled is selected
  void OnChangeStyle(wxCommandEvent &event);

  //! A message dialog that appears if a change cannot be applied now.
  void OnChangeWarning(wxCommandEvent &event);

  //! Called if one of the checkboxes for bold, italic or underlined is toggled
  void OnCheckbox(wxCommandEvent &event);

  //! Reads the style settings from a file
  void ReadStyles(wxString file = wxEmptyString);

  //! Saves the style settings to a file.
  void WriteStyles(wxString file = wxEmptyString);

  //! Sets the style example's style on style changes.
  void UpdateExample();

  //! Called if the font family is changed.
  void OnChangeFontFamily(wxCommandEvent &event);

  //! A "export the configuration" dialog
  void LoadSave(wxCommandEvent &event);

  //! The size of the text font
  int m_fontSize;
  //! The size of the maths font.
  int m_mathFontSize;

  /*! A pointer to the style that is currently selected for being edited.

    \attention Should match whatever is put in m_styleFor
  */
  style *GetStylePointer();

  //! A list containing the pictograms for the tabs.
  wxImageList *m_imageList;
DECLARE_EVENT_TABLE()
};

#ifndef __WXMSW__

#endif // __WXMSW__

#endif // CONFIG_H
