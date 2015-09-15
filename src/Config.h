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

/*!
\file 
The configuration dialog.

This file contains the code for the preferences dialog. The preferences themself will
be read directly using <code> config->Read </code>, instead, where needed.
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

#ifndef CONFIG_H
#define CONFIG_H

#include "TextStyle.h"
#include "Setup.h"

enum {
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

/*! TheSample text that is shown by the style selector.

This is a piece of text that shows the user how the selected style will look.
*/
class ExamplePanel : public wxPanel
{
public:
  //! The constructor
  ExamplePanel(wxWindow *parent, int id, wxPoint pos, wxSize size) : wxPanel(parent, id, pos, size)
  {
#if defined (__WXGTK12__) && !defined (__WXGTK20__)
    m_size = 12;
#elif defined (__WXMAC__)
    m_size = 12;
#else
    m_size = 10;
#endif

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
  void SetFontSize(int size) { m_size = size; }
private:
  /*! Actually updates the formatting example

    This function is called after Config::UpdateExample() changes the example's style.
 */
  void OnPaint(wxPaintEvent& event);

 private:
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

/*! The configuration dialog

This class draws and handles the configuration dialog. 

Code that needs to know the value of the preferences that are set here reads 
them directly using <code> config->Read </code>, instead.
 */
class Config: public wxPropertySheetDialog
{
public:
  //! The constructor
  Config(wxWindow* parent);
  //! The destructor
  ~Config();

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
  /*! begin wxGlade: Config::methods

    This method sets the window title, the tool tips etc.
   */
  void SetProperties();
  //! The panel that allows to set the editing options
  wxPanel* CreateWorksheetPanel();
  //! A panel that allows to set general options
  wxPanel* CreateOptionsPanel();
  //! The panel that allows to set options affecting the export functionality
  wxPanel* CreateExportPanel();
  //! The panel that allows to change styles
  wxPanel* CreateStylePanel();
  //! The panel that allows to change maxima-specific configurations.
  wxPanel* CreateMaximaPanel();
  // end wxGlade
protected:
  // begin wxGlade: Config::attributes
  wxTextCtrl* m_maximaProgram;
  wxTextCtrl* m_documentclass;
  wxTextCtrl* m_texPreamble;
  wxSpinCtrl* m_autoSaveInterval;
  wxButton* m_mpBrowse;
  wxTextCtrl* m_additionalParameters;
  wxComboBox* m_language;
  wxCheckBox* m_saveSize;
  wxCheckBox* m_abortOnError;
  wxCheckBox* m_pollStdOut;
  wxCheckBox* m_savePanes;
  wxCheckBox* m_usepngCairo;
  wxCheckBox* m_uncomressedWXMX;
  wxSpinCtrl* m_defaultFramerate;
  wxSpinCtrl* m_defaultPlotWidth;
  wxSpinCtrl* m_defaultPlotHeight;
  wxSpinCtrl* m_displayedDigits;
  //! A checkbox that allows to select if the LaTeX file should contain animations.
  wxCheckBox* m_AnimateLaTeX;
  //! A checkbox that asks if TeX should put the exponents above or after the subscripts.
  wxCheckBox* m_TeXExponentsAfterSubscript;
  //! A checkbox that asks if all newlines in text cells have to be passed to HTML.
  wxCheckBox* m_flowedTextRequested;
  //! A checkbox that asks if we want to export the input for maxima, as well.
  wxCheckBox* m_exportInput;
  wxCheckBox* m_exportContainsWXMX;
  wxCheckBox* m_exportWithMathJAX;
  wxCheckBox* m_matchParens;

  wxChoice* m_showLength;
  wxCheckBox* m_enterEvaluates;
  wxCheckBox* m_saveUntitled;
  wxCheckBox* m_openHCaret;
  wxCheckBox* m_insertAns;
  wxSpinCtrl* m_labelWidth;
  wxSpinCtrl* m_undoLimit;
  wxSpinCtrl* m_bitmapScale;
  wxCheckBox* m_fixReorderedIndices;
  wxCheckBox* m_showUserDefinedLabels;
  wxButton* m_getFont;
  wxButton* m_getStyleFont;
  wxFontEncoding m_fontEncoding;
  wxListBox* m_styleFor;
#ifndef __WXMSW__
  wxPanel* m_styleColor;
#else
  wxButton* m_styleColor;
#endif
  wxCheckBox* m_boldCB;
  wxCheckBox* m_italicCB;
  wxCheckBox* m_underlinedCB;
  wxCheckBox* m_fixedFontInTC;
  wxCheckBox* m_unixCopy;
  wxCheckBox* m_changeAsterisk;
  wxCheckBox* m_useJSMath;
  wxCheckBox* m_keepPercentWithSpecials;
  wxBookCtrlBase* m_notebook;
  wxStaticText* m_mathFont;
  wxButton* m_getMathFont;
  wxString m_mathFontName;
  wxButton *m_saveStyle, *m_loadStyle;
  wxSpinCtrl* m_defaultPort;
  #ifdef __WXMSW__
  wxCheckBox* m_wxcd;
  #endif
  ExamplePanel* m_examplePanel;
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
    m_styleText,
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

  //! Is called when the configuration dialog is closed.
  void OnClose(wxCloseEvent& event);
  //! Starts the file chooser that allows selecting where the maxima binary lies
  void OnMpBrowse(wxCommandEvent& event);
#if defined __WXMSW__
  //! Is called when the color button is pressed.
  void OnColorButton(wxCommandEvent& event);
#endif
  //! Starts the font selector dialog for the math font
  void OnMathBrowse(wxCommandEvent& event);
  //! Called if a new item type that is to be styled is selected
  void OnChangeStyle(wxCommandEvent& event);
  //! A message dialog that appears if a change cannot be applied now.
  void OnChangeWarning(wxCommandEvent& event);
  //! Called if one of the checkboxes for bold, italic or underlined is toggled
  void OnCheckbox(wxCommandEvent& event);
  //! Reads the style settings from a file
  void ReadStyles(wxString file = wxEmptyString);
  //! Saves the style settings to a file.
  void WriteStyles(wxString file = wxEmptyString);
  //! Sets the style example's style on style changes.
  void UpdateExample();
  //! Called if the font family is changed.
  void OnChangeFontFamily(wxCommandEvent& event);
  //! A "export the configuration" dialog
  void LoadSave(wxCommandEvent& event);
  //! The size of the text font
  int m_fontSize;
  //! The size of the maths font.
  int m_mathFontSize;
  /*! A pointer to the style that is currently selected for being edited.
    
    \attention Should match whatever is put in m_styleFor
  */
  style* GetStylePointer();
  //! A list containing the pictograms for the tabs.
  wxImageList *m_imageList;
  DECLARE_EVENT_TABLE()
};

#ifndef __WXMSW__
class ColorPanel : public wxPanel
{
public:
  ColorPanel(Config * conf, wxWindow *parent, int id, wxPoint pos, wxSize size, long style) : wxPanel(parent, id, pos, size, style)
  {
     config = conf;
     SetBackgroundColour(wxColour(0,0,0));
  };
  void OnClick(wxMouseEvent &event) {
      config->OnChangeColor();
  }
private:
  Config * config;
  DECLARE_EVENT_TABLE()
};
#endif // __WXMSW__

#endif // CONFIG_H
