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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!
  \file
  The configuration dialog.

  This file contains the code for ConfigDialogue, the class that handles the preferences
  dialog. The preferences themself will be read directly using
  <code> config->Read </code>, instead, where needed or from Configuration.
*/

extern unsigned int view_refresh_svg_gz_len;
extern unsigned char view_refresh_svg_gz[];

#include "precomp.h"
#include <wx/wx.h>
#include <wx/image.h>
#include <wx/grid.h>
#include <wx/hashmap.h>
#include <wx/clrpicker.h>
#include <memory>
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
#include "Worksheet.h"
#include "Configuration.h"

enum
{
  color_id,
  listbox_styleFor,
  checkbox_bold,
  checkbox_italic,
  checkbox_underlined,
  checkbox_slanted,
  checkbox_strikethrough,
  button_defaultFont,
  button_mathFont,
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
  ConfigDialogue(wxWindow *parent, Configuration *cfg);

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

  /*! Called if the color of an item has been changed */
  void OnChangeColor(wxColourPickerEvent& event);

  /*! Stores the settings from the configuration dialog.

    wxWidgets knows how to store the settings to gconf, the registry or wherever the current
    system expects settings to be saved to.
  */
  void WriteSettings();

private:
  const int mMinPanelWidth = 500;
  const int mMinPanelHeight = 500;

private:
  enum newVariables
  {
    MAXIMA_DEFAULT_LISP,
    MAXIMA_IMAGESDIR,
    MAXIMA_USERDIR,
    MAXIMA_DIRECTORY,
    MAXIMA_TEMPDIR,
    MAXIMA_OBJDIR,
    MAXIMA_DOC_PREFIX,
    GCL_GC_PAGE_THRESH,
    GCL_GC_ALLOC_MIN,
    GCL_GC_PAGE_MAX,
    GCL_MEM_MULTIPLE,
    GCL_MULTIPROCESS_MEMORY_POOL,
    LANG,
    HOME,
    VAR_DELETE
  };
  std::unique_ptr<struct NSVGrasterizer, decltype(std::free)*> m_svgRast{nullptr, std::free};
  //! The configuration storage
  Configuration *m_configuration;
  
  WX_DECLARE_STRING_HASH_MAP(wxString, StringHash);
  WX_DECLARE_STRING_HASH_MAP(long, Languages);
  Languages m_languages;
  /*! TheSample text that is shown by the style selector.

    This is a piece of text that shows the user how the selected style will look.
  */
  class ExamplePanel : public wxPanel
  {
  public:
    //! The constructor
    ExamplePanel(wxWindow *parent, int id, wxPoint pos, wxSize size) : wxPanel(parent, id, pos, size)
      { Connect(wxEVT_PAINT, wxPaintEventHandler(ConfigDialogue::ExamplePanel::OnPaint)); };

    //! Sets the text style of the example
    void SetStyle(const Style &style)
      {
        m_style = style;
        Refresh();
      }

  private:
    /*! Actually updates the formatting example

      This function is called after ConfigDialogue::UpdateExample() changes the example's style.
    */
    void OnPaint(wxPaintEvent &event);

    //! The text style of this example
    Style m_style;
  };


  /*! begin wxGlade: ConfigDialogue::methods

    This method sets the window title, the tool tips etc.
  */
  void SetCheckboxValues();

  //! Calculates the size of the images for a configuration tab
  int GetImageSize();

  //! The panel that allows to choose which formats to put on the clipboard
  wxWindow *CreateClipboardPanel();

  //! The panel that allows to choose which formats to put on the clipboard
  wxWindow *CreateRevertToDefaultsPanel();

  wxCheckBox *m_copyBitmap, *m_copyMathML, *m_copyMathMLHTML, *m_copyRTF, *m_copySVG;
#if wxUSE_ENH_METAFILE
  wxCheckBox *m_copyEMF;
#endif

  //! The panel that allows to set the editing options
  wxWindow *CreateWorksheetPanel();

  //! A panel that allows to set general options
  wxWindow *CreateOptionsPanel();

  //! The panel that allows to set options affecting the export functionality
  wxWindow *CreateExportPanel();

  //! The panel that allows to change styles
  wxWindow *CreateStylePanel();

  //! The panel that allows to change maxima-specific configurations.
  wxWindow *CreateMaximaPanel();

  //! The panel that allows to specify startup commands
  wxWindow *CreateStartupPanel();
  
protected:
  Worksheet *m_sampleWorksheet = NULL;
  wxGrid *m_maximaEnvVariables;
  void OnImport(wxCommandEvent& event);
  static void CopyConfig(wxConfigBase *src, wxConfigBase *dst, wxString dir = wxT("/"));
  void OnReloadAll(wxCommandEvent& event);
  void OnReloadStyles(wxCommandEvent& event);
  void OnResetAllToDefaults(wxCommandEvent& event);
  void OnExportAll(wxCommandEvent& event);
  void OnResetStyles(wxCommandEvent& event);
  void OnChangeMaximaEnvVar(wxGridEvent& event);
  void OnMaximaEnvRightClick(wxGridEvent& event);
  void OnMouseMotion_MaximaEnv(wxMouseEvent &event);
  void OnNewEnvMenu(wxCommandEvent &event);
  void OnClickMaximaEnvVal(int row);
  void OnChangeMaximaCellClick(wxGridEvent& event);

//! The name of maxima's startup file.
  wxString m_startupFileName;
  //! The name of wxMaxima's startup file.
  wxString m_wxStartupFileName;
  //! The text "Maxima Program" that can change color.
  wxStaticText *m_mp;
  StringHash m_maximaEnvDoc;
  //! Autodetect the maxima location?
  wxRadioButton *m_autodetectMaxima;
  //! The radio button that is set if m_autodetectMaxima is unset
  wxRadioButton *m_noAutodetectMaxima;
  wxRadioButton *m_autodetectHelpBrowser;
  wxRadioButton *m_noAutodetectHelpBrowser;
  wxRadioButton *m_internalHelpBrowser;
  wxCheckBox *m_singlePageManual;
  wxRadioButton *m_maximaUsesInternalHelp;
  wxRadioButton *m_maximaUsesHtmlHelp;
  wxRadioButton *m_maximaUsesWxmaximaHelp;
  //! Autodetect the mathJaX location?
  wxRadioButton *m_autodetectMathJaX;
  //! The radio button that is set if m_autodetectMathJaX is unset
  wxRadioButton *m_noAutodetectMathJaX;
  //! Called if the currently active dialogue sheet is changed.
  void OnTabChange(wxBookCtrlEvent &event);

  //! Called if the user changes the style that is to be edited.
  void OnStyleToEditChanged(wxCommandEvent &event);

  // begin wxGlade: ConfigDialogue::attributes
  //! A textbox containing maxima's startup commands
  wxTextCtrl *m_startupCommands;
  //! A textbox containing wxMaxima's startup commands
  wxTextCtrl *m_wxStartupCommands;
  wxTextCtrl *m_maximaUserLocation;
  wxTextCtrl *m_helpBrowserUserLocation;
  wxTextCtrl *m_documentclass;
  wxTextCtrl *m_documentclassOptions;
  wxTextCtrl *m_texPreamble;
  wxCheckBox *m_autoSave;
  wxButton *m_mpBrowse;
  wxButton *m_wxMathMLBrowse;
  wxTextCtrl *m_additionalParameters;
  wxTextCtrl *m_mathJaxURL;
  wxChoice *m_language;
  wxTextCtrl *m_symbolPaneAdditionalChars;
  wxCheckBox *m_abortOnError;
  wxCheckBox *m_offerKnownAnswers;
  wxCheckBox *m_restartOnReEvaluation;
  wxCheckBox *m_wrapLatexMath;
  wxCheckBox *m_usesvg;
  wxCheckBox *m_antialiasLines;
  wxSpinCtrl *m_defaultFramerate;
  wxSpinCtrl *m_defaultPlotWidth;
  wxSpinCtrl *m_defaultPlotHeight;
  wxSpinCtrl *m_displayedDigits;
  wxRadioButton *m_displayNDigits;
  wxRadioButton *m_displayAllDigits;
  wxRadioButton *m_linebreaksInLongNums;
  wxSpinCtrl *m_maxClipbrdBitmapMegabytes;
  //! A checkbox that asks if TeX should put the exponents above or after the subscripts.
  wxCheckBox *m_TeXExponentsAfterSubscript;
  //! A checkbox that asks if TeX should use the \\partial symbol for representing diff()
  wxCheckBox *m_usePartialForDiff;
  //! A checkbox that asks if all newlines in text cells have to be passed to HTML.
  wxCheckBox *m_exportContainsWXMX;
  wxCheckBox *m_printBrackets;
  wxChoice *m_exportWithMathJAX;
  wxCheckBox *m_matchParens;
  wxCheckBox *m_showMatchingParens;
  wxChoice *m_showLength;
  wxChoice *m_autosubscript;
  wxRadioButton *m_enterEvaluates;
  wxRadioButton *m_ctrlEnterEvaluates;
  wxCheckBox *m_numpadEnterEvaluates;
  wxCheckBox *m_saveImgFileName;
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
  wxButton *m_getStyleFont;
  wxListBox *m_styleFor;
  //! An example rectangle with the font color
  wxColourPickerCtrl *m_styleColor;
  wxCheckBox *m_boldCB;
  wxCheckBox *m_italicCB;
  wxCheckBox *m_slantedCB;
  wxCheckBox *m_strikethroughCB;
  wxCheckBox *m_underlinedCB;
  wxCheckBox *m_fixedFontInTC;
  wxCheckBox *m_unixCopy;
  wxCheckBox *m_changeAsterisk;
  wxCheckBox *m_hidemultiplicationSign;
  wxCheckBox *m_latin2Greek;
  wxCheckBox *m_useUnicodeMaths;
  wxCheckBox *m_keepPercentWithSpecials;
  wxBookCtrlBase *m_notebook;
  wxButton *m_saveStyle, *m_loadStyle;
  wxSpinCtrl *m_defaultPort;
  ExamplePanel *m_examplePanel;
  wxSpinCtrl *m_maxGnuplotMegabytes;
  wxSpinCtrl *m_autosaveMinutes;
  wxTextCtrl *m_autoMathJaxURL;
  int m_maximaEmvRightClickRow = 0;
  //! Is called when the path to the maxima binary was changed.
  void MaximaLocationChanged(wxCommandEvent &unused);

  //! Is called when the path to the maxima binary was changed.
  void UsesvgChanged(wxCommandEvent &event);

  //! Is called when the configuration dialog is closed.
  void OnClose(wxCloseEvent &event);

  //! Starts the file chooser that allows selecting where the maxima binary lies
  void OnMpBrowse(wxCommandEvent &event);

  void OnwxMathMLBrowse(wxCommandEvent &event);

  void OnHelpBrowserBrowse(wxCommandEvent&  event);

  void OnIdle(wxIdleEvent &event);

  //! Called if a new item type that is to be styled is selected
  void OnChangeStyle(wxCommandEvent &event);

  //! A message dialog that appears if a change cannot be applied now.
  void OnChangeWarning(wxCommandEvent &event);

  //! Called if one of the checkboxes for bold, italic or underlined is toggled
  void OnCheckbox(wxCommandEvent &event);

  //! Sets the style example's style on style changes.
  void UpdateExample();

  //! Called if the font family is changed.
  void OnChangeFontFamily(wxCommandEvent &event);

  //! A "export the configuration" dialog
  void LoadSave(wxCommandEvent &event);

  //! Get the style currently selected in the m_styleFor control
  TextStyle GetSelectedStyle() const;

  //! The size of the text font
  int m_fontSize;
  //! The size of the maths font.
  int m_mathFontSize;

  //! A list containing the pictograms for the tabs.
  std::unique_ptr<wxImageList> m_imageList;
};

#ifndef __WXMSW__

#endif // __WXMSW__

#endif // CONFIG_H
