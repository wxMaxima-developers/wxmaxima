// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*\file

  The C code for ConfigDialogue, the preferences dialog.
*/

#include "ConfigDialogue.h"
#include "WXMformat.h"
#include "BTextCtrl.h"
#include "cells/Cell.h"
#include "BinaryNameCtrl.h"
#include "Dirstructure.h"
#include "ArtProvider.h"
#include "WrappingStaticText.h"
#include "art/config/edit-copy-confdialogue.h"
#include "art/config/options.h"
#include "art/config/document-export.h"
#include "art/config/editing.h"
#include "art/config/styles.h"
#include "art/config/edit-copy_backup.h"
#include "art/config/maxima.h"
#include "art/config/view-refresh.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "ToolBar.h"
#include <cstdlib>
#include <wx/colordlg.h>
#include <wx/memtext.h>
#include <wx/config.h>
#include <wx/dcbuffer.h>
#include <wx/dirdlg.h>
#include <wx/display.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/font.h>
#include <wx/fontdlg.h>
#include <wx/mstream.h>
#include <wx/persist/toplevel.h>
#include <wx/scrolwin.h>
#include <wx/settings.h>
#include <wx/txtstrm.h>
#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include "sampleWorksheet.h"
#include <utility>
#include <algorithm>

extern unsigned char GTK_PRINT_SVG_GZ[];
extern size_t GTK_PRINT_SVG_GZ_SIZE;

/* declared in wxm_media_playback_start_svg.h,
   which is included in Toolbar.cpp */
extern unsigned char MEDIA_PLAYBACK_START_SVG_GZ[];
extern size_t MEDIA_PLAYBACK_START_SVG_GZ_SIZE;

#define CONFIG_ICON_SCALE (1.0)

int ConfigDialogue::GetImageSize() {
  int ppi;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;

  int display_idx = wxDisplay::GetFromWindow(this);
  if (display_idx < 0)
    ppi = 72;
  else
    ppi = wxDisplay(display_idx).GetPPI().x;
#else
  ppi = wxGetDisplayPPI().x;
#endif
  if (ppi < 10)
    ppi = 72;

  int targetSize = std::max(ppi, 75) * CONFIG_ICON_SCALE;

  int sizeA = 128 << 4;
  while (sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  }

  int sizeB = 192 << 4;
  while (sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if (std::abs(targetSize - sizeA) < std::abs(targetSize - sizeB)) {
    return sizeA;
  } else {
    return sizeB;
  }
}

ConfigDialogue::ConfigDialogue(wxWindow *parent)
  : wxPropertySheetDialog() {
#if defined __WXOSX__
  SetSheetStyle(wxPROPSHEET_CHOICEBOOK);
#else
  SetSheetStyle(wxPROPSHEET_LISTBOOK);
#endif
  Create(parent, wxID_ANY, _("wxMaxima configuration"), wxDefaultPosition,
         wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
  m_maximaEnvDoc["MAXIMA_DEFAULT_LISP"] =
    _("If maxima versions compiled with different lisps are installed: The "
      "name of the lisp to use by default");
  m_maximaEnvDoc["MAXIMA_OBJDIR"] =
    _("Tells maxima where to store the result of compiling libraries");
  m_maximaEnvDoc["MAXIMA_USERDIR"] =
    _("The directory containing the startup files, any user libraries and, "
      "if MAXIMA_OBJDIR isn't set the subdirectory with the results from "
      "compiling maxima libraries.");
  m_maximaEnvDoc["MAXIMA_TEMPDIR"] =
    _("The directory maxima places temporary files in, for example plots "
      "that are to be included in the worksheet.");
  m_maximaEnvDoc["MAXIMA_IMAGESDIR"] =
    _("The directory the compiled versions of maxima are placed in");
  m_maximaEnvDoc["MAXIMA_DOC_PREFIX"] =
    _("The directory the maxima manual can be found in");
  m_maximaEnvDoc["HOME"] = _("The directory the user's home directory is in");
  m_maximaEnvDoc["PATH"] =
    _("The list of directories the operating system looks for programs in");
  m_maximaEnvDoc["LANG"] = _("Sets the language, line ending type and charset "
                             "encoding programs will use");
  m_maximaEnvDoc["PAGER"] = _("Setting this to \"cat\" causes gnuplot not to "
                              "halt if there is much output");
  m_maximaEnvDoc["GCL_GC_PAGE_THRESH"] =
    _("If maxima was compiled by GCL: Only garbage collect past this "
      "heapsize fraction of working mem");
  m_maximaEnvDoc["GCL_GC_ALLOC_MIN"] =
    _("If maxima was compiled by GCL: Minimum allocation fraction between "
      "Garbage Collects");
  m_maximaEnvDoc["GCL_GC_PAGE_MAX"] =
    _("If maxima was compiled by GCL: Garbage Collect at minimum allocation "
      "past this heapsize");
  m_maximaEnvDoc["GCL_MEM_MULTIPLE"] =
    _("If maxima was compiled by GCL: The fraction of the total installed "
      "RAM to reserve for maxima");
  m_maximaEnvDoc["GCL_MULTIPROCESS_MEMORY_POOL"] =
    _("If maxima was compiled by GCL: Share the allocated memory between gcl "
      "processes. Allows more than one gcl-compiled maxima to run at the "
      "same time, but might provoke crashes.");
  m_languages[_("(Use default language)")] = wxLANGUAGE_DEFAULT;
  m_languages[_("Catalan")] = wxLANGUAGE_CATALAN;
  m_languages[_("Chinese (Simplified)")] = wxLANGUAGE_CHINESE_SIMPLIFIED;
  m_languages[_("Chinese (traditional)")] = wxLANGUAGE_CHINESE_TRADITIONAL;
  m_languages[_("Czech")] = wxLANGUAGE_CZECH;
  m_languages[_("Danish")] = wxLANGUAGE_DANISH;
  m_languages[_("English")] = wxLANGUAGE_ENGLISH;
  m_languages[_("Finnish")] = wxLANGUAGE_FINNISH;
  m_languages[_("French")] = wxLANGUAGE_FRENCH;
  m_languages[_("Galician")] = wxLANGUAGE_GALICIAN;
  m_languages[_("German")] = wxLANGUAGE_GERMAN;
  m_languages[_("Greek")] = wxLANGUAGE_GREEK;
  m_languages[_("Hungarian")] = wxLANGUAGE_HUNGARIAN;
  m_languages[_("Italian")] = wxLANGUAGE_ITALIAN;
  m_languages[_("Japanese")] = wxLANGUAGE_JAPANESE;
#if wxCHECK_VERSION(3, 0, 1)
  m_languages[_("Kabyle")] = wxLANGUAGE_KABYLE;
#endif
  m_languages[_("Norwegian")] = wxLANGUAGE_NORWEGIAN_BOKMAL;
  m_languages[_("Polish")] = wxLANGUAGE_POLISH;
  m_languages[_("Portuguese (Brazilian)")] = wxLANGUAGE_PORTUGUESE_BRAZILIAN;
  m_languages[_("Russian")] = wxLANGUAGE_RUSSIAN;
  m_languages[_("Spanish")] = wxLANGUAGE_SPANISH;
  m_languages[_("Turkish")] = wxLANGUAGE_TURKISH;
  m_languages[_("Ukrainian")] = wxLANGUAGE_UKRAINIAN;

  m_configuration = std::unique_ptr<Configuration>(new Configuration());
  SetSheetInnerBorder(3);
  SetSheetOuterBorder(3);

  SetName("Configuration");
  m_notebook = GetBookCtrl();

#if defined __WXOSX__
#else
  int imgSize = GetImageSize();
  m_imageList = std::unique_ptr<wxImageList>(new wxImageList(imgSize, imgSize, false, 0));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("editing"), imgSize, EDITING_SVG_GZ,
                                         EDITING_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("maxima"), imgSize, MAXIMA_SVG_GZ,
                                         MAXIMA_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("styles"), imgSize, STYLES_SVG_GZ,
                                         STYLES_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("document-export"), imgSize,
                                         DOCUMENT_EXPORT_SVG_GZ,
                                         DOCUMENT_EXPORT_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("options"), imgSize, OPTIONS_SVG_GZ,
                                         OPTIONS_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("edit-copy"), imgSize,
                                         EDIT_COPY_CONFDIALOGUE_SVG_GZ,
                                         EDIT_COPY_CONFDIALOGUE_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("media-playback-start"), imgSize,
                                         MEDIA_PLAYBACK_START_SVG_GZ,
                                         MEDIA_PLAYBACK_START_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("gtk-print"), imgSize, GTK_PRINT_SVG_GZ,
                                         GTK_PRINT_SVG_GZ_SIZE));
  m_imageList->Add(ArtProvider::GetImage(this, wxS("edit-undo"), imgSize, VIEW_REFRESH_SVG_GZ,
                                         VIEW_REFRESH_SVG_GZ_SIZE));
  m_notebook->SetImageList(m_imageList.get());
#endif
  m_notebook->AddPage(CreateWorksheetPanel(), _("Worksheet"), true, 0);
  m_notebook->AddPage(CreateMaximaPanel(), _("Maxima"), false, 1);
  m_notebook->AddPage(CreateStylePanel(), _("Style"), false, 2);
  m_notebook->AddPage(CreateExportPanel(), _("Export"), false, 3);
  m_notebook->AddPage(CreateOptionsPanel(), _("Options"), false, 4);
  m_notebook->AddPage(CreateClipboardPanel(), _("Copy"), false, 5);
  m_notebook->AddPage(CreateStartupPanel(), _("Startup commands"), false, 6);
  m_notebook->AddPage(CreatePrintPanel(), _("Printout settings"), false, 7);
  m_notebook->AddPage(CreateRevertToDefaultsPanel(),
                      _("Revert all to defaults"), false, 8);

#if !defined(__WXOSX__)
  CreateButtons(wxOK | wxCANCEL);
#endif

  wxConfigBase *config = wxConfig::Get();
  int notebookTab = 0;
  config->Read(wxS("ConfigDialogTab"), &notebookTab);
  if ((notebookTab < 0) ||
      ((unsigned int)notebookTab >= m_notebook->GetPageCount()))
    notebookTab = 0;
  m_notebook->SetSelection(notebookTab);

  LayoutDialog();

  SetTitle(_("wxMaxima configuration"));
  wxPersistenceManager::Get().RegisterAndRestore(this);

  SetCheckboxValues();

  Connect(wxEVT_CLOSE_WINDOW, wxCloseEventHandler(ConfigDialogue::OnClose),
          NULL, this);
  Connect(listbox_styleFor, wxEVT_LISTBOX,
          wxCommandEventHandler(ConfigDialogue::OnChangeStyle), NULL, this);
  Connect(language_id, wxEVT_COMBOBOX,
          wxCommandEventHandler(ConfigDialogue::OnChangeWarning), NULL, this);
  Connect(checkbox_bold, wxEVT_CHECKBOX,
          wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_italic, wxEVT_CHECKBOX,
          wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_slanted, wxEVT_CHECKBOX,
          wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_strikethrough, wxEVT_CHECKBOX,
          wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_underlined, wxEVT_CHECKBOX,
          wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(save_id, wxEVT_BUTTON,
          wxCommandEventHandler(ConfigDialogue::LoadSave), NULL, this);
  Connect(load_id, wxEVT_BUTTON,
          wxCommandEventHandler(ConfigDialogue::LoadSave), NULL, this);
  Connect(style_font_family, wxEVT_BUTTON,
          wxCommandEventHandler(ConfigDialogue::OnChangeFontFamily), NULL,
          this);
}

ConfigDialogue::~ConfigDialogue() {}

void ConfigDialogue::UsesvgChanged(
#ifdef __WXOSX__
                                   wxCommandEvent &event
#else
                                   wxCommandEvent &WXUNUSED(event)
#endif
                                   ) {
#ifdef __WXOSX__
  if (event.IsChecked()) {
    m_usesvg->SetForegroundColour(*wxRED);
  } else {
    wxSystemSettings systemSettings;
    m_usesvg->SetForegroundColour(systemSettings.GetColour(wxSYS_COLOUR_WINDOWTEXT));
  }
#endif
}

void ConfigDialogue::SetCheckboxValues() {
  Configuration *configuration = m_configuration.get();

  m_showUserDefinedLabels->SetToolTip(_(
                                        "Maxima assigns each command/equation an automatic label (which looks "
                                        "like %i1 or %o1). If a command begins with a descriptive name followed "
                                        "by a : wxMaxima will call the descriptive name a \"user-defined label\" "
                                        "instead. This selection now allows to tell wxMaxima if to show only "
                                        "automatic labels, automatic labels if there aren't user-defined ones or "
                                        "no label at all until a user-defined label can be found by wxMaxima's "
                                        "heuristics. If automatic labels are suppressed extra vertical space is "
                                        "added between equations in order to ease discerning which line starts a "
                                        "new equation and which one only continues the one from the last line."));
  m_enterEvaluates->SetToolTip(
                               _("If this checkbox isn't checked the current command is sent to maxima "
                                 "only on pressing Ctrl+Enter"));
  m_abortOnError->SetToolTip(
                             _("If multiple cells are evaluated in one go: Abort evaluation if "
                               "wxMaxima detects that maxima has encountered any error."));
  m_openHCaret->SetToolTip(
                           _("If this checkbox is set a new code cell is opened as soon as maxima "
                             "requests data. If it isn't set a new code cell is opened in this case "
                             "as soon as the user starts typing in code."));
  m_restartOnReEvaluation->SetToolTip(
                                      _("Maxima provides no \"forget all\" command that flushes all settings a "
                                        "maxima session could make. wxMaxima therefore normally defaults to "
                                        "starting a fresh maxima process every time the worksheet is to be "
                                        "re-evaluated. As this needs a little bit of time this switch allows "
                                        "to disable this behavior."));
  m_maximaUserLocation->SetToolTip(
                                   _("Enter the path to the Maxima executable."));
  m_additionalParameters->SetToolTip(_("Additional parameters for Maxima"
                                       " (e.g. -l clisp)."));

  m_enableDynamicMemorySpace->SetToolTip(_("If enabled the value in the spinbox is used new memory limit for maxima "
                                           "equal to: -X \"--dynamic-space-size <value>\""));
  m_dynamicMemorySpace->SetToolTip(_("The new memory limit for maxima in MB"));

  m_mathJaxURL->SetToolTip(
                           _("The URL MathJaX.js should be downloaded from by our HTML export."));
  m_texPreamble->SetToolTip(_("Additional commands to be added to the preamble "
                              "of LaTeX output for pdftex."));
  m_useUnicodeMaths->SetToolTip(
                                _("If the font provides big parenthesis symbols: Use them when big "
                                  "parenthesis are needed for maths display."));
  m_autoSave->SetToolTip(
                         _("If this checkbox is checked wxMaxima automatically saves the file "
                           "closing and every few minutes giving wxMaxima a more "
                           "cellphone-app-like feel as the file is virtually always saved. If "
                           "this checkbox is unchecked from time to time a backup is made in the "
                           "temp folder instead."));
  m_defaultFramerate->SetToolTip(_("Define the default speed (in frames per "
                                   "second) animations are played back with."));
  m_maxGnuplotMegabytes->SetToolTip(
                                    _("wxMaxima normally stores the gnuplot sources for every plot made "
                                      "using draw() in order to be able to open plots interactively in "
                                      "gnuplot later. This setting defines the limit [in Megabytes per plot] "
                                      "for this feature."));
  m_defaultPlotWidth->SetToolTip(
                                 _("The default width for embedded plots. Can be read out or overridden "
                                   "by the maxima variable wxplot_size"));
  m_defaultPlotHeight->SetToolTip(
                                  _("The default height for embedded plots. Can be read out or overridden "
                                    "by the maxima variable wxplot_size."));
  m_displayedDigits->SetToolTip(
                                _("If numbers are getting longer than this number of digits they will be "
                                  "displayed abbreviated by an ellipsis."));
  m_TeXExponentsAfterSubscript->SetToolTip(
                                           _("In the LaTeX output: Put exponents after an eventual subscript "
                                             "instead of above it. Might increase readability for some fonts and "
                                             "short subscripts."));
  m_usePartialForDiff->SetToolTip(
                                  _("Use the \"partial derivative\" symbol to represent the fraction that "
                                    "represents a diff() when exporting LaTeX"));
  m_wrapLatexMath->SetToolTip(
                              _("Wrap equations exported by the \"copy as LaTeX\" feature between \\[ "
                                "and \\] as equation markers"));
  m_bitmapScale->SetToolTip(
                            _("Normally html expects images to be rather low-res but space saving. "
                              "These images tend to look rather blurry when viewed on modern "
                              "screens. Therefore this setting was introduces that selects the "
                              "factor by which the HTML export increases the resolution in respect "
                              "to the default value."));
  m_printScale->SetToolTip(_("A scale factor for the printout. Helpful for "
                             "printing big equations on small pdf pages."));
  m_exportContainsWXMX->SetToolTip(
                                   _("If this option is set the .wxmx source of the current file is copied "
                                     "to a place a link to is put into the result of an export."));
  m_printBrackets->SetToolTip(
                              _("For each Text-, Sectioning or code cell wxMaxima can display a "
                                "bracket showing the extend of the cell and allowing to fold it. This "
                                "setting now tells if this bracket is to be printed, as well."));
  m_exportWithMathJAX->SetToolTip(
                                  _("MathJAX creates scalable High-Quality representations of 2D Maths "
                                    "that can be used for Drag-And-Drop and provides accessibility "
                                    "options. The disadvantage of MathJAX is that it needs JavaScript and "
                                    "a little bit of time in order to typeset equations.\nMathML is much "
                                    "faster than MathJaX, if it is supported by the browser. But many "
                                    "MathML implementations tend to lack necessary features.\nBitmaps tend "
                                    "to need more band width than the other two options. They lack support "
                                    "for advanced features like drag-and-drop or accessibility. Also they "
                                    "have problems aligning and scaling with the rest of the text and "
                                    "might use fonts that don't match the rest of the document."));
  m_usesvg->SetToolTip(_("PNG images can be read by old wxMaxima versions - "
                         "but aren't really scalable."));
  m_matchParens->SetToolTip(_("Automatically insert a closing parenthesis the "
                              "moment the user enters an opening one."));
  m_showMatchingParens->SetToolTip(
                                   _("Highlight the opening or closing parenthesis for the parenthesis the "
                                     "cursor is at."));
  m_showLength->SetToolTip(_("Show long expressions in wxMaxima document."));
  m_autosubscript->SetToolTip(
                              _("false=Don't generate subscripts\ntrue=Automatically convert "
                                "underscores to subscript markers if the would-be subscript is a "
                                "number or a single letter\nall=_ marks subscripts."));
  m_language->SetToolTip(_("Language used for wxMaxima GUI."));
  m_symbolPaneAdditionalChars->SetToolTip(
                                          _("Symbols that are entered or copied here will appear in the symbols "
                                            "sidebar so they can be entered into the worksheet easily."));
  m_documentclass->SetToolTip(
                              _("The document class LaTeX is instructed to use for our documents."));
  m_documentclassOptions->SetToolTip(
                                     _("The options the document class LaTeX is instructed to use for our "
                                       "documents gets."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_offerKnownAnswers->SetToolTip(
                                  _("wxMaxima remembers the answers to maxima's questions. If this "
                                    "checkbox is set it automatically offers to enter the last answer to "
                                    "this question the user has input."));
  m_changeAsterisk->SetToolTip(
                               _("Use centered dot and Minus, not Star and Hyphen"));
  m_hidemultiplicationSign->SetToolTip(
                                       _("Hide all multiplication signs that aren't really necessary for "
                                         "understanding the equation"));
  m_latin2Greek->SetToolTip(_("Change the text \"alpha\" and \"beta\" to the "
                              "corresponding greek letter?"));
  m_defaultPort->SetToolTip(_(
                              "The default port used for communication between Maxima and wxMaxima."));
  m_undoLimit->SetToolTip(
                          _("Save only this number of actions in the undo buffer. 0 means: save an "
                            "infinite number of actions."));
  m_recentItems->SetToolTip(
                            _("The number of recently opened files that is to be remembered."));
  m_incrementalSearch->SetToolTip(_(
                                    "Start searching while the phrase to search for is still being typed."));
  m_notifyIfIdle->SetToolTip(
                             _("Issue a notification if maxima finishes calculating while the "
                               "wxMaxima window isn't in focus."));

  m_hideBrackets->SetToolTip(
                             _("Hide the brackets that mark the extend of the worksheet cells at the "
                               "worksheet's right side and that contain the \"hide\" button of the "
                               "cell if the cells aren't active."));
  m_indentMaths->SetToolTip(_("Indent maths so all lines are in par with the "
                              "first line that starts after the label."));

  // The default values for all config items that will be used if there is no
  // saved configuration data for this item.
  m_documentclass->SetValue(configuration->Documentclass());
  m_maxClipbrdBitmapMegabytes->SetValue(
                                        configuration->MaxClipbrdBitmapMegabytes());
  m_documentclassOptions->SetValue(configuration->DocumentclassOptions());
  m_mathJaxURL->SetValue(configuration->MathJaXURL_User());
  m_autodetectMathJaX->SetValue(!configuration->MathJaXURL_UseUser());
  m_noAutodetectMathJaX->SetValue(configuration->MathJaXURL_UseUser());
  m_texPreamble->SetValue(configuration->TexPreamble());
  m_autoSave->SetValue(!configuration->AutoSaveAsTempFile());

  m_printMargin_Right->SetValue(configuration->PrintMargin_Right());
  m_printMargin_Left->SetValue(configuration->PrintMargin_Left());
  m_printMargin_Top->SetValue(configuration->PrintMargin_Top());
  m_printMargin_Bot->SetValue(configuration->PrintMargin_Bot());
  m_maximaEnvVariables->BeginBatch();
  if (m_maximaEnvVariables->GetNumberRows() > 0)
    m_maximaEnvVariables->DeleteRows(0, m_maximaEnvVariables->GetNumberRows());
  wxEnvVariableHashMap::const_iterator it;
  for (it = m_configuration->MaximaEnvVars().begin();
       it != m_configuration->MaximaEnvVars().end(); ++it) {
    m_maximaEnvVariables->AppendRows(1);
    int row = m_maximaEnvVariables->GetNumberRows() - 1;
    m_maximaEnvVariables->SetCellValue(row, 0, it->first);
    m_maximaEnvVariables->SetCellValue(row, 1, it->second);
  }
  m_maximaEnvVariables->AppendRows(1);
  m_maximaEnvVariables->SetColLabelValue(0, _("Variable"));
  m_maximaEnvVariables->SetColLabelValue(1, _("Value"));
  m_maximaEnvVariables->AutoSize();
  m_maximaEnvVariables->EndBatch();
  m_maximaEnvVariables->GetParent()->GetParent()->Layout();

  m_maximaUserLocation->SetValue(configuration->MaximaUserLocation());

  m_additionalParameters->SetValue(configuration->MaximaParameters());
  m_enableDynamicMemorySpace->SetValue(configuration->MaximaEnableCustomDynamicMemory());
  m_dynamicMemorySpace->SetValue(configuration->MaximaCustomDynamicMemory());

  m_usesvg->SetValue(configuration->UseSVG());

  m_TeXExponentsAfterSubscript->SetValue(
                                         configuration->TeXExponentsAfterSubscript());
  m_usePartialForDiff->SetValue(configuration->UsePartialForDiff());
  m_wrapLatexMath->SetValue(configuration->WrapLatexMath());
  m_exportContainsWXMX->SetValue(configuration->ExportContainsWXMX());
  m_printBrackets->SetValue(configuration->PrintBrackets());
  m_exportWithMathJAX->SetSelection(configuration->HTMLequationFormat());
  m_matchParens->SetValue(configuration->GetMatchParens());
  m_showMatchingParens->SetValue(configuration->ShowMatchingParens());
  m_showLength->SetSelection(configuration->ShowLength());
  m_autosubscript->SetSelection(configuration->GetAutosubscript_Num());
  m_changeAsterisk->SetValue(configuration->GetChangeAsterisk());
  m_hidemultiplicationSign->SetValue(configuration->HidemultiplicationSign());
  m_latin2Greek->SetValue(configuration->Latin2Greek());
  if (configuration->EnterEvaluates())
    m_enterEvaluates->SetValue(true);
  else
    m_ctrlEnterEvaluates->SetValue(true);
  m_numpadEnterEvaluates->SetValue(configuration->NumpadEnterEvaluates());
  m_saveImgFileName->SetValue(configuration->SaveImgFileName());
  m_saveUntitled->SetValue(configuration->SaveUntitled());
  m_openHCaret->SetValue(configuration->GetOpenHCaret());
  m_insertAns->SetValue(configuration->GetInsertAns());
  m_autoIndent->SetValue(configuration->GetAutoIndent());
  m_cursorJump->SetValue(configuration->CursorJump());
  m_hideBrackets->SetValue(configuration->HideBrackets());
  m_indentMaths->SetValue(configuration->IndentMaths());
  int val = 0;
  if (configuration->GetAutoWrap())
    val = 1;
  //  if(configuration->GetAutoWrapCode()) val = 2;
  m_recentItems->SetValue(configuration->RecentItems());
  m_autoWrap->SetSelection(val);
  m_labelWidth->SetValue(configuration->LabelWidth());
  m_undoLimit->SetValue(configuration->UndoLimit());
  m_bitmapScale->SetValue(configuration->BitmapScale());
  m_printScale->SetValue(configuration->PrintScale());
  m_fixReorderedIndices->SetValue(configuration->FixReorderedIndices());
  m_incrementalSearch->SetValue(configuration->IncrementalSearch());
  m_notifyIfIdle->SetValue(configuration->NotifyIfIdle());
  m_fixedFontInTC->SetValue(configuration->FixedFontInTextControls());
  m_offerKnownAnswers->SetValue(m_configuration->OfferKnownAnswers());
  m_keepPercentWithSpecials->SetValue(configuration->CheckKeepPercent());
  m_abortOnError->SetValue(configuration->GetAbortOnError());
  m_restartOnReEvaluation->SetValue(configuration->RestartOnReEvaluation());
  m_defaultFramerate->SetValue(m_configuration->DefaultFramerate());
  m_maxGnuplotMegabytes->SetValue(configuration->MaxGnuplotMegabytes());
  m_autosaveMinutes->SetValue(configuration->AutosaveMinutes());
  m_defaultPlotWidth->SetValue(configuration->DefaultPlotWidth());
  m_defaultPlotHeight->SetValue(configuration->DefaultPlotHeight());
  m_displayedDigits->SetValue(configuration->GetDisplayedDigits());

  if (configuration->LineBreaksInLongNums() && configuration->ShowAllDigits())
    m_linebreaksInLongNums->SetValue(true);
  else {
    if (configuration->LineBreaksInLongNums())
      m_displayAllDigits->SetValue(true);
    else
      m_displayNDigits->SetValue(true);
  }
  m_symbolPaneAdditionalChars->SetValue(
                                        configuration->SymbolPaneAdditionalChars());
  m_getStyleFont->Enable(!configuration->GetStyle(GetSelectedStyle())->CantChangeFontName());
  m_showUserDefinedLabels->SetSelection(configuration->GetLabelChoice());
  unsigned int i = 0;
  // First set the language to "default".
  for (const auto &it2 : m_languages) {
    if (it2.second == wxLANGUAGE_DEFAULT) {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }
  // Now try to set the language to the one from the config
  i = 0;
  int lang = m_configuration->GetLanguage();
  for (const auto &it2 : m_languages) {
    if (it2.second == lang) {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }

  m_autoMathJaxURL->SetValue(m_configuration->MathJaXURL_Auto());
  m_singlePageManual->SetValue(m_configuration->SinglePageManual());
  m_internalHelpBrowser->SetValue(m_configuration->InternalHelpBrowser());
  m_singlePageManual->SetValue(configuration->SinglePageManual());

  if (!m_configuration->InternalHelpBrowser()) {
    m_autodetectHelpBrowser->SetValue(m_configuration->AutodetectHelpBrowser());
    m_noAutodetectHelpBrowser->SetValue(
                                        !m_configuration->AutodetectHelpBrowser());
  }
  m_maximaUserLocation->SetValue(m_configuration->MaximaUserLocation());
  m_autodetectMaxima->SetValue(m_configuration->AutodetectMaxima());
  m_noAutodetectMaxima->SetValue(!m_configuration->AutodetectMaxima());
  m_helpBrowserUserLocation->SetValue(
                                      m_configuration->HelpBrowserUserLocation());
  if (m_configuration->MaximaUsesWxmaximaBrowser()) {
    m_maximaUsesWxmaximaHelp->SetValue(true);
  } else {
    if (m_configuration->MaximaUsesHtmlBrowser())
      m_maximaUsesHtmlHelp->SetValue(true);
    else
      m_maximaUsesInternalHelp->SetValue(true);
  }
  m_maximaUsesHtmlHelp->SetValue(m_configuration->MaximaUsesHtmlBrowser());
  m_defaultPort->SetValue(m_configuration->DefaultPort());
  m_copyBitmap->SetValue(m_configuration->CopyBitmap());
  m_copyMathML->SetValue(m_configuration->CopyMathML());
  m_copyMathMLHTML->SetValue(m_configuration->CopyMathMLHTML());
  m_copyRTF->SetValue(m_configuration->CopyRTF());
  m_copySVG->SetValue(m_configuration->CopySVG());
#if wxUSE_ENH_METAFILE
  m_copyEMF->SetValue(m_configuration->CopyEMF());
#endif

  m_useUnicodeMaths->SetValue(m_configuration->UseUnicodeMaths());
}

wxWindow *ConfigDialogue::CreateWorksheetPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxStaticBoxSizer *displaySizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Display"));
  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(10, 2, 5, 5);
  auto *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);

  wxBoxSizer *PlotWidthHbox = new wxBoxSizer(wxHORIZONTAL);
  m_defaultPlotWidth = new wxSpinCtrl(
                                      displaySizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                      wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 100, 16384);
  PlotWidthHbox->Add(m_defaultPlotWidth, wxSizerFlags().Expand());
  wxStaticText *xx = new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY, _("x"));
  PlotWidthHbox->Add(xx, 0, wxALIGN_CENTER_VERTICAL, 0);
  m_defaultPlotHeight = new wxSpinCtrl(
                                       displaySizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                       wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 100, 16384);
  PlotWidthHbox->Add(m_defaultPlotHeight, wxSizerFlags().Expand());
  grid_sizer->Add(
                  new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY,
                                   _("Default plot size for new maxima sessions:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  grid_sizer->Add(PlotWidthHbox, wxSizerFlags());

  grid_sizer->Add(new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY,
                                   _("Show long expressions:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  wxArrayString showLengths;
  showLengths.Add(_("No"));
  showLengths.Add(_("If not very long"));
  showLengths.Add(_("If not extremely long"));
  showLengths.Add(_("Yes"));
  m_showLength = new wxChoice(displaySizer->GetStaticBox(), wxID_ANY,
                              wxDefaultPosition, wxDefaultSize, showLengths);
  grid_sizer->Add(m_showLength, 0, wxUP | wxDOWN, 5 * GetContentScaleFactor());

  grid_sizer->Add(new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY,
                                   _("Autowrap long lines:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  wxArrayString autoWrap;
  autoWrap.Add(_("No"));
  autoWrap.Add(_("Text Only"));
  //  autoWrap.Add(_("Text & Code"));
  m_autoWrap = new wxChoice(displaySizer->GetStaticBox(), wxID_ANY, wxDefaultPosition,
                            wxDefaultSize, autoWrap);
  grid_sizer->Add(m_autoWrap, 0, wxUP | wxDOWN, 5 * GetContentScaleFactor());

  grid_sizer->Add(new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY,
                                   _("Underscore converts to subscripts:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  wxArrayString autosubscripts;
  autosubscripts.Add(_("Never"));
  autosubscripts.Add(_("Integers and single letters"));
  autosubscripts.Add(_("All variable names"));
  m_autosubscript =
    new wxChoice(displaySizer->GetStaticBox(), wxID_ANY, wxDefaultPosition,
                 wxDefaultSize, autosubscripts);
  grid_sizer->Add(m_autosubscript, 0, wxUP | wxDOWN,
                  5 * GetContentScaleFactor());

  grid_sizer->Add(
                  new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY, _("Label width:")), 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  m_labelWidth = new wxSpinCtrl(
                                displaySizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, LABELWIDTH_MIN, LABELWIDTH_MAX);
  grid_sizer->Add(m_labelWidth, 0, wxUP | wxDOWN, 5 * GetContentScaleFactor());

  grid_sizer->Add(
                  new wxStaticText(displaySizer->GetStaticBox(), wxID_ANY, _("Show labels:")), 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  wxArrayString labelchoices;
  labelchoices.Add(_("Automatic labels (%i1, %o1,...)"));
  labelchoices.Add(_("User-defined labels if available"));
  labelchoices.Add(_("Only user-defined labels"));
  labelchoices.Add(_("Never"));
  m_showUserDefinedLabels =
    new wxChoice(displaySizer->GetStaticBox(), wxID_ANY, wxDefaultPosition,
                 wxDefaultSize, labelchoices);

  grid_sizer->Add(
                  m_showUserDefinedLabels,
                  wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));

  displaySizer->Add(grid_sizer,
                    wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *numDigitsSizer = new wxStaticBoxSizer(
                                                          wxVERTICAL, displaySizer->GetStaticBox(), _("Display of long numbers"));
  wxFlexGridSizer *numDigitsGrid = new wxFlexGridSizer(10, 2, 5, 5);

  numDigitsSizer->Add(
                      numDigitsGrid, wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));
  numDigitsGrid->Add(m_displayNDigits = new wxRadioButton(
                                                          numDigitsSizer->GetStaticBox(), -1,
                                                          _("Maximum number of displayed digits:")),
                     0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  m_displayedDigits = new wxSpinCtrl(
                                     numDigitsSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                     wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 20, INT_MAX);
  numDigitsGrid->Add(m_displayedDigits, wxSizerFlags());

  numDigitsGrid->Add(m_displayAllDigits =
                     new wxRadioButton(numDigitsSizer->GetStaticBox(), wxID_ANY,
                                       _("Display all digits")),
                     0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  numDigitsGrid->Add(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());

  numDigitsGrid->Add(m_linebreaksInLongNums =
                     new wxRadioButton(
                                       numDigitsSizer->GetStaticBox(), wxID_ANY,
                                       _("Display all and allow linebreaks in long numbers")),
                     0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL);
  numDigitsGrid->Add(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());
  displaySizer->Add(numDigitsSizer,
                    wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_hideBrackets = new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                                  _("Intelligently hide cell brackets"));
  displaySizer->Add(m_hideBrackets, wxSizerFlags());

  m_indentMaths = new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                                 _("Indent equations by the label width"));
  displaySizer->Add(m_indentMaths, wxSizerFlags());

  m_changeAsterisk =
    new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                   _("Use centered dot character for multiplication"));
  displaySizer->Add(m_changeAsterisk, wxSizerFlags());

  m_hidemultiplicationSign =
    new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                   _("Hide multiplication signs, if possible"));
  displaySizer->Add(m_hidemultiplicationSign, wxSizerFlags());

  m_latin2Greek =
    new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                   _("Change names of greek letters to greek letters"));
  displaySizer->Add(m_latin2Greek, wxSizerFlags());

  m_keepPercentWithSpecials =
    new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                   _("Keep percent sign with special symbols: %e, %i, etc."));
  displaySizer->Add(m_keepPercentWithSpecials, wxSizerFlags());

  m_showMatchingParens =
    new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                   _("Highlight the matching parenthesis"));
  displaySizer->Add(m_showMatchingParens, wxSizerFlags());

  m_fixedFontInTC = new wxCheckBox(displaySizer->GetStaticBox(), wxID_ANY,
                                   _("Fixed font in text controls"));
  displaySizer->Add(m_fixedFontInTC, wxSizerFlags());

  vsizer->Add(displaySizer, wxSizerFlags().Expand().Border(
                                                           wxALL, 5 * GetContentScaleFactor()));
  vsizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  wxStaticBoxSizer *actionSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Interaction"));

  m_incrementalSearch =
    new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY, _("Incremental Search"));
  actionSizer->Add(m_incrementalSearch, wxSizerFlags());

  m_insertAns = new wxCheckBox(
                               actionSizer->GetStaticBox(), wxID_ANY,
                               _("Insert % before an operator at the beginning of a cell"));
  actionSizer->Add(m_insertAns, wxSizerFlags());

  m_matchParens = new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY,
                                 _("Auto-insert closing parenthesis"));
  actionSizer->Add(m_matchParens, wxSizerFlags());

  m_autoIndent = new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY,
                                _("Auto-indent new lines"));
  actionSizer->Add(m_autoIndent, wxSizerFlags());

  m_cursorJump = new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY,
                                _("New lines: Jump to text"));
  actionSizer->Add(m_cursorJump, wxSizerFlags());

  m_openHCaret = new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY,
                                _("Open a cell when Maxima expects input"));
  actionSizer->Add(m_openHCaret, wxSizerFlags());

  m_offerKnownAnswers =
    new wxCheckBox(actionSizer->GetStaticBox(), wxID_ANY,
                   _("Offer answers for questions known from previous runs"));
  actionSizer->Add(m_offerKnownAnswers, wxSizerFlags());

  vsizer->Add(actionSizer, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));
  vsizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  wxStaticBoxSizer *evalSizer = new wxStaticBoxSizer(
                                                     wxVERTICAL, panel, _("Hotkeys for sending commands to maxima"));
  m_enterEvaluates =
    new wxRadioButton(evalSizer->GetStaticBox(), wxID_ANY,
                      _("Enter evaluates cells, Ctrl+Enter adds newlines"));
  evalSizer->Add(m_enterEvaluates, wxSizerFlags());
  m_ctrlEnterEvaluates =
    new wxRadioButton(evalSizer->GetStaticBox(), wxID_ANY,
                      _("Enter adds newlines, Ctrl+Enter evaluates cells"));
  evalSizer->Add(m_ctrlEnterEvaluates, wxSizerFlags());

  m_numpadEnterEvaluates =
    new wxCheckBox(evalSizer->GetStaticBox(), wxID_ANY,
                   _("\"Numpad Enter\" always evaluates cells"));
  evalSizer->Add(m_numpadEnterEvaluates, wxSizerFlags());
  vsizer->Add(evalSizer, wxSizerFlags().Expand().Border(
                                                        wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *evalPrivacy =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Privacy settings"));
  m_saveImgFileName =
    new wxCheckBox(evalPrivacy->GetStaticBox(), wxID_ANY,
                   _("Store filenames in image cells to enable image "
                     "reloading after re-opening the worksheet"));
  m_saveImgFileName->SetToolTip(
                                _("If this option is enabled, the filename and path of images inserted "
                                  "with \"Cell\" -> \"Insert Image...\" "
                                  "is stored in the worksheet file on save. This enables reloading of "
                                  "the image file via the "
                                  "context menu after re-opening the worksheet, but storing the "
                                  "filenames and paths may be an "
                                  "undesired data leak. If this option is disabled, reloading still "
                                  "works as long as the worksheet "
                                  "is not closed."
                                  "\n\nNote: Storing the filenames for the reload functionality is "
                                  "independent of the automatically "
                                  "generated image captions."));
  evalPrivacy->Add(m_saveImgFileName, wxSizerFlags());
  vsizer->Add(evalPrivacy, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));

  panel->FitInside();

  return panel;
}

wxWindow *ConfigDialogue::CreateRevertToDefaultsPanel() {
  wxPanel *panel = new wxPanel(m_notebook, wxID_ANY);
  // panel->SetScrollRate(5 * GetContentScaleFactor(),
  //                      5 * GetContentScaleFactor());
  // panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
  //                          GetContentScaleFactor() * mMinPanelHeight));

  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  // WrappingStaticText *helpText1 = new WrappingStaticText(
  //                                                   panel, wxID_ANY,
  //                                                   _("The buttons in this category reset wxMaxima's settings "
  //                                                     "immediately, once they are pressed."));
  // vsizer->Add(helpText1, wxSizerFlags()
  //        .Border(wxUP | wxDOWN, 5 * GetContentScaleFactor())
  //        .Expand());

  wxStaticBoxSizer *revertSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Revert changes"));

  wxButton *resetAllButton = new wxButton(revertSizer->GetStaticBox(), wxID_ANY,
                                          _("Reset all GUI settings"));
  resetAllButton->Connect(
                          wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnResetAllToDefaults),
                          NULL, this);
  revertSizer->Add(
                   resetAllButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());

  wxButton *resetStylesButton = new wxButton(revertSizer->GetStaticBox(), wxID_ANY,
                                             _("Reset the Style settings"));
  resetStylesButton->Connect(
                             wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnResetStyles), NULL,
                             this);
  revertSizer->Add(
                   resetStylesButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());
  wxButton *reloadAllButton =
    new wxButton(
                 revertSizer->GetStaticBox(), wxID_ANY, _("Reload all GUI settings from disc"));
  reloadAllButton->Connect(wxEVT_BUTTON,
                           wxCommandEventHandler(ConfigDialogue::OnReloadAll),
                           NULL, this);
  revertSizer->Add(
                   reloadAllButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());
  wxButton *reloadStylesButton =
    new wxButton(revertSizer->GetStaticBox(), wxID_ANY,
                 _("Reload the style settings from disc"));
  reloadStylesButton->Connect(
                              wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnReloadStyles), NULL,
                              this);
  revertSizer->Add(
                   reloadStylesButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());
  vsizer->Add(revertSizer, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *exportSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Import+Export"));

  wxButton *exportAllButton =
    new wxButton(exportSizer->GetStaticBox(), wxID_ANY, _("Export all settings"));
  exportAllButton->Connect(wxEVT_BUTTON,
                           wxCommandEventHandler(ConfigDialogue::OnExportAll),
                           NULL, this);
  exportSizer->Add(
                   exportAllButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());
  wxButton *exportStylesButton = new wxButton(
                                              exportSizer->GetStaticBox(), save_id, _("Export the style settings"));
  exportStylesButton->Connect(wxEVT_BUTTON,
                              wxCommandEventHandler(ConfigDialogue::LoadSave),
                              NULL, this);
  exportSizer->Add(
                   exportStylesButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());
  wxButton *importSettingsButton =
    new wxButton(exportSizer->GetStaticBox(), wxID_ANY, _("Import settings"));
  importSettingsButton->Connect(wxEVT_BUTTON,
                                wxCommandEventHandler(ConfigDialogue::OnImport),
                                NULL, this);
  exportSizer->Add(
                   importSettingsButton,
                   wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()).Expand());

  vsizer->Add(exportSizer, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));
  // WrappingStaticText *helpText2 = new WrappingStaticText(
  //                                                   panel, wxID_ANY,
  //                                                   _("While wxMaxima is controlled by the settings here "
  //                                                     "Maxima as a command-line program isn't controlled by "
  //                                                     "settings, except of the few wxMaxima settings that set "
  //                                                     "the value of variables within Maxima: Instead Maxima "
  //                                                     "is configured using environment variables, the Startup "
  //                                                     "File and command-line switches."));
  // vsizer->Add(helpText2, wxSizerFlags()
  //        .Border(wxUP | wxDOWN, 5 * GetContentScaleFactor())
  //        .Expand());
  panel->SetSizer(vsizer);
  panel->FitInside();
  return panel;
}

wxWindow *ConfigDialogue::CreateStartupPanel() {
  wxPanel *panel = new wxPanel(this, -wxID_ANY);
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);

  m_startupFileName = Dirstructure::Get()->UserConfDir();
  m_wxStartupFileName += m_startupFileName + wxS("wxmaxima-init.mac");
  m_startupFileName += wxS("maxima-init.mac");

  wxStaticBoxSizer *wxMaximaStartupSizer = new wxStaticBoxSizer(
                                                                wxVERTICAL, panel,
                                                                _("Maxima commands to be executed every time wxMaxima starts Maxima"));
  panel->SetToolTip(
                    _("The part of the output of these commands that isn't declared as "
                      "\"math\" might be suppressed by wxMaxima. As always maxima "
                      "commands are required to end in a \";\" or a \"$\""));

  // Read the contents of wxMaxima's startup file
  wxString contents;
  if (wxFileExists(m_wxStartupFileName)) {
    wxFileInputStream input(m_wxStartupFileName);
    if (input.IsOk()) {
      wxTextInputStream text(input, wxS('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      while (input.IsOk() && !input.Eof()) {
        wxString line = text.ReadLine();
        if ((!input.Eof()) || (line != wxEmptyString))
          contents += line + wxS("\n");
      }
    }
  }
  m_wxStartupCommands =
    new BTextCtrl(
                  wxMaximaStartupSizer->GetStaticBox(), wxID_ANY, m_configuration.get(),
                  wxEmptyString,
                  wxDefaultPosition,
                  wxSize(150 * GetContentScaleFactor(), 150 * GetContentScaleFactor()),
                  wxTE_MULTILINE | wxHSCROLL);
  m_wxStartupCommands->SetValue(contents);
  wxMaximaStartupSizer->Add(
                            m_wxStartupCommands,
                            wxSizerFlags(1).Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  wxStaticText *wxStartupFileLocation = new wxStaticText(
                                                         wxMaximaStartupSizer->GetStaticBox(), wxID_ANY,
                                                         _("wxMaxima startup file location: ") + m_wxStartupFileName);
  wxStartupFileLocation->SetToolTip(
                                    _("This file won't be read by maxima if maxima is used "
                                      "without wxMaxima. In order to add startup commands "
                                      "that are executed in this case, "
                                      "too, please add them to maxima-init.mac, instead."));
  wxMaximaStartupSizer->Add(
                            wxStartupFileLocation,
                            wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  vsizer->Add(wxMaximaStartupSizer, wxSizerFlags(1).Expand().Border(
                                                                    wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *maximaStartupSizer = new wxStaticBoxSizer(
                                                              wxVERTICAL, panel,
                                                              _("Maxima commands to be executed at every start of Maxima"));
  panel->SetToolTip(
                    _("The part of the output of these commands that isn't declared as "
                      "\"math\" might be suppressed by wxMaxima. As always maxima "
                      "commands are required to end in a \";\" or a \"$\""));
  // Read maxima's startup file's contents
  contents.Clear();
  if (wxFileExists(m_startupFileName)) {
    wxFileInputStream input(m_startupFileName);
    if (input.IsOk()) {
      wxTextInputStream text(input, wxS('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      while (input.IsOk() && !input.Eof()) {
        wxString line = text.ReadLine();
        if ((!input.Eof()) || (line != wxEmptyString))
          contents += line + wxS("\n");
      }
    }
  }
  m_startupCommands =
    new BTextCtrl(
                  maximaStartupSizer->GetStaticBox(), wxID_ANY, m_configuration.get(),
                  wxEmptyString,
                  wxDefaultPosition,
                  wxSize(150 * GetContentScaleFactor(), 150 * GetContentScaleFactor()),
                  wxTE_MULTILINE | wxHSCROLL);
  m_startupCommands->SetValue(contents);

  maximaStartupSizer->Add(
                          m_startupCommands,
                          wxSizerFlags(1).Expand().
                          Border(wxALL, 5 * GetContentScaleFactor()));
  wxStaticText *startupFileLocation =
    new wxStaticText(maximaStartupSizer->GetStaticBox(), wxID_ANY,
                     _("Maxima startup file location: ") + m_startupFileName);
  startupFileLocation->SetToolTip(
                                  _("Commands that are executed at every start of maxima."));
  maximaStartupSizer->Add(
                          startupFileLocation,
                          wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));
  vsizer->Add(maximaStartupSizer, wxSizerFlags(1).Expand().Border(
                                                                  wxALL, 5 * GetContentScaleFactor()));
  panel->FitInside();
  return panel;
}

wxWindow *ConfigDialogue::CreateExportPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);

  wxStaticBoxSizer *texSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("LaTeX"));
  wxFlexGridSizer *texGrid_sizer = new wxFlexGridSizer(9, 2, 5, 5);

  wxStaticText *dc = new wxStaticText(texSizer->GetStaticBox(), wxID_ANY,
                                      _("Documentclass for TeX export:"));
  m_documentclass = new wxTextCtrl(
                                   texSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                   wxSize(350 * GetContentScaleFactor(), wxDefaultSize.GetY()));
  texGrid_sizer->Add(dc, wxSizerFlags());
  texGrid_sizer->Add(m_documentclass, wxSizerFlags());

  wxStaticText *dco = new wxStaticText(texSizer->GetStaticBox(), wxID_ANY,
                                       _("Documentclass options:"));
  m_documentclassOptions = new wxTextCtrl(
                                          texSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                          wxSize(350 * GetContentScaleFactor(), wxDefaultSize.GetY()));
  texGrid_sizer->Add(dco, wxSizerFlags());
  texGrid_sizer->Add(m_documentclassOptions, wxSizerFlags());

  wxStaticText *tp =
    new wxStaticText(texSizer->GetStaticBox(), wxID_ANY,
                     _("Additional lines for the TeX preamble:"));
  m_texPreamble =
    new wxTextCtrl(
                   texSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                   wxSize(350 * GetContentScaleFactor(), 100), wxTE_MULTILINE | wxHSCROLL);
  texGrid_sizer->Add(tp, wxSizerFlags());
  texGrid_sizer->Add(m_texPreamble, wxSizerFlags());

  texSizer->Add(texGrid_sizer, wxSizerFlags(1).Expand().Border(
                                                               wxALL, 5 * GetContentScaleFactor()));
  m_TeXExponentsAfterSubscript = new wxCheckBox(
                                                texSizer->GetStaticBox(), wxID_ANY,
                                                _("LaTeX: Place exponents after, instead above subscripts"));
  texSizer->Add(m_TeXExponentsAfterSubscript, wxSizerFlags());

  m_usePartialForDiff = new wxCheckBox(
                                       texSizer->GetStaticBox(), wxID_ANY,
                                       _("LaTeX: Use the \"partial derivative\" symbol to represent diff()"));
  texSizer->Add(m_usePartialForDiff, wxSizerFlags());

  m_wrapLatexMath = new wxCheckBox(texSizer->GetStaticBox(), wxID_ANY,
                                   _("\"Copy LaTeX\" adds equation markers"));
  texSizer->Add(m_wrapLatexMath, wxSizerFlags());

  vsizer->Add(texSizer, wxSizerFlags().Expand().Border(
                                                       wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *html_sizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("HTML"));
  wxFlexGridSizer *htmlGrid_sizer = new wxFlexGridSizer(9, 2, 5, 5);
  wxStaticText *mju = new wxStaticText(html_sizer->GetStaticBox(), wxID_ANY,
                                       _("Export equations to HTML as:"));
  wxArrayString mathJaxChoices;
  mathJaxChoices.Add(_("TeX, interpreted by MathJaX"));
  mathJaxChoices.Add(_("Bitmaps"));
  mathJaxChoices.Add(_("MathML + MathJaX as Fill-In"));
  mathJaxChoices.Add(_("SVG graphics"));
  m_exportWithMathJAX =
    new wxChoice(html_sizer->GetStaticBox(), wxID_ANY, wxDefaultPosition,
                 wxDefaultSize, mathJaxChoices);
  htmlGrid_sizer->Add(mju, wxSizerFlags());
  htmlGrid_sizer->Add(m_exportWithMathJAX, wxSizerFlags());
  wxStaticText *mj = new wxStaticText(html_sizer->GetStaticBox(), wxID_ANY,
                                      _("URL MathJaX.js is located at:"));
  htmlGrid_sizer->Add(mj, wxSizerFlags());
  htmlGrid_sizer->Add(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());

  m_autodetectMathJaX =
    new wxRadioButton(html_sizer->GetStaticBox(), wxID_ANY, _("Automatic"),
                      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  htmlGrid_sizer->Add(
                      m_autodetectMathJaX,
                      wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  m_autoMathJaxURL = new wxTextCtrl(
                                    html_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                    wxSize(350 * GetContentScaleFactor(), wxDefaultSize.GetY()),
                                    wxTE_READONLY);
  htmlGrid_sizer->Add(m_autoMathJaxURL, wxSizerFlags());

  m_noAutodetectMathJaX =
    new wxRadioButton(html_sizer->GetStaticBox(), wxID_ANY, _("User specified"));
  htmlGrid_sizer->Add(m_noAutodetectMathJaX, 0,
                      wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      5 * GetContentScaleFactor());

  m_mathJaxURL = new wxTextCtrl(
                                html_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                wxSize(350 * GetContentScaleFactor(), wxDefaultSize.GetY()));
  htmlGrid_sizer->Add(m_mathJaxURL, wxSizerFlags());
  html_sizer->Add(htmlGrid_sizer, wxSizerFlags().Expand().Border(
                                                                 wxALL, 5 * GetContentScaleFactor()));

  m_exportContainsWXMX =
    new wxCheckBox(html_sizer->GetStaticBox(), wxID_ANY,
                   _("Add the .wxmx file to the HTML export"));
  html_sizer->Add(m_exportContainsWXMX,
                  wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  vsizer->Add(html_sizer, wxSizerFlags().Expand().Border(
                                                         wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *misc_sizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Misc"));
  wxFlexGridSizer *miscGrid_sizer = new wxFlexGridSizer(9, 2, 5, 5);
  wxStaticText *bs = new wxStaticText(misc_sizer->GetStaticBox(), wxID_ANY,
                                      _("Bitmap scale for export:"));
  m_bitmapScale = new wxSpinCtrl(
                                 misc_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                 wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1, 3);
  miscGrid_sizer->Add(bs, 0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      5 * GetContentScaleFactor());
  miscGrid_sizer->Add(m_bitmapScale, wxSizerFlags());
  misc_sizer->Add(miscGrid_sizer, wxSizerFlags().Expand().Border(
                                                                 wxALL, 5 * GetContentScaleFactor()));
  vsizer->Add(misc_sizer, wxSizerFlags().Expand().Border(
                                                         wxALL, 5 * GetContentScaleFactor()));

  panel->FitInside();
  return panel;
}

wxWindow *ConfigDialogue::CreateOptionsPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(20, 2, 5, 5);
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);
  wxStaticBoxSizer *stdOpts_sizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Standard Options"));

  wxArrayString languages;
  for (Languages::const_iterator it = m_languages.begin();
       it != m_languages.end(); ++it)
    languages.Add(it->first);

  m_language = new wxChoice(
                            stdOpts_sizer->GetStaticBox(), language_id, wxDefaultPosition,
                            wxSize(230 * GetContentScaleFactor(), -1), languages);
  grid_sizer->Add(
                  new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY, _("Language:")), 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL, 5 * GetContentScaleFactor());
  grid_sizer->Add(m_language, 0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());

  grid_sizer->Add(
                  new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                   _("Additional symbols for the \"symbols\" sidebar:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL, 5 * GetContentScaleFactor());
  m_symbolPaneAdditionalChars =
    new wxTextCtrl(stdOpts_sizer->GetStaticBox(), wxID_ANY);
  grid_sizer->Add(m_symbolPaneAdditionalChars, 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());

  wxStaticText *ul = new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                      _("Undo limit (0 for none):"));
  m_undoLimit = new wxSpinCtrl(
                               stdOpts_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                               wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 0, 10000);
  grid_sizer->Add(ul, 0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());
  grid_sizer->Add(m_undoLimit, wxSizerFlags());

  grid_sizer->Add(new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                   _("Recent files list length:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());
  m_recentItems = new wxSpinCtrl(
                                 stdOpts_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                 wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 5, 30);
  grid_sizer->Add(m_recentItems, wxSizerFlags());

  grid_sizer->Add(new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                   _("Default animation framerate:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());
  m_defaultFramerate = new wxSpinCtrl(
                                      stdOpts_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                      wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1, 200);
  grid_sizer->Add(m_defaultFramerate, 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());

  grid_sizer->Add(
                  new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                   _("Interactive popup memory limit [MB/plot]:")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL, 5 * GetContentScaleFactor());
  m_maxGnuplotMegabytes = new wxSpinCtrl(
                                         stdOpts_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                         wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 0, 2000);
  grid_sizer->Add(m_maxGnuplotMegabytes, 0,
                  wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());

  grid_sizer->Add(new wxStaticText(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                                   _("Time [in Minutes] between autosaves")),
                  0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());
  m_autosaveMinutes = new wxSpinCtrl(
                                     stdOpts_sizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                     wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1, 60);

  grid_sizer->Add(m_autosaveMinutes, 0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                  5 * GetContentScaleFactor());

  stdOpts_sizer->Add(grid_sizer,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_autoSave = new wxCheckBox(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                              _("Save the worksheet automatically"));
  stdOpts_sizer->Add(m_autoSave,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_usesvg = new wxCheckBox(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                            _("Create scalable plots."));
  m_usesvg->Connect(wxEVT_CHECKBOX,
                    wxCommandEventHandler(ConfigDialogue::UsesvgChanged), NULL,
                    this);
  m_usesvg->Show(false);
  stdOpts_sizer->Add(m_usesvg,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_saveUntitled =
    new wxCheckBox(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                   _("Ask to save untitled documents"));
  stdOpts_sizer->Add(m_saveUntitled,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_fixReorderedIndices =
    new wxCheckBox(
                   stdOpts_sizer->GetStaticBox(), wxID_ANY,
                   _("Fix reordered reference indices (of %i, %o) before saving"));
  stdOpts_sizer->Add(m_fixReorderedIndices,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  m_notifyIfIdle =
    new wxCheckBox(stdOpts_sizer->GetStaticBox(), wxID_ANY,
                   _("Warn if an inactive window is idle"));
  stdOpts_sizer->Add(m_notifyIfIdle,
                     wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));

  vsizer->Add(stdOpts_sizer, wxSizerFlags().Expand().Border(
                                                            wxALL, 5 * GetContentScaleFactor()));

  panel->FitInside();
  return panel;
}

wxWindow *ConfigDialogue::CreateMaximaPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxFlexGridSizer *sizer = new wxFlexGridSizer(5, 2, 0, 0);
  wxFlexGridSizer *sizer2 = new wxFlexGridSizer(6, 2, 0, 0);
  wxFlexGridSizer *dynamicMemory = new wxFlexGridSizer(1, 3, 0, 0);
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);
  wxStaticBoxSizer *invocationSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Maxima Location"));

  wxFlexGridSizer *nameSizer = new wxFlexGridSizer(9, 2, 0, 0);
  nameSizer->Add(new wxStaticText(invocationSizer->GetStaticBox(), wxID_ANY,
                                  _("Maxima program:")),
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  m_autodetectMaxima =
    new wxRadioButton(invocationSizer->GetStaticBox(), wxID_ANY, _("Autodetect"),
                      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_autodetectMaxima,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(new wxTextCtrl(
                                invocationSizer->GetStaticBox(), wxID_ANY,
                                m_configuration->MaximaDefaultLocation(), wxDefaultPosition,
                                wxSize(250 * GetContentScaleFactor(), -1), wxTE_RICH | wxTE_READONLY));

  m_noAutodetectMaxima =
    new wxRadioButton(invocationSizer->GetStaticBox(), wxID_ANY,
                      _("User specified"));
  nameSizer->Add(m_noAutodetectMaxima,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  m_maximaUserLocation = new BinaryNameCtrl(invocationSizer->GetStaticBox(), wxID_ANY);

  nameSizer->Add(m_maximaUserLocation,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));

  nameSizer->Add(
                 new wxStaticText(invocationSizer->GetStaticBox(), wxID_ANY, _("Help browser:")),
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  m_internalHelpBrowser =
    new wxRadioButton(invocationSizer->GetStaticBox(), wxID_ANY, _("Internal"),
                      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_internalHelpBrowser,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  m_internalHelpBrowser->Show(m_configuration->OfferInternalHelpBrowser());
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  m_autodetectHelpBrowser =
    new wxRadioButton(invocationSizer->GetStaticBox(), wxID_ANY, _("Autodetect"),
                      wxDefaultPosition, wxDefaultSize);
  nameSizer->Add(m_autodetectHelpBrowser,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  m_noAutodetectHelpBrowser =
    new wxRadioButton(invocationSizer->GetStaticBox(),
                      wxID_ANY, _("User specified"));
  nameSizer->Add(m_noAutodetectHelpBrowser,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));

  m_helpBrowserUserLocation =
    new BinaryNameCtrl(
                   invocationSizer->GetStaticBox());

  nameSizer->Add(m_helpBrowserUserLocation,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  m_singlePageManual =
    new wxCheckBox(invocationSizer->GetStaticBox(), wxID_ANY,
                   _("Prefer the all-in-one-file >1000 page manual"));

  nameSizer->Add(m_singlePageManual,
                 wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  nameSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  invocationSizer->Add(nameSizer,
                       wxSizerFlags().Expand().Border(wxUP | wxDOWN, 0));

  vsizer->Add(invocationSizer, wxSizerFlags().Expand().Border(
                                                              wxALL, 5 * GetContentScaleFactor()));
  vsizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  wxStaticBoxSizer *configSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Maxima configuration"));

  m_maximaUsesInternalHelp =
    new wxRadioButton(
                      configSizer->GetStaticBox(), wxID_ANY, _("Maxima shows help in the console"),
                      wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  configSizer->Add(
                   m_maximaUsesInternalHelp,
                   wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  m_maximaUsesHtmlHelp =
    new wxRadioButton(configSizer->GetStaticBox(), wxID_ANY,
                      _("Maxima shows help in a browser"));
  configSizer->Add(
                   m_maximaUsesHtmlHelp,
                   wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  m_maximaUsesWxmaximaHelp = new wxRadioButton(
                                               configSizer->GetStaticBox(), wxID_ANY, _("Maxima shows help in a sidebar"));
  configSizer->Add(
                   m_maximaUsesWxmaximaHelp,
                   wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  m_maximaUsesWxmaximaHelp->Show(m_configuration->OfferInternalHelpBrowser());
  m_defaultPort = new wxSpinCtrl(
                                 configSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                 wxSize(230 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 50, 65534);

  wxStaticText *dp =
    new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                     _("Default port for communication with wxMaxima:"));
  sizer->Add(dp, 0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
             5 * GetContentScaleFactor());
  sizer->Add(m_defaultPort,
             wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));

  sizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  configSizer->Add(sizer);
  wxStaticText *ap = new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                                      _("Additional parameters for maxima"));
  sizer2->Add(ap, wxSizerFlags());
  sizer2->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  wxStaticText *ap1 =
    new wxStaticText(configSizer->GetStaticBox(), wxID_ANY, _("Examples:"));
  sizer2->Add(ap1, wxSizerFlags());
  sizer2->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  sizer2->Add(
              new wxStaticText(configSizer->GetStaticBox(), wxID_ANY, _("      -l <name>")),
              wxSizerFlags());
  sizer2->Add(new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                               _("choose a lisp maxima was compiled with")),
              wxSizerFlags());
  sizer2->Add(
              new wxStaticText(configSizer->GetStaticBox(), wxID_ANY, _("      -u <number>")),
              wxSizerFlags());
  sizer2->Add(new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                               _("choose between installed maxima versions")),
              wxSizerFlags());
  if (m_configuration->GetLispType().Lower().Contains(wxS("sbcl"))) {
    wxString sbclMemoryParameter1;
    wxString sbclMemoryParameter2;
#ifdef __WXMSW__
    sbclMemoryParameter1 = _("      -X \"--dynamic-space-size <int>\"");
    sbclMemoryParameter2 = _("      -X \"--control-stack-size <int>\"");
#else
    sbclMemoryParameter1 = _("      -X '--dynamic-space-size <int>'");
    sbclMemoryParameter2 = _("      -X '--control-stack-size <int>'");
#endif
    sizer2->Add(
                new wxStaticText(configSizer->GetStaticBox(), wxID_ANY, sbclMemoryParameter1),
                wxSizerFlags());
    sizer2->Add(new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                                 _("sbcl: use <int>Mbytes of heap")),
                wxSizerFlags());
    sizer2->Add(
                new wxStaticText(configSizer->GetStaticBox(), wxID_ANY, sbclMemoryParameter2),
                wxSizerFlags());
    sizer2->Add(new wxStaticText(
                                 configSizer->GetStaticBox(), wxID_ANY,
                                 _("sbcl: use <int>Mbytes of stack for function calls")),
                wxSizerFlags());
  }
  configSizer->Add(sizer2);
  m_additionalParameters = new wxTextCtrl(
                                          configSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                          wxSize(600 * GetContentScaleFactor(), -1), wxTE_RICH);
  configSizer->Add(
                   m_additionalParameters,
                   wxSizerFlags().Border(wxRIGHT, 10 * GetContentScaleFactor()).Expand());

  configSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());

  dynamicMemory->Add(new wxStaticText(
      configSizer->GetStaticBox(), wxID_ANY,
      _("Overwrite Maxima Default Memory [MB]: ")), wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 20);

  m_enableDynamicMemorySpace = new wxCheckBox(
      configSizer->GetStaticBox(), wxID_ANY, _(""), wxDefaultPosition);
  dynamicMemory->Add(
      m_enableDynamicMemorySpace,
      wxSizerFlags().Border(wxRIGHT, 10 * GetContentScaleFactor()).Expand());

  m_dynamicMemorySpace = new wxSpinCtrl(configSizer->GetStaticBox(), wxID_ANY, _(""), wxDefaultPosition);
  m_dynamicMemorySpace->SetRange(1000, std::numeric_limits<int>::max());
  m_dynamicMemorySpace->SetValue(1000);
  dynamicMemory->Add(
      m_dynamicMemorySpace,
      wxSizerFlags().Border(wxRIGHT, 10 * GetContentScaleFactor()).Expand());

  configSizer->Add(dynamicMemory);

  configSizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  m_maximaEnvVariables = new wxGrid(configSizer->GetStaticBox(), wxID_ANY);
  m_maximaEnvVariables->CreateGrid(0, 2);
  m_maximaEnvVariables->Connect(
                                wxEVT_GRID_CELL_LEFT_CLICK,
                                wxGridEventHandler(ConfigDialogue::OnChangeMaximaCellClick), NULL, this);
  m_maximaEnvVariables->Connect(
                                wxEVT_GRID_CELL_CHANGED,
                                wxGridEventHandler(ConfigDialogue::OnChangeMaximaEnvVar), NULL, this);
  m_maximaEnvVariables->Connect(
                                wxEVT_GRID_CELL_RIGHT_CLICK,
                                wxGridEventHandler(ConfigDialogue::OnMaximaEnvRightClick), NULL, this);
  m_maximaEnvVariables->GetGridWindow()->Connect(
                                                 wxEVT_MOTION,
                                                 wxMouseEventHandler(ConfigDialogue::OnMouseMotion_MaximaEnv), NULL, this);

  configSizer->Add(new wxStaticText(configSizer->GetStaticBox(), wxID_ANY,
                                    _("Environment variables for maxima")),
                   wxSizerFlags().Expand());
  configSizer->Add(m_maximaEnvVariables,
                   wxSizerFlags().Expand().Border(
                                                  wxRIGHT | wxBOTTOM, 10 * GetContentScaleFactor()));
  vsizer->Add(configSizer, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));

  vsizer->Add(10 * GetContentScaleFactor(), 10 * GetContentScaleFactor());
  wxStaticBoxSizer *handlingSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Maxima usage"));

  m_abortOnError = new wxCheckBox(handlingSizer->GetStaticBox(), wxID_ANY,
                                  _("Abort evaluation on error"));
  handlingSizer->Add(m_abortOnError, wxSizerFlags());
  m_restartOnReEvaluation =
    new wxCheckBox(handlingSizer->GetStaticBox(), wxID_ANY,
                   _("Start a new maxima for each re-evaluation"));
  handlingSizer->Add(m_restartOnReEvaluation, wxSizerFlags());
  vsizer->Add(handlingSizer, wxSizerFlags().Expand().Border(
                                                            wxALL, 5 * GetContentScaleFactor()));

#ifdef __WXMSW__
  wxStaticBoxSizer *gnuplotSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Gnuplot flavour"));
  m_useGnuplot = new wxRadioButton(gnuplotSizer->GetStaticBox(), wxID_ANY, _("gnuplot: Without command console"));
  gnuplotSizer->Add(m_useGnuplot,
                   wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  m_useWgnuplot = new wxRadioButton(gnuplotSizer->GetStaticBox(), wxID_ANY, _("wgnuplot: Steals the mouse focus during plotting"));
  gnuplotSizer->Add(m_useWgnuplot,
                   wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  m_useWgnuplot->SetValue(m_configuration->UseWGnuplot());
  m_useGnuplot->SetValue(!m_configuration->UseWGnuplot());

  vsizer->Add(gnuplotSizer, wxSizerFlags().Expand().Border(
                                                            wxALL, 5 * GetContentScaleFactor()));
#endif
  panel->FitInside();
  return panel;
}

void ConfigDialogue::OnMouseMotion_MaximaEnv(wxMouseEvent &event) {
  int row = m_maximaEnvVariables->YToRow(event.GetY(), true);
  wxString toolTip;
  if (m_maximaEnvDoc.find(m_maximaEnvVariables->GetCellValue(row, 0)) !=
      m_maximaEnvDoc.end())
    toolTip = m_maximaEnvDoc[m_maximaEnvVariables->GetCellValue(row, 0)];
  if (toolTip != m_maximaEnvVariables->GetGridWindow()->GetToolTipText())
    m_maximaEnvVariables->GetGridWindow()->SetToolTip(toolTip);
}

void ConfigDialogue::OnMaximaEnvRightClick(wxGridEvent &event) {
  m_maximaEmvRightClickRow = event.GetRow();
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  if ((m_maximaEmvRightClickRow >= 0) &&
      (m_maximaEmvRightClickRow < m_maximaEnvVariables->GetNumberRows()) &&
      (!m_maximaEnvVariables->GetCellValue(m_maximaEmvRightClickRow, 0)
       .IsEmpty()) &&
      (!m_maximaEnvVariables->GetCellValue(m_maximaEmvRightClickRow, 1)
       .IsEmpty()))
    popupMenu->Append(VAR_DELETE, wxS("Delete this entry"));

  if (event.GetCol() == 0) {
    if (popupMenu->GetMenuItemCount() > 0)
      popupMenu->AppendSeparator();
    popupMenu->Append(MAXIMA_DEFAULT_LISP, wxS("MAXIMA_DEFAULT_LISP"));
    popupMenu->Append(MAXIMA_IMAGESDIR, wxS("MAXIMA_IMAGESDIR"));
    popupMenu->Append(MAXIMA_USERDIR, wxS("MAXIMA_USERDIR"));
    popupMenu->Append(MAXIMA_TEMPDIR, wxS("MAXIMA_TEMPDIR"));
    popupMenu->Append(MAXIMA_OBJDIR, wxS("MAXIMA_OBJDIR"));
    popupMenu->Append(MAXIMA_DOC_PREFIX, wxS("MAXIMA_DOC_PREFIX"));
    popupMenu->Append(HOME, wxS("HOME"));
    popupMenu->Append(GCL_GC_PAGE_THRESH, wxS("GCL_GC_PAGE_THRESH"));
    popupMenu->Append(GCL_GC_ALLOC_MIN, wxS("GCL_GC_ALLOC_MIN"));
    popupMenu->Append(GCL_GC_PAGE_MAX, wxS("GCL_GC_PAGE_MAX"));
    popupMenu->Append(GCL_MEM_MULTIPLE, wxS("GCL_MEM_MULTIPLE"));
    popupMenu->Append(GCL_MULTIPROCESS_MEMORY_POOL,
                      wxS("GCL_MULTIPROCESS_MEMORY_POOL"));
    popupMenu->Append(LANG, wxS("LANG"));
    Connect(wxEVT_MENU, wxCommandEventHandler(ConfigDialogue::OnNewEnvMenu),
            NULL, this);
    PopupMenu(&*popupMenu);
  }
}

void ConfigDialogue::OnClickMaximaEnvVal(int row) {
  if (row < 0)
    return;
  if (row >= m_maximaEnvVariables->GetNumberRows())
    return;
  wxString var = m_maximaEnvVariables->GetCellValue(row, 0);
  if (var == "MAXIMA_IMAGESDIR") {
    wxDirDialog dirChooser(this, "Directory the compiled maxima images are in");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "MAXIMA_USERDIR") {
    wxDirDialog dirChooser(
                           this, "Directory with maxima user libraries and/or startup files");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "MAXIMA_DIRECTORY") {
    wxDirDialog dirChooser(this, "Directory maxima is started in by default");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "MAXIMA_TEMPDIR") {
    wxDirDialog dirChooser(this, "Directory for maxima temp files");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "MAXIMA_OBJDIR") {
    wxDirDialog dirChooser(
                           this, "Directory the results of compiling libraries are stored in");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "MAXIMA_DOC_PREFIX") {
    wxDirDialog dirChooser(this, "Directory with the maxima documentation");
    if (dirChooser.ShowModal() == wxID_OK)
      m_maximaEnvVariables->SetCellValue(row, 1, dirChooser.GetPath());
  }
  if (var == "GCL_MULTIPROCESS_MEMORY_POOL") {
    if (m_maximaEnvVariables->GetCellValue(row, 1) == "t")
      m_maximaEnvVariables->SetCellValue(row, 1, "nil");
    else
      m_maximaEnvVariables->SetCellValue(row, 1, "t");
  }
  m_maximaEnvVariables->AutoSize();
}

void ConfigDialogue::OnNewEnvMenu(wxCommandEvent &event) {
  if (event.GetId() == VAR_DELETE) {
    if ((m_maximaEmvRightClickRow >= 0) &&
        (m_maximaEmvRightClickRow < m_maximaEnvVariables->GetNumberRows()))
      m_maximaEnvVariables->DeleteRows(m_maximaEmvRightClickRow, 1);
    return;
  }
  if ((m_maximaEmvRightClickRow >= 0) &&
      (m_maximaEmvRightClickRow < m_maximaEnvVariables->GetNumberRows())) {
    if (!m_maximaEnvVariables->GetCellValue(m_maximaEmvRightClickRow, 0)
        .IsEmpty())
      m_maximaEmvRightClickRow = m_maximaEnvVariables->GetNumberRows() - 1;
  }
  if ((m_maximaEmvRightClickRow >= 0) &&
      (m_maximaEmvRightClickRow < m_maximaEnvVariables->GetNumberRows())) {
    if (!m_maximaEnvVariables->GetCellValue(m_maximaEmvRightClickRow, 0)
        .IsEmpty()) {
      m_maximaEnvVariables->AppendRows(1);
      m_maximaEmvRightClickRow = m_maximaEnvVariables->GetNumberRows() - 1;
    }
  }
  switch (event.GetId()) {
  case MAXIMA_DEFAULT_LISP:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_DEFAULT_LISP");
    break;
  case MAXIMA_IMAGESDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_IMAGESDIR");
    break;
  case MAXIMA_USERDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_USERDIR");
    break;
  case MAXIMA_DIRECTORY:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_DIRECTORY");
    break;
  case GCL_GC_PAGE_THRESH:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "GCL_GC_PAGE_THRESH");
    break;
  case GCL_GC_ALLOC_MIN:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "GCL_GC_ALLOC_MIN");
    break;
  case GCL_GC_PAGE_MAX:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "GCL_GC_PAGE_MAX");
    break;
  case GCL_MEM_MULTIPLE:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "GCL_MEM_MULTIPLE");
    break;
  case GCL_MULTIPROCESS_MEMORY_POOL:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "GCL_MULTIPROCESS_MEMORY_POOL");
    break;
  case LANG:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0, "LANG");
    break;
  case HOME:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0, "HOME");
    break;
  case MAXIMA_TEMPDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_TEMPDIR");
    break;
  case MAXIMA_OBJDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_OBJDIR");
    break;
  case MAXIMA_DOC_PREFIX:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow, 0,
                                       "MAXIMA_DOC_PREFIX");
    break;
  }
  if (!((m_maximaEnvVariables
         ->GetCellValue(m_maximaEnvVariables->GetNumberRows() - 1, 0)
         .IsEmpty()) &&
        (m_maximaEnvVariables
         ->GetCellValue(m_maximaEnvVariables->GetNumberRows() - 1, 1)
         .IsEmpty())))
    m_maximaEnvVariables->AppendRows(1);
  m_maximaEnvVariables->AutoSize();
  m_maximaEnvVariables->GetParent()->GetParent()->Layout();
  //  OnClickMaximaEnvVal(m_maximaEmvRightClickRow);
}

void ConfigDialogue::OnChangeMaximaCellClick(wxGridEvent &event) {
  if (event.GetCol() == 1)
    OnClickMaximaEnvVal(event.GetRow());
  event.Skip();
}

void ConfigDialogue::OnChangeMaximaEnvVar(wxGridEvent &WXUNUSED(event)) {
  //  Make sure we have exactly one empty row the user can add variables to
  for (int row = m_maximaEnvVariables->GetNumberRows() - 2; row >= 0; row--) {
    if ((m_maximaEnvVariables->GetCellValue(row, 0)
         .Trim(true)
         .Trim(false)
         .IsEmpty()) &&
        (m_maximaEnvVariables->GetCellValue(row, 1)
         .Trim(true)
         .Trim(false)
         .IsEmpty()))
      m_maximaEnvVariables->DeleteRows(row, 1);
  }
  int row = m_maximaEnvVariables->GetNumberRows() - 1;
  if ((!m_maximaEnvVariables->GetCellValue(row, 0)
       .Trim(true)
       .Trim(false)
       .IsEmpty()) &&
      (!m_maximaEnvVariables->GetCellValue(row, 1)
       .Trim(true)
       .Trim(false)
       .IsEmpty()))
    m_maximaEnvVariables->AppendRows(1);
  m_maximaEnvVariables->GetParent()->GetParent()->Layout();
}

wxWindow *ConfigDialogue::CreateClipboardPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxStaticBoxSizer *formatSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel,
                         _("Additional clipboard formats to put on the "
                           "clipboard on ordinary copy"));
  m_copyBitmap = new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY, _("Bitmap"));
  formatSizer->Add(m_copyBitmap, wxSizerFlags());

  m_copyMathML =
    new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY, _("MathML description"));
  formatSizer->Add(m_copyMathML, wxSizerFlags());

  m_copyMathMLHTML =
    new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY, _("MathML as HTML"));
  formatSizer->Add(m_copyMathMLHTML, wxSizerFlags());

  m_copyRTF =
    new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY, _("RTF with OMML maths"));
  formatSizer->Add(m_copyRTF, wxSizerFlags());

  m_copySVG = new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY,
                             _("Scalable Vector Graphics (svg)"));
  formatSizer->Add(m_copySVG, wxSizerFlags());

#if wxUSE_ENH_METAFILE
  m_copyEMF = new wxCheckBox(formatSizer->GetStaticBox(), wxID_ANY,
                             _("Enhanced meta file (emf)"));
  formatSizer->Add(m_copyEMF, wxSizerFlags());
#endif
  vbox->Add(formatSizer,
            wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *formatParamsSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel, _("Clipboard format parameters"));

  wxFlexGridSizer *sizer = new wxFlexGridSizer(5, 2, 0, 0);

  sizer->Add(new wxStaticText(formatParamsSizer->GetStaticBox(), wxID_ANY,
                              _("Maximum bitmap size on clipboard [Mb]:")),
             0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
             5 * GetContentScaleFactor());
  m_maxClipbrdBitmapMegabytes = new wxSpinCtrl(
                                               formatParamsSizer->GetStaticBox(), wxID_ANY, wxEmptyString, wxDefaultPosition,
                                               wxSize(150 * GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1, 16384);
  sizer->Add(m_maxClipbrdBitmapMegabytes, wxSizerFlags().Expand());
  formatParamsSizer->Add(sizer, wxSizerFlags().Expand().Border(
                                                               wxALL, 5 * GetContentScaleFactor()));
  vbox->Add(formatParamsSizer,
            wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  panel->SetSizer(vbox);
  panel->FitInside();

  return panel;
}





wxWindow *ConfigDialogue::CreatePrintPanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));

  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  wxStaticBoxSizer *miscSettingsSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel,
                         _("Misc settings"));
  wxBoxSizer *vbox2 = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer *printGrid_sizer = new wxFlexGridSizer(2);
  printGrid_sizer->AddGrowableCol(1);
  printGrid_sizer->Add(new wxStaticText(miscSettingsSizer->GetStaticBox(),
                                        wxID_ANY, _("Print scale:")),
                       0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL, 0);
  m_printScale = new wxSpinCtrlDouble(
                                      miscSettingsSizer->GetStaticBox(),
                                      wxID_ANY, wxEmptyString, wxDefaultPosition,
                                      wxSize(150 * GetContentScaleFactor(), -1),
                                      wxSP_ARROW_KEYS, .1, 4, .1);
  m_printScale->SetDigits(2);
  m_printScale->SetIncrement(.1);
  printGrid_sizer->Add(m_printScale, wxSizerFlags().Expand().Border(wxRIGHT,
                                                                    5 * GetContentScaleFactor()));
  vbox2->Add(printGrid_sizer, wxSizerFlags().Expand());
  m_printBrackets = new wxCheckBox(miscSettingsSizer->GetStaticBox(), wxID_ANY,
                                   _("Print the cell brackets [drawn to their left]"));
  vbox2->Add(m_printBrackets, wxSizerFlags());
  miscSettingsSizer->Add(vbox2, wxSizerFlags().Expand());
  vbox->Add(miscSettingsSizer, wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  wxStaticBoxSizer *marginSizer =
    new wxStaticBoxSizer(wxVERTICAL, panel,
                         _("Print Margins"));
  wxFlexGridSizer *dimensionSizer = new wxFlexGridSizer(2);
  dimensionSizer->AddGrowableCol(1);
  dimensionSizer->Add(new wxStaticText(marginSizer->GetStaticBox(), wxID_ANY, _("Top")),
                      0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      0);
  m_printMargin_Top = new wxSpinCtrl(marginSizer->GetStaticBox(), wxID_ANY);
  m_printMargin_Top->SetRange(0, 100);
  dimensionSizer->Add(m_printMargin_Top, wxSizerFlags().Expand().Border(wxRIGHT,
                                                                        5 * GetContentScaleFactor()));

  dimensionSizer->Add(new wxStaticText(marginSizer->GetStaticBox(), wxID_ANY, _("Bottom")),
                      0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      0);
  m_printMargin_Bot = new wxSpinCtrl(marginSizer->GetStaticBox(), wxID_ANY);
  m_printMargin_Bot->SetRange(0, 100);
  dimensionSizer->Add(m_printMargin_Bot, wxSizerFlags().Expand().Border(wxRIGHT,
                                                                        5 * GetContentScaleFactor()));

  dimensionSizer->Add(new wxStaticText(marginSizer->GetStaticBox(), wxID_ANY, _("Left")),
                      0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      0);
  m_printMargin_Left = new wxSpinCtrl(marginSizer->GetStaticBox(), wxID_ANY);
  m_printMargin_Left->SetRange(0, 100);
  dimensionSizer->Add(m_printMargin_Left, wxSizerFlags().Expand().Border(wxRIGHT,
                                                                         5 * GetContentScaleFactor()));

  dimensionSizer->Add(new wxStaticText(marginSizer->GetStaticBox(), wxID_ANY, _("Right")),
                      0, wxUP | wxDOWN | wxALIGN_CENTER_VERTICAL,
                      0);
  m_printMargin_Right = new wxSpinCtrl(marginSizer->GetStaticBox(), wxID_ANY);
  m_printMargin_Right->SetRange(0, 100);
  dimensionSizer->Add(m_printMargin_Right, wxSizerFlags().Expand().Border(wxRIGHT,
                                                                          5 * GetContentScaleFactor()));
  marginSizer->Add(dimensionSizer, wxSizerFlags().Expand());
  vbox->Add(marginSizer,
            wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  panel->SetSizer(vbox);
  panel->FitInside();

  return panel;
}

TextStyle ConfigDialogue::GetSelectedStyle() const {
  return static_cast<TextStyle>(m_styleFor->GetSelection());
}

wxWindow *ConfigDialogue::CreateStylePanel() {
  wxScrolled<wxPanel> *panel = new wxScrolled<wxPanel>(m_notebook, wxID_ANY);
  m_sampleWorksheet = new Worksheet(panel, wxID_ANY, m_configuration.get(),
                                    wxDefaultPosition, wxDefaultSize, false);
  panel->SetScrollRate(5 * GetContentScaleFactor(),
                       5 * GetContentScaleFactor());
  panel->SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth,
                           GetContentScaleFactor() * mMinPanelHeight));
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  panel->SetSizer(vsizer);

  // The fonts box
  wxStaticBox *fonts = new wxStaticBox(panel, wxID_ANY, _("Fonts"));
  wxStaticBoxSizer *fontsSizer = new wxStaticBoxSizer(fonts, wxVERTICAL);
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 2, 2, 2);
  m_useUnicodeMaths =
    new wxCheckBox(fontsSizer->GetStaticBox(), wxID_ANY,
                   _("Use unicode Math Symbols, if available"));
  grid_sizer_1->Add(
                    m_useUnicodeMaths,
                    wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));

  fontsSizer->Add(grid_sizer_1, wxSizerFlags(1).Expand());
  vsizer->Add(fontsSizer, wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  // The styles box
  wxStaticBox *styles = new wxStaticBox(panel, wxID_ANY, _("Styles"));
  wxStaticBoxSizer *stylesSizer = new wxStaticBoxSizer(styles, wxVERTICAL);
  wxBoxSizer *fontoptionsizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *hbox_sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *hbox3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *vbox_sizer = new wxBoxSizer(wxVERTICAL);

  wxArrayString m_styleFor_choices;
  for (int style = 0; style < NUMBEROFSTYLES; style++) {
    m_styleFor_choices.Add(m_configuration->GetStyleName(static_cast<TextStyle>(style)));
  }
  m_styleFor = new wxListBox(stylesSizer->GetStaticBox(), listbox_styleFor,
                             wxDefaultPosition,
                             wxSize(250 * GetContentScaleFactor(), -1),
                             m_styleFor_choices, wxLB_SINGLE);
  m_styleFor->Connect(
                      wxEVT_LISTBOX,
                      wxCommandEventHandler(ConfigDialogue::OnStyleToEditChanged), NULL, this);

  m_getStyleFont = new wxButton(stylesSizer->GetStaticBox(), style_font_family,
                                _("Choose font"), wxDefaultPosition,
                                wxSize(350 * GetContentScaleFactor(), -1));
  m_styleColor = new wxColourPickerCtrl(stylesSizer->GetStaticBox(), wxID_ANY, *wxBLACK,
                                        wxDefaultPosition,
                                        wxSize(350 * GetContentScaleFactor(), -1 /*30 * GetContentScaleFactor()*/),
                                        wxCLRP_USE_TEXTCTRL | wxCLRP_SHOW_LABEL);
  m_styleColor-> Connect(wxEVT_COLOURPICKER_CHANGED,
                         wxColourPickerEventHandler(ConfigDialogue::OnChangeColor), NULL, this);

  m_boldCB =
    new wxCheckBox(stylesSizer->GetStaticBox(), checkbox_bold, _("Bold"));
  m_italicCB =
    new wxCheckBox(stylesSizer->GetStaticBox(), checkbox_italic, _("Italic"));
  m_slantedCB =
    new wxCheckBox(stylesSizer->GetStaticBox(), checkbox_slanted, _("Slanted"));
  m_underlinedCB = new wxCheckBox(stylesSizer->GetStaticBox(),
                                  checkbox_underlined, _("Underlined"));
  m_strikethroughCB = new wxCheckBox(stylesSizer->GetStaticBox(),
                                     checkbox_strikethrough, _("Strike Through"));
  m_examplePanel =
    new ExamplePanel(stylesSizer->GetStaticBox(), wxID_ANY, wxDefaultPosition,
                     wxSize(250 * GetContentScaleFactor(), 60));

  vbox_sizer->Add(m_styleColor, 0, wxUP | wxDOWN | wxALIGN_CENTER,
                  5 * GetContentScaleFactor());
  vbox_sizer->Add(m_getStyleFont, 0, wxUP | wxDOWN | wxALIGN_CENTER,
                  5 * GetContentScaleFactor());
  fontoptionsizer->Add(m_boldCB, wxSizerFlags().Border(
                                                       wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  fontoptionsizer->Add(
                       m_italicCB,
                       wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  fontoptionsizer->Add(
                       m_slantedCB,
                       wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  fontoptionsizer->Add(
                       m_underlinedCB,
                       wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  fontoptionsizer->Add(
                       m_strikethroughCB,
                       wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  hbox3->Add(fontoptionsizer, wxSizerFlags().Expand());

  hbox3->Add(m_examplePanel, wxSizerFlags().Expand().Border(
                                                            wxALL, 5 * GetContentScaleFactor()));
  vbox_sizer->Add(hbox3, 1, wxUP | wxDOWN | wxEXPAND, 0);
  hbox_sizer_2->Add(m_styleFor,
                    wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));
  hbox_sizer_2->Add(vbox_sizer, 1, wxUP | wxDOWN | wxEXPAND, 0);
  stylesSizer->Add(hbox_sizer_2, 0, wxUP | wxDOWN | wxEXPAND, 0);
  wxConfigBase *config = wxConfig::Get();
  int styleToEditNum = 0;
  config->Read(wxS("StyleToEdit"), &styleToEditNum);
  if ((styleToEditNum >= TextStyle::NUMBEROFSTYLES) || (styleToEditNum < 0))
    styleToEditNum = 0;
  m_styleFor->SetSelection(styleToEditNum);
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
  vsizer->Add(stylesSizer, wxSizerFlags().Expand().Border(
                                                          wxALL, 5 * GetContentScaleFactor()));

  // load+save buttons
  wxBoxSizer *loadSavesizer = new wxBoxSizer(wxHORIZONTAL);
  m_loadStyle = new wxButton(panel, load_id, _("Load"));
  m_saveStyle = new wxButton(panel, save_id, _("Save"));
  loadSavesizer->Add(
                     m_loadStyle,
                     wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  loadSavesizer->Add(
                     m_saveStyle,
                     wxSizerFlags().Border(wxUP | wxDOWN, 5 * GetContentScaleFactor()));
  vsizer->Add(loadSavesizer, 0, wxALIGN_RIGHT | wxALL,
              5 * GetContentScaleFactor());

  m_configuration->SetZoomFactor(1.0);

  // Load the sample worksheet's contents
  {
    wxMemoryInputStream istream(SAMPLEWORKSHEET_WXMX, SAMPLEWORKSHEET_WXMX_SIZE);
    wxZipInputStream zipstream(istream);
    wxZipEntry *entry;
    do
      {
        entry = zipstream.GetNextEntry();
      } while((entry != NULL) && (entry->GetName() != "content.xml"));
    wxXmlDocument xmlText;
    xmlText.Load(zipstream);
    wxXmlNode *xmlcells = xmlText.GetRoot();
    if (xmlcells)
      xmlcells = xmlcells->GetChildren();

    MathParser mp(m_configuration.get());
    CellListBuilder<GroupCell> tree;
    tree.DynamicAppend(mp.ParseTag(xmlcells));
    m_sampleWorksheet->InsertGroupCells(std::move(tree));
  }
  vsizer->Add(m_sampleWorksheet, wxSizerFlags(1).Expand().
              Border(wxALL, 5 * GetContentScaleFactor()));

  panel->FitInside();
  return panel;
}

void ConfigDialogue::OnStyleToEditChanged(wxCommandEvent &event) {
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxS("StyleToEdit"), static_cast<int>(GetSelectedStyle()));
  OnChangeStyle(event);
}

void ConfigDialogue::OnClose(wxCloseEvent &event) {
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxS("ConfigDialogTab"), m_notebook->GetSelection());
#if defined __WXOSX__
  EndModal(wxID_OK);
#else
  EndModal(wxID_CANCEL);
#endif
  event.Skip();
}

void ConfigDialogue::WriteSettings() {
  wxArrayString out;
  wxConfigBase *config = wxConfig::Get();
  Configuration *configuration = m_configuration.get();

  configuration->m_maximaEnvVars.clear();
  for (int row = m_maximaEnvVariables->GetNumberRows() - 1; row >= 0; row--) {
    if (!m_maximaEnvVariables->GetCellValue(row, 0)
        .Trim(true)
        .Trim(false)
        .IsEmpty())
      configuration
        ->m_maximaEnvVars[m_maximaEnvVariables->GetCellValue(row, 0)] =
        m_maximaEnvVariables->GetCellValue(row, 1);
  }

  configuration->SetAbortOnError(m_abortOnError->GetValue());
  configuration->MaxClipbrdBitmapMegabytes(
                                           m_maxClipbrdBitmapMegabytes->GetValue());
  configuration->RestartOnReEvaluation(m_restartOnReEvaluation->GetValue());
  configuration->MaximaUserLocation(m_maximaUserLocation->GetValue());
  configuration->AutodetectMaxima(m_autodetectMaxima->GetValue());
  configuration->HelpBrowserUserLocation(m_helpBrowserUserLocation->GetValue());
  if (m_maximaUsesHtmlHelp->GetValue()) {
    configuration->MaximaUsesHtmlBrowser(true);
    configuration->MaximaUsesWxmaximaBrowser(false);
  }
#ifdef __WXMSW__
  m_configuration->UseWGnuplot(m_useWgnuplot->GetValue());
#endif
  
  if (m_maximaUsesInternalHelp->GetValue()) {
    configuration->MaximaUsesHtmlBrowser(false);
    configuration->MaximaUsesWxmaximaBrowser(false);
  }
  if (m_maximaUsesWxmaximaHelp->GetValue()) {
    configuration->MaximaUsesHtmlBrowser(false);
    configuration->MaximaUsesWxmaximaBrowser(true);
  }
  configuration->SinglePageManual(m_singlePageManual->GetValue());
  if (m_internalHelpBrowser->GetValue()) {
    configuration->InternalHelpBrowser(true);
  } else {
    configuration->InternalHelpBrowser(false);
    configuration->AutodetectHelpBrowser(m_autodetectHelpBrowser->GetValue());
  }
  configuration->MaximaParameters(m_additionalParameters->GetValue());
  configuration->MaximaEnableCustomDynamicMemory(m_enableDynamicMemorySpace->GetValue());
  configuration->MaximaCustomDynamicMemory(m_dynamicMemorySpace->GetValue());
  configuration->SetMatchParens(m_matchParens->GetValue());
  configuration->ShowMatchingParens(m_showMatchingParens->GetValue());
  configuration->ShowLength(m_showLength->GetSelection());
  configuration->SetAutosubscript_Num(m_autosubscript->GetSelection());
  configuration->FixedFontInTextControls(m_fixedFontInTC->GetValue());
  configuration->OfferKnownAnswers(m_offerKnownAnswers->GetValue());
  configuration->SetChangeAsterisk(m_changeAsterisk->GetValue());
  configuration->HidemultiplicationSign(m_hidemultiplicationSign->GetValue());
  configuration->Latin2Greek(m_latin2Greek->GetValue());
  configuration->EnterEvaluates(m_enterEvaluates->GetValue());
  configuration->NumpadEnterEvaluates(m_numpadEnterEvaluates->GetValue());
  configuration->SaveImgFileName(m_saveImgFileName->GetValue());
  configuration->SaveUntitled(m_saveUntitled->GetValue());
  configuration->SetOpenHCaret(m_openHCaret->GetValue());
  configuration->SetInsertAns(m_insertAns->GetValue());
  configuration->SetAutoIndent(m_autoIndent->GetValue());
  configuration->CursorJump(m_cursorJump->GetValue());
  configuration->HideBrackets(m_hideBrackets->GetValue());
  configuration->IndentMaths(m_indentMaths->GetValue());
  configuration->SetAutoWrap(m_autoWrap->GetSelection());
  configuration->LabelWidth(m_labelWidth->GetValue());
  configuration->UndoLimit(m_undoLimit->GetValue());
  configuration->RecentItems(m_recentItems->GetValue());
  configuration->BitmapScale(m_bitmapScale->GetValue());
  configuration->PrintScale(m_printScale->GetValue());
  configuration->FixReorderedIndices(m_fixReorderedIndices->GetValue());
  configuration->IncrementalSearch(m_incrementalSearch->GetValue());
  configuration->NotifyIfIdle(m_notifyIfIdle->GetValue());
  configuration->SetLabelChoice(
                                (Configuration::showLabels)m_showUserDefinedLabels->GetSelection());
  configuration->DefaultPort(m_defaultPort->GetValue());
  configuration->UseSVG(m_usesvg->GetValue());
  configuration->DefaultFramerate(m_defaultFramerate->GetValue());
  configuration->MaxGnuplotMegabytes(m_maxGnuplotMegabytes->GetValue());
  configuration->AutosaveMinutes(m_autosaveMinutes->GetValue());
  configuration->DefaultPlotWidth(m_defaultPlotWidth->GetValue());
  configuration->DefaultPlotHeight(m_defaultPlotHeight->GetValue());
  configuration->SetDisplayedDigits(m_displayedDigits->GetValue());

  configuration->PrintMargin_Right(m_printMargin_Right->GetValue());
  configuration->PrintMargin_Left(m_printMargin_Left->GetValue());
  configuration->PrintMargin_Top(m_printMargin_Top->GetValue());
  configuration->PrintMargin_Bot(m_printMargin_Bot->GetValue());

  configuration->TeXExponentsAfterSubscript(
                                            m_TeXExponentsAfterSubscript->GetValue());
  configuration->UsePartialForDiff(m_usePartialForDiff->GetValue());
  configuration->WrapLatexMath(m_wrapLatexMath->GetValue());
  configuration->ExportContainsWXMX(m_exportContainsWXMX->GetValue());
  configuration->PrintBrackets(m_printBrackets->GetValue());
  configuration->HTMLequationFormat(
                                    (Configuration::htmlExportFormat)m_exportWithMathJAX->GetSelection());

  if (m_linebreaksInLongNums->GetValue()) {
    configuration->ShowAllDigits(true);
    configuration->LineBreaksInLongNums(true);
  } else {
    if (m_displayAllDigits->GetValue()) {
      configuration->ShowAllDigits(true);
      configuration->LineBreaksInLongNums(false);
    } else {
      configuration->ShowAllDigits(false);
      configuration->LineBreaksInLongNums(false);
    }
  }
  configuration->UseUnicodeMaths(m_useUnicodeMaths->GetValue());
  configuration->SetKeepPercent(m_keepPercentWithSpecials->GetValue());
  configuration->TexPreamble(m_texPreamble->GetValue());
  configuration->AutoSaveAsTempFile(!m_autoSave->GetValue());
  configuration->Documentclass(m_documentclass->GetValue());
  configuration->DocumentclassOptions(m_documentclassOptions->GetValue());
  configuration->MathJaXURL(m_mathJaxURL->GetValue());
  configuration->MathJaXURL_UseUser(m_noAutodetectMathJaX->GetValue());
  {
    configuration->SetLanguage(static_cast<int>(wxLANGUAGE_DEFAULT));
    long i = 0;
    for (Languages::const_iterator it = m_languages.begin();
         it != m_languages.end(); ++it) {
      if (i == m_language->GetSelection())
        configuration->SetLanguage(it->second);
      ++i;
    }
  }
  configuration->SymbolPaneAdditionalChars(
                                           m_symbolPaneAdditionalChars->GetValue());

  configuration->CopyBitmap(m_copyBitmap->GetValue());
  configuration->CopyMathML(m_copyMathML->GetValue());
  configuration->CopyMathMLHTML(m_copyMathMLHTML->GetValue());
  configuration->CopyRTF(m_copyRTF->GetValue());
  configuration->CopySVG(m_copySVG->GetValue());
#if wxUSE_ENH_METAFILE
  configuration->CopyEMF(m_copyEMF->GetValue());
#endif

  m_configuration->WriteStyles();
  config->Flush();

  {
    wxFileName startupFile(m_startupFileName);
    startupFile.MakeAbsolute();
    wxString startupDir = startupFile.GetPath();
    if (!wxDirExists(startupDir)) {
      wxLogMessage(_("The directory %s with maxima's startup file "
                     "doesn't exist. Trying to create it..."),
                   startupDir.mb_str());
      wxMkdir(startupDir);
    }

    wxFileOutputStream output(m_startupFileName);
    if (output.IsOk()) {
      wxTextOutputStream text(output);
      text << m_startupCommands->GetValue();
      text.Flush();
    }
  }

  {
    wxFileOutputStream output(m_wxStartupFileName);
    if (output.IsOk()) {
      wxTextOutputStream text(output);
      text << m_wxStartupCommands->GetValue();
      text.Flush();
    }
  }
  config->Write(wxS("ConfigDialogTab"), m_notebook->GetSelection());
}

void ConfigDialogue::OnChangeFontFamily(wxCommandEvent &WXUNUSED(event)) {
  TextStyle const st = GetSelectedStyle();
  auto style = m_configuration->GetWritableStyle(st);

  auto userFont =
    wxGetFontFromUser(this, style->GetFont(),
                      wxString::Format(_("Choose the %s Font"),
                                       m_configuration->GetStyleName(st)));
  if (!userFont.IsOk())
    return;

  style->SetFromFont(userFont);
  m_configuration->GetWritableStyle(st)->SetFontName(style->GetFontName());
  m_configuration->GetWritableStyle(st)->FontSize(style->GetFontSize());

  UpdateExample();
}

void ConfigDialogue::OnChangeColor(wxColourPickerEvent& event) {
  TextStyle const st = GetSelectedStyle();
  wxColour col = event.GetColour();
  if (col.IsOk())
    m_configuration->GetWritableStyle(st)->SetColor(col);
  UpdateExample();
}

void ConfigDialogue::OnChangeStyle(wxCommandEvent &WXUNUSED(event)) {
  auto const st = GetSelectedStyle();

  bool canChangeFontName    =   !m_configuration->GetStyle(st)->CantChangeFontName();
  bool canChangeFontVariant =   !m_configuration->GetStyle(st)->CantChangeFontVariant();

  m_getStyleFont->Enable(canChangeFontName);

  // Text styles with adjustable bold/italic/underline
  m_boldCB->SetValue(m_configuration->GetStyle(st)->IsBold());
  m_italicCB->SetValue(m_configuration->GetStyle(st)->IsItalic());
  m_slantedCB->SetValue(m_configuration->GetStyle(st)->IsSlant());
  m_underlinedCB->SetValue(m_configuration->GetStyle(st)->IsUnderlined());
  m_strikethroughCB->SetValue(m_configuration->GetStyle(st)->IsStrikethrough());
  m_underlinedCB->SetValue(m_configuration->GetStyle(st)->IsUnderlined());
  m_boldCB->Enable(canChangeFontVariant);
  m_italicCB->Enable(canChangeFontVariant);
  m_slantedCB->Enable(canChangeFontVariant);
  m_underlinedCB->Enable(canChangeFontVariant);
  m_strikethroughCB->Enable(canChangeFontVariant);

  UpdateExample();
}

void ConfigDialogue::OnExportAll(wxCommandEvent &WXUNUSED(event)) {
  wxString file = wxFileSelector(
                                 _("Save config to file"), wxEmptyString, wxS("style.ini"), wxS("ini"),
                                 _("Config file (*.ini)|*.ini"), wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
  if (file != wxEmptyString) {
    WriteSettings();
    m_configuration->WriteSettings(file);
  }
}

void ConfigDialogue::OnImport(wxCommandEvent &WXUNUSED(event)) {
  wxString file = wxFileSelector(
                                 _("Read config to file"), wxEmptyString, wxS("style.ini"), wxS("ini"),
                                 _("Config file (*.ini)|*.ini"), wxFD_OPEN | wxFD_FILE_MUST_EXIST);
  if (file != wxEmptyString) {
    wxFileInputStream fis(file);
    wxConfigBase *src = new wxFileConfig(fis);

    wxString str;
    long dummy;
    // first enum all entries
    bool bCont = src->GetFirstEntry(str, dummy);
    if (bCont)
      CopyConfig(src, wxConfigBase::Get());
    m_configuration->ReadStyles(file);
    SetCheckboxValues();
    wxCommandEvent dmmy;
    OnChangeStyle(dmmy);
  }
}

void ConfigDialogue::CopyConfig(wxConfigBase *src, wxConfigBase *dst,
                                wxString dir) {
  bool bCont = true;
  long dummy;
  wxString str;
  src->SetPath(dir);
  dst->SetPath(dir);
  src->GetFirstEntry(str, dummy);
  while (bCont) {
    switch (src->GetEntryType(str)) {
    case wxConfigBase::EntryType::Type_String:
      wxLogMessage(_("Copying config string \"%s\""),
                   wxString(src->GetPath() + wxS("/") + str).mb_str());
      dst->Write(str, src->ReadBool(str, wxEmptyString));
      break;
    case wxConfigBase::EntryType::Type_Boolean:
      wxLogMessage(_("Copying config bool \"%s\""),
                   wxString(src->GetPath() + wxS("/") + str).mb_str());
      dst->Write(str, src->ReadBool(str, false));
      break;
    case wxConfigBase::EntryType::Type_Integer:
      wxLogMessage(_("Copying config int \"%s\""),
                   wxString(src->GetPath() + wxS("/") + str).mb_str());
      dst->Write(str, src->ReadLong(str, 0));
      break;
    case wxConfigBase::EntryType::Type_Float:
      wxLogMessage(_("Copying config float \"%s\""),
                   wxString(src->GetPath() + wxS("/") + str).mb_str());
      dst->Write(str, src->ReadDouble(str, 0.0));
      break;
    default:
      wxLogMessage(
                   _("Config item \"%s\" was of an unknown type"),
                   wxString(src->GetPath() + wxS("/") + str).mb_str());
    }
    bCont = src->GetNextEntry(str, dummy);
  }

  bCont = src->GetFirstGroup(str, dummy);
  while (bCont) {
    CopyConfig(src, dst, str);
    bCont = src->GetNextGroup(str, dummy);
  }
  if (dir != "/") {
    src->SetPath("..");
    dst->SetPath("..");
  }
}

void ConfigDialogue::OnResetAllToDefaults(wxCommandEvent &WXUNUSED(event)) {
  wxLogMessage(_("Resetting all configuration settings"));
  m_configuration->ResetAllToDefaults();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnResetStyles(wxCommandEvent &WXUNUSED(event)) {
  wxLogMessage(_("Resetting all configuration settings"));
  m_configuration->InitStyles();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnReloadAll(wxCommandEvent &WXUNUSED(event)) {
  wxLogMessage(_("Reloading the configuration from disc"));
  m_configuration->ReadConfig();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnReloadStyles(wxCommandEvent &WXUNUSED(event)) {
  wxLogMessage(_("Reloading the styles from disc"));
  m_configuration->ReadStyles();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnCheckbox(wxCommandEvent &event) {
  // Slanted italic doesn't make sense
  if((event.GetId() == checkbox_italic) && (m_italicCB->GetValue()))
    m_slantedCB->SetValue(false);
  if((event.GetId() == checkbox_slanted) && (m_slantedCB->GetValue()))
    m_italicCB->SetValue(false);
  TextStyle const st = GetSelectedStyle();
  m_configuration->GetWritableStyle(st)->SetBold(m_boldCB->GetValue());
  if(m_italicCB->GetValue())
    m_configuration->GetWritableStyle(st)->SetItalic(m_italicCB->GetValue());
  else
    m_configuration->GetWritableStyle(st)->SetSlant(m_slantedCB->GetValue());
  m_configuration->GetWritableStyle(st)->SetUnderlined(m_underlinedCB->GetValue());
  m_configuration->GetWritableStyle(st)->SetStrikethrough(m_strikethroughCB->GetValue());
  UpdateExample();
}

void ConfigDialogue::OnChangeWarning(wxCommandEvent &WXUNUSED(event)) {
  LoggingMessageBox(_("Please restart wxMaxima for changes to take effect!"),
                    _("Configuration warning"), wxOK | wxICON_WARNING);
  UpdateExample();
}

void ConfigDialogue::UpdateExample() {
  m_configuration->MakeStylesConsistent();
  TextStyle const st = GetSelectedStyle();
  auto style = m_configuration->GetStyle(st);
  m_getStyleFont->SetLabel(style->GetFont().GetNativeFontInfoDesc());

  m_styleColor->SetColour(style->GetColor());

  m_examplePanel->SetStyle(*style);

  if (st == TS_TEXT_BACKGROUND || st == TS_TEXT) {
    m_examplePanel->SetBackgroundColour(
                                        m_configuration->m_styles[TS_TEXT_BACKGROUND].GetColor());
  } else {
    m_examplePanel->SetBackgroundColour(
                                        m_configuration->m_styles[TS_DOCUMENT_BACKGROUND].GetColor());
  }
  m_sampleWorksheet->Refresh();
}

void ConfigDialogue::OnTabChange(wxBookCtrlEvent &event) {
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxS("ConfigDialogTab"), event.GetSelection());
  UpdateExample();
}

void ConfigDialogue::LoadSave(wxCommandEvent &event) {
  if (event.GetId() == save_id) {
    wxString file = wxFileSelector(
                                   _("Save style to file"), wxEmptyString, wxS("style.ini"), wxS("ini"),
                                   _("Config file (*.ini)|*.ini"), wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (file != wxEmptyString)
      m_configuration->WriteStyles(file);
  } else {
    wxString file = wxFileSelector(_("Load style from file"), wxEmptyString,
                                   wxS("style.ini"), wxS("ini"),
                                   _("Config file (*.ini)|*.ini"), wxFD_OPEN);
    if (file != wxEmptyString) {
      m_configuration->ReadStyles(file);
      wxCommandEvent dummy;
      OnChangeStyle(dummy);
    }
  }
  m_sampleWorksheet->Refresh();
}


void ConfigDialogue::ExamplePanel::OnPaint(wxPaintEvent &WXUNUSED(event)) {
  wxString example(_("Example text"));
  wxPaintDC dc(this);
  int panel_width, panel_height;
  int text_width, text_height;

  GetClientSize(&panel_width, &panel_height);

  auto style = m_style;

  dc.SetTextForeground(style.GetColor());

  if (!style.IsFontOk()) {
    style.SetFontName({});
    style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);
    style.SetFontSize(m_style.GetFontSize());
  }

  if (style.IsFontOk())
    dc.SetFont(style.GetFont());

  dc.GetTextExtent(example, &text_width, &text_height);

  dc.DrawText(example, (panel_width - text_width) / 2,
              (panel_height - text_height) / 2);
}
