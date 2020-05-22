// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
#include "BTextCtrl.h"
#include "Cell.h"
#include "Dirstructure.h"
#include "FontCache.h"
#include <wx/config.h>
#include <wx/display.h>
#include <wx/fileconf.h>
#include <wx/font.h>
#include <wx/txtstrm.h>
#include <wx/fontdlg.h>
#include <wx/sstream.h>
#include <wx/colordlg.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include "../art/config/images.h"
#include <wx/dcbuffer.h>
#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include "Image.h"

#define CONFIG_ICON_SCALE (1.0)

#define ABS(val) ((val) >= 0 ? (val) : -(val))

int ConfigDialogue::GetImageSize()
{
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
  if(ppi < 10)
    ppi = wxGetDisplayPPI().x;
  if(ppi <= 10)
    ppi = 72;

  double targetSize = wxMax(ppi,75) * CONFIG_ICON_SCALE;

  int sizeA = 128 << 4;
  while(sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while(sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if(ABS(targetSize - sizeA) < ABS(targetSize - sizeB)) {
    return sizeA;
  } else {
    return sizeB;
  }
}

wxBitmap ConfigDialogue::GetImage(wxString name,
                          unsigned char *data, size_t len)
{
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
  if(ppi <= 10)
    ppi = wxGetDisplayPPI().x;
  if(ppi <= 10)
    ppi = 72;
  
  int targetSize = wxMax(ppi,75) * CONFIG_ICON_SCALE;

  int sizeA = 128 << 4;
  while(sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while(sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if(ABS(targetSize - sizeA) < ABS(targetSize - sizeB)) {
    targetSize = sizeA;
  } else {
    targetSize = sizeB;
  }

  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_MENU, wxSize(targetSize, targetSize));
  wxImage img;

  if(bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }
  if(!img.IsOk())
    return SvgBitmap(data, len, targetSize, targetSize);
  else
  {
    img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH); 
    return wxBitmap(img,wxBITMAP_SCREEN_DEPTH);
  }
}


ConfigDialogue::ConfigDialogue(wxWindow *parent, Configuration *cfg)
{
  m_svgRast.reset(nsvgCreateRasterizer());
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
  
  m_configuration = cfg;
#if defined __WXOSX__
  SetSheetStyle(wxPROPSHEET_BUTTONTOOLBOOK | wxPROPSHEET_SHRINKTOFIT);
#else
  SetSheetStyle(wxPROPSHEET_LISTBOOK);
#endif
  SetSheetInnerBorder(3);
  SetSheetOuterBorder(3);

  int imgSize = GetImageSize();
  m_imageList = std::unique_ptr<wxImageList>(new wxImageList(imgSize, imgSize));
  m_imageList->Add(GetImage(wxT("editing"),
                            editing_svg_gz,editing_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("maxima"),
                            maxima_svg_gz,maxima_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("styles"),
                            styles_svg_gz,styles_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("document-export"),
                            document_export_svg_gz,document_export_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("options"),
                            options_svg_gz,options_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("edit-copy"),
                            edit_copy_confdialogue_svg_gz,edit_copy_confdialogue_svg_gz_len
                     ));
  m_imageList->Add(GetImage(wxT("media-playback-start"),
                            media_playback_start_confdialogue_svg_gz,media_playback_start_confdialogue_svg_gz_len
                     ));

  Create(parent, wxID_ANY, _("wxMaxima configuration"),
         wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE);

  m_notebook = GetBookCtrl();

  m_notebook->SetImageList(m_imageList.get());

  m_notebook->AddPage(CreateWorksheetPanel(), _("Worksheet"), true, 0);
  m_notebook->AddPage(CreateMaximaPanel(), _("Maxima"), false, 1);
  m_notebook->AddPage(CreateStylePanel(), _("Style"), false, 2);
  m_notebook->AddPage(CreateExportPanel(), _("Export"), false, 3);
  m_notebook->AddPage(CreateOptionsPanel(), _("Options"), false, 4);
  m_notebook->AddPage(CreateClipboardPanel(), _("Copy"), false, 5);
  m_notebook->AddPage(CreateStartupPanel(), _("Startup commands"), false, 6);
#ifndef __WXOSX__
  CreateButtons(wxOK | wxCANCEL);
#endif

  wxConfigBase *config = wxConfig::Get();
  int notebookTab = 0;
  config->Read(wxT("ConfigDialogTab"), &notebookTab);
  if((notebookTab < 0) || ((unsigned int)notebookTab > m_notebook->GetPageCount()))
     notebookTab = 0;
  m_notebook->SetSelection(notebookTab);

  // Allow the property dialogue sheets to scroll, if they don't fit
  // on the screen.
  SetLayoutAdaptationMode(wxDIALOG_ADAPTATION_MODE_ENABLED);

  LayoutDialog();

  SetProperties();

  Connect(wxEVT_CLOSE_WINDOW, wxCloseEventHandler(ConfigDialogue::OnClose),NULL, this);
  Connect(wxID_OPEN, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnMpBrowse), NULL, this);
  Connect(button_mathFont, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnMathBrowse), NULL, this);
  Connect(font_family, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnChangeFontFamily), NULL, this);
  Connect(listbox_styleFor, wxEVT_LISTBOX, wxCommandEventHandler(ConfigDialogue::OnChangeStyle), NULL, this);
  Connect(language_id, wxEVT_COMBOBOX, wxCommandEventHandler(ConfigDialogue::OnChangeWarning), NULL, this);
  Connect(checkbox_bold, wxEVT_CHECKBOX, wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_italic, wxEVT_CHECKBOX, wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(checkbox_underlined, wxEVT_CHECKBOX, wxCommandEventHandler(ConfigDialogue::OnCheckbox), NULL, this);
  Connect(save_id, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::LoadSave), NULL, this);
  Connect(load_id, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::LoadSave), NULL, this);
  Connect(style_font_family, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnChangeFontFamily), NULL, this);
}

ConfigDialogue::~ConfigDialogue()
{}


void ConfigDialogue::UsesvgChanged(
  #ifdef __WXOSX__
  wxCommandEvent &event
  #else
  wxCommandEvent &WXUNUSED(event)
  #endif
  )
{
  #ifdef __WXOSX__
  if(event.IsChecked())
  {
    m_usesvg->SetForegroundColour(*wxRED);
  }
  else
  {
    wxSystemSettings systemSettings;
    m_usesvg->SetForegroundColour(systemSettings.GetColour(wxSYS_COLOUR_WINDOWTEXT));
  }
  #endif
}

void ConfigDialogue::MaximaLocationChanged(wxCommandEvent& WXUNUSED(unused))
{
  if (m_configuration->MaximaFound(m_maximaUserLocation->GetValue()))
    m_noAutodetectMaxima->SetForegroundColour(
      wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)
      );
  else
    m_noAutodetectMaxima->SetForegroundColour(*wxRED);
}

void ConfigDialogue::SetProperties()
{
  Configuration *configuration = m_configuration;
  SetTitle(_("wxMaxima configuration"));

  m_showUserDefinedLabels->SetToolTip(
          _("Maxima assigns each command/equation an automatic label (which looks like %i1 or %o1). If a command begins with a descriptive name followed by a : wxMaxima will call the descriptive name an \"user-defined label\" instead. This selection now allows to tell wxMaxima if to show only automatic labels, automatic labels if there aren't user-defined ones or no label at all until an user-defined label can be found by wxMaxima's heuristics. If automatic labels are suppressed extra vertical space is added between equations in order to ease discerning which line starts a new equation and which one only continues the one from the last line."));
  m_abortOnError->SetToolTip(
          _("If multiple cells are evaluated in one go: Abort evaluation if wxMaxima detects that maxima has encountered any error."));
  m_openHCaret->SetToolTip(_("If this checkbox is set a new code cell is opened as soon as maxima requests data. If it isn't set a new code cell is opened in this case as soon as the user starts typing in code."));
  m_restartOnReEvaluation->SetToolTip(
          _("Maxima provides no \"forget all\" command that flushes all settings a maxima session could make. wxMaxima therefore normally defaults to starting a fresh maxima process every time the worksheet is to be re-evaluated. As this needs a little bit of time this switch allows to disable this behavior."));
  m_maximaUserLocation->SetToolTip(_("Enter the path to the Maxima executable."));
  m_additionalParameters->SetToolTip(_("Additional parameters for Maxima"
                                               " (e.g. -l clisp)."));
  m_mathJaxURL->SetToolTip(_("The URL MathJaX.js should be downloaded from by our HTML export."));
  m_texPreamble->SetToolTip(_("Additional commands to be added to the preamble of LaTeX output for pdftex."));
  m_useJSMath->SetToolTip(_("Use nice js math symbols in order to get nice integral, sum, product and sqrt signs\nWill only work if the corresponding js math fonts can be found by wxMaxima."));
  m_useUnicodeMaths->SetToolTip(_("If the font provides big parenthesis symbols: Use them when big parenthesis are needed for maths display."));
  m_autoSave->SetToolTip(
          _("If this checkbox is checked wxMaxima automatically saves the file closing and every few minutes giving wxMaxima a more cellphone-app-like feel as the file is virtually always saved. If this checkbox is unchecked from time to time a backup is made in the temp folder instead."));
  m_defaultFramerate->SetToolTip(_("Define the default speed (in frames per second) animations are played back with."));
  m_maxGnuplotMegabytes->SetToolTip(_("wxMaxima normally stores the gnuplot sources for every plot made using draw() in order to be able to open plots interactively in gnuplot later. This setting defines the limit [in Megabytes per plot] for this feature."));
  m_defaultPlotWidth->SetToolTip(
          _("The default width for embedded plots. Can be read out or overridden by the maxima variable wxplot_size"));
  m_defaultPlotHeight->SetToolTip(
          _("The default height for embedded plots. Can be read out or overridden by the maxima variable wxplot_size."));
  m_displayedDigits->SetToolTip(
          _("If numbers are getting longer than this number of digits they will be displayed abbreviated by an ellipsis."));
  m_AnimateLaTeX->SetToolTip(
          _("Some PDF viewers are able to display moving images and wxMaxima is able to output them. If this option is selected additional LaTeX packages might be needed in order to compile the output, though."));
  m_TeXExponentsAfterSubscript->SetToolTip(
          _("In the LaTeX output: Put exponents after an eventual subscript instead of above it. Might increase readability for some fonts and short subscripts."));
  m_usePartialForDiff->SetToolTip(
          _("Use the \"partial derivative\" symbol to represent the fraction that represents a diff() when exporting LaTeX"));
  m_wrapLatexMath->SetToolTip(
          _("Wrap equations exported by the \"copy as LaTeX\" feature between \\[ and \\] as equation markers"));
  m_bitmapScale->SetToolTip(
          _("Normally html expects images to be rather low-res but space saving. These images tend to look rather blurry when viewed on modern screens. Therefore this setting was introduces that selects the factor by which the HTML export increases the resolution in respect to the default value."));
  m_printScale->SetToolTip(
          _("A scale factor for the printout. Helpful for printing big equations on small pdf pages."));
  m_exportContainsWXMX->SetToolTip(
          _("If this option is set the .wxmx source of the current file is copied to a place a link to is put into the result of an export."));
  m_printBrackets->SetToolTip(
          _("For each Text-, Sectioning or code cell wxMaxima can display a bracket showing the extend of the cell and allowing to fold it. This setting now tells if this bracket is to be printed, as well."));
  m_exportWithMathJAX->SetToolTip(
          _("MathJAX creates scalable High-Quality representations of 2D Maths that can be used for Drag-And-Drop and provides accessibility options. The disadvantage of MathJAX is that it needs JavaScript and a little bit of time in order to typeset equations.\nMathML is much faster than MathJaX, if it is supported by the browser. But many MathML implementations tend to lack necessary features.\nBitmaps tend to need more band width than the other two options. They lack support for advanced features like drag-and-drop or accessibility. Also they have problems aligning and scaling with the rest of the text and might use fonts that don't match the rest of the document."));
  m_savePanes->SetToolTip(_("Save panes layout between sessions."));
  m_usesvg->SetToolTip(
          _("PNG images can be read by old wxMaxima versions - but aren't really scalable."));
  m_antialiasLines->SetToolTip(
          _("Try to antialias lines (which allows to move them by a fraction of a pixel, but reduces their sharpness)."));
  m_matchParens->SetToolTip(
          _("Automatically insert matching parenthesis in text controls. Automatic highlighting of matching parenthesis can be suppressed by setting the respective color to match the background of ordinary text."));
  m_showLength->SetToolTip(_("Show long expressions in wxMaxima document."));
  m_autosubscript->SetToolTip(
          _("false=Don't generate subscripts\ntrue=Automatically convert underscores to subscript markers if the would-be subscript is a number or a single letter\nall=_ marks subscripts."));
  m_language->SetToolTip(_("Language used for wxMaxima GUI."));
  m_symbolPaneAdditionalChars->SetToolTip(
          _("Symbols that are entered or copied here will appear in the symbols sidebar so they can be entered into the worksheet easily."));
  m_documentclass->SetToolTip(_("The document class LaTeX is instructed to use for our documents."));
  m_documentclassOptions->SetToolTip(_("The options the document class LaTeX is instructed to use for our documents gets."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_offerKnownAnswers->SetToolTip(_("wxMaxima remembers the answers to maxima's questions. If this checkbox is set it automatically offers to enter the last answer to this question the user has input."));
  m_getFont->SetToolTip(_("Font used for display in document."));
  m_getMathFont->SetToolTip(_("Font used for displaying math characters in document."));
  m_changeAsterisk->SetToolTip(_("Use centered dot and Minus, not Star and Hyphen"));
  m_hidemultiplicationSign->SetToolTip(_("Hide all multiplication signs that aren't really necessary for understanding the equation"));
  m_latin2Greek->SetToolTip(_("Change the text \"alpha\" and \"beta\" to the corresponding greek letter?"));
  m_defaultPort->SetToolTip(_("The default port used for communication between Maxima and wxMaxima."));
  m_undoLimit->SetToolTip(
          _("Save only this number of actions in the undo buffer. 0 means: save an infinite number of actions."));
  m_recentItems->SetToolTip(_("The number of recently opened files that is to be remembered."));
  m_incrementalSearch->SetToolTip(_("Start searching while the phrase to search for is still being typed."));
  m_notifyIfIdle->SetToolTip(_("Issue a notification if maxima finishes calculating while the wxMaxima window isn't in focus."));

  m_hideBrackets->SetToolTip(
          _("Hide the brackets that mark the extend of the worksheet cells at the worksheet's right side and that contain the \"hide\" button of the cell if the cells aren't active."));
  m_indentMaths->SetToolTip(
          _("Indent maths so all lines are in par with the first line that starts after the label."));

  wxConfigBase *config = wxConfig::Get();
  wxString mp, mc, ib, mf;

  // The default values for all config items that will be used if there is no saved
  // configuration data for this item.
  bool savePanes = true;
  bool fixedFontTC = true, usejsmath = true, keepPercent = true;
  bool saveUntitled = true,
          AnimateLaTeX = true, TeXExponentsAfterSubscript = false,
          usePartialForDiff = false,
          wrapLatexMath = true,
          exportContainsWXMX = false;
  int exportWithMathJAX = 0;
  bool cursorJump = true;

  int labelWidth = 4;
  int undoLimit = 0;
  int recentItems = 10;
  int bitmapScale = 3;
  bool incrementalSearch = true;
  int defaultFramerate = 2;
  wxString texPreamble = wxEmptyString;

  int panelSize = 1;
  config->Read(wxT("maxima"), &mp);
  config->Read(wxT("parameters"), &mc);
  config->Read(wxT("AUI/savePanes"), &savePanes);
  config->Read(wxT("DefaultFramerate"), &defaultFramerate);
  int defaultPlotWidth = 600;

  config->Read(wxT("defaultPlotWidth"), &defaultPlotWidth);
  int defaultPlotHeight = 400;
  config->Read(wxT("defaultPlotHeight"), &defaultPlotHeight);
  config->Read(wxT("AnimateLaTeX"), &AnimateLaTeX);
  config->Read(wxT("TeXExponentsAfterSubscript"), &TeXExponentsAfterSubscript);
  config->Read(wxT("usePartialForDiff"), &usePartialForDiff);
  config->Read(wxT("wrapLatexMath"), &wrapLatexMath);
  config->Read(wxT("exportContainsWXMX"), &exportContainsWXMX);
  config->Read(wxT("HTMLequationFormat"), &exportWithMathJAX);
  config->Read(wxT("texPreamble"), &texPreamble);
  config->Read(wxT("fixedFontTC"), &fixedFontTC);
  config->Read(wxT("panelSize"), &panelSize);
  config->Read(wxT("saveUntitled"), &saveUntitled);
  config->Read(wxT("cursorJump"), &cursorJump);
  config->Read(wxT("labelWidth"), &labelWidth);
  config->Read(wxT("undoLimit"), &undoLimit);
  config->Read(wxT("recentItems"), &recentItems);
  config->Read(wxT("bitmapScale"), &bitmapScale);
  config->Read(wxT("incrementalSearch"), &incrementalSearch);
  config->Read(wxT("usejsmath"), &usejsmath);
  config->Read(wxT("keepPercent"), &keepPercent);
  
  m_documentclass->SetValue(configuration->Documentclass());
  m_documentclassOptions->SetValue(configuration->DocumentclassOptions());
  m_mathJaxURL->SetValue(configuration->MathJaXURL_User());
  m_autodetectMathJaX->SetValue(!configuration->MathJaXURL_UseUser());
  m_noAutodetectMathJaX->SetValue(configuration->MathJaXURL_UseUser());
  m_texPreamble->SetValue(texPreamble);
  m_autoSave->SetValue(!configuration->AutoSaveAsTempFile());

  m_maximaUserLocation->SetValue(configuration->MaximaUserLocation());
  wxCommandEvent dummy;
  MaximaLocationChanged(dummy);

  m_additionalParameters->SetValue(configuration->MaximaParameters());
  m_savePanes->SetValue(savePanes);
  m_usesvg->SetValue(configuration->UseSVG());
  m_antialiasLines->SetValue(configuration->AntiAliasLines());

  m_AnimateLaTeX->SetValue(AnimateLaTeX);
  m_TeXExponentsAfterSubscript->SetValue(TeXExponentsAfterSubscript);
  m_usePartialForDiff->SetValue(usePartialForDiff);
  m_wrapLatexMath->SetValue(wrapLatexMath);
  m_exportContainsWXMX->SetValue(exportContainsWXMX);
  m_printBrackets->SetValue(configuration->PrintBrackets());
  m_exportWithMathJAX->SetSelection((int)configuration->HTMLequationFormat());
  m_matchParens->SetValue(configuration->GetMatchParens());
  m_showLength->SetSelection(configuration->ShowLength());
  m_autosubscript->SetSelection(configuration->GetAutosubscript_Num());
  m_changeAsterisk->SetValue(configuration->GetChangeAsterisk());
  m_hidemultiplicationSign->SetValue(configuration->HidemultiplicationSign());
  m_latin2Greek->SetValue(configuration->Latin2Greek());
  m_enterEvaluates->SetValue(configuration->EnterEvaluates());
  m_saveUntitled->SetValue(saveUntitled);
  m_openHCaret->SetValue(configuration->GetOpenHCaret());
  m_insertAns->SetValue(configuration->GetInsertAns());
  m_autoIndent->SetValue(configuration->GetAutoIndent());
  m_cursorJump->SetValue(cursorJump);
  m_hideBrackets->SetValue(configuration->HideBrackets());
  m_indentMaths->SetValue(configuration->IndentMaths());
  int val = 0;
  if (configuration->GetAutoWrap()) val = 1;
//  if(configuration->GetAutoWrapCode()) val = 2;
  m_autoWrap->SetSelection(val);
  m_labelWidth->SetValue(labelWidth);
  m_undoLimit->SetValue(undoLimit);
  m_recentItems->SetValue(recentItems);
  m_bitmapScale->SetValue(bitmapScale);
  m_printScale->SetValue(configuration->PrintScale());
  m_fixReorderedIndices->SetValue(configuration->FixReorderedIndices());
  m_incrementalSearch->SetValue(incrementalSearch);
  m_notifyIfIdle->SetValue(configuration->NotifyIfIdle());
  m_fixedFontInTC->SetValue(fixedFontTC);
  m_offerKnownAnswers->SetValue(m_configuration->OfferKnownAnswers());
  m_useJSMath->SetValue(usejsmath);
  m_keepPercentWithSpecials->SetValue(keepPercent);
  m_abortOnError->SetValue(configuration->GetAbortOnError());
  m_restartOnReEvaluation->SetValue(configuration->RestartOnReEvaluation());
  m_defaultFramerate->SetValue(defaultFramerate);
  m_maxGnuplotMegabytes->SetValue(configuration->MaxGnuplotMegabytes());
  m_defaultPlotWidth->SetValue(defaultPlotWidth);
  m_defaultPlotHeight->SetValue(defaultPlotHeight);
  m_displayedDigits->SetValue(configuration->GetDisplayedDigits());
  m_symbolPaneAdditionalChars->SetValue(configuration->SymbolPaneAdditionalChars());
  if (m_styleFor->GetSelection() >= 14 && m_styleFor->GetSelection() <= 18)
    m_getStyleFont->Enable(true);
  else
    m_getStyleFont->Enable(false);

  if (!wxFontEnumerator::IsValidFacename(CMEX10) ||
      !wxFontEnumerator::IsValidFacename(CMSY10) ||
      !wxFontEnumerator::IsValidFacename(CMR10)  ||
      !wxFontEnumerator::IsValidFacename(CMMI10) ||
      !wxFontEnumerator::IsValidFacename(CMTI10))
    m_useJSMath->Enable(false);
}

wxPanel *ConfigDialogue::CreateWorksheetPanel()
{
  Configuration *configuration = m_configuration;
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(10, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(32, 1, 5, 5);

  wxStaticText *pw = new wxStaticText(panel, -1, _("Default plot size for new maxima sessions:"));
  wxBoxSizer *PlotWidthHbox = new wxBoxSizer(wxHORIZONTAL);
  m_defaultPlotWidth = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS,
                                      100, 16384);
  PlotWidthHbox->Add(m_defaultPlotWidth, 0, wxEXPAND, 0);
  wxStaticText *xx = new wxStaticText(panel, -1, _("x"));
  PlotWidthHbox->Add(xx, 0, wxALIGN_CENTER_VERTICAL, 0);
  m_defaultPlotHeight = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS,
                                       100, 16384);
  PlotWidthHbox->Add(m_defaultPlotHeight, 0, wxEXPAND, 0);
  //  plotWidth->SetSizerAndFit(PlotWidthHbox);
  grid_sizer->Add(pw, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(PlotWidthHbox, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  wxStaticText *dd = new wxStaticText(panel, -1, _("Maximum displayed number of digits:"));
  m_displayedDigits = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 20,
                                     INT_MAX);
  grid_sizer->Add(dd, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_displayedDigits, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  wxStaticText *sl = new wxStaticText(panel, -1, _("Show long expressions:"));
  grid_sizer->Add(sl, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxArrayString showLengths;
  showLengths.Add(_("No"));
  showLengths.Add(_("If not very long"));
  showLengths.Add(_("If not extremely long"));
  showLengths.Add(_("Yes"));
  m_showLength = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, showLengths);
  grid_sizer->Add(m_showLength, 0, wxALL, 5);

  wxStaticText *aw = new wxStaticText(panel, -1, _("Autowrap long lines:"));
  grid_sizer->Add(aw, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxArrayString autoWrap;
  autoWrap.Add(_("No"));
  autoWrap.Add(_("Text Only"));
//  autoWrap.Add(_("Text & Code"));
  m_autoWrap = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, autoWrap);
  grid_sizer->Add(m_autoWrap, 0, wxALL, 5);

  wxStaticText *as = new wxStaticText(panel, -1, _("Underscore converts to subscripts:"));
  grid_sizer->Add(as, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxArrayString autosubscripts;
  autosubscripts.Add(_("Never"));
  autosubscripts.Add(_("Integers and single letters"));
  autosubscripts.Add(_("All variable names"));
  m_autosubscript = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, autosubscripts);
  grid_sizer->Add(m_autosubscript, 0, wxALL, 5);

  wxStaticText *lw = new wxStaticText(panel, -1, _("Label width:"));
  grid_sizer->Add(lw, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  m_labelWidth = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 3, 10);
  grid_sizer->Add(m_labelWidth, 0, wxALL, 5);

  wxStaticText *slt = new wxStaticText(panel, -1, _("Show labels:"));
  grid_sizer->Add(slt, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxArrayString labelchoices;
  labelchoices.Add(_("Automatic labels (%i1, %o1,...)"));
  labelchoices.Add(_("User-defined labels if available"));
  labelchoices.Add(_("Only user-defined labels"));
  labelchoices.Add(_("Never"));
  m_showUserDefinedLabels = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, labelchoices);
  m_showUserDefinedLabels->SetSelection(configuration->GetLabelChoice());

  grid_sizer->Add(m_showUserDefinedLabels, 0, wxALL, 5);

  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);

  m_hideBrackets = new wxCheckBox(panel, -1, _("Intelligently hide cell brackets"));
  vsizer->Add(m_hideBrackets, 0, wxALL, 5);

  m_indentMaths = new wxCheckBox(panel, -1, _("Indent equations by the label width"));
  vsizer->Add(m_indentMaths, 0, wxALL, 5);

  m_enterEvaluates = new wxCheckBox(panel, -1, _("Enter evaluates cells"));
  vsizer->Add(m_enterEvaluates, 0, wxALL, 5);

  m_openHCaret = new wxCheckBox(panel, -1, _("Open a cell when Maxima expects input"));
  vsizer->Add(m_openHCaret, 0, wxALL, 5);

  m_matchParens = new wxCheckBox(panel, -1, _("Match parenthesis in text controls"));
  vsizer->Add(m_matchParens, 0, wxALL, 5);

  m_changeAsterisk = new wxCheckBox(panel, -1, _("Use centered dot character for multiplication"));
  vsizer->Add(m_changeAsterisk, 0, wxALL, 5);

  m_hidemultiplicationSign = new wxCheckBox(panel, -1, _("Hide multiplication signs, if possible"));
  vsizer->Add(m_hidemultiplicationSign, 0, wxALL, 5);

  m_latin2Greek = new wxCheckBox(panel, -1, _("Change names of greek letters to greek letters"));
  vsizer->Add(m_latin2Greek, 0, wxALL, 5);

  m_keepPercentWithSpecials = new wxCheckBox(panel, -1, _("Keep percent sign with special symbols: %e, %i, etc."));
  vsizer->Add(m_keepPercentWithSpecials, 0, wxALL, 5);

  m_insertAns = new wxCheckBox(panel, -1, _("Insert % before an operator at the beginning of a cell"));
  vsizer->Add(m_insertAns, 0, wxALL, 5);

  m_autoIndent = new wxCheckBox(panel, -1, _("Auto-indent new lines"));
  vsizer->Add(m_autoIndent, 0, wxALL, 5);

  m_cursorJump = new wxCheckBox(panel, -1, _("New lines: Jump to text"));
  vsizer->Add(m_cursorJump, 0, wxALL, 5);

  m_fixedFontInTC = new wxCheckBox(panel, -1, _("Fixed font in text controls"));
  vsizer->Add(m_fixedFontInTC, 0, wxALL, 5);

  m_offerKnownAnswers = new wxCheckBox(panel, -1, _("Offer known answers"));
  vsizer->Add(m_offerKnownAnswers, 0, wxALL, 5);
  
  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel *ConfigDialogue::CreateStartupPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);

  wxPanel *panel_maximaStartup = new wxPanel(panel, -1);
  wxPanel *panel_wxMaximaStartup = new wxPanel(panel, -1);
  wxBoxSizer *vsizer_maximaStartup = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *vsizer_wxMaximaStartup = new wxBoxSizer(wxVERTICAL);


  m_startupFileName = Dirstructure::Get()->UserConfDir();
  m_wxStartupFileName += m_startupFileName + wxT("wxmaxima-init.mac");
  m_startupFileName += wxT("maxima-init.mac");

  wxStaticText *wxStartupText =
    new wxStaticText(panel_wxMaximaStartup, wxID_ANY,
                     _("Maxima commands to be executed every time wxMaxima starts Maxima: "));
  wxStartupText->SetToolTip(_("The part of the output of these commands that isn't declared as "
                              "\"math\" might be suppressed by wxMaxima. As always maxima "
                              "commands are required to end in a \";\" or a \"$\""));
  vsizer_wxMaximaStartup->Add(wxStartupText, wxSizerFlags().Border(wxALL,5));

  // Read the contents of wxMaxima's startup file
  wxString contents;
  if(wxFileExists(m_wxStartupFileName))
  {
    wxFileInputStream input(m_wxStartupFileName);
    if(input.IsOk())
    {
      wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      while(input.IsOk() && !input.Eof())
      {
        wxString line = text.ReadLine();
        if((!input.Eof()) || (line != wxEmptyString))
          contents += line + wxT("\n");
      }
    }
  }
  m_wxStartupCommands = new BTextCtrl(panel_wxMaximaStartup, -1, m_configuration, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(),250*GetContentScaleFactor()),
                                     wxTE_MULTILINE | wxHSCROLL);
  m_wxStartupCommands->SetValue(contents);
  vsizer_wxMaximaStartup->Add(m_wxStartupCommands, wxSizerFlags().Expand().Border(wxALL,5));
  wxStaticText *wxStartupFileLocation = new wxStaticText(panel_wxMaximaStartup, wxID_ANY,
                                                         _("wxMaxima startup file location: ") +
                                                         m_wxStartupFileName);
  wxStartupFileLocation->SetToolTip(_("This file won't be read by maxima if maxima is used "
                                      "without wxMaxima. In order to add startup commands "
                                      "that are executed in this case, "
                                      "too, please add them to maxima-init.mac, instead."));
  vsizer_wxMaximaStartup->Add(wxStartupFileLocation, wxSizerFlags().Border(wxALL,5));

  panel_wxMaximaStartup->SetSizerAndFit(vsizer_wxMaximaStartup);
  vsizer->Add(panel_wxMaximaStartup,wxSizerFlags().Expand().Border(wxBOTTOM, 5));

  wxStaticText *startupText =
    new wxStaticText(panel_maximaStartup, wxID_ANY,
                     _("Maxima commands to be executed at every start of Maxima: "));
  startupText->SetToolTip(_("The part of the output of these commands that isn't declared as "
                              "\"math\" might be suppressed by wxMaxima. As always maxima "
                              "commands are required to end in a \";\" or a \"$\""));
  vsizer_maximaStartup->Add(startupText, wxSizerFlags().Border(wxALL,5));
  // Read maxima's startup file's contents
  contents = wxEmptyString;
  if(wxFileExists(m_startupFileName))
  {
    wxFileInputStream input(m_startupFileName);
    if(input.IsOk())
    {
      wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      while(input.IsOk() && !input.Eof())
      {
        wxString line = text.ReadLine();
        if((!input.Eof()) || (line != wxEmptyString))
          contents += line + wxT("\n");
      }
    }
  }
  m_startupCommands = new BTextCtrl(panel_maximaStartup, -1, m_configuration, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(),250*GetContentScaleFactor()),
                                     wxTE_MULTILINE | wxHSCROLL);
  m_startupCommands->SetValue(contents);

  vsizer_maximaStartup->Add(m_startupCommands, wxSizerFlags().Expand().Border(wxALL,5));
  wxStaticText *startupFileLocation = new wxStaticText(panel_maximaStartup, wxID_ANY,
                                                       _("Maxima startup file location: ") +
                                                       m_startupFileName);
  startupFileLocation->SetToolTip(_("Commands that are executed at every start of maxima."));
  vsizer_maximaStartup->Add(startupFileLocation, wxSizerFlags().Border(wxALL,5));
  panel_maximaStartup->SetSizerAndFit(vsizer_maximaStartup);
  vsizer->Add(panel_maximaStartup,wxSizerFlags().Expand().Border(wxTOP, 5));
  panel->SetSizerAndFit(vsizer);
  return panel;
}

wxPanel *ConfigDialogue::CreateExportPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(9, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(17, 1, 5, 5);

  wxStaticText *dc = new wxStaticText(panel, -1, _("Documentclass for TeX export:"));
  m_documentclass = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350*GetContentScaleFactor(), wxDefaultSize.GetY()));
  grid_sizer->Add(dc, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_documentclass, 0, wxALL, 5);

  wxStaticText *dco = new wxStaticText(panel, -1, _("Documentclass options:"));
  m_documentclassOptions = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350*GetContentScaleFactor(), wxDefaultSize.GetY()));
  grid_sizer->Add(dco, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_documentclassOptions, 0, wxALL, 5);

  wxStaticText *tp = new wxStaticText(panel, -1, _("Additional lines for the TeX preamble:"));
  m_texPreamble = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350*GetContentScaleFactor(), 100),
                                 wxTE_MULTILINE | wxHSCROLL);
  grid_sizer->Add(tp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_texPreamble, 0, wxALL, 5);
  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);

  wxStaticText *mju = new wxStaticText(panel, -1, _("Export equations to HTML as:"));
  wxArrayString mathJaxChoices;
  mathJaxChoices.Add(_("TeX, interpreted by MathJaX"));
  mathJaxChoices.Add(_("Bitmaps"));
  mathJaxChoices.Add(_("MathML + MathJaX as Fill-In"));
  mathJaxChoices.Add(_("SVG graphics"));
  m_exportWithMathJAX = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, mathJaxChoices);
  grid_sizer->Add(mju, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_exportWithMathJAX, 0, wxALL, 5);
  wxStaticText *mj = new wxStaticText(panel, -1, _("URL MathJaX.js lies at:"));
  grid_sizer->Add(mj, 0, wxALL, 5);
  grid_sizer->Add(5,5);

  m_autodetectMathJaX = new wxRadioButton(panel, -1, _("Automatic"), wxDefaultPosition,
                                          wxDefaultSize, wxRB_GROUP);
  grid_sizer->Add(m_autodetectMathJaX, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxTextCtrl *autoMathJaxURL = new wxTextCtrl(panel, -1, m_configuration->MathJaXURL_Auto(), wxDefaultPosition, wxSize(350*GetContentScaleFactor(), wxDefaultSize.GetY()), wxTE_READONLY);
  grid_sizer->Add(autoMathJaxURL, 0, wxALL, 5);

  m_noAutodetectMathJaX = new wxRadioButton(panel, -1, _("User specified"));
  grid_sizer->Add(m_noAutodetectMathJaX, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  m_mathJaxURL = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350*GetContentScaleFactor(), wxDefaultSize.GetY()));
  grid_sizer->Add(m_mathJaxURL, 0, wxALL, 5);

  wxStaticText *bs = new wxStaticText(panel, -1, _("Bitmap scale for export:"));
  m_bitmapScale = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1, 3);
  grid_sizer->Add(bs, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_bitmapScale, 0, wxALL, 5);

  wxStaticText *ps = new wxStaticText(panel, -1, _("Print scale:"));
  m_printScale = new wxSpinCtrlDouble(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, .1, 4, .1);
  m_printScale->SetDigits(2);
  m_printScale->SetIncrement(.1);
  grid_sizer->Add(ps, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_printScale, 0, wxALL, 5);

  m_AnimateLaTeX = new wxCheckBox(panel, -1,
                                  _("Export animations to TeX (Images only move if the PDF viewer supports this)"));
  vsizer->Add(m_AnimateLaTeX, 0, wxALL, 5);

  m_TeXExponentsAfterSubscript = new wxCheckBox(panel, -1, _("LaTeX: Place exponents after, instead above subscripts"));
  vsizer->Add(m_TeXExponentsAfterSubscript, 0, wxALL, 5);

  m_usePartialForDiff = new wxCheckBox(panel, -1,
                                       _("LaTeX: Use the \"partial derivative\" symbol to represent diff()"));
  vsizer->Add(m_usePartialForDiff, 0, wxALL, 5);

  m_wrapLatexMath = new wxCheckBox(panel, -1, _("\"Copy LaTeX\" adds equation markers"));
  vsizer->Add(m_wrapLatexMath, 0, wxALL, 5);

  m_exportContainsWXMX = new wxCheckBox(panel, -1, _("Add the .wxmx file to the HTML export"));
  vsizer->Add(m_exportContainsWXMX, 0, wxALL, 5);

  m_printBrackets = new wxCheckBox(panel, -1, _("Print the cell brackets [drawn to their left]"));
  vsizer->Add(m_printBrackets, 0, wxALL, 5);

  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel *ConfigDialogue::CreateOptionsPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(7, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(18, 1, 5, 5);

  wxArrayString languages;
  for(Languages::const_iterator it = m_languages.begin(); it != m_languages.end(); ++it )
    languages.Add(it->first);
  
  m_language = new wxChoice(panel, language_id, wxDefaultPosition, wxSize(230*GetContentScaleFactor(), -1), languages);
  grid_sizer->Add(
    new wxStaticText(panel, -1, _("Language:")), 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_language, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  int lang = m_configuration->GetLanguage();
  unsigned int i = 0;
  // First set the language to "default".
  for(Languages::const_iterator it = m_languages.begin(); it != m_languages.end(); ++it )
  {
    if(it->second == wxLANGUAGE_DEFAULT)
    {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }

  // Now try to set the language to the one from the config
  i = 0;
  for(Languages::const_iterator it = m_languages.begin(); it != m_languages.end(); ++it )
  {
    if(it->second == lang)
    {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }

  wxStaticText *additionalSymbols = new wxStaticText(panel, -1, _("Additional symbols for the \"symbols\" sidebar:"));
  m_symbolPaneAdditionalChars = new wxTextCtrl(panel, -1);
  grid_sizer->Add(additionalSymbols, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_symbolPaneAdditionalChars, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  wxStaticText *ul = new wxStaticText(panel, -1, _("Undo limit (0 for none):"));
  m_undoLimit = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 0, 10000);
  grid_sizer->Add(ul, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_undoLimit, 0, wxALL, 5);

  wxStaticText *rf = new wxStaticText(panel, -1, _("Recent files list length:"));
  m_recentItems = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 5, 30);
  grid_sizer->Add(rf, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_recentItems, 0, wxALL, 5);

  wxStaticText *df = new wxStaticText(panel, -1, _("Default animation framerate:"));
  m_defaultFramerate = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 1,
                                      200);    
  grid_sizer->Add(df, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_defaultFramerate, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  wxStaticText *mm = new wxStaticText(panel, -1, _("Interactive popup memory limit [MB/plot]:"));
  m_maxGnuplotMegabytes = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 0,
                                         2000);
  
  grid_sizer->Add(mm, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_maxGnuplotMegabytes, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);
  
  m_savePanes = new wxCheckBox(panel, -1, _("Save panes layout"));
  vsizer->Add(m_savePanes, 0, wxALL, 5);

  m_autoSave = new wxCheckBox(panel, -1, _("Save the worksheet automatically"));
  vsizer->Add(m_autoSave, 0, wxALL, 5);

  m_usesvg = new wxCheckBox(panel, -1, _("Create scalable plots."));
  m_usesvg->Connect(wxEVT_CHECKBOX,
                         wxCommandEventHandler(ConfigDialogue::UsesvgChanged),
                         NULL, this);
  m_usesvg->Show(false);

  vsizer->Add(m_usesvg, 0, wxALL, 5);

  m_antialiasLines = new wxCheckBox(panel, -1, _("Antialias lines."));
  vsizer->Add(m_antialiasLines, 0, wxALL, 5);

  m_saveUntitled = new wxCheckBox(panel, -1, _("Ask to save untitled documents"));
  vsizer->Add(m_saveUntitled, 0, wxALL, 5);

  m_fixReorderedIndices = new wxCheckBox(panel, -1, _("Fix reordered reference indices (of %i, %o) before saving"));
  vsizer->Add(m_fixReorderedIndices, 0, wxALL, 5);
  m_incrementalSearch = new wxCheckBox(panel, -1, _("Incremental Search"));
  vsizer->Add(m_incrementalSearch, 0, wxALL, 5);

  m_notifyIfIdle = new wxCheckBox(panel, -1, _("Warn if an inactive window is idle"));
  vsizer->Add(m_notifyIfIdle, 0, wxALL, 5);


  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel *ConfigDialogue::CreateMaximaPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *sizer = new wxFlexGridSizer(5, 2, 0, 0);
  wxFlexGridSizer *sizer2 = new wxFlexGridSizer(6, 2, 0, 0);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(9, 1, 0, 0);

  wxFlexGridSizer *nameSizer = new wxFlexGridSizer(2, 3, 0, 0);
  m_mp = new wxStaticText(panel, -1, _("Maxima program:"));
  vsizer->Add(m_mp, wxSizerFlags().Expand().Border(wxALL, 0));
  m_autodetectMaxima = new wxRadioButton(panel, -1, _("Autodetect"), wxDefaultPosition,
                                         wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_autodetectMaxima, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(
    new wxTextCtrl(panel, -1, m_configuration->MaximaDefaultLocation(),
                   wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), wxTE_RICH|wxTE_READONLY));
  nameSizer->Add(10, 10);
  
  m_noAutodetectMaxima = new wxRadioButton(panel, -1, _("User specified"));
  m_autodetectMaxima->SetValue(m_configuration->AutodetectMaxima());
  m_noAutodetectMaxima->SetValue(!m_configuration->AutodetectMaxima());
  nameSizer->Add(m_noAutodetectMaxima, wxSizerFlags().Expand().Border(wxALL, 0));
  m_maximaUserLocation = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), wxTE_RICH);
  m_mpBrowse = new wxButton(panel, wxID_OPEN, _("Open"));
  nameSizer->Add(m_maximaUserLocation, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(m_mpBrowse, wxSizerFlags().Expand().Border(wxALL, 0));
  m_maximaUserLocation->Connect(wxEVT_COMMAND_TEXT_UPDATED,
                           wxCommandEventHandler(ConfigDialogue::MaximaLocationChanged),
                           NULL, this);
  vsizer->Add(nameSizer, wxSizerFlags().Expand().Border(wxALL, 0));
  m_defaultPort = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(230*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 50,
                                 65534, m_configuration->DefaultPort());
  wxStaticText *dp = new wxStaticText(panel, -1, _("Default port for communication with wxMaxima:"));
  sizer->Add(dp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(m_defaultPort, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  sizer->Add(10, 10);
  sizer->Add(10, 10);
  vsizer->Add(sizer);
  wxStaticText *ap = new wxStaticText(panel, -1, _("Additional parameters for maxima"));
  sizer2->Add(ap, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(10, 10);
  wxStaticText *ap1 = new wxStaticText(panel, -1, _("Examples:"));
  sizer2->Add(ap1, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(10, 10);
  sizer2->Add(new wxStaticText(panel, -1, _("      -l <name>")), 0,
              wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(new wxStaticText(panel, -1, _("choose a lisp maxima was compiled with")),
              0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(new wxStaticText(panel, -1, _("      -u <number>")), 0,
              wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(new wxStaticText(panel, -1, _("choose between installed maxima versions")),
              0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxString sbclMemoryParameter;
#ifdef __WXMSW__
  sbclMemoryParameter = _("      -X \"--dynamic-space-size <int>\"");
#else
  sbclMemoryParameter = _("      -X '--dynamic-space-size <int>'");
#endif
  sizer2->Add(new wxStaticText(panel, -1, sbclMemoryParameter), 0,
              wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer2->Add(new wxStaticText(panel, -1, _("tell sbcl to use <int>Mbytes of heap")),
              0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vsizer->Add(sizer2);
  m_additionalParameters = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(600*GetContentScaleFactor(), -1), wxTE_RICH);
  vsizer->Add(m_additionalParameters, 0, wxALL, 0);

  vsizer->Add(10, 10);

  m_abortOnError = new wxCheckBox(panel, -1, _("Abort evaluation on error"));
  vsizer->Add(m_abortOnError, 0, wxALL, 5);

  panel->SetSizerAndFit(vsizer);

  m_restartOnReEvaluation = new wxCheckBox(panel, -1, _("Start a new maxima for each re-evaluation"));
  vsizer->Add(m_restartOnReEvaluation, 0, wxALL, 5);
  panel->SetSizerAndFit(vsizer);

  return panel;
}

wxPanel *ConfigDialogue::CreateClipboardPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  Configuration *configuration = m_configuration;

  wxStaticText *descr = new wxStaticText(panel, -1, _("Additional clipboard formats to put on the clipboard on ordinary copy:"));
  vbox->Add(descr, 0, wxALL);

  m_copyBitmap = new wxCheckBox(panel, -1, _("Bitmap"));
  m_copyBitmap->SetValue(configuration->CopyBitmap());
  vbox->Add(m_copyBitmap, 0, wxALL, 5);

  m_copyMathML = new wxCheckBox(panel, -1, _("MathML description"));
  m_copyMathML->SetValue(configuration->CopyMathML());
  vbox->Add(m_copyMathML, 0, wxALL, 5);

  m_copyMathMLHTML = new wxCheckBox(panel, -1, _("MathML as HTML"));
  m_copyMathMLHTML->SetValue(configuration->CopyMathMLHTML());
  vbox->Add(m_copyMathMLHTML, 0, wxALL, 5);

  m_copyRTF = new wxCheckBox(panel, -1, _("RTF with OMML maths"));
  m_copyRTF->SetValue(configuration->CopyRTF());
  vbox->Add(m_copyRTF, 0, wxALL, 5);

  m_copySVG = new wxCheckBox(panel, -1, _("Scalable Vector Graphics (svg)"));
  m_copySVG->SetValue(configuration->CopySVG());
  vbox->Add(m_copySVG, 0, wxALL, 5);

  #if wxUSE_ENH_METAFILE
  m_copyEMF = new wxCheckBox(panel, -1, _("Enhanced meta file (emf)"));
  m_copyEMF->SetValue(configuration->CopyEMF());
  vbox->Add(m_copyEMF, 0, wxALL, 5);
  #endif

  panel->SetSizerAndFit(vbox);

  return panel;
}

wxPanel *ConfigDialogue::CreateStylePanel()
{
  Configuration *configuration = m_configuration;
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxStaticBox *fonts = new wxStaticBox(panel, -1, _("Fonts"));
  wxStaticBox *styles = new wxStaticBox(panel, -1, _("Styles"));

  wxFlexGridSizer *vsizer = new wxFlexGridSizer(4, 1, 5, 5);
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 2, 2, 2);
  wxStaticBoxSizer *sb_sizer_1 = new wxStaticBoxSizer(fonts, wxVERTICAL);
  wxStaticBoxSizer *sb_sizer_2 = new wxStaticBoxSizer(styles, wxVERTICAL);
  wxBoxSizer *hbox_sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *hbox_sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *hbox_sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *vbox_sizer = new wxBoxSizer(wxVERTICAL);

  wxStaticText *df = new wxStaticText(panel, -1, _("Documentation+Code font:"));
  m_getFont = new wxButton(panel, font_family, _("Choose font"), wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1));
  
  if (m_configuration->FontName() != wxEmptyString)
    m_getFont->SetLabel(m_configuration->FontName() + wxString::Format(wxT(" (%g)"), (double)m_configuration->GetDefaultFontSize()));

  m_mathFont = new wxStaticText(panel, -1, _("Math font:"));
  m_getMathFont = new wxButton(panel, button_mathFont, _("Choose font"), wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1));
  if (!m_configuration->MathFontName().IsEmpty())
    m_getMathFont->SetLabel(m_configuration->MathFontName() + wxString::Format(wxT(" (%g)"), (double)m_configuration->GetMathFontSize()));

  m_useJSMath = new wxCheckBox(panel, -1, _("Use jsMath fonts"));
  wxArrayString m_styleFor_choices;
  for(int i = 0; i < NUMBEROFSTYLES; i++)
    m_styleFor_choices.Add(m_configuration->m_styles[i].Name());
  m_styleFor = new wxListBox(panel, listbox_styleFor, wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), m_styleFor_choices,
                             wxLB_SINGLE);
  m_styleFor->Connect(wxEVT_LISTBOX,
                         wxCommandEventHandler(ConfigDialogue::OnStyleToEditChanged),
                         NULL, this);

  m_getStyleFont = new wxButton(panel, style_font_family, _("Choose font"), wxDefaultPosition, wxSize(150*GetContentScaleFactor(), -1));
  m_styleColor = new ColorPanel(this, panel, color_id, wxDefaultPosition, wxSize(150*GetContentScaleFactor(), 30),
                                wxSUNKEN_BORDER | wxFULL_REPAINT_ON_RESIZE);
  m_boldCB = new wxCheckBox(panel, checkbox_bold, _("Bold"));
  m_italicCB = new wxCheckBox(panel, checkbox_italic, _("Italic"));
  m_underlinedCB = new wxCheckBox(panel, checkbox_underlined, _("Underlined"));
  m_examplePanel = new ExamplePanel(panel, -1, wxDefaultPosition, wxSize(250*GetContentScaleFactor(), 60));
  m_loadStyle = new wxButton(panel, load_id, _("Load"));
  m_saveStyle = new wxButton(panel, save_id, _("Save"));

  grid_sizer_1->Add(df, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_mathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getMathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(10, 10);
  grid_sizer_1->Add(m_useJSMath, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  m_useUnicodeMaths = new wxCheckBox(panel, -1, _("Use unicode Math Symbols, if available"));
  m_useUnicodeMaths->SetValue(configuration->UseUnicodeMaths());
  grid_sizer_1->Add(10, 10);
  grid_sizer_1->Add(m_useUnicodeMaths, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  sb_sizer_1->Add(grid_sizer_1, 1, wxALL | wxEXPAND, 0);
  vsizer->Add(sb_sizer_1, 1, wxALL | wxEXPAND, 3);

  vbox_sizer->Add(m_styleColor, 0, wxALL | wxALIGN_CENTER, 5);
  vbox_sizer->Add(m_getStyleFont, 0, wxALL | wxALIGN_CENTER, 5);
  hbox_sizer_1->Add(m_boldCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_1->Add(m_italicCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_1->Add(m_underlinedCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vbox_sizer->Add(hbox_sizer_1, 1, wxALL | wxEXPAND, 0);
  vbox_sizer->Add(m_examplePanel, 0, wxALL | wxEXPAND, 5);
  hbox_sizer_2->Add(m_styleFor, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_2->Add(vbox_sizer, 1, wxALL | wxEXPAND, 0);
  sb_sizer_2->Add(hbox_sizer_2, 0, wxALL | wxEXPAND, 0);

  vsizer->Add(sb_sizer_2, 1, wxALL | wxEXPAND, 3);

  // load save buttons
  hbox_sizer_3->Add(m_loadStyle, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_3->Add(m_saveStyle, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vsizer->Add(hbox_sizer_3, 1, wxALIGN_RIGHT, 3);

  panel->SetSizer(vsizer);
  vsizer->Fit(panel);
  wxConfigBase *config = wxConfig::Get();
  int styleToEditNum = 0;
  config->Read(wxT("StyleToEdit"),&styleToEditNum);
  if(((unsigned) styleToEditNum >= m_styleFor->GetCount()) || (styleToEditNum < 0))
    styleToEditNum = 0;
  m_styleFor->SetSelection(styleToEditNum);
  wxCommandEvent dummy;
  dummy.SetInt(m_styleFor->GetSelection());
  OnChangeStyle(dummy);
  return panel;
}

void ConfigDialogue::OnStyleToEditChanged(wxCommandEvent &event)
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("StyleToEdit"), event.GetSelection());
  OnChangeStyle(event);
}

void ConfigDialogue::OnClose(wxCloseEvent &event)
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ConfigDialogTab"), m_notebook->GetSelection());
#if defined __WXOSX__
  EndModal(wxID_OK);
#else
  EndModal(wxID_CANCEL);
#endif
  event.Skip();
}

void ConfigDialogue::WriteSettings()
{
  wxArrayString out;
  wxConfigBase *config = wxConfig::Get();
  Configuration *configuration = m_configuration;
  configuration->SetAbortOnError(m_abortOnError->GetValue());
  configuration->RestartOnReEvaluation(m_restartOnReEvaluation->GetValue());
  configuration->MaximaUserLocation(m_maximaUserLocation->GetValue());
  configuration->AutodetectMaxima(m_autodetectMaxima->GetValue());
  configuration->MaximaParameters(m_additionalParameters->GetValue());
  config->Write(wxT("fontSize"), m_configuration->GetDefaultFontSize());
  config->Write(wxT("mathFontsize"), m_configuration->GetMathFontSize());
  configuration->SetMatchParens(m_matchParens->GetValue());
  configuration->ShowLength(m_showLength->GetSelection());
  configuration->SetAutosubscript_Num(m_autosubscript->GetSelection());
  config->Write(wxT("fixedFontTC"), m_fixedFontInTC->GetValue());
  configuration->OfferKnownAnswers(m_offerKnownAnswers->GetValue());
  configuration->SetChangeAsterisk(m_changeAsterisk->GetValue());
  configuration->HidemultiplicationSign(m_hidemultiplicationSign->GetValue());
  configuration->Latin2Greek(m_latin2Greek->GetValue());
  configuration->EnterEvaluates(m_enterEvaluates->GetValue());
  config->Write(wxT("saveUntitled"), m_saveUntitled->GetValue());
  configuration->SetOpenHCaret(m_openHCaret->GetValue());
  configuration->SetInsertAns(m_insertAns->GetValue());
  configuration->SetAutoIndent(m_autoIndent->GetValue());
  config->Write(wxT("cursorJump"), m_cursorJump->GetValue());
  configuration->HideBrackets(m_hideBrackets->GetValue());
  configuration->IndentMaths(m_indentMaths->GetValue());
  configuration->SetAutoWrap(m_autoWrap->GetSelection());
  config->Write(wxT("labelWidth"), m_labelWidth->GetValue());
  config->Write(wxT("undoLimit"), m_undoLimit->GetValue());
  config->Write(wxT("recentItems"), m_recentItems->GetValue());
  config->Write(wxT("bitmapScale"), m_bitmapScale->GetValue());
  configuration->PrintScale(m_printScale->GetValue());
  configuration->FixReorderedIndices(m_fixReorderedIndices->GetValue());
  config->Write(wxT("incrementalSearch"), m_incrementalSearch->GetValue());
  configuration->NotifyIfIdle(m_notifyIfIdle->GetValue());
  configuration->SetLabelChoice((Configuration::showLabels) m_showUserDefinedLabels->GetSelection());
  configuration->DefaultPort(m_defaultPort->GetValue());
  config->Write(wxT("AUI/savePanes"), m_savePanes->GetValue());
  configuration->UseSVG(m_usesvg->GetValue());
  configuration->AntiAliasLines(m_antialiasLines->GetValue());
  config->Write(wxT("DefaultFramerate"), m_defaultFramerate->GetValue());
  configuration->MaxGnuplotMegabytes(m_maxGnuplotMegabytes->GetValue());
  config->Write(wxT("defaultPlotWidth"), m_defaultPlotWidth->GetValue());
  config->Write(wxT("defaultPlotHeight"), m_defaultPlotHeight->GetValue());
  configuration->SetDisplayedDigits(m_displayedDigits->GetValue());
  config->Write(wxT("AnimateLaTeX"), m_AnimateLaTeX->GetValue());
  config->Write(wxT("TeXExponentsAfterSubscript"), m_TeXExponentsAfterSubscript->GetValue());
  config->Write(wxT("usePartialForDiff"), m_usePartialForDiff->GetValue());
  config->Write(wxT("wrapLatexMath"), m_wrapLatexMath->GetValue());
  config->Write(wxT("exportContainsWXMX"), m_exportContainsWXMX->GetValue());
  configuration->PrintBrackets(m_printBrackets->GetValue());
  configuration->HTMLequationFormat((Configuration::htmlExportFormat) m_exportWithMathJAX->GetSelection());
  config->Write(wxT("usejsmath"), m_useJSMath->GetValue());
  configuration->UseUnicodeMaths(m_useUnicodeMaths->GetValue());
  config->Write(wxT("keepPercent"), m_keepPercentWithSpecials->GetValue());
  config->Write(wxT("texPreamble"), m_texPreamble->GetValue());
  configuration->AutoSaveAsTempFile(!m_autoSave->GetValue());
  configuration->Documentclass(m_documentclass->GetValue());
  configuration->DocumentclassOptions(m_documentclassOptions->GetValue());
  configuration->MathJaXURL(m_mathJaxURL->GetValue());
  configuration->MathJaXURL_UseUser(m_noAutodetectMathJaX->GetValue());
  {
    configuration->SetLanguage((int) wxLANGUAGE_DEFAULT);
    long i = 0;
    for(Languages::const_iterator it = m_languages.begin(); it != m_languages.end(); ++it )
    {
      if(i == m_language->GetSelection())
        configuration->SetLanguage(it->second);
      ++i;
    }
  }
  configuration->SymbolPaneAdditionalChars(m_symbolPaneAdditionalChars->GetValue());
  
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
    if(!wxDirExists(startupDir))
    {
      wxLogMessage(
        wxString::Format(_("The directory %s with maxima's startup file doesn't exist. Trying to create it..."),startupDir.utf8_str()));
      wxMkdir(startupDir);
    }
      
    wxFileOutputStream output(m_startupFileName);
    if(output.IsOk())
    {
      wxTextOutputStream text(output);
      text << m_startupCommands->GetValue();
      text.Flush();
    }
  }

  {
    wxFileOutputStream output(m_wxStartupFileName);
    if(output.IsOk())
    {
      wxTextOutputStream text(output);
      text << m_wxStartupCommands->GetValue();
      text.Flush();
    }
  }
  config->Write(wxT("ConfigDialogTab"), m_notebook->GetSelection());
}

void ConfigDialogue::OnMpBrowse(wxCommandEvent&  WXUNUSED(event))
{
  wxConfigBase *config = wxConfig::Get();
  wxString dd;
  config->Read(wxT("maxima"), &dd);
  wxString file = wxFileSelector(_("Select Maxima program"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxEmptyString,
#if defined __WXMSW__
                                 _("Bat files (*.bat)|*.bat|All|*"),
#else
                                 _("All|*"),
#endif
                                 wxFD_OPEN);

  if (file.Length())
  {
    if (file.Right(8).Lower() == wxT("wxmaxima") || file.Right(12).Lower() == wxT("wxmaxima.exe"))
      LoggingMessageBox(_("Invalid entry for Maxima program.\n\nPlease enter the path to Maxima program again."),
                   _("Error"),
                   wxOK | wxICON_ERROR);
    else
      m_maximaUserLocation->SetValue(file);
  }
}

void ConfigDialogue::OnMathBrowse(wxCommandEvent&  WXUNUSED(event))
{
  wxFontInfo req;
#ifdef __WXMSW__
  req = wxFontInfo()
    .Family(wxFONTFAMILY_MODERN)
    .FaceName(wxT("Linux Libertine O"))
    .Style(wxFONTSTYLE_NORMAL);
  if (!FontCache::GetAFont(req).IsOk())
#endif
  req = wxFontInfo(m_configuration->GetMathFontSize())
          .Family(wxFONTFAMILY_DEFAULT)
          .Italic(false)
          .Light(false)
          .Underlined(false)
          .FaceName(m_configuration->MathFontName());
  if (!FontCache::GetAFont(req).IsOk())
    req = FontInfo::GetFor(*wxNORMAL_FONT);

  wxFont math = wxGetFontFromUser(this, FontCache::GetAFont(req));
  FontCache::Get().AddFont(math);

  if (math.Ok())
  {
    m_configuration->MathFontName(math.GetFaceName());
    m_configuration->SetMathFontSize(math.GetPointSize());
    math.SetPointSize(m_configuration->GetMathFontSize());
    m_getMathFont->SetLabel(m_configuration->MathFontName() + wxString::Format(wxT(" (%g)"), (double)m_configuration->GetMathFontSize()));
  }

  UpdateExample();
}

void ConfigDialogue::OnChangeFontFamily(wxCommandEvent &event)
{
  int fontsize = m_configuration->GetDefaultFontSize();
  wxString fontName;
  
  TextStyle st = static_cast<TextStyle>(m_styleFor->GetSelection());
  
  if (
    (st == TS_TEXT)          ||
    (st == TS_DEFAULT)       ||
    (st == TS_TITLE)         ||
    (st == TS_HEADING6)      ||
    (st == TS_HEADING5)      ||
    (st == TS_SUBSUBSECTION) ||
    (st == TS_SUBSECTION)    ||
    (st == TS_SECTION))
  {
    if (m_configuration->m_styles[st].FontSize() != 0)
      fontsize = m_configuration->m_styles[st].FontSize();
    fontName = m_configuration->m_styles[st].FontName();
  }
  else
    fontName = m_configuration->m_styles[TS_DEFAULT].FontName();

  auto req = wxFontInfo(fontsize)
          .Family(wxFONTFAMILY_DEFAULT).Italic(false)
          .Light(false)
          .Underlined(false).FaceName(fontName);

  wxFont font = FontCache::GetAFont(req);

  if (!font.IsOk())
  {
    fontName = wxT("Linux Libertine O");
    req.FaceName(fontName);
    font = FontCache::GetAFont(req);
  }

  if (!font.IsOk()) {
    FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
    font = FontCache::GetAFont(req);
  }

  font = wxGetFontFromUser(this, font);
  req = FontCache::AddAFont(font);
  
  if (font.IsOk())
  {
    if (event.GetId() == font_family)
    {
      m_configuration->m_styles[TS_DEFAULT].FontName(font.GetFaceName());
      m_configuration->FontName(font.GetFaceName());
      m_configuration->SetDefaultFontSize(wxMax(
                                            wxMin(
                                              font.GetPointSize(), MC_MAX_SIZE),
                                            MC_MIN_SIZE)
        );
      m_getFont->SetLabel(m_configuration->FontName() +
                          wxString::Format(wxT(" (%g)"), (double)m_configuration->GetDefaultFontSize()));
    }
    else
    {
      m_configuration->m_styles[st].FontName(font.GetFaceName());
      m_configuration->m_styles[st].FontSize(wxMax(MC_MIN_SIZE,font.GetPointSize()));
    }
  }
  UpdateExample();
}

void ConfigDialogue::OnChangeColor()
{
  TextStyle st = static_cast<TextStyle>(m_styleFor->GetSelection());
  wxColour col = wxGetColourFromUser(this, m_configuration->m_styles[st].Color());
  if (col.IsOk())
    m_configuration->m_styles[st].Color(col);
  UpdateExample();
}

void ConfigDialogue::OnChangeStyle(wxCommandEvent&  WXUNUSED(event))
{
  TextStyle st = static_cast<TextStyle>(m_styleFor->GetSelection());
  m_styleColor->SetColor(m_configuration->m_styles[st].Color());
  // MAGIC NUMBERS:
  // the positions of TEXT and TITLE style in the list.
  if (st >= TS_TEXT&& st <= TS_TITLE)
    m_getStyleFont->Enable(true);
  else
    m_getStyleFont->Enable(false);

  // Colors only
  if (st >= TS_TITLE)
  {
    if (st > TS_OUTDATED)
    {
      m_boldCB->Enable(false);
      m_italicCB->Enable(false);
      m_underlinedCB->Enable(false);
      m_boldCB->SetValue(false);
      m_italicCB->SetValue(false);
      m_underlinedCB->SetValue(false);
    }
    else
    {
      m_boldCB->SetValue(m_configuration->m_styles[st].Bold());
      m_italicCB->SetValue(m_configuration->m_styles[st].Italic());
      m_underlinedCB->SetValue(m_configuration->m_styles[st].Underlined());
    }
  }
  else
  {
    m_boldCB->Enable(true);
    m_italicCB->Enable(true);
    m_underlinedCB->Enable(true);
    m_boldCB->SetValue(m_configuration->m_styles[st].Bold());
    m_italicCB->SetValue(m_configuration->m_styles[st].Italic());
    m_underlinedCB->SetValue(m_configuration->m_styles[st].Underlined());
  }
  UpdateExample();
}

void ConfigDialogue::OnCheckbox(wxCommandEvent&  WXUNUSED(event))
{
  TextStyle st = static_cast<TextStyle>(m_styleFor->GetSelection());
  m_configuration->m_styles[st].Bold(m_boldCB->GetValue());
  m_configuration->m_styles[st].Italic(m_italicCB->GetValue());
  m_configuration->m_styles[st].Underlined(m_underlinedCB->GetValue());
  UpdateExample();
}

void ConfigDialogue::OnChangeWarning(wxCommandEvent&  WXUNUSED(event))
{
  LoggingMessageBox(_("Please restart wxMaxima for changes to take effect!"),
               _("Configuration warning"),
               wxOK | wxICON_WARNING);
  UpdateExample();
}

void ConfigDialogue::UpdateExample()
{
  TextStyle st = static_cast<TextStyle>(m_styleFor->GetSelection());

  m_styleColor->SetColor(m_configuration->m_styles[st].Color());

  wxColour color(m_configuration->m_styles[st].Color());
  wxString font(m_configuration->m_styles[TS_DEFAULT].FontName());

  if (st == TS_TEXT_BACKGROUND)
    color = m_configuration->m_styles[st].Color();

  int fontsize = m_configuration->GetDefaultFontSize();
  if (st == TS_TEXT || st == TS_HEADING5 || st == TS_HEADING6 ||
      st == TS_SUBSUBSECTION || st == TS_SUBSECTION ||
      st == TS_SECTION || st == TS_TITLE)
  {
    fontsize = m_configuration->m_styles[st].FontSize();
    m_configuration->m_styles[st].FontName();
    if (fontsize <= 0)
      fontsize = m_configuration->GetDefaultFontSize();
  }
  else if (st == TS_VARIABLE || st == TS_NUMBER || st == TS_FUNCTION ||
           st == TS_SPECIAL_CONSTANT)
  {
    fontsize = m_configuration->GetMathFontSize();
  }

  if (st == TS_TEXT_BACKGROUND)
  {
    m_examplePanel->SetFontSize(m_configuration->m_styles[st].FontSize());
    m_examplePanel->SetStyle(m_configuration->m_styles[st].Color(),
                             m_configuration->m_styles[st].Italic(),
                             m_configuration->m_styles[st].Bold(),
                             m_configuration->m_styles[st].Underlined(),
                             m_configuration->m_styles[st].FontName());
  }
  else
  {
    m_examplePanel->SetFontSize(fontsize);
    m_examplePanel->SetStyle(color,
                             m_configuration->m_styles[st].Italic(),
                             m_configuration->m_styles[st].Bold(),
                             m_configuration->m_styles[st].Underlined(),
                             m_configuration->m_styles[st].FontName());
  }

  if (st == TS_TEXT_BACKGROUND ||
      st == TS_TEXT)
  {
    if(m_examplePanel->GetBackgroundColour() != m_configuration->m_styles[TS_TEXT_BACKGROUND].Color())
      m_examplePanel->SetBackgroundColour(m_configuration->m_styles[TS_TEXT_BACKGROUND].Color());
  }
  else
  {
    if(m_examplePanel->GetBackgroundColour() != m_configuration->m_styles[TS_DOCUMENT_BACKGROUND].Color())
    m_examplePanel->SetBackgroundColour(m_configuration->m_styles[TS_DOCUMENT_BACKGROUND].Color());
  }

  m_examplePanel->Refresh();
}

void ConfigDialogue::OnTabChange(wxBookCtrlEvent &event)
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ConfigDialogTab"), event.GetSelection());
  UpdateExample();
}

void ConfigDialogue::LoadSave(wxCommandEvent &event)
{
  if (event.GetId() == save_id)
  {
    wxString file = wxFileSelector(_("Save style to file"),
                                   wxEmptyString, wxT("style.ini"), wxT("ini"),
                                   _("Config file (*.ini)|*.ini"),
                                   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
    if (file != wxEmptyString)
      m_configuration->WriteStyles(file);
  }
  else
  {
    wxString file = wxFileSelector(_("Load style from file"),
                                   wxEmptyString, wxT("style.ini"), wxT("ini"),
                                   _("Config file (*.ini)|*.ini"),
                                   wxFD_OPEN);
    if (file != wxEmptyString)
    {
      m_configuration->ReadStyles(file);
      wxCommandEvent dummy;
      dummy.SetInt(m_styleFor->GetSelection());
      OnChangeStyle(dummy);
    }
  }
}

ConfigDialogue::ColorPanel::ColorPanel(ConfigDialogue *conf, wxWindow *parent,
                                       int id, wxPoint pos, wxSize size, long style) :
  wxPanel(parent, id,
          pos, size,
          style),
  m_color(0, 0, 0)
{
  m_configDialogue = conf;
  SetBackgroundColour(m_color);
  SetBackgroundStyle(wxBG_STYLE_PAINT);
  Connect(wxEVT_LEFT_UP, wxMouseEventHandler(ConfigDialogue::ColorPanel::OnClick), NULL, this);
  Connect(wxEVT_PAINT, wxPaintEventHandler(ConfigDialogue::ColorPanel::OnPaint), NULL, this);
}

void ConfigDialogue::ColorPanel::OnClick(wxMouseEvent& WXUNUSED(event))
{
  m_configDialogue->OnChangeColor();
}

void ConfigDialogue::ColorPanel::OnPaint(wxPaintEvent &WXUNUSED(event))
{
  wxAutoBufferedPaintDC dc(this);
  if(!m_color.IsOk())
    m_color = *wxWHITE;
  
  wxColor backgroundColor(
    m_color.Red() * m_color.Alpha() / wxALPHA_OPAQUE,
    m_color.Green() * m_color.Alpha() / wxALPHA_OPAQUE,
    m_color.Blue() * m_color.Alpha() / wxALPHA_OPAQUE
    );
  dc.SetPen(*(wxThePenList->FindOrCreatePen(backgroundColor, 1, wxPENSTYLE_SOLID)));
  dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(backgroundColor)));
  int width;
  int height;
  GetClientSize(&width, &height);

  int columns = (width+11)  / 12;
  int rows    = (height+11) / 12;

  for(int x=0;x<columns;x++)
    for(int  y=0;y<rows;y++)
    {
      if(((x+y)&1) == 1)
        dc.DrawRectangle(x*12,y*12,12,12);
    }

  wxColor foregroundColor
    (
      m_color.Red()   * m_color.Alpha() / wxALPHA_OPAQUE + (wxALPHA_OPAQUE - m_color.Alpha()),
      m_color.Green() * m_color.Alpha() / wxALPHA_OPAQUE + (wxALPHA_OPAQUE - m_color.Alpha()),
      m_color.Blue()  * m_color.Alpha() / wxALPHA_OPAQUE + (wxALPHA_OPAQUE - m_color.Alpha())
      );
  dc.SetPen(*(wxThePenList->FindOrCreatePen(foregroundColor, 1, wxPENSTYLE_SOLID)));
  dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(foregroundColor)));
  GetClientSize(&width, &height);

  for(int x=0;x<columns;x++)
    for(int  y=0;y<rows;y++)
    {
      if(((x+y)&1) == 0)
        dc.DrawRectangle(x*12,y*12,12,12);
    }
}

void ConfigDialogue::ExamplePanel::OnPaint(wxPaintEvent& WXUNUSED(event))
{
  wxString example(_("Example text"));
  wxPaintDC dc(this);
  int panel_width, panel_height;
  int text_width, text_height;
  wxFontWeight bold = wxFONTWEIGHT_NORMAL;
  wxFontStyle italic = wxFONTSTYLE_NORMAL;
  bool underlined = false;

  GetClientSize(&panel_width, &panel_height);

  dc.SetTextForeground(m_fgColor);

  if (m_bold)
    bold = wxFONTWEIGHT_BOLD;
  if (m_italic)
    italic = wxFONTSTYLE_SLANT;
  if (m_underlined)
    underlined = true;

  auto req = wxFontInfo(m_size).Family(wxFONTFAMILY_MODERN)
               .Italic(italic).Bold(bold).Underlined(underlined).FaceName(m_font);
  wxFont font = FontCache::GetAFont(req);
  if (!font.IsOk())
  {
    FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
    font = FontCache::GetAFont(req);
  }

  if (font.IsOk())
    dc.SetFont(font);

  dc.GetTextExtent(example, &text_width, &text_height);

  dc.DrawText(example, (panel_width - text_width) / 2,
              (panel_height - text_height) / 2);
}
