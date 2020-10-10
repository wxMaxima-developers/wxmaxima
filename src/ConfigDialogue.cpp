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
#include "WrappingStaticText.h"
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
  m_imageList->Add(GetImage(wxT("edit-undo"),
                            view_refresh_svg_gz, view_refresh_svg_gz_len
                     ));

  Create(parent, wxID_ANY, _("wxMaxima configuration"),
         wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE);

#if defined(__WXMSW__)
  // Must be called before pages are added, otherwise wxWidgets dumps a warning to the console:
  // [...]\src\msw\window.cpp(594): 'SetFocus' failed with error 0x00000057 (The parameter is incorrect.).
  CreateButtons(wxOK | wxCANCEL);
#endif
  m_notebook = GetBookCtrl();

  m_notebook->SetImageList(m_imageList.get());

  m_notebook->AddPage(CreateWorksheetPanel(), _("Worksheet"), true, 0);
  m_notebook->AddPage(CreateMaximaPanel(), _("Maxima"), false, 1);
  m_notebook->AddPage(CreateStylePanel(), _("Style"), false, 2);
  m_notebook->AddPage(CreateExportPanel(), _("Export"), false, 3);
  m_notebook->AddPage(CreateOptionsPanel(), _("Options"), false, 4);
  m_notebook->AddPage(CreateClipboardPanel(), _("Copy"), false, 5);
  m_notebook->AddPage(CreateStartupPanel(), _("Startup commands"), false, 6);
  m_notebook->AddPage(CreateRevertToDefaultsPanel(), _("Revert all to defaults"), false, 7);

#if !defined(__WXMSW__) && !defined(__WXOSX__)
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

  SetCheckboxValues();

  Connect(wxEVT_CLOSE_WINDOW, wxCloseEventHandler(ConfigDialogue::OnClose),NULL, this);
  Connect(button_defaultFont, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnFontButton), NULL, this);
  Connect(button_mathFont, wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnFontButton), NULL, this);
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

void ConfigDialogue::SetCheckboxValues()
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
  m_getDefaultFont->SetToolTip(_("Font used for display in document."));
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
          _("Indent maths so all lines are in par with the first line that starts after the label.")
    );

  wxConfigBase *config = wxConfig::Get();
  wxString mp, mc, ib, mf;

  // The default values for all config items that will be used if there is no saved
  // configuration data for this item. 
  m_documentclass->SetValue(configuration->Documentclass());
  m_documentclassOptions->SetValue(configuration->DocumentclassOptions());
  m_mathJaxURL->SetValue(configuration->MathJaXURL_User());
  m_autodetectMathJaX->SetValue(!configuration->MathJaXURL_UseUser());
  m_noAutodetectMathJaX->SetValue(configuration->MathJaXURL_UseUser());
  m_texPreamble->SetValue(configuration->TexPreamble());
  m_autoSave->SetValue(!configuration->AutoSaveAsTempFile());

  m_maximaUserLocation->SetValue(configuration->MaximaUserLocation());
  wxCommandEvent dummy;
  MaximaLocationChanged(dummy);

  m_additionalParameters->SetValue(configuration->MaximaParameters());
  m_usesvg->SetValue(configuration->UseSVG());
  m_antialiasLines->SetValue(configuration->AntiAliasLines());

  m_TeXExponentsAfterSubscript->SetValue(configuration->TeXExponentsAfterSubscript());
  m_usePartialForDiff->SetValue(configuration->UsePartialForDiff());
  m_wrapLatexMath->SetValue(configuration->WrapLatexMath());
  m_exportContainsWXMX->SetValue(configuration->ExportContainsWXMX());
  m_printBrackets->SetValue(configuration->PrintBrackets());
  m_exportWithMathJAX->SetSelection(configuration->HTMLequationFormat());
  m_matchParens->SetValue(configuration->GetMatchParens());
  m_showLength->SetSelection(configuration->ShowLength());
  m_autosubscript->SetSelection(configuration->GetAutosubscript_Num());
  m_changeAsterisk->SetValue(configuration->GetChangeAsterisk());
  m_hidemultiplicationSign->SetValue(configuration->HidemultiplicationSign());
  m_latin2Greek->SetValue(configuration->Latin2Greek());
  m_enterEvaluates->SetValue(configuration->EnterEvaluates());
  m_saveUntitled->SetValue(configuration->SaveUntitled());
  m_openHCaret->SetValue(configuration->GetOpenHCaret());
  m_insertAns->SetValue(configuration->GetInsertAns());
  m_autoIndent->SetValue(configuration->GetAutoIndent());
  m_cursorJump->SetValue(configuration->CursorJump());
  m_hideBrackets->SetValue(configuration->HideBrackets());
  m_indentMaths->SetValue(configuration->IndentMaths());
  int val = 0;
  if (configuration->GetAutoWrap()) val = 1;
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
  m_defaultPlotWidth->SetValue(configuration->DefaultPlotWidth());
  m_defaultPlotHeight->SetValue(configuration->DefaultPlotHeight());
  m_displayedDigits->SetValue(configuration->GetDisplayedDigits());
  m_symbolPaneAdditionalChars->SetValue(configuration->SymbolPaneAdditionalChars());
  m_getStyleFont->Enable(GetSelectedStyle() >= TS_ASCIIMATHS && GetSelectedStyle() <= TS_TITLE);
  m_showUserDefinedLabels->SetSelection(configuration->GetLabelChoice());
  unsigned int i = 0;
  // First set the language to "default".
  for(auto it : m_languages)
  {
    if(it.second == wxLANGUAGE_DEFAULT)
    {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }
  // Now try to set the language to the one from the config
  i = 0;
  int lang = m_configuration->GetLanguage();
  for(auto it : m_languages)
  {
    if(it.second == lang)
    {
      m_language->SetSelection(i);
      break;
    }
    ++i;
  }

  m_autoMathJaxURL->SetValue(m_configuration->MathJaXURL_Auto());
  m_autodetectHelpBrowser->SetValue(m_configuration->AutodetectHelpBrowser());
  m_noAutodetectHelpBrowser->SetValue(!m_configuration->AutodetectHelpBrowser());
  m_maximaUserLocation->SetValue(m_configuration->MaximaUserLocation());
  m_autodetectMaxima->SetValue(m_configuration->AutodetectMaxima());
  m_noAutodetectMaxima->SetValue(!m_configuration->AutodetectMaxima());
  m_helpBrowserUserLocation->SetValue(m_configuration->HelpBrowserUserLocation());
  m_defaultPort->SetValue(m_configuration->DefaultPort());
  m_copyBitmap->SetValue(m_configuration->CopyBitmap());
  m_copyMathML->SetValue(m_configuration->CopyMathML());
  m_copyMathMLHTML->SetValue(m_configuration->CopyMathMLHTML());
  m_copyRTF->SetValue(m_configuration->CopyRTF());
  m_copySVG->SetValue(m_configuration->CopySVG());
  #if wxUSE_ENH_METAFILE
  m_copyEMF->SetValue(m_configuration->CopyEMF());
  #endif

  UpdateButton(TS_DEFAULT);
  UpdateButton(TS_MATH);
  m_useUnicodeMaths->SetValue(m_configuration->UseUnicodeMaths());
}

wxPanel *ConfigDialogue::CreateWorksheetPanel()
{
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

  m_offerKnownAnswers = new wxCheckBox(panel, -1, _("Offer answers for questions known from previous runs"));
  vsizer->Add(m_offerKnownAnswers, 0, wxALL, 5);
  
  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel *ConfigDialogue::CreateRevertToDefaultsPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  WrappingStaticText *helpText1 = new WrappingStaticText(
    panel, -1,
    _("The buttons in this category reset wxMaxima's settings "
      "immediately, once they are pressed."));
  vsizer->Add(helpText1,
              wxSizerFlags().Border(wxALL,5).
              Expand()
    );
  wxButton *resetAllButton = new wxButton(panel, -1, _("Reset all GUI settings"));
  resetAllButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnResetAllToDefaults),
                          NULL, this);
  vsizer->Add(
    resetAllButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  wxButton *exportAllButton = new wxButton(panel, -1, _("Export all settings"));
  exportAllButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnExportAll),
                          NULL, this);
  vsizer->Add(
    exportAllButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  wxButton *importSettingsButton = new wxButton(panel, -1, _("Import settings"));
  importSettingsButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnImport),
                          NULL, this);
  vsizer->Add(
    importSettingsButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  wxButton *resetStylesButton = new wxButton(panel, -1, _("Reset the Style settings"));
  resetStylesButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnResetStyles),
                          NULL, this);
  vsizer->Add(
    resetStylesButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  wxButton *reloadAllButton = new wxButton(panel, -1, _("Reload all GUI settings from disc"));
  reloadAllButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnReloadAll),
                          NULL, this);
  vsizer->Add(
    reloadAllButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  wxButton *reloadStylesButton = new wxButton(panel, -1, _("Reload the style settings from disc"));
  reloadStylesButton->Connect(wxEVT_BUTTON,
                          wxCommandEventHandler(ConfigDialogue::OnReloadStyles),
                          NULL, this);
  vsizer->Add(
    reloadStylesButton,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  WrappingStaticText *helpText2 = new WrappingStaticText(
    panel, -1,
    _("While wxMaxima is controlled by the settings here "
      "Maxima as a command-line program isn't controlled by "
      "settings, except of the few wxMaxima settings that set "
      "the value of variables within Maxima: Instead Maxima "
      "is configured using environment variables, the Startup "
      "File and command-line switches."));
  vsizer->Add(
    helpText2,
    wxSizerFlags().Border(wxALL,5).
    Expand()
    );
  panel->SetSizerAndFit(vsizer);
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
  m_autoMathJaxURL = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350*GetContentScaleFactor(), wxDefaultSize.GetY()), wxTE_READONLY);
  grid_sizer->Add(m_autoMathJaxURL, 0, wxALL, 5);

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
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(11, 1, 0, 0);

  wxFlexGridSizer *nameSizer = new wxFlexGridSizer(6, 3, 0, 0);
  nameSizer->Add(new wxStaticText(panel, -1, _("Maxima program:")),
                 wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(10, 10);
  nameSizer->Add(10, 10);
  m_autodetectMaxima = new wxRadioButton(panel, -1, _("Autodetect"), wxDefaultPosition,
                                         wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_autodetectMaxima, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(
    new wxTextCtrl(panel, -1, m_configuration->MaximaDefaultLocation(),
                   wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), wxTE_RICH|wxTE_READONLY));
  nameSizer->Add(10, 10);
  
  m_noAutodetectMaxima = new wxRadioButton(panel, -1, _("User specified"));
  nameSizer->Add(m_noAutodetectMaxima, wxSizerFlags().Expand().Border(wxALL, 0));
  m_maximaUserLocation = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), wxTE_RICH);
  m_maximaUserLocation->AutoCompleteFileNames();
  m_mpBrowse = new wxButton(panel, wxID_OPEN, _("Open"));
  m_mpBrowse->Connect(wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnMpBrowse), NULL, this);

  nameSizer->Add(m_maximaUserLocation, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(m_mpBrowse, wxSizerFlags().Expand().Border(wxALL, 0));
  m_maximaUserLocation->Connect(wxEVT_COMMAND_TEXT_UPDATED,
                           wxCommandEventHandler(ConfigDialogue::MaximaLocationChanged),
                           NULL, this);

  nameSizer->Add(new wxStaticText(panel, -1, _("Help browser:")),
                 wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(10, 10);
  nameSizer->Add(10, 10);
  m_autodetectHelpBrowser = new wxRadioButton(panel, -1, _("Autodetect"), wxDefaultPosition,
                                              wxDefaultSize, wxRB_GROUP);
  nameSizer->Add(m_autodetectHelpBrowser, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(10, 10);
  nameSizer->Add(10, 10);
  
  m_noAutodetectHelpBrowser= new wxRadioButton(panel, -1, _("User specified"));
  nameSizer->Add(m_noAutodetectHelpBrowser, wxSizerFlags().Expand().Border(wxALL, 0));
  m_helpBrowserUserLocation = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1), wxTE_RICH);
  m_helpBrowserUserLocation->AutoCompleteFileNames();
  wxButton *mpBrowse2 = new wxButton(panel, wxID_OPEN, _("Open"));
  mpBrowse2->Connect(wxEVT_BUTTON, wxCommandEventHandler(ConfigDialogue::OnHelpBrowserBrowse), NULL, this);

  nameSizer->Add(m_helpBrowserUserLocation, wxSizerFlags().Expand().Border(wxALL, 0));
  nameSizer->Add(mpBrowse2, wxSizerFlags().Expand().Border(wxALL, 0));

  vsizer->Add(nameSizer, wxSizerFlags().Expand().Border(wxALL, 0));

  m_defaultPort = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(230*GetContentScaleFactor(), -1), wxSP_ARROW_KEYS, 50,
                                 65534);
  
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
  if(m_configuration->LispType().Lower().Contains(wxT("sbcl")))
  {
    wxString sbclMemoryParameter1;
    wxString sbclMemoryParameter2;
#ifdef __WXMSW__
    sbclMemoryParameter1 = _("      -X \"--dynamic-space-size <int>\"");
    sbclMemoryParameter2 = _("      -X \"--control-stack-size <int>\"");
#else
    sbclMemoryParameter1 = _("      -X '--dynamic-space-size <int>'");
    sbclMemoryParameter2 = _("      -X '--control-stack-size <int>'");
#endif
    sizer2->Add(new wxStaticText(panel, -1, sbclMemoryParameter1), 0,
                wxALL | wxALIGN_CENTER_VERTICAL, 5);
    sizer2->Add(new wxStaticText(panel, -1, _("sbcl: use <int>Mbytes of heap")),
                0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
    sizer2->Add(new wxStaticText(panel, -1, sbclMemoryParameter2), 0,
                wxALL | wxALIGN_CENTER_VERTICAL, 5);
    sizer2->Add(new wxStaticText(panel, -1, _("sbcl: use <int>Mbytes of stack for function calls")),
                0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  }
  vsizer->Add(sizer2);
  m_additionalParameters = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(600*GetContentScaleFactor(), -1), wxTE_RICH);
  vsizer->Add(m_additionalParameters, 0, wxALL, 0);

  vsizer->Add(10, 10);

  m_abortOnError = new wxCheckBox(panel, -1, _("Abort evaluation on error"));
  vsizer->Add(m_abortOnError, 0, wxALL, 5);
  m_restartOnReEvaluation = new wxCheckBox(panel, -1, _("Start a new maxima for each re-evaluation"));
  vsizer->Add(m_restartOnReEvaluation, 0, wxALL, 5);
  m_maximaEnvVariables = new wxGrid(panel,-1);
  m_maximaEnvVariables->CreateGrid(0,2);
  m_maximaEnvVariables->BeginBatch();
  wxEnvVariableHashMap::const_iterator it;
  for (it = m_configuration->MaximaEnvVars().begin();
       it != m_configuration->MaximaEnvVars().end();
       ++it)
  {
    m_maximaEnvVariables->AppendRows(1);
    int row = m_maximaEnvVariables->GetNumberRows() - 1;
    m_maximaEnvVariables->SetCellValue(row,0,it->first);
    m_maximaEnvVariables->SetCellValue(row,1,it->second);
  }
    m_maximaEnvVariables->AppendRows(1);
  m_maximaEnvVariables->SetColLabelValue(0,_("Variable"));
  m_maximaEnvVariables->SetColLabelValue(1,_("Value"));
  m_maximaEnvVariables->AutoSize();
  m_maximaEnvVariables->EndBatch();
  m_maximaEnvVariables->Connect(wxEVT_GRID_CELL_CHANGED,
                                wxGridEventHandler(ConfigDialogue::OnChangeMaximaEnvVar),
                                NULL, this);
  m_maximaEnvVariables->Connect(wxEVT_GRID_CELL_RIGHT_CLICK,
                                wxGridEventHandler(ConfigDialogue::OnMaximaEnvRightClick),
                                NULL, this);
  
  vsizer->Add(new wxStaticText(panel, -1,
                             _("Environment variables for maxima")), wxSizerFlags().Expand());
  vsizer->Add(m_maximaEnvVariables, wxSizerFlags().Expand());
  panel->SetSizerAndFit(vsizer);

  return panel;
}

void ConfigDialogue::OnMaximaEnvRightClick(wxGridEvent& event)
{
  m_maximaEmvRightClickRow = event.GetRow();
  if((event.GetCol() == 0) && (event.GetRow() >= 0) &&
     m_maximaEnvVariables->GetCellValue(event.GetRow(),0).IsEmpty()
    )
  {
    std::unique_ptr<wxMenu> popupMenu(new wxMenu());
    popupMenu->Append(MAXIMA_DEFAULT_LISP, wxT("MAXIMA_DEFAULT_LISP"));
    popupMenu->Append(MAXIMA_IMAGESDIR, wxT("MAXIMA_IMAGESDIR"));
    popupMenu->Append(MAXIMA_USERDIR, wxT("MAXIMA_USERDIR"));
    popupMenu->Append(MAXIMA_TEMPDIR, wxT("MAXIMA_TEMPDIR"));
    popupMenu->Append(MAXIMA_OBJDIR, wxT("MAXIMA_OBJDIR"));
    popupMenu->Append(MAXIMA_DOC_PREFIX, wxT("MAXIMA_DOC_PREFIX"));
    popupMenu->Append(HOME, wxT("HOME"));
    popupMenu->Append(GCL_GC_PAGE_THRESH, wxT("GCL_GC_PAGE_THRESH"));
    popupMenu->Append(GCL_GC_ALLOC_MIN, wxT("GCL_GC_ALLOC_MIN"));
    popupMenu->Append(GCL_GC_PAGE_MAX, wxT("GCL_GC_PAGE_MAX"));
    popupMenu->Append(GCL_MEM_MULTIPLE, wxT("GCL_MEM_MULTIPLE"));
    popupMenu->Append(GCL_MULTIPROCESS_MEMORY_POOL, wxT("GCL_MULTIPROCESS_MEMORY_POOL"));
    popupMenu->Append(LANG, wxT("LANG"));
    Connect(wxEVT_MENU,
            wxCommandEventHandler(ConfigDialogue::OnNewEnvMenu), NULL, this);
    PopupMenu(&*popupMenu);
  }
}

void ConfigDialogue::OnNewEnvMenu(wxCommandEvent &event)
{
  if(m_maximaEmvRightClickRow>=m_maximaEnvVariables->GetNumberRows())
    return;
  
  switch(event.GetId())
  {
  case MAXIMA_DEFAULT_LISP:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_DEFAULT_LISP");
    break;
  case MAXIMA_IMAGESDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_IMAGESDIR");
    break;
  case MAXIMA_USERDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_USERDIR");
    break;
  case MAXIMA_DIRECTORY:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_DIRECTORY");
    break;
  case GCL_GC_PAGE_THRESH:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"GCL_GC_PAGE_THRESH");
    break;
  case GCL_GC_ALLOC_MIN:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"GCL_GC_ALLOC_MIN");
    break;
  case GCL_GC_PAGE_MAX:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"GCL_GC_PAGE_MAX");
    break;
  case GCL_MEM_MULTIPLE:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"GCL_MEM_MULTIPLE");
    break;
  case GCL_MULTIPROCESS_MEMORY_POOL:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"GCL_MULTIPROCESS_MEMORY_POOL");
    break;
  case LANG:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"LANG");
    break;
  case HOME:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"HOME");
    break;
  case MAXIMA_TEMPDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_TEMPDIR");
    break;
  case MAXIMA_OBJDIR:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_OBJDIR");
    break;
  case MAXIMA_DOC_PREFIX:
    m_maximaEnvVariables->SetCellValue(m_maximaEmvRightClickRow,0,"MAXIMA_DOC_PREFIX");
    break;
  }
}

void ConfigDialogue::OnChangeMaximaEnvVar(wxGridEvent& event)
{
  //  Make sure we have exactly one empty row the user can add variables to
  for(int row=m_maximaEnvVariables->GetNumberRows()-2;row>=0;row--)
  {
    if(
      (m_maximaEnvVariables->GetCellValue(row,0).Trim(true).Trim(false).IsEmpty()) &&
      (m_maximaEnvVariables->GetCellValue(row,1).Trim(true).Trim(false).IsEmpty())
      )
      m_maximaEnvVariables->DeleteRows(row,1);
  }
  int row = m_maximaEnvVariables->GetNumberRows() - 1;
  if((!m_maximaEnvVariables->GetCellValue(row,0).Trim(true).Trim(false).IsEmpty()) &&
     (!m_maximaEnvVariables->GetCellValue(row,1).Trim(true).Trim(false).IsEmpty()))
    m_maximaEnvVariables->AppendRows(1);
  m_maximaEnvVariables->GetParent()->Layout();
}

wxPanel *ConfigDialogue::CreateClipboardPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  wxStaticText *descr = new wxStaticText(panel, -1, _("Additional clipboard formats to put on the clipboard on ordinary copy:"));
  vbox->Add(descr, 0, wxALL);
  m_copyBitmap = new wxCheckBox(panel, -1, _("Bitmap"));
  vbox->Add(m_copyBitmap, 0, wxALL, 5);

  m_copyMathML = new wxCheckBox(panel, -1, _("MathML description"));
  vbox->Add(m_copyMathML, 0, wxALL, 5);

  m_copyMathMLHTML = new wxCheckBox(panel, -1, _("MathML as HTML"));
  vbox->Add(m_copyMathMLHTML, 0, wxALL, 5);

  m_copyRTF = new wxCheckBox(panel, -1, _("RTF with OMML maths"));
  vbox->Add(m_copyRTF, 0, wxALL, 5);

  m_copySVG = new wxCheckBox(panel, -1, _("Scalable Vector Graphics (svg)"));
  vbox->Add(m_copySVG, 0, wxALL, 5);

  #if wxUSE_ENH_METAFILE
  m_copyEMF = new wxCheckBox(panel, -1, _("Enhanced meta file (emf)"));
  vbox->Add(m_copyEMF, 0, wxALL, 5);
  #endif

  panel->SetSizerAndFit(vbox);

  return panel;
}

TextStyle ConfigDialogue::StyleForListIndex(int index)
{
  if (index == 0)
    return TS_DEFAULT;
  if (index == 1)
    return TS_MATH;
  return TextStyle(index - 1);
}

int ConfigDialogue::StyleListIndexForStyle(TextStyle style)
{
  if (style == TS_DEFAULT)
    return 0;
  if (style == TS_MATH)
    return 1;
  return style + 1;
}

TextStyle ConfigDialogue::GetSelectedStyle() const
{
  return StyleForListIndex(m_styleFor->GetSelection());
}

void ConfigDialogue::UpdateButton(TextStyle const st)
{
  Style const style = m_configuration->m_styles[st];
  wxButton *fontButton = {};
  if (st == TS_DEFAULT)
    fontButton = m_getDefaultFont;
  else if (st == TS_MATH)
    fontButton = m_getMathFont;
  if (fontButton)
    fontButton->SetLabel(wxString::Format(wxT("%s (%g)"),
                                          style.GetFontName().GetAsString(),
                                          style.GetFontSize().Get()));
}

wxPanel *ConfigDialogue::CreateStylePanel()
{
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

  auto *defaultFontLabel = new wxStaticText(panel, -1, _("Documentation+Code font:"));
  m_getDefaultFont = new wxButton(panel, button_defaultFont, _("Choose font"), wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1));
  
  auto *mathFontLabel = new wxStaticText(panel, -1, _("Math font:"));
  m_getMathFont = new wxButton(panel, button_mathFont, _("Choose font"), wxDefaultPosition, wxSize(250*GetContentScaleFactor(), -1));

  wxArrayString m_styleFor_choices;
  for (int i = 0; i < NUMBEROFSTYLES; i++)
  {
    auto style = StyleForListIndex(i);
    m_styleFor_choices.Add(m_configuration->GetStyleName(style));
  }
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

  grid_sizer_1->Add(defaultFontLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getDefaultFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(mathFontLabel, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getMathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(10, 10);
  m_useUnicodeMaths = new wxCheckBox(panel, -1, _("Use unicode Math Symbols, if available"));
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
  config->Read(wxT("StyleToEdit"), &styleToEditNum);
  if (styleToEditNum >= TextStyle::NUMBEROFSTYLES || styleToEditNum < 0)
    styleToEditNum = 0;
  m_styleFor->SetSelection(StyleListIndexForStyle(TextStyle(styleToEditNum)));
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
  return panel;
}

void ConfigDialogue::OnStyleToEditChanged(wxCommandEvent &event)
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("StyleToEdit"), int(GetSelectedStyle()));
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

  configuration->m_maximaEnvVars.clear();
    for(int row=m_maximaEnvVariables->GetNumberRows()-1;row>=0;row--)
  {
    if(!m_maximaEnvVariables->GetCellValue(row,0).Trim(true).Trim(false).IsEmpty())
      configuration->m_maximaEnvVars[m_maximaEnvVariables->GetCellValue(row,0)] =
        m_maximaEnvVariables->GetCellValue(row,1);
  }
  
  configuration->SetAbortOnError(m_abortOnError->GetValue());
  configuration->RestartOnReEvaluation(m_restartOnReEvaluation->GetValue());
  configuration->MaximaUserLocation(m_maximaUserLocation->GetValue());
  configuration->AutodetectMaxima(m_autodetectMaxima->GetValue());
  configuration->HelpBrowserUserLocation(m_helpBrowserUserLocation->GetValue());
  configuration->AutodetectHelpBrowser(m_autodetectHelpBrowser->GetValue());
  configuration->MaximaParameters(m_additionalParameters->GetValue());
  configuration->SetMatchParens(m_matchParens->GetValue());
  configuration->ShowLength(m_showLength->GetSelection());
  configuration->SetAutosubscript_Num(m_autosubscript->GetSelection());
  configuration->FixedFontInTextControls(m_fixedFontInTC->GetValue());
  configuration->OfferKnownAnswers(m_offerKnownAnswers->GetValue());
  configuration->SetChangeAsterisk(m_changeAsterisk->GetValue());
  configuration->HidemultiplicationSign(m_hidemultiplicationSign->GetValue());
  configuration->Latin2Greek(m_latin2Greek->GetValue());
  configuration->EnterEvaluates(m_enterEvaluates->GetValue());
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
  configuration->SetLabelChoice((Configuration::showLabels) m_showUserDefinedLabels->GetSelection());
  configuration->DefaultPort(m_defaultPort->GetValue());
  configuration->UseSVG(m_usesvg->GetValue());
  configuration->AntiAliasLines(m_antialiasLines->GetValue());
  configuration->DefaultFramerate(m_defaultFramerate->GetValue());
  configuration->MaxGnuplotMegabytes(m_maxGnuplotMegabytes->GetValue());
  configuration->DefaultPlotWidth(m_defaultPlotWidth->GetValue());
  configuration->DefaultPlotHeight(m_defaultPlotHeight->GetValue());
  configuration->SetDisplayedDigits(m_displayedDigits->GetValue());
  configuration->TeXExponentsAfterSubscript(m_TeXExponentsAfterSubscript->GetValue());
  configuration->UsePartialForDiff(m_usePartialForDiff->GetValue());
  configuration->WrapLatexMath(m_wrapLatexMath->GetValue());
  configuration->ExportContainsWXMX(m_exportContainsWXMX->GetValue());
  configuration->PrintBrackets(m_printBrackets->GetValue());
  configuration->HTMLequationFormat((Configuration::htmlExportFormat) m_exportWithMathJAX->GetSelection());
  configuration->UseUnicodeMaths(m_useUnicodeMaths->GetValue());
  configuration->SetKeepPercent(m_keepPercentWithSpecials->GetValue());
  configuration->TexPreamble(m_texPreamble->GetValue());
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

void ConfigDialogue::OnHelpBrowserBrowse(wxCommandEvent&  WXUNUSED(event))
{
  wxString dd = m_configuration->HelpBrowserUserLocation();
  wxString file = wxFileSelector(_("Select help browser"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxEmptyString,
#if defined __WXMSW__
                                 _("Bat files (*.bat)|*.bat|Exe files (*.exe)||All|*"),
#else
                                 _("All|*"),
#endif
                                 wxFD_OPEN);

  if (file.Length())
    m_helpBrowserUserLocation->SetValue(file);
}

void ConfigDialogue::OnFontButton(wxCommandEvent &event)
{
  TextStyle const prevSt = GetSelectedStyle();
  TextStyle curSt = {};
  if (event.GetId() == button_defaultFont)
    curSt = TS_DEFAULT;
  else if (event.GetId() == button_mathFont)
    curSt = TS_MATH;

  m_styleFor->SetSelection(StyleListIndexForStyle(curSt));
  OnChangeFontFamily(event);
  m_styleFor->SetSelection(StyleListIndexForStyle(prevSt));
}

void ConfigDialogue::OnChangeFontFamily(wxCommandEvent &WXUNUSED(event))
{
  TextStyle const st = GetSelectedStyle();
  Style style = m_configuration->m_styles[st];
  style.ResolveToFont();
  
  if (!style.IsFontOk())
    style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);

  auto userFont = wxGetFontFromUser(
      this, style.GetFont(),
      wxString::Format(_("Choose the %s Font"), m_configuration->GetStyleName(st)));
  if (!userFont.IsOk())
    return;

  style.SetFromFont(userFont);
  m_configuration->m_styles[st].FontName(style.GetFontName());
  m_configuration->m_styles[st].FontSize(style.GetFontSize());

  UpdateButton(st);
  UpdateExample();
}

void ConfigDialogue::OnChangeColor()
{
  TextStyle const st = GetSelectedStyle();
  wxColour col = wxGetColourFromUser(this, m_configuration->m_styles[st].GetColor());
  if (col.IsOk())
    m_configuration->m_styles[st].SetColor(col);
  UpdateExample();
}

void ConfigDialogue::OnChangeStyle(wxCommandEvent& WXUNUSED(event))
{
  auto const st = GetSelectedStyle();
  m_styleColor->SetColor(m_configuration->m_styles[st].GetColor());

  // MAGIC NUMBERS:
  // the positions of TEXT and TITLE style in the list.  
  m_getStyleFont->Enable(st >= TS_ASCIIMATHS && st <= TS_TITLE);

  if (st <= TS_TITLE || st == TS_MATH)
  {
    // Text styles with adjustable bold/italic/underline
    m_boldCB->Enable(true);
    m_italicCB->Enable(true);
    m_underlinedCB->Enable(true);
    m_boldCB->SetValue(m_configuration->m_styles[st].IsBold());
    m_italicCB->SetValue(m_configuration->m_styles[st].IsItalic());
    m_underlinedCB->SetValue(m_configuration->m_styles[st].IsUnderlined());
  }
  else
  {
    // Background color only
    m_boldCB->Enable(false);
    m_italicCB->Enable(false);
    m_underlinedCB->Enable(false);
    m_boldCB->SetValue(false);
    m_italicCB->SetValue(false);
    m_underlinedCB->SetValue(false);
  }

  UpdateExample();
}


void ConfigDialogue::OnExportAll(wxCommandEvent&  WXUNUSED(event))
{
  wxString file = wxFileSelector(_("Save config to file"),
                                 wxEmptyString, wxT("style.ini"), wxT("ini"),
                                 _("Config file (*.ini)|*.ini"),
                                 wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
  if (file != wxEmptyString)
  {
    WriteSettings();
    m_configuration->WriteSettings(file);
  }
}

void ConfigDialogue::OnImport(wxCommandEvent&  WXUNUSED(event))
{
  wxString file = wxFileSelector(_("Read config to file"),
                                 wxEmptyString, wxT("style.ini"), wxT("ini"),
                                 _("Config file (*.ini)|*.ini"),
                                 wxFD_OPEN | wxFD_FILE_MUST_EXIST);
  if (file != wxEmptyString)
  {
    wxFileInputStream str(file);
    wxConfigBase *src = new wxFileConfig(str);
    if(src)
    {
      wxString str;
      long dummy;
      // first enum all entries
      bool bCont = src->GetFirstEntry(str, dummy);
      if(bCont)
        CopyConfig(src, wxConfigBase::Get());
      m_configuration->ReadStyles(file);
      SetCheckboxValues();
      wxCommandEvent dmmy;
      OnChangeStyle(dmmy);
    }
  }
}

void ConfigDialogue::CopyConfig(wxConfigBase *src, wxConfigBase *dst, wxString dir)
{
  bool bCont = true;
  long dummy;
  wxString str;
  src->SetPath(dir);
  dst->SetPath(dir);
  src->GetFirstEntry(str, dummy);
  while ( bCont )
  {
    switch(src->GetEntryType(str))
    {
    Type_String:
        wxLogMessage(wxString::Format(_("Copying config string \"%s\""),
                                      src->GetPath()+wxT("/")+str));
        dst->Write(str, src->ReadBool(str, wxEmptyString));
        break;
    Type_Boolean:
      wxLogMessage(wxString::Format(_("Copying config bool \"%s\""),
                                    src->GetPath()+wxT("/")+str));
      dst->Write(str, src->ReadBool(str, false));
      break;
    Type_Integer:
      wxLogMessage(wxString::Format(_("Copying config int \"%s\""),
                                    src->GetPath()+wxT("/")+str));
      dst->Write(str, src->ReadLong(str, 0));
      break;
    Type_Float:
      wxLogMessage(wxString::Format(_("Copying config float \"%s\""),
                                    src->GetPath()+wxT("/")+str));
      dst->Write(str, src->ReadDouble(str, 0.0));
      break;
    default:
      wxLogMessage(wxString::Format(_("Config item \"%s\" was of an unknown type"),
                                    src->GetPath()+wxT("/")+str));
    }
    bCont = src->GetNextEntry(str, dummy);
  }

  bCont = src->GetFirstGroup(str, dummy);
  while ( bCont ) {
    CopyConfig(src, dst, str);
    bCont = src->GetNextGroup(str, dummy);
  }
  if(dir != "/")
  {
    src->SetPath("..");
    dst->SetPath("..");
  }
}

void ConfigDialogue::OnResetAllToDefaults(wxCommandEvent&  WXUNUSED(event))
{
  wxLogMessage(_("Resetting all configuration settings"));
  m_configuration->ResetAllToDefaults();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnResetStyles(wxCommandEvent&  WXUNUSED(event))
{
  wxLogMessage(_("Resetting all configuration settings"));
  m_configuration->InitStyles();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnReloadAll(wxCommandEvent&  WXUNUSED(event))
{
  wxLogMessage(_("Reloading the configuration from disc"));
  m_configuration->ReadConfig();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnReloadStyles(wxCommandEvent&  WXUNUSED(event))
{
  wxLogMessage(_("Reloading the styles from disc"));
  m_configuration->ReadStyles();
  SetCheckboxValues();
  wxCommandEvent dummy;
  OnChangeStyle(dummy);
}

void ConfigDialogue::OnCheckbox(wxCommandEvent&  WXUNUSED(event))
{
  TextStyle const st = GetSelectedStyle();
  m_configuration->m_styles[st].SetBold(m_boldCB->GetValue());
  m_configuration->m_styles[st].SetItalic(m_italicCB->GetValue());
  m_configuration->m_styles[st].SetUnderlined(m_underlinedCB->GetValue());
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
  TextStyle const st = GetSelectedStyle();
  auto style = m_configuration->m_styles[st];

  m_styleColor->SetColor(style.GetColor());
  if (st == TS_VARIABLE || st == TS_NUMBER || st == TS_FUNCTION ||
      st == TS_SPECIAL_CONSTANT)
    style.SetFontSize(m_configuration->GetMathFontSize());
  else if (m_configuration->GetFontSize(st).IsNull())
    style.SetFontSize(m_configuration->GetDefaultFontSize());

  m_examplePanel->SetStyle(style);

  if (st == TS_TEXT_BACKGROUND || st == TS_TEXT)
  {
    m_examplePanel->SetBackgroundColour(m_configuration->m_styles[TS_TEXT_BACKGROUND].GetColor());
  } else {
    m_examplePanel->SetBackgroundColour(m_configuration->m_styles[TS_DOCUMENT_BACKGROUND].GetColor());
  }
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

  GetClientSize(&panel_width, &panel_height);

  auto style = m_style;

  dc.SetTextForeground(style.GetColor());

  if (!style.IsFontOk())
    style.SetFontName({});

  // cppcheck-suppress duplicateCondition
  if (!style.IsFontOk())
  {
    style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);
    style.SetFontSize(m_style.GetFontSize());
  }

  if (style.IsFontOk())
    dc.SetFont(style.GetFont());

  dc.GetTextExtent(example, &text_width, &text_height);

  dc.DrawText(example, (panel_width - text_width) / 2,
              (panel_height - text_height) / 2);
}
