// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
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

/*\file

  The C code for ConfigDialogue, the preferences dialog.
*/

#include "ConfigDialogue.h"
#include "MathCell.h"
#include "Configuration.h"

#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/font.h>
#include <wx/fontdlg.h>
#include <wx/wfstream.h>
#include <wx/sstream.h>
#include <wx/colordlg.h>
#include <wx/settings.h>
#include "Dirstructure.h"

#define MAX(a, b) ((a)>(b) ? (a) : (b))
#define MIN(a, b) ((a)>(b) ? (b) : (a))

/*! The enum that chooses the language in the language drop-down menu. 

  \attention
  - Should match whatever is put in the m_language.
*/
const int langs[] =
        {
                wxLANGUAGE_DEFAULT,
                wxLANGUAGE_CATALAN,
                wxLANGUAGE_CHINESE_SIMPLIFIED,
                wxLANGUAGE_CHINESE_TRADITIONAL,
                wxLANGUAGE_CZECH,
                wxLANGUAGE_DANISH,
                wxLANGUAGE_ENGLISH,
                wxLANGUAGE_FINNISH,
                wxLANGUAGE_FRENCH,
                wxLANGUAGE_GALICIAN,
                wxLANGUAGE_GERMAN,
                wxLANGUAGE_GREEK,
                wxLANGUAGE_HUNGARIAN,
                wxLANGUAGE_ITALIAN,
                wxLANGUAGE_JAPANESE,
#if wxCHECK_VERSION(3, 0, 1)
                wxLANGUAGE_KABYLE,
#endif
                wxLANGUAGE_NORWEGIAN_BOKMAL,
                wxLANGUAGE_POLISH,
                wxLANGUAGE_PORTUGUESE_BRAZILIAN,
                wxLANGUAGE_RUSSIAN,
                wxLANGUAGE_SPANISH,
                wxLANGUAGE_TURKISH,
                wxLANGUAGE_UKRAINIAN
        };

#define LANGUAGE_NUMBER (long)(sizeof(langs)/(signed)sizeof(langs[1]))

int ConfigDialogue::GetImageWidth()
{
  // We want to scale the images according to the display's resolution.
  // But we want to do so in discrete steps as scaling bitmaps by odd
  // factors will add visible antialiassing to things that are clearly
  // meant to be sharp lines.
  int resolutionMultiplier = wxGetDisplayPPI().x / 72;
  int imgWidth = 32 * resolutionMultiplier;
  int width, height;
  wxDisplaySize(&width, &height);
  if (width <= 800)
    imgWidth = 32;
  if (imgWidth < 32)
    imgWidth = 32;
  return (imgWidth);
}

wxImage ConfigDialogue::GetImage(wxString name)
{
  Dirstructure dirstruct;
  wxImage img(dirstruct.ConfigArtDir() + wxT("/") + name);

  // We want to scale the images according to the display's resolution.
  // But we want to do so in discrete steps as scaling bitmaps by odd
  // factors will add visible antialiassing to things that are clearly
  // meant to be sharp lines.
  int imgWidth = GetImageWidth();
  if (imgWidth < 32)
    imgWidth = 32;

  double scaleFactor = (double) imgWidth / img.GetWidth();
  img.Rescale(imgWidth, img.GetHeight() * scaleFactor, wxIMAGE_QUALITY_HIGH);

  return img;
}


ConfigDialogue::ConfigDialogue(wxWindow *parent, Configuration *cfg)
{
  m_configuration = cfg;
#if defined __WXMAC__
  SetSheetStyle(wxPROPSHEET_BUTTONTOOLBOOK | wxPROPSHEET_SHRINKTOFIT);
#else
  SetSheetStyle(wxPROPSHEET_LISTBOOK);
#endif
  SetSheetInnerBorder(3);
  SetSheetOuterBorder(3);
  // Allow the property dialogue sheets to scroll, if they don't fit
  // on the screen.
  SetLayoutAdaptationMode(wxDIALOG_ADAPTATION_MODE_ENABLED);

  int imgWidth = GetImageWidth();
  wxSize imageSize(imgWidth, imgWidth);
  m_imageList = new wxImageList(imgWidth, imgWidth);
  m_imageList->Add(GetImage(wxT("editing.png")));
  m_imageList->Add(GetImage(wxT("maxima.png")));
  m_imageList->Add(GetImage(wxT("styles.png")));
  m_imageList->Add(GetImage(wxT("Document-export.png")));
  m_imageList->Add(GetImage(wxT("options.png")));

  Create(parent, wxID_ANY, _("wxMaxima configuration"),
         wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE);

  m_notebook = GetBookCtrl();

  m_notebook->SetImageList(m_imageList);

  m_notebook->AddPage(CreateWorksheetPanel(), _("Worksheet"), true, 0);
  m_notebook->AddPage(CreateMaximaPanel(), _("Maxima"), false, 1);
  m_notebook->AddPage(CreateStylePanel(), _("Style"), false, 2);
  m_notebook->AddPage(CreateExportPanel(), _("Export"), false, 3);
  m_notebook->AddPage(CreateOptionsPanel(), _("Options"), false, 4);
#ifndef __WXMAC__
  CreateButtons(wxOK | wxCANCEL);
#endif

  wxConfigBase *config = wxConfig::Get();
  int notebookTab = 0;
  config->Read(wxT("ConfigDialogTab"), &notebookTab);
  m_notebook->SetSelection(notebookTab);
  LayoutDialog();

  SetProperties();
  UpdateExample();
}

ConfigDialogue::~ConfigDialogue()
{
  wxDELETE(m_imageList);
  m_imageList = NULL;
}


void ConfigDialogue::MaximaLocationChanged(wxCommandEvent &unused)
{
  if (m_configuration->MaximaFound(m_maximaProgram->GetValue()))
  {
    m_mp->SetForegroundColour(
            wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT)
    );
  }
  else
  {
    m_mp->SetForegroundColour(*wxRED);
  }
}

void ConfigDialogue::SetProperties()
{
  Configuration *configuration = m_configuration;
  SetTitle(_("wxMaxima configuration"));

  m_showUserDefinedLabels->SetToolTip(
          _("Maxima assigns each command/equation an automatic label (which looks like %i1 or %o1). If a command begins with a descriptive name followed by a : wxMaxima will call the descriptive name an \"user-defined label\" instead. This selection now allows to tell wxMaxima if to show only automatic labels, automatic labels if there aren't user-defined ones or no label at all until an user-defined label can be found by wxMaxima's heuristics. If automatic labels are suppressed extra vertical space is added between equations in order to ease discerning which line starts a new equation and which one only continues the one from the last line."));
  m_abortOnError->SetToolTip(
          _("If multiple cells are evaluated in one go: Abort evaluation if wxMaxima detects that maxima has encountered any error."));
  m_openHCaret->SetToolTip("If this checkbox is set a new code cell is opened as soon as maxima requests data. If it isn't set a new code cell is opened in this case as soon as the user starts typing in code.");
  m_pollStdOut->SetToolTip(
          _("Once the local network link between maxima and wxMaxima has been established maxima has no reason to send any messages using the system's stdout stream so all this stream transport should be a greeting message; The lisp running maxima will send eventual error messages using the system's stderr stream instead. If this box is checked we will nonetheless watch maxima's stdout stream for messages."));
  m_restartOnReEvaluation->SetToolTip(
          _("Maxima provides no \"forget all\" command that flushes all settings a maxima session could make. wxMaxima therefore normally defaults to starting a fresh maxima process every time the worksheet is to be re-evaluated. As this needs a little bit of time this switch allows to disable this behavior."));
  m_maximaProgram->SetToolTip(_("Enter the path to the Maxima executable."));
  m_additionalParameters->SetToolTip(_("Additional parameters for Maxima"
                                               " (e.g. -l clisp)."));
  m_mathJaxURL->SetToolTip(_("The URL MathJaX.js should be downloaded from by our HTML export."));
  m_saveSize->SetToolTip(_("Save wxMaxima window size/position between sessions."));
  m_texPreamble->SetToolTip(_("Additional commands to be added to the preamble of LaTeX output for pdftex."));
  m_autoSaveInterval->SetToolTip(
          _("If this number of minutes has elapsed after the last save of the file, the file has been given a name (by opening or saving it) and the keyboard has been inactive for > 10 seconds the file is saved. If this number is zero the file isn't saved automatically at all."));
  m_uncomressedWXMX->SetToolTip(
          _("Don't compress the maxima input text and compress images individually: This enables version control systems like git and svn to effectively spot the differences."));
  m_defaultFramerate->SetToolTip(_("Define the default speed (in frames per second) animations are played back with."));
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
  m_exportContainsWXMX->SetToolTip(
          _("If this option is set the .wxmx source of the current file is copied to a place a link to is put into the result of an export."));
  m_printBrackets->SetToolTip(
          _("For each Text-, Sectioning or code cell wxMaxima can display a bracket showing the extend of the cell and allowing to fold it. This setting now tells if this bracket is to be printed, as well."));
  m_exportWithMathJAX->SetToolTip(
          _("MathJAX creates scalable High-Quality representations of 2D Maths that can be used for Drag-And-Drop and provides accessability options. The disadvantage of MathJAX is that it needs JavaScript and a little bit of time in order to typeset equations.\nMathML is much faster than MathJaX, if it is supported by the browser. But many MathML implementations tend to lack necessary features.\nBitmaps tend to need more band width than the other two options. They lack support for advanced features like drag-and-drop or accessibility. Also they have problems aligning and scaling with the rest of the text and might use fonts that don't match the rest of the document."));
  m_savePanes->SetToolTip(_("Save panes layout between sessions."));
  m_usepngCairo->SetToolTip(
          _("The pngCairo terminal offers much better graphics quality (antialiassing and additional line styles). But it will only produce plots if the gnuplot installed on the current system actually supports it."));
  m_matchParens->SetToolTip(
          _("Automatically insert matching parenthesis in text controls. Automatic highlighting of matching parenthesis can be suppressed by setting the respective color to match the background of ordinary text."));
  m_showLength->SetToolTip(_("Show long expressions in wxMaxima document."));
  m_autosubscript->SetToolTip(
          _("false=Don't generate subscripts\ntrue=Automatically convert underscores to subscript markers if the would-be subscript is a number or a single letter\nall=_ marks subscripts."));
  m_language->SetToolTip(_("Language used for wxMaxima GUI."));
#ifdef wxUSE_UNICODE
  m_symbolPaneAdditionalChars->SetToolTip(
          _("Symbols that are entered or copied here will appear in the symbols sidebar so they can be entered into the worksheet easily."));
#endif
  m_documentclass->SetToolTip(_("The document class LaTeX is instructed to use for our documents."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_getFont->SetToolTip(_("Font used for display in document."));
  m_getMathFont->SetToolTip(_("Font used for displaying math characters in document."));
  m_changeAsterisk->SetToolTip(_("Use centered dot and Minus, not Star and Hyphen"));
  m_defaultPort->SetToolTip(_("The default port used for communication between Maxima and wxMaxima."));
  m_undoLimit->SetToolTip(
          _("Save only this number of actions in the undo buffer. 0 means: save an infinite number of actions."));
  m_recentItems->SetToolTip(_("The number of recently opened files that is to be remembered."));
  m_incrementalSearch->SetToolTip(_("Start searching while the phrase to search for is still being typed."));
  m_notifyIfIdle->SetToolTip(_("Issue a notification if maxima finishes calculating while the wxMaxima window isn't in focus."));

  m_hideBrackets->SetToolTip(
          _("Hide the brackets that mark the extend of the worksheet cells at the worksheet's right side and that contain the \"hide\" button of the cell if the cells aren't active."));

#ifdef __WXMSW__
  m_wxcd->SetToolTip(_("Automatically change maxima's working directory to the one the current document is in: "
                       "This is necessary if the document uses File I/O relative to the current directory "
                       "but will make maxima 5.35 fail to find its own installation path when the current "
                       "document resides on a different drive than the maxima installation."));
#endif
  wxConfig *config = (wxConfig *) wxConfig::Get();
  wxString mp, mc, ib, mf;

  // The default values for all config items that will be used if there is no saved
  // configuration data for this item.
  bool savePanes = true, UncompressedWXMX = true;
  bool fixedFontTC = true, usejsmath = true, keepPercent = true, abortOnError = true, pollStdOut = false;
  bool enterEvaluates = false, saveUntitled = true,
          AnimateLaTeX = true, TeXExponentsAfterSubscript = false,
          usePartialForDiff = false,
          wrapLatexMath = true,
          exportContainsWXMX = false;
  int exportWithMathJAX = 0;
  bool cursorJump = true;

  int labelWidth = 4;
  int undoLimit = 0;
  int recentItems = 10;
  int showLength = 0;
  int autosubscript = 1;
  int bitmapScale = 3;
  bool incrementalSearch = true;
  int defaultFramerate = 2;
  wxString texPreamble = wxEmptyString;
  wxString documentclass = wxT("article");
#ifdef wxUSE_UNICODE
  wxString symbolPaneAdditionalChars = wxT("üØ");
  m_symbolPaneAdditionalChars->SetValue(symbolPaneAdditionalChars);
#endif
  int autoSaveInterval = 0;

#if defined (__WXMAC__)
  bool usepngCairo = false;
#else
  bool usepngCairo=true;
#endif


  int rs = 0;
  int lang = wxLANGUAGE_UNKNOWN;
  int panelSize = 1;

  config->Read(wxT("maxima"), &mp);
  config->Read(wxT("parameters"), &mc);
  config->Read(wxT("AUI/savePanes"), &savePanes);
  config->Read(wxT("usepngCairo"), &usepngCairo);
  config->Read(wxT("DefaultFramerate"), &defaultFramerate);
  int defaultPlotWidth = 600;

  config->Read(wxT("defaultPlotWidth"), &defaultPlotWidth);
  int defaultPlotHeight = 400;
  config->Read(wxT("defaultPlotHeight"), &defaultPlotHeight);
  config->Read(wxT("OptimizeForVersionControl"), &UncompressedWXMX);
  config->Read(wxT("AnimateLaTeX"), &AnimateLaTeX);
  config->Read(wxT("TeXExponentsAfterSubscript"), &TeXExponentsAfterSubscript);
  config->Read(wxT("usePartialForDiff"), &usePartialForDiff);
  config->Read(wxT("wrapLatexMath"), &wrapLatexMath);
  config->Read(wxT("exportContainsWXMX"), &exportContainsWXMX);
  config->Read(wxT("HTMLequationFormat"), &exportWithMathJAX);
  config->Read(wxT("pos-restore"), &rs);
  config->Read(wxT("showLength"), &showLength);
  config->Read(wxT("autosubscript"), &autosubscript);
  config->Read(wxT("language"), &lang);
  config->Read(wxT("documentclass"), &documentclass);
  config->Read(wxT("texPreamble"), &texPreamble);
  config->Read(wxT("autoSaveInterval"), &autoSaveInterval);
  config->Read(wxT("fixedFontTC"), &fixedFontTC);
  config->Read(wxT("panelSize"), &panelSize);
  config->Read(wxT("enterEvaluates"), &enterEvaluates);
  config->Read(wxT("saveUntitled"), &saveUntitled);
  config->Read(wxT("cursorJump"), &cursorJump);
  config->Read(wxT("labelWidth"), &labelWidth);
  config->Read(wxT("undoLimit"), &undoLimit);
  config->Read(wxT("recentItems"), &recentItems);
  config->Read(wxT("bitmapScale"), &bitmapScale);
  config->Read(wxT("incrementalSearch"), &incrementalSearch);
  config->Read(wxT("usejsmath"), &usejsmath);
  config->Read(wxT("keepPercent"), &keepPercent);
  config->Read(wxT("abortOnError"), &abortOnError);
  config->Read(wxT("pollStdOut"), &pollStdOut);
  unsigned int i = 0;
  for (i = 0; i < LANGUAGE_NUMBER; i++)
    if (langs[i] == lang)
      break;
  if (i < LANGUAGE_NUMBER)
    m_language->SetSelection(i);
  else
    m_language->SetSelection(0);

#ifdef wxUSE_UNICODE
  config->Read(wxT("symbolPaneAdditionalChars"), &symbolPaneAdditionalChars);
#endif

  m_documentclass->SetValue(documentclass);
  m_mathJaxURL->SetValue(configuration->MathJaXURL());
  m_texPreamble->SetValue(texPreamble);
  m_autoSaveInterval->SetValue(autoSaveInterval);

  m_maximaProgram->SetValue(configuration->MaximaLocation());
  wxCommandEvent dummy;
  MaximaLocationChanged(dummy);

  m_additionalParameters->SetValue(mc);
  if (rs == 1)
    m_saveSize->SetValue(true);
  else
    m_saveSize->SetValue(false);
  m_savePanes->SetValue(savePanes);
  m_usepngCairo->SetValue(usepngCairo);

  m_uncomressedWXMX->SetValue(UncompressedWXMX);
  m_AnimateLaTeX->SetValue(AnimateLaTeX);
  m_TeXExponentsAfterSubscript->SetValue(TeXExponentsAfterSubscript);
  m_usePartialForDiff->SetValue(usePartialForDiff);
  m_wrapLatexMath->SetValue(wrapLatexMath);
  m_exportContainsWXMX->SetValue(exportContainsWXMX);
  m_printBrackets->SetValue(configuration->PrintBrackets());
  m_exportWithMathJAX->SetSelection(exportWithMathJAX);
  m_matchParens->SetValue(configuration->GetMatchParens());
  m_showLength->SetSelection(showLength);
  m_autosubscript->SetSelection(autosubscript);
  m_changeAsterisk->SetValue(configuration->GetChangeAsterisk());
  m_enterEvaluates->SetValue(enterEvaluates);
  m_saveUntitled->SetValue(saveUntitled);
  m_openHCaret->SetValue(configuration->GetOpenHCaret());
  m_insertAns->SetValue(configuration->GetInsertAns());
  m_autoIndent->SetValue(configuration->GetAutoIndent());
  m_cursorJump->SetValue(cursorJump);
  m_hideBrackets->SetValue(configuration->HideBrackets());
  int val = 0;
  if (configuration->GetAutoWrap()) val = 1;
//  if(configuration->GetAutoWrapCode()) val = 2;
  m_autoWrap->SetSelection(val);
  m_labelWidth->SetValue(labelWidth);
  m_undoLimit->SetValue(undoLimit);
  m_recentItems->SetValue(recentItems);
  m_bitmapScale->SetValue(bitmapScale);
  m_fixReorderedIndices->SetValue(configuration->FixReorderedIndices());
  m_incrementalSearch->SetValue(incrementalSearch);
  m_notifyIfIdle->SetValue(configuration->NotifyIfIdle());
  m_fixedFontInTC->SetValue(fixedFontTC);
  m_useJSMath->SetValue(usejsmath);
  m_keepPercentWithSpecials->SetValue(keepPercent);
  m_abortOnError->SetValue(abortOnError);
  m_pollStdOut->SetValue(pollStdOut);
  m_restartOnReEvaluation->SetValue(configuration->RestartOnReEvaluation());
  m_defaultFramerate->SetValue(defaultFramerate);
  m_defaultPlotWidth->SetValue(defaultPlotWidth);
  m_defaultPlotHeight->SetValue(defaultPlotHeight);
  m_displayedDigits->SetValue(configuration->GetDisplayedDigits());
#ifdef wxUSE_UNICODE
  m_symbolPaneAdditionalChars->SetValue(symbolPaneAdditionalChars);
#endif
  m_getStyleFont->Enable(false);

  if (!wxFontEnumerator::IsValidFacename(CMEX10) ||
      !wxFontEnumerator::IsValidFacename(CMSY10) ||
      !wxFontEnumerator::IsValidFacename(CMR10)  ||
      !wxFontEnumerator::IsValidFacename(CMMI10) ||
      !wxFontEnumerator::IsValidFacename(CMTI10))
    m_useJSMath->Enable(false);

  ReadStyles();
}

wxPanel *ConfigDialogue::CreateWorksheetPanel()
{
  Configuration *configuration = m_configuration;
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(10, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(19, 1, 5, 5);

  wxStaticText *pw = new wxStaticText(panel, -1, _("Default plot size for new maxima sessions:"));
  wxBoxSizer *PlotWidthHbox = new wxBoxSizer(wxHORIZONTAL);
  m_defaultPlotWidth = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS,
                                      100, 16384);
  PlotWidthHbox->Add(m_defaultPlotWidth, 0, wxEXPAND, 0);
  wxStaticText *xx = new wxStaticText(panel, -1, _("x"));
  PlotWidthHbox->Add(xx, 0, wxALIGN_CENTER_VERTICAL, 0);
  m_defaultPlotHeight = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS,
                                       100, 16384);
  PlotWidthHbox->Add(m_defaultPlotHeight, 0, wxEXPAND, 0);
  //  plotWidth->SetSizerAndFit(PlotWidthHbox);
  grid_sizer->Add(pw, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(PlotWidthHbox, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

  wxStaticText *dd = new wxStaticText(panel, -1, _("Maximum displayed number of digits:"));
  m_displayedDigits = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 20,
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
  autosubscripts.Add(_("No"));
  autosubscripts.Add(_("Integers and single letters"));
  autosubscripts.Add(_("All variable names"));
  m_autosubscript = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, autosubscripts);
  grid_sizer->Add(m_autosubscript, 0, wxALL, 5);

  wxStaticText *lw = new wxStaticText(panel, -1, _("Label width:"));
  grid_sizer->Add(lw, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  m_labelWidth = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 3, 10);
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

  m_enterEvaluates = new wxCheckBox(panel, -1, _("Enter evaluates cells"));
  vsizer->Add(m_enterEvaluates, 0, wxALL, 5);

  m_openHCaret = new wxCheckBox(panel, -1, _("Open a cell when Maxima expects input"));
  vsizer->Add(m_openHCaret, 0, wxALL, 5);

  m_matchParens = new wxCheckBox(panel, -1, _("Match parenthesis in text controls"));
  vsizer->Add(m_matchParens, 0, wxALL, 5);

  m_changeAsterisk = new wxCheckBox(panel, -1, _("Use centered dot character for multiplication"));
  vsizer->Add(m_changeAsterisk, 0, wxALL, 5);

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

  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel *ConfigDialogue::CreateExportPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(6, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(17, 1, 5, 5);

  wxStaticText *dc = new wxStaticText(panel, -1, _("Documentclass for TeX export:"));
  m_documentclass = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350, wxDefaultSize.GetY()));
  grid_sizer->Add(dc, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_documentclass, 0, wxALL, 5);

  wxStaticText *tp = new wxStaticText(panel, -1, _("Additional lines for the TeX preamble:"));
  m_texPreamble = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350, 100),
                                 wxTE_MULTILINE | wxHSCROLL);
  grid_sizer->Add(tp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_texPreamble, 0, wxALL, 5);
  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);

  wxStaticText *mj = new wxStaticText(panel, -1, _("URL MathJaX.js lies at:"));
  m_mathJaxURL = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(350, wxDefaultSize.GetY()));
  grid_sizer->Add(mj, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_mathJaxURL, 0, wxALL, 5);

  wxStaticText *bs = new wxStaticText(panel, -1, _("Bitmap scale for export:"));
  m_bitmapScale = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 1, 3);
  grid_sizer->Add(bs, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_bitmapScale, 0, wxALL, 5);

  wxStaticText *mju = new wxStaticText(panel, -1, _("Export equations to HTML as:"));
  wxArrayString mathJaxChoices;
  mathJaxChoices.Add(_("TeX, interpreted by MathJaX"));
  mathJaxChoices.Add(_("Bitmaps"));
  mathJaxChoices.Add(_("MathML + MathJaX as Fill-In"));
  m_exportWithMathJAX = new wxChoice(panel, -1, wxDefaultPosition, wxDefaultSize, mathJaxChoices);
  grid_sizer->Add(mju, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_exportWithMathJAX, 0, wxALL, 5);

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

  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(6, 2, 5, 5);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(18, 1, 5, 5);

  wxStaticText *lang = new wxStaticText(panel, -1, _("Language:"));
  const wxString m_language_choices[] =
          {
                  _("(Use default language)"),
                  _("Catalan"),
                  _("Chinese Simplified"),
                  _("Chinese traditional"),
                  _("Czech"),
                  _("Danish"),
                  _("English"),
                  _("Finnish"),
                  _("French"),
                  _("Galician"),
                  _("German"),
                  _("Greek"),
                  _("Hungarian"),
                  _("Italian"),
                  _("Japanese"),
#if wxCHECK_VERSION(3, 0, 1)
                  _("Kabyle"),
#endif
                  _("Norwegian"),
                  _("Polish"),
                  _("Portuguese (Brazilian)"),
                  _("Russian"),
                  _("Spanish"),
                  _("Turkish"),
                  _("Ukrainian")
          };
  m_language = new wxChoice(panel, language_id, wxDefaultPosition, wxSize(230, -1), LANGUAGE_NUMBER,
                            m_language_choices);
  grid_sizer->Add(lang, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_language, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

#ifdef wxUSE_UNICODE
  wxStaticText *additionalSymbols = new wxStaticText(panel, -1, _("Additional symbols for the \"symbols\" sidebar:"));
  m_symbolPaneAdditionalChars = new wxTextCtrl(panel, -1);
  grid_sizer->Add(additionalSymbols, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_symbolPaneAdditionalChars, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
#endif

  wxStaticText *as = new wxStaticText(panel, -1, _("Autosave interval (minutes, 0 means: off):"));
  m_autoSaveInterval = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(230, -1), wxSP_ARROW_KEYS, 0,
                                      30);
  grid_sizer->Add(as, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_autoSaveInterval, 0, wxALL, 5);

  wxStaticText *ul = new wxStaticText(panel, -1, _("Undo limit (0 for none):"));
  m_undoLimit = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 0, 10000);
  grid_sizer->Add(ul, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_undoLimit, 0, wxALL, 5);

  wxStaticText *rf = new wxStaticText(panel, -1, _("Recent files list length:"));
  m_recentItems = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 5, 30);
  grid_sizer->Add(rf, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_recentItems, 0, wxALL, 5);

  wxStaticText *df = new wxStaticText(panel, -1, _("Default animation framerate:"));
  m_defaultFramerate = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(100, -1), wxSP_ARROW_KEYS, 1,
                                      200);
  grid_sizer->Add(df, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_defaultFramerate, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);


  m_saveSize = new wxCheckBox(panel, -1, _("Save wxMaxima window size/position"));
  vsizer->Add(m_saveSize, 0, wxALL, 5);

  m_savePanes = new wxCheckBox(panel, -1, _("Save panes layout"));
  vsizer->Add(m_savePanes, 0, wxALL, 5);

  m_usepngCairo = new wxCheckBox(panel, -1, _("Use cairo to improve plot quality."));
  vsizer->Add(m_usepngCairo, 0, wxALL, 5);

  m_uncomressedWXMX = new wxCheckBox(panel, -1, _("Optimize wxmx files for version control"));
  vsizer->Add(m_uncomressedWXMX, 0, wxALL, 5);

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

  wxFlexGridSizer *sizer = new wxFlexGridSizer(4, 2, 0, 0);
  wxFlexGridSizer *sizer2 = new wxFlexGridSizer(6, 2, 0, 0);
  wxFlexGridSizer *vsizer = new wxFlexGridSizer(9, 1, 0, 0);

  m_mp = new wxStaticText(panel, -1, _("Maxima program:"));
  m_maximaProgram = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  m_mpBrowse = new wxButton(panel, wxID_OPEN, _("Open"));
  sizer->Add(m_mp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(10, 10);
  sizer->Add(m_maximaProgram, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(m_mpBrowse, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  m_maximaProgram->Connect(wxEVT_COMMAND_TEXT_UPDATED,
                           wxCommandEventHandler(ConfigDialogue::MaximaLocationChanged),
                           NULL, this);

  int defaultPort = 4010;
  wxConfig::Get()->Read(wxT("defaultPort"), &defaultPort);
  m_defaultPort = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(230, -1), wxSP_ARROW_KEYS, 50,
                                 65534, defaultPort);
  m_defaultPort->SetValue(defaultPort);
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
  wxStaticText *ap2 = new wxStaticText(panel, -1, _("      -l <name>"));
  sizer2->Add(ap2, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxStaticText *ap3 = new wxStaticText(panel, -1, _("choose a lisp maxima was compiled with"));
  sizer2->Add(ap3, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxString sbclMemoryParameter;
#ifdef __WXMSW__
  sbclMemoryParameter = "      -X \"--dynamic-space-size <int>\"";
#else
  sbclMemoryParameter = "      -X '--dynamic-space-size <int>'";
#endif
  wxStaticText *ap4 = new wxStaticText(panel, -1, sbclMemoryParameter);
  sizer2->Add(ap4, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  wxStaticText *ap5 = new wxStaticText(panel, -1, _("tell sbcl to use <int>Mbytes of heap"));
  sizer2->Add(ap5, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vsizer->Add(sizer2);
  m_additionalParameters = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(600, -1), wxTE_RICH);
  vsizer->Add(m_additionalParameters, 0, wxALL, 0);

  vsizer->Add(10, 10);
#ifdef __WXMSW__
  bool wxcd = true;
  wxConfig::Get()->Read(wxT("wxcd"), &wxcd);
  m_wxcd = new wxCheckBox(panel, -1, _("maxima's pwd is path to document"));
  m_wxcd-> SetValue(wxcd);
  vsizer->Add(m_wxcd, 0, wxALL, 5);
  vsizer->Add(10, 10);
#endif

  m_abortOnError = new wxCheckBox(panel, -1, _("Abort evaluation on error"));
  vsizer->Add(m_abortOnError, 0, wxALL, 5);

  m_pollStdOut = new wxCheckBox(panel, -1, _("Debug: watch maxima's stdout stream"));
  vsizer->Add(m_pollStdOut, 0, wxALL, 5);
  panel->SetSizerAndFit(vsizer);

  m_restartOnReEvaluation = new wxCheckBox(panel, -1, _("Start a new maxima for each re-evaluation"));
  vsizer->Add(m_restartOnReEvaluation, 0, wxALL, 5);
  panel->SetSizerAndFit(vsizer);

  return panel;
}

wxPanel *ConfigDialogue::CreateStylePanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxStaticBox *fonts = new wxStaticBox(panel, -1, _("Fonts"));
  wxStaticBox *styles = new wxStaticBox(panel, -1, _("Styles"));

  wxFlexGridSizer *vsizer = new wxFlexGridSizer(3, 1, 5, 5);
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(3, 2, 2, 2);
  wxStaticBoxSizer *sb_sizer_1 = new wxStaticBoxSizer(fonts, wxVERTICAL);
  wxStaticBoxSizer *sb_sizer_2 = new wxStaticBoxSizer(styles, wxVERTICAL);
  wxBoxSizer *hbox_sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *hbox_sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *hbox_sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *vbox_sizer = new wxBoxSizer(wxVERTICAL);

  wxStaticText *df = new wxStaticText(panel, -1, _("Default font:"));
  m_getFont = new wxButton(panel, font_family, _("Choose font"), wxDefaultPosition, wxSize(250, -1));
  m_mathFont = new wxStaticText(panel, -1, _("Math font:"));
  m_getMathFont = new wxButton(panel, button_mathFont, _("Choose font"), wxDefaultPosition, wxSize(250, -1));
  m_useJSMath = new wxCheckBox(panel, -1, _("Use jsMath fonts"));
  const wxString m_styleFor_choices[] =
          {
                  _("Default"),
                  _("Variables"),
                  _("Numbers"),
                  _("Function names"),
                  _("Special constants"),
                  _("Greek constants"),
                  _("Strings"),
                  _("Maxima input"),
                  _("Input labels"),
                  _("Maxima questions"),
                  _("Output labels"),
                  _("User-defined labels"),
                  _("Highlight (dpart)"),
                  _("Maxima warnings"),
                  _("Text cell"),
                  _("Subsubsection cell"),
                  _("Subsection cell"),
                  _("Section cell"),
                  _("Title cell"),
                  _("Text cell background"),
                  _("Document background"),
                  _("Cell bracket"),
                  _("Active cell bracket"),
                  _("Cursor"),
                  _("Selection"),
                  _("Text equal to selection"),
                  _("Outdated cells"),
                  _("Code highlighting: Variables"),
                  _("Code highlighting: Functions"),
                  _("Code highlighting: Comments"),
                  _("Code highlighting: Numbers"),
                  _("Code highlighting: Strings"),
                  _("Code highlighting: Operators"),
                  _("Code highlighting: End of line")
          };

  m_styleFor = new wxListBox(panel, listbox_styleFor, wxDefaultPosition, wxSize(250, -1), 33, m_styleFor_choices,
                             wxLB_SINGLE);
  m_getStyleFont = new wxButton(panel, style_font_family, _("Choose font"), wxDefaultPosition, wxSize(150, -1));
#ifndef __WXMSW__
  m_styleColor = new ColorPanel(this, panel, color_id, wxDefaultPosition, wxSize(150, 30),
                                wxSUNKEN_BORDER | wxFULL_REPAINT_ON_RESIZE);
#else
  m_styleColor = new wxButton(panel, color_id, wxEmptyString, wxDefaultPosition, wxSize(150, -1));
#endif
  m_boldCB = new wxCheckBox(panel, checkbox_bold, _("Bold"));
  m_italicCB = new wxCheckBox(panel, checkbox_italic, _("Italic"));
  m_underlinedCB = new wxCheckBox(panel, checkbox_underlined, _("Underlined"));
  m_examplePanel = new ExamplePanel(panel, -1, wxDefaultPosition, wxSize(250, 60));
  m_loadStyle = new wxButton(panel, load_id, _("Load"));
  m_saveStyle = new wxButton(panel, save_id, _("Save"));

  grid_sizer_1->Add(df, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_mathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(m_getMathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_1->Add(10, 10);
  grid_sizer_1->Add(m_useJSMath, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

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

  return panel;
}

void ConfigDialogue::OnClose(wxCloseEvent &event)
{
#if defined __WXMAC__
  EndModal(wxID_OK);
#else
  EndModal(wxID_CANCEL);
#endif
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ConfigDialogTab"), m_notebook->GetSelection());
}

void ConfigDialogue::WriteSettings()
{
  wxString search = wxT("maxima-htmldir");
  wxArrayString out;
  wxConfig *config = (wxConfig *) wxConfig::Get();
  Configuration *configuration = m_configuration;
  config->Write(wxT("abortOnError"), m_abortOnError->GetValue());
  config->Write(wxT("pollStdOut"), m_pollStdOut->GetValue());
  configuration->RestartOnReEvaluation(m_restartOnReEvaluation->GetValue());
  if (
          (configuration->MaximaFound()) ||
          (configuration->MaximaLocation() != m_maximaProgram->GetValue())
          )
    configuration->MaximaLocation(m_maximaProgram->GetValue());
  config->Write(wxT("parameters"), m_additionalParameters->GetValue());
  config->Write(wxT("fontSize"), m_fontSize);
  config->Write(wxT("mathFontsize"), m_mathFontSize);
  config->Write(wxT("matchParens"), m_matchParens->GetValue());
  config->Write(wxT("showLength"), m_showLength->GetSelection());
  config->Write(wxT("autosubscript"), m_autosubscript->GetSelection());
  config->Write(wxT("fixedFontTC"), m_fixedFontInTC->GetValue());
  configuration->SetChangeAsterisk(m_changeAsterisk->GetValue());
  config->Write(wxT("enterEvaluates"), m_enterEvaluates->GetValue());
  config->Write(wxT("saveUntitled"), m_saveUntitled->GetValue());
  configuration->SetOpenHCaret(m_openHCaret->GetValue());
  configuration->SetInsertAns(m_insertAns->GetValue());
  configuration->SetAutoIndent(m_autoIndent->GetValue());
  config->Write(wxT("cursorJump"), m_cursorJump->GetValue());
  configuration->HideBrackets(m_hideBrackets->GetValue());
  configuration->SetAutoWrap(m_autoWrap->GetSelection());
  config->Write(wxT("labelWidth"), m_labelWidth->GetValue());
  config->Write(wxT("undoLimit"), m_undoLimit->GetValue());
  config->Write(wxT("recentItems"), m_recentItems->GetValue());
  config->Write(wxT("bitmapScale"), m_bitmapScale->GetValue());
  configuration->FixReorderedIndices(m_fixReorderedIndices->GetValue());
  config->Write(wxT("incrementalSearch"), m_incrementalSearch->GetValue());
  configuration->NotifyIfIdle(m_notifyIfIdle->GetValue());
  configuration->SetLabelChoice(m_showUserDefinedLabels->GetSelection());
  config->Write(wxT("defaultPort"), m_defaultPort->GetValue());
#ifdef __WXMSW__
  config->Write(wxT("wxcd"), m_wxcd->GetValue());
#endif
  config->Write(wxT("AUI/savePanes"), m_savePanes->GetValue());
  config->Write(wxT("usepngCairo"), m_usepngCairo->GetValue());
  config->Write(wxT("OptimizeForVersionControl"), m_uncomressedWXMX->GetValue());
  config->Write(wxT("DefaultFramerate"), m_defaultFramerate->GetValue());
  config->Write(wxT("defaultPlotWidth"), m_defaultPlotWidth->GetValue());
  config->Write(wxT("defaultPlotHeight"), m_defaultPlotHeight->GetValue());
  configuration->SetDisplayedDigits(m_displayedDigits->GetValue());
  config->Write(wxT("AnimateLaTeX"), m_AnimateLaTeX->GetValue());
  config->Write(wxT("TeXExponentsAfterSubscript"), m_TeXExponentsAfterSubscript->GetValue());
  config->Write(wxT("usePartialForDiff"), m_usePartialForDiff->GetValue());
  config->Write(wxT("wrapLatexMath"), m_wrapLatexMath->GetValue());
  config->Write(wxT("exportContainsWXMX"), m_exportContainsWXMX->GetValue());
  configuration->PrintBrackets(m_printBrackets->GetValue());
  config->Write(wxT("HTMLequationFormat"), m_exportWithMathJAX->GetSelection());
  config->Write(wxT("usejsmath"), m_useJSMath->GetValue());
  config->Write(wxT("keepPercent"), m_keepPercentWithSpecials->GetValue());
  config->Write(wxT("texPreamble"), m_texPreamble->GetValue());
  config->Write(wxT("autoSaveInterval"), m_autoSaveInterval->GetValue());
  config->Write(wxT("documentclass"), m_documentclass->GetValue());
  configuration->MathJaXURL(m_mathJaxURL->GetValue());
  if (m_saveSize->GetValue())
    config->Write(wxT("pos-restore"), 1);
  else
    config->Write(wxT("pos-restore"), 0);
  long i = 0;
  i = m_language->GetSelection();
  if (i > -1 && i < LANGUAGE_NUMBER)
    config->Write(wxT("language"), langs[i]);
#ifdef wxUSE_UNICODE
  config->Write(wxT("symbolPaneAdditionalChars"), m_symbolPaneAdditionalChars->GetValue());
#endif
  WriteStyles();
  config->Flush();
}

void ConfigDialogue::OnMpBrowse(wxCommandEvent &event)
{
  wxConfig *config = (wxConfig *) wxConfig::Get();
  wxString dd;
  config->Read(wxT("maxima"), &dd);
#if defined __WXMSW__
  wxString file = wxFileSelector(_("Select Maxima program"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxEmptyString, _("Bat files (*.bat)|*.bat|All|*"),
                                 wxFD_OPEN);
#else
  wxString file = wxFileSelector(_("Select Maxima program"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxEmptyString, _("All|*"),
                                 wxFD_OPEN);
#endif

  if (file.Length())
  {
    if (file.Right(8).Lower() == wxT("wxmaxima") || file.Right(12).Lower() == wxT("wxmaxima.exe"))
      wxMessageBox(_("Invalid entry for Maxima program.\n\nPlease enter the path to Maxima program again."),
                   _("Error"),
                   wxOK | wxICON_ERROR);
    else
      m_maximaProgram->SetValue(file);
  }
}

void ConfigDialogue::OnMathBrowse(wxCommandEvent &event)
{
    wxFont font;
#ifdef __WXMSW__
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(wxT("Linux Libertine O"));
  font.SetEncoding(wxFONTENCODING_UTF8);
  font.SetStyle(wxFONTSTYLE_NORMAL );
  if(!font.IsOk())
#endif
    font = wxFont(m_mathFontSize, wxFONTFAMILY_DEFAULT,
                  wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
                  false, m_mathFontName);
  
  wxFont math;
  math = wxGetFontFromUser(this, font);

  if (math.Ok())
  {
    m_mathFontName = math.GetFaceName();
    m_mathFontSize = math.GetPointSize();
    math.SetPointSize(m_mathFontSize);
    m_getMathFont->SetLabel(m_mathFontName + wxString::Format(wxT(" (%d)"), m_mathFontSize));
    UpdateExample();
  }
}

void ConfigDialogue::OnChangeFontFamily(wxCommandEvent &event)
{
  wxFont font;
  int fontsize = m_fontSize;
  style *tmp = GetStylePointer();
  wxString fontName;

#ifdef __WXMSW__
  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(wxT("Linux Libertine O"));
  font.SetEncoding(wxFONTENCODING_UTF8);
  font.SetStyle(wxFONTSTYLE_NORMAL );
  if(font.IsOk())
    fontName = wxT("Linux Libertine O");
#endif

  if (tmp == &m_styleText || tmp == &m_styleTitle || tmp == &m_styleSubsubsection || tmp == &m_styleSubsection ||
      tmp == &m_styleSection)
  {
    if (tmp->fontSize != 0)
      fontsize = tmp->fontSize;
    fontName = tmp->font;
  }
  else
    fontName = m_styleDefault.font;

  font = wxGetFontFromUser(this, wxFont(fontsize,
                                        wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                                        wxFONTWEIGHT_NORMAL,
                                        false, fontName,
                                        m_fontEncoding));

  if (font.Ok())
  {
    if (event.GetId() == font_family)
    {
      m_styleDefault.font = font.GetFaceName();
      m_fontEncoding = font.GetEncoding();
      m_fontSize = font.GetPointSize();
      m_fontSize = MIN(m_fontSize, MC_MAX_SIZE);
      m_fontSize = MAX(m_fontSize, MC_MIN_SIZE);
      m_getFont->SetLabel(m_styleDefault.font + wxString::Format(wxT(" (%d)"), m_fontSize));
    }
    else
    {
      tmp->font = font.GetFaceName();
      tmp->fontSize = font.GetPointSize();
      tmp->fontSize = MAX(tmp->fontSize, MC_MIN_SIZE);
    }
    UpdateExample();
  }
}

void ConfigDialogue::ReadStyles(wxString file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
  {
    wxFileInputStream str(file);
    config = new wxFileConfig(str);
  }

  m_fontSize = m_mathFontSize = 12;
  config->Read(wxT("fontsize"), &m_fontSize);
  config->Read(wxT("mathfontsize"), &m_mathFontSize);
  config->Read(wxT("Style/fontname"), &m_styleDefault.font);
  if (m_styleDefault.font.Length())
    m_getFont->SetLabel(m_styleDefault.font + wxString::Format(wxT(" (%d)"), m_fontSize));

  int encoding = wxFONTENCODING_DEFAULT;
  config->Read(wxT("fontEncoding"), &encoding);
  m_fontEncoding = (wxFontEncoding) encoding;

  m_mathFontName = wxEmptyString;
  config->Read(wxT("Style/Math/fontname"), &m_mathFontName);
  if (m_mathFontName.Length() > 0)
    m_getMathFont->SetLabel(m_mathFontName + wxString::Format(wxT(" (%d)"), m_mathFontSize));

  wxString tmp;
  // Document background color
  m_styleBackground.color = wxT("white");
  if (config->Read(wxT("Style/Background/color"),
                   &tmp))
    m_styleBackground.color.Set(tmp);

  // Text background
  m_styleTextBackground.color = wxT("white");
  if (config->Read(wxT("Style/TextBackground/color"),
                   &tmp))
    m_styleTextBackground.color.Set(tmp);

  // Highlighting color
  m_styleHighlight.color = wxT("red");
  if (config->Read(wxT("Style/Highlight/color"),
                   &tmp))
    m_styleHighlight.color.Set(tmp);

  // Groupcell bracket color
  m_styleCellBracket.color = wxT("rgb(0,0,0)");
  if (config->Read(wxT("Style/CellBracket/color"),
                   &tmp))
    m_styleCellBracket.color.Set(tmp);

  // Active groupcell bracket color
  m_styleActiveCellBracket.color = wxT("rgb(255,0,0)");
  if (config->Read(wxT("Style/ActiveCellBracket/color"),
                   &tmp))
    m_styleActiveCellBracket.color.Set(tmp);

  // Horizontal caret color/ cursor color
  m_styleCursor.color = wxT("rgb(0,0,0)");
  if (config->Read(wxT("Style/Cursor/color"),
                   &tmp))
    m_styleCursor.color.Set(tmp);

  // Outdated cells
  m_styleOutdated.color = wxT("rgb(153,153,153)");
  if (config->Read(wxT("Style/Outdated/color"),
                   &tmp))
    m_styleOutdated.color.Set(tmp);

  // Selection color defaults to light grey on windows
#if defined __WXMSW__
  m_styleSelection.color = wxColour(wxT("light grey"));
#else
  m_styleSelection.color = wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT);
#endif
  if (config->Read(wxT("Style/Selection/color"),
                   &tmp))
    m_styleSelection.color.Set(tmp);

  // Text equal to the current selection
  m_styleEqualsSelection.color = wxT("rgb(192,255,192)");
  if (config->Read(wxT("Style/EqualsSelection/color"),
                   &tmp))
    m_styleEqualsSelection.color.Set(tmp);

#define READ_STYLE(style, where)                                        \
  if (config->Read(wxT(where "color"), &tmp)) style.color.Set(tmp);     \
  config->Read(wxT(where "bold"),                                       \
               &style.bold);                                            \
  config->Read(wxT(where "italic"),                                     \
               &style.italic);                                          \
  config->Read(wxT(where "underlined"),                                 \
               &style.underlined);

  // Text in math output
  m_styleDefault.color = wxT("black");
  m_styleDefault.bold = false;
  m_styleDefault.italic = false;
  m_styleDefault.underlined = false;
  READ_STYLE(m_styleDefault, "Style/NormalText/")

  // Main prompt
  m_styleMainPrompt.color = wxT("rgb(255,128,128)");
  m_styleMainPrompt.bold = false;
  m_styleMainPrompt.italic = false;
  m_styleMainPrompt.underlined = false;
  READ_STYLE(m_styleMainPrompt, "Style/MainPrompt/")

  // Other prompt
  m_styleOtherPrompt.color = wxT("red");
  m_styleOtherPrompt.bold = false;
  m_styleOtherPrompt.italic = true;
  m_styleOtherPrompt.underlined = false;
  READ_STYLE(m_styleOtherPrompt, "Style/OtherPrompt/")

  // Labels
  m_styleLabel.color = wxT("rgb(255,192,128)");
  m_styleLabel.bold = false;
  m_styleLabel.italic = false;
  m_styleLabel.underlined = false;
  READ_STYLE(m_styleLabel, "Style/Label/")

  // Warnings
  m_styleWarning.color = wxT("orange");
  m_styleWarning.bold = true;
  m_styleWarning.italic = false;
  m_styleWarning.underlined = false;
  READ_STYLE(m_styleWarning, "Style/Warning/")

  // User-defined Labels
  m_styleUserDefinedLabel.color = wxT("rgb(255,64,0)");
  m_styleUserDefinedLabel.bold = false;
  m_styleUserDefinedLabel.italic = false;
  m_styleUserDefinedLabel.underlined = false;
  READ_STYLE(m_styleUserDefinedLabel, "Style/UserDefinedLabel/")

  // Special
  m_styleSpecial.color = m_styleDefault.color;
  m_styleSpecial.bold = false;
  m_styleSpecial.italic = false;
  m_styleSpecial.underlined = false;
  READ_STYLE(m_styleSpecial, "Style/Special/")

  // Input
  m_styleInput.color = wxT("blue");
  m_styleInput.bold = false;
  m_styleInput.italic = false;
  m_styleInput.underlined = false;
  READ_STYLE(m_styleInput, "Style/Input/")

  // Number
  m_styleNumber.color = m_styleDefault.color;
  m_styleNumber.bold = false;
  m_styleNumber.italic = false;
  m_styleNumber.underlined = false;
  READ_STYLE(m_styleNumber, "Style/Number/")

  // String
  m_styleString.color = m_styleDefault.color;
  m_styleString.bold = false;
  m_styleString.italic = true;
  m_styleString.underlined = false;
  READ_STYLE(m_styleString, "Style/String/")

  // Operator
  m_styleString.color = m_styleDefault.color;
  m_styleString.bold = false;
  m_styleString.italic = true;
  m_styleString.underlined = false;
  READ_STYLE(m_styleString, "Style/String/Operator")

  // Greek
  m_styleGreek.color = m_styleDefault.color;
  m_styleGreek.bold = false;
  m_styleGreek.italic = false;
  m_styleGreek.underlined = false;
  READ_STYLE(m_styleGreek, "Style/Greek/")

  // Variable
  m_styleVariable.color = m_styleDefault.color;
  m_styleVariable.bold = false;
  m_styleVariable.italic = true;
  m_styleVariable.underlined = false;
  READ_STYLE(m_styleVariable, "Style/Variable/")

  // Function
  m_styleFunction.color = m_styleDefault.color;
  m_styleFunction.bold = false;
  m_styleFunction.italic = false;
  m_styleFunction.underlined = false;
  READ_STYLE(m_styleFunction, "Style/Function/")

  // Text
  m_styleText.color = wxT("black");
  m_styleText.bold = true;
  m_styleText.italic = false;
  m_styleText.underlined = false;
  m_styleText.font = m_styleDefault.font;
  m_styleText.fontSize = m_fontSize;
  config->Read(wxT("Style/Text/fontsize"),
               &m_styleText.fontSize);
  config->Read(wxT("Style/Text/fontname"),
               &m_styleText.font);
  READ_STYLE(m_styleText, "Style/Text/")

  // Variables in highlighted code
  m_styleCodeHighlightingVariable.color = wxT("rgb(0,128,0)");
  m_styleCodeHighlightingVariable.bold = false;
  m_styleCodeHighlightingVariable.italic = true;
  m_styleCodeHighlightingVariable.underlined = false;
  READ_STYLE(m_styleCodeHighlightingVariable, "Style/CodeHighlighting/Variable/")

  // Functions in highlighted code
  m_styleCodeHighlightingFunction.color = wxT("rgb(128,0,0)");
  m_styleCodeHighlightingFunction.bold = false;
  m_styleCodeHighlightingFunction.italic = true;
  m_styleCodeHighlightingFunction.underlined = false;
  READ_STYLE(m_styleCodeHighlightingFunction, "Style/CodeHighlighting/Function/")

  // Comments in highlighted code
  m_styleCodeHighlightingComment.color = wxT("rgb(64,64,64)");
  m_styleCodeHighlightingComment.bold = false;
  m_styleCodeHighlightingComment.italic = true;
  m_styleCodeHighlightingComment.underlined = false;
  READ_STYLE(m_styleCodeHighlightingComment, "Style/CodeHighlighting/Comment/")

  // Numbers in highlighted code
  m_styleCodeHighlightingNumber.color = wxT("rgb(128,64,0)");
  m_styleCodeHighlightingNumber.bold = false;
  m_styleCodeHighlightingNumber.italic = true;
  m_styleCodeHighlightingNumber.underlined = false;
  READ_STYLE(m_styleCodeHighlightingNumber, "Style/CodeHighlighting/Number/")

  // Strings in highlighted code
  m_styleCodeHighlightingString.color = wxT("rgb(0,0,128)");
  m_styleCodeHighlightingString.bold = false;
  m_styleCodeHighlightingString.italic = true;
  m_styleCodeHighlightingString.underlined = false;
  READ_STYLE(m_styleCodeHighlightingString, "Style/CodeHighlighting/String/")

  // Operators in highlighted code
  m_styleCodeHighlightingOperator.color = wxT("rgb(0,0,128)");
  m_styleCodeHighlightingOperator.bold = false;
  m_styleCodeHighlightingOperator.italic = true;
  m_styleCodeHighlightingOperator.underlined = false;
  READ_STYLE(m_styleCodeHighlightingOperator, "Style/CodeHighlighting/Operator/")
  // Line endings in highlighted code
  m_styleCodeHighlightingEndOfLine.color = wxT("rgb(128,128,128)");
  m_styleCodeHighlightingEndOfLine.bold = false;
  m_styleCodeHighlightingEndOfLine.italic = true;
  m_styleCodeHighlightingEndOfLine.underlined = false;
  READ_STYLE(m_styleCodeHighlightingEndOfLine, "Style/CodeHighlighting/EndOfLine/")

  // Subsubsection
  m_styleSubsubsection.color = wxT("black");
  m_styleSubsubsection.bold = true;
  m_styleSubsubsection.italic = false;
  m_styleSubsubsection.underlined = false;
  m_styleSubsubsection.font = m_styleDefault.font;
  m_styleSubsubsection.fontSize = 16;
  config->Read(wxT("Style/Subsubsection/fontsize"),
               &m_styleSubsubsection.fontSize);
  config->Read(wxT("Style/Subsubsection/fontname"),
               &m_styleSubsubsection.font);
  READ_STYLE(m_styleSubsubsection, "Style/Subsubsection/")
  // Subsection
  m_styleSubsection.color = wxT("black");
  m_styleSubsection.bold = true;
  m_styleSubsection.italic = false;
  m_styleSubsection.underlined = false;
  m_styleSubsection.font = m_styleDefault.font;
  m_styleSubsection.fontSize = 16;
  config->Read(wxT("Style/Subsection/fontsize"),
               &m_styleSubsection.fontSize);
  config->Read(wxT("Style/Subsection/fontname"),
               &m_styleSubsection.font);
  READ_STYLE(m_styleSubsection, "Style/Subsection/")

  // Section
  m_styleSection.color = wxT("black");
  m_styleSection.bold = true;
  m_styleSection.italic = true;
  m_styleSection.underlined = false;
  m_styleSection.font = m_styleDefault.font;
  m_styleSection.fontSize = 18;
  config->Read(wxT("Style/Section/fontsize"),
               &m_styleSection.fontSize);
  config->Read(wxT("Style/Section/fontname"),
               &m_styleSection.font);
  READ_STYLE(m_styleSection, "Style/Section/")

  // Title
  m_styleTitle.color = wxT("black");
  m_styleTitle.bold = true;
  m_styleTitle.italic = false;
  m_styleTitle.underlined = true;
  m_styleTitle.font = m_styleDefault.font;
  m_styleTitle.fontSize = 24;
  config->Read(wxT("Style/Title/fontsize"),
               &m_styleTitle.fontSize);
  config->Read(wxT("Style/Title/fontname"),
               &m_styleTitle.font);
  READ_STYLE(m_styleTitle, "Style/Title/")

#undef READ_STYLE

  // Set values in dialog
  m_styleFor->SetSelection(0);
  m_styleColor->SetBackgroundColour(m_styleDefault.color); // color the panel, after the styles are loaded
  m_boldCB->SetValue(m_styleDefault.bold);
  m_italicCB->SetValue(m_styleDefault.italic);
  m_underlinedCB->SetValue(m_styleDefault.underlined);

  if (file != wxEmptyString)
    wxDELETE(config);
}

void ConfigDialogue::WriteStyles(wxString file)
{
  wxConfigBase *config = NULL;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else
  {
    wxStringInputStream str(wxEmptyString);
    config = new wxFileConfig(str);
  }

  config->Write(wxT("Style/Background/color"),
                m_styleBackground.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/Highlight/color"),
                m_styleHighlight.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/TextBackground/color"),
                m_styleTextBackground.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/CellBracket/color"),
                m_styleCellBracket.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/ActiveCellBracket/color"),
                m_styleActiveCellBracket.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/Cursor/color"),
                m_styleCursor.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/Selection/color"),
                m_styleSelection.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/EqualsSelection/color"),
                m_styleEqualsSelection.color.GetAsString(wxC2S_CSS_SYNTAX));
  config->Write(wxT("Style/Outdated/color"),
                m_styleOutdated.color.GetAsString(wxC2S_CSS_SYNTAX));

  config->Write(wxT("Style/fontname"), m_styleDefault.font);
  config->Write(wxT("fontEncoding"), (int) m_fontEncoding);

  config->Write(wxT("Style/Math/fontname"), m_mathFontName);

#define WRITE_STYLE(style, where)                                       \
  config->Write(wxT(where "color"), style.color.GetAsString(wxC2S_CSS_SYNTAX)); \
  config->Write(wxT(where "bold"), style.bold);                         \
  config->Write(wxT(where "italic"), style.italic);                     \
  config->Write(wxT(where "underlined"), style.underlined);

  // Normal text
  WRITE_STYLE(m_styleDefault, "Style/NormalText/")

  // Main prompt
  WRITE_STYLE(m_styleMainPrompt, "Style/MainPrompt/")

  // Other prompt
  WRITE_STYLE(m_styleOtherPrompt, "Style/OtherPrompt/")

  // Label
  WRITE_STYLE(m_styleLabel, "Style/Label/")

  // Warning
  WRITE_STYLE(m_styleWarning, "Style/Warning/")

  // User-defined Label
  WRITE_STYLE(m_styleUserDefinedLabel, "Style/UserDefinedLabel/")

  // Special
  WRITE_STYLE(m_styleSpecial, "Style/Special/")

  // Input
  WRITE_STYLE(m_styleInput, "Style/Input/")

  // Number
  WRITE_STYLE(m_styleNumber, "Style/Number/")

  // Greek
  WRITE_STYLE(m_styleGreek, "Style/Greek/")

  // String
  WRITE_STYLE(m_styleString, "Style/String/")

  // Variable
  WRITE_STYLE(m_styleVariable, "Style/Variable/")

  // Text
  config->Write(wxT("Style/Text/fontname"), m_styleText.font);
  config->Write(wxT("Style/Text/fontsize"), m_styleText.fontSize);
  WRITE_STYLE(m_styleText, "Style/Text/")

  // Syntax highlighting
  WRITE_STYLE(m_styleCodeHighlightingVariable, "Style/CodeHighlighting/Variable/")
  WRITE_STYLE(m_styleCodeHighlightingFunction, "Style/CodeHighlighting/Function/")
  WRITE_STYLE(m_styleCodeHighlightingComment, "Style/CodeHighlighting/Comment/")
  WRITE_STYLE(m_styleCodeHighlightingNumber, "Style/CodeHighlighting/Number/")
  WRITE_STYLE(m_styleCodeHighlightingString, "Style/CodeHighlighting/String/")
  WRITE_STYLE(m_styleCodeHighlightingOperator, "Style/CodeHighlighting/Operator/")
  WRITE_STYLE(m_styleCodeHighlightingEndOfLine, "Style/CodeHighlighting/EndOfLine/")

  // Subsubsection
  config->Write(wxT("Style/Subsubsection/fontname"), m_styleSubsubsection.font);
  config->Write(wxT("Style/Subsubsection/fontsize"), m_styleSubsubsection.fontSize);
  WRITE_STYLE(m_styleSubsubsection, "Style/Subsubsection/")

  // Subsection
  config->Write(wxT("Style/Subsection/fontname"), m_styleSubsection.font);
  config->Write(wxT("Style/Subsection/fontsize"), m_styleSubsection.fontSize);
  WRITE_STYLE(m_styleSubsection, "Style/Subsection/")

  // Section
  config->Write(wxT("Style/Section/fontname"), m_styleSection.font);
  config->Write(wxT("Style/Section/fontsize"), m_styleSection.fontSize);
  WRITE_STYLE(m_styleSection, "Style/Section/")

  // Title
  config->Write(wxT("Style/Title/fontname"), m_styleTitle.font);
  config->Write(wxT("Style/Title/fontsize"), m_styleTitle.fontSize);
  WRITE_STYLE(m_styleTitle, "Style/Title/")

  // Function names
  WRITE_STYLE(m_styleFunction, "Style/Function/")

  config->Flush();

  if (file != wxEmptyString)
  {
    wxFile fl(file, wxFile::write);
    wxFileOutputStream str(fl);
    ((wxFileConfig *) config)->Save(str);
    wxDELETE(config);
  }
}

void ConfigDialogue::OnChangeColor()
{
  style *tmp = GetStylePointer();
  wxColour col = wxGetColourFromUser(this, tmp->color);
  if (col.IsOk())
  {
    tmp->color = col.GetAsString(wxC2S_CSS_SYNTAX);
    UpdateExample();
    m_styleColor->SetBackgroundColour(tmp->color);
  }
}

void ConfigDialogue::OnChangeStyle(wxCommandEvent &event)
{
  style *tmp = GetStylePointer();
  int st = m_styleFor->GetSelection();

  m_styleColor->SetBackgroundColour(tmp->color);

  // MAGIC NUMBERS:
  // the positions of TEXT and TITLE style in the list.
  if (st >= 13 && st <= 17)
    m_getStyleFont->Enable(true);
  else
    m_getStyleFont->Enable(false);

  // Background color only
  if (st >= TS_SECTION)
  {
    m_boldCB->Enable(false);
    m_italicCB->Enable(false);
    m_underlinedCB->Enable(false);
  }
  else
  {
    if (st > TS_OUTDATED)
    {
      m_boldCB->Enable(false);
      m_italicCB->Enable(false);
      m_underlinedCB->Enable(false);
    }
    else
    {
      m_boldCB->Enable(true);
      m_italicCB->Enable(true);
      m_underlinedCB->Enable(true);
      m_boldCB->SetValue(tmp->bold);
      m_italicCB->SetValue(tmp->italic);
      m_underlinedCB->SetValue(tmp->underlined);
    }
  }
  UpdateExample();
}

void ConfigDialogue::OnCheckbox(wxCommandEvent &event)
{
  style *tmp = GetStylePointer();

  tmp->bold = m_boldCB->GetValue();
  tmp->italic = m_italicCB->GetValue();
  tmp->underlined = m_underlinedCB->GetValue();

  UpdateExample();
}

void ConfigDialogue::OnChangeWarning(wxCommandEvent &event)
{
  wxMessageBox(_("Please restart wxMaxima for changes to take effect!"),
               _("Configuration warning"),
               wxOK | wxICON_WARNING);
}

style *ConfigDialogue::GetStylePointer()
{
  style *tmp;
  switch (m_styleFor->GetSelection())
  {
    case 1:
      tmp = &m_styleVariable;
      break;
    case 2:
      tmp = &m_styleNumber;
      break;
    case 3:
      tmp = &m_styleFunction;
      break;
    case 4:
      tmp = &m_styleSpecial;
      break;
    case 5:
      tmp = &m_styleGreek;
      break;
    case 6:
      tmp = &m_styleString;
      break;
    case 7:
      tmp = &m_styleInput;
      break;
    case 8:
      tmp = &m_styleMainPrompt;
      break;
    case 9:
      tmp = &m_styleOtherPrompt;
      break;
    case 10:
      tmp = &m_styleLabel;
      break;
    case 11:
      tmp = &m_styleUserDefinedLabel;
      break;
    case 12:
      tmp = &m_styleHighlight;
      break;
    case 13:
      tmp = &m_styleWarning;
      break;
    case 14:
      tmp = &m_styleText;
      break;
    case 15:
      tmp = &m_styleSubsubsection;
      break;
    case 16:
      tmp = &m_styleSubsection;
      break;
    case 17:
      tmp = &m_styleSection;
      break;
    case 18:
      tmp = &m_styleTitle;
      break;
    case 19:
      tmp = &m_styleTextBackground;
      break;
    case 20:
      tmp = &m_styleBackground;
      break;
    case 21:
      tmp = &m_styleCellBracket;
      break;
    case 22:
      tmp = &m_styleActiveCellBracket;
      break;
    case 23:
      tmp = &m_styleCursor;
      break;
    case 24:
      tmp = &m_styleSelection;
      break;
    case 25:
      tmp = &m_styleEqualsSelection;
      break;
    case 26:
      tmp = &m_styleOutdated;
      break;
    case 27:
      tmp = &m_styleCodeHighlightingVariable;
      break;
    case 28:
      tmp = &m_styleCodeHighlightingFunction;
      break;
    case 29:
      tmp = &m_styleCodeHighlightingComment;
      break;
    case 30:
      tmp = &m_styleCodeHighlightingNumber;
      break;
    case 31:
      tmp = &m_styleCodeHighlightingString;
      break;
    case 32:
      tmp = &m_styleCodeHighlightingOperator;
      break;
    case 33:
      tmp = &m_styleCodeHighlightingEndOfLine;
      break;
    default:
      tmp = &m_styleDefault;
  }
  return tmp;
}

void ConfigDialogue::UpdateExample()
{
  style *tmp = GetStylePointer();
  wxString example = _("Example text");
  wxColour color(tmp->color);
  wxString font(m_styleDefault.font);

  if (tmp == &m_styleBackground)
    color = m_styleInput.color;

  int fontsize = m_fontSize;
  if (tmp == &m_styleText || tmp == &m_styleSubsubsection || tmp == &m_styleSubsection || tmp == &m_styleSection ||
      tmp == &m_styleTitle)
  {
    fontsize = tmp->fontSize;
    font = tmp->font;
    if (fontsize == 0)
      fontsize = m_fontSize;
  }
  else if (tmp == &m_styleVariable || tmp == &m_styleNumber || tmp == &m_styleFunction ||
           tmp == &m_styleSpecial)
  {
    fontsize = m_mathFontSize;
    font = m_mathFontName;
  }

  if (tmp == &m_styleTextBackground)
  {
    m_examplePanel->SetFontSize(m_styleText.fontSize);
    m_examplePanel->SetStyle(m_styleText.color, m_styleText.italic, m_styleText.bold, m_styleText.underlined,
                             m_styleText.font);
  }
  else
  {
    m_examplePanel->SetFontSize(fontsize);
    m_examplePanel->SetStyle(color, tmp->italic, tmp->bold, tmp->underlined, font);
  }

  if (tmp == &m_styleTextBackground ||
      tmp == &m_styleText)
    m_examplePanel->SetBackgroundColour(m_styleTextBackground.color);
  else
    m_examplePanel->SetBackgroundColour(m_styleBackground.color);

  m_examplePanel->Refresh();
}

void ConfigDialogue::OnTabChange(wxBookCtrlEvent &event)
{
  wxConfigBase *config = wxConfig::Get();
  config->Write(wxT("ConfigDialogTab"), event.GetSelection());
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
      WriteStyles(file);
  }
  else
  {
    wxString file = wxFileSelector(_("Load style from file"),
                                   wxEmptyString, wxT("style.ini"), wxT("ini"),
                                   _("Config file (*.ini)|*.ini"),
                                   wxFD_OPEN);
    if (file != wxEmptyString)
    {
      ReadStyles(file);
      UpdateExample();
    }
  }
}

#if defined __WXMSW__
void ConfigDialogue::OnColorButton(wxCommandEvent &event)
{
  OnChangeColor();
}
#endif

BEGIN_EVENT_TABLE(ConfigDialogue, wxPropertySheetDialog)
                EVT_BUTTON(wxID_OPEN, ConfigDialogue::OnMpBrowse)
                EVT_BUTTON(button_mathFont, ConfigDialogue::OnMathBrowse)
                EVT_BUTTON(font_family, ConfigDialogue::OnChangeFontFamily)
#if defined __WXMSW__
                EVT_BUTTON(color_id, ConfigDialogue::OnColorButton)
#endif
                EVT_LISTBOX(listbox_styleFor, ConfigDialogue::OnChangeStyle)
                EVT_COMBOBOX(language_id, ConfigDialogue::OnChangeWarning)
                EVT_CHECKBOX(checkbox_bold, ConfigDialogue::OnCheckbox)
                EVT_CHECKBOX(checkbox_italic, ConfigDialogue::OnCheckbox)
                EVT_CHECKBOX(checkbox_underlined, ConfigDialogue::OnCheckbox)
                EVT_BUTTON(save_id, ConfigDialogue::LoadSave)
                EVT_BUTTON(load_id, ConfigDialogue::LoadSave)
                EVT_BUTTON(style_font_family, ConfigDialogue::OnChangeFontFamily)
                EVT_CLOSE(ConfigDialogue::OnClose)
END_EVENT_TABLE()

void ExamplePanel::OnPaint(wxPaintEvent &event)
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
  wxFont font = wxFont(m_size, wxFONTFAMILY_MODERN, italic, bold, underlined, m_font);
  font.SetPointSize(m_size);
  dc.SetFont(font);

  dc.GetTextExtent(example, &text_width, &text_height);

  dc.DrawText(example, (panel_width - text_width) / 2,
              (panel_height - text_height) / 2);
}

BEGIN_EVENT_TABLE(ExamplePanel, wxPanel)
                EVT_PAINT(ExamplePanel::OnPaint)
END_EVENT_TABLE()

#ifndef __WXMSW__
BEGIN_EVENT_TABLE(ColorPanel, wxPanel)
                EVT_LEFT_UP(ColorPanel::OnClick)
END_EVENT_TABLE()
#endif
