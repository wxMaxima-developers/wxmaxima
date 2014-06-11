///
///  Copyright (C) 2004-2013 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
///            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "Config.h"
#include "MathCell.h"

#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/font.h>
#include <wx/fontdlg.h>
#include <wx/wfstream.h>
#include <wx/sstream.h>
#include <wx/colordlg.h>
#include <wx/settings.h>

#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define MIN(a,b) ((a)>(b) ? (b) : (a))

// Should match whatever is put in the m_language
const int langs[] =
  {
    wxLANGUAGE_DEFAULT,
    wxLANGUAGE_CATALAN,
    wxLANGUAGE_CHINESE_SIMPLIFIED,
    wxLANGUAGE_CHINESE_TRADITIONAL,
    wxLANGUAGE_CZECH,
    wxLANGUAGE_DANISH,
    wxLANGUAGE_ENGLISH,
    wxLANGUAGE_FRENCH,
    wxLANGUAGE_GALICIAN,
    wxLANGUAGE_GERMAN,
    wxLANGUAGE_GREEK,
    wxLANGUAGE_HUNGARIAN,
    wxLANGUAGE_ITALIAN,
    wxLANGUAGE_JAPANESE,
    wxLANGUAGE_POLISH,
    wxLANGUAGE_PORTUGUESE_BRAZILIAN,
    wxLANGUAGE_RUSSIAN,
    wxLANGUAGE_SPANISH,
    wxLANGUAGE_UKRAINIAN
  };

#define LANGUAGE_NUMBER 18


Config::Config(wxWindow* parent)
{
#if defined __WXMAC__
#if wxCHECK_VERSION(2,9,1)
  SetSheetStyle(wxPROPSHEET_BUTTONTOOLBOOK | wxPROPSHEET_SHRINKTOFIT);
#else
  SetSheetStyle(wxPROPSHEET_LISTBOOK | wxPROPSHEET_SHRINKTOFIT);
#endif
#else
  SetSheetStyle(wxPROPSHEET_LISTBOOK);
#endif
  SetSheetInnerBorder(3);
  SetSheetOuterBorder(3);

#if defined __WXMAC__
  #define IMAGE(img) wxImage(wxT("wxMaxima.app/Contents/Resources/config/") wxT(img))
#elif defined __WXMSW__
  #define IMAGE(img) wxImage(wxT("art/config/") wxT(img))
#else
  wxString prefix = wxT(PREFIX);
  #define IMAGE(img) wxImage(prefix + wxT("/share/wxMaxima/") + wxT(img))
#endif

  wxSize imageSize(32, 32);
  m_imageList = new wxImageList(32, 32);
  m_imageList->Add(IMAGE("options.png"));
  m_imageList->Add(IMAGE("maxima.png"));
  m_imageList->Add(IMAGE("styles.png"));

  Create(parent, wxID_ANY, _("wxMaxima configuration"),
      wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE);

  m_notebook = GetBookCtrl();

  m_notebook->SetImageList(m_imageList);

  m_notebook->AddPage(CreateOptionsPanel(), _("Options"), true, 0);
  m_notebook->AddPage(CreateMaximaPanel(), _("Maxima"), false, 1);
  m_notebook->AddPage(CreateStylePanel(), _("Style"), false, 2);

#ifndef __WXMAC__
  CreateButtons(wxOK | wxCANCEL);
#endif

  LayoutDialog();

  SetProperties();
  UpdateExample();
}

Config::~Config()
{
  if (m_imageList != NULL)
    delete m_imageList;
}

void Config::SetProperties()
{
  SetTitle(_("wxMaxima configuration"));

  m_maximaProgram->SetToolTip(_("Enter the path to the Maxima executable."));
  m_additionalParameters->SetToolTip(_("Additional parameters for Maxima"
                                       " (e.g. -l clisp)."));
  m_saveSize->SetToolTip(_("Save wxMaxima window size/position between sessions."));
  m_savePanes->SetToolTip(_("Save panes layout between sessions."));
  m_matchParens->SetToolTip(_("Write matching parenthesis in text controls."));
  m_showLong->SetToolTip(_("Show long expressions in wxMaxima document."));
  m_language->SetToolTip(_("Language used for wxMaxima GUI."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_getFont->SetToolTip(_("Font used for display in document."));
  m_getMathFont->SetToolTip(_("Font used for displaying math characters in document."));
  m_changeAsterisk->SetToolTip(_("Use centered dot character for multiplication"));
  m_defaultPort->SetToolTip(_("The default port used for communication between Maxima and wxMaxima."));

  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString mp, mc, ib, mf;
  bool match = true, showLongExpr = false, savePanes = false;
  bool fixedFontTC = true, changeAsterisk = false, usejsmath = true, keepPercent = true;
  bool enterEvaluates = false, saveUntitled = true, openHCaret = false;
  bool insertAns = true;
  bool fixReorderedIndices = false;
  int rs = 0;
  int lang = wxLANGUAGE_UNKNOWN;
  int panelSize = 1;

  config->Read(wxT("maxima"), &mp);
  config->Read(wxT("parameters"), &mc);
  config->Read(wxT("AUI/savePanes"), &savePanes);
  config->Read(wxT("pos-restore"), &rs);
  config->Read(wxT("matchParens"), &match);
  config->Read(wxT("showLong"), &showLongExpr);
  config->Read(wxT("language"), &lang);
  config->Read(wxT("changeAsterisk"), &changeAsterisk);
  config->Read(wxT("fixedFontTC"), &fixedFontTC);
  config->Read(wxT("panelSize"), &panelSize);
  config->Read(wxT("enterEvaluates"), &enterEvaluates);
  config->Read(wxT("saveUntitled"), &saveUntitled);
  config->Read(wxT("openHCaret"), &openHCaret);
  config->Read(wxT("insertAns"), &insertAns);
  config->Read(wxT("fixReorderedIndices"), &fixReorderedIndices);
  config->Read(wxT("usejsmath"), &usejsmath);
  config->Read(wxT("keepPercent"), &keepPercent);

  int i = 0;
  for (i = 0; i < LANGUAGE_NUMBER; i++)
    if (langs[i] == lang)
      break;
  if (i < LANGUAGE_NUMBER)
    m_language->SetSelection(i);
  else
    m_language->SetSelection(0);

#if defined __WXMSW__
  wxString cwd = wxGetCwd();
  cwd.Replace(wxT("wxMaxima"), wxT("\\bin\\maxima.bat"));
  if (wxFileExists(cwd))
  {
    m_maximaProgram->SetValue(cwd);
    m_maximaProgram->Enable(false);
    m_mpBrowse->Enable(false);
  }
  else
  {
    if (mp.Length())
      m_maximaProgram->SetValue(mp);
    else
      m_maximaProgram->SetValue(wxT("maxima.bat"));
  }
#elif defined __WXMAC__
  if (mp.Length())
    m_maximaProgram->SetValue(mp);
  else
    // this is where the mac installer installs maxima
    m_maximaProgram->SetValue(wxT("/Applications/Maxima.app"));
#else
  if (mp.Length())
    m_maximaProgram->SetValue(mp);
  else
    m_maximaProgram->SetValue(wxT("maxima"));
#endif
  m_additionalParameters->SetValue(mc);
  if (rs == 1)
    m_saveSize->SetValue(true);
  else
    m_saveSize->SetValue(false);
  m_savePanes->SetValue(savePanes);
  m_matchParens->SetValue(match);
  m_showLong->SetValue(showLongExpr);
  m_changeAsterisk->SetValue(changeAsterisk);
  m_enterEvaluates->SetValue(enterEvaluates);
  m_saveUntitled->SetValue(saveUntitled);
  m_openHCaret->SetValue(openHCaret);
  m_insertAns->SetValue(insertAns);
  m_fixReorderedIndices->SetValue(fixReorderedIndices);
  m_fixedFontInTC->SetValue(fixedFontTC);
  m_useJSMath->SetValue(usejsmath);
  m_keepPercentWithSpecials->SetValue(keepPercent);

  m_getStyleFont->Enable(false);

  if (!wxFontEnumerator::IsValidFacename(wxT("jsMath-cmex10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmsy10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmr10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmmi10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmti10")))
    m_useJSMath->Enable(false);

  ReadStyles();
}

wxPanel* Config::CreateOptionsPanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer* grid_sizer = new wxFlexGridSizer(2, 2, 5, 5);
  wxFlexGridSizer* vsizer = new wxFlexGridSizer(13,1,5,5);

  int defaultPort = 4010;
  wxConfig::Get()->Read(wxT("defaultPort"), &defaultPort);

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
      _("French"),
      _("Galician"),
      _("German"),
      _("Greek"),
      _("Hungarian"),
      _("Italian"),
      _("Japanese"),
      _("Polish"),
      _("Portuguese (Brazilian)"),
      _("Russian"),
      _("Spanish"),
      _("Ukrainian")
    };
  m_language = new wxComboBox(panel, language_id, wxEmptyString, wxDefaultPosition, wxSize(230, -1), LANGUAGE_NUMBER, m_language_choices, wxCB_DROPDOWN | wxCB_READONLY);
  wxStaticText* dp = new wxStaticText(panel, -1, _("Default port:"));
  m_defaultPort = new wxSpinCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(70, -1), wxSP_ARROW_KEYS, 50, 5000, defaultPort);
  m_defaultPort->SetValue(defaultPort);
  m_saveSize = new wxCheckBox(panel, -1, _("Save wxMaxima window size/position"));
  m_savePanes = new wxCheckBox(panel, -1, _("Save panes layout"));
  m_matchParens = new wxCheckBox(panel, -1, _("Match parenthesis in text controls"));
  m_fixedFontInTC = new wxCheckBox(panel, -1, _("Fixed font in text controls"));
  m_showLong = new wxCheckBox(panel, -1, _("Show long expressions"));
  m_changeAsterisk = new wxCheckBox(panel, -1, _("Use centered dot character for multiplication"));
  m_keepPercentWithSpecials = new wxCheckBox(panel, -1, _("Keep percent sign with special symbols: %e, %i, etc."));
  m_enterEvaluates = new wxCheckBox(panel, -1, _("Enter evaluates cells"));
  m_saveUntitled = new wxCheckBox(panel, -1, _("Ask to save untitled documents"));
  m_openHCaret = new wxCheckBox(panel, -1, _("Open a cell when Maxima expects input"));
  m_insertAns = new wxCheckBox(panel, -1, _("Insert % before an operator at the beginning of a cell"));
  m_fixReorderedIndices = new wxCheckBox(panel, -1, _("Fix reordered reference indices (of %i, %o) before saving"));

  // TAB 1
  // Maxima options box

  // wxMaxima options box
  grid_sizer->Add(lang, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_language, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(dp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer->Add(m_defaultPort, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vsizer->Add(grid_sizer, 1, wxEXPAND, 5);
  vsizer->Add(m_saveSize, 0, wxALL, 5);
  vsizer->Add(m_savePanes, 0, wxALL, 5);
  vsizer->Add(m_matchParens, 0, wxALL, 5);
  vsizer->Add(m_fixedFontInTC, 0, wxALL, 5);
  vsizer->Add(m_showLong, 0, wxALL, 5);
  vsizer->Add(m_changeAsterisk, 0, wxALL, 5);
  vsizer->Add(m_keepPercentWithSpecials, 0, wxALL, 5);
  vsizer->Add(m_enterEvaluates, 0, wxALL, 5);
  vsizer->Add(m_saveUntitled, 0, wxALL, 5);
  vsizer->Add(m_openHCaret, 0, wxALL, 5);
  vsizer->Add(m_insertAns, 0, wxALL, 5);
  vsizer->Add(m_fixReorderedIndices, 0, wxALL, 5);

  vsizer->AddGrowableRow(10);
  panel->SetSizer(vsizer);
  vsizer->Fit(panel);

  return panel;
}

wxPanel* Config::CreateMaximaPanel()
{
  wxPanel* panel = new wxPanel(m_notebook, -1);

  wxFlexGridSizer* sizer = new wxFlexGridSizer(5, 2, 0, 0);

  wxStaticText *mp = new wxStaticText(panel, -1, _("Maxima program:"));
  m_maximaProgram = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  m_mpBrowse = new wxButton(panel, wxID_OPEN, _("Open"));
  wxStaticText *ap = new wxStaticText(panel, -1, _("Additional parameters:"));
  m_additionalParameters = new wxTextCtrl(panel, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);

  sizer->Add(mp, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(10, 10);
  sizer->Add(m_maximaProgram, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(m_mpBrowse, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(10, 10);
  sizer->Add(10, 10);
  sizer->Add(ap, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer->Add(10, 10);
  sizer->Add(m_additionalParameters, 0, wxALL, 5);

  panel->SetSizer(sizer);
  sizer->Fit(panel);

  return panel;
}

wxPanel* Config::CreateStylePanel()
{
  wxPanel *panel = new wxPanel(m_notebook, -1);

  wxStaticBox* fonts = new wxStaticBox(panel, -1, _("Fonts"));
  wxStaticBox* styles = new wxStaticBox(panel, -1, _("Styles"));

  wxFlexGridSizer* vsizer = new wxFlexGridSizer(3,1,5,5);
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(3, 2, 2, 2);
  wxStaticBoxSizer* sb_sizer_1 = new wxStaticBoxSizer(fonts, wxVERTICAL);
  wxStaticBoxSizer* sb_sizer_2 = new wxStaticBoxSizer(styles, wxVERTICAL);
  wxBoxSizer* hbox_sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* hbox_sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* hbox_sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* vbox_sizer = new wxBoxSizer(wxVERTICAL);

  wxStaticText* df = new wxStaticText(panel, -1, _("Default font:"));
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
      _("Highlight (dpart)"),
      _("Text cell"),
      _("Subsection cell"),
      _("Section cell"),
      _("Title cell"),
      _("Text cell background"),
      _("Document background"),
      _("Cell bracket"),
      _("Active cell bracket"),
      _("Cursor"),
      _("Selection"),
      _("Outdated cells")
    };
  m_styleFor = new wxListBox(panel, listbox_styleFor, wxDefaultPosition, wxSize(200, -1), 23, m_styleFor_choices, wxLB_SINGLE);
  m_getStyleFont = new wxButton(panel, style_font_family, _("Choose font"), wxDefaultPosition, wxSize(150, -1));
#ifndef __WXMSW__
  m_styleColor = new ColorPanel(this, panel, color_id, wxDefaultPosition, wxSize(150, 30), wxSUNKEN_BORDER | wxFULL_REPAINT_ON_RESIZE);
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

  vbox_sizer->Add(m_styleColor, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vbox_sizer->Add(m_getStyleFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_1->Add(m_boldCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_1->Add(m_italicCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  hbox_sizer_1->Add(m_underlinedCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  vbox_sizer->Add(hbox_sizer_1, 1, wxALL | wxEXPAND, 0);
  vbox_sizer->Add(m_examplePanel, 0, wxALL | wxEXPAND | wxALIGN_CENTER_HORIZONTAL, 5);
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

void Config::OnClose(wxCloseEvent& event)
{
#if defined __WXMAC__
  EndModal(wxID_OK);
#else
  EndModal(wxID_CANCEL);
#endif
}

void Config::WriteSettings()
{
  int i = 0;
  wxString search = wxT("maxima-htmldir");
  wxArrayString out;
  wxString maxima = m_maximaProgram->GetValue();
  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Write(wxT("maxima"), m_maximaProgram->GetValue());
  config->Write(wxT("parameters"), m_additionalParameters->GetValue());
  config->Write(wxT("fontSize"), m_fontSize);
  config->Write(wxT("mathFontsize"), m_mathFontSize);
  config->Write(wxT("matchParens"), m_matchParens->GetValue());
  config->Write(wxT("showLong"), m_showLong->GetValue());
  config->Write(wxT("fixedFontTC"), m_fixedFontInTC->GetValue());
  config->Write(wxT("changeAsterisk"), m_changeAsterisk->GetValue());
  config->Write(wxT("enterEvaluates"), m_enterEvaluates->GetValue());
  config->Write(wxT("saveUntitled"), m_saveUntitled->GetValue());
  config->Write(wxT("openHCaret"), m_openHCaret->GetValue());
  config->Write(wxT("insertAns"), m_insertAns->GetValue());
  config->Write(wxT("fixReorderedIndices"), m_fixReorderedIndices->GetValue());
  config->Write(wxT("defaultPort"), m_defaultPort->GetValue());
  config->Write(wxT("AUI/savePanes"), m_savePanes->GetValue());
  config->Write(wxT("usejsmath"), m_useJSMath->GetValue());
  config->Write(wxT("keepPercent"), m_keepPercentWithSpecials->GetValue());
  if (m_saveSize->GetValue())
    config->Write(wxT("pos-restore"), 1);
  else
    config->Write(wxT("pos-restore"), 0);
  i = m_language->GetSelection();
  if (i > -1 && i < LANGUAGE_NUMBER)
    config->Write(wxT("language"), langs[i]);

  WriteStyles();
  config->Flush();
}

void Config::OnMpBrowse(wxCommandEvent& event)
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
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
    if (file.Right(8) == wxT("wxmaxima") || file.Right(12) == wxT("wxmaxima.exe") ||
        file.Right(12) == wxT("wxMaxima.exe"))
      wxMessageBox(_("Invalid entry for Maxima program.\n\nPlease enter the path to Maxima program again."),
                   _("Error"),
                   wxOK|wxICON_ERROR);
    else
      m_maximaProgram->SetValue(file);
  }
}

void Config::OnMathBrowse(wxCommandEvent& event)
{
  wxFont math;
  math = wxGetFontFromUser(this, wxFont(m_mathFontSize, wxFONTFAMILY_DEFAULT,
                                        wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
                                        false, m_mathFontName));

  if (math.Ok())
  {
    m_mathFontName = math.GetFaceName();
    m_mathFontSize = math.GetPointSize();
    m_getMathFont->SetLabel(m_mathFontName + wxString::Format(wxT(" (%d)"), m_mathFontSize));
    UpdateExample();
  }
}

void Config::OnChangeFontFamily(wxCommandEvent& event)
{
  wxFont font;
  int fontsize = m_fontSize;
  style *tmp = GetStylePointer();
  wxString fontName;

  if (tmp == &m_styleText || tmp == &m_styleTitle || tmp == &m_styleSubsection || tmp == &m_styleSection)
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

void Config::ReadStyles(wxString file)
{
  wxConfigBase* config;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else {
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
  m_fontEncoding = (wxFontEncoding)encoding;

  m_mathFontName = wxEmptyString;
  config->Read(wxT("Style/Math/fontname"), &m_mathFontName);
  if (m_mathFontName.Length() > 0)
    m_getMathFont->SetLabel(m_mathFontName + wxString::Format(wxT(" (%d)"), m_mathFontSize));

  wxString tmp;
  // Document background color
  m_styleBackground.color = wxT("white");
  if (config->Read(wxT("Style/Background/color"),
                   &tmp)) m_styleBackground.color.Set(tmp);

  // Text background
  m_styleTextBackground.color = wxT("light blue");
  if (config->Read(wxT("Style/TextBackground/color"),
                   &tmp)) m_styleTextBackground.color.Set(tmp);

  // Highlighting color
  m_styleHighlight.color = wxT("red");
  if (config->Read(wxT("Style/Highlight/color"),
                   &tmp)) m_styleHighlight.color.Set(tmp);

  // Groupcell bracket color
  m_styleCellBracket.color = wxT("rgb(0,0,0)");
  if (config->Read(wxT("Style/CellBracket/color"),
                   &tmp)) m_styleCellBracket.color.Set(tmp);

  // Active groupcell bracket color
  m_styleActiveCellBracket.color = wxT("rgb(255,0,0)");
  if (config->Read(wxT("Style/ActiveCellBracket/color"),
                   &tmp)) m_styleActiveCellBracket.color.Set(tmp);

  // Horizontal caret color/ cursor color
  m_styleCursor.color = wxT("rgb(0,0,0)");
  if (config->Read(wxT("Style/Cursor/color"),
                   &tmp)) m_styleCursor.color.Set(tmp);

  // Outdated cells
  m_styleOutdated.color = wxT("rgb(153,153,153)");
  if (config->Read(wxT("Style/Outdated/color"),
                   &tmp)) m_styleOutdated.color.Set(tmp);

  // Selection color defaults to light grey on windows
#if defined __WXMSW__
  m_styleSelection.color = wxColour(wxT("light grey"));
#else
  m_styleSelection.color = wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT);
#endif
  if (config->Read(wxT("Style/Selection/color"),
                   &tmp)) m_styleSelection.color.Set(tmp);

#define READ_STYLE(style, where)        \
  if (config->Read(wxT(where "color"), &tmp)) style.color.Set(tmp); \
  config->Read(wxT(where "bold"),       \
               &style.bold);            \
  config->Read(wxT(where "italic"),     \
               &style.italic);          \
  config->Read(wxT(where "underlined"), \
               &style.underlined);

  // Text in math output
  m_styleDefault.color = wxT("black");
  m_styleDefault.bold = false;
  m_styleDefault.italic = false;
  m_styleDefault.underlined = false;
  READ_STYLE(m_styleDefault, "Style/NormalText/")

  // Main prompt
  m_styleMainPrompt.color = wxT("red");
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
  m_styleLabel.color = wxT("brown");
  m_styleLabel.bold = false;
  m_styleLabel.italic = false;
  m_styleLabel.underlined = false;
  READ_STYLE(m_styleLabel, "Style/Label/")

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
  m_styleText.bold = false;
  m_styleText.italic = false;
  m_styleText.underlined = false;
  m_styleText.font = m_styleDefault.font;
  m_styleText.fontSize = m_fontSize;
  config->Read(wxT("Style/Text/fontsize"),
                 &m_styleText.fontSize);
  config->Read(wxT("Style/Text/fontname"),
               &m_styleText.font);
  READ_STYLE(m_styleText, "Style/Text/")

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
    delete config;
}

void Config::WriteStyles(wxString file)
{
  wxConfigBase* config;
  if (file == wxEmptyString)
    config = wxConfig::Get();
  else {
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
  config->Write(wxT("Style/Outdated/color"),
                m_styleOutdated.color.GetAsString(wxC2S_CSS_SYNTAX));

  config->Write(wxT("Style/fontname"), m_styleDefault.font);
  config->Write(wxT("fontEncoding"), (int)m_fontEncoding);

  config->Write(wxT("Style/Math/fontname"), m_mathFontName);

#define WRITE_STYLE(style, where)                   \
  config->Write(wxT(where "color"), style.color.GetAsString(wxC2S_CSS_SYNTAX));   \
  config->Write(wxT(where "bold"), style.bold);     \
  config->Write(wxT(where "italic"), style.italic); \
  config->Write(wxT(where "underlined"), style.underlined);

  // Normal text
  WRITE_STYLE(m_styleDefault, "Style/NormalText/")

  // Main prompt
  WRITE_STYLE(m_styleMainPrompt, "Style/MainPrompt/")

  // Other prompt
  WRITE_STYLE(m_styleOtherPrompt, "Style/OtherPrompt/")

  // Label
  WRITE_STYLE(m_styleLabel, "Style/Label/")

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

  // Section
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
    ((wxFileConfig *)config)->Save(str);
    delete config;
  }
}

void Config::OnChangeColor()
{
  style* tmp = GetStylePointer();
  wxColour col = wxGetColourFromUser(this, tmp->color);
  if (col.IsOk())
  {
    tmp->color = col.GetAsString(wxC2S_CSS_SYNTAX);
    UpdateExample();
    m_styleColor->SetBackgroundColour(tmp->color);
  }
}

void Config::OnChangeStyle(wxCommandEvent& event)
{
  style* tmp = GetStylePointer();
  int st = m_styleFor->GetSelection();

  m_styleColor->SetBackgroundColour(tmp->color);

  if (st >= 12 && st <= 15)
    m_getStyleFont->Enable(true);
  else
    m_getStyleFont->Enable(false);

  // Background color only
  if (st >= 16)
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
  UpdateExample();
}

void Config::OnCheckbox(wxCommandEvent& event)
{
  style* tmp = GetStylePointer();

  tmp->bold = m_boldCB->GetValue();
  tmp->italic = m_italicCB->GetValue();
  tmp->underlined = m_underlinedCB->GetValue();

  UpdateExample();
}

void Config::OnChangeWarning(wxCommandEvent &event)
{
  wxMessageBox(_("Please restart wxMaxima for changes to take effect!"),
               _("Configuration warning"),
               wxOK|wxICON_WARNING);
}

//
// Should match whatever is put in m_styleFor
//
style* Config::GetStylePointer()
{
  style* tmp = &m_styleDefault;
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
    tmp = &m_styleHighlight;
    break;
  case 12:
    tmp = &m_styleText;
    break;
  case 13:
    tmp = &m_styleSubsection;
    break;
  case 14:
    tmp = &m_styleSection;
    break;
  case 15:
    tmp = &m_styleTitle;
    break;
  case 16:
    tmp = &m_styleTextBackground;
    break;
  case 17:
    tmp = &m_styleBackground;
    break;
  case 18:
    tmp = &m_styleCellBracket;
    break;
  case 19:
    tmp = &m_styleActiveCellBracket;
    break;
  case 20:
    tmp = &m_styleCursor;
    break;
  case 21:
    tmp = &m_styleSelection;
    break;
  case 22:
    tmp = &m_styleOutdated;
    break;
  }
  return tmp;
}

void Config::UpdateExample()
{
  style *tmp = GetStylePointer();
  wxString example = _("Example text");
  wxColour color(tmp->color);
  wxString font(m_styleDefault.font);

  if (tmp == &m_styleBackground)
      color = m_styleInput.color;

  int fontsize = m_fontSize;
  if (tmp == &m_styleText || tmp == &m_styleSubsection || tmp == &m_styleSection || tmp == &m_styleTitle)
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

  if (tmp == &m_styleTextBackground) {
    m_examplePanel->SetFontSize(m_styleText.fontSize);
    m_examplePanel->SetStyle(m_styleText.color, m_styleText.italic, m_styleText.bold, m_styleText.underlined, m_styleText.font);
  }
  else {
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

void Config::LoadSave(wxCommandEvent& event)
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
  else {
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
void Config::OnColorButton(wxCommandEvent &event)
{
  OnChangeColor();
}
#endif

BEGIN_EVENT_TABLE(Config, wxPropertySheetDialog)
  EVT_BUTTON(wxID_OPEN, Config::OnMpBrowse)
  EVT_BUTTON(button_mathFont, Config::OnMathBrowse)
  EVT_BUTTON(font_family, Config::OnChangeFontFamily)
#if defined __WXMSW__
  EVT_BUTTON(color_id, Config::OnColorButton)
#endif
  EVT_LISTBOX(listbox_styleFor, Config::OnChangeStyle)
  EVT_COMBOBOX(language_id, Config::OnChangeWarning)
  EVT_CHECKBOX(checkbox_bold, Config::OnCheckbox)
  EVT_CHECKBOX(checkbox_italic, Config::OnCheckbox)
  EVT_CHECKBOX(checkbox_underlined, Config::OnCheckbox)
  EVT_BUTTON(save_id, Config::LoadSave)
  EVT_BUTTON(load_id, Config::LoadSave)
  EVT_BUTTON(style_font_family, Config::OnChangeFontFamily)
  EVT_CLOSE(Config::OnClose)
END_EVENT_TABLE()


void ExamplePanel::OnPaint(wxPaintEvent& event)
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
  dc.SetFont(wxFont(m_size, wxFONTFAMILY_MODERN, italic, bold, underlined, m_font));
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
