///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
///            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
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
    wxLANGUAGE_CHINESE_TRADITIONAL,
    wxLANGUAGE_CZECH,
    wxLANGUAGE_DANISH,
    wxLANGUAGE_ENGLISH,
    wxLANGUAGE_FRENCH,
    wxLANGUAGE_GERMAN,
    wxLANGUAGE_GREEK,
    wxLANGUAGE_HUNGARIAN,
    wxLANGUAGE_ITALIAN,
    wxLANGUAGE_POLISH,
    wxLANGUAGE_PORTUGUESE_BRAZILIAN,
    wxLANGUAGE_RUSSIAN,
    wxLANGUAGE_SPANISH,
    wxLANGUAGE_UKRAINIAN
  };

#define LANGUAGE_NUMBER 15

Config::Config(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  int defaultPort = 4010;
  wxConfig::Get()->Read(wxT("defaultPort"), &defaultPort);

  notebook_1 = new wxNotebook(this, -1, wxDefaultPosition, wxDefaultSize, 0);
  notebook_1_pane_2 = new wxPanel(notebook_1, -1);
  notebook_1_pane_1 = new wxPanel(notebook_1, -1);
  sizer_6_staticbox = new wxStaticBox(notebook_1_pane_1, -1, _("wxMaxima options"));
  sizer_4_staticbox = new wxStaticBox(notebook_1_pane_1, -1, _("Maxima options"));
  sizer_9_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Fonts"));
  sizer_11_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Styles"));
  label_5 = new wxStaticText(notebook_1_pane_1, -1, _("Maxima program:"));
  m_maximaProgram = new wxTextCtrl(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  m_mpBrowse = new wxButton(notebook_1_pane_1, wxID_OPEN, _("Open"));
  label_6 = new wxStaticText(notebook_1_pane_1, -1, _("Additional parameters:"));
  m_additionalParameters = new wxTextCtrl(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  label_4 = new wxStaticText(notebook_1_pane_1, -1, _("Language:"));
  const wxString m_language_choices[] =
    {
      _("(Use default language)"),
      _("Chinese traditional"),
      _("Czech"),
      _("Danish"),
      _("English"),
      _("French"),
      _("German"),
      _("Greek"),
      _("Hungarian"),
      _("Italian"),
      _("Polish"),
      _("Portuguese (Brazilian)"),
      _("Russian"),
      _("Spanish"),
      _("Ukrainian")
    };
  m_language = new wxComboBox(notebook_1_pane_1, language_id, wxEmptyString, wxDefaultPosition, wxSize(230, -1), LANGUAGE_NUMBER, m_language_choices, wxCB_DROPDOWN | wxCB_READONLY);
  label_12 = new wxStaticText(notebook_1_pane_1, -1, _("Default port:"));
  m_defaultPort = new wxSpinCtrl(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(70, -1), wxSP_ARROW_KEYS, 50, 5000, defaultPort);
  m_defaultPort->SetValue(defaultPort);
  m_saveSize = new wxCheckBox(notebook_1_pane_1, -1, _("Save wxMaxima window size/position"));
  m_savePanes = new wxCheckBox(notebook_1_pane_1, -1, _("Save panes layout"));
  m_matchParens = new wxCheckBox(notebook_1_pane_1, -1, _("Match parenthesis in text controls"));
  m_fixedFontInTC = new wxCheckBox(notebook_1_pane_1, -1, _("Fixed font in text controls"));
  m_showLong = new wxCheckBox(notebook_1_pane_1, -1, _("Show long expressions"));
  m_changeAsterisk = new wxCheckBox(notebook_1_pane_1, -1, _("Use centered dot character for multiplication"));
  m_enterEvaluates = new wxCheckBox(notebook_1_pane_1, -1, _("Enter evaluates cells"));
  label_8 = new wxStaticText(notebook_1_pane_2, -1, _("Default font:"));
  m_getFont = new wxButton(notebook_1_pane_2, font_family, _("Choose font"), wxDefaultPosition, wxSize(250, -1));
  m_mathFont = new wxStaticText(notebook_1_pane_2, -1, _("Math font:"));
  m_getMathFont = new wxButton(notebook_1_pane_2, button_mathFont, _("Choose font"), wxDefaultPosition, wxSize(250, -1));
  m_useJSMath = new wxCheckBox(notebook_1_pane_2, -1, _("Use jsMath fonts"));
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
  m_styleFor = new wxListBox(notebook_1_pane_2, listbox_styleFor, wxDefaultPosition, wxSize(200, -1), 23, m_styleFor_choices, wxLB_SINGLE);
  m_getStyleFont = new wxButton(notebook_1_pane_2, style_font_family, _("Choose font"), wxDefaultPosition, wxSize(150, -1));
#ifndef __WXMSW__
  m_styleColor = new ColorPanel(this, notebook_1_pane_2, color_id, wxDefaultPosition, wxSize(150, 30), wxSUNKEN_BORDER | wxFULL_REPAINT_ON_RESIZE);
#else
  m_styleColor = new wxButton(notebook_1_pane_2, color_id, wxEmptyString, wxDefaultPosition, wxSize(150, -1));
#endif
  m_boldCB = new wxCheckBox(notebook_1_pane_2, checkbox_bold, _("Bold"));
  m_italicCB = new wxCheckBox(notebook_1_pane_2, checkbox_italic, _("Italic"));
  m_underlinedCB = new wxCheckBox(notebook_1_pane_2, checkbox_underlined, _("Underlined"));
  label_11 = new ExamplePanel(notebook_1_pane_2, -1, wxDefaultPosition, wxSize(350, 60));
  m_loadStyle = new wxButton(notebook_1_pane_2, load_id, _("Load"));
  m_saveStyle = new wxButton(notebook_1_pane_2, save_id, _("Save"));
#if defined __WXMSW__
  m_button1 = new wxButton(this, wxID_OK, _("OK"));
  m_button2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  m_button1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  m_button2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
  UpdateExample();
}

void Config::set_properties()
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
  bool fixedFontTC = true, changeAsterisk = false, usejsmath = true;
  bool enterEvaluates = false;
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
  config->Read(wxT("usejsmath"), &usejsmath);

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
  m_fixedFontInTC->SetValue(fixedFontTC);
  m_useJSMath->SetValue(usejsmath);

  m_getStyleFont->Enable(false);

#if defined __WXMSW__
  m_button1->SetDefault();
#else
  m_button2->SetDefault();
#endif

  if (!wxFontEnumerator::IsValidFacename(wxT("jsMath-cmex10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmsy10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmr10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmmi10")) ||
      !wxFontEnumerator::IsValidFacename(wxT("jsMath-cmti10")))
    m_useJSMath->Enable(false);

  ReadStyles();
}


void Config::do_layout()
{
  // begin wxGlade: Config::do_layout
  wxFlexGridSizer* sizer_1 = new wxFlexGridSizer(5, 1, 0, 0);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* sizer_8 = new wxFlexGridSizer(4, 1, 3, 3);
  wxStaticBoxSizer* sizer_11 = new wxStaticBoxSizer(sizer_11_staticbox, wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_4 = new wxFlexGridSizer(4, 1, 2, 7);
  wxStaticBoxSizer* sizer_9 = new wxStaticBoxSizer(sizer_9_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(2, 2, 2, 2);
  wxFlexGridSizer* sizer_3 = new wxFlexGridSizer(2, 1, 3, 3);
  wxStaticBoxSizer* sizer_6 = new wxStaticBoxSizer(sizer_6_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_5 = new wxFlexGridSizer(2, 2, 2, 2);
  wxStaticBoxSizer* sizer_4 = new wxStaticBoxSizer(sizer_4_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(2, 3, 3, 3);
  wxBoxSizer* sizer_5 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer* sizer_10 = new wxBoxSizer(wxHORIZONTAL);

  // TAB 1
  // Maxima options box
  grid_sizer_2->Add(label_5, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_maximaProgram, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_mpBrowse, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(label_6, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_additionalParameters, 0, wxALL, 3);
  sizer_4->Add(grid_sizer_2, 1, wxALL | wxEXPAND, 3);
  sizer_3->Add(sizer_4, 1, wxALL | wxEXPAND, 3);

  // wxMaxima options box
  grid_sizer_5->Add(label_4, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(m_language, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(label_12, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(m_defaultPort, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_6->Add(grid_sizer_5, 1, wxEXPAND, 0);
  sizer_6->Add(m_saveSize, 0, wxALL, 3);
  sizer_6->Add(m_savePanes, 0, wxALL, 3);
  sizer_6->Add(m_matchParens, 0, wxALL, 3);
  sizer_6->Add(m_fixedFontInTC, 0, wxALL, 3);
  sizer_6->Add(m_showLong, 0, wxALL, 3);
  sizer_6->Add(m_changeAsterisk, 0, wxALL, 3);
  sizer_6->Add(m_enterEvaluates, 0, wxALL, 3);
  sizer_3->Add(sizer_6, 1, wxALL | wxEXPAND, 3);

  notebook_1_pane_1->SetAutoLayout(true);
  notebook_1_pane_1->SetSizer(sizer_3);
  sizer_3->Fit(notebook_1_pane_1);
  sizer_3->SetSizeHints(notebook_1_pane_1);
  sizer_3->AddGrowableCol(0);

  // TAB 2
  // Font box
  grid_sizer_1->Add(label_8, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_getFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_mathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_getMathFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(10, 10);
  grid_sizer_1->Add(m_useJSMath, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_9->Add(grid_sizer_1, 1, wxALL | wxEXPAND, 3);
  sizer_8->Add(sizer_9, 1, wxALL | wxEXPAND, 3);

  // Styles box
  grid_sizer_4->Add(m_styleColor, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(m_getStyleFont, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_boldCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_italicCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_underlinedCB, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(sizer_5, 1, wxALL | wxEXPAND, 3);
  grid_sizer_4->Add(label_11, 0, wxALL | wxEXPAND | wxALIGN_CENTER_HORIZONTAL, 3); // example panel

  sizer_11->Add(m_styleFor, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_11->Add(grid_sizer_4, 1, wxALL | wxEXPAND, 3);
  sizer_8->Add(sizer_11, 1, wxALL | wxEXPAND, 3);
  // load save buttons
  sizer_10->Add(m_loadStyle, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_10->Add(m_saveStyle, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
  sizer_8->Add(sizer_10, 1, wxALIGN_RIGHT, 3);

  // Tab 2
  notebook_1_pane_2->SetAutoLayout(true);
  notebook_1_pane_2->SetSizer(sizer_8);
  sizer_8->Fit(notebook_1_pane_2);
  sizer_8->SetSizeHints(notebook_1_pane_2);
  sizer_8->AddGrowableCol(0);

  // Add tabs to notebook and
  notebook_1->AddPage(notebook_1_pane_1, _("Options"));
  notebook_1->AddPage(notebook_1_pane_2, _("Style"));

  // Add notebook to dialog
  sizer_1->Add(notebook_1, 1, wxEXPAND | wxALL, 2);

  // OK and cancel buttons
  sizer_2->Add(m_button1, 0, wxALL, 5);
  sizer_2->Add(m_button2, 0, wxALL, 5);
  sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT, 0);

  SetAutoLayout(true);
  SetSizer(sizer_1);
  sizer_1->Fit(this);
  sizer_1->SetSizeHints(this);
  sizer_1->AddGrowableRow(1);
  sizer_1->AddGrowableCol(0);
  Layout();
  // end wxGlade
}

void Config::OnOk(wxCommandEvent& event)
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
  config->Write(wxT("defaultPort"), m_defaultPort->GetValue());
  config->Write(wxT("AUI/savePanes"), m_savePanes->GetValue());
  config->Write(wxT("usejsmath"), m_useJSMath->GetValue());
  if (m_saveSize->GetValue())
    config->Write(wxT("pos-restore"), 1);
  else
    config->Write(wxT("pos-restore"), 0);
  i = m_language->GetSelection();
  if (i > -1 && i < LANGUAGE_NUMBER)
  {
    config->Write(wxT("language"), langs[i]);
  }

  WriteStyles();
  config->Flush();

  EndModal(wxID_OK);
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
  math = wxGetFontFromUser(this, wxFont(m_mathFontSize, wxNORMAL, wxNORMAL, wxNORMAL,
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
                                        wxNORMAL, wxNORMAL, wxNORMAL,
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
  m_styleVariable.color = m_styleVariable.color;
  m_styleVariable.bold = false;
  m_styleVariable.italic = true;
  m_styleVariable.underlined = false;
  READ_STYLE(m_styleVariable, "Style/Variable/")

  // Function
  m_styleFunction.color = m_styleVariable.color;
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
  m_boldCB->SetValue(m_styleVariable.bold);
  m_italicCB->SetValue(m_styleVariable.italic);
  m_underlinedCB->SetValue(m_styleVariable.underlined);

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
      label_11->SetFontSize(m_styleText.fontSize);
      label_11->SetStyle(m_styleText.color, m_styleText.italic, m_styleText.bold, m_styleText.underlined, m_styleText.font); }
  else {
      label_11->SetFontSize(fontsize);
      label_11->SetStyle(color, tmp->italic, tmp->bold, tmp->underlined, font);
  }

  if (tmp == &m_styleTextBackground ||
      tmp == &m_styleText)
    label_11->SetBackgroundColour(m_styleTextBackground.color);
  else
    label_11->SetBackgroundColour(m_styleBackground.color);

  label_11->Refresh();
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

BEGIN_EVENT_TABLE(Config, wxDialog)
  EVT_BUTTON(wxID_OK, Config::OnOk)
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
END_EVENT_TABLE()


void ExamplePanel::OnPaint(wxPaintEvent& event)
{
  wxString example(_("Example text"));
  wxPaintDC dc(this);
  int panel_width, panel_height;
  int text_width, text_height;
  int bold = wxNORMAL, italic = wxNORMAL, underlined = 0;

  GetClientSize(&panel_width, &panel_height);

  dc.SetTextForeground(m_fgColor);

  if (m_bold)
    bold = wxBOLD;
  if (m_italic)
    italic = wxSLANT;
  if (m_underlined)
    underlined = 1;
  dc.SetFont(wxFont(m_size, wxMODERN, italic, bold, underlined, m_font));
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
