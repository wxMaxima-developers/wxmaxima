/*
 *  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "Config.h"

#include <wx/config.h>
#include <wx/fontenum.h>
#include <wx/fontdlg.h>

// Should match whatever  is put in the m_language
const int langs[] =
{
  wxLANGUAGE_DEFAULT,
  wxLANGUAGE_ENGLISH,
  wxLANGUAGE_FRENCH,
  wxLANGUAGE_ITALIAN,
  wxLANGUAGE_SPANISH
};

#define LANGUAGE_NUMBER 5

const wxString colorlist[] = {
  wxT("aquamarine"), wxT("black"), wxT("blue"), wxT("blue violet"),
  wxT("brown"),  wxT("cadet blue"),  wxT("coral"), wxT("cornflower blue"),
  wxT("cyan"), wxT("dark grey"), wxT("dark green"), wxT("dark olive green"),
  wxT("dark orchid"), wxT("dark slate blue"), wxT("dark slate grey"),
  wxT("dark turquoise"), wxT("dim grey"), wxT("firebrick"), wxT("forest green"),
  wxT("gold"), wxT("goldenrod"), wxT("grey"), wxT("green"), wxT("green yellow"),
  wxT("indian red"), wxT("khaki"), wxT("light blue"), wxT("light grey"),
  wxT("light steel blue"), wxT("lime green"), wxT("magenta"), wxT("maroon"),
  wxT("medium aquamarine"), wxT("medium blue"), wxT("medium forrest green"),
  wxT("medium goldenrod"), wxT("medium orchid"), wxT("medium sea green"),
  wxT("medium slate blue"), wxT("medium spring green"), wxT("medium turquoise"),
  wxT("medium violet red"), wxT("midnight blue"), wxT("navy"), wxT("orange"),
  wxT("orange red"), wxT("orchid"), wxT("pale green"), wxT("pink"), wxT("plum"),
  wxT("purple"), wxT("red"), wxT("salmon"), wxT("sea green"), wxT("sienna"),
  wxT("sky blue"), wxT("slate blue"), wxT("spring green"), wxT("steel blue"),
  wxT("tan"), wxT("thistle"), wxT("turquoise"), wxT("violet"),
  wxT("violet red"), wxT("wheat"), wxT("white"), wxT("yellow"),
  wxT("yellow green")
};

#define COLORLIST_LENGTH 68

Config::Config(wxWindow* parent, int id, const wxString& title,
               const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
  // begin wxGlade: Config::Config
  notebook_1 = new wxNotebook(this, -1, wxDefaultPosition, wxDefaultSize, 0);
  notebook_1_pane_2 = new wxPanel(notebook_1, -1);
  notebook_1_pane_1 = new wxPanel(notebook_1, -1);
  sizer_6_staticbox = new wxStaticBox(notebook_1_pane_1, -1, _("wxMaxima options"));
  sizer_4_staticbox = new wxStaticBox(notebook_1_pane_1, -1, _("Maxima options"));
  sizer_9_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Font"));
  sizer_12_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Greek font"));
  sizer_11_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Styles"));
  label_1 = new wxStaticText(this, -1, _("wxMaxima configuration"));
  label_5 = new wxStaticText(notebook_1_pane_1, -1, _("Maxima program:"));
  m_maximaProgram = new wxTextCtrl(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  m_mpBrowse = new wxButton(notebook_1_pane_1, wxID_OPEN, _("Open"));
  label_6 = new wxStaticText(notebook_1_pane_1, -1, _("Additional parameters:"));
  m_additionalParameters = new wxTextCtrl(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(250, -1), wxTE_RICH);
  label_4 = new wxStaticText(notebook_1_pane_1, -1, _("Language:"));
  const wxString m_language_choices[] = {
    _("(Use default language)"), _("English"),
    _("French"),  _("Italian"), _("Spanish")
  };
  m_language = new wxComboBox(notebook_1_pane_1, -1, wxEmptyString, wxDefaultPosition, wxSize(230, -1), 5, m_language_choices, wxCB_DROPDOWN|wxCB_READONLY);
  label_9 = new wxStaticText(notebook_1_pane_1, -1, _("Button panel:"));
  const wxString m_panelSize_choices[] = {
    _("Off"), _("Basic"), _("Full")
  };
  m_panelSize = new wxComboBox(notebook_1_pane_1, panel_size, wxEmptyString, wxDefaultPosition, wxSize(230, -1), 3, m_panelSize_choices, wxCB_DROPDOWN|wxCB_READONLY);
  m_saveSize = new wxCheckBox(notebook_1_pane_1, -1, _("Save wxMaxima window size/position"));
  m_matchParens = new wxCheckBox(notebook_1_pane_1, -1, _("Match parenthesis in text controls"));
  m_fixedFontInTC = new wxCheckBox(notebook_1_pane_1, -1, _("Fixed font in text controls"));
  m_showLong = new wxCheckBox(notebook_1_pane_1, -1, _("Show long expressions"));
  m_showHeader = new wxCheckBox(notebook_1_pane_1, -1, _("Show maxima header"));
  label_7 = new wxStaticText(notebook_1_pane_2, -1, _("Font size:"));
  m_fontSize = new wxSpinCtrl(notebook_1_pane_2, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100);
  label_8 = new wxStaticText(notebook_1_pane_2, -1, _("Font family:"));
  const wxString m_fontFamily_choices[] = {

  };
  m_fontFamily = new wxComboBox(notebook_1_pane_2, font_family, wxEmptyString, wxDefaultPosition, wxSize(230, -1), 0, m_fontFamily_choices, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  SetupFontList();
  m_symbolFontOk = new wxCheckBox(notebook_1_pane_2, checkbox_symbol, _("Use greek font"));
  m_getSymbolFont = new wxButton(notebook_1_pane_2, button_symbol, _("Choose font"), wxDefaultPosition, wxSize(250, -1));
  label_10 = new wxStaticText(notebook_1_pane_2, -1, _("Adjustment:"));
  m_symbolFontAdj = new wxSpinCtrl(notebook_1_pane_2, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, -4, 4);
  const wxString m_styleFor_choices[] = {
    _("Variables"), _("Numbers"),  _("Special constants"), _("Greek constants"),
    _("Strings"), _("Text"), _("Input"), _("Main prompts"),
    _("Other prompts"), _("Labels"), _("Hidden groups"), _("Highilight"), _("Background")

  };
  m_styleFor = new wxComboBox(notebook_1_pane_2, combobox_styleFor, wxEmptyString, wxDefaultPosition, wxSize(150, -1), 13, m_styleFor_choices, wxCB_DROPDOWN|wxCB_READONLY);
  const wxString m_styleColor_choices[] = {
    _("aquamarine"), _("black"), _("blue"), _("blue violet"),
    _("brown"), _("cadet blue"), _("coral"), _("cornflower blue"),
    _("cyan"), _("dark grey"), _("dark green"), _("dark olive green"),
    _("dark orchid"), _("dark slate blue"), _("dark slate grey"),
    _("dark turquoise"), _("dim grey"), _("firebrick"), _("forest green"),
    _("gold"), _("goldenrod"), _("grey"), _("green"), _("green yellow"),
    _("indian red"), _("khaki"), _("light blue"), _("light grey"),
    _("light steel blue"), _("lime green"), _("magenta"), _("maroon"),
    _("medium aquamarine"), _("medium blue"), _("medium forrest green"),
    _("medium goldenrod"), _("medium orchid"), _("medium sea green"),
    _("medium slate blue"), _("medium spring green"), _("medium turquoise"),
    _("medium violet red"), _("midnight blue"), _("navy"), _("orange"),
    _("orange red"), _("orchid"), _("pale green"), _("pink"), _("plum"),
    _("purple"), _("red"), _("salmon"), _("sea green"), _("sienna"),
    _("sky blue"), _("slate blue"), _("spring green"), _("steel blue"),
    _("tan"), _("thistle"), _("turquoise"), _("violet"), _("violet red"),
    _("wheat"), _("white"), _("yellow"), _("yellow green")
  };
  m_styleColor = new wxComboBox(notebook_1_pane_2, combobox_colour, wxEmptyString, wxDefaultPosition, wxSize(150, -1), 68, m_styleColor_choices, wxCB_DROPDOWN|wxCB_READONLY);
  m_boldCB = new wxCheckBox(notebook_1_pane_2, checkbox_bold, _("Bold"));
  m_italicCB = new wxCheckBox(notebook_1_pane_2, checkbox_italic, _("Italic"));
  m_underlinedCB = new wxCheckBox(notebook_1_pane_2, checkbox_underlined, _("Underlined"));
  label_11 = new ExamplePanel(notebook_1_pane_2, -1, wxDefaultPosition, wxDefaultSize);
#if defined __WXMSW__
  m_button1 = new wxButton(this, wxID_OK, _("OK"));
  m_button2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  m_button1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  m_button2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
  // end wxGlade
  UpdateExample();
}


void Config::set_properties()
{
  // begin wxGlade: Config::set_properties
  SetTitle(_("wxMaxima configuration"));
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxEmptyString));
  // end wxGlade

  m_maximaProgram->SetToolTip(_("Enter the path to the maxima executable."));
  m_additionalParameters->SetToolTip(_("Additional parameters for maxima"
                                       " (e.g. -l clisp)."));
  m_saveSize->SetToolTip(_("Save wxMaxima window size/position between sessions."));
  m_fontSize->SetToolTip(_("Font size in console window."));
  m_matchParens->SetToolTip(_("Write matching parenthesis in text controls."));
  m_showLong->SetToolTip(_("Show long expressions in wxMaxima console."));
  m_language->SetToolTip(_("Language used for wxMaxima GUI."));
  m_showHeader->SetToolTip(_("Show initial header with maxima system information."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_fixedFontInTC->SetToolTip(_("Set fixed font in text controls."));
  m_fontFamily->SetToolTip(_("Font used for display in console."));
  m_symbolFontOk->SetToolTip(_("Use greek font to display greek characters."));
  m_symbolFontAdj->SetToolTip(_("Adjustment for the size of greek font."));

  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString mp, mc, ib, br, mf;
  int fntsz = 12;
  bool match = true, showLongExpr = false;
  bool showHeader = true, fixedFontTC = true;
  int rs=0;
  int lang = wxLANGUAGE_UNKNOWN;
  int panelSize = 1;

  m_fontSize->SetRange(8, 20);
  config->Read(wxT("maxima"), &mp);
  config->Read(wxT("parameters"), &mc);
  config->Read(wxT("browser"), &br);
  config->Read(wxT("pos-restore"), &rs);
  config->Read(wxT("matchParens"), &match);
  config->Read(wxT("fontsize"), &fntsz);
  config->Read(wxT("showLong"), &showLongExpr);
  config->Read(wxT("language"), &lang);
  config->Read(wxT("showHeader"), &showHeader);
  config->Read(wxT("fixedFontTC"), &fixedFontTC);
  config->Read(wxT("panelSize"), &panelSize);

  int i=0;
  for (i=0; i<LANGUAGE_NUMBER; i++)
    if (langs[i] == lang)
      break;
  if (i<LANGUAGE_NUMBER)
    m_language->SetSelection(i);
  else
    m_language->SetSelection(0);

  m_panelSize->SetSelection(panelSize);

  if (mp.Length())
    m_maximaProgram->SetValue(mp);
  else
    m_maximaProgram->SetValue(wxT("maxima"));
  m_additionalParameters->SetValue(mc);
  m_fontSize->SetValue(fntsz);
  if (rs==1)
    m_saveSize->SetValue(true);
  else
    m_saveSize->SetValue(false);
  m_matchParens->SetValue(match);
  m_showLong->SetValue(showLongExpr);
  m_showHeader->SetValue(showHeader);
  m_fixedFontInTC->SetValue(fixedFontTC);

#if defined __WXMSW__
  m_button1->SetDefault();
#else
  m_button2->SetDefault();
#endif
  ReadStyles();
}


void Config::do_layout()
{
  // begin wxGlade: Config::do_layout
  wxFlexGridSizer* sizer_1 = new wxFlexGridSizer(5, 1, 0, 0);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* sizer_8 = new wxFlexGridSizer(4, 1, 3, 3);
  wxStaticBoxSizer* sizer_11 = new wxStaticBoxSizer(sizer_11_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_4 = new wxFlexGridSizer(2, 2, 2, 7);
  wxStaticBoxSizer* sizer_9 = new wxStaticBoxSizer(sizer_9_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(2, 2, 2, 2);
  wxFlexGridSizer* sizer_3 = new wxFlexGridSizer(2, 1, 3, 3);
  wxStaticBoxSizer* sizer_6 = new wxStaticBoxSizer(sizer_6_staticbox, wxVERTICAL);
  //wxBoxSizer* sizer_7 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* grid_sizer_5 = new wxFlexGridSizer(2, 2, 2, 2);
  wxStaticBoxSizer* sizer_4 = new wxStaticBoxSizer(sizer_4_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(2, 3, 3, 3);
  wxStaticBoxSizer* sizer_12 = new wxStaticBoxSizer(sizer_12_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_3 = new wxFlexGridSizer(2, 2, 3, 3);
  wxBoxSizer* sizer_5 = new wxBoxSizer(wxHORIZONTAL);

  // Title
  sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);

  // TAB 1
  // Maxima options box
  grid_sizer_2->Add(label_5, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_maximaProgram, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_mpBrowse, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(label_6, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_additionalParameters, 0, wxALL, 3);
  sizer_4->Add(grid_sizer_2, 1, wxALL|wxEXPAND, 3);
  sizer_3->Add(sizer_4, 1, wxALL|wxEXPAND, 3);

  // TAB 2
  // wxMaxima options box
  grid_sizer_5->Add(label_4, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(m_language, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(label_9, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_5->Add(m_panelSize, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_6->Add(grid_sizer_5, 1, wxEXPAND, 0);
  sizer_6->Add(m_saveSize, 0, wxALL, 3);
  sizer_6->Add(m_matchParens, 0, wxALL, 3);
  sizer_6->Add(m_fixedFontInTC, 0, wxALL, 3);
  sizer_6->Add(m_showLong, 0, wxALL, 3);
  sizer_6->Add(m_showHeader, 0, wxALL, 3);
  sizer_3->Add(sizer_6, 1, wxALL|wxEXPAND, 3);

  // Tab 1
  notebook_1_pane_1->SetAutoLayout(true);
  notebook_1_pane_1->SetSizer(sizer_3);
  sizer_3->Fit(notebook_1_pane_1);
  sizer_3->SetSizeHints(notebook_1_pane_1);
  sizer_3->AddGrowableCol(0);

  // TAB 2
  // Font box
  grid_sizer_1->Add(label_7, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_fontSize, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(label_8, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_fontFamily, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_9->Add(grid_sizer_1, 1, wxALL|wxEXPAND, 3);
  sizer_8->Add(sizer_9, 1, wxALL|wxEXPAND, 3);

  // Greek font box
  grid_sizer_3->Add(m_symbolFontOk, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_3->Add(m_getSymbolFont, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_3->Add(label_10, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_3->Add(m_symbolFontAdj, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
  sizer_12->Add(grid_sizer_3, 1, wxALL|wxEXPAND, 3);
  sizer_8->Add(sizer_12, 1, wxALL|wxEXPAND, 3);

  // Styles box
  grid_sizer_4->Add(m_styleFor, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(20, 20, 0, wxALL, 0);
  grid_sizer_4->Add(m_styleColor, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_boldCB, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_italicCB, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_5->Add(m_underlinedCB, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(sizer_5, 1, wxALL|wxEXPAND, 3);
  grid_sizer_4->Add(20, 20, 0, wxALL, 0);
  grid_sizer_4->Add(label_11, 0, wxALL|wxEXPAND, 3);
  sizer_11->Add(grid_sizer_4, 1, wxALL|wxEXPAND, 3);
  sizer_8->Add(sizer_11, 1, wxALL|wxEXPAND, 3);

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
#if wxCHECK_VERSION(2,5,3)
  sizer_1->Add(notebook_1, 1, wxEXPAND|wxALL, 2);
#else
  sizer_1->Add(new wxNotebookSizer(notebook_1), 1, wxEXPAND|wxALL, 2);
#endif

  // OK and cancel buttons
  sizer_2->Add(m_button1, 0, wxLEFT|wxRIGHT, 5);
  sizer_2->Add(m_button2, 0, wxLEFT|wxRIGHT, 5);
  sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT|wxTOP|wxBOTTOM, 3);

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
  int i=0;
  wxString search = wxT("maxima-htmldir");
  wxArrayString out;
  wxString maxima = m_maximaProgram->GetValue();
  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Write(wxT("maxima"), m_maximaProgram->GetValue());
  config->Write(wxT("parameters"), m_additionalParameters->GetValue());
  config->Write(wxT("fontSize"), m_fontSize->GetValue());
  config->Write(wxT("matchParens"), m_matchParens->GetValue());
  config->Write(wxT("showLong"), m_showLong->GetValue());
  config->Write(wxT("showHeader"), m_showHeader->GetValue());
  config->Write(wxT("fixedFontTC"), m_fixedFontInTC->GetValue());
  config->Write(wxT("panelSize"), m_panelSize->GetSelection());
  if (m_saveSize->GetValue())
    config->Write(wxT("pos-restore"), 1);
  else
    config->Write(wxT("pos-restore"), 0);
  i = m_language->GetSelection();
  if (i>-1 && i<LANGUAGE_NUMBER) {
    config->Write(wxT("language"), langs[i]);
  }
  config->Flush();

  WriteStyles();
  EndModal(wxID_OK);
}

void Config::OnMpBrowse(wxCommandEvent& event)
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString dd;
  config->Read(wxT("maxima"), &dd);
  wxString file = wxFileSelector(_("Select maxima program"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxEmptyString, _("Bat files (*.bat)|*.bat|All|*"),
                                 wxOPEN);
  if (file.Length()) {
    m_maximaProgram->SetValue(file);
  }
}

void Config::OnSymbolBrowse(wxCommandEvent& event)
{
  wxFont symbol;
#if defined __WXMSW__
  symbol = wxGetFontFromUser(this, wxFont(12, wxNORMAL, wxNORMAL, wxNORMAL,
                                          false, m_symbolFontName,
                                          wxFONTENCODING_CP1253));
#else
  symbol = wxGetFontFromUser(this, wxFont(12, wxNORMAL, wxNORMAL, wxNORMAL,
                                          false, m_symbolFontName,
                                          wxFONTENCODING_ISO8859_7));
#endif
  if (symbol.Ok()) {
    m_symbolFontName = symbol.GetFaceName();
    m_getSymbolFont->SetLabel(m_symbolFontName);
  }
}

void Config::OnChangeFontFamily(wxCommandEvent& event)
{
  UpdateExample();
}

void Config::ReadStyles()
{
  wxConfigBase* config = wxConfig::Get();

  wxString font;
  int adj = 0;
  bool symbolOk = false;

  config->Read(wxT("Style/fontname"), &font);
  m_fontFamily->SetValue(font);

  m_symbolFontName = wxEmptyString;
  config->Read(wxT("Style/Symbol/ok"), &symbolOk);
  config->Read(wxT("Style/Symbol/fontname"), &m_symbolFontName);
  config->Read(wxT("Style/Symbol/adj"), &adj);
  m_symbolFontAdj->SetValue(adj);
  m_symbolFontOk->SetValue(symbolOk);
  if (m_symbolFontName.Length()>0)
    m_getSymbolFont->SetLabel(m_symbolFontName);
  m_getSymbolFont->Enable(symbolOk);
  m_symbolFontAdj->Enable(symbolOk);

  m_styleBackground.color = wxT("white");
  config->Read(wxT("Style/Background/color"),
               &m_styleBackground.color);

  m_styleHighlight.color = wxT("red");
  config->Read(wxT("Style/Highlight/color"),
               &m_styleHighlight.color);

  // Normal text
  m_styleNormalText.color = wxT("black");
  m_styleNormalText.bold = false;
  m_styleNormalText.italic = false;
  m_styleNormalText.underlined = false;
  config->Read(wxT("Style/NormalText/color"),
               &m_styleNormalText.color);
  config->Read(wxT("Style/NormalText/bold"),
               &m_styleNormalText.bold);
  config->Read(wxT("Style/NormalText/italic"),
               &m_styleNormalText.italic);
  config->Read(wxT("Style/NormalText/underlined"),
               &m_styleNormalText.underlined);

  // Hidden group
  m_styleHiddenText.bold = false;
  m_styleHiddenText.italic = true;
  m_styleHiddenText.underlined = true;
  config->Read(wxT("Style/HiddenText/bold"),
               &m_styleHiddenText.bold);
  config->Read(wxT("Style/HiddenText/italic"),
               &m_styleHiddenText.italic);
  config->Read(wxT("Style/HiddenText/underlined"),
               &m_styleHiddenText.underlined);

  // Main prompt
  m_styleMainPrompt.color = wxT("red");
  m_styleMainPrompt.bold = false;
  m_styleMainPrompt.italic = false;
  m_styleMainPrompt.underlined = false;
  config->Read(wxT("Style/MainPrompt/color"),
               &m_styleMainPrompt.color);
  config->Read(wxT("Style/MainPrompt/bold"),
               &m_styleMainPrompt.bold);
  config->Read(wxT("Style/MainPrompt/italic"),
               &m_styleMainPrompt.italic);
  config->Read(wxT("Style/MainPrompt/underlined"),
               &m_styleMainPrompt.underlined);

  // Other prompt
  m_styleOtherPrompt.color = wxT("red");
  m_styleOtherPrompt.bold = false;
  m_styleOtherPrompt.italic = true;
  m_styleOtherPrompt.underlined = false;
  config->Read(wxT("Style/OtherPrompt/color"),
               &m_styleOtherPrompt.color);
  config->Read(wxT("Style/OtherPrompt/bold"),
               &m_styleOtherPrompt.bold);
  config->Read(wxT("Style/OtherPrompt/italic"),
               &m_styleOtherPrompt.italic);
  config->Read(wxT("Style/OtherPrompt/underlined"),
               &m_styleOtherPrompt.underlined);

  // Labels
  m_styleLabel.color = wxT("brown");
  m_styleLabel.bold = false;
  m_styleLabel.italic = false;
  m_styleLabel.underlined = false;
  config->Read(wxT("Style/Label/color"),
               &m_styleLabel.color);
  config->Read(wxT("Style/Label/bold"),
               &m_styleLabel.bold);
  config->Read(wxT("Style/Label/italic"),
               &m_styleLabel.italic);
  config->Read(wxT("Style/Label/underlined"),
               &m_styleLabel.underlined);

  // Special
  m_styleSpecial.color = m_styleNormalText.color;
  m_styleSpecial.bold = false;
  m_styleSpecial.italic = false;
  m_styleSpecial.underlined = false;
  config->Read(wxT("Style/Special/color"),
               &m_styleSpecial.color);
  config->Read(wxT("Style/Special/bold"),
               &m_styleSpecial.bold);
  config->Read(wxT("Style/Special/italic"),
               &m_styleSpecial.italic);
  config->Read(wxT("Style/Special/underlined"),
               &m_styleSpecial.underlined);

  // Input
  m_styleInput.color = wxT("blue");
  m_styleInput.bold = false;
  m_styleInput.italic = false;
  m_styleInput.underlined = false;
  config->Read(wxT("Style/Input/color"),
               &m_styleInput.color);
  config->Read(wxT("Style/Input/bold"),
               &m_styleInput.bold);
  config->Read(wxT("Style/Input/italic"),
               &m_styleInput.italic);
  config->Read(wxT("Style/Input/underlined"),
               &m_styleInput.underlined);

  // Number
  m_styleNumber.color = m_styleNormalText.color;
  m_styleNumber.bold = false;
  m_styleNumber.italic = false;
  m_styleNumber.underlined = false;
  config->Read(wxT("Style/Number/color"),
               &m_styleNumber.color);
  config->Read(wxT("Style/Number/bold"),
               &m_styleNumber.bold);
  config->Read(wxT("Style/Number/italic"),
               &m_styleNumber.italic);
  config->Read(wxT("Style/Number/underlined"),
               &m_styleNumber.underlined);

  // String
  m_styleString.color = m_styleNormalText.color;
  m_styleString.bold = false;
  m_styleString.italic = true;
  m_styleString.underlined = false;
  config->Read(wxT("Style/String/color"),
               &m_styleString.color);
  config->Read(wxT("Style/String/bold"),
               &m_styleString.bold);
  config->Read(wxT("Style/String/italic"),
               &m_styleString.italic);
  config->Read(wxT("Style/String/underlined"),
               &m_styleString.underlined);

  // Greek
  m_styleGreek.color = m_styleNormalText.color;
  m_styleGreek.bold = false;
  m_styleGreek.italic = false;
  m_styleGreek.underlined = false;
  config->Read(wxT("Style/Greek/color"),
               &m_styleGreek.color);
  config->Read(wxT("Style/Greek/bold"),
               &m_styleGreek.bold);
  config->Read(wxT("Style/Greek/italic"),
               &m_styleGreek.italic);
  config->Read(wxT("Style/Greek/underlined"),
               &m_styleGreek.underlined);

  // Variable
  m_styleVariable.color = m_styleNormalText.color;
  m_styleVariable.bold = false;
  m_styleVariable.italic = true;
  m_styleVariable.underlined = false;
  config->Read(wxT("Style/Variable/color"),
               &m_styleVariable.color);
  config->Read(wxT("Style/Variable/bold"),
               &m_styleVariable.bold);
  config->Read(wxT("Style/Variable/italic"),
               &m_styleVariable.italic);
  config->Read(wxT("Style/Variable/underlined"),
               &m_styleVariable.underlined);

  // Set values in dialog
  m_styleFor->SetSelection(0);
  int i=0;
  for (i=0; i<COLORLIST_LENGTH; i++)
    if (m_styleNormalText.color == colorlist[i])
      break;
  m_styleColor->SetSelection(i);
  m_boldCB->SetValue(m_styleVariable.bold);
  m_italicCB->SetValue(m_styleVariable.italic);
  m_underlinedCB->SetValue(m_styleVariable.underlined);
}

void Config::WriteStyles()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();

  config->Write(wxT("Style/Background/color"),
                m_styleBackground.color);
  config->Write(wxT("Style/Highlight/color"),
                m_styleHighlight.color);

  config->Write(wxT("Style/fontname"), m_fontFamily->GetValue());

  config->Write(wxT("Style/Symbol/ok"), m_symbolFontOk->GetValue());
  config->Write(wxT("Style/Symbol/fontname"), m_symbolFontName);
  config->Write(wxT("Style/Symbol/adj"), m_symbolFontAdj->GetValue());

  // Normal text
  config->Write(wxT("Style/NormalText/color"),
                m_styleNormalText.color);
  config->Write(wxT("Style/NormalText/bold"),
                m_styleNormalText.bold);
  config->Write(wxT("Style/NormalText/italic"),
                m_styleNormalText.italic);
  config->Write(wxT("Style/NormalText/underlined"),
                m_styleNormalText.underlined);

  // Hidden
  config->Write(wxT("Style/HiddenText/bold"),
                m_styleHiddenText.bold);
  config->Write(wxT("Style/HiddenText/italic"),
                m_styleHiddenText.italic);
  config->Write(wxT("Style/HiddenText/underlined"),
                m_styleHiddenText.underlined);

  // Main prompt
  config->Write(wxT("Style/MainPrompt/color"),
                m_styleMainPrompt.color);
  config->Write(wxT("Style/MainPrompt/bold"),
                m_styleMainPrompt.bold);
  config->Write(wxT("Style/MainPrompt/italic"),
                m_styleMainPrompt.italic);
  config->Write(wxT("Style/MainPrompt/underlined"),
                m_styleMainPrompt.underlined);

  // Other prompt
  config->Write(wxT("Style/OtherPrompt/color"),
                m_styleOtherPrompt.color);
  config->Write(wxT("Style/OtherPrompt/bold"),
                m_styleOtherPrompt.bold);
  config->Write(wxT("Style/OtherPrompt/italic"),
                m_styleOtherPrompt.italic);
  config->Write(wxT("Style/OtherPrompt/underlined"),
                m_styleOtherPrompt.underlined);

  // Label
  config->Write(wxT("Style/Label/color"),
                m_styleLabel.color);
  config->Write(wxT("Style/Label/bold"),
                m_styleLabel.bold);
  config->Write(wxT("Style/Label/italic"),
                m_styleLabel.italic);
  config->Write(wxT("Style/Label/underlined"),
                m_styleLabel.underlined);

  // Special
  config->Write(wxT("Style/Special/color"),
                m_styleSpecial.color);
  config->Write(wxT("Style/Special/bold"),
                m_styleSpecial.bold);
  config->Write(wxT("Style/Special/italic"),
                m_styleSpecial.italic);
  config->Write(wxT("Style/Special/underlined"),
                m_styleSpecial.underlined);

  // Input
  config->Write(wxT("Style/Input/color"),
                m_styleInput.color);
  config->Write(wxT("Style/Input/bold"),
                m_styleInput.bold);
  config->Write(wxT("Style/Input/italic"),
                m_styleInput.italic);
  config->Write(wxT("Style/Input/underlined"),
                m_styleInput.underlined);

  // Number
  config->Write(wxT("Style/Number/color"),
                m_styleNumber.color);
  config->Write(wxT("Style/Number/bold"),
                m_styleNumber.bold);
  config->Write(wxT("Style/Number/italic"),
                m_styleNumber.italic);
  config->Write(wxT("Style/Number/underlined"),
                m_styleNumber.underlined);

  // Greek
  config->Write(wxT("Style/Greek/color"),
                m_styleGreek.color);
  config->Write(wxT("Style/Greek/bold"),
                m_styleGreek.bold);
  config->Write(wxT("Style/Greek/italic"),
                m_styleGreek.italic);
  config->Write(wxT("Style/Greek/underlined"),
                m_styleGreek.underlined);

  // String
  config->Write(wxT("Style/String/color"),
                m_styleString.color);
  config->Write(wxT("Style/String/bold"),
                m_styleString.bold);
  config->Write(wxT("Style/String/italic"),
                m_styleString.italic);
  config->Write(wxT("Style/String/underlined"),
                m_styleString.underlined);

  // Variable
  config->Write(wxT("Style/Variable/color"),
                m_styleVariable.color);
  config->Write(wxT("Style/Variable/bold"),
                m_styleVariable.bold);
  config->Write(wxT("Style/Variable/italic"),
                m_styleVariable.italic);
  config->Write(wxT("Style/Variable/underlined"),
                m_styleVariable.underlined);

  config->Flush();
}

void Config::OnChangeColor(wxCommandEvent& event)
{
  style* tmp =GetStylePointer();

  int i = m_styleColor->GetSelection();
  tmp->color = colorlist[i];

  UpdateExample();
}

void Config::OnChangeStyle(wxCommandEvent& event)
{
  style* tmp = GetStylePointer();
  int st = m_styleFor->GetSelection();

  if (tmp == &m_styleHiddenText) {
    m_styleColor->Enable(false);
  }
  else {
    m_styleColor->Enable(true);
    int i=0;
    for (i=0; i<COLORLIST_LENGTH; i++)
      if (colorlist[i] == tmp->color)
        break;
    if (i<COLORLIST_LENGTH)
      m_styleColor->SetSelection(i);
  }

  if (st == 11 || st == 12) {
    m_boldCB->Enable(false);
    m_italicCB->Enable(false);
    m_underlinedCB->Enable(false);
  }
  else {
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

void Config::OnCheckSymbol(wxCommandEvent& event)
{
  m_getSymbolFont->Enable(m_symbolFontOk->GetValue());
  m_symbolFontAdj->Enable(m_symbolFontOk->GetValue());
}

class FixedFonts : public wxFontEnumerator
{
public:
  const wxArrayString& GetFacenames() { return m_facenames; }
protected:
  virtual bool OnFacename(const wxString& facename)
  {
    m_facenames.Add(facename);
    return true;
  }
private:
   wxArrayString m_facenames;
};

void Config::SetupFontList()
{
  FixedFonts enumerator;
  enumerator.EnumerateFacenames(wxFONTENCODING_SYSTEM, true);

  const wxArrayString& strings = enumerator.GetFacenames();
  for (unsigned int i=0; i<strings.Count(); i++)
    m_fontFamily->Append(strings.Item(i));
}

void Config::OnChangePanelSize(wxCommandEvent &event)
{
  wxMessageBox(_("Please restart wxMaxima for changes to take effect!"),
               _("Configuration warning"),
               wxOK);
}

//
// Should match whatever is put in m_styleFor
//
style* Config::GetStylePointer()
{
  style* tmp = &m_styleNormalText;
  switch (m_styleFor->GetSelection())
  {
    case 1:
      tmp = &m_styleNumber;
      break;
    case 2:
      tmp = &m_styleSpecial;
      break;
    case 3:
      tmp = &m_styleGreek;
      break;
    case 4:
      tmp = &m_styleString;
      break;
    case 5:
      tmp = &m_styleNormalText;
      break;
    case 6:
      tmp = &m_styleInput;
      break;
    case 7:
      tmp = &m_styleMainPrompt;
      break;
    case 8:
      tmp = &m_styleOtherPrompt;
      break;
    case 9:
      tmp = &m_styleLabel;
      break;
    case 10:
      tmp = &m_styleHiddenText;
      break;
    case 11:
      tmp = &m_styleHighlight;
      break;
    case 12:
      tmp = &m_styleBackground;
      break;
    default:
      tmp = &m_styleVariable;
      break;
  }
  return tmp;
}

void Config::UpdateExample()
{
  style *tmp = GetStylePointer();
  wxString example = _("Example text");
  wxString color(tmp->color);

  wxClientDC dc(label_11);

  if (tmp == &m_styleBackground)
    color = m_styleNormalText.color;
  else if (tmp == &m_styleHiddenText)
    color = m_styleMainPrompt.color;

  label_11->SetStyle(color, tmp->italic, tmp->bold, tmp->underlined, m_fontFamily->GetValue(), m_fontSize->GetValue());
#if wxCHECK_VERSION(2, 5, 3)
  label_11->SetBackgroundColour(wxTheColourDatabase->Find(m_styleBackground.color));
#else
  label_11->SetBackgroundColour(*(wxTheColourDatabase->FindColour(m_styleBackground.color)));
#endif
  label_11->Refresh();
}

BEGIN_EVENT_TABLE(Config, wxDialog)
  EVT_BUTTON(wxID_OK, Config::OnOk)
  EVT_BUTTON(wxID_OPEN, Config::OnMpBrowse)
  EVT_BUTTON(button_symbol, Config::OnSymbolBrowse)
  EVT_COMBOBOX(combobox_colour, Config::OnChangeColor)
  EVT_COMBOBOX(combobox_styleFor, Config::OnChangeStyle)
  EVT_COMBOBOX(font_family, Config::OnChangeFontFamily)
  EVT_COMBOBOX(panel_size, Config::OnChangePanelSize)
  EVT_CHECKBOX(checkbox_bold, Config::OnCheckbox)
  EVT_CHECKBOX(checkbox_italic, Config::OnCheckbox)
  EVT_CHECKBOX(checkbox_underlined, Config::OnCheckbox)
  EVT_CHECKBOX(checkbox_symbol, Config::OnCheckSymbol)
END_EVENT_TABLE()

void ExamplePanel::OnPaint(wxPaintEvent& event)
{
  wxString example(_("Example text"));
  wxPaintDC dc(this);
  int panel_width, panel_height;
  int text_width, text_height;
  int bold = wxNORMAL, italic = wxNORMAL, underlined = 0;

  GetClientSize(&panel_width, &panel_height);

#if wxCHECK_VERSION(2, 5, 3)
  dc.SetTextForeground(wxTheColourDatabase->Find(m_fgColor));
#else
  dc.SetTextForeground(*(wxTheColourDatabase->FindColour(m_fgColor)));
#endif

  if (m_bold)
    bold = wxBOLD;
  if (m_italic)
    italic = wxSLANT;
  if (m_underlined)
    underlined = 1;
  dc.SetFont(wxFont(m_size, wxMODERN, italic, bold, underlined, m_font));
  dc.GetTextExtent(example, &text_width, &text_height);

  dc.DrawText(example, (panel_width - text_width)/2,
                       (panel_height - text_height)/2);
}

BEGIN_EVENT_TABLE(ExamplePanel, wxPanel)
  EVT_PAINT(ExamplePanel::OnPaint)
END_EVENT_TABLE()
