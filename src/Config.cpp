/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
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
  sizer_9_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Font"));
  sizer_11_staticbox = new wxStaticBox(notebook_1_pane_2, -1, _("Styles"));
  sizer_4_staticbox = new wxStaticBox(notebook_1_pane_1, -1, _("Maxima options"));
  label_1 = new wxStaticText(this, -1, _("wxMaxima configuration"));
  label_5 = new wxStaticText(notebook_1_pane_1, -1, _("Maxima program:"));
  m_maximaProgram = new wxTextCtrl(notebook_1_pane_1, -1, wxT(""), wxDefaultPosition, wxSize(250, -1));
  m_mpBrowse = new wxButton(notebook_1_pane_1, mp_browse_id, _("Browse"));
  label_6 = new wxStaticText(notebook_1_pane_1, -1, _("Additional parameters:"));
  m_additionalParameters = new wxTextCtrl(notebook_1_pane_1, -1, wxT(""), wxDefaultPosition, wxSize(250, -1));
  label_4 = new wxStaticText(notebook_1_pane_1, -1, _("Language:"));
  const wxString m_language_choices[] = {
    _("(Use default language)"), _("English"),
    _("French"),  _("Italian"), _("Spanish")
  };
  m_language = new wxComboBox(notebook_1_pane_1, -1, wxT(""), wxDefaultPosition, wxSize(230, -1), 5, m_language_choices, wxCB_DROPDOWN|wxCB_READONLY);
  m_saveSize = new wxCheckBox(notebook_1_pane_1, -1, _("Save wxMaxima window size/position"));
  m_matchParens = new wxCheckBox(notebook_1_pane_1, -1, _("Match parenthesis"));
  m_showLong = new wxCheckBox(notebook_1_pane_1, -1, _("Show long expressions"));
  m_showHeader = new wxCheckBox(notebook_1_pane_1, -1, _("Show maxima header"));
  label_7 = new wxStaticText(notebook_1_pane_2, -1, _("Font size:"));
  m_fontSize = new wxSpinCtrl(notebook_1_pane_2, -1, wxT(""), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100);
  label_8 = new wxStaticText(notebook_1_pane_2, -1, _("Font family:"));
  const wxString m_fontFamily_choices[] = {
    
  };
  m_fontFamily = new wxComboBox(notebook_1_pane_2, -1, wxT(""), wxDefaultPosition, wxSize(230, -1), 0, m_fontFamily_choices, wxCB_DROPDOWN|wxCB_READONLY|wxCB_SORT);
  setupFontList();
  const wxString m_styleFor_choices[] = {
    _("Normal text"),  _("Hidden groups"), _("Main prompts"),
    _("Other prompts"), _("Input"), _("Labels"), _("Special constants")
  };
  m_styleFor = new wxComboBox(notebook_1_pane_2, combobox_styleFor, wxT(""), wxDefaultPosition, wxSize(150, -1), 7, m_styleFor_choices, wxCB_DROPDOWN|wxCB_READONLY);
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
  m_styleColor = new wxComboBox(notebook_1_pane_2, combobox_colour, wxT(""), wxDefaultPosition, wxSize(150, -1), 68, m_styleColor_choices, wxCB_DROPDOWN|wxCB_READONLY);
  m_styleBold = new wxCheckBox(notebook_1_pane_2, checkbox_bold, _("Bold"));
  m_styleItalic = new wxCheckBox(notebook_1_pane_2, checkbox_italic, _("Italic"));
  m_styleUnderlined = new wxCheckBox(notebook_1_pane_2, checkbox_underlined, _("Underlined"));
  m_buttonOK = new wxButton(this, wxOK, _("Ok"));
  m_buttonCancel = new wxButton(this, wxCANCEL, _("Cancel"));
  
  set_properties();
  do_layout();
  // end wxGlade
  m_ok = false;
}


void Config::set_properties()
{
  // begin wxGlade: Config::set_properties
  SetTitle(_("wxMaxima configuration"));
  label_1->SetFont(wxFont(20, wxROMAN, wxITALIC, wxNORMAL, 0, wxT("")));
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
  
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString mp, mc, ib, br, mf;
  int fntsz = 12;
  bool match = true, showLongExpr = false, showHeader = true;
  int rs=0;
  int lang = wxLANGUAGE_UNKNOWN;
  
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
  
  int i=0;
  for (i=0; i<LANGUAGE_NUMBER; i++)
    if (langs[i] == lang)
      break;
  if (i<LANGUAGE_NUMBER)
    m_language->SetSelection(i);
  else
    m_language->SetSelection(0);
  
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
  
  m_buttonOK->SetDefault();
  readStyles();
}


void Config::do_layout()
{
  // begin wxGlade: Config::do_layout
  wxFlexGridSizer* sizer_1 = new wxFlexGridSizer(5, 1, 0, 0);
  wxBoxSizer* sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer* sizer_8 = new wxFlexGridSizer(4, 1, 3, 3);
  wxStaticBoxSizer* sizer_11 = new wxStaticBoxSizer(sizer_11_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_4 = new wxFlexGridSizer(2, 4, 3, 7);
  wxStaticBoxSizer* sizer_9 = new wxStaticBoxSizer(sizer_9_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_1 = new wxFlexGridSizer(2, 2, 2, 2);
  wxFlexGridSizer* sizer_3 = new wxFlexGridSizer(2, 1, 3, 3);
  wxStaticBoxSizer* sizer_6 = new wxStaticBoxSizer(sizer_6_staticbox, wxVERTICAL);
  wxBoxSizer* sizer_7 = new wxBoxSizer(wxHORIZONTAL);
  wxStaticBoxSizer* sizer_4 = new wxStaticBoxSizer(sizer_4_staticbox, wxVERTICAL);
  wxFlexGridSizer* grid_sizer_2 = new wxFlexGridSizer(2, 3, 3, 3);
  sizer_1->Add(label_1, 0, wxALL|wxALIGN_CENTER_HORIZONTAL, 3);
  grid_sizer_2->Add(label_5, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_maximaProgram, 0, wxALL, 3);
  grid_sizer_2->Add(m_mpBrowse, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(label_6, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_2->Add(m_additionalParameters, 0, wxALL, 3);
  sizer_4->Add(grid_sizer_2, 1, wxEXPAND, 3);
  sizer_3->Add(sizer_4, 1, wxALL|wxEXPAND, 3);
  sizer_7->Add(label_4, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_7->Add(m_language, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_6->Add(sizer_7, 1, wxEXPAND, 0);
  sizer_6->Add(m_saveSize, 0, wxALL, 3);
  sizer_6->Add(m_matchParens, 0, wxALL, 3);
  sizer_6->Add(m_showLong, 0, wxALL, 3);
  sizer_6->Add(m_showHeader, 0, wxALL, 3);
  sizer_3->Add(sizer_6, 1, wxALL|wxEXPAND, 3);
  notebook_1_pane_1->SetAutoLayout(true);
  notebook_1_pane_1->SetSizer(sizer_3);
  sizer_3->Fit(notebook_1_pane_1);
  sizer_3->SetSizeHints(notebook_1_pane_1);
  sizer_3->AddGrowableCol(0);
  grid_sizer_1->Add(label_7, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_fontSize, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(label_8, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_1->Add(m_fontFamily, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  sizer_9->Add(grid_sizer_1, 1, wxEXPAND, 0);
  sizer_8->Add(sizer_9, 1, wxALL|wxEXPAND, 3);
  grid_sizer_4->Add(m_styleFor, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(20, 20, 0, wxALL, 0);
  grid_sizer_4->Add(20, 20, 0, wxALL, 0);
  grid_sizer_4->Add(20, 20, 0, wxALL, 0);
  grid_sizer_4->Add(m_styleColor, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(m_styleBold, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(m_styleItalic, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
  grid_sizer_4->Add(m_styleUnderlined, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5);
  sizer_11->Add(grid_sizer_4, 1, wxALL|wxEXPAND, 3);
  sizer_8->Add(sizer_11, 1, wxALL|wxEXPAND, 3);
  notebook_1_pane_2->SetAutoLayout(true);
  notebook_1_pane_2->SetSizer(sizer_8);
  sizer_8->Fit(notebook_1_pane_2);
  sizer_8->SetSizeHints(notebook_1_pane_2);
  sizer_8->AddGrowableCol(0);
  notebook_1->AddPage(notebook_1_pane_1, _("Options"));
  notebook_1->AddPage(notebook_1_pane_2, _("Style"));
  sizer_1->Add(new wxNotebookSizer(notebook_1), 1, wxEXPAND, 0);
  sizer_2->Add(m_buttonOK, 0, wxALL, 3);
  sizer_2->Add(m_buttonCancel, 0, wxALL, 4);
  sizer_1->Add(sizer_2, 1, wxALL|wxALIGN_RIGHT, 3);
  SetAutoLayout(true);
  SetSizer(sizer_1);
  sizer_1->Fit(this);
  sizer_1->SetSizeHints(this);
  sizer_1->AddGrowableRow(1);
  sizer_1->AddGrowableCol(0);
  Layout();
  // end wxGlade
}

void Config::onOk(wxCommandEvent& event)
{
  int i=0;
  m_ok = true;
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
  if (m_saveSize->GetValue())
    config->Write(wxT("pos-restore"), 1);
  else
    config->Write(wxT("pos-restore"), 0);
  i = m_language->GetSelection();
  if (i>-1 && i<LANGUAGE_NUMBER) {
    config->Write(wxT("language"), langs[i]);
  }
  config->Flush();

  writeStyles();
  Close();
}

void Config::onCancel(wxCommandEvent& event)
{
  Close();
}

void Config::onMpBrowse(wxCommandEvent& event)
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString dd;
  config->Read(wxT("maxima"), &dd);
  wxString file = wxFileSelector(_("Select maxima program"),
                                 wxPathOnly(dd), wxFileNameFromPath(dd),
                                 wxT(""), _("Bat files (*.bat)|*.bat|All|*"),
                                 wxOPEN);
  if (file.Length()) {
    m_maximaProgram->SetValue(file);
  }
}

void Config::readStyles()
{
  wxConfigBase* config = wxConfig::Get();
  
  wxString font;
  config->Read(wxT("Style/fontname"), &font);
  m_fontFamily->SetValue(font);

  m_styleNormalText.color = wxT("black");
  m_styleNormalText.bold = false;
  m_styleNormalText.italic = false;
  m_styleNormalText.underlined = false;
  // Normal text
  config->Read(wxT("Style/NormalText/color"),
               &m_styleNormalText.color);
  config->Read(wxT("Style/NormalText/bold"),
               &m_styleNormalText.bold);
  config->Read(wxT("Style/NormalText/italic"),
               &m_styleNormalText.italic);
  config->Read(wxT("Style/NormalText/underlined"),
               &m_styleNormalText.underlined);
  
  m_styleHiddenText.bold = false;
  m_styleHiddenText.italic = true;
  m_styleHiddenText.underlined = true;
  // Hidden group
  config->Read(wxT("Style/HiddenText/bold"),
               &m_styleHiddenText.bold);
  config->Read(wxT("Style/HiddenText/italic"),
               &m_styleHiddenText.italic);
  config->Read(wxT("Style/HiddenText/underlined"),
               &m_styleHiddenText.underlined);
  
  m_styleMainPrompt.color = wxT("red");
  m_styleMainPrompt.bold = false;
  m_styleMainPrompt.italic = false;
  m_styleMainPrompt.underlined = false;
  // Main prompt
  config->Read(wxT("Style/MainPrompt/color"),
               &m_styleMainPrompt.color);
  config->Read(wxT("Style/MainPrompt/bold"),
               &m_styleMainPrompt.bold);
  config->Read(wxT("Style/MainPrompt/italic"),
               &m_styleMainPrompt.italic);
  config->Read(wxT("Style/MainPrompt/underlined"),
               &m_styleMainPrompt.underlined);

  m_styleOtherPrompt.color = wxT("red");
  m_styleOtherPrompt.bold = false;
  m_styleOtherPrompt.italic = true;
  m_styleOtherPrompt.underlined = false;
  // Other prompt
  config->Read(wxT("Style/OtherPrompt/color"),
               &m_styleOtherPrompt.color);
  config->Read(wxT("Style/OtherPrompt/bold"),
               &m_styleOtherPrompt.bold);
  config->Read(wxT("Style/OtherPrompt/italic"),
               &m_styleOtherPrompt.italic);
  config->Read(wxT("Style/OtherPrompt/underlined"),
               &m_styleOtherPrompt.underlined);
  
  m_styleLabel.color = wxT("brown");
  m_styleLabel.bold = false;
  m_styleLabel.italic = false;
  m_styleLabel.underlined = false;
  // Labels
  config->Read(wxT("Style/Label/color"),
               &m_styleLabel.color);
  config->Read(wxT("Style/Label/bold"),
               &m_styleLabel.bold);
  config->Read(wxT("Style/Label/italic"),
               &m_styleLabel.italic);
  config->Read(wxT("Style/Label/underlined"),
               &m_styleLabel.underlined);
  
  m_styleSpecial.color = wxT("black");
  m_styleSpecial.bold = false;
  m_styleSpecial.italic = false;
  m_styleSpecial.underlined = false;
  // Special
  config->Read(wxT("Style/Special/bold"),
               &m_styleSpecial.bold);
  config->Read(wxT("Style/Special/italic"),
               &m_styleSpecial.italic);
  config->Read(wxT("Style/Special/underlined"),
               &m_styleSpecial.underlined);
  
  m_styleInput.color = wxT("blue");
  m_styleInput.bold = false;
  m_styleInput.italic = false;
  m_styleInput.underlined = false;
  // Input
  config->Read(wxT("Style/Input/color"),
               &m_styleInput.color);
  config->Read(wxT("Style/Input/bold"),
               &m_styleInput.bold);
  config->Read(wxT("Style/Input/italic"),
               &m_styleInput.italic);
  config->Read(wxT("Style/Input/underlined"),
               &m_styleInput.underlined);
  
  // Set values in dialog
  m_styleFor->SetSelection(0);
  int i=0;
  for (i=0; i<COLORLIST_LENGTH; i++)
    if (m_styleNormalText.color == colorlist[i])
      break;
  m_styleColor->SetSelection(i);
  m_styleBold->SetValue(m_styleNormalText.bold);
  m_styleItalic->SetValue(m_styleNormalText.italic);
  m_styleUnderlined->SetValue(m_styleNormalText.underlined);
}

void Config::writeStyles()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  
  config->Write(wxT("Style/fontname"), m_fontFamily->GetValue());
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
           
  config->Flush();
}

void Config::onChangeColor(wxCommandEvent& event)
{
  style* tmp =GetStylePointer();
  
  int i = m_styleColor->GetSelection();
  tmp->color = colorlist[i];
}

void Config::onChangeStyle(wxCommandEvent& event)
{
  style* tmp = GetStylePointer();
  
  if (m_styleFor->GetSelection() == 1 || m_styleFor->GetSelection() == 6)
    m_styleColor->Enable(false);
  else {
    m_styleColor->Enable(true);
    int i=0;
    for (i=0; i<COLORLIST_LENGTH; i++)
      if (colorlist[i] == tmp->color)
        break;
    if (i<COLORLIST_LENGTH)
      m_styleColor->SetSelection(i);
  }
  m_styleBold->SetValue(tmp->bold);
  m_styleItalic->SetValue(tmp->italic);
  m_styleUnderlined->SetValue(tmp->underlined);
}

void Config::onCheckbox(wxCommandEvent& event)
{
  style* tmp = GetStylePointer();
  
  tmp->bold = m_styleBold->GetValue();
  tmp->italic = m_styleItalic->GetValue();
  tmp->underlined = m_styleUnderlined->GetValue();
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

void Config::setupFontList()
{
  FixedFonts enumerator;
  enumerator.EnumerateFacenames(wxFONTENCODING_SYSTEM, true);
  
  const wxArrayString& strings = enumerator.GetFacenames();
  for (int i=0; i<strings.Count(); i++)
    m_fontFamily->Append(strings.Item(i));
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
      tmp = &m_styleHiddenText;
      break;
    case 2:
      tmp = &m_styleMainPrompt;
      break;
    case 3:
      tmp = &m_styleOtherPrompt;
      break;
    case 4:
      tmp = &m_styleInput;
      break;
    case 5:
      tmp = &m_styleLabel;
      break;
    case 6:
      tmp = &m_styleSpecial;
      break;
    default:
      tmp = &m_styleNormalText;
      break;
  }
  return tmp;
}

BEGIN_EVENT_TABLE(Config, wxDialog)
  EVT_BUTTON(wxOK, Config::onOk)
  EVT_BUTTON(wxCANCEL, Config::onCancel)
  EVT_BUTTON(mp_browse_id, Config::onMpBrowse)
  EVT_COMBOBOX(combobox_colour, Config::onChangeColor)
  EVT_COMBOBOX(combobox_styleFor, Config::onChangeStyle)
  EVT_CHECKBOX(checkbox_bold, Config::onCheckbox)
  EVT_CHECKBOX(checkbox_italic, Config::onCheckbox)
  EVT_CHECKBOX(checkbox_underlined, Config::onCheckbox)
END_EVENT_TABLE()
