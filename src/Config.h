///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include <wx/wx.h>
#include <wx/image.h>

#include <wx/spinctrl.h>
#include <wx/notebook.h>

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
  checkbox_header,
  button_greek,
  checkbox_greek,
  font_family,
  style_font_family,
  language_id,
  unicode_glyphs,
  save_id,
  load_id
};

class ExamplePanel : public wxPanel
{
public:
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
  void SetStyle(wxColour fg_color, bool italic, bool bold, bool underlined, wxString font)
  {
    m_fgColor = fg_color;
    m_italic = italic;
    m_bold = bold;
    m_underlined = underlined;
    m_font = font;
  }
  void SetFontSize(int size) { m_size = size; }
private:
  void OnPaint(wxPaintEvent& event);
  wxColour m_fgColor;
  bool m_italic, m_bold, m_underlined;
  wxString m_font;
  int m_size;
  DECLARE_EVENT_TABLE()
};



class Config: public wxDialog
{
public:
  Config(wxWindow* parent, int id, const wxString& title,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE);
  void OnChangeColor(); // called from class ColorPanel
private:
  // begin wxGlade: Config::methods
  void set_properties();
  void do_layout();
  // end wxGlade
protected:
  // begin wxGlade: Config::attributes
  wxStaticBox* sizer_11_staticbox;
  wxStaticBox* sizer_9_staticbox;
  wxStaticBox* sizer_6_staticbox;
  wxStaticBox* sizer_4_staticbox;
  wxStaticText* label_5;
  wxTextCtrl* m_maximaProgram;
  wxButton* m_mpBrowse;
  wxStaticText* label_6;
  wxTextCtrl* m_additionalParameters;
  wxStaticText* label_4;
  wxComboBox* m_language;
  wxCheckBox* m_saveSize;
  wxCheckBox* m_savePanes;
  wxCheckBox* m_matchParens;
  wxCheckBox* m_showLong;
  wxCheckBox* m_showHeader;
  wxCheckBox* m_enterEvaluates;
  wxPanel* notebook_1_pane_1;
  wxStaticText* label_8;
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
  wxPanel* notebook_1_pane_2;
  wxNotebook* notebook_1;
  wxButton* m_button1;
  wxButton* m_button2;
  wxCheckBox* m_greekFontOk;
  wxButton* m_getGreekFont;
  wxStaticText* label_10;
  wxSpinCtrl* m_greekFontAdj;
  wxString m_greekFontName;
  wxButton *m_saveStyle, *m_loadStyle;
#if (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  wxStaticText* m_unicodeGlyphs;
  wxButton* m_getUnicodeFont;
  wxString m_unicodeFont;
#endif
  wxStaticText* label_12;
  wxSpinCtrl* m_defaultPort;
  ExamplePanel* label_11;
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
        m_styleHighlight,
        m_styleText,
        m_styleSubsection,
        m_styleSection,
        m_styleTitle,
        m_styleTextBackground,
        m_styleBackground,
        m_styleCellBracket,
        m_styleActiveCellBracket,
        m_styleCursor,
        m_styleSelection,
        m_styleOutdated;
  void OnOk(wxCommandEvent& event);
  void OnMpBrowse(wxCommandEvent& event);
#if (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  void OnChangeUnicodeFont(wxCommandEvent& event);
#endif
#if defined __WXMSW__
  void OnColorButton(wxCommandEvent& event);
#endif
  void OnGreekBrowse(wxCommandEvent& event);
  void OnChangeStyle(wxCommandEvent& event);
  void OnChangeWarning(wxCommandEvent& event);
  void OnCheckbox(wxCommandEvent& event);
  void OnCheckGreek(wxCommandEvent& event);
  void ReadStyles(wxString file = wxEmptyString);
  void WriteStyles(wxString file = wxEmptyString);
  void SetupFontList();
  void UpdateExample();
  void OnChangeFontFamily(wxCommandEvent& event);
  void LoadSave(wxCommandEvent& event);
  int m_fontSize;
  style* GetStylePointer();
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
