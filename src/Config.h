///
///  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include <wx_inc.h>
#include <wx/image.h>

#include <wx/propdlg.h>
#include <wx/generic/propdlg.h>
#include <wx/spinctrl.h>
#include <wx/notebook.h>

#include <wx/imaglist.h>
#include <wx/bookctrl.h>
#include <wx/artprov.h>

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
  button_mathFont,
  font_family,
  style_font_family,
  language_id,
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

class Config: public wxPropertySheetDialog
{
public:
  Config(wxWindow* parent);
  ~Config();
  void OnChangeColor(); // called from class ColorPanel
  void WriteSettings();
private:
  // begin wxGlade: Config::methods
  void SetProperties();
  wxPanel* CreateOptionsPanel();
  wxPanel* CreateStylePanel();
  wxPanel* CreateMaximaPanel();
  // end wxGlade
protected:
  // begin wxGlade: Config::attributes
  wxTextCtrl* m_maximaProgram;
  wxButton* m_mpBrowse;
  wxTextCtrl* m_additionalParameters;
  wxComboBox* m_language;
  wxCheckBox* m_saveSize;
  wxCheckBox* m_savePanes;
  wxCheckBox* m_matchParens;
  wxCheckBox* m_showLong;
  wxCheckBox* m_enterEvaluates;
  wxCheckBox* m_saveUntitled;
  wxCheckBox* m_openHCaret;
  wxCheckBox* m_insertAns;
  wxCheckBox* m_fixReorderedIndices;
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
  wxCheckBox* m_useJSMath;
  wxCheckBox* m_keepPercentWithSpecials;
  wxBookCtrlBase* m_notebook;
  wxStaticText* m_mathFont;
  wxButton* m_getMathFont;
  wxString m_mathFontName;
  wxButton *m_saveStyle, *m_loadStyle;
  wxSpinCtrl* m_defaultPort;
  ExamplePanel* m_examplePanel;
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
  void OnClose(wxCloseEvent& event);
  void OnMpBrowse(wxCommandEvent& event);
#if defined __WXMSW__
  void OnColorButton(wxCommandEvent& event);
#endif
  void OnMathBrowse(wxCommandEvent& event);
  void OnChangeStyle(wxCommandEvent& event);
  void OnChangeWarning(wxCommandEvent& event);
  void OnCheckbox(wxCommandEvent& event);
  void ReadStyles(wxString file = wxEmptyString);
  void WriteStyles(wxString file = wxEmptyString);
  void SetupFontList();
  void UpdateExample();
  void OnChangeFontFamily(wxCommandEvent& event);
  void LoadSave(wxCommandEvent& event);
  int m_fontSize, m_mathFontSize;
  style* GetStylePointer();
  wxImageList *m_imageList;
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
