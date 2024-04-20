// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef GENWIZ_PANEL_H
#define GENWIZ_PANEL_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/statline.h>
#include <vector>
#include <wx/cshelp.h>
#include "../BTextCtrl.h"
#include <wx/notebook.h>
#include "MaximaManual.h"
#include <wx/hashmap.h>
#include <unordered_map>


/*! A dockable, embeddable generic wizard

  The generic wizard dialogue we use almost everywhere where a wizard is needed.
*/
class GenWizPanel : public wxPanel
{
public:
  GenWizPanel(wxWindow *parent, Configuration *cfg, MaximaManual *manual,
              const wxString &description, const wxString &description_tooltip,
              const wxString &commandRule,
              const wxString &label1, const wxString &defaultval1, const wxString &tooltip1 = {},
              const wxString &label2 = {}, const wxString &defaultval2 = {}, const wxString &tooltip2 = {},
              const wxString &label3 = {}, const wxString &defaultval3 = {}, const wxString &tooltip3 = {},
              const wxString &label4 = {}, const wxString &defaultval4 = {}, const wxString &tooltip4 = {},
              const wxString &label5 = {}, const wxString &defaultval5 = {}, const wxString &tooltip5 = {},
              const wxString &label6 = {}, const wxString &defaultval6 = {}, const wxString &tooltip6 = {},
              const wxString &label7 = {}, const wxString &defaultval7 = {}, const wxString &tooltip7 = {},
              const wxString &label8 = {}, const wxString &defaultval8 = {}, const wxString &tooltip8 = {},
              const wxString &label9 = {}, const wxString &defaultval9 = {}, const wxString &tooltip9 = {});

  GenWizPanel(wxWindow *parent, Configuration *cfg, MaximaManual *manual,
              const wxString &description = {}, const wxString &description_tooltip = {},
              const wxString &commandRule = {}, bool dockable = true,
              const wxString &label1 = {}, const wxString &defaultval1 = {}, const wxString &tooltip1 = {},
              const wxString &label2 = {}, const wxString &defaultval2 = {}, const wxString &tooltip2 = {},
              const wxString &label3 = {}, const wxString &defaultval3 = {}, const wxString &tooltip3 = {},
              const wxString &label4 = {}, const wxString &defaultval4 = {}, const wxString &tooltip4 = {},
              const wxString &label5 = {}, const wxString &defaultval5 = {}, const wxString &tooltip5 = {},
              const wxString &label6 = {}, const wxString &defaultval6 = {}, const wxString &tooltip6 = {},
              const wxString &label7 = {}, const wxString &defaultval7 = {}, const wxString &tooltip7 = {},
              const wxString &label8 = {}, const wxString &defaultval8 = {}, const wxString &tooltip8 = {},
              const wxString &label9 = {}, const wxString &defaultval9 = {}, const wxString &tooltip9 = {});

  //! Replace the contents of this panel with a new wizard
  void NewWizard(wxString description, const wxString &description_tooltip,
                 const wxString &commandRule,
                 const wxString &label1, const wxString &defaultval1, const wxString &tooltip1,
                 const wxString &label2, const wxString &defaultval2, const wxString &tooltip2,
                 const wxString &label3, const wxString &defaultval3, const wxString &tooltip3,
                 const wxString &label4, const wxString &defaultval4, const wxString &tooltip4,
                 const wxString &label5, const wxString &defaultval5, const wxString &tooltip5,
                 const wxString &label6, const wxString &defaultval6, const wxString &tooltip6,
                 const wxString &label7, const wxString &defaultval7, const wxString &tooltip7,
                 const wxString &label8, const wxString &defaultval8, const wxString &tooltip8,
                 const wxString &label9, const wxString &defaultval9, const wxString &tooltip9);

  //! Returns the contents of the nth field.
  wxString operator[](int i) const { return m_textctrl[i]->GetValue(); }

  //! Returns the command the wizard has generated, if commandRule wasn't empty
  wxString GetOutput() const {return m_output->GetValue();}

#if defined __WXMSW__
  wxButton *GetOKButton() const {return button_1;}
  wxButton *GetAbortButton() const {return button_2;}
#else
  wxButton *GetOKButton() const {return button_2;}
  wxButton *GetAbortButton() const {return button_1;}
#endif
  wxButton *GetInsertButton() const{return m_insertButton;}
  wxString GetHelpKeyword(wxWindowID ID);
  virtual ~GenWizPanel();
protected:
  typedef std::unordered_map <wxString, int, wxStringHash> keywordHash;

  void OnSize(wxSizeEvent &event);
  wxNotebook *m_notebook;
  void UpdateOutput();
  void OnParamChange(wxCommandEvent& event);
  void OnTextEnter(wxCommandEvent& event);
  void OnNotebookPageChange(wxBookCtrlEvent& event);

  std::vector<BTextCtrl*> m_textctrl;
  std::vector<wxStaticText *> m_label;
  wxButton *button_1;
  wxButton *button_2;
  wxContextHelpButton *m_helpButton = NULL;
  wxButton *m_insertButton;
  wxString m_commandRule;
  wxTextCtrl *m_output;
  wxString m_description;
  wxString m_descriptionToolTip;
  wxString m_title;
  Configuration *m_configuration;
  MaximaManual *m_maximaManual;
private:
  int m_topicButtonID0;
  std::vector<wxWindowIDRef> m_topicButtonIDs;
  bool m_ignorePageChange = true;
  keywordHash m_manualKeywords;
};

#endif // GENWIZ_PANEL_H
