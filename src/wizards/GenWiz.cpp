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

#include "GenWiz.h"
#include <wx/persist/toplevel.h>

GenWiz::GenWiz(wxWindow *parent, Configuration *cfg, const wxString &title,
               const wxString &description, const wxString &description_tooltip,
               const wxString &commandRule,
               wxString label1, wxString defaultval1, wxString tooltip1,
               wxString label2, wxString defaultval2, wxString tooltip2,
               wxString label3, wxString defaultval3, wxString tooltip3,
               wxString label4, wxString defaultval4, wxString tooltip4,
               wxString label5, wxString defaultval5, wxString tooltip5,
               wxString label6, wxString defaultval6, wxString tooltip6,
               wxString label7, wxString defaultval7, wxString tooltip7,
               wxString label8, wxString defaultval8, wxString tooltip8,
               wxString label9, wxString defaultval9, wxString tooltip9) :
  wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
    wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN),
  m_commandRule(commandRule)
{
  SetName(title);
  wxFlexGridSizer *vbox =
    new wxFlexGridSizer(1,
                        wxSize(5*GetContentScaleFactor(), 5*GetContentScaleFactor()));
  m_description = new WrappingStaticText(this, wxID_ANY, description);
  if(description.IsEmpty())
    m_description->Show(false);
  m_description->SetToolTip(description_tooltip);
  vbox->Add(m_description, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()));
  
  for(int i = 0; i< 9; i++)
  {
    m_label.push_back(new wxStaticText(this, -1, wxEmptyString));
    m_textctrl.push_back(new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                                       wxSize(300*GetContentScaleFactor(), -1)));
  }

  m_textctrl[0]->SetValue(defaultval1);
  m_label[0]->SetLabel(label1);
  m_label[0]->SetToolTip(tooltip1);
  m_textctrl[1]->SetToolTip(tooltip1);

  m_textctrl[1]->SetValue(defaultval2);
  m_label[1]->SetLabel(label2);
  m_label[1]->Show(!label2.IsEmpty());
  m_textctrl[1]->Show(!label2.IsEmpty());
  m_label[1]->SetToolTip(tooltip2);
  m_textctrl[1]->SetToolTip(tooltip2);

  m_textctrl[2]->SetValue(defaultval3);
  m_label[2]->SetLabel(label3);
  m_label[2]->Show(!label3.IsEmpty());
  m_textctrl[2]->Show(!label3.IsEmpty());
  m_label[2]->SetToolTip(tooltip3);
  m_textctrl[2]->SetToolTip(tooltip3);

  m_textctrl[3]->SetValue(defaultval4);
  m_label[3]->SetLabel(label4);
  m_label[3]->Show(!label4.IsEmpty());
  m_textctrl[3]->Show(!label4.IsEmpty());
  m_label[3]->SetToolTip(tooltip4);
  m_textctrl[3]->SetToolTip(tooltip4);

  m_label[4]->SetLabel(label5);
  m_textctrl[4]->SetValue(defaultval5);
  m_label[4]->Show(!label5.IsEmpty());
  m_textctrl[4]->Show(!label5.IsEmpty());
  m_label[4]->SetToolTip(tooltip5);
  m_textctrl[4]->SetToolTip(tooltip5);

  m_label[5]->SetLabel(label6);
  m_textctrl[5]->SetValue(defaultval6);
  m_label[5]->Show(!label6.IsEmpty());
  m_textctrl[5]->Show(!label6.IsEmpty());
  m_label[5]->SetToolTip(tooltip6);
  m_textctrl[5]->SetToolTip(tooltip6);

  m_label[6]->SetLabel(label7);
  m_textctrl[6]->SetValue(defaultval7);
  m_label[6]->Show(!label7.IsEmpty());
  m_textctrl[6]->Show(!label7.IsEmpty());
  m_label[6]->SetToolTip(tooltip7);
  m_textctrl[6]->SetToolTip(tooltip7);

  m_label[7]->SetLabel(label8);
  m_textctrl[7]->SetValue(defaultval8);
  m_label[7]->Show(!label8.IsEmpty());
  m_textctrl[7]->Show(!label8.IsEmpty());
  m_label[7]->SetToolTip(tooltip8);
  m_textctrl[7]->SetToolTip(tooltip8);

  m_label[8]->SetLabel(label9);
  m_textctrl[8]->SetValue(defaultval9);
  m_label[8]->Show(!label9.IsEmpty());
  m_textctrl[8]->Show(!label9.IsEmpty());
  m_label[8]->SetToolTip(tooltip9);
  m_textctrl[8]->SetToolTip(tooltip9);
    
  m_textctrl[0]->SetFocus();
  wxFlexGridSizer *grid_sizer =
    new wxFlexGridSizer(2,
                        wxSize(5*GetContentScaleFactor(), 5*GetContentScaleFactor()));
  grid_sizer->AddGrowableCol(1);
  for(int i = 0; i<9; i++)
  {
    grid_sizer->Add(m_label[i], 0, wxALIGN_CENTER_VERTICAL, 0);
    m_textctrl[i]->Connect(wxEVT_TEXT,
                           wxCommandEventHandler(GenWiz::OnParamChange),
                           NULL, this);
    grid_sizer->Add(m_textctrl[i], wxSizerFlags(1).Expand());
  }
  
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_1->SetDefault();
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
  button_2->SetDefault();
#endif
    
  vbox->Add(grid_sizer, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()));
  
//  if(m_warning != NULL)
//    grid_sizer->Add(m_warning, 0, wxALL, 5);
  m_output = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition,
                            wxDefaultSize,
                            wxTE_READONLY|wxTE_MULTILINE|wxTE_CHARWRAP);
  wxStaticBoxSizer *resultBox = new wxStaticBoxSizer(wxVERTICAL, this, _("Maxima Code:"));
  resultBox->Add(m_output, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()).Expand());
  vbox->Add(resultBox, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()).Expand());
  if(commandRule.IsEmpty())
    resultBox->Show(false);

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  buttonSizer->Add(button_1, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()));
  buttonSizer->Add(button_2, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()));

  vbox->Add(buttonSizer, wxSizerFlags(1).Border(wxALL, 5*GetContentScaleFactor()));
  
  // SetAutoLayout(true);
  UpdateOutput();
  SetSizerAndFit(vbox);
  
//  wxPersistenceManager::Get().RegisterAndRestore(this);
}

void GenWiz::UpdateOutput()
{
  wxString output(m_commandRule);
  for(int i=0;i<m_textctrl.size();i++)
    output.Replace(wxString::Format("#%i#",i+1), m_textctrl[i]->GetValue());
  m_output->SetValue(output);
}

void GenWiz::OnParamChange(wxCommandEvent& event)
{
  UpdateOutput();
}
