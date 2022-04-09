// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2017-2018      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "ListSortWiz.h"

ListSortWiz::ListSortWiz(Configuration *WXUNUSED(cfg),
                         wxWindow *parent, int id, const wxString &title,
                         wxString list,
                         bool WXUNUSED(eq),
                         const wxPoint &pos,
                         const wxSize &size,
                         long style) :
  wxDialog(parent, id, title, pos, size, style)
{
  wxBoxSizer *vsizer = new wxBoxSizer(wxVERTICAL);
  wxPanel *choicePanel  = new wxPanel(this,-1);
  wxFlexGridSizer *grid = new wxFlexGridSizer(5, 2, 0, 0);

  wxStaticText *listText = new wxStaticText(choicePanel,-1,_("List name:"));
  grid->Add(listText, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  m_list = new wxTextCtrl(choicePanel,-1,list,wxDefaultPosition,wxSize(300,wxDefaultSize.y));
  grid->Add(m_list, wxSizerFlags().Border(wxBOTTOM|wxLEFT, 10));

  wxStaticText *criterionText = new wxStaticText(choicePanel,-1,_("Sort Criterion:"));
  grid->Add(criterionText, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  grid->Add(10,10);

  m_sortTraditional = new wxRadioButton(choicePanel,-1,_("a>b"),
                                        wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  grid->Add(m_sortTraditional, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  grid->Add(10,10);
  m_sortTraditional->SetValue(true);
  
  m_sortFunction    = new wxRadioButton(choicePanel,-1,_("A function f(a,b), named"));
  grid->Add(m_sortFunction, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  m_CriterionFunc = new wxTextCtrl(choicePanel,-1,wxEmptyString,wxDefaultPosition,wxSize(300,wxDefaultSize.y));
  m_CriterionFunc->Connect(wxEVT_TEXT,
                           wxGridEventHandler(ListSortWiz::OnFunctionChange),
                           NULL, this);
  grid->Add(m_CriterionFunc, wxSizerFlags().Border(wxTOP|wxLEFT, 10));

  m_sortLambda      = new wxRadioButton(choicePanel,-1,_("Create f(a,b) on-the-fly, contents:"));
  grid->Add(m_sortLambda, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  m_Criterion = new wxTextCtrl(choicePanel,-1,wxEmptyString,wxDefaultPosition,wxSize(300,wxDefaultSize.y));
  m_Criterion->SetValue(wxT("a<b"));
  m_Criterion->Connect(wxEVT_TEXT,
                       wxGridEventHandler(ListSortWiz::OnLambdaChange),
                       NULL, this);
  grid->Add(m_Criterion, wxSizerFlags().Border(wxTOP|wxLEFT, 10));
  choicePanel->SetSizerAndFit(grid);
  vsizer->Add(choicePanel, wxSizerFlags().Expand().Border(wxALL, 0));
  
  wxPanel *buttonPanel = new wxPanel(this,-1);
#if defined __WXMSW__
  button_1 = new wxButton(buttonPanel, wxID_OK, _("OK"));
  button_2 = new wxButton(buttonPanel, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(buttonPanel, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(buttonPanel, wxID_OK, _("OK"));
#endif
  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  buttonSizer->Add(button_1, 0, wxALL, 5);
  buttonSizer->Add(button_2, 0, wxALL, 5);
  buttonPanel->SetSizer(buttonSizer);
  vsizer->Add(buttonPanel, wxSizerFlags().Right());
  SetSizerAndFit(vsizer);

  set_properties();
  SetAutoLayout(true);
  Layout();
}

void ListSortWiz::set_properties()
{
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif
}

wxString ListSortWiz::GetValue()
{
  wxString retval = wxT("sort(")+m_list->GetValue();
  if(m_sortFunction->GetValue())
    retval += wxT(",") + m_CriterionFunc->GetValue();
  if(m_sortLambda->GetValue())
    retval += wxT(",lambda([a,b],") + m_Criterion->GetValue()+ wxT(")");
  retval += wxT(")");
  return retval;
}
