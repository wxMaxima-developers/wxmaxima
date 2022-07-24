// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2019 Gunter Königsmann     <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class HelpBrowser.

  HelpBrowser creates the list of maxima's manual anchors.
*/

#include "HelpBrowser.h"
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/button.h>

HelpBrowser::HelpBrowser(wxWindow *parent, Configuration *configuration, wxString url):
  wxPanel(parent, wxID_ANY),
  m_configuration(configuration),
  m_startUrl(url)
{
  m_vbox = new wxBoxSizer(wxVERTICAL);
  Connect(wxEVT_ACTIVATE, wxActivateEventHandler(HelpBrowser::OnActivate),NULL, this);
  SetSizer(m_vbox);
  FitInside();
}

void HelpBrowser::OnActivate(wxActivateEvent &WXUNUSED(event))
{
  CreateIfNeeded();
}

void HelpBrowser::CreateIfNeeded()
{
  if(m_webView == NULL)
  {
    wxLogMessage(_("Instantiating the HTML manual browser"));
    m_webView = wxWebView::New(this, wxID_ANY, m_startUrl);
    m_webView->Connect(wxEVT_KEY_DOWN,
                       wxCharEventHandler(HelpBrowser::OnWebviewKeyDown), NULL, this);

    m_webView->SetMinSize(wxSize(GetContentScaleFactor()*100,GetContentScaleFactor()*100));
    #ifdef __WXMSW__
    // Don't emulate bugs in IE7
    m_webView->MSWSetEmulationLevel();
    #endif
    
    m_vbox->Add(m_webView, wxSizerFlags(1).Expand());

    auto *searchbox = new wxBoxSizer(wxHORIZONTAL);
    m_searchText = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition,
                                  wxDefaultSize, wxTE_PROCESS_ENTER);
    searchbox->Add(m_searchText, wxSizerFlags(1).Expand());
    m_webView->Connect(wxEVT_KEY_DOWN,
                       wxCharEventHandler(HelpBrowser::OnSearchboxKeyDown), NULL, this);

    m_searchText->Connect(wxEVT_TEXT_ENTER,
                          wxCommandEventHandler(HelpBrowser::OnTextEnter),
                          NULL, this);
    Connect(wxID_FIND, wxEVT_MENU,
            wxCommandEventHandler(HelpBrowser::OnTextEnter), NULL, this);
    m_webView->Connect(wxID_FIND, wxEVT_MENU,
            wxCommandEventHandler(HelpBrowser::OnTextEnter), NULL, this);
    
    wxButton *upbutton = new wxButton(this, wxID_UP);
    upbutton->Connect(
      wxEVT_BUTTON, wxCommandEventHandler(HelpBrowser::OnSearchUp), NULL, this);
    searchbox->Add(upbutton, wxSizerFlags());
    wxButton *downbutton = new wxButton(this, wxID_DOWN);
    downbutton->Connect(
      wxEVT_BUTTON, wxCommandEventHandler(HelpBrowser::OnSearchDown), NULL, this);
    searchbox->Add(downbutton, wxSizerFlags());
    m_vbox->Add(searchbox, wxSizerFlags().Expand());
  }
}

void HelpBrowser::OnSearchboxKeyDown(wxKeyEvent &event)
{
  if(event.ControlDown() && (event.GetUnicodeKey() == wxT('F')))
  {
    wxCommandEvent dummy;
    OnTextEnter(dummy);
  }
  else
    event.Skip();
}

void HelpBrowser::OnWebviewKeyDown(wxKeyEvent &event)
{
  if(event.ControlDown() && (event.GetUnicodeKey() == wxT('F')))
    m_searchText->SetFocus();
  else
    event.Skip();
}

void HelpBrowser::SetURL(wxString url)
{
  CreateIfNeeded();
  m_webView->LoadURL(url);
}

void HelpBrowser::OnTextEnter(wxCommandEvent& event)
{
  wxWebViewFindFlags flags = wxWEBVIEW_FIND_DEFAULT;
  if(!m_findDown)
    flags = wxWEBVIEW_FIND_BACKWARDS;
  wxString searchString = m_searchText->GetValue();
  m_webView->Find(searchString, flags);
  event.Skip();
}

void HelpBrowser::OnSearchUp(wxCommandEvent& event)
{
  m_findDown = false;
  m_webView->Find(m_searchText->GetValue(), wxWEBVIEW_FIND_DEFAULT| wxWEBVIEW_FIND_BACKWARDS);
}
void HelpBrowser::OnSearchDown(wxCommandEvent& event)
{
  m_findDown = true;
  m_webView->Find(m_searchText->GetValue(), wxWEBVIEW_FIND_DEFAULT);
}
