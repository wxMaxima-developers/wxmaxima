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
#include "WrappingStaticText.h"
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/wupdlock.h>

HelpBrowser::HelpBrowser(wxWindow *parent, Configuration *configuration,
                         MaximaManual *manual, wxString url):
  wxPanel(parent, wxID_ANY),
  m_maximaManual(manual),
  m_configuration(configuration),
  m_startUrl(url)
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  m_vbox = new wxBoxSizer(wxVERTICAL);
  m_browserPanel = new wxPanel(this, wxID_ANY);
  m_browserPanel->SetSizer(m_vbox);
  vbox->Add(m_browserPanel, wxSizerFlags(1).Expand());
  m_topicSizer = new wxBoxSizer(wxVERTICAL);
  m_topicPanel = new wxPanel(this, wxID_ANY);
  m_topicSizer = new wxBoxSizer(wxVERTICAL);
  m_topicPanel->SetSizer(m_vbox);
  vbox->Add(m_topicPanel, wxSizerFlags(1).Expand());

  SetSizer(vbox);
  Connect(wxEVT_ACTIVATE, wxActivateEventHandler(HelpBrowser::OnActivate),NULL, this);
  FitInside();
}

wxString HelpBrowser::GetKeyword(int id)
{
  if(id < wxID_HIGHEST + 8000)
    return wxEmptyString;
  id -= wxID_HIGHEST + 8000;
  if(id > m_keywords.GetCount())
    return wxEmptyString;
  return m_keywords[id];
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
    m_webView = wxWebView::New(m_browserPanel, wxID_ANY, m_startUrl);
    m_webView->Connect(wxEVT_KEY_DOWN,
                       wxCharEventHandler(HelpBrowser::OnWebviewKeyDown), NULL, m_browserPanel);

    m_webView->SetMinSize(wxSize(GetContentScaleFactor()*100,GetContentScaleFactor()*100));
    #ifdef __WXMSW__
    // Don't emulate bugs in IE7
    m_webView->MSWSetEmulationLevel();
    #endif
    
    m_vbox->Add(m_webView, wxSizerFlags(1).Expand());

    auto *searchbox = new wxBoxSizer(wxHORIZONTAL);
    m_searchText = new wxTextCtrl(m_browserPanel, wxID_ANY, wxEmptyString, wxDefaultPosition,
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
    
    wxButton *upbutton = new wxButton(m_browserPanel, wxID_UP);
    upbutton->Connect(
      wxEVT_BUTTON, wxCommandEventHandler(HelpBrowser::OnSearchUp), NULL, this);
    searchbox->Add(upbutton, wxSizerFlags());
    wxButton *downbutton = new wxButton(m_browserPanel, wxID_DOWN);
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

bool HelpBrowser::AllowOnlineManualP()
{
  if(m_configuration->AllowNetworkHelp())
    return true;
  
  LoggingMessageDialog dialog(this,
                              _("Allow to access a online manual for maxima?"),
                              "Manual", wxCENTER | wxYES_NO | wxCANCEL);
  
  dialog.SetExtendedMessage(_("Didn't find an installed offline manual."));
  
  int result = dialog.ShowModal();

  if(result == wxID_CANCEL)
    return false;

 
  if(result == wxID_YES)
  {
    m_configuration->AllowNetworkHelp(true);
    return true;
  }

  m_configuration->AllowNetworkHelp(false);
  return false;
}

void HelpBrowser::JumpToKeyword(wxString keyword)
{
  wxWindowUpdateLocker speedUp(this);
  wxString maximaHelpURL = m_maximaManual->GetHelpfileURL(keyword);
  m_topicPanel->Show(false);
  
  wxBusyCursor crs;
  if(!maximaHelpURL.IsEmpty())
  {
    wxLogMessage(wxString::Format(_("Opening help file %s"),maximaHelpURL.utf8_str()));
    SetURL(maximaHelpURL);
  }
  else
  {
    if(AllowOnlineManualP())
    {
      wxLogMessage(_(wxT("No offline manual found => Redirecting to the Maxima homepage")));
      SetURL("https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#"+keyword);
    }
  }
  m_browserPanel->Show(true);
  Layout();
}

void HelpBrowser::SelectKeywords(wxArrayString keywords)
{
  wxWindowUpdateLocker speedUp(this);
  if(keywords.GetCount() == 0)
    return;

  if(keywords.GetCount() == 1)
  {
    JumpToKeyword(keywords[0]);
    return;
  }
  
  m_browserPanel->Show(false);
  m_topicPanel->DestroyChildren();
  m_topicSizer->Add(
    new WrappingStaticText(m_topicPanel, wxID_ANY,
                           _("Choose between the following help topics:")),
    wxSizerFlags());
  int id = 6000 + wxID_HIGHEST;

  
  m_keywords = keywords;
  for(auto i:keywords)
  {
    m_topicSizer->Add(
      new wxButton(m_topicPanel, id++, i),
    wxSizerFlags().Expand());
  }
  m_topicPanel->Show(true);
  Layout();
}

void HelpBrowser::SetURL(wxString url)
{
  CreateIfNeeded();
  m_browserPanel->Show(true);
  m_topicPanel->Show(false);
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
