// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
  This file declares the class HelpBrowser.

  HelpBrowser creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#ifndef HELPBROWSER_H
#define HELPBROWSER_H

#include <wx/dir.h>
#include "precomp.h"
#include <wx/wx.h>
#ifdef USE_WEBVIEW
#include <wx/webview.h>
#endif
#include <wx/panel.h>
#include <wx/sizer.h>
#include "Configuration.h"
#include "MaximaManual.h"

/* The help browser sidebar
 */
class HelpBrowser : public wxPanel
{
public:
  //! Ask the user if we are allowed to access an online manual
  static bool AllowOnlineManualP(Configuration *configuration, wxWindow *parent);
  //! Ask the user if we are allowed to access an online manual
  bool AllowOnlineManualP(){return AllowOnlineManualP(m_configuration, this);}
  explicit HelpBrowser(wxWindow *parent, Configuration *configuration, MaximaManual *manual,
                       wxString url);
#ifdef USE_WEBVIEW
  void SetURL(wxString url);
  void JumpToKeyword(wxString keyword);
  void SelectKeywords(wxArrayString keywords);
  wxString GetKeyword(unsigned int id);
  
private:
  void CreateIfNeeded();
  void OnTextEnter(wxCommandEvent& event);
  void OnSearchUp(wxCommandEvent& event);
  void OnSearchDown(wxCommandEvent& event);
  void OnSearchboxKeyDown(wxKeyEvent &event);
  void OnWebviewKeyDown(wxKeyEvent &event);
  void OnActivate(wxActivateEvent &event);
  void OnTopicButton(wxCommandEvent& event);

  MaximaManual *m_maximaManual = NULL;
  wxWebView *m_webView = NULL;
  wxTextCtrl *m_searchText = NULL;
  Configuration *m_configuration;
  wxString m_startUrl;
  bool m_findDown = true;
  wxBoxSizer *m_vbox;
  wxPanel *m_browserPanel;
  wxPanel *m_topicPanel;
  wxBoxSizer *m_topicSizer;
  wxArrayString m_keywords;
#endif
};

#endif // HELPBROWSER_H
