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
#include <wx/webview.h>
#include <wx/scrolwin.h>
#include <wx/panel.h>
#include "Configuration.h"

/* The help browser sidebar
 */
class HelpBrowser : public wxScrolled<wxPanel>
{
public:
  explicit HelpBrowser(wxWindow *parent, Configuration *configuration, wxString url);
  void SetURL(wxString url);
private:
  wxWebView *m_webView;
  Configuration *m_configuration;
};

#endif // HELPBROWSER_H
