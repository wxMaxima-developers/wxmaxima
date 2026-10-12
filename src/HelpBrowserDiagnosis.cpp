// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Why the integrated help browser is, or isn't, available; see
  wxm::HelpBrowserDiagnosis().
*/

#include "HelpBrowserDiagnosis.h"
#include "BuildConfig.h"

#include <wx/defs.h>
#ifdef USE_WEBVIEW
#include <wx/webview.h>
#include <wx/versioninfo.h>
#endif
#ifdef __WINDOWS__
#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>
#endif

namespace {
[[maybe_unused]] wxString YesNo(bool yes) { return yes ? wxS("yes") : wxS("no"); }

#ifdef USE_WEBVIEW
//! The version a backend reports, or a note that it reports none
[[maybe_unused]] wxString BackendVersion(const char *backend) {
  const wxVersionInfo info = wxWebView::GetBackendVersionInfo(wxString(backend));
  const wxString version = info.GetVersionString();
  if (!version.empty())
    return version;
  // Some backends put just a name, or nothing at all, there.
  return info.GetName().empty() ? wxString(wxS("unknown")) : info.GetName();
}
#endif
} // namespace

wxString wxm::HelpBrowserDiagnosis() {
  wxString text = wxS("Integrated help browser:\n");
#ifndef USE_WEBVIEW
  text += wxS("  wxMaxima was built without wxWebView (WXM_DISABLE_WEBVIEW,\n"
              "  or wxWidgets' webview library was missing), so it can only\n"
              "  show the manual in an external browser.\n");
#else
  text += wxS("  wxMaxima built with wxWebView: yes\n");
#ifdef __WINDOWS__
  // Only the Edge backend is offered (see
  // Configuration::OfferInternalHelpBrowser()); the old Internet Explorer one
  // can't show the manual properly.
#if defined(wxUSE_WEBVIEW_EDGE) && wxUSE_WEBVIEW_EDGE
  text += wxS("  wxWidgets built with the Edge (WebView2) backend: yes\n");
#if defined(wxUSE_WEBVIEW_EDGE_STATIC) && wxUSE_WEBVIEW_EDGE_STATIC
  text += wxS("  WebView2 loader: linked into wxMaxima\n");
#else
  {
    // wxWidgets loads WebView2Loader.dll at run time; without it the Edge
    // backend silently reports itself unavailable.
    const wxString dll = wxS("WebView2Loader.dll");
    const wxFileName beside(
      wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath(), dll);
    wxDynamicLibrary loader;
    const bool loads = loader.Load(dll, wxDL_DEFAULT | wxDL_QUIET);
    text += wxS("  ") + dll + wxS(" next to wxMaxima: ") +
      YesNo(beside.FileExists()) + wxS("\n");
    text += wxS("  ") + dll + wxS(" can be loaded: ") + YesNo(loads) + wxS("\n");
  }
#endif
  const bool edge = wxWebView::IsBackendAvailable(wxWebViewBackendEdge);
  text += wxS("  Edge backend usable (needs the WebView2 runtime): ") +
    YesNo(edge) + wxS("\n");
  if (edge)
    text += wxS("  WebView2 runtime version: ") +
      BackendVersion(wxWebViewBackendEdge) + wxS("\n");
#else
  text += wxS("  wxWidgets built with the Edge (WebView2) backend: no\n"
              "  (wxUSE_WEBVIEW_EDGE was 0 when wxWidgets was compiled. The\n"
              "  WebView2 runtime alone can't help then: wxWidgets has to be\n"
              "  rebuilt with the WebView2 SDK and wxUSE_WEBVIEW_EDGE=1.)\n");
#endif
#else
  const bool available = wxWebView::IsBackendAvailable(wxWebViewBackendDefault);
  text += wxS("  Default wxWebView backend usable: ") + YesNo(available) + wxS("\n");
  if (available)
    text += wxS("  Backend: ") + BackendVersion(wxWebViewBackendDefault) + wxS("\n");
#endif
#endif
  return text;
}
