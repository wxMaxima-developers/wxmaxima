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
  Tests wxm::HelpBrowserDiagnosis() (GH #2481): that it says how wxMaxima was
  built, and, on Windows, which of the Edge backend's prerequisites it found.
  The test prints the diagnosis, so a CI log shows what the build it ran in
  actually has.
*/

#define CATCH_CONFIG_RUNNER
#include "HelpBrowserDiagnosis.cpp"
#include <catch2/catch.hpp>
#include <wx/init.h>
#include <iostream>

SCENARIO("The help browser diagnosis says what this build has (GH #2481)") {
  const wxString text = wxm::HelpBrowserDiagnosis();
  std::cout << text.utf8_str() << std::flush;
  CHECK(text.StartsWith(wxS("Integrated help browser:\n")));
#ifdef USE_WEBVIEW
  CHECK(text.Contains(wxS("built with wxWebView: yes")));
#ifdef __WINDOWS__
  CHECK(text.Contains(wxS("built with the Edge (WebView2) backend: ")));
  CHECK(text.Contains(wxS("Edge backend usable")) ==
        text.Contains(wxS("Edge (WebView2) backend: yes")));
#endif
#else
  CHECK(text.Contains(wxS("built without wxWebView")));
#endif
}

int main(int argc, char *argv[]) {
  wxInitializer initializer;
  return Catch::Session().run(argc, argv);
}
