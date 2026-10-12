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
  Why the integrated help browser is, or isn't, available (GH #2481).
*/

#ifndef HELPBROWSERDIAGNOSIS_H
#define HELPBROWSERDIAGNOSIS_H

#include <wx/string.h>

namespace wxm {
/*! A few lines saying what the integrated help browser needs and what was found

  The integrated help browser is a wxWebView. Whether wxMaxima can offer one
  depends on several things, any of which can be missing without an error
  message: wxMaxima has to be built with wxWebView, and on Windows wxWidgets
  must have been built with the Edge (WebView2) backend, the WebView2 loader
  must be found and the WebView2 runtime must be installed. This lists each
  of them, so that a user whose wxMaxima offers no integrated browser can
  tell -- or send in a bug report -- which one is missing.

  The text is deliberately not translated: it is meant to be pasted into bug
  reports, and it names things (backends, DLLs, build options) that have no
  translation anyway.
*/
wxString HelpBrowserDiagnosis();
} // namespace wxm

#endif // HELPBROWSERDIAGNOSIS_H
