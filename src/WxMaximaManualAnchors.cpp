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
  The commands and variables wxMaxima's own manual documents.
*/

#include "WxMaximaManualAnchors.h"
#include <algorithm>

namespace WxMaximaManualAnchors {

const std::vector<wxString> &Keywords() {
  // Only the commands Maxima's own manual doesn't document: For the others
  // (wxdraw2d(), wxhistogram(), with_slider_draw(), ...) MaximaManual already
  // leads to the description of the Maxima command they wrap, which is where
  // their arguments are explained.
  static const std::vector<wxString> keywords = {
    wxS("wxsubscripts"),        wxS("wxdeclare_subscript"),
    wxS("wxstatusbar"),         wxS("wxworksheettohtml"),
    wxS("wxworksheettotex"),    wxS("wxplot2d"),
    wxS("wxplot3d"),            wxS("wximplicit_plot"),
    wxS("wxcontour_plot"),      wxS("wxplot_size"),
    wxS("with_slider"),         wxS("wxanimate"),
    wxS("wxanimate_framerate"), wxS("wxanimate_autoplay"),
    wxS("wxfilename"),          wxS("wxdirname"),
    wxS("wxplot_pngcairo"),     wxS("wxchangedir"),
    wxS("wxmaximaversion"),     wxS("wxwidgetsversion"),
    wxS("wxdirs"),              wxS("table_form"),
    wxS("wx_matrix"),           wxS("wxbuild_info"),
    wxS("wxbug_report"),        wxS("wx_version_min")
  };
  return keywords;
}

wxString AnchorFor(wxString keyword) {
  keyword.Trim(true).Trim(false);
  if (keyword.EndsWith(wxS("(")))
    keyword.RemoveLast();
  const auto &keywords = Keywords();
  if (std::find(keywords.begin(), keywords.end(), keyword) == keywords.end())
    return wxEmptyString;
  // The anchors are named after the keyword they document.
  return keyword;
}

bool HtmlHasAnchor(const wxString &html, const wxString &anchor) {
  if (anchor.IsEmpty())
    return false;
  return html.Contains(wxS("id=\"") + anchor + wxS("\""));
}

} // namespace WxMaximaManualAnchors
