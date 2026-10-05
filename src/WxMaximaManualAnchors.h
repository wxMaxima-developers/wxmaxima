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

  Context-sensitive help used to know only Maxima's manual, whose index
  wxMaxima reads from the manual itself (see MaximaManual). The commands
  wxMaxima adds to Maxima - wx_matrix(), table_form(), wxstatusbar(), the wx...
  variables - are documented in wxMaxima's own manual (info/wxmaxima.md)
  instead, so they got no "Help on" entry at all.

  wxMaxima's manual has no index to read: it is prose, and its headings, and
  therefore the ids pandoc derives from them, are translated. So each of these
  keywords has an explicit `<div id="keyword"></div>` in front of the paragraph
  that documents it, in the English manual and in every translated one, and
  this file lists them. test_WxMaximaManualAnchors checks that the two stay in
  step.
*/

#ifndef WXMAXIMAMANUALANCHORS_H
#define WXMAXIMAMANUALANCHORS_H

#include <wx/string.h>
#include <vector>

namespace WxMaximaManualAnchors {
  //! Every keyword wxMaxima's own manual has an anchor for
  const std::vector<wxString> &Keywords();

  /*! The anchor in wxMaxima's manual that documents \p keyword

    Empty if wxMaxima's manual doesn't document it. A trailing "(" (as in
    "wx_matrix(") is ignored, so that the word a user selected can be passed
    as it is.
  */
  wxString AnchorFor(wxString keyword);

  /*! Whether the HTML text \p html contains the anchor \p anchor

    A translated manual that was converted to HTML before an anchor was added
    to the manual lacks it; the help browser then has to be pointed at a
    manual that has it.
  */
  bool HtmlHasAnchor(const wxString &html, const wxString &anchor);
}

#endif // WXMAXIMAMANUALANCHORS_H
