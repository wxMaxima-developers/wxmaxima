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
  Which words of a wizard's command a "Help on" button might be offered for.
*/

#ifndef WIZARDHELPKEYWORDS_H
#define WIZARDHELPKEYWORDS_H

#include <wx/string.h>
#include <vector>

namespace wxm {
  /*! The words and operators of a wizard's command rule that might have help

    \p commandRule is the template the wizard fills in, e.g.
    "ode2(#1#,#2#,#3#);". The result lists, in the order they first appear and
    each once, every identifier ("ode2") and every operator the manual might
    document ("'", "''", "!", "#", "\\", and ";" or "$" when they separate two
    statements). Whether the manual actually has an entry for each of them is
    for the caller to check: this only does the text analysis, so it can be
    tested without a manual.

    Two things this deliberately leaves out:
     - The "#1#", "#2#", ... placeholders: they become the user's input.
     - The ";" or "$" that ends the command. Every wizard's command ends with
       one, so offering help on it says nothing about this particular wizard
       -- and the Solve ODE wizard used to offer nothing else, since its
       "ode2" contains a digit and wasn't recognized as a word at all.
  */
  std::vector<wxString> WizardHelpKeywords(const wxString &commandRule);
}

#endif // WIZARDHELPKEYWORDS_H
