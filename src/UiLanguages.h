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
  The languages wxMaxima's user interface can be switched to (GH #849).

  Each language is listed under its own name ("Deutsch", "日本語"), followed
  by its name in the current UI language where that differs. Someone who
  ended up with a UI language they cannot read still finds their own
  language in the list that way, which a list of names translated into the
  unreadable language would not let them do.

  Deliberately no flags: a flag stands for a country, not a language.
  English, Spanish, Portuguese, Arabic or Chinese would each need one
  country picked over the others, Catalan, Basque, Corsican, Galician or
  Kabyle would need a regional or no flag at all, and Traditional Chinese
  is written in more than one place whose flag is a political statement.
  Names are what the operating systems use for the same purpose, and the
  two Chinese variants are named by their script, not by a region.
*/
#ifndef UILANGUAGES_H
#define UILANGUAGES_H

#include <wx/string.h>
#include <vector>

namespace wxm {

//! One entry of the UI language list
struct UiLanguage {
  //! The wxLanguage this entry selects
  int id;
  //! The language's name, translated into the current UI language
  wxString name;
  /*! The language's name in that language itself, never translated.

    Empty for the "use the system's default language" entry.
  */
  wxString nativeName;
};

/*! The UI languages, in the order the language chooser lists them

  The "use the default language" entry comes first, then every language
  sorted by its own name. The translated names are looked up on every
  call, so the result follows a UI language change.
*/
std::vector<UiLanguage> UiLanguages();

/*! The key the language list is sorted by: a language's native name

  A plain comparison of code points would sort "Čeština" after "Türkçe".
  wxWidgets offers no collation that works without the OS having that
  locale installed, so this folds the Latin-1 and Latin Extended-A letters
  (U+00C0 to U+017F, which covers every name in the list) to the first
  character of their canonical decomposition, Č to C and ã to a, and
  ignores case. Other scripts sort by code point, which groups them by
  script after the Latin one, as the operating systems' language choosers
  do.
*/
wxString UiLanguageSortKey(const wxString &name);

/*! The text the language chooser shows for a language

  "Deutsch — German" in an English UI, "Deutsch" alone in a German one.
*/
wxString UiLanguageLabel(const UiLanguage &language);

} // namespace wxm

#endif // UILANGUAGES_H
