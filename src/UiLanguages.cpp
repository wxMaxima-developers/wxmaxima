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
*/

#include "UiLanguages.h"
#include <wx/intl.h>
#include <algorithm>

namespace {
//! Does the text start with a right-to-left letter (Hebrew, Arabic, ...)?
bool StartsRightToLeft(const wxString &text) {
  if (text.IsEmpty())
    return false;
  const auto code = static_cast<wxUint32>(text[0].GetValue());
  return ((code >= 0x0590) && (code < 0x0900)) ||
    ((code >= 0xFB1D) && (code < 0xFE00)) ||
    ((code >= 0xFE70) && (code < 0xFF00));
}
} // namespace

wxString wxm::UiLanguageSortKey(const wxString &name) {
  // What U+00C0 + n decomposes to, starting with its base letter. Letters
  // that don't decompose (Æ, ß, Ł, ...) stand for themselves.
  static const wxString fold =
    wxS("AAAAAAÆCEEEEIIIIÐNOOOOO×ØUUUUYÞßaaaaaaæceeeeiiiiðnooooo÷øuuuuyþy")
    wxS("AaAaAaCcCcCcCcDdĐđEeEeEeEeEeGgGgGgGgHhĦħIiIiIiIiIıĲĳJjKkĸLlLlLlĿ")
    wxS("ŀŁłNnNnNnŉŊŋOoOoOoŒœRrRrRrSsSsSsSsTtTtŦŧUuUuUuUuUuUuWwYyYZzZzZzſ");
  wxString key;
  for (const auto ch : name) {
    const auto code = static_cast<wxUint32>(wxUniChar(ch).GetValue());
    if ((code >= 0xC0) && (code < 0xC0 + fold.Length()))
      key += fold[code - 0xC0];
    else
      key += ch;
  }
  return key.Lower();
}

std::vector<wxm::UiLanguage> wxm::UiLanguages() {
  // The native names are the ones the Unicode CLDR uses for each language.
  std::vector<UiLanguage> languages = {
    {wxLANGUAGE_AFRIKAANS, _("Afrikaans"), wxS("Afrikaans")},
    {wxLANGUAGE_ALBANIAN, _("Albanian"), wxS("Shqip")},
    {wxLANGUAGE_ARABIC, _("Arabic"), wxS("العربية")},
    {wxLANGUAGE_BASQUE, _("Basque"), wxS("Euskara")},
    {wxLANGUAGE_CATALAN, _("Catalan"), wxS("Català")},
    {wxLANGUAGE_CHINESE_SIMPLIFIED, _("Chinese (Simplified)"), wxS("简体中文")},
    {wxLANGUAGE_CHINESE_TRADITIONAL, _("Chinese (traditional)"), wxS("繁體中文")},
    {wxLANGUAGE_CORSICAN, _("Corsican"), wxS("Corsu")},
    {wxLANGUAGE_CROATIAN, _("Croatian"), wxS("Hrvatski")},
    {wxLANGUAGE_CZECH, _("Czech"), wxS("Čeština")},
    {wxLANGUAGE_DANISH, _("Danish"), wxS("Dansk")},
    {wxLANGUAGE_DUTCH, _("Dutch"), wxS("Nederlands")},
    {wxLANGUAGE_ENGLISH, _("English"), wxS("English")},
    {wxLANGUAGE_FINNISH, _("Finnish"), wxS("Suomi")},
    {wxLANGUAGE_FRENCH, _("French"), wxS("Français")},
    {wxLANGUAGE_GALICIAN, _("Galician"), wxS("Galego")},
    {wxLANGUAGE_GEORGIAN, _("Georgian"), wxS("ქართული")},
    {wxLANGUAGE_GERMAN, _("German"), wxS("Deutsch")},
    {wxLANGUAGE_GREEK, _("Greek"), wxS("Ελληνικά")},
    {wxLANGUAGE_HEBREW, _("Hebrew"), wxS("עברית")},
    {wxLANGUAGE_HINDI, _("Hindi"), wxS("हिन्दी")},
    {wxLANGUAGE_HUNGARIAN, _("Hungarian"), wxS("Magyar")},
    {wxLANGUAGE_INDONESIAN, _("Indonesian"), wxS("Bahasa Indonesia")},
    {wxLANGUAGE_ITALIAN, _("Italian"), wxS("Italiano")},
    {wxLANGUAGE_JAPANESE, _("Japanese"), wxS("日本語")},
    {wxLANGUAGE_KABYLE, _("Kabyle"), wxS("Taqbaylit")},
    {wxLANGUAGE_KOREAN, _("Korean"), wxS("한국어")},
    {wxLANGUAGE_LATVIAN, _("Latvian"), wxS("Latviešu")},
    {wxLANGUAGE_LITHUANIAN, _("Lithuanian"), wxS("Lietuvių")},
    {wxLANGUAGE_MALAY, _("Malay"), wxS("Bahasa Melayu")},
    {wxLANGUAGE_NEPALI, _("Nepali"), wxS("नेपाली")},
    {wxLANGUAGE_NORWEGIAN_BOKMAL, _("Norwegian"), wxS("Norsk bokmål")},
    {wxLANGUAGE_FARSI, _("Persian"), wxS("فارسی")},
    {wxLANGUAGE_POLISH, _("Polish"), wxS("Polski")},
    {wxLANGUAGE_PORTUGUESE, _("Portuguese"), wxS("Português")},
    {wxLANGUAGE_PORTUGUESE_BRAZILIAN, _("Portuguese (Brazilian)"),
     wxS("Português (Brasil)")},
    {wxLANGUAGE_ROMANIAN, _("Romanian"), wxS("Română")},
    {wxLANGUAGE_RUSSIAN, _("Russian"), wxS("Русский")},
    {wxLANGUAGE_SERBIAN, _("Serbian"), wxS("Српски")},
    {wxLANGUAGE_SLOVAK, _("Slovak"), wxS("Slovenčina")},
    {wxLANGUAGE_SLOVENIAN, _("Slovenian"), wxS("Slovenščina")},
    {wxLANGUAGE_SPANISH, _("Spanish"), wxS("Español")},
    {wxLANGUAGE_SWEDISH, _("Swedish"), wxS("Svenska")},
    {wxLANGUAGE_TAMIL, _("Tamil"), wxS("தமிழ்")},
    {wxLANGUAGE_TURKISH, _("Turkish"), wxS("Türkçe")},
    {wxLANGUAGE_UKRAINIAN, _("Ukrainian"), wxS("Українська")},
    {wxLANGUAGE_VIETNAMESE, _("Vietnamese"), wxS("Tiếng Việt")},
  };
  // Sorted by the name that does not change with the UI language, so a
  // language stays where its speakers last saw it. This puts the Latin
  // script first and the others after it, grouped by script, which is how
  // the operating systems' own language choosers do it, too.
  std::sort(languages.begin(), languages.end(),
            [](const UiLanguage &a, const UiLanguage &b) {
              return UiLanguageSortKey(a.nativeName) <
                UiLanguageSortKey(b.nativeName);
            });
  languages.insert(languages.begin(),
                   {wxLANGUAGE_DEFAULT, _("(Use default language)"), wxString()});
  return languages;
}

wxString wxm::UiLanguageLabel(const UiLanguage &language) {
  if (language.nativeName.IsEmpty())
    return language.name;
  if (language.name.IsSameAs(language.nativeName, false))
    return language.nativeName;
  wxString label = language.nativeName + wxS(" — ") + language.name;
  // A label starting with a Hebrew or Arabic name would be laid out right to
  // left as a whole, which puts the native name on the right, unlike in
  // every other entry. A left-to-right mark in front keeps them aligned.
  if (StartsRightToLeft(language.nativeName))
    label = wxString(wxUniChar(0x200E)) + label;
  return label;
}
