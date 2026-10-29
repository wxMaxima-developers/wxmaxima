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
  Tests for the UI language list (GH #849): every language is findable by
  its own name, whatever the UI language currently is. GUI-free.
*/

#include "UiLanguages.h"
#include <wx/intl.h>
#include <set>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
const wxm::UiLanguage *Find(const std::vector<wxm::UiLanguage> &languages,
                            int id) {
  for (const auto &language : languages)
    if (language.id == id)
      return &language;
  return nullptr;
}
} // namespace

SCENARIO("The language chooser lists each language under its own name") {
  const auto languages = wxm::UiLanguages();
  REQUIRE(languages.size() > 1);

  THEN("\"Use the default language\" comes first") {
    CHECK(languages.front().id == wxLANGUAGE_DEFAULT);
    CHECK(languages.front().nativeName.IsEmpty());
  }
  THEN("Every language has its own native name, and none appears twice") {
    std::set<int> ids;
    std::set<wxString> nativeNames;
    for (std::size_t i = 1; i < languages.size(); ++i) {
      INFO(languages[i].name);
      CHECK(!languages[i].nativeName.IsEmpty());
      CHECK(ids.insert(languages[i].id).second);
      CHECK(nativeNames.insert(languages[i].nativeName).second);
    }
  }
  THEN("The languages are sorted by their native names") {
    for (std::size_t i = 2; i < languages.size(); ++i) {
      INFO(languages[i - 1].nativeName << " vs. " << languages[i].nativeName);
      CHECK(wxm::UiLanguageSortKey(languages[i - 1].nativeName) <
            wxm::UiLanguageSortKey(languages[i].nativeName));
    }
  }
  THEN("An accented letter sorts next to its base letter") {
    CHECK(wxm::UiLanguageSortKey(wxS("Čeština")) <
          wxm::UiLanguageSortKey(wxS("Dansk")));
    CHECK(wxm::UiLanguageSortKey(wxS("Català")) <
          wxm::UiLanguageSortKey(wxS("Čeština")));
  }
  THEN("A right-to-left native name still comes first in its label") {
    const auto *hebrew = Find(languages, wxLANGUAGE_HEBREW);
    REQUIRE(hebrew != nullptr);
    CHECK(wxm::UiLanguageLabel(*hebrew) ==
          wxString(wxUniChar(0x200E)) + wxS("עברית — Hebrew"));
  }
  THEN("A language's label starts with its native name") {
    for (std::size_t i = 1; i < languages.size(); ++i) {
      wxString label = wxm::UiLanguageLabel(languages[i]);
      label.Replace(wxString(wxUniChar(0x200E)), wxEmptyString);
      CHECK(label.StartsWith(languages[i].nativeName));
    }
  }
  THEN("The translated name follows where it differs from the native one") {
    // No translation is loaded here, so the translated names are English.
    const auto *german = Find(languages, wxLANGUAGE_GERMAN);
    REQUIRE(german != nullptr);
    CHECK(wxm::UiLanguageLabel(*german) == wxString(wxS("Deutsch — German")));
    const auto *english = Find(languages, wxLANGUAGE_ENGLISH);
    REQUIRE(english != nullptr);
    CHECK(wxm::UiLanguageLabel(*english) == wxString(wxS("English")));
  }
  THEN("The two Chinese variants are named by their script, not a region") {
    const auto *simplified = Find(languages, wxLANGUAGE_CHINESE_SIMPLIFIED);
    const auto *traditional = Find(languages, wxLANGUAGE_CHINESE_TRADITIONAL);
    REQUIRE(simplified != nullptr);
    REQUIRE(traditional != nullptr);
    CHECK(simplified->nativeName == wxString(wxS("简体中文")));
    CHECK(traditional->nativeName == wxString(wxS("繁體中文")));
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
