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
  Tests for WxMaximaManualAnchors: the keywords context-sensitive help looks up
  in wxMaxima's own manual, and that every manual really has their anchors.

  The anchors are hand-written `<div id="..."></div>` lines in info/*.md, and
  the keyword list is in the code, so nothing else would notice if a manual
  edit dropped one, or if a keyword was added to the list but not to a manual:
  Help would then silently open the top of the manual.
*/

#define CATCH_CONFIG_RUNNER
#include "WxMaximaManualAnchors.cpp"
#include <catch2/catch.hpp>
#include <wx/dir.h>
#include <wx/ffile.h>
#include <wx/init.h>

namespace {
wxString ReadFile(const wxString &name) {
  wxFFile file(name, wxS("rb"));
  wxString contents;
  if (file.IsOpened())
    file.ReadAll(&contents, wxConvUTF8);
  return contents;
}

//! The English manual and every translation of it that is in the source tree
std::vector<wxString> Manuals(const wxString &extension) {
  std::vector<wxString> manuals;
  const wxString infoDir = wxS(WXM_SOURCE_DIR) wxS("/info");
  wxArrayString files;
  wxDir::GetAllFiles(infoDir, &files, wxS("wxmaxima*.") + extension, wxDIR_FILES);
  for (const auto &file : files)
    manuals.push_back(file);
  return manuals;
}
} // namespace

SCENARIO("wxMaxima's own commands are found, Maxima's are not") {
  THEN("wx_matrix is documented in wxMaxima's manual") {
    REQUIRE(WxMaximaManualAnchors::AnchorFor(wxS("wx_matrix")) == wxS("wx_matrix"));
  }
  THEN("a trailing parenthesis, as in a selected call, is ignored") {
    REQUIRE(WxMaximaManualAnchors::AnchorFor(wxS("table_form(")) == wxS("table_form"));
  }
  THEN("a Maxima command isn't") {
    REQUIRE(WxMaximaManualAnchors::AnchorFor(wxS("sin")).IsEmpty());
    REQUIRE(WxMaximaManualAnchors::AnchorFor(wxEmptyString).IsEmpty());
  }
}

SCENARIO("HtmlHasAnchor finds exactly the anchor it is asked for") {
  const wxString html = wxS("<p>x</p>\n<div id=\"wx_matrix\">\n\n</div>\n");
  REQUIRE(WxMaximaManualAnchors::HtmlHasAnchor(html, wxS("wx_matrix")));
  REQUIRE_FALSE(WxMaximaManualAnchors::HtmlHasAnchor(html, wxS("wx_mat")));
  REQUIRE_FALSE(WxMaximaManualAnchors::HtmlHasAnchor(html, wxEmptyString));
}

SCENARIO("Every manual has an anchor for every keyword") {
  const auto manuals = Manuals(wxS("md"));
  // The English manual and at least one translation
  REQUIRE(manuals.size() > 1);
  for (const auto &manual : manuals) {
    const wxString contents = ReadFile(manual);
    REQUIRE_FALSE(contents.IsEmpty());
    for (const auto &keyword : WxMaximaManualAnchors::Keywords()) {
      INFO("manual: " << manual.utf8_string() << ", keyword: " << keyword.utf8_string());
      CHECK(contents.Contains(wxS("<div id=\"") + keyword + wxS("\"></div>")));
    }
  }
}

SCENARIO("The English HTML manual shipped for builds without pandoc has every anchor") {
  const wxString html = ReadFile(wxS(WXM_SOURCE_DIR) wxS("/info/wxmaxima.html"));
  REQUIRE_FALSE(html.IsEmpty());
  for (const auto &keyword : WxMaximaManualAnchors::Keywords()) {
    INFO("keyword: " << keyword.utf8_string());
    CHECK(WxMaximaManualAnchors::HtmlHasAnchor(html, keyword));
  }
}

int main(int argc, char *argv[]) {
  wxInitializer init;
  return Catch::Session().run(argc, argv);
}
