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
  Tests for UrlDetection: where a link in a text cell starts and ends, which
  links may be opened, and that the exporters' placeholders round-trip
  (GH #2396). GUI-free, so tested directly.
*/

#include "UrlDetection.h"

// Not CATCH_CONFIG_MAIN: with _UNICODE defined (as wxWidgets does on Windows)
// Catch then defines wmain(), which MinGW's startup code does not look for --
// the link fails with "undefined symbol: WinMain". Every test here therefore
// supplies its own main(), at the end of this file.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::FindUrls;
using wxm::UrlSpan;

namespace {
//! The links FindUrls() finds in text, as strings.
std::vector<wxString> Links(const wxString &text) {
  std::vector<wxString> result;
  for (const auto &span : FindUrls(text))
    result.push_back(text.Mid(span.start, span.length));
  return result;
}
} // namespace

SCENARIO("Links are found where they are") {
  THEN("text without a link has none") {
    REQUIRE(FindUrls(wxS("")).empty());
    REQUIRE(FindUrls(wxS("no link here")).empty());
    REQUIRE(FindUrls(wxS("x: y := z")).empty());
  }
  THEN("a bare link is found whole, with its position") {
    const wxString text = wxS("see https://example.org/a/b?x=1&y=2#top now");
    const auto spans = FindUrls(text);
    REQUIRE(spans.size() == 1);
    REQUIRE(spans[0] == UrlSpan{4, 35});
    REQUIRE(Links(text).at(0) == wxS("https://example.org/a/b?x=1&y=2#top"));
  }
  THEN("http, https and mailto are all recognized, in any case") {
    REQUIRE(Links(wxS("http://a.org")) == std::vector<wxString>{wxS("http://a.org")});
    REQUIRE(Links(wxS("HTTPS://A.ORG")) == std::vector<wxString>{wxS("HTTPS://A.ORG")});
    REQUIRE(Links(wxS("mailto:me@a.org")) == std::vector<wxString>{wxS("mailto:me@a.org")});
  }
  THEN("several links in one text are all found") {
    REQUIRE(Links(wxS("a http://x.org b https://y.org c")) ==
            std::vector<wxString>{wxS("http://x.org"), wxS("https://y.org")});
  }
  THEN("other schemes are not links") {
    REQUIRE(FindUrls(wxS("file:///etc/passwd")).empty());
    REQUIRE(FindUrls(wxS("ftp://a.org")).empty());
    REQUIRE(FindUrls(wxS("javascript:alert(1)")).empty());
  }
  THEN("a scheme in the middle of a word doesn't start a link") {
    REQUIRE(FindUrls(wxS("xhttp://a.org")).empty());
    REQUIRE(FindUrls(wxS("a_https://a.org")).empty());
  }
  THEN("a scheme with nothing after it is not a link") {
    REQUIRE(FindUrls(wxS("https://")).empty());
    REQUIRE(FindUrls(wxS("mailto: someone")).empty());
    REQUIRE(FindUrls(wxS("use https://.")).empty());
  }
}

SCENARIO("A link ends where the sentence around it takes over") {
  THEN("whitespace, including a tab, a newline and a non-breaking space, ends it") {
    REQUIRE(Links(wxS("https://a.org b")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org\tb")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org\nb")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org\u00A0b")).at(0) == wxS("https://a.org"));
  }
  THEN("characters a URL can't contain unescaped end it") {
    REQUIRE(Links(wxS("<https://a.org>")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("\"https://a.org\"")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org{x}")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org\\x")).at(0) == wxS("https://a.org"));
  }
  THEN("sentence punctuation after it is left out") {
    REQUIRE(Links(wxS("See https://a.org.")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org, and")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("https://a.org!?")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("'https://a.org'")).at(0) == wxS("https://a.org"));
  }
  THEN("punctuation inside it is kept") {
    REQUIRE(Links(wxS("https://a.org/x.html.")).at(0) ==
            wxS("https://a.org/x.html"));
    REQUIRE(Links(wxS("https://a.org:8080/x")).at(0) ==
            wxS("https://a.org:8080/x"));
  }
  THEN("an unbalanced closing parenthesis belongs to the sentence") {
    REQUIRE(Links(wxS("(see https://a.org)")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("(see https://a.org).")).at(0) == wxS("https://a.org"));
    REQUIRE(Links(wxS("[https://a.org]")).at(0) == wxS("https://a.org"));
  }
  THEN("a balanced one belongs to the link") {
    REQUIRE(Links(wxS("https://en.wikipedia.org/wiki/Mass_(physics)")).at(0) ==
            wxS("https://en.wikipedia.org/wiki/Mass_(physics)"));
    REQUIRE(Links(wxS("(https://en.wikipedia.org/wiki/Mass_(physics))")).at(0) ==
            wxS("https://en.wikipedia.org/wiki/Mass_(physics)"));
  }
}

SCENARIO("Only web and mail links may be opened") {
  REQUIRE(wxm::IsLaunchableUrl(wxS("https://a.org")));
  REQUIRE(wxm::IsLaunchableUrl(wxS("http://a.org")));
  REQUIRE(wxm::IsLaunchableUrl(wxS("mailto:me@a.org")));
  REQUIRE(wxm::IsLaunchableUrl(wxS("HTTPS://A.ORG")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS("")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS("https://")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS("file:///etc/passwd")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS("javascript:alert(1)")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS("/usr/bin/xterm")));
  REQUIRE_FALSE(wxm::IsLaunchableUrl(wxS(" https://a.org")));
}

SCENARIO("Links survive an exporter's escaping via placeholders") {
  GIVEN("a text with two links and characters exporters escape") {
    const wxString text =
      wxS("a_b https://a.org/x_y%20z#q & http://b.org 100%");
    std::vector<wxString> links;
    const wxString protectedText = wxm::ProtectUrls(text, links);

    THEN("the links are taken out and nothing else is touched") {
      REQUIRE(links == std::vector<wxString>{wxS("https://a.org/x_y%20z#q"),
                                             wxS("http://b.org")});
      REQUIRE_FALSE(protectedText.Contains(wxS("://")));
      REQUIRE(protectedText.StartsWith(wxS("a_b ")));
      REQUIRE(protectedText.EndsWith(wxS(" 100%")));
    }
    THEN("escaping the rest leaves the placeholders intact") {
      wxString escaped = protectedText;
      escaped.Replace(wxS("_"), wxS("\\_"));
      escaped.Replace(wxS("%"), wxS("\\%"));
      escaped.Replace(wxS("&"), wxS("\\&"));
      escaped.Replace(wxS("#"), wxS("\\#"));
      const wxString restored = wxm::RestoreUrls(
        escaped, links,
        [](const wxString &url) { return wxS("\\url{") + url + wxS("}"); });
      REQUIRE(restored ==
              wxS("a\\_b \\url{https://a.org/x_y%20z#q} \\& \\url{http://b.org} 100\\%"));
    }
    THEN("restoring without changes gives the original text back") {
      REQUIRE(wxm::RestoreUrls(protectedText, links,
                               [](const wxString &url) { return url; }) == text);
    }
  }
  GIVEN("a text without links") {
    std::vector<wxString> links;
    THEN("it is returned unchanged") {
      REQUIRE(wxm::ProtectUrls(wxS("plain 100%"), links) == wxS("plain 100%"));
      REQUIRE(links.empty());
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
