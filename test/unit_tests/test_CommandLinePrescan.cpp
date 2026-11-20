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
  Tests for finding --language on the command line before it is parsed.
  GUI-free.
*/

#include "CommandLinePrescan.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::cmdline::PrescanLongOption;

namespace {
std::vector<wxString> Args(std::initializer_list<const char *> args) {
  std::vector<wxString> result{wxS("wxmaxima")};
  for (const char *arg : args)
    result.emplace_back(wxString::FromUTF8(arg));
  return result;
}
} // namespace

SCENARIO("An option can be found before the command line is parsed") {
  THEN("Both spellings wxCmdLineParser accepts are found") {
    CHECK(PrescanLongOption(Args({"--language=de"}), wxS("language")) ==
          wxString(wxS("de")));
    CHECK(PrescanLongOption(Args({"--language", "pt_BR"}), wxS("language")) ==
          wxString(wxS("pt_BR")));
  }
  THEN("It is found among other options and files") {
    CHECK(PrescanLongOption(Args({"-e", "--ini=x.ini", "--language=fr",
                                  "file.wxmx"}),
                            wxS("language")) == wxString(wxS("fr")));
  }
  THEN("The last occurrence wins, as it does for the parser") {
    CHECK(PrescanLongOption(Args({"--language=de", "--language", "ru"}),
                            wxS("language")) == wxString(wxS("ru")));
  }
  THEN("A missing option, or one without a value, gives nothing") {
    CHECK(!PrescanLongOption(Args({"-e", "file.wxmx"}), wxS("language")));
    CHECK(!PrescanLongOption(Args({"--language"}), wxS("language")));
  }
  THEN("Options that merely start alike are not mistaken for it") {
    CHECK(!PrescanLongOption(Args({"--languages=de"}), wxS("language")));
    CHECK(!PrescanLongOption(Args({"-language=de"}), wxS("language")));
  }
  THEN("After \"--\" everything is a file name") {
    CHECK(!PrescanLongOption(Args({"--", "--language=de"}), wxS("language")));
  }
  THEN("The program name is never taken for an option") {
    const std::vector<wxString> args{wxS("--language=de")};
    CHECK(!PrescanLongOption(args, wxS("language")));
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
