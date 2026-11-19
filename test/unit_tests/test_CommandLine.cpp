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
  Tests for CommandLine: how wxMaxima's command line becomes settings.
  Several of these pin down what fuzz_cmdline (test/fuzz/) found.
*/

#include "CommandLine.h"
#include "CommandLineOptions.h"

#include <wx/filename.h>
#include <wx/init.h>
#include <wx/msgout.h>

#include <cstdio>
#include <cstdlib>
#include <vector>

// Not CATCH_CONFIG_MAIN: see test_SyntaxErrorLocation.cpp for why.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::cmdline::DiffAliasArguments;
using wxm::cmdline::Interpret;
using wxm::cmdline::Settings;

namespace {
//! Collects what the parser prints instead of letting it reach the terminal
class CapturedOutput : public wxMessageOutput {
public:
  CapturedOutput() : m_previous(wxMessageOutput::Set(this)) {}
  ~CapturedOutput() override { wxMessageOutput::Set(m_previous); }
  void Output(const wxString &str) override { text += str; }
  wxString text;

private:
  wxMessageOutput *m_previous;
};

//! Parses a command line (program name first) the way main.cpp does
struct Parsed {
  explicit Parsed(const std::vector<wxString> &args) {
    CapturedOutput output;
    result = wxm::cmdline::Parse(parser, DiffAliasArguments(args), false);
    settings = Interpret(parser);
    messages = output.text;
  }
  wxCmdLineParser parser;
  int result = 0;
  Settings settings;
  wxString messages;
};

wxString Absolute(const wxString &file) {
  wxFileName name(file);
  name.MakeAbsolute();
  return name.GetFullPath();
}
} // namespace

SCENARIO("Every option in the table is known to the parser") {
  for (const auto &opt : wxm::cmdline::Options) {
    if (opt.isHelp)
      continue;
    std::vector<wxString> args{wxS("wxmaxima"),
                               wxS("--") + wxString(opt.longName.data())};
    if (opt.kind == wxm::cmdline::Kind::Option)
      args.push_back(wxS("value"));
    Parsed parsed(args);
    INFO(opt.longName.data());
    CHECK(parsed.result == 0);
  }
}

SCENARIO("Options become settings") {
  Parsed parsed({wxS("wxmaxima"), wxS("-b"), wxS("--pipe"), wxS("-l"),
                 wxS("sbcl"), wxS("--use-version=5.47"), wxS("-o"),
                 wxS("a.wxm#123"), wxS("b.mac"), wxS("--ini"),
                 wxS("my.ini")});
  REQUIRE(parsed.result == 0);
  CHECK(parsed.settings.batch);
  CHECK(parsed.settings.pipe);
  CHECK_FALSE(parsed.settings.diff);
  CHECK(parsed.settings.extraMaximaArgs == wxS(" -l sbcl -u 5.47"));
  REQUIRE(parsed.settings.ini);
  CHECK(*parsed.settings.ini == Absolute(wxS("my.ini")));
  // --open's file first, with its cell address kept
  REQUIRE(parsed.settings.filesToOpen.size() == 2);
  CHECK(parsed.settings.filesToOpen[0] == Absolute(wxS("a.wxm")) + wxS("#123"));
  CHECK(parsed.settings.filesToOpen[1] == Absolute(wxS("b.mac")));
}

SCENARIO("--diff keeps its files to itself") {
  Parsed parsed({wxS("wxmaxima"), wxS("--diff"), wxS("a.wxmx"), wxS("b.wxmx")});
  REQUIRE(parsed.result == 0);
  CHECK(parsed.settings.diff);
  CHECK(parsed.settings.params ==
        std::vector<wxString>{wxS("a.wxmx"), wxS("b.wxmx")});
  CHECK(parsed.settings.filesToOpen.empty());
}

SCENARIO("wxmxdiff compares exactly the files it was given") {
  GIVEN("Two files") {
    Parsed parsed({wxS("/usr/bin/wxmxdiff"), wxS("a.wxmx"), wxS("b.wxmx")});
    REQUIRE(parsed.result == 0);
    CHECK(parsed.settings.diff);
    // The program used to be compared with the files: its own name was
    // passed on as the first file.
    CHECK(parsed.settings.params ==
          std::vector<wxString>{wxS("a.wxmx"), wxS("b.wxmx")});
  }
  GIVEN("File names with spaces and quotes") {
    Parsed parsed({wxS("wxmxdiff"), wxS("my file.wxm"), wxS("it's \"old\".wxm")});
    REQUIRE(parsed.result == 0);
    CHECK(parsed.settings.params ==
          std::vector<wxString>{wxS("my file.wxm"), wxS("it's \"old\".wxm")});
  }
  GIVEN("A file called -d after --") {
    // Found by fuzz_cmdline: the "-d" was taken for the switch, so no
    // --diff was added and the files were opened instead of compared.
    Parsed parsed({wxS("wxmxdiff"), wxS("a.wxm"), wxS("--"), wxS("-d")});
    REQUIRE(parsed.result == 0);
    CHECK(parsed.settings.diff);
    CHECK(parsed.settings.params ==
          std::vector<wxString>{wxS("a.wxm"), wxS("-d")});
  }
  GIVEN("An explicit --diff") {
    const std::vector<wxString> args{wxS("WxMxDiff.exe"), wxS("--diff"),
                                     wxS("a"), wxS("b")};
    CHECK(DiffAliasArguments(args) == args);
  }
  GIVEN("Any other program name") {
    const std::vector<wxString> args{wxS("wxmaxima"), wxS("a"), wxS("b")};
    CHECK(DiffAliasArguments(args) == args);
  }
}

SCENARIO("An option name wxCmdLineParser can't read is an error, not an assertion") {
  // Found by fuzz_cmdline: "--" followed by a character that can't be part of
  // an option name made wxWidgets assert (wxString::Last() of an empty name).
  // The test binary would abort on that assertion.
  const std::vector<wxString> unreadable{
    wxS("--+"), wxS("--="), wxS("--=value"), wxS("--.wxm"),
    wxString::FromUTF8("--\xc3\xa9t\xc3\xa9")};
  for (const auto &bad : unreadable) {
    INFO(bad);
    Parsed parsed({wxS("wxmaxima"), wxS("-e"), bad, wxS("file.wxm")});
    CHECK(parsed.result > 0);
    CHECK(parsed.messages.Contains(bad.Mid(2)));
    // Not the name it was replaced by while finding out
    CHECK_FALSE(parsed.messages.Contains(wxS("wxm-probe")));
  }
  GIVEN("The same text as the value of an option") {
    Parsed parsed({wxS("wxmaxima"), wxS("-o"), wxS("--+.wxm")});
    REQUIRE(parsed.result == 0);
    REQUIRE(parsed.settings.filesToOpen.size() == 1);
    CHECK(parsed.settings.filesToOpen[0] == Absolute(wxS("--+.wxm")));
  }
  GIVEN("The same text after --") {
    Parsed parsed({wxS("wxmaxima"), wxS("--"), wxS("--=.wxm")});
    REQUIRE(parsed.result == 0);
    CHECK(parsed.settings.params == std::vector<wxString>{wxS("--=.wxm")});
  }
  GIVEN("--help in front of it") {
    // Parsing stops at --help; that is what is reported.
    Parsed parsed({wxS("wxmaxima"), wxS("--help"), wxS("--+")});
    CHECK(parsed.result == -1);
  }
}

int main(int argc, char *argv[]) {
  wxInitializer init;
  // A failed wxWidgets assertion is a failed test, not a message box
  wxSetAssertHandler([](const wxString &file, int line, const wxString &,
                        const wxString &cond, const wxString &msg) {
    fprintf(stderr, "wxWidgets assertion failed: %s:%d %s %s\n", // flawfinder: ignore -- constant format string
            static_cast<const char *>(file.utf8_str()), line,
            static_cast<const char *>(cond.utf8_str()),
            static_cast<const char *>(msg.utf8_str()));
    abort();
  });
  return Catch::Session().run(argc, argv);
}
