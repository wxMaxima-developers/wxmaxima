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
  Tests SaveAsDefaults(), what the "Save As" dialog starts out with (GH #2440).
*/

#include "SaveAsDefaults.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

SCENARIO("Save As proposes a name whose extension matches the file type") {
  GIVEN("a document that was opened from a .wxm file") {
    const auto proposal = SaveAsDefaults(wxS("/home/me/notes.wxm"), wxS("wxmx"),
                                         wxS("untitled"));
    THEN("it is proposed as a .wxm, with the .wxm file type selected") {
      // The bug: the name came without its extension, and wxGTK appended the
      // first file type's one, so this was proposed (and saved) as .wxmx.
      CHECK(proposal.name == wxS("notes.wxm"));
      CHECK(proposal.extension == wxS("wxm"));
      CHECK(proposal.filterIndex == 1);
    }
  }
  GIVEN("a document that was opened from a .wxmx file") {
    const auto proposal = SaveAsDefaults(wxS("/home/me/notes.wxmx"), wxS("wxm"),
                                         wxS("untitled"));
    THEN("it is proposed as a .wxmx, whatever was saved last") {
      CHECK(proposal.name == wxS("notes.wxmx"));
      CHECK(proposal.filterIndex == 0);
    }
  }
  GIVEN("an extension in upper case") {
    const auto proposal = SaveAsDefaults(wxS("/home/me/NOTES.WXM"), wxS("wxmx"),
                                         wxS("untitled"));
    THEN("it is recognised, and the name gets the extension the type implies") {
      CHECK(proposal.filterIndex == 1);
      CHECK(proposal.name == wxS("NOTES.wxm"));
    }
  }
  GIVEN("a document opened from a file wxMaxima cannot save in that format") {
    const auto proposal = SaveAsDefaults(wxS("/home/me/package.mac"), wxS("wxm"),
                                         wxS("untitled"));
    THEN("it is offered as a .wxmx under its old name") {
      CHECK(proposal.name == wxS("package.wxmx"));
      CHECK(proposal.filterIndex == 0);
    }
  }
  GIVEN("a file name with more than one dot") {
    const auto proposal = SaveAsDefaults(wxS("/home/me/lecture.v2.wxm"), wxS("wxmx"),
                                         wxS("untitled"));
    THEN("only the real extension counts") {
      CHECK(proposal.name == wxS("lecture.v2.wxm"));
      CHECK(proposal.filterIndex == 1);
    }
  }
}

SCENARIO("An untitled document gets the type that was saved last") {
  GIVEN("the last document was saved as .wxm") {
    const auto proposal = SaveAsDefaults(wxEmptyString, wxS("wxm"), wxS("untitled"));
    THEN("the untitled one is proposed as a .wxm") {
      CHECK(proposal.name == wxS("untitled.wxm"));
      CHECK(proposal.filterIndex == 1);
    }
  }
  GIVEN("no usable default") {
    const auto proposal = SaveAsDefaults(wxEmptyString, wxEmptyString, wxS("untitled"));
    THEN("it is a .wxmx") {
      CHECK(proposal.name == wxS("untitled.wxmx"));
      CHECK(proposal.filterIndex == 0);
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
