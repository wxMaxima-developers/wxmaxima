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
  Tests wxMathML::GetCmd(), which squeezes wxMathML.lisp into the one line it
  is sent to Maxima as.
*/

#include <wx/app.h>
#include <wx/sysopt.h>
#include "wxMathml.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

/*! How deep the parentheses outside strings are nested at the end of a lisp text

  0 means every form is closed. GetCmd() drops comments, so none are expected.
*/
static long OpenParens(const wxString &lisp) {
  long depth = 0;
  bool inString = false;
  for (auto ch = lisp.begin(); ch != lisp.end(); ++ch) {
    if (*ch == wxS('\\')) {
      // Escapes the next character, inside strings and in #\x alike
      if (++ch == lisp.end())
        break;
      continue;
    }
    if (*ch == wxS('"'))
      inString = !inString;
    else if (!inString) {
      if (*ch == wxS('('))
        ++depth;
      else if (*ch == wxS(')'))
        --depth;
    }
  }
  return inString ? -1000000 : depth;
}

// GetCmd() strips ";" comments line by line. It once forgot at each new line
// that it was inside a string, so a ";" on the second line of a docstring cut
// off the line with the closing quote, and the rest of the file became part
// of that string: Maxima loaded none of what followed, without any error.
SCENARIO("The command wxMaxima sends Maxima contains every form of wxMathML.lisp") {
  wxMathML mathml(nullptr);
  wxString cmd = mathml.GetCmd();
  REQUIRE(cmd.StartsWith(wxS(":lisp-quiet ")));
  REQUIRE(OpenParens(cmd) == 0);
  REQUIRE(cmd.Contains(wxS("(finish-output)")));
}

class MyApp : public wxApp
{
public:
  MyApp() {
    // See test_ImgCell.cpp: suppresses wx 3.3's modal manifest warning
    wxSystemOptions::SetOption(wxS("msw.no-manifest-check"), 1);
  }
  bool OnInit() override {
    int rc = Catch::Session().run();
    std::exit(rc);
    return false;
  }
};

IMPLEMENT_APP(MyApp);
