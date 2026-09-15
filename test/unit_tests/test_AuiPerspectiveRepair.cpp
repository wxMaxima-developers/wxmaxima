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
  Tests RepairAuiPerspective(), which makes a stored wxAUI layout safe to load.
*/

#include "AuiPerspectiveRepair.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

//! The worksheet pane exactly as an older wxMaxima wrote it: dir=5 (centre)
//! with row=2, which is what wxAuiPaneInfo::IsValid() rejects.
static const wxString BAD_CONSOLE_PANE =
  wxS("name=console;caption=;state=16892;dir=5;layer=0;row=2;pos=0;prop=100000;")
  wxS("bestw=100;besth=100;minw=100;minh=100;maxw=-1;maxh=-1;floatx=-1;floaty=-1;")
  wxS("floatw=-1;floath=-1");

//! A docked sidebar: dir=4 (right), and legitimately at a non-zero row.
static const wxString SIDEBAR_PANE =
  wxS("name=structure;caption=Table of Contents;state=2044;dir=4;layer=0;row=1;")
  wxS("pos=0;prop=100000;bestw=300;besth=300;minw=100;minh=100;maxw=-1;maxh=-1;")
  wxS("floatx=-1;floaty=-1;floatw=-1;floath=-1");

SCENARIO("A centre pane stored at a non-zero row is repaired") {
  GIVEN("a perspective written by a wxMaxima that used .Center().Row(2)") {
    wxString perspective =
      wxS("layout2|") + BAD_CONSOLE_PANE + wxS("|") + SIDEBAR_PANE +
      wxS("|dock_size(5,0,0)=100|dock_size(4,0,1)=302|");
    wxString repaired = RepairAuiPerspective(perspective);

    THEN("the centre pane's row is forced to 0") {
      CHECK(repaired.Contains(wxS("dir=5;layer=0;row=0;pos=0")));
      CHECK_FALSE(repaired.Contains(wxS("row=2")));
    }
    THEN("nothing else about the centre pane is disturbed") {
      CHECK(repaired.Contains(wxS("name=console;caption=;state=16892;")));
      CHECK(repaired.Contains(wxS("prop=100000;bestw=100;besth=100")));
    }
    THEN("a docked sidebar keeps the non-zero row it is entitled to") {
      // Only the *centre* pane has to sit at row 0; repairing every pane
      // would silently flatten the user's sidebar arrangement instead.
      CHECK(repaired.Contains(SIDEBAR_PANE));
    }
    THEN("the version header and wxAUI's own dock sizes survive") {
      CHECK(repaired.StartsWith(wxS("layout2|")));
      CHECK(repaired.Contains(wxS("dock_size(5,0,0)=100")));
      CHECK(repaired.Contains(wxS("dock_size(4,0,1)=302")));
    }
    THEN("the entry count is unchanged") {
      CHECK(repaired.Freq('|') == perspective.Freq('|'));
    }
  }
}

SCENARIO("A perspective that needs no repair is passed through untouched") {
  GIVEN("a centre pane already at layer/row/pos 0") {
    wxString perspective =
      wxS("layout2|") + BAD_CONSOLE_PANE + wxS("|");
    // Repairing twice must reach the same string as repairing once: the
    // second pass has nothing left to do.
    wxString once = RepairAuiPerspective(perspective);
    THEN("repairing it again changes nothing") {
      CHECK(RepairAuiPerspective(once) == once);
    }
  }
  GIVEN("a perspective with no centre pane at all") {
    wxString perspective = wxS("layout2|") + SIDEBAR_PANE + wxS("|");
    THEN("it comes back byte for byte") {
      CHECK(RepairAuiPerspective(perspective) == perspective);
    }
  }
  GIVEN("input this function has no business understanding") {
    // A perspective is opaque, versioned, wxAUI-owned data. Anything
    // unrecognised has to survive rather than be "corrected".
    CHECK(RepairAuiPerspective(wxEmptyString) == wxEmptyString);
    CHECK(RepairAuiPerspective(wxS("garbage")) == wxS("garbage"));
    CHECK(RepairAuiPerspective(wxS("layout2|")) == wxS("layout2|"));
    CHECK(RepairAuiPerspective(wxS("|||")) == wxS("|||"));
  }
}

SCENARIO("Fields that merely look like geometry are left alone") {
  GIVEN("a centre pane whose caption contains text resembling a field") {
    // Captions are stored in this same ';'-separated string and hold
    // translated text, so a naive search-and-replace could corrupt one.
    wxString perspective =
      wxS("layout2|name=console;caption=row=2 layer=9;state=16892;dir=5;")
      wxS("layer=0;row=2;pos=0;prop=100000|");
    wxString repaired = RepairAuiPerspective(perspective);
    THEN("the real geometry field is repaired") {
      CHECK(repaired.Contains(wxS(";layer=0;row=0;pos=0;")));
    }
    THEN("the caption is not") {
      CHECK(repaired.Contains(wxS("caption=row=2 layer=9;")));
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
