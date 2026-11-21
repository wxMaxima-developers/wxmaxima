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
  Tests for reading a screenshot scene's [Screenshot] group. GUI-free.
*/

#include "Screenshot.h"
#include <wx/fileconf.h>
#include <wx/sstream.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::ParseScreenshotTarget;
using Kind = wxm::ScreenshotTarget::Kind;

SCENARIO("Screenshot/show names what the screenshot shows") {
  THEN("Nothing, or \"window\", means the main window") {
    REQUIRE(ParseScreenshotTarget(wxEmptyString));
    CHECK(ParseScreenshotTarget(wxEmptyString)->kind == Kind::Window);
    REQUIRE(ParseScreenshotTarget(wxS("window")));
    CHECK(ParseScreenshotTarget(wxS("window"))->kind == Kind::Window);
  }
  THEN("\"worksheet\" means the worksheet") {
    REQUIRE(ParseScreenshotTarget(wxS("worksheet")));
    CHECK(ParseScreenshotTarget(wxS("worksheet"))->kind == Kind::Worksheet);
  }
  THEN("\"cells\" means the cells, drawn without the screen") {
    REQUIRE(ParseScreenshotTarget(wxS("cells")));
    CHECK(ParseScreenshotTarget(wxS("cells"))->kind == Kind::Cells);
  }
  THEN("A sidebar is named by its pane name") {
    const auto target = ParseScreenshotTarget(wxS("pane:structure"));
    REQUIRE(target);
    CHECK(target->kind == Kind::Pane);
    CHECK(target->pane == wxString(wxS("structure")));
  }
  THEN("A configuration dialog page is named by its number") {
    const auto target = ParseScreenshotTarget(wxS("config:1"));
    REQUIRE(target);
    CHECK(target->kind == Kind::Config);
    CHECK(target->page == 1);
  }
  THEN("Anything else is refused") {
    CHECK(!ParseScreenshotTarget(wxS("everything")));
    CHECK(!ParseScreenshotTarget(wxS("pane:")));
    CHECK(!ParseScreenshotTarget(wxS("config:")));
    CHECK(!ParseScreenshotTarget(wxS("config:-1")));
    CHECK(!ParseScreenshotTarget(wxS("config:first")));
  }
}

namespace {
//! A configuration read from text, never touching a file
std::unique_ptr<wxFileConfig> Config(const char *text) {
  wxStringInputStream stream(wxString::FromUTF8(text));
  return std::make_unique<wxFileConfig>(stream);
}
} // namespace

SCENARIO("A scene's [Screenshot] group is read completely") {
  GIVEN("A scene that names everything") {
    const auto config = Config("[Screenshot]\n"
                               "show=pane:structure\n"
                               "sidebars=structure, history,,\n"
                               "sidebarWidth=250\n"
                               "width=800\n"
                               "height=600\n");
    const auto scene = wxm::ReadScreenshotScene(*config);
    REQUIRE(scene);
    THEN("Every setting is taken, and empty sidebar names are dropped") {
      CHECK(scene->target.kind == Kind::Pane);
      CHECK(scene->target.pane == wxString(wxS("structure")));
      CHECK(scene->setSidebars);
      REQUIRE(scene->sidebars.GetCount() == 2);
      CHECK(scene->sidebars[0] == wxString(wxS("structure")));
      CHECK(scene->sidebars[1] == wxString(wxS("history")));
      CHECK(scene->size == wxSize(800, 600));
      CHECK(scene->sidebarWidth == 250);
    }
  }
  GIVEN("A scene with no [Screenshot] group at all") {
    const auto scene = wxm::ReadScreenshotScene(*Config("[Other]\nx=1\n"));
    REQUIRE(scene);
    THEN("The main window is shown as it is") {
      CHECK(scene->target.kind == Kind::Window);
      CHECK(!scene->setSidebars);
      CHECK(scene->size == wxDefaultSize);
      CHECK(scene->sidebarWidth == 0);
    }
  }
  GIVEN("An empty sidebar list") {
    const auto scene =
      wxm::ReadScreenshotScene(*Config("[Screenshot]\nsidebars=\n"));
    REQUIRE(scene);
    THEN("It hides every sidebar") {
      CHECK(scene->setSidebars);
      CHECK(scene->sidebars.IsEmpty());
    }
  }
  GIVEN("Only one of the two dimensions") {
    const auto scene =
      wxm::ReadScreenshotScene(*Config("[Screenshot]\nwidth=800\n"));
    REQUIRE(scene);
    THEN("The size is left alone") { CHECK(scene->size == wxDefaultSize); }
  }
  GIVEN("Something that can't be shown") {
    THEN("The scene is refused") {
      CHECK(!wxm::ReadScreenshotScene(*Config("[Screenshot]\nshow=all\n")));
    }
  }
}

SCENARIO("A screenshot of cells is cropped to what is drawn") {
  GIVEN("A white image with a black rectangle in it") {
    wxImage image(100, 80);
    image.SetRGB(wxRect(0, 0, 100, 80), 255, 255, 255);
    image.SetRGB(wxRect(30, 20, 10, 5), 0, 0, 0);
    THEN("Only the rectangle and the padding around it are left") {
      const wxImage trimmed = wxm::TrimToContent(image, 4);
      CHECK(trimmed.GetWidth() == 18);
      CHECK(trimmed.GetHeight() == 13);
      CHECK(trimmed.GetRed(4, 4) == 0);
      CHECK(trimmed.GetRed(0, 0) == 255);
    }
    THEN("Padding never reaches past the image's edges") {
      wxImage corner(image);
      corner.SetRGB(wxRect(95, 75, 5, 5), 0, 0, 0);
      const wxImage trimmed = wxm::TrimToContent(corner, 10);
      CHECK(trimmed.GetWidth() == 100 - 20);
      CHECK(trimmed.GetHeight() == 80 - 10);
    }
  }
  GIVEN("An image that is all background") {
    wxImage image(20, 10);
    image.SetRGB(wxRect(0, 0, 20, 10), 1, 2, 3);
    THEN("It stays as it is") {
      CHECK(wxm::TrimToContent(image, 4).GetSize() == wxSize(20, 10));
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
