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
  The tint behind cells that have a mouse-over tooltip must leave their text
  readable on a light as well as on a dark background.

  It used to be a fixed, half-transparent pale yellow. On the dark style set's
  background that blended to a light grey-yellow, which the light text drawn
  on top of it hardly stood out from (and where the DC ignores alpha, as MSW's
  plain wxDC does, it was solid pale yellow under light text).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/fileconf.h>
#include <wx/log.h>

#include <algorithm>
#include <cmath>

#include "Configuration.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
//! WCAG relative luminance of @p c
double Luminance(const wxColour &c) {
  auto channel = [](unsigned char v) {
    const double s = v / 255.0;
    return (s <= 0.03928) ? s / 12.92 : std::pow((s + 0.055) / 1.055, 2.4);
  };
  return 0.2126 * channel(c.Red()) + 0.7152 * channel(c.Green()) +
    0.0722 * channel(c.Blue());
}

//! WCAG contrast ratio between @p a and @p b (1 ... 21)
double Contrast(const wxColour &a, const wxColour &b) {
  const double la = Luminance(a), lb = Luminance(b);
  return (std::max(la, lb) + 0.05) / (std::min(la, lb) + 0.05);
}
} // namespace

SCENARIO("The tooltip tint keeps text readable") {
  GIVEN("the light style set's white background and black text") {
    const wxColour marker = Configuration::TooltipMarkerColour(*wxWHITE);
    THEN("the tint is the pale yellow it always was") {
      CHECK(marker == wxColour(255, 255, 224));
    }
    THEN("black text stays readable on it") {
      CHECK(Contrast(marker, *wxBLACK) >= 7.0);
    }
  }

  GIVEN("the dark style set's background and default text color") {
    const wxColour background(30, 30, 30);
    const wxColour text(220, 220, 220);
    const wxColour marker = Configuration::TooltipMarkerColour(background);
    THEN("light text stays readable on it") {
      // WCAG AA for normal text. The old tint, blended at 50% onto this
      // background, gave about 2.4.
      CHECK(Contrast(marker, text) >= 4.5);
    }
    THEN("it is still visible against the background") {
      CHECK(marker != background);
      CHECK(Contrast(marker, background) >= 1.5);
    }
    THEN("it is opaque, so a DC without alpha draws it the same") {
      CHECK(marker.Alpha() == wxALPHA_OPAQUE);
    }
  }

  GIVEN("a black background") {
    const wxColour marker = Configuration::TooltipMarkerColour(*wxBLACK);
    THEN("white text stays readable on it") {
      CHECK(Contrast(marker, *wxWHITE) >= 4.5);
    }
  }
}

SCENARIO("The tooltip brush follows the background") {
  wxBitmap bmp(10, 10);
  wxMemoryDC dc;
  dc.SelectObject(bmp);
  Configuration config(&dc);
  config.MakeTemporary();

  config.SetBackgroundBrush(*wxWHITE_BRUSH);
  const wxColour light = config.GetTooltipBrush().GetColour();
  config.SetBackgroundBrush(wxBrush(wxColour(30, 30, 30)));
  const wxColour dark = config.GetTooltipBrush().GetColour();

  CHECK(light == Configuration::TooltipMarkerColour(*wxWHITE));
  CHECK(dark == Configuration::TooltipMarkerColour(wxColour(30, 30, 30)));
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  delete wxConfig::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                        wxEmptyString, wxEmptyString, 0));
  const int result = Catch::Session().run(argc, argv);
  wxEntryCleanup();
  return result;
}
