// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include <wx/log.h>

wxLogNull dontLog;

#define CATCH_CONFIG_RUNNER
#include "Cell.cpp"
#include "CellList.cpp"
#include "CellPointers.cpp"
#include "CellPtr.cpp"
#include "FontAttribs.cpp"
#include "FontCache.cpp"
#include "StringUtils.cpp"
#include "TextCell.cpp"
#include "TextStyle.cpp"
#include "VisiblyInvalidCell.cpp"

#include "SqrtCell.cpp"

#include <catch2/catch.hpp>

CellPointers pointers(nullptr);
CellPointers *Cell::GetCellPointers() const { return &pointers; }

long Configuration::Scale_Px(double) const { return 1; }
AFontSize Configuration::Scale_Px(AFontSize) const { return AFontSize(10.0); }
wxFontStyle Configuration::IsItalic(long) const { return {}; }
bool Configuration::HideMarkerForThisMessage(wxString) {return false;}
wxColour Configuration::GetColor(TextStyle) { return {}; }
Style Configuration::GetStyle(TextStyle, AFontSize) const {
  return Style(AFontSize(10.0));
}
void Configuration::NotifyOfCellRedraw(const Cell *) {}
Configuration::drawMode Configuration::GetParenthesisDrawMode() { return {}; }
bool Configuration::InUpdateRegion(wxRect) const { return true; }
Configuration::Configuration(wxDC *dc, InitOpt) : m_dc(dc) {}
Configuration::~Configuration() {}

void Configuration::SetZoomFactor(double newzoom)
{
  if (newzoom > GetMaxZoomFactor())
    newzoom = GetMaxZoomFactor();
  if (newzoom < GetMinZoomFactor())
    newzoom = GetMinZoomFactor();

  m_zoomFactor = newzoom;
}

SCENARIO("SqrtCell recalculates") {
  wxBitmap bitmap(128, 128);
  wxMemoryDC dc(bitmap);
  Configuration configuration(&dc);
  configuration.SetZoomFactor(1.0);

  Configuration *pConfig = &configuration;
  Configuration **config = &pConfig;

  GIVEN("a SqrtCell") {
    SqrtCell cell(nullptr, config, std::make_unique<VisiblyInvalidCell>(nullptr, config));

    WHEN("the cell is copied") THEN("the copy succeeds")
      REQUIRE(cell.Copy());

    WHEN("the cell is broken up") {
      cell.BreakUp();
      THEN("when it is copied, the copy succeeds")
        REQUIRE(cell.Copy());
      THEN("when it is copied, the copy can recalculate") {
        auto copy = cell.Copy();
        copy->Recalculate(AFontSize(10));
      }
    }
  }
}

class MyApp : public wxApp
{
public:
  Catch::Session catchSession;
  int OnRun() override {
    return catchSession.run();
  }
};

// If we don't provide our own main when compiling on MinGW
// we currently get an error message that WinMain@16 is missing
// (https://github.com/catchorg/Catch2/issues/1287)
int main(int argc, char *argv[])
{
  auto *app = new MyApp;
  app->catchSession.applyCommandLine(argc, argv);
  return wxEntry(argc, argv);
}
