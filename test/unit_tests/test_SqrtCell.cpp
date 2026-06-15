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
#include "FontAttribs.cpp"
#include "StringUtils.cpp"
#include "TestStubs.cpp"
#include "TextCell.cpp"
#include "VisiblyInvalidCell.cpp"

#include "SqrtCell.cpp"
#include "DigitCell.cpp"
#include "LongNumberCell.cpp"

#include <catch2/catch.hpp>

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

  GroupCell group(&configuration, GC_TYPE_TEXT);

  GIVEN("a SqrtCell") {
    SqrtCell cell(&group, &configuration, std::make_unique<VisiblyInvalidCell>(&group, &configuration));

    WHEN("the cell is copied") THEN("the copy succeeds")
      REQUIRE(cell.Copy(&group));

    WHEN("the cell is broken up") {
      cell.BreakUp();
      THEN("when it is copied, the copy succeeds")
        REQUIRE(cell.Copy(&group));
      THEN("when it is copied, the copy can recalculate") {
        auto copy = cell.Copy(&group);
        copy->Recalculate(AFontSize(10));
      }
    }
  }
}

SCENARIO("LongNumberCell behaviour") {
  wxBitmap bitmap(128, 128);
  wxMemoryDC dc(bitmap);
  Configuration configuration(&dc);
  configuration.SetZoomFactor(1.0);
  configuration.ShowAllDigits(true);
  configuration.LineBreaksInLongNums(true);

  GroupCell group(&configuration, GC_TYPE_TEXT);

  GIVEN("a LongNumberCell with a 43-digit number") {
    wxString val = "9647293028308316131448074967389816654817441";
    LongNumberCell cell(&group, &configuration, val);

    WHEN("recalculating the cell") {
      cell.Recalculate(AFontSize(10));
      THEN("it starts not broken into lines") {
        REQUIRE_FALSE(cell.IsBrokenIntoLines());
      }
    }

    WHEN("breaking up the cell") {
      bool didBreak = cell.BreakUp();
      THEN("break up succeeds") {
        REQUIRE(didBreak);
        REQUIRE(cell.IsBrokenIntoLines());
      }
      AND_WHEN("recalculating after breakup") {
        cell.Recalculate(AFontSize(10));
        // Verify width/height/center of cell is 0
        REQUIRE(cell.GetWidth() == 0);
        // Verify inner cells have some width/height
        REQUIRE(cell.GetInnerCellCount() == 1);
        auto inner = cell.GetInnerCell(0);
        REQUIRE(inner != nullptr);
        // Verify we can iterate and find all DigitCells
        int count = 0;
        for ([[maybe_unused]] const Cell &c : OnList(inner)) {
          count++;
        }
        // with groupSize 3, 43 digits should bundle into 15 groups
        REQUIRE(count == 15);

        // Test copy of broken-up cell
        auto copy = cell.Copy(&group);
        REQUIRE(copy != nullptr);
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

IMPLEMENT_APP(MyApp);
wxDECLARE_APP(MyApp);
