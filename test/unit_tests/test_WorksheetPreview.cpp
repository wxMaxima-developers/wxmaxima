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
  The sample worksheet the configuration dialog's "Worksheet" tab shows to
  preview what its display settings do.

  Checks that each topic the dialog scrolls to exists and is followed by the
  cells it is about, and that those cells react to the settings they are
  there to show. The output is parsed from the XML Maxima would send, so no
  Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "cells/GroupCell.h"
#include "cells/LabelCell.h"
#include "cells/MatrCell.h"
#include "dialogs/WorksheetPreview.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
//! How many wxASSERTs failed: a failed one must fail the test, not open a dialog
int g_failedAsserts = 0;
void CountFailedAssert(const wxString &, int, const wxString &, const wxString &,
                       const wxString &) {
  g_failedAsserts++;
}
} // namespace

using wxm::WorksheetPreview;
using Topic = WorksheetPreview::Topic;

// The first code cell after the heading of this topic
static GroupCell *FirstCodeCellOf(const WorksheetPreview &preview, Topic topic) {
  GroupCell *heading = preview.HeadingFor(topic);
  REQUIRE(heading != nullptr);
  GroupCell *code = heading->GetNext();
  REQUIRE(code != nullptr);
  REQUIRE(code->GetGroupType() == GC_TYPE_CODE);
  return code;
}

// How wide a group's output is, after laying it out
static int OutputWidth(GroupCell *group) {
  group->Recalculate();
  REQUIRE(group->GetOutput() != nullptr);
  int width = 0;
  for (const Cell &cell : OnDrawList(group->GetOutput()))
    width += cell.GetWidth();
  return width;
}

SCENARIO("The Worksheet tab's preview has every topic, in order") {
  auto preview = WorksheetPreview::Make(g_cfg);
  REQUIRE(preview.tree != nullptr);
  THEN("each topic starts with a text cell, in the tree, after the previous one") {
    GroupCell *group = preview.tree.get();
    for (size_t i = 0; i < static_cast<size_t>(Topic::count); i++) {
      GroupCell *heading = preview.headings[i];
      REQUIRE(heading != nullptr);
      CHECK(heading->GetGroupType() == GC_TYPE_TEXT);
      while (group && (group != heading))
        group = group->GetNext();
      CHECK(group == heading);
    }
  }
}

SCENARIO("The preview's labels follow the label settings") {
  auto preview = WorksheetPreview::Make(g_cfg);
  GroupCell *userLabelled = FirstCodeCellOf(preview, Topic::labels);
  auto *label = dynamic_cast<LabelCell *>(userLabelled->GetLabel());
  REQUIRE(label != nullptr);
  CHECK(label->ToXML().Contains(wxS("userdefinedlabel=\"kinetic_energy\"")));

  WHEN("automatic labels are shown") {
    g_cfg->SetLabelChoice(Configuration::labels_automatic);
    userLabelled->Recalculate();
    THEN("the label is %o1") { CHECK(label->GetDisplayedString().Contains(wxS("%o1"))); }
  }
  WHEN("user-defined labels are preferred") {
    g_cfg->SetLabelChoice(Configuration::labels_prefer_user);
    userLabelled->Recalculate();
    THEN("the label is the variable's name") {
      CHECK(label->GetDisplayedString().Contains(wxS("kinetic_energy")));
    }
  }
  g_cfg->SetLabelChoice(Configuration::labels_automatic);
}

SCENARIO("The preview's multiplication signs follow the settings") {
  auto preview = WorksheetPreview::Make(g_cfg);
  GroupCell *product = FirstCodeCellOf(preview, Topic::labels)->GetNext();
  REQUIRE(product != nullptr);
  g_cfg->HidemultiplicationSign(true);
  const int hidden = OutputWidth(product);
  g_cfg->HidemultiplicationSign(false);
  const int shown = OutputWidth(product);
  THEN("hiding them makes 2*a*b narrower") { CHECK(hidden < shown); }
  g_cfg->HidemultiplicationSign(true);
}

SCENARIO("The preview's special symbols follow their settings") {
  auto preview = WorksheetPreview::Make(g_cfg);
  GroupCell *symbols = FirstCodeCellOf(preview, Topic::specialSymbols);
  REQUIRE(symbols->GetOutput() != nullptr);
  CHECK(symbols->GetOutput()->ListToString().Contains(wxS("%e")));
  CHECK(symbols->GetOutput()->ListToString().Contains(wxS("%pi")));
  const bool keepPercent = g_cfg->CheckKeepPercent();
  const bool latin2Greek = g_cfg->Latin2Greek();

  g_cfg->SetKeepPercent(false);
  const int withoutPercent = OutputWidth(symbols);
  g_cfg->SetKeepPercent(true);
  const int withPercent = OutputWidth(symbols);
  THEN("keeping the percent signs makes %e, %i and %pi wider") {
    CHECK(withoutPercent < withPercent);
  }

  g_cfg->Latin2Greek(true);
  const int greek = OutputWidth(symbols);
  g_cfg->Latin2Greek(false);
  const int latin = OutputWidth(symbols);
  THEN("alpha shown as a Greek letter is narrower than spelled out") {
    CHECK(greek < latin);
  }

  g_cfg->SetKeepPercent(keepPercent);
  g_cfg->Latin2Greek(latin2Greek);
}

SCENARIO("The preview's long number is 200!") {
  const wxString digits = WorksheetPreview::Factorial200();
  CHECK(digits.Length() == 375);
  CHECK(digits.StartsWith(wxS("78865786736479050355236321393218506")));
  // 200! has 49 factors of 5, so as many trailing zeros.
  CHECK(digits.EndsWith(wxString(49, wxS('0'))));
  CHECK_FALSE(digits.EndsWith(wxString(50, wxS('0'))));
  THEN("it is longer than the digits shown by default, so it gets shortened") {
    CHECK(digits.Length() > static_cast<size_t>(g_cfg->GetDisplayedDigits()));
    auto preview = WorksheetPreview::Make(g_cfg);
    GroupCell *number = FirstCodeCellOf(preview, Topic::longNumbers);
    const bool showAll = g_cfg->ShowAllDigits();
    g_cfg->ShowAllDigits(false);
    const int shortened = OutputWidth(number);
    g_cfg->ShowAllDigits(true);
    const int full = OutputWidth(number);
    CHECK(shortened < full);
    g_cfg->ShowAllDigits(showAll);
  }
  WHEN("long numbers may be broken into lines") {
    g_cfg->SetZoomFactor(1.0);
    g_cfg->SetCanvasSize(wxSize(300, 350));
    auto preview = WorksheetPreview::Make(g_cfg);
    GroupCell *number = FirstCodeCellOf(preview, Topic::longNumbers);
    const bool showAll = g_cfg->ShowAllDigits();
    const bool lineBreaks = g_cfg->LineBreaksInLongNums();
    g_cfg->ShowAllDigits(true);
    g_cfg->LineBreaksInLongNums(true);
    g_failedAsserts = 0;
    number->Recalculate();
    THEN("the lines it is broken into belong to its group") {
      // They are made from the parsed number, and take their group from it.
      size_t cells = 0;
      for (const Cell &cell : OnDrawList(number->GetOutput())) {
        CHECK(cell.GetGroup() == number);
        cells++;
      }
      CHECK(cells > 2);
      CHECK(g_failedAsserts == 0);
    }
    g_cfg->ShowAllDigits(showAll);
    g_cfg->LineBreaksInLongNums(lineBreaks);
  }
}

SCENARIO("The Worksheet tab's preview holds an oversized matrix") {
  g_cfg->SetZoomFactor(1.0);
  // About as much room as the preview gets in the dialog.
  g_cfg->SetCanvasSize(wxSize(500, 350));

  auto preview = WorksheetPreview::Make(g_cfg);
  GroupCell *matrixGroup = preview.matrixGroup;
  REQUIRE(matrixGroup != nullptr);
  CHECK(matrixGroup == FirstCodeCellOf(preview, Topic::oversizedMatrix));
  CHECK(matrixGroup->GetEditable()->GetValue().StartsWith(wxS("genmatrix(")));

  auto *matrix = dynamic_cast<MatrCell *>(matrixGroup->GetOutput());
  REQUIRE(matrix != nullptr);
  THEN("the output is the full matrix, its entries telling row and column") {
    CHECK(matrix->GetMatrixRows() == WorksheetPreview::MatrixSize);
    CHECK(matrix->GetMatrixColumns() == WorksheetPreview::MatrixSize);
    CHECK(matrix->GetInnerCell(0, 0)->ToString() == wxS("101"));
    CHECK(matrix->GetInnerCell(1, 2)->ToString() == wxS("203"));
  }

  WHEN("oversized matrices are elided") {
    g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::elide);
    matrixGroup->Recalculate();
    THEN("only part of the matrix is shown") {
      CHECK(matrix->IsShownPartially());
      CHECK(matrix->ElidedRows() > 0);
      CHECK(matrix->ElidedColumns() > 0);
    }
  }
  WHEN("oversized matrices are shown in full") {
    g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
    matrixGroup->Recalculate();
    THEN("all of it is shown, and it is wider than the preview") {
      CHECK_FALSE(matrix->IsShownPartially());
      CHECK(matrix->GetWidth() > 500);
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxSetAssertHandler(CountFailedAssert);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  wxBitmap bmp(400, 400);
  wxMemoryDC dc;
  dc.SelectObject(bmp);
  g_cfg = new Configuration(&dc);
  // Cells reach their worksheet through their configuration.
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  auto *worksheet = new Worksheet(frame, wxID_ANY, g_cfg, wxDefaultPosition,
                                  wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(worksheet);
  g_cfg->MakeTemporary();

  const int result = Catch::Session().run(argc, argv);

  worksheet->Destroy();
  delete g_cfg;
  dc.SelectObject(wxNullBitmap);
  wxEntryCleanup();
  return result;
}
