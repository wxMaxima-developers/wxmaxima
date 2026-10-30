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
  A matrix power A^^n is marked as one: A^⟨n⟩ (GH #1005).

  In Maxima A^n of a matrix is the elementwise power, so an A^^n drawn as a
  plain exponent looks exactly like a different result. Until this, the only
  place the difference survived was copy as text: the screen, LaTeX, MathML
  and RTF all showed A^n, and saving and reloading a .wxmx lost it entirely.

  The maths is hand-written XML of the shape wxMathML.lisp emits, parsed by
  the real MathParser, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "MathParser.h"
#include "cells/Cell.h"
#include "cells/ExptCell.h"
#include "cells/GroupCell.h"
#include "cells/SubSupCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;

// What wxxml-mexpt-simple and wxxml-mexpt-array send for A^^2, A^2,
// a[1]^^2 and a[1]^2.
const wxString matrixPower =
  wxS("<msup mat=\"true\"><mrow><v>A</v></mrow><mn>2</mn></msup>");
const wxString plainPower = wxS("<msup><mrow><v>A</v></mrow><mn>2</mn></msup>");
const wxString indexedMatrixPower =
  wxS("<ie mat=\"true\"><mrow><v>a</v></mrow><mrow><mn>1</mn></mrow>"
      "<mn>2</mn></ie>");
const wxString indexedPlainPower =
  wxS("<ie><mrow><v>a</v></mrow><mrow><mn>1</mn></mrow><mn>2</mn></ie>");

//! A code cell whose output is the maths in xml, laid out
std::unique_ptr<GroupCell> Output(const wxString &xml) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("x;"));
  MathParser parser(g_cfg);
  parser.SetGroup(group.get());
  group->AppendOutput(parser.ParseLine(
    wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>") + xml + wxS("</mth>")));
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  return group;
}

//! The first output cell of type T in group
template <class T> T *Find(GroupCell *group) {
  for (Cell &cell : OnList(group->GetOutput()))
    if (auto *found = dynamic_cast<T *>(&cell))
      return found;
  return nullptr;
}

//! The maths in xml, parsed but not laid out
std::unique_ptr<Cell> Parse(const wxString &xml) {
  MathParser parser(g_cfg);
  auto cell = parser.ParseLine(wxS("<mth>") + xml + wxS("</mth>"));
  REQUIRE(cell != nullptr);
  return cell;
}
} // namespace

SCENARIO("A matrix power draws its exponent between angle brackets") {
  g_cfg->SetLayoutStrategy(Configuration::LayoutStrategy::layout2DWheneverPossible);
  auto matrix = Output(matrixPower);
  auto plain = Output(plainPower);
  ExptCell *matrixExpt = Find<ExptCell>(matrix.get());
  ExptCell *plainExpt = Find<ExptCell>(plain.get());
  REQUIRE(matrixExpt != nullptr);
  REQUIRE(plainExpt != nullptr);
  REQUIRE(matrixExpt->IsMatrix());
  REQUIRE(!plainExpt->IsMatrix());

  const Cell *open = matrixExpt->GetInnerCell(5);
  const Cell *exponent = matrixExpt->GetInnerCell(3);
  const Cell *close = matrixExpt->GetInnerCell(6);
  REQUIRE(open != nullptr);
  REQUIRE(close != nullptr);

  THEN("the brackets are the angle brackets Maxima's own display uses") {
    CHECK(open->ToString() == wxS("⟨"));
    CHECK(close->ToString() == wxS("⟩"));
  }
  THEN("they take room") {
    CHECK(open->GetWidth() > 0);
    CHECK(close->GetWidth() > 0);
    CHECK(matrixExpt->GetWidth() ==
          plainExpt->GetWidth() + open->GetWidth() + close->GetWidth());
  }
  THEN("they enclose the exponent") {
    CHECK(exponent->GetCurrentPoint().x ==
          open->GetCurrentPoint().x + open->GetWidth());
    CHECK(close->GetCurrentPoint().x ==
          exponent->GetCurrentPoint().x + exponent->GetWidth());
    CHECK(open->GetCurrentPoint().y == exponent->GetCurrentPoint().y);
    CHECK(close->GetCurrentPoint().y == exponent->GetCurrentPoint().y);
  }
  THEN("hovering over it says what a matrix power is") {
    CHECK(!matrixExpt->GetLocalToolTip().empty());
    CHECK(plainExpt->GetLocalToolTip().empty());
  }
}

SCENARIO("A matrix power broken into lines reads A^^n") {
  const auto strategy = g_cfg->GetLayoutStrategy();
  g_cfg->SetLayoutStrategy(Configuration::LayoutStrategy::layoutPrefer1D);
  auto group = Output(matrixPower);
  wxString drawn;
  for (const Cell &cell : OnDrawList(group->GetOutput()))
    drawn += cell.ToString();
  CHECK(drawn.Contains(wxS("A^^2")));
  CHECK(!drawn.Contains(wxS("⟨")));
  g_cfg->SetLayoutStrategy(strategy);
}

SCENARIO("Exports say it is a matrix power") {
  auto cell = Parse(matrixPower);
  CHECK(cell->ListToString() == wxS("A^^2"));
  // Exactly what Maxima's tex() writes
  CHECK(cell->ListToTeX() == wxS("A^{\\langle 2\\rangle}"));
  CHECK(cell->ListToMathML().Contains(wxS("⟨")));
  CHECK(cell->ListToMathML().Contains(wxS("⟩")));
  CHECK(cell->ListToOMML().Contains(wxS("⟨")));
  CHECK(cell->ListToOMML().Contains(wxS("⟩")));
  THEN("Matlab's ^ already is the matrix power") {
    CHECK(cell->ListToMatlab() == wxS("A^2"));
  }

  THEN("an ordinary power is left as it was") {
    auto plain = Parse(plainPower);
    CHECK(plain->ListToString() == wxS("A^2"));
    CHECK(plain->ListToTeX() == wxS("A^2"));
    CHECK(!plain->ListToMathML().Contains(wxS("⟨")));
    CHECK(!plain->ListToOMML().Contains(wxS("⟨")));
  }
}

SCENARIO("A matrix power survives saving, loading and copying") {
  auto cell = Parse(matrixPower);
  const wxString xml = cell->ListToXML();
  CHECK(xml.Contains(wxS("mat=\"true\"")));

  auto reloaded = Parse(xml);
  auto *expt = dynamic_cast<ExptCell *>(reloaded.get());
  REQUIRE(expt != nullptr);
  CHECK(expt->IsMatrix());
  CHECK(reloaded->ListToTeX() == wxS("A^{\\langle 2\\rangle}"));

  auto copy = cell->Copy(nullptr);
  auto *copiedExpt = dynamic_cast<ExptCell *>(copy.get());
  REQUIRE(copiedExpt != nullptr);
  CHECK(copiedExpt->IsMatrix());
  CHECK(copy->ToTeX() == wxS("A^{\\langle 2\\rangle}"));

  THEN("an ordinary power doesn't become one") {
    CHECK(!Parse(plainPower)->ListToXML().Contains(wxS("mat=")));
  }
}

SCENARIO("A matrix power of an indexed variable is marked, too") {
  g_cfg->SetLayoutStrategy(Configuration::LayoutStrategy::layout2DWheneverPossible);
  auto cell = Parse(indexedMatrixPower);
  CHECK(cell->ListToString() == wxS("a[1]^^2"));
  CHECK(cell->ListToTeX() == wxS("a^{\\langle 2\\rangle}_1"));
  CHECK(cell->ListToMathML().Contains(wxS("⟨")));
  CHECK(cell->ListToOMML().Contains(wxS("⟩")));

  auto plain = Parse(indexedPlainPower);
  CHECK(plain->ListToString() == wxS("a[1]^2"));
  CHECK(!plain->ListToMathML().Contains(wxS("⟨")));

  THEN("it survives saving and copying") {
    const wxString xml = cell->ListToXML();
    CHECK(xml.Contains(wxS("mat=\"true\"")));
    CHECK(Parse(xml)->ListToString() == wxS("a[1]^^2"));
    CHECK(cell->Copy(nullptr)->ToString() == wxS("a[1]^^2"));
    CHECK(!plain->ListToXML().Contains(wxS("mat=")));
  }
  THEN("the brackets take room") {
    auto matrixGroup = Output(indexedMatrixPower);
    auto plainGroup = Output(indexedPlainPower);
    auto *matrixCell = Find<SubSupCell>(matrixGroup.get());
    auto *plainCell = Find<SubSupCell>(plainGroup.get());
    REQUIRE(matrixCell != nullptr);
    REQUIRE(plainCell != nullptr);
    CHECK(matrixCell->GetWidth() > plainCell->GetWidth());
  }
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

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  static DocumentCellPointers documentPointers;
  static ViewCellPointers viewPointers(nullptr);
  g_cfg->SetDocumentCellPointers(&documentPointers);
  g_cfg->SetViewCellPointers(&viewPointers);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
