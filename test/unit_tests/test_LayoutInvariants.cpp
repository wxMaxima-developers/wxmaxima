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
  Layout idempotency invariants.

  Guards against the recurring "cell marked valid with stale geometry" family
  of layout bugs (matrix column widths after a cancelled layout, ParenCell /
  ListCell spacing after zoom changes): after ANY sequence of configuration
  changes (zoom, canvas size), letting the layout converge must yield exactly
  the same geometry as laying out a freshly parsed copy of the same content
  under the final configuration. If any cell in the tree reports
  NeedsRecalculation()==false while holding geometry from an older
  configuration, the comparison fails and names the divergent cell.

  The math content is parsed through the real MathParser from output captured
  from a live Maxima session (a list with subscripted variables, strings and
  hidden multiplications - the constructs from the 2026-07 layout bug
  reports), so the test exercises genuine nested ParenCell/ListCell trees.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "worksheet/Worksheet.h"
#include "cells/Cell.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
#include "cells/ProductCell.h"
#include "cells/SetCell.h"
#include "cells/MatrixScrollHost.h"
#include "worksheet/MatrixScrollbars.h"

#include <wx/region.h>
#include <wx/scrolwin.h>
#include <wx/dcgraph.h>
#include <wx/graphics.h>

#include <cstdlib>
#include <vector>
#ifndef _WIN32
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
} // namespace

// Maxima output (via wxMathML.lisp) for
//   f_aa\,bb\+cc[ff]=[min=11.1111*10^3,...,Fail=0.00111*10^3*"ppm"];
//   E_Oooooo\,Dddddd*R_1000;
// - a ListCell of equations with subscripted names, a string and hidden
// multiplications, i.e. the constructs from the 2026-07 layout bug reports.
static const char *const richMathXml =
  R"(<mth><lbl altCopy="%o1">(%o1) </lbl><munder><mrow><munder altCopy="f_aa\,bb\+cc"><mrow><mi>f</mi></mrow><mrow><mi>aa,bb+cc</mi></mrow></munder></mrow><mrow><mi>ff</mi></mrow></munder><mo>=</mo><mrow list="true"><t listdelim="true">[</t><mrow><mi>min</mi><mo>=</mo><mn>11111.1</mn></mrow><mo>,</mo><mrow><mi>typ</mi><mo>=</mo><mn>22222.2</mn></mrow><mo>,</mo><mrow><mi>max</mi><mo>=</mo><mn>33333.3</mn></mrow><mo>,</mo><mrow><mi>Fail</mi><mo>=</mo><mn>1.11</mn><h>*</h><st>ppm</st></mrow><t listdelim="true">]</t></mrow><lbl altCopy="%o2">(%o2) </lbl><munder altCopy="E_Oooooo\,Dddddd"><mrow><mi>E</mi></mrow><mrow><mi>Oooooo,Dddddd</mi></mrow></munder><h>*</h><munder altCopy="R_1000"><mrow><mi>R</mi></mrow><mrow><mi>1000</mi></mrow></munder></mth>)";

// Builds a code group whose output is the rich math tree above.
static std::unique_ptr<GroupCell> MakeRichGroup() {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                           wxS("f_aa\\,bb\\+cc[ff];"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(richMathXml));
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  return group;
}

// One cell's geometry plus where it sits in the tree walk, for failure output.
struct CellGeometry {
  wxString description;
  int width, height, center;
  bool operator==(const CellGeometry &o) const {
    return width == o.width && height == o.height && center == o.center;
  }
};

// Records the geometry of every cell reachable from "list" (following both the
// sibling chain and all inner cells, i.e. the full tree).
static void CollectGeometry(Cell *list, const wxString &path,
                            std::vector<CellGeometry> &out) {
  int idx = 0;
  for (Cell *c = list; c != nullptr; c = c->GetNext(), ++idx) {
    wxString here = wxString::Format(wxS("%s/%d:%s"), path, idx,
                                     c->GetInfo().GetName());
    out.push_back({here, c->GetWidth(), c->GetHeight(), c->GetCenter()});
    int innerIdx = 0;
    for (Cell &inner : OnInner(c))
      CollectGeometry(&inner, here + wxString::Format(wxS("(%d)"), innerIdx++),
                      out);
  }
}

static std::vector<CellGeometry> GroupGeometry(GroupCell *group) {
  std::vector<CellGeometry> out;
  out.push_back({wxS("group"), group->GetWidth(), group->GetHeight(),
                 group->GetCenter()});
  CollectGeometry(group->GetOutput(), wxS("out"), out);
  return out;
}

// REQUIREs that both trees have identical geometry, naming the first cell that
// differs (the cell whose stale size survived the configuration change).
static void RequireSameGeometry(const std::vector<CellGeometry> &converged,
                                const std::vector<CellGeometry> &fresh) {
  REQUIRE(converged.size() == fresh.size());
  for (size_t i = 0; i < converged.size(); i++) {
    INFO("divergent cell: " << converged[i].description.utf8_str()
         << " converged " << converged[i].width << "x" << converged[i].height
         << " center " << converged[i].center << " vs fresh "
         << fresh[i].width << "x" << fresh[i].height << " center "
         << fresh[i].center);
    CHECK(converged[i] == fresh[i]);
  }
}

SCENARIO("Zoom change detected only via the config counter relayouts every nested cell") {
  // This is the path production code takes whenever a zoom / print scale
  // change is applied WITHOUT manually resetting the whole tree (e.g.
  // Configuration::SetZoomFactor_temporarily used by the print/copy-as-bitmap
  // code): every cell must notice via ConfigChanged() and recompute. A cell
  // that re-marks itself valid without recomputing its inner list keeps its
  // stale size and diverges from the freshly laid out reference.
  g_cfg->SetCanvasSize(wxSize(900, 600));

  GIVEN("a rich math group laid out at zoom 1.0") {
    g_cfg->SetZoomFactor(1.0);
    auto group = MakeRichGroup();
    group->Recalculate();

    WHEN("the zoom changes to 1.5 and the layout re-converges") {
      g_cfg->SetZoomFactor(1.5);
      group->Recalculate();

      THEN("its geometry equals that of a fresh layout at zoom 1.5") {
        auto fresh = MakeRichGroup();
        fresh->Recalculate();
        RequireSameGeometry(GroupGeometry(group.get()),
                            GroupGeometry(fresh.get()));
      }
    }
    g_cfg->SetZoomFactor(1.0);
  }
}

SCENARIO("An app-style zoom (full tree reset) relayouts every nested cell") {
  // The Worksheet::SetZoomFactor path: fonts changed + sizes reset + caches
  // cleared, then recalculate.
  g_cfg->SetCanvasSize(wxSize(900, 600));

  GIVEN("a rich math group laid out at zoom 1.0") {
    g_cfg->SetZoomFactor(1.0);
    auto group = MakeRichGroup();
    group->Recalculate();

    WHEN("zoom 1.5 is applied the way Worksheet::SetZoomFactor does") {
      g_cfg->SetZoomFactor(1.5);
      group->FontsChangedList();
      group->ResetSizeList();
      group->ClearCacheList();
      group->Recalculate();

      THEN("its geometry equals that of a fresh layout at zoom 1.5") {
        auto fresh = MakeRichGroup();
        fresh->Recalculate();
        RequireSameGeometry(GroupGeometry(group.get()),
                            GroupGeometry(fresh.get()));
      }
    }
    g_cfg->SetZoomFactor(1.0);
  }
}

SCENARIO("A narrow-wide canvas round trip restores the original layout") {
  // Shrinking the canvas forces the break-up pipeline (2D cells linearized,
  // lines wrapped); widening it back must restore exactly the wide layout.
  // Leftover break-state or stale widths from the narrow pass diverge here.
  GIVEN("a rich math group laid out at a wide canvas, zoom 1.5") {
    g_cfg->SetZoomFactor(1.5);
    g_cfg->SetCanvasSize(wxSize(900, 600));
    auto group = MakeRichGroup();
    group->Recalculate();

    WHEN("the canvas narrows enough to break up cells, then widens again") {
      g_cfg->SetCanvasSize(wxSize(220, 600));
      group->Recalculate();
      g_cfg->SetCanvasSize(wxSize(900, 600));
      group->Recalculate();

      THEN("its geometry equals that of a fresh layout at the wide canvas") {
        auto fresh = MakeRichGroup();
        fresh->Recalculate();
        RequireSameGeometry(GroupGeometry(group.get()),
                            GroupGeometry(fresh.get()));
      }
    }
    g_cfg->SetZoomFactor(1.0);
  }
}

SCENARIO("A special matrix with row/column names but no entries does not crash on draw") {
  // MathParser sets a matrix's special / rownames / colnames flags purely from
  // the .wxmx XML attributes, independent of whether the matrix actually has
  // any rows or columns. A corrupt (or hand-crafted) document can thus yield a
  // "special" matrix carrying rownames/colnames but zero columns/rows, whose
  // per-column width / per-row drop-center vectors stay empty. MatrCell::Draw
  // used to index those empty vectors with .at(0) when drawing the row/column
  // separator lines - an out-of-range throw that, uncaught in the paint path,
  // terminated wxMaxima. Reproduce that exact state and require Draw to survive.
  GIVEN("a special 0x0 matrix that claims to have row and column names") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
    auto matrOwned = std::make_unique<MatrCell>(group.get(), g_cfg);
    MatrCell *matr = matrOwned.get();
    matr->SetStyle(TS_VARIABLE);
    matr->SetSpecialFlag(true);
    matr->RowNames(true);
    matr->ColNames(true);
    matr->SetDimension(); // m_matHeight == 0 -> m_matWidth stays 0, vectors empty
    group->AppendOutput(std::move(matrOwned));
    group->Recalculate();
    matr->SetCurrentPoint(wxPoint(50, 50)); // give it a valid on-screen position

    WHEN("it is drawn") {
      NoClipToDrawRegion noClip(g_cfg); // force DrawThisCell() to actually draw
      THEN("drawing does not throw") {
        REQUIRE_NOTHROW(matr->Draw(g_dc, g_dc));
      }
    }
  }
}

// Maxima-style output for the nested construct from the 2026-07 "parenthesis
// narrower than its content" reports: a parenthesis containing a fraction
// whose numerator holds another parenthesized fraction with subscripted
// variables. The nesting means the inner cells must be laid out at reduced
// font sizes, and a narrow canvas breaks up the nesting levels in several
// waves - the outer cells first, then (at the font size that grew back to
// full in the linearized form) the inner ones. A cell whose break-up happens
// in a later wave sits below ancestors that already consider themselves laid
// out; if those ancestors skip the recursion, the cell is re-measured behind
// their back at whatever font size it happens to remember.
static const char *const nestedFracXml =
  R"(<mth><lbl altCopy="%o1">(%o1) </lbl><p><f><r><p><f><r><i><r><mi>x</mi></r><r><mn>1</mn></r></i><mo>+</mo><mi>aLongVariableName</mi></r><r><i><r><mi>y</mi></r><r><mn>2</mn></r></i><mo>+</mo><mi>anotherLongName</mi></r></f><mo>+</mo><mi>moreNumeratorContent</mi></p></r><r><mi>denominatorName</mi><mo>+</mo><mn>1234.5678</mn></r></f><mo>+</mo><mi>tailTerm</mi></p></mth>)";

// Builds a code group whose output is the nested fraction tree above.
static std::unique_ptr<GroupCell> MakeNestedFracGroup() {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                           wxS("nested;"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(wxString::FromUTF8(nestedFracXml));
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  return group;
}

// CHECKs, for every cell reachable from "list", that the cell would draw at
// the font size the surrounding layout was computed with - the invariant
// Cell::Draw() checks in debug mode. A violation is the "text inside a 2D
// fraction shown at full size / parenthesis narrower than its content" bug.
static void RequireExpectedFontSizes(Cell *list, const wxString &path) {
  int idx = 0;
  for (Cell *c = list; c != nullptr; c = c->GetNext(), ++idx) {
    wxString here = wxString::Format(wxS("%s/%d:%s"), path, idx,
                                     c->GetInfo().GetName());
    INFO("cell measured at a different font size than its owner dictated: "
         << here.utf8_str());
    CHECK(c->FontSizeMatchesExpectation());
    int innerIdx = 0;
    for (Cell &inner : OnInner(c))
      RequireExpectedFontSizes(
          &inner, here + wxString::Format(wxS("(%d)"), innerIdx++));
  }
}

SCENARIO("Nested fractions keep their reduced font sizes through partial break-ups") {
  GIVEN("nested parens/fractions/subscripts laid out at a wide canvas") {
    g_cfg->SetZoomFactor(1.0);
    g_cfg->SetCanvasSize(wxSize(900, 600));
    auto group = MakeNestedFracGroup();
    group->Recalculate();
    RequireExpectedFontSizes(group->GetOutput(), wxS("out"));

    // Different widths hit different partial break-up depths; each one must
    // leave a self-consistent layout.
    const int narrowWidth = GENERATE(400, 300, 240);

    WHEN("the canvas narrows, breaking up part of the nesting") {
      INFO("narrow canvas width: " << narrowWidth);
      g_cfg->SetCanvasSize(wxSize(narrowWidth, 600));
      group->Recalculate();

      THEN("every cell is sized at the font size its owner dictated") {
        RequireExpectedFontSizes(group->GetOutput(), wxS("out"));
      }

      THEN("after widening again, geometry equals a fresh wide layout") {
        g_cfg->SetCanvasSize(wxSize(900, 600));
        group->Recalculate();
        RequireExpectedFontSizes(group->GetOutput(), wxS("out"));
        auto fresh = MakeNestedFracGroup();
        fresh->Recalculate();
        RequireSameGeometry(GroupGeometry(group.get()),
                            GroupGeometry(fresh.get()));
      }
    }
  }
}

// Builds a worksheet of "count" one-line code groups in g_ws and lays it out.
// Returns the group at "index" (0-based) for the scenario to operate on.
static GroupCell *BuildWorksheet(int count, int index) {
  GroupCell *last = nullptr;
  for (int i = 0; i < count; i++)
    last = g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                  wxString::Format(wxS("x%d;"), i)),
      last);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetLastCellsVisited() == count);
  GroupCell *cell = g_ws->GetTree();
  for (int i = 0; i < index; i++)
    cell = cell->GetNext();
  REQUIRE(cell != nullptr);
  return cell;
}

SCENARIO("Editing operations on one cell do not visit the cells above it") {
  // Guards the "stray whole-worksheet recalculation" regression: operations
  // that change the tree at one known position must schedule the layout pass
  // from that position (RequestRecalculation(cell)), not from the tree top
  // (the no-argument RequestRecalculation()). The layout pass counts the
  // cells it visited; a count spanning the whole document means some caller
  // fell back to a global recalculation.
  g_cfg->SetCanvasSize(wxSize(900, 600));

  GIVEN("a worksheet of 10 code groups, fully laid out") {
    GroupCell *sixth = BuildWorksheet(10, 5);

    WHEN("the 6th group is deleted") {
      g_ws->DeleteRegion(sixth, sixth);
      g_ws->RecalculateIfNeeded();

      THEN("the layout pass starts at the cell before it") {
        // 9 cells remain; the pass may only span the deletion point (the
        // 5th cell) to the end = 5 cells. The 4 cells above must not even
        // be visited.
        CHECK(g_ws->GetLastCellsVisited() <= 5);
        CHECK(g_ws->GetLastCellsVisited() >= 1);
      }
    }

    WHEN("a new group is inserted after the 6th group") {
      g_ws->InsertGroupCells(
        std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("new;")), sixth);
      g_ws->RecalculateIfNeeded();

      THEN("the layout pass starts at the new cell") {
        // 11 cells now; the pass may only span the new cell (7th) to the
        // end = 5 cells.
        CHECK(g_ws->GetLastCellsVisited() <= 5);
        CHECK(g_ws->GetLastCellsVisited() >= 1);
      }
    }

    WHEN("ReplaceAll changes only the 6th group's input") {
      // The cells are named x0; .. x9;, so this matches exactly one cell.
      // Each changed group's ResetSize() notifies the layout engine itself;
      // ReplaceAll must not fall back to a whole-worksheet recalculation.
      REQUIRE(g_ws->ReplaceAll(wxS("x5"), wxS("y5"), true, true, false) == 1);
      g_ws->RecalculateIfNeeded();

      THEN("the layout pass doesn't visit the cells above the change") {
        CHECK(g_ws->GetLastCellsVisited() <= 5);
        CHECK(g_ws->GetLastCellsVisited() >= 1);
      }
    }

    g_ws->DestroyTree();
  }
}

// Maxima output (via wxMathML.lisp's wxxml-matchfix handler for $set) for
//   {1,2,3};
// -- a SetCell wrapping three plain numbers, small enough to never need to be
// broken into lines.
static const char *const setMathXml =
  R"(<mth><lbl altCopy="%o1">(%o1) </lbl><mrow set="true"><t listdelim="true">{</t><mrow><n>1</n></mrow><mo>,</mo><mrow><n>2</n></mrow><mo>,</mo><mrow><n>3</n></mrow><t listdelim="true">}</t></mrow></mth>)";

SCENARIO("A SetCell positions its brace and content cells (GH #2270)") {
  // SetCell::SetCurrentPoint() used to shadow the inherited
  // ListCell::SetCurrentPoint() with an override that positioned only the
  // SetCell itself, never m_open/m_innerCell/m_close -- leaving those
  // children at their default, never-positioned {-1,-1} sentinel and
  // drawing them nowhere near the set's own bounding box (GH #2270: a set
  // rendering as completely blank output, despite computing the right
  // value). GetCurrentPoint() defaults to {-1,-1} (see Cell::m_currentPoint),
  // so a child cell left at that sentinel after layout is the direct,
  // reproducible symptom of the bug.
  g_cfg->SetCanvasSize(wxSize(900, 600));
  g_cfg->SetZoomFactor(1.0);

  GIVEN("a group whose output is a small set") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("{1,2,3};"));
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(setMathXml));
    REQUIRE(output != nullptr);
    group->AppendOutput(std::move(output));
    group->Recalculate();
    group->SetCurrentPoint(wxPoint(50, 50));

    Cell *setCell = group->GetOutput();
    REQUIRE(setCell != nullptr);
    REQUIRE(dynamic_cast<SetCell *>(setCell) != nullptr);

    THEN("the set is not broken into lines and has a real, positive size") {
      CHECK_FALSE(setCell->IsBrokenIntoLines());
      CHECK(setCell->GetWidth() > 0);
      CHECK(setCell->GetHeight() > 0);
    }

    THEN("its opening brace, content and closing brace are all positioned") {
      std::vector<Cell *> pieces;
      for (Cell &piece : OnInner(setCell))
        pieces.push_back(&piece);
      REQUIRE(pieces.size() == 3);
      Cell *open = pieces[0];
      Cell *inner = pieces[1];
      Cell *close = pieces[2];

      // None of the three may be left at the "never positioned" sentinel.
      CHECK(open->GetCurrentPoint() != wxPoint(-1, -1));
      CHECK(inner->GetCurrentPoint() != wxPoint(-1, -1));
      CHECK(close->GetCurrentPoint() != wxPoint(-1, -1));

      // The three pieces must be laid out left to right, inside the set's
      // own bounding box, in that order -- not off in some stale location.
      const int left = setCell->GetCurrentPoint().x;
      const int right = left + setCell->GetWidth();
      CHECK(open->GetCurrentPoint().x >= left);
      CHECK(open->GetCurrentPoint().x < inner->GetCurrentPoint().x);
      CHECK(inner->GetCurrentPoint().x < close->GetCurrentPoint().x);
      CHECK(close->GetCurrentPoint().x < right);
    }
  }
}

// Maxima output (via wxMathML.lisp's wxxml-matchfix handler for $set) for
//   {x, sqrt(x^2+1)};
// -- the odelin()-style set from GH #2282: at least one element (the sqrt)
// isn't a bare alphanumeric token, so SetCell::ToTeX() takes its
// \left/\right branch.
static const char *const setWithSqrtMathXml =
  R"(<mth><lbl altCopy="%o1">(%o1) </lbl><mrow set="true"><t listdelim="true">{</t><mrow><mi>x</mi></mrow><mo>,</mo><mrow><q><mrow><msup><mi>x</mi><mn>2</mn></msup><mo>+</mo><mn>1</mn></mrow></q></mrow><t listdelim="true">}</t></mrow></mth>)";

SCENARIO("A SetCell's LaTeX export escapes its braces (GH #2282)") {
  // SetCell::ToTeX() used to emit "\left{ ... \right} " for a set whose
  // content needs \left/\right sizing -- but LaTeX's \left/\right require
  // an escaped "\{"/"\}" to mean a literal brace; a bare "{" after \left is
  // TeX's own group-opening character instead, so the exported code failed
  // to compile. ListCell's analogous "\left[ ... \right] " is correct
  // as-is: "["/"]" are already literal delimiters in TeX, unlike "{"/"}",
  // so this bug is specific to SetCell.
  GIVEN("a group whose output is a set containing a sqrt") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                             wxS("{x, sqrt(x^2+1)};"));
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(setWithSqrtMathXml));
    REQUIRE(output != nullptr);
    group->AppendOutput(std::move(output));

    Cell *setCell = group->GetOutput();
    REQUIRE(dynamic_cast<SetCell *>(setCell) != nullptr);

    THEN("the exported LaTeX escapes both braces") {
      const wxString tex = setCell->ToTeX();
      CHECK(tex.Contains(wxS("\\left\\{")));
      CHECK(tex.Contains(wxS("\\right\\}")));
      CHECK_FALSE(tex.Contains(wxS("\\left{")));
      CHECK_FALSE(tex.Contains(wxS("\\right}")));
    }
  }
}

// Maxima output (via wxMathML.lisp's wxxml-sum handler for %product) for
//   product(k,k,1,n);
// -- a ProductCell with a non-empty upper limit ("n"), so
// GetMaximaCommandName() must return "product(", never SumCell's own
// "sum(".
// needsparen="true" so DisplayedBase() is m_paren itself (index 1 below) --
// with needsparen="false" DisplayedBase() would be m_paren's bare inner
// cell instead, which GetInnerCell(1) does not return.
static const char *const productMathXml =
  R"(<mth><lbl altCopy="%o1">(%o1) </lbl><sm type="prod" needsparen="true"><mrow><mi>k</mi><mo>=</mo><mn>1</mn></mrow><mrow><mi>n</mi></mrow><mrow><mi>k</mi></mrow></sm></mth>)";

SCENARIO("A ProductCell positions its symbol/limits/base and breaks up with the right command name") {
  // Two independent bugs, both from calling a virtual function where it
  // can't reach ProductCell's override:
  //
  // 1. SumCell::MakeBreakUpCells() (run from SumCell's own constructor)
  //    built m_open's text from GetMaximaCommandName() -- a virtual call
  //    made during a base class constructor, which can never dispatch to
  //    ProductCell::GetMaximaCommandName() since the derived part of the
  //    object doesn't exist yet. m_open ended up permanently reading
  //    "sum("/"lsum(", even for a product, no matter how the cell was
  //    later broken into lines.
  // 2. ProductCell::SetCurrentPoint()/Draw() only called
  //    Cell::SetCurrentPoint()/Cell::Draw(), skipping SumCell's own
  //    implementations entirely -- the ones that actually position/paint
  //    the operator glyph, the limits and the base. The exact same
  //    "override does strictly less than what it shadows" shape as the
  //    SetCell bug (GH #2270) right above this scenario: an unbroken
  //    ProductCell rendered as nothing at all.
  g_cfg->SetCanvasSize(wxSize(900, 600));
  g_cfg->SetZoomFactor(1.0);

  GIVEN("a group whose output is product(k,k,1,n)") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                             wxS("product(k,k,1,n);"));
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxString::FromUTF8(productMathXml));
    REQUIRE(output != nullptr);
    group->AppendOutput(std::move(output));
    group->Recalculate();
    group->SetCurrentPoint(wxPoint(50, 50));

    ProductCell *prod = dynamic_cast<ProductCell *>(group->GetOutput());
    REQUIRE(prod != nullptr);

    THEN("unbroken, its symbol/limits/base are all positioned, not left at the sentinel") {
      CHECK_FALSE(prod->IsBrokenIntoLines());
      CHECK(prod->GetWidth() > 0);
      CHECK(prod->GetHeight() > 0);

      // Index 1 = m_paren (the base), 8 = m_over (upper limit),
      // 9 = m_under (lower limit) -- see SumCell::GetInnerCell().
      Cell *base = prod->GetInnerCell(1);
      Cell *over = prod->GetInnerCell(8);
      Cell *under = prod->GetInnerCell(9);
      REQUIRE(base != nullptr);
      REQUIRE(over != nullptr);
      REQUIRE(under != nullptr);
      CHECK(base->GetCurrentPoint() != wxPoint(-1, -1));
      CHECK(over->GetCurrentPoint() != wxPoint(-1, -1));
      CHECK(under->GetCurrentPoint() != wxPoint(-1, -1));
    }

    THEN("broken into lines, its command name is product(, never sum(") {
      REQUIRE(prod->BreakUp());
      Cell *open = prod->GetBrokenCell(0);
      REQUIRE(open != nullptr); // flawfinder: ignore -- "open" is a glyph cell, not a file open
      CHECK(open->ToString() == wxS("product("));
    }
  }
}

// Maxima-style output for a rows x cols matrix whose entries are all
// different, so a test can tell from ToString() whether every one survived.
static wxString MatrixTableXml(size_t rows, size_t cols) {
  wxString xml = wxS("<tb roundedParens=\"true\">");
  for (size_t r = 0; r < rows; r++) {
    xml += wxS("<mtr>");
    for (size_t c = 0; c < cols; c++)
      xml += wxString::Format(wxS("<mtd><mn>%lu</mn></mtd>"),
                              static_cast<unsigned long>(100000 + r * 1000 + c));
    xml += wxS("</mtr>");
  }
  return xml + wxS("</tb>");
}

static wxString MatrixXml(size_t rows, size_t cols) {
  return wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>") +
    MatrixTableXml(rows, cols) + wxS("</mth>");
}

// matrix([<rows x cols matrix>, x], [y, z]): an oversized matrix nested in
// the first column of a small one.
static wxString NestedMatrixXml(size_t rows, size_t cols) {
  return wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl><tb roundedParens=\"true\">"
             "<mtr><mtd>") + MatrixTableXml(rows, cols) +
    wxS("</mtd><mtd><mi>x</mi></mtd></mtr>"
        "<mtr><mtd><mi>y</mi></mtd><mtd><mi>z</mi></mtd></mtr></tb></mth>");
}

// Lays out a group whose output is this XML, and returns the (outer) matrix.
static MatrCell *LayOutMatrixXml(std::unique_ptr<GroupCell> &group,
                                 const wxString &xml) {
  group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(xml);
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  auto *matr = dynamic_cast<MatrCell *>(group->GetOutput());
  REQUIRE(matr != nullptr);
  return matr;
}

// Lays out a group whose output is a rows x cols matrix, and returns it.
static MatrCell *LayOutMatrix(std::unique_ptr<GroupCell> &group, size_t rows,
                              size_t cols) {
  return LayOutMatrixXml(group, MatrixXml(rows, cols));
}

// Sets how oversized matrices are shown for the scope of one test, so the
// shared configuration is back to the default for every other scenario.
class OversizedMatricesMode {
public:
  explicit OversizedMatricesMode(Configuration::OversizedMatrices mode)
    : m_old(g_cfg->GetOversizedMatrices()) { g_cfg->SetOversizedMatrices(mode); }
  ~OversizedMatricesMode() { g_cfg->SetOversizedMatrices(m_old); }
private:
  Configuration::OversizedMatrices m_old;
};

SCENARIO("A matrix too wide for the window is elided unless shown in full") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 600));
  const size_t rows = 3, cols = 40;

  GIVEN("show-in-full mode") {
    OversizedMatricesMode mode(Configuration::OversizedMatrices::showInFull);
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, rows, cols);
    THEN("nothing is left out, and the matrix is wider than the window") {
      CHECK(matr->ElidedColumns() == 0);
      CHECK(matr->ElidedRows() == 0);
      CHECK(matr->GetWidth() > 600);
    }
  }

  GIVEN("elide mode") {
    OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, rows, cols);

    THEN("middle columns are left out and what is left fits the window") {
      CHECK(matr->ElidedColumns() > 0);
      CHECK(matr->ElidedColumns() < cols - 1);
      CHECK(matr->ElidedRows() == 0);
      CHECK(matr->GetWidth() < 600);
    }

    THEN("the first and the last column are kept") {
      for (size_t row = 0; row < rows; row++) {
        CHECK_FALSE(matr->IsElided(row, 0));
        CHECK_FALSE(matr->IsElided(row, cols - 1));
      }
    }

    THEN("what is left out is one contiguous run in the middle") {
      size_t firstHidden = cols, lastHidden = 0;
      for (size_t col = 0; col < cols; col++)
        if (matr->IsElided(0, col)) {
          firstHidden = std::min(firstHidden, col);
          lastHidden = std::max(lastHidden, col);
        }
      REQUIRE(firstHidden < cols);
      CHECK(lastHidden - firstHidden + 1 == matr->ElidedColumns());
    }

    THEN("the shown entries are laid out left to right inside the matrix") {
      const int left = matr->GetCurrentPoint().x;
      const int right = left + matr->GetWidth();
      int previousX = left;
      for (size_t col = 0; col < cols; col++) {
        if (matr->IsElided(0, col))
          continue;
        Cell *entry = matr->GetInnerCell(0, static_cast<int>(col));
        CHECK(entry->GetCurrentPoint().x > previousX);
        CHECK(entry->GetCurrentPoint().x + entry->GetWidth() < right);
        previousX = entry->GetCurrentPoint().x;
      }
    }

    THEN("copying it as text still yields every entry") {
      const wxString text = matr->ToString();
      for (size_t row = 0; row < rows; row++)
        for (size_t col = 0; col < cols; col++)
          CHECK(text.Contains(wxString::Format(
            wxS("%lu"), static_cast<unsigned long>(100000 + row * 1000 + col))));
      CHECK(static_cast<size_t>(matr->ToTeX().Freq('&')) == rows * (cols - 1));
    }

    THEN("hovering over it says which columns are not shown") {
      const wxRect rect = matr->GetRect();
      const wxString toolTip =
        matr->GetToolTip(wxPoint(rect.x + rect.width / 2, rect.y + rect.height / 2));
      CHECK(toolTip.Contains(wxS("Columns")));
    }

    THEN("drawing it does not throw") {
      NoClipToDrawRegion noClip(g_cfg);
      REQUIRE_NOTHROW(matr->Draw(g_dc, g_dc));
    }

    WHEN("the window is made wide enough for all of it") {
      g_cfg->SetCanvasSize(wxSize(20000, 600));
      group->Recalculate();
      group->SetCurrentPoint(wxPoint(50, 50));
      THEN("nothing is left out any more, and it matches a fresh layout") {
        CHECK(matr->ElidedColumns() == 0);
        std::unique_ptr<GroupCell> freshGroup;
        LayOutMatrix(freshGroup, rows, cols);
        RequireSameGeometry(GroupGeometry(group.get()),
                            GroupGeometry(freshGroup.get()));
      }
      g_cfg->SetCanvasSize(wxSize(600, 600));
    }

    WHEN("the mode is switched back to showing matrices in full") {
      g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
      group->Recalculate();
      THEN("nothing is left out any more") {
        CHECK(matr->ElidedColumns() == 0);
        CHECK(matr->GetWidth() > 600);
      }
    }
  }
}

SCENARIO("A matrix too tall for the window leaves out middle rows") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(900, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);

  GIVEN("a matrix with many short rows") {
    const size_t rows = 60, cols = 3;
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, rows, cols);

    THEN("middle rows are left out and it fits in 80% of the window's height") {
      CHECK(matr->ElidedRows() > 0);
      CHECK(matr->ElidedColumns() == 0);
      CHECK(matr->GetHeight() <= 300 * 8 / 10);
      for (size_t col = 0; col < cols; col++) {
        CHECK_FALSE(matr->IsElided(0, col));
        CHECK_FALSE(matr->IsElided(rows - 1, col));
      }
    }

    THEN("the shown rows are laid out top to bottom inside the matrix") {
      const int top = matr->GetCurrentPoint().y - matr->GetCenter();
      const int bottom = top + matr->GetHeight();
      int previousY = top;
      for (size_t row = 0; row < rows; row++) {
        if (matr->IsElided(row, 0))
          continue;
        Cell *entry = matr->GetInnerCell(static_cast<int>(row), 0);
        CHECK(entry->GetCurrentPoint().y > previousY);
        CHECK(entry->GetCurrentPoint().y < bottom);
        previousY = entry->GetCurrentPoint().y;
      }
    }
  }

  GIVEN("a matrix too wide and too tall") {
    g_cfg->SetCanvasSize(wxSize(600, 300));
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 60, 40);
    THEN("both rows and columns are left out, and it still draws") {
      CHECK(matr->ElidedRows() > 0);
      CHECK(matr->ElidedColumns() > 0);
      CHECK(matr->GetToolTip(matr->GetRect().GetPosition() + wxPoint(1, 1))
              .Contains(wxS("Rows")));
      NoClipToDrawRegion noClip(g_cfg);
      REQUIRE_NOTHROW(matr->Draw(g_dc, g_dc));
    }
  }

}

// Stands in for the worksheet's MatrixScrollbars: a fixed scrollbar thickness,
// and a record of which matrices reported in as drawn.
class FakeScrollHost final : public MatrixScrollHost {
public:
  wxCoord ScrollbarThickness() const override { return 15; }
  void MatrixDrawn(MatrCell *matrix) override { drawn.push_back(matrix); }
  std::vector<MatrCell *> drawn;
};

// Scroll mode with a scrollbar host installed, for the scope of one test.
class ScrollModeWithHost {
public:
  explicit ScrollModeWithHost(MatrixScrollHost *host)
    : m_mode(Configuration::OversizedMatrices::scroll),
      m_oldHost(g_cfg->GetMatrixScrollHost()) { g_cfg->SetMatrixScrollHost(host); }
  ~ScrollModeWithHost() { g_cfg->SetMatrixScrollHost(m_oldHost); }
private:
  OversizedMatricesMode m_mode;
  MatrixScrollHost *m_oldHost;
};

SCENARIO("A matrix too large for the window can scroll in a viewport") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  FakeScrollHost host;
  ScrollModeWithHost scrollMode(&host);
  const size_t rows = 60, cols = 40;

  GIVEN("a matrix too wide and too tall") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, rows, cols);

    THEN("it gets both scrollbars, nothing is left out, and it fits") {
      CHECK(matr->HasHorizontalScrollbar());
      CHECK(matr->HasVerticalScrollbar());
      CHECK(matr->ElidedRows() == 0);
      CHECK(matr->ElidedColumns() == 0);
      CHECK(matr->GetWidth() < 600);
      CHECK(matr->ViewportSize().y == 300 * 8 / 10);
      CHECK(matr->ScrollableSize().x > matr->ViewportSize().x);
      CHECK(matr->ScrollableSize().y > matr->ViewportSize().y);
    }

    THEN("the scrollbars sit right of and below the viewport, outside it") {
      const wxRect viewport = matr->ViewportRect();
      const wxRect horizontal = matr->HorizontalScrollbarRect();
      const wxRect vertical = matr->VerticalScrollbarRect();
      CHECK(horizontal.GetHeight() == 15);
      CHECK(vertical.GetWidth() == 15);
      CHECK(horizontal.GetTop() > viewport.GetBottom());
      CHECK(vertical.GetLeft() > viewport.GetRight());
      CHECK_FALSE(horizontal.Intersects(vertical));
      // ...and inside the cell, so the layout makes room for them.
      CHECK(matr->GetRect().Contains(horizontal));
      CHECK(matr->GetRect().Contains(vertical));
    }

    THEN("it starts scrolled to the top left, where the first entry shows") {
      CHECK(matr->ScrollPosition() == wxPoint(0, 0));
      CHECK(matr->GetInnerCell(0, 0)->GetRect().Intersects(matr->ViewportRect()));
    }

    WHEN("it is scrolled") {
      const wxPoint before = matr->GetInnerCell(0, 0)->GetCurrentPoint();
      REQUIRE(matr->ScrollTo(wxPoint(50, 40)));
      THEN("its entries move by exactly that, without a relayout") {
        CHECK(matr->ScrollPosition() == wxPoint(50, 40));
        CHECK(matr->GetInnerCell(0, 0)->GetCurrentPoint() == before - wxPoint(50, 40));
      }
      THEN("scrolling to the same place again changes nothing") {
        CHECK_FALSE(matr->ScrollTo(wxPoint(50, 40)));
      }
      THEN("the scroll position survives a relayout") {
        g_cfg->SetCanvasSize(wxSize(700, 300));
        group->Recalculate();
        CHECK(matr->ScrollPosition() == wxPoint(50, 40));
        g_cfg->SetCanvasSize(wxSize(600, 300));
      }
    }

    WHEN("it is scrolled past its end") {
      matr->ScrollTo(wxPoint(1000000, 1000000));
      THEN("it stops where the last entries meet the viewport's far edges") {
        const wxSize range = matr->ScrollableSize() - matr->ViewportSize();
        CHECK(matr->ScrollPosition() == wxPoint(range.x, range.y));
        Cell *last = matr->GetInnerCell(static_cast<int>(rows - 1),
                                        static_cast<int>(cols - 1));
        CHECK(last->GetRect().Intersects(matr->ViewportRect()));
      }
      THEN("entries scrolled out of view can't be hit with the mouse") {
        Cell *first = matr->GetInnerCell(0, 0);
        REQUIRE_FALSE(first->GetRect().Intersects(matr->ViewportRect()));
        const Cell::Range hit = matr->GetInnerCellsInRect(first->GetRect());
        CHECK(hit.first != first);
      }
      THEN("a relayout that shrinks the scroll range pulls it back in range") {
        g_cfg->SetCanvasSize(wxSize(700, 300));
        group->Recalculate();
        const wxSize range = matr->ScrollableSize() - matr->ViewportSize();
        CHECK(matr->ScrollPosition() == wxPoint(range.x, range.y));
        g_cfg->SetCanvasSize(wxSize(600, 300));
      }
    }

    WHEN("it is drawn") {
      NoClipToDrawRegion noClip(g_cfg);
      REQUIRE_NOTHROW(matr->Draw(g_dc, g_dc));
      THEN("it asks the host for its scrollbars") {
        REQUIRE(host.drawn.size() == 1);
        CHECK(host.drawn.front() == matr);
      }
    }

    THEN("copying it as text still yields every entry") {
      CHECK(static_cast<size_t>(matr->ToTeX().Freq('&')) == rows * (cols - 1));
    }
  }

  GIVEN("a matrix that fits") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 3, 3);
    THEN("it has no scrollbars, and drawing it doesn't ask for any") {
      CHECK_FALSE(matr->HasHorizontalScrollbar());
      CHECK_FALSE(matr->HasVerticalScrollbar());
      NoClipToDrawRegion noClip(g_cfg);
      matr->Draw(g_dc, g_dc);
      CHECK(host.drawn.empty());
    }
  }

  GIVEN("a matrix that is only too tall") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, rows, 3);
    THEN("it only gets a vertical scrollbar") {
      CHECK(matr->HasVerticalScrollbar());
      CHECK_FALSE(matr->HasHorizontalScrollbar());
      CHECK(matr->HorizontalScrollbarRect().IsEmpty());
    }
  }
}

SCENARIO("An oversized matrix nested in another one") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));

  GIVEN("scroll mode") {
    FakeScrollHost host;
    ScrollModeWithHost scrollMode(&host);
    std::unique_ptr<GroupCell> group;
    MatrCell *outer = LayOutMatrixXml(group, NestedMatrixXml(60, 40));
    auto *inner = dynamic_cast<MatrCell *>(outer->GetInnerCell(0, 0));
    REQUIRE(inner != nullptr);

    THEN("only the outer one knows it is nested") {
      CHECK(inner->IsNestedInMatrix());
      CHECK_FALSE(outer->IsNestedInMatrix());
    }
    THEN("only the outer one scrolls: one viewport, one set of scrollbars") {
      CHECK_FALSE(inner->HasHorizontalScrollbar());
      CHECK_FALSE(inner->HasVerticalScrollbar());
      CHECK(inner->ElidedColumns() == 0);
      CHECK(outer->HasHorizontalScrollbar());
      CHECK(outer->HasVerticalScrollbar());
      CHECK(outer->GetWidth() < 600);
    }
    THEN("drawing asks the host for the outer matrix's scrollbars only") {
      NoClipToDrawRegion noClip(g_cfg);
      REQUIRE_NOTHROW(outer->Draw(g_dc, g_dc));
      REQUIRE(host.drawn.size() == 1);
      CHECK(host.drawn.front() == outer);
    }
    THEN("a copy of the outer matrix still knows its inner one is nested") {
      auto copy = outer->Copy(group.get());
      auto *copiedOuter = dynamic_cast<MatrCell *>(copy.get());
      REQUIRE(copiedOuter != nullptr);
      auto *copiedInner = dynamic_cast<MatrCell *>(copiedOuter->GetInnerCell(0, 0));
      REQUIRE(copiedInner != nullptr);
      CHECK(copiedInner->IsNestedInMatrix());
      CHECK_FALSE(copiedOuter->IsNestedInMatrix());
    }
  }

  GIVEN("elide mode") {
    OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);
    std::unique_ptr<GroupCell> group;
    MatrCell *outer = LayOutMatrixXml(group, NestedMatrixXml(60, 40));
    auto *inner = dynamic_cast<MatrCell *>(outer->GetInnerCell(0, 0));
    REQUIRE(inner != nullptr);
    THEN("the inner one elides itself to the window and everything draws") {
      CHECK(inner->ElidedColumns() > 0);
      CHECK(inner->ElidedRows() > 0);
      // The outer one has only two columns and rows, both always kept.
      CHECK(outer->ElidedColumns() == 0);
      CHECK(outer->ElidedRows() == 0);
      NoClipToDrawRegion noClip(g_cfg);
      REQUIRE_NOTHROW(outer->Draw(g_dc, g_dc));
    }
  }
}

SCENARIO("Without a scrollbar host, scroll mode elides instead") {
  // Printing has no window to put scrollbars in, and paper can't scroll.
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  REQUIRE(g_cfg->GetMatrixScrollHost() == nullptr);
  OversizedMatricesMode mode(Configuration::OversizedMatrices::scroll);
  std::unique_ptr<GroupCell> group;
  MatrCell *matr = LayOutMatrix(group, 60, 40);
  CHECK_FALSE(matr->HasHorizontalScrollbar());
  CHECK_FALSE(matr->HasVerticalScrollbar());
  CHECK(matr->ElidedRows() > 0);
  CHECK(matr->ElidedColumns() > 0);
}

SCENARIO("The worksheet's matrix scrollbars follow the matrices they belong to") {
  // MatrixScrollbars is driven exactly as Worksheet::OnPaint() drives it --
  // BeginPaint(), the drawn matrices reporting in, EndPaint() with what was
  // repainted -- and Sync() is then called directly rather than waiting for
  // the CallAfter() a real paint would leave behind.
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("scrollbars"));
  auto *canvas = new wxScrolled<wxWindow>(frame, wxID_ANY);
  canvas->SetScrollRate(10, 10);
  canvas->SetVirtualSize(4000, 4000);
  MatrixScrollbars scrollbars(canvas);
  ScrollModeWithHost scrollMode(&scrollbars);

  GIVEN("a scrolling matrix that has been drawn") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 60, 40);
    const wxRegion everything(0, 0, 4000, 4000);
    scrollbars.BeginPaint();
    scrollbars.MatrixDrawn(matr);
    scrollbars.EndPaint(everything);
    scrollbars.Sync();

    THEN("both its scrollbars are shown") {
      CHECK(scrollbars.VisibleScrollbars() == 2);
    }

    WHEN("a paint covering it no longer draws it (folded, output hidden)") {
      scrollbars.BeginPaint();
      scrollbars.EndPaint(everything);
      scrollbars.Sync();
      THEN("its scrollbars are hidden") {
        CHECK(scrollbars.VisibleScrollbars() == 0);
      }
      AND_WHEN("it is drawn again") {
        scrollbars.BeginPaint();
        scrollbars.MatrixDrawn(matr);
        scrollbars.EndPaint(everything);
        scrollbars.Sync();
        THEN("they are back") {
          CHECK(scrollbars.VisibleScrollbars() == 2);
        }
      }
    }

    WHEN("a paint elsewhere doesn't draw it") {
      scrollbars.BeginPaint();
      scrollbars.EndPaint(wxRegion(3000, 3000, 10, 10));
      scrollbars.Sync();
      THEN("its scrollbars stay, since it was never asked to draw") {
        CHECK(scrollbars.VisibleScrollbars() == 2);
      }
    }

    WHEN("the matrix is deleted") {
      group.reset();
      scrollbars.BeginPaint();
      scrollbars.EndPaint(wxRegion(3000, 3000, 10, 10));
      scrollbars.Sync();
      THEN("its scrollbars are gone, wherever the paint was") {
        CHECK(scrollbars.VisibleScrollbars() == 0);
      }
    }
  }
  frame->Destroy();
}

// Draws the matrix onto a white bitmap through a real graphics context, the
// way the worksheet does, and returns the colour just above the given entry:
// inside that entry's row band, but in the gap between two rows of text, so
// no glyph gets in the way.
static wxColour ColourAboveEntry(MatrCell *matr, int row, int col) {
  wxBitmap bitmap(2000, 1200);
  wxMemoryDC dc(bitmap);
  dc.SetBackground(*wxWHITE_BRUSH);
  dc.Clear();
  wxGCDC antialiassingDC(dc);
  NoClipToDrawRegion noClip(g_cfg);
  matr->Draw(&dc, &antialiassingDC);
  // wxGCDC may buffer; make sure everything has reached the bitmap.
  antialiassingDC.GetGraphicsContext()->Flush();
  const wxRect entry = matr->GetInnerCell(row, col)->GetRect();
  wxColour colour;
  dc.GetPixel(entry.x + entry.width / 2, entry.y - 2, &colour);
  return colour;
}

SCENARIO("Only a matrix too large for the window gets alternating bands") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);

  GIVEN("an elided matrix") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 60, 40);
    REQUIRE(matr->ElidedColumns() > 0);
    THEN("it is banded: odd rows are tinted, the first row isn't") {
      CHECK(matr->IsBanded());
      CHECK(ColourAboveEntry(matr, 0, 0) == *wxWHITE);
      CHECK(ColourAboveEntry(matr, 1, 0) != *wxWHITE);
    }
    THEN("where an odd row crosses an odd column the tint doubles") {
      const wxColour single = ColourAboveEntry(matr, 1, 0);
      const wxColour crossing = ColourAboveEntry(matr, 1, 1);
      CHECK(crossing.Red() < single.Red());
    }
    THEN("the tint stays faint, so it reads as shading, not as colour") {
      CHECK(ColourAboveEntry(matr, 1, 0).Red() > 220);
    }
  }

  GIVEN("a matrix that fits") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 3, 3);
    THEN("it stays plain") {
      CHECK_FALSE(matr->IsBanded());
      CHECK(ColourAboveEntry(matr, 1, 0) == *wxWHITE);
      CHECK(ColourAboveEntry(matr, 1, 1) == *wxWHITE);
    }
  }

  GIVEN("a scrolling matrix") {
    FakeScrollHost host;
    ScrollModeWithHost scrollMode(&host);
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOutMatrix(group, 60, 40);
    THEN("it is banded too, and the bands move with the entries") {
      CHECK(matr->IsBanded());
      CHECK(ColourAboveEntry(matr, 1, 0) != *wxWHITE);
      matr->ScrollTo(wxPoint(0, 60));
      // Whatever row now sits at the top of the viewport, its band follows
      // its own index, not its position on screen.
      for (int row = 0; row < 12; row++) {
        const wxRect entry = matr->GetInnerCell(row, 0)->GetRect();
        if (!matr->ViewportRect().Contains(wxPoint(entry.x + 1, entry.y - 2)))
          continue;
        INFO("row " << row);
        CHECK((ColourAboveEntry(matr, row, 0) == *wxWHITE) == (row % 2 == 0));
      }
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
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(400, 400);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  auto *frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
