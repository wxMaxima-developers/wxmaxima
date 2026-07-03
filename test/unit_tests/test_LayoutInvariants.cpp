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
#include "Worksheet.h"
#include "cells/Cell.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#include <vector>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
} // namespace

static void EnsureDisplay() {
#ifndef _WIN32
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);
  }
#endif
}

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

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  EnsureDisplay();
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
