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
  The matrix viewer a double-click on an elided matrix opens (GH #2344).

  Checks that the viewer's copy shows every entry of a matrix the worksheet
  only shows part of -- whatever the worksheet's setting or the matrix's own
  oversized option says -- and that a double-click finds the right matrix.
  The matrices are parsed from the XML Maxima sends, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/config.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
#include "dialogs/MatrixViewer.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
} // namespace

// A rows x cols matrix whose entries all differ, as Maxima sends it, with an
// optional oversized="..." attribute (wx_matrix()'s oversized option).
static wxString MatrixTableXml(size_t rows, size_t cols,
                               const wxString &oversized = {}) {
  wxString xml = wxS("<tb roundedParens=\"true\"");
  if (!oversized.IsEmpty())
    xml += wxS(" oversized=\"") + oversized + wxS("\"");
  xml += wxS(">");
  for (size_t r = 0; r < rows; r++) {
    xml += wxS("<mtr>");
    for (size_t c = 0; c < cols; c++)
      xml += wxString::Format(wxS("<mtd><mn>%lu</mn></mtd>"),
                              static_cast<unsigned long>(100000 + r * 1000 + c));
    xml += wxS("</mtr>");
  }
  return xml + wxS("</tb>");
}

static wxString OutputXml(const wxString &math) {
  return wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>") + math + wxS("</mth>");
}

// matrix([<rows x cols matrix>, x], [y, z]): an oversized matrix nested in a
// small one.
static wxString NestedMatrixXml(size_t rows, size_t cols) {
  return wxS("<tb roundedParens=\"true\"><mtr><mtd>") +
    MatrixTableXml(rows, cols) +
    wxS("</mtd><mtd><mi>x</mi></mtd></mtr>"
        "<mtr><mtd><mi>y</mi></mtd><mtd><mi>z</mi></mtd></mtr></tb>");
}

// Lays out a group whose output is this XML in cfg and returns its output.
static MatrCell *LayOut(std::unique_ptr<GroupCell> &group, Configuration *cfg,
                        const wxString &xml) {
  group = std::make_unique<GroupCell>(cfg, GC_TYPE_CODE, wxS("m;"));
  MathParser parser(cfg);
  auto output = parser.ParseLine(xml);
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  group->Recalculate();
  group->SetCurrentPoint(wxPoint(50, 50));
  auto *matr = dynamic_cast<MatrCell *>(group->GetOutput());
  REQUIRE(matr != nullptr);
  return matr;
}

/* The configuration a viewer would get when opened from g_cfg's worksheet

   Cells reach their worksheet through their configuration, so, as in the
   viewer itself, the configuration gets a worksheet of its own before any
   cell is made with it -- and is configured for the viewer only after that,
   as the worksheet's constructor reads the config file into it. The worksheet is destroyed first, as it still uses
   the configuration while it goes.
*/
class ViewerConfiguration {
public:
  ViewerConfiguration() : m_cfg(std::make_unique<Configuration>(*g_cfg)) {
    m_ws = new Worksheet(g_frame, wxID_ANY, m_cfg.get(), wxDefaultPosition,
                         wxDefaultSize, /*reactToEvents=*/false);
    MatrixViewer::ConfigureForViewer(*m_cfg);
    m_cfg->SetCanvasSize(wxSize(600, 300));
  }
  ~ViewerConfiguration() { m_ws->Destroy(); }
  Configuration *get() const { return m_cfg.get(); }
  Configuration *operator->() const { return m_cfg.get(); }
private:
  std::unique_ptr<Configuration> m_cfg;
  Worksheet *m_ws = nullptr;
};

// Is any matrix in this list, or inside it, shown only partially?
static bool AnyMatrixShownPartially(Cell *list) {
  for (Cell &cell : OnList(list)) {
    if (auto *matrix = dynamic_cast<MatrCell *>(&cell))
      if (matrix->IsShownPartially())
        return true;
    for (Cell &inner : OnInner(&cell))
      if (AnyMatrixShownPartially(&inner))
        return true;
  }
  return false;
}

// Sets how oversized matrices are shown for the scope of one test.
class OversizedMatricesMode {
public:
  explicit OversizedMatricesMode(Configuration::OversizedMatrices mode)
    : m_old(g_cfg->GetOversizedMatrices()) { g_cfg->SetOversizedMatrices(mode); }
  ~OversizedMatricesMode() { g_cfg->SetOversizedMatrices(m_old); }
private:
  Configuration::OversizedMatrices m_old;
};

SCENARIO("The viewer's copy of an elided matrix shows all of it") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);
  const size_t rows = 60, cols = 40;

  GIVEN("a matrix the worksheet elides") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg, OutputXml(MatrixTableXml(rows, cols)));
    REQUIRE(matr->IsShownPartially());
    REQUIRE(matr->ElidedColumns() > 0);

    WHEN("the viewer copies it") {
      ViewerConfiguration cfg;
      auto copy = MatrixViewer::CopyForViewer(*matr, cfg.get());
      REQUIRE(copy != nullptr);
      copy->Recalculate();
      auto *copied = dynamic_cast<MatrCell *>(copy->GetOutput());
      THEN("the output is the matrix, behind an empty label") {
        REQUIRE(copied != nullptr);
        REQUIRE(copy->GetLabel() != nullptr);
        CHECK(copy->GetLabel()->ToString().Trim().IsEmpty());
      }
      THEN("the copy has every entry, and shows every one of them") {
        REQUIRE(copied != nullptr);
        CHECK(copied->GetMatrixRows() == rows);
        CHECK(copied->GetMatrixColumns() == cols);
        CHECK(copied->ToString() == matr->ToString());
        CHECK_FALSE(copied->IsShownPartially());
        CHECK(copied->ElidedColumns() == 0);
        CHECK(copied->ElidedRows() == 0);
        CHECK(copied->GetWidth() > 600);
      }
      THEN("its cells belong to the viewer's configuration, not the worksheet's") {
        REQUIRE(copied != nullptr);
        CHECK(copied->GetConfiguration() == cfg.get());
        CHECK(copied->GetInnerCell(0, 0)->GetConfiguration() == cfg.get());
      }
      THEN("the viewer hides the input and the label") {
        REQUIRE(g_cfg->ShowCodeCells());
        REQUIRE(g_cfg->ShowLabels());
        CHECK_FALSE(cfg->ShowCodeCells());
        CHECK_FALSE(cfg->ShowLabels());
      }
    }
  }

  GIVEN("a matrix that asks to be elided even where others are shown in full") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg,
                            OutputXml(MatrixTableXml(rows, cols, wxS("elide"))));
    REQUIRE(matr->ElidedColumns() > 0);
    THEN("the viewer shows it in full anyway") {
      ViewerConfiguration cfg;
      auto copy = MatrixViewer::CopyForViewer(*matr, cfg.get());
      copy->Recalculate();
      auto *copied = dynamic_cast<MatrCell *>(copy->GetOutput());
      REQUIRE(copied != nullptr);
      // The copy still says what the matrix asked for, so copying it out of
      // the viewer keeps the option; the viewer just doesn't honour it.
      CHECK(copied->GetOversizedMode() == Configuration::OversizedMatrices::elide);
      CHECK_FALSE(copied->IsShownPartially());
    }
  }

  GIVEN("an oversized matrix nested in an elided one") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg,
                            OutputXml(NestedMatrixXml(rows, cols)));
    REQUIRE(AnyMatrixShownPartially(group->GetOutput()));
    THEN("the viewer shows both in full") {
      ViewerConfiguration cfg;
      auto copy = MatrixViewer::CopyForViewer(*matr, cfg.get());
      copy->Recalculate();
      CHECK_FALSE(AnyMatrixShownPartially(copy->GetOutput()));
    }
  }
}

SCENARIO("A double-click finds the matrix the worksheet shows only part of") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);

  GIVEN("an elided matrix") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg, OutputXml(MatrixTableXml(60, 40)));
    const wxRect rect = matr->GetRect();
    THEN("a click on it finds it") {
      CHECK(MatrixViewer::PartiallyShownMatrixAt(group.get(),
                                                 rect.GetPosition() + wxPoint(5, 5)) == matr);
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), wxPoint(rect.x + rect.width / 2, rect.y + rect.height / 2)) == matr);
    }
    THEN("its tooltip says that a double-click shows all of it") {
      CHECK(matr->GetToolTip(wxPoint(rect.x + rect.width / 2,
                                     rect.y + rect.height / 2))
            .Contains(wxS("Double-click")));
    }
    THEN("a click beside it finds nothing") {
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), wxPoint(rect.GetRight() + 20, rect.y + 5)) == nullptr);
    }
    WHEN("the output is hidden") {
      group->Hide(true);
      THEN("nothing is found, as nothing is drawn") {
        CHECK(MatrixViewer::PartiallyShownMatrixAt(
                group.get(), rect.GetPosition() + wxPoint(5, 5)) == nullptr);
      }
    }
  }

  GIVEN("a matrix that fits") {
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg, OutputXml(MatrixTableXml(2, 2)));
    REQUIRE_FALSE(matr->IsShownPartially());
    THEN("a click on it finds nothing: there is nothing more to show") {
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), matr->GetRect().GetPosition() + wxPoint(2, 2)) == nullptr);
    }
  }

  GIVEN("an oversized matrix nested in a small one") {
    std::unique_ptr<GroupCell> group;
    MatrCell *outer = LayOut(group, g_cfg, OutputXml(NestedMatrixXml(60, 40)));
    MatrCell *inner = dynamic_cast<MatrCell *>(outer->GetInnerCell(0, 0));
    REQUIRE(inner != nullptr);
    THEN("a click on the inner one finds the outermost one shown partially") {
      MatrCell *expected = outer->IsShownPartially() ? outer : inner;
      REQUIRE(expected->IsShownPartially());
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), inner->GetRect().GetPosition() + wxPoint(5, 5)) == expected);
    }
  }

  GIVEN("a matrix shown in full") {
    OversizedMatricesMode full(Configuration::OversizedMatrices::showInFull);
    std::unique_ptr<GroupCell> group;
    MatrCell *matr = LayOut(group, g_cfg, OutputXml(MatrixTableXml(60, 40)));
    THEN("a click on it finds nothing") {
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), matr->GetRect().GetPosition() + wxPoint(5, 5)) == nullptr);
    }
  }
}

// How many matrix viewers are open right now?
static std::vector<MatrixViewer *> OpenViewers() {
  std::vector<MatrixViewer *> viewers;
  for (wxWindow *window : wxTopLevelWindows)
    if (auto *viewer = dynamic_cast<MatrixViewer *>(window))
      if (!window->IsBeingDeleted())
        viewers.push_back(viewer);
  return viewers;
}

SCENARIO("Double-clicking an elided matrix in the worksheet opens a viewer") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);
  REQUIRE(OpenViewers().empty());

  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
  MathParser parser(g_cfg);
  group->AppendOutput(parser.ParseLine(OutputXml(MatrixTableXml(60, 40))));
  g_ws->InsertGroupCells(std::move(group), nullptr, nullptr);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetTree() != nullptr);
  auto *matr = dynamic_cast<MatrCell *>(g_ws->GetTree()->GetOutput());
  REQUIRE(matr != nullptr);
  REQUIRE(matr->IsShownPartially());

  // Mouse events report window (scrolled) coordinates.
  wxPoint onMatrix;
  g_ws->CalcScrolledPosition(matr->GetRect().x + 5, matr->GetRect().y + 5,
                             &onMatrix.x, &onMatrix.y);

  // What the config file says before a viewer is opened.
  wxConfig::Get()->Write(wxS("showLabelChoice"),
                         static_cast<long>(Configuration::labels_automatic));
  wxConfig::Get()->Write(wxS("oversizedMatrices"),
                         static_cast<long>(Configuration::OversizedMatrices::elide));
  const long savedLabels = static_cast<long>(Configuration::labels_automatic);
  const long savedOversized =
    static_cast<long>(Configuration::OversizedMatrices::elide);

  WHEN("the matrix is double-clicked") {
    REQUIRE(g_ws->OpenMatrixViewerAt(onMatrix));
    THEN("a viewer opens, showing all of the matrix") {
      auto viewers = OpenViewers();
      REQUIRE(viewers.size() == 1);
      Worksheet *viewerWorksheet = viewers.front()->GetWorksheet();
      REQUIRE(viewerWorksheet != nullptr);
      viewerWorksheet->RecalculateIfNeeded();
      REQUIRE(viewerWorksheet->GetTree() != nullptr);
      auto *copied = dynamic_cast<MatrCell *>(viewerWorksheet->GetTree()->GetOutput());
      REQUIRE(copied != nullptr);
      CHECK(copied != matr);
      CHECK(copied->ToString() == matr->ToString());
      CHECK_FALSE(copied->IsShownPartially());
      // ...and the worksheet it was opened from is unaffected.
      CHECK(matr->IsShownPartially());
      CHECK(g_cfg->GetOversizedMatrices() == Configuration::OversizedMatrices::elide);
      CHECK(g_cfg->ShowCodeCells());
    }
    for (auto *viewer : OpenViewers())
      delete viewer;
    THEN("closing it leaves the config file alone") {
      // The viewer's configuration shows every matrix in full and hides the
      // labels. Written back, as a configuration is when it is destroyed,
      // that would have become the user's own setting.
      long labels = -1, oversized = -1;
      wxConfig::Get()->Read(wxS("showLabelChoice"), &labels);
      wxConfig::Get()->Read(wxS("oversizedMatrices"), &oversized);
      CHECK(labels == savedLabels);
      CHECK(oversized == savedOversized);
    }
  }

  WHEN("the empty space beside it is double-clicked") {
    wxPoint beside;
    g_ws->CalcScrolledPosition(matr->GetRect().GetRight() + 20,
                               matr->GetRect().y + 5, &beside.x, &beside.y);
    THEN("no viewer opens") {
      CHECK_FALSE(g_ws->OpenMatrixViewerAt(beside));
      CHECK(OpenViewers().empty());
    }
  }

  g_ws->DestroyTree();
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
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  // What a worksheet shows by default, whatever the config file the
  // worksheet's constructor just read says, so the tests can tell that the
  // viewer hides them.
  g_cfg->SetLabelChoice(Configuration::labels_automatic);
  g_cfg->ShowCodeCells(true);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
