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
  oversized option says -- that a double-click finds the right matrix, and
  that Ctrl+F's search steps through the matrix entry by entry.
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
#include "dialogs/FindReplaceDialog.h"
#include "dialogs/LoggingMessageDialog.h"
#include "dialogs/MatrixViewer.h"
#include "worksheet/Worksheet.h"
#include "worksheet/WorksheetSearch.h"

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
      THEN("the copy is banded and has no brackets, unlike the original") {
        REQUIRE(copied != nullptr);
        CHECK(copied->IsBanded());
        // Only ToMathML() says which brackets a matrix draws.
        CHECK(matr->ToMathML().StartsWith(wxS("<mrow><mo>")));
        CHECK_FALSE(copied->ToMathML().StartsWith(wxS("<mrow><mo>")));
        // The original is only copied, never changed.
        CHECK(matr->IsBanded());
      }
      THEN("the matrix starts at the left margin, not indented like output") {
        REQUIRE(copied != nullptr);
        CHECK(cfg->GetIndent() == cfg->GetBaseIndent());
        CHECK(cfg->GetIndent() < g_cfg->GetIndent());
        CHECK_FALSE(cfg->IndentMaths());
        CHECK(cfg->HideBrackets());
        copy->SetCurrentPoint(wxPoint(cfg->GetIndent(), 50));
        CHECK(copied->GetRect().GetLeft() == cfg->GetIndent());
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
      // ...but bands it, as it bands every matrix it shows.
      CHECK(copied->IsBanded());
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
    THEN("only the outer one loses its brackets and is banded") {
      ViewerConfiguration cfg;
      auto copy = MatrixViewer::CopyForViewer(*matr, cfg.get());
      copy->Recalculate();
      auto *outer = dynamic_cast<MatrCell *>(copy->GetOutput());
      REQUIRE(outer != nullptr);
      auto *inner = dynamic_cast<MatrCell *>(outer->GetInnerCell(0, 0));
      REQUIRE(inner != nullptr);
      CHECK(outer->IsBanded());
      CHECK_FALSE(outer->ToMathML().StartsWith(wxS("<mrow><mo>")));
      CHECK_FALSE(inner->IsBanded());
      CHECK(inner->ToMathML().StartsWith(wxS("<mrow><mo>")));
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
    THEN("a click near its right edge finds it, too") {
      // The output starts right of the group's own left edge, so the matrix
      // reaches further right than GroupCell::GetRect() does. Only the right
      // part of such a matrix used to ignore a double-click.
      REQUIRE(rect.GetRight() > group->GetRect().GetRight());
      CHECK(MatrixViewer::PartiallyShownMatrixAt(
              group.get(), wxPoint(rect.GetRight() - 5, rect.y + 5)) == matr);
      // ...and so does its tooltip, which comes through the group, too.
      CHECK(group->GetToolTip(wxPoint(rect.GetRight() - 5, rect.y + 5))
            .Contains(wxS("Double-click")));
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

SCENARIO("A double-click finds an elided matrix that has no output label") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);

  GIVEN("a matrix shown by disp(), read back from a file") {
    // disp() prints no "(%o1)" label, so the matrix is the output's first
    // cell -- which a GroupCell keeps in its label slot, where GetOutput()
    // doesn't look. It is drawn all the same.
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
    MathParser parser(g_cfg);
    auto output = parser.ParseLine(wxS("<mth>") + MatrixTableXml(60, 40) + wxS("</mth>"));
    REQUIRE(output != nullptr);
    group->AppendOutput(std::move(output));
    group->Recalculate();
    group->SetCurrentPoint(wxPoint(50, 50));
    auto *matr = dynamic_cast<MatrCell *>(group->GetLabel());
    REQUIRE(matr != nullptr);
    REQUIRE(matr->IsShownPartially());
    const wxRect rect = matr->GetRect();
    THEN("a double-click on it finds it") {
      CHECK(MatrixViewer::PartiallyShownMatrixAt(group.get(),
                                                 rect.GetPosition() + wxPoint(5, 5)) == matr);
    }
  }
}

// How many matrix viewers are open right now?
// The entry a search in the viewer has selected, if it selected exactly one
static std::optional<MatrixEntry> SelectedEntry(const MatrixViewer &viewer) {
  const auto item = viewer.GetWorksheet()->SelectedOutputItem();
  if (!item || !item->IsMatrixEntry() || (item->matrix != viewer.GetMatrix()))
    return std::nullopt;
  return item->entry;
}

SCENARIO("A search in the viewer finds matrix entries, one at a time") {
  // Entry (r, c) of MatrixTableXml() is 100000 + 1000 r + c.
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  ViewerConfiguration cfg;
  std::unique_ptr<GroupCell> group;
  MatrCell *matr = LayOut(group, cfg.get(), OutputXml(MatrixTableXml(3, 4)));
  const WorksheetSearch::StringMatcher every(wxS("00"), false);
  bool wrapped = true;

  GIVEN("no entry to start at") {
    THEN("searching down finds the first match, searching up the last") {
      const WorksheetSearch::StringMatcher row1(wxS("1010"), false);
      CHECK(MatrixViewer::FindEntry(*matr, std::nullopt, true, false, row1,
                                    &wrapped) == MatrixEntry{1, 0});
      CHECK_FALSE(wrapped);
      CHECK(MatrixViewer::FindEntry(*matr, std::nullopt, false, false, row1,
                                    &wrapped) == MatrixEntry{1, 3});
      CHECK_FALSE(wrapped);
    }
  }
  GIVEN("a start entry") {
    THEN("the search moves on row by row, in either direction") {
      CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{0, 3}, true, false,
                                    every, &wrapped) == MatrixEntry{1, 0});
      CHECK_FALSE(wrapped);
      CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{1, 0}, false, false,
                                    every, &wrapped) == MatrixEntry{0, 3});
      CHECK_FALSE(wrapped);
    }
    THEN("past the last entry it starts over, and says so") {
      CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{2, 3}, true, false,
                                    every, &wrapped) == MatrixEntry{0, 0});
      CHECK(wrapped);
      CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{0, 0}, false, false,
                                    every, &wrapped) == MatrixEntry{2, 3});
      CHECK(wrapped);
    }
    THEN("the only match is found again after a full round") {
      const WorksheetSearch::StringMatcher one(wxS("102002"), false);
      CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{2, 2}, true, false,
                                    one, &wrapped) == MatrixEntry{2, 2});
      CHECK(wrapped);
      AND_THEN("an incremental search keeps it without wrapping") {
        CHECK(MatrixViewer::FindEntry(*matr, MatrixEntry{2, 2}, true, true,
                                      one, &wrapped) == MatrixEntry{2, 2});
        CHECK_FALSE(wrapped);
      }
    }
  }
  THEN("regular expressions work, and a search without a match finds nothing") {
    const WorksheetSearch::RegexMatcher regex(wxS("^10200[23]$"));
    REQUIRE(regex.IsValid());
    CHECK(MatrixViewer::FindEntry(*matr, std::nullopt, true, false, regex) ==
          MatrixEntry{2, 2});
    const WorksheetSearch::StringMatcher none(wxS("x"), false);
    CHECK_FALSE(MatrixViewer::FindEntry(*matr, std::nullopt, true, false, none));
  }
}

SCENARIO("Ctrl+F in the viewer searches the viewer's matrix") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  OversizedMatricesMode mode(Configuration::OversizedMatrices::elide);
  LoggingMessageDialog::SetNonInteractive(true);
  std::unique_ptr<GroupCell> group;
  MatrCell *matr = LayOut(group, g_cfg, OutputXml(MatrixTableXml(60, 40)));
  auto *viewer = new MatrixViewer(g_frame, *matr, g_cfg);
  viewer->GetWorksheet()->RecalculateIfNeeded();
  REQUIRE(viewer->GetMatrix() != nullptr);

  WHEN("an entry is searched for") {
    REQUIRE(viewer->FindNext(wxS("159039"), true, true, false, false));
    THEN("that entry is selected, even though the worksheet elides it") {
      CHECK(SelectedEntry(*viewer) == MatrixEntry{59, 39});
    }
    THEN("the viewer is scrolled so that the entry is visible") {
      // The last entry: far right and far below of what the window shows
      // unscrolled.
      Worksheet *ws = viewer->GetWorksheet();
      const wxRect rect = viewer->GetMatrix()->BlockRect({59, 59, 39, 39});
      REQUIRE_FALSE(rect.IsEmpty());
      wxPoint topLeft, bottomRight;
      ws->CalcScrolledPosition(rect.GetLeft(), rect.GetTop(),
                               &topLeft.x, &topLeft.y);
      ws->CalcScrolledPosition(rect.GetRight(), rect.GetBottom(),
                               &bottomRight.x, &bottomRight.y);
      const wxSize client = ws->GetClientSize();
      INFO("entry at " << topLeft.x << "," << topLeft.y << " in a view of "
           << client.x << "x" << client.y);
      CHECK(topLeft.x >= 0);
      CHECK(topLeft.y >= 0);
      CHECK(bottomRight.x < client.x);
      CHECK(bottomRight.y < client.y);
    }
    AND_WHEN("the search is repeated for something several entries share") {
      // 101000 ... 101039, i.e. row 1
      REQUIRE(viewer->FindNext(wxS("1010"), true, true, false, false));
      CHECK(SelectedEntry(*viewer) == MatrixEntry{1, 0});
      REQUIRE(viewer->FindNext(wxS("1010"), true, true, false, false));
      THEN("it steps on to the next match") {
        CHECK(SelectedEntry(*viewer) == MatrixEntry{1, 1});
      }
    }
  }
  WHEN("nothing matches") {
    THEN("nothing gets selected") {
      CHECK_FALSE(viewer->FindNext(wxS("nowhere"), true, true, false, false));
      CHECK_FALSE(SelectedEntry(*viewer));
    }
  }
  WHEN("the search dialog's Find button is pressed") {
    viewer->OpenFindDialog();
    FindReplaceDialog *dialog = viewer->GetFindDialog();
    REQUIRE(dialog != nullptr);
    dialog->SetFindString(wxS("100002"));
    // The search runs down, whichever direction the config file remembers.
    dialog->GetData()->SetFlags(dialog->GetData()->GetFlags() | wxFR_DOWN);
    wxWindow *findButton = dialog->FindWindow(wxID_FIND);
    REQUIRE(findButton != nullptr);
    wxCommandEvent press(wxEVT_BUTTON, wxID_FIND);
    press.SetEventObject(findButton);
    findButton->GetEventHandler()->ProcessEvent(press);
    wxTheApp->ProcessPendingEvents();
    THEN("the viewer searches its own matrix, not the main worksheet") {
      CHECK(SelectedEntry(*viewer) == MatrixEntry{0, 2});
    }
    THEN("the dialog has nothing to replace") {
      wxWindow *replaceButton = dialog->FindWindow(wxID_REPLACE);
      REQUIRE(replaceButton != nullptr);
      CHECK_FALSE(replaceButton->IsShown());
    }
  }

  // Closes the search dialog, too: it is the viewer's child.
  delete viewer;
  LoggingMessageDialog::SetNonInteractive(false);
}

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
