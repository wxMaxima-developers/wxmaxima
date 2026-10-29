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
  Selecting and copying a sub-matrix (GH #2345).

  Dragging a rectangle across several entries of a matrix used to select the
  whole matrix. It now selects the block of entries the rectangle touches, and
  every "Copy ..." command copies just that block. These tests drive
  Worksheet::SelectOutputRect() -- what a mouse drag in an output calls --
  with points taken from the laid-out entries, and check what gets selected
  and copied. The matrices are parsed from the XML Maxima sends, so no Maxima
  is needed, and nothing touches the system clipboard.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dataobj.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "CompositeDataObject.h"
#include "Configuration.h"
#include "MathParser.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
#include "cells/MatrixScrollHost.h"
#include "graphical_io/BitmapOut.h"
#include "worksheet/ClipboardContents.h"
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

// A rows x cols matrix whose entry in row r, column c reads 1<r><c> (e.g.
// 123 for row 2, column 3, counting from 0), as Maxima sends it. extraAttrs
// go into the <tb> tag.
static wxString MatrixTableXml(size_t rows, size_t cols,
                               const wxString &extraAttrs = {}) {
  wxString xml = wxS("<tb roundedParens=\"true\"") + extraAttrs + wxS(">");
  for (size_t r = 0; r < rows; r++) {
    xml += wxS("<mtr>");
    for (size_t c = 0; c < cols; c++)
      xml += wxString::Format(wxS("<mtd><mn>1%lu%lu</mn></mtd>"),
                              static_cast<unsigned long>(r),
                              static_cast<unsigned long>(c));
    xml += wxS("</mtr>");
  }
  return xml + wxS("</tb>");
}

static wxString OutputXml(const wxString &math) {
  return wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>") + math + wxS("</mth>");
}

// Puts a cell with this output into the (otherwise empty) worksheet, lays it
// out and returns the matrix. Unless partialOk, requires every entry to be
// shown.
static MatrCell *ShowMatrix(const wxString &tableXml, bool partialOk = false) {
  g_ws->DestroyTree();
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
  MathParser parser(g_cfg);
  parser.SetGroup(group.get());
  group->AppendOutput(parser.ParseLine(OutputXml(tableXml)));
  g_ws->InsertGroupCells(std::move(group), nullptr, nullptr);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetTree() != nullptr);
  auto *matr = dynamic_cast<MatrCell *>(g_ws->GetTree()->GetOutput());
  REQUIRE(matr != nullptr);
  if (!partialOk)
    REQUIRE_FALSE(matr->IsShownPartially());
  return matr;
}

// The middle of an entry, in worksheet coordinates
static wxPoint Middle(const MatrCell *matr, int row, int col) {
  const wxRect rect = matr->GetInnerCell(row, col)->GetRect();
  return wxPoint(rect.x + rect.width / 2, rect.y + rect.height / 2);
}

// Drags from the middle of one entry to the middle of another.
static void Drag(MatrCell *matr, int fromRow, int fromCol, int toRow,
                 int toCol) {
  g_ws->SelectOutputRect(g_ws->GetTree(), Middle(matr, fromRow, fromCol),
                         Middle(matr, toRow, toCol));
}

static const DocumentCellPointers &Pointers() {
  return g_ws->GetDocumentCellPointers();
}

SCENARIO("Dragging across several entries of a matrix selects just that block") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  MatrCell *matr = ShowMatrix(MatrixTableXml(4, 4));

  WHEN("a rectangle is dragged from row 1, column 1 to row 2, column 2") {
    Drag(matr, 1, 1, 2, 2);
    THEN("the matrix is selected, narrowed to that block") {
      CHECK(Pointers().GetSelectionStart() == matr);
      CHECK(Pointers().GetSelectionEnd() == matr);
      const auto block = Pointers().GetSelectedMatrixBlock();
      REQUIRE(block);
      CHECK(*block == MatrixBlock{1, 2, 1, 2});
    }
    THEN("copying it copies the 2x2 sub-matrix") {
      auto copy = g_ws->CopySelectedMatrixBlock();
      REQUIRE(copy != nullptr);
      CHECK(copy->GetMatrixRows() == 2);
      CHECK(copy->GetMatrixColumns() == 2);
      const wxString expected =
        wxS("matrix(\n\t\t[111,\t112],\n\t\t[121,\t122]\n\t)");
      CHECK(copy->ToString() == expected);
      // What "Copy", "Copy as text", "Copy output to input" and drag-and-drop
      // use...
      CHECK(g_ws->GetString(true) == expected);
      // ...and what the MathML, RTF and image flavours are rendered from.
      auto cells = g_ws->CopySelection();
      REQUIRE(cells != nullptr);
      CHECK(cells->GetNext() == nullptr);
      CHECK(cells->ToString() == expected);
      CHECK(cells->ToTeX().Contains(wxS("111")));
      CHECK_FALSE(cells->ToTeX().Contains(wxS("100")));
      CHECK(cells->ToMatlab() == wxS("[111, 112;\n121, 122];"));
    }
    THEN("the highlight covers those entries and no others") {
      const wxRect rect = matr->BlockRect(MatrixBlock{1, 2, 1, 2});
      CHECK(rect.Contains(Middle(matr, 1, 1)));
      CHECK(rect.Contains(Middle(matr, 2, 2)));
      CHECK_FALSE(rect.Contains(Middle(matr, 0, 0)));
      CHECK_FALSE(rect.Contains(Middle(matr, 3, 3)));
      CHECK_FALSE(rect.Contains(Middle(matr, 1, 3)));
    }
  }

  WHEN("the rectangle is dragged the other way round") {
    Drag(matr, 2, 3, 0, 1);
    THEN("it selects the same kind of block") {
      const auto block = Pointers().GetSelectedMatrixBlock();
      REQUIRE(block);
      CHECK(*block == MatrixBlock{0, 2, 1, 3});
    }
  }

  WHEN("a rectangle spans the whole matrix") {
    g_ws->SelectOutputRect(g_ws->GetTree(),
                           matr->GetRect().GetTopLeft() + wxPoint(1, 1),
                           matr->GetRect().GetBottomRight() - wxPoint(1, 1));
    THEN("the whole matrix is selected, as before") {
      CHECK(Pointers().GetSelectionStart() == matr);
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
      CHECK(g_ws->CopySelectedMatrixBlock() == nullptr);
      CHECK(g_ws->GetString(true) == matr->ToString());
    }
  }

  WHEN("a rectangle stays within a single entry") {
    const wxRect entry = matr->GetInnerCell(1, 2)->GetRect();
    g_ws->SelectOutputRect(g_ws->GetTree(), entry.GetTopLeft() + wxPoint(1, 1),
                           entry.GetBottomRight() - wxPoint(1, 1));
    THEN("that entry is selected, as before") {
      CHECK(Pointers().GetSelectionStart() == matr->GetInnerCell(1, 2));
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
    }
  }

  WHEN("a block is selected and the selection then changes") {
    Drag(matr, 0, 0, 1, 1);
    REQUIRE(Pointers().GetSelectedMatrixBlock());
    g_ws->ClearSelection();
    THEN("the block is forgotten") {
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
    }
    AND_WHEN("the same matrix is selected as a whole afterwards") {
      g_ws->SetSelection(matr, matr);
      THEN("it is the whole matrix that is selected, not the old block") {
        CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
        CHECK(g_ws->CopySelectedMatrixBlock() == nullptr);
        CHECK(g_ws->GetString(true) == matr->ToString());
      }
    }
  }

  g_ws->DestroyTree();
}

SCENARIO("A copied block keeps a heading only if it includes it") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  MatrCell *matr = ShowMatrix(MatrixTableXml(
    3, 3, wxS(" special=\"true\" rownames=\"true\" colnames=\"true\"")));
  REQUIRE(matr->ToXML().Contains(wxS("rownames=\"true\"")));

  WHEN("the block includes the heading row and column") {
    auto copy = matr->CopyBlock(MatrixBlock{0, 1, 0, 1}, matr->GetGroup());
    THEN("the copy keeps both headings") {
      CHECK(copy->ToXML().Contains(wxS("rownames=\"true\"")));
      CHECK(copy->ToXML().Contains(wxS("colnames=\"true\"")));
    }
  }
  WHEN("the block leaves the headings out") {
    auto copy = matr->CopyBlock(MatrixBlock{1, 2, 1, 2}, matr->GetGroup());
    THEN("its first row and column are not mistaken for headings") {
      CHECK(copy->ToXML().Contains(wxS("rownames=\"false\"")));
      CHECK(copy->ToXML().Contains(wxS("colnames=\"false\"")));
      CHECK(copy->ToString() ==
            wxS("matrix(\n\t\t[111,\t112],\n\t\t[121,\t122]\n\t)"));
    }
  }
  WHEN("the block reaches past the matrix") {
    auto copy = matr->CopyBlock(MatrixBlock{1, 7, 2, 9}, matr->GetGroup());
    THEN("it is clamped to the matrix") {
      CHECK(copy->GetMatrixRows() == 2);
      CHECK(copy->GetMatrixColumns() == 1);
      CHECK(copy->ToString() == wxS("matrix(\n\t\t[112],\n\t\t[122]\n\t)"));
    }
  }

  g_ws->DestroyTree();
}

SCENARIO("A matrix, or a block of one, can be copied as CSV (GH #2364)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  MatrCell *matr = ShowMatrix(MatrixTableXml(3, 3));

  THEN("each row becomes one line of delimited values") {
    CHECK(matr->ToCSV(wxS(",")) == wxS("100,101,102\n110,111,112\n120,121,122\n"));
    CHECK(matr->ToCSV(wxS("\t")) ==
          wxS("100\t101\t102\n110\t111\t112\n120\t121\t122\n"));
  }
  THEN("the delimiter is a comma unless numbers are written with a decimal comma") {
    const wxString delimiter = Worksheet::CSVDelimiter();
    CHECK(((delimiter == wxS(",")) || (delimiter == wxS("\t"))));
  }
  WHEN("the whole matrix is selected") {
    g_ws->SetSelection(matr);
    THEN("\"Copy as CSV\" is offered and copies all of it") {
      CHECK(g_ws->CanCopyCSV());
      CHECK(g_ws->SelectionToCSV() == matr->ToCSV(Worksheet::CSVDelimiter()));
    }
  }
  WHEN("a block of it is selected") {
    Drag(matr, 1, 1, 2, 2);
    REQUIRE(Pointers().GetSelectedMatrixBlock());
    THEN("only the block is copied") {
      const wxString d = Worksheet::CSVDelimiter();
      CHECK(g_ws->CanCopyCSV());
      CHECK(g_ws->SelectionToCSV() ==
            wxS("111") + d + wxS("112\n121") + d + wxS("122\n"));
    }
  }
  WHEN("a single entry is selected") {
    g_ws->SetSelection(matr->GetInnerCell(0, 0));
    THEN("\"Copy as CSV\" is not offered") {
      CHECK_FALSE(g_ws->CanCopyCSV());
      CHECK(g_ws->SelectionToCSV().IsEmpty());
    }
  }
  g_ws->DestroyTree();

  GIVEN("entries that contain the delimiter, quotes or a line break") {
    MatrCell *special = ShowMatrix(
      wxS("<tb roundedParens=\"true\"><mtr>"
          "<mtd><st>a,b</st></mtd>"
          "<mtd><st>say \"hi\"</st></mtd>"
          "<mtd><mn>1</mn></mtd>"
          "</mtr></tb>"));
    const wxString csv = special->ToCSV(wxS(","));
    THEN("they are quoted, with inner quotes doubled, as RFC 4180 says") {
      INFO("CSV: " << csv.ToStdString());
      // The first entry holds a comma, so it is quoted...
      CHECK(csv.StartsWith(wxS("\"")));
      // ...the last one, a plain number, isn't...
      CHECK(csv.EndsWith(wxS(",1\n")));
      // ...and no lone double quote is left inside a quoted field: splitting
      // at the field-separating commas leaves exactly three fields.
      wxString rest = csv.BeforeLast(wxS('\n'));
      size_t fields = 1;
      bool inQuotes = false;
      for (size_t i = 0; i < rest.Length(); i++) {
        if (rest[i] == wxS('"'))
          inQuotes = !inQuotes;
        else if ((rest[i] == wxS(',')) && !inQuotes)
          fields++;
      }
      CHECK_FALSE(inQuotes);
      CHECK(fields == 3);
    }
    g_ws->DestroyTree();
  }
}

SCENARIO("Copy as HTML copies just the selected part of an output (GH #2369)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  const auto oldFormat = g_cfg->HTMLequationFormat();
  // MathML keeps the entries as text in the HTML, where the test can see them.
  g_cfg->HTMLequationFormat(Configuration::mathML);
  MatrCell *matr = ShowMatrix(MatrixTableXml(4, 4));

  WHEN("a block of the matrix is selected") {
    Drag(matr, 1, 1, 2, 2);
    REQUIRE(Pointers().GetSelectedMatrixBlock());
    const wxString html = g_ws->SelectionToSelfContainedHTML();
    THEN("the HTML holds that block's entries") {
      CHECK(html.Contains(wxS("<math")));
      for (const auto *entry : {"111", "112", "121", "122"})
        CHECK(html.Contains(wxString(entry)));
    }
    THEN("and none of the others, nor the input or the label") {
      for (const auto *entry : {"100", "103", "113", "130", "133"})
        CHECK_FALSE(html.Contains(wxString(entry)));
      CHECK_FALSE(html.Contains(wxS("class=\"input\"")));
      CHECK_FALSE(html.Contains(wxS("%o1")));
    }
  }
  WHEN("a single entry of the matrix is selected") {
    const wxRect entry = matr->GetInnerCell(3, 0)->GetRect();
    g_ws->SelectOutputRect(g_ws->GetTree(), entry.GetTopLeft() + wxPoint(1, 1),
                           entry.GetBottomRight() - wxPoint(1, 1));
    REQUIRE(Pointers().GetSelectionStart() == matr->GetInnerCell(3, 0));
    const wxString html = g_ws->SelectionToSelfContainedHTML();
    THEN("the HTML holds just that entry") {
      CHECK(html.Contains(wxS("130")));
      CHECK_FALSE(html.Contains(wxS("100")));
    }
  }
  WHEN("the whole cell is selected") {
    g_ws->SetSelection(g_ws->GetTree());
    const wxString html = g_ws->SelectionToSelfContainedHTML();
    THEN("the HTML holds the whole matrix and its label, as before") {
      CHECK(html.Contains(wxS("100")));
      CHECK(html.Contains(wxS("133")));
      CHECK(html.Contains(wxS("%o1")));
      if (g_cfg->ShowCodeCells())
        CHECK(html.Contains(wxS("class=\"input\"")));
    }
  }
  WHEN("nothing is selected") {
    g_ws->ClearSelection();
    THEN("there is nothing to copy") {
      CHECK(g_ws->SelectionToSelfContainedHTML().IsEmpty());
    }
  }

  g_ws->DestroyTree();
  g_cfg->HTMLequationFormat(oldFormat);
}

SCENARIO("Shift+arrow keys grow or shrink a selected block (GH #2370)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  MatrCell *matr = ShowMatrix(MatrixTableXml(4, 4));

  WHEN("nothing in the matrix is selected") {
    g_ws->ClearSelection();
    THEN("the arrow keys are left alone") {
      CHECK_FALSE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
    }
  }
  WHEN("the whole matrix was selected by other means than a block") {
    g_ws->SetSelection(matr);
    THEN("the arrow keys are left alone") {
      CHECK_FALSE(g_ws->StepSelectedMatrixBlock(WXK_LEFT));
      CHECK(Pointers().GetSelectionStart() == matr);
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
    }
  }

  WHEN("a block is dragged from row 1, column 1 to row 2, column 2") {
    Drag(matr, 1, 1, 2, 2);
    REQUIRE(Pointers().GetSelectedMatrixBlock());
    AND_WHEN("Shift+Right is pressed") {
      REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
      THEN("the block grows by one column on the far side") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{1, 2, 1, 3});
      }
      THEN("what gets copied follows the block") {
        CHECK(g_ws->GetString(true) ==
              wxS("matrix(\n\t\t[111,\t112,\t113],\n\t\t[121,\t122,\t123]\n\t)"));
      }
      AND_WHEN("Shift+Right is pressed at the matrix's edge") {
        REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
        THEN("the block stays as it is") {
          CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{1, 2, 1, 3});
        }
      }
    }
    AND_WHEN("Shift+Up is pressed twice") {
      g_ws->StepSelectedMatrixBlock(WXK_UP);
      THEN("the first press shrinks the block to its anchor's row") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{1, 1, 1, 2});
      }
      g_ws->StepSelectedMatrixBlock(WXK_UP);
      THEN("the second one grows it past the anchor") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{0, 1, 1, 2});
      }
    }
  }

  WHEN("a block is dragged the other way round, from row 2, column 3") {
    Drag(matr, 2, 3, 0, 1);
    REQUIRE(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{0, 2, 1, 3});
    AND_WHEN("Shift+Left and Shift+Down are pressed") {
      g_ws->StepSelectedMatrixBlock(WXK_LEFT);
      g_ws->StepSelectedMatrixBlock(WXK_DOWN);
      THEN("it is the corner the drag ended at that moves") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{1, 2, 0, 3});
      }
    }
  }

  WHEN("a block grows to cover the whole matrix") {
    Drag(matr, 0, 0, 2, 2);
    g_ws->StepSelectedMatrixBlock(WXK_RIGHT);
    g_ws->StepSelectedMatrixBlock(WXK_DOWN);
    THEN("it becomes an ordinary whole-matrix selection") {
      CHECK(Pointers().GetSelectionStart() == matr);
      CHECK(Pointers().GetSelectionEnd() == matr);
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
      CHECK(g_ws->CopySelectedMatrixBlock() == nullptr);
      CHECK(g_ws->GetString(true) == matr->ToString());
    }
    AND_WHEN("Shift+Left is pressed then") {
      REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_LEFT));
      THEN("it shrinks back into a block") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{0, 3, 0, 2});
      }
    }
  }

  WHEN("a drag spans the whole matrix") {
    Drag(matr, 3, 3, 0, 0);
    REQUIRE_FALSE(Pointers().GetSelectedMatrixBlock());
    AND_WHEN("Shift+Right is pressed") {
      REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
      THEN("the whole-matrix selection shrinks from where the drag ended") {
        CHECK(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{0, 3, 1, 3});
      }
    }
  }

  WHEN("a block is selected and the selection then changes") {
    Drag(matr, 0, 0, 1, 1);
    g_ws->SetSelection(matr, matr);
    THEN("the arrow keys no longer act on the old block") {
      CHECK_FALSE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
      CHECK_FALSE(Pointers().GetSelectedMatrixBlock());
    }
  }

  g_ws->DestroyTree();
}

SCENARIO("Stepping over elided rows takes a single step (GH #2370)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(900, 300));
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::elide);
  const size_t rows = 60;
  MatrCell *matr = ShowMatrix(MatrixTableXml(rows, 3), true);
  REQUIRE(matr->ElidedRows() > 0);
  REQUIRE_FALSE(matr->IsElided(0, 0));
  REQUIRE_FALSE(matr->IsElided(rows - 1, 0));

  size_t lastShownAbove = 0;
  while (!matr->IsElided(lastShownAbove + 1, 0))
    lastShownAbove++;
  const size_t firstShownBelow = lastShownAbove + matr->ElidedRows() + 1;
  REQUIRE_FALSE(matr->IsElided(firstShownBelow, 0));

  THEN("stepping down into the elided run lands on the first row beyond it") {
    CHECK(matr->StepEntry(MatrixEntry{lastShownAbove, 1}, 1, 0) ==
          MatrixEntry{firstShownBelow, 1});
  }
  THEN("stepping up across it lands on the last row before it") {
    CHECK(matr->StepEntry(MatrixEntry{firstShownBelow, 1}, -1, 0) ==
          MatrixEntry{lastShownAbove, 1});
  }
  THEN("at the matrix's edges an entry stays where it is") {
    CHECK(matr->StepEntry(MatrixEntry{0, 0}, -1, 0) == MatrixEntry{0, 0});
    CHECK(matr->StepEntry(MatrixEntry{0, 0}, 0, -1) == MatrixEntry{0, 0});
    CHECK(matr->StepEntry(MatrixEntry{rows - 1, 2}, 1, 0) ==
          MatrixEntry{rows - 1, 2});
    CHECK(matr->StepEntry(MatrixEntry{rows - 1, 2}, 0, 1) ==
          MatrixEntry{rows - 1, 2});
  }

  g_ws->DestroyTree();
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
}

// Stands in for the worksheet's MatrixScrollbars, which only a worksheet that
// reacts to events installs.
class FakeScrollHost final : public MatrixScrollHost {
public:
  wxCoord ScrollbarThickness() const override { return 15; }
  void MatrixDrawn(MatrCell *) override {}
};

// Is the middle of this entry inside the matrix's viewport?
static bool InViewport(const MatrCell *matr, int row, int col) {
  return matr->ViewportRect().Contains(Middle(matr, row, col));
}

SCENARIO("Shift+arrow keys scroll a scrolling matrix along (GH #2380)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  FakeScrollHost host;
  g_cfg->SetMatrixScrollHost(&host);
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::scroll);
  const int rows = 60, cols = 40;
  MatrCell *matr = ShowMatrix(MatrixTableXml(rows, cols), true);
  REQUIRE(matr->HasHorizontalScrollbar());
  REQUIRE(matr->HasVerticalScrollbar());
  REQUIRE(matr->ScrollPosition() == wxPoint(0, 0));
  REQUIRE_FALSE(InViewport(matr, rows - 1, cols - 1));

  // Anchored at row 1, column 1, so that no block reaches all of the matrix
  // and turns into a whole-matrix selection.
  Drag(matr, 1, 1, 2, 2);
  REQUIRE(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{1, 2, 1, 2});

  WHEN("Shift+Right and Shift+Down move the corner past the viewport's edge") {
    for (int i = 2; i < cols - 1; i++)
      REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
    for (int i = 2; i < rows - 1; i++)
      REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_DOWN));
    REQUIRE(Pointers().GetSelectedMatrixBlock());
    REQUIRE(*Pointers().GetSelectedMatrixBlock() ==
            MatrixBlock{1, static_cast<size_t>(rows - 1), 1,
                        static_cast<size_t>(cols - 1)});
    THEN("the matrix has scrolled so that the corner is visible") {
      CHECK(matr->ScrollPosition().x > 0);
      CHECK(matr->ScrollPosition().y > 0);
      CHECK(InViewport(matr, rows - 1, cols - 1));
    }
    AND_WHEN("the corner is moved back to the start") {
      for (int i = 1; i < cols; i++)
        g_ws->StepSelectedMatrixBlock(WXK_LEFT);
      for (int i = 1; i < rows; i++)
        g_ws->StepSelectedMatrixBlock(WXK_UP);
      REQUIRE(Pointers().GetSelectedMatrixBlock());
      REQUIRE(*Pointers().GetSelectedMatrixBlock() == MatrixBlock{0, 1, 0, 1});
      THEN("the matrix scrolls back to its top left") {
        CHECK(matr->ScrollPosition() == wxPoint(0, 0));
        CHECK(InViewport(matr, 0, 0));
      }
    }
  }

  WHEN("the corner moves within the part that is already visible") {
    REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_RIGHT));
    REQUIRE(g_ws->StepSelectedMatrixBlock(WXK_DOWN));
    THEN("the matrix doesn't scroll") {
      CHECK(matr->ScrollPosition() == wxPoint(0, 0));
    }
  }

  g_ws->DestroyTree();
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
  g_cfg->SetMatrixScrollHost(nullptr);
}

// A mouse wheel event as wx delivers one: rotation is in units of the wheel
// delta, positive for up (vertical) or right (horizontal).
static wxMouseEvent Wheel(int rotation,
                          wxMouseWheelAxis axis = wxMOUSE_WHEEL_VERTICAL,
                          bool shift = false) {
  wxMouseEvent event(wxEVT_MOUSEWHEEL);
  event.m_wheelRotation = rotation;
  event.m_wheelDelta = 120;
  event.m_linesPerAction = 3;
  event.m_wheelAxis = axis;
  event.m_shiftDown = shift;
  return event;
}

SCENARIO("The mouse wheel scrolls a selected scrolling matrix (GH #2479)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  FakeScrollHost host;
  g_cfg->SetMatrixScrollHost(&host);
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::scroll);
  MatrCell *matr = ShowMatrix(MatrixTableXml(60, 40), true);
  REQUIRE(matr->HasHorizontalScrollbar());
  REQUIRE(matr->HasVerticalScrollbar());

  WHEN("nothing is selected") {
    g_ws->ClearSelection();
    THEN("the wheel is left to the worksheet") {
      CHECK(g_ws->GetSelectedScrollingMatrix() == nullptr);
      CHECK_FALSE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
      CHECK(matr->ScrollPosition() == wxPoint(0, 0));
    }
  }

  WHEN("a block of the matrix's entries is selected") {
    Drag(matr, 1, 1, 2, 2);
    REQUIRE(g_ws->GetSelectedScrollingMatrix() == matr);
    AND_WHEN("the wheel is turned down") {
      REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
      THEN("the matrix scrolls down") {
        CHECK(matr->ScrollPosition().x == 0);
        CHECK(matr->ScrollPosition().y > 0);
      }
      AND_WHEN("it is turned back up") {
        REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(120)));
        THEN("the matrix is back at its top") {
          CHECK(matr->ScrollPosition() == wxPoint(0, 0));
        }
      }
    }
    AND_WHEN("the wheel is turned up at the matrix's top") {
      THEN("the matrix stays put, but still keeps the wheel") {
        CHECK(g_ws->ScrollSelectedMatrix(Wheel(120)));
        CHECK(matr->ScrollPosition() == wxPoint(0, 0));
      }
    }
    AND_WHEN("a horizontal wheel is turned right") {
      REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(120, wxMOUSE_WHEEL_HORIZONTAL)));
      THEN("the matrix scrolls right") {
        CHECK(matr->ScrollPosition().x > 0);
        CHECK(matr->ScrollPosition().y == 0);
      }
    }
    AND_WHEN("the wheel is turned down with Shift held") {
      REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-120, wxMOUSE_WHEEL_VERTICAL, true)));
      THEN("the matrix scrolls right") {
        CHECK(matr->ScrollPosition().x > 0);
        CHECK(matr->ScrollPosition().y == 0);
      }
    }
    AND_WHEN("a touchpad sends many small steps") {
      for (int i = 0; i < 10; i++)
        REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-12)));
      THEN("they add up to about what one notch does") {
        const int smallSteps = matr->ScrollPosition().y;
        matr->ScrollTo(wxPoint(0, 0));
        REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
        CHECK(smallSteps > 0);
        CHECK(std::abs(smallSteps - matr->ScrollPosition().y) <= 10);
      }
    }
  }

  WHEN("the whole matrix is selected") {
    g_ws->SetSelection(matr);
    THEN("the wheel scrolls it") {
      CHECK(g_ws->GetSelectedScrollingMatrix() == matr);
      REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
      CHECK(matr->ScrollPosition().y > 0);
    }
  }

  WHEN("just a cell inside one entry is selected") {
    Cell *entry = matr->GetInnerCell(1, 1);
    REQUIRE(entry != nullptr);
    g_ws->SetSelection(entry);
    THEN("the wheel scrolls the matrix the entry is in") {
      CHECK(g_ws->GetSelectedScrollingMatrix() == matr);
      REQUIRE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
      CHECK(matr->ScrollPosition().y > 0);
    }
  }

  g_ws->ClearSelection();
  g_ws->DestroyTree();
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
  g_cfg->SetMatrixScrollHost(nullptr);
}

SCENARIO("A selected matrix that doesn't scroll leaves the wheel alone (GH #2479)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(600, 300));
  MatrCell *matr = ShowMatrix(MatrixTableXml(2, 2));
  g_ws->SetSelection(matr);
  CHECK(g_ws->GetSelectedScrollingMatrix() == nullptr);
  CHECK_FALSE(g_ws->ScrollSelectedMatrix(Wheel(-120)));
  g_ws->ClearSelection();
  g_ws->DestroyTree();
}

/*! The child of a clipboard object that provides a format

  Where enhanced metafiles exist (MSW), a selection is put on the clipboard as
  our own CompositeDataObject, since wxDataObjectComposite only accepts a
  wxDataObjectSimple and the metafile isn't one; elsewhere it is a plain
  wxDataObjectComposite. Both are handled, so the tests needn't know which.
*/
static wxDataObject *ClipboardObject(const wxDataObject &data,
                                     const wxDataFormat &format) {
  if (auto *ours = dynamic_cast<const CompositeDataObject *>(&data))
    return ours->GetObject(format);
  return dynamic_cast<const wxDataObjectComposite &>(data).GetObject(format);
}

//! The bitmap a clipboard object offers, drawn now; invalid if it offers none.
static wxBitmap ClipboardBitmap(const wxDataObject &data) {
  std::vector<wxDataFormat> formats(data.GetFormatCount(wxDataObject::Get));
  data.GetAllFormats(formats.data(), wxDataObject::Get);
  for (const auto &format : formats)
    if (auto *bitmap = dynamic_cast<LazyBitmapDataObject *>(
          ClipboardObject(data, format)))
      return bitmap->GetBitmap();
  return {};
}

SCENARIO("Copying a whole large matrix keeps the bitmap within bounds") {
  // Copying a whole matrix copies all of it, however much of it is shown, and
  // the bitmap used to be drawn at full size, whatever that took: Ctrl+C on a
  // 300x300 matrix grew wxMaxima to 8 GB, and on wxQt, which draws every
  // format right away, nothing reached the clipboard for minutes.
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(900, 300));
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::elide);
  const double scale = g_cfg->BitmapScale();
  REQUIRE(scale > 1);

  GIVEN("a matrix too large for a bitmap at the bitmap scale, but not at scale 1") {
    MatrCell *matr = ShowMatrix(MatrixTableXml(60, 60), true);
    REQUIRE(matr->ElidedRows() > 0);
    g_ws->SetSelection(matr, matr);
    const auto data = g_ws->CreateSelectionDataObject();
    THEN("it is offered as a bitmap at scale 1") {
      const wxBitmap bitmap = ClipboardBitmap(*data);
      REQUIRE(bitmap.IsOk());
      CHECK((long)bitmap.GetWidth() * bitmap.GetHeight() <
            BitmapOut::CLIPBOARD_MAX_PIXELS);
      // A scale-1 bitmap of 60 columns is far narrower than 60 entries at
      // the bitmap scale would be: checks it really fell back to scale 1.
      CHECK(bitmap.GetWidth() < 60 * 30 * scale);
    }
  }
  GIVEN("a matrix too large for a bitmap even at scale 1") {
    // Longer than the default "maximum output length to display".
    const long showLength = g_cfg->ShowLength();
    g_cfg->ShowLength(3);
    MatrCell *matr = ShowMatrix(MatrixTableXml(150, 150), true);
    g_cfg->ShowLength(showLength);
    REQUIRE(matr->ElidedRows() > 0);
    g_ws->SetSelection(matr, matr);
    const auto data = g_ws->CreateSelectionDataObject();
    THEN("there is no bitmap, but everything else is copied") {
      CHECK_FALSE(ClipboardBitmap(*data).IsOk());
      const auto *text = dynamic_cast<wxTextDataObject *>(
        ClipboardObject(*data, wxDF_UNICODETEXT));
      REQUIRE(text != nullptr);
      CHECK(text->GetText().Contains(wxS("1149149")));
    }
  }
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);
}

//! The RTF a clipboard object offers, drawn now
static std::string ClipboardRtf(const wxDataObject &data) {
  const wxDataFormat rtfFormat(wxS("application/rtf"));
  const wxDataObject *rtf = ClipboardObject(data, rtfFormat);
  REQUIRE(rtf != nullptr);
  std::string bytes(rtf->GetDataSize(rtfFormat), '\0');
  REQUIRE(rtf->GetDataHere(rtfFormat, bytes.data()));
  return bytes;
}

static size_t Occurrences(const wxString &haystack, const wxString &needle) {
  size_t count = 0;
  for (size_t pos = haystack.find(needle); pos != wxString::npos;
       pos = haystack.find(needle, pos + 1))
    count++;
  return count;
}

SCENARIO("Selected matrix entries are copied as an RTF table (GH #959)") {
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(1000, 1000));

  GIVEN("a matrix of numbers") {
    MatrCell *matr = ShowMatrix(MatrixTableXml(3, 3));
    THEN("its entries make an RTF table with one row per row") {
      const wxString table = matr->ToRTFTable();
      INFO("RTF: " << table.ToStdString());
      CHECK(Occurrences(table, wxS("\\trowd")) == 3);
      CHECK(Occurrences(table, wxS("\\row\n")) == 3);
      CHECK(Occurrences(table, wxS("\\cell\n")) == 9);
      // Every row says where its 3 cells end.
      CHECK(Occurrences(table, wxS("\\cellx")) == 9);
      // In the order of the matrix: row by row
      CHECK(table.find(wxS("100")) < table.find(wxS("101")));
      CHECK(table.find(wxS("102")) < table.find(wxS("110")));
      CHECK(table.find(wxS("121")) < table.find(wxS("122")));
      // The text after the table is no part of it.
      CHECK(table.EndsWith(wxS("\\pard\n")));
    }
    WHEN("a block of its entries is selected and copied") {
      Drag(matr, 1, 1, 2, 2);
      REQUIRE(Pointers().GetSelectedMatrixBlock());
      const auto data = g_ws->CreateSelectionDataObject();
      const wxString rtf = wxString::FromUTF8(ClipboardRtf(*data));
      INFO("RTF: " << rtf.ToStdString());
      THEN("the RTF flavour is a 2x2 table of just that block") {
        CHECK(rtf.StartsWith(wxS("{\\rtf")));
        CHECK(Occurrences(rtf, wxS("\\trowd")) == 2);
        CHECK(Occurrences(rtf, wxS("\\cell\n")) == 4);
        CHECK(rtf.Contains(wxS("111")));
        CHECK(rtf.Contains(wxS("122")));
        CHECK_FALSE(rtf.Contains(wxS("100")));
        // ...and no equation object
        CHECK_FALSE(rtf.Contains(wxS("\\mmath")));
      }
      THEN("\"Copy as RTF\" makes the same table") {
        CHECK(g_ws->MatrixEntriesSelected());
        CHECK(Worksheet::SelectionRTFBody(g_ws->CopySelection().get(), true) ==
              g_ws->CopySelectedMatrixBlock()->ToRTFTable());
      }
    }
    WHEN("all of its entries are selected by dragging across them") {
      g_ws->SelectOutputRect(g_ws->GetTree(),
                             matr->GetRect().GetTopLeft() + wxPoint(1, 1),
                             matr->GetRect().GetBottomRight() - wxPoint(1, 1));
      REQUIRE(g_ws->MatrixEntriesSelected());
      THEN("they are copied as a table, too") {
        const auto data = g_ws->CreateSelectionDataObject();
        const wxString rtf = wxString::FromUTF8(ClipboardRtf(*data));
        CHECK(Occurrences(rtf, wxS("\\trowd")) == 3);
      }
    }
    WHEN("the matrix is selected as a whole") {
      g_ws->SetSelection(matr, matr);
      THEN("it is copied as an equation, parentheses and all, as before") {
        CHECK_FALSE(g_ws->MatrixEntriesSelected());
        const auto data = g_ws->CreateSelectionDataObject();
        const wxString rtf = wxString::FromUTF8(ClipboardRtf(*data));
        INFO("RTF: " << rtf.ToStdString());
        CHECK_FALSE(rtf.Contains(wxS("\\trowd")));
        CHECK(rtf.Contains(wxS("\\mmath")));
      }
    }
    g_ws->DestroyTree();
  }

  GIVEN("a matrix with an entry that is maths, not text") {
    MatrCell *matr = ShowMatrix(
      wxS("<tb roundedParens=\"true\"><mtr>"
          "<mtd><mn>1</mn></mtd>"
          "<mtd><f><r><mn>1</mn></r><r><mn>2</mn></r></f></mtd>"
          "</mtr><mtr>"
          "<mtd><mn>3</mn></mtd><mtd><mn>4</mn></mtd>"
          "</mtr></tb>"));
    THEN("it can't be made a table of text") {
      CHECK(matr->ToRTFTable().IsEmpty());
    }
    WHEN("all of its entries are selected and copied") {
      Drag(matr, 0, 0, 1, 1);
      REQUIRE(g_ws->MatrixEntriesSelected());
      THEN("the RTF has it as an equation instead") {
        const auto data = g_ws->CreateSelectionDataObject();
        const wxString rtf = wxString::FromUTF8(ClipboardRtf(*data));
        INFO("RTF: " << rtf.ToStdString());
        CHECK_FALSE(rtf.Contains(wxS("\\trowd")));
        CHECK(rtf.Contains(wxS("\\mmath")));
      }
    }
    g_ws->DestroyTree();
  }

  GIVEN("a matrix with a heading row") {
    MatrCell *matr = ShowMatrix(MatrixTableXml(2, 2, wxS(" colnames=\"true\"")));
    THEN("the heading is bold in the table, the rest isn't") {
      const wxString table = matr->ToRTFTable();
      INFO("RTF: " << table.ToStdString());
      CHECK(Occurrences(table, wxS("\\b ")) == 2);
      CHECK(table.find(wxS("\\b ")) < table.find(wxS("100")));
      CHECK(table.find(wxS("110")) > table.rfind(wxS("\\b0")));
    }
    g_ws->DestroyTree();
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
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  // Whatever the config file says, show matrices in full, so every entry is
  // laid out where the tests expect it.
  g_cfg->SetOversizedMatrices(Configuration::OversizedMatrices::showInFull);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
