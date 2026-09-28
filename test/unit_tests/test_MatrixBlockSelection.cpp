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
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
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
