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
  Reaching a cell's output with the keyboard (GH #2382).

  Down at the end of a cell's input used to leave the cell, so its output
  could only be selected with the mouse. Now Down steps into the output, one
  result at a time, and Up walks back. These tests send the key events to the
  worksheet's own key handler and check what ends up selected. The output is
  parsed from the XML Maxima sends, so no Maxima is needed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/MatrCell.h"
#include "cells/GroupCell.h"
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

// An output with a label and an expression, as Maxima sends it
static wxString Result(int n, const wxString &math) {
  return wxString::Format(wxS("<mth><lbl altCopy=\"%%o%d\">(%%o%d) </lbl>"), n, n) +
    math + wxS("</mth>");
}

// Puts a code cell with this input and these outputs into the (otherwise
// empty) worksheet, followed by an empty code cell, and lays them out.
static GroupCell *ShowCell(const wxString &input,
                           const std::vector<wxString> &outputs) {
  g_ws->DestroyTree();
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, input);
  MathParser parser(g_cfg);
  parser.SetGroup(group.get());
  for (const auto &output : outputs)
    group->AppendOutput(parser.ParseLine(output));
  g_ws->InsertGroupCells(std::move(group), nullptr, nullptr);
  g_ws->InsertGroupCells(std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("b;")),
                         g_ws->GetTree(), nullptr);
  g_ws->RecalculateIfNeeded();
  REQUIRE(g_ws->GetTree() != nullptr);
  return g_ws->GetTree();
}

// Sends a key to the worksheet as if it had been typed
static void Press(int keyCode) {
  wxKeyEvent event(wxEVT_CHAR);
  event.m_keyCode = keyCode;
  g_ws->OnChar(event);
}

// Sends Shift and a key to the worksheet
static void PressShifted(int keyCode) {
  wxKeyEvent event(wxEVT_CHAR);
  event.m_keyCode = keyCode;
  event.m_shiftDown = true;
  g_ws->OnChar(event);
}

// Puts the text cursor at the end of the cell's input
static void CursorToEndOfInput(GroupCell *group) {
  g_ws->SetHCaret(nullptr);
  g_ws->SetActiveCell(group->GetEditable());
  group->GetEditable()->CaretToEnd();
}

static const DocumentCellPointers &Pointers() {
  return g_ws->GetDocumentCellPointers();
}

SCENARIO("A cell's output is split into results") {
  GIVEN("a cell with two results") {
    GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                              Result(2, wxS("<mi>b</mi>"))});
    const auto results = group->GetOutputResults();
    THEN("each result is a label and its expression") {
      REQUIRE(results.size() == 2);
      CHECK(results[0].first->ToString().Contains(wxS("%o1")));
      CHECK(results[0].last->ToString().Trim() == wxS("a"));
      CHECK(results[1].first->ToString().Contains(wxS("%o2")));
      CHECK(results[1].last->ToString().Trim() == wxS("b"));
    }
    AND_WHEN("the output is hidden") {
      group->Hide(true);
      THEN("there is nothing to step into") {
        CHECK(group->GetOutputResults().empty());
      }
      group->Hide(false);
    }
  }
  GIVEN("a cell without output") {
    GroupCell *group = ShowCell(wxS("a$"), {});
    THEN("it has no results") {
      CHECK(group->GetOutputResults().empty());
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Up and Down walk through a cell's output (GH #2382)") {
  GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                            Result(2, wxS("<mi>b</mi>"))});
  const auto results = group->GetOutputResults();
  REQUIRE(results.size() == 2);

  WHEN("Down is pressed at the end of the cell's input") {
    CursorToEndOfInput(group);
    Press(WXK_DOWN);
    THEN("the first result is selected and the input is left") {
      CHECK(g_ws->GetActiveCell() == nullptr);
      CHECK(Pointers().GetSelectionStart() == results[0].first);
      CHECK(Pointers().GetSelectionEnd() == results[0].last);
      CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
      CHECK(g_ws->GetString().Contains(wxS("a")));
    }
    AND_WHEN("Down is pressed again") {
      Press(WXK_DOWN);
      THEN("the second result is selected") {
        CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(1));
        CHECK(Pointers().GetSelectionStart() == results[1].first);
      }
      AND_WHEN("Down is pressed past the last result") {
        Press(WXK_DOWN);
        THEN("the horizontal cursor sits below the cell, as before") {
          CHECK_FALSE(Pointers().GetSelectionStart());
          CHECK(g_ws->HCaretActive());
          CHECK(g_ws->GetHCaret() == group);
        }
        AND_WHEN("Up is pressed there") {
          Press(WXK_UP);
          THEN("the last result is selected again") {
            CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(1));
          }
        }
      }
      AND_WHEN("Up is pressed twice") {
        Press(WXK_UP);
        THEN("the first press goes back to the first result") {
          CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
        }
        Press(WXK_UP);
        THEN("the second one goes back into the input, at its end") {
          CHECK_FALSE(Pointers().GetSelectionStart());
          CHECK(g_ws->GetActiveCell() == group->GetEditable());
          CHECK(group->GetEditable()->CaretAtEnd());
        }
      }
    }
  }

  WHEN("Down is pressed in the input, but not at its end") {
    CursorToEndOfInput(group);
    group->GetEditable()->CaretToStart();
    Press(WXK_DOWN);
    THEN("the cursor stays in the input") {
      CHECK(g_ws->GetActiveCell() == group->GetEditable());
      CHECK_FALSE(Pointers().GetSelectionStart());
    }
  }

  WHEN("a part of a result was selected with the mouse") {
    // A mouse selection leaves neither cursor active: selecting a whole
    // result gets there, then narrowing it makes it only part of one.
    g_ws->SelectOutputResult(group, 0);
    g_ws->SetSelection(results[0].last);
    REQUIRE_FALSE(g_ws->SelectedOutputResult());
    Press(WXK_DOWN);
    THEN("Down leaves the cell, as it always did") {
      CHECK(g_ws->HCaretActive());
      CHECK(g_ws->GetHCaret() == group);
    }
  }

  g_ws->DestroyTree();
}

SCENARIO("Down leaves a cell that has no output, as before") {
  GroupCell *group = ShowCell(wxS("a$"), {});
  CursorToEndOfInput(group);
  Press(WXK_DOWN);
  THEN("the horizontal cursor sits below the cell") {
    CHECK_FALSE(Pointers().GetSelectionStart());
    CHECK(g_ws->HCaretActive());
    CHECK(g_ws->GetHCaret() == group);
  }
  AND_WHEN("Up is pressed there") {
    Press(WXK_UP);
    THEN("the cursor goes back into the input") {
      CHECK(g_ws->GetActiveCell() == group->GetEditable());
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Up and Down can be told to skip the output, as they used to") {
  GroupCell *group = ShowCell(wxS("a;b;"), {Result(1, wxS("<mi>a</mi>")),
                                            Result(2, wxS("<mi>b</mi>"))});
  g_cfg->ArrowKeysSkipOutput(true);

  WHEN("Down is pressed at the end of the cell's input") {
    CursorToEndOfInput(group);
    Press(WXK_DOWN);
    THEN("the output is skipped and the horizontal cursor sits below the cell") {
      CHECK_FALSE(Pointers().GetSelectionStart());
      CHECK(g_ws->HCaretActive());
      CHECK(g_ws->GetHCaret() == group);
    }
    AND_WHEN("Up is pressed there") {
      Press(WXK_UP);
      THEN("the cursor goes back into the input, not into the output") {
        CHECK_FALSE(Pointers().GetSelectionStart());
        CHECK(g_ws->GetActiveCell() == group->GetEditable());
      }
    }
  }

  g_cfg->ArrowKeysSkipOutput(false);
  g_ws->DestroyTree();
}

SCENARIO("By default Up and Down don't skip the output") {
  // Temporary, so it doesn't write its settings back to the config file
  Configuration cfg(g_dc, Configuration::temporary);
  cfg.ArrowKeysSkipOutput(true);
  cfg.ResetAllToDefaults();
  CHECK_FALSE(cfg.ArrowKeysSkipOutput());
}

// The text of the selection, without the spaces between the cells
static wxString Selected() {
  wxString text = g_ws->GetString();
  text.Replace(wxS(" "), wxEmptyString);
  return text;
}

SCENARIO("Enter and Escape go into an expression and back out (GH #2382)") {
  GroupCell *group = ShowCell(
    wxS("(a+b)/c;"),
    {Result(1, wxS("<f><r><mi>a</mi><mo>+</mo><mi>b</mi></r><r><mi>c</mi></r></f>"))});
  const auto results = group->GetOutputResults();
  REQUIRE(results.size() == 1);

  GIVEN("the result is selected") {
    g_ws->SelectOutputResult(group, 0);
    Cell *fraction = results[0].last;

    WHEN("Enter is pressed") {
      Press(WXK_RETURN);
      THEN("the fraction, without its label, is selected") {
        CHECK(Pointers().GetSelectionStart() == fraction);
        CHECK(Pointers().GetSelectionEnd() == fraction);
        CHECK(g_ws->GetActiveCell() == nullptr);
      }
      AND_WHEN("Enter is pressed again") {
        Press(WXK_RETURN);
        THEN("the numerator is selected") {
          CHECK(Selected() == wxS("a+b"));
        }
        AND_WHEN("Right is pressed") {
          Press(WXK_RIGHT);
          THEN("the denominator is selected, not the fraction bar") {
            CHECK(Selected() == wxS("c"));
          }
          AND_WHEN("Right is pressed at the last part") {
            Press(WXK_RIGHT);
            THEN("the selection stays") {
              CHECK(Selected() == wxS("c"));
            }
          }
          AND_WHEN("Left is pressed") {
            Press(WXK_LEFT);
            THEN("the numerator is selected again") {
              CHECK(Selected() == wxS("a+b"));
            }
          }
          AND_WHEN("Shift+Left is pressed") {
            PressShifted(WXK_LEFT);
            THEN("the selection doesn't grow, as numerator and denominator "
                 "are no run of cells") {
              CHECK(Selected() == wxS("c"));
            }
          }
        }
        AND_WHEN("Enter goes into the numerator, and Escape comes back out step by step") {
          Press(WXK_RETURN);
          CHECK(Selected() == wxS("a"));
          Press(WXK_RIGHT);
          CHECK(Selected() == wxS("+"));
          Press(WXK_ESCAPE);
          CHECK(Selected() == wxS("a+b"));
          Press(WXK_ESCAPE);
          CHECK(Pointers().GetSelectionStart() == fraction);
          Press(WXK_ESCAPE);
          CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
          Press(WXK_ESCAPE);
          THEN("the last Escape goes back into the input") {
            CHECK_FALSE(Pointers().GetSelectionStart());
            CHECK(g_ws->GetActiveCell() == group->GetEditable());
          }
        }
      }
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("The parts of an expression leave out linear-form glyphs") {
  GroupCell *group = ShowCell(
    wxS("sin(x)/c;"),
    {Result(1, wxS("<f><r><fn><r><fnm>sin</fnm></r><r><p><mi>x</mi></p></r></fn></r>"
                   "<r><mi>c</mi></r></f>"))});
  const auto results = OutputNavigation::Results(group);
  REQUIRE(results.size() == 1);
  const auto fraction = OutputNavigation::Children(results[0]);
  REQUIRE(fraction.size() == 1);

  THEN("a fraction has two parts, and no \"/\"") {
    const auto parts = OutputNavigation::Children(fraction[0]);
    REQUIRE(parts.size() == 2);
    CHECK(parts[1].first->ToString() == wxS("c"));
    AND_THEN("a function's parts are its name and its argument, without brackets") {
      const auto function = OutputNavigation::Children(parts[0]);
      REQUIRE(function.size() == 2);
      CHECK(function[0].first->ToString() == wxS("sin"));
      const auto argument = OutputNavigation::Children(function[1]);
      REQUIRE(argument.size() == 1);
      CHECK(argument[0].first->ToString() == wxS("x"));
      CHECK(OutputNavigation::Children(argument[0]).empty());
    }
    AND_THEN("every part can be found again, with its path") {
      const auto location = OutputNavigation::Locate(group, parts[1]);
      REQUIRE(location.found);
      CHECK(location.path.size() == 3);
      CHECK(location.index == 1);
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Enter on a part that has no parts keeps its old meaning") {
  GroupCell *group = ShowCell(wxS("x;"), {Result(1, wxS("<mi>x</mi>"))});
  g_ws->SelectOutputResult(group, 0);
  Press(WXK_RETURN);
  REQUIRE(Selected() == wxS("x"));
  WHEN("Enter is pressed on the x") {
    Press(WXK_RETURN);
    THEN("a new input cell holding it opens, as before") {
      REQUIRE(g_ws->GetActiveCell() != nullptr);
      CHECK(g_ws->GetActiveCell()->GetValue() == wxS("x"));
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("Shift+Left/Right grow and shrink the selection over neighbours") {
  GroupCell *group = ShowCell(
    wxS("a+b+c;"),
    {Result(1, wxS("<mi>a</mi><mo>+</mo><mi>b</mi><mo>+</mo><mi>c</mi>"))});
  g_ws->SelectOutputResult(group, 0);
  Press(WXK_RETURN);
  REQUIRE(Selected() == wxS("a"));

  WHEN("Shift+Right is pressed twice") {
    PressShifted(WXK_RIGHT);
    CHECK(Selected() == wxS("a+"));
    PressShifted(WXK_RIGHT);
    THEN("the selection covers a+b") {
      CHECK(Selected() == wxS("a+b"));
    }
    AND_WHEN("Shift+Left is pressed") {
      PressShifted(WXK_LEFT);
      THEN("it shrinks again from the moving end") {
        CHECK(Selected() == wxS("a+"));
      }
    }
    AND_WHEN("Escape is pressed") {
      Press(WXK_ESCAPE);
      THEN("the whole result is selected") {
        CHECK(g_ws->SelectedOutputResult() == std::optional<std::size_t>(0));
      }
    }
    AND_WHEN("Right is pressed") {
      Press(WXK_RIGHT);
      THEN("the part after the run is selected") {
        CHECK(Selected() == wxS("+"));
      }
    }
  }
  WHEN("the selection starts at the end and grows to the left") {
    for (int i = 0; i < 4; i++)
      Press(WXK_RIGHT);
    REQUIRE(Selected() == wxS("c"));
    PressShifted(WXK_LEFT);
    PressShifted(WXK_LEFT);
    CHECK(Selected() == wxS("b+c"));
    PressShifted(WXK_RIGHT);
    THEN("Shift+Right moves the left end, the c stays") {
      CHECK(Selected() == wxS("+c"));
    }
  }
  g_ws->DestroyTree();
}

SCENARIO("In a matrix the arrow keys move from entry to entry") {
  GroupCell *group = ShowCell(
    wxS("matrix([1,2],[3,a/b]);"),
    {Result(1, wxS("<tb><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr>"
                   "<mtr><mtd><mn>3</mn></mtd><mtd><f><r><mi>a</mi></r><r><mi>b</mi></r></f></mtd></mtr></tb>"))});
  const auto results = group->GetOutputResults();
  REQUIRE(results.size() == 1);
  auto *matrix = dynamic_cast<MatrCell *>(results[0].last);
  REQUIRE(matrix != nullptr);
  g_ws->SelectOutputResult(group, 0);
  Press(WXK_RETURN);
  REQUIRE(Pointers().GetSelectionStart() == matrix);

  auto entry = [](std::size_t row, std::size_t col) {
    const auto item = g_ws->SelectedOutputItem();
    return item && item->IsMatrixEntry() && (item->entry.row == row) &&
      (item->entry.col == col);
  };

  WHEN("Enter is pressed on the matrix") {
    Press(WXK_RETURN);
    THEN("its first entry is selected, as a one-entry block") {
      CHECK(entry(0, 0));
      CHECK(Pointers().GetSelectedMatrixBlock().has_value());
    }
    AND_WHEN("the arrow keys go round the matrix") {
      Press(WXK_RIGHT);
      CHECK(entry(0, 1));
      Press(WXK_DOWN);
      CHECK(entry(1, 1));
      Press(WXK_LEFT);
      CHECK(entry(1, 0));
      Press(WXK_UP);
      CHECK(entry(0, 0));
      Press(WXK_UP);
      THEN("at the edge the entry stays where it is") {
        CHECK(entry(0, 0));
      }
    }
    AND_WHEN("Shift+Right is pressed") {
      PressShifted(WXK_RIGHT);
      THEN("the entry grows into a block, as a block dragged with the mouse would") {
        const auto block = Pointers().GetSelectedMatrixBlock();
        REQUIRE(block.has_value());
        CHECK(block->firstCol == 0);
        CHECK(block->lastCol == 1);
      }
    }
    AND_WHEN("Enter goes into the entry holding a fraction") {
      Press(WXK_RIGHT);
      Press(WXK_DOWN);
      Press(WXK_RETURN);
      THEN("its numerator is selected straight away") {
        CHECK(Selected() == wxS("a"));
      }
      AND_WHEN("Escape is pressed") {
        Press(WXK_ESCAPE);
        THEN("the entry is selected again") {
          CHECK(entry(1, 1));
        }
        AND_WHEN("Escape is pressed again") {
          Press(WXK_ESCAPE);
          THEN("the whole matrix is selected") {
            CHECK(Pointers().GetSelectionStart() == matrix);
            CHECK_FALSE(Pointers().GetSelectedMatrixBlockCorners().has_value());
          }
        }
      }
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
  g_cfg->SetCanvasSize(wxSize(1000, 1000));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition,
                       wxDefaultSize, /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);
  // Whatever the config file says, show code cells, so the input is reachable
  g_cfg->ShowCodeCells(true);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
