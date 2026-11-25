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
  Checks the page breaking printing and the PDF export share
  (wxm::BreakIntoPages()), and that printing (Printout) uses it: pages
  start only between lines, a page break cell starts a new page, a heading
  stays with what follows it, and what follows a page break cell is printed
  at all. And how a matrix shown in full that doesn't fit the page is laid
  out for paper (MatrCell, "On paper"): shrunk, split into blocks of
  columns, or continued on the next page between two rows.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/fileconf.h>
#include <wx/frame.h>
#include <wx/filename.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/xml/xml.h>

#include "Configuration.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
#include "graphical_io/PageBreaks.h"
#include "graphical_io/Printout.h"
#include "worksheet/Worksheet.h"

#include <algorithm>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
//! The cells need a worksheet to exist in (for their cell pointers)
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;

//! A worksheet made from the cells' XML
std::unique_ptr<GroupCell> ParseCells(const wxString &cells) {
  const wxString xml = wxS("<wxMaximaDocument version=\"1.5\" zoom=\"100\">\n") +
    cells + wxS("</wxMaximaDocument>\n");
  const wxScopedCharBuffer utf8 = xml.utf8_str();
  wxMemoryInputStream in(utf8.data(), utf8.length());
  wxXmlDocument doc;
  REQUIRE(doc.Load(in));
  MathParser mp(g_cfg);
  std::unique_ptr<GroupCell> tree = mp.CreateTreeFromXMLNode(doc.GetRoot());
  REQUIRE(tree != nullptr);
  return tree;
}

wxString TextGroup(const wxString &type, const wxString &text) {
  return wxS("<cell type=\"") + type + wxS("\"><editor type=\"") + type +
    wxS("\"><line>") + text + wxS("</line></editor></cell>\n");
}

wxString PageBreak() { return wxS("<cell type=\"pagebreak\"></cell>\n"); }

//! Lays a worksheet out as one endless scroll, the way printing does
void Layout(GroupCell *tree) {
  g_cfg->SetRecalcContext(*g_dc);
  g_cfg->LineWidth_em(10000);
  tree->ResetSize();
  for (int pass = 0; pass < 2; pass++) {
    g_cfg->RecalculateForce();
    for (GroupCell &group : OnList(tree))
      group.Recalculate();
  }
  tree->UpdateYPositionList();
}

std::vector<const GroupCell *> Groups(const GroupCell *tree) {
  std::vector<const GroupCell *> groups;
  for (const GroupCell &group : OnList(tree))
    groups.push_back(&group);
  return groups;
}

//! Does the image have anything but white on it?
bool HasInk(const wxImage &image) {
  const unsigned char *data = image.GetData();
  const std::size_t size = static_cast<std::size_t>(image.GetWidth()) *
    static_cast<std::size_t>(image.GetHeight()) * 3;
  return std::any_of(data, data + size, [](unsigned char c) { return c < 200; });
}

//! A rows x cols matrix shown in full, as wx_matrix(..., oversized=full) sends it
wxString FullMatrixXml(std::size_t rows, std::size_t cols) {
  wxString xml = wxS("<mth><lbl altCopy=\"%o1\">(%o1) </lbl>"
                     "<tb roundedParens=\"true\" oversized=\"full\">");
  for (std::size_t r = 0; r < rows; r++) {
    xml += wxS("<mtr>");
    for (std::size_t c = 0; c < cols; c++)
      xml += wxString::Format(wxS("<mtd><mn>%lu</mn></mtd>"),
                              static_cast<unsigned long>(r * 100 + c));
    xml += wxS("</mtr>");
  }
  return xml + wxS("</tb></mth>");
}

//! A code cell whose output is this XML
std::unique_ptr<GroupCell> GroupWithOutput(const wxString &xml) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("m;"));
  MathParser parser(g_cfg);
  auto output = parser.ParseLine(xml);
  REQUIRE(output != nullptr);
  group->AppendOutput(std::move(output));
  return group;
}

MatrCell *TheMatrix(GroupCell *group) {
  for (Cell &cell : OnDrawList(group->GetOutput()))
    if (auto *matr = dynamic_cast<MatrCell *>(&cell))
      return matr;
  FAIL("The output holds no matrix");
  return nullptr;
}

/*! Lays the worksheet out as Printout does, for a page of this size

  The shared configuration is back to the screen's afterwards.
*/
class OnPaper {
public:
  explicit OnPaper(wxSize page) : m_printing(g_cfg), m_oldCanvas(g_cfg->GetCanvasSize()) {
    g_cfg->SetCanvasSize(page);
  }
  ~OnPaper() { g_cfg->SetCanvasSize(m_oldCanvas); }
private:
  Printing m_printing;
  wxSize m_oldCanvas;
};
} // namespace

SCENARIO("A worksheet that fits onto one page is one page") {
  auto tree = ParseCells(TextGroup(wxS("text"), wxS("One")) +
                         TextGroup(wxS("text"), wxS("Two")));
  Layout(tree.get());
  const auto pages = wxm::BreakIntoPages(tree.get(), 100000);
  REQUIRE(pages.size() == 1);
  CHECK(pages.front() <= tree->GetRect().GetTop());
}

SCENARIO("An empty worksheet has no pages") {
  CHECK(wxm::BreakIntoPages(nullptr, 1000).empty());
}

SCENARIO("A page break cell starts a new page") {
  auto tree = ParseCells(TextGroup(wxS("text"), wxS("Before")) + PageBreak() +
                         TextGroup(wxS("text"), wxS("After")));
  Layout(tree.get());
  const auto groups = Groups(tree.get());
  REQUIRE(groups.size() == 3);
  const auto pages = wxm::BreakIntoPages(tree.get(), 100000);
  REQUIRE(pages.size() == 2);
  CHECK(pages.at(1) == groups.at(2)->GetRect().GetTop());
}

SCENARIO("Pages start between groups and are no taller than the paper") {
  wxString cells;
  for (int i = 0; i < 40; i++)
    cells += TextGroup(wxS("text"), wxString::Format(wxS("Paragraph %i"), i));
  auto tree = ParseCells(cells);
  Layout(tree.get());
  const auto groups = Groups(tree.get());
  const wxCoord pageHeight = 5 * (groups.at(1)->GetRect().GetTop() -
                                  groups.at(0)->GetRect().GetTop());
  const auto pages = wxm::BreakIntoPages(tree.get(), pageHeight);
  CHECK(pages.size() > 5);
  for (std::size_t i = 1; i < pages.size(); i++) {
    // Each page starts at the top of a group, so no line is cut in two ...
    CHECK(std::any_of(groups.begin(), groups.end(), [&](const GroupCell *g) {
      return g->GetRect().GetTop() == pages.at(i);
    }));
    // ... and every group on it ends on it.
    for (const GroupCell *group : groups) {
      const wxRect rect = group->GetRect();
      if ((rect.GetTop() >= pages.at(i - 1)) && (rect.GetTop() < pages.at(i)))
        CHECK(rect.GetBottom() - pages.at(i - 1) <= pageHeight);
    }
  }
}

SCENARIO("A heading is not left alone at the bottom of a page") {
  auto tree = ParseCells(TextGroup(wxS("text"), wxS("First")) +
                         TextGroup(wxS("section"), wxS("Heading")) +
                         TextGroup(wxS("text"), wxS("Below the heading")));
  Layout(tree.get());
  const auto groups = Groups(tree.get());
  REQUIRE(groups.size() == 3);
  // Room for the first paragraph and the heading, but not for what follows.
  const auto first = wxm::BreakIntoPages(tree.get(), 100000);
  const wxCoord pageHeight = groups.at(1)->GetRect().GetBottom() - first.front() + 1;
  REQUIRE(groups.at(2)->GetRect().GetBottom() - first.front() > pageHeight);
  const auto pages = wxm::BreakIntoPages(tree.get(), pageHeight);
  REQUIRE(pages.size() == 2);
  CHECK(pages.at(1) == groups.at(1)->GetRect().GetTop());
}

SCENARIO("Printing prints what follows a page break") {
  // Printing used to stop drawing a page at a page break cell without
  // starting a new page there, unless the page was full anyway: what
  // followed the page break was never printed.
  auto tree = ParseCells(TextGroup(wxS("text"), wxS("Before")) + PageBreak() +
                         TextGroup(wxS("text"), wxS("After")));
  Printout printout(wxS("test"), tree.get(), 1.0);
  wxBitmap paper(600, 800);
  wxMemoryDC dc(paper);
  printout.SetDC(&dc);
  printout.SetPageSizePixels(600, 800);
  printout.OnPreparePrinting();
  int minPage = 0, maxPage = 0, fromPage = 0, toPage = 0;
  printout.GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);
  REQUIRE(maxPage == 2);
  CHECK(printout.HasPage(2));
  CHECK(!printout.HasPage(3));
  for (int page = 1; page <= maxPage; page++) {
    CHECK(printout.OnPrintPage(page));
    dc.SelectObject(wxNullBitmap);
    CHECK(HasInk(paper.ConvertToImage()));
    dc.SelectObject(paper);
  }
}

SCENARIO("A matrix taller than a page is continued between its rows") {
  const wxSize page(800, 300);
  OnPaper paper(page);
  auto group = GroupWithOutput(FullMatrixXml(40, 3));
  Layout(group.get());
  MatrCell *matr = TheMatrix(group.get());
  REQUIRE(matr->GetHeight() > page.y);

  THEN("it is laid out for paper, but not shrunk or split") {
    CHECK(matr->GetPaperColumnBlocks() == 1);
    CHECK(matr->GetPaperScale() == 1);
    CHECK(matr->GetPaperBlockCaption(0).IsEmpty());
  }
  THEN("it lines up with its label by its first row, not by its middle") {
    CHECK(matr->GetCenter() < matr->GetHeight() / 10);
    REQUIRE(group->GetLabel() != nullptr);
    CHECK(!matr->BreakLineHere());
    CHECK(group->GetLabel()->GetCurrentPoint().y ==
          matr->GetInnerCell(0, 0)->GetCurrentPoint().y);
  }
  THEN("a page may start between any two of its rows") {
    const auto candidates = matr->PageBreakCandidates();
    CHECK(candidates.size() == 39);
    CHECK(std::is_sorted(candidates.begin(), candidates.end()));
  }
  THEN("its pages start between rows, and none is taller than the paper") {
    const auto candidates = matr->PageBreakCandidates();
    const auto pages = wxm::BreakIntoPages(group.get(), page.y);
    REQUIRE(pages.size() > 2);
    for (std::size_t i = 1; i < pages.size(); i++) {
      CHECK(std::find(candidates.begin(), candidates.end(), pages.at(i)) !=
            candidates.end());
      CHECK(pages.at(i) - pages.at(i - 1) <= page.y);
    }
    CHECK(matr->GetRect().GetBottom() - pages.back() <= page.y);
    // The matrix starts on the page its group starts on: the page is
    // filled rather than left empty below the input.
    CHECK(pages.at(1) > matr->GetRect().GetTop());
  }
}

SCENARIO("A matrix that fits a fresh page is moved there whole") {
  // Only a matrix that can't fit any page is cut.
  const wxSize page(800, 600);
  OnPaper paper(page);
  wxString cells;
  for (int i = 0; i < 15; i++)
    cells += TextGroup(wxS("text"), wxString::Format(wxS("Paragraph %i"), i));
  auto tree = ParseCells(cells);
  auto matrixGroup = GroupWithOutput(FullMatrixXml(8, 3));
  MatrCell *matr = TheMatrix(matrixGroup.get());
  CellList::AppendCell(tree.get(), std::move(matrixGroup));
  Layout(tree.get());
  CHECK(matr->GetPaperColumnBlocks() == 0);
  CHECK(matr->PageBreakCandidates().empty());
  const auto pages = wxm::BreakIntoPages(tree.get(), page.y);
  for (const wxCoord start : pages)
    CHECK(!((start > matr->GetRect().GetTop()) && (start <= matr->GetRect().GetBottom())));
}

SCENARIO("A matrix a bit too wide for the page is shrunk to fit it") {
  // First find out how wide the matrix is at full size.
  wxCoord fullWidth = 0;
  {
    OnPaper paper(wxSize(100000, 100000));
    auto group = GroupWithOutput(FullMatrixXml(3, 12));
    Layout(group.get());
    fullWidth = TheMatrix(group.get())->GetWidth();
  }
  // A page on which it needs to shrink to about 3/4 of its size.
  // (The label column takes about 150 of the page.)
  const wxSize page(fullWidth * 3 / 4 + 200, 600);
  OnPaper paper(page);
  auto group = GroupWithOutput(FullMatrixXml(3, 12));
  Layout(group.get());
  MatrCell *matr = TheMatrix(group.get());
  CHECK(matr->GetPaperScale() < 0.95);
  CHECK(matr->GetPaperScale() >= MatrCell::MinPaperScale);
  CHECK(matr->GetWidth() < fullWidth);
  CHECK(group->GetOutputRect().GetRight() <= page.x);
  // It stays on its label's line.
  CHECK(!matr->BreakLineHere());
  // It isn't split, and it is short enough not to need pages of its own.
  CHECK(matr->GetPaperColumnBlocks() == 0);
  CHECK(matr->ElidedColumns() == 0);
}

SCENARIO("A matrix far too wide for the page is split into blocks of columns") {
  const wxSize page(600, 100000);
  OnPaper paper(page);
  const std::size_t cols = 60;
  auto group = GroupWithOutput(FullMatrixXml(3, cols));
  Layout(group.get());
  MatrCell *matr = TheMatrix(group.get());
  const std::size_t blocks = matr->GetPaperColumnBlocks();

  THEN("it is split, at full size, and fits the page") {
    CHECK(blocks > 2);
    CHECK(matr->GetPaperScale() == 1);
    CHECK(group->GetOutputRect().GetRight() <= page.x);
    CHECK(matr->ElidedColumns() == 0);
  }
  THEN("each block is headed by its columns, and together they are all of them") {
    unsigned long next = 1;
    for (std::size_t k = 0; k < blocks; k++) {
      unsigned long first = 0, last = 0;
      const wxString caption = matr->GetPaperBlockCaption(k);
      REQUIRE(caption.StartsWith(wxS("Columns ")));
      CHECK(caption.AfterFirst(wxS(' ')).BeforeFirst(wxS(' ')).ToULong(&first));
      CHECK(caption.AfterLast(wxS(' ')).ToULong(&last));
      CHECK(first == next);
      CHECK(last >= first);
      next = last + 1;
    }
    CHECK(next == cols + 1);
  }
  THEN("a page may start before each block but the first, and between rows") {
    CHECK(matr->PageBreakCandidates().size() == blocks - 1 + blocks * 2);
  }
  THEN("it lines up with its label by its first row") {
    REQUIRE(group->GetLabel() != nullptr);
    CHECK(!matr->BreakLineHere());
    CHECK(group->GetLabel()->GetCurrentPoint().y ==
          matr->GetInnerCell(0, 0)->GetCurrentPoint().y);
  }
  THEN("the matrix still exports whole") {
    for (const wxString entry : {wxS("0"), wxS("59"), wxS("259")})
      CHECK(matr->ToString().Contains(entry));
  }
}

SCENARIO("A block of columns is printed on one page with its heading") {
  // A page that holds about one block: each block starts a page, heading
  // and all.
  OnPaper probe(wxSize(600, 100000));
  auto group = GroupWithOutput(FullMatrixXml(3, 60));
  Layout(group.get());
  MatrCell *matr = TheMatrix(group.get());
  const std::size_t blocks = matr->GetPaperColumnBlocks();
  REQUIRE(blocks > 2);
  const wxCoord blockHeight = matr->GetHeight() / static_cast<wxCoord>(blocks);
  const auto candidates = matr->PageBreakCandidates();
  const auto pages = wxm::BreakIntoPages(group.get(), blockHeight + blockHeight / 4);
  CHECK(pages.size() >= blocks);
  for (std::size_t i = 1; i < pages.size(); i++)
    CHECK(std::find(candidates.begin(), candidates.end(), pages.at(i)) !=
          candidates.end());
}

SCENARIO("Paper doesn't change how a matrix is shown on screen or elided") {
  GIVEN("the screen") {
    g_cfg->SetCanvasSize(wxSize(600, 300));
    auto group = GroupWithOutput(FullMatrixXml(40, 60));
    Layout(group.get());
    MatrCell *matr = TheMatrix(group.get());
    THEN("a matrix shown in full is shown whole, as before") {
      CHECK(matr->GetPaperColumnBlocks() == 0);
      CHECK(matr->GetPaperScale() == 1);
      CHECK(matr->GetWidth() > 600);
      CHECK(matr->PageBreakCandidates().empty());
    }
    g_cfg->SetCanvasSize(wxSize(800, 600));
  }
  GIVEN("paper and a matrix that asked to be elided") {
    OnPaper paper(wxSize(600, 300));
    wxString xml = FullMatrixXml(40, 60);
    xml.Replace(wxS("oversized=\"full\""), wxS("oversized=\"elide\""));
    auto group = GroupWithOutput(xml);
    Layout(group.get());
    MatrCell *matr = TheMatrix(group.get());
    THEN("it is elided, as before") {
      CHECK(matr->ElidedColumns() > 0);
      CHECK(matr->ElidedRows() > 0);
      CHECK(matr->GetPaperColumnBlocks() == 0);
      CHECK(matr->PageBreakCandidates().empty());
    }
  }
}

SCENARIO("Printing a matrix taller than a page prints every page of it") {
  auto group = GroupWithOutput(FullMatrixXml(80, 3));
  Printout printout(wxS("test"), group.get(), 1.0);
  wxBitmap paper(600, 400);
  wxMemoryDC dc(paper);
  printout.SetDC(&dc);
  printout.SetPageSizePixels(600, 400);
  printout.OnPreparePrinting();
  int minPage = 0, maxPage = 0, fromPage = 0, toPage = 0;
  printout.GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);
  CHECK(maxPage > 2);
  for (int pageNum = 1; pageNum <= maxPage; pageNum++) {
    CHECK(printout.OnPrintPage(pageNum));
    dc.SelectObject(wxNullBitmap);
    CHECK(HasInk(paper.ConvertToImage()));
    dc.SelectObject(paper);
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
  wxInitAllImageHandlers();

  // Hermetic settings: the styles come from the global wxConfig.
  const wxString cfgFile = wxFileName::CreateTempFileName(wxS("wxmpagecfg"));
  wxConfig::Set(new wxFileConfig(wxS("wxMaxima"), wxEmptyString, cfgFile));

  g_bmp = new wxBitmap(1000, 1000);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxRemoveFile(cfgFile);
  wxEntryCleanup();
  return result;
}
