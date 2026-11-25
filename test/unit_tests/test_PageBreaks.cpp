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
  at all.
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
