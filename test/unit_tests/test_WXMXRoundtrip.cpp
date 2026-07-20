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
  Round-trip tests for the .wxmx content.xml (the MathML-like math format).

  The math cell classes each have a ToXML() and a matching MathParser handler,
  with various parameters and special cases. This checks that parsing a
  content.xml into a cell tree and serialising it back is a fixed point: the
  serialisation of the reparsed tree must equal the first serialisation. If any
  cell class loses or changes information on parse or serialise, the two differ.

  Inputs: the real content.xml corpus (in test/fuzz/corpus_mathparser, exported by
  actual wxMaxima) for realistic coverage, plus a crafted fragment that exercises
  the math constructs the corpus happens not to contain (sqrt, integral, sum,
  matrix, abs, conjugate, sub/superscript) so every ToXML class is covered.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/xml/xml.h>

#include "Configuration.h"
#include "MathParser.h"
#include "worksheet/Worksheet.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
#endif

#ifndef WXM_CORPUS_DIR
#define WXM_CORPUS_DIR "."
#endif

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
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

// Parses a whole content.xml document into a cell tree and serialises the tree
// back to the content.xml cell markup (without the <wxMaximaDocument> wrapper).
static wxString ParseSerialize(const wxString &documentXml) {
  const wxScopedCharBuffer utf8 = documentXml.utf8_str();
  wxMemoryInputStream in(utf8.data(), utf8.length());
  wxXmlDocument doc;
  REQUIRE(doc.Load(in));
  REQUIRE(doc.GetRoot() != nullptr);

  MathParser mp(g_cfg);
  std::unique_ptr<GroupCell> tree = mp.CreateTreeFromXMLNode(doc.GetRoot());
  REQUIRE(tree != nullptr);
  return tree->ListToXML();
}

static wxString WrapDocument(const wxString &cells) {
  return wxS("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<wxMaximaDocument version=\"1.5\" zoom=\"100\">\n") +
         cells + wxS("\n</wxMaximaDocument>\n");
}

// The round-trip fixed-point check: serialising the reparsed tree must equal the
// first serialisation.
static void RequireIdempotent(const wxString &documentXml) {
  const wxString cells1 = ParseSerialize(documentXml);
  const wxString cells2 = ParseSerialize(WrapDocument(cells1));
  REQUIRE(cells1 == cells2);
}

static wxString ReadFile(const wxString &path) {
  wxFFile f(path, wxS("rb"));
  REQUIRE(f.IsOpened());
  wxString contents;
  REQUIRE(f.ReadAll(&contents, wxConvUTF8));
  return contents;
}

SCENARIO("Real content.xml corpus files round-trip to a stable serialization") {
  // Image cells reference bytes stored in the .wxmx zip, which this XML-only test
  // does not provide, so image-free corpus files are used here (image data is
  // covered byte-for-byte by test_WXMRoundtrip).
  const wxChar *files[] = {
    wxS("sampleWorksheet.xml"),
    wxS("intervals.xml"),
    // A rich seed exercising every math cell class plus nesting and the
    // frac-in-exponent context flag (also a fuzzer seed).
    wxS("math-constructs.xml"),
  };
  for (const wxChar *name : files) {
    const wxString path =
      wxFileName(wxString(wxS(WXM_CORPUS_DIR)), name).GetFullPath();
    THEN(wxString::Format(wxS("%s is idempotent"), name).ToStdString()) {
      RequireIdempotent(ReadFile(path));
    }
  }
}

SCENARIO("A folded section's hidden children survive the content.xml round-trip") {
  auto section = std::make_unique<GroupCell>(g_cfg, GC_TYPE_SECTION, wxS("A section"));
  auto child1 = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("1+1;"));
  auto child2 = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, wxS("child text"));

  CellListBuilder<GroupCell> children;
  children.DynamicAppend(child1.release());
  children.DynamicAppend(child2.release());
  REQUIRE(section->HideTree(std::move(children)));

  const wxString xml = section->ToXML();
  THEN("the <fold> survives a parse/reserialize cycle unchanged") {
    RequireIdempotent(WrapDocument(xml));
  }
  THEN("re-parsing it directly recovers the hidden tree structurally") {
    const wxString wrapped = WrapDocument(xml);
    const wxScopedCharBuffer utf8 = wrapped.utf8_str();
    wxMemoryInputStream in(utf8.data(), utf8.length());
    wxXmlDocument doc;
    REQUIRE(doc.Load(in));
    MathParser mp(g_cfg);
    std::unique_ptr<GroupCell> tree = mp.CreateTreeFromXMLNode(doc.GetRoot());
    REQUIRE(tree != nullptr);
    REQUIRE(tree->GetGroupType() == GC_TYPE_SECTION);
    REQUIRE(tree->GetNext() == nullptr); // hidden children are NOT in the main list
    GroupCell *hidden = tree->GetHiddenTree();
    REQUIRE(hidden != nullptr);
    std::vector<GroupCell *> hiddenCells;
    for (auto &h : OnList(hidden))
      hiddenCells.push_back(&h);
    REQUIRE(hiddenCells.size() == 2);
    REQUIRE(hiddenCells[0]->GetGroupType() == GC_TYPE_CODE);
    REQUIRE(hiddenCells[0]->GetEditable()->GetValue() == wxS("1+1;"));
    REQUIRE(hiddenCells[1]->GetGroupType() == GC_TYPE_TEXT);
    REQUIRE(hiddenCells[1]->GetEditable()->GetValue() == wxS("child text"));
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
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
