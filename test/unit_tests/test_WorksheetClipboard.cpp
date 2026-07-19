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
  Safety net for the multi-format clipboard / drag-and-drop payload of Worksheet.

  When wxMaxima copies (or cuts, or drag-exports) a selection it advertises the
  same content under many flavors at once - the .wxm batch code, MathML (under
  two mime types), RTF (under two mime types), plain text, and a bitmap. Those
  live inside one wxDataObjectComposite / CompositeDataObject. A composite whose
  children collide on a wxDataFormat, or whose children carry no retrievable
  data, is a latent clipboard bug (and trips a wxWidgets assertion when it is
  handed to the real clipboard).

  Worksheet::CreateSelectionDataObject() and Worksheet::CreateCellsDataObject()
  are the exact builders Copy()/CopyCells() use, factored out so the composite
  can be inspected without opening the (headless-unfriendly) system clipboard.
  This test builds them over a real document and pins the invariants:

  - every advertised format is distinct (no two children share a wxDataFormat),
  - every advertised format round-trips (GetDataSize > 0 and GetDataHere fills a
    buffer), and
  - the flavors that the enabled Copy* settings ask for are actually present,
    with a sensible "preferred" flavor.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dataobj.h>
#include <wx/dcmemory.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/xml/xml.h>

#include "Configuration.h"
#include "MathParser.h"
#include "Worksheet.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#include <string>
#include <vector>
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

static wxString ReadTextFile(const wxString &path) {
  wxFFile f(path, wxS("rb"));
  REQUIRE(f.IsOpened());
  wxString contents;
  REQUIRE(f.ReadAll(&contents, wxConvUTF8));
  return contents;
}

static std::unique_ptr<GroupCell> ParseCorpusFile(const wxString &name) {
  const wxString path =
    wxFileName(wxString(wxS(WXM_CORPUS_DIR)), name).GetFullPath();
  const wxString xml = ReadTextFile(path);
  const wxScopedCharBuffer utf8 = xml.utf8_str();
  wxMemoryInputStream in(utf8.data(), utf8.length());
  wxXmlDocument doc;
  REQUIRE(doc.Load(in));
  REQUIRE(doc.GetRoot() != nullptr);
  MathParser mp(g_cfg);
  std::unique_ptr<GroupCell> tree = mp.CreateTreeFromXMLNode(doc.GetRoot());
  REQUIRE(tree != nullptr);
  return tree;
}

//! Fills the worksheet with the real math corpus plus a couple of text cells.
static void BuildDocumentOnce() {
  if (g_ws->GetTree())
    return;

  g_ws->InsertGroupCells(ParseCorpusFile(wxS("sampleWorksheet.xml")), nullptr);
  g_ws->InsertGroupCells(ParseCorpusFile(wxS("math-constructs.xml")),
                         g_ws->GetLastCellInWorksheet());

  auto appendCell = [](GroupType type, const wxChar *text) {
    g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, type, wxString(text)),
      g_ws->GetLastCellInWorksheet());
  };
  appendCell(GC_TYPE_TITLE, wxS("ClipboardNetTitle"));
  appendCell(GC_TYPE_CODE, wxS("factor(xclip^2-1);"));

  g_ws->RecalculateIfNeeded();
}

//! The formats a data object advertises for the "Get" (paste-out) direction.
static std::vector<wxDataFormat> GetFormats(const wxDataObject &obj) {
  const size_t n = obj.GetFormatCount(wxDataObject::Get);
  std::vector<wxDataFormat> fmts(n);
  if (n)
    obj.GetAllFormats(fmts.data(), wxDataObject::Get);
  return fmts;
}

static bool HasFormat(const std::vector<wxDataFormat> &fmts,
                      const wxDataFormat &wanted) {
  for (const auto &f : fmts)
    if (f == wanted)
      return true;
  return false;
}

/*! The core invariants every clipboard composite we build must satisfy.

  No two children may share a wxDataFormat (the collision that trips the
  clipboard assertion), and each advertised format must carry retrievable data.
*/
static void RequireDistinctAndRetrievable(const wxDataObject &obj) {
  const std::vector<wxDataFormat> fmts = GetFormats(obj);
  REQUIRE(fmts.size() >= 1);

  for (size_t i = 0; i < fmts.size(); ++i) {
    // Distinct: no earlier child advertised the same format.
    for (size_t j = 0; j < i; ++j) {
      INFO("duplicate clipboard format at indices " << j << " and " << i);
      REQUIRE_FALSE(fmts[i] == fmts[j]);
    }
    // Retrievable: the format actually has data behind it.
    INFO("clipboard format index " << i << " carries no data");
    const size_t size = obj.GetDataSize(fmts[i]);
    REQUIRE(size > 0);
    std::vector<char> buf(size);
    REQUIRE(obj.GetDataHere(fmts[i], buf.data()));
  }
}

static const wxDataFormat kWxmFormat{wxS("text/x-wxmaxima-batch")};
static const wxDataFormat kMathMlFormat{wxS("MathML")};
static const wxDataFormat kMathMl2Format{wxS("application/mathml-presentation+xml")};
static const wxDataFormat kRtfFormat{wxS("application/rtf")};
static const wxDataFormat kRtf2Format{wxS("text/rtf")};

SCENARIO("The whole-cell (cut/copy-cells) clipboard object is well-formed") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  GIVEN("RTF on, image flavors off") {
    g_cfg->CopyRTF(true);
    g_cfg->CopyBitmap(false);
    g_cfg->CopySVG(false);
    g_cfg->CopyEMF(false);

    std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
    REQUIRE(data);

    THEN("its formats are distinct and every format round-trips") {
      RequireDistinctAndRetrievable(*data);
    }
    THEN("it offers the wxm, both RTF and the plain-text flavors") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
    }
    THEN("RTF is the preferred flavor") {
      REQUIRE(data->GetPreferredFormat(wxDataObject::Get) == kRtfFormat);
    }
  }

  GIVEN("the bitmap flavor is added") {
    g_cfg->CopyRTF(true);
    g_cfg->CopyBitmap(true);
    g_cfg->CopySVG(false);
    g_cfg->CopyEMF(false);

    std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
    REQUIRE(data);

    THEN("the formats stay distinct and retrievable (the bitmap does not "
         "collide with a text flavor)") {
      RequireDistinctAndRetrievable(*data);
    }
  }

  g_ws->ClearSelection();
}

SCENARIO("The selection (copy-as-output) clipboard object is well-formed") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  GIVEN("MathML and RTF on, bitmap off") {
    g_cfg->CopyMathML(true);
    g_cfg->CopyMathMLHTML(false);
    g_cfg->CopyRTF(true);
    g_cfg->CopyBitmap(false);

    std::unique_ptr<wxDataObject> data = g_ws->CreateSelectionDataObject();
    REQUIRE(data);

    THEN("its formats are distinct and every format round-trips") {
      RequireDistinctAndRetrievable(*data);
    }
    THEN("it offers the wxm, both MathML, both RTF and the plain-text flavors") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kMathMlFormat));
      REQUIRE(HasFormat(fmts, kMathMl2Format));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
    }
    THEN("the preferred flavor is a rich (non-plain-text) one") {
      // wxDataObjectComposite resolves "preferred" as the LAST child added with
      // preferred=true. CreateSelectionDataObject() marks the two MathML
      // flavors preferred and then RtfDataObject2, so RTF - added last - is the
      // one that actually wins here (despite the "MathML is preferred" comment
      // in the builder: an intent/behavior mismatch worth a second look). What
      // matters for paste quality is that the preferred flavor is a rich one,
      // never the raw plain-text or .wxm batch flavor.
      const wxDataFormat pref = data->GetPreferredFormat(wxDataObject::Get);
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, pref));
      REQUIRE_FALSE(pref == wxDataFormat(wxDF_UNICODETEXT));
      REQUIRE_FALSE(pref == wxDataFormat(wxDF_TEXT));
      REQUIRE_FALSE(pref == kWxmFormat);
    }
  }

  GIVEN("the HTML-flavored MathML and the bitmap are added") {
    g_cfg->CopyMathML(true);
    g_cfg->CopyMathMLHTML(true);
    g_cfg->CopyRTF(true);
    g_cfg->CopyBitmap(true);

    std::unique_ptr<wxDataObject> data = g_ws->CreateSelectionDataObject();
    REQUIRE(data);

    THEN("the extra flavors keep the formats distinct and retrievable") {
      RequireDistinctAndRetrievable(*data);
      REQUIRE(HasFormat(GetFormats(*data), wxDataFormat(wxDF_HTML)));
    }
  }

  g_ws->ClearSelection();
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
  wxInitAllImageHandlers(); // the bitmap flavor renders the selection to a bmp

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

  wxEntryCleanup();
  return result;
}
