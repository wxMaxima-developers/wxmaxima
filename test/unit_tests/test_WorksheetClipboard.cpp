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
  - every flavor is offered, with a sensible "preferred" flavor.

  GH #2030: only the .wxm and the plain-text flavors are made when the copy
  is made. Everything else (RTF, MathML, the bitmap, the SVG, ...) is
  rendered from a private copy of the selection only when a program asks
  for it, which is why every flavor is offered now instead of only the ones
  a config setting asked for. That is pinned here too: offering renders
  nothing, a payload offered under several names renders once, a copy
  survives the worksheet changing under it, and the data handed over when
  the worksheet closes refers to neither the cells nor the configuration.

  GH #2264: "Copy as RTF" pasted into MS Word was silently ignored. RTF was
  only advertised under the MIME-style names "application/rtf"/"text/rtf" -
  what GTK/Linux word processors look for - but Windows registers CF_RTF
  under the literal name "Rich Text Format" (via RegisterClipboardFormat),
  which is the name Word's clipboard handler actually looks up. Neither MIME
  name matches it, so on Windows Word found no RTF data on the clipboard at
  all. Fixed by additionally advertising RTF under a third wxDataFormat with
  that exact name (RtfDataObject3 / Worksheet::m_rtfFormat3) and making it the
  preferred flavor everywhere RTF is offered; this test pins its presence and
  that it is now the winning "preferred" format.

  GH #2265/#2266/#2267: "Copy as HTML" -- WorksheetExport::
  SelectionToSelfContainedHTML() renders a GroupCell range into one
  self-contained HTML document (inline <style>, no external file
  references), for Worksheet::CopyHTML()'s dedicated context-menu command.
  It can't itself be exercised via the real system clipboard here (same
  headless-unfriendly reason as the rest of this file), but it is a pure
  function of the cell tree, so it is tested directly: the returned document
  must carry its own stylesheet inline and never point at a file outside
  itself, and it must clean up whatever private scratch directory it used to
  render into.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dataobj.h>
#include <wx/dcmemory.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/xml/xml.h>
#ifdef __WXMSW__
#include <wx/msw/wrapwin.h>
#endif

#include "Configuration.h"
#include "Dirstructure.h"
#include "MathParser.h"
#include "worksheet/ClipboardContents.h"
#include "worksheet/Worksheet.h"
#include "worksheet/WorksheetExport.h"
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
#if defined(__WXMSW__) && wxUSE_ENH_METAFILE
    // An enhanced metafile is handed over as a handle, not as bytes, so its
    // size is 0 by design (see wxEnhMetaFileDataObject::GetDataSize()).
    if (fmts[i] == wxDataFormat(wxDF_ENHMETAFILE)) {
      HENHMETAFILE metafile = nullptr;
      REQUIRE(obj.GetDataHere(fmts[i], &metafile));
      REQUIRE(metafile != nullptr);
      DeleteEnhMetaFile(metafile);
      continue;
    }
#endif
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
static const wxDataFormat kRtf3Format{wxS("Rich Text Format")};

static const wxDataFormat kSvgFormat{wxS("image/svg+xml")};

//! The format a bitmap is offered in: wxDF_BITMAP on GTK and macOS, a DIB on
//! Windows.
static wxDataFormat BitmapFormat() {
  return wxBitmapDataObject().GetPreferredFormat();
}

//! The bytes a data object holds in one format, as a string
static std::string GetData(const wxDataObject &obj, const wxDataFormat &format) {
  const size_t size = obj.GetDataSize(format);
  std::string data(size, '\0');
  if (size)
    REQUIRE(obj.GetDataHere(format, data.data()));
  return data;
}

SCENARIO("The whole-cell (cut/copy-cells) clipboard object is well-formed") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  GIVEN("the clipboard object for a selection of whole cells") {
    std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
    REQUIRE(data);

    THEN("its formats are distinct and every format round-trips") {
      RequireDistinctAndRetrievable(*data);
    }
    THEN("it offers the wxm, all three RTF flavors, the plain-text flavor, "
         "a bitmap and an SVG") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, kRtf3Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
      REQUIRE(HasFormat(fmts, BitmapFormat()));
      REQUIRE(HasFormat(fmts, kSvgFormat));
    }
    THEN("the \"Rich Text Format\"-named flavor MS Word looks for (GH #2264) "
         "is the preferred one") {
      REQUIRE(data->GetPreferredFormat(wxDataObject::Get) == kRtf3Format);
    }
    THEN("all three RTF flavors carry the same document") {
      const std::string rtf = GetData(*data, kRtfFormat);
      REQUIRE(rtf.rfind("{\\rtf", 0) == 0);
      REQUIRE(GetData(*data, kRtf2Format) == rtf);
      REQUIRE(GetData(*data, kRtf3Format) == rtf);
    }
  }

  g_ws->ClearSelection();
}

SCENARIO("The selection (copy-as-output) clipboard object is well-formed") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  GIVEN("the clipboard object for a selection") {
    std::unique_ptr<wxDataObject> data = g_ws->CreateSelectionDataObject();
    REQUIRE(data);

    THEN("its formats are distinct and every format round-trips") {
      RequireDistinctAndRetrievable(*data);
    }
    THEN("it offers the wxm, both MathML, all three RTF, the plain-text and "
         "the bitmap flavors") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kMathMlFormat));
      REQUIRE(HasFormat(fmts, kMathMl2Format));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, kRtf3Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
      REQUIRE(HasFormat(fmts, BitmapFormat()));
    }
    THEN("it doesn't offer MathML as HTML, which word processors mishandle") {
      REQUIRE_FALSE(HasFormat(GetFormats(*data), wxDataFormat(wxDF_HTML)));
    }
    THEN("the MathML is a MathML document") {
      const std::string mathML = GetData(*data, kMathMlFormat);
      REQUIRE(mathML.find("<math") != std::string::npos);
      REQUIRE(GetData(*data, kMathMl2Format) == mathML);
    }
    THEN("the preferred flavor is the \"Rich Text Format\"-named one "
         "(GH #2264)") {
      // wxDataObjectComposite resolves "preferred" as the LAST child added with
      // preferred=true. CreateSelectionDataObject() marks the two MathML
      // flavors preferred and then RtfDataObject3 ("Rich Text Format", the
      // name MS Word's clipboard handler actually looks up) last, so that one -
      // added last - is what wins here (despite the "MathML is preferred"
      // comment in the builder: an intent/behavior mismatch worth a second
      // look). What matters for paste quality is that the preferred flavor is
      // a rich one Word can actually recognize, never the raw plain-text or
      // .wxm batch flavor.
      const wxDataFormat pref = data->GetPreferredFormat(wxDataObject::Get);
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, pref));
      REQUIRE(pref == kRtf3Format);
      REQUIRE_FALSE(pref == wxDataFormat(wxDF_UNICODETEXT));
      REQUIRE_FALSE(pref == wxDataFormat(wxDF_TEXT));
      REQUIRE_FALSE(pref == kWxmFormat);
    }
  }

  g_ws->ClearSelection();
}

SCENARIO("Clipboard formats are only rendered when they are asked for "
         "(GH #2030)") {
  GIVEN("one RTF payload offered under two format names") {
    int renders = 0;
    auto payload = std::make_shared<const LazyValue<std::string>>([&renders] {
      ++renders;
      return std::string("{\\rtf1 x}");
    });
    wxDataObjectComposite data;
    data.Add(new LazyDataObject(kRtfFormat, payload));
    data.Add(new LazyDataObject(kRtf2Format, payload), true);

    THEN("offering the formats renders nothing") {
      REQUIRE(GetFormats(data).size() == 2);
      REQUIRE(data.GetPreferredFormat() == kRtf2Format);
      REQUIRE(renders == 0);
    }
    THEN("the first request renders it, and only once for both names") {
      REQUIRE(GetData(data, kRtfFormat) == "{\\rtf1 x}");
      REQUIRE(GetData(data, kRtf2Format) == "{\\rtf1 x}");
      REQUIRE(renders == 1);
    }
  }

  GIVEN("a bitmap") {
    int renders = 0;
    auto payload = std::make_shared<const LazyValue<wxBitmap>>([&renders] {
      ++renders;
      return wxBitmap(10, 10);
    });
    LazyBitmapDataObject data(payload);
    THEN("it is drawn on the first request, and only then") {
      REQUIRE(renders == 0);
      REQUIRE(data.GetDataSize(data.GetPreferredFormat()) > 0);
      REQUIRE(data.GetBitmap().GetWidth() == 10);
      REQUIRE(renders == 1);
    }
  }

  GIVEN("a bitmap that is too big to be drawn") {
    auto payload =
      std::make_shared<const LazyValue<wxBitmap>>([] { return wxBitmap(); });
    LazyBitmapDataObject data(payload);
    THEN("it offers no data instead of an invalid bitmap") {
      REQUIRE(data.GetDataSize(data.GetPreferredFormat()) == 0);
      std::vector<char> buf(16);
      REQUIRE_FALSE(data.GetDataHere(data.GetPreferredFormat(), buf.data()));
    }
  }

  GIVEN("the clipboard object for a selection") {
    BuildDocumentOnce();
    g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());
    std::unique_ptr<wxDataObject> data = g_ws->CreateSelectionDataObject();
    g_ws->ClearSelection();

    THEN("there is nothing to hand over once it is gone") {
      // RenderClipboardContents() would replace the clipboard's data if this
      // object were still on it; once it is gone, nothing refers to its
      // contents any more and the worksheet must leave the clipboard alone.
      data.reset();
      REQUIRE_FALSE(g_ws->RenderClipboardContents());
    }
  }
}

SCENARIO("What was copied is pasted, even if the worksheet has changed since "
         "(GH #2030)") {
  BuildDocumentOnce();

  GIVEN("a copied cell that has been deleted before anything was pasted") {
    g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                  wxS("lazysnapshotcell: 42;")),
      g_ws->GetLastCellInWorksheet());
    g_ws->RecalculateIfNeeded();
    GroupCell *const cell = g_ws->GetLastCellInWorksheet();
    g_ws->SetSelection(cell, cell);
    std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
    REQUIRE(data);
    g_ws->ClearSelection();
    // No undo buffer: the cell is really destroyed, not kept for an undo.
    g_ws->DeleteRegion(cell, cell, nullptr);
    g_ws->RecalculateIfNeeded();

    THEN("every format is still rendered from the copy") {
      RequireDistinctAndRetrievable(*data);
      REQUIRE(GetData(*data, kRtfFormat).find("lazysnapshotcell") !=
              std::string::npos);
      REQUIRE(GetData(*data, kWxmFormat).find("lazysnapshotcell") !=
              std::string::npos);
    }
  }
}

SCENARIO("The data handed over when the worksheet closes no longer needs it "
         "(GH #2030)") {
  GIVEN("contents with every format a copy can offer") {
    int renders = 0;
    auto counted = [&renders](std::string value) {
      return LazyValue<std::string>([&renders, value] {
        ++renders;
        return value;
      });
    };
    ClipboardContents contents;
    contents.wxm = wxS("/* wxm */");
    contents.text = wxS("x^2");
    contents.mathML = counted("<math><mi>x</mi></math>");
    contents.rtf = counted("{\\rtf1 x}");
    contents.svg = counted("<svg/>");
    contents.bitmap = LazyValue<wxBitmap>([&renders] {
      ++renders;
      return wxBitmap(10, 10);
    });

    std::unique_ptr<wxDataObject> data =
      Worksheet::CreateIndependentDataObject(contents);

    THEN("it keeps wxm, MathML, RTF, text and the bitmap, but not the SVG") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kMathMlFormat));
      REQUIRE(HasFormat(fmts, kMathMl2Format));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, kRtf3Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
      REQUIRE(HasFormat(fmts, BitmapFormat()));
      REQUIRE_FALSE(HasFormat(fmts, kSvgFormat));
      RequireDistinctAndRetrievable(*data);
    }
    THEN("everything in it was rendered before it was returned") {
      REQUIRE(renders == 3);
      REQUIRE(contents.rtf.IsRendered());
      REQUIRE_FALSE(contents.svg.IsRendered());
      REQUIRE(GetData(*data, kRtf3Format) == "{\\rtf1 x}");
    }
  }

  GIVEN("contents without MathML and with a bitmap too big to draw") {
    ClipboardContents contents;
    contents.wxm = wxS("/* wxm */");
    contents.text = wxS("x^2");
    contents.rtf = LazyValue<std::string>([] { return std::string("{\\rtf1 x}"); });
    contents.bitmap = LazyValue<wxBitmap>([] { return wxBitmap(); });

    std::unique_ptr<wxDataObject> data =
      Worksheet::CreateIndependentDataObject(contents);
    THEN("it offers neither") {
      const auto fmts = GetFormats(*data);
      REQUIRE_FALSE(HasFormat(fmts, kMathMlFormat));
      REQUIRE_FALSE(HasFormat(fmts, wxDataFormat(wxDF_BITMAP)));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      RequireDistinctAndRetrievable(*data);
    }
  }
}

//! Number of "htmlclip*" entries left behind anywhere SelectionTo-
//! SelfContainedHTML() might have rendered its scratch images into, so a
//! test can confirm it cleans up after itself. This unit test binary never
//! constructs a Dirstructure (that only happens as part of building a full
//! wxMaxima app object), so Dirstructure::UserConfDir() is empty here and
//! MakeSelfContainedHtmlTempDir() takes its documented fallback: the
//! system temp directory via wxFileName::CreateTempFileName()'s own
//! default. Check both locations so this stays correct regardless of which
//! one a future change ends up exercising.
static size_t HtmlClipTempEntryCount() {
  size_t count = 0;
  auto countIn = [&](const wxString &dir) {
    if (!wxFileName::DirExists(dir))
      return;
    wxDir d(dir);
    if (!d.IsOpened())
      return;
    wxString name;
    bool cont = d.GetFirst(&name, wxS("htmlclip*"),
                          wxDIR_FILES | wxDIR_DIRS);
    while (cont) {
      ++count;
      cont = d.GetNext(&name);
    }
  };
  countIn(wxFileName::GetTempDir());
  if (!Dirstructure::UserConfDir().IsEmpty())
    countIn(Dirstructure::UserConfDir() + wxFileName::GetPathSeparator() +
           wxS("tmp"));
  return count;
}

SCENARIO("Copy as HTML renders a self-contained document (GH #2265/#2266/#2267)") {
  BuildDocumentOnce();
  GroupCell *const start = g_ws->GetTree();
  GroupCell *const end = g_ws->GetLastCellInWorksheet();
  REQUIRE(start != nullptr);
  REQUIRE(end != nullptr);

  const size_t entriesBefore = HtmlClipTempEntryCount();

  const wxString html =
    WorksheetExport::SelectionToSelfContainedHTML(start, end, g_cfg);

  THEN("it isn't empty and carries its own stylesheet inline") {
    REQUIRE_FALSE(html.IsEmpty());
    REQUIRE(html.Contains(wxS("<style>")));
    REQUIRE(html.Contains(wxS("</style>")));
  }
  THEN("it never links to an external stylesheet") {
    REQUIRE_FALSE(html.Contains(wxS("<link rel=\"stylesheet\"")));
  }
  THEN("every image reference (if any) is a data: URI, never a file path") {
    wxString remaining = html;
    size_t pos;
    while ((pos = remaining.find(wxS("src=\""))) != wxString::npos) {
      remaining = remaining.Mid(pos + 5);
      const size_t end2 = remaining.find(wxS('"'));
      REQUIRE(end2 != wxString::npos);
      const wxString src = remaining.Left(end2);
      INFO("src attribute value: " << src);
      REQUIRE(src.StartsWith(wxS("data:")));
      remaining = remaining.Mid(end2 + 1);
    }
  }
  THEN("the document's actual content made it through") {
    // BuildDocumentOnce() appends these two synthetic cells after the real
    // math corpus -- their text/code showing up confirms the whole selected
    // range was rendered, not just its first cell.
    REQUIRE(html.Contains(wxS("ClipboardNetTitle")));
    REQUIRE(html.Contains(wxS("factor")));
  }
  THEN("the private scratch directory it rendered images into left no trace") {
    REQUIRE(HtmlClipTempEntryCount() == entriesBefore);
  }
}

SCENARIO("SelectionToSelfContainedHTML rejects a null range (GH #2265)") {
  THEN("a null start or end yields an empty string, not a crash") {
    REQUIRE(WorksheetExport::SelectionToSelfContainedHTML(nullptr, nullptr, g_cfg)
            .IsEmpty());
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
  wxInitAllImageHandlers(); // the bitmap flavor renders the selection to a bmp

  g_bmp = new wxBitmap(1000, 1000);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  // Every flavor is offered now, the bitmap included; let it fit even when the
  // whole test document is selected, so every flavor carries data.
  g_cfg->MaxClipbrdBitmapMegabytes(1000);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
