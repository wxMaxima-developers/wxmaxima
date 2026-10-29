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

  GH #2474: copied cells are also offered as one self-contained XML
  document (Format::CellsToXMLDocument()) -- the XML of a .wxmx, with the
  images that a .wxmx keeps beside it in the zip carried inside, base64
  encoded. It keeps what the .wxm loses: text formatting, output, images.
  Pinned here: it is offered, it round-trips through
  Format::TreeFromXMLDocument(), and making it doesn't disturb a .wxmx
  being saved at the same time.
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
#include <wx/utils.h>
#include <wx/wfstream.h>
#include <wx/zipstrm.h>
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
#include "cells/CellList.h"
#include "cells/ImgCell.h"
#include "cells/LabelCell.h"
#include "cells/TextCell.h"
#include "AttachmentSource.h"
#include "WXMXformat.h"

#include <array>
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

static wxDataFormat BitmapFormat();

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
#ifdef __WXQT__
      // wxQt maps both wxDF_TEXT and wxDF_UNICODETEXT to "text/plain" (see
      // DataFormatIdToMimeType() in its dataobj.cpp), and it is
      // wxTextDataObject itself that advertises both. Nothing wxMaxima
      // offers twice is hidden by skipping exactly that pair.
      if (fmts[i] == wxDataFormat(wxDF_UNICODETEXT) &&
          fmts[j] == wxDataFormat(wxDF_UNICODETEXT))
        continue;
#endif
      INFO("duplicate clipboard format at indices " << j << " and " << i);
      REQUIRE_FALSE(fmts[i] == fmts[j]);
    }
#ifdef __WXQT__
    // wxQt converts a wxBitmap into clipboard image data only in
    // wxBitmapDataObject::QtAddDataTo(), which is never reached for one
    // child of a composite -- there every format is asked for its bytes,
    // and a bitmap has none. Confirmed against a real wxQt build with a
    // plain composite of a text and a bitmap object, so this is wxQt's
    // limitation rather than wxMaxima's. LazyPngDataObject is what gives
    // wxQt an image it really can hand over, and the scenario below pins
    // that; here the byteless bitmap flavour is the one thing skipped.
    if (fmts[i] == BitmapFormat())
      continue;
#endif
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
static const wxDataFormat kXmlFormat{wxS("application/x-wxmaxima-xml")};
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
      REQUIRE(HasFormat(fmts, kXmlFormat));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, kRtf3Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
      REQUIRE(HasFormat(fmts, BitmapFormat()));
      REQUIRE(HasFormat(fmts, kSvgFormat));
#ifdef __WXQT__
      // wxQt cannot hand a wxBitmap over inside a composite at all.
      REQUIRE(HasFormat(fmts, LazyPngDataObject::GetDataFormat()));
#endif
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
    THEN("it offers the wxm, both MathML, all three RTF, the plain-text, "
         "the bitmap and the SVG flavors") {
      const auto fmts = GetFormats(*data);
      REQUIRE(HasFormat(fmts, kWxmFormat));
      REQUIRE(HasFormat(fmts, kMathMlFormat));
      REQUIRE(HasFormat(fmts, kMathMl2Format));
      REQUIRE(HasFormat(fmts, kRtfFormat));
      REQUIRE(HasFormat(fmts, kRtf2Format));
      REQUIRE(HasFormat(fmts, kRtf3Format));
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_UNICODETEXT)));
      REQUIRE(HasFormat(fmts, BitmapFormat()));
      // Vector formats cost nothing until they are pasted, so a part of an
      // output is offered as SVG (and, where it exists, EMF), too (GH #2472).
      REQUIRE(HasFormat(fmts, kSvgFormat));
#if wxUSE_ENH_METAFILE
      REQUIRE(HasFormat(fmts, wxDataFormat(wxDF_ENHMETAFILE)));
#endif
#ifdef __WXQT__
      // wxQt cannot hand a wxBitmap over inside a composite at all.
      REQUIRE(HasFormat(fmts, LazyPngDataObject::GetDataFormat()));
#endif
    }
    THEN("the SVG is an SVG image") {
      const std::string svg = GetData(*data, kSvgFormat);
      REQUIRE(svg.find("<svg") != std::string::npos);
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
      const size_t size = data.GetDataSize(data.GetPreferredFormat());
#ifndef __WXQT__
      // On wxQt a bitmap inside a composite has no bytes to offer at all --
      // see RequireDistinctAndRetrievable() and LazyPngDataObject. Asking
      // still draws it, which is what this scenario is about.
      REQUIRE(size > 0);
#else
      wxUnusedVar(size);
#endif
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

/*! GH #2444 follow-up: a bitmap inside a composite data object reaches
  nobody on wxQt.

  wxQt only turns a wxBitmap into clipboard image data in
  wxBitmapDataObject::QtAddDataTo(), which runs for the whole clipboard
  contents, never for one child of a composite: there every format is asked
  for its bytes, and a bitmap has none. Confirmed against a real wxQt build
  and a plain wxDataObjectComposite holding nothing but a text and a bitmap
  object -- so this predates the lazily rendered clipboard and is not
  specific to wxMaxima. LazyPngDataObject is what gives wxQt an image it can
  hand over, so the invariants that matter are that it encodes the same
  bitmap as PNG and that it offers nothing when there is no bitmap.
*/
SCENARIO("The bitmap is also offered as PNG bytes (wxQt)") {
  GIVEN("a bitmap") {
    wxBitmap bmp(40, 20);
    {
      wxMemoryDC dc(bmp);
      dc.SetBackground(*wxWHITE_BRUSH);
      dc.Clear();
    }
    auto value = std::make_shared<const LazyValue<wxBitmap>>(
      [bmp] { return bmp; });
    LazyPngDataObject data(value);

    THEN("it is offered as image/png") {
      REQUIRE(data.GetPreferredFormat() == wxDataFormat(wxS("image/png")));
    }
    THEN("the bytes are a PNG of that bitmap") {
      const size_t size = data.GetDataSize();
      REQUIRE(size > 8);
      std::string png(size, '\0');
      REQUIRE(data.GetDataHere(&png[0]));
      // The PNG signature, as every image editor identifies the format by.
      REQUIRE(png.compare(0, 4, "\x89PNG") == 0);
      wxMemoryInputStream in(png.data(), png.size());
      wxImage image;
      REQUIRE(image.LoadFile(in, wxBITMAP_TYPE_PNG));
      REQUIRE(image.GetWidth() == bmp.GetWidth());
      REQUIRE(image.GetHeight() == bmp.GetHeight());
    }
  }

  GIVEN("a bitmap that could not be drawn") {
    auto value = std::make_shared<const LazyValue<wxBitmap>>(
      [] { return wxBitmap(); });
    LazyPngDataObject data(value);

    THEN("nothing is offered under it") {
      REQUIRE(data.GetDataSize() == 0);
      std::array<char, 1> buf{};
      REQUIRE_FALSE(data.GetDataHere(buf.data()));
    }
  }
}

//! The bytes of a small PNG image, as a program would store them
static wxMemoryBuffer SmallPng() {
  wxImage image(7, 5);
  image.SetRGB(wxRect(0, 0, 7, 5), 200, 30, 30);
  wxMemoryOutputStream out;
  REQUIRE(image.SaveFile(out, wxBITMAP_TYPE_PNG));
  wxMemoryBuffer png;
  png.AppendData(out.GetOutputStreamBuffer()->GetBufferStart(),
                 out.GetOutputStreamBuffer()->GetBufferSize());
  return png;
}

static bool SameBytes(const wxMemoryBuffer &a, const wxMemoryBuffer &b) {
  return (a.GetDataLen() == b.GetDataLen()) &&
    (std::string(static_cast<const char *>(a.GetData()), a.GetDataLen()) ==
     std::string(static_cast<const char *>(b.GetData()), b.GetDataLen()));
}

SCENARIO("Copied cells round-trip as a self-contained XML document (GH #2474)") {
  GIVEN("a formatted text cell and an image cell") {
    const wxString text = wxS("bold and plain");
    auto textGroup = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, text);
    TextFormat::Formats formats(text.length(), TextFormat::None);
    for (size_t i = 0; i < 4; i++)
      formats[i] = TextFormat::Bold;
    textGroup->GetEditable()->SetFormats(formats);

    const wxMemoryBuffer png = SmallPng();
    auto imageGroup = std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE);
    imageGroup->SetOutput(
      std::make_unique<ImgCell>(imageGroup.get(), g_cfg, png, wxS("png")));

    CellListBuilder<GroupCell> tree;
    tree.Append(std::move(textGroup));
    tree.Append(std::move(imageGroup));
    std::unique_ptr<GroupCell> cells = tree.TakeHead();

    WHEN("they are written as a document and read back") {
      const wxString xml = Format::CellsToXMLDocument(cells.get(), g_cfg);
      auto copy = Format::TreeFromXMLDocument(xml, g_cfg);

      THEN("the image travels inside the document, not as a file") {
        REQUIRE(xml.Contains(wxS("<attachments>")));
        REQUIRE(xml.Contains(wxS("<file name=\"")));
      }
      THEN("the text keeps its formatting") {
        REQUIRE(copy);
        REQUIRE(copy->GetGroupType() == GC_TYPE_TEXT);
        REQUIRE(copy->GetEditable()->GetValue() == text);
        REQUIRE(copy->GetEditable()->GetFormats() == formats);
      }
      THEN("the image has the same bytes as before") {
        REQUIRE(copy);
        GroupCell *image = copy->GetNext();
        REQUIRE(image);
        REQUIRE(image->GetGroupType() == GC_TYPE_IMAGE);
        auto *img = dynamic_cast<ImgCell *>(image->GetLabel());
        REQUIRE(img);
        REQUIRE(img->GetExtension() == wxS("png"));
        REQUIRE(SameBytes(img->GetCompressedImage(), png));
        REQUIRE(img->GetOriginalWidth() == 7);
      }
      THEN("the attachments are no cell of their own") {
        REQUIRE(copy);
        size_t count = 0;
        for (const auto &cell : OnList(copy.get())) {
          (void)cell;
          count++;
        }
        REQUIRE(count == 2);
      }
    }

    WHEN("a .wxmx being saved has already collected files of its own") {
      g_cfg->ClearFilesToSave();
      g_cfg->PushFileToSave(wxS("someone-elses.png"), png);
      Format::CellsToXMLDocument(cells.get(), g_cfg);

      THEN("those are still there, and nothing else is") {
        REQUIRE(g_cfg->GetFilesToSave().size() == 1);
        REQUIRE(g_cfg->GetFilesToSave().front().FileName() ==
                wxS("someone-elses.png"));
      }
      g_cfg->ClearFilesToSave();
    }
  }

  GIVEN("text that is no such document") {
    THEN("nothing is read from it") {
      REQUIRE_FALSE(Format::TreeFromXMLDocument(wxS("not xml"), g_cfg));
      REQUIRE_FALSE(Format::TreeFromXMLDocument(wxS("<other/>"), g_cfg));
      REQUIRE_FALSE(Format::TreeFromXMLDocument(wxEmptyString, g_cfg));
    }
  }
}

SCENARIO("The clipboard's XML flavour carries the copied cells (GH #2474)") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());
  std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
  REQUIRE(data);
  const std::string xml = GetData(*data, kXmlFormat);
  auto copy = Format::TreeFromXMLDocument(wxString::FromUTF8(xml), g_cfg);
  REQUIRE(copy);
  size_t copied = 0;
  for (const auto &cell : OnList(copy.get())) {
    (void)cell;
    copied++;
  }
  size_t original = 0;
  for (const auto &cell : OnList(g_ws->GetTree())) {
    (void)cell;
    original++;
  }
  REQUIRE(copied == original);
  g_ws->ClearSelection();
}

//! Appends an output label like "(%o1)" to \p group
static void AppendLabel(GroupCell *group, const wxString &label) {
  group->AppendOutput(
    std::make_unique<LabelCell>(group, g_cfg, label, TS_LABEL));
}

//! Counts the cells of \p group's output that have the type \p type
static size_t CountOutputCells(const GroupCell *group, CellType type) {
  size_t count = 0;
  for (const Cell *cell = group->GetLabel(); cell; cell = cell->GetNext())
    if (cell->GetType() == type)
      count++;
  return count;
}

SCENARIO("Copied code cells lose their mathematical output, not their plots") {
  GIVEN("a code cell whose output is only maths") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                             wxS("integrate(x,x);"));
    AppendLabel(group.get(), wxS("(%o1)"));
    group->AppendOutput(std::make_unique<TextCell>(group.get(), g_cfg,
                                                   wxS("x^2/2"), TS_VARIABLE));
    WHEN("its maths output is removed") {
      group->RemoveMathOutput();
      THEN("nothing is left of the output, and the input stays") {
        REQUIRE(group->GetLabel() == nullptr);
        REQUIRE(group->GetEditable()->GetValue() == wxS("integrate(x,x);"));
      }
    }
  }

  GIVEN("a code cell that made a plot and returned a value") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE,
                                             wxS("wxdraw2d(explicit(x,x,0,1));"));
    AppendLabel(group.get(), wxS("(%t1)"));
    group->AppendOutput(
      std::make_unique<ImgCell>(group.get(), g_cfg, SmallPng(), wxS("png")));
    AppendLabel(group.get(), wxS("(%o1)"));
    group->AppendOutput(std::make_unique<TextCell>(
                          group.get(), g_cfg, wxS("[gr2d(explicit)]"),
                          TS_VARIABLE));
    WHEN("its maths output is removed") {
      group->RemoveMathOutput();
      THEN("the plot stays, with the label in front of it") {
        REQUIRE(group->GetLabel());
        REQUIRE(group->GetLabel()->GetType() == MC_TYPE_LABEL);
        REQUIRE(group->GetLabel()->ToString().Contains(wxS("%t1")));
        REQUIRE(CountOutputCells(group.get(), MC_TYPE_IMAGE) == 1);
        REQUIRE(CountOutputCells(group.get(), MC_TYPE_LABEL) == 1);
        REQUIRE(CountOutputCells(group.get(), MC_TYPE_DEFAULT) == 0);
        REQUIRE(group->GetLabel()->GetNext()->GetNext() == nullptr);
      }
    }
  }

  GIVEN("an image cell") {
    auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE);
    group->SetOutput(
      std::make_unique<ImgCell>(group.get(), g_cfg, SmallPng(), wxS("png")));
    WHEN("its maths output is removed") {
      group->RemoveMathOutput();
      THEN("its image is still there") {
        REQUIRE(group->GetLabel());
        REQUIRE(group->GetLabel()->GetType() == MC_TYPE_IMAGE);
      }
    }
  }
}

SCENARIO("The clipboard's XML flavour has no maths output, the others do") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());
  std::unique_ptr<wxDataObject> data = g_ws->CreateCellsDataObject();
  REQUIRE(data);

  // The corpus has evaluated code cells; their first result is a function
  // definition, function:=...
  bool worksheetHasOutput = false;
  for (const auto &cell : OnList(g_ws->GetTree()))
    if ((cell.GetGroupType() == GC_TYPE_CODE) && cell.GetLabel())
      worksheetHasOutput = true;
  REQUIRE(worksheetHasOutput);

  THEN("the cells wxMaxima pastes back carry no output") {
    auto copy = Format::TreeFromXMLDocument(
      wxString::FromUTF8(GetData(*data, kXmlFormat)), g_cfg);
    REQUIRE(copy);
    for (const auto &cell : OnList(copy.get()))
      if (cell.GetGroupType() == GC_TYPE_CODE)
        REQUIRE(cell.GetLabel() == nullptr);
  }
  THEN("the worksheet's own cells keep theirs") {
    GetData(*data, kXmlFormat);
    bool stillHasOutput = false;
    for (const auto &cell : OnList(g_ws->GetTree()))
      if ((cell.GetGroupType() == GC_TYPE_CODE) && cell.GetLabel())
        stillHasOutput = true;
    REQUIRE(stillHasOutput);
  }
  THEN("the text for other programs still shows the results") {
    const std::string rtf = GetData(*data, kRtfFormat);
    REQUIRE(rtf.find("%o1") != std::string::npos);
  }
  g_ws->ClearSelection();
}

SCENARIO("AttachmentSource reads files from memory") {
  AttachmentSource::Files files;
  files[wxS("a.png")] = std::string("\x89PNG\0x", 6);
  const AttachmentSource source = AttachmentSource::FromMemory(files);
  REQUIRE_FALSE(source.IsEmpty());
  const wxMemoryBuffer a = source.Read(wxS("a.png"));
  REQUIRE(a.GetDataLen() == 6);
  REQUIRE(std::string(static_cast<const char *>(a.GetData()), 6) ==
          files[wxS("a.png")]);
  REQUIRE(source.Read(wxS("missing.png")).GetDataLen() == 0);
  REQUIRE(AttachmentSource().IsEmpty());
}

SCENARIO("AttachmentSource reads files from a .wxmx archive") {
  const wxString wxmx = wxFileName::CreateTempFileName(wxS("wxm-attach"));
  {
    wxFFileOutputStream out(wxmx);
    wxZipOutputStream zip(out);
    zip.PutNextEntry(wxS("content.xml"));
    zip.Write("<x/>", 4);
    zip.PutNextEntry(wxS("image1.png"));
    zip.Write("PNGDATA", 7);
    zip.CloseEntry();
    REQUIRE(zip.Close());
  }
  const AttachmentSource source = AttachmentSource::FromWxmx(wxmx);
  REQUIRE_FALSE(source.IsEmpty());
  const wxMemoryBuffer image = source.Read(wxS("image1.png"));
  REQUIRE(std::string(static_cast<const char *>(image.GetData()),
                      image.GetDataLen()) == "PNGDATA");
  REQUIRE(source.Read(wxS("missing.png")).GetDataLen() == 0);
  wxRemoveFile(wxmx);
}

SCENARIO("AttachmentSource reads a file bigger than one read chunk") {
  // Not a multiple of the 64 KiB chunk size, so the last chunk is a partial
  // one, and not all one byte, so chunks appended in the wrong place or
  // twice would show.
  std::string big(3 * 65536 + 1234, '\0');
  for (size_t i = 0; i < big.size(); i++)
    big[i] = static_cast<char>(i * 7 + i / 251);
  const wxString wxmx = wxFileName::CreateTempFileName(wxS("wxm-attach"));
  {
    wxFFileOutputStream out(wxmx);
    wxZipOutputStream zip(out);
    zip.PutNextEntry(wxS("content.xml"));
    zip.Write("<x/>", 4);
    zip.PutNextEntry(wxS("big.dat"));
    zip.Write(big.data(), big.size());
    zip.PutNextEntry(wxS("after.dat"));
    zip.Write("AFTER", 5);
    zip.CloseEntry();
    REQUIRE(zip.Close());
  }
  const AttachmentSource source = AttachmentSource::FromWxmx(wxmx);
  const wxMemoryBuffer data = source.Read(wxS("big.dat"));
  REQUIRE(data.GetDataLen() == big.size());
  REQUIRE(std::string(static_cast<const char *>(data.GetData()),
                      data.GetDataLen()) == big);
  const wxMemoryBuffer after = source.Read(wxS("after.dat"));
  REQUIRE(std::string(static_cast<const char *>(after.GetData()),
                      after.GetDataLen()) == "AFTER");
  wxRemoveFile(wxmx);
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  wxInitAllImageHandlers(); // the bitmap flavor renders the selection to a bmp

  // A temp directory of our own: HtmlClipTempEntryCount() counts the
  // "htmlclip*" entries in it, and in the shared system temp directory other
  // test binaries running in parallel (ctest -j) create and remove those as
  // well, which made the "left no trace" check fail now and then.
  const wxString privateTmp = wxFileName::GetTempDir() +
    wxFileName::GetPathSeparator() +
    wxString::Format(wxS("wxm-clipboard-test-%lu"),
                     static_cast<unsigned long>(wxGetProcessId()));
  wxFileName::Mkdir(privateTmp, wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL);
  for (const auto &var : {wxS("TMPDIR"), wxS("TMP"), wxS("TEMP")})
    wxSetEnv(var, privateTmp);

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

  wxFileName::Rmdir(privateTmp, wxPATH_RMDIR_RECURSIVE);
  wxEntryCleanup();
  return result;
}
