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
  Safety net for the export/serialization cluster of Worksheet.

  ExportToHTML/ExportToTeX/ExportToMAC, the RTF header/footer and the
  selection-to-string converters form the document-serialization part of
  Worksheet (they read only the cell tree, the selection and the
  Configuration). This test pins their behavior down before that cluster is
  moved out of the Worksheet class:

  - A rich document (the real content.xml corpus parsed through MathParser,
    plus sentinel title/section/text/code cells) is loaded into a real
    Worksheet.
  - Every exporter runs twice; the resulting file trees must be byte-for-byte
    identical, and must contain the sentinel texts. The double run also proves
    exporting leaves the worksheet in a state that exports identically again.

  Refactor harness: set WXM_EXPORT_DUMP_DIR=<dir> to keep the exported files.
  Run once at the old revision and once at the new one with different dirs and
  `diff -r` them - byte-identical trees mean the extraction preserved behavior.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/dir.h>
#include <wx/ffile.h>
// Not pulled in transitively on wxMSW (whose default wxConfig is the registry
// one), so include it explicitly for the hermetic wxFileConfig below.
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/xml/xml.h>
#include <wx/zipstrm.h>

#include "Configuration.h"
#include "MathParser.h"
#include "Worksheet.h"
#include "cells/GroupCell.h"

#include <cstdlib>
#include <map>
#include <string>
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
//! The root all exported files land under (kept if WXM_EXPORT_DUMP_DIR is set)
wxString g_outputRoot;
bool g_keepOutput = false;
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

//! Reads a file as raw bytes (images aren't UTF-8).
static std::string ReadBinaryFile(const wxString &path) {
  wxFFile f(path, wxS("rb"));
  REQUIRE(f.IsOpened());
  std::string contents;
  contents.resize(static_cast<size_t>(f.Length()));
  if (!contents.empty())
    REQUIRE(f.Read(&contents[0], contents.size()) == contents.size());
  return contents;
}

/*! Blanks the numbers in wxSVGFileDC's id="imageNN" attributes.

  wxSVGFileDC numbers embedded bitmaps with a process-global counter that is
  never reset, so the same document exported twice yields different (but
  equally meaningless - nothing references them) image ids. Normalize them so
  the byte comparison checks the actual content.
*/
static void NormalizeSvgImageIds(std::string *svg) {
  const std::string needle = "id=\"image";
  size_t pos = 0;
  while ((pos = svg->find(needle, pos)) != std::string::npos) {
    size_t digits = pos + needle.size();
    size_t end = digits;
    while (end < svg->size() && isdigit(static_cast<unsigned char>((*svg)[end])))
      ++end;
    svg->erase(digits, end - digits);
    pos = digits;
  }
}

/*! The entry names and bytes of a zip archive, without the zip metadata.

  A .wxmx is a zip; its entries carry wall-clock modification timestamps, so
  two exports of identical content yield different archive bytes. The entry
  contents are what the export produces - compare those.
*/
static std::string ZipContentFingerprint(const wxString &path) {
  wxFFileInputStream in(path);
  REQUIRE(in.IsOk());
  wxZipInputStream zip(in);
  std::string out;
  while (std::unique_ptr<wxZipEntry> entry{zip.GetNextEntry()}) {
    out += entry->GetName().utf8_str();
    out += '\0';
    char buf[4096];
    for (;;) {
      zip.Read(buf, sizeof(buf));
      if (zip.LastRead() == 0)
        break;
      out.append(buf, zip.LastRead());
    }
    out += '\0';
  }
  REQUIRE(!out.empty());
  return out;
}

//! Collects every file below dir as relative-path -> comparable bytes.
static void CollectFiles(const wxString &dir, const wxString &rel,
                         std::map<wxString, std::string> *out) {
  wxDir d(dir);
  REQUIRE(d.IsOpened());
  wxString name;
  bool cont = d.GetFirst(&name, wxEmptyString, wxDIR_FILES);
  while (cont) {
    std::string bytes;
    if (name.EndsWith(wxS(".wxmx"))) {
      bytes = ZipContentFingerprint(dir + wxS("/") + name);
    } else {
      bytes = ReadBinaryFile(dir + wxS("/") + name);
      if (name.EndsWith(wxS(".svg")))
        NormalizeSvgImageIds(&bytes);
    }
    (*out)[rel + name] = std::move(bytes);
    cont = d.GetNext(&name);
  }
  cont = d.GetFirst(&name, wxEmptyString, wxDIR_DIRS);
  while (cont) {
    CollectFiles(dir + wxS("/") + name, rel + name + wxS("/"), out);
    cont = d.GetNext(&name);
  }
}

static std::map<wxString, std::string> SnapshotDir(const wxString &dir) {
  std::map<wxString, std::string> out;
  CollectFiles(dir, wxEmptyString, &out);
  return out;
}

//! The two snapshots must contain the same files with the same bytes.
static void RequireIdenticalTrees(const std::map<wxString, std::string> &a,
                                  const std::map<wxString, std::string> &b) {
  for (const auto &entry : a) {
    INFO("file: " << entry.first.ToStdString());
    auto it = b.find(entry.first);
    REQUIRE(it != b.end());
    REQUIRE(entry.second == it->second);
  }
  REQUIRE(a.size() == b.size());
}

//! A subdirectory of the output root, freshly created.
static wxString MakeExportDir(const wxString &name) {
  const wxString dir = g_outputRoot + wxS("/") + name;
  if (!wxDirExists(dir))
    REQUIRE(wxMkdir(dir));
  return dir;
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

// Sentinel texts that must be findable in every export format.
static const wxChar *const kTitleSentinel = wxS("ExportNetDocumentTitle");
static const wxChar *const kSectionSentinel = wxS("ExportNetSectionHeading");
static const wxChar *const kTextSentinel = wxS("ExportNetTextParagraph");
static const wxChar *const kCodeSentinel = wxS("factor(xexportnet^2-1);");

/*! Fills the worksheet with the real math corpus plus sentinel cells.

  Built once; all scenarios export the same document, which is exactly what
  the refactor-diff harness wants (one fixed input, many outputs).
*/
static void BuildDocumentOnce() {
  if (g_ws->GetTree())
    return;

  // A rich body of real math output, exported by actual wxMaxima.
  g_ws->InsertGroupCells(ParseCorpusFile(wxS("sampleWorksheet.xml")), nullptr);
  g_ws->InsertGroupCells(ParseCorpusFile(wxS("math-constructs.xml")),
                         g_ws->GetLastCellInWorksheet());

  // Sentinels for asserting content presence per format.
  auto appendCell = [](GroupType type, const wxChar *text) {
    g_ws->InsertGroupCells(
      std::make_unique<GroupCell>(g_cfg, type, wxString(text)),
      g_ws->GetLastCellInWorksheet());
  };
  appendCell(GC_TYPE_TITLE, kTitleSentinel);
  appendCell(GC_TYPE_SECTION, kSectionSentinel);
  appendCell(GC_TYPE_TEXT, kTextSentinel);
  appendCell(GC_TYPE_CODE, kCodeSentinel);

  g_ws->RecalculateIfNeeded();
}

static void RequireContainsSentinels(const wxString &text) {
  REQUIRE(text.Contains(kTitleSentinel));
  REQUIRE(text.Contains(kSectionSentinel));
  REQUIRE(text.Contains(kTextSentinel));
  REQUIRE(text.Contains(wxS("xexportnet")));
}

/*! Every <img src="..."> in the HTML must point at a file that exists.

  Regression guard: the bitmap equation flavor used to build the link from the
  .html file's own extension instead of ".png", producing "doc_0html" links to
  files named "doc_0.png".
*/
static void RequireImgSrcsExist(const wxString &html, const wxString &htmlDir) {
  size_t checked = 0;
  const wxString needle = wxS("<img src=\"");
  size_t pos = 0;
  wxString remainder = html;
  while ((pos = remainder.find(needle)) != wxString::npos) {
    remainder = remainder.Mid(pos + needle.length());
    const size_t end = remainder.find(wxS("\""));
    REQUIRE(end != wxString::npos);
    const wxString src = remainder.Left(end);
    INFO("img src: " << src.ToStdString());
    REQUIRE(wxFileExists(htmlDir + wxS("/") + src));
    ++checked;
  }
  // The document contains math output, so the image flavors must link images.
  REQUIRE(checked > 0);
}

SCENARIO("HTML export succeeds, is deterministic and contains the document") {
  BuildDocumentOnce();

  // Every equation-rendering flavor is a separate code path in the exporter;
  // bitmap and svg additionally exercise the CopyToFile image rendering.
  struct EqFormat { Configuration::htmlExportFormat format; const wxChar *name; };
  const EqFormat formats[] = {
    {Configuration::mathJaX_TeX, wxS("mathjax")},
    {Configuration::bitmap, wxS("bitmap")},
    {Configuration::mathML_mathJaX, wxS("mathml")},
    {Configuration::svg, wxS("svg")},
  };
  const Configuration::htmlExportFormat oldFormat =
    g_cfg->HTMLequationFormat();

  for (const auto &eq : formats) {
    g_cfg->HTMLequationFormat(eq.format);
    const wxString dir1 =
      MakeExportDir(wxString(wxS("html_run1_")) + eq.name);
    const wxString dir2 =
      MakeExportDir(wxString(wxS("html_run2_")) + eq.name);
    REQUIRE(g_ws->ExportToHTML(dir1 + wxS("/doc.html")));
    REQUIRE(g_ws->ExportToHTML(dir2 + wxS("/doc.html")));

    THEN((wxString(wxS("the ")) + eq.name +
          wxS(" flavor is deterministic and complete")).ToStdString()) {
      const auto snap1 = SnapshotDir(dir1);
      const auto snap2 = SnapshotDir(dir2);
      REQUIRE(snap1.count(wxS("doc.html")) == 1);
      REQUIRE(snap1.count(wxS("doc_htmlimg/doc.css")) == 1);
      // The image-rendering flavors must actually produce equation images.
      if (eq.format == Configuration::bitmap ||
          eq.format == Configuration::svg)
        REQUIRE(snap1.size() > 3);
      RequireIdenticalTrees(snap1, snap2);
      const wxString html = ReadTextFile(dir1 + wxS("/doc.html"));
      RequireContainsSentinels(html);
      // Image links must not dangle (broken-link regression, see helper).
      if (eq.format == Configuration::bitmap ||
          eq.format == Configuration::svg)
        RequireImgSrcsExist(html, dir1);
    }
  }
  g_cfg->HTMLequationFormat(oldFormat);
}

SCENARIO("TeX export succeeds, is deterministic and contains the document") {
  BuildDocumentOnce();

  const wxString dir1 = MakeExportDir(wxS("tex_run1"));
  const wxString dir2 = MakeExportDir(wxS("tex_run2"));
  REQUIRE(g_ws->ExportToTeX(dir1 + wxS("/doc.tex")));
  REQUIRE(g_ws->ExportToTeX(dir2 + wxS("/doc.tex")));

  THEN("both runs produce byte-identical file trees") {
    RequireIdenticalTrees(SnapshotDir(dir1), SnapshotDir(dir2));
  }

  THEN("the TeX file is a document and contains the content") {
    const wxString tex = ReadTextFile(dir1 + wxS("/doc.tex"));
    REQUIRE(tex.Contains(wxS("\\documentclass")));
    REQUIRE(tex.Contains(wxS("\\end{document}")));
    RequireContainsSentinels(tex);
  }
}

SCENARIO("Batch (.mac) and .wxm export succeed, are deterministic and complete") {
  BuildDocumentOnce();

  for (const wxChar *ext : {wxS("mac"), wxS("wxm")}) {
    const wxString name = wxString(wxS("doc.")) + ext;
    const wxString dir1 = MakeExportDir(wxString(wxS("mac_run1_")) + ext);
    const wxString dir2 = MakeExportDir(wxString(wxS("mac_run2_")) + ext);
    REQUIRE(g_ws->ExportToMAC(dir1 + wxS("/") + name));
    REQUIRE(g_ws->ExportToMAC(dir2 + wxS("/") + name));

    THEN((wxString(wxS("the .")) + ext +
          wxS(" export is deterministic and complete")).ToStdString()) {
      RequireIdenticalTrees(SnapshotDir(dir1), SnapshotDir(dir2));
      const wxString mac = ReadTextFile(dir1 + wxS("/") + name);
      // The wxMaxima version header is a .wxm-only feature.
      REQUIRE(mac.Contains(wxS("Created with wxMaxima")) ==
              (wxString(ext) == wxS("wxm")));
      REQUIRE(mac.Contains(wxS("xexportnet")));
      // Comment-type cells survive as maxima comments in both flavors.
      REQUIRE(mac.Contains(kTextSentinel));
    }
  }
}

SCENARIO("The selection-to-string converters are deterministic and complete") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  THEN("GetString returns the whole selection, stably") {
    const wxString s1 = g_ws->GetString(true);
    const wxString s2 = g_ws->GetString(true);
    REQUIRE(s1 == s2);
    REQUIRE(s1.Contains(wxS("xexportnet")));
  }

  THEN("ConvertSelectionToMathML is stable and produces math markup") {
    const wxString m1 = g_ws->ConvertSelectionToMathML();
    const wxString m2 = g_ws->ConvertSelectionToMathML();
    REQUIRE(m1 == m2);
    REQUIRE(m1.Contains(wxS("<math")));
  }

  THEN("the RTF frame is stable and well-formed") {
    const wxString start = g_ws->RTFStart();
    const wxString end = g_ws->RTFEnd();
    REQUIRE(start.StartsWith(wxS("{\\rtf")));
    REQUIRE(end.Contains(wxS("}")));
    REQUIRE(start == g_ws->RTFStart());
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
  wxInitAllImageHandlers(); // the HTML export renders math to PNG files

  // Hermetic settings: ExportToHTML reads styles from the global wxConfig.
  const wxString cfgFile = wxFileName::CreateTempFileName(wxS("wxmexportcfg"));
  wxConfig::Set(new wxFileConfig(wxS("wxMaxima"), wxEmptyString, cfgFile));

  // Where the exported files go; kept if the refactor-diff harness asks for it.
  if (const char *dump = getenv("WXM_EXPORT_DUMP_DIR")) {
    g_outputRoot = wxString::FromUTF8(dump);
    g_keepOutput = true;
    if (!wxDirExists(g_outputRoot))
      wxMkdir(g_outputRoot);
  } else {
    g_outputRoot = wxFileName::CreateTempFileName(wxS("wxmexport"));
    wxRemoveFile(g_outputRoot);
    wxMkdir(g_outputRoot);
  }

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

  if (!g_keepOutput)
    wxFileName::Rmdir(g_outputRoot, wxPATH_RMDIR_RECURSIVE);
  wxRemoveFile(cfgFile);
  wxEntryCleanup();
  return result;
}
