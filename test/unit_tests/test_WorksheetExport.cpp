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
#include <wx/utils.h>
#include <wx/xml/xml.h>
#include <wx/zipstrm.h>

#include "Configuration.h"
#include "MathParser.h"
#include "worksheet/Worksheet.h"
#include "cells/GroupCell.h"
#include "cells/AnimationCell.h"

#include <algorithm>
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

static std::unique_ptr<GroupCell> ParseXmlString(const wxString &xml) {
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

static std::unique_ptr<GroupCell> ParseCorpusFile(const wxString &name) {
  const wxString path =
    wxFileName(wxString(wxS(WXM_CORPUS_DIR)), name).GetFullPath();
  return ParseXmlString(ReadTextFile(path));
}

// Sentinel texts that must be findable in every export format.
static const wxChar *const kTitleSentinel = wxS("ExportNetDocumentTitle");
static const wxChar *const kSectionSentinel = wxS("ExportNetSectionHeading");
static const wxChar *const kTextSentinel = wxS("ExportNetTextParagraph");
static const wxChar *const kCodeSentinel = wxS("factor(xexportnet^2-1);");

// A code cell whose output is *non-math* text: a warning (whose message
// contains characters that must be HTML-escaped) and a labelled string. Both
// must be exported as plain text, not routed through the equation renderer.
static const wxChar *const kStringSentinel = wxS("ExportNetString value");
static const wxChar *const kTextOutputXml = wxS(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<wxMaximaDocument version=\"1.5\" zoom=\"100\">\n"
  "<cell type=\"code\">\n"
  "<input><editor type=\"input\"><line>warnexample;</line></editor></input>\n"
  "<output>\n"
  "<mth><t breakline=\"true\" type=\"warning\">ExportNetWarning: replaced a"
  " &amp; b &lt; c</t></mth>\n"
  "</output>\n"
  "</cell>\n"
  "<cell type=\"code\">\n"
  "<input><editor type=\"input\"><line>strexample;</line></editor></input>\n"
  "<output>\n"
  "<mth><lbl altCopy=\"(%o9)&#9;\">(%o9) </lbl><st>ExportNetString value</st>"
  "</mth>\n"
  "</output>\n"
  "</cell>\n"
  "<cell type=\"code\">\n"
  "<input><editor type=\"input\"><line>textexample;</line></editor></input>\n"
  "<output>\n"
  "<mth><t breakline=\"true\">ExportNetTextOutput line: alpha=&#945; "
  "backslash=\\ nabla=&#8711; equiv=&#8801; approx=&#8776; ok</t></mth>\n"
  "</output>\n"
  "</cell>\n"
  // A labelled block of 2-D ASCII-art maths: four lines whose meaning is
  // carried entirely by their column alignment (a fraction bar drawn out of
  // hyphens, an exponent parked above the base). Math mode would collapse the
  // spaces and drop the line breaks, so this pins the verbatim block.
  "<cell type=\"code\">\n"
  "<input><editor type=\"input\"><line>asciiartexample;</line></editor></input>\n"
  "<output>\n"
  "<mth><lbl altCopy=\"(%o11)&#9;\">(%o11) </lbl>"
  "<t breakline=\"true\" type=\"ASCII-Art\">                        2</t>\n"
  "<t breakline=\"true\" type=\"ASCII-Art\">    ExportNetAsciiArt x  + 1</t>\n"
  "<t breakline=\"true\" type=\"ASCII-Art\">    ------------------------</t>\n"
  "<t breakline=\"true\" type=\"ASCII-Art\">         ExportNetAsciiArt x</t>\n"
  "</mth>\n"
  "</output>\n"
  "</cell>\n"
  "</wxMaximaDocument>\n");

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
  // Non-math text output (a warning and a string), to pin that it is exported
  // as text rather than as a <math> element or a rendered image.
  g_ws->InsertGroupCells(ParseXmlString(kTextOutputXml),
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

/*! Guard against clipped equation bitmaps in the HTML "bitmap" flavor.

  BitmapOut sizes its canvas to the rendered content, so a correctly drawn
  equation fills essentially the whole image. A scale-handling regression made
  the exporter magnify the drawing by BitmapScale (default 3) once too often, so
  at scale 3 the equation was laid down three times too large for the canvas and
  only its upper-left third (or nothing at all) survived -- the exported images
  came out mostly or entirely blank. The content assertions never noticed
  because they only look at the HTML text, not the pixels.

  For every exported equation PNG we therefore require that it contains ink at
  all, and -- for images large enough that clipping is unambiguous -- that the
  ink's bounding box covers at least half the canvas in both dimensions. Post-fix
  the large images cover >=0.97 of the width and >=0.73 of the height; the buggy
  ones dropped below 0.2 in at least one dimension or were completely blank, so
  the 0.5 threshold separates them with a wide margin. Small glyph images (a lone
  "1", a comma) legitimately leave more slack, so only the non-blank check
  applies to them.
*/
static void RequireBitmapsNotClipped(const wxString &htmlDir) {
  const wxString imgDir = htmlDir + wxS("/doc_htmlimg");
  wxArrayString pngs;
  wxDir::GetAllFiles(imgDir, &pngs, wxS("doc_*.png"), wxDIR_FILES);
  REQUIRE(pngs.GetCount() > 0);
  for (const wxString &png : pngs) {
    wxImage img;
    INFO("equation bitmap: " << png.ToStdString());
    REQUIRE(img.LoadFile(png));
    const int w = img.GetWidth(), h = img.GetHeight();
    REQUIRE(w > 0);
    REQUIRE(h > 0);

    // The top-left pixel is background (the canvas is cleared to the text
    // background colour before drawing); anything differing from it is ink.
    const unsigned char bgR = img.GetRed(0, 0), bgG = img.GetGreen(0, 0),
                        bgB = img.GetBlue(0, 0);
    int minX = w, minY = h, maxX = -1, maxY = -1;
    for (int y = 0; y < h; ++y)
      for (int x = 0; x < w; ++x) {
        const int dr = std::abs((int)img.GetRed(x, y) - bgR);
        const int dg = std::abs((int)img.GetGreen(x, y) - bgG);
        const int db = std::abs((int)img.GetBlue(x, y) - bgB);
        if (dr + dg + db > 48) {
          minX = std::min(minX, x); maxX = std::max(maxX, x);
          minY = std::min(minY, y); maxY = std::max(maxY, y);
        }
      }

    // Every exported equation must render *something*.
    REQUIRE(maxX >= 0);

    // On images big enough that a partial render is unambiguous, the ink must
    // fill a good part of the canvas the exporter sized to fit it.
    if (w >= 200 && h >= 100) {
      const double coverX = double(maxX - minX + 1) / w;
      const double coverY = double(maxY - minY + 1) / h;
      INFO("ink coverage " << coverX << " x " << coverY << " of " << w << "x" << h);
      REQUIRE(coverX >= 0.5);
      REQUIRE(coverY >= 0.5);
    }
  }
}

/*! Validate an exported HTML file with HTML Tidy, when it is installed.

  Our HTML is assembled by string concatenation, so a structural slip (an
  unbalanced tag, a badly nested element) is easy to introduce and invisible to
  the content assertions. Tidy catches those. It is optional: if the `tidy`
  binary isn't on PATH (wxExecute returns -1) the check is skipped so the suite
  still runs everywhere.

  Tidy's exit code is 0 (clean), 1 (warnings) or 2 (errors). We fail only on
  errors: warnings are advisory and version-dependent -- e.g. tidy 5.6.0
  (Ubuntu 24.04) doesn't know the HTML5 `loading` attribute our <img> tags use
  and warns about it, while newer tidy is silent. Errors are the real
  structural problems this guards against. tidy writes its messages to either
  stream depending on version, so capture both for a failing test's log.
*/
static void RequireValidHtml(const wxString &htmlPath) {
  wxArrayString out, err;
  const wxString cmd = wxS("tidy -q -e \"") + htmlPath + wxS("\"");
  const long rc = wxExecute(cmd, out, err, wxEXEC_SYNC);
  if (rc < 0)
    return; // tidy not installed -> skip cleanly
  for (const auto &line : out)
    INFO("tidy: " << line.ToStdString());
  for (const auto &line : err)
    INFO("tidy: " << line.ToStdString());
  REQUIRE(rc < 2);
}

SCENARIO("HTML export succeeds, is deterministic and contains the document") {
  BuildDocumentOnce();

  // Every equation-rendering flavor is a separate code path in the exporter;
  // bitmap and svg additionally exercise the CopyToFile image rendering.
  struct EqFormat { Configuration::htmlExportFormat format; const wxChar *name; };
  const EqFormat formats[] = {
    {Configuration::mathML, wxS("mathml_native")},
    {Configuration::mathML_mathJaX, wxS("mathml_fillin")},
    {Configuration::bitmap, wxS("bitmap")},
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
      // Non-math output must be emitted as text in every flavor: it carries
      // the .outputtext class, its message is HTML-escaped, and it is never
      // wrapped in <math> (which would turn a sentence into a run of <mo>).
      REQUIRE(html.Contains(wxS("class=\"outputtext\"")));
      REQUIRE(html.Contains(wxS("replaced a &amp; b &lt; c")));
      REQUIRE(html.Contains(kStringSentinel));
      REQUIRE_FALSE(html.Contains(wxS("<mo>ExportNetWarning")));
      // The exported HTML must be structurally valid (skipped if tidy absent).
      RequireValidHtml(dir1 + wxS("/doc.html"));
      // Image links must not dangle (broken-link regression, see helper).
      if (eq.format == Configuration::bitmap ||
          eq.format == Configuration::svg)
        RequireImgSrcsExist(html, dir1);
      // The bitmap flavor must not clip equations to a corner of the canvas
      // (a BitmapScale double-magnification regression, see helper).
      if (eq.format == Configuration::bitmap)
        RequireBitmapsNotClipped(dir1);
      // Both MathML flavors emit native <math> with the label beside it as
      // HTML. MathML Core dropped <mlabeledtr>, so it must never appear.
      if (eq.format == Configuration::mathML ||
          eq.format == Configuration::mathML_mathJaX) {
        REQUIRE(html.Contains(wxS("<math")));
        REQUIRE(html.Contains(wxS("class=\"equation\"")));
        REQUIRE(html.Contains(wxS("class=\"eqlabel\"")));
        REQUIRE_FALSE(html.Contains(wxS("mlabeledtr")));
        // MathJax must never be loaded as an unconditional <script src>.
        REQUIRE_FALSE(
          html.Contains(wxS("<script id=\"MathJax-script\" async src=")));
      }
      // The default MathML mode must be fully self-contained: no MathJax, no
      // JavaScript, no reference to any external server at all.
      if (eq.format == Configuration::mathML) {
        REQUIRE_FALSE(html.Contains(wxS("MathJax")));
        REQUIRE_FALSE(html.Contains(wxS("mathjax")));
        REQUIRE_FALSE(html.Contains(wxS("<script")));
      }
      // The fall-back mode adds MathJax, but only via feature detection.
      if (eq.format == Configuration::mathML_mathJaX) {
        REQUIRE(html.Contains(wxS("window.MathJax")));
        REQUIRE(html.Contains(wxS("loadMathJaxIfNeeded")));
      }
    }
  }
  g_cfg->HTMLequationFormat(oldFormat);
}

/*! Compile an exported .tex with a LaTeX engine, when it is installed.

  The LaTeX export is assembled by string concatenation across ~30 ToTeX() cell
  methods, so a malformed macro or an unbalanced math mode is easy to introduce
  and invisible to the content assertions -- but it breaks the whole document.
  Actually compiling it is the real guard. We exercise both engine families,
  because the preamble branches on \ifPDFTeX: `pdflatex` (inputenc path) and
  `lualatex` (fontspec + unicode-math path). Optional: if the engine isn't on
  PATH (wxExecute returns -1) that engine is skipped so the suite still runs
  everywhere (e.g. CI without a TeX toolchain). The engine runs in the .tex's
  own directory so the relative <name>_img/ graphics paths resolve; a per-engine
  jobname keeps their aux/pdf outputs from clobbering each other.
*/
static void RequireTexCompiles(const wxString &texPath, const wxString &engine) {
  const wxFileName texFile(texPath);
  wxArrayString out, err;
  wxExecuteEnv env;
  env.cwd = texFile.GetPath();
  const wxString cmd = engine +
    wxS(" -interaction=nonstopmode -halt-on-error -jobname=out_") + engine +
    wxS(" \"") + texFile.GetFullName() + wxS("\"");
  const long rc = wxExecute(cmd, out, err, wxEXEC_SYNC, &env);
  if (rc < 0)
    return; // this engine not installed -> skip cleanly
  if (rc != 0) {
    INFO("engine: " << engine.ToStdString());
    for (const auto &line : out)
      INFO(line.ToStdString());
  }
  REQUIRE(rc == 0);
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

  THEN("non-math output is emitted as text, not forced through math mode") {
    const wxString tex = ReadTextFile(dir1 + wxS("/doc.tex"));
    // The warning is emitted as \texttt{...} text ...
    REQUIRE(tex.Contains(wxS("\\texttt{%error")));
    // ... without the old "escape out of and back into math mode" hack.
    REQUIRE_FALSE(tex.Contains(wxS("\\] \\texttt{%error")));
    // A plain text output line survives too.
    REQUIRE(tex.Contains(wxS("ExportNetTextOutput")));
  }

  THEN("2-D ASCII-art output keeps its alignment in a verbatim block") {
    const wxString tex = ReadTextFile(dir1 + wxS("/doc.tex"));
    REQUIRE(tex.Contains(wxS("\\begin{verbatim}")));
    REQUIRE(tex.Contains(wxS("\\end{verbatim}")));
    // The art is emitted character for character: the indentation that parks
    // the exponent above its base, and the fraction bar as plain ASCII hyphens
    // -- the parser turns "-" into a unicode minus for the screen, and a
    // unicode minus would abort the pdfTeX run.
    REQUIRE(tex.Contains(wxS("                        2")));
    REQUIRE(tex.Contains(wxS("    ExportNetAsciiArt x  + 1")));
    REQUIRE(tex.Contains(wxS("    ------------------------")));
    // Its label is written as coloured text rather than as an equation tag,
    // which would have left an empty numbered equation in front of the block.
    REQUIRE(tex.Contains(wxS("\\textcolor{labelcolor}{\\texttt{(\\%o11)}}")));
    REQUIRE_FALSE(tex.Contains(wxS("\\tag{%o11}")));
  }

  THEN("the worksheet itself is carried inside the PDF") {
    // The .tex only says \attachfile; whether anything actually reaches the
    // PDF is decided by the LaTeX run, so this asks the finished PDF rather
    // than the source. Skipped where the tools are absent, like the compile
    // check below.
    const bool wanted = g_cfg->ExportContainsWXMX();
    g_cfg->ExportContainsWXMX(true);
    const wxString dir = MakeExportDir(wxS("tex_attach"));
    REQUIRE(g_ws->ExportToTeX(dir + wxS("/doc.tex")));
    g_cfg->ExportContainsWXMX(wanted);

    // The worksheet is written beside the .tex, which is where LaTeX looks.
    REQUIRE(wxFileExists(dir + wxS("/doc.wxmx")));

    const wxString tex = ReadTextFile(dir + wxS("/doc.tex"));
    REQUIRE(tex.Contains(wxS("\\usepackage{attachfile2}")));
    REQUIRE(tex.Contains(wxS("\\attachfile")));

    RequireTexCompiles(dir + wxS("/doc.tex"), wxS("pdflatex"));
    const wxString pdf = dir + wxS("/out_pdflatex.pdf");
    if (wxFileExists(pdf)) {
      wxArrayString out, err;
      if (wxExecute(wxS("pdfdetach -list \"") + pdf + wxS("\""), out, err,
                    wxEXEC_SYNC) == 0) {
        wxString listing;
        for (const auto &line : out)
          listing += line + wxS("\n");
        INFO(listing.ToStdString());
        REQUIRE(listing.Contains(wxS("doc.wxmx")));
      }
    }
  }

  THEN("the exported LaTeX compiles under both engine families") {
    // pdfTeX (inputenc path) and a Unicode engine (fontspec + unicode-math
    // path); each is skipped if that binary isn't installed.
    RequireTexCompiles(dir1 + wxS("/doc.tex"), wxS("pdflatex"));
    RequireTexCompiles(dir1 + wxS("/doc.tex"), wxS("lualatex"));
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

  THEN("CanCopyAsMathML offers MathML only for selections with real math") {
    // Selecting whole GroupCells or an in-output text message must not offer
    // MathML; selecting a code cell's math output must. (A whole-GroupCell
    // selection serializes to empty MathML -- GroupCell has no ToMathML -- so
    // the offer is judged on in-output selections, which is the useful case.)
    bool mathOutputOffered = false;
    GroupCell *warn = nullptr;
    for (GroupCell *g = g_ws->GetTree(); g != nullptr; g = g->GetNext()) {
      if (g->GetEditable() &&
          g->GetEditable()->ToString().Contains(wxS("warnexample")))
        warn = g;
      // Select this cell's whole output (label .. last output cell).
      Cell *out = g->GetLabel();
      if (out == nullptr)
        continue;
      Cell *outEnd = out;
      while (outEnd->GetNext() != nullptr)
        outEnd = outEnd->GetNext();
      g_ws->SetSelection(out, outEnd);
      if (g_ws->CanCopyAsMathML())
        mathOutputOffered = true;
    }
    // At least one corpus cell has genuine math output.
    REQUIRE(mathOutputOffered);

    // The plain-text warning output serializes to <mo>/<mtext> only, so it
    // must never offer MathML.
    REQUIRE(warn != nullptr);
    Cell *wout = warn->GetLabel();
    REQUIRE(wout != nullptr);
    g_ws->SetSelection(wout, wout);
    REQUIRE_FALSE(g_ws->CanCopyAsMathML());
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

SCENARIO("Per-cell output export writes one image file per selected output") {
  BuildDocumentOnce();
  g_ws->SetSelection(g_ws->GetTree(), g_ws->GetLastCellInWorksheet());

  for (const bool svg : {true, false}) {
    const wxString dir =
      MakeExportDir(svg ? wxS("outimg_svg") : wxS("outimg_png"));
    const int written = g_ws->ExportSelectionOutputToDir(dir, svg);

    THEN("one non-empty file of the requested type is written per output") {
      REQUIRE(written > 0);
      const auto snap = SnapshotDir(dir);
      // Exactly one file per exported output (names are de-duplicated).
      REQUIRE(snap.size() == static_cast<size_t>(written));
      const wxString ext = svg ? wxS(".svg") : wxS(".png");
      for (const auto &entry : snap) {
        INFO("file: " << entry.first.ToStdString());
        REQUIRE(entry.first.EndsWith(ext));
        REQUIRE_FALSE(entry.second.empty());
      }
    }
  }

  g_ws->ClearSelection();
}

SCENARIO("An animation exports to LaTeX as \\animategraphics and compiles") {
  // A small multi-frame GIF, loaded into an AnimationCell the way an exported
  // slideshow would be.
  const wxString gif =
    wxFileName(wxString(wxS(WXM_TESTFILES_DIR)), wxS("anim3frames.gif"))
      .GetFullPath();
  REQUIRE(wxFileExists(gif));
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE);
  AnimationCell anim(group.get(), g_cfg, gif, /*remove=*/false);
  REQUIRE(anim.Length() == 3);

  const wxString dir = MakeExportDir(wxS("anim"));
  std::size_t counter = 0;
  const wxString tex = GroupCell::ToTeXAnimation(&anim, dir + wxS("/doc_img"),
                                                 wxS("doc"), &counter);

  THEN("it emits \\animategraphics over all frames and writes the frames") {
    REQUIRE(tex.Contains(wxS("\\animategraphics")));
    REQUIRE(tex.Contains(wxS("{0}{2}"))); // 3 frames -> indices 0..2
    for (int i = 0; i < 3; i++)
      REQUIRE(wxFileExists(
        dir + wxString::Format(wxS("/doc_img/doc_1_frame_%d.png"), i)));
  }

  THEN("the \\animategraphics compiles with the animate package") {
    const wxString docTex = dir + wxS("/anim.tex");
    wxFFile f(docTex, wxS("wb"));
    REQUIRE(f.IsOpened());
    REQUIRE(f.Write(wxS("\\documentclass{article}\n\\usepackage{graphicx}\n"
                        "\\usepackage{animate}\n\\begin{document}\n") +
                      tex + wxS("\n\\end{document}\n"),
                    wxConvUTF8));
    f.Close();
    RequireTexCompiles(docTex, wxS("pdflatex"));
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
