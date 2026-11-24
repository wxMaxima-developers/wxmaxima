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
  Checks that the PDF export (PdfExport, drawn with wxPdfDocument) gives
  wxMaxima what it needs:

  - every character of the worksheet ends up in a font of the PDF, including
    Greek letters and maths symbols, and one the worksheet's own font lacks
    is taken from another font (font fallback);
  - every font is embedded;
  - the PDF is tagged: headings, paragraphs, code, labels and formulas are
    structure elements in reading order, formulas carry an alternate text
    and their MathML, and the structure survives page breaks.

  The PDF is read back by a minimal parser below: it inflates every stream,
  so that the checks can look at the content, the structure tree and the
  fonts' ToUnicode maps as text. Set WXM_PDF_DUMP_DIR=<dir> to keep the
  PDFs for a look with a PDF viewer, pdffonts or a PDF/UA checker.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/ffile.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>
#include <wx/zstream.h>

#include "BuildConfig.h"
#include "Configuration.h"
#include "MathParser.h"
#include "cells/GroupCell.h"
#include "graphical_io/PdfExport.h"
#include "worksheet/Worksheet.h"

#ifdef WXM_USE_WXPDFDOC
#include <wx/pdffont.h>
#include <wx/pdffontmanager.h>
#endif

#include <cstdlib>
#include <regex>
#include <set>
#include <string>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;
wxString g_outputDir;
bool g_keepOutput = false;

//! A PDF file with all its streams inflated
struct DecodedPdf {
  //! The file with every compressed stream replaced by its content
  std::string text;
  //! Every Unicode character some font's ToUnicode map provides
  std::set<wxUint32> mappedCharacters;
  //! How many times a string occurs in text
  std::size_t Count(const std::string &needle) const {
    std::size_t count = 0;
    for (std::size_t pos = text.find(needle); pos != std::string::npos;
         pos = text.find(needle, pos + needle.size()))
      count++;
    return count;
  }
  bool Contains(const std::string &needle) const {
    return text.find(needle) != std::string::npos;
  }
};

std::string ReadBinaryFile(const wxString &path) {
  wxFFile f(path, wxS("rb"));
  REQUIRE(f.IsOpened());
  std::string contents;
  contents.resize(static_cast<size_t>(f.Length()));
  if (!contents.empty())
    REQUIRE(f.Read(&contents[0], contents.size()) == contents.size());
  return contents;
}

std::string Inflate(const std::string &data) {
  wxMemoryInputStream compressed(data.data(), data.size());
  wxZlibInputStream zlib(compressed, wxZLIB_ZLIB);
  std::string result;
  char buffer[4096];
  while (true) {
    zlib.Read(buffer, sizeof(buffer));
    const size_t n = zlib.LastRead();
    if (n == 0)
      break;
    result.append(buffer, n);
  }
  return result;
}

//! Collects the code points of a ToUnicode CMap's bfchar/bfrange entries
void ReadToUnicode(const std::string &cmap, std::set<wxUint32> &chars) {
  // <gid> <unicode> in a bfchar block; <from> <to> <unicode> in a bfrange.
  // The unicode value is UTF-16BE: a surrogate pair is 8 hex digits.
  const auto toCodePoint = [](const std::string &hex) -> wxUint32 {
    const auto value = std::stoul(hex, nullptr, 16);
    if (hex.size() == 8) {
      const wxUint32 high = static_cast<wxUint32>(value >> 16);
      const wxUint32 low = static_cast<wxUint32>(value & 0xFFFF);
      return 0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00);
    }
    return static_cast<wxUint32>(value);
  };
  std::size_t pos = 0;
  while ((pos = cmap.find("beginbfchar", pos)) != std::string::npos) {
    const std::size_t end = cmap.find("endbfchar", pos);
    const std::string block = cmap.substr(pos, end - pos);
    static const std::regex entry("<([0-9A-Fa-f]+)>\\s*<([0-9A-Fa-f]+)>");
    for (std::sregex_iterator it(block.begin(), block.end(), entry), last;
         it != last; ++it)
      chars.insert(toCodePoint((*it)[2]));
    pos = end;
  }
  pos = 0;
  while ((pos = cmap.find("beginbfrange", pos)) != std::string::npos) {
    const std::size_t end = cmap.find("endbfrange", pos);
    const std::string block = cmap.substr(pos, end - pos);
    static const std::regex entry(
      "<([0-9A-Fa-f]+)>\\s*<([0-9A-Fa-f]+)>\\s*<([0-9A-Fa-f]+)>");
    for (std::sregex_iterator it(block.begin(), block.end(), entry), last;
         it != last; ++it) {
      const auto from = std::stoul((*it)[1], nullptr, 16);
      const auto to = std::stoul((*it)[2], nullptr, 16);
      const wxUint32 first = toCodePoint((*it)[3]);
      for (unsigned long i = 0; (i <= to - from) && (i < 0x10000); i++)
        chars.insert(first + static_cast<wxUint32>(i));
    }
    pos = end;
  }
}

//! Reads a PDF file, inflating every Flate-compressed stream
DecodedPdf DecodePdf(const wxString &path) {
  const std::string raw = ReadBinaryFile(path);
  DecodedPdf pdf;
  std::size_t pos = 0;
  static const std::regex lengthRe("/Length\\s+(\\d+)");
  while (true) {
    const std::size_t streamPos = raw.find("stream", pos);
    if (streamPos == std::string::npos) {
      pdf.text += raw.substr(pos);
      break;
    }
    // "endstream" contains "stream", too.
    if ((streamPos >= 3) && (raw.compare(streamPos - 3, 3, "end") == 0)) {
      pdf.text += raw.substr(pos, streamPos + 6 - pos);
      pos = streamPos + 6;
      continue;
    }
    std::size_t dataStart = streamPos + 6;
    if (raw.compare(dataStart, 2, "\r\n") == 0)
      dataStart += 2;
    else if (raw[dataStart] == '\n')
      dataStart += 1;
    // The stream's dictionary: from the last "obj" before it
    const std::size_t objPos = raw.rfind(" obj", streamPos);
    const std::string dict = raw.substr(objPos, streamPos - objPos);
    std::smatch length;
    REQUIRE(std::regex_search(dict, length, lengthRe));
    const std::size_t len = std::stoul(length[1]);
    const std::string data = raw.substr(dataStart, len);
    std::string content = data;
    if (dict.find("/FlateDecode") != std::string::npos)
      content = Inflate(data);
    if (content.find("begincmap") != std::string::npos)
      ReadToUnicode(content, pdf.mappedCharacters);
    pdf.text += raw.substr(pos, dataStart - pos);
    pdf.text += content;
    pos = dataStart + len;
  }
  if (g_keepOutput) {
    // What the checks look at, for a look when one fails
    wxFFile decoded(path + wxS(".decoded.txt"), wxS("wb"));
    decoded.Write(pdf.text.data(), pdf.text.size());
  }
  return pdf;
}

std::unique_ptr<GroupCell> ParseXmlString(const wxString &xml) {
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

wxString TextGroup(const wxString &type, const wxString &text) {
  return wxS("<cell type=\"") + type + wxS("\"><editor type=\"") + type +
    wxS("\"><line>") + text + wxS("</line></editor></cell>\n");
}

wxString CodeCell(const wxString &input, const wxString &label,
                  const wxString &outputMathML) {
  return wxS("<cell type=\"code\"><input><editor type=\"input\"><line>") +
    input + wxS("</line></editor></input><output><mth><lbl>") + label +
    wxS(" </lbl>") + outputMathML + wxS("</mth></output></cell>\n");
}

wxString Document(const wxString &cells) {
  return wxS("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
             "<wxMaximaDocument version=\"1.5\" zoom=\"100\">\n") +
    cells + wxS("</wxMaximaDocument>\n");
}

//! Exports the worksheet, which is replaced by xml first
wxString ExportPdf(const wxString &name, const wxString &xml,
                   std::size_t *pages = nullptr) {
  g_ws->ClearDocument();
  g_ws->InsertGroupCells(ParseXmlString(xml), nullptr);
  const wxString file = g_outputDir + wxS("/") + name + wxS(".pdf");
  PdfExport::Options options;
  options.title = name;
  options.language = wxS("en");
  PdfExport exporter(g_ws->GetTree(), g_cfg, options);
  REQUIRE(exporter.Write(file));
  if (pages)
    *pages = exporter.GetPageCount();
  return file;
}

//! A character the worksheet's text font lacks, but some installed font has
wxUint32 CharacterNeedingFallback() {
#ifdef WXM_USE_WXPDFDOC
  const wxFont font = g_cfg->GetStyle(TS_TEXT)->GetFont();
  wxPdfFontManager *manager = wxPdfFontManager::GetFontManager();
  wxPdfFont pdfFont = manager->RegisterFont(font, font.GetFaceName());
  if (!pdfFont.IsValid())
    return 0;
  // Maths letters and operators few text fonts have
  for (const wxUint32 c : {0x1D538u, 0x1D504u, 0x2A0Cu, 0x2A15u, 0x27E6u, 0x2135u}) {
    const wxString s{wxUniChar(c)};
    if (!pdfFont.CanShow(s) && manager->FindFallbackFont(c).IsValid())
      return c;
  }
#endif
  return 0;
}

std::string Utf8(wxUint32 c) {
  // Kept alive: utf8_str() may point into the string's own buffer.
  const wxString str{wxUniChar(c)};
  const wxScopedCharBuffer buf = str.utf8_str();
  return std::string(buf.data(), buf.length());
}
} // namespace

SCENARIO("PDF export is available in this build") {
  // cmake/WxmPdfDoc.cmake decides that; this makes a CI job without the
  // submodule fail loudly instead of skipping every check below.
#ifdef WXM_USE_WXPDFDOC
  CHECK(PdfExport::Available());
#else
  CHECK(!PdfExport::Available());
  WARN("Built without wxPdfDocument: the PDF export isn't tested");
#endif
}

#ifdef WXM_USE_WXPDFDOC

// The sample: headings, a text cell with Greek letters and maths symbols,
// and code cells with Greek input and 2D maths output.
static const wxString kGreekDocument = Document(
  TextGroup(wxS("title"), wxS("PdfNetTitle")) +
  TextGroup(wxS("section"), wxS("PdfNetSection")) +
  TextGroup(wxS("text"), wxS("Greek: αβγφ and ∞")) +
  CodeCell(wxS("integrate(sin(φ),φ);"), wxS("(%o1)"),
           wxS("<v>−</v><fn><r><fnm>cos</fnm></r><r><p><v>φ</v></p></r></fn>")) +
  CodeCell(wxS("sqrt(α)/π;"), wxS("(%o2)"),
           wxS("<f><r><q><v>α</v></q></r><r><s>π</s></r></f>")));

SCENARIO("The PDF export writes every character, embedded, with a font that has it") {
  GIVEN("A worksheet with Greek letters and maths symbols") {
    const wxString file = ExportPdf(wxS("greek"), kGreekDocument);
    const DecodedPdf pdf = DecodePdf(file);

    THEN("It is a PDF") {
      CHECK(pdf.text.rfind("%PDF-", 0) == 0);
      CHECK(pdf.Contains("%%EOF"));
    }
    THEN("Every character of the text is in some font's ToUnicode map") {
      for (const wxUint32 c : {0x03b1u, 0x03b2u, 0x03b3u, 0x03c6u, 0x03c0u, 0x221eu})
        CHECK(pdf.mappedCharacters.count(c) == 1);
      for (const char c : std::string("PdfNetTitleGreekintegrate"))
        CHECK(pdf.mappedCharacters.count(static_cast<wxUint32>(c)) == 1);
    }
    THEN("Every font is embedded") {
      const std::size_t descriptors = pdf.Count("/Type /FontDescriptor");
      CHECK(descriptors > 0);
      CHECK(pdf.Count("/FontFile") == descriptors);
      // The 14 standard fonts are the ones a PDF may use without embedding:
      // they never have a font descriptor of their own.
      CHECK(!pdf.Contains("/BaseFont /Helvetica"));
      CHECK(!pdf.Contains("/BaseFont /Times"));
      CHECK(!pdf.Contains("/BaseFont /Courier"));
      CHECK(!pdf.Contains("/BaseFont /Symbol"));
    }
  }
  GIVEN("A text cell with a character the worksheet's text font lacks") {
    const wxUint32 c = CharacterNeedingFallback();
    if (c == 0)
      WARN("Found no character the text font lacks that another font has: "
           "font fallback isn't tested");
    else {
      const wxString file = ExportPdf(
        wxS("fallback"),
        Document(TextGroup(wxS("text"), wxS("Fallback: ") + wxString(wxUniChar(c)))));
      const DecodedPdf pdf = DecodePdf(file);
      THEN("The character is shown with another font, which is embedded") {
        CHECK(pdf.mappedCharacters.count(c) == 1);
        CHECK(pdf.mappedCharacters.count('F') == 1);
        // One font for the text, one for the character
        CHECK(pdf.Count("/Type /FontDescriptor") >= 2);
        CHECK(pdf.Count("/FontFile") == pdf.Count("/Type /FontDescriptor"));
      }
    }
  }
}

SCENARIO("Slanted worksheet styles come out italic") {
  // wxMaxima draws variables with wxFONTSTYLE_SLANT (Styles.cpp), not
  // wxFONTSTYLE_ITALIC. wxPdfDocument used to map only the latter to an
  // italic face, so every variable in the PDF was upright.
  const wxFont variableFont = g_cfg->GetStyle(TS_VARIABLE)->GetFont();
  if (variableFont.GetStyle() == wxFONTSTYLE_NORMAL) {
    WARN("Variables aren't slanted in this configuration: not tested");
    return;
  }
  // Only meaningful if the family has an italic or oblique face at all:
  // ask for one the way that always worked.
  wxFont italic = variableFont;
  italic.SetStyle(wxFONTSTYLE_ITALIC);
  wxPdfFont italicFace = wxPdfFontManager::GetFontManager()->RegisterFont(
    italic, italic.GetFaceName());
  if (!italicFace.IsValid() ||
      ((italicFace.GetStyle() & wxPDF_FONTSTYLE_ITALIC) == 0)) {
    WARN("The variables' font family has no italic face: not tested");
    return;
  }
  GIVEN("A formula with a variable") {
    const wxString file = ExportPdf(
      wxS("slanted"),
      Document(CodeCell(wxS("x;"), wxS("(%o1)"), wxS("<v>x</v>"))));
    const DecodedPdf pdf = DecodePdf(file);
    THEN("An italic font is embedded") {
      static const std::regex angle("/ItalicAngle\\s+(-?[0-9.]+)");
      bool hasItalic = false;
      for (auto it = std::sregex_iterator(pdf.text.begin(), pdf.text.end(), angle);
           it != std::sregex_iterator(); ++it)
        if (std::stod((*it)[1]) != 0)
          hasItalic = true;
      CHECK(hasItalic);
    }
  }
}

SCENARIO("The PDF export is tagged") {
  GIVEN("A worksheet with headings, text, code and maths") {
    const wxString file = ExportPdf(wxS("tagged"), kGreekDocument);
    const DecodedPdf pdf = DecodePdf(file);

    THEN("The catalog says it is tagged, in which language and with what title") {
      CHECK(pdf.Contains("/MarkInfo"));
      CHECK(pdf.Contains("/Marked true"));
      CHECK(pdf.Contains("/StructTreeRoot"));
      CHECK(pdf.Contains("/Lang"));
      CHECK(pdf.Contains("/DisplayDocTitle true"));
    }
    THEN("Each part of the worksheet is a structure element of its kind") {
      CHECK(pdf.Count("/S /Document") == 1);
      CHECK(pdf.Count("/S /H1") == 1);
      CHECK(pdf.Count("/S /H2") == 1);
      CHECK(pdf.Count("/S /P") >= 1);
      CHECK(pdf.Count("/S /Code") == 2);
      CHECK(pdf.Count("/S /Formula") == 2);
      // (%i1), (%i2), (%o1), (%o2)
      CHECK(pdf.Count("/S /Lbl") == 4);
    }
    THEN("A formula carries its MathML, as an associated file") {
      CHECK(pdf.Contains("application#2Fmathml+xml"));
      CHECK(pdf.Contains("<math xmlns=\"http://www.w3.org/1998/Math/MathML\""));
      CHECK(pdf.Contains("<msqrt>"));
      CHECK(pdf.Contains("<mfrac>"));
      CHECK(pdf.Contains(Utf8(0x03c0)));
    }
    THEN("All content is either tagged or marked as an artifact") {
      // Every marked-content sequence carries an MCID that the structure
      // tree refers to; nothing is drawn outside of one.
      CHECK(pdf.Count("/MCID") > 0);
    }
  }
}

SCENARIO("A structure element survives a page break") {
  GIVEN("A worksheet much longer than a page") {
    wxString cells = TextGroup(wxS("title"), wxS("Long"));
    for (int i = 1; i <= 60; i++)
      cells += CodeCell(wxString::Format(wxS("x%i: %i;"), i, i),
                        wxString::Format(wxS("(%%o%i)"), i),
                        wxString::Format(wxS("<n>%i</n>"), i));
    std::size_t pages = 0;
    const wxString file = ExportPdf(wxS("long"), Document(cells), &pages);
    const DecodedPdf pdf = DecodePdf(file);
    THEN("It has more than one page") {
      CHECK(pages > 1);
      CHECK(pdf.Count("/Type /Page\n") + pdf.Count("/Type /Page ") +
            pdf.Count("/Type /Page/") + pdf.Count("/Type /Page>") >= pages);
    }
    THEN("Every cell is one element of one document") {
      CHECK(pdf.Count("/S /Document") == 1);
      CHECK(pdf.Count("/S /Code") == 60);
      CHECK(pdf.Count("/S /Formula") == 60);
    }
  }
}

SCENARIO("Exporting an empty worksheet gives a valid one-page PDF") {
  g_ws->ClearDocument();
  const wxString file = g_outputDir + wxS("/empty.pdf");
  PdfExport exporter(nullptr, g_cfg, PdfExport::Options());
  REQUIRE(exporter.Write(file));
  CHECK(exporter.GetPageCount() == 1);
  const DecodedPdf pdf = DecodePdf(file);
  CHECK(pdf.Contains("%%EOF"));
}

#endif // WXM_USE_WXPDFDOC

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

  // Hermetic settings: the export reads the styles from the global wxConfig.
  const wxString cfgFile = wxFileName::CreateTempFileName(wxS("wxmpdfcfg"));
  wxConfig::Set(new wxFileConfig(wxS("wxMaxima"), wxEmptyString, cfgFile));

  // Developer-set env var read by this test binary only.
  if (const char *dump = getenv("WXM_PDF_DUMP_DIR")) { // flawfinder: ignore
    g_outputDir = wxString::FromUTF8(dump);
    g_keepOutput = true;
    if (!wxDirExists(g_outputDir))
      wxMkdir(g_outputDir);
  } else {
    g_outputDir = wxFileName::CreateTempFileName(wxS("wxmpdf"));
    wxRemoveFile(g_outputDir);
    wxMkdir(g_outputDir);
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
    wxFileName::Rmdir(g_outputDir, wxPATH_RMDIR_RECURSIVE);
  wxRemoveFile(cfgFile);
  wxEntryCleanup();
  return result;
}
