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
  Regression tests for images surviving the .wxm serialization round-trip.

  Copying cells to the clipboard is essentially the same code path as saving a
  .wxm worksheet (Format::TreeToWXM), so these tests guard both. The concrete bug
  that motivated them: ImgCell::GetCompressedImage() read the raw compressed
  buffer without waiting for a still-running background image load, so copying a
  freshly drag-dropped image serialized it as zero bytes -> a "cannot render the
  image / zero length" cell after paste. Here we build image cells in several
  formats, serialize them to .wxm and parse them back, and check the image data
  survives (byte-for-byte -- .wxm stores the compressed bytes verbatim as
  base64). A file-loaded image additionally exercises the background-load path
  that GetCompressedImage() must wait for.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/tokenzr.h>

#include "Configuration.h"
#include "WXMformat.h"
#include "Worksheet.h"
#include "cells/CellList.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/ImgCell.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
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

// Builds a small image and encodes it to the given format's bytes.
static wxMemoryBuffer EncodeImage(wxBitmapType type) {
  wxImage img(12, 8);
  unsigned char *data = img.GetData();
  for (int i = 0; i < 12 * 8 * 3; ++i)
    data[i] = static_cast<unsigned char>((i * 37 + 11) & 0xff);
  wxMemoryOutputStream stream;
  REQUIRE(img.SaveFile(stream, type));
  wxMemoryBuffer buf;
  buf.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                 stream.GetOutputStreamBuffer()->GetBufferSize());
  return buf;
}

// Serializes a one-image worksheet to .wxm and parses it back, returning the
// compressed bytes of the image in the reloaded worksheet.
static wxMemoryBuffer RoundTrip(std::unique_ptr<ImgCell> &&imgCell) {
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE);
  group->SetOutput(std::move(imgCell));

  const wxString wxm = Format::TreeToWXM(group.get());

  std::vector<wxString> lines;
  wxStringTokenizer tokenizer(wxm, wxS("\n"), wxTOKEN_RET_EMPTY);
  while (tokenizer.HasMoreTokens())
    lines.push_back(tokenizer.GetNextToken());

  auto reloaded = Format::TreeFromWXM(lines, g_cfg);
  REQUIRE(reloaded != nullptr);
  REQUIRE(reloaded->GetGroupType() == GC_TYPE_IMAGE);
  auto *img = dynamic_cast<ImgCell *>(reloaded->GetLabel());
  REQUIRE(img != nullptr);
  return img->GetCompressedImage();
}

static bool SameBytes(const wxMemoryBuffer &a, const wxMemoryBuffer &b) {
  return (a.GetDataLen() == b.GetDataLen()) &&
         (memcmp(a.GetData(), b.GetData(), a.GetDataLen()) == 0);
}

SCENARIO("Raster images survive the .wxm round-trip byte-for-byte") {
  struct Fmt { wxBitmapType type; const wxChar *ext; };
  const Fmt formats[] = {
    {wxBITMAP_TYPE_PNG, wxS("png")},
    {wxBITMAP_TYPE_JPEG, wxS("jpg")},
    {wxBITMAP_TYPE_BMP, wxS("bmp")},
  };

  for (const auto &fmt : formats) {
    const wxMemoryBuffer original = EncodeImage(fmt.type);
    REQUIRE(original.GetDataLen() > 0);

    THEN(wxString::Format(wxS("a %s image is retained"), fmt.ext).ToStdString()) {
      const wxMemoryBuffer reloaded = RoundTrip(
        std::make_unique<ImgCell>(nullptr, g_cfg, original, wxString(fmt.ext)));
      REQUIRE(reloaded.GetDataLen() > 0);      // not the "zero length" bug
      REQUIRE(SameBytes(reloaded, original));  // .wxm stores the bytes verbatim
    }
  }
}

SCENARIO("An image loaded from a file (background task) survives the .wxm round-trip") {
  // Exercises the path where the bug lived: ImgCell::GetCompressedImage() must
  // wait for the background image load before serializing, otherwise the copy
  // captures zero bytes.
  const wxMemoryBuffer png = EncodeImage(wxBITMAP_TYPE_PNG);
  const wxString file = wxFileName::CreateTempFileName(wxS("wxmimgtest")) + wxS(".png");
  {
    wxFile out(file, wxFile::write);
    REQUIRE(out.IsOpened());
    out.Write(png.GetData(), png.GetDataLen());
  }

  GIVEN("an image cell loaded from that file") {
    auto imgCell = std::make_unique<ImgCell>(nullptr, g_cfg, file, wxEmptyString,
                                             /*remove=*/false);
    THEN("its bytes still make it into the reloaded worksheet") {
      const wxMemoryBuffer reloaded = RoundTrip(std::move(imgCell));
      REQUIRE(reloaded.GetDataLen() > 0);
    }
  }
  wxRemoveFile(file);
}

SCENARIO("Cells following an image are not lost by the .wxm round-trip") {
  // Regression guard for the truncation bug: the image WXM block lacked a
  // trailing newline, and the clipboard copy (Worksheet::CopyCells) concatenates
  // each cell's WXM with no separator -- so the next cell's start marker fused
  // onto the image's end marker and got swallowed on paste. We serialize an
  // image cell followed by a code cell the same way and check both come back.
  const wxMemoryBuffer png = EncodeImage(wxBITMAP_TYPE_PNG);
  auto imgGroup = std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE);
  imgGroup->SetOutput(std::make_unique<ImgCell>(nullptr, g_cfg, png, wxS("png")));
  auto codeGroup = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("2+2;"));

  wxString wxm; // exactly how CopyCells joins cells: concatenation, no separator
  wxm += Format::TreeToWXM(imgGroup.get());
  wxm += Format::TreeToWXM(codeGroup.get());

  std::vector<wxString> lines;
  wxStringTokenizer tok(wxm, wxS("\n"), wxTOKEN_RET_EMPTY);
  while (tok.HasMoreTokens())
    lines.push_back(tok.GetNextToken());

  auto reloaded = Format::TreeFromWXM(lines, g_cfg);
  REQUIRE(reloaded != nullptr);

  THEN("both the image and the cell after it survive") {
    int count = 0;
    bool sawImage = false, sawCode = false;
    for (auto &g : OnList(reloaded.get())) {
      ++count;
      if (g.GetGroupType() == GC_TYPE_IMAGE)
        sawImage = true;
      if (g.GetGroupType() == GC_TYPE_CODE)
        sawCode = true;
    }
    REQUIRE(count == 2); // the code cell must not be swallowed by the image
    REQUIRE(sawImage);
    REQUIRE(sawCode);
  }
}

// Serializes a list of cells exactly as Worksheet::CopyCells does -- concatenate
// each cell's WXM with no separator -- and parses them back.
static std::unique_ptr<GroupCell> SerializeAndReload(
  const std::vector<GroupCell *> &cells) {
  wxString wxm;
  for (GroupCell *c : cells)
    wxm += Format::TreeToWXM(c);

  std::vector<wxString> lines;
  wxStringTokenizer tok(wxm, wxS("\n"), wxTOKEN_RET_EMPTY);
  while (tok.HasMoreTokens())
    lines.push_back(tok.GetNextToken());
  return Format::TreeFromWXM(lines, g_cfg);
}

SCENARIO("Every text cell type survives the .wxm round-trip with its content and order") {
  struct Spec { GroupType type; const wxChar *text; };
  const Spec specs[] = {
    {GC_TYPE_TITLE, wxS("The document title")},
    {GC_TYPE_SECTION, wxS("A section heading")},
    {GC_TYPE_SUBSECTION, wxS("A subsection heading")},
    {GC_TYPE_SUBSUBSECTION, wxS("A subsubsection heading")},
    {GC_TYPE_HEADING5, wxS("A level five heading")},
    {GC_TYPE_HEADING6, wxS("A level six heading")},
    {GC_TYPE_TEXT, wxS("A text/comment paragraph")},
    {GC_TYPE_CODE, wxS("factor(x^2-1);")},
  };
  const size_t n = sizeof(specs) / sizeof(specs[0]);

  std::vector<std::unique_ptr<GroupCell>> owned;
  std::vector<GroupCell *> cells;
  for (const auto &s : specs) {
    owned.push_back(std::make_unique<GroupCell>(g_cfg, s.type, wxString(s.text)));
    cells.push_back(owned.back().get());
  }

  auto reloaded = SerializeAndReload(cells);
  REQUIRE(reloaded != nullptr);

  THEN("each cell comes back with the same type and text, none dropped") {
    size_t i = 0;
    for (auto &g : OnList(reloaded.get())) {
      REQUIRE(i < n);
      REQUIRE(g.GetGroupType() == specs[i].type);
      REQUIRE(g.GetEditable() != nullptr);
      REQUIRE(g.GetEditable()->GetValue() ==
              cells[i]->GetEditable()->GetValue());
      ++i;
    }
    REQUIRE(i == n);
  }
}

SCENARIO("Code cell answers and the send-known-answers flag survive the round-trip") {
  auto withAnswer = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("read(a);"));
  withAnswer->SetAnswer(wxS("Enter a:"), wxS("42"));

  auto autoAnswer = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("read(b);"));
  autoAnswer->SetAnswer(wxS("Enter b:"), wxS("7"));
  autoAnswer->SetAutoAnswer(true); // the "send known answers" flag

  // A trailing cell: the auto-answer marker used to be written without a
  // newline, which would fuse the next cell onto it and lose it on paste.
  auto trailing = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TITLE, wxS("A cell after"));

  auto reloaded = SerializeAndReload(
    {withAnswer.get(), autoAnswer.get(), trailing.get()});
  REQUIRE(reloaded != nullptr);

  std::vector<GroupCell *> got;
  for (auto &g : OnList(reloaded.get()))
    got.push_back(&g);

  THEN("all three cells, both answers, and the flag are preserved") {
    REQUIRE(got.size() == 3); // nothing swallowed by the auto-answer cell

    REQUIRE(got[0]->GetGroupType() == GC_TYPE_CODE);
    REQUIRE(got[0]->GetAnswer(wxS("Enter a:")) == wxS("42"));
    REQUIRE_FALSE(got[0]->AutoAnswer());

    REQUIRE(got[1]->GetGroupType() == GC_TYPE_CODE);
    REQUIRE(got[1]->GetAnswer(wxS("Enter b:")) == wxS("7"));
    REQUIRE(got[1]->AutoAnswer());

    REQUIRE(got[2]->GetGroupType() == GC_TYPE_TITLE);
    REQUIRE(got[2]->GetEditable()->GetValue() ==
            trailing->GetEditable()->GetValue());
  }
}

SCENARIO("A page break does not swallow the cell that follows it") {
  auto pageBreak = std::make_unique<GroupCell>(g_cfg, GC_TYPE_PAGEBREAK);
  auto after = std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, wxS("After the break"));

  auto reloaded = SerializeAndReload({pageBreak.get(), after.get()});
  REQUIRE(reloaded != nullptr);

  std::vector<GroupCell *> got;
  for (auto &g : OnList(reloaded.get()))
    got.push_back(&g);

  THEN("both the page break and the following cell survive") {
    REQUIRE(got.size() == 2);
    REQUIRE(got[0]->GetGroupType() == GC_TYPE_PAGEBREAK);
    REQUIRE(got[1]->GetGroupType() == GC_TYPE_TEXT);
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
  wxInitAllImageHandlers(); // PNG/JPEG/GIF encoders and decoders

  g_bmp = new wxBitmap(64, 64);
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
