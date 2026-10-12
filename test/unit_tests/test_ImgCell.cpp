// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#define CATCH_CONFIG_RUNNER

#include <wx/sysopt.h>
#include <wx/dcmemory.h>
#include <wx/mstream.h>

#include "test_ImgCell.h"
#include "FontAttribs.cpp"
#include "nanoSVG.cpp"
#include "Image.cpp"
#include "BackgroundQueue.cpp"
#include "ImgCell.cpp"
#include "ImgCellBase.cpp"
#include "DigitGrouping.cpp"
#include "StringUtils.cpp"
#include "TestStubs.cpp"
#include "TextCell.cpp"
#include "VisiblyInvalidCell.cpp"
#include <catch2/catch.hpp>

template <typename C>
wxString HexEncoding(C &&bits)
{
  wxString output;
  for (auto ch : bits)
    output += wxString::Format("%02x", ch);
  return output;
}

SCENARIO("RTF Output represents the image") {
  wxMemoryBuffer image;
  image.AppendData(wxmaxima_art_wxmac_doc_png, wxmaxima_art_wxmac_doc_png_size);
  Configuration config;
  GroupCell group(&config, GC_TYPE_IMAGE, wxString());
  GIVEN("An image with test data") {
    ImgCell cell(&group, &config, image, "png");
    WHEN("we convert it to RTF") {
      auto rtf = cell.ToRTF();
      THEN("the RTF output ends in \"}\\n\"")
      REQUIRE(rtf.EndsWith("}\n"));
      THEN("the RTF output contains the hex encoding of the image")
      {
        rtf.Truncate(rtf.size() - 2);
        auto hex = HexEncoding(wxmaxima_art_wxmac_doc_png);
        rtf.erase(0, rtf.size() - hex.size());
        REQUIRE(rtf == hex);
      }
    }
  }
}

// GIF and XPM store transparency as a mask colour, not as alpha. ImgCell and
// AnimationCell Blit() the bitmap Image::GetBitmap() returns without asking for
// the mask, which painted the mask colour (black-ish) where the image was meant
// to be see-through (GH #2227).
SCENARIO("A masked image stays transparent when drawn like ImgCell draws it") {
  // A 40x40 image: an opaque white square in the middle of a border whose
  // pixels carry the mask colour.
  const unsigned char maskR = 1, maskG = 2, maskB = 3;
  wxImage source(40, 40);
  source.SetRGB(wxRect(0, 0, 40, 40), maskR, maskG, maskB);
  source.SetRGB(wxRect(10, 10, 20, 20), 255, 255, 255);
  source.SetMaskColour(maskR, maskG, maskB);
  wxMemoryOutputStream ostream;
  REQUIRE(source.SaveFile(ostream, wxBITMAP_TYPE_XPM));
  wxMemoryBuffer xpm;
  xpm.AppendData(ostream.GetOutputStreamBuffer()->GetBufferStart(),
                 ostream.GetSize());

  Configuration config;
  GIVEN("an XPM with a masked border") {
    Image image(&config, xpm, wxS("xpm"));
    WHEN("its screen bitmap is blitted onto a red background without a mask") {
      wxBitmap bitmap = image.GetBitmap();
      REQUIRE(bitmap.IsOk());
      wxBitmap target(bitmap.GetWidth(), bitmap.GetHeight(), 24);
      {
        wxMemoryDC targetDC(target);
        targetDC.SetBackground(*wxRED_BRUSH);
        targetDC.Clear();
        wxMemoryDC bitmapDC;
        bitmapDC.SelectObject(bitmap);
        targetDC.Blit(0, 0, bitmap.GetWidth(), bitmap.GetHeight(), &bitmapDC, 0, 0);
      }
      wxImage result = target.ConvertToImage();
      THEN("the border shows the background, not the mask colour") {
        CHECK(result.GetRed(0, 0) == 255);
        CHECK(result.GetGreen(0, 0) == 0);
        CHECK(result.GetBlue(0, 0) == 0);
      }
      THEN("the opaque middle is still drawn") {
        int cx = result.GetWidth() / 2, cy = result.GetHeight() / 2;
        CHECK(result.GetRed(cx, cy) == 255);
        CHECK(result.GetGreen(cx, cy) == 255);
        CHECK(result.GetBlue(cx, cy) == 255);
      }
    }
  }
}

class MyApp : public wxApp
{
public:
  MyApp() {
    // wxWidgets 3.3's wxApp::Initialize() pops up a *modal* "no correct
    // manifest" warning box when the running .exe lacks a Common-Controls-v6
    // manifest (GetComCtl32Version() < 610). The unit-test executables ship no
    // manifest, so on a headless CI runner that box blocks forever -- before
    // OnInit() ever runs -- and the test only dies at ctest's timeout. The app
    // object is constructed before wxApp::Initialize() reads this option, so
    // setting it here suppresses the check. (The shipped wxmaxima.exe has a
    // proper manifest and was never affected.)
    wxSystemOptions::SetOption(wxS("msw.no-manifest-check"), 1);
  }
  bool OnInit() override {
    wxImage::AddHandler(new wxPNGHandler);
    wxImage::AddHandler(new wxXPMHandler);
    int rc = Catch::Session().run();
    std::exit(rc);
    return false;
  }
};

IMPLEMENT_APP(MyApp);
