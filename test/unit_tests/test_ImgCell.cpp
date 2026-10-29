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
#include <wx/zstream.h>
#include <algorithm>

#include "test_ImgCell.h"
#include "FontAttribs.cpp"
#include "SvgSize.cpp"
#include "AttachmentSource.cpp"
#include "Image.cpp"
#include "BackgroundQueue.cpp"
#include "ImgCell.cpp"
#include "AnimationCell.cpp"
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

// AnimationCell::ToRTF() used to write the hex digits through a wxStringBuffer
// that had already been destroyed: into freed memory, leaving them out of the
// RTF. clang-analyzer found it.
SCENARIO("RTF Output of an animation represents its current frame") {
  wxMemoryBuffer image;
  image.AppendData(wxmaxima_art_wxmac_doc_png, wxmaxima_art_wxmac_doc_png_size);
  Configuration config;
  GroupCell group(&config, GC_TYPE_IMAGE, wxString());
  GIVEN("An animation with one frame of test data") {
    AnimationCell cell(&group, &config, image, "png");
    REQUIRE(cell.IsOk());
    WHEN("we convert it to RTF") {
      // ToRTF() is private in AnimationCell, public in Cell
      auto rtf = static_cast<const Cell &>(cell).ToRTF();
      THEN("the RTF output is a PNG picture")
        REQUIRE(rtf.StartsWith(wxS("{\\pict\\pngblip")));
      THEN("the RTF output ends in a hex-encoded PNG of the frame and \"}\\n\"")
      {
        REQUIRE(rtf.EndsWith("}\n"));
        rtf.Truncate(rtf.size() - 2);
        // The hex digits follow the last control word and its blank.
        const wxString hex = rtf.AfterLast(wxS(' '));
        REQUIRE(hex.size() > 100);
        REQUIRE(hex.size() % 2 == 0);
        wxMemoryBuffer png;
        for (size_t i = 0; i < hex.size(); i += 2) {
          unsigned long byte = 0;
          REQUIRE(hex.Mid(i, 2).ToULong(&byte, 16));
          png.AppendByte(static_cast<char>(byte));
        }
        wxMemoryInputStream stream(png.GetData(), png.GetDataLen());
        wxImage decoded;
        REQUIRE(decoded.LoadFile(stream, wxBITMAP_TYPE_PNG));
        wxMemoryInputStream originalStream(wxmaxima_art_wxmac_doc_png,
                                           wxmaxima_art_wxmac_doc_png_size);
        wxImage original;
        REQUIRE(original.LoadFile(originalStream, wxBITMAP_TYPE_PNG));
        REQUIRE(decoded.GetSize() == original.GetSize());
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

//! A 40x20 SVG with a red 20x10 rectangle in its middle, gzipped like the
//! .svgz images a .wxm file embeds.
static wxMemoryBuffer RedRectSvgz() {
  const char svg[] =
    "<?xml version=\"1.0\"?>\n"
    "<svg width=\"40\" height=\"20\" viewBox=\"0 0 40 20\" "
    "xmlns=\"http://www.w3.org/2000/svg\">"
    "<rect x=\"10\" y=\"5\" width=\"20\" height=\"10\" fill=\"#ff0000\"/></svg>";
  wxMemoryOutputStream mstream;
  {
    wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION, wxZLIB_GZIP);
    zstream.Write(svg, sizeof(svg) - 1);
  }
  wxMemoryBuffer svgz;
  svgz.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                  mstream.GetSize());
  return svgz;
}

//! Is the pixel at x, y the opaque red of RedRectSvgz()'s rectangle?
static bool IsRed(const wxImage &img, int x, int y) {
  return (img.GetRed(x, y) == 255) && (img.GetGreen(x, y) == 0) &&
    (img.GetBlue(x, y) == 0) && (!img.HasAlpha() || (img.GetAlpha(x, y) == 255));
}

// SVG images are rendered by wxWidgets (wxBitmapBundle::FromSVG()) since the
// vendored nanoSVG copy was dropped; wxWidgets can't tell the SVG's own size,
// which we therefore read from the root element.
SCENARIO("An SVG image is drawn at its own size, and re-rendered when zoomed") {
  Configuration config;
  // Room for the image even at 200%: an image is shrunk to fit the window.
  config.SetCanvasSize(wxSize(2000, 2000));
  GIVEN("an .svgz image of 40x20 pixels") {
    Image image(&config, RedRectSvgz(), wxS("svgz"));
    THEN("it knows its own size") {
      CHECK(image.GetOriginalWidth() == 40);
      CHECK(image.GetOriginalHeight() == 20);
    }
    WHEN("it is drawn at 100%") {
      wxBitmap bitmap = image.GetBitmap(1.0);
      REQUIRE(bitmap.IsOk());
      THEN("the bitmap has the SVG's size, pixel for pixel") {
        CHECK(bitmap.GetWidth() == 40);
        CHECK(bitmap.GetHeight() == 20);
        CHECK(bitmap.GetScaleFactor() == 1.0);
      }
      THEN("the rectangle is drawn where the SVG says it is") {
        wxImage img = bitmap.ConvertToImage();
        CHECK(IsRed(img, 20, 10));
        CHECK(!IsRed(img, 5, 10));
      }
    }
    WHEN("it is drawn at 200%") {
      wxBitmap bitmap = image.GetBitmap(2.0);
      REQUIRE(bitmap.IsOk());
      THEN("it is rendered at twice the size, not stretched to it") {
        CHECK(bitmap.GetWidth() == 80);
        CHECK(bitmap.GetHeight() == 40);
        CHECK(bitmap.GetScaleFactor() == 1.0);
        wxImage img = bitmap.ConvertToImage();
        // The rectangle's edge is at x=20 now. Rendered from the vector data
        // it is sharp; a 40px bitmap scaled up would blur it across x=19..20.
        CHECK(IsRed(img, 20, 20));
        CHECK(IsRed(img, 59, 20));
        CHECK(img.GetAlpha(19, 20) == 0);
      }
    }
    WHEN("its cache is cleared, as for a cell scrolled out of view") {
      REQUIRE(image.GetBitmap(1.0).IsOk());
      REQUIRE(image.HoldsSvgRendering());
      image.ClearCache();
      THEN("the parsed SVG and its full-size rendering are released") {
        CHECK(!image.HoldsSvgRendering());
      }
      THEN("it is rendered again when drawn next") {
        wxBitmap bitmap = image.GetBitmap(2.0);
        REQUIRE(bitmap.IsOk());
        CHECK(bitmap.GetWidth() == 80);
        CHECK(IsRed(bitmap.ConvertToImage(), 40, 20));
      }
    }
    WHEN("the image is copied, as copying its cell does") {
      Image copy(&config, image);
      THEN("the copy is drawn, too") {
        wxBitmap bitmap = copy.GetBitmap(1.0);
        REQUIRE(bitmap.IsOk());
        CHECK(bitmap.GetWidth() == 40);
        CHECK(IsRed(bitmap.ConvertToImage(), 20, 10));
      }
    }
  }
  GIVEN("data that isn't an SVG") {
    wxMemoryOutputStream mstream;
    {
      wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION, wxZLIB_GZIP);
      zstream.Write("not an image", 12);
    }
    wxMemoryBuffer svgz;
    svgz.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                    mstream.GetSize());
    Image image(&config, svgz, wxS("svgz"));
    THEN("the image is marked invalid instead of drawn empty") {
      CHECK(image.m_isInvalid);
    }
  }
}

// wxplot_scale: wxMaxima shows a plot larger or smaller by a factor the user
// chose. It applies after the image was fitted into the window, so a plot
// can be made wider than the window.
SCENARIO("An image's user scale applies after fitting it into the window") {
  Configuration config;
  config.SetCanvasSize(wxSize(2000, 2000));
  GIVEN("an .svgz image of 40x20 pixels with a user scale of 2") {
    Image image(&config, RedRectSvgz(), wxS("svgz"));
    REQUIRE(image.GetUserScale() == 1.0);
    image.SetUserScale(2.0);
    WHEN("it is drawn at 100%") {
      wxBitmap bitmap = image.GetBitmap(1.0);
      REQUIRE(bitmap.IsOk());
      THEN("it is rendered at twice its size, from the vector data") {
        CHECK(bitmap.GetWidth() == 80);
        CHECK(bitmap.GetHeight() == 40);
        wxImage img = bitmap.ConvertToImage();
        CHECK(IsRed(img, 20, 20));
        CHECK(img.GetAlpha(19, 20) == 0);
      }
    }
    WHEN("it is copied, as copying its cell does") {
      Image copy(&config, image);
      THEN("the copy keeps the scale") {
        CHECK(copy.GetUserScale() == 2.0);
      }
    }
    WHEN("a non-positive scale is set") {
      image.SetUserScale(0);
      image.SetUserScale(-1);
      THEN("it is ignored") {
        CHECK(image.GetUserScale() == 2.0);
      }
    }
  }
  GIVEN("a window too narrow for the image") {
    config.SetCanvasSize(wxSize(30, 2000));
    // Image::Recalculate() never fits an image into less than 10 pixels.
    const long lineWidth = std::max(config.GetLineWidth(), 10L);
    REQUIRE(lineWidth < 40);
    Image image(&config, RedRectSvgz(), wxS("svgz"));
    image.Recalculate(1.0);
    const long fittedWidth = image.m_width;
    REQUIRE(fittedWidth <= lineWidth);
    WHEN("the image gets a user scale of 3") {
      image.SetUserScale(3.0);
      image.Recalculate(1.0);
      THEN("it is three times as wide as the fitted image, wider than the window") {
        // fittedWidth was rounded down, which the factor multiplies.
        CHECK(image.m_width >= 3 * fittedWidth);
        CHECK(image.m_width <= 3 * (fittedWidth + 1));
        CHECK(image.m_width > lineWidth);
      }
    }
    WHEN("the image isn't fitted into the window (wxplot_scale:'native)") {
      image.SetFitToWindow(false);
      image.Recalculate(1.0);
      THEN("it keeps its own size, wider than the window") {
        CHECK(image.m_width == 40);
        CHECK(image.m_height == 20);
      }
      THEN("a copy keeps that") {
        Image copy(&config, image);
        CHECK_FALSE(copy.GetFitToWindow());
      }
    }
    WHEN("the image gets a user scale of 0.5") {
      image.SetUserScale(0.5);
      image.Recalculate(1.0);
      THEN("it is half as wide as the fitted image") {
        CHECK(image.m_width >= fittedWidth / 2 - 1);
        CHECK(image.m_width <= fittedWidth / 2 + 1);
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
