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

// TEMPORARY: flushed stderr stage markers to bisect the MSW-only CI hang of this
// test (times out at 380 s on MinGW). The last WXMARK before the timeout is the
// stage that wedges. Remove once localized.
#include <cstdio>
#define WXMARK(msg) do { std::fprintf(stderr, "WXMARK " msg "\n"); std::fflush(stderr); } while (0)

// The whole test is one translation unit (the cell .cpp files are #included
// below), so global ctors run top-to-bottom from here. These two markers bracket
// that static-init phase: if "static-init-begin" prints but "static-init-end"
// does not, a global constructor among the #included files (e.g. the file-scope
// wxString in VisiblyInvalidCell.cpp) is what wedges; if both print but
// "app:OnInit-before-catch" never does, the hang is in wxEntry/wxApp startup.
struct WxmStaticInitBegin { WxmStaticInitBegin() { WXMARK("static-init-begin"); } };
static WxmStaticInitBegin wxmStaticInitBegin;

#include "test_ImgCell.h"
#include "FontAttribs.cpp"
#include "nanoSVG.cpp"
#include "Image.cpp"
#include "BackgroundQueue.cpp"
#include "ImgCell.cpp"
#include "ImgCellBase.cpp"
#include "StringUtils.cpp"
#include "TestStubs.cpp"
#include "TextCell.cpp"
#include "VisiblyInvalidCell.cpp"
#include <catch2/catch.hpp>

struct WxmStaticInitEnd { WxmStaticInitEnd() { WXMARK("static-init-end"); } };
static WxmStaticInitEnd wxmStaticInitEnd;

template <typename C>
wxString HexEncoding(C &&bits)
{
  wxString output;
  for (auto ch : bits)
    output += wxString::Format("%02x", ch);
  return output;
}

// ... (license header omitted for brevity but preserved in replace)
SCENARIO("RTF Output represents the image") {
  WXMARK("img:scenario-enter");
  wxMemoryBuffer image;
  image.AppendData(wxmaxima_art_wxmac_doc_png, wxmaxima_art_wxmac_doc_png_size);
  Configuration config;
  WXMARK("img:config-built");
  GroupCell group(&config, GC_TYPE_IMAGE, wxString());
  GIVEN("An image with test data") {
    ImgCell cell(&group, &config, image, "png");
    WXMARK("img:cell-built");
    WHEN("we convert it to RTF") {
      WXMARK("img:before-ToRTF");
      auto rtf = cell.ToRTF();
      WXMARK("img:after-ToRTF");
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

class MyApp : public wxApp
{
public:
  bool OnInit() override {
    WXMARK("app:OnInit-before-catch");
    wxImage::AddHandler(new wxPNGHandler);
    int rc = Catch::Session().run();
    WXMARK("app:OnInit-after-catch");
    std::exit(rc);
    return false;
  }
};

IMPLEMENT_APP(MyApp);
