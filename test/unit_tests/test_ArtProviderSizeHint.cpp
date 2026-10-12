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
  Regression test for GH #2483: wxMaxima built against wxQt 3.2 aborted on
  startup.

  wxQt 3.2 has no native art provider, so wxArtProvider::GetSizeHint() answers
  wxDefaultSize there for every client, including wxART_TOOLBAR. Our SVG
  toolbar icons were then created with that "size", which gave bitmaps
  without any pixels: invisible icons, and the disabled "follow" button failed
  an assertion on every repaint when wxAUI tried to grey it out.

  This test reproduces the port's behaviour on every port by pushing an art
  provider that knows no size hint and no images on top of ours, as wxQt 3.2's
  default provider does.
*/

#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/bmpbndl.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "wxMaximaArtProvider.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxFrame *g_frame = nullptr;

//! An art provider that, like wxQt 3.2's, doesn't know any size hint
class NoSizeHintArtProvider : public wxArtProvider {
protected:
  wxSize DoGetSizeHint(const wxArtClient &) override { return wxDefaultSize; }
  wxBitmapBundle CreateBitmapBundle(const wxArtID &, const wxArtClient &,
                                    const wxSize &) override {
    return wxBitmapBundle();
  }
};
} // namespace

SCENARIO("Toolbar icons have pixels even if the port knows no size hint") {
  GIVEN("a port that answers wxDefaultSize when asked for a toolbar's icon size") {
    REQUIRE(wxArtProvider::GetSizeHint(wxART_TOOLBAR) == wxDefaultSize);
    WHEN("the follow button's icon is requested for the toolbar") {
      const wxBitmapBundle bundle = wxArtProvider::GetBitmapBundle(
        wxmaximaART_ARROW_UP_SQUARE, wxART_TOOLBAR);
      REQUIRE(bundle.IsOk());
      THEN("it has a real size") {
        REQUIRE(bundle.GetDefaultSize().GetWidth() > 0);
        REQUIRE(bundle.GetDefaultSize().GetHeight() > 0);
      }
      THEN("the bitmap drawn for a window can be greyed out") {
        // What wxAuiToolBarItem::GetCurrentBitmapFor() does for a disabled
        // tool that has no separate disabled bitmap.
        const wxBitmap bitmap = bundle.GetBitmapFor(g_frame);
        REQUIRE(bitmap.IsOk());
        REQUIRE(bitmap.GetWidth() > 0);
        REQUIRE(bitmap.GetHeight() > 0);
        REQUIRE(bitmap.ConvertToImage().IsOk());
      }
    }
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
  wxArtProvider::Push(new wxMaximaArtProvider);
  wxArtProvider::Push(new NoSizeHintArtProvider);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
