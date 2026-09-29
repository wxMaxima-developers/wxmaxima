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
  Regression test: changing a toolbar button's state must repaint the toolbar.

  wxAuiToolBar::EnableTool() and SetToolBitmap() only change the tool's data;
  neither repaints. So the "return to the cell being evaluated" button, which
  the worksheet enables when the user scrolls away from the evaluation, kept
  looking disabled until the mouse pointer entered the toolbar and its hover
  highlight happened to repaint it.

  Whether a paint event actually arrives depends on the display and the
  toolkit's frame clock, which makes counting paint events flaky. Refresh() is
  the virtual that queues the repaint, though, so a subclass counting calls to
  it tells deterministically whether one was requested.
*/

#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "ToolBar.h"
#include "wxMaximaArtProvider.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxFrame *g_frame = nullptr;

//! A ToolBar that counts the repaints it is asked for
class RefreshCountingToolBar : public ToolBar {
public:
  explicit RefreshCountingToolBar(wxWindow *parent) : ToolBar(parent) {}
  void Refresh(bool eraseBackground = true,
               const wxRect *rect = nullptr) override {
    ++m_refreshCount;
    ToolBar::Refresh(eraseBackground, rect);
  }
  int m_refreshCount = 0;
};
} // namespace

SCENARIO("Enabling the follow button repaints the toolbar") {
  auto *toolbar = new RefreshCountingToolBar(g_frame);
  // The button starts out disabled: there is no evaluation to return to.
  REQUIRE_FALSE(toolbar->GetToolEnabled(ToolBar::tb_follow));

  WHEN("it is enabled, as scrolling away from the evaluation does") {
    toolbar->m_refreshCount = 0;
    toolbar->EnableTool(ToolBar::tb_follow, true);
    THEN("it is enabled and a repaint was requested") {
      REQUIRE(toolbar->GetToolEnabled(ToolBar::tb_follow));
      REQUIRE(toolbar->m_refreshCount > 0);
    }
    AND_WHEN("it is disabled again") {
      toolbar->m_refreshCount = 0;
      toolbar->EnableTool(ToolBar::tb_follow, false);
      THEN("that is repainted, too") {
        REQUIRE_FALSE(toolbar->GetToolEnabled(ToolBar::tb_follow));
        REQUIRE(toolbar->m_refreshCount > 0);
      }
    }
  }

  WHEN("it is set to the state it already has") {
    toolbar->m_refreshCount = 0;
    toolbar->EnableTool(ToolBar::tb_follow, false);
    THEN("nothing is repainted") {
      // The toolbar is updated from idle events: repainting it every time
      // would redraw it continuously.
      REQUIRE(toolbar->m_refreshCount == 0);
    }
  }
  toolbar->Destroy();
}

SCENARIO("Switching the follow button's icon repaints the toolbar") {
  auto *toolbar = new RefreshCountingToolBar(g_frame);
  toolbar->m_refreshCount = 0;
  // Maxima asks a question: the button changes to the "needs input" icon.
  toolbar->ShowUserInputBitmap();
  REQUIRE(toolbar->m_refreshCount > 0);
  toolbar->m_refreshCount = 0;
  toolbar->ShowFollowBitmap();
  REQUIRE(toolbar->m_refreshCount > 0);
  toolbar->Destroy();
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
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
