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
  Events that reach a Worksheet while its wxWindow base is being destroyed.

  On wxGTK4, ~wxWindow detaches the widget from its parent, GTK unmaps it
  and emits a leave event - after ~Worksheet has already run and set
  m_configuration to null. Worksheet::OnMouseExit() then crashed wxMaxima on
  close. GTK3 doesn't do that, so the test recreates the situation another
  way: a child window of the worksheet is destroyed by the same base class
  destructor, and its destroy event sends the worksheet mouse, focus and
  size events at that very moment. Without the fix this dereferences the
  null configuration (and, under ASan, the already destroyed members).
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "Configuration.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
wxFrame *g_frame = nullptr;
} // namespace

//! Send the worksheet every event the GUI toolkit might deliver late.
static void SendLateEvents(wxWindow *worksheet) {
  for (wxEventType type : {wxEVT_LEAVE_WINDOW, wxEVT_ENTER_WINDOW,
                           wxEVT_MOTION, wxEVT_MOUSEWHEEL, wxEVT_LEFT_UP}) {
    wxMouseEvent event(type);
    event.SetEventObject(worksheet);
    worksheet->ProcessWindowEvent(event);
  }
  for (wxEventType type : {wxEVT_KILL_FOCUS, wxEVT_SET_FOCUS}) {
    wxFocusEvent event(type);
    event.SetEventObject(worksheet);
    worksheet->ProcessWindowEvent(event);
  }
  wxSizeEvent size(wxSize(100, 100));
  size.SetEventObject(worksheet);
  worksheet->ProcessWindowEvent(size);
}

SCENARIO("A worksheet ignores events that arrive while its window dies") {
  GIVEN("an interactive worksheet with a child window") {
    Configuration cfg(g_dc);
    cfg.MakeTemporary();
    cfg.SetZoomFactor(1.0);
    cfg.SetCanvasSize(wxSize(800, 600));
    auto *worksheet = new Worksheet(g_frame, wxID_ANY, &cfg, wxDefaultPosition,
                                    wxDefaultSize, /*reactToEvents=*/true);
    auto *child = new wxWindow(worksheet, wxID_ANY);
    bool sent = false;
    child->Bind(wxEVT_DESTROY, [worksheet, child, &sent](wxWindowDestroyEvent &event) {
      event.Skip();
      // Only the child's own destroy event: it propagates to the parent,
      // too, and that is the worksheet itself.
      if (event.GetEventObject() != child)
        return;
      SendLateEvents(worksheet);
      sent = true;
    });
    WHEN("the worksheet is deleted") {
      delete worksheet;
      THEN("the late events were sent and didn't crash it") {
        CHECK(sent);
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

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
