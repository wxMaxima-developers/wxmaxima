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
  Logic-level regression tests for wxMaxima's custom wxAccessible classes.

  Screen-reader behaviour can only be validated on Windows with an actual screen
  reader, and that feedback loop is measured in days. But every accessibility bug
  we have hit lived in the *logic* of our own wxAccessible code -- the worksheet
  accessible was never attached, its role was a plain panel instead of a
  document, the toolbar exposed no tools -- and that logic is just C++ methods
  returning values we can assert on, no screen reader required.

  These tests call those methods directly. They only do real work where
  wxWidgets was built with wxUSE_ACCESSIBILITY (the Windows CI, and any wx built
  with --enable-accessibility); on the Linux dev build (accessibility off) the
  guarded scenarios compile out and the harness sanity check still runs. This
  turns the whole logic layer from "wait days for the tester" into "seconds in
  CI".
*/

#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>
#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include "Configuration.h"
#include "ToolBar.h"
#include "worksheet/Worksheet.h"
#include "wxMaximaArtProvider.h"
#include "cells/GroupCell.h"
#include "sidebars/CharButton.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h> // sleep(), used only by the POSIX EnsureDisplay() path
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

// Runs everywhere (even with accessibility compiled out) so Catch always has a
// test case and gross construction breakage is still caught.
SCENARIO("The accessibility test harness builds the worksheet and a toolbar") {
  REQUIRE(g_ws != nullptr);
  REQUIRE(g_frame != nullptr);
  ToolBar *toolbar = new ToolBar(g_frame);
  REQUIRE(toolbar->GetToolCount() > 0);
  toolbar->Destroy();

  const CharButton::Definition def{L'\u03B1', wxS("Greek small letter alpha"),
                                   false};
  CharButton *button = new CharButton(g_frame, g_ws, g_cfg, def,
                                      /*forceShow=*/true);
  REQUIRE(button->GetTextObject() != nullptr);
  button->Destroy();
}

#if wxUSE_ACCESSIBILITY
SCENARIO("The worksheet is exposed to screen readers as a document") {
  // Regression guard for the "custom accessible never attached" bug: without the
  // SetAccessible() call the window silently falls back to the default
  // wxWindowAccessible (a generic panel with raw scrollbars).
  wxAccessible *acc = g_ws->GetTargetWindow()->GetAccessible();
  REQUIRE(acc != nullptr);

  THEN("its root reports a document role and a non-empty name") {
    wxAccRole role = wxROLE_NONE;
    REQUIRE(acc->GetRole(0, &role) == wxACC_OK);
    REQUIRE(role == wxROLE_SYSTEM_DOCUMENT);

    wxString name;
    REQUIRE(acc->GetName(0, &name) == wxACC_OK);
    REQUIRE_FALSE(name.IsEmpty());
  }

  THEN("its parent is not itself (a self-parent hangs screen readers)") {
    // Regression guard: GetParent() once returned the worksheet's own accessible
    // -- i.e. this same object -- so a screen reader walking up the parent chain
    // looped forever and froze. The parent must be something else (or unset).
    wxAccessible *parent = acc; // deliberately non-null start
    const wxAccStatus status = acc->GetParent(&parent);
    if (status == wxACC_OK)
      REQUIRE(parent != acc);
    else
      REQUIRE(parent == nullptr); // not-implemented -> framework derives it
  }
}

SCENARIO("The toolbar exposes its owner-drawn tools to screen readers") {
  // wxAuiToolBar has no accessibility of its own; ToolBarAccessible provides it.
  ToolBar *toolbar = new ToolBar(g_frame);
  wxAccessible *acc = toolbar->GetAccessible();
  REQUIRE(acc != nullptr);

  int childCount = -1;
  REQUIRE(acc->GetChildCount(&childCount) == wxACC_OK);
  REQUIRE(childCount > 0);

  THEN("at least one push-button tool carries a spoken name") {
    bool namedButton = false;
    for (int i = 1; i <= childCount; ++i) {
      wxAccRole role = wxROLE_NONE;
      wxString name;
      if ((acc->GetRole(i, &role) == wxACC_OK) &&
          (role == wxROLE_SYSTEM_PUSHBUTTON) &&
          (acc->GetName(i, &name) == wxACC_OK) && !name.IsEmpty())
        namedButton = true;
    }
    REQUIRE(namedButton);
  }
  toolbar->Destroy();
}

SCENARIO("A sidebar CharButton is exposed as a named push button") {
  // CharButton is a wxPanel wrapping a wxStaticText; without CharButtonAccessible
  // it reports as an unnamed pane. It should read as a single named push button.
  const CharButton::Definition def{L'\u03B1', wxS("Greek small letter alpha"), false};
  CharButton *button = new CharButton(g_frame, g_ws, g_cfg, def,
                                      /*forceShow=*/true);
  wxAccessible *acc = button->GetAccessible();
  REQUIRE(acc != nullptr);

  THEN("it is a push button, named by the symbol description, with no children") {
    wxAccRole role = wxROLE_NONE;
    REQUIRE(acc->GetRole(0, &role) == wxACC_OK);
    REQUIRE(role == wxROLE_SYSTEM_PUSHBUTTON);

    wxString name;
    REQUIRE(acc->GetName(0, &name) == wxACC_OK);
    REQUIRE(name == wxS("Greek small letter alpha"));

    int childCount = -1;
    REQUIRE(acc->GetChildCount(&childCount) == wxACC_OK);
    REQUIRE(childCount == 0);
  }
  button->Destroy();
}
#endif // wxUSE_ACCESSIBILITY

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
  // The toolbar draws itself from this art provider; push it so tool creation
  // behaves as it does in the real application.
  wxArtProvider::Push(new wxMaximaArtProvider);

  g_bmp = new wxBitmap(400, 400);
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
