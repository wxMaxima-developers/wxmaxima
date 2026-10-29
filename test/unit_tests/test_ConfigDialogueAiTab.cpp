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
  The configuration dialog's AI tab: what is on it, and when it exists.

  It holds two independent things in two boxes: "Contacting AIs" (the AI
  Chat sidebar's providers and API keys), which needs the AI tools compiled
  in and a secret store, and the MCP server, which gives external tools
  access to the worksheet and needs neither -- it is part of every build and
  stores nothing secret. So the tab has to exist in every build, and the
  MCP settings have to be on it, whether or not a secret store is
  reachable. Before, the MCP settings sat on the Options tab and the AI tab
  vanished without a secret store.
*/

#include <wx/app.h>
#include <wx/bookctrl.h>
#include <wx/checkbox.h>
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/statbox.h>

#include "dialogs/ConfigDialogue.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
ConfigDialogue *g_dialog = nullptr;

//! The notebook page with the given (untranslated) title, or nullptr
wxWindow *Page(const wxString &title) {
  wxBookCtrlBase *book = g_dialog->GetBookCtrl();
  for (size_t i = 0; i < book->GetPageCount(); i++)
    if (book->GetPageText(i) == title)
      return book->GetPage(i);
  return nullptr;
}

//! The window labelled `label` anywhere on `page`, or nullptr
wxWindow *OnPage(wxWindow *page, const wxString &label) {
  return wxWindow::FindWindowByLabel(label, page);
}

const wxString mcpCheckbox =
  wxS("Let external tools read this worksheet (MCP server)");
} // namespace

SCENARIO("The AI tab exists and holds the MCP server settings") {
  wxWindow *aiPage = Page(wxS("AI"));
  REQUIRE(aiPage != nullptr);

  THEN("the MCP server settings are in the MCP server box") {
    auto *box = dynamic_cast<wxStaticBox *>(
      OnPage(aiPage, wxS("MCP server: letting external AI tools read "
                         "the worksheet")));
    REQUIRE(box != nullptr);
    wxWindow *checkbox = OnPage(box, mcpCheckbox);
    CHECK(dynamic_cast<wxCheckBox *>(checkbox) != nullptr);
    CHECK(OnPage(box, wxS("MCP server port:")) != nullptr);
  }
  THEN("the Options tab no longer has them") {
    wxWindow *optionsPage = Page(wxS("Options"));
    REQUIRE(optionsPage != nullptr);
    CHECK(OnPage(optionsPage, mcpCheckbox) == nullptr);
  }
  THEN("the \"Contacting AIs\" box exists exactly if the AI tools are compiled in") {
#ifdef WXM_USE_AI_TOOLS
    CHECK(OnPage(aiPage, wxS("Contacting AIs")) != nullptr);
#else
    CHECK(OnPage(aiPage, wxS("Contacting AIs")) == nullptr);
#endif
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

  // Keep the dialog away from the user's real configuration.
  const wxString configFile =
    wxFileName::CreateTempFileName(wxS("wxm_configaitabtest"));
  wxConfig::Set(new wxFileConfig(wxS("wxMaxima"), wxEmptyString, configFile));

  g_dialog = new ConfigDialogue(nullptr);

  const int result = Catch::Session().run(argc, argv);

  g_dialog->Destroy();
  delete wxConfig::Set(nullptr);
  wxRemoveFile(configFile);
  wxEntryCleanup();
  return result;
}
