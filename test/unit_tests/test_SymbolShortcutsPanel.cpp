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
  Keyboard shortcuts for sidebar symbols (GH #2578), from the configuration
  dialogue's table to the worksheet: the table, the configuration, the
  shortcut dialog, a sidebar button with a symbol of several characters, and
  the worksheet typing the symbol when its shortcut is pressed.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>

#include "Configuration.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "dialogs/SymbolShortcutDialog.h"
#include "dialogs/SymbolShortcutsPanel.h"
#include "sidebars/CharButton.h"
#include "sidebars/UnicodeSidebar.h"
#include "worksheet/Worksheet.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
wxBitmap *g_bmp = nullptr;
wxMemoryDC *g_dc = nullptr;
Configuration *g_cfg = nullptr;
Worksheet *g_ws = nullptr;
wxFrame *g_frame = nullptr;

const wxString tMacron = wxString(wxS("t")) + wxUniChar(0x0304);
const wxString alpha(wxUniChar(0x03B1));
const wxString infinity(wxUniChar(0x221E));

wxString Raw(const wxString &shortcut) { return wxm::NormalizeShortcut(shortcut); }

wxKeyEvent KeyDown(int keyCode, bool ctrl, bool alt) {
  wxKeyEvent event(wxEVT_KEY_DOWN);
  event.m_keyCode = keyCode;
  event.SetControlDown(ctrl);
  event.SetAltDown(alt);
  return event;
}

//! A fresh configuration state for each scenario
void ResetSymbols() {
  g_cfg->SymbolPaneAdditionalChars(wxS("Ø") + tMacron);
  g_cfg->SymbolShortcuts({});
  g_cfg->GreekSidebar_ShowLatinLookalikes(false);
}
} // namespace

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);

SCENARIO("The table lists the extra symbols and every symbol with a shortcut") {
  ResetSymbols();
  g_cfg->SetShortcutForSymbol(tMacron, Raw(wxS("Ctrl+Alt+T")));
  g_cfg->SetShortcutForSymbol(alpha, Raw(wxS("Ctrl+Alt+A")));
  SymbolShortcutsPanel *panel = new SymbolShortcutsPanel(g_frame, g_cfg);

  THEN("the extra symbols come first, then the built-in ones with a shortcut") {
    const std::vector<SymbolShortcutsPanel::Row> expected{
      {wxS("Ø"), {}}, {tMacron, Raw(wxS("Ctrl+Alt+T"))}, {alpha, Raw(wxS("Ctrl+Alt+A"))}};
    CHECK(panel->GetRows() == expected);
  }
  THEN("the table says where each symbol is shown") {
    CHECK(SymbolShortcutsPanel::Locate(alpha, g_cfg) ==
          SymbolShortcutsPanel::Location::GreekLowercase);
    CHECK(SymbolShortcutsPanel::Locate(wxUniChar(0x0393), g_cfg) ==
          SymbolShortcutsPanel::Location::GreekUppercase);
    CHECK(SymbolShortcutsPanel::Locate(infinity, g_cfg) ==
          SymbolShortcutsPanel::Location::SymbolsBuiltIn);
    CHECK(SymbolShortcutsPanel::Locate(tMacron, g_cfg) ==
          SymbolShortcutsPanel::Location::SymbolsExtra);
  }
  THEN("a Greek letter the sidebar hides is an extra symbol, or it wouldn't be shown at all") {
    const wxString capitalAlpha(wxUniChar(0x0391));
    CHECK(SymbolShortcutsPanel::Locate(capitalAlpha, g_cfg) ==
          SymbolShortcutsPanel::Location::SymbolsExtra);
    g_cfg->GreekSidebar_ShowLatinLookalikes(true);
    CHECK(SymbolShortcutsPanel::Locate(capitalAlpha, g_cfg) ==
          SymbolShortcutsPanel::Location::GreekUppercase);
    g_cfg->GreekSidebar_ShowLatinLookalikes(false);
  }
  panel->Destroy();
}

SCENARIO("Editing the table changes the configuration only when written") {
  ResetSymbols();
  SymbolShortcutsPanel *panel = new SymbolShortcutsPanel(g_frame, g_cfg);

  WHEN("several symbols are pasted into a new row") {
    panel->AddRow();
    panel->SetSymbolText(2, alpha + wxS("§") + wxS("Ø"));
    THEN("they become a row each, and a symbol listed already isn't added twice") {
      const std::vector<SymbolShortcutsPanel::Row> expected{
        {wxS("Ø"), {}}, {tMacron, {}}, {alpha, {}}, {wxS("§"), {}}};
      CHECK(panel->GetRows() == expected);
    }
    AND_WHEN("they get shortcuts and the table is written") {
      panel->SetShortcut(2, Raw(wxS("Ctrl+Alt+A")));
      panel->SetShortcut(1, Raw(wxS("Ctrl+Alt+T")));
      CHECK(g_cfg->SymbolShortcuts().empty());
      panel->Write(g_cfg);
      THEN("the built-in α only gets a shortcut, § becomes an extra symbol") {
        CHECK(g_cfg->SymbolPaneAdditionalChars() == wxS("Ø") + tMacron + wxS("§"));
        CHECK(g_cfg->ShortcutForSymbol(alpha) == Raw(wxS("Ctrl+Alt+A")));
        CHECK(g_cfg->ShortcutForSymbol(tMacron) == Raw(wxS("Ctrl+Alt+T")));
      }
    }
  }
  WHEN("a shortcut is given to a second symbol") {
    panel->SetShortcut(0, Raw(wxS("Ctrl+Alt+X")));
    panel->SetShortcut(1, Raw(wxS("Ctrl+Alt+X")));
    THEN("the first one loses it: a shortcut types one symbol") {
      CHECK(panel->GetRows()[0].shortcut.IsEmpty());
      CHECK(panel->GetRows()[1].shortcut == Raw(wxS("Ctrl+Alt+X")));
    }
  }
  WHEN("a row's symbol is deleted") {
    panel->SetSymbolText(0, wxEmptyString);
    THEN("the row goes") {
      REQUIRE(panel->GetRows().size() == 1);
      CHECK(panel->GetRows()[0].symbol == tMacron);
    }
  }
  panel->Destroy();
}

SCENARIO("A shortcut belongs to one symbol") {
  ResetSymbols();
  g_cfg->SetShortcutForSymbol(alpha, Raw(wxS("Ctrl+Alt+A")));
  g_cfg->SetShortcutForSymbol(tMacron, Raw(wxS("Ctrl+Alt+A")));
  CHECK(g_cfg->ShortcutForSymbol(alpha).IsEmpty());
  CHECK(g_cfg->ShortcutForSymbol(tMacron) == Raw(wxS("Ctrl+Alt+A")));
  g_cfg->SetShortcutForSymbol(tMacron, wxEmptyString);
  CHECK(g_cfg->SymbolShortcuts().empty());
}

SCENARIO("The shortcut dialog records keys and warns about clashes") {
  ResetSymbols();
  g_cfg->SetShortcutForSymbol(alpha, Raw(wxS("Ctrl+Alt+A")));
  wxMenuBar *menuBar = new wxMenuBar;
  wxMenu *menu = new wxMenu;
  menu->Append(wxID_ANY, wxS("&Evaluate\tCtrl+Alt+E"));
  menuBar->Append(menu, wxS("&Cell"));
  g_frame->SetMenuBar(menuBar);

  SymbolShortcutDialog dialog(
    g_frame, tMacron, {},
    SymbolShortcutDialog::MakeUsageLookup(g_frame, g_cfg->SymbolShortcuts(), tMacron));
  THEN("a usable combination is recorded") {
    CHECK(dialog.RecordKey(KeyDown('T', true, true)));
    CHECK(wxm::SameShortcut(dialog.GetShortcut(), wxS("Ctrl+Alt+T")));
  }
  THEN("a key needed for typing or for the dialog is not") {
    CHECK(!dialog.RecordKey(KeyDown('T', false, false)));
    CHECK(!dialog.RecordKey(KeyDown(WXK_RETURN, false, false)));
    CHECK(!dialog.RecordKey(KeyDown(WXK_CONTROL, true, false)));
    CHECK(dialog.GetShortcut().IsEmpty());
  }
  THEN("the menu entry with the same shortcut is found, without its mnemonic") {
    CHECK(SymbolShortcutDialog::MenuItemUsingShortcut(menuBar, Raw(wxS("Ctrl+Alt+E"))) ==
          wxS("Evaluate"));
    CHECK(SymbolShortcutDialog::MenuItemUsingShortcut(menuBar, Raw(wxS("Ctrl+Alt+T")))
          .IsEmpty());
    CHECK(SymbolShortcutDialog::FindMenuBar(g_ws) == menuBar);
  }
  THEN("both kinds of clash are explained") {
    auto usage =
      SymbolShortcutDialog::MakeUsageLookup(g_frame, g_cfg->SymbolShortcuts(), tMacron);
    CHECK(usage(Raw(wxS("Ctrl+Alt+E"))).Contains(wxS("Evaluate")));
    CHECK(usage(Raw(wxS("Ctrl+Alt+A"))).Contains(alpha));
    CHECK(usage(Raw(wxS("Ctrl+Alt+T"))).IsEmpty());
  }
  g_frame->SetMenuBar(nullptr);
  delete menuBar;
}

SCENARIO("The worksheet types a symbol when its shortcut is pressed") {
  ResetSymbols();
  g_cfg->SetShortcutForSymbol(tMacron, Raw(wxS("Ctrl+Alt+T")));
  auto group = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("f()"));
  GroupCell *cell = g_ws->InsertGroupCells(std::move(group), nullptr);
  EditorCell *editor = cell->GetEditable();
  REQUIRE(editor);
  g_ws->SetActiveCell(editor);
  editor->CaretToPosition(2);

  WHEN("the shortcut is pressed") {
    wxKeyEvent event = KeyDown('T', true, true);
    g_ws->OnKeyDown(event);
    THEN("the whole symbol is typed at the cursor") {
      CHECK(editor->GetValue() == wxS("f(") + tMacron + wxS(")"));
    }
  }
  WHEN("a sidebar button with that symbol is pressed") {
    wxCommandEvent event(SIDEBARKEYEVENT, 't');
    event.SetString(tMacron);
    g_ws->OnSidebarKey(event);
    THEN("the whole symbol is typed, not only its first character") {
      CHECK(editor->GetValue() == wxS("f(") + tMacron + wxS(")"));
    }
  }
  g_ws->SetActiveCell(nullptr);
  g_ws->DeleteRegion(cell, cell);
}

SCENARIO("A sidebar button can enter a letter with an accent") {
  ResetSymbols();
  CharButton *button = new CharButton(g_frame, g_ws, g_cfg,
                                      {tMacron, wxS("t with macron")}, true);
  CHECK(button->GetSymbol() == tMacron);
  THEN("a right click on its text still finds the button") {
    CHECK(CharButton::FromEventObject(button->GetTextObject()) == button);
    CHECK(CharButton::FromEventObject(g_frame) == nullptr);
  }
  button->Destroy();
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
  // Nothing here may end up in the user's own config file.
  g_cfg->MakeTemporary();
  g_cfg->SetZoomFactor(1.0);
  g_cfg->SetCanvasSize(wxSize(800, 600));
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));
  g_ws = new Worksheet(g_frame, wxID_ANY, g_cfg, wxDefaultPosition, wxDefaultSize,
                       /*reactToEvents=*/false);
  g_cfg->SetWorkSheet(g_ws);

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
