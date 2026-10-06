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
  Tests for the reorderable sections of the main toolbar.

  Covers the stored order (ToolBarSections), the toolbar built from it
  (ToolBar::AddTools()) and the configuration dialogue's "Toolbar" tab
  (ToolBarSectionsPanel).
*/

#include <algorithm>
#include <set>
#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/fileconf.h>
#include <wx/frame.h>
#include <wx/log.h>

#include "ToolBar.h"
#include "ToolBarSections.h"
#include "dialogs/ToolBarSectionsPanel.h"
#include "wxMaximaArtProvider.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using ToolBarSections::Section;

namespace {
wxFrame *g_frame = nullptr;

//! Start every scenario from a config in which the user never changed anything
void ResetConfig() {
  wxConfig::Get()->DeleteAll();
}

//! Where in \p order \p section is
long PositionIn(const std::vector<Section> &order, Section section) {
  return std::find(order.begin(), order.end(), section) - order.begin();
}

//! The position of the first tool with this id, or -1
int ToolPosition(ToolBar *toolbar, int id) {
  for (size_t i = 0; i < toolbar->GetToolCount(); i++)
    if (toolbar->FindToolByIndex(i)->GetId() == id)
      return static_cast<int>(i);
  return -1;
}

//! Gives the test access to the toolbar's context menu handler
class TestToolBar : public ToolBar {
public:
  explicit TestToolBar(wxWindow *parent) : ToolBar(parent) {}
  void ClickContextMenuEntry(Section section) {
    wxCommandEvent ev(wxEVT_MENU, SectionMenuId(section));
    OnMenu(ev);
  }
  //! The cell type drop-down, which is the toolbar's only wxChoice
  wxChoice *CellTypeChoice() {
    for (auto *child : GetChildren())
      if (auto *choice = dynamic_cast<wxChoice *>(child))
        return choice;
    return nullptr;
  }
};
} // namespace

SCENARIO("The stored order of the toolbar sections is read robustly") {
  const auto &defaults = ToolBarSections::DefaultOrder();

  THEN("every section has its own name in the config") {
    std::set<wxString> keys;
    std::set<wxString> visibilityKeys;
    for (const auto section : defaults) {
      keys.insert(ToolBarSections::Key(section));
      visibilityKeys.insert(ToolBarSections::VisibilityConfigKey(section));
    }
    REQUIRE(keys.size() == defaults.size());
    REQUIRE(visibilityKeys.size() == defaults.size());
  }
  THEN("the sections that could be hidden before keep their config keys") {
    REQUIRE(ToolBarSections::VisibilityConfigKey(Section::UndoRedo) ==
            wxS("Toolbar/showUndoRedo"));
    REQUIRE(ToolBarSections::VisibilityConfigKey(Section::TextFormat) ==
            wxS("Toolbar/showTextFormat"));
    REQUIRE_FALSE(ToolBarSections::ShownByDefault(Section::UndoRedo));
    REQUIRE(ToolBarSections::ShownByDefault(Section::Evaluate));
  }
  WHEN("nothing is stored") {
    THEN("the default order is used") {
      REQUIRE(ToolBarSections::ParseOrder(wxEmptyString) == defaults);
    }
  }
  WHEN("an order is stored and read back") {
    std::vector<Section> reversed(defaults.rbegin(), defaults.rend());
    THEN("it is the same order") {
      REQUIRE(ToolBarSections::ParseOrder(
                ToolBarSections::OrderToString(reversed)) == reversed);
    }
  }
  WHEN("the stored order contains unknown names, duplicates and spaces") {
    auto order = ToolBarSections::ParseOrder(
      wxS("help, frobnicate,help ,new,,evaluate"));
    THEN("those are ignored and every section is there exactly once") {
      REQUIRE(order.size() == defaults.size());
      REQUIRE(std::set<Section>(order.begin(), order.end()).size() ==
              defaults.size());
      REQUIRE(order[0] == Section::Help);
      REQUIRE(PositionIn(order, Section::New) <
              PositionIn(order, Section::Evaluate));
    }
  }
  WHEN("the stored order was written before a section existed") {
    // Everything but the text formatting buttons, Help moved to the front
    wxString stored = wxS("help");
    for (const auto section : defaults)
      if ((section != Section::Help) && (section != Section::TextFormat))
        stored += wxS(",") + ToolBarSections::Key(section);
    auto order = ToolBarSections::ParseOrder(stored);
    THEN("the new section appears after its predecessor in the default order") {
      REQUIRE(order[0] == Section::Help);
      REQUIRE(PositionIn(order, Section::TextFormat) ==
              PositionIn(order, Section::CellStyle) + 1);
    }
  }
  WHEN("the very first default section is missing") {
    auto order = ToolBarSections::ParseOrder(wxS("help,openSave"));
    THEN("it goes in front, followed by the sections that follow it by default") {
      REQUIRE(order[0] == Section::New);
      REQUIRE(order[1] == Section::Help);
      REQUIRE(order[2] == Section::OpenSave);
      REQUIRE(order[3] == Section::Print);
    }
  }
}

SCENARIO("Separators go between sections that don't belong together") {
  using ToolBarSections::SeparatorBetween;
  REQUIRE_FALSE(SeparatorBetween(Section::New, Section::OpenSave));
  REQUIRE_FALSE(SeparatorBetween(Section::CopyPaste, Section::SelectAll));
  REQUIRE_FALSE(SeparatorBetween(Section::CellStyle, Section::TextFormat));
  REQUIRE(SeparatorBetween(Section::OpenSave, Section::Print));
  REQUIRE(SeparatorBetween(Section::Help, Section::New));
  REQUIRE(SeparatorBetween(Section::MaximaControl, Section::Evaluate));
  REQUIRE_FALSE(SeparatorBetween(Section::Animation, Section::FlexibleSpace));
  REQUIRE_FALSE(SeparatorBetween(Section::FlexibleSpace, Section::Help));
}

SCENARIO("The toolbar shows its sections in the stored order") {
  ResetConfig();
  GIVEN("the default order") {
    auto *toolbar = new TestToolBar(g_frame);
    THEN("it looks as it always did") {
      REQUIRE(ToolPosition(toolbar, wxID_NEW) == 0);
      REQUIRE(ToolPosition(toolbar, wxID_OPEN) == 1);
      REQUIRE(ToolPosition(toolbar, wxID_SAVE) == 2);
      // a separator, then Print
      REQUIRE(ToolPosition(toolbar, wxID_PRINT) == 4);
      REQUIRE(toolbar->FindToolByIndex(3)->GetKind() == wxITEM_SEPARATOR);
      // Undo/Redo is hidden by default
      REQUIRE(ToolPosition(toolbar, wxID_UNDO) == -1);
      // Help is the last tool, after the flexible space
      REQUIRE(ToolPosition(toolbar, wxID_HELP) ==
              static_cast<int>(toolbar->GetToolCount()) - 1);
      // (wxAuiToolBar has no public "spacer" kind; only a stretch spacer has
      // a proportion)
      REQUIRE(toolbar->FindToolByIndex(toolbar->GetToolCount() - 2)
                ->GetProportion() > 0);
    }
    toolbar->Destroy();
  }
  GIVEN("an order with Help first and Evaluate before Restart/Interrupt") {
    auto order = ToolBarSections::DefaultOrder();
    order.erase(order.begin() + PositionIn(order, Section::Help));
    order.insert(order.begin(), Section::Help);
    const auto evaluate = order.begin() + PositionIn(order, Section::Evaluate);
    const auto maxima = order.begin() + PositionIn(order, Section::MaximaControl);
    std::iter_swap(evaluate, maxima);
    ToolBar::SectionOrder(order);
    auto *toolbar = new TestToolBar(g_frame);
    THEN("the tools are in that order, with a separator after Help") {
      REQUIRE(ToolPosition(toolbar, wxID_HELP) == 0);
      REQUIRE(toolbar->FindToolByIndex(1)->GetKind() == wxITEM_SEPARATOR);
      REQUIRE(ToolPosition(toolbar, wxID_NEW) == 2);
      REQUIRE(ToolPosition(toolbar, ToolBar::tb_eval) <
              ToolPosition(toolbar, ToolBar::tb_interrupt));
    }
    toolbar->Destroy();
  }
  ResetConfig();
}

SCENARIO("Every toolbar section can be hidden") {
  ResetConfig();
  for (const auto section : ToolBarSections::DefaultOrder())
    ToolBar::ShowSection(section, false);
  auto *toolbar = new TestToolBar(g_frame);
  THEN("the toolbar is empty") {
    REQUIRE(toolbar->GetToolCount() == 0);
  }
  THEN("updating the state of tools that aren't there is harmless") {
    toolbar->CanUndo(false);
    toolbar->CanCopy(false);
    toolbar->CanEvalThisCell(false);
    toolbar->WorksheetEmpty(true);
    toolbar->TextFormatState(true, true, false, false, false);
    toolbar->AnimationButtonState(ToolBar::Running);
    toolbar->UpdateBitmaps();
  }
  THEN("the hidden cell type drop-down still decides the type of new cells") {
    REQUIRE(toolbar->CellTypeChoice() != nullptr);
    REQUIRE_FALSE(toolbar->CellTypeChoice()->IsShown());
    toolbar->SetCellStyle(GC_TYPE_TEXT);
    REQUIRE(toolbar->GetCellType() == GC_TYPE_TEXT);
  }
  THEN("the hidden animation slider can still be updated") {
    REQUIRE(toolbar->m_plotSlider != nullptr);
    REQUIRE_FALSE(toolbar->m_plotSlider->IsShown());
  }
  WHEN("a section is shown again using the context menu") {
    toolbar->ClickContextMenuEntry(Section::Search);
    THEN("it is there, and only it") {
      REQUIRE(ToolBar::ShowSection(Section::Search));
      REQUIRE(toolbar->GetToolCount() == 1);
      REQUIRE(ToolPosition(toolbar, wxID_FIND) == 0);
    }
  }
  toolbar->Destroy();
  ResetConfig();
}

SCENARIO("The configuration dialogue's Toolbar tab reorders the sections") {
  ResetConfig();
  auto *panel = new ToolBarSectionsPanel(g_frame);
  const auto &defaults = ToolBarSections::DefaultOrder();

  THEN("it starts with the stored order and visibility") {
    REQUIRE(panel->GetOrder() == defaults);
    REQUIRE(panel->IsSectionShown(Section::Evaluate));
    REQUIRE_FALSE(panel->IsSectionShown(Section::UndoRedo));
  }
  WHEN("an entry is dragged to another position") {
    panel->MoveEntry(0, 3);
    THEN("it is there, and the others moved up") {
      auto order = panel->GetOrder();
      REQUIRE(order[3] == Section::New);
      REQUIRE(order[0] == Section::OpenSave);
      REQUIRE(order[1] == Section::Print);
      REQUIRE(order[2] == Section::UndoRedo);
    }
    THEN("its check box moved with it") {
      REQUIRE(panel->IsSectionShown(Section::New));
      REQUIRE_FALSE(panel->IsSectionShown(Section::UndoRedo));
    }
    THEN("nothing is stored before the dialogue is confirmed") {
      REQUIRE(ToolBar::SectionOrder() == defaults);
    }
  }
  WHEN("an entry is dragged upwards") {
    panel->MoveEntry(static_cast<int>(defaults.size()) - 1, 0);
    THEN("it is the first one") {
      REQUIRE(panel->GetOrder()[0] == Section::Help);
      REQUIRE(panel->GetOrder()[1] == Section::New);
    }
  }
  WHEN("an entry is dragged outside the list") {
    panel->MoveEntry(0, 1000);
    panel->MoveEntry(-1, 0);
    THEN("nothing changes") { REQUIRE(panel->GetOrder() == defaults); }
  }
  WHEN("the changes are written") {
    panel->MoveEntry(0, 3);
    panel->SetSectionShown(Section::UndoRedo, true);
    panel->SetSectionShown(Section::Help, false);
    panel->Write();
    THEN("the toolbar uses them") {
      REQUIRE(ToolBar::SectionOrder() == panel->GetOrder());
      REQUIRE(ToolBar::ShowSection(Section::UndoRedo));
      REQUIRE_FALSE(ToolBar::ShowSection(Section::Help));
      auto *toolbar = new TestToolBar(g_frame);
      REQUIRE(ToolPosition(toolbar, wxID_OPEN) == 0);
      REQUIRE(ToolPosition(toolbar, wxID_UNDO) >= 0);
      REQUIRE(ToolPosition(toolbar, wxID_HELP) == -1);
      toolbar->Destroy();
    }
    AND_WHEN("the user goes back to the defaults") {
      panel->ResetToDefaults();
      THEN("the list shows the default order and visibility") {
        REQUIRE(panel->GetOrder() == defaults);
        for (const auto section : defaults)
          REQUIRE(panel->IsSectionShown(section) ==
                  ToolBarSections::ShownByDefault(section));
      }
    }
  }
  panel->Destroy();
  ResetConfig();
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
  // An in-memory configuration (style 0 never touches the disk), so that
  // the toolbar's layout doesn't depend on, or leak into, a config file a
  // previous run left behind.
  delete wxConfigBase::Set(new wxFileConfig(wxEmptyString, wxEmptyString,
                                            wxEmptyString, wxEmptyString, 0));
  wxArtProvider::Push(new wxMaximaArtProvider);
  g_frame = new wxFrame(nullptr, wxID_ANY, wxS("test"));

  const int result = Catch::Session().run(argc, argv);

  wxEntryCleanup();
  return result;
}
