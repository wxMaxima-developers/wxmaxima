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
  Regression tests for the worksheet's right-click context menu.

  PopulateWorksheetContextMenu builds the menu as a function of the
  worksheet's state without showing it, so we can drive the main states a
  right click can hit (cursor between cells, a selected group cell, a selected
  image cell, an active editor cell) on a real Worksheet and check that the
  expected entries appear. This pins the menu across refactorings of the
  builder - it was extracted verbatim out of a 868-line
  Worksheet::OnMouseRightDown.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/mstream.h>

#include "Configuration.h"
#include "Worksheet.h"
#include "WorksheetContextMenu.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/ImgCell.h"

#include <cstdlib>
#ifndef _WIN32
#include <unistd.h>
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

//! Does the menu (including submenus) contain an item whose label starts so?
static bool HasItem(const wxMenu &menu, const wxString &labelStart) {
  for (size_t i = 0; i < menu.GetMenuItemCount(); ++i) {
    wxMenuItem *item = menu.FindItemByPosition(i);
    if (item->IsSeparator())
      continue;
    if (wxMenuItem::GetLabelText(item->GetItemLabel()).StartsWith(labelStart))
      return true;
    if (item->GetSubMenu() && HasItem(*item->GetSubMenu(), labelStart))
      return true;
  }
  return false;
}

//! A fresh two-cell document: a code cell and a text cell.
static void BuildDocument() {
  g_ws->ClearDocument();
  auto code = std::make_unique<GroupCell>(g_cfg, GC_TYPE_CODE, wxS("1+1;"));
  GroupCell *first = g_ws->InsertGroupCells(std::move(code), nullptr);
  g_ws->InsertGroupCells(
    std::make_unique<GroupCell>(g_cfg, GC_TYPE_TEXT, wxS("some text")), first);
  g_ws->SetActiveCell(nullptr);
  g_ws->ClearSelection();
  g_ws->RequestRecalculation();
  g_ws->RecalculateIfNeeded();
}

SCENARIO("The between-cells (hCaret) menu offers paste, select-all and cell insertion") {
  BuildDocument();
  g_ws->SetHCaret(g_ws->GetLastCellInWorksheet()); // activates the hCaret

  wxMenu menu;
  PopulateWorksheetContextMenu(*g_ws, menu, 0, 0, false);

  THEN("the structural entries are there") {
    REQUIRE(HasItem(menu, wxS("Paste")));
    REQUIRE(HasItem(menu, wxS("Select All")));
    REQUIRE(HasItem(menu, wxS("Insert Text Cell")));
    REQUIRE(HasItem(menu, wxS("Insert Section Cell")));
    REQUIRE(HasItem(menu, wxS("Evaluate Cells Above")));
    REQUIRE(HasItem(menu, wxS("Evaluate Cells Below")));
    REQUIRE_FALSE(HasItem(menu, wxS("Cut"))); // no active cell
  }
}

SCENARIO("The selected-group-cell menu offers copy, delete and evaluation") {
  BuildDocument();
  GroupCell *group = g_ws->GetTree();
  g_ws->SetSelection(group, group);

  wxMenu menu;
  PopulateWorksheetContextMenu(*g_ws, menu, 0, 0, true);

  THEN("copy/delete/evaluate entries are there") {
    REQUIRE(HasItem(menu, wxS("Copy")));
    REQUIRE(HasItem(menu, wxS("Copy as LaTeX")));
    REQUIRE(HasItem(menu, wxS("Copy as Image")));
    REQUIRE(HasItem(menu, wxS("Delete Selection")));
    REQUIRE(HasItem(menu, wxS("Evaluate Cell")));
    REQUIRE(HasItem(menu, wxS("Evaluate Cells Above")));
    REQUIRE_FALSE(HasItem(menu, wxS("Save Image")));   // not an image
    REQUIRE_FALSE(HasItem(menu, wxS("Insert Text Cell"))); // not the hCaret menu
  }
}

SCENARIO("The selected-image menu offers saving and image manipulation") {
  BuildDocument();
  // Build an image group cell holding a small generated PNG.
  wxImage img(12, 8);
  img.SetRGB(wxRect(0, 0, 12, 8), 128, 64, 32);
  wxMemoryOutputStream stream;
  REQUIRE(img.SaveFile(stream, wxBITMAP_TYPE_PNG));
  wxMemoryBuffer buf;
  buf.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                 stream.GetOutputStreamBuffer()->GetBufferSize());
  auto imgGroup = std::make_unique<GroupCell>(g_cfg, GC_TYPE_IMAGE);
  // The group must be set at construction: m_group is only ever assigned in
  // the Cell constructor; AppendOutput does not re-parent cells.
  imgGroup->SetOutput(
    std::make_unique<ImgCell>(imgGroup.get(), g_cfg, buf, wxS("png")));
  GroupCell *group =
    g_ws->InsertGroupCells(std::move(imgGroup), g_ws->GetLastCellInWorksheet());
  g_ws->RequestRecalculation();
  g_ws->RecalculateIfNeeded();
  Cell *imgCell = group->GetLabel();
  REQUIRE(imgCell != nullptr);
  g_ws->SetSelection(imgCell, imgCell);

  wxMenu menu;
  PopulateWorksheetContextMenu(*g_ws, menu, 0, 0, true);

  THEN("the image entries are there") {
    REQUIRE(HasItem(menu, wxS("Copy")));
    REQUIRE(HasItem(menu, wxS("Save Image")));
    REQUIRE(HasItem(menu, wxS("Restrict Maximum size")));
    REQUIRE(HasItem(menu, wxS("Set image resolution")));
    REQUIRE_FALSE(HasItem(menu, wxS("Evaluate Cell")));
  }
}

SCENARIO("The active-cell menu offers editing operations") {
  BuildDocument();
  GroupCell *group = g_ws->GetTree();
  REQUIRE(group->GetEditable() != nullptr);
  g_ws->SetActiveCell(group->GetEditable());

  wxMenu menu;
  PopulateWorksheetContextMenu(*g_ws, menu, 0, 0, false);

  THEN("the editing entries are there") {
    REQUIRE(HasItem(menu, wxS("Cut")));
    REQUIRE(HasItem(menu, wxS("Copy")));
    REQUIRE(HasItem(menu, wxS("Paste")));
    REQUIRE(HasItem(menu, wxS("Select All")));
    REQUIRE(HasItem(menu, wxS("Divide Cell"))); // click outside the selection
    REQUIRE_FALSE(HasItem(menu, wxS("Insert Text Cell")));
  }

  THEN("clicking inside the selection of a code cell offers commenting instead of dividing") {
    wxMenu menu2;
    PopulateWorksheetContextMenu(*g_ws, menu2, 0, 0, true);
    REQUIRE(HasItem(menu2, wxS("Comment Selection")));
    REQUIRE_FALSE(HasItem(menu2, wxS("Divide Cell")));
  }
}

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
  wxInitAllImageHandlers();

  g_bmp = new wxBitmap(800, 600);
  g_dc = new wxMemoryDC();
  g_dc->SelectObject(*g_bmp);
  g_cfg = new Configuration(g_dc);
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
