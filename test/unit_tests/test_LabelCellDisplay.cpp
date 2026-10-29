// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  Tests that a LabelCell shows the right text as soon as it exists.

  TextCell's constructor sets the displayed text, but a constructor can only
  call its own class's version of a virtual function: LabelCell's
  UpdateDisplayedText(), which hides automatic labels or shows a user-defined
  one, didn't run until the cell was first laid out under a changed
  configuration. clang-analyzer's VirtualCall check pointed at this.
*/

#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/log.h>

#include "CellPointers.h"
#include "Configuration.h"
#include "cells/GroupCell.h"
#include "cells/LabelCell.h"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
Configuration *g_cfg = nullptr;
} // namespace

SCENARIO("A new label cell already shows what the label settings ask for") {
  GroupCell group(g_cfg, GC_TYPE_CODE, wxS("x;"));
  GIVEN("Only user-defined labels are to be shown") {
    g_cfg->SetLabelChoice(Configuration::labels_useronly);
    WHEN("an automatic label is created") {
      LabelCell label(&group, g_cfg, wxS("(%o1)"), TS_LABEL);
      THEN("it shows nothing")
        REQUIRE(label.GetDisplayedString().IsEmpty());
    }
  }
  GIVEN("User-defined labels are preferred") {
    g_cfg->SetLabelChoice(Configuration::labels_prefer_user);
    WHEN("a label with a user-defined name is copied") {
      LabelCell label(&group, g_cfg, wxS("(%o1)"), TS_LABEL);
      label.SetUserDefinedLabel(wxS("(myLabel)"));
      REQUIRE(label.GetDisplayedString() == wxS("(myLabel)"));
      LabelCell copy(&group, label);
      THEN("the copy shows the user-defined name")
        REQUIRE(copy.GetDisplayedString() == wxS("(myLabel)"));
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

  int result = 0;
  {
    wxBitmap bmp(400, 400);
    wxMemoryDC dc;
    dc.SelectObject(bmp);
    Configuration cfg(&dc, Configuration::temporary);
    cfg.SetZoomFactor(1.0);
    static DocumentCellPointers documentPointers;
    static ViewCellPointers viewPointers(nullptr);
    cfg.SetDocumentCellPointers(&documentPointers);
    cfg.SetViewCellPointers(&viewPointers);
    g_cfg = &cfg;

    result = Catch::Session().run(argc, argv);

    g_cfg = nullptr;
  }
  wxEntryCleanup();
  return result;
}
