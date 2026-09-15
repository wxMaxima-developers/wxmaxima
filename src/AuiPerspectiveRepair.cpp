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
  Implements RepairAuiPerspective().
*/

#include "AuiPerspectiveRepair.h"
#include <wx/tokenzr.h>

namespace {
//! wxAUI's dock direction for the centre pane (wxAUI_DOCK_CENTRE).
//!
//! Spelled out as a literal rather than including <wx/aui/framemanager.h>:
//! this file deliberately deals in the stored *text*, so that it needs none
//! of wxAUI and can be unit-tested on its own. The value is part of the
//! saved-perspective format, so it cannot change without the format
//! changing too.
const wxString CENTRE_DOCK_DIRECTION = wxS("5");

/*! Rewrites one "key=value;key=value;..." pane entry, if it is the centre one.

  Returns \p entry unchanged unless it actually says dir=5, so every other
  pane -- and wxAUI's own dock_size(...) entries, which have no dir field at
  all -- passes through byte for byte. */
wxString RepairPaneEntry(const wxString &entry) {
  // Splitting on ';' and rebuilding is what keeps this from matching a value
  // that merely looks like a field: a caption is stored in this same string
  // and can contain anything the translations happen to contain.
  wxArrayString fields;
  wxStringTokenizer tokenizer(entry, wxS(";"), wxTOKEN_RET_EMPTY_ALL);
  while (tokenizer.HasMoreTokens())
    fields.Add(tokenizer.GetNextToken());

  bool isCentrePane = false;
  for (const auto &field : fields)
    if (field == wxS("dir=") + CENTRE_DOCK_DIRECTION)
      isCentrePane = true;
  if (!isCentrePane)
    return entry;

  wxString result;
  for (std::size_t i = 0; i < fields.GetCount(); i++) {
    wxString field = fields[i];
    if (field.StartsWith(wxS("layer=")))
      field = wxS("layer=0");
    else if (field.StartsWith(wxS("row=")))
      field = wxS("row=0");
    else if (field.StartsWith(wxS("pos=")))
      field = wxS("pos=0");
    if (i > 0)
      result += wxS(";");
    result += field;
  }
  return result;
}
} // namespace

wxString RepairAuiPerspective(const wxString &perspective) {
  // A perspective is '|'-separated: a version header, then one entry per
  // pane, then wxAUI's dock sizes. Keeping every token -- including empty
  // ones, since the string ends in a separator -- and re-joining with '|'
  // reproduces the original exactly wherever nothing needed repairing.
  wxString result;
  wxStringTokenizer tokenizer(perspective, wxS("|"), wxTOKEN_RET_EMPTY_ALL);
  bool first = true;
  while (tokenizer.HasMoreTokens()) {
    if (!first)
      result += wxS("|");
    first = false;
    result += RepairPaneEntry(tokenizer.GetNextToken());
  }
  return result;
}
