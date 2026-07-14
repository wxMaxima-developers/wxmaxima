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

#include "PerformanceSidebar.h"
#include "Configuration.h"
#include <wx/sizer.h>

PerformanceSidebar::PerformanceSidebar(wxWindow *parent, int ID)
  : wxScrolled<wxPanel>(parent, ID)
{
  wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
  wxFlexGridSizer *gridSizer = new wxFlexGridSizer(2, 5, 10);
  gridSizer->AddGrowableCol(1);

  AddStat(gridSizer, _("Manual anchors (built-in):"), wxS("builtin"));
  AddStat(gridSizer, _("Manual anchors (cache):"), wxS("cache"));
  AddStat(gridSizer, _("Manual anchors (compiled):"), wxS("compiled"));
  AddStat(gridSizer, _("Maxima starts:"), wxS("maxima"));
  AddStat(gridSizer, _("Font cache hits:"), wxS("font_hits"));
  AddStat(gridSizer, _("Font cache misses:"), wxS("font_misses"));
  AddStat(gridSizer, _("Recalc (Font invalid):"), wxS("recalc_font"));
  AddStat(gridSizer, _("Recalc (Size invalid):"), wxS("recalc_size"));
  AddStat(gridSizer, _("Recalc (Font mismatch):"), wxS("recalc_mismatch"));
  AddStat(gridSizer, _("Recalc (Config changed):"), wxS("recalc_config"));
  AddStat(gridSizer, _("Recalc (Cells appended):"), wxS("recalc_appended"));
  AddStat(gridSizer, _("Recalc (Editor dirty):"), wxS("recalc_dirty"));
  AddStat(gridSizer, _("Converted to linear:"), wxS("to_linear"));
  AddStat(gridSizer, _("Converted to 2D:"), wxS("to_2d"));
  AddStat(gridSizer, _("Worksheet repaints:"), wxS("repaints"));
  AddStat(gridSizer, _("Full-window repaints:"), wxS("repaints_full"));

  mainSizer->Add(gridSizer, 1, wxALL | wxEXPAND, 10);
  SetSizer(mainSizer);
  SetScrollRate(5, 5);
  UpdateContents();
}

void PerformanceSidebar::AddStat(wxSizer* sizer, const wxString& label, const wxString& key)
{
  sizer->Add(new wxStaticText(this, wxID_ANY, label));
  wxStaticText* value = new wxStaticText(this, wxID_ANY, wxS("0"));
  sizer->Add(value, 0, wxALIGN_RIGHT);
  m_valueLabels[key] = value;
}

void PerformanceSidebar::UpdateContents()
{
  // Throttle: this is called from the idle loop, and the stats it displays
  // change on every repaint (repainting bumps the font-cache counters). An
  // update changes label text, which re-layouts and repaints - which changes
  // the stats again, so updating on every idle event kept the GUI repainting
  // in a loop. A couple of updates per second is plenty for a monitor.
  if (m_sinceLastUpdate.Time() < 500)
    return;
  m_sinceLastUpdate.Start();

  const auto& stats = Configuration::g_stats;
  bool changed = false;
  const auto setStat = [&](const wxString &key, long value) {
    wxStaticText *label = m_valueLabels[key];
    const wxString text = wxString::Format(wxS("%ld"), value);
    if (label->GetLabel() != text) {
      label->SetLabel(text);
      changed = true;
    }
  };
  setStat(wxS("builtin"), stats.manualAnchorsFromBuiltin.load());
  setStat(wxS("cache"), stats.manualAnchorsFromCache.load());
  setStat(wxS("compiled"), stats.manualAnchorsCompiled.load());
  setStat(wxS("maxima"), stats.maximaProcessesSpawned.load());
  setStat(wxS("font_hits"), stats.fontCacheHits.load());
  setStat(wxS("font_misses"), stats.fontCacheMisses.load());
  setStat(wxS("recalc_font"), stats.recalculationNeeded_FontInvalid.load());
  setStat(wxS("recalc_size"), stats.recalculationNeeded_SizeInvalid.load());
  setStat(wxS("recalc_mismatch"), stats.recalculationNeeded_FontMismatch.load());
  setStat(wxS("recalc_config"), stats.recalculationNeeded_ConfigChanged.load());
  setStat(wxS("recalc_appended"), stats.recalculationNeeded_CellsAppended.load());
  setStat(wxS("recalc_dirty"), stats.recalculationNeeded_EditorDirty.load());
  setStat(wxS("to_linear"), stats.cellsConvertedToLinear.load());
  setStat(wxS("to_2d"), stats.cellsConvertedTo2D.load());
  setStat(wxS("repaints"), stats.worksheetRepaints.load());
  setStat(wxS("repaints_full"), stats.worksheetFullRepaints.load());
  if (changed)
    Layout();
}
