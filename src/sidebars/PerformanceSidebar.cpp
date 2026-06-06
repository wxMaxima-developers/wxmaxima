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
  AddStat(gridSizer, _("Maxima processes:"), wxS("maxima"));
  AddStat(gridSizer, _("Font cache hits:"), wxS("font_hits"));
  AddStat(gridSizer, _("Font cache misses:"), wxS("font_misses"));
  AddStat(gridSizer, _("Recalc: Font invalid:"), wxS("recalc_font"));
  AddStat(gridSizer, _("Recalc: Size invalid:"), wxS("recalc_size"));
  AddStat(gridSizer, _("Recalc: Font mismatch:"), wxS("recalc_mismatch"));
  AddStat(gridSizer, _("Recalc: Config changed:"), wxS("recalc_config"));
  AddStat(gridSizer, _("Recalc: Cells appended:"), wxS("recalc_appended"));
  AddStat(gridSizer, _("Recalc: Editor dirty:"), wxS("recalc_dirty"));
  AddStat(gridSizer, _("Converted to linear:"), wxS("to_linear"));
  AddStat(gridSizer, _("Converted to 2D:"), wxS("to_2d"));

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
  const auto& stats = Configuration::g_stats;
  m_valueLabels[wxS("builtin")]->SetLabel(wxString::Format(wxS("%ld"), stats.manualAnchorsFromBuiltin.load()));
  m_valueLabels[wxS("cache")]->SetLabel(wxString::Format(wxS("%ld"), stats.manualAnchorsFromCache.load()));
  m_valueLabels[wxS("compiled")]->SetLabel(wxString::Format(wxS("%ld"), stats.manualAnchorsCompiled.load()));
  m_valueLabels[wxS("maxima")]->SetLabel(wxString::Format(wxS("%ld"), stats.maximaProcessesSpawned.load()));
  m_valueLabels[wxS("font_hits")]->SetLabel(wxString::Format(wxS("%ld"), stats.fontCacheHits.load()));
  m_valueLabels[wxS("font_misses")]->SetLabel(wxString::Format(wxS("%ld"), stats.fontCacheMisses.load()));
  m_valueLabels[wxS("recalc_font")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_FontInvalid.load()));
  m_valueLabels[wxS("recalc_size")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_SizeInvalid.load()));
  m_valueLabels[wxS("recalc_mismatch")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_FontMismatch.load()));
  m_valueLabels[wxS("recalc_config")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_ConfigChanged.load()));
  m_valueLabels[wxS("recalc_appended")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_CellsAppended.load()));
  m_valueLabels[wxS("recalc_dirty")]->SetLabel(wxString::Format(wxS("%ld"), stats.recalculationNeeded_EditorDirty.load()));
  m_valueLabels[wxS("to_linear")]->SetLabel(wxString::Format(wxS("%ld"), stats.cellsConvertedToLinear.load()));
  m_valueLabels[wxS("to_2d")]->SetLabel(wxString::Format(wxS("%ld"), stats.cellsConvertedTo2D.load()));
  Layout();
}
