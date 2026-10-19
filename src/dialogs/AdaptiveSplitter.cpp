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
  Defines AdaptiveSplitter, a splitter that puts a preview below or beside
  a settings pane, whichever leaves the preview more room.
*/

#include "AdaptiveSplitter.h"
#include <wx/config.h>
#include <algorithm>

AdaptiveSplitter::AdaptiveSplitter(wxWindow *parent, wxWindowID id)
  : wxSplitterWindow(parent, id, wxDefaultPosition, wxDefaultSize,
                     wxSP_3D | wxSP_LIVE_UPDATE) {
  // A minimum pane size above 0 keeps a double-click on the sash (or dragging
  // it to the edge) from unsplitting the window, which would hide the preview
  // with no obvious way to get it back.
  SetMinimumPaneSize(FromDIP(40));
  // Growing the dialog gives the new room to the preview.
  SetSashGravity(0.0);
  Bind(wxEVT_SIZE, &AdaptiveSplitter::OnSize, this);
  Bind(wxEVT_SPLITTER_SASH_POS_CHANGED, &AdaptiveSplitter::OnSashMoved, this);
}

wxSplitMode AdaptiveSplitter::ChooseSplitMode(wxSize available,
                                              wxSize preferredSettingsSize) {
  const long long below =
    static_cast<long long>(std::max(available.x, 0)) *
    std::max(available.y - preferredSettingsSize.y, 0);
  const long long beside =
    static_cast<long long>(std::max(available.x - preferredSettingsSize.x, 0)) *
    std::max(available.y, 0);
  return (beside > below) ? wxSPLIT_VERTICAL : wxSPLIT_HORIZONTAL;
}

void AdaptiveSplitter::SplitAdaptively(wxWindow *settings, wxWindow *preview,
                                       wxSize preferredSettingsSize) {
  m_settings = settings;
  m_preview = preview;
  m_preferredSettingsSize = preferredSettingsSize.IsFullySpecified()
    ? preferredSettingsSize : settings->GetBestSize();

  // Without an explicit minimum size the splitter's best size would be the
  // sum of both panes' best sizes, and in the dialog's scrolled tab that
  // would make the tab scroll instead of the splitter fitting into it.
  // Ask only for what the settings pane can shrink to, plus a strip of
  // preview.
  wxSize settingsMin = settings->GetMinSize();
  settingsMin.IncTo(wxSize(0, 0));
  SetMinSize(wxSize(settingsMin.x,
                    settingsMin.y + GetSashSize() + GetMinimumPaneSize()));

  // The direction is chosen once the splitter knows its size (in OnSize);
  // until then, the layout the Style tab always had.
  SplitHorizontally(settings, preview, m_preferredSettingsSize.y);
  const wxSize size = GetClientSize();
  if ((size.x > 0) && (size.y > 0))
    SplitIn(ChooseSplitMode(size, m_preferredSettingsSize));
}

void AdaptiveSplitter::SplitIn(wxSplitMode mode) {
  // SetSplitMode() only records the mode; SetSashPosition(..., true) then
  // lays both panes out anew. Unsplit() + Split...() would also work, but
  // hides and re-shows the preview, which flickers.
  SetSplitMode(mode);
  SetSashPosition((mode == wxSPLIT_VERTICAL) ? m_preferredSettingsSize.x
                                             : m_preferredSettingsSize.y,
                  true);
}

void AdaptiveSplitter::OnSize(wxSizeEvent &event) {
  // wxSplitterWindow's own size handler runs after this one and lays the
  // panes out for the new size.
  event.Skip();
  if (!m_settings || !IsSplit())
    return;
  const wxSize size = GetClientSize();
  if ((size.x <= 0) || (size.y <= 0))
    return;
  // Re-applied on every resize, not just when the direction changes: while
  // the splitter is too small for the settings' preferred size (as it is
  // while the dialog is first laid out) the sash position gets clamped, and
  // it has to return to where it belongs once there is room again. A sash
  // the user moved is safe from this, as moving it changes the preferred
  // size (see OnSashMoved()).
  SplitIn(ChooseSplitMode(size, m_preferredSettingsSize));
}

void AdaptiveSplitter::RememberSashIn(const wxString &configKey) {
  m_configKey = configKey;
  wxConfigBase *config = wxConfig::Get();
  int width = m_preferredSettingsSize.x;
  int height = m_preferredSettingsSize.y;
  config->Read(m_configKey + wxS("_width"), &width);
  config->Read(m_configKey + wxS("_height"), &height);
  // A stored size below the minimum (say, the dialog's font got bigger since)
  // would only be corrected by the splitter later, after choosing the split
  // direction from it.
  wxSize preferred(width, height);
  preferred.IncTo(m_settings ? m_settings->GetMinSize() : wxSize(0, 0));
  if (preferred == m_preferredSettingsSize)
    return;
  m_preferredSettingsSize = preferred;
  if (IsSplit() && (GetClientSize().x > 0) && (GetClientSize().y > 0))
    SplitIn(ChooseSplitMode(GetClientSize(), m_preferredSettingsSize));
}

void AdaptiveSplitter::OnSashMoved(wxSplitterEvent &event) {
  event.Skip();
  if (GetSplitMode() == wxSPLIT_VERTICAL)
    m_preferredSettingsSize.x = event.GetSashPosition();
  else
    m_preferredSettingsSize.y = event.GetSashPosition();
  if (m_configKey.IsEmpty())
    return;
  wxConfigBase *config = wxConfig::Get();
  config->Write(m_configKey + wxS("_width"), m_preferredSettingsSize.x);
  config->Write(m_configKey + wxS("_height"), m_preferredSettingsSize.y);
}
