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
  The native scrollbars of matrices shown in a viewport; see MatrixScrollbars.
*/

#include "MatrixScrollbars.h"

#include <wx/settings.h>

#include <algorithm>

namespace {
/*! A scrollbar that never takes the keyboard focus from the worksheet

  Otherwise a click on it would leave the arrow keys scrolling the matrix
  instead of moving the cursor, with no visible sign of why.
*/
class MatrixScrollBar final : public wxScrollBar
{
public:
  MatrixScrollBar(wxWindow *parent, int orientation)
    : wxScrollBar(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                  orientation == wxHORIZONTAL ? wxSB_HORIZONTAL : wxSB_VERTICAL) {}
  bool AcceptsFocus() const override { return false; }
  bool AcceptsFocusFromKeyboard() const override { return false; }
};

//! Every event a scrollbar sends while or after it is moved. Typed, not
//! plain wxEventTypes: Bind() needs the tag to know the event's class.
const wxEventTypeTag<wxScrollEvent> g_scrollEvents[] = {
  wxEVT_SCROLL_TOP,       wxEVT_SCROLL_BOTTOM,       wxEVT_SCROLL_LINEUP,
  wxEVT_SCROLL_LINEDOWN,  wxEVT_SCROLL_PAGEUP,       wxEVT_SCROLL_PAGEDOWN,
  wxEVT_SCROLL_THUMBTRACK, wxEVT_SCROLL_THUMBRELEASE, wxEVT_SCROLL_CHANGED};
} // namespace

MatrixScrollbars::MatrixScrollbars(wxScrolled<wxWindow> *worksheet)
  : m_worksheet(worksheet) {}

MatrixScrollbars::~MatrixScrollbars() {
  // The scrollbars are the worksheet's children and would die with it anyway,
  // but their event handlers point here, and this dies first.
  for (auto &entry : m_entries)
    DestroyScrollbars(*entry);
}

wxCoord MatrixScrollbars::ScrollbarThickness() const {
  const wxCoord thickness =
    std::max(wxSystemSettings::GetMetric(wxSYS_HSCROLL_Y, m_worksheet),
             wxSystemSettings::GetMetric(wxSYS_VSCROLL_X, m_worksheet));
  // A platform that can't say (or whose overlay scrollbars report next to
  // nothing) still gets something that can be grabbed with the mouse.
  return (thickness > 4) ? thickness : 15;
}

void MatrixScrollbars::MatrixDrawn(MatrCell *matrix) {
  // Only take note: this runs inside the paint handler.
  auto found = std::find_if(m_entries.begin(), m_entries.end(),
                            [matrix](const std::unique_ptr<Entry> &entry) {
                              return entry->matrix.get() == matrix;
                            });
  Entry *entry;
  if (found != m_entries.end())
    entry = found->get();
  else {
    m_entries.emplace_back(std::make_unique<Entry>());
    entry = m_entries.back().get();
    entry->matrix = matrix;
  }
  entry->drawnThisPaint = true;
  entry->lastDrawnAt = matrix->GetRect();
}

void MatrixScrollbars::BeginPaint() {
  for (auto &entry : m_entries)
    entry->drawnThisPaint = false;
}

void MatrixScrollbars::EndPaint(const wxRegion &updateRegion) {
  bool needsSync = false;
  for (auto &entry : m_entries) {
    if (!entry->matrix) {
      // Deleted, so its scrollbars have to go.
      needsSync = true;
      continue;
    }
    if (entry->drawnThisPaint) {
      // Drawn, possibly somewhere new, or scrolled, or resized.
      entry->shown = true;
      needsSync = true;
      continue;
    }
    // Not drawn. That is only news if this paint covered where it was last
    // drawn: then it isn't there any more. Anywhere else, it simply wasn't
    // part of what needed repainting.
    const wxRect lastSeen(m_worksheet->CalcScrolledPosition(entry->lastDrawnAt.GetTopLeft()),
                          entry->lastDrawnAt.GetSize());
    if (entry->shown && (updateRegion.Contains(lastSeen) != wxOutRegion)) {
      entry->shown = false;
      needsSync = true;
    }
  }
  if (needsSync && !m_syncPending) {
    m_syncPending = true;
    m_worksheet->CallAfter([this] { Sync(); });
  }
}

void MatrixScrollbars::Sync() {
  m_syncPending = false;
  for (auto it = m_entries.begin(); it != m_entries.end();) {
    Entry &entry = **it;
    const MatrCell *matrix = entry.matrix.get();
    if (!matrix) {
      DestroyScrollbars(entry);
      it = m_entries.erase(it);
      continue;
    }
    const wxPoint scroll = matrix->ScrollPosition();
    SyncScrollbar(entry.horizontal, wxHORIZONTAL,
                  entry.shown && matrix->HasHorizontalScrollbar(),
                  matrix->HorizontalScrollbarRect(), scroll.x,
                  matrix->ViewportSize().x, matrix->ScrollableSize().x);
    SyncScrollbar(entry.vertical, wxVERTICAL,
                  entry.shown && matrix->HasVerticalScrollbar(),
                  matrix->VerticalScrollbarRect(), scroll.y,
                  matrix->ViewportSize().y, matrix->ScrollableSize().y);
    ++it;
  }
}

void MatrixScrollbars::SyncScrollbar(wxScrollBar *&scrollbar, int orientation,
                                     bool wanted, const wxRect &where,
                                     int position, int thumbSize, int range) {
  if (!wanted) {
    if (scrollbar && scrollbar->IsShown())
      scrollbar->Hide();
    return;
  }
  if (!scrollbar) {
    scrollbar = new MatrixScrollBar(m_worksheet, orientation);
    for (const auto &type : g_scrollEvents)
      scrollbar->Bind(type, &MatrixScrollbars::OnScroll, this);
  }
  // Every call below is only made if it changes something: each can make the
  // worksheet repaint, which would bring us back here.
  const wxRect client(m_worksheet->CalcScrolledPosition(where.GetTopLeft()),
                      where.GetSize());
  if (scrollbar->GetRect() != client)
    scrollbar->SetSize(client);
  if ((scrollbar->GetThumbPosition() != position) ||
      (scrollbar->GetThumbSize() != thumbSize) ||
      (scrollbar->GetRange() != range))
    scrollbar->SetScrollbar(position, thumbSize, range, thumbSize);
  if (!scrollbar->IsShown())
    scrollbar->Show();
}

void MatrixScrollbars::OnScroll(wxScrollEvent &event) {
  event.Skip();
  const wxObject *scrollbar = event.GetEventObject();
  auto found = std::find_if(m_entries.begin(), m_entries.end(),
                            [scrollbar](const std::unique_ptr<Entry> &entry) {
                              return (entry->horizontal == scrollbar) ||
                                (entry->vertical == scrollbar);
                            });
  if (found == m_entries.end())
    return;
  MatrCell *matrix = (*found)->matrix.get();
  if (!matrix)
    return;

  wxPoint position = matrix->ScrollPosition();
  if ((*found)->horizontal == scrollbar)
    position.x = event.GetPosition();
  else
    position.y = event.GetPosition();
  if (matrix->ScrollTo(position)) {
    const wxRect rect = matrix->GetRect();
    m_worksheet->RefreshRect(
      wxRect(m_worksheet->CalcScrolledPosition(rect.GetTopLeft()), rect.GetSize()));
  }
}

void MatrixScrollbars::DestroyScrollbars(Entry &entry) {
  if (entry.horizontal)
    entry.horizontal->Destroy();
  if (entry.vertical)
    entry.vertical->Destroy();
  entry.horizontal = nullptr;
  entry.vertical = nullptr;
}

size_t MatrixScrollbars::VisibleScrollbars() const {
  size_t count = 0;
  for (const auto &entry : m_entries) {
    if (entry->horizontal && entry->horizontal->IsShown())
      count++;
    if (entry->vertical && entry->vertical->IsShown())
      count++;
  }
  return count;
}
