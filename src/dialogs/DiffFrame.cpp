// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Gemini CLI
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

#include "DiffFrame.h"
#include "DiffAlgorithm.h"
#include "DiffScrollSync.h"
#include "WXMformat.h"
#include "MathParser.h"
#include "cells/CellList.h"
#if wxCHECK_VERSION(3, 1, 6)
#include "wxMaximaArtProvider.h"
#endif
#include <wx/artprov.h>
#include <wx/file.h>
#include <wx/wfstream.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <map>
#include <algorithm>

/**
 * @class SpacerGroupCell
 * @brief A specialized GroupCell that provides empty vertical space for alignment.
 *
 * When two or three files are compared and one file lacks a cell that exists
 * in the others, this cell is inserted as a placeholder to keep the
 * corresponding cells in the other worksheets aligned horizontally.
 */
class SpacerGroupCell : public GroupCell {
public:
  SpacerGroupCell(Configuration *config, wxCoord height)
      : GroupCell(config, GC_TYPE_INVALID), m_forcedHeight(height) {
    m_height = m_forcedHeight;
    m_width = 0;
    m_center = 0;
  }

  /**
   * @brief Forces the cell to a specific height determined by its matching
   * counterpart in another worksheet.
   */
  void Recalculate(AFontSize fontsize) const override {
    Cell::Recalculate(fontsize);
    m_height = m_forcedHeight;
    m_width = 0;
    m_center = 0;
    Reposition();
  }

  std::unique_ptr<Cell> Copy(GroupCell *parent) const override {
    (void)parent;
    return std::make_unique<SpacerGroupCell>(m_configuration, m_forcedHeight);
  }

private:
  wxCoord m_forcedHeight;
};

int DiffFrame::CurrentScrollY(size_t idx) const {
  if (idx >= m_worksheets.size())
    return 0;
  int x, y, ux, uy;
  m_worksheets[idx]->GetViewStart(&x, &y);
  m_worksheets[idx]->GetScrollPixelsPerUnit(&ux, &uy);
  return y * uy;
}

bool DiffFrame::IsDiffEntry(const DiffEntry &e) const {
  for (auto cell : e.cells) {
    if (cell && (cell->GetGroupType() == GC_TYPE_INVALID || cell->GetHighlight()))
      return true;
  }
  return false;
}

void DiffFrame::OnDiffNext(wxCommandEvent &WXUNUSED(event)) {
  int startIdx = m_currentDiffIdx + 1;
  if (!m_worksheets.empty()) {
      // Read the live scroll position: the mouse wheel/touchpad does not update
      // m_lastScrollY, so relying on the cache made "next difference" jump from
      // a stale viewport after a wheel scroll.
      int y_top = CurrentScrollY(0);
      int y_bottom = y_top + m_worksheets[0]->GetClientSize().y;
      
      bool currentVisible = false;
      if (m_currentDiffIdx >= 0 && m_currentDiffIdx < (int)m_diffEntries.size()) {
          GroupCell* cell = m_diffEntries[m_currentDiffIdx].cells[0];
          if (cell && cell->GetRect().y >= y_top && cell->GetRect().y <= y_bottom) {
              currentVisible = true;
          }
      }
      
      if (!currentVisible) {
          startIdx = 0;
          for (size_t i = 0; i < m_diffEntries.size(); ++i) {
              if (m_diffEntries[i].cells[0] && m_diffEntries[i].cells[0]->GetRect().y >= y_top) {
                  startIdx = (int)i;
                  break;
              }
          }
      }
  }

  for (int i = startIdx; i < (int)m_diffEntries.size(); ++i) {
    if (IsDiffEntry(m_diffEntries[i])) {
      m_currentDiffIdx = i;
      for (size_t j = 0; j < m_worksheets.size(); ++j) {
        if (m_diffEntries[i].cells[j]) {
          m_worksheets[j]->ScheduleScrollToCell(m_diffEntries[i].cells[j]);
          m_worksheets[j]->ScrollToCellIfNeeded();
          m_worksheets[j]->Refresh();
        }
      }
      return;
    }
  }
}

void DiffFrame::OnDiffPrev(wxCommandEvent &WXUNUSED(event)) {
  int startIdx = m_currentDiffIdx - 1;
  if (!m_worksheets.empty()) {
      // Read the live scroll position (see OnDiffNext) -- the wheel/touchpad
      // does not refresh m_lastScrollY.
      int y_top = CurrentScrollY(0);
      int y_bottom = y_top + m_worksheets[0]->GetClientSize().y;
      
      bool currentVisible = false;
      if (m_currentDiffIdx >= 0 && m_currentDiffIdx < (int)m_diffEntries.size()) {
          GroupCell* cell = m_diffEntries[m_currentDiffIdx].cells[0];
          if (cell && cell->GetRect().y >= y_top && cell->GetRect().y <= y_bottom) {
              currentVisible = true;
          }
      }
      
      if (!currentVisible) {
          startIdx = (int)m_diffEntries.size() - 1;
          for (int i = (int)m_diffEntries.size() - 1; i >= 0; --i) {
              if (m_diffEntries[i].cells[0] && m_diffEntries[i].cells[0]->GetRect().y < y_top) {
                  startIdx = i;
                  break;
              }
          }
      }
  }

  for (int i = startIdx; i >= 0; --i) {
    if (IsDiffEntry(m_diffEntries[i])) {
      m_currentDiffIdx = i;
      for (size_t j = 0; j < m_worksheets.size(); ++j) {
        if (m_diffEntries[i].cells[j]) {
          m_worksheets[j]->ScheduleScrollToCell(m_diffEntries[i].cells[j]);
          m_worksheets[j]->ScrollToCellIfNeeded();
          m_worksheets[j]->Refresh();
        }
      }
      return;
    }
  }
}

void DiffFrame::OnIdle(wxIdleEvent &event) {
  event.Skip();
  UpdateDiffNavUI();
}

void DiffFrame::UpdateDiffNavUI() {
  if (!m_toolBar)
    return;

  // A few pixels of slack so a difference sitting essentially at the viewport top
  // counts as the "current" one rather than as a difference further up or down.
  const int tol = 4;
  const int viewportTop = m_worksheets.empty() ? 0 : CurrentScrollY(0);

  int total = 0;
  int current = 0; // 1-based ordinal of the nearest difference at or above the top
  bool hasPrev = false, hasNext = false;
  for (const auto &e : m_diffEntries) {
    if (!IsDiffEntry(e))
      continue;
    ++total;
    GroupCell *cell = nullptr;
    for (auto c : e.cells) {
      if (c) { cell = c; break; }
    }
    const int y = cell ? cell->GetRect().y : 0;
    if (y < viewportTop - tol) {
      hasPrev = true;
      current = total;
    } else if (y > viewportTop + tol) {
      hasNext = true;
    } else {
      current = total;
    }
  }

  wxString status;
  if (total == 0)
    status = _("No differences");
  else if (current == 0)
    status = wxString::Format(_("%d differences"), total);
  else
    status = wxString::Format(_("Difference %d / %d"), current, total);
  if (m_diffStatus && m_diffStatus->GetLabel() != status)
    m_diffStatus->SetLabel(status);

  if (static_cast<int>(hasPrev) != m_shownPrevEnabled) {
    m_toolBar->EnableTool(EventIDs::button_diff_prev, hasPrev);
    m_shownPrevEnabled = static_cast<int>(hasPrev);
  }
  if (static_cast<int>(hasNext) != m_shownNextEnabled) {
    m_toolBar->EnableTool(EventIDs::button_diff_next, hasNext);
    m_shownNextEnabled = static_cast<int>(hasNext);
  }
}

DiffFrame::DiffFrame(wxWindow *parent, const wxArrayString &files, Configuration *config)
  : wxFrame(parent, wxID_ANY, _("wxMaxima Diff Viewer"), wxDefaultPosition, wxSize(1000, 800)),
    m_configuration(config) {
wxBoxSizer *topSizer = new wxBoxSizer(wxVERTICAL);

// Add a simple toolbar for synchronization controls
wxToolBar *toolBar = CreateToolBar();
m_toolBar = toolBar;
const int idSyncHorizontal = wxWindow::NewControlId();
const int idSyncVertical = wxWindow::NewControlId();
#if wxCHECK_VERSION(3, 1, 6)
  wxBitmapBundle syncBmp = wxArtProvider::GetBitmapBundle(wxmaximaART_SYNC_HORIZONTAL, wxART_TOOLBAR);
  toolBar->AddCheckTool(idSyncHorizontal, _("Sync Horizontal"), syncBmp, wxNullBitmap, _("Toggle horizontal scroll synchronization"));
  // There is no dedicated "sync vertical" icon, so reuse the horizontal one
  // rotated by 90°: a horizontal double-arrow becomes a vertical one.
  wxBitmapBundle syncVBmp = syncBmp;
  {
    wxBitmap hBmp = syncBmp.GetBitmap(syncBmp.GetDefaultSize());
    if (hBmp.IsOk()) {
      wxImage vImg = hBmp.ConvertToImage().Rotate90();
      if (vImg.IsOk())
        syncVBmp = wxBitmapBundle::FromBitmap(wxBitmap(vImg));
    }
  }
  toolBar->AddCheckTool(idSyncVertical, _("Sync Vertical"), syncVBmp, wxNullBitmap, _("Toggle vertical scroll synchronization"));
#else
  // old wxWidgets version. Don't use the graphic from wxMaximaArtprovider, but a standard one (wxART_MINUS)
  // not a such nice graphics, but compiles (and we are going to require wxWidgets >= 3.2 maybe soon...
  wxBitmap syncBmp = wxArtProvider::GetBitmap(wxART_MINUS, wxART_TOOLBAR);
  toolBar->AddCheckTool(idSyncHorizontal, _("Sync Horizontal"), syncBmp, wxNullBitmap, _("Toggle horizontal scroll synchronization"));
  toolBar->AddCheckTool(idSyncVertical, _("Sync Vertical"), syncBmp, wxNullBitmap, _("Toggle vertical scroll synchronization"));
#endif

toolBar->ToggleTool(idSyncHorizontal, m_syncHorizontal);
toolBar->ToggleTool(idSyncVertical, m_syncVertical);
toolBar->Bind(wxEVT_TOOL, &DiffFrame::OnToggleHorizontalSync, this, idSyncHorizontal);
toolBar->Bind(wxEVT_TOOL, &DiffFrame::OnToggleVerticalSync, this, idSyncVertical);

toolBar->AddSeparator();
#if wxCHECK_VERSION(3, 1, 6)
wxBitmapBundle prevBmp = wxArtProvider::GetBitmapBundle(wxART_GO_UP, wxART_TOOLBAR);
wxBitmapBundle nextBmp = wxArtProvider::GetBitmapBundle(wxART_GO_DOWN, wxART_TOOLBAR);
#else
wxBitmap prevBmp = wxArtProvider::GetBitmap(wxART_GO_UP, wxART_TOOLBAR);
wxBitmap nextBmp = wxArtProvider::GetBitmap(wxART_GO_DOWN, wxART_TOOLBAR);
#endif
toolBar->AddTool(EventIDs::button_diff_prev, _("Previous Difference"), prevBmp, _("Jump to previous difference"));
toolBar->AddTool(EventIDs::button_diff_next, _("Next Difference"), nextBmp, _("Jump to next difference"));
toolBar->Bind(wxEVT_TOOL, &DiffFrame::OnDiffPrev, this, EventIDs::button_diff_prev);
toolBar->Bind(wxEVT_TOOL, &DiffFrame::OnDiffNext, this, EventIDs::button_diff_next);

// Indicator showing which difference is currently in view ("Difference N / M").
// A fixed width + wxST_NO_AUTORESIZE keeps the toolbar layout stable as the
// number changes, so the idle handler can just SetLabel() without Realize().
m_diffStatus = new wxStaticText(toolBar, wxID_ANY, wxEmptyString,
                                wxDefaultPosition, wxSize(150, -1),
                                wxALIGN_CENTRE_HORIZONTAL | wxST_NO_AUTORESIZE);
toolBar->AddControl(m_diffStatus);

toolBar->AddSeparator();

  m_searchCtrl = new wxSearchCtrl(toolBar, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize(200, -1), wxTE_PROCESS_ENTER);
  m_searchCtrl->SetDescriptiveText(_("Search input / UUID"));
  m_searchCtrl->ShowCancelButton(true);
  toolBar->AddControl(m_searchCtrl);
  m_searchCtrl->Bind(wxEVT_TEXT, &DiffFrame::OnSearch, this);
  m_searchCtrl->Bind(wxEVT_TEXT_ENTER, &DiffFrame::OnSearch, this);
  m_searchCtrl->Bind(wxEVT_SEARCHCTRL_CANCEL_BTN, &DiffFrame::OnSearchCancel, this);

  toolBar->AddSeparator();
  m_searchDownRadio = new wxRadioButton(toolBar, wxID_ANY, _("Down"), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  m_searchUpRadio = new wxRadioButton(toolBar, wxID_ANY, _("Up"));
  m_searchDownRadio->SetValue(true);
  toolBar->AddControl(m_searchDownRadio);
  toolBar->AddControl(m_searchUpRadio);

  m_searchDownRadio->Bind(wxEVT_RADIOBUTTON, &DiffFrame::OnSearch, this);
  m_searchUpRadio->Bind(wxEVT_RADIOBUTTON, &DiffFrame::OnSearch, this);

  toolBar->Realize();

  // Keep the "Difference N / M" indicator and the Prev/Next button enabled-state
  // current as the user scrolls (by wheel as well as by the buttons).
  Bind(wxEVT_IDLE, &DiffFrame::OnIdle, this);

  wxBoxSizer *mainSizer = new wxBoxSizer(wxHORIZONTAL);

  m_lastScrollY.assign(files.size(), 0);

  // Initialize worksheets for each file provided
  for (size_t i = 0; i < files.size(); ++i) {
    m_worksheetConfigurations.push_back(std::make_unique<Configuration>(*m_configuration));
    Worksheet *ws = new Worksheet(this, wxID_ANY, m_worksheetConfigurations.back().get());
    m_worksheets.push_back(ws);
    ws->SetCurrentFile(files[i]);
    mainSizer->Add(ws, 1, wxEXPAND);
    
    // Bind all vertical scroll event types to our synchronization handler
    ws->Bind(wxEVT_SCROLLWIN_THUMBTRACK, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_THUMBRELEASE, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_LINEUP, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_LINEDOWN, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_PAGEUP, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_PAGEDOWN, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_TOP, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_BOTTOM, &DiffFrame::OnScroll, this);

    // Bind horizontal scroll events
    ws->Bind(wxEVT_SCROLLWIN_THUMBTRACK, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_THUMBRELEASE, &DiffFrame::OnScroll, this);

    // The mouse wheel / touchpad does not emit wxEVT_SCROLLWIN_* on all
    // platforms (notably Windows), so the scrollbar-only bindings above miss
    // it and the panes stop following each other. Catch the wheel directly:
    // let the worksheet scroll, then sync the others to its new position once
    // the scroll has actually been applied.
    ws->Bind(wxEVT_MOUSEWHEEL, [this, i](wxMouseEvent &ev) {
      ev.Skip();
      const int idx = static_cast<int>(i);
      CallAfter([this, idx] {
        if (idx < static_cast<int>(m_worksheets.size()))
          SyncScrollFrom(idx, CurrentScrollY(static_cast<size_t>(idx)));
      });
    });
  }

  topSizer->Add(mainSizer, 1, wxEXPAND);
  SetSizer(topSizer);
  
  // Align cells between files and populate the worksheets
  AlignCells();
  
  m_resizeTimer.SetOwner(this);
  Bind(wxEVT_TIMER, [this](wxTimerEvent &) { RelayoutWorksheets(); });
  Bind(wxEVT_SIZE, [this](wxSizeEvent &event) {
    event.Skip();
    // A drag-resize fires a burst of size events; re-laying out every cell of
    // every worksheet on each one is O(cells) and made big worksheets feel
    // quadratic. Debounce: restart a short timer and relayout once, after the
    // resize settles. Using a timer (rather than CallAfter per event) both
    // coalesces the burst and still fires without an idle loop, and runs after
    // the sizer has resized the worksheets, so the newly exposed area is not
    // left blank (seen on Windows).
    m_resizeTimer.StartOnce(40);
  });

  // Lay the worksheets out exactly once, after the frame has been shown and
  // sized. AlignCells() already recalculated the individual cells (to measure
  // their heights), and showing the frame fires a wxEVT_SIZE that runs
  // RelayoutWorksheets() at the real client size. Doing an extra full
  // Recalculate() here, at the default pre-show size, is wasted work that the
  // first resize immediately throws away -- it was making the diff viewer
  // recalculate the whole worksheet several times during startup. This timer is
  // a belt-and-braces trigger in case no size event arrives.
  m_resizeTimer.StartOnce(40);
}

DiffFrame::~DiffFrame() {}

void DiffFrame::RelayoutWorksheets() {
  if (m_worksheets.empty())
    return;
  // Cell line-wrapping (and hence the whole layout) only changes when the
  // available width changes. A shown/resized window emits several size events
  // that often share the same width (height-only changes, scrollbar appearing,
  // ...); re-running ResetSize_RecursivelyList()+Recalculate() for each of them
  // is wasted work that re-touched every cell (and re-queried every font). Skip
  // it unless the width actually changed.
  const int width = m_worksheets[0]->GetClientSize().x;
  if (width == m_lastLayoutWidth)
    return;
  m_lastLayoutWidth = width;

  for (auto ws : m_worksheets) {
    ws->UpdateConfigurationClientSize();
    if (ws->GetTree()) {
      ws->GetTree()->ResetSize_RecursivelyList();
      ws->Recalculate();
    }
    ws->AdjustSize();
    ws->Refresh();
  }
}

void DiffFrame::OnToggleHorizontalSync(wxCommandEvent &event) {
  m_syncHorizontal = event.IsChecked();
}

void DiffFrame::OnToggleVerticalSync(wxCommandEvent &event) {
  m_syncVertical = event.IsChecked();
}

void DiffFrame::OnSearch(wxCommandEvent &WXUNUSED(event)) {
  wxString query = m_searchCtrl->GetValue();
  if (query.IsEmpty()) {
      for (auto ws : m_worksheets) ws->ClearSelection();
      return;
  }

  m_searchDown = m_searchDownRadio->GetValue();

  // UUIDs in wxMaxima look like {UUID-STRING}
  bool isUUID = query.StartsWith(wxS("{")) && query.EndsWith(wxS("}"));
  
  if (isUUID) {
      for (size_t i = 0; i < m_diffEntries.size(); ++i) {
          for (size_t j = 0; j < m_worksheets.size(); ++j) {
              if (m_diffEntries[i].cells[j] && m_diffEntries[i].cells[j]->GetUUID() == query) {
                  // Found the UUID. Scroll the worksheet that contains it.
                  // Sync logic will handle the other worksheets.
                  m_worksheets[j]->ScheduleScrollToCell(m_diffEntries[i].cells[j]);
                  m_worksheets[j]->ScrollToCellIfNeeded();
                  m_worksheets[j]->Refresh();
                  m_worksheets[j]->Update();
                  return;
              }
          }
      }
  } else {
      // Incremental search in input cells only
      for (auto ws : m_worksheets) {
          if (ws->FindIncremental(query, m_searchDown, true, true, false)) {
              // Found a match. The worksheet should have requested a redraw/scroll.
              // We force it here to provide immediate feedback.
              ws->ScrollToCaret();
              ws->ScrollToCaretIfNeeded();
              ws->ScrollToCellIfNeeded();
              ws->Refresh();
              ws->Update();
              break; 
          }
      }
  }
}

void DiffFrame::OnSearchCancel(wxCommandEvent &WXUNUSED(event)) {
  m_searchCtrl->SetValue(wxEmptyString);
  for (auto ws : m_worksheets) {
      ws->ClearSelection();
  }
}

void DiffFrame::OnScroll(wxScrollWinEvent &event) {
  // Prevent recursive calls when we manually scroll other worksheets
  if (m_syncing) return;
  
  Worksheet* ws_src = dynamic_cast<Worksheet*>(event.GetEventObject());
  if (!ws_src) { event.Skip(); return; }
  
  // Identify which worksheet triggered the scroll
  int src_idx = -1;
  for (size_t i = 0; i < m_worksheets.size(); ++i) {
      if (m_worksheets[i] == ws_src) { src_idx = (int)i; break; }
  }
  if (src_idx == -1) { event.Skip(); return; }

  // We only synchronize vertical scrolling by default
  int orientation = event.GetOrientation();
  if (orientation == wxHORIZONTAL) {
      if (!m_syncHorizontal) { event.Skip(); return; }
      
      m_syncing = true;
      int x_new = event.GetPosition();
      for (auto ws : m_worksheets) {
          if (ws != ws_src) {
              int _, uy_other;
              ws->GetScrollPixelsPerUnit(&_, &uy_other);
              ws->Scroll(x_new, -1);
          }
      }
      m_syncing = false;
      event.Skip();
      return;
  }

  if (orientation != wxVERTICAL) { event.Skip(); return; }

  // Vertical scroll synchronization can be turned off independently of the
  // horizontal one: then let this worksheet scroll on its own and don't move
  // the others.
  if (!m_syncVertical) { event.Skip(); return; }

  // Get scroll units (pixels per scroll step)
  int ux, uy;
  ws_src->GetScrollPixelsPerUnit(&ux, &uy);

  // Determine the new scroll position in pixels
  int y_new_src_units = event.GetPosition();
  if (y_new_src_units == -1) {
      // For some events (like LINEUP/PAGEDOWN), the position is not in the event
      int x, y;
      ws_src->GetViewStart(&x, &y);
      y_new_src_units = y;
      
      auto type = event.GetEventType();
      if (type == wxEVT_SCROLLWIN_LINEUP) y_new_src_units--;
      else if (type == wxEVT_SCROLLWIN_LINEDOWN) y_new_src_units++;
      else if (type == wxEVT_SCROLLWIN_PAGEUP) {
          y_new_src_units -= ws_src->GetClientSize().y / uy;
      }
      else if (type == wxEVT_SCROLLWIN_PAGEDOWN) {
          y_new_src_units += ws_src->GetClientSize().y / uy;
      }
      else if (type == wxEVT_SCROLLWIN_TOP) y_new_src_units = 0;
      else if (type == wxEVT_SCROLLWIN_BOTTOM) {
          int w, vh;
          ws_src->GetVirtualSize(&w, &vh);
          y_new_src_units = vh / uy;
      }
  }
  
  // Clamp the new scroll position to valid range
  int vw_src, vh_src;
  ws_src->GetVirtualSize(&vw_src, &vh_src);
  int max_y_units_src = std::max(0, (vh_src - ws_src->GetClientSize().y + uy - 1) / uy);
  y_new_src_units = std::max(0, std::min(y_new_src_units, max_y_units_src));

  int y_new_src = y_new_src_units * uy;
  SyncScrollFrom(src_idx, y_new_src);
  event.Skip();
}

void DiffFrame::SyncScrollFrom(int src_idx, int y_new_src) {
  if (m_syncing) return;
  if (!m_syncVertical) return;
  if (src_idx < 0 || src_idx >= static_cast<int>(m_worksheets.size())) return;
  int y_old_src = m_lastScrollY[src_idx];
  
  // If position hasn't changed, nothing to do
  if (y_new_src == y_old_src) return;

  m_syncing = true;

  // Ensure all worksheets have updated their cell positions
  for (auto ws : m_worksheets) ws->RecalculateIfNeeded();

  // Collect the per-diff-entry top coordinates of the source worksheet's cells.
  std::vector<int> srcTops(m_diffEntries.size());
  for (size_t k = 0; k < m_diffEntries.size(); ++k) {
    GroupCell *cell = m_diffEntries[k].cells[src_idx];
    srcTops[k] = cell ? cell->GetRect().y : DIFFSYNC_NO_CELL;
  }

  // Scroll every other worksheet to keep it aligned with the source. A single
  // top-anchored rule handles both scroll directions; the geometry lives in the
  // GUI-free, unit-tested ComputeSyncedScrollY() (see DiffScrollSync.h).
  for (size_t j = 0; j < m_worksheets.size(); ++j) {
    if (j == static_cast<size_t>(src_idx)) continue;
    std::vector<int> otherTops(m_diffEntries.size());
    for (size_t k = 0; k < m_diffEntries.size(); ++k) {
      GroupCell *cell = m_diffEntries[k].cells[j];
      otherTops[k] = cell ? cell->GetRect().y : DIFFSYNC_NO_CELL;
    }
    const int target = ComputeSyncedScrollY(srcTops, otherTops, y_new_src);
    if (target != DIFFSYNC_NO_CELL) {
      int ux_other, uy_other;
      m_worksheets[j]->GetScrollPixelsPerUnit(&ux_other, &uy_other);
      if (uy_other > 0)
        m_worksheets[j]->Scroll(-1, target / uy_other);
    }
  }

  // Finalize: Record the actual scroll positions of all worksheets
  for (size_t i = 0; i < m_worksheets.size(); ++i) {
      int x, y;
      m_worksheets[i]->GetViewStart(&x, &y);
      int _, uy_i;
      m_worksheets[i]->GetScrollPixelsPerUnit(&_, &uy_i);
      m_lastScrollY[i] = y * uy_i;
  }

  m_syncing = false;
}

static std::unique_ptr<GroupCell> LoadTree(const wxString &file, Configuration *config) {
    if (file.Lower().EndsWith(wxS(".wxm"))) {
      wxTextFile text(file);
      if (text.Open()) {
        return Format::ParseWXMFile(text, config);
      }
    } else if (file.Lower().EndsWith(wxS(".wxmx"))) {
        wxXmlDocument xmldoc;
        wxFileInputStream wxmxFile(file);
        if (!wxmxFile.IsOk()) return nullptr;
        wxZipInputStream wxmxContents(wxmxFile);
        while (wxZipEntry *entry = wxmxContents.GetNextEntry()) {
            if (entry->GetName() == wxS("content.xml")) {
                xmldoc.Load(wxmxContents);
                delete entry;
                break;
            }
            delete entry;
        }
        if (xmldoc.IsOk()) {
            MathParser parser(config, file);
            return parser.CreateTreeFromXMLNode(xmldoc.GetRoot());
        }
    }
    return nullptr;
}

void DiffFrame::LoadFiles(const wxArrayString &WXUNUSED(files)) {
}

/**
 * @brief Coordinates the cell-by-cell alignment of multiple files.
 *
 * This method:
 * 1. Loads the cell trees for each file.
 * 2. Extracts UUIDs for matching.
 * 3. Performs multi-way alignment (supporting 2 or 3 files).
 * 4. Populates the worksheets with either matched cells (highlighted if 
 *    content differs) or SpacerGroupCells for gaps.
 */
void DiffFrame::AlignCells() {
  size_t numFiles = m_worksheets.size();
  if (numFiles < 2) return;

  std::vector<std::unique_ptr<GroupCell>> sourceTrees;
  std::vector<std::vector<GroupCell*>> cellLists;
  std::vector<std::vector<Diff::CellMatchData>> matchDataLists;

  // Extract cell structure and metadata from each file
  for (size_t i = 0; i < numFiles; ++i) {
      // Parse with the per-worksheet Configuration (whose worksheet pointer the
      // Worksheet ctor set), not the app-wide m_configuration (which has no
      // worksheet in --diff mode). Otherwise the cells' GetWorksheet() returns
      // null -> assert in debug, null-deref of GetCellPointers() in release.
      sourceTrees.push_back(LoadTree(m_worksheets[i]->GetCurrentFile(),
                                     m_worksheetConfigurations[i].get()));
      std::vector<GroupCell*> cells;
      std::vector<Diff::CellMatchData> matchData;
      if (sourceTrees.back()) {
          for (auto &c : OnList(sourceTrees.back().get())) {
              cells.push_back(&c);
              Diff::CellMatchData md;
              md.uuid = c.GetUUID();
              md.type = c.GetGroupType();
              auto ed = c.GetEditable();
              if (ed) md.content = ed->GetValue();
              matchData.push_back(md);
          }
      }
      cellLists.push_back(cells);
      matchDataLists.push_back(matchData);
  }

  // finalAlignment will contain rows of indices into each tree
  std::vector<std::vector<int>> finalAlignment; 

  if (numFiles == 2) {
      int threshold = Diff::FindOptimalThreshold(matchDataLists[0], matchDataLists[1]);
      auto a = Diff::Align2(matchDataLists[0], matchDataLists[1], threshold);
      for (auto const &p : a) {
          finalAlignment.push_back({p.first, p.second});
      }
  } else if (numFiles == 3) {
      // For 3-way diff, we perform a 2-step alignment
      int t12 = Diff::FindOptimalThreshold(matchDataLists[0], matchDataLists[1]);
      auto a12 = Diff::Align2(matchDataLists[0], matchDataLists[1], t12);
      
      // Create a "merged" metadata sequence for comparison with file 3
      std::vector<Diff::CellMatchData> matchData12;
      for (auto const &p : a12) {
          if (p.first != -1) matchData12.push_back(matchDataLists[0][p.first]);
          else matchData12.push_back(matchDataLists[1][p.second]);
      }
      
      int t123 = Diff::FindOptimalThreshold(matchData12, matchDataLists[2]);
      auto a123 = Diff::Align2(matchData12, matchDataLists[2], t123);
      
      int idx12 = 0;
      for (auto const &p : a123) {
          if (p.first != -1) {
              finalAlignment.push_back({a12[idx12].first, a12[idx12].second, p.second});
              idx12++;
          } else {
              finalAlignment.push_back({-1, -1, p.second});
          }
      }
  }

  // Clear existing worksheet content and rebuild using the alignment
  for (auto ws : m_worksheets) ws->DestroyTree();

  m_diffEntries.clear();
  std::vector<CellListBuilder<GroupCell>> builders(numFiles);
  for (auto const &row : finalAlignment) {
      // Calculate the maximum height for this "row" of cells
      std::vector<wxCoord> heights;
      for (size_t i = 0; i < numFiles; ++i) {
          if (row[i] != -1) {
              cellLists[i][row[i]]->Recalculate(m_configuration->GetDefaultFontSize());
              heights.push_back(cellLists[i][row[i]]->GetHeight());
          }
      }
      wxCoord maxHeight = heights.empty() ? 0 : *std::max_element(heights.begin(), heights.end());

      DiffEntry entry = { {nullptr, nullptr, nullptr} };
      for (size_t i = 0; i < numFiles; ++i) {
          if (row[i] != -1) {
              // Cell exists in this file: Insert a copy
              auto copy = std::unique_ptr<GroupCell>(dynamic_cast<GroupCell*>(cellLists[i][row[i]]->Copy(nullptr).release()));
              
              // Check if the cell content differs from any of the matching cells in other files
              bool modified = false;
              for (size_t j = 0; j < numFiles; ++j) {
                  if (i != j) {
                      if (row[j] == -1) {
                          // Matching cell doesn't exist in file j
                          modified = true;
                      } else {
                          // Check if textual content differs
                          auto ed1 = cellLists[i][row[i]]->GetEditable();
                          auto ed2 = cellLists[j][row[j]]->GetEditable();
                          if (ed1 && ed2) {
                              if (ed1->GetValue() != ed2->GetValue())
                                  modified = true;
                          } else if (ed1 != ed2) {
                              modified = true;
                          }
                      }
                  }
              }
              if (modified) copy->SetHighlight(true);
              entry.cells[i] = builders[i].Append(std::move(copy));
          } else {
              // Gap in this file: Insert a SpacerGroupCell to maintain alignment.
              // Use this worksheet's Configuration (see the note in AlignCells)
              // so the spacer's GetWorksheet() is valid.
              entry.cells[i] = builders[i].Append(
                  std::make_unique<SpacerGroupCell>(m_worksheetConfigurations[i].get(), maxHeight));
          }
      }
      m_diffEntries.push_back(entry);
  }
  for (size_t i = 0; i < numFiles; ++i) {
      m_worksheets[i]->InsertGroupCells(builders[i].TakeHead());
  }
}


