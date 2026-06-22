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

#ifndef DIFFFRAME_H
#define DIFFFRAME_H

#include "Worksheet.h"
#include <wx/wx.h>
#include <wx/srchctrl.h>
#include <vector>
#include <memory>

class DiffFrame : public wxFrame
{
public:
  DiffFrame(wxWindow *parent, const wxArrayString &files, Configuration *config);
  virtual ~DiffFrame();

private:
  void OnScroll(wxScrollWinEvent &event);
  void OnToggleHorizontalSync(wxCommandEvent &event);
  void OnToggleVerticalSync(wxCommandEvent &event);
  void OnSearch(wxCommandEvent &event);
  void OnSearchCancel(wxCommandEvent &event);
  void OnDiffNext(wxCommandEvent &event);
  void OnDiffPrev(wxCommandEvent &event);

  //! The current vertical scroll offset (in pixels) of worksheet @p idx, read
  //! live from the worksheet rather than from the m_lastScrollY cache (which is
  //! only refreshed by scrollbar events, not by the mouse wheel/touchpad).
  int CurrentScrollY(size_t idx) const;
  //! Synchronizes the other worksheets to worksheet @p src_idx, whose viewport
  //! top is now at @p y_new_src pixels. Shared by the scrollbar handler
  //! (OnScroll) and the mouse-wheel handler.
  void SyncScrollFrom(int src_idx, int y_new_src);

  std::vector<Worksheet *> m_worksheets;
  std::vector<std::unique_ptr<Configuration>> m_worksheetConfigurations;
  Configuration *m_configuration;
  wxSearchCtrl *m_searchCtrl = nullptr;
  wxRadioButton *m_searchDownRadio = nullptr;
  wxRadioButton *m_searchUpRadio = nullptr;
  int m_currentDiffIdx = -1;
  bool m_syncing = false;
  bool m_syncHorizontal = true;
  bool m_syncVertical = true;
  bool m_searchDown = true;

  void LoadFiles(const wxArrayString &files);
  void AlignCells();

  struct DiffEntry {
    GroupCell *cells[3]; // 0, 1, 2
  };
  std::vector<DiffEntry> m_diffEntries;
  std::vector<int> m_lastScrollY;

  //! True if diff entry @p e represents an actual difference (a missing cell, or
  //! a cell highlighted as changed) rather than an aligned/identical region.
  bool IsDiffEntry(const DiffEntry &e) const;
  //! Updates the "Difference N / M" indicator and grays out the Prev/Next
  //! difference buttons when there is nothing to jump to. Called from the idle
  //! handler so it tracks both button navigation and free wheel/touchpad
  //! scrolling.
  void UpdateDiffNavUI();
  void OnIdle(wxIdleEvent &event);
  //! Index of the next (@p direction == +1) / previous (-1) difference to jump
  //! to, or -1 if there is none; falls back to the live viewport when the current
  //! difference is off-screen. Shared by the buttons and the button-greying.
  int FindAdjacentDiff(int direction) const;
  //! Makes difference @p idx the current one: selects its cell in each pane (so
  //! the bracket is drawn highlighted and visibly jumps) and scrolls it into view.
  void SetCurrentDiff(int idx);

  wxToolBar *m_toolBar = nullptr;
  //! The "Difference N / M" indicator shown in the toolbar.
  wxStaticText *m_diffStatus = nullptr;
  //! Cached enabled-state of the Prev/Next tools so the idle handler only touches
  //! the toolbar when it actually changes (-1 = not yet applied).
  int m_shownPrevEnabled = -1;
  int m_shownNextEnabled = -1;

  //! Debounces the expensive relayout done on window resize: a drag produces a
  //! burst of size events and re-laying out every cell of every worksheet on
  //! each one made resizing a big worksheet feel quadratic. The timer is
  //! restarted on every size event and relays out once, after the resize
  //! settles. (See the wxEVT_SIZE handler.)
  wxTimer m_resizeTimer;
  void RelayoutWorksheets();
  //! The client width the worksheets were last laid out for. Cell line-wrapping
  //! only depends on the width, so RelayoutWorksheets() can skip the expensive
  //! full re-layout when the width has not changed -- which collapses the burst
  //! of same-width size events a window emits while it is being shown.
  int m_lastLayoutWidth = -1;
};

#endif // DIFFFRAME_H
