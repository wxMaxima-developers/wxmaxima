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
  void OnSearch(wxCommandEvent &event);
  void OnSearchCancel(wxCommandEvent &event);
  void OnDiffNext(wxCommandEvent &event);
  void OnDiffPrev(wxCommandEvent &event);

  std::vector<Worksheet *> m_worksheets;
  std::vector<std::unique_ptr<Configuration>> m_worksheetConfigurations;
  Configuration *m_configuration;
  wxSearchCtrl *m_searchCtrl = nullptr;
  wxRadioButton *m_searchDownRadio = nullptr;
  wxRadioButton *m_searchUpRadio = nullptr;
  int m_currentDiffIdx = -1;
  bool m_syncing = false;
  bool m_syncHorizontal = true;
  bool m_searchDown = true;

  void LoadFiles(const wxArrayString &files);
  void AlignCells();

  struct DiffEntry {
    GroupCell *cells[3]; // 0, 1, 2
  };
  std::vector<DiffEntry> m_diffEntries;
  std::vector<int> m_lastScrollY;
};

#endif // DIFFFRAME_H
