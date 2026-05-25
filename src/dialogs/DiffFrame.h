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
#include <vector>
#include <memory>

class DiffFrame : public wxFrame
{
public:
  DiffFrame(wxWindow *parent, const wxArrayString &files, Configuration *config);
  virtual ~DiffFrame();

private:
  void OnScroll(wxScrollWinEvent &event);
  void SyncScroll();

  std::vector<Worksheet *> m_worksheets;
  Configuration *m_configuration;
  bool m_syncing = false;

  void LoadFiles(const wxArrayString &files);
  void AlignCells();

  struct DiffEntry {
    wxString uuid;
    GroupCell *cells[3]; // 0, 1, 2
  };
};

#endif // DIFFFRAME_H
