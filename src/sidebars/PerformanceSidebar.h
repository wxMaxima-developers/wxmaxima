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

#ifndef PERFORMANCESIDEBAR_H
#define PERFORMANCESIDEBAR_H

#include <wx/wx.h>
#include <wx/panel.h>
#include <wx/stattext.h>
#include <wx/stopwatch.h>
#include <map>

class PerformanceSidebar : public wxScrolled<wxPanel>
{
public:
  explicit PerformanceSidebar(wxWindow *parent, int ID = wxID_ANY);
  //! Refresh the displayed statistics. Called from the idle loop; throttles
  //! itself and only touches labels whose value actually changed.
  void UpdateContents();

private:
  std::map<wxString, wxStaticText*> m_valueLabels;
  //! Time since the last displayed refresh, for throttling UpdateContents().
  wxStopWatch m_sinceLastUpdate;
  void AddStat(wxSizer* sizer, const wxString& label, const wxString& key);
};

#endif // PERFORMANCESIDEBAR_H
