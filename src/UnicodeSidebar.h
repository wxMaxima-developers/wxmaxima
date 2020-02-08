// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file

  This file contains the definition of the class Unicodesidebar that allows to 
  select arbitrary unicode symbols.
 */
#include <wx/wx.h>
#include <wx/grid.h>

#ifndef UNICODESIDEBAR_H
#define UNICODESIDEBAR_H

/*! This class generates a pane containing the last commands that were issued.

 */
class UnicodeSidebar : public wxPanel
{
public:
  UnicodeSidebar(wxWindow *parent, wxWindow *worksheet);

  /* The destructor

   */
  ~UnicodeSidebar();

  //! The popup menu IDs this sidebar uses
  enum PopIds
  {
    popid_addToSymbols = wxID_HIGHEST + 3500,
  };

  //! Is called if the RegEx changes
  void OnRegExEvent(wxCommandEvent &ev);
  //! Update the display after the regex has changed
  void UpdateDisplay();

protected:
  void OnMenu(wxCommandEvent &event);
  void OnPaint(wxPaintEvent &event);
  void OnSize(wxSizeEvent &event);
  void OnDClick(wxGridEvent &event);
  void OnRightClick(wxGridEvent &event);
  void OnChangeAttempt(wxGridEvent &event);
  
private:
  bool m_initialized;
  long m_charRightClickedOn;
  wxWindow *m_worksheet;
  wxGrid *m_grid;
  wxTextCtrl *m_regex;
};

class SidebarKeyEvent: public wxCommandEvent
{
public:
	explicit SidebarKeyEvent(int id = 0)
        		:  wxCommandEvent(id) { }
 
	explicit SidebarKeyEvent(const SidebarKeyEvent& event)
          :  wxCommandEvent(event) {}
};

class SymboladdEvent: public wxCommandEvent
{
public:
	explicit SymboladdEvent(int id = 0)
        		:  wxCommandEvent(id) { }
 
	explicit SymboladdEvent(const SymboladdEvent& event)
          :  wxCommandEvent(event) {}
};

wxDECLARE_EVENT(SIDEBARKEYEVENT, SidebarKeyEvent);
wxDECLARE_EVENT(SYMBOLADDEVENT, SymboladdEvent);

#endif // UNICODESIDEBAR_H
