// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include <wx/wx.h>
#include <wx/aui/aui.h>

#ifndef _WXMAXIMA_TOOLBAR_H
#define _WXMAXIMA_TOOLBAR_H

class ToolBar
{
 public:
#if defined __WXGTK__
  wxBitmap GetImage(wxString img);
#else
  wxImage GetImage(wxString img);
#endif
  ToolBar(wxToolBar *toolbar);
  virtual ~ToolBar();
  //! Show that user input is needed for maxima to continue
  void ShowUserInputBitmap() {
    m_toolBar->SetToolNormalBitmap(tb_follow, m_needsInformationIcon);
  }
  //! Stop showing that user input is needed for maxima to continue
  void ShowFollowBitmap() {
    m_toolBar->SetToolNormalBitmap(tb_follow,m_followIcon);
  }
  void EnableTool(int id, bool enable) {
    m_toolBar->EnableTool(id, enable);
  }
  wxToolBar *GetToolBar() {
    return m_toolBar;
  }
  /*! A list of all events the Toolbar can receive
   */
  enum Event {
    plot_slider_id = 5500,
    tb_new,
    tb_open,
    tb_save,
    tb_copy,
    tb_paste,
    tb_cut,
    tb_select_all,
    tb_print,
    tb_pref,
    tb_interrupt,
    tb_follow,
    tb_help,
    tb_animation_start,
    tb_animation_stop,
    tb_find,
    menu_restart_id
  };

  //! The slider for animations
  wxSlider* m_plotSlider;
#if defined __WXGTK__
  wxBitmap  m_followIcon;
  wxBitmap  m_needsInformationIcon;
#else
  wxImage m_followIcon;
  wxImage  m_needsInformationIcon;
#endif
  

private:
  wxToolBar *m_toolBar;
};

#endif
