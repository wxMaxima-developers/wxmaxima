//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef _TOOLBAR_H
#define _TOOLBAR_H

class ToolBar: public wxToolBar
{
 public:
  ToolBar(wxWindow* parent, int id);
  //! Show that user input is needed for maxima to continue
  void ShowUserInputBitmap(){SetToolNormalBitmap(tb_follow,m_needsInformationIcon);}
  //! Stop showing that user input is needed for maxima to continue
  void ShowFollowBitmap(){SetToolNormalBitmap(tb_follow,m_followIcon);}
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
    tb_find
  };

  //! The slider for animations
  wxSlider* m_plotSlider;
  wxBitmap  m_followIcon;
  wxBitmap  m_needsInformationIcon;
};

#endif
