// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file declares the class ToolBar that represents wxMaxima's main tool bar.
 */

#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/choice.h>
#include "SlideShowCell.h"

#define MAX(a, b) ((a)>(b) ? (a) : (b))
#define MIN(a, b) ((a)>(b) ? (b) : (a))

#ifndef _WXMAXIMA_TOOLBAR_H
#define _WXMAXIMA_TOOLBAR_H

class ToolBar
{
public:
  /*! All states the "start/stop animation" toolbar button can be in
   */
  enum AnimationStartStopState
  {
    Running, //!< The animation is running
    Stopped, //!< The animation is stopped
    Inactive //!< No animation is currently running
  };
  
  wxImage GetImage(wxString img);

  ToolBar(wxToolBar *tbar);

  virtual ~ToolBar();

  //! Show that user input is needed for maxima to continue
  void ShowUserInputBitmap()
  {
    if (!m_needsInformation)
    {
      m_toolBar->SetToolNormalBitmap(tb_follow, m_needsInformationIcon);
      m_needsInformation = true;
    }
  }

  //! Stop showing that user input is needed for maxima to continue
  void ShowFollowBitmap()
  {
    if (m_needsInformation)
    {
      m_toolBar->SetToolNormalBitmap(tb_follow, m_followIcon);
      m_needsInformation = false;
    }
  }

  void EnableTool(int id, bool enable)
  {
    m_toolBar->EnableTool(id, enable);
  }

  wxToolBar *GetToolBar()
  {
    return m_toolBar;
  }

  void AnimationButtonState(AnimationStartStopState state);

  /*! A list of all events the Toolbar can receive
   */
  enum Event
  {
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
    tb_evaltillhere,
    tb_evaluate_rest,
    tb_help,
    tb_animation_startStop,
    tb_animation_start,
    tb_animation_stop,
    tb_find,
    tb_hideCode,
    tb_changeStyle,
    menu_restart_id
  };

  //! The slider for animations
  wxSlider *m_plotSlider;

#if defined __WXGTK__
  wxBitmap  m_followIcon;
  wxBitmap  m_needsInformationIcon;
  wxBitmap  m_PlayButton;
  wxBitmap  m_StopButton;
#else
  wxImage m_followIcon;
  wxImage m_needsInformationIcon;
  wxImage m_PlayButton;
  wxImage m_StopButton;
#endif

  void CanCopy(bool value)
  {
    if (value != m_canCopy_old)
    {
      EnableTool(tb_copy, value);
      m_canCopy_old = value;
    }
  }

  void CanCut(bool value)
  {
    if (value != m_canCut_old)
    {
      EnableTool(tb_cut, value);
      m_canCut_old = value;
    }
  }

  void CanSave(bool value)
  {
    if (value != m_canSave_old)
    {
      EnableTool(tb_save, value);
      m_canSave_old = value;
    }
  }

  void CanPrint(bool value)
  {
    if (value != m_canPrint_old)
    {
      EnableTool(tb_print, value);
      m_canPrint_old = value;
    }
  }

  void CanEvalTillHere(bool value)
    {
    if (value != m_canEvalTillHere_old)
    {
      EnableTool(tb_evaltillhere, value);
      m_canEvalTillHere_old = value;
    }
  }

  //! Updates the slider to match the Slide Show cell.
  void UpdateSlider(SlideShow *cell);

  //! Get the cell style for new cells
  int GetCellStyle();
  //! Set the cell style to show for the current cell
  void SetCellStyle(int style);
  //! Called if there is no cell to show the style for
  void UnsetCellStyle(){SetCellStyle(-1);}
  //! Set the default cell style for new cells
  void SetDefaultCellStyle(int style)
    {
      m_defaultCellStyle = style;
    }
  //! The current style is the new style for new cells
  void SetDefaultCellStyle();
  
private:
  //! The default style for new cells.
  int m_defaultCellStyle;
  //! The drop-down-box for text styles
  wxChoice *m_textStyle;
  //! The position in the current slideshow at the last call of UpdateSlider()
  int m_slideShowDisplayedIndex;
  //! The length of the current slideshow at the last call of UpdateSlider()
  int m_slideShowMaxIndex;
  bool m_canCopy_old;
  bool m_canCut_old;
  bool m_canSave_old;
  bool m_canPrint_old;
  bool m_canEvalTillHere_old;
  wxToolBar *m_toolBar;
  AnimationStartStopState m_AnimationStartStopState;
  //! True if we show the "needs information" button.
  bool m_needsInformation;

};

#endif
