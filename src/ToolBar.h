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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares the class ToolBar that represents wxMaxima's main tool bar.
 */

#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/choice.h>
#include "SlideShowCell.h"
#include "GroupCell.h"

#ifndef _WXMAXIMA_TOOLBAR_H
#define _WXMAXIMA_TOOLBAR_H

class ToolBar : public wxAuiToolBar
{
public:
  explicit ToolBar(wxWindow *parent);
  /*! All states the "start/stop animation" toolbar button can be in
   */
  enum AnimationStartStopState
  {
    Running, //!< The animation is running
    Stopped, //!< The animation is stopped
    Inactive //!< No animation is currently running
  };

  enum popupitems
  {
    copy_paste,
    open_save,
    print,
    options,
    shownew,
    search,
    help,
    selectAll
  };

  wxBitmap GetBitmap(wxString name,
                     unsigned char *data, size_t len, wxSize siz = wxSize(-1,-1));

  virtual ~ToolBar();

  //! Show that user input is needed for maxima to continue
  void ShowUserInputBitmap()
  {
    if (!m_needsInformation)
    {
      SetToolBitmap(tb_follow, m_needsInformationIcon);
      m_needsInformation = true;
    }
  }

  //! Stop showing that user input is needed for maxima to continue
  void ShowFollowBitmap()
  {
    if (m_needsInformation)
    {
      SetToolBitmap(tb_follow, m_followIcon);
      m_needsInformation = false;
    }
  }

  void AnimationButtonState(AnimationStartStopState state);

  /*! A list of all events the Toolbar can receive
   */
  enum Event
  {
    plot_slider_id = 5500,
    tb_interrupt,
    tb_follow,
    tb_eval,
    tb_eval_all,
    tb_evaltillhere,
    tb_evaluate_rest,
    tb_animation_startStop,
    tb_animation_start,
    tb_animation_stop,
    tb_hideCode,
    tb_changeStyle,
    menu_restart_id
  };

  //! The slider for animations
  wxSlider *m_plotSlider;

  wxBitmap  m_followIcon;
  wxBitmap  m_needsInformationIcon;
  wxBitmap  m_PlayButton;
  wxBitmap  m_StopButton;

  void CanCopy(bool value)
  {
    if (value != m_canCopy_old)
    {
      EnableTool(wxID_COPY, value);
      m_canCopy_old = value;
    }
  }

  void CanCut(bool value)
  {
    if (value != m_canCut_old)
    {
      EnableTool(wxID_CUT, value);
      m_canCut_old = value;
    }
  }

  void CanSave(bool value)
  {
    if (value != m_canSave_old)
    {
      EnableTool(wxID_SAVE, value);
      m_canSave_old = value;
    }
  }

  void CanPrint(bool value)
  {
    if (value != m_canPrint_old)
    {
      EnableTool(wxID_PRINT, value);
      m_canPrint_old = value;
    }
  }

  void CanEvalTillHere(bool value)
    {
    if (value != m_canEvalTillHere_old)
    {
      EnableTool(tb_evaltillhere, value);
      EnableTool(tb_evaluate_rest, value);
      m_canEvalTillHere_old = value;
    }
  }

  void CanEvalThisCell(bool value)
    {
      if (value != m_canEvalThisCell_old)
      {
        EnableTool(tb_eval, value);
        m_canEvalThisCell_old = value;
      }
    }

  void WorksheetEmpty(bool value)
    {
      if (value != m_worksheetEmpty_old)
      {
        EnableTool(tb_eval_all, !value);
        m_worksheetEmpty_old = value;
      }
    }

  //! Updates the slider to match the Slide Show cell.
  void UpdateSlider(SlideShow *cell);

  int GetIdealHeight(){
    return m_needsInformationIcon.GetSize().y;
  }
  
  //! Remove all ools and then add all tools that are requested/fit in the toolbar/...
  void AddTools();

  //! Get the cell style for new cells
  GroupType GetCellType();
  //! Set the cell style to show for the current cell
  void SetCellStyle(int style);
  //! Called if there is no cell to show the style for
  void UnsetCellStyle(){SetCellStyle(-1);}
  //! Set the default cell style for new cells
  void SetDefaultCellStyle(int style) { m_defaultCellStyle = style; }
  //! The current style is the new style for new cells
  void SetDefaultCellStyle();
  //! Update the bitmaps on ppi changes.
  void UpdateBitmaps();

  wxBitmap GetEvalAllBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetEvalBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetNewBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetOpenBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetSaveBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetPrintBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetPreferencesBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetCutBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetCopyBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetPasteBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetSelectAllBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetFindBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetRestartBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetInterruptBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetEvalTillHereBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetHelpBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetEvalRestBitmap(wxSize siz = wxSize(-1, -1));
  wxBitmap GetHideCodeBitmap(wxSize siz = wxSize(-1, -1));

  bool ShowCopyPaste(){bool show = true;wxConfig::Get()->Read("Toolbar/showCopyPaste",&show);
    return show;}
  void ShowCopyPaste(bool show){wxConfig::Get()->Write("Toolbar/showCopyPaste",show);}

  bool ShowOpenSave(){bool show = true;wxConfig::Get()->Read("Toolbar/showOpenSave",&show);
    return show;}
  void ShowOpenSave(bool show){wxConfig::Get()->Write("Toolbar/showOpenSave",show);}
  
  bool ShowNew(){bool show = true;wxConfig::Get()->Read("Toolbar/showNew",&show);
    return show;}
  void ShowNew(bool show){wxConfig::Get()->Write("Toolbar/showNew",show);}

  bool ShowSearch(){bool show = true;wxConfig::Get()->Read("Toolbar/showSearch",&show);
    return show;}
  void ShowSearch(bool show){wxConfig::Get()->Write("Toolbar/showSearch",show);}

  bool ShowHelp(){bool show = true;wxConfig::Get()->Read("Toolbar/showHelp",&show);
    return show;}
  void ShowHelp(bool show){wxConfig::Get()->Write("Toolbar/showHelp",show);}

  bool ShowPrint(){bool show = true;wxConfig::Get()->Read("Toolbar/showPrint",&show);
    return show;}
  void ShowPrint(bool show){wxConfig::Get()->Write("Toolbar/showPrint",show);}

  bool ShowOptions(){bool show = true;wxConfig::Get()->Read("Toolbar/showOptions",&show);
    return show;}
  void ShowOptions(bool show){wxConfig::Get()->Write("Toolbar/showOptions",show);}

  bool ShowSelectAll(){bool show = true;wxConfig::Get()->Read("Toolbar/showSelectAll",&show);
    return show;}
  void ShowSelectAll(bool show){wxConfig::Get()->Write("Toolbar/showSelectAll",show);}

protected:
  void OnSize(wxSizeEvent &event);
  void OnMouseRightDown(wxMouseEvent &event);
  void OnMenu(wxMenuEvent &event);
private:
  //! The ppi rate.
  wxSize m_ppi;
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
  bool m_canEvalThisCell_old;
  std::unique_ptr<struct NSVGrasterizer, decltype(std::free)*> m_svgRast{nullptr, std::free};
  bool m_worksheetEmpty_old;
  AnimationStartStopState m_AnimationStartStopState;
  //! True if we show the "needs information" button.
  bool m_needsInformation;
};

#endif
