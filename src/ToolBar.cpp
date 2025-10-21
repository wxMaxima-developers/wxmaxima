// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ToolBar that represents wxMaxima's main tool bar.
*/

#include "ToolBar.h"
#include "Image.h"
#include "SvgBitmap.h"
#include "ArtProvider.h"
#include "art/toolbar/arrow-up-square.h"
#include "art/toolbar/dialog-information.h"
#include "art/toolbar/gtk-preferences.h"
#include "art/toolbar/eye-slash.h"
#include "art/toolbar/go-bottom.h"
#include "art/toolbar/gtk-select-all.h"
#include "art/toolbar/go-jump.h"
#include "art/toolbar/gtk-stop.h"
#include "art/toolbar/go-last.h"
#include "art/toolbar/go-next.h"
#include "art/toolbar/input.h"
#include "art/toolbar/media-playback-stop.h"
#include "art/toolbar/software-update-urgent.h"
#include "art/toolbar/text.h"
#include "art/toolbar/view-refresh1.h"
#include "art/media-playback-start.h"
#include "art/media-playback-reverse.h"
#include <cstdlib>
#include <wx/artprov.h>
#include <wx/display.h>
#include <wx/filename.h>
#include <wx/mstream.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <algorithm>

#if wxCHECK_VERSION(3, 1, 0)
#define TOOLBAR_ICON_SCALE (0.25)
#else
#define TOOLBAR_ICON_SCALE (0.35)
#endif

wxSize ToolBar::GetOptimalBitmapSize()
{
  wxSize siz;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    m_ppi = wxSize(72, 72);
  else
    m_ppi = wxDisplay(display_idx).GetPPI();
#else
  m_ppi = wxGetDisplayPPI();
#endif
  if ((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxSize(72, 72);

#if defined __WXOSX__
  int targetSize =
    std::max(m_ppi.x, 75) * TOOLBAR_ICON_SCALE * GetContentScaleFactor();
#else
  int targetSize = std::max(m_ppi.x, 75) * TOOLBAR_ICON_SCALE;
#endif
  int sizeA = 128 << 4;
  while (sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while (sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if (std::abs(targetSize - sizeA) < std::abs(targetSize - sizeB))
    targetSize = sizeA;
  else
    targetSize = sizeB;
  siz = wxSize(targetSize, targetSize);
  return siz;
}

ToolBar::~ToolBar() { m_plotSlider = NULL; }

void ToolBar::UpdateSlider(AnimationCell *cell) {
  if (cell == NULL)
    return;
  std::size_t animationDisplayedIndex = cell->GetDisplayedIndex();
  std::size_t animationMaxIndex = cell->Length();

  if ((m_animationDisplayedIndex != animationDisplayedIndex) ||
      (m_animationMaxIndex != animationMaxIndex)) {
    m_animationMaxIndex = animationMaxIndex;
    m_animationDisplayedIndex = animationDisplayedIndex;
    if (m_plotSlider != NULL) {
      m_plotSlider->SetRange(0, cell->Length() - 1);
      m_plotSlider->SetValue(cell->GetDisplayedIndex());
      m_plotSlider->SetToolTip(wxString::Format(
                                                _("Frame %li of %li"),
                                                static_cast<long>(cell->GetDisplayedIndex()) + 1,
                                                static_cast<long>(cell->Length())));
    }
  }
}

ToolBar::ToolBar(wxWindow *parent)
  : wxAuiToolBar(parent, -1, wxDefaultPosition, wxDefaultSize,
                 wxAUI_TB_OVERFLOW | wxAUI_TB_PLAIN_BACKGROUND |
                 wxAUI_TB_HORIZONTAL),
    m_defaultCellStyle(GC_TYPE_CODE),
    m_canCopy_old(true),
    m_canCut_old(true),
    m_canSave_old(true),
    m_canPrint_old(true),
    m_canEvalTillHere_old(true),
    m_canEvalThisCell_old(true),
    m_worksheetEmpty_old(false)
{
  m_svgRast.reset(wxm_nsvgCreateRasterizer());
  SetGripperVisible(false);
  SetToolBitmapSize(GetOptimalBitmapSize());
  AddTools();

  // For some reason overflow and re-sizing don't play together in some cases if
  // we don't do the following:
  Realize();
  SetInitialSize(wxSize(100000, GetBestSize().y));
  Realize();
}

void ToolBar::AddTools() {
  wxSize bitmapSize = GetOptimalBitmapSize();
  Clear();
  m_ppi = wxSize(-1, -1);
  if (ShowNew())
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_NEW, _("New"), wxArtProvider::GetBitmapBundle(wxART_NEW, wxART_TOOLBAR), _("New document"));
#else
    AddTool(wxID_NEW, _("New"), wxArtProvider::GetBitmap(wxART_NEW, wxART_TOOLBAR), _("New document"));
#endif
  if (ShowOpenSave()) {
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_OPEN, _("Open"), wxArtProvider::GetBitmapBundle(wxART_FILE_OPEN, wxART_TOOLBAR), _("Open document"));
    AddTool(wxID_SAVE, _("Save"), wxArtProvider::GetBitmapBundle(wxART_FILE_SAVE, wxART_TOOLBAR), _("Save document"));
#else
    AddTool(wxID_OPEN, _("Open"), wxArtProvider::GetBitmap(wxART_FILE_OPEN, wxART_TOOLBAR), _("Open document"));
    AddTool(wxID_SAVE, _("Save"), wxArtProvider::GetBitmap(wxART_FILE_SAVE, wxART_TOOLBAR), _("Save document"));
#endif
  }
  if (ShowPrint()) {
#ifndef __WXOSX__
    if (ShowOpenSave() || ShowNew())
      AddSeparator();
#endif
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_PRINT, _("Print"), wxArtProvider::GetBitmapBundle(wxART_PRINT, wxART_TOOLBAR), _("Print document"));
#else
    AddTool(wxID_PRINT, _("Print"), wxArtProvider::GetBitmap(wxART_PRINT, wxART_TOOLBAR), _("Print document"));
#endif
  }
  if (ShowUndoRedo()) {
#ifndef __WXOSX__
    if (ShowOpenSave() || ShowNew())
      AddSeparator();
#endif
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_UNDO, _("Undo"), wxArtProvider::GetBitmapBundle(wxART_UNDO, wxART_TOOLBAR));
    AddTool(wxID_REDO, _("Redo"), wxArtProvider::GetBitmapBundle(wxART_REDO, wxART_TOOLBAR));
#else
    AddTool(wxID_UNDO, _("Undo"), wxArtProvider::GetBitmap(wxART_UNDO, wxART_TOOLBAR));
    AddTool(wxID_REDO, _("Redo"), wxArtProvider::GetBitmap(wxART_REDO, wxART_TOOLBAR));
#endif
  }
  if (ShowOptions()) {
#ifndef __WXOSX__
    if (ShowOpenSave() || ShowNew() || ShowUndoRedo())
      AddSeparator();
#endif
    AddTool(wxID_PREFERENCES, _("Options"), GetPreferencesBitmap(bitmapSize), _("Configure wxMaxima"));
  }
  if (ShowCopyPaste()) {
#ifndef __WXOSX__
    if (ShowSelectAll() || ShowOpenSave() || ShowNew() || ShowPrint() ||
        ShowUndoRedo())
      AddSeparator();
#endif
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_CUT, _("Cut"), wxArtProvider::GetBitmapBundle(wxART_CUT, wxART_TOOLBAR), _("Cut selection"));
    AddTool(wxID_COPY, _("Copy"), wxArtProvider::GetBitmapBundle(wxART_COPY, wxART_TOOLBAR), _("Copy selection"));
    AddTool(wxID_PASTE, _("Paste"), wxArtProvider::GetBitmapBundle(wxART_PASTE, wxART_TOOLBAR), _("Paste from clipboard"));
#else
    AddTool(wxID_CUT, _("Cut"), wxArtProvider::GetBitmap(wxART_CUT, wxART_TOOLBAR), _("Cut selection"));
    AddTool(wxID_COPY, _("Copy"), wxArtProvider::GetBitmap(wxART_COPY, wxART_TOOLBAR), _("Copy selection"));
    AddTool(wxID_PASTE, _("Paste"), wxArtProvider::GetBitmap(wxART_PASTE, wxART_TOOLBAR), _("Paste from clipboard"));
#endif
  }
  if (ShowSelectAll())
    AddTool(wxID_SELECTALL, _("Select all"), GetSelectAllBitmap(bitmapSize), _("Select all"));  // Maybe GetSelectAllBitmapBundle would be good?

  if (ShowSearch()) {
#ifndef __WXOSX__
    if (ShowSelectAll() || ShowOpenSave() || ShowNew() || ShowPrint() ||
        ShowUndoRedo() || ShowCopyPaste())
      AddSeparator();
#endif
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_FIND, _("Find"), wxArtProvider::GetBitmapBundle(wxART_FIND_AND_REPLACE, wxART_TOOLBAR), _("Find and replace"));
#else
    AddTool(wxID_FIND, _("Find"), wxArtProvider::GetBitmap(wxART_FIND_AND_REPLACE, wxART_TOOLBAR), _("Find and replace"));
#endif
  }
#ifndef __WXOSX__
  if (ShowSelectAll() || ShowOpenSave() || ShowNew() || ShowPrint() ||
      ShowOptions() || ShowUndoRedo() || ShowSearch())
    AddSeparator();
#endif
  AddTool(menu_restart_id, _("Restart Maxima"), GetRestartBitmap(bitmapSize),
          _("Completely stop maxima and restart it"));
  AddTool(tb_interrupt, _("Interrupt"), GetInterruptBitmap(bitmapSize),
          _("Interrupt current computation. To completely restart maxima press "
            "the button left to this one."));
  int bitmapWidth = GetOptimalBitmapSize().x;
  m_followIcon = ArtProvider::GetImage(this, wxS("arrow_up_square"), bitmapWidth, ARROW_UP_SQUARE_SVG_GZ,
                                       ARROW_UP_SQUARE_SVG_GZ_SIZE);
  m_needsInformationIcon =
    ArtProvider::GetImage(this, wxS("software-update-urgent"), bitmapWidth, SOFTWARE_UPDATE_URGENT_SVG_GZ,
                          SOFTWARE_UPDATE_URGENT_SVG_GZ_SIZE);
  AddTool(tb_follow, _("Follow"), m_followIcon,
          _("Return to the cell that is currently being evaluated"));
  EnableTool(tb_follow, false);

#ifndef __WXOSX__
  AddSeparator();
#endif

  AddTool(tb_eval, _("Evaluate current cell"), GetEvalBitmap(bitmapSize),
          _("Send the current cell to maxima"));

  AddTool(tb_eval_all, _("Evaluate all"), GetEvalAllBitmap(bitmapSize),
          _("Send all cells to maxima"));

  AddTool(tb_evaltillhere, _("Evaluate to point"), GetEvalTillHereBitmap(bitmapSize),
          _("Evaluate the file from its beginning to the cell above the cursor"));

  AddTool(tb_evaluate_rest, _("Evaluate the rest"), GetEvalRestBitmap(bitmapSize),
          _("Evaluate the file from the cursor to its end"));

#ifndef __WXOSX__
  AddSeparator();
#endif
  AddTool(tb_hideCode, _("Hide Code"), GetHideCodeBitmap(bitmapSize),
          _("Toggle the visibility of code cells"));
#ifndef __WXOSX__
  AddSeparator();
#endif
  wxArrayString textStyle;
  textStyle.Add(_("Maths"));
  textStyle.Add(_("Text"));
  textStyle.Add(_("Title"));
  textStyle.Add(_("Section"));
  textStyle.Add(_("Subsection"));
  textStyle.Add(_("Subsubsection"));
  textStyle.Add(_("Heading 5"));
  textStyle.Add(_("Heading 6"));
  int textStyleSelection = 0;
  if (m_textStyle)
    textStyleSelection = m_textStyle->GetSelection();
  wxDELETE(m_textStyle);
  m_textStyle = new wxChoice(this, tb_changeStyle, wxDefaultPosition,
                             wxDefaultSize, textStyle);
  m_textStyle->SetToolTip(
                          _("For faster creation of cells the following shortcuts exist:\n\n"
                            "   Ctrl+0: Math cell\n"
                            "   Ctrl+1: Text cell\n"
                            "   Ctrl+2: Title cell\n"
                            "   Ctrl+3: Section cell\n"
                            "   Ctrl+4: Subsection cell\n"
                            "   Ctrl+5: Sub-Subsection cell\n"
                            "   Ctrl+6: Heading5 cell\n"
                            "   Ctrl+7: Heading6 cell\n"));
  m_textStyle->SetSelection(textStyleSelection);
  AddControl(m_textStyle);

  m_PlayButton =
    ArtProvider::GetImage(this, wxS("media-playback-start"), bitmapWidth, MEDIA_PLAYBACK_START_SVG_GZ,
                          MEDIA_PLAYBACK_START_SVG_GZ_SIZE);
  m_StopButton =
    ArtProvider::GetImage(this, wxS("media-playback-stop"), bitmapWidth, MEDIA_PLAYBACK_STOP_SVG_GZ,
                          MEDIA_PLAYBACK_STOP_SVG_GZ_SIZE);

  // It felt like a good idea to combine the play and the stop button.
  AddTool(tb_animation_startStop, _("Start or Stop animation"), m_PlayButton,
          _("Start or stop the currently selected animation that has been "
            "created with the with_slider class of commands"));
  EnableTool(tb_animation_startStop, false);

  m_ppi = GetPPI();

  int sliderWidth = std::max(m_ppi.x, 75) * 200 / 72;
  int width, height;
  wxDisplaySize(&width, &height);
  if (width < 800)
    sliderWidth = std::min(sliderWidth, 100);
  wxDELETE(m_plotSlider);
  m_plotSlider = new wxSlider(this, plot_slider_id, 0, 0, 10, wxDefaultPosition,
                              wxSize(sliderWidth, -1), wxSL_HORIZONTAL);
  m_plotSlider->SetToolTip(
                           _("After clicking on animations created with with_slider_draw() or "
                             "similar this slider allows to change the current frame."));
  m_plotSlider->Enable(false);
  m_animationMaxIndex = 0;
  AddControl(m_plotSlider);
  AddStretchSpacer(100);
  if (ShowHelp())
#if wxCHECK_VERSION(3, 1, 6)
    AddTool(wxID_HELP, _("Help"), wxArtProvider::GetBitmapBundle(wxART_HELP, wxART_TOOLBAR), _("Show wxMaxima help"));
#else
    AddTool(wxID_HELP, _("Help"), wxArtProvider::GetBitmap(wxART_HELP, wxART_TOOLBAR), _("Show wxMaxima help"));
#endif
  Connect(wxEVT_SIZE, wxSizeEventHandler(ToolBar::OnSize), NULL, this);
  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(ToolBar::OnMouseRightDown),
          NULL, this);
  Realize();
}

wxSize ToolBar::GetPPI()
{
  wxSize ppi(-1, -1);
#if wxCHECK_VERSION(3, 1, 1)
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72, 72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#else
  m_ppi = wxGetDisplayPPI();
#endif
  if ((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72, 72);
  return ppi;
}

void ToolBar::UpdateBitmaps() {
  wxSize bitmapSize = GetOptimalBitmapSize();
  SetToolBitmapSize(bitmapSize);
  int bitmapWidth = bitmapSize.x;

  wxSize ppi = GetPPI();
  if ((ppi.x == m_ppi.x) && (ppi.y == m_ppi.y))
    return;
  wxLogMessage(_("Display resolution according to wxWidgets: %li x %li ppi"),
               static_cast<long>(ppi.x),
               static_cast<long>(ppi.y));

  m_ppi = ppi;

  SetToolBitmap(tb_eval, GetEvalBitmap(bitmapSize));
  SetToolBitmap(tb_eval_all, GetEvalAllBitmap(bitmapSize));
  SetToolBitmap(wxID_PREFERENCES, GetPreferencesBitmap(bitmapSize));
  SetToolBitmap(wxID_SELECTALL, GetSelectAllBitmap(bitmapSize));
  SetToolBitmap(menu_restart_id, GetRestartBitmap(bitmapSize));
  SetToolBitmap(tb_interrupt, GetInterruptBitmap(bitmapSize));
  m_followIcon = ArtProvider::GetImage(this, wxS("arrow_up_square"), bitmapWidth, ARROW_UP_SQUARE_SVG_GZ,
                                       ARROW_UP_SQUARE_SVG_GZ_SIZE);
  m_needsInformationIcon =
    ArtProvider::GetImage(this, wxS("software-update-urgent"), bitmapWidth, SOFTWARE_UPDATE_URGENT_SVG_GZ,
                          SOFTWARE_UPDATE_URGENT_SVG_GZ_SIZE);
  SetToolBitmap(tb_follow, m_followIcon);
  SetToolBitmap(tb_evaltillhere, GetEvalTillHereBitmap(bitmapSize));
  SetToolBitmap(tb_evaluate_rest, GetEvalRestBitmap(bitmapSize));
  SetToolBitmap(tb_hideCode, GetHideCodeBitmap(bitmapSize));
  m_PlayButton =
    ArtProvider::GetImage(this, wxS("media-playback-start"), bitmapWidth, MEDIA_PLAYBACK_START_SVG_GZ,
                          MEDIA_PLAYBACK_START_SVG_GZ_SIZE);
  m_StopButton =
    ArtProvider::GetImage(this, wxS("media-playback-stop"), bitmapWidth, MEDIA_PLAYBACK_STOP_SVG_GZ,
                          MEDIA_PLAYBACK_STOP_SVG_GZ_SIZE);
  SetToolBitmap(tb_animation_startStop, m_PlayButton);
  Realize();
}

wxBitmap ToolBar::GetEvalAllBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("go-next"), siz.x, GO_JUMP_SVG_GZ, GO_JUMP_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetEvalBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("go-next"), siz.x, GO_NEXT_SVG_GZ, GO_NEXT_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetPreferencesBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("gtk-preferences"), siz.x, GTK_PREFERENCES_SVG_GZ,
                               GTK_PREFERENCES_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetSelectAllBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("gtk-select-all"), siz.x, GTK_SELECT_ALL_SVG_GZ,
                               GTK_SELECT_ALL_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetRestartBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("view-refresh"), siz.x, VIEW_REFRESH1_SVG_GZ,
                               VIEW_REFRESH1_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetInterruptBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("gtk-stop"), siz.x, GTK_STOP_SVG_GZ, GTK_STOP_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetEvalTillHereBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("go-bottom"), siz.x, GO_BOTTOM_SVG_GZ, GO_BOTTOM_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetEvalRestBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("go-last"), siz.x, GO_LAST_SVG_GZ, GO_LAST_SVG_GZ_SIZE);
}
wxBitmap ToolBar::GetHideCodeBitmap(wxSize siz) {
  return ArtProvider::GetImage(this, wxS("eye-slash"), siz.x, EYE_SLASH_SVG_GZ, EYE_SLASH_SVG_GZ_SIZE);
}

void ToolBar::SetDefaultCellStyle() {
  switch (m_textStyle->GetSelection()) {
  case 0:
    m_defaultCellStyle = GC_TYPE_CODE;
    break;
  case 1:
    m_defaultCellStyle = GC_TYPE_TEXT;
    break;
  case 2:
    m_defaultCellStyle = GC_TYPE_TITLE;
    break;
  case 3:
    m_defaultCellStyle = GC_TYPE_SECTION;
    break;
  case 4:
    m_defaultCellStyle = GC_TYPE_SUBSECTION;
    break;
  case 5:
    m_defaultCellStyle = GC_TYPE_SUBSUBSECTION;
    break;
  case 6:
    m_defaultCellStyle = GC_TYPE_HEADING5;
    break;
  case 7:
    m_defaultCellStyle = GC_TYPE_HEADING6;
    break;
  default: {
  }
  }
}

GroupType ToolBar::GetCellType() {
  switch (m_textStyle->GetSelection()) {
  case 1:
    return GC_TYPE_TEXT;
  case 2:
    return GC_TYPE_TITLE;
  case 3:
    return GC_TYPE_SECTION;
  case 4:
    return GC_TYPE_SUBSECTION;
  case 5:
    return GC_TYPE_SUBSUBSECTION;
  case 6:
    return GC_TYPE_HEADING5;
  case 7:
    return GC_TYPE_HEADING6;
  case 8:
    return GC_TYPE_IMAGE;
  case 9:
    return GC_TYPE_PAGEBREAK;
  default:
    return GC_TYPE_CODE;
  }
}

void ToolBar::SetCellStyle(GroupType style) {
  switch (style) {
  case GC_TYPE_CODE:
  case GC_TYPE_TEXT:
  case GC_TYPE_TITLE:
  case GC_TYPE_SECTION:
  case GC_TYPE_SUBSECTION:
  case GC_TYPE_SUBSUBSECTION:
  case GC_TYPE_HEADING5:
  case GC_TYPE_HEADING6:
    break;
  default:
    style = m_defaultCellStyle;
  }

  switch (style) {
  case GC_TYPE_CODE:
    m_textStyle->SetSelection(0);
    break;
  case GC_TYPE_TEXT:
    m_textStyle->SetSelection(1);
    break;
  case GC_TYPE_TITLE:
    m_textStyle->SetSelection(2);
    break;
  case GC_TYPE_SECTION:
    m_textStyle->SetSelection(3);
    break;
  case GC_TYPE_SUBSECTION:
    m_textStyle->SetSelection(4);
    break;
  case GC_TYPE_SUBSUBSECTION:
    m_textStyle->SetSelection(5);
    break;
  case GC_TYPE_HEADING5:
    m_textStyle->SetSelection(6);
    break;
  case GC_TYPE_HEADING6:
    m_textStyle->SetSelection(7);
    break;
  default:
    break;
  }
}

void ToolBar::AnimationButtonState(AnimationStartStopState state) {
  if (m_AnimationStartStopState != state) {
    switch (state) {
    case Running:
      m_plotSlider->Enable(true);
      if (m_AnimationStartStopState != Running) {
        SetToolBitmap(tb_animation_startStop, m_StopButton);
      }
      EnableTool(tb_animation_startStop, true);
      break;
    case Stopped:
      if (m_AnimationStartStopState == Running) {
        SetToolBitmap(tb_animation_startStop, m_PlayButton);
      }
      EnableTool(tb_animation_startStop, true);
      m_plotSlider->Enable(true);
      break;
    case Inactive:
      EnableTool(tb_animation_startStop, false);
      m_plotSlider->Enable(false);
      m_plotSlider->SetToolTip(
                               _("After clicking on animations created with with_slider_draw() or "
                                 "similar this slider allows to change the current frame."));
      m_animationMaxIndex = 0;
      m_animationDisplayedIndex = 0;

      if (m_AnimationStartStopState == Running) {
        SetToolBitmap(tb_animation_startStop, m_PlayButton);
      }
      break;
    }
    m_AnimationStartStopState = state;
    Realize();
  }
}

void ToolBar::OnSize(wxSizeEvent &event) {
  //  AddTools();
  event.Skip();
}

void ToolBar::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  wxMenu *popupMenu = new wxMenu();
  popupMenu->AppendCheckItem(shownew, _("New button"),
                             _("Show the \"New\" button?"));
  popupMenu->Check(shownew, ShowNew());
  popupMenu->AppendCheckItem(undo_redo, _("Undo and redo button"),
                             _("Show the undo and redo button?"));
  popupMenu->Check(undo_redo, ShowUndoRedo());
  popupMenu->AppendCheckItem(open_save, _("Open and save button"),
                             _("Show the open and the save button?"));
  popupMenu->Check(open_save, ShowOpenSave());
  popupMenu->AppendCheckItem(print, _("Print button"),
                             _("Show the print button?"));
  popupMenu->Check(print, ShowPrint());
  popupMenu->AppendCheckItem(copy_paste, _("Copy, Cut and Paste button"),
                             _("Show the Copy, Cut and the Paste button?"));
  popupMenu->Check(copy_paste, ShowCopyPaste());
  popupMenu->AppendCheckItem(options, _("Preferences button"),
                             _("Show the preferences button?"));
  popupMenu->Check(options, ShowOptions());
  popupMenu->AppendCheckItem(selectAll, _("Select All button"),
                             _("Show the \"select all\" button?"));
  popupMenu->Check(selectAll, ShowSelectAll());
  popupMenu->AppendCheckItem(search, _("Search button"),
                             _("Show the \"search\" button?"));
  popupMenu->Check(search, ShowSearch());
  popupMenu->AppendCheckItem(help, _("Help button"),
                             _("Show the \"help\" button?"));
  popupMenu->Check(help, ShowHelp());

  if (popupMenu->GetMenuItemCount() > 0) {
    popupMenu->Connect(wxEVT_MENU, wxMenuEventHandler(ToolBar::OnMenu), NULL,
                       this);
    PopupMenu(popupMenu);
  }
  wxDELETE(popupMenu);
}

void ToolBar::OnMenu(wxMenuEvent &event) {
  switch (event.GetId()) {
  case copy_paste:
    ShowCopyPaste(!ShowCopyPaste());
    AddTools();
    break;
  case open_save:
    ShowOpenSave(!ShowOpenSave());
    AddTools();
    break;
  case undo_redo:
    ShowUndoRedo(!ShowUndoRedo());
    AddTools();
    break;
  case print:
    ShowPrint(!ShowPrint());
    AddTools();
    break;
  case options:
    ShowOptions(!ShowOptions());
    AddTools();
    break;
  case shownew:
    ShowNew(!ShowNew());
    AddTools();
    break;
  case search:
    ShowSearch(!ShowSearch());
    AddTools();
    break;
  case help:
    ShowHelp(!ShowHelp());
    AddTools();
    break;
  case selectAll:
    ShowSelectAll(!ShowSelectAll());
    AddTools();
    break;
  }
  Realize();
}
