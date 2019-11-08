// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ToolBar that represents wxMaxima's main tool bar.
 */

#include "ToolBar.h"
#include "../art/toolbar/images.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include "GroupCell.h"
#include <wx/display.h>
#include <wx/artprov.h>
#include <wx/filename.h>
#include "invalidImage.h"

#if wxCHECK_VERSION(3, 1, 0)
#define TOOLBAR_ICON_SCALE (0.25)
#else
#define TOOLBAR_ICON_SCALE (0.35)
#endif

#define ABS(val) ((val) >= 0 ? (val) : -(val))

wxBitmap ToolBar::GetImage(wxString name,
                          unsigned char *data_128, size_t len_128,
                          unsigned char *data_192, size_t len_192)
{
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    m_ppi = wxSize(72,72);
  else
    m_ppi = wxDisplay(display_idx).GetPPI();
#else
  m_ppi = wxGetDisplayPPI();
#endif
  if((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxGetDisplayPPI();

  if((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxSize(72,72);

  double targetSize = wxMax(m_ppi.x,75) * TOOLBAR_ICON_SCALE * GetContentScaleFactor();
  int prescale;

  int sizeA = 128 << 4;
  while(sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while(sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if(ABS(targetSize - sizeA) < ABS(targetSize - sizeB)) {
    targetSize = sizeA;
    prescale = 128;
  } else {
    targetSize = sizeB;
    prescale = 192;
  }

  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_TOOLBAR, wxSize(targetSize, targetSize));
  wxImage img;

  if(bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }
  if(!img.IsOk()) {
    void *data;
    size_t len;
    if(prescale == 128)
    {
      data = (void *)data_128;
      len  = len_128;
    }
    else
    {
      data = (void *)data_192;
      len  = len_192;
    }
    wxMemoryInputStream istream(data,len);
    img.LoadFile(istream);
  }
  if(!img.IsOk()) {
    img = wxImage(invalidImage_xpm);
  }

  img.Rescale(targetSize, targetSize, wxIMAGE_QUALITY_HIGH);
#if defined __WXOSX__
  return wxBitmap(img,wxBITMAP_SCREEN_DEPTH,GetContentScaleFactor());
#else
  return wxBitmap(img,wxBITMAP_SCREEN_DEPTH);
#endif
}

ToolBar::~ToolBar()
{
  m_plotSlider = NULL;
}

void ToolBar::UpdateSlider(SlideShow *cell)
{
  if(cell == NULL)
    return;
  int slideShowDisplayedIndex = cell->GetDisplayedIndex();
  int slideShowMaxIndex = cell->Length();

  if ((m_slideShowDisplayedIndex != slideShowDisplayedIndex) || (m_slideShowMaxIndex != slideShowMaxIndex))
  {
    m_slideShowMaxIndex = slideShowMaxIndex;
    m_slideShowDisplayedIndex = slideShowDisplayedIndex;
    if (m_plotSlider != NULL)
    {
      m_plotSlider->SetRange(0, cell->Length() - 1);
      m_plotSlider->SetValue(cell->GetDisplayedIndex());
      m_plotSlider->SetToolTip(wxString::Format(_("Frame %i of %i"), cell->GetDisplayedIndex() + 1, cell->Length()));
    }
  }
}

ToolBar::ToolBar(wxWindow *parent) : wxAuiToolBar(parent,-1, wxDefaultPosition, wxDefaultSize,
                                                  wxAUI_TB_OVERFLOW | wxAUI_TB_PLAIN_BACKGROUND |
                                                  wxAUI_TB_HORIZONTAL)
{
  m_defaultCellStyle = GC_TYPE_CODE;
  m_canEvalThisCell_old = true;
  m_worksheetEmpty_old = false;
  m_canCopy_old = true;
  m_canCut_old = true;
  m_canSave_old = true;
  m_canPrint_old = true;
  m_canEvalTillHere_old = true;

  m_needsInformation = false;
  m_AnimationStartStopState = Inactive;

  SetToolBitmapSize(wxSize(24, 24));
  SetGripperVisible(false);
  m_plotSlider = NULL;
  m_textStyle = NULL;
  AddTools();

  // For some reason overflow and re-sizing don't play together in some cases if we don't
  // do the following:
  Realize();
  SetInitialSize(wxSize(100000,GetBestSize().y));
  Realize();  
}

void ToolBar::AddTools()
{
  Clear();
  m_ppi = wxSize(-1,-1);  
  if(ShowNew())
    AddTool(tb_new, _("New"),
            GetImage(wxT("gtk-new"),
                     gtk_new_128_png,gtk_new_128_png_len,
                     gtk_new_192_png,gtk_new_192_png_len
              ),
            _("New document"));
  if(ShowOpenSave())
  {
    AddTool(tb_open, _("Open"),
            GetImage(wxT("gtk-open"),
                     gtk_open_128_png,gtk_open_128_png_len,
                     gtk_open_192_png,gtk_open_192_png_len
              ),
            _("Open document"));
    AddTool(tb_save, _("Save"),
            GetImage(wxT("gtk-save"),
                     gtk_save_128_png,gtk_save_128_png_len,
                     gtk_save_192_png,gtk_save_192_png_len
              ),
            _("Save document"));
  }
  if(ShowPrint())
  {
#ifndef __WXOSX__
    AddSeparator();
#endif
    AddTool(tb_print, _("Print"),
            GetImage(wxT("gtk-print"),
                     gtk_print_128_png,gtk_print_128_png_len,
                     gtk_print_192_png,gtk_print_192_png_len
              ),
            _("Print document"));
  }
  if(ShowOptions())
    AddTool(tb_pref, _("Options"),
            GetImage(wxT("gtk-preferences"),
                     gtk_preferences_128_png,gtk_preferences_128_png_len,
                     gtk_preferences_192_png,gtk_preferences_192_png_len
              ),
            _("Configure wxMaxima"));
  if(ShowCopyPaste())
  {
#ifndef __WXOSX__
    AddSeparator();
#endif
    AddTool(tb_cut, _("Cut"),
            GetImage(wxT("gtk-cut"),
                     gtk_cut_128_png,gtk_cut_128_png_len,
                     gtk_cut_192_png,gtk_cut_192_png_len
              ),
            _("Cut selection"));
    AddTool(tb_copy, _("Copy"),
            GetImage(wxT("gtk-copy"),
                     gtk_copy_128_png,gtk_copy_128_png_len,
                     gtk_copy_192_png,gtk_copy_192_png_len
              ),
            _("Copy selection"));
    AddTool(tb_paste, _("Paste"),
            GetImage(wxT("gtk-paste"),
                     gtk_paste_128_png,gtk_paste_128_png_len,
                     gtk_paste_192_png,gtk_paste_192_png_len
              ),
            _("Paste from clipboard"));
  }
  AddTool(tb_select_all, _("Select all"),
          GetImage(wxT("gtk-select-all"),
                   gtk_select_all_128_png,gtk_select_all_128_png_len,
                   gtk_select_all_192_png,gtk_select_all_192_png_len
            ),
          _("Select all"));
  if(ShowSearch())
  {
#ifndef __WXOSX__
    AddSeparator();
#endif
    AddTool(tb_find, _("Find"),
            GetImage(wxT("gtk-find"),
                     gtk_find_128_png,gtk_find_128_png_len,
                     gtk_find_192_png,gtk_find_192_png_len
              ),
            _("Find and replace"));
  }
#ifndef __WXOSX__
  AddSeparator();
#endif
  AddTool(menu_restart_id, _("Restart maxima"),
          GetImage(wxT("view-refresh"),
                   view_refresh_128_png,view_refresh_128_png_len,
                   view_refresh_192_png,view_refresh_192_png_len
            ),
          _("Completely stop maxima and restart it"));
  AddTool(tb_interrupt, _("Interrupt"),
          GetImage(wxT("gtk-stop"),
                   gtk_stop_128_png,gtk_stop_128_png_len,
                   gtk_stop_192_png,gtk_stop_192_png_len
            ),
          _("Interrupt current computation. To completely restart maxima press the button left to this one."));
  m_followIcon = GetImage(wxT("weather-clear"),
                          weather_clear_128_png,weather_clear_128_png_len,
                          weather_clear_192_png,weather_clear_192_png_len
    );
  m_needsInformationIcon = GetImage(wxT("software-update-urgent"),
                                    software_update_urgent_128_png,software_update_urgent_128_png_len,
                                    software_update_urgent_192_png,software_update_urgent_192_png_len
    );
  AddTool(tb_follow, _("Follow"), m_followIcon,
          _("Return to the cell that is currently being evaluated"));
  EnableTool(tb_follow, false);

  AddTool(tb_eval, _("Evaluate current cell"),
          GetImage(wxT("go-next"),
                   go_next_128_png,go_next_128_png_len,
                   go_next_192_png,go_next_192_png_len
            ),
          _("Send the current cell to maxima"));

  AddTool(tb_eval_all, _("Evaluate all"),
          GetImage(wxT("go-jump"),
                   go_jump_128_png,go_jump_128_png_len,
                   go_jump_192_png,go_jump_192_png_len
            ),
          _("Send the current cell to maxima"));

  AddTool(tb_evaltillhere, _("Evaluate to point"),
          GetImage(wxT("go-bottom"),
                   go_bottom_128_png,go_bottom_128_png_len,
                   go_bottom_192_png,go_bottom_192_png_len
            ),
          _("Evaluate the file from its beginning to the cell above the cursor"));
  
  AddTool(tb_evaluate_rest, _("Evaluate the rest"),
          GetImage(wxT("go-last"),
                   go_last_128_png,go_last_128_png_len,
                   go_last_192_png,go_last_192_png_len
            ),
          _("Evaluate the file from the cursor to its end"));
  
#ifndef __WXOSX__
  AddSeparator();
#endif
  AddTool(tb_hideCode, _("Hide Code"),
          GetImage(wxT("weather-few-clouds"),
                   weather_few_clouds_128_png,weather_few_clouds_128_png_len,
                   weather_few_clouds_192_png,weather_few_clouds_192_png_len
            ),
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
  textStyle.Add(_("Heading5"));
  textStyle.Add(_("Heading6"));
  int textStyleSelection = 0;
  if(m_textStyle)
    textStyleSelection = m_textStyle->GetSelection();
  wxDELETE(m_textStyle);
  m_textStyle = new wxChoice(this, tb_changeStyle, wxDefaultPosition, wxDefaultSize, textStyle);
  m_textStyle->SetToolTip(_("For faster creation of cells the following shortcuts exist:\n\n"
                            "   Ctrl+0: Math cell\n"
                            "   Ctrl+1: Text cell\n"
                            "   Ctrl+2: Title cell\n"
                            "   Ctrl+3: Section cell\n"
                            "   Ctrl+4: Subsection cell\n"
                            "   Ctrl+5: Sub-Subsection cell\n"
                            "   Ctrl+6: Heading5 cell\n"
                            "   Ctrl+7: Heading6 cell\n"
                            ));
  m_textStyle->SetSelection(textStyleSelection);
  AddControl(m_textStyle);
  
  // Seems like on MSW changing the image of this button has strange side-effects
  // so we combine both images into one for this OS.
  m_PlayButton = GetImage(wxT("media-playback-start"),
                          media_playback_start_128_png,media_playback_start_128_png_len,
                          media_playback_start_192_png,media_playback_start_192_png_len
    );
  m_StopButton = GetImage(wxT("media-playback-stop"),
                          media_playback_stop_128_png,media_playback_stop_128_png_len,
                          media_playback_stop_192_png,media_playback_stop_192_png_len
    );

  // It felt like a good idea to combine the play and the stop button.
  AddTool(tb_animation_startStop, _("Start or Stop animation"),
          m_PlayButton,
          _("Start or stop the currently selected animation that has been created with the with_slider class of commands"));
  EnableTool(tb_animation_startStop, false);

#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    m_ppi = wxSize(72,72);
  else
    m_ppi = wxDisplay(display_idx).GetPPI();
#else
  m_ppi = wxGetDisplayPPI();
#endif
  if((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxGetDisplayPPI();
  if((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxSize(72,72);

  int sliderWidth = wxMax(m_ppi.x,75) * 200 / 72;
  int width, height;
  wxDisplaySize(&width, &height);
  if (width < 800)
    sliderWidth = wxMin(sliderWidth, 100);
  wxDELETE(m_plotSlider);
  m_plotSlider = new wxSlider(this, plot_slider_id, 0, 0, 10,
                              wxDefaultPosition, wxSize(sliderWidth, -1),
                              wxSL_HORIZONTAL | !wxSL_AUTOTICKS);
  m_plotSlider->SetToolTip(
    _("After clicking on animations created with with_slider_draw() or similar this slider allows to change the current frame."));
  m_plotSlider->Enable(false);
  m_slideShowMaxIndex = -1;
  m_slideShowDisplayedIndex = -1;
  AddControl(m_plotSlider);
  AddStretchSpacer(100);
  if(ShowHelp())
    AddTool(tb_help, _("Help"),
            GetImage(wxT("gtk-help"),
                     gtk_help_128_png,gtk_help_128_png_len,
                     gtk_help_192_png,gtk_help_192_png_len
              ),
            _("Show Maxima help"));
  Connect(wxEVT_SIZE,
          wxSizeEventHandler(ToolBar::OnSize),
          NULL, this);
  Connect(wxEVT_RIGHT_DOWN,
          wxMouseEventHandler(ToolBar::OnMouseRightDown),
          NULL, this);
  if(!GetToolBarFits())
    DeleteTool(tb_animation_startStop);

  Realize();
}

void ToolBar::UpdateBitmaps()
{
  wxSize ppi(-1,-1);
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72,72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#endif
  
  if((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxGetDisplayPPI();

  if((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72,72);

  if((ppi.x == m_ppi.x) && (ppi.y == m_ppi.y))
    return;

  wxLogMessage(
    wxString::Format(
      _("Display resolution according to wxWidgets: %i x %i ppi"),
      ppi.x,
      ppi.y)
    );

  if(ppi.x == 0)
    return;

  m_ppi = ppi;
  
  SetToolBitmap(tb_new,GetImage(wxT("gtk-new"),
                                gtk_new_128_png,gtk_new_128_png_len,
                                gtk_new_192_png,gtk_new_192_png_len));
  SetToolBitmap(tb_open,GetImage(wxT("gtk-open"),
                                 gtk_open_128_png,gtk_open_128_png_len,
                                 gtk_open_192_png,gtk_open_192_png_len));
  SetToolBitmap(tb_save,GetImage(wxT("gtk-save"),
                                 gtk_save_128_png,gtk_save_128_png_len,
                                 gtk_save_192_png,gtk_save_192_png_len
                  ));
  SetToolBitmap(tb_print,GetImage(wxT("gtk-print"),
                                  gtk_print_128_png,gtk_print_128_png_len,
                                  gtk_print_192_png,gtk_print_192_png_len
                  ));
  SetToolBitmap(tb_pref,GetImage(wxT("gtk-preferences"),
                                 gtk_preferences_128_png,gtk_preferences_128_png_len,
                                 gtk_preferences_192_png,gtk_preferences_192_png_len
                  ));
  SetToolBitmap(tb_cut,GetImage(wxT("gtk-cut"),
                                gtk_cut_128_png,gtk_cut_128_png_len,
                                gtk_cut_192_png,gtk_cut_192_png_len
                  ));
  SetToolBitmap(tb_copy,GetImage(wxT("gtk-copy"),
                                 gtk_copy_128_png,gtk_copy_128_png_len,
                                 gtk_copy_192_png,gtk_copy_192_png_len
                  ));
  SetToolBitmap(tb_paste,GetImage(wxT("gtk-paste"),
                                  gtk_paste_128_png,gtk_paste_128_png_len,
                                  gtk_paste_192_png,gtk_paste_192_png_len
                  ));
  SetToolBitmap(tb_select_all,
                GetImage(wxT("gtk-select-all"),
                         gtk_select_all_128_png,gtk_select_all_128_png_len,
                         gtk_select_all_192_png,gtk_select_all_192_png_len
                  ));
  SetToolBitmap(tb_find,GetImage(wxT("gtk-find"),
                                 gtk_find_128_png,gtk_find_128_png_len,
                                 gtk_find_192_png,gtk_find_192_png_len
                  ));
  SetToolBitmap(menu_restart_id,GetImage(wxT("view-refresh"),
                                         view_refresh_128_png,view_refresh_128_png_len,
                                         view_refresh_192_png,view_refresh_192_png_len
                  ));
  SetToolBitmap(tb_interrupt,
                GetImage(wxT("gtk-stop"),
                         gtk_stop_128_png,gtk_stop_128_png_len,
                         gtk_stop_192_png,gtk_stop_192_png_len
                  ));
  m_followIcon = GetImage(wxT("weather-clear"),
                          weather_clear_128_png,weather_clear_128_png_len,
                          weather_clear_192_png,weather_clear_192_png_len
    );
  m_needsInformationIcon = GetImage(wxT("software-update-urgent"),
                                    software_update_urgent_128_png,software_update_urgent_128_png_len,
                                    software_update_urgent_192_png,software_update_urgent_192_png_len
    );
  SetToolBitmap(tb_follow, m_followIcon);
  SetToolBitmap(tb_evaltillhere,GetImage(wxT("go-bottom"),
                                         go_bottom_128_png,go_bottom_128_png_len,
                                         go_bottom_192_png,go_bottom_192_png_len
                  ));
  SetToolBitmap(tb_evaluate_rest, 
                GetImage(wxT("go-last"),
                         go_last_128_png,go_last_128_png_len,
                         go_last_192_png,go_last_192_png_len
                  ));
  SetToolBitmap(tb_hideCode,GetImage(wxT("weather-few-clouds"),
                                     weather_few_clouds_128_png,weather_few_clouds_128_png_len,
                                     weather_few_clouds_192_png,weather_few_clouds_192_png_len
                  ));
  m_PlayButton = GetImage(wxT("media-playback-start"),
                          media_playback_start_128_png,media_playback_start_128_png_len,
                          media_playback_start_192_png,media_playback_start_192_png_len
    );
  m_StopButton = GetImage(wxT("media-playback-stop"),
                          media_playback_stop_128_png,media_playback_stop_128_png_len,
                          media_playback_stop_192_png,media_playback_stop_192_png_len
    );
  SetToolBitmap(tb_animation_startStop,
                m_PlayButton);
  SetToolBitmap(tb_help,GetImage(wxT("gtk-help"),
                                 gtk_help_128_png,gtk_help_128_png_len,
                                 gtk_help_192_png,gtk_help_192_png_len
                  ));
  Realize();
}

void ToolBar::SetDefaultCellStyle()
{
  switch(m_textStyle->GetSelection())
  {
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
  default:
  {}
  }
}

GroupType ToolBar::GetCellType()
{
  switch(m_textStyle->GetSelection())
  {
  case 1:
    return GC_TYPE_TEXT;
    break;
  case 2:
    return GC_TYPE_TITLE;
    break;
  case 3:
    return GC_TYPE_SECTION;
    break;    
  case 4:
    return GC_TYPE_SUBSECTION;
    break;
  case 5:
    return GC_TYPE_SUBSUBSECTION;
    break;
  case 6:
    return GC_TYPE_HEADING5;
    break;
  case 7:
    return GC_TYPE_HEADING6;
    break;
  case 8:
    return GC_TYPE_IMAGE;
    break;
  case 9:
    return GC_TYPE_PAGEBREAK;
    break;
  default:
    return GC_TYPE_CODE;
  }
}

void ToolBar::SetCellStyle(int style)
{

  switch(style)
  {
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

  switch(style)
  {
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
  }
}

void ToolBar::AnimationButtonState(AnimationStartStopState state)
{
  if (m_AnimationStartStopState != state)
  {
    switch (state)
    {
      case Running:
        m_plotSlider->Enable(true);
        if (m_AnimationStartStopState != Running)
        {
          SetToolBitmap(tb_animation_startStop, m_StopButton);
        }
        EnableTool(tb_animation_startStop, true);
        break;
      case Stopped:
        if (m_AnimationStartStopState == Running)
        {
          SetToolBitmap(tb_animation_startStop, m_PlayButton);
        }
        EnableTool(tb_animation_startStop, true);
        m_plotSlider->Enable(true);
        break;
      case Inactive:
        EnableTool(tb_animation_startStop, false);
        m_plotSlider->Enable(false);
        m_plotSlider->SetToolTip(
                _("After clicking on animations created with with_slider_draw() or similar this slider allows to change the current frame."));
        m_slideShowMaxIndex = -1;
        m_slideShowDisplayedIndex = -1;

        if (m_AnimationStartStopState == Running)
        {
          SetToolBitmap(tb_animation_startStop, m_PlayButton);
        }
        break;
    }
    m_AnimationStartStopState = state;
  }
  //  Realize() flickers on GTK3
 Refresh();
}

void ToolBar::OnSize(wxSizeEvent &event)
{
//  AddTools();
  event.Skip();
}

void ToolBar::OnMouseRightDown(wxMouseEvent &event)
{
  wxMenu *popupMenu = new wxMenu();
  popupMenu->AppendCheckItem(copy_paste, _("Copy, Cut and Paste button"),
                             _("Show the Copy, Cut and the Paste button?"));
  popupMenu->Check(copy_paste, ShowCopyPaste());
  popupMenu->AppendCheckItem(open_save, _("Open and save button"),
                             _("Show the open and the save button?"));
  popupMenu->Check(open_save, ShowOpenSave());
  popupMenu->AppendCheckItem(print, _("Print button"),
                             _("Show the print button?"));
  popupMenu->Check(print, ShowPrint());
  popupMenu->AppendCheckItem(options, _("Preferences button"),
                             _("Show the preferences button?"));
  popupMenu->Check(options, ShowOptions());
  popupMenu->AppendCheckItem(shownew, _("New button"),
                             _("Show the \"New\" button?"));
  popupMenu->Check(shownew, ShowNew());
  popupMenu->AppendCheckItem(search, _("Search button"),
                             _("Show the \"search\" button?"));
  popupMenu->Check(search, ShowSearch());
  popupMenu->AppendCheckItem(help, _("Help button"),
                             _("Show the \"help\" button?"));
  popupMenu->Check(help, ShowHelp());

  if (popupMenu->GetMenuItemCount() > 0)
  {
    popupMenu->Connect(wxEVT_MENU,
                       wxMenuEventHandler(ToolBar::OnMenu),
                       NULL, this);
    PopupMenu(popupMenu);
  }
  wxDELETE(popupMenu);
}

void ToolBar::OnMenu(wxMenuEvent &event)
{
  switch(event.GetId())
  {
  case copy_paste:
    ShowCopyPaste(!ShowCopyPaste());
    AddTools();
    break;
  case open_save:
    ShowOpenSave(!ShowOpenSave());
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
  }
}
