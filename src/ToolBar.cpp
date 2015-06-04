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

#include "ToolBar.h"
#include "Dirstructure.h"
#include <wx/artprov.h>
#include <wx/filename.h>

#if defined (__WXMSW__) || defined (__WXMAC__)
wxImage ToolBar::GetImage(wxString img)
{
  Dirstructure dirstructure;
  return wxImage(dirstructure.ConfigToolbarDir() + img + wxT(".png"));
}
#else
wxBitmap ToolBar::GetImage(wxString img)
{
#if defined (__WXMSW__) || defined (__WXMAC__)
  return wxImage(dirstructure.ConfigToolbarDir() + img + wxT(".png"));
#else
  return wxArtProvider::GetBitmap(img,wxART_TOOLBAR);
#endif
}
#endif

ToolBar::~ToolBar()
{
  m_plotSlider = NULL;
}

ToolBar::ToolBar(wxWindow* parent, int id)
{
  m_toolBar = new wxToolBar(parent,id);
  m_needsInformation = false;
  m_AnimationStartStopState=Inactive;
  
  m_toolBar->SetToolBitmapSize(wxSize(24, 24));

#if defined __WXMSW__
  // If there are packaging issues we want to have a detailed error message.
  Dirstructure dirstructure;
  wxFileName test(dirstructure.ConfigToolbarDir() + wxT("gtk-new.png"));  
  wxASSERT_MSG(test.IsFileReadable(),_(wxT("Expected the icon files to be found at"))+dirstructure.ConfigToolbarDir());
               
  m_toolBar->AddTool(tb_new, _("New"),
                     GetImage(wxT("gtk-new")),
                     _("New document"));
#endif
  m_toolBar->AddTool(tb_open, _("Open"),
                     GetImage(wxT("gtk-open")),
                     _("Open document"));
  m_toolBar->AddTool(tb_save, _("Save"),
                     GetImage(wxT("gtk-save")),
                     _("Save document"));
#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif
  m_toolBar->AddTool(tb_print, _("Print"),
                     GetImage(wxT("gtk-print")),
                     _("Print document"));
  m_toolBar->AddTool(tb_pref, _("Options"),
                     GetImage(wxT("gtk-preferences")),
                     _("Configure wxMaxima"));
#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif
  m_toolBar->AddTool(tb_cut, _("Cut"),
                     GetImage(wxT("gtk-cut")),
                     _("Cut selection"));
  m_toolBar->AddTool(tb_copy, _("Copy"),
                     GetImage(wxT("gtk-copy")),
                     _("Copy selection"));
  m_toolBar->AddTool(tb_paste, _("Paste"),
                     GetImage(wxT("gtk-paste")),
                     _("Paste from clipboard"));
  m_toolBar->AddTool(tb_select_all, _("Select all"),
                     GetImage(wxT("gtk-select-all")),
                     _("Select all"));
#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif
  m_toolBar->AddTool(tb_find, _("Find"),
                     GetImage(wxT("gtk-find")),
                     _("Find and replace"));
#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif
  m_toolBar->AddTool(menu_restart_id, _("Restart maxima"),
                     GetImage(wxT("view-refresh")),
                     _("Completely stop maxima and restart it"));
  m_toolBar->AddTool(tb_interrupt, _("Interrupt"),
                     GetImage(wxT("gtk-stop")),
                     _("Interrupt current computation. To completely restart maxima press the button left to this one."));
  m_followIcon = GetImage(wxT("weather-clear"));
  m_needsInformationIcon = GetImage(wxT("software-update-urgent"));
  m_toolBar->AddTool(tb_follow, _("Follow"),m_followIcon,
                     _("Return to the cell that is currently being evaluated"));
  m_toolBar->EnableTool(tb_follow,false);

#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif

  // Seems like on MSW changing the image of this button has strange side-effects
  // so we combine both images into one for this OS.
  #if defined __WXMSW__
  m_PlayButton = GetImage(wxT("media-playback-startstop"));
  #else
  m_PlayButton = GetImage(wxT("media-playback-start"));
  #endif
  m_StopButton = GetImage(wxT("media-playback-stop"));

  // It felt like a good idea to combine the play and the stop button.
  // On windows changing a button seems to somehow stop the animation, though, so
  // this OS requires the buttons to be separate.
  m_toolBar->AddTool(tb_animation_startStop, _("Start or Stop animation"),
                     m_PlayButton,
                     _("Start or stop the currently selected animation that has been created with the with_slider class of commands"));
  m_toolBar->EnableTool(tb_animation_startStop,false);
  m_plotSlider = new wxSlider(m_toolBar, plot_slider_id, 0, 0, 10,
			      wxDefaultPosition, wxSize(200, -1),
			      wxSL_HORIZONTAL | !wxSL_AUTOTICKS);
  m_toolBar->AddControl(m_plotSlider);
#ifndef __WXMAC__
  m_toolBar->AddSeparator();
#endif
  m_toolBar->AddTool(tb_help, _("Help"),
                     GetImage(wxT("gtk-help")),
                     _("Show Maxima help"));
  m_toolBar->Realize();
}

void ToolBar::AnimationButtonState(AnimationStartStopState state)
{
  switch(state)
  {
  case Running:
    m_plotSlider->Enable(true);
    if(m_AnimationStartStopState!=Running)
    {
      #ifndef __WXMSW__
      m_toolBar->SetToolNormalBitmap(tb_animation_startStop,m_StopButton);
      #endif
    }
    break;
    m_toolBar->EnableTool(tb_animation_startStop,true);
    m_plotSlider->Enable(true);
  case Stopped:
    if(m_AnimationStartStopState==Running)
    {
      #ifndef __WXMSW__
      m_toolBar->SetToolNormalBitmap(tb_animation_startStop,m_PlayButton);      
      #endif
    }
    m_toolBar->EnableTool(tb_animation_startStop,true);
    m_plotSlider->Enable(true);
    break;
  case Inactive:
    m_toolBar->EnableTool(tb_animation_startStop,false);
    m_plotSlider->Enable(false);
    if(m_AnimationStartStopState==Running)
    {
      #ifndef __WXMSW__
      m_toolBar->SetToolNormalBitmap(tb_animation_startStop,m_PlayButton);
      #endif
    }
    break;
  }
  m_AnimationStartStopState = state;
}
