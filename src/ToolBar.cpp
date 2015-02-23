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

ToolBar::ToolBar(wxToolBar* toolbar)
{
  m_toolBar = toolbar;
  
  toolbar->SetToolBitmapSize(wxSize(24, 24));

#if defined __WXMSW__
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
  m_toolBar->AddSeparator();
  m_toolBar->AddTool(tb_print, _("Print"),
                     GetImage(wxT("gtk-print")),
                     _("Print document"));
  m_toolBar->AddTool(tb_pref, _("Options"),
                     GetImage(wxT("gtk-preferences")),
                     _("Configure wxMaxima"));
  m_toolBar->AddSeparator();
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
  m_toolBar->AddSeparator();
  m_toolBar->AddTool(tb_find, _("Find"),
                     GetImage(wxT("gtk-find")),
                     _("Find and replace"));
  m_toolBar->AddSeparator();
  m_toolBar->AddTool(tb_interrupt, _("Interrupt"),
                     GetImage(wxT("gtk-stop")),
                     _("Interrupt current computation"));
  m_followIcon = GetImage(wxT("weather-clear"));
  m_needsInformationIcon = GetImage(wxT("software-update-urgent"));
  m_toolBar->AddTool(tb_follow, _("Follow"),m_followIcon,
                     _("Return to the cell that is currently being evaluated"));
  m_toolBar->AddSeparator();
  m_toolBar->AddTool(tb_animation_start, _("Start animation"),
                     GetImage(wxT("media-playback-start")),
                     _("Start animation"));
  m_toolBar->AddTool(tb_animation_stop, _("Stop animation"),
                     GetImage(wxT("media-playback-stop")),
                     _("Stop animation"));
  m_plotSlider = new wxSlider(m_toolBar, plot_slider_id, 0, 0, 10,
			      wxDefaultPosition, wxSize(200, -1),
			      wxSL_HORIZONTAL | !wxSL_AUTOTICKS);
  m_toolBar->AddControl(m_plotSlider);
  m_toolBar->AddSeparator();
  m_toolBar->AddTool(tb_help, _("Help"),
                     GetImage(wxT("gtk-help")),
                     _("Show Maxima help"));
  m_toolBar->Realize();
}
