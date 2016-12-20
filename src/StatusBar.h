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

/*! \file

  This file contains the definition of the class History that handles the recently 
  issued commands for the history pane.
 */
#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/timer.h>
#include <wx/statbmp.h>
#include <wx/statusbr.h>

#ifndef STATUSBAR_H
#define STATUSBAR_H


class StatusBar : public wxStatusBar
{
public:
  StatusBar(wxWindow *parent, int id);
  
  enum networkState
  {
    idle,
    error,
    offline,
    receive,
    transmit
  };
  
  void NetworkStatus(networkState status);
  
protected:
  void OnSize(wxSizeEvent& event);
  void OnTimerEvent(wxTimerEvent& event);

private:
  bool m_icon_shows_receive;
  bool m_icon_shows_transmit;
#if defined __WXGTK__
  wxBitmap GetImage(wxString img);
#else
  wxImage GetImage(wxString img);
#endif
  //! The currently shown network status bitmap
  wxStaticBitmap *m_networkStatus;
  wxBitmap m_network_error;
  wxBitmap m_network_offline;
  wxBitmap m_network_transmit;
  wxBitmap m_network_idle;
  wxBitmap m_network_receive;
  wxBitmap m_network_transmit_receive;
  wxTimer SendTimer;
  wxTimer ReceiveTimer;
  wxDECLARE_EVENT_TABLE();
};
#endif

