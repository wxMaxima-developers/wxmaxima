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
#include "Dirstructure.h"

#ifndef STATUSBAR_H
#define STATUSBAR_H

/*! The class that draws the status bar

  \todo: Rescale the icons if OnSize returns a new vertical size: This enables
  for screen resolution change awareness.
 */
class StatusBar : public wxStatusBar
{
public:
  StatusBar(wxWindow *parent, int id);

  //! The network states that can be passed to NetworkStatus()
  enum networkState
  {
    idle,
    error,
    offline,
    receive,
    transmit
  };

  //! Informs the status bar about networking events.
  void NetworkStatus(networkState status);

  wxStaticBitmap *GetNetworkStatusElement()
  { return m_networkStatus; }

  //! Inform the status bar how many percents of the available CPU power maxima uses
  void SetMaximaCPUPercentage(float percentage)
    {
      m_maximaPercentage = percentage;
      NetworkStatus(m_oldNetworkState);
    }
protected:
  void OnSize(wxSizeEvent &event);

  void OnTimerEvent(wxTimerEvent &event);

private:
  /*! How many percents of the available CPU power does maxima use?

    See m_maximaPercentage and SetMaximaCPUPercentage()
   */
  float m_maximaPercentage;
  /*! How many percents of the available CPU power did maxima use when updating the network icon last?

    See m_maximaPercentage and SetMaximaCPUPercentage()
   */
  float m_oldmaximaPercentage;
  networkState m_oldNetworkState;
  wxString m_stdToolTip;
  wxString m_networkErrToolTip;
  wxString m_noConnectionToolTip;
  //! The basic network state we currently display without receive or transmit info
  networkState m_networkState;
  //! Does the icon show that we currently receive data?
  bool m_icon_shows_receive;
  //! Does the icon show that we currently transmit data?
  bool m_icon_shows_transmit;

  wxBitmap GetImage(wxString img,
                    unsigned char *data_128, size_t len_128,
                    unsigned char *data_192, size_t len_192
    );
  
  //! The currently shown network status bitmap
  wxStaticBitmap *m_networkStatus;
  //! The bitmap shown on network errors
  wxBitmap m_network_error;
  //! The bitmap shown while not connected to the network
  wxBitmap m_network_offline;
  //! The bitmap shown while transmitting data
  wxBitmap m_network_transmit;
  //! The bitmap shown while not transmitting or receiving data
  wxBitmap m_network_idle;
  //! The bitmap shown while not transmitting or receiving data and maxima not using CPU power
  wxBitmap m_network_idle_inactive;
  //! The bitmap shown while receiving data
  wxBitmap m_network_receive;
  //! The bitmap shown while simultaneously receiving and transmitting data
  wxBitmap m_network_transmit_receive;
  //! The timer that prolongs the showing of the "sending" bitmap a bit.
  wxTimer SendTimer;
  //! The timer that prolongs the showing of the "receiving" bitmap a bit.
  wxTimer ReceiveTimer;
wxDECLARE_EVENT_TABLE();
};

#endif

