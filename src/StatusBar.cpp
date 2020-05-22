// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file defines the contents of the class StatusBar that represents wxMaxima's status bar.

*/
#include "StatusBar.h"
#include <wx/artprov.h>
#include <wx/display.h>
#include "../art/statusbar/images.h"
#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include "Image.h"

StatusBar::StatusBar(wxWindow *parent, int id) : wxStatusBar(parent, id),
                                                 m_ppi(wxSize(-1,-1))
{
  m_svgRast.reset(nsvgCreateRasterizer());
  int widths[] = {-1, 300, GetSize().GetHeight()};
  m_maximaPercentage = -1;
  m_oldmaximaPercentage = -1;
  SetFieldsCount(3, widths);
  m_stdToolTip = _(
          "Maxima, the program that does the actual mathematics is started as a separate process. This has the advantage that an eventual crash of maxima cannot harm wxMaxima, which displays the worksheet.\nThis icon indicates if data is transferred between maxima and wxMaxima.");
  m_networkErrToolTip = _(
          "Maxima, the program that does the actual mathematics and wxMaxima, which displays the worksheet are kept in separate processes. This means that even if maxima crashes wxMaxima (and therefore the worksheet) stays intact. Both programs communicate over a local network socket. This time this socket could not be created which might be caused by a firewall that it setup to not only intercepts connections from the outside, but also to intercept connections between two programs that run on the same computer.");
  m_noConnectionToolTip = _(
          "Maxima, the program that does the actual mathematics and wxMaxima, which displays the worksheet are kept in separate processes. This means that even if maxima crashes wxMaxima (and therefore the worksheet) stays intact. Currently the two programs aren't connected to each other which might mean that maxima is still starting up or couldn't be started. Alternatively it can be caused by a firewall that it setup to not only intercepts connections from the outside, but also to intercept connections between two programs that run on the same computer. Another reason for maxima not starting up might be that maxima cannot be found (see wxMaxima's Configuration dialogue for a way to specify maxima's location) or isn't in a working order.");
  UpdateBitmaps();
  m_networkStatus = new wxStaticBitmap(this, wxID_ANY, m_network_offline);
  m_networkStatus->SetToolTip(m_stdToolTip);
  ReceiveTimer.SetOwner(this, wxID_ANY);
  SendTimer.SetOwner(this, wxID_ANY);
  m_icon_shows_receive = m_icon_shows_transmit = false;
  m_networkState = offline;
  // Mark the network state as "to be changed"
  m_oldNetworkState = receive;
  Connect(wxEVT_SIZE, wxSizeEventHandler(StatusBar::OnSize));
  Connect(wxEVT_TIMER, wxTimerEventHandler(StatusBar::OnTimerEvent));
}

StatusBar::~StatusBar()
{}

void StatusBar::UpdateBitmaps()
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

  if((ppi.x < 10) || (ppi.y < 10))
    ppi = wxGetDisplayPPI();
  if((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72,72);

  if((ppi.x == m_ppi.x) && (ppi.y == m_ppi.y))
    return;

  if(m_ppi != ppi)
  {
    m_ppi = ppi;
    m_network_error = GetImage("network-error",
                               network_error_svg_gz,network_error_svg_gz_len
      );
    m_network_offline = GetImage("network-offline",
                                 network_offline_svg_gz,network_offline_svg_gz_len
      );
    m_network_transmit = GetImage("network-transmit",
                                  network_transmit_svg_gz,network_transmit_svg_gz_len
      );
    m_network_idle = GetImage("network-idle",
                              network_idle_svg_gz,network_idle_svg_gz_len
      );
    m_network_idle_inactive = wxBitmap(m_network_idle.ConvertToImage().ConvertToDisabled());
    m_network_receive = GetImage("network-receive",
                                 network_receive_svg_gz,network_receive_svg_gz_len
      );
    m_network_transmit_receive = GetImage("network-transmit-receive",
                                          network_transmit_receive_svg_gz,network_transmit_receive_svg_gz_len
      );
  }
}

void StatusBar::HandleTimerEvent()
{
  // don't do anything if the network status didn't change.
  if ((m_icon_shows_receive == (ReceiveTimer.IsRunning())) &&
      (m_icon_shows_transmit == (SendTimer.IsRunning())))
    return;

  // don't do anything if the timer expired, but we aren't connected
  // to the network any more.
  if ((m_networkState == error) || (m_networkState == offline))
    return;

  m_icon_shows_receive = ReceiveTimer.IsRunning();
  m_icon_shows_transmit = SendTimer.IsRunning();

  if (m_icon_shows_receive && m_icon_shows_transmit)
  {
    m_networkStatus->SetBitmap(m_network_transmit_receive);
    if(m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (m_icon_shows_receive && !m_icon_shows_transmit)
  {
    m_networkStatus->SetBitmap(m_network_receive);
    m_oldNetworkState = receive;
    if(m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (!m_icon_shows_receive && m_icon_shows_transmit)
  {
    m_networkStatus->SetBitmap(m_network_transmit);
    m_oldNetworkState = transmit;
    if(m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (!m_icon_shows_receive && !m_icon_shows_transmit)
  {
    m_networkStatus->SetBitmap(m_network_idle);
      if(m_maximaPercentage != 0)
        m_networkStatus->SetBitmap(m_network_idle);
      else
        m_networkStatus->SetBitmap(m_network_idle_inactive);
    m_oldNetworkState = idle;
  }
}

void StatusBar::OnTimerEvent(wxTimerEvent &WXUNUSED(event))
{
  HandleTimerEvent();
}

void StatusBar::NetworkStatus(networkState status)
{
  UpdateBitmaps();
  if((status != m_oldNetworkState) || (m_maximaPercentage !=
                                       m_oldmaximaPercentage))
  {
    switch (status)
    {
    case idle:
    {
//      if(status != m_oldNetworkState)
//        m_maximaPercentage = m_oldmaximaPercentage = -1;
      if(m_maximaPercentage != 0)
        m_networkStatus->SetBitmap(m_network_idle);
      else
        m_networkStatus->SetBitmap(m_network_idle_inactive);
      
      m_networkState = status;
      wxString toolTip = m_stdToolTip;
      if(m_maximaPercentage >= 0)
        toolTip +=wxString::Format(
          _("\n\nMaxima is currently using %3.3f%% of all available CPUs."),
          m_maximaPercentage
          );
      m_networkStatus->SetToolTip(toolTip);
    }
    break;
    case error:
      m_networkStatus->SetBitmap(m_network_error);
      m_networkState = status;
      m_networkStatus->SetToolTip(m_networkErrToolTip);
      break;
    case offline:
      m_networkStatus->SetBitmap(m_network_offline);
      m_networkState = status;
      m_networkStatus->SetToolTip(m_noConnectionToolTip);
      break;
    case receive:
    {
      ReceiveTimer.StartOnce(200);
      HandleTimerEvent();
      if((m_oldmaximaPercentage >= 0) &&(m_maximaPercentage < 0))
        m_networkStatus->SetToolTip(m_stdToolTip);
    }
    break;
    case transmit:
    {
      SendTimer.StartOnce(200);
      HandleTimerEvent();
      if((m_oldmaximaPercentage >= 0) &&(m_maximaPercentage < 0))
        m_networkStatus->SetToolTip(m_stdToolTip);
    }
    break;
    }
    m_oldNetworkState = status;
    m_oldmaximaPercentage = m_maximaPercentage;
  }
}

void StatusBar::OnSize(wxSizeEvent &event)
{
  wxRect rect;

  GetFieldRect(2, rect);
  wxSize size = m_networkStatus->GetSize();

  m_networkStatus->Move(rect.x + (rect.width - size.x) / 2,
                        rect.y + (rect.height - size.y) / 2);

  event.Skip();
}

#define ABS(val) ((val) >= 0 ? (val) : -(val))

wxBitmap StatusBar::GetImage(wxString name,
                          unsigned char *data, size_t len)
{
  wxSize ppi;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72,72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#else
  ppi = wxGetDisplayPPI();

  if (ppi.x < 72)
    ppi.x = 72;
  if (ppi.y < 72)
    ppi.y = 72;
#endif
  int targetWidth = static_cast<double>(GetSize().GetHeight()) / ppi.y * ppi.x*GetContentScaleFactor();
  int targetHeight = static_cast<double>(GetSize().GetHeight())*GetContentScaleFactor();

  if(targetWidth < 16)
    targetWidth = 16;
  if(targetHeight < 16)
    targetHeight = 16;
  
  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_TOOLBAR, wxSize(targetWidth, targetHeight));
  wxImage img;

  if(bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }

  int sizeA = 128 << 4;
  while(sizeA * 3 / 2 > targetWidth && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while(sizeB * 4 / 3 > targetWidth && sizeB >= 32) {
    sizeB >>= 1;
  }

  if(!img.IsOk())
    return SvgBitmap(data, len, targetWidth, targetWidth);

  targetWidth = static_cast<double>(GetSize().GetHeight());
  targetHeight = static_cast<double>(GetSize().GetHeight());

  img.Rescale(targetWidth, targetHeight, wxIMAGE_QUALITY_HIGH);

#if defined __WXOSX__
  return wxBitmap(img,wxBITMAP_SCREEN_DEPTH,GetContentScaleFactor());
#else
  return wxBitmap(img,wxBITMAP_SCREEN_DEPTH);
#endif
}
