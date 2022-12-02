// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the contents of the class StatusBar that represents
  wxMaxima's status bar.

*/
#include "StatusBar.h"
#include "Image.h"
#include "SvgBitmap.h"
#include "wxm_statusbar_images.h"
#include <wx/artprov.h>
#include <wx/display.h>
#include <wx/mstream.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>

StatusBar::StatusBar(wxWindow *parent, int id)
  : wxStatusBar(parent, id), m_ppi(wxSize(-1, -1)) {
  m_svgRast.reset(nsvgCreateRasterizer());
  int widths[] = {-1, GetSize().GetHeight(), GetSize().GetHeight()};
  m_maximaPercentage = -1;
  m_oldmaximaPercentage = -1;
  SetFieldsCount(3, widths);
  int styles[] = {wxSB_NORMAL, wxSB_NORMAL, wxSB_FLAT};
  SetStatusStyles(3, styles);
  m_stdToolTip =
    _("Maxima, the program that does the actual mathematics is started as a "
      "separate process. This has the advantage that an eventual crash of "
      "maxima cannot harm wxMaxima, which displays the worksheet.\nThis icon "
      "indicates if data is transferred between maxima and wxMaxima.");
  m_networkErrToolTip =
    _("Maxima, the program that does the actual mathematics and wxMaxima, "
      "which displays the worksheet are kept in separate processes. This "
      "means that even if maxima crashes wxMaxima (and therefore the "
      "worksheet) stays intact. Both programs communicate over a local "
      "network socket. This time this socket could not be created which "
      "might be caused by a firewall that it setup to not only intercepts "
      "connections from the outside, but also to intercept connections "
      "between two programs that run on the same computer.");
  m_noConnectionToolTip =
    _("Maxima, the program that does the actual mathematics and wxMaxima, "
      "which displays the worksheet are kept in separate processes. This "
      "means that even if maxima crashes wxMaxima (and therefore the "
      "worksheet) stays intact. Currently the two programs aren't connected "
      "to each other which might mean that maxima is still starting up or "
      "couldn't be started. Alternatively it can be caused by a firewall "
      "that it setup to not only intercepts connections from the outside, "
      "but also to intercept connections between two programs that run on "
      "the same computer. Another reason for maxima not starting up might be "
      "that maxima cannot be found (see wxMaxima's Configuration dialogue "
      "for a way to specify maxima's location) or isn't in a working order.");
  UpdateBitmaps();
  m_statusTextPanel = new wxPanel(this, wxID_ANY);
  m_statusText = new wxStaticText(m_statusTextPanel, wxID_ANY, wxEmptyString);
  m_statusText->Connect(
			wxEVT_LEFT_DCLICK, wxCommandEventHandler(StatusBar::StatusMsgDClick), NULL,
			this);

  m_maximaStatus = new wxStaticBitmap(this, wxID_ANY, m_network_offline);
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

StatusBar::~StatusBar() {}

void StatusBar::StatusMsgDClick(wxCommandEvent &ev)
{
  wxCommandEvent *evt = new wxCommandEvent(wxEVT_LEFT_DCLICK);
  m_statusTextPanel->GetEventHandler()->QueueEvent(evt);
  ev.Skip();
}

void StatusBar::UpdateBitmaps() {
  wxSize ppi(-1, -1);
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;

  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72, 72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#endif

  if ((ppi.x < 10) || (ppi.y < 10))
    ppi = wxGetDisplayPPI();
  if ((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72, 72);

  if ((ppi.x == m_ppi.x) && (ppi.y == m_ppi.y))
    return;

  if (m_ppi != ppi) {
    m_ppi = ppi;
    m_network_error = GetImage("network-error", NETWORK_ERROR_SVG_GZ,
                               NETWORK_ERROR_SVG_GZ_SIZE);
    m_network_offline = GetImage("network-offline", NETWORK_OFFLINE_SVG_GZ,
                                 NETWORK_OFFLINE_SVG_GZ_SIZE);
    m_network_transmit = GetImage("network-transmit", NETWORK_TRANSMIT_SVG_GZ,
                                  NETWORK_TRANSMIT_SVG_GZ_SIZE);
    m_network_idle =
      GetImage("network-idle", NETWORK_IDLE_SVG_GZ, NETWORK_IDLE_SVG_GZ_SIZE);
    m_network_idle_inactive =
      wxBitmap(m_network_idle.ConvertToImage().ConvertToDisabled());
    m_network_receive = GetImage("network-receive", NETWORK_RECEIVE_SVG_GZ,
                                 NETWORK_RECEIVE_SVG_GZ_SIZE);
    m_network_transmit_receive =
      GetImage("network-transmit-receive", NETWORK_TRANSMIT_RECEIVE_SVG_GZ,
	       NETWORK_TRANSMIT_RECEIVE_SVG_GZ_SIZE);

    
    m_bitmap_waitForStart =
      GetImage("image-loading", WAITING_SVG_GZ,
	       WAITING_SVG_GZ_SIZE);
    m_bitmap_process_wont_start =
      GetImage("network-error", NETWORK_ERROR_SVG_GZ,
	       NETWORK_ERROR_SVG_GZ_SIZE);
    m_bitmap_sending =
      GetImage("go-next", GO_NEXT_SVG_GZ,
	       GO_NEXT_SVG_GZ_SIZE);
    m_bitmap_waiting =
      GetImage("dialog-accept", DIALOG_ACCEPT_SVG_GZ,
	       DIALOG_ACCEPT_SVG_GZ_SIZE);
    m_bitmap_waitingForPrompt =
      GetImage("image-loading", IMAGE_LOADING_SVG_GZ,
	       IMAGE_LOADING_SVG_GZ_SIZE);
    m_bitmap_waitingForAuth = 
      GetImage("lock", SYSTEM_LOCK_SCREEN_SVG_GZ,
	       SYSTEM_LOCK_SCREEN_SVG_GZ_SIZE);
    m_bitmap_calculating = 
      GetImage("calc", EMBLEM_EQUAL_DEFINED_SVG_GZ,
	       EMBLEM_EQUAL_DEFINED_SVG_GZ_SIZE);
    m_bitmap_parsing = 
      GetImage("go-up", GO_UP_SVG_GZ,
	       GO_UP_SVG_GZ_SIZE);
    m_bitmap_transferring =
      GetImage("go-previous", GO_PREVIOUS_SVG_GZ,
	       GO_PREVIOUS_SVG_GZ_SIZE);
    m_bitmap_userinput =
      GetImage("important", EMBLEM_IMPORTANT_SVG_GZ,
	       EMBLEM_IMPORTANT_SVG_GZ_SIZE);
    m_bitmap_disconnected =
      GetImage("network-offline", NETWORK_OFFLINE_SVG_GZ,
	       NETWORK_OFFLINE_SVG_GZ_SIZE);
  }
}

void StatusBar::UpdateStatusMaximaBusy(MaximaStatus status, long bytesFromMaxima)
{
  switch(status)
    {
    case wait_for_start:
      m_maximaStatus->SetBitmap(m_bitmap_waitForStart);
      m_maximaStatus->SetToolTip(_("Maxima started. Waiting for connection..."));
      break;
    case process_wont_start:
      m_maximaStatus->SetBitmap(m_bitmap_process_wont_start);
      m_maximaStatus->SetToolTip(_("Cannot start the maxima binary"));
      break;
    case sending:
      m_maximaStatus->SetBitmap(m_bitmap_sending);
      m_maximaStatus->SetToolTip(_("Sending a command to Maxima"));
      break;
    case waiting:
      m_maximaStatus->SetBitmap(m_bitmap_waiting);
      m_maximaStatus->SetToolTip(_("Ready for user input"));
      break;
    case waitingForPrompt:
      m_maximaStatus->SetBitmap(m_bitmap_waitingForPrompt);
      m_maximaStatus->SetToolTip(_("Maxima started. Waiting for initial prompt..."));
      break;
    case waitingForAuth:
      m_maximaStatus->SetBitmap(m_bitmap_waitingForAuth);
      m_maximaStatus->SetToolTip(_("Maxima started. Waiting for authentication..."));
      break;
    case calculating:
      m_maximaStatus->SetBitmap(m_bitmap_calculating);
      m_maximaStatus->SetToolTip(_("Maxima is calculating"));
      break;
    case parsing:
      m_maximaStatus->SetBitmap(m_bitmap_parsing);
      m_maximaStatus->SetToolTip(_("Parsing output"));
      break;
    case transferring:
      m_maximaStatus->SetBitmap(m_bitmap_transferring);
      if (bytesFromMaxima == 0)
	m_maximaStatus->SetToolTip(_("Reading Maxima output"));
      else
	m_maximaStatus->SetToolTip(wxString::Format(_("Reading Maxima output: %li bytes"),
					      bytesFromMaxima));
      break;
    case userinput:
      m_maximaStatus->SetBitmap(m_bitmap_userinput);
      m_maximaStatus->SetToolTip(_("Maxima asks a question"));
      break;
    case disconnected:
      m_maximaStatus->SetBitmap(m_bitmap_disconnected);
      m_maximaStatus->SetToolTip(_("Not connected to Maxima"));
      break;
    default:
      wxASSERT(false);
    }
}

void StatusBar::HandleTimerEvent() {
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

  if (m_icon_shows_receive && m_icon_shows_transmit) {
    m_networkStatus->SetBitmap(m_network_transmit_receive);
    if (m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (m_icon_shows_receive && !m_icon_shows_transmit) {
    m_networkStatus->SetBitmap(m_network_receive);
    m_oldNetworkState = receive;
    if (m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (!m_icon_shows_receive && m_icon_shows_transmit) {
    m_networkStatus->SetBitmap(m_network_transmit);
    m_oldNetworkState = transmit;
    if (m_maximaPercentage == 0)
      m_maximaPercentage = -1;
  }
  if (!m_icon_shows_receive && !m_icon_shows_transmit) {
    m_networkStatus->SetBitmap(m_network_idle);
    if (m_maximaPercentage != 0)
      m_networkStatus->SetBitmap(m_network_idle);
    else
      m_networkStatus->SetBitmap(m_network_idle_inactive);
    m_oldNetworkState = idle;
  }
}

void StatusBar::OnTimerEvent(wxTimerEvent &WXUNUSED(event)) {
  HandleTimerEvent();
}

void StatusBar::NetworkStatus(networkState status) {
  UpdateBitmaps();
  if ((status != m_oldNetworkState) ||
      (m_maximaPercentage != m_oldmaximaPercentage)) {
    switch (status) {
    case idle: {
      //      if(status != m_oldNetworkState)
      //        m_maximaPercentage = m_oldmaximaPercentage = -1;
      if (m_maximaPercentage != 0) {
        if ((!!m_maximaPercentage) != (!!m_oldmaximaPercentage))
          m_networkStatus->SetBitmap(m_network_idle);
      } else {
        if ((!!m_maximaPercentage) != (!!m_oldmaximaPercentage))
          m_networkStatus->SetBitmap(m_network_idle_inactive);
      }

      m_networkState = status;
      wxString toolTip = m_stdToolTip;
      if (m_maximaPercentage >= 0)
        toolTip += wxString::Format(
				    _("\n\nMaxima is currently using %3.3f%% of all available CPUs."),
				    m_maximaPercentage);
      m_networkStatus->SetToolTip(toolTip);
    } break;
    case error:
      if ((status != m_oldNetworkState)) {
        m_networkStatus->SetBitmap(m_network_error);
        m_networkState = status;
        m_networkStatus->SetToolTip(m_networkErrToolTip);
      }
      break;
    case offline:
      if ((status != m_oldNetworkState)) {
        m_networkStatus->SetBitmap(m_network_offline);
        m_networkState = status;
      }
      m_networkStatus->SetToolTip(m_noConnectionToolTip);
      break;
    case receive: {
      ReceiveTimer.StartOnce(200);
      HandleTimerEvent();
      m_networkStatus->SetToolTip(m_stdToolTip);
    } break;
    case transmit: {
      SendTimer.StartOnce(200);
      HandleTimerEvent();
      if ((m_oldmaximaPercentage >= 0) && (m_maximaPercentage < 0))
        m_networkStatus->SetToolTip(m_stdToolTip);
    } break;
    }
    m_oldNetworkState = status;
    m_oldmaximaPercentage = m_maximaPercentage;
  }
}

void StatusBar::OnSize(wxSizeEvent &event) {
  wxRect rect;

  GetFieldRect(2, rect);
  wxSize size = m_networkStatus->GetSize();

  m_networkStatus->Move(rect.x + (rect.width - size.x) / 2,
                        rect.y + (rect.height - size.y) / 2);
  GetFieldRect(1, rect);
  m_maximaStatus->Move(rect.x + (rect.width - size.x) / 2,
		       rect.y + (rect.height - size.y) / 2);
  GetFieldRect(0, rect);
  wxSize borders = GetBorders();
  size = wxSize(rect.GetWidth(), rect.GetHeight());
  m_statusTextPanel->Move(borders.x + rect.x + 1,
			  borders.y + rect.y);
  m_statusTextPanel->SetSize(size);
  
  event.Skip();
}

wxBitmap StatusBar::GetImage(wxString name, unsigned char *data, size_t len) {
  wxSize ppi;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;

  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72, 72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
#else
  ppi = wxGetDisplayPPI();

  if (ppi.x < 72)
    ppi.x = 72;
  if (ppi.y < 72)
    ppi.y = 72;
#endif
  int targetWidth = static_cast<double>(GetSize().GetHeight()) / ppi.y * ppi.x *
    GetContentScaleFactor();
  int targetHeight =
    static_cast<double>(GetSize().GetHeight()) * GetContentScaleFactor();

  if (targetWidth < 16)
    targetWidth = 16;
  if (targetHeight < 16)
    targetHeight = 16;

  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_TOOLBAR,
                                          wxSize(targetWidth, targetHeight));
  wxImage img;

  if (bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }

  int sizeA = 128 << 4;
  while (sizeA * 3 / 2 > targetWidth && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while (sizeB * 4 / 3 > targetWidth && sizeB >= 32) {
    sizeB >>= 1;
  }

  if (!img.IsOk())
    return SvgBitmap(this, data, len, targetWidth, targetWidth);

  targetWidth = static_cast<double>(GetSize().GetHeight());
  targetHeight = static_cast<double>(GetSize().GetHeight());

  img.Rescale(targetWidth, targetHeight, wxIMAGE_QUALITY_HIGH);

#if defined __WXOSX__
  return wxBitmap(img, wxBITMAP_SCREEN_DEPTH, GetContentScaleFactor());
#else
  return wxBitmap(img, wxBITMAP_SCREEN_DEPTH);
#endif
}
