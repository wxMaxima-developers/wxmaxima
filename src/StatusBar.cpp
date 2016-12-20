// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file defines the contents of the class StatusBar that represents wxMaxima's status bar.

*/
#include "StatusBar.h"
#include <wx/artprov.h>

StatusBar::StatusBar(wxWindow *parent, int id): wxStatusBar(parent, id)
{
  int widths[] = { -1, 300, GetSize().GetHeight()};
  SetFieldsCount(3, widths);
  m_network_error = GetImage("network-error");
  m_network_offline = GetImage("network-offline");
  m_network_transmit = GetImage("network-transmit");
  m_network_idle = GetImage("network-idle");
  m_network_receive = GetImage("network-receive");
  m_network_transmit_receive = GetImage("network-transmit-receive");
  m_networkStatus = new wxStaticBitmap(this, wxID_ANY, m_network_offline);
  m_networkStatus->SetToolTip(_("This icon shows if data is transmitted to or from maxima."));
  ReceiveTimer.SetOwner(this,wxID_ANY);
  SendTimer.SetOwner(this,wxID_ANY);
  m_icon_shows_receive = m_icon_shows_transmit = false;
}

void StatusBar::OnTimerEvent(wxTimerEvent& event)
{
  if((m_icon_shows_receive  == (ReceiveTimer.IsRunning())) &&
     (m_icon_shows_transmit == (SendTimer.IsRunning())))
    return;

  m_icon_shows_receive  = ReceiveTimer.IsRunning();
  m_icon_shows_transmit = SendTimer.IsRunning();
  bool m_icon_shows_transmit;

  if((ReceiveTimer.IsRunning())&&(SendTimer.IsRunning()))
    {
	m_networkStatus->SetBitmap(m_network_transmit_receive);
    }			       
  if((ReceiveTimer.IsRunning())&&(!SendTimer.IsRunning()))
    {
	m_networkStatus->SetBitmap(m_network_receive);
    }			       
  if((!ReceiveTimer.IsRunning())&&(SendTimer.IsRunning()))
    {
	m_networkStatus->SetBitmap(m_network_transmit);
    }			       
  if((!ReceiveTimer.IsRunning())&&(!SendTimer.IsRunning()))
    {
	m_networkStatus->SetBitmap(m_network_idle);
    }			       
}

void StatusBar::NetworkStatus(networkState status)
{
  switch(status)
    {
    case idle:
      m_networkStatus->SetBitmap(m_network_idle);
      break;
    case error:
      m_networkStatus->SetBitmap(m_network_error);
      break;
    case offline:
      m_networkStatus->SetBitmap(m_network_offline);
      break;
    case receive:
      {
	ReceiveTimer.StartOnce(100);
	wxTimerEvent dummy;
	OnTimerEvent(dummy);
      }
      break;
    case transmit:
      {
	SendTimer.StartOnce(100);
	wxTimerEvent dummy;
	OnTimerEvent(dummy);
      }
      break;	
    }
}

void StatusBar::OnSize(wxSizeEvent& event)
{
  wxRect rect;
  
  GetFieldRect(2, rect);
  wxSize size = m_networkStatus->GetSize();
  
  m_networkStatus->Move(rect.x + (rect.width - size.x) / 2,
			rect.y + (rect.height - size.y) / 2);
  
  event.Skip();
}

wxBitmap StatusBar::GetImage(wxString name)
#if defined (__WXMSW__) || defined (__WXMAC__)
{
  Dirstructure dirstructure;
  wxImage img = wxImage(dirstructure.ConfigStatusbarDir() + name + wxT(".png"));
  int imgWidth = GetSize().GetHeight();
  double scaleFactor = (double)imgWidth / img.GetWidth();
  img.Rescale(imgWidth,img.GetHeight()*scaleFactor,wxIMAGE_QUALITY_HIGH );
  return wxBitmap(img);
}
#else
{
  wxImage img;
  img = wxArtProvider::GetBitmap(name,wxART_TOOLBAR).ConvertToImage();
  double imgWidth = GetSize().GetHeight();
  double scaleFactor = imgWidth / img.GetWidth();
  img.Rescale(img.GetWidth()*scaleFactor,img.GetHeight()*scaleFactor,wxIMAGE_QUALITY_HIGH );
  return wxBitmap(img);
}
#endif


wxBEGIN_EVENT_TABLE(StatusBar, wxStatusBar)
EVT_SIZE(StatusBar::OnSize)
EVT_TIMER(wxID_ANY, StatusBar::OnTimerEvent)
wxEND_EVENT_TABLE()
