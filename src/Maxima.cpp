// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include <utility>
#include "Maxima.h"
#include "wxMaxima.h"
#include <iostream>
#include <wx/app.h>
#include <wx/debug.h>

//! The time, in ms, we'll wait for an end of string to arrive from maxima after
//! the input was first read.
static constexpr int STRING_END_TIMEOUT = 5000;
//! The period, in ms, with which we may force a read from Maxima in case we
//! expect some input but got no notifications for it.
#ifdef __WINDOWS__
static constexpr int INPUT_RESTART_PERIOD = 3000;
#else
static constexpr int INPUT_RESTART_PERIOD = -1;
#endif

wxDEFINE_EVENT(EVT_MAXIMA, MaximaEvent);

// Attention: 'wxS("\n"), wxConvUTF8' should be the default and one might think, one can omit these
// parameters, but don't do it. wxWidgets (at least older versions, which are still used in Linux
// distributions) had a bug, so that without these parameters one get wrong characters.
// See: https://github.com/wxWidgets/wxWidgets/issues/14720#issuecomment-1010968576
Maxima::Maxima(wxSocketBase *socket) :
  m_socket(socket),
  m_socketInput(*m_socket),
  m_textInput(m_socketInput, wxS("\n"), wxConvUTF8)
{
  m_socketInputData.reserve(1000000);
  wxASSERT(socket);
  Bind(wxEVT_TIMER, wxTimerEventHandler(Maxima::TimerEvent), this);
  Bind(wxEVT_SOCKET, wxSocketEventHandler(Maxima::SocketEvent), this);
  m_socket->SetEventHandler(*this);
  m_socket->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_OUTPUT_FLAG |
                      wxSOCKET_LOST_FLAG);
  m_socket->Notify(true);
  m_socket->SetFlags(wxSOCKET_NOWAIT_READ);
  m_socket->SetTimeout(120);

  // There are some hints in the code history that wxSOCKET_INPUT
  // event may be "flaky". We don't want wxMaxima to get stuck, but we don't
  // want to be forcing idle events for nothing. So this is a "way out" - we
  // monitor the progress of input, and only act if we expected some but none
  // came.
  if (INPUT_RESTART_PERIOD > 0)
    m_readIdleTimer.Start(INPUT_RESTART_PERIOD);
}

Maxima::~Maxima() { m_socket->Close(); }

bool Maxima::Write(const void *buffer, std::size_t length) {
  if (!m_socketOutputData.IsEmpty()) {
    if (buffer && length)
      m_socketOutputData.AppendData(buffer, length);
    buffer = m_socketOutputData.GetData();
    length = m_socketOutputData.GetDataLen();
  }
  if (!length)
    return false;
  m_socket->Write(buffer, length);
  if (m_socket->Error() && m_socket->LastError() != wxSOCKET_WOULDBLOCK) {
    MaximaEvent *event = new MaximaEvent(MaximaEvent::WRITE_ERROR, this);
    QueueEvent(event);
    m_socketOutputData.Clear();
    return true;
  }
  auto const wrote = m_socket->LastWriteCount();
  if (wrote < length) {
    auto *const source = reinterpret_cast<const char *>(buffer);
    auto const leftToWrite = length - wrote;
    if (m_socketOutputData.IsEmpty())
      m_socketOutputData.AppendData(source + wrote, leftToWrite);
    else {
      memmove(m_socketOutputData.GetData(), source + wrote, leftToWrite);
      m_socketOutputData.SetDataLen(leftToWrite);
    }
  } else
    m_socketOutputData.Clear();
  return true;
}

void Maxima::SocketEvent(wxSocketEvent &event) {
  switch (event.GetSocketEvent()) {
  case wxSOCKET_INPUT:
    ReadSocket();
    break;
  case wxSOCKET_OUTPUT:
    if (Write(nullptr, 0)) {
      MaximaEvent *discEvent =
        new MaximaEvent(MaximaEvent::WRITE_PENDING, this);
      QueueEvent(discEvent);
    }
    break;
  case wxSOCKET_LOST: {
    MaximaEvent *discEvent = new MaximaEvent(MaximaEvent::DISCONNECTED, this);
    QueueEvent(discEvent);
    break;
  }
  case wxSOCKET_CONNECTION:
    // We don't get these events, as we only deal with connected sockets.
    break;
  }
}

void Maxima::TimerEvent(wxTimerEvent &event) {
  if (&event.GetTimer() == &m_stringEndTimer) {
    MaximaEvent *sendevent = new MaximaEvent(MaximaEvent::READ_TIMEOUT, this,
                                             std::move(m_socketInputData));
    QueueEvent(sendevent);
  } else if (&event.GetTimer() == &m_readIdleTimer) {
    // Let's keep the input from Maxima flowing in. This is a platform-specific
    // workaround, so this timer is not guaranteed to fire at all.
    ReadSocket();
  }
}

void Maxima::ReadSocket() {
  // It is theoretically possible that the client has exited after sending us
  // data and before we had been able to process it.
  if (!m_socket->IsConnected() || !m_socket->IsData())
    return;

  // std::cerr<<"------ transmission start ------\n";
  wxString line;
  wxString newLine = wxS("\n");
  wxChar ch;
  do
    {
      ch = m_textInput.GetChar();
      if(ch == wxS('\r'))
        ch = wxS('\n');
      if(ch != wxS('\0'))
        m_socketInputData.append(ch);
    }  while (m_socket->LastReadCount() > 0);
  {
    MaximaEvent *event = new MaximaEvent(MaximaEvent::READ_PENDING, this);
    QueueEvent(event);
  }
  // std::cerr<<m_socketInputData<<"\n";
  // std::cerr<<"------ transmission end ------\n";

  if ((m_pipeToStderr) && (!m_socketInputData.IsEmpty()))
    {
      std::cerr << m_socketInputData;
      std::cerr.flush();
    }

  if (m_first || wxm::EndsWithChar(m_socketInputData, '\n') ||
      m_socketInputData.EndsWith(wxMaxima::m_promptSuffix)) {
    m_stringEndTimer.Stop();
    MaximaEvent *event = new MaximaEvent(MaximaEvent::READ_DATA, this,
                                         std::move(m_socketInputData));
    QueueEvent(event);
  } else
    m_stringEndTimer.StartOnce(STRING_END_TIMEOUT);
}

MaximaEvent::MaximaEvent(MaximaEvent::Cause cause, Maxima *source,
                         wxString &&data)
  : wxEvent(0, EVT_MAXIMA), m_cause(cause), m_source(source) {
  m_data.swap(data);
}

MaximaEvent::MaximaEvent(MaximaEvent::Cause cause, Maxima *source)
  : wxEvent(0, EVT_MAXIMA), m_cause(cause), m_source(source) {}

wxEvent *MaximaEvent::Clone() const {
  auto event = std::make_unique<MaximaEvent>(GetCause(), GetSource());
  event->SetData(GetData());
  return event.release();
}
