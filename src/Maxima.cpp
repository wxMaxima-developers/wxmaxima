// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#include "Maxima.h"
#include "wxMaxima.h"
#include <wx/app.h>
#include <wx/debug.h>
#include <iostream>

//! The time, in ms, we'll wait for an end of string to arrive from maxima after
//! the input was first read.
static constexpr int STRING_END_TIMEOUT = 5000;
//! The period, in ms, with which we may force a read from Maxima in case we expect
//! some input but got no notifications for it.
#ifdef __WINDOWS__
static constexpr int INPUT_RESTART_PERIOD = 3000;
#else
static constexpr int INPUT_RESTART_PERIOD = -1;
#endif

wxDEFINE_EVENT(EVT_MAXIMA, MaximaEvent);

Maxima::Maxima(wxSocketBase *socket) : m_socket(socket)
{
  wxASSERT(socket);
  Bind(wxEVT_TIMER, wxTimerEventHandler(Maxima::TimerEvent), this);
  Bind(wxEVT_SOCKET, wxSocketEventHandler(Maxima::SocketEvent), this);

  m_socket->SetEventHandler(*this);
  m_socket->SetNotify(wxSOCKET_INPUT_FLAG|wxSOCKET_OUTPUT_FLAG|wxSOCKET_LOST_FLAG);
  m_socket->Notify(true);
  m_socket->SetFlags(wxSOCKET_NOWAIT|wxSOCKET_REUSEADDR);
  m_socket->SetTimeout(30);

  // There are some hints in the code history that wxSOCKET_INPUT
  // event may be "flaky". We don't want wxMaxima to get stuck, but we don't want
  // to be forcing idle events for nothing. So this is a "way out" - we monitor
  // the progress of input, and only act if we expected some but none came.
  if (INPUT_RESTART_PERIOD > 0)
    m_readIdleTimer.Start(INPUT_RESTART_PERIOD);
}

Maxima::~Maxima()
{
  m_socket->Close();
}

bool Maxima::Write(const void *buffer, size_t length)
{
  if (!m_socketOutputData.IsEmpty())
  {
    if (buffer && length)
      m_socketOutputData.AppendData(buffer, length);
    buffer = m_socketOutputData.GetData();
    length = m_socketOutputData.GetDataLen();
  }
  if (!length)
    return false;
  m_socket->Write(buffer, length);
  if (m_socket->Error() && m_socket->LastError() != wxSOCKET_WOULDBLOCK)
  {
    MaximaEvent event(MaximaEvent::WRITE_ERROR, this);
    ProcessEvent(event);
    m_socketOutputData.Clear();
    return true;
  }
  auto const wrote = m_socket->LastWriteCount();
  if (wrote < length)
  {
    auto *const source = reinterpret_cast<const char *>(buffer);
    auto const leftToWrite = length - wrote;
    if (m_socketOutputData.IsEmpty())
      m_socketOutputData.AppendData(source + wrote, leftToWrite);
    else
    {
      memmove(m_socketOutputData.GetData(), source + wrote, leftToWrite);
      m_socketOutputData.SetDataLen(leftToWrite);
    }
  }
  else
    m_socketOutputData.Clear();
  return true;
}

UTF8Decoder::DecodeResult Maxima::DecodeFromSocket(size_t maxRead)
{
  return m_decoder.Decode(m_socketDecodeState, m_socketInput, maxRead, maxRead + 16);
}

void Maxima::SocketEvent(wxSocketEvent &event)
{
  switch (event.GetSocketEvent())
  {
  case wxSOCKET_INPUT:
    ReadSocket();
    break;
  case wxSOCKET_OUTPUT:
    if (Write(nullptr, 0))
    {
      MaximaEvent discEvent(MaximaEvent::WRITE_PENDING, this);
      ProcessEvent(discEvent);
    }
    break;
  case wxSOCKET_LOST:
  {
    MaximaEvent discEvent(MaximaEvent::DISCONNECTED, this);
    ProcessEvent(discEvent);
    break;
  }
  case wxSOCKET_CONNECTION:
    // We don't get these events, as we only deal with connected sockets.
    break;
  }
}

void Maxima::TimerEvent(wxTimerEvent &event)
{
  if (&event.GetTimer() == &m_stringEndTimer)
  {
    MaximaEvent sendevent(MaximaEvent::READ_TIMEOUT, this, std::move(m_socketInputData));
    ProcessEvent(sendevent);
  }
  else if (&event.GetTimer() == &m_readIdleTimer)
  {
    // Let's keep the input from Maxima flowing in. This is a platform-specific
    // workaround, so this timer is not guaranteed to fire at all.
    ReadSocket();
  }
}

void Maxima::ReadSocket()
{
  // It is theoretically possible that the client has exited after sending us
  // data and before we had been able to process it.
  if (!m_socket->IsConnected() || !m_socket->IsData())
    return;

  // Read up to 64k of data in one go
  constexpr auto readChunkSize = 65536;
  if (!m_socketInput.Eof())
  {
    auto const decoded = DecodeFromSocket(readChunkSize);
    m_socketInputData.append(decoded.output, decoded.outputSize);
    if (decoded.bytesRead >= (readChunkSize - 16))
    {
      // More data is possibly available for reading, so let's schedule another
      // read. "Losing" the input triggers is a frequent source of bugs,
      // so it's best to play it safe and trigger a "continuation" read anyway.
      // The 16 byte "margin of safety" is probably unnecessary but at least it'd protect
      // against platform-specific off-by-one errors.
      MaximaEvent event(MaximaEvent::READ_PENDING, this);
      ProcessEvent(event);
      CallAfter(&Maxima::ReadSocket);
      return; // Don't process pending data further
    }
  }

  wxm::NormalizeEOLsRemoveNULs(m_socketInputData);

  if (m_pipeToStdout)
    std::cout << m_socketInputData;

  if (m_first || wxm::EndsWithChar(m_socketInputData, '\n') || m_socketInputData.EndsWith(wxMaxima::m_promptSuffix))
  {
    m_stringEndTimer.Stop();
    MaximaEvent event(MaximaEvent::READ_DATA, this, std::move(m_socketInputData));
    ProcessEvent(event);
  }
  else
    m_stringEndTimer.StartOnce(STRING_END_TIMEOUT);
}

MaximaEvent::MaximaEvent(MaximaEvent::Cause cause, Maxima *source, wxString &&data)
    : wxEvent(0, EVT_MAXIMA), m_cause(cause), m_source(source)
{
  m_data.swap(data);
}

MaximaEvent::MaximaEvent(MaximaEvent::Cause cause, Maxima *source)
    : wxEvent(0, EVT_MAXIMA), m_cause(cause), m_source(source) {}

wxEvent *MaximaEvent::Clone() const
{
  auto event = std::make_unique<MaximaEvent>(GetCause(), GetSource());
  event->SetData(GetData());
  return event.release();
}
