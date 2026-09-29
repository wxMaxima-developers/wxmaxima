// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  Implements MaximaInterruptChannel.
*/

#include "MaximaInterruptChannel.h"
#include <wx/log.h>

MaximaInterruptChannel::MaximaInterruptChannel(wxSocketBase *socket,
                                               const wxString &token)
  : m_socket(socket), m_handshake(std::string(token.utf8_str()))
{
  if (!m_socket || !m_socket->IsConnected()) {
    Close();
    return;
  }
  // Nothing here may block the GUI thread: we are told when there is data,
  // and the one thing ever written is a few bytes into an empty buffer.
  // (wxWidgets 3.2 asserts on wxSOCKET_NOWAIT_READ | wxSOCKET_WAITALL_WRITE.)
  m_socket->SetFlags(wxSOCKET_NOWAIT);
  m_socket->SetTimeout(1);
  Bind(wxEVT_SOCKET, &MaximaInterruptChannel::OnSocketEvent, this);
  m_socket->SetEventHandler(*this);
  m_socket->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_LOST_FLAG);
  m_socket->Notify(true);
}

MaximaInterruptChannel::~MaximaInterruptChannel() {
  if (m_socket)
    m_socket->Notify(false);
}

bool MaximaInterruptChannel::IsReady() const {
  return m_socket && m_socket->IsConnected() &&
    (m_handshake.GetState() == InterruptChannelHandshake::State::accepted);
}

bool MaximaInterruptChannel::IsDead() const {
  return !m_socket || !m_socket->IsConnected() ||
    (m_handshake.GetState() == InterruptChannelHandshake::State::rejected);
}

void MaximaInterruptChannel::Close() {
  if (!m_socket)
    return;
  m_socket->Notify(false);
  m_socket->Close();
}

void MaximaInterruptChannel::OnSocketEvent(wxSocketEvent &event) {
  if (!m_socket)
    return;
  switch (event.GetSocketEvent()) {
  case wxSOCKET_INPUT: {
    char buffer[512];
    m_socket->Read(buffer, sizeof(buffer));
    const auto count = m_socket->LastReadCount();
    if (m_handshake.GetState() != InterruptChannelHandshake::State::pending)
      break; // The Lisp has nothing to say after its token.
    switch (m_handshake.Feed(buffer, count)) {
    case InterruptChannelHandshake::State::accepted:
      wxLogMessage("Maxima has opened a channel wxMaxima can interrupt it through.");
      break;
    case InterruptChannelHandshake::State::rejected:
      wxLogMessage("A connection claiming to be Maxima's interrupt channel "
                   "didn't authenticate. Closing it.");
      Close();
      break;
    case InterruptChannelHandshake::State::pending:
      break;
    }
    break;
  }
  case wxSOCKET_LOST:
    wxLogMessage("Maxima's interrupt channel was closed.");
    Close();
    break;
  default:
    break;
  }
}

bool MaximaInterruptChannel::SendInterrupt() {
  if (!IsReady())
    return false;
  static const char command[] = "interrupt\n";
  m_socket->Write(command, sizeof(command) - 1);
  if (m_socket->Error() || (m_socket->LastWriteCount() != sizeof(command) - 1)) {
    wxLogMessage("Could not write to Maxima's interrupt channel.");
    return false;
  }
  return true;
}
