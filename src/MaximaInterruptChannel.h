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
  Declares MaximaInterruptChannel, the connection wxMaxima interrupts Maxima through.
*/

#ifndef MAXIMAINTERRUPTCHANNEL_H
#define MAXIMAINTERRUPTCHANNEL_H

#include "InterruptChannelHandshake.h"
#include <wx/event.h>
#include <wx/socket.h>
#include <memory>

/*! A second connection from Maxima's Lisp that interrupts its computation.

  Interrupting Maxima used to rely on the operating system: kill(SIGINT) on
  POSIX, and on MS Windows a shared-memory segment named after the Lisp's pid
  or, failing that, a console Ctrl+C. The Windows ways fail in ways a user
  can do nothing about (GH #2289): the pid may be unknown, the segment is
  only created if Maxima's signal thread was started, and wxmaxima.exe has no
  console.

  So a Lisp that has threads opens a second connection to wxMaxima's server
  right after connecting (wx-open-interrupt-channel in wxMathML.lisp). A
  thread in the Lisp waits on it, and SendInterrupt() writing "interrupt"
  makes that thread interrupt the one evaluating Maxima's commands. A Lisp
  without threads never opens one; IsReady() then stays false and the caller
  falls back to the old ways.

  The connection only counts after it has sent the secret wxMaxima gave
  Maxima (see InterruptChannelHandshake): anybody on this machine can connect
  to the server's port.
*/
class MaximaInterruptChannel : public wxEvtHandler
{
public:
  /*! Takes ownership of \p socket, a connection wxSocketServer::Accept() returned.

    \param token The secret the connection has to send first.
  */
  MaximaInterruptChannel(wxSocketBase *socket, const wxString &token);
  ~MaximaInterruptChannel() override;

  //! True once the connection has proved it comes from our Maxima, and while it lasts.
  bool IsReady() const;
  //! True once the connection failed the handshake or was closed.
  bool IsDead() const;

  //! Asks Maxima to interrupt its computation. Returns false if that isn't possible.
  bool SendInterrupt();

private:
  void OnSocketEvent(wxSocketEvent &event);
  void Close();

  //! wxSocketBase has to be deleted by Destroy(), which waits for pending events.
  struct SocketDeleter {
    void operator()(wxSocketBase *socket) const { socket->Destroy(); }
  };
  std::unique_ptr<wxSocketBase, SocketDeleter> m_socket;
  InterruptChannelHandshake m_handshake;
};

#endif // MAXIMAINTERRUPTCHANNEL_H
