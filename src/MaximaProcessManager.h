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
  The wxMaxima-side owner of the Maxima process and its control socket.

  Starting, stopping, connecting to and interrupting the Maxima process (and
  the gnuplot subprocess it drives) used to be spread across the wxMaxima god
  class. That lifecycle is being peeled off into this class to shrink
  wxMaxima.cpp. Handlers still drive the frame (its status bar, worksheet,
  configuration) through the m_wxMaxima reference, so MaximaProcessManager is a
  friend of wxMaxima; the wxMaxima frame owns this object by value.

  The first piece to move is the async-signal-safe safety net that kills any
  still-running child Maxima if wxMaxima is terminated by a signal or crashes
  before its destructor runs. That net is entirely static (a fixed array of PIDs
  plus signal handlers) and needs no wxMaxima instance.
*/

#ifndef MAXIMAPROCESSMANAGER_H
#define MAXIMAPROCESSMANAGER_H

#include <wx/string.h>

class wxMaxima;
class wxSocketEvent;
class wxProcessEvent;
class wxThreadEvent;
class wxCommandEvent;

/*! The Maxima process/socket lifecycle extracted from the wxMaxima god class.

  Owned by value by the wxMaxima frame. Holds a reference back to that frame for
  the services the lifecycle handlers need.
*/
class MaximaProcessManager
{
public:
  explicit MaximaProcessManager(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Remember a running child Maxima's (group-leader) PID so a
  //! signal/crash handler can kill it. Async-signal-safe; no-op on Windows.
  static void RegisterChildMaxima(long pid);

  //! Forget a child Maxima's PID once we have taken it down ourselves.
  //! Async-signal-safe; no-op on Windows.
  static void UnregisterChildMaxima(long pid);

  /*! SIGKILL every child Maxima we still have registered.

    Async-signal-safe (only kill() and reads of an array of sig_atomic_t) so it
    is safe to call from a signal handler or from wxWidgets' fatal-exception
    handler. No-op on Windows, where Maxima runs under maxima.bat and the
    taskkill-based cleanup applies instead.
  */
  static void KillAllChildMaximas();

  /*! Install the SIGTERM/SIGINT/SIGHUP handlers that run KillAllChildMaximas()
    so a child Maxima is not orphaned and left eating CPU/RAM if we are
    terminated by a signal instead of shutting down cleanly. No-op on Windows.
  */
  static void SetupTerminationHandlers();

  //! Opens the local socket server Maxima connects back to. Returns false if no
  //! server could be created.
  bool StartServer();

  //! Handles a socket event on the server: on a connection, accepts it.
  void ServerEvent(wxSocketEvent &event);

  //! Accepts Maxima's connection to our server socket, wires up the data
  //! stream and kicks off the initial variable/setup queries.
  void OnMaximaConnect();

  /*! (Re)starts the Maxima process (and its server socket) if needed.

    \param force true means restart Maxima unconditionally.
  */
  bool StartMaxima(bool force = false);

  //! Kills the Maxima process (and cleans up its temp files / registered PID).
  void KillMaxima(bool logMessage = true);

  //! Reacts to the Maxima process having exited (reports it and, unless we
  //! closed it on purpose, tries to restart it).
  void OnMaximaClose();
  //! wxEVT_END_PROCESS handler for the Maxima process; forwards to
  //! OnMaximaClose() if the PID matches.
  void OnMaximaClose(wxProcessEvent &event);

  //! The EVT_MAXIMA handler: classifies each chunk Maxima sent over the socket
  //! and hands it to the matching reader / updates the status bar.
  void MaximaEvent(wxThreadEvent &event);

  //! Sends the running Maxima an interrupt (Ctrl+G / the interrupt button).
  void Interrupt(wxCommandEvent &event);

  //! Resolves the gnuplot executable path and (re)starts a background query of
  //! the graphics terminals it supports.
  void GnuplotCommandName(wxString gnuplot);

  //! wxEVT_END_PROCESS handler for the gnuplot terminal-query subprocess:
  //! reads which terminals gnuplot supports and picks the plot driver.
  void OnGnuplotQueryTerminals(wxProcessEvent &event);

  //! wxEVT_END_PROCESS handler for the gnuplot subprocess: notes it has closed.
  void OnGnuplotClose(wxProcessEvent &event);

private:
  //! The wxMaxima frame whose services the lifecycle handlers drive. Not owned;
  //! the frame owns this object.
  wxMaxima &m_wxMaxima;
  /*! Reentrancy tripwire for MaximaEvent().

    True while MaximaEvent() is on the stack. MaximaEvent() must never nest:
    nesting means something below it pumped the event queue (a modal dialog,
    wxYield, ...) while a chunk from Maxima was still being interpreted, so a
    second chunk would be interpreted against half-updated state. All dialogs
    reachable from Maxima event handlers are therefore CallAfter-deferred;
    the flag catches (via wxASSERT) any future violation of that rule.
  */
  bool m_inMaximaEvent = false;
};

#endif // MAXIMAPROCESSMANAGER_H
