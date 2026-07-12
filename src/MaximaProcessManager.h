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

class wxMaxima;

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

private:
  //! The wxMaxima frame whose services the lifecycle handlers drive. Not owned;
  //! the frame owns this object. Unused until the (non-static) lifecycle
  //! handlers move here in a later slice; the safety net moved first is static.
  [[maybe_unused]] wxMaxima &m_wxMaxima;
};

#endif // MAXIMAPROCESSMANAGER_H
