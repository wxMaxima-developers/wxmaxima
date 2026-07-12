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
  Implements the Maxima process/socket lifecycle extracted from the wxMaxima
  god class.
*/

#include "MaximaProcessManager.h"

#ifndef __WXMSW__
#include <csignal>
#include <cstring>
#include <sys/types.h>
#endif

// ---------------------------------------------------------------------------
// If wxMaxima is terminated by a signal (or crashes) our destructor (and hence
// KillMaxima()) does not run, so a child Maxima would be orphaned. Worse, a
// Maxima busy in a computation only notices its closed control socket the next
// time it reads from it, so it keeps running and eats CPU/RAM. To avoid that we
// keep an async-signal-safe registry of running child Maxima group-leader PIDs
// and SIGKILL them from a SIGTERM/SIGINT/SIGHUP handler (and from
// OnFatalException()). On Windows Maxima runs under maxima.bat and the existing
// taskkill-based cleanup is used instead; everything here compiles to no-ops.
// ---------------------------------------------------------------------------
#ifndef __WXMSW__
namespace {
// One slot per wxMaxima window. We only register/unregister from the main (GUI)
// thread and each slot is a sig_atomic_t (atomic single writes), so the handler
// never sees a torn value and no locking is needed.
constexpr int kMaxChildMaximas = 256;
volatile sig_atomic_t s_childMaximaPids[kMaxChildMaximas] = {};

void wxmTerminationSignalHandler(int sig) {
  MaximaProcessManager::KillAllChildMaximas();
  // The disposition was reset to default by SA_RESETHAND before we were called,
  // so re-raising now performs the default action (terminate) with the right
  // status. raise() and the work above are all async-signal-safe.
  raise(sig);
}
} // namespace
#endif

void MaximaProcessManager::RegisterChildMaxima(long pid) {
#ifndef __WXMSW__
  if (pid <= 0)
    return;
  for (auto &slot : s_childMaximaPids) {
    if (slot == 0) {
      slot = static_cast<sig_atomic_t>(pid);
      return;
    }
  }
#else
  (void)pid;
#endif
}

void MaximaProcessManager::UnregisterChildMaxima(long pid) {
#ifndef __WXMSW__
  if (pid <= 0)
    return;
  for (auto &slot : s_childMaximaPids) {
    if (slot == static_cast<sig_atomic_t>(pid)) {
      slot = 0;
      return;
    }
  }
#else
  (void)pid;
#endif
}

void MaximaProcessManager::KillAllChildMaximas() {
#ifndef __WXMSW__
  for (sig_atomic_t pid : s_childMaximaPids) {
    if (pid > 0) {
      // Interactive Maxima is a process-group leader, so the negative pid kills
      // the whole group (the maxima wrapper and the Lisp it forks). In batch
      // mode it is not a group leader: the group kill is then a harmless ESRCH
      // no-op and the direct kill takes down the wrapper.
      kill(static_cast<pid_t>(-pid), SIGKILL);
      kill(static_cast<pid_t>(pid), SIGKILL);
    }
  }
#endif
}

void MaximaProcessManager::SetupTerminationHandlers() {
#ifndef __WXMSW__
  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = wxmTerminationSignalHandler;
  sigemptyset(&sa.sa_mask);
  // SA_RESETHAND: after we kill Maxima and re-raise, the default action runs and
  // a repeated signal is handled normally (no risk of looping in the handler).
  sa.sa_flags = SA_RESETHAND;
  sigaction(SIGTERM, &sa, nullptr);
  sigaction(SIGINT, &sa, nullptr);
  sigaction(SIGHUP, &sa, nullptr);
#endif
}
