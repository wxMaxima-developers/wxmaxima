// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef WXMAXIMA_MAXIMA_IPC_H
#define WXMAXIMA_MAXIMA_IPC_H

#include <wx/hashmap.h>
#include <wx/string.h>
#include <memory>
#include <unordered_map>
#include <vector>

class wxMaxima;
class wxEvent;
class wxEvtHandler;

/*! Handles the "<ipc>" tag from Maxima
 *
 * This is used mainly for testing, so that Maxima can send synthetic events to wxMaxima.
 */
class MaximaIPC
{
public:
  explicit MaximaIPC(wxMaxima *wxm);

  /*! Reads the interprocess communications tag, used in test scripts,
   * etc.
   *
   * Since it may be unsafe, it must be enabled via command line.
   */
  void ReadInputData(wxString &data);
  static void EnableIPC() { m_enabled = true; }

  /*! Drains the event queue and dispatches a queued IPC event to its target.
   *
   * Only one event is drained at a time. This method should be invoked from
   * the idle loop, on the condition that the idle loop was about to sleep, i.e.
   * there was no more work for it to do.
   *
   * \returns true if there was an event in the queue. This value is meant to
   * re-schedule another idle event, so that the consequences of the event
   * could be handled by the idle handler, and so that any potential other
   * events could be subsequently drained.
   */
  bool DrainQueue();

private:
  wxMaxima *m_wxMaxima = nullptr;
  std::unordered_map<wxString, wxEvtHandler*, wxStringHash> m_eventTargets;

  struct QueuedEvent {
    wxEvtHandler *target;
    std::unique_ptr<wxEvent> event;
    QueuedEvent(wxEvtHandler *target, std::unique_ptr<wxEvent> &&event)
        : target(target), event(std::move(event)) {}
  };
  size_t m_queueTail = 0;
  std::vector<QueuedEvent> m_queue;

  static bool m_enabled;
};

#endif
