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
  Drives the evaluation of the worksheet: it pulls cells off the evaluation
  queue, sends their commands to the Maxima process and reacts to the results.

  This "what do we send to Maxima next, and when" logic used to be spread across
  the wxMaxima god class; it is being peeled off into this class to shrink
  wxMaxima.cpp. The handlers still drive the frame (its worksheet, status bar,
  configuration, the socket in m_client) through the m_wxMaxima reference, so
  MaximaEvaluator is a friend of wxMaxima; the wxMaxima frame owns this object by
  value. Note this is the driver, not the queue data structure itself (that is
  the EvaluationQueue the worksheet owns).
*/

#ifndef MAXIMAEVALUATOR_H
#define MAXIMAEVALUATOR_H

#include <wx/string.h>

class wxMaxima;
class wxCommandEvent;

/*! The evaluation-queue driver / command protocol extracted from wxMaxima.

  Owned by value by the wxMaxima frame. Holds a reference back to that frame for
  the services it needs (the worksheet, the socket, the status bar).
*/
class MaximaEvaluator
{
public:
  explicit MaximaEvaluator(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Sends a string to the Maxima process (optionally adding it to the history
  //! and/or marking it a background command that must not change the busy state).
  void SendMaxima(wxString s, bool addToHistory = false, bool background = false);

  //! Prepares the worksheet once Maxima's first prompt has been seen.
  void FirstOutput();

  /*! Reacts to a Maxima error: decides whether to keep evaluating the queue.

    \return true if the evaluation queue was cleared (do not send more).
  */
  bool AbortOnError();

  /*! Queries the value of the next variable wxMaxima wants to know about.

    \return true if a query was sent.
  */
  bool QueryVariableValue();

  //! The "evaluate" menu/hotkey handler: queues the active/selected cells and
  //! starts evaluation (or sends an answer to a pending question).
  void EvaluateEvent(wxCommandEvent &event);

  //! If Maxima is idle and the queue is non-empty, sends the next command;
  //! otherwise settles the UI into its idle state. The core queue pump.
  void TriggerEvaluation();

private:
  //! The wxMaxima frame whose services the driver uses. Not owned; the frame
  //! owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMAEVALUATOR_H
