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
#include "cells/CellPtr.h"

class wxMaxima;
class wxCommandEvent;
class GroupCell;

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

  /*! Sends Maxima the one-time startup configuration.

    Runs once when Maxima connects: loads wxmathml.lisp and sets the option
    variables (help format, prompts, gnuplot binary, version handshake).
  */
  void SetupVariables();

  /*! Remembers where in its cell the command that is being sent starts.

    Also forgets everything about the previous command's output. The end of
    the command is wxMaxima::m_commandIndex.
  */
  void StartCommand(long start);

  /*! Collects what Maxima printed in reply to the current command.

    Only plain text (the kind an error message is) is collected: a syntax
    error report can arrive in several pieces, and only all of it together
    tells where the error is.
  */
  void AddCommandOutput(const wxString &text);

  /*! Marks the statement the current command came from as the one that
    caused an error, in the cell \p cell (GH #289).

    Does nothing if the cell isn't the one that is being evaluated: an error
    can also arrive when nothing is, and there is no statement to blame then.
    Marks the statement only once per command, however many pieces the error
    message arrives in. If the error is a syntax error, also marks where in
    the statement it is, as soon as Maxima's output tells.
  */
  void MarkErrorStatement(GroupCell *cell);

  /*! If Maxima reported a syntax error for the current command, marks where
    in its statement it is, and puts the cursor there.

    Called whenever more of Maxima's output arrived, as the part that tells
    where the error is may come after the part that tells that there is one.
  */
  void MarkSyntaxErrorSpot();

private:
  //! A ":lisp-quiet (setq $linel ...)" command that tells Maxima's ASCII-art
  //! 2D/1D printer how many columns of the worksheet's monospace ASCII-math
  //! font currently fit into the window, freshly computed so it stays
  //! correct across window resizes and font-size changes between cells
  //! (unlike the rest of m_configCommands, which is only refreshed on
  //! ConfigChanged()). See Configuration::GetAsciiArtColumns().
  wxString LinelConfigCommand() const;

  /*! A ":lisp-quiet (setq *wx-cell-id* ...)" naming the cell whose commands
    are about to be sent, or an empty string if Maxima already knows.

    This is the only thing in this protocol that ties an output to a cell by
    name rather than by timing, and it exists for exactly one reason: a
    future Maxima that can run a command in the background needs to be able
    to say which cell that background job's eventual output belongs to, long
    after the cell stopped being the current one. See
    Doxygen/AsyncMaximaOutput.md.

    ":lisp-quiet" is not a stylistic choice: it produces no prompt of its
    own, and EvaluationQueue::RemoveFirst() advances the queue by one cell
    for every main prompt it sees, with no way to tell whose prompt it is.
    A plain Maxima statement here would silently drop a queued cell per
    command sent. */
  wxString CellIdConfigCommand(GroupCell *cell);

  //! What CellIdConfigCommand() last told Maxima, so the id is only resent
  //! when it actually changes rather than ahead of every single command of
  //! a multi-statement cell.
  wxString m_lastSentCellId;

  //! Where in its cell the current command starts. See StartCommand().
  long m_commandStart = -1;
  //! What Maxima printed in reply to the current command. See AddCommandOutput().
  wxString m_commandOutput;
  //! The cell an error for the current command was marked in, if any
  CellPtr<GroupCell> m_errorGroup;
  //! True once the place of the current command's syntax error is marked.
  bool m_errorSpotMarked = false;

  //! The wxMaxima frame whose services the driver uses. Not owned; the frame
  //! owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMAEVALUATOR_H
