// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009      Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012      Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  The evaluation queue

  The class EvaluationQueue that is declared here handles the queue of commands
  that still have to be sent to maxima.
*/


#ifndef EVALUATIONQUEUE_H
#define EVALUATIONQUEUE_H

#include "precomp.h"
#include "cells/GroupCell.h"
#include <wx/arrstr.h>
#include <vector>
#include <utility>

//! A simple FIFO queue with manual removal of elements
class EvaluationQueue
{
private:
  class Command{
  public:
    Command(const wxString &strng, int index) : m_indexStart(index), m_command(strng) {}
    Command(Command &&o) noexcept : m_indexStart(o.m_indexStart), m_command(std::move(o.m_command)) {}
    Command(const Command &o) : m_indexStart(o.m_indexStart), m_command(o.m_command) {}
    Command &operator=(Command &&o) noexcept
      {
        m_indexStart = o.m_indexStart;
        m_command = std::move(o.m_command);
        return *this;
      }
    Command &operator=(const Command &o)
      {
        m_indexStart = o.m_indexStart;
        m_command = o.m_command;
        return *this;
      }

    const wxString &GetString() const { return m_command; }
    void AddEnding()
      {
        wxString trimmed = m_command;
        trimmed.Trim(true);
        if (!trimmed.EndsWith(wxS(";")) && !trimmed.EndsWith(wxS("$")))
          m_command += ";";
      }
    int GetIndex() const { return m_indexStart; }
  private:
    long m_indexStart = -1;
    wxString m_command;
  };

  /*! The command that is currently ready to be sent (0 or 1 entries).

    Commands of the current cell are produced ONE AT A TIME, lazily, so that each
    is tokenized in the lisp/maxima mode that is current when it is its turn -
    which we only know from the prompt maxima printed for the previous command.
    That is what makes lisp mode purely prompt-driven, and it lets us decide the
    trailing ";" only for the actual last command of a cell. (Sending more than
    one command at once would also make maxima treat later commands as the answer
    to a question the first one asked.)
  */
  std::vector<EvaluationQueue::Command> m_commands;
  //! The not-yet-tokenized remainder of the current cell's input.
  wxString m_pendingText;
  //! The configuration of the current cell (needed to re-tokenize m_pendingText).
  Configuration *m_pendingConfig = nullptr;
  //! How many characters of the current cell have been turned into commands
  //! already (so the current command can report its position within the cell).
  long m_cellConsumedChars = 0;
  std::size_t m_size;
  //! The label the user has assigned to the current command.
  wxString m_userLabel;
  //! The groupCells in the evaluation Queue.
  std::vector<CellPtr<GroupCell>> m_queue;

  //! Starts tokenizing a cell: remembers its text and produces its first command.
  void AddTokens(const GroupCell *cell);
  //! Tokenizes the next single command out of m_pendingText, in the mode current
  //! now, appending a ";" if it is the cell's last command and we are in maxima
  //! (not lisp) mode.
  void ProduceNextCommand();

public:
  /*! Query for the label the user has assigned to the current command.

    If there is no such label or the label as hidden deep down inside the command
    (in which case we assume the user wanted to hide it and for example didn't use
    it as a label at all) we return wxEmptyString.
  */
  wxString GetUserLabel() const
    { return m_userLabel; }

  int GetIndex() const
    {
      if (!m_commands.empty())
        return  m_commands.front().GetIndex();
      else
        return -1;
    }

  bool m_workingGroupChanged = false;

  EvaluationQueue();

  void AddEnding()
    {
      if (!m_commands.empty())
        m_commands.back().AddEnding();
    }

  virtual ~EvaluationQueue()
    {};

  //! Is GroupCell gr part of the evaluation queue?
  bool IsLastInQueue(GroupCell const *gr)
    {
      return !m_queue.empty() && (gr == m_queue.front());
    }

  //! Is GroupCell gr part of the evaluation queue?
  bool IsInQueue(GroupCell *gr) const;

  //! Adds a GroupCell to the evaluation queue.
  void AddToQueue(GroupCell *gr);

  //! Remove a GroupCell from the evaluation queue.
  void Remove(GroupCell *gr);

  //! Adds all hidden cells attached to the GroupCell gr to the evaluation queue.
  void AddHiddenTreeToQueue(const GroupCell *gr);

  //! Removes the first command in the queue
  void RemoveFirst();

  /*! Gets the cell the next command in the queue belongs to

    The command itself can be read out by issuing GetCommand();
  */
  GroupCell *GetCell();

  //! Is the queue empty?
  bool Empty() const;

  //! Clear the queue
  void Clear();

  //! Return the next command that needs to be evaluated.
  wxString GetCommand();

  //! Get the size of the queue [in cells]
  int Size() const { return m_size; }

  //! Roughly how many commands are still to be run in the current cell. Because
  //! commands are tokenized lazily (their exact split depends on the lisp/maxima
  //! mode, only known once each prior command's prompt arrives) this is a
  //! best-effort progress hint, not an exact count.
  int CommandsLeftInCell() const;
};


#endif /* EVALUATIONQUEUE_H */
