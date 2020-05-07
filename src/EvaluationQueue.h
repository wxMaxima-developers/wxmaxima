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

#include "GroupCell.h"
#include "wx/arrstr.h"
#include <vector>

//! A simple FIFO queue with manual removal of elements
class EvaluationQueue
{
private:

  class Command{
  public:
    Command(const wxString &string, int index) : m_indexStart(index), m_command(string) {}
    Command(Command &&o) : m_indexStart(o.m_indexStart), m_command(std::move(o.m_command)) {}
    Command(const Command &o) : m_indexStart(o.m_indexStart), m_command(o.m_command) {}
    Command &operator=(Command &&o)
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
    void AddEnding() { m_command += ";"; }
    int GetIndex() const { return m_indexStart; }
  private:
    int m_indexStart;
    wxString m_command;
  };
    
  /*! A list of all the commands in the current cell
    
    We need to track each single command:
    - If we send more than one command at once maxima will interpret the command
       as an answer to an eventual question and
       - we need to know when to switch to the next cell
  */
  std::vector<EvaluationQueue::Command> m_commands;
  int m_size;
  //! The label the user has assigned to the current command.
  wxString m_userLabel;
  //! The groupCells in the evaluation Queue.
  std::vector<GroupCell *> m_queue;

  //! Adds all commands in commandString as separate tokens to the queue.
  void AddTokens(GroupCell *cell);

  //! A list of answers provided by the user
  wxArrayString m_knownAnswers;
  wxArrayString m_knownQuestions;

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

  bool m_workingGroupChanged;

  EvaluationQueue();

  void AddEnding()
    {
      if (!m_commands.empty())
        m_commands.back().AddEnding();
    }
  
  ~EvaluationQueue()
  {};

  //! Is GroupCell gr part of the evaluation queue?
  bool IsLastInQueue(GroupCell *gr)
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
  void AddHiddenTreeToQueue(GroupCell *gr);

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

  //! Get the size of the queue
  int CommandsLeftInCell() const { return m_commands.size(); }
};


#endif /* EVALUATIONQUEUE_H */
