// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file 
The evaluation queue

The class EvaluationQueue that is declared here handles the queue of commands 
that still have to be sent to maxima.
 */


#ifndef EVALUATIONQUEUE_H
#define EVALUATIONQUEUE_H

#include "GroupCell.h"
#include "wx/arrstr.h"

//! A queue element
class EvaluationQueueElement {
  public:
    EvaluationQueueElement(GroupCell* gr);
    ~EvaluationQueueElement() {
    }
    GroupCell* group;
    EvaluationQueueElement* next;
};

//! A simple FIFO queue with manual removal of elements
class EvaluationQueue
{
private:
  wxArrayString m_tokens;
  int m_size;
  //! The label the user has assigned to the current command.
  wxString m_userLabel;
  EvaluationQueueElement* m_queue;
  EvaluationQueueElement* m_last;
  //! Adds all commands in commandString as separate tokens to the queue.
  void AddTokens(wxString commandString);
public:
  /*! Query for the label the user has assigned to the current command.  

    If there is no such label or the label as hidden deep down inside the command
    (in which case we assume the user wanted to hide it and for example didn't use
    it as a label at all) we return wxEmptyString.
  */
  wxString GetUserLabel(){return m_userLabel;}
  bool m_workingGroupChanged;
  EvaluationQueue();
  ~EvaluationQueue() {};

  //! Is GroupCell gr part of the evaluation queue?
  bool IsLastInQueue(GroupCell* gr)
    {
      if(m_last == NULL)
        return false;
      else
        return(gr == m_last->group);
    }
  //! Is GroupCell gr part of the evaluation queue?
  bool IsInQueue(GroupCell* gr);
  //! Adds a GroupCell to the evaluation queue.
  void AddToQueue(GroupCell* gr);
  //! Remove a GroupCell from the evaluation queue.
  void Remove(GroupCell* gr);
  //! Adds all hidden cells attached to the GroupCell gr to the evaluation queue.
  void AddHiddenTreeToQueue(GroupCell* gr);
  //! Removes the first cell in the queue
  void RemoveFirst();
  /*! Gets the cell the next command in the queue belongs to

    The command itself can be read out by issuing GetCommand();
   */
  GroupCell* GetCell();
  //! Is the queue empty?
  bool Empty();
  //! Clear the queue
  void Clear();
  //! Return the next command that needs to be evaluated.
  wxString GetCommand();
  
  //! Get the size of the queue
  int Size()
    {
      return m_size;
    }

  //! Get the size of the queue
  int CommandsLeftInCell()
    {
      return m_tokens.GetCount();
    }
};


#endif /* EVALUATIONQUEUE_H */
