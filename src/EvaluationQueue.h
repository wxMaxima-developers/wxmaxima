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


#ifndef EVALUATIONQUEUE_H
#define EVALUATIONQUEUE_H

#include "GroupCell.h"
#include "wx/arrstr.h"

class EvaluationQueueElement {
  public:
    EvaluationQueueElement(GroupCell* gr);
    ~EvaluationQueueElement() {
    }
    GroupCell* group;
    EvaluationQueueElement* next;
};

// A simple FIFO queue with manual removal of elements
class EvaluationQueue
{
private:
  wxArrayString m_tokens;
  int m_size;
  EvaluationQueueElement* m_queue;
  EvaluationQueueElement* m_last;
public:
  bool m_workingGroupChanged;
  EvaluationQueue();
  ~EvaluationQueue() {};
  
  bool IsInQueue(GroupCell* gr);
  
  void AddToQueue(GroupCell* gr);
  //! Adds all commands in commandString as separate tokens to the queue.
  void AddTokens(wxString commandString);
  void AddHiddenTreeToQueue(GroupCell* gr);
  //! Removes the first cell in the queue
  void RemoveFirst();
  //! Gets the first cell in the queue
  GroupCell* GetFirst();
  bool Empty();
  //! Clear the queue
  void Clear();
  //! Return the next string that needs to be evaluated.
  wxString GetString();
  
  //! Get the size of the queue
  int Size()
    {
      return m_size;
    }
};


#endif /* EVALUATIONQUEUE_H */
