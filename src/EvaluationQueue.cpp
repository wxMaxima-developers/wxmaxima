///
///  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
///            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "EvaluationQueue.h"

EvaluationQueueElement::EvaluationQueueElement(GroupCell* gr)
{
  group = gr;
  next = NULL;
}

EvaluationQueue::EvaluationQueue()
{
  m_queue = NULL;
  m_last = NULL;
}

bool EvaluationQueue::IsInQueue(GroupCell* gr)
{
  EvaluationQueueElement* tmp = m_queue;
  while (tmp != NULL) {
    if (tmp->group == gr)
      return true;
    tmp = tmp->next;
  }
  return false;
}

void EvaluationQueue::AddToQueue(GroupCell* gr)
{
  if (gr->GetGroupType() != GC_TYPE_CODE
      || gr->GetEditable() == NULL) // dont add cells which can't be evaluated
    return;
  EvaluationQueueElement* newelement = new EvaluationQueueElement(gr);
  if (m_last == NULL)
    m_queue = m_last = newelement;
  else {
    m_last->next = newelement;
    m_last = newelement;
  }
}

/**
 * Add the tree of hidden cells to the EQ by recursively adding cells'
 * hidden branches to the EQ.
 */
void EvaluationQueue::AddHiddenTreeToQueue(GroupCell* gr)
{
  if (gr == NULL)
    return; // caller should check, but just in case

  GroupCell* cell = gr->GetHiddenTree();
  while (cell != NULL) {
    AddToQueue((GroupCell*) cell);
    AddHiddenTreeToQueue(cell);
    cell = dynamic_cast<GroupCell*>(cell->m_next);
  }
}

void EvaluationQueue::RemoveFirst()
{
  if (m_queue == NULL)
    return; // shouldn't happen
  EvaluationQueueElement* tmp = m_queue;
  if (m_queue == m_last) {
    m_queue = m_last = NULL;
  }
  else
    m_queue = m_queue->next;

  delete tmp;
}

GroupCell* EvaluationQueue::GetFirst()
{
  if (m_queue != NULL)
    return m_queue->group;
  else
    return NULL; // queu is empty
}
