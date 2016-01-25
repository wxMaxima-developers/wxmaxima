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

#include "EvaluationQueue.h"

EvaluationQueueElement::EvaluationQueueElement(GroupCell* gr)
{
  group = gr;
  next = NULL;
}

bool EvaluationQueue::Empty()
{
  return (m_queue == NULL) && (m_tokens.IsEmpty());
}

EvaluationQueue::EvaluationQueue()
{
  m_size = 0;
  m_queue = NULL;
  m_last = NULL;
  m_workingGroupChanged = false;
}

void EvaluationQueue::Clear()
{
  while(!Empty())
    RemoveFirst();
  m_size = 0;
  m_tokens.Clear();
  m_workingGroupChanged = false;
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
  bool emptyWas = Empty();
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
  m_size++;
  if(emptyWas)
  {
    AddTokens(gr->GetEditable()->GetValue());
    m_workingGroupChanged = true;
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
  if(!m_tokens.IsEmpty())
  {
    m_workingGroupChanged = false;
    m_tokens.RemoveAt(0);
  }
  else
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
    m_size--;
    if(!Empty())
    {
      AddTokens(GetCell()->GetEditable()->GetValue());
      m_workingGroupChanged = true;
    }
  }

}

void EvaluationQueue::AddTokens(wxString commandString)
{
  size_t index = 0;


  wxString token;

  while(index < commandString.Length())
  {
    wxChar ch = commandString[index];

    // Add strings as a whole
    if(ch == wxT('\"'))
    {
      token += ch;
      index++;
      while((index < commandString.Length()) && ((ch = commandString[index]) != wxT('\"')))
      {
        if(commandString[index]==wxT('\\'))
        {
          token += commandString[index];
          index++;
        }
        token += commandString[index];
        index++;
      }
      if (index < commandString.Length())
      {
        token += commandString[index];
        index++;  
      }
      continue;
    }
    
    // :lisp -commands should be added as a whole
    if(ch == wxT(':'))
    {
      if(commandString.find(wxT("lisp"),index + 1) == index + 1)
      {
        token += commandString.Right(
          commandString.Length()-index
          );
        break;
      }
    }

    // Handle escaped chars
    if(ch == wxT('\\'))
    {
      token += ch;
      index++;
      if(index < commandString.Length())
      {
        ch = commandString[index];
        if(ch == wxT('\n'))
          continue;
      }
      else
        continue;
    }
      
    // Remove comments
    if((ch == wxT('/'))&&
       (index < commandString.Length() - 1)&&
       (commandString[index + 1]==wxT('*'))
      )
    {
      while((index < commandString.Length()) &&
            !(
              (commandString[index] == wxT('*')) &&
              (commandString[index + 1] == wxT('/'))
              )
        )
        index++;
      index += 2;
    }
    else
    {
      // Add the current char to the current token
      token += ch;
      index++;
    }
      
    // If we ended a command we now have to add a new token to the list.
    if(
      (ch == wxT(';')) || 
      (ch == wxT('$'))
      )
    {
      // trim() the token to allow MathCtrl::TryEvaluateNextInQueue()
      // to detect if the token is empty.      
      token.Trim(true);
      m_tokens.Add(token);
      std::cerr<<"Token1:"<<token<<"\n";
      token = wxEmptyString;
    }
  }
  // There might be a last token in the string we still haven't added.
  // Let's trim() it first: This way MathCtrl::TryEvaluateNextInQueue()
  // will detect if the token is empty.
  token.Trim(true);
  if(token != wxEmptyString)
  {
    std::cerr<<"Token2:"<<token<<"\n";
    m_tokens.Add(token);
  }
}

GroupCell* EvaluationQueue::GetCell()
{
  if(!m_tokens.IsEmpty())
  {
    return m_queue->group;
  }
  else
  {
    if (m_queue != NULL)
    {
      m_queue->group->GetEditable()->AddEnding();
      m_queue->group->GetEditable()->ContainsChanges(false);
      return m_queue->group;
    }
    else
      return NULL; // queue is empty
  }
}

wxString EvaluationQueue::GetCommand()
{
  wxString retval;
  m_userLabel = wxEmptyString;
  if(!m_tokens.IsEmpty())
  {
    retval = m_tokens[0];

    wxString userLabel;
    int colonPos;
    if((colonPos = retval.find(wxT(":")))!=wxNOT_FOUND)
    {
      userLabel = retval.Left(colonPos);
      userLabel.Trim(true);
      userLabel.Trim(false);
      if((wxIsalpha(userLabel[0]))||(userLabel[0]==wxT('\\'))||(userLabel[0]==wxT('_')))
      {
        for(size_t i=0;i<userLabel.Length();i++)
        {
          if(userLabel[i]==wxT('\\'))
            i++;
          else
          {
            if((!wxIsalnum(userLabel[i]))&&(userLabel[i]!='_'))
            {
              userLabel = wxEmptyString;
              break;
            }
          }
        }
        m_userLabel = userLabel;
      } 
    };
  }
  return retval;
}
