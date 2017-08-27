// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009      Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012      Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class EvaluationQueue

  This queue contains all code cells that still have to be passed to maxima.
*/

#include "EvaluationQueue.h"

bool EvaluationQueue::Empty()
{
  return (m_queue.empty()) && (m_commands.empty());
}

EvaluationQueue::EvaluationQueue()
{
  m_size = 0;
  m_workingGroupChanged = false;
}

void EvaluationQueue::Clear()
{
  while (!Empty())
    RemoveFirst();
  m_size = 0;
  m_commands.clear();
  m_workingGroupChanged = false;
}

bool EvaluationQueue::IsInQueue(GroupCell *gr)
{
  for(std::list<GroupCell *>::iterator it=m_queue.begin(); it != m_queue.end(); ++it)
    if (*it == gr)
      return true;
  
  return false;
}

void EvaluationQueue::Remove(GroupCell *gr)
{
  bool removeFirst = (gr == m_queue.front());
  m_queue.remove(gr);
  if(removeFirst)
  {
    m_commands.clear();
    if(!m_queue.empty())
      AddTokens(gr);
  }
  m_size = m_queue.size();
}

void EvaluationQueue::AddToQueue(GroupCell *gr)
{
  if(gr == NULL)
    return;
  
  if (gr->GetGroupType() != GC_TYPE_CODE
      || gr->GetEditable() == NULL) // don't add cells which can't be evaluated
    return;

  gr->GetEditable()->AddEnding();

  if(m_queue.empty())
  {
    AddTokens(gr);
    m_workingGroupChanged = true;
  }
  m_size++;
  m_queue.push_back(gr);
}

/**
 * Add the tree of hidden cells to the EQ by recursively adding cells'
 * hidden branches to the EQ.
 */
void EvaluationQueue::AddHiddenTreeToQueue(GroupCell *gr)
{
  if (gr == NULL)
    return; // caller should check, but just in case

  GroupCell *cell = gr->GetHiddenTree();
  while (cell != NULL)
  {
    AddToQueue(dynamic_cast<GroupCell *>(cell));
    AddHiddenTreeToQueue(cell);
    cell = dynamic_cast<GroupCell *>(cell->m_next);
  }
}

void EvaluationQueue::RemoveFirst()
{
  if (!m_commands.empty())
  {
    m_workingGroupChanged = false;
    m_commands.pop_front();
  }
  else
  {
    if(m_queue.empty())
      return;

    m_queue.pop_front();
    m_size--;
    if (!Empty())
    {
      AddTokens(GetCell());
      m_workingGroupChanged = true;
    }
  }
}

void EvaluationQueue::AddTokens(GroupCell *cell)
{
  if(cell == NULL)
    return;
  
  wxString commandString = cell->GetEditable()->GetValue();
  size_t index = 0;

  m_knownAnswers = cell->m_knownAnswers;

  wxString token;

  while (index < commandString.Length())
  {
    wxChar ch = commandString[index];

    // Add strings as a whole
    if (ch == wxT('\"'))
    {
      token += ch;
      index++;
      while ((index < commandString.Length()) && ((ch = commandString[index]) != wxT('\"')))
      {
        if (commandString[index] == wxT('\\'))
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
    if (ch == wxT(':'))
    {
      if (commandString.find(wxT("lisp"), index + 1) == index + 1)
      {
        token += commandString.Right(
                commandString.Length() - index
        );
        break;
      }
    }

    // Handle escaped chars
    if (ch == wxT('\\'))
    {
      if ((index + 1 < commandString.Length()) && (commandString[index + 1] == wxT('\n')))
      {
        index += 2;
        continue;
      }

      token += ch;
      index++;
      if (index < commandString.Length())
        token += commandString[index++];
      continue;
    }

    // Remove comments
    if ((ch == wxT('/')) &&
        (index < commandString.Length() - 1) &&
        (commandString[index + 1] == wxT('*'))
            )
    {
      while ((index < commandString.Length()) &&
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
    if (
            (ch == wxT(';')) ||
            (ch == wxT('$'))
            )
    {
      // trim() the token to allow MathCtrl::TryEvaluateNextInQueue()
      // to detect if the token is empty.
      // The Trim(false) is needed in order to make a cell ending in a semicolon
      // followed by a comment followed by a semicolon work.
      token.Trim(false);
      token.Trim(true);
      if (token.Length() > 1)
        m_commands.push_back(command(token, index));
      token = wxEmptyString;
    }
  }
  // There might be a last token in the string we still haven't added.
  // Let's trim() it first: This way MathCtrl::TryEvaluateNextInQueue()
  // will detect if the token is empty.
  token.Trim(false);
  token.Trim(true);
  if (token.Length() > 1)
    m_commands.push_back(command(token, index));
}

GroupCell *EvaluationQueue::GetCell()
{
  if(m_queue.empty())
    return NULL;
  else
    return m_queue.front();
}

wxString EvaluationQueue::GetCommand()
{
  wxString retval;
  m_userLabel = wxEmptyString;
  if (!m_commands.empty())
  {
    retval = m_commands.front().GetString();

    wxString userLabel;
    int colonPos;
    if ((colonPos = retval.find(wxT(":"))) != wxNOT_FOUND)
    {
      userLabel = retval.Left(colonPos);
      userLabel.Trim(true);
      userLabel.Trim(false);
      if ((wxIsalpha(userLabel[0])) || (userLabel[0] == wxT('\\')) || (userLabel[0] == wxT('_')))
      {
        for (size_t i = 0; i < userLabel.Length(); i++)
        {
          if (userLabel[i] == wxT('\\'))
            i++;
          else
          {
            if ((!wxIsalnum(userLabel[i])) && (userLabel[i] != '_'))
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

