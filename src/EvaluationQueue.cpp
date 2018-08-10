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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class EvaluationQueue

  This queue contains all code cells that still have to be passed to maxima.
*/

#include "EvaluationQueue.h"

bool EvaluationQueue::Empty()
{
  return (m_queue.size() <= 1) && (m_commands.empty());
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
  bool removeFirst = !(m_queue.empty()) && gr == m_queue.front();
  m_queue.remove(gr);
  m_size = m_queue.size();
  if(removeFirst)
  {
    m_commands.clear();
    if(!m_queue.empty())
      AddTokens(gr);
  }
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
    do
    {
      if(m_queue.empty())
        return;
      
      m_queue.pop_front();
      m_size--;
      AddTokens(GetCell());
    } while (m_commands.empty() && (!m_queue.empty()));
    m_workingGroupChanged = true;
  }
}

void EvaluationQueue::AddTokens(GroupCell *cell)
{
  if(cell == NULL)
    return;
  
  wxString commandString = cell->GetEditable()->GetValue();

  m_knownAnswers = cell->m_knownAnswers;

  wxString token;

  wxString::iterator it = commandString.begin();
  int index = 0;
  
  while(it != commandString.end())
  {

    // Add strings as a whole
    if (*it == wxT('\"'))
    {
      token += *it;
      ++it;++index;
      while ((it != commandString.end()) && (*it != wxT('\"')))
      {
        if (*it == wxT('\\'))
        {
          token += *it;
          ++it;++index;
        }
        if(it != commandString.end())
        {
          token += *it;
          ++it;++index;
        }
      }
        // add the closing quote;
      if(it != commandString.end())
      {
        token += *it;
        ++it;++index;
      }
      continue;
    }

    // :lisp -commands should be added as a whole
    if (*it == wxT(':'))
    {
      wxString::iterator it2(it);
      wxString chars;
      for(int i=0; (i<6) && it2 != commandString.end(); i++)
      {
        chars += *it2;
        ++it2;
      }
      if (chars.StartsWith(wxT(":lisp")))
      {
        while(it != commandString.end())
        {
          token += *it;
          ++it;++index;
        }
        continue;
      }
    }

    // Handle escaped chars
    if (*it == wxT('\\'))
    {
      token += *it;
      ++it;++index;
      if(it != commandString.end())
      {
        token += *it;
        ++it;++index;
        continue;
      }
    }

    // Skip comments
    if (*it == wxT('/'))
    {
      wxString::iterator it2(it);
      ++it2;
      if((it2 != commandString.end()) && (*it2 == wxT('*')))
      {
        // Skip the comment start
        ++it;++index;
        if(it != commandString.end())
        {
          ++it;++index;
        }

        // skip the rest of the comment.
        if(it != commandString.end())
        {
          wxChar lastCh = *it;
          ++it;++index;
          while ((it != commandString.end()) &&
                 (! ((lastCh == wxT('*'))&&(*it == wxT('/'))))
            )
          {
            lastCh = *it;
            ++it;++index;
          }
        }
        if(it != commandString.end())
        {
          ++it;++index;
        }
        continue;
      }
    }
    // Add the current char to the current token
    token += *it;

    // If we ended a command we now have to add a new token to the list.
    if (
            (*it == wxT(';')) ||
            (*it == wxT('$'))
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
    ++it;++index;
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

