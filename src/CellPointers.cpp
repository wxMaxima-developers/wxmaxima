// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class CellPonters

  CellPointers is the class cells keep per-worksheet-information in that needs to be 
  invalidate on deleting cells it points to.
*/

#include "CellPointers.h"

CellPointers::CellPointers(wxScrolledCanvas *mathCtrl)
{
  m_mathCtrl = mathCtrl;
  m_cellMouseSelectionStartedIn = NULL;
  m_cellKeyboardSelectionStartedIn = NULL;
  m_cellUnderPointer = NULL;
  m_cellSearchStartedIn = NULL;
  m_answerCell = NULL;
  m_indexSearchStartedAt = -1;
  m_activeCell = NULL;
  m_groupCellUnderPointer = NULL;
  m_lastWorkingGroup = NULL;
  m_workingGroup = NULL;
  m_selectionString = wxEmptyString;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;

}

bool CellPointers::ErrorList::Contains(MathCell *cell)
{
  for(std::list<MathCell *>::iterator it = m_errorList.begin(); it != m_errorList.end();++it)
  {
    if((*it)==cell)
      return true;
  }
  return false;
}
