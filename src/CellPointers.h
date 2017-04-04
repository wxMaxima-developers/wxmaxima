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

#ifndef CELLPOINTERS_H
#define CELLPOINTERS_H

#include <wx/wx.h>
#include "MathCell.h"

/*! The storage for pointers to cells.

  If a cell is deleted it is necessary to remove all pointers that might
allow to access the now-defunct cell. These pointers are kept in this 
per-worksheet structure.
 */
class CellPointers
{
public:
  CellPointers();

  //! The EditorCell the mouse selection has started in
  MathCell *m_cellMouseSelectionStartedIn;
  //! The EditorCell the keyboard selection has started in
  MathCell *m_cellKeyboardSelectionStartedIn;
  //! The EditorCell the search was started in
  MathCell *m_cellSearchStartedIn;
  //! Which cursor position incremental search has started at?
  int m_indexSearchStartedAt;
  //! Which cell the blinking cursor is in?
  MathCell *m_activeCell;
  //! The y position the selection starts at
  int m_selectionStart_px;
  //! The y position the selection ends at
  int m_selectionEnd_px;
  //! The GroupCell that is under the mouse pointer 
  MathCell *m_groupCellUnderPointer;
  //! The last group cell maxima was working on.
  MathCell *m_lastWorkingGroup;
  /*! The currently selected string. 

    Since this string is defined here it is available in every editor cell
    for highlighting other instances of the selected string.
  */
  wxString m_selectionString;

  //! Set the y position of the selection start and end
  void SetSelectionRange_px(int start, int end)
  {
    m_selectionStart_px = start;
    m_selectionEnd_px = end;
  }

  //! Forget where the search was started
  void ResetSearchStart()
  {
    m_cellSearchStartedIn = NULL;
    m_indexSearchStartedAt = -1;
  }

  //! Forget where the mouse selection was started
  void ResetMouseSelectionStart()
  { m_cellMouseSelectionStartedIn = NULL; }

  //! Forget where the keyboard selection was started
  void ResetKeyboardSelectionStart()
  { m_cellKeyboardSelectionStartedIn = NULL; }
};

#endif
