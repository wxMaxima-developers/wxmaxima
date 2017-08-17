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
#include <list>

/*! The storage for pointers to cells.

  If a cell is deleted it is necessary to remove all pointers that might
allow to access the now-defunct cell. These pointers are kept in this 
per-worksheet structure.
 */
class CellPointers
{
public:
  CellPointers();
  /*! Returns the cell maxima currently works on. NULL if there isn't such a cell.
    
    \param resortToLast true = if we already have set the cell maxima works on to NULL
    use the last cell maxima was known to work on.
   */
  MathCell *GetWorkingGroup(bool resortToLast = false)
    {
      if ((m_workingGroup != NULL) || (!resortToLast))
        return m_workingGroup;
      else
        return m_lastWorkingGroup;
    }
  //! Sets the cell maxima currently works on. NULL if there isn't such a cell.
  void SetWorkingGroup(MathCell *group)
    {
      if(group != NULL)
        m_lastWorkingGroup = group;
      m_workingGroup = group;
    }
  //! A list of editor cells containing error messages.
  class ErrorList
  {
  public:
    ErrorList(){};
    //! Is the list of errors empty?
    bool Empty(){return m_errorList.empty();}
    //! Remove one specific GroupCell from the list of errors
    void Remove(MathCell * cell){m_errorList.remove(cell);}
    //! Does the list of GroupCell with errors contain cell?
    bool Contains(MathCell * cell);
    //! Mark this GroupCell as containing errors
    void Add(MathCell * cell){m_errorList.push_back(cell);}
    //! The first GroupCell with error that is still in the list
    MathCell *FirstError(){if(m_errorList.empty())return NULL; else return m_errorList.front();}
    //! The last GroupCell with errors in the list
    MathCell *LastError(){if(m_errorList.empty())return NULL; else return m_errorList.back();}
    //! Empty the list of GroupCells with errors
    void Clear(){m_errorList.clear();}
  private:
    //! A list of GroupCells that contain errors
    std::list<MathCell *> m_errorList;
  };

  //! The list of cells maxima has complained about errors in
  ErrorList m_errorList;
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
  //! The GroupCell that is under the mouse pointer 
  MathCell *m_groupCellUnderPointer;
  //! The EditorCell that contains the currently active question from maxima 
  MathCell *m_answerCell;
  //! The last group cell maxima was working on.
  MathCell *m_lastWorkingGroup;
  /*! The group cell maxima is currently working on.

    NULL means that maxima isn't currently evaluating a cell.
   */
  MathCell *m_workingGroup;
  /*! The currently selected string. 

    Since this string is defined here it is available in every editor cell
    for highlighting other instances of the selected string.
  */
  wxString m_selectionString;

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
  
  /*! The first cell of the currently selected range of Cells.
    
    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is 
    always above m_selectionEnd.
    
    See also m_hCaretPositionStart and m_selectionEnd
  */
  MathCell *m_selectionStart;
  /*! The last cell of the currently selected range of groupCells.
    
    NULL, when no GroupCells are selected and NULL, if only stuff inside a GroupCell
    is selected and therefore the selection is handled by EditorCell; This cell is 
    always below m_selectionStart.
    
    See also m_hCaretPositionEnd
  */

  //! The cell currently under the mouse pointer
  MathCell *m_cellUnderPointer;
  
  /*! The last cell of the currently selected range of Cells.
    
    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is 
    always above m_selectionEnd.
    
    See also m_hCaretPositionStart, m_hCaretPositionEnd and m_selectionStart.
  */
  MathCell *m_selectionEnd;
};

#endif
