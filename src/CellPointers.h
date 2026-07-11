// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef WXMAXIMA_CELLPOINTERS_H
#define WXMAXIMA_CELLPOINTERS_H

#include "cells/Cell.h"
#include <wx/string.h>
#include <vector>

class wxWindow;
template<class T> class wxScrolled;
typedef wxScrolled<wxWindow> wxScrolledCanvas;

class EditorCell;
class TextCell;

/*! The storage for pointers to cells.

  If a cell is deleted it is necessary to remove all pointers that might
  allow to access the now-defunct cell. These pointers are kept in this
  per-worksheet structure.
*/
class CellPointers
{
  // The members below are being untangled into two conceptual groups as part of
  // the WorksheetDocument / view split (see the "Document-side" and "View /
  // interaction-side" banners further down). Document-side pointers describe the
  // document model (what is selected, which cell is active/answering, what maxima
  // is working on, which cells hold errors). View/interaction-side pointers
  // describe transient window state (hover, drag/keyboard-selection anchors,
  // scroll targets, animation timers). Both halves must remain reachable through
  // the Configuration -> CellPointers registry because cells read and write them
  // via Cell::GetCellPointers(); the split is therefore an *internal* reorg, not
  // a relocation of these members onto another owner. Members are moved behind
  // accessors one self-contained concern at a time; until then most stay public.
public:
  explicit CellPointers(wxScrolledCanvas *worksheet);

  //! A list of editor cells containing error messages.
  class ErrorList
  {
  public:
    ErrorList() = default;
    //! Is the list of errors empty?
    bool Empty() const { return m_errors.empty(); }
    //! Remove one specific GroupCell from the list of errors
    void Remove(GroupCell * cell);
    //! Does the list of GroupCell with errors contain cell?
    bool Contains(GroupCell * cell) const;
    //! Mark this GroupCell as containing errors
    void Add(GroupCell * cell);
    //! The first GroupCell with error that is still in the list
    GroupCell *FirstError() const;
    //! The last GroupCell with errors in the list
    GroupCell *LastError() const;
    //! Empty the list of GroupCells with errors
    void Clear() { m_errors.clear(); }
  private:
    //! A list of GroupCells that contain errors
    std::vector<CellPtr<GroupCell>> m_errors;
  };

  // ======================================================================
  //  Document-side pointers (the document model)
  // ======================================================================

  //! Returns the cell maxima currently works on. NULL if there isn't such a cell.
  /*!
    \param resortToLast true = if we already have set the cell maxima works on to NULL
    use the last cell maxima was known to work on.
  */
  GroupCell *GetWorkingGroup(bool resortToLast = false) const;

  //! Sets the cell maxima currently works on. NULL if there isn't such a cell.
  void SetWorkingGroup(GroupCell *group);

  //! The last group cell maxima was working on (regardless of the current one).
  GroupCell *GetLastWorkingGroup() const { return m_lastWorkingGroup; }

  //! Are any whole cells (as opposed to text inside an editor) selected?
  bool HasCellsSelected() const { return m_selectionStart && m_selectionEnd; }

  // GetAnswerCell/SetAnswerCell are defined out-of-line: converting the CellPtr
  // to (or assigning a raw pointer from) EditorCell* needs the complete
  // EditorCell type, which this header only forward-declares.
  //! The editor cell maxima's current question is being answered in, or null.
  EditorCell *GetAnswerCell() const;
  //! Set the editor cell maxima's current question is answered in.
  void SetAnswerCell(EditorCell *cell);
  //! Forget the editor cell that held maxima's current question.
  void ClearAnswerCell() { m_answerCell = {}; }
  //! Forget the question editor if it belongs to group, e.g. because that
  //! group's output is being reset or the group is going away.
  void ClearAnswerCellIfInGroup(GroupCell *group);

  //! The list of cells maxima has complained about errors in
  ErrorList m_errorList;
  //! Which EditCell the blinking cursor is in?
  CellPtr<EditorCell> m_activeCell;
  //! The textcell the text maxima is sending us was ending in.
  CellPtr<TextCell> m_currentTextCell;
  /*! The first cell of the currently selected range of Cells.

    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is
    always above m_selectionEnd.

    See also m_hCaretPositionStart and m_selectionEnd
  */
  CellPtr<Cell> m_selectionStart;
  /*! The last cell of the currently selected range of Cells.

    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is
    always above m_selectionStart.

    See also m_hCaretPositionStart, m_hCaretPositionEnd and m_selectionStart.
  */
  CellPtr<Cell> m_selectionEnd;
  /*! The currently selected string.

    Since this string is defined here it is available in every editor cell
    for highlighting other instances of the selected string.
  */
  wxString m_selectionString;

  // ======================================================================
  //  View / interaction-side pointers (transient window state)
  // ======================================================================

  void ScrollToCell(Cell *cell) { m_cellToScrollTo = cell; }
  Cell *CellToScrollTo() { return m_cellToScrollTo; }

  //! Forget where the search was started
  void ResetSearchStart()
    {
      m_cellSearchStartedIn = {};
      m_indexSearchStartedAt = -1;
    }

  //! Forget where the mouse selection was started
  void ResetMouseSelectionStart()
    { m_cellMouseSelectionStartedIn = {}; }

  //! Forget where the keyboard selection was started
  void ResetKeyboardSelectionStart()
    { m_cellKeyboardSelectionStartedIn = {}; }

  void SetTimerIdForCell(Cell *cell, int timerId);
  int GetTimerIdForCell(Cell *cell) const;
  Cell *GetCellForTimerId(int timerId) const;
  void RemoveTimerIdForCell(const Cell *const cell);

  //! The EditorCell the mouse selection has started in
  CellPtr<EditorCell> m_cellMouseSelectionStartedIn;
  //! The EditorCell the keyboard selection has started in
  CellPtr<EditorCell> m_cellKeyboardSelectionStartedIn;
  //! The EditorCell the search was started in
  CellPtr<EditorCell> m_cellSearchStartedIn;
  //! Which cursor position incremental search has started at?
  int m_indexSearchStartedAt = -1;
  //! The GroupCell that is under the mouse pointer
  CellPtr<GroupCell> m_groupCellUnderPointer;
  //! The cell currently under the mouse pointer
  CellPtr<Cell> m_cellUnderPointer;
  //! Is scrolling to a cell scheduled?
  bool m_scrollToCell = false;

  // ======================================================================
  //  Serialization helpers (.wxmx image numbering)
  // ======================================================================

  void WXMXResetCounter() { m_wxmxImgCounter = 0; }
  wxString WXMXGetNewFileName();
  std::size_t WXMXImageCount() const { return m_wxmxImgCounter; }

  wxScrolledCanvas *GetWorksheet() { return m_worksheet; }

private:
  // ---- Document-side (encapsulated) ----
  /*! The group cell maxima is currently working on.

    NULL means that maxima isn't currently evaluating a cell.
  */
  CellPtr<GroupCell> m_workingGroup;
  //! The last group cell maxima was working on.
  CellPtr<GroupCell> m_lastWorkingGroup;
  //! The EditorCell that contains the currently active question from maxima
  CellPtr<EditorCell> m_answerCell;

  // ---- View-side (encapsulated) ----
  struct CellTimerId {
    // A CellPtr, not a raw pointer: animation timers fire asynchronously, so a
    // timer entry can outlive its cell. Auto-nulling means GetCellForTimerId()
    // returns nullptr for a destroyed cell instead of a dangling pointer, even
    // if RemoveTimerIdForCell() was somehow not reached (see the "Long-lived
    // cell references" rule in CellPtr.h).
    CellPtr<Cell> cell;
    int timerId = -1;
    CellTimerId() = default;
    CellTimerId(Cell *cell, int timerId) : cell(cell), timerId(timerId) {}
  };
  //! Timer ids for animation cells
  std::vector<CellTimerId> m_timerIds;
  //! If m_scrollToCell = true: Which cell do we need to scroll to?
  CellPtr<Cell> m_cellToScrollTo;
  //! The object of the function to call if an animation has to be stepped.
  wxScrolledCanvas *const m_worksheet;
  //! The image counter for saving .wxmx files
  std::size_t m_wxmxImgCounter = 0;
};

#endif
