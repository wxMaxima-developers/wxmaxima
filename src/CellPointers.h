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

/*! The document-model half of the cell-pointer registry.

  These pointers describe the edited document itself: what is selected, which
  editor is active or answering maxima's question, which text cell maxima's
  incoming output is appended to, which group cell maxima is working on, and
  which group cells hold errors. None of them depend on how the document is
  displayed, so this half will eventually be owned by WorksheetDocument.

  Every stored pointer is a CellPtr, which auto-nulls when its cell is
  destroyed, so a stale reference reads as null instead of dangling.
*/
class DocumentCellPointers
{
public:
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

  //! The first cell of the currently selected range of cells, or null. Returned
  //! by reference so callers keep the CellPtr conveniences (CastAs<>,
  //! auto-nulling) without a raw-pointer copy.
  const CellPtr<Cell> &GetSelectionStart() const { return m_selectionStart; }
  //! The last cell of the currently selected range of cells, or null.
  const CellPtr<Cell> &GetSelectionEnd() const { return m_selectionEnd; }
  //! Set the first cell of the selected range (may be null).
  void SetSelectionStart(Cell *cell) { m_selectionStart = cell; }
  //! Set the last cell of the selected range (may be null).
  void SetSelectionEnd(Cell *cell) { m_selectionEnd = cell; }

  // The out-of-line definitions convert a CellPtr to (or a raw pointer from)
  // EditorCell*/TextCell*, which needs the complete type this header only
  // forward-declares.
  //! The editor cell maxima's current question is being answered in, or null.
  EditorCell *GetAnswerCell() const;
  //! Set the editor cell maxima's current question is answered in.
  void SetAnswerCell(EditorCell *cell);
  //! Forget the editor cell that held maxima's current question.
  void ClearAnswerCell() { m_answerCell = {}; }
  //! Forget the question editor if it belongs to group, e.g. because that
  //! group's output is being reset or the group is going away.
  void ClearAnswerCellIfInGroup(GroupCell *group);

  //! The text cell the text maxima is currently sending us is being appended to.
  TextCell *GetCurrentTextCell() const;
  //! Set the text cell maxima's incoming text is being appended to (may be null).
  void SetCurrentTextCell(TextCell *cell);

  //! The editor cell the blinking text cursor is in, or null.
  EditorCell *GetActiveCell() const;
  //! Set the editor cell the blinking text cursor is in (may be null).
  void SetActiveCell(EditorCell *cell);
  //! Forget the editor cell the text cursor was in.
  void ClearActiveCell() { m_activeCell = {}; }

  //! The currently selected string, or empty.
  const wxString &GetSelectionString() const { return m_selectionString; }
  //! Set the currently selected string.
  void SetSelectionString(const wxString &str) { m_selectionString = str; }
  //! Forget the currently selected string.
  void ClearSelectionString() { m_selectionString.Clear(); }

  //! The list of cells maxima has complained about errors in.
  ErrorList &GetErrorList() { return m_errorList; }
  //! The list of cells maxima has complained about errors in (read-only view).
  const ErrorList &GetErrorList() const { return m_errorList; }

private:
  /*! The group cell maxima is currently working on.

    NULL means that maxima isn't currently evaluating a cell.
  */
  CellPtr<GroupCell> m_workingGroup;
  //! The last group cell maxima was working on.
  CellPtr<GroupCell> m_lastWorkingGroup;
  //! The EditorCell that contains the currently active question from maxima
  CellPtr<EditorCell> m_answerCell;
  //! The textcell the text maxima is sending us was ending in.
  CellPtr<TextCell> m_currentTextCell;
  //! Which EditCell the blinking cursor is in?
  CellPtr<EditorCell> m_activeCell;
  /*! The first cell of the currently selected range of Cells.

    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is
    always above m_selectionEnd.
  */
  CellPtr<Cell> m_selectionStart;
  /*! The last cell of the currently selected range of Cells.

    NULL, when no Cells are selected and NULL, if only stuff inside a EditorCell
    is selected and therefore the selection is handled by EditorCell; This cell is
    always below m_selectionStart.
  */
  CellPtr<Cell> m_selectionEnd;
  /*! The currently selected string.

    Since this string is reachable from every editor cell (via the accessors
    above) it is available for highlighting other instances of the selection.
  */
  wxString m_selectionString;
  //! The list of cells maxima has complained about errors in.
  ErrorList m_errorList;
};

/*! The view/interaction half of the cell-pointer registry.

  These pointers describe transient window state that has nothing to do with the
  document model: which cell/group cell is under the mouse pointer, where a
  mouse/keyboard/incremental-search selection was started, which cell we still
  need to scroll to, and the per-cell animation-timer ids. This half will
  eventually be owned by the Worksheet window.

  Every stored cell pointer is a CellPtr, so it auto-nulls when its cell dies.
*/
class ViewCellPointers
{
public:
  explicit ViewCellPointers(wxScrolledCanvas *worksheet) : m_worksheet(worksheet) {}

  void ScrollToCell(Cell *cell) { m_cellToScrollTo = cell; }
  Cell *CellToScrollTo() { return m_cellToScrollTo; }

  //! Is a scroll to CellToScrollTo() currently scheduled?
  bool ScrollToCellScheduled() const { return m_scrollToCell; }
  //! Schedule (or cancel) scrolling to CellToScrollTo().
  void SetScrollToCellScheduled(bool scheduled) { m_scrollToCell = scheduled; }

  //! The EditorCell an incremental search was started in, or null.
  const CellPtr<EditorCell> &SearchStart() const { return m_cellSearchStartedIn; }
  //! The cursor position an incremental search was started at (-1 = none).
  int IndexSearchStartedAt() const { return m_indexSearchStartedAt; }
  //! Record where an incremental search started (its cell and cursor index).
  //! Out-of-line: assigning the raw EditorCell* needs the complete type.
  void SetSearchStart(EditorCell *cell, int index);
  //! Forget where the search was started
  void ResetSearchStart()
    {
      m_cellSearchStartedIn = {};
      m_indexSearchStartedAt = -1;
    }

  //! The EditorCell a mouse selection was started in, or null.
  const CellPtr<EditorCell> &MouseSelectionStart() const
    { return m_cellMouseSelectionStartedIn; }
  //! Record the EditorCell a mouse selection started in (out-of-line: see above).
  void SetMouseSelectionStart(EditorCell *cell);
  //! Forget where the mouse selection was started
  void ResetMouseSelectionStart()
    { m_cellMouseSelectionStartedIn = {}; }

  //! The EditorCell a keyboard selection was started in, or null.
  const CellPtr<EditorCell> &KeyboardSelectionStart() const
    { return m_cellKeyboardSelectionStartedIn; }
  //! Record the EditorCell a keyboard selection started in (out-of-line: see above).
  void SetKeyboardSelectionStart(EditorCell *cell);
  //! Forget where the keyboard selection was started
  void ResetKeyboardSelectionStart()
    { m_cellKeyboardSelectionStartedIn = {}; }

  void SetTimerIdForCell(Cell *cell, int timerId);
  int GetTimerIdForCell(Cell *cell) const;
  Cell *GetCellForTimerId(int timerId) const;
  void RemoveTimerIdForCell(const Cell *const cell);

  //! The GroupCell currently under the mouse pointer, or null.
  GroupCell *GetGroupCellUnderPointer() const;
  //! Set the GroupCell under the mouse pointer (may be null).
  void SetGroupCellUnderPointer(GroupCell *cell);
  //! The cell currently under the mouse pointer, or null.
  Cell *GetCellUnderPointer() const { return m_cellUnderPointer; }
  //! Set the cell under the mouse pointer (may be null).
  void SetCellUnderPointer(Cell *cell) { m_cellUnderPointer = cell; }
  //! Forget which cell was under the mouse pointer.
  void ClearCellUnderPointer() { m_cellUnderPointer = {}; }

  void WXMXResetCounter() { m_wxmxImgCounter = 0; }
  wxString WXMXGetNewFileName();
  std::size_t WXMXImageCount() const { return m_wxmxImgCounter; }

  wxScrolledCanvas *GetWorksheet() { return m_worksheet; }

private:
  //! The GroupCell that is under the mouse pointer
  CellPtr<GroupCell> m_groupCellUnderPointer;
  //! The cell currently under the mouse pointer
  CellPtr<Cell> m_cellUnderPointer;
  //! The EditorCell the mouse selection has started in
  CellPtr<EditorCell> m_cellMouseSelectionStartedIn;
  //! The EditorCell the keyboard selection has started in
  CellPtr<EditorCell> m_cellKeyboardSelectionStartedIn;
  //! The EditorCell the search was started in
  CellPtr<EditorCell> m_cellSearchStartedIn;
  //! Which cursor position incremental search has started at?
  int m_indexSearchStartedAt = -1;
  //! Is scrolling to a cell scheduled?
  bool m_scrollToCell = false;
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

/*! The storage for pointers to cells.

  If a cell is deleted it is necessary to remove all pointers that might allow to
  access the now-defunct cell. These pointers are kept in this per-worksheet
  structure.

  As part of the WorksheetDocument / view split the pointers have been separated
  into a document-model half (DocumentCellPointers) and a transient view-state
  half (ViewCellPointers). CellPointers now holds one of each and forwards its
  historical accessors to the appropriate half; call sites will be routed to the
  two halves directly, and the halves moved onto their real owners, in following
  increments. Both halves must stay reachable through the Configuration registry
  because cells read and write them via Cell::GetCellPointers().
*/
class CellPointers
{
public:
  explicit CellPointers(wxScrolledCanvas *worksheet) : m_view(worksheet) {}

  //! The document-model half of the pointers.
  DocumentCellPointers &Document() { return m_document; }
  const DocumentCellPointers &Document() const { return m_document; }
  //! The transient view-state half of the pointers.
  ViewCellPointers &View() { return m_view; }
  const ViewCellPointers &View() const { return m_view; }

  //! Kept so existing "CellPointers::ErrorList" references keep compiling.
  using ErrorList = DocumentCellPointers::ErrorList;

  // ---- Document-side forwarders ----
  GroupCell *GetWorkingGroup(bool resortToLast = false) const
    { return m_document.GetWorkingGroup(resortToLast); }
  void SetWorkingGroup(GroupCell *group) { m_document.SetWorkingGroup(group); }
  GroupCell *GetLastWorkingGroup() const { return m_document.GetLastWorkingGroup(); }
  bool HasCellsSelected() const { return m_document.HasCellsSelected(); }
  const CellPtr<Cell> &GetSelectionStart() const { return m_document.GetSelectionStart(); }
  const CellPtr<Cell> &GetSelectionEnd() const { return m_document.GetSelectionEnd(); }
  void SetSelectionStart(Cell *cell) { m_document.SetSelectionStart(cell); }
  void SetSelectionEnd(Cell *cell) { m_document.SetSelectionEnd(cell); }
  EditorCell *GetAnswerCell() const { return m_document.GetAnswerCell(); }
  void SetAnswerCell(EditorCell *cell) { m_document.SetAnswerCell(cell); }
  void ClearAnswerCell() { m_document.ClearAnswerCell(); }
  void ClearAnswerCellIfInGroup(GroupCell *group)
    { m_document.ClearAnswerCellIfInGroup(group); }
  TextCell *GetCurrentTextCell() const { return m_document.GetCurrentTextCell(); }
  void SetCurrentTextCell(TextCell *cell) { m_document.SetCurrentTextCell(cell); }
  EditorCell *GetActiveCell() const { return m_document.GetActiveCell(); }
  void SetActiveCell(EditorCell *cell) { m_document.SetActiveCell(cell); }
  void ClearActiveCell() { m_document.ClearActiveCell(); }
  const wxString &GetSelectionString() const { return m_document.GetSelectionString(); }
  void SetSelectionString(const wxString &str) { m_document.SetSelectionString(str); }
  void ClearSelectionString() { m_document.ClearSelectionString(); }
  ErrorList &GetErrorList() { return m_document.GetErrorList(); }
  const ErrorList &GetErrorList() const { return m_document.GetErrorList(); }

  // ---- View-side forwarders ----
  void ScrollToCell(Cell *cell) { m_view.ScrollToCell(cell); }
  Cell *CellToScrollTo() { return m_view.CellToScrollTo(); }
  bool ScrollToCellScheduled() const { return m_view.ScrollToCellScheduled(); }
  void SetScrollToCellScheduled(bool scheduled)
    { m_view.SetScrollToCellScheduled(scheduled); }
  const CellPtr<EditorCell> &SearchStart() const { return m_view.SearchStart(); }
  int IndexSearchStartedAt() const { return m_view.IndexSearchStartedAt(); }
  void SetSearchStart(EditorCell *cell, int index)
    { m_view.SetSearchStart(cell, index); }
  void ResetSearchStart() { m_view.ResetSearchStart(); }
  const CellPtr<EditorCell> &MouseSelectionStart() const
    { return m_view.MouseSelectionStart(); }
  void SetMouseSelectionStart(EditorCell *cell)
    { m_view.SetMouseSelectionStart(cell); }
  void ResetMouseSelectionStart() { m_view.ResetMouseSelectionStart(); }
  const CellPtr<EditorCell> &KeyboardSelectionStart() const
    { return m_view.KeyboardSelectionStart(); }
  void SetKeyboardSelectionStart(EditorCell *cell)
    { m_view.SetKeyboardSelectionStart(cell); }
  void ResetKeyboardSelectionStart() { m_view.ResetKeyboardSelectionStart(); }
  void SetTimerIdForCell(Cell *cell, int timerId)
    { m_view.SetTimerIdForCell(cell, timerId); }
  int GetTimerIdForCell(Cell *cell) const { return m_view.GetTimerIdForCell(cell); }
  Cell *GetCellForTimerId(int timerId) const { return m_view.GetCellForTimerId(timerId); }
  void RemoveTimerIdForCell(const Cell *const cell)
    { m_view.RemoveTimerIdForCell(cell); }
  GroupCell *GetGroupCellUnderPointer() const
    { return m_view.GetGroupCellUnderPointer(); }
  void SetGroupCellUnderPointer(GroupCell *cell)
    { m_view.SetGroupCellUnderPointer(cell); }
  Cell *GetCellUnderPointer() const { return m_view.GetCellUnderPointer(); }
  void SetCellUnderPointer(Cell *cell) { m_view.SetCellUnderPointer(cell); }
  void ClearCellUnderPointer() { m_view.ClearCellUnderPointer(); }
  void WXMXResetCounter() { m_view.WXMXResetCounter(); }
  wxString WXMXGetNewFileName() { return m_view.WXMXGetNewFileName(); }
  std::size_t WXMXImageCount() const { return m_view.WXMXImageCount(); }
  wxScrolledCanvas *GetWorksheet() { return m_view.GetWorksheet(); }

private:
  DocumentCellPointers m_document;
  ViewCellPointers m_view;
};

#endif
