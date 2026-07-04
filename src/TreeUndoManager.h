// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  Declares TreeUndoManager: the owner of the cell-tree undo/redo state.

  Second step of extracting the cell-tree undo/redo subsystem out of the
  Worksheet god class (the first was moving TreeUndoAction into its own
  header). The manager owns the two action stacks and the active-cell
  snapshot, and implements every operation that only touches that state.
  The operations that perform actual tree surgery (applying an undo action
  to the worksheet) remain in Worksheet, which accesses the stacks through
  UndoStack()/RedoStack().
*/

#ifndef TREEUNDOMANAGER_H
#define TREEUNDOMANAGER_H

#include "TreeUndoAction.h"

/*! Owns the cell-tree undo/redo stacks and the active-cell snapshot.

  Pure state + bookkeeping: this class never touches the worksheet's cell
  tree itself. Applying an action (the tree surgery) is Worksheet's job;
  it reaches the stacks via UndoStack()/RedoStack().

  The "active cell snapshot" is the text and selection an EditorCell had
  when the cursor entered it; when the cursor leaves the cell again, the
  snapshot is compared against the current text to decide whether a
  text-change undo action has to be recorded (see CellLeft()).
*/
class TreeUndoManager
{
public:
  //! The list of tree actions that can be undone.
  UndoActions &UndoStack() { return m_undoActions; }
  const UndoActions &UndoStack() const { return m_undoActions; }

  //! The list of undone tree actions that can be redone.
  UndoActions &RedoStack() { return m_redoActions; }
  const UndoActions &RedoStack() const { return m_redoActions; }

  /*! Remove one action from an action list.

    Removes the whole atomic group: trailing actions that are marked as
    m_partOfAtomicAction belong to the action in front of them and are
    dropped along with it.
  */
  static void DiscardAction(UndoActions *actionList)
    {
      if (actionList->empty())
        return;
      do {
        actionList->pop_back();
      } while (!actionList->empty() && actionList->back().m_partOfAtomicAction);
    }

  //! Mark the newest action of the list as part of the following atomic action.
  static void AppendAction(UndoActions *actionList)
    {
      if (!actionList->empty())
        actionList->front().m_partOfAtomicAction = true;
    }

  //! Mark the newest undo action as part of the following atomic action.
  void AppendAction() { AppendAction(&m_undoActions); }

  //! Clear the list of actions that can be redone.
  void ClearRedoActionList()
    {
      while (!m_redoActions.empty())
        DiscardAction(&m_redoActions);
    }

  //! Clear the list of actions that can be undone.
  void ClearUndoActionList()
    {
      while (!m_undoActions.empty())
        DiscardAction(&m_undoActions);
    }

  //! Clear both action lists and forget the active-cell snapshot.
  void ClearBuffers()
    {
      ClearRedoActionList();
      ClearUndoActionList();
      m_activeCell = nullptr;
    }

  /*! Drop actions from the back of the undo list until it is within the limit.

    \param undoLimit The maximum number of undo actions to keep; 0 = unlimited.
  */
  void LimitUndoBuffer(long undoLimit)
    {
      if (undoLimit == 0)
        return;
      while (static_cast<long>(m_undoActions.size()) > undoLimit)
        DiscardAction(&m_undoActions);
    }

  /*! The cursor has entered a cell: snapshot its contents.

    \param group    The group cell that was entered
    \param text     The text its editor currently contains
    \param selStart The editor's selection start (or cursor position)
    \param selEnd   The editor's selection end (or cursor position)
  */
  void CellEntered(GroupCell *group, const wxString &text,
                   long long selStart, long long selEnd)
    {
      m_activeCell = group;
      m_activeCellOldText = text;
      m_activeCellOldSelStart = selStart;
      m_activeCellOldSelEnd = selEnd;
    }

  /*! The cursor is leaving a cell: record a text-change undo action if needed.

    Compares the entry snapshot with the cell's current text; if the text has
    genuinely changed (more than just appending a ";"), an undo action with
    the old text and selection is pushed, the undo buffer is limited and the
    redo list is cleared.

    \param activeCell  The group cell being left
    \param currentText The text its editor contains now
    \param undoLimit   The undo-buffer limit to enforce (0 = unlimited)
    \returns true if a text-change action was recorded
  */
  bool CellLeft(GroupCell *activeCell, const wxString &currentText,
                long undoLimit)
    {
      if (m_activeCell) //-V1051
        wxASSERT_MSG(m_activeCell == activeCell,
                     _("Bug: Cell left but not entered."));

      // We only can undo a text change if the text has actually changed.
      if ((m_activeCellOldText.Length() > 1) &&
          (m_activeCellOldText != currentText) &&
          (m_activeCellOldText + wxS(";") != currentText)) {
        m_undoActions.emplace_front(activeCell, m_activeCellOldText,
                                    m_activeCellOldSelStart,
                                    m_activeCellOldSelEnd);
        LimitUndoBuffer(undoLimit);
        ClearRedoActionList();
        return true;
      }
      return false;
    }

  //! Forget the active-cell snapshot (e.g. the next change must not be undoable).
  void ForgetActiveCell() { m_activeCell = nullptr; }

  //! The cell the cursor is in, as recorded by CellEntered().
  GroupCell *ActiveCell() const { return m_activeCell; }

private:
  //! The list of tree actions that can be undone
  UndoActions m_undoActions;

  //! The list of undone tree actions that can be redone
  UndoActions m_redoActions;

  /*! The last cell the cursor entered.

    Needed for keeping track of cell content changes. A CellPtr so it
    auto-nulls if the cell is deleted while the cursor is still recorded
    as being inside it.
  */
  CellPtr<GroupCell> m_activeCell;

  //! The text ActiveCell() contained when the cursor entered it
  wxString m_activeCellOldText;

  //! The selection range in ActiveCell() when the cursor entered it (-1 = not recorded)
  long long m_activeCellOldSelStart = -1;
  long long m_activeCellOldSelEnd = -1;
};

#endif // TREEUNDOMANAGER_H
