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
  The document half of the worksheet: the state and commands that describe the
  edited document itself, independent of how it is displayed.

  WorksheetDocument is the growing home for the state the document/view split
  peels off Worksheet (see the split map): the cell tree, the undo history, the
  evaluation queue, the between-cells cursor and the small bits of bookkeeping
  gathered here first - the file the document was loaded from and the "current
  Maxima question" state. Worksheet holds one instance and forwards its
  document accessors to it; view-only state (scrolling, timers, the drawing
  context) stays on Worksheet.

  Only genuinely view-independent state belongs here. Notably m_saved is *not*
  here yet: its setter also flips Worksheet's m_updateControls (a view flag), so
  moving it needs a view callback and waits for a later increment.
*/

#ifndef WORKSHEETDOCUMENT_H
#define WORKSHEETDOCUMENT_H

#include "EvaluationQueue.h"
#include "TreeUndoManager.h"
#include "WorksheetCursor.h"
#include "cells/CellPtr.h"
#include "cells/GroupCell.h"
#include <memory>
#include <wx/string.h>

class WorksheetDocumentView;

/*! The view-independent state and commands of an edited worksheet document.

  Currently a small aggregate that Worksheet delegates to; it grows as further
  document state is migrated off Worksheet. Held by value by Worksheet.
*/
class WorksheetDocument {
public:
  //! The list of cells scheduled to be sent to Maxima for evaluation.
  EvaluationQueue &GetEvaluationQueue() { return m_evaluationQueue; }
  //! The list of cells scheduled for evaluation (read-only view).
  const EvaluationQueue &GetEvaluationQueue() const { return m_evaluationQueue; }

  //! The document's undo/redo history of tree-structure edits.
  TreeUndoManager &GetTreeUndo() { return m_treeUndo; }
  //! The document's tree-edit undo/redo history (read-only view).
  const TreeUndoManager &GetTreeUndo() const { return m_treeUndo; }

  //! The between-cells cursor (the h-caret) and its group-level selection.
  WorksheetCursor &GetCursor() { return m_hCaret; }
  //! The between-cells cursor (read-only view).
  const WorksheetCursor &GetCursor() const { return m_hCaret; }

  //! The first GroupCell of the document (null when the document is empty).
  GroupCell *GetTree() const { return m_tree.get(); }
  /*! Mutable access to the owning pointer of the cell tree.

    Transitional: while the tree-editing surgery still lives in Worksheet, it
    reseats the head of the list (moves a new tree in, resets it, splices), so
    it needs the unique_ptr itself, not just GetTree(). Moves onto
    WorksheetDocument with the tree-surgery methods in a later increment.
  */
  std::unique_ptr<GroupCell> &TreeOwner() { return m_tree; }
  /*! The cached pointer to the document's last GroupCell (<0 = unknown).

    Mutable so the const "find the last cell" lookup can memoise its result;
    the surgery code also updates it directly when it reseats the tail.
  */
  CellPtr<GroupCell> &LastCache() const { return m_last; }

  //! Register the view the document notifies when it edits its own structure.
  void SetDocumentView(WorksheetDocumentView *view) { m_view = view; }

  //! The document's last GroupCell, memoised in m_last (null if empty).
  GroupCell *GetLastCell() const;

  //! Renumber all sectioning cells (titles, sections, ...) from the top.
  void NumberSections() const;

  /*! Insert a list of group cells into the document after \p where.

    \p where == null inserts at the very top. Splices the cells into the tree,
    renumbers sections if needed and notifies the view (recalc/redraw/adjust
    size/modified) through the registered WorksheetDocumentView. Returns the
    last inserted cell. The caller (Worksheet) records the undo action, which
    needs a Configuration and so stays on the view side.
  */
  GroupCell *InsertCells(std::unique_ptr<GroupCell> &&cells, GroupCell *where);

  //! The file this document was last loaded from / saved to (empty if none).
  const wxString &GetCurrentFile() const { return m_currentFile; }
  //! Record the file this document is associated with.
  void SetCurrentFile(const wxString &file) { m_currentFile = file; }

  //! The text of the Maxima question the user is currently being asked.
  const wxString &GetLastQuestion() const { return m_lastQuestion; }
  //! Remember the text of the question Maxima is currently asking.
  void SetLastQuestion(const wxString &lastQuestion) {
    m_lastQuestion = lastQuestion;
  }

  //! Is Maxima currently waiting for an answer to a question?
  bool QuestionPending() const { return m_questionPrompt; }
  //! Record whether Maxima is waiting for an answer to a question.
  void SetQuestionPending(bool pending) { m_questionPrompt = pending; }

private:
  //! The cells scheduled to be sent to Maxima, in evaluation order.
  EvaluationQueue m_evaluationQueue;
  //! Undo/redo history of edits to the cell-tree structure.
  TreeUndoManager m_treeUndo;
  //! The between-cells cursor (h-caret): its position and group selection.
  WorksheetCursor m_hCaret;
  //! Owns the document's cell tree: the first GroupCell and, via its list, all
  //! the rest. Null when the document is empty.
  std::unique_ptr<GroupCell> m_tree;
  //! Cached pointer to the last GroupCell of the tree (a lookup memo).
  mutable CellPtr<GroupCell> m_last;
  //! The view notified when the document edits its own structure (not owned).
  WorksheetDocumentView *m_view = nullptr;
  //! The file the document was loaded from / saved to.
  wxString m_currentFile;
  //! The text of the question Maxima is currently asking, if any.
  wxString m_lastQuestion;
  //! True while Maxima is waiting for the answer to a question.
  bool m_questionPrompt = false;
};

#endif // WORKSHEETDOCUMENT_H
