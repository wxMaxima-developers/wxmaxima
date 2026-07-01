// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Declares TreeUndoAction, the immutable record of one cell-tree undo/redo step,
  and the UndoActions list type.

  Extracted from Worksheet as the first step of pulling the cell-tree undo/redo
  subsystem out of the Worksheet god class into a TreeUndoManager.
*/

#ifndef TREEUNDOACTION_H
#define TREEUNDOACTION_H

#include "cells/CellPtr.h"
#include "cells/GroupCell.h"
#include <wx/string.h>
#include <list>
#include <memory>

/*! The description of one action for the undo (or redo) command.
  This object is immutable - the undo/redo buffer cannot be modified.
*/
class TreeUndoAction
{
public:
  TreeUndoAction(GroupCell *start, const wxString &oldText,
                 long long oldSelStart = -1, long long oldSelEnd = -1) :
    m_start(start), m_oldText(oldText),
    m_oldSelStart(oldSelStart), m_oldSelEnd(oldSelEnd)
    {
      wxASSERT_MSG(start, _("Bug: Trying to record a cell contents change for undo without a cell."));
    }
  TreeUndoAction(GroupCell *start, GroupCell *end) :
    m_start(start), m_newCellsEnd(end)
    {
      wxASSERT_MSG(start, _("Bug: Trying to record a cell contents change for undo without a cell."));
    }
  TreeUndoAction(GroupCell *start, GroupCell *end, GroupCell *oldCells) :
    m_start(start), m_newCellsEnd(end), m_oldCells(oldCells)
    {
    }

  /*! True = This undo action is only part of an atomic undo action.

    This is the only mutable part of this action: is is used to indicate its relation to
    other actions in the undo list.
  */
  bool m_partOfAtomicAction = false;

  /*! The position this action started at.

    NULL = At the begin of the document.

    A CellPtr (not a raw pointer) so that it auto-nulls if the GroupCell it
    refers to is destroyed while this action still sits in the undo/redo list -
    otherwise invoking the action would dereference freed memory. The consumer
    code already null-checks this field, so auto-nulling makes those checks
    correct instead of relying on the freed pointer's stale value.
  */
  CellPtr<GroupCell> m_start;

  /*! The old contents of the cell start

    if this field != wxEmptyString this field contains the old contents of the text
    cell pointed to by the field start.
  */
  const wxString m_oldText;

  /*! The old cursor/selection range in the EditorCell pointed to by m_start.

    -1 means "not recorded". Both fields store the selection endpoints; when
    they are equal the value represents a plain cursor position (no selection).
    The convention matches EditorCell::SelectionStart()/SelectionEnd().
  */
  const long long m_oldSelStart = -1;
  const long long m_oldSelEnd   = -1;

  /*! This action inserted all cells from start to newCellsEnd.

    To undo it these cells have to be deleted again.

    If this field's value is NULL no cells have to be deleted to undo this action.

    A CellPtr for the same reason as m_start: it auto-nulls if the referenced
    GroupCell is destroyed before this action is undone.
  */
  CellPtr<GroupCell> m_newCellsEnd;

  /*! Cells that were deleted in this action.

    This field will have to contain the cells themselves, not a copy of them because
    the latter might break consecutive undos.

    If this field's value is NULL no cells have to be added to undo this action.
  */
  std::unique_ptr<GroupCell> m_oldCells;
};

//! The type of the list of tree actions that can be undone
using UndoActions = std::list<TreeUndoAction>;

#endif // TREEUNDOACTION_H
