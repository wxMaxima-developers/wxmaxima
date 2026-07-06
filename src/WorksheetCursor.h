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
  The worksheet's "between the cells" cursor (the horizontally drawn caret).

  The worksheet has two cursors: this one, which sits between two group cells
  (or above the first one), and the text cursor inside an active EditorCell
  (tracked as CellPointers::m_activeCell, which the EditorCell itself
  maintains). At most one of the two is active at a time - and while whole
  cells are selected, neither is. That exclusivity is currently enforced at
  the Worksheet::SetHCaret / Worksheet::SetActiveCell seam, not here: cells
  and the autocompletion popup re-activate an editor directly, so this class
  cannot own the invariant yet.

  Note Worksheet::GetHCaret() is more than Position(): it is a derived
  "where would inserted cells go" anchor that falls back to the active cell's
  group, the selection, and finally the last cell (pinned by
  test_WorksheetCursor).
*/

#ifndef WORKSHEETCURSOR_H
#define WORKSHEETCURSOR_H

#include "cells/CellPtr.h"

class GroupCell;

//! The between-the-cells cursor: active flag, position, selection anchors.
class WorksheetCursor
{
public:
  //! Is this (rather than the in-cell text cursor) the active cursor?
  bool IsActive() const { return m_active; }
  //! Make this the active cursor. The caller deactivates the editor cursor.
  void Activate() { m_active = true; }
  //! Deactivate; the position is kept for a later re-activation.
  void Deactivate() { m_active = false; }

  /*! The group cell the cursor sits below; null = above the first cell.

    Returned as a reference so call sites can also assign and compare;
    auto-nulls when the cell dies (CellPtr).
  */
  CellPtr<GroupCell> &Position() { return m_position; }
  const CellPtr<GroupCell> &Position() const { return m_position; }

  /*! Where a select-with-the-caret gesture started.

    This is where the selection was begun, so it need not be above
    SelectionEnd() in the worksheet.
  */
  CellPtr<GroupCell> &SelectionStart() { return m_selectionStart; }
  //! Where a select-with-the-caret gesture currently ends.
  CellPtr<GroupCell> &SelectionEnd() { return m_selectionEnd; }

private:
  bool m_active = true;
  CellPtr<GroupCell> m_position;
  CellPtr<GroupCell> m_selectionStart;
  CellPtr<GroupCell> m_selectionEnd;
};

#endif // WORKSHEETCURSOR_H
