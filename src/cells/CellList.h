// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
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

/*! \file
 *
 * Declares the cell list management functionality.
 *
 * The eventual plan is to have a list of cells be a dedicated lightweight
 * class working together with an arena allocator.
 */

#ifndef WXMAXIMA_CELLLIST_H
#define WXMAXIMA_CELLLIST_H

#include "CellPtr.h"
#include <memory>

class CellListBuilderBase
{
protected:
  std::unique_ptr<Cell> m_head;
  CellPtr<Cell> m_tail;
  CellPtr<Cell> m_lastAppended;

  //! Appends one or more cells
  void base_Append(std::unique_ptr<Cell> &&cells);

  //! Appends one or more cells if they are of the correct type, otherwise
  //! deletes them \returns the appended cells, or null if they weren't
  //! appended.
  Cell *base_DynamicAppend(std::unique_ptr<Cell> &&cells,
                           Cell *(*caster)(Cell *));

  //! Clears the pointer to the last appended cell. Useful when tree building.
  void ClearLastAppended() { m_lastAppended = {}; }
};

//! Manages building a list of cells, keeping the head and tail of the list.
template <typename T = Cell> class CellListBuilder : CellListBuilderBase
{
  static Cell *DynamicCast(Cell *cell) { return dynamic_cast<T *>(cell); }

public:
  using CellListBuilderBase::ClearLastAppended;

  //! Returns true if the tree is non-empty
  explicit operator bool() const { return bool(m_head); }

  /*! Releases the ownership of the list head of the list to the caller.
   *
   * \todo
   * This function is deprecated. It is provided temporarily to support porting
   * to unique_ptr-based Cell ownership. It should be removed after the porting
   * is done.
   */
  T *ReleaseHead()
  {
    T *ptr = dynamic_cast<T *>(m_head.get());
    if (ptr) {
      m_head.release();
      m_tail.reset();
      m_lastAppended.reset();
    } else
      m_head.reset();

    wxASSERT(!m_head && !m_tail && !m_lastAppended);
    return ptr;
  }

  //! Passes on the ownership of the list head.
  std::unique_ptr<T> TakeHead()
  {
    m_tail.reset();
    m_lastAppended.reset();
    auto retval = dynamic_unique_ptr_cast<T>(std::move(m_head));

    wxASSERT(!m_head && !m_tail && !m_lastAppended);
    return retval;
  }

  //! Provides the last cell in the list (if any).
  T *GetTail() const { return m_tail.CastAs<T *>(); }

  //! Provides the most cell passed to the most recent `Append` call.
  T *GetLastAppended() const { return m_lastAppended.CastAs<T *>(); }

  //! Appends one or more cells
  // cppcheck-suppress deallocret
  T *Append(T *cells)
  {
    base_Append(std::unique_ptr<Cell>(cells));
    return cells;
  }

  //! Appends one or more cells if they are all of the correct type, otherwise
  //! deletes them. \returns the appended cells, or null if they weren't
  //! appended.
  T *DynamicAppend(Cell *cells)
  {
    return static_cast<T *>(
        base_DynamicAppend(std::unique_ptr<Cell>(cells), DynamicCast));
  }

  //! Appends one or more cells if they are all of the correct type, otherwise
  //! deletes them. \returns the appended cells, or null if they weren't
  //! appended.
  T *DynamicAppend(std::unique_ptr<Cell> &&cells)
  {
    return static_cast<T *>(base_DynamicAppend(std::move(cells), DynamicCast));
  }

  //! Appends one or more cells
  T *Append(std::unique_ptr<T> &&cells)
  {
    auto *const retval = cells.get();
    base_Append(std::move(cells));
    return retval;
  }
};

#endif
