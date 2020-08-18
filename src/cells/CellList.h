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
  Cell *m_tail = {};
  Cell *m_lastAppended = {};

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

  //! Passes on the ownership of the list head
  operator std::unique_ptr<T>() && { return TakeHead(); }

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
      m_tail = {};
      m_lastAppended = {};
    } else
      m_head.reset();

    wxASSERT(!m_head && !m_tail && !m_lastAppended);
    return ptr;
  }

  //! Passes on the ownership of the list head.
  std::unique_ptr<T> TakeHead()
  {
    m_tail = {};
    m_lastAppended = {};
    auto retval = dynamic_unique_ptr_cast<T>(std::move(m_head));

    wxASSERT(!m_head && !m_tail && !m_lastAppended);
    return retval;
  }

  //! Provides the last cell in the list (if any).
  T *GetTail() const { return dynamic_cast<T*>(m_tail); }

  //! Provides the most cell passed to the most recent `Append` call.
  T *GetLastAppended() const { return dynamic_cast<T*>(m_lastAppended); }

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

class CellList
{
public:

  //! Checks the integrity of the list pointers of the given cell in relation to
  //! its neighbors.
  static void Check(const Cell *cell);
  static void Check(const GroupCell *cell);

  /*! Replaces the successor of a given cell, and returns the old one (if any).
   *
   * \param cell is the cell to modify.
   * \param next is the replacement succssor. It can be a list, or a single cell,
   * or null. Null means that the cell won't have a successor.
   */
  static std::unique_ptr<Cell> SetNext(Cell *cell, std::unique_ptr<Cell> &&next);

  /*! Deletes the list of cells anchored at the given cell.
   *
   * \param afterMe is the cell whose m_next will become null.
   */
  static void DeleteList(Cell *afterMe);

  /*! Appends a cell to the end of the cell list that starts with a given cell.
   *
   * \param cell is the cell list to append to.
   * \param tail is the cell to append. It can be a list, a single cell, or null.
   */
  static void AppendCell(Cell *cell, std::unique_ptr<Cell> &&tail);

  template <typename T>
  static void AppendCell(const std::unique_ptr<T> &cell, std::unique_ptr<Cell> &&tail)
  { AppendCell(cell.get(), std::move(tail)); }

  struct SplicedIn
  {
    //! The last cell in the in the list of spliced-in cells - copied from
    //! the `last` parameter, or computed if not provided.
    Cell *lastSpliced;
  };

  /*! Splices a given list of cells after the given cell.
   *
   * \param where is the cell after which the head will be spliced in. Cannot be null.
   * \param head is the head cell in the list of cells. It can be a single cell.
   * No changes are made when head is null.
   * \param last is the optional hint for the last cell in the spliced-in list.
   * It's computed if not provided.
   */
  static SplicedIn SpliceInAfter(Cell *where, std::unique_ptr<Cell> &&head, Cell *last = nullptr);

  struct TornOut {
    //! The first in the torn-out list of cells, or null if the tearing out had failed.
    CellPtr<Cell> cell;
    //! The owner of the torn-out cell list, or null if the cell had no predecessor in the
    //! list.
    std::unique_ptr<Cell> cellOwner;
    //! The owner of the tail beyond the last cell, or null if the cell did have
    //! a predecessor in the list.
    std::unique_ptr<Cell> tailOwner;
  };

  /*! Tears out a cell range and returns the list thus formed.
   *
   * \param first is the first cell to be torn out. Cannot be null.
   * \param last is the last cell to be torn out. Cannot be null.
   *
   * \returns The pair of owning pointers to the cell itself, and to the tail.
   */
  static TornOut TearOut(Cell *first, Cell *last);
};

#endif
