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
 * Declares the various iterators useful with the Cell class.
 */

#ifndef CELLITERATORS_H
#define CELLITERATORS_H

#include <wx/debug.h>
#include <memory>
#include <type_traits>

template <typename Cell> class CellListIterator final {
  static_assert(std::is_class<Cell>::value, "The type argument must be a class");
  Cell *m_ptr = {};

public:
  constexpr CellListIterator() = default;
  constexpr explicit CellListIterator(const std::unique_ptr<Cell> &p)
    : m_ptr(p.get()) {}
  constexpr explicit CellListIterator(Cell *p) : m_ptr(p) {}
  constexpr CellListIterator(const CellListIterator &o) = default;
  constexpr CellListIterator &operator=(const CellListIterator &o) = default;
  constexpr CellListIterator operator++(int) {
    auto ret = *this;
    return operator++(), ret;
  }
  // constexpr fails if wxASSERT contains assembler code, which is true on MinGW
  CellListIterator &operator++() {
    if (m_ptr)
    {
      auto *const prev = m_ptr;
      m_ptr = m_ptr->GetNext();
      wxASSERT(prev != m_ptr);
    }
    return *this;
  }
  constexpr bool operator==(const CellListIterator &o) const {
    return m_ptr == o.m_ptr;
  }
  constexpr bool operator!=(const CellListIterator &o) const {
    return m_ptr != o.m_ptr;
  }
  constexpr operator bool() const { return m_ptr; }
  constexpr operator Cell *() const { return m_ptr; }
  constexpr Cell *operator->() const { return m_ptr; }
};

template <typename Cell> class CellListAdapter final {
  static_assert(std::is_class<Cell>::value, "The type argument must be a class");
  Cell *m_cell = {};
  using iterator = CellListIterator<Cell>;
  using const_iterator = CellListIterator<typename std::add_const<Cell>::type>;

public:
  explicit CellListAdapter(Cell *cell) : m_cell(cell) {}
  constexpr iterator begin() const { return iterator(m_cell); }
  constexpr iterator end() const { return {}; }
  constexpr const_iterator cbegin() const { return const_iterator(m_cell); }
  constexpr const_iterator cend() const { return {}; }
};

template <typename Cell>
class CellDrawListIterator final {
  static_assert(std::is_class<Cell>::value, "The type argument must be a class");
  Cell *m_ptr = {};

public:
  constexpr CellDrawListIterator() = default;
  constexpr explicit CellDrawListIterator(const std::unique_ptr<Cell> &p) : m_ptr(p.get()) {}
  constexpr explicit CellDrawListIterator(Cell *p) : m_ptr(p) {}
  constexpr CellDrawListIterator(const CellDrawListIterator &o) = default;
  constexpr CellDrawListIterator &operator=(const CellDrawListIterator &o) = default;
  constexpr CellDrawListIterator operator++(int) {
    auto ret = *this;
    return operator++(), ret;
  }
  // constexpr fails if wxASSERT contains assembler code, which is true on MinGW
  CellDrawListIterator &operator++()
    {
      if (m_ptr)
      {
        auto *const prev = m_ptr;
        m_ptr = m_ptr->GetNextToDraw();
        wxASSERT(prev != m_ptr);
      }
      return *this;
    }
  constexpr bool operator==(const CellDrawListIterator &o) const
    { return m_ptr == o.m_ptr; }
  constexpr bool operator!=(const CellDrawListIterator &o) const
    { return m_ptr != o.m_ptr; }
  constexpr operator bool() const { return m_ptr; }
  constexpr operator Cell*() const { return m_ptr; }
  constexpr Cell *operator->() const { return m_ptr; }
};

template <typename Cell> class CellDrawListAdapter final
{
  static_assert(std::is_class<Cell>::value, "The type argument must be a class");
  Cell *m_cell = {};
  using iterator = CellDrawListIterator<Cell>;
  using const_iterator = CellDrawListIterator<typename std::add_const<Cell>::type>;

public:
  explicit CellDrawListAdapter(Cell *cell) : m_cell(cell) {}
  constexpr iterator begin() const { return iterator(m_cell); }
  constexpr iterator end() const { return {}; }
  constexpr const_iterator cbegin() const { return const_iterator(m_cell); }
  constexpr const_iterator cend() const { return {}; }
};

class Cell;

//! Iterates the inner cells of a cell
class InnerCellIterator
{
  enum class Advance { Always, OnlyIfNull };
  const Cell *m_parentCell = {};
  Cell *m_innerCell = {};
  int16_t m_index = 0;
  int16_t m_endIndex = 0;

  static int GetInnerCellCount(const Cell *cell);
  static Cell *GetInnerCell(const Cell *cell, int index);
public:
  InnerCellIterator() = default;
  explicit InnerCellIterator(Cell *parentCell) :
    m_parentCell(parentCell),
    m_endIndex(parentCell ? GetInnerCellCount(parentCell) : 0)
    {
      FindFirstInnerCell();
    }
  InnerCellIterator(const InnerCellIterator &o) = default;
  InnerCellIterator &operator=(const InnerCellIterator &o) = default;
  InnerCellIterator operator++(int)
    {
      auto ret = *this;
      return operator++(), ret;
    }
  InnerCellIterator &operator++()
    {
      if (m_parentCell)
        AdvanceLoop(Advance::Always);
      return *this;
    }
  bool operator==(const InnerCellIterator &o) const
    { return m_innerCell == o.m_innerCell; }
  bool operator!=(const InnerCellIterator &o) const
    { return m_innerCell != o.m_innerCell; }
  operator bool() const { return m_innerCell; }
  operator Cell*() const { return m_innerCell; }
  Cell *operator->() const { return m_innerCell; }

private:
  void FindFirstInnerCell();
  void AdvanceLoop(Advance mode);
};

inline void InnerCellIterator::FindFirstInnerCell()
{
  if (m_endIndex)
  {
    m_innerCell = GetInnerCell(m_parentCell, 0);
    AdvanceLoop(Advance::OnlyIfNull);
  }
}

inline void InnerCellIterator::AdvanceLoop(Advance mode)
{
  Cell *prev = m_innerCell;
  if (mode == Advance::OnlyIfNull && prev)
    return;
  for (;;)
  {
    ++m_index;
    if (m_index == m_endIndex)
    {
      m_innerCell = nullptr;
      break;
    }
    m_innerCell = GetInnerCell(m_parentCell, m_index);
    wxASSERT(!prev || prev != m_innerCell);
    if (m_innerCell)
      break;
  }
}

class InnerCellAdapter final {
  using iterator = InnerCellIterator;
  iterator const m_iter;

public:
  explicit InnerCellAdapter(const iterator &) = delete;
  explicit InnerCellAdapter(Cell *cell) : m_iter(cell) {}
  iterator begin() const { return m_iter; }
  iterator end() const { return {}; }
};

#endif
