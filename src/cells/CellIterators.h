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
  constexpr CellListIterator &operator++() {
    if (m_ptr)
    {
      auto *const prev = m_ptr;
      m_ptr = m_ptr->GetNext();
      bool getNextFails = (prev != m_ptr);
      wxASSERT(getNextFails);
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
  constexpr CellDrawListIterator &operator++()
  {
    if (m_ptr)
    {
      auto *const prev = m_ptr;
      m_ptr = m_ptr->GetNextToDraw();
      bool getNextFails = (prev != m_ptr);
      wxASSERT(getNextFails);
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

class InnerCellIterator
{
  enum class Uses : uint16_t { SmartPtr, RawPtr};
  using SmartPtr_ = const std::unique_ptr<Cell> *;
  using RawPtr_ = Cell* const*;
  void const* m_ptr = {};
  uint16_t m_count = 0;
  Uses m_uses = Uses::SmartPtr;

public:
  InnerCellIterator() = default;
  InnerCellIterator(const std::unique_ptr<Cell> *p, const std::unique_ptr<Cell> *last) :
    m_ptr((p != last) ? p : nullptr), m_count(uint16_t(1+(last-p))), m_uses(Uses::SmartPtr) {}
  InnerCellIterator(Cell* const *p, Cell* const *last) :
    m_ptr((p != last) ? p : nullptr), m_count(uint16_t(1+(last-p))), m_uses(Uses::RawPtr) {}
  InnerCellIterator(const InnerCellIterator &o) = default;
  InnerCellIterator &operator=(const InnerCellIterator &o) = default;
  InnerCellIterator operator++(int)
  {
    auto ret = *this;
    return operator++(), ret;
  }
  InnerCellIterator &operator++()
  {
    wxASSERT_MSG(!m_ptr == !m_count, "Bug: The InnerCellIterator pointer and count must be both either zero or nonzero.");
    if (m_ptr)
    {
      if (m_uses == Uses::SmartPtr) AdvanceLoop<Uses::SmartPtr>();
      else if (m_uses == Uses::RawPtr) AdvanceLoop<Uses::RawPtr>();
      else wxASSERT_MSG(false, "Internal error in InnerCellIterator");
      if (!m_count)
        m_ptr = nullptr;
    }
    return *this;
  }
  bool operator==(const InnerCellIterator &o) const
  { return (m_uses == o.m_uses || !m_ptr) && m_ptr == o.m_ptr; }
  bool operator!=(const InnerCellIterator &o) const
  { return ((m_uses != o.m_uses) && m_ptr) || m_ptr != o.m_ptr; }
  operator bool() const { return GetInner(); }
  operator Cell*() const { return GetInner(); }
  Cell *operator->() const { return GetInner(); }

private:
  template <Uses> Cell *GetInner_() const;
  template <Uses> void Advance_();
  Cell *GetInner() const;
  template <Uses uses> void AdvanceLoop();
};

template <> inline Cell *InnerCellIterator::GetInner_<InnerCellIterator::Uses::SmartPtr>() const
{ return static_cast<SmartPtr_>(m_ptr)->get(); }
template <> inline Cell *InnerCellIterator::GetInner_<InnerCellIterator::Uses::RawPtr>() const
{ return *static_cast<RawPtr_>(m_ptr); }
template <> inline void InnerCellIterator::Advance_<InnerCellIterator::Uses::SmartPtr>()
{ m_ptr = static_cast<SmartPtr_>(m_ptr) + 1; }
template <> inline void InnerCellIterator::Advance_<InnerCellIterator::Uses::RawPtr>()
{ m_ptr = static_cast<RawPtr_>(m_ptr) + 1; }

inline Cell *InnerCellIterator::GetInner() const
{
  if (!m_ptr) return {};
  if (m_uses == Uses::SmartPtr) return GetInner_<Uses::SmartPtr>();
  else if (m_uses == Uses::RawPtr) return GetInner_<Uses::RawPtr>();
  else return {};
}

template <InnerCellIterator::Uses uses>
inline void InnerCellIterator::AdvanceLoop()
{
  Cell *prev = GetInner_<uses>();
  for (;;)
  {
    Advance_<uses>();
    if (!--m_count)
      break;
    Cell *const cur = GetInner_<uses>();
    wxASSERT(!prev || prev != cur);
    if (cur)
      break;
  }
}

class InnerCellAdapter final {
  using iterator = InnerCellIterator;
  iterator const m_iter;

public:
  explicit InnerCellAdapter(iterator inner) : m_iter(inner) {}
  constexpr iterator begin() const { return m_iter; }
  constexpr iterator end() const { return {}; }
};

#endif
