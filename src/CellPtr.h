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
 * Implementation of an observing weak Cell pointer.
 *
 * It doesn't have the overhead of atomic reference counting, as would be imposed by
 * shared_ptr. The latter has high likelihood of misuse, and suffers from the locking
 * boilerplate needed to use the `weak_ptr`. But - above all - wxMaxima's data model does
 * not use shared ownership. All cells are have a single owner - either a cell list rooted
 * in a unique_ptr, or are owned by internal cell pointers.
 */

#ifndef CELLPTR_H
#define CELLPTR_H

#include <wx/debug.h>
#include <memory>
#include <type_traits>

/*! Objects deriving from this class can be observed by the CellPtr.
 *
 * This class is not copyable.
 */
class Observed
{
  struct ControlBlock
  {
    static ControlBlock empty;

    //! Pointer to the object
    Observed *const m_object = {};
    //! Number of observers for this object
    unsigned int m_refCount = 0;

    ControlBlock *Ref() { ++m_refCount; return this; }
    void Deref() {
      wxASSERT(m_refCount);
      if (!--m_refCount)
        delete this;
    }
    explicit ControlBlock(Observed *object) : m_object(object) {}
    explicit ControlBlock(decltype(nullptr)) : m_refCount(1) {}
    ControlBlock(const ControlBlock &) = delete;
    void operator=(const ControlBlock &) = delete;
  };

  ControlBlock *const m_cb = (new ControlBlock(this))->Ref();
  Observed(const Observed &) = delete;
  void operator=(const Observed &) = delete;

protected:
  Observed() = default;
  virtual ~Observed()
  {
    m_cb->Deref();
  }
  template <typename T> friend class CellPtr;
};

class Cell;
class GroupCell;

#define CELLPTR_CAST_TO_PTR 1

/*! A weak non-owning pointer that becomes null whenever the observed object is
 * destroyed.
 *
 * The type must be derived from Observed, and this fact is checked at the point
 * of instantiation. The pointer's instance can be declared with forward-defined classes.
 */
template <typename T>
class CellPtr
{
  using ControlBlock = Observed::ControlBlock;
  ControlBlock *m_cb = Ref(nullptr);
  ControlBlock *Ref(Observed *obj) { return (obj ? obj->Observed::m_cb : &ControlBlock::empty)->Ref(); }
public:
  using value_type = T;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;

  CellPtr() = default;
  ~CellPtr() { m_cb->Deref(); }
  void reset() { reset(nullptr); }

  // Observers
  //
  explicit operator bool() const { return m_cb->m_object; }
  pointer get() const;
  inline reference operator*() const { return *get(); }
  inline pointer operator->() const { return get(); };

#if CELLPTR_CAST_TO_PTR
  operator pointer() const { return get(); }
#endif

  template <typename PtrT>
  PtrT CastAs() const { return dynamic_cast<PtrT>(m_cb->m_object); }

  // Operations with NULL and integers in general
  //
  CellPtr(int) = delete;
  CellPtr(void *) = delete;

  // Operations with nullptr_t
  //
  CellPtr(decltype(nullptr)) {}
  bool operator==(decltype(nullptr)) const { return !bool(this); }
  bool operator!=(decltype(nullptr)) const { return bool(this); }

  // Operations with convertible-to-pointer types
  //
  template <typename U,
           typename std::enable_if<std::is_convertible<U, pointer>::value, bool>::type = true>
  explicit CellPtr(U obj) : m_cb(Ref(obj)) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<U, pointer>::value, bool>::type = true>
  CellPtr &operator=(U obj)
  {
    reset(obj);
    return *this;
  }

  template <typename U,
           typename std::enable_if<std::is_convertible<U, pointer>::value, bool>::type = true>
  void reset(U obj)
  {
    if (obj != m_cb->m_object) {
      m_cb->Deref();
      m_cb = Ref(obj);
    } else {
      // If the observed objects are the same, the control block must be the same as well.
      pointer pObj = obj; // needed because !nullptr is invalid on gcc <8.
      wxASSERT((!pObj && m_cb == &ControlBlock::empty)
               || (pObj && m_cb == static_cast<const Observed*>(obj)->m_cb));
    }
  }

  // Operations with compatible CellPtrs
  //
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr(CellPtr<U> &&o) {
    using namespace std;
    swap(m_cb, o.m_cb);
  }

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr(const CellPtr<U> &o) : m_cb(o.m_cb->Ref()) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(CellPtr<U> &&o)
  {
    using namespace std;
    swap(m_cb, o.m_cb);
    return *this;
  }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const CellPtr<U> &o)
  {
    reset(o.get());
    return *this;
  }

#if !CELLPTR_CAST_TO_PTR
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator==(const CellPtr<U> &ptr) const { return m_cb == ptr.m_cb; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator!=(const CellPtr<U> &ptr) const { return m_cb != ptr.m_cb; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator<(const CellPtr<U> &ptr) const { return m_cb->m_object < ptr.m_cb->m_object; }
#endif

  // Operations with compatible unique_ptr
  //
  template <typename U, typename Del>
  explicit CellPtr(std::unique_ptr<U> &&) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  explicit CellPtr(const std::unique_ptr<U> &ptr) : CellPtr(ptr.get()) {}

  template <typename U, typename Del>
  CellPtr &operator=(std::unique_ptr<U, Del> &&o) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const std::unique_ptr<U, Del> &o)
  {
    reset(o.get());
    return *this;
  }
};

//

template <typename T> typename
CellPtr<T>::pointer CellPtr<T>::get() const { return static_cast<pointer>(m_cb->m_object); }

//! Declaration of a specialization within GroupCell.cpp. This allows
//! use of CellPtr<GroupCell> when the GroupCell class is not fully defined yet.
template <>
CellPtr<GroupCell>::pointer CellPtr<GroupCell>::get() const;

//

#if !CELLPTR_CAST_TO_PTR
template <typename T,
         typename U, typename
         std::enable_if<std::is_convertible<U, const Observed*>::value, bool>::type = true>
bool operator==(U obj, const CellPtr<T> &cellPtr) { return cellPtr.get() == obj; }
#endif

template <typename T,
         typename U, typename
         std::enable_if<std::is_convertible<U, const Observed*>::value, bool>::type = true>
bool operator==(const CellPtr<T> &cellPtr, U obj) { return cellPtr.get() == obj; }

#if !CELLPTR_CAST_TO_PTR
template <typename T,
         typename U, typename
         std::enable_if<std::is_convertible<U, const Observed*>::value, bool>::type = true>
bool operator!=(U obj, const CellPtr<T> &cellPtr) { return cellPtr.get() != obj; }
#endif

template <typename T,
         typename U, typename
         std::enable_if<std::is_convertible<U, const Observed*>::value, bool>::type = true>
bool operator!=(const CellPtr<T> &cellPtr, U obj) { return cellPtr.get() != obj; }


#endif // CELLPTR_H
