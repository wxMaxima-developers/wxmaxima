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
#include <wx/log.h>
#include <memory>
#include <type_traits>
#include <cstddef>

//! Set to 1 to enable casting from CellPtr<U> to U*
#define CELLPTR_CAST_TO_PTR 1

//! Set to 1 to enable CellPtr control block reference count logs
#define CELLPTR_LOG_REFS 0

//! Set to 1 to enable CellPtr lifetime logging
#define CELLPTR_LOG_INSTANCES 0

class CellPtrBase;

/*! Objects deriving from this class can be observed by the CellPtr.
 *
 * This class is not copyable.
 */
class Observed
{
  friend class CellPtrBase;

  static size_t m_instanceCount;

  class ControlBlock final
  {
    //! Pointer to the object
    Observed *m_object = {};
    //! Number of observers for this object
    mutable unsigned int m_refCount = 0;

  public:
    static const ControlBlock empty;    

    explicit ControlBlock(Observed *object) : m_object(object) {}
    explicit ControlBlock(decltype(nullptr)) : m_refCount(1) {}
    ControlBlock(const ControlBlock &) = delete;
    void operator=(const ControlBlock &) = delete;

    void Clear() { m_object = nullptr; }

    inline Observed *Get() const { return m_object; }

    //! References the control block.
    ControlBlock *Ref(const CellPtrBase *cellptr) {
      const_cast<const ControlBlock *>(this)->Ref(cellptr);
      return this;
    }
    //! References the control block.
    const ControlBlock *Ref(const CellPtrBase *cellptr) const {
      if (CELLPTR_LOG_REFS)
        wxLogDebug("%p CB::Ref (%d->%d) cb=%p obj=%p", cellptr, m_refCount, m_refCount+1, this, m_object);
      else
        wxUnusedVar(cellptr);
      ++m_refCount;
      return this;
    }
    //! Dereferences the control block and deletes it if necessary.
    static std::nullptr_t Deref(const ControlBlock * cb, const CellPtrBase *cellptr)
    {
      // Note: Deref only returns non-null value once - when its reference count reaches zero.
      delete cb->Deref(cellptr);
      return nullptr;
    }

  private:
    //! Dereferences the control block and returns nullptr is the block should be retained,
    //! or its address if the block should be deleted.
    const ControlBlock *Deref(const CellPtrBase *cellptr) const
    {
      if (CELLPTR_LOG_REFS)
        wxLogDebug("%p CB::Deref (%d->%d) cb=%p obj=%p", cellptr, m_refCount, m_refCount-1, this, m_object);
      else
        wxUnusedVar(cellptr);
      wxASSERT(m_refCount > 1 || (m_refCount == 1 && this != &empty));
      if (!--m_refCount)
      {
        wxASSERT(!m_object);
        return this;
      }
      return nullptr;
    }
  };

  ControlBlock *const m_cb = (new ControlBlock(this))->Ref(nullptr);
  Observed(const Observed &) = delete;
  void operator=(const Observed &) = delete;

protected:
  Observed() { ++ m_instanceCount; }
  virtual ~Observed()
  {
    m_cb->Clear();
    ControlBlock::Deref(m_cb, nullptr);
    -- m_instanceCount;
  }

public:
  static size_t GetLiveInstanceCount() { return m_instanceCount; }
};

class Cell;
class GroupCell;

class CellPtrBase
{
private:
  using ControlBlock = Observed::ControlBlock;
  static size_t m_instanceCount;

  const ControlBlock *m_cb = nullptr;

  const ControlBlock *Ref(Observed *obj) const {
    return (obj ? obj->Observed::m_cb : &ControlBlock::empty)->Ref(this); }

protected:
  explicit CellPtrBase(Observed *obj = nullptr) : m_cb(Ref(obj))
  {
    ++m_instanceCount;
    if (CELLPTR_LOG_INSTANCES) wxLogDebug("%p->CellPtr(%p) cb=%p", this, obj, m_cb);
  }

  CellPtrBase(const CellPtrBase &o) : CellPtrBase(o.base_get()) {}

  CellPtrBase(CellPtrBase &&o)
  {
    ++m_instanceCount;
    if (CELLPTR_LOG_INSTANCES)
      wxLogDebug("%p->Cellptr(&&%p) cb=%p<->%p", this, &o, m_cb, o.m_cb);
    using namespace std;
    swap(m_cb, o.m_cb);
  }

  ~CellPtrBase()
  {
    --m_instanceCount;
    if (CELLPTR_LOG_INSTANCES)
      wxLogDebug("%p->~CellPtr() cb=%p obj=%p", this, m_cb, m_cb->Get());
    wxASSERT(m_cb);
    m_cb = ControlBlock::Deref(m_cb, this);
  }

  Observed *base_get() const { return m_cb->Get(); }

  CellPtrBase &operator=(const CellPtrBase &o)
  {
    base_reset(o.base_get());
    return *this;
  }

  CellPtrBase &operator=(CellPtrBase &&o)
  {
    if (CELLPTR_LOG_INSTANCES)
      wxLogDebug("%p->CellPtr::operator=(&&%p) cb=%p<->%p", this, &o, m_cb, o.m_cb);
    using namespace std;
    swap(m_cb, o.m_cb);
    return *this;
  }

  void base_reset(Observed *obj = nullptr)
  {
    if (obj != m_cb->Get()) {
      wxASSERT(!obj || m_cb != obj->m_cb);
      if (CELLPTR_LOG_REFS)
        wxLogDebug("%p->CellPtr::reset(%p->%p) cb=%p->%p", this, m_cb->Get(), obj, m_cb, obj ? obj->m_cb : &ControlBlock::empty);
      m_cb = ControlBlock::Deref(m_cb, this);
      m_cb = Ref(obj);
    } else {
      // The objects are the same - their control blocks must be the same as well,
      // unless the objects are null. If they are null, then the control blocks
      // may be different - both blocks had originated in different objects that both
      // got freed.
      wxASSERT(!obj || (m_cb == static_cast<const Observed*>(obj)->m_cb));
    }
  }

public:
  template <typename U>
  static bool constexpr is_pointer() {
    return std::is_same<U, decltype(nullptr)>::value
           || (std::is_pointer<U>::value && std::is_convertible<U, Observed*>::value);
  }

  explicit operator bool() const { return m_cb->Get(); }

  inline void reset() { base_reset(); }

  //! This is exactly like the spaceship operator in C++20
  auto cmpControlBlocks(const CellPtrBase &o) const { return m_cb - o.m_cb; }

  //! This is the spaceship operator acting on pointed-to objects
  auto cmpObjects(const CellPtrBase &o) const { return m_cb->Get() - o.m_cb->Get(); }

  //! This is the spaceship operator acting on pointed-to objects
  auto cmpObjects(const Observed *o) const { return m_cb->Get() - o; }

  static size_t GetLiveInstanceCount() { return m_instanceCount; }
};

/*! A weak non-owning pointer that becomes null whenever the observed object is
 * destroyed.
 *
 * The type must be derived from Observed, and this fact is checked at the point
 * of instantiation. The pointer's instance can be declared with forward-defined classes.
 */
template <typename T>
class CellPtr final : public CellPtrBase
{
  template <typename U>
  static bool constexpr is_pointer() {
    return std::is_same<U, decltype(nullptr)>::value
           || (std::is_pointer<U>::value && std::is_convertible<U, pointer>::value);
  }
public:
  using value_type = T;
  using pointer = T*;
  using const_pointer = const T*;
  using reference = T&;

  CellPtr() = default;

  // Observers
  //
  pointer get() const;
  inline reference operator*() const { return *get(); }
  inline pointer operator->() const { return get(); };

#if CELLPTR_CAST_TO_PTR
  operator pointer() const { return get(); }
#endif

  template <typename PtrT, typename std::enable_if<std::is_pointer<PtrT>::value, bool>::type = true>
  PtrT CastAs() const { return dynamic_cast<PtrT>(base_get()); }

  // Operations with NULL and integers in general
  //
  explicit CellPtr(int) = delete;
  explicit CellPtr(void *) = delete;

  // Operations with nullptr_t
  //
  void reset() { base_reset(); }
  explicit CellPtr(decltype(nullptr)) {}
  CellPtr &operator=(decltype(nullptr)) { base_reset(); return *this; }
  bool operator==(decltype(nullptr)) const { return !bool(*this); }
  bool operator!=(decltype(nullptr)) const { return bool(*this); }

  // Operations with convertible-to-pointer types
  //
  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  explicit CellPtr(U obj) : CellPtrBase(obj) {}

  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  CellPtr &operator=(U obj)
  {
    base_reset(obj);
    return *this;
  }

  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  void reset(U obj)
  { base_reset(obj); }
  // Operations with compatible CellPtrs
  //
  CellPtr(CellPtr &o) : CellPtrBase(o) {}
  CellPtr(CellPtr &&o) : CellPtrBase(std::move(o)) {}
  CellPtr &operator=(const CellPtr &o) { CellPtrBase::operator=(o); return *this; }
  CellPtr &operator=(CellPtr &&o) { CellPtrBase::operator=(std::move(o)); return *this; }

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  // cppcheck-suppress noExplicitConstructor
  CellPtr(CellPtr<U> &&o) : CellPtrBase(o) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  // cppcheck-suppress noExplicitConstructor
  CellPtr(const CellPtr<U> &o) : CellPtrBase(o.get()) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(CellPtr<U> &&o)
  {
    CellPtrBase::operator=(o);
    return *this;
  }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const CellPtr<U> &o)
  {
    CellPtrBase::operator=(o);
    return *this;
  }

#if !CELLPTR_CAST_TO_PTR
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator==(const CellPtr<U> &ptr) const { return cmpControlBlocks(ptr) == 0; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator!=(const CellPtr<U> &ptr) const { return cmpControlBlocks(ptr) != 0; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator<(const CellPtr<U> &ptr) const { return cmpObjects(ptr) < 0; }
#endif

  // Operations with compatible unique_ptr
  //
  template <typename U, typename Del>
  explicit CellPtr(std::unique_ptr<U, Del> &&) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  explicit CellPtr(const std::unique_ptr<U, Del> &ptr) : CellPtrBase(ptr.get()) {}

  template <typename U, typename Del>
  CellPtr &operator=(std::unique_ptr<U, Del> &&) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const std::unique_ptr<U, Del> &o)
  { return *this = o.get(); }
};

//

template <typename T> typename
CellPtr<T>::pointer CellPtr<T>::get() const { return static_cast<pointer>(base_get()); }

//! Declaration of a specialization for GroupCell. This allows
//! use of CellPtr<GroupCell> when the GroupCell class is not fully defined yet.
template <>
CellPtr<GroupCell>::pointer CellPtr<GroupCell>::get() const;

//

template <typename T, typename U>
bool operator==(const CellPtr<T> &left, const CellPtr<U> &right) { return left.cmpControlBlocks(right) == 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator==(U left, const CellPtr<T> &right) { return right.cmpObjects(left) == 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator==(const CellPtr<T> &left, U right) { return left.cmpObjects(right) == 0; }

template <typename T, typename U>
bool operator!=(const CellPtr<T> &left, const CellPtr<U> &right) { return left.cmpControlBlocks(right) != 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator!=(U left, const CellPtr<T> &right) { return right.cmpObjects(left) != 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator!=(const CellPtr<T> &left, U right) { return left.cmpObjects(right) != 0; }

template <typename T, typename U>
bool operator<(const CellPtr<T> &left, const CellPtr<U> &right) { return left.cmpObjects(right) < 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator<(U left, const CellPtr<T> &right) { return right.cmpObjects(left) > 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator<(const CellPtr<T> &left, U right) { return left.cmpObjects(right) < 0; }

#endif // CELLPTR_H
