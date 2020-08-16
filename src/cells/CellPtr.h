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
#include <cstddef>
#include <cstdint>
#include <cinttypes>
#include <memory>
#include <type_traits>

//! Set to 1 to enable casting from CellPtr\<U\> to U*
#define CELLPTR_CAST_TO_PTR 1

//! Set to 1 to enable CellPtr control block reference count logs
#define CELLPTR_LOG_REFS 0

//! Set to 1 to enable CellPtr lifetime logging
#define CELLPTR_LOG_INSTANCES 0

//! Set to the type of logging you wish for CellPtr (can be e.g. wxLogDebug or wxLogMessage)
#define CELLPTR_LOG_METHOD wxLogDebug

class CellPtrBase;

/*! Objects deriving from this class can be observed by the CellPtr.
 *
 * This class is not copyable.
 */
class Observed
{
  class ControlBlock final
  {
    //! Pointer to the object this control block tracks.
    Observed *m_object = {};
    //! Number of observers for this object
    unsigned int m_refCount = 0;

  #if CELLPTR_LOG_REFS
    void LogConstruct(const Observed *) const;
    void LogRef(const CellPtrBase *) const;
    void LogDeref(const CellPtrBase *) const;
    void LogDestruct() const;
  #else
    void LogConstruct(const Observed *) const {}
    void LogRef(const CellPtrBase *) const {}
    void LogDeref(const CellPtrBase *) const {}
    void LogDestruct() const {}
  #endif

  public:
    explicit ControlBlock(Observed *object) : m_object(object)
    {
      LogConstruct(object);
      wxASSERT(object);
    }
    ~ControlBlock() { LogDestruct(); }
    ControlBlock(const ControlBlock &) = delete;
    void operator=(const ControlBlock &) = delete;

    constexpr void reset() noexcept { m_object = nullptr; }
    constexpr inline Observed *Get() const noexcept { return m_object; }

    //! References the control block, and returns the pointer to the control block.
    ControlBlock *Ref(const CellPtrBase *cellptr)
    {
      LogRef(cellptr);
      ++m_refCount;
      return this;
    }

    //! Dereferences the control block and returns true if the block should be retained,
    //! otherwise false.
    bool Deref(const CellPtrBase *cellptr)
    {
      LogDeref(cellptr);
      wxASSERT(m_refCount >= 1);
      return --m_refCount;
    }
  };

  /*! A tagged pointer that can carry one of the following types:
   *
   * 1. nullptr_t
   * 2. Observed*
   * 3. Observed::ControlBlock*
   * 4. CellPtrBase
   */
  class CellPtrImplPointer final
  {
    friend void swap(CellPtrImplPointer &a, CellPtrImplPointer &b) noexcept;
    uintptr_t m_ptr = {};
    enum Tag : uintptr_t {
      to_nullptr_t = 0,
      to_Observed = 1,
      to_ControlBlock = 2,
      to_CellPtrBase = 3,
      to_MASK = 3,
      to_MASK_OUT = uintptr_t(-intptr_t(to_MASK + 1)),
    };
  static uintptr_t ReprFor(Observed *ptr) noexcept
    { return (reinterpret_cast<uintptr_t>(ptr) & to_MASK_OUT) | to_Observed; }
  static uintptr_t ReprFor(ControlBlock *ptr) noexcept
    { return (reinterpret_cast<uintptr_t>(ptr) & to_MASK_OUT) | to_ControlBlock; }
  static uintptr_t ReprFor(CellPtrBase *ptr) noexcept
    { return (reinterpret_cast<uintptr_t>(ptr) & to_MASK_OUT) | to_CellPtrBase; }
  public:
    constexpr CellPtrImplPointer() noexcept {}
    constexpr CellPtrImplPointer(const CellPtrImplPointer &) noexcept = default;
    constexpr CellPtrImplPointer(decltype(nullptr)) noexcept {}

    CellPtrImplPointer(Observed *ptr) noexcept    : m_ptr(ReprFor(ptr)) {}
    CellPtrImplPointer(ControlBlock *ptr) noexcept : m_ptr(ReprFor(ptr)) {}
    CellPtrImplPointer(CellPtrBase *ptr) noexcept  : m_ptr(ReprFor(ptr)) {}

    constexpr CellPtrImplPointer &operator=(decltype(nullptr)) noexcept    { m_ptr = {};           return *this; }
    constexpr CellPtrImplPointer &operator=(CellPtrImplPointer o) noexcept { m_ptr = o.m_ptr;      return *this; }
    CellPtrImplPointer &operator=(Observed *ptr) noexcept                  { m_ptr = ReprFor(ptr); return *this; }
    CellPtrImplPointer &operator=(ControlBlock *ptr) noexcept              { m_ptr = ReprFor(ptr); return *this; }
    CellPtrImplPointer &operator=(CellPtrBase *ptr) noexcept               { m_ptr = ReprFor(ptr); return *this; }

    constexpr explicit inline operator bool() const noexcept { return m_ptr != 0; }
    constexpr inline bool HasObserved() const noexcept     { return (m_ptr & to_MASK) == to_Observed; }
    constexpr inline bool HasControlBlock() const noexcept { return (m_ptr & to_MASK) == to_ControlBlock; }
    constexpr inline bool HasCellPtrBase() const noexcept  { return (m_ptr & to_MASK) == to_CellPtrBase; }
    constexpr inline auto GetObserved() const noexcept     { return HasObserved()     ? reinterpret_cast<Observed *>    (m_ptr & to_MASK_OUT) : nullptr; }
    constexpr inline auto GetControlBlock() const noexcept { return HasControlBlock() ? reinterpret_cast<ControlBlock *>(m_ptr & to_MASK_OUT) : nullptr; }
    constexpr inline auto GetCellPtrBase() const noexcept  { return HasCellPtrBase()  ? reinterpret_cast<CellPtrBase *> (m_ptr & to_MASK_OUT) : nullptr; }

    constexpr auto GetImpl() const noexcept { return m_ptr; }
  };

  friend void swap(CellPtrImplPointer &a, CellPtrImplPointer &b) noexcept;
  friend class CellPtrBase;
  static size_t m_instanceCount;

  /*! Pointer to null, CellPtrBase, or ControlBlock.
   *
   * 1. If no CellPtrs point to this object, m_ptr is null.
   * 2. If one CellPtr points to this object, and no more CellPtrs have pointed to it at once
   *    from the time m_ptr became non-null, m_ptr points to the sole CellPtr.
   * 3. If more than one CellPtr points to this object, or more than one CellPtr has pointed
   *    to it from the time m_ptr last became non-null, m_ptr points to the ControlBlock.
   */
  CellPtrImplPointer m_ptr;

  Observed(const Observed &) = delete;
  void operator=(const Observed &) = delete;
  //! Perform cleanup when we're indeed observed and are being destroyed.
  void OnEndOfLife() noexcept;

#if CELLPTR_LOG_REFS
  void LogRef(const CellPtrBase *) const;
  void LogDeref(const CellPtrBase *) const;
#else
  void LogRef(const CellPtrBase *) const {}
  void LogDeref(const CellPtrBase *) const {}
#endif

protected:
  Observed() noexcept { ++ m_instanceCount; }
  ~Observed()
  {
    if (m_ptr) OnEndOfLife();
    -- m_instanceCount;
  }

public:
  static size_t GetLiveInstanceCount() { return m_instanceCount; }
  constexpr bool IsNull() const { return !m_ptr.GetImpl(); }
  constexpr bool HasControlBlock() const { return m_ptr.GetControlBlock(); }
  constexpr bool HasOneCellPtr() const { return m_ptr.GetCellPtrBase(); }
};

inline void swap(Observed::CellPtrImplPointer &a, Observed::CellPtrImplPointer &b) noexcept
{ std::swap(a.m_ptr, b.m_ptr); }

class Cell;
class GroupCell;

/*! An implementation detail for the type-specific templated cell pointers.
 *
 * A CellPtrBase (and thus the derived CellPtr) can be in one of 3 states:
 * 1. Pointing to no object: its m_ptr is null.
 * 2. A sole pointer to an object: its m_ptr points to the Observed, and observed's
 *    m_ptr points *back* at this pointer.
 * 3. One of many pointers to the object: its m_ptr points to the Observed::ControlBlock,
 *    and the observed's m_ptr *also* points to the control block.
 */
class CellPtrBase
{
  using CellPtrImplPointer = Observed::CellPtrImplPointer;
  using ControlBlock = Observed::ControlBlock;
  static size_t m_instanceCount;

  /*! Pointer to null, the object itself, or to the control block.
   *
   * 1. If CellPtr is null, m_ptr is null as well.
   * 2. If CellPtr is the only pointer to a given object, and no other pointers pointed to the object before,
   *    m_ptr points to that object.
   * 3. If CellPtr is one of many pointers to a given object, or more pointers pointed to it in the past,
   *    then m_ptr points to the control block for that object.
   */
  mutable CellPtrImplPointer m_ptr;

  //! Adds a reference from this pointer to the given object
  void Ref(Observed *obj) noexcept;

  //! Removes a reference from this pointer to its object
  void Deref() noexcept;

  //! Removes a reference from this pointer to an object's control block. This
  //! is a special case, invoked by Deref().
  void DerefControlBlock() const noexcept;

#if CELLPTR_LOG_INSTANCES
  void LogConstruction(Observed *obj) const;
  void LogMove(const CellPtrBase &o) const;
  void LogAssignment(const CellPtrBase &o) const;
  void LogDestruction() const;
#else
  inline void LogConstruction(Observed *) const {}
  inline void LogMove(const CellPtrBase &) const {}
  inline void LogAssignment(const CellPtrBase &) const {}
  inline void LogDestruction() const {}
#endif

protected:
  explicit CellPtrBase(Observed *obj = nullptr) noexcept
  {
    ++m_instanceCount;
    if (obj) Ref(obj);
    LogConstruction(obj);
  }

  CellPtrBase(const CellPtrBase &o) noexcept : CellPtrBase(o.base_get()) {}

  CellPtrBase(CellPtrBase &&o) noexcept
  {
    ++m_instanceCount;
    LogMove(o);
    using namespace std;

    auto *thisObserved = m_ptr.GetObserved();
    if (thisObserved)
    {
      wxASSERT(thisObserved->m_ptr.GetCellPtrBase() == this);
      thisObserved->LogDeref(this);
      thisObserved->LogRef(&o);
      thisObserved->m_ptr = &o;
    }

    auto *otherObserved = o.m_ptr.GetObserved();
    if (otherObserved)
    {
      wxASSERT(otherObserved->m_ptr.GetCellPtrBase() == &o);
      otherObserved->LogDeref(&o);
      otherObserved->LogRef(this);
      otherObserved->m_ptr = this;
    }

    swap(m_ptr, o.m_ptr);
  }

  ~CellPtrBase() noexcept
  {
    --m_instanceCount;
    LogDestruction();
    Deref();
  }

  CellPtrBase &operator=(const CellPtrBase &o) noexcept
  {
    base_reset(o.base_get());
    return *this;
  }

  CellPtrBase &operator=(CellPtrBase &&o) noexcept
  {
    LogAssignment(o);
    using namespace std;
    swap(m_ptr, o.m_ptr);
    return *this;
  }

  inline Observed *base_get() const noexcept
  {
    auto *const observed1 = m_ptr.GetObserved();
    if (!m_ptr || observed1)
      return observed1;
    auto *const cb = m_ptr.GetControlBlock();
    auto *const observed2 = cb->Get();
    if (observed2)
      return observed2;
    DerefControlBlock();
    return nullptr;
  }

  void base_reset(Observed *obj = nullptr) noexcept;

public:
  template <typename U>
  static bool constexpr is_pointer() {
    return std::is_same<U, decltype(nullptr)>::value
           || (std::is_pointer<U>::value && std::is_convertible<U, Observed*>::value);
  }

  explicit operator bool() const noexcept { return base_get(); }

  inline void reset() noexcept { base_reset(); }

  //! This is exactly like the spaceship operator in C++20
  auto cmpPointers(const CellPtrBase &o) const noexcept { return m_ptr.GetImpl() - o.m_ptr.GetImpl(); }

  //! This is the spaceship operator acting on pointed-to objects
  auto cmpObjects(const CellPtrBase &o) const noexcept { return base_get() - o.base_get(); }

  //! This is the spaceship operator acting on pointed-to objects
  auto cmpObjects(const Observed *o) const noexcept { return base_get() - o; }

  static size_t GetLiveInstanceCount() noexcept { return m_instanceCount; }

  constexpr bool IsNull() const { return !m_ptr.GetImpl(); }
  constexpr bool HasOneObserved() const { return m_ptr.GetObserved(); }
  constexpr bool HasControlBlock() const { return m_ptr.GetControlBlock(); }
};

/*! A weak non-owning pointer that becomes null whenever the observed object is
 * destroyed.
 *
 * **The use of this pointer type has performance implications. It is not a "free"
 * abstraction!**
 *
 * **Warning:** To maintain performance, most cells should have at most one CellPtr
 * pointing at them. Currently, this is the m_nextToDraw - it uses up our "CellPtr
 * budget". The remaining CellPtrs are in CellPointers, and there is very few cells
 * at any given time that are pointed-to by those pointers, and thus the performance
 * impact is minimal.
 *
 * In the common case of being null, or of being the only CellPtr to a given cell,
 * the performance is similar to a raw pointer: it stores the cell pointer directly.
 * The pointed-to cell points back to this sole pointer, so that it can reset it to null
 * when it gets destroyed. A CellPtr is exactly the size of an `Observed *`.
 *
 * When the second CellPtr is made to point to a cell, a shared control block is allocated
 * on behalf of the cell, and both the CellPtr and the cell point to it. The control block is
 * greedily dereferenced whenever the CellPtr notices that the cell it pointed to has
 * vanished. But, once a cell has a ControlBlock, it doesn't get rid of it until no pointers
 * point to it. So, if there are two+ pointers pointing to a cell, then only one is left,
 * there still is a control block, and the pointers still "pointer chase" through that
 * control block. Getting rid of the control block in this case is a low-priority TODO at the
 * moment, since there's no performance impact seen from this.
 *
 * The observed type must be derived from Observed, and this fact is checked at the point
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

  CellPtr() noexcept = default;

  // Observers
  //
  pointer get() const noexcept;
  inline reference operator*() const noexcept { return *get(); }
  inline pointer operator->() const noexcept { return get(); };

#if CELLPTR_CAST_TO_PTR
  operator pointer() const noexcept { return get(); }
#endif

  template <typename PtrT, typename std::enable_if<std::is_pointer<PtrT>::value, bool>::type = true>
  PtrT CastAs() const noexcept;

  // Operations with NULL and integers in general
  //
  explicit CellPtr(int) = delete;
  explicit CellPtr(void *) = delete;

  // Operations with nullptr_t
  //
  void reset() noexcept { base_reset(); }
  explicit CellPtr(decltype(nullptr)) noexcept {}
  CellPtr &operator=(decltype(nullptr)) noexcept { base_reset(); return *this; }
  bool operator==(decltype(nullptr)) const noexcept { return !bool(*this); }
  bool operator!=(decltype(nullptr)) const noexcept { return bool(*this); }

  // Operations with convertible-to-pointer types
  //
  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  explicit CellPtr(U obj) noexcept : CellPtrBase(obj) {}

  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  CellPtr &operator=(U obj) noexcept
  {
    base_reset(obj);
    return *this;
  }

  template <typename U, typename std::enable_if<is_pointer<U>(), bool>::type = true>
  void reset(U obj) noexcept
  { base_reset(obj); }
  // Operations with compatible CellPtrs
  //
  CellPtr(CellPtr &o) noexcept : CellPtrBase(o) {}
  CellPtr(CellPtr &&o) noexcept : CellPtrBase(std::move(o)) {}
  CellPtr &operator=(const CellPtr &o) noexcept { CellPtrBase::operator=(o); return *this; }
  CellPtr &operator=(CellPtr &&o) noexcept { CellPtrBase::operator=(std::move(o)); return *this; }

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  // cppcheck-suppress noExplicitConstructor
  CellPtr(CellPtr<U> &&o) noexcept : CellPtrBase(o) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  // cppcheck-suppress noExplicitConstructor
  CellPtr(const CellPtr<U> &o) noexcept : CellPtrBase(o.get()) {}

  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(CellPtr<U> &&o) noexcept
  {
    CellPtrBase::operator=(o);
    return *this;
  }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const CellPtr<U> &o) noexcept
  {
    CellPtrBase::operator=(o);
    return *this;
  }

#if !CELLPTR_CAST_TO_PTR
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator==(const CellPtr<U> &ptr) const noexcept { return cmpControlBlocks(ptr) == 0; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator!=(const CellPtr<U> &ptr) const noexcept { return cmpControlBlocks(ptr) != 0; }
  template <typename U,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  bool operator<(const CellPtr<U> &ptr) const noexcept { return cmpObjects(ptr) < 0; }
#endif

  // Operations with compatible unique_ptr
  //
  template <typename U, typename Del>
  explicit CellPtr(std::unique_ptr<U, Del> &&) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  explicit CellPtr(const std::unique_ptr<U, Del> &ptr) noexcept : CellPtrBase(ptr.get()) {}

  template <typename U, typename Del>
  CellPtr &operator=(std::unique_ptr<U, Del> &&) = delete;

  template <typename U, typename Del,
           typename std::enable_if<std::is_convertible<typename std::add_pointer<U>::type, pointer>::value, bool>::type = true>
  CellPtr &operator=(const std::unique_ptr<U, Del> &o) noexcept
  { return *this = o.get(); }
};

//

template <typename T> typename
CellPtr<T>::pointer CellPtr<T>::get() const noexcept { return static_cast<pointer>(base_get()); }

/*! Declaration of a specialization for GroupCell. 

This allows use of CellPtr<GroupCell> when the GroupCell class is not fully defined yet.
*/
template <>
CellPtr<GroupCell>::pointer CellPtr<GroupCell>::get() const noexcept;

//

template <typename T, typename U>
bool operator==(const CellPtr<T> &left, const CellPtr<U> &right) noexcept { return left.cmpPointers(right) == 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator==(U left, const CellPtr<T> &right) noexcept { return right.cmpObjects(left) == 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator==(const CellPtr<T> &left, U right) noexcept { return left.cmpObjects(right) == 0; }

template <typename T, typename U>
bool operator!=(const CellPtr<T> &left, const CellPtr<U> &right) noexcept { return left.cmpPointers(right) != 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator!=(U left, const CellPtr<T> &right) noexcept { return right.cmpObjects(left) != 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator!=(const CellPtr<T> &left, U right) noexcept { return left.cmpObjects(right) != 0; }

template <typename T, typename U>
bool operator<(const CellPtr<T> &left, const CellPtr<U> &right) noexcept { return left.cmpObjects(right) < 0; }

#if !CELLPTR_CAST_TO_PTR
template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator<(U left, const CellPtr<T> &right) noexcept { return right.cmpObjects(left) > 0; }
#endif

template <typename T, typename U,
         typename std::enable_if<CellPtrBase::is_pointer<U>(), bool>::type = true>
bool operator<(const CellPtr<T> &left, U right) noexcept { return left.cmpObjects(right) < 0; }

//

//! A cast for unique pointers, used to downcast to a derived type iff we're certain
//! the cell is indeed of a derived type
template<typename Derived, typename Base>
std::unique_ptr<Derived> static_unique_ptr_cast(std::unique_ptr<Base>&& p) noexcept
{
  auto d = static_cast<Derived *>(p.release());
  return std::unique_ptr<Derived>(d);
  // Note: We don't move the deleter, since it's not special.
}

//! A cast for unique pointers, used to downcast to a derived type in a type-safe
//! manner.
template<typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_unique_ptr_cast(std::unique_ptr<Base>&& p) noexcept
{
  auto d = dynamic_cast<Derived *>(p.get());
  if (d)
    p.release();
  return std::unique_ptr<Derived>(d);
  // Note: We don't move the deleter, since it's not special.
}

#endif // CELLPTR_H
