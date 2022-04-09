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

#include "CellPtr.h"
#include "GroupCell.h"

#define CELL_PRIXPTR "010" PRIXPTR

size_t Observed::m_instanceCount;
size_t Observed::ControlBlock::m_instanceCount;
size_t CellPtrBase::m_instanceCount;

void Observed::OnEndOfLife() noexcept
{
  // TODO Both cases are equivalent: we're resetting
  // a back-pointer pointed to by our pointer. In binary terms,
  // both operations are identical.

  auto *cellPtr = m_ptr.GetCellPtrBase();
  if (cellPtr)
  { // Reset the single CellPtr pointing to this cell
    cellPtr->reset();
  }
  else
  { // Reset the object in our control block.
    auto *cb = m_ptr.GetControlBlock();
    if (CELLPTR_LOG_REFS)
      CELLPTR_LOG_METHOD("-- ~Obs=%p cb=%p", this, cb);
    wxASSERT(cb);
    cb->reset();
  }
}

#if CELLPTR_LOG_REFS
void Observed::LogRef(const CellPtrBase *cellptr) const
{
  CELLPTR_LOG_METHOD("%p Obs::Ref obj=%p", cellptr, this);
}

void Observed::LogDeref(const CellPtrBase *cellptr) const
{
  CELLPTR_LOG_METHOD("%p Obs::Deref obj=%p", cellptr, this);
}
#endif

void CellPtrBase::Ref(Observed *obj)
{
  // References can only be set on null pointers
  wxASSERT(obj && !m_ptr);

  auto obj_ptr = obj->m_ptr;
  if (!obj_ptr)
  {
    // The object has no pointers pointing to it yet
    obj->LogRef(this);
    obj->m_ptr = this;
    m_ptr = obj;
    return;
  }
  auto *otherCellPtr = obj_ptr.GetCellPtrBase();
  if (otherCellPtr)
  {
    // The object claims that it has a single pointer pointing to it

    // The pointer should indeed point at that object.
    wxASSERT(otherCellPtr->m_ptr.GetObserved() == obj);
    auto *const cb = new ControlBlock(obj);
    obj->LogDeref(otherCellPtr);
    obj->m_ptr = cb;
    otherCellPtr->m_ptr = cb->Ref(this);
    this->m_ptr = cb->Ref(this);
    return;
  }
  // The object has multiple pointers pointing to it
  auto *const cb = obj_ptr.GetControlBlock();
  wxASSERT(cb && cb->Get() == obj);
  this->m_ptr = cb->Ref(this);
}

void CellPtrBase::Deref() noexcept
{
  if (!m_ptr)
    return;

  auto *observed = m_ptr.GetObserved();
  if (observed)
  {
    // We're the sole pointer to the observed

    // The observed should point back to this pointer
    auto *const backPtr = observed->m_ptr.GetCellPtrBase();
    wxASSERT(backPtr == this);
    observed->m_ptr = nullptr;
    m_ptr = nullptr;
    return;
  }

  DerefControlBlock();
}

decltype(nullptr) CellPtrBase::DerefControlBlock() const noexcept
{
  // The object has multiple pointers pointing to it
  auto *const cb = m_ptr.GetControlBlock();
  wxASSERT(cb);
  if (!cb->Deref(this))
  {
    // The last reference to the object has been lost.
    // If there's an object, clear its reference to the control block (it's gone now)
    if (cb->Get())
      cb->Get()->m_ptr = nullptr;
    delete cb;
  }
  m_ptr = nullptr;
  return nullptr;
}

#if CELLPTR_LOG_INSTANCES
void CellPtrBase::LogConstruction(Observed *obj) const
{
  CELLPTR_LOG_METHOD("%p->CellPtr(%p) ptr=%" CELL_PRIXPTR, this, obj, m_ptr.GetImpl());
}

void CellPtrBase::LogMove(const CellPtrBase &o) const
{
  CELLPTR_LOG_METHOD("%p->Cellptr(&&%p) ptr=%" CELL_PRIXPTR "<->%" CELL_PRIXPTR, this, &o, m_ptr.GetImpl(), o.m_ptr.GetImpl());
}

void CellPtrBase::LogAssignment(const CellPtrBase &o) const
{
  CELLPTR_LOG_METHOD("%p->CellPtr::operator=(&&%p) ptr=%" CELL_PRIXPTR "<->%" CELL_PRIXPTR, this, &o, m_ptr.GetImpl(), o.m_ptr.GetImpl());
}

void CellPtrBase::LogDestruction() const
{
  CELLPTR_LOG_METHOD("%p->~CellPtr() ptr=%" CELL_PRIXPTR " obj=%p", this, m_ptr.GetImpl(), base_get());
}
#endif

void CellPtrBase::base_reset(Observed *obj) noexcept
{
  if (obj != base_get())
  {
    //! Different objects must have control blocks that are either null or non-null but different.
    //wxASSERT(!obj || !m_cb || !obj->m_cb || (m_cb != obj->m_cb));
    auto const prevPtr = m_ptr.GetImpl();
    Deref();
    if (obj) Ref(obj);
    auto const curPtr = m_ptr.GetImpl();
    if (CELLPTR_LOG_REFS)
      CELLPTR_LOG_METHOD("%p->CellPtr::reset(%p->%p) ptr=%08" PRIXPTR "->%08" PRIXPTR, this, base_get(), obj, prevPtr, curPtr);
  } else {
    // The objects are the same - their control blocks must be the same as well.
    //wxASSERT(!obj || (m_cb == obj->m_cb));
  }
}

// This is a specialization of this method. It's useful when GroupCell
// is not a fully defined class, but someone wants to use the methods of
// CellPtr<GroupCell>.
template <>
CellPtr<GroupCell>::pointer CellPtr<GroupCell>::get() const noexcept
{ return static_cast<pointer>(base_get()); }

#if CELLPTR_LOG_REFS

void Observed::ControlBlock::LogConstruct(const Observed *p) const
{
  CELLPTR_LOG_METHOD("---------------- CB::CB  cb=%p obj=%p", this, p);
}

void Observed::ControlBlock::LogRef(const CellPtrBase *cellptr) const
{
  CELLPTR_LOG_METHOD("%p CB::Ref (%d->%d) cb=%p obj=%p", cellptr, m_refCount, m_refCount+1, this, m_object);
}

void Observed::ControlBlock::LogDeref(const CellPtrBase *cellptr) const
{
  CELLPTR_LOG_METHOD("%p CB::Deref (%d->%d) cb=%p obj=%p", cellptr, m_refCount, m_refCount-1, this, m_object);
}

void Observed::ControlBlock::LogDestruct() const
{
  CELLPTR_LOG_METHOD("---------------- CB::~CB cb=%p", this);
}

#endif
