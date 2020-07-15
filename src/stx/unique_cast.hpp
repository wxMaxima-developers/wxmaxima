// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Use, modification, and distribution is subject to the Boost Software
//  License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
//  SPDX-License-Identifier: BSL-1.0

#ifndef STX_UNIQUE_CAST_HPP_INCLUDED
#define STX_UNIQUE_CAST_HPP_INCLUDED

#include <memory>

namespace stx {

// std::make_unique for non-array types is available since C++14
#if 0
template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args &&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}
#endif

//! A cast for unique pointers, used to downcast to a derived type iff we're
//! certain the cell is indeed of a derived type
template <typename Derived, typename Base>
std::unique_ptr<Derived> static_unique_ptr_cast(std::unique_ptr<Base> &&p) {
  auto d = static_cast<Derived *>(p.release());
  return std::unique_ptr<Derived>(d);
  // Note: We don't move the deleter, since it's not special.
}

//! A cast for unique pointers, used to downcast to a derived type in a
//! type-safe manner.
template <typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_unique_ptr_cast(std::unique_ptr<Base> &&p) {
  auto d = dynamic_cast<Derived *>(p.get());
  if (d)
    p.release();
  return std::unique_ptr<Derived>(d);
  // Note: We don't move the deleter, since it's not special.
}

}

#endif
