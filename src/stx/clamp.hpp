// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Use, modification, and distribution is subject to the Boost Software
//  License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
//  SPDX-License-Identifier: BSL-1.0

#ifndef WXMAXIMA_UTILS_CLAMP_H
#define WXMAXIMA_UTILS_CLAMP_H

#if __cplusplus >= 201703L

#include <algorithm>

namespace stx {

using std::clamp;

}

#else

#include <cassert>

namespace stx {

template <class T>
constexpr const T &clamp(const T &val, const T &min, const T &max)
{
  assert(!(max < min));
  return (val < min) ? min : (max < val) ? max : val;
}

template <class T, class Compare>
constexpr const T &clamp(const T &val, const T &min, const T &max, Compare comp)
{
  assert(!comp(max, min));
  return comp(val, min) ? min : comp(max, val) ? max : val;
}

}

#endif // __cplusplus < 201703L

#endif
