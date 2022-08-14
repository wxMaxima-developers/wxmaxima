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

#ifndef WXMAXIMA_CACHEDVALUE_H
#define WXMAXIMA_CACHEDVALUE_H

/*! \file
 * Declares "mostly read-only" wrappers around values that
 * are expensive to compute and are thus cached.
 */

#include <wx/debug.h>
#include <wx/log.h>
#include <limits>
#include <type_traits>

/*! A cached integer value
 *
 * The value can be used in contexts where the integer could be used,
 * but is "mostly" read-only: it can not be assigned to, but can be
 * SetCached(). This is to enforce a protocol where a dedicated method updates
 * the cached value, but elsewhere the value is only read from.
 * Attempting to access an invalid value is checked for.
 */
template <typename T, typename std::enable_if<std::is_integral<T>::value, bool>::type = true>
class CachedInteger
{
  static constexpr T invalid = std::numeric_limits<T>::max();
  mutable T m_value = invalid;

public:
  constexpr CachedInteger() = default;
  constexpr CachedInteger(const CachedInteger &) = default;
  constexpr CachedInteger &operator=(const CachedInteger &) = default;
  constexpr bool IsValid() const { return m_value != invalid; }
  constexpr bool IsInvalid() const { return m_value == invalid; }
  constexpr void Invalidate() { m_value = invalid; }
  operator T() const { return Get(); }
  T Get() const
    {
      wxASSERT_MSG(m_value != invalid, "Attempted to use an invalid cached value");
      return (m_value != invalid) ? m_value : T{};
    }
  void SetCached(T newValue) const
    {
      wxASSERT_MSG(newValue != invalid, "Attempted to set an out-of-range cached value.");
      m_value = newValue;
    }
};

#endif
