// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Everyone is permitted to copy and distribute verbatim copies
//  of this licence document, but changing it is not allowed.
//
//                       WXWINDOWS LIBRARY LICENCE
//     TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public Licence as published by
//  the Free Software Foundation; either version 2 of the Licence, or (at your
//  option) any later version.
//
//  This library is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//  Licence for more details.
//
//  You should have received a copy of the GNU Library General Public Licence
//  along with this software, usually in a file named COPYING.LIB.  If not,
//  write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
//  Floor, Boston, MA 02110-1301 USA.
//
//  EXCEPTION NOTICE
//
//  1. As a special exception, the copyright holders of this library give
//  permission for additional uses of the text contained in this release of the
//  library as licenced under the wxWindows Library Licence, applying either
//  version 3.1 of the Licence, or (at your option) any later version of the
//  Licence as published by the copyright holders of version 3.1 of the Licence
//  document.
//
//  2. The exception is that you may use, copy, link, modify and distribute
//  under your own terms, binary object code versions of works based on the
//  Library.
//
//  3. If you copy code from files distributed under the terms of the GNU
//  General Public Licence or the GNU Library General Public Licence into a
//  copy of this library, as this licence permits, the exception does not apply
//  to the code that you add in this way.  To avoid misleading anyone as to the
//  status of such modified files, you must delete this exception notice from
//  such code and/or adjust the licensing conditions notice accordingly.
//
//  4. If you write modifications of your own for this library, it is your
//  choice whether to permit this exception to apply to your modifications.  If
//  you do not wish that, you must delete the exception notice from such code
//  and/or adjust the licensing conditions notice accordingly.
//
//  SPDX-License-Identifier: wxWindows

#ifndef WXMAXIMA_FONTATTRIBS_H
#define WXMAXIMA_FONTATTRIBS_H

#include "EnumWrapper.h"
#include "stx/clamp.hpp"
#include <wx/font.h>
#include <wx/version.h>
#include <algorithm>
#include <cstdint>
#include <functional>
#include <limits>

using AFontEncoding = EnumWrapper<wxFontEncoding, int16_t, wxFONTENCODING_DEFAULT>;
using AFontFamily = EnumWrapper<wxFontFamily, int16_t, wxFONTFAMILY_DEFAULT>;
using AFontStyle = EnumWrapper<wxFontStyle, int16_t, wxFONTSTYLE_NORMAL>;
using AFontWeight = EnumWrapper<wxFontWeight, int16_t, wxFONTWEIGHT_NORMAL>;

/*! A Type-Safe Fixed-Point Font Size
 *
 * The use of this type enforces type-safety: a font size isn't just any other
 * float. Additionally, the built-in comparison of floating point types does not
 * behave as one would expect, whereas this type is comparable without any gotchas.
 *
 * The size default-constructs to a null value (AFontSize().IsNull() is true).
 *
 * The range of font size is Minimum_Size (4.0) to Maximum_Size (1638.35),
 * inclusive.
 *
 * It is an invariant: the numerical value (available via Get(), GetAsLong(),
 * and operators that convert to double) is always within that range.
 * The Set() operation clamps the value to these two limits, and cannot be used
 * to set the null value.
 *
 * To null the size, assign the default value or Clear() it.
 *
 * By the time the getters are invoked, we know that a valid font size is
 * needed, even if only to gracefully degrade to a small displayed text size instead
 * of nastier bugs. I.e. Get() and GetAsLong() on a null font size are valid and
 * return Minimum_Size, but such use is not correct: if null size is expected,
 * e.g. as a default from the configuration system, null check should be done before
 * accessing the numerical value.
 *
 * To check if a font size is null, use IsNull(), or its inverse IsValid().
 */
class AFontSize final
{
  constexpr static float Size_Unit = 0.05f;

public:
  using value_type = int16_t;

  constexpr static float Minimum_Size = 4.0f;
  constexpr static float Maximum_Size = std::numeric_limits<value_type>::max() * Size_Unit;

  constexpr AFontSize() = default;
  constexpr explicit AFontSize(float size) : m_uSize(ToUSize(size)) {}
  constexpr AFontSize(AFontSize minimum, double size) : m_uSize(std::max(minimum.m_uSize, ToUSize(float(size)))) {}
  constexpr AFontSize(AFontSize minimum, AFontSize size) : m_uSize(std::max(minimum.m_uSize, size.m_uSize)) {}
  constexpr AFontSize(const AFontSize &o) = default;

  constexpr void Set(float size) { m_uSize = ToUSize(size); }
  constexpr void Clear() { m_uSize = {}; }
  // Old cppcheck bugs:
  // cppcheck-suppress operatorEq
  constexpr AFontSize &operator=(const AFontSize &o) = default;

  constexpr bool operator==(AFontSize o) const { return m_uSize == o.m_uSize; }
  constexpr bool operator!=(AFontSize o) const { return m_uSize != o.m_uSize; }
  constexpr bool operator<(AFontSize o) const { return m_uSize < o.m_uSize; }
  constexpr bool operator>(AFontSize o) const { return m_uSize > o.m_uSize; }
  constexpr float Get() const { return IsValid() ? m_uSize * Size_Unit : Minimum_Size; }
  constexpr long GetAsLong() const { return long(Get() + 0.5f); }
  constexpr auto GetForWX() const;
  constexpr bool IsNull() const { return !IsValid(); }
  constexpr bool IsValid() const { return m_uSize > 0; }
  constexpr bool IsMinimal() const { return m_uSize == ToUSize(Minimum_Size); }

  struct Equals {
    bool operator()(AFontSize l, AFontSize r) const { return l == r; }
  };

private:
  friend struct std::hash<AFontSize>;
  value_type m_uSize = {};
  constexpr static value_type ToUSize(float size);
};

template <> struct std::hash<AFontSize> final
{
  size_t operator()(AFontSize name) const { return std::hash<int16_t>()(name.m_uSize); }
};

constexpr double operator*(double factor, AFontSize size)   { return factor * size.Get(); }
constexpr double operator*(AFontSize size, double factor)   { return size.Get() * factor; }
constexpr double operator/(double dividend, AFontSize size) { return dividend / size.Get(); }
constexpr double operator/(AFontSize size, double divisor)  { return size.Get() / divisor; }
constexpr double operator+(double offset, AFontSize size)   { return offset + size.Get(); }
constexpr double operator+(AFontSize size, double offset)   { return size.Get() + offset; }
constexpr double operator-(double offset, AFontSize size)   { return offset - size.Get(); }
constexpr double operator-(AFontSize size, double offset)   { return size.Get() - offset; }
constexpr AFontSize operator-=(AFontSize &size, double factor) { return size.Set(float(size - factor)), size; }

constexpr AFontSize::value_type AFontSize::ToUSize(float size)
{
  return AFontSize::value_type(stx::clamp(size, Minimum_Size, Maximum_Size) / Size_Unit + 0.5f);
}

//! Get the numerical value suitable for passing to wxFont/wxFontInfo.
constexpr auto AFontSize::GetForWX() const
{
#if wxCHECK_VERSION(3,1,2)
  return Get();
#else
  return GetAsLong();
#endif
}

#endif
