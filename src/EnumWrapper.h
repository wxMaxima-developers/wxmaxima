// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#ifndef WXMAXIMA_ENUMWRAPPER_H
#define WXMAXIMA_ENUMWRAPPER_H

#include <cstddef>
#include <functional>
#include <type_traits>

/*! A wrapper for legacy enumerated types that can be stored in
 * a type smaller than an int.
 *
 * This wrapper is meant for storage as a class member.
 *
 * 1. *Uses outside of class members are unnecessary and a bug.*
 *
 * 2. *Passing this wrapper as an argument to a function/method
 *    is a pessimization. It's not meant to be used that way.*
 */
template <typename Enum, typename Storage, Enum defaultValue = Enum{},
          typename std::enable_if<std::is_enum<Enum>::value &&
                                      std::is_integral<Storage>::value,
                                  bool>::type = true>
class EnumWrapper {
  Storage value = Storage(defaultValue);

  // We can at least make sure that the default value fits
  static_assert(
      Enum(Storage(defaultValue)) == defaultValue,
      "The default value doesn't survive a roundtrip through this wrapper.");
  // There's no point in using this type if it doesn't save space.
  static_assert(
      sizeof(Storage) < sizeof(Enum),
      "EnumWrapper should be used only to save space."
      "When removing it, don't forget to take care of default value!");

public:
  constexpr EnumWrapper() = default;
  explicit constexpr EnumWrapper(Storage) = delete;
  // cppcheck-suppress noExplicitConstructor
  explicit constexpr EnumWrapper(Enum value) noexcept : value(value) {}
  constexpr operator Enum() const noexcept { return Enum(value); }
  constexpr operator Storage() const = delete;
  constexpr size_t hash() const { return std::hash<Storage>()(value); }

  constexpr bool operator==(EnumWrapper o) const { return value == o.value;  }
  constexpr bool operator==(Enum o) const { return value == o;  }
  constexpr bool operator!=(EnumWrapper o) const { return value != o.value;  }
  constexpr bool operator!=(Enum o) const { return value != o;  }
};

template <typename E, typename S, E defVal>
constexpr bool operator==(E a, EnumWrapper<E, S, defVal> b) { return b == a;  }

template <typename E, typename S, E defVal>
constexpr bool operator!=(E a, EnumWrapper<E, S, defVal> b) { return b != a;  }

namespace std {

template <typename Enum, typename Storage, Enum defaultValue>
struct hash<EnumWrapper<Enum, Storage, defaultValue>> {
  constexpr auto
  operator()(EnumWrapper<Enum, Storage, defaultValue> value) const {
    return value.hash();
  }
};

} // namespace std

#endif
