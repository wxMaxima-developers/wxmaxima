// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@bertec.com>
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

#ifndef STD_COMPATIBILITY_H
#define STD_COMPATIBILITY_H

#include <wx/debug.h>
#include <algorithm>

/** \file
 * Implements the missing facilities in the C++ standard library
 */

#if __cpp_lib_clamp
#else
// A missing C++17 feature
namespace std {
template<class T>
constexpr const T& clamp(const T& v, const T& lo, const T& hi)
{
  wxASSERT(!(hi < lo));
  return (v < lo) ? lo : (hi < v) ? hi : v;
}
};
#endif

#endif // STD_COMPATIBILITY_H
