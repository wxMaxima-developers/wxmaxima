// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Defines where a number gets its digit-group gaps, see DigitGrouping.h.
*/

#include "DigitGrouping.h"
#include <clocale>

namespace wxm {

namespace {
bool IsAsciiDigit(wxUniChar c) { return (c >= wxS('0')) && (c <= wxS('9')); }

bool IsSign(wxUniChar c) {
  return (c == wxS('-')) || (c == wxS('+')) || (c == wxUniChar(0x2212));
}
} // namespace

int LocaleDigitGroupSize() {
  const struct lconv *lc = localeconv();
  if (lc && lc->grouping && ((lc->grouping[0] == 3) || (lc->grouping[0] == 4)))
    return lc->grouping[0];
  return 3;
}

std::vector<size_t> DigitGroupGaps(const wxString &number, size_t minDigits,
                                   size_t groupSize) {
  std::vector<size_t> gaps;
  if (groupSize == 0)
    return gaps;
  const size_t len = number.Length();

  size_t pos = 0;
  while ((pos < len) && IsSign(number[pos]))
    pos++;

  // The integer part: [intStart, intEnd)
  const size_t intStart = pos;
  while ((pos < len) && IsAsciiDigit(number[pos]))
    pos++;
  const size_t intEnd = pos;

  // The fractional part: [fracStart, fracEnd)
  size_t fracStart = pos;
  if ((pos < len) && (number[pos] == wxS('.'))) {
    pos++;
    fracStart = pos;
    while ((pos < len) && IsAsciiDigit(number[pos]))
      pos++;
  }
  const size_t fracEnd = pos;

  const size_t intDigits = intEnd - intStart;
  const size_t fracDigits = fracEnd - fracStart;
  if ((intDigits + fracDigits < minDigits) || (intDigits + fracDigits == 0))
    return gaps;

  // Anything directly after the mantissa that isn't an exponent marker means
  // this isn't a plain number (a hexadecimal one with obase > 10, say, or a
  // digit-led identifier): better to leave it alone than to group half of it.
  if (pos < len) {
    const wxUniChar c = number[pos];
    if (!((c == wxS('e')) || (c == wxS('E')) || (c == wxS('b')) ||
          (c == wxS('B')) || (c == wxS('d')) || (c == wxS('D'))))
      return gaps;
  }

  for (size_t i = intStart + 1; i < intEnd; i++)
    if ((intEnd - i) % groupSize == 0)
      gaps.push_back(i);
  for (size_t i = fracStart + groupSize; i < fracEnd; i += groupSize)
    gaps.push_back(i);
  return gaps;
}

} // namespace wxm
