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
  Declares where a number gets the small gaps that separate its digit groups
  (GH #192), "2111230496" being displayed as "2 111 230 496".

  This only decides *where* the gaps go. They are a display matter only: the
  text a cell holds, copies, saves and exports never contains them, which is
  why this works on indices into the text rather than producing a new string.
*/

#ifndef DIGITGROUPING_H
#define DIGITGROUPING_H

#include <cstddef>
#include <utility>
#include <vector>
#include <wx/string.h>

namespace wxm {

/*! How many digits a group has: what the C library's LC_NUMERIC locale says,
  if that is 3 or 4, otherwise 3.

  Locales with irregular grouping (India's 12,34,56,789) get groups of their
  first size throughout, which is what LongNumberCell::BreakUp() has always
  done, too.
*/
int LocaleDigitGroupSize();

/*! The indices into number before which a digit-group gap is drawn, ascending.

  number is a number as Maxima prints it, optionally preceded by a sign
  (including the unicode minus sign MathParser turns "-" into): "12345",
  "−3.14159", "1.234567b10", "6.02214076e23".

  - The digits before the decimal point are grouped from the right, the ones
    after it from the decimal point on, so the groups always line up with the
    point: "12 345.678 9".
  - Nothing is grouped unless the mantissa (the digits before and after the
    point together) has at least minDigits digits, so a short number like a
    year (2026) stays as it is by default.
  - An exponent ("e-10", "b25") and anything else after the mantissa is never
    grouped, nor is anything that doesn't start with a (signed) digit or point.
*/
std::vector<size_t> DigitGroupGaps(const wxString &number, size_t minDigits,
                                   size_t groupSize = 3);

/*! DigitGroupGaps() for code, where a number may span several tokens.

  MaximaTokenizer makes "3.14159" three tokens, "3", "." and "14159", and
  ".5" two. Grouped one token at a time, the digits after the point would be
  grouped from the right, like an integer's; this joins such tokens up into
  the number they form first.

  tokens holds each token's text and whether it is a number token. The result
  has one entry per token: the gaps inside it, as indices into that token's
  own text. A gap never falls on a token boundary, so each gap belongs to
  exactly one token.
*/
std::vector<std::vector<size_t>> DigitGroupGapsInTokens(
  const std::vector<std::pair<wxString, bool>> &tokens, size_t minDigits,
  size_t groupSize = 3);

} // namespace wxm

#endif // DIGITGROUPING_H
