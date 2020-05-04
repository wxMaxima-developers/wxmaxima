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

#ifndef WXMAXIMA_STRINGUTILS_H
#define WXMAXIMA_STRINGUTILS_H

#include <wx/string.h>
#include <wx/wxcrt.h>
#include <algorithm>

/*! Appends a given number of initial characters of the source string.
 *! Negative lengths are relative to the source string length.
 *
 * Equivalent to but faster than:
 * - for positive lengths: dst += src.Left(length);
 * - for negative lengths: dst += src.Left(src.size() + length);
 */
inline wxString &AppendLeft(wxString &dst, const wxString &src, ssize_t length)
{
  auto srcSize = ssize_t(src.size());
  if (length > srcSize) length = srcSize;
  else if (length < -srcSize) length = -srcSize;
  return
    (length >= 0) ? dst.append(src.begin(), src.begin() + length)
                  : dst.append(src.begin(), src.end() + length);
}

/*! Appends a given number of final characters of the source string.
 *! Negative lengths are relative to the source string length.
 *
 * Equivalent to but faster than:
 * - for positive lengths: dst += src.Right(length);
 * - for negative lengths: dst += src.Right(src.size() + length);
 */
inline wxString &AppendRight(wxString &dst, const wxString &src, ssize_t length)
{
  auto srcSize = ssize_t(src.size());
  if (length > srcSize) length = srcSize;
  else if (length < -srcSize) length = -srcSize;
  return
    (length >= 0) ? dst.append(src.end() - length, src.end())
                  : dst.append(src.begin() - length, src.end());
}

//! Shortens given string by a given number of characters.
inline wxString &Shorten(wxString &str, size_t shortenBy)
{
  if (shortenBy >= str.length())
    return str.Truncate(0);
  return str.Truncate(str.size() - shortenBy);
}

//! Returns the begin iterator advanced as far as the predicate holds
template <typename Pred>
wxString::const_iterator AdvanceIf(wxString::const_iterator begin, wxString::const_iterator end, Pred pred)
{
  while (begin != end && pred(*begin))
    ++ begin;
  return begin;
}

//! Returnd the end iterator retracted as far as the predicate holds
template <typename Pred>
wxString::const_iterator RetractIf(wxString::const_iterator begin, wxString::const_iterator end, Pred pred)
{
  while (begin != end)
  {
    auto nextEnd = end;
    if (!pred(*-- nextEnd)) break;
    end = nextEnd;
  }
  return end;
}

//! Returns the begin iterator advanced over whitespace
inline wxString::const_iterator AdvanceTrim(wxString::const_iterator begin, wxString::const_iterator end)
{
  return AdvanceIf(begin, end, wxIsspace);
}

//! Returns the end iterator retracted over whitespace
inline wxString::const_iterator RetractTrim(wxString::const_iterator begin, wxString::const_iterator end)
{
  return RetractIf(begin, end, wxIsspace);
}

//! Advances the begin iterator over one completely matching string, or leaves it unchanged.
inline bool AdvanceOverOne(wxString::const_iterator &ioBegin, wxString::const_iterator end, const wxString &needle)
{
  auto begin = ioBegin;
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*begin++ != *nBegin++) return false;
  }
  ioBegin = begin;
  return true;
}

//! Retracts the end iterator over one completely matching string, or leaves it unchanged.
inline bool RetractOverOne(wxString::const_iterator begin, wxString::const_iterator &ioEnd, const wxString &needle)
{
  auto end = ioEnd;
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*--end != *--nEnd) return false;
  }
  ioEnd = end;
  return true;
}

//! Retracts the end iterator over matching characters
inline bool RetractOver(wxString::const_iterator begin, wxString::const_iterator &ioEnd, const wxChar ch)
{
  auto end = ioEnd;
  if (begin == end || *--end != ch) return false;
  if (begin != end)
  {
    while (*--end == ch && begin != end);
    ++end;
  }
  ioEnd = end;
  return true;
}

//! Returns whether the range starts with a given string.
inline bool RangeStartsWith(wxString::const_iterator begin, wxString::const_iterator end, const wxString &needle)
{
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*begin++ != *nBegin++) return false;
  }
  return true;
}

//! Returns whether the range ends with a given string.
inline bool RangeEndsWith(wxString::const_iterator begin, wxString::const_iterator end, const wxString &needle)
{
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*--end != *--nEnd) return false;
  }
  return true;
}

//! Returns whether the given character is equal to one of the other arguments,
//! checked in order.
template <typename Arg, typename ...Args>
inline bool IsOneOf(wxUniChar ch, Arg arg, Args...args)
{
  return ch == arg || IsOneOf(ch, (args)...);
}

inline bool IsOneOf(wxUniChar) { return false; }

#endif // WXMAXIMA_STRINGUTILS_H
