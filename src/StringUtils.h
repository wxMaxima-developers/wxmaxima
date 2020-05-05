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
#include <vector>

//! An empty string that can be passed via const reference.
extern const wxString wxmEmptyString;

/* General Notes on wxString
 *
 * 1. When wxString uses wchar_t for Unicode, the Empty() and Clear() both call
 *    underlying std::basic_string<wchar_t>::clear(). This is an incorrect implementation
 *    - instead, Truncate(0) should be called, eventually invoking std::basic_string::erase().
 *
 * 2. wxString always copies on construction. When a string literal (whether char[] or
 *    wxchar_t[]) is used to initialize the string, the string is repeatedly appended
 *    to, since the length of the initializer is not known. This causes reallocations and
 *    prevents optimized implementations from using e.g. memcpy to efficiently copy
 *    the data into the string.
 *
 * Workarounds:
 *
 * 1. string << Empty() is used instead of the Empty() method. Empty() is an empty tag
 *    class, and the operator<<(wxString &, Empty) invokes Truncate(0).
 *
 * 2. On initialization, the fasT("foo") macro expands to `ptr, length`, thus letting
 *    the string know the initializer length and do a single allocation followed by
 *    a fast copy. Example:
 *    wxString foo{fasT("bar")};
 *
 *    On assignment, the situation is more dire: there's no way to externally override
 *    the wxString::operator=. Assigning wxT("foo") is OK for short strings - it's a
 *    short copy, so optimizations don't yield big improements.
 *
 *    When appending, the operator+, operator+= and operator<< are overloaded for both
 *    wxString and std::basic_string to accept the rvalue of a lightweight
 *    (pointer, length) wrapper class StringLiteral. This class is instantiated by the
 *    liT("foo") macro. Upon appending, it calls append(ptr, length) on the underlying
 *    string (whether wxString or std::basic_string). This avoids reallocations as the
 *    source string is scanned and assigned from character-by-character. The literal is
 *    effectively memcpy-ied into the string's storage, allocated to correct length.
 */

//! A non-owning, non-reference counting temporary view of a section of a string
using StringView = wxScopedCharTypeBuffer<wxStringCharType>;

/*! This is a string substitution table, keyed with single characters.
 *
 * It consists of pairs of a character with a replacement string.
 * The table must be terminated by a default-constructed element.
 */
struct CharToStringEntry
{
  wxUniChar match = '\0';
  wxString replacement;
};

using CharToStringTable = const CharToStringEntry[];

/*! This is a character substitution table.
 *
 * It consists of pairs with character to replace followed by the replacement character.
 * The table must be terminated by default-constructed element.
 */
struct CharToCharEntry
{
  wxUniChar match = '\0';
  wxUniChar replacement;
};
using CharToCharTable = const CharToCharEntry[];

//! Return the input string transformed with substitutions in the substitution table
wxString TableReplaced(const wxString &input, CharToStringTable table, int *count = nullptr);
wxString TableReplaced(const wxString &input, CharToCharTable table, int *count = nullptr);

//! Transform the string by substituting using the substitution table.
//! Returns the number of replacements performed.
int TableReplace(wxString &string, CharToStringTable table);
int TableReplace(wxString &string, CharToCharTable table);

//! Return the replacement for the entire input string with value from the substitution table,
//! or empty string if none found.
wxString TableReplacedWhole(const wxString &input, CharToStringTable table);
wxString TableReplacedWhole(const wxString &input, CharToCharTable table);

namespace StringUtils
{
extern const CharToStringTable HTMLEscapes;
}

//! Return a string with escaping applied to all chars that cannot be used in HTML
inline wxString EscapedHTMLChars(const wxString &input)
{
  return TableReplaced(input, StringUtils::HTMLEscapes);
}

//! Escape all chars that cannot be used in HTML
inline void EscapeHTMLChars(wxString &input)
{
  TableReplace(input, StringUtils::HTMLEscapes);
}

//! A macro used to wrap a string literal that can be appended to a wxString and
//! basic_string as cheaply as possible - without a length scan..
#define liT(x) (StringLiteral{wxT(x)})

//! A macro used to wrap a string literal for creation of a wxString without the
//! need for a length scan.
#define fasT(x) (wxT(x)), (sizeof(wxT(x))/sizeof(wxT(' ')))

//! A length-carrying wrapper for string literals.
struct StringLiteral
{
  const wxStringCharType *ptr = {};
  size_t len = 0;

  using std_string = std::basic_string<wxStringCharType>;
  template <size_t N>
  constexpr StringLiteral(const wxStringCharType (&literal)[N]) : ptr(literal), len(N-1) {}
  explicit StringLiteral(nullptr_t) {}
  StringLiteral() = delete;
  StringLiteral(const StringLiteral &o) = delete;
  StringLiteral(StringLiteral &&o) : ptr(o.ptr), len(o.len) {}
  StringLiteral &operator=(const StringLiteral &o) = delete;
  StringLiteral &operator=(StringLiteral &&o) { ptr = o.ptr; len = o.len; return *this; }
  operator wxString() && { return wxString(ptr, len); }
};

//! Appends a string literal wrapped with liT().
inline wxString &operator<<(wxString &str, StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

//! Appends a string literal wrapped with liT().
inline wxString &operator+=(wxString &str, StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

//! Appends a string literal wrapped with liT().
inline wxString &operator+(wxString &str, StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

//! Appends a string literal wrapped with liT().
inline StringLiteral::std_string &operator<<(StringLiteral::std_string &str,
                                             StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

//! Appends a string literal wrapped with liT().
inline StringLiteral::std_string &operator+=(StringLiteral::std_string &str,
                                             StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

//! Appends a string literal wrapped with liT().
inline StringLiteral::std_string &operator+(StringLiteral::std_string &str,
                                            StringLiteral &&lit)
{
  return str.append(lit.ptr, lit.len);
}

struct Empty {};

/*! Empties the string by truncating it.
 *
 * On most C++ library implementations, it won't free the storage allocated by the string.
 * Use the string << Empty() pattern to avoid reallocations when the string is being
 * reused, especially in loops.
 *
 * This is a workaround for mis-implemented wxString::Empty() when it's using wchar_t.
 */
inline wxString &operator<<(wxString & str, Empty)
{
  return str.Truncate(0);
}

/*! A convenience operator that empties the string by truncating it.
 *
 * Usage: string << Empty();
 */
inline StringLiteral::std_string &operator<<(StringLiteral::std_string &str, Empty)
{
  return str.erase(0);
}

struct Reserve {
  size_t n;
  Reserve(size_t n) : n(n) {}
  Reserve(const wxString &str, int delta) : n(str.size() + delta) {}
};

/*! A convenience operator that reserves space in the string.
 *
 * Usage: string << Reserve(10); or string << Reserve(sizeReference, -3);
 */
inline wxString &operator<<(wxString &str, Reserve r)
{
  str.reserve(r.n);
  return str;
}

/*! A convenience operator that reserves space in a string rvalue.
 *
 * Usage: wxString() << Reserve(10); or wxString() << Reserve(sizeReference, -3);
 */
inline wxString &&operator<<(wxString &&str, Reserve r)
{
  str.reserve(r.n);
  return std::move(str);
}

/*! A convenience operator that reserves space in a string rvalue.
 *
 * Usage: std::string() << Reserve(10); or std::string() << Reserve(sizeReference, -3);
 */
inline StringLiteral::std_string &&operator<<(StringLiteral::std_string &&str, Reserve r)
{
  str.reserve(r.n);
  return std::move(str);
}

/*! Creates a temporary view of a string between two indices, without copying.
 *
 * The view is not subject to reference counting, and the string must outlive the
 * view, or undefined behavior occurs. The \arg to index is the index of
 * the last character.
 */
inline StringView ViewRange(const wxString &string, size_t from, size_t to)
{
  wxASSERT(to >= from);
  return StringView::CreateNonOwned(string.wx_str() + from, 1 + to - from);
}

/*! Creates a temporary view of the left substring of a string, without copying.
 *
 * Equivalent to wxString::Right, but without copying. \arg count can be larger
 * than the string length - it will be adjusted down.
 */
inline StringView ViewLeft(const wxString &string, size_t count)
{
  auto length = string.length();
  if (count > length) count = length;
  return StringView::CreateNonOwned(string.wx_str(), count);
}

/*! Creates a temporary view of the right substring of a string, without copying.
 *
 * Equivalent to wxString::Right, but without copying. \arg count can be larger
 * than the string length - it will be adjusted down.
 */
inline StringView ViewRight(const wxString &string, size_t count)
{
  auto length = string.length();
  if (count > length) count = length;
  return StringView::CreateNonOwned(string.wx_str() + (length - count), count);
}

//
// String Modifications
//

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

//
// Iterator Operations
//

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

//
// Comparisons
//

//! Returns whether the given character is equal to one of the other arguments,
//! checked in order.
template <typename Arg, typename ...Args>
inline bool IsOneOf(wxUniChar ch, Arg arg, Args...args)
{
  return ch == arg || IsOneOf(ch, (args)...);
}

inline bool IsOneOf(wxUniChar) { return false; }

//! Returns whether a range starts with a given string.
inline bool StartsWith(wxString::const_iterator begin, wxString::const_iterator end, const wxString &needle)
{
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*begin++ != *nBegin++) return false;
  }
  return true;
}

//! Returns whether a string starts with a given character.
inline bool StartsWith(const wxString &str, wxUniChar c)
{
  return !str.IsEmpty() && *str.begin() == c;
}

//! Returns whether a string starts with a given string literal.
inline bool StartsWith(const wxString &str, StringLiteral &&needle)
{
  if (needle.len > str.size()) return false;
  return str.compare(0, needle.len, needle.ptr, needle.len) == 0;
}

//! Returns whether a range ends with a given string.
inline bool EndsWith(wxString::const_iterator begin, wxString::const_iterator end, const wxString &needle)
{
  auto nBegin = needle.begin(), nEnd = needle.end();
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*--end != *--nEnd) return false;
  }
  return true;
}

//! Returns whether a string ends with a given character.
inline bool EndsWith(const wxString &str, wxUniChar c)
{
  return !str.IsEmpty() && str.end()[-1] == c;
}

//! Returns whether a string ends with a given string literal.
inline bool EndsWith(const wxString &str, StringLiteral &&needle)
{
  if (needle.len > str.size()) return false;
  return str.compare(str.size() - needle.len, needle.len, needle.ptr, needle.len) == 0;
}

//
// String Editing (WIP)
//

#if 0
struct EditOp
{
  enum OpFlags {
    OpInsert = 1 << 0,
    OpMask = 0xFF << 0,
    IdxOriginal = 0 << 8,
    IdxMask = 0xFF << 8,
    TypeWxChar = 1 << 16,
    TypeWxCharEnc = 2 << 16,
    TypeWxStringP = 3 << 16,
    TypeLiteral = 4 << 16,
    TypeMask = 0xFF << 16
  };
  int flags, index;
  union {
    wxUniChar ch;
    decltype(wxStringOperations::EncodeChar(0)) chEnc;
    const wxString* strP;
    StringLiteral lit;
  };
  EditOp(OpFlags op, OpFlags idx, int index, wxUniChar ch) :
      flags((op&OpMask) | (idx&IdxMask)), index(index)
  {
    if (wxStringOperations::IsSingleCodeUnitCharacter(ch))
      flags |= TypeWxChar, this->ch = ch;
    else
      flags |= TypeWxCharEnc, chEnc = wxStringOperations::EncodeChar(ch);
  }
  EditOp(OpFlags op, OpFlags idx, int index, const wxString &str) :
      flags((op&OpMask) | (idx&IdxMask) | TypeWxStringP), index(index), strP(&str) {}
  EditOp(OpFlags op, OpFlags idx, int index, StringLiteral &&lit) :
      flags((op&OpMask) | (idx&IdxMask) | TypeLiteral), index(index), lit(std::move(lit)) {}
};

/*! Edits the string by applying editing operations to it.
 * Minimum amount of data motion is performed.
 */
void EditString(wxString &, std::initializer_list<EditOp>);
#endif

#endif // WXMAXIMA_STRINGUTILS_H
