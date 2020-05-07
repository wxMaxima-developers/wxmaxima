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
//
//  Parts of StringView code are taken from http://cppreference.com
//  The following license applies to those parts:
//
//  This work is licensed under the Creative Commons Attribution-ShareAlike 3.0
//  Unported License. To view a copy of this license, visit
//  http://creativecommons.org/licenses/by-sa/3.0/ or send a letter to Creative
//  Commons, PO Box 1866, Mountain View, CA 94042, USA.
//
//  SPDX-License-Identifier: CC-BY-SA-3.0

#ifndef WXMAXIMA_STRINGUTILS_H
#define WXMAXIMA_STRINGUTILS_H

#include <wx/string.h>
#include <wx/wxcrt.h>
#include <algorithm>
#include <cwchar>
#include <stdexcept>
#include <vector>

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

//! A non-owning, non-reference counting temporary view of a section of a string,
//! essentially a std::string_view for C++11.
class StringView
{
public:
  using value_type = wxStringCharType;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using reference = value_type&;
  using const_reference = const value_type&;

  using const_iterator = const_pointer;
  using iterator = pointer;

  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  static constexpr size_type npos = size_type(-1);

  StringView() noexcept = default;
  StringView(const StringView& o) noexcept = default;
  StringView(const_pointer s, size_type length) : s(s), len(length) {}
  explicit  StringView(const_pointer s) : s(s), len(std::wcslen(s)) {}
  template <size_type N>
  StringView(const value_type (&&literal)[N]) : s(literal), len(N-1) {}
  template <class It>
  StringView(It first, It last) :
      s(&*first), len(std::distance(first, last)) {}
  StringView(const wxString &str) noexcept : s(str.wx_str()), len(str.size()) {}
  StringView(const std::basic_string<value_type> &str) noexcept : s(str.data()), len(str.size()) {}
  StringView &operator=(const StringView& o) noexcept = default;

  const_iterator begin() const noexcept { return s; }
  const_iterator cbegin() const noexcept { return s; }
  const_iterator end() const noexcept { return s + size(); }
  const_iterator cend() const noexcept { return s + size(); }

  const_reference operator[](size_type pos) const { return s[pos]; }
  const_reference at(size_type pos) const;
  const_reference front() const { return s[0]; }
  const_reference back() const { return s[size()-1]; }
  const_pointer data() const noexcept { return s; }

  size_type size() const noexcept { return len; }
  size_type length() const noexcept { return len; }
  size_type max_size() const noexcept { return std::numeric_limits<size_type>::max(); }

  bool empty() const noexcept { return !len; }

  void remove_prefix(size_type n) { s += n; }
  void remove_suffix(size_type n) { len -= n; }

  void swap(StringView& v) noexcept
  { StringView temp = *this; *this = v; v = temp; }

  size_type copy(pointer dest, size_type count, size_type pos = 0) const;

  StringView substr(size_type pos = 0, size_type count = npos) const;

  int compare(StringView v) const noexcept;
  int compare(size_type pos1, size_type count1, StringView v) const
  { return substr(pos1, count1).compare(v); }
  int compare(size_type pos1, size_type count1, StringView v,
              size_type pos2, size_type count2) const
  { return substr(pos1, count1).compare(v.substr(pos2, count2)); }
  int compare(const_pointer s) const { return compare(StringView(s)); }
  int compare(size_type pos1, size_type count1,
              const_pointer s) const
  { return substr(pos1, count1).compare(StringView(s)); }
  int compare(size_type pos1, size_type count1,
              const_pointer s, size_type count2) const
  { return substr(pos1, count1).compare(StringView(s, count2)); }

  bool starts_with(StringView v) const noexcept
  { return size() >= v.size() && v.compare(StringView(s, v.size())) == 0; }
  bool starts_with(value_type c) const noexcept
  { return !empty() && *s == c; }
  bool starts_with(const_pointer s) const;

  bool ends_with(StringView sv) const noexcept
  { return size() >= sv.size() && compare(size() - sv.size(), npos, sv) == 0; }
  bool ends_with(value_type c ) const noexcept
  { return !empty() && back() == c; }
  bool ends_with(const_pointer s) const
  { return ends_with(StringView(s)); }

  size_type find(StringView v, size_type pos = 0) const noexcept;
  size_type find(value_type ch, size_type pos = 0) const noexcept
  { return find(StringView(&ch, 1), pos); }
  size_type find(const_pointer s, size_type pos, size_type count) const
  { return find(StringView(s, count), pos); }
  size_type find(const_pointer s, size_type pos = 0) const
  { return find(StringView(s), pos); }

  size_type rfind(StringView v, size_type pos = npos) const noexcept;
  size_type rfind(value_type ch, size_type pos = npos) const noexcept
  { return rfind(StringView(&ch, 1), pos); }
  size_type rfind(const_pointer s, size_type pos, size_type count) const
  { return rfind(StringView(s, count), pos); }
  size_type rfind(const_pointer s, size_type pos = npos) const
  { return rfind(StringView(s), pos); }

  //! Finds the first occurrence of any of the characters in v in the view,
  //! starting at position pos.
  size_type find_first_of(StringView v, size_type pos = 0) const noexcept;
  size_type find_first_of(value_type ch, size_type pos = 0) const noexcept
  { return find(ch, pos); }
  size_type find_first_of(const_pointer s, size_type pos, size_type count) const noexcept
  { return find_first_of(StringView(s, count), pos); }
  size_type find_first_of(const_pointer s, size_type pos = 0) const;

  //! Finds the last occurrence of any of the characters in v in the view,
  //! starting at position pos.
  size_type find_last_of(StringView v, size_type pos = npos) const noexcept;
  size_type find_last_of(value_type ch, size_type pos = 0) const noexcept
  { return rfind(ch, pos); }
  size_type find_last_of(const_pointer s, size_type pos, size_type count) const noexcept
  { return find_last_of(StringView(s, count), pos); }
  size_type find_last_of(const_pointer s, size_type pos = 0) const
  { return find_last_of(StringView(s), pos); }

  //! Finds the first occurrence of a character not present in v in the view,
  //! starting at position pos.
  size_type find_first_not_of(StringView v, size_type pos = 0) const noexcept;
  size_type find_first_not_of(value_type ch, size_type pos = 0) const noexcept;
  size_type find_first_not_of(const_pointer s, size_type pos, size_type count) const noexcept
  { return find_first_not_of(StringView(s, count), pos); }
  size_type find_first_not_of(const_pointer s, size_type pos = 0) const;

private:
  const_pointer s = {};
  size_type len = 0;
};

bool operator==(StringView lhs, StringView rhs ) noexcept
{ return lhs.size() == rhs.size() && lhs.compare(rhs) == 0; }

bool operator!=(StringView lhs, StringView rhs ) noexcept
{ return lhs.size() != rhs.size() || lhs.compare(rhs) != 0; }

bool operator<(StringView lhs, StringView rhs ) noexcept
{ return lhs.compare(rhs) < 0; }

bool operator<=(StringView lhs, StringView rhs ) noexcept
{ return lhs.compare(rhs) <= 0; }

bool operator>(StringView lhs, StringView rhs ) noexcept
{ return lhs.compare(rhs) > 0; }

bool operator>=(StringView lhs, StringView rhs ) noexcept
{ return lhs.compare(rhs) >= 0; }


/*! This is a string substitution table, keyed with single characters.
 *
 * It consists of pairs of a character with a replacement string.
 * The table must be terminated by a default-constructed element.
 */
struct StringForCharSubstitution
{
  wxUniChar match = '\0';
  wxString replacement;
};

using StringForCharSubstitutions = const StringForCharSubstitution[];

/*! This is a character substitution table.
 *
 * It consists of pairs with character to replace followed by the replacement character.
 * The table must be terminated by default-constructed element.
 */
struct CharForCharSubstitution
{
  wxUniChar match = '\0';
  wxUniChar replacement;
};
using CharForCharSubstitutions = const CharForCharSubstitution[];

//! Return the input string transformed with substitutions in the substitution table
wxString ReplacedChars(const wxString &input, StringForCharSubstitutions substs);
wxString ReplacedChars(const wxString &input, CharForCharSubstitutions substs);

//! Transform the string by substituting using the substitution table
void ReplaceChars(wxString &string, StringForCharSubstitutions substs);
void ReplaceChars(wxString &string, CharForCharSubstitutions substs);

namespace StringUtils
{
extern const StringForCharSubstitutions HTMLEscapes;
}

//! Return a string with escaping applied to all chars that cannot be used in HTML
inline wxString EscapedHTMLChars(const wxString &input)
{
  return ReplacedChars(input, StringUtils::HTMLEscapes);
}

//! Escape all chars that cannot be used in HTML
inline void EscapeHTMLChars(wxString &input)
{
  ReplaceChars(input, StringUtils::HTMLEscapes);
}

//! \internal
#define stR_2(x) L##x

/*! A macro used to retain an instance of wxString without further reallocations.
 *
 * Use with "narrow" literals, e.g. stR('c'), stR("word"). Do not use with wide
 * literals, i.e. stR(L'c') is invalid.
 */
#define stR(x) ([]() -> const wxString & { \
  static const wxString _str_{stR_2(x)}; return _str_; \
}())

/*! A macro used to retain an instance of wxString without further reallocations.
 *
 * Use with "wide" literals, e.g. stL(L'c'), stL(L"word"). Do not use with narrow
 * literals, i.e. stL('c') is invalid.
 */
#define stL(x) ([]() -> const wxString & { \
  static_assert(std::is_same<decltype(x), wchar_t>::value \
                || !std::is_same<std::decay<decltype(x)>::type, const wchar_t *>::value, \
                "Use wide literals only with stL"); \
  static const wxString _str_{x}; return _str_; \
}())

//! A macro used to wrap a string literal for creation of a wxString without the
//! need for a length scan.
#define fasT(x) (wxT(x)), (sizeof(wxT(x))/sizeof(wxT(' ')))

#if 0
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
#endif

//! Appends a string literal without the need for a length scan.
template <size_t N>
inline wxString &operator<<(wxString &str, const wxStringCharType (&literal)[N])
{
  return str.append(literal, N-1);
}

//! Appends a string literal without the need for a length scan.
template <size_t N>
inline wxString &operator+=(wxString &str, const wxStringCharType (&literal)[N])
{
  return str.append(literal, N-1);
}

//! Appends a string literal without the need for a length scan.
template <size_t N>
inline wxString &operator+(wxString &str, const wxStringCharType (&literal)[N])
{
  return str.append(literal, N-1);
}

//! Appends a string literal without the need for a length scan.
template <size_t N, typename Char>
inline std::basic_string<Char> &operator<<(std::basic_string<Char> &str,
                                           const Char (&literal)[N])
{
  return str.append(literal, N-1);
}

//! Appends a string literal without the need for a length scan.
template <size_t N, typename Char>
inline std::basic_string<Char> &operator+=(std::basic_string<Char> &str,
                                           const Char (&literal)[N])
{
  return str.append(literal, N-1);
}

//! Appends a string literal without the need for a length scan.
template <size_t N, typename Char>
inline std::basic_string<Char> &operator+(std::basic_string<Char> &str,
                                          const Char (&literal)[N])
{
  return str.append(literal, N-1);
}

#if 0
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
#endif

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
template <typename Char>
inline std::basic_string<Char> &operator<<(std::basic_string<Char> &str, Empty)
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
template <typename Char>
inline std::basic_string<Char> &&operator<<(std::basic_string<Char> &&str, Reserve r)
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
// String Editing and Modifications
//

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
    struct {
      wchar_t *ptr;
      size_t len;
    } lit;
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


  template <size_t N>
  EditOp(OpFlags op, OpFlags idx, int index, const wxStringCharType (&literal)[N]) :
      flags((op&OpMask) | (idx&IdxMask) | TypeLiteral), index(index), lit{literal, N} {}
};

/*! Edits the string by applying editing operations to it.
 * Minimum amount of data motion is performed.
 * THIS CODE IS NOT READY FOR USE YET.
 */
void EditString(wxString &, std::initializer_list<EditOp>);

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
bool AdvanceOverOne(wxString::const_iterator &ioBegin, wxString::const_iterator end,
                    const wxStringCharType *needle, size_t length);

//! Advances the begin iterator over one completely matching string, or leaves it unchanged.
inline bool AdvanceOverOne(wxString::const_iterator &ioBegin, wxString::const_iterator end,
                           const wxString &needle)
{
  return AdvanceOverOne(ioBegin, end, needle.wx_str(), needle.size());
}

//! Advances the begin iterator over one completely matching string, or leaves it unchanged.
template <size_t N>
inline bool AdvanceOverOne(wxString::const_iterator &ioBegin, wxString::const_iterator end,
                           const wxStringCharType (&needle)[N])
{
  return AdvanceOverOne(ioBegin, end, needle, N);
}

//! Retracts the end iterator over one completely matching string, or leaves it unchanged.
bool RetractOverOne(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                    const wxStringCharType *needle, size_t length);

//! Retracts the end iterator over one completely matching string, or leaves it unchanged.
inline bool RetractOverOne(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                           const wxString &needle)
{
  return RetractOverOne(begin, ioEnd, needle.wx_str(), needle.size());
}

//! Retracts the end iterator over one completely matching string, or leaves it unchanged.
template <size_t N>
inline bool RetractOverOne(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                           const wxStringCharType (&needle)[N])
{
  return RetractOverOne(begin, ioEnd, needle, N);
}

//! Retracts the end iterator over matching characters
inline bool RetractOver(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                        const wxChar ch);

//
// Comparisons
//

//! Returns whether a range starts with a given string.
bool StartsWith(wxString::const_iterator begin, wxString::const_iterator end,
                const wxStringCharType *needle, size_t length);

//! Returns whether a range starts with a given string.
inline bool StartsWith(wxString::const_iterator begin, wxString::const_iterator end,
                       const wxString &needle)
{
  return StartsWith(begin, end, needle.wx_str(), needle.size());
}

//! Returns whether a range starts with a given string.
template <size_t N>
inline bool StartsWith(wxString::const_iterator begin, wxString::const_iterator end,
                       const wxStringCharType (&needle)[N])
{
  return StartsWith(begin, end, needle, N);
}

//! Returns whether a string starts with a given character.
inline bool StartsWith(const wxString &str, wxUniChar c)
{
  return !str.IsEmpty() && *str.begin() == c;
}

//! Returns whether a string starts with a given string literal.
template <size_t N>
inline bool StartsWith(const wxString &str, const wxStringCharType (&needle)[N])
{
  if (N > str.size()) return false;
  return str.compare(0, N, needle, N) == 0;
}

//! Returns whether a range ends with a given string.
bool EndsWith(wxString::const_iterator begin, wxString::const_iterator end,
                     const wxStringCharType *needle, size_t length);

//! Returns whether a range ends with a given string.
inline bool EndsWith(wxString::const_iterator begin, wxString::const_iterator end,
                     const wxString &needle)
{
  return EndsWith(begin, end, needle.wx_str(), needle.size());
}

//! Returns whether a range ends with a given string.
template <size_t N>
inline bool EndsWith(wxString::const_iterator begin, wxString::const_iterator end,
                     const wxStringCharType (&needle)[N])
{
  return EndsWith(begin, end, needle, N);
}

//! Returns whether a string ends with a given character.
inline bool EndsWith(const wxString &str, wxUniChar c)
{
  return !str.IsEmpty() && str.end()[-1] == c;
}

//! Returns whether a string ends with a given string literal.
template <size_t N>
inline bool EndsWith(const wxString &str, const wxStringCharType (&needle)[N])
{
  if (N > str.size()) return false;
  return str.compare(str.size() - N, N, needle, N) == 0;
}

//! Returns whether the given character is equal to one of the other arguments,
//! checked in order.
template <typename Arg, typename ...Args>
inline bool IsOneOf(wxUniChar ch, Arg arg, Args...args)
{
  return ch == arg || IsOneOf(ch, (args)...);
}

inline bool IsOneOf(wxUniChar) { return false; }

//
// StringView Implementation
//

StringView::const_reference StringView::at(size_type pos) const
{
  if (pos >= size())
    throw std::out_of_range("StringView::at(): out of bounds access");
  return s[pos];
}

#endif // WXMAXIMA_STRINGUTILS_H
