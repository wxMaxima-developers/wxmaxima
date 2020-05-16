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

#include "StringUtils.h"
#include <array>

//
// StringView
//

StringView::size_type StringView::copy(pointer dest, size_type count, size_type pos) const
{
  if (pos > size())
    throw std::out_of_range("StringView::copy(): position is greater than size()");
  auto const rcount = std::min(count == npos ? size() : count, size() - pos);
  std::copy_n(s + pos, rcount, dest);
  return rcount;
}

StringView StringView::substr(size_type pos, size_type count) const
{
  if (pos > size())
    throw std::out_of_range("StringView::substr(): position is greater than size()");
  auto const rcount = std::min(count == npos ? size() : count, size() - pos);
  return {s+pos, rcount};
}

int StringView::compare(StringView v) const noexcept
{
  auto rcount = std::min(len, v.len);
  for (const_pointer left = s, right = v.s; rcount; --rcount)
  {
    value_type l = *left++, r = *right++;
    if (l < r) return -1; else if (l > r) return 1;
  }
  if (len < v.len) return -1;
  else if (len > v.len) return 1;
  else return 0;
}

bool StringView::starts_with(const_pointer s) const
{
  for (auto it = begin(); it != end(); ++ it, ++ s)
  {
    auto const ch = *s;
    if (!ch || ch != *it)
      return false;
  }
  return true;
}

StringView::size_type StringView::find(StringView v, size_type pos) const noexcept
{
  if (pos >= size() || v.empty())
    return npos;
  auto found = std::search(std::next(begin(), pos), end(), v.begin(), v.end());
  return (found != end()) ? std::distance(begin(), found) : npos;
}

StringView::size_type StringView::rfind(StringView v, size_type pos) const noexcept
{
  if (pos == npos)
    pos = size();
  if (pos == 0 || v.empty())
    return npos;
  auto found = std::find_end(begin(), std::next(begin(), pos), v.begin(), v.end());
  return (found != end()) ? std::distance(begin(), found) : npos;
}

StringView::size_type StringView::find_first_of(StringView v, size_type pos) const noexcept
{
  if (pos >= size() || v.empty())
    return npos;
  for (auto it = std::next(begin(), pos); it != end(); ++it)
    if (v.find(*it) != npos)
      return std::distance(begin(), it);
  return npos;
}

StringView::size_type StringView::find_first_of(const_pointer s, size_type pos) const
{
  if (pos >= size() || !*s)
    return npos;

  value_type cmp = *std::next(begin(), pos);
  value_type ch;
  const_pointer s_end;
  for (s_end = s; (ch = *s_end); ++ s_end)
    if (ch == cmp)
      return pos;

  return find_first_of(StringView(s, s_end), pos + 1);
}

StringView::size_type StringView::find_last_of(StringView v, size_type pos) const noexcept
{
  if (pos == npos)
    pos = size();
  if (empty() || v.empty())
    return npos;
  auto const last = begin() + pos;
  for (auto it = std::prev(end(), 1); it >= last; --it)
    if (v.find(*it) != npos)
      return std::distance(begin(), it);
  return npos;
}

StringView::size_type StringView::find_first_not_of(StringView v, size_type pos) const noexcept
{
  if (pos >= size())
    return npos;
  if (v.empty())
    return empty() ? npos : 0;
  if (v.size() == 1)
    return find_first_not_of(v[0], pos);
  for (auto it = std::next(begin(), pos); it != end(); ++it)
    if (v.find(*it) == npos)
      return std::distance(begin(), it);
  return npos;
}

StringView::size_type StringView::find_first_not_of(value_type ch, size_type pos) const noexcept
{
  for (auto it = std::next(begin(), pos); it != end(); ++it)
    if (*it != ch)
      return std::distance(begin(), it);
  return npos;
}

StringView::size_type StringView::find_first_not_of(const_pointer s, size_type pos) const
{
  auto it = std::next(begin(), pos);
  if (it >= end())
    return npos;

  value_type cmp = *it++, ch;
  const_pointer s_end;
  bool matched = false;
  for (s_end = s; (ch = *s_end); ++ s_end)
    matched |= (ch == cmp);

  if (s == s_end)
    return npos;
  if (!matched)
    return 0;

  return find_first_not_of(StringView(s, s_end), 1);

  for (; it != end(); ++ it)
  {
    matched = false;
    cmp = *it;
    for (auto s2 = s; s2 != s_end; ++s2)
      matched |= (ch == cmp);
    if (!matched)
      return std::distance(begin(), it);
  }
  return npos;
}




//
//
//

namespace StringUtils
{

const StringForCharSubstitutions HTMLEscapes = {
  {'&',  {fasT("&amp;")}},
  {'\"', {fasT("&quot;")}},
  {'<',  {fasT("&lt;")}},
  {'>',  {fasT("&gt;")}},
  {'\n', {fasT("<br/>\n")}},
  {'\r', {fasT(" ")}},
  {}
};

}

wxString ReplacedChars(const wxString &input, StringForCharSubstitutions substs)
{
  wxString retval;
  retval.reserve(input.size());
  for (auto ch : input)
  {
    for (auto *subst = substs; subst->match; subst++)
    {
      if (ch == subst->match)
      {
        retval += subst->replacement;
        goto nextchar;
      }
    }
    retval += ch;
  nextchar: ;
  }
  return retval;
}

void ReplaceChars(wxString &string, StringForCharSubstitutions substs)
{
  string = ReplacedChars(std::move(string), substs);
}

wxString ReplacedChars(const wxString &input, CharForCharSubstitutions substs)
{
  wxString retval;
  retval.reserve(input.size());
  for (wxUniChar ch : input)
  {
    for (auto *subst = substs; subst->match; subst++)
      if (ch == subst->match)
      {
        retval += subst->replacement;
        goto nextchar;
      }
    retval += ch;
  nextchar: ;
  }
  return retval;
}

void ReplaceChars(wxString &string, CharForCharSubstitutions substs)
{
  string = ReplacedChars(std::move(string), substs);
}

struct MoveOp
{
  std::initializer_list<EditOp>::const_iterator op;
  size_t src, length, opLength;
  ssize_t delta;
};

using OpFlags = EditOp::OpFlags;
static inline OpFlags OpOp(const EditOp &op) { return OpFlags(op.flags & EditOp::OpMask); }
static inline OpFlags OpIdx(const EditOp &op) { return OpFlags(op.flags & EditOp::IdxMask); }
static inline OpFlags OpType(const  EditOp &op) { return OpFlags(op.flags & EditOp::TypeMask); }

void EditString(wxString &str, std::initializer_list<EditOp> ops)
{
  std::array<MoveOp, 32> moves;
  wxASSERT(ops.size() < moves.size());

  auto lastMove = moves.begin();
  size_t prevIndex = 0;
  bool moveIndicesSorted = true;
  for (auto op = ops.begin(); op != ops.end(); op++)
  {
    int length = 0;
    switch (OpType(*op))
    {
    case EditOp::TypeWxChar: length = 1; break;
    case EditOp::TypeWxCharEnc: length = wxStrlen(op->chEnc);
    case EditOp::TypeWxStringP: length = op->strP ? op->strP->length() : 0;
    case EditOp::TypeLiteral: length = op->lit.ptr ? op->lit.len : 0;
    default: ;
    }
    if (!length) continue;

    lastMove->op = op;
    lastMove->opLength = length;
    lastMove->src = op->index;
    moveIndicesSorted &= (lastMove->src >= prevIndex);
    prevIndex = lastMove->src;
    switch (OpOp(*op))
    {
    case EditOp::OpInsert:
      lastMove->delta = +length;
      lastMove->length = 0;
      lastMove++;
      break;
    default: ;
    }
  }
  lastMove->src = str.length();
  lastMove->delta = 0;

  // Sort moves on indices, if needed
  if (!moveIndicesSorted)
    std::sort(moves.begin(), lastMove, [](auto &m1, auto &m2){
      return m1.src < m2.src;
    });

  // Adjust indices
  ssize_t delta = 0;
  bool first = true;
  for (auto m = moves.begin(); m <= lastMove; m++)
  {
    delta += m->delta;
    m->delta = delta;
    if (!first)
      m[-1].length = m[0].src - m[-1].src;
    first = false;
  }

  // Grow the size if needed
  if (delta > 0) str.resize(str.size() + delta);

  // Perform the moves
  for (auto rm = std::make_reverse_iterator(lastMove); rm != moves.rbegin(); rm ++)
  {
    str.replace(rm->src + rm->delta, rm->length, str.wx_str() + rm->src, rm->length);
  }

  // Perform the operations
  for (auto m = moves.begin(); m != lastMove; m++)
  {
    auto op = m->op;
    auto dst = m->src + m->delta;
    if (OpOp(*op) == EditOp::OpInsert)
    {
      switch (OpType(*op))
      {
      case EditOp::TypeWxChar:
        str.replace(dst, 1, 1, op->ch); break;
      case EditOp::TypeWxCharEnc:
        str.replace(dst, m->opLength, op->chEnc, m->opLength); break;
      case EditOp::TypeWxStringP:
        str.replace(dst, m->opLength, *op->strP); break;
      case EditOp::TypeLiteral:
        str.replace(dst, m->opLength, op->lit.ptr, op->lit.len); break;
      default: ;
      }
    }
  }

  // Shrink the size if needed
  if (delta < 0) str.resize(str.size() - delta);;
}

bool AdvanceOverOne(wxString::const_iterator &ioBegin, wxString::const_iterator end,
                    const wxStringCharType *needle, size_t length)
{
  auto begin = ioBegin;
  auto *nBegin = needle, *nEnd = needle + length;
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*begin++ != *nBegin++) return false;
  }
  ioBegin = begin;
  return true;
}

bool RetractOverOne(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                    const wxStringCharType *needle, size_t length)
{
  auto end = ioEnd;
  auto *nBegin = needle, *nEnd = needle + length;
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*--end != *--nEnd) return false;
  }
  ioEnd = end;
  return true;
}

bool RetractOver(wxString::const_iterator begin, wxString::const_iterator &ioEnd,
                 const wxChar ch)
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

bool StartsWith(wxString::const_iterator begin, wxString::const_iterator end,
                const wxStringCharType *needle, size_t length)
{
  auto *nBegin = needle, *nEnd = needle + length;
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*begin++ != *nBegin++) return false;
  }
  return true;
}

inline bool EndsWith(wxString::const_iterator begin, wxString::const_iterator end,
                     const wxStringCharType *needle, size_t length)
{
  auto *nBegin = needle, *nEnd = needle + length;
  while (nBegin != nEnd)
  {
    if (begin == end) return false;
    if (*--end != *--nEnd) return false;
  }
  return true;
}

